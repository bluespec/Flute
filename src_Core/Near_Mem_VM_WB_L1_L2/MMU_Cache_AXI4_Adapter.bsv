// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package MMU_Cache_AXI4_Adapter;

// ================================================================
// Adapter converting generic 64b-wide read/write requests into an
// AXI4 bus master.  Two 'clients' upstream:
// - a cache: requests/responses are for full cache lines
// - an MMIO: requests/responses are for 64b word or sub-word,
//            and where lane-alignment is already done.

// The AXI4 bus master can be used with 32b or 64b buses, and manages
// byte-lane alignment, number of beats in a burst, write-strobes,
// etc. accordingly.

// TODO: Wrapping bursts (when cache can handle that)

// ================================================================

export  MMU_Cache_AXI4_Adapter_IFC (..);
export  mkMMU_Cache_AXI4_Adapter;

// ================================================================
// BSV lib imports

import Vector       :: *;
import BRAMCore     :: *;
import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Assert       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle     :: *;
import GetPut_Aux    :: *;
import Semi_FIFOF    :: *;

// ================================================================
// Project imports

import Cache_Decls      :: *;
import MMU_Cache_Common :: *;

import AXI4_Types  :: *;
import Fabric_Defs :: *;

// ================================================================
// This module collects and discards write-responses.
// There is no write-data response to either client.
// Errors on writes are reported on a separate method that can be used
// to trigger an interrupt.

// This module avoids interleaving read and write requests,
// i.e., it launches a read request only when no write-responses from
// the AXI4 fabric are pending.  

// ================================================================
// MODULE INTERFACE

interface MMU_Cache_AXI4_Adapter_IFC;
   // ----------------
   // interface for cache-line read/write client

   interface Server #(L1_to_L2_Req, L2_to_L1_Rsp)  cline_server;

   // ----------------
   // interface for word/sub-word read/write client

   interface Server #(Single_Req, Single_Rsp)  mmio_server;

   // ----------------
   // Write-error from memory
   (* always_ready *)
   method Bool mv_write_error;

   // ----------------
   // Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;
endinterface

// ================================================================
// Misc. help functions

// ----------------------------------------------------------------
// Byte-width of the fabric

Integer bytes_per_fabric_data = ((valueOf (Wd_Data) == 32) ? 4 : 8);

// ----------------------------------------------------------------
// Convert size code into AXI4_Size code (number of bytes in a beat).
// It just so happens that our coding coincides with AXI4's coding.

function AXI4_Size  fv_size_code_to_AXI4_Size (Bit #(2) size_code);
   return { 1'b0, size_code };
endfunction

// ----------------------------------------------------------------
// Check if addr is cache-line-aligned

Bit #(64) byte_in_line_mask = fromInteger (bytes_per_cline - 1);

function Bool fv_is_line_aligned (Bit #(64) addr);
   return ((addr & byte_in_line_mask) == 0);
endfunction

// ----------------------------------------------------------------
// Align an address to beginning of cache line containing the addr

function Bit #(64) fv_align_to_line (Bit #(64) addr);
   return (addr & (~ byte_in_line_mask));
endfunction

// ----------------------------------------------------------------
// Convert a 64-bit Address to an AXI4 Fabric Address
// For FABRIC64 this does nothing.
// For FABRIC32 it discards the upper 32 bits.

function Fabric_Addr fv_Addr_to_Fabric_Addr (Bit #(64) addr);
   return truncate (addr);
endfunction

// ================================================================
// MODULE IMPLEMENTATION

(* synthesize *)
module mkMMU_Cache_AXI4_Adapter #(parameter Bit #(3) verbosity)
                                (MMU_Cache_AXI4_Adapter_IFC);

   // Verbosity: 0=quiet, 1 = rule firings
   // Integer verbosity = 1;

   // FIFOs for line-client
   FIFOF #(L1_to_L2_Req)  f_cline_reqs <- mkFIFOF;
   FIFOF #(L2_to_L1_Rsp)  f_cline_rsps <- mkFIFOF;

   // FIFOs for single-word client
   FIFOF #(Single_Req) f_single_reqs  <- mkFIFOF;
   FIFOF #(Single_Rsp) f_single_rsps  <- mkFIFOF;

   // Identifiers for requestor client
   Bit #(1) client_id_line   = 0;
   Bit #(1) client_id_single = 1;

   // Limit the number of reads/writes outstanding to 15
   // TODO: change these to concurrent up/down counters?
   Reg #(Bit #(4)) rg_rd_rsps_pending <- mkReg (0);
   Reg #(Bit #(4)) rg_wr_rsps_pending <- mkReg (0);

   // Record errors on write-responses from mem
   Reg #(Bool) rg_wr_error <- mkReg (False);

   // AXI4 fabric request/response
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;

   // ****************************************************************
   // BEHAVIOR: READ RESPONSES (for line and single clients)
   // The following registers identify the client, the beat, etc.

   Reg #(Bit #(1)) rg_rd_client_id <- mkRegU;

   // TODO: change mkFIFOF to mkSizedFIFOF (allowing multiple outstanding reads)
   // may reads can be outstanding
   FIFOF #(Tuple3 #(Bit #(2),     // size_code
		    Bit #(3),     // addr lsbs
		    Bit #(8)))    // Num beats read-data
         f_rd_rsp_control <- mkFIFOF;
   match { .rd_req_size_code, .rd_req_addr_lsbs, .rd_req_beats } = f_rd_rsp_control.first;

   // Count beats in a single transaction.
   // Note: beat-count is for fabric, not for client-side data.
   // Former is 2 x latter for Fabric32 for 64b line data and 64b single data.

   Reg #(Bit #(8)) rg_rd_beat <- mkRegU;

   // Buffer to hold a cache line from mem
   Reg #(CLine) rg_read_data_buf <- mkRegU;

   // The following are needed for FABRIC32 during each burst response
   // to assemble lower and upper 32b into a 64b response to client
   Reg #(Bool)       rg_rd_data_lower32_ok <- mkRegU;
   Reg #(Bit #(32))  rg_rd_data_lower32    <- mkRegU;

   rule rl_read_data (rg_rd_beat < rd_req_beats);
      // Get read-data response from AXI4
      let rd_data <- pop_o (master_xactor.o_rd_data);
      Bool      ok     = (rd_data.rresp == axi4_resp_okay);
      Bit #(64) word64 = zeroExtend (rd_data.rdata);

      // Accumulate into cline
      CLine cline = ((rg_rd_beat == 0) ? 0 : rg_read_data_buf);
      if (valueOf (Wd_Data) == 32)
	 cline = ((cline >> 32) | { word64 [31:0], 0 });
      else
	 cline = ((cline >> 64) | { word64, 0 });
      rg_read_data_buf <= cline;

      rg_rd_beat <= rg_rd_beat + 1;
      Bool last_beat = (rg_rd_beat == (rd_req_beats - 1));

      // If last beat, deliver to CPU-side client
      if (last_beat) begin
	 f_rd_rsp_control.deq;
	 rg_rd_rsps_pending <= rg_rd_rsps_pending - 1;

	 if (rg_rd_client_id == client_id_line) begin
	    let rsp = L2_to_L1_Rsp {ok: ok, cline: cline};
	    f_cline_rsps.enq (rsp);
	 end
	 else begin
	    // Adjust alignment of single-read data
	    word64 = truncate (cline);
	    if (valueOf (Wd_Data) == 32)
	       word64 = (word64 << rd_req_addr_lsbs [1:0]);
	    else if (valueOf (Wd_Data) == 64)
	       word64 = (word64 << rd_req_addr_lsbs [2:0]);
	    else begin
	       $display ("%0d: ERROR: %m.rl_write_data", cur_cycle);
	       $display ("    Unsupported fabric width %0d", valueOf (Wd_Data));
	       $finish (1);
	    end
	    let rsp = Single_Rsp {ok: ok, data: word64};
	    f_single_rsps.enq (rsp);
	 end
      end

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_read_data: ", cur_cycle);
	 if (rg_rd_client_id == client_id_line) $write ("    line");
	 else                                   $write ("    single");
	 $display (" beat %0d ok %0d data %0h", rg_rd_beat, pack (ok), word64);
	 $display ("    cline %0h", cline);
      end
   endrule: rl_read_data

   // ****************************************************************
   // BEHAVIOR: LINE READ REQUESTS

   rule rl_line_read_req (f_cline_reqs.first.is_read
			  && (rg_rd_rsps_pending < '1)
			  && (rg_wr_rsps_pending == 0));
      let req <- pop (f_cline_reqs);
      
      dynamicAssert (fv_is_line_aligned (req.addr), "rl_line_read_req addr misaligned");

      let addr = fv_align_to_line (req.addr);

      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (addr);
      AXI4_Size   fabric_size = ((valueOf (Wd_Data) == 32) ? axsize_4 : axsize_8);

      // Note: AXI4 codes a burst length of 'n' as 'n-1'
      Integer   num_beats  = bytes_per_cline / bytes_per_fabric_data;
      AXI4_Len  fabric_len = fromInteger (num_beats - 1);

      let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					  araddr:   fabric_addr,
					  arlen:    fabric_len,
					  arsize:   fabric_size,
					  arburst:  axburst_incr,
					  arlock:   fabric_default_lock,
					  arcache:  fabric_default_arcache,
					  arprot:   fabric_default_prot,
					  arqos:    fabric_default_qos,
					  arregion: fabric_default_region,
					  aruser:   fabric_default_user};
      master_xactor.i_rd_addr.enq (mem_req_rd_addr);

      f_rd_rsp_control.enq (tuple3 (2'b11, addr [2:0], fromInteger (num_beats)));
      rg_rd_client_id    <= client_id_line;
      rg_rd_beat         <= 0;
      rg_rd_rsps_pending <= rg_rd_rsps_pending + 1;

      // Debugging
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_line_read_req: ", cur_cycle);
	 $display ("    AXI4_Rd_Addr{araddr %0h arlen %0d (burst len %0d) ",
		   fabric_addr,
		   fabric_len, num_beats,
		   fshow_AXI4_Size (fabric_size),
		   " incr}");
      end
   endrule: rl_line_read_req

   // ****************************************************************
   // BEHAVIOR: Single read requests (not a burst)

   rule rl_single_read_req (f_single_reqs.first.is_read
			    && (rg_rd_rsps_pending < '1)
			    && (rg_wr_rsps_pending == 0));
      let         req        <- pop (f_single_reqs);
      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
      AXI4_Size   fabric_size = fv_size_code_to_AXI4_Size (req.size_code);

      if (verbosity >= 1)
	 $display ("%0d: %m.rl_single_read_req:\n    AXI4_Rd_Addr{araddr %0h arlen 0 (burst length 1) ",
		   cur_cycle, fabric_addr,  fshow_AXI4_Size (fabric_size), "}");

      // Only size D in 32-bit fabrics needs 2 beats
      Integer num_beats   = 1;
      if ((valueOf (Wd_Data) == 32) && (req.size_code == 2'b11)) begin
	 fabric_size = axsize_4;
	 num_beats   = 2;
      end
      // Note: AXI4 codes a burst length of 'n' as 'n-1'
      AXI4_Len  fabric_len = fromInteger (num_beats - 1);

      let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					  araddr:   fabric_addr,
					  arlen:    fabric_len,
					  arsize:   fabric_size,
					  arburst:  fabric_default_burst,
					  arlock:   fabric_default_lock,
					  arcache:  fabric_default_arcache,
					  arprot:   fabric_default_prot,
					  arqos:    fabric_default_qos,
					  arregion: fabric_default_region,
					  aruser:   fabric_default_user};
      master_xactor.i_rd_addr.enq (mem_req_rd_addr);

      f_rd_rsp_control.enq (tuple3 (req.size_code, req.addr [2:0], fromInteger (num_beats)));
      rg_rd_client_id    <= client_id_single;
      rg_rd_beat         <= 0;
      rg_rd_rsps_pending <= rg_rd_rsps_pending + 1;
   endrule: rl_single_read_req

   // ****************************************************************
   // BEHAVIOR: WRITE RESPONSES (for line and single clients)
   // The following register identifies the client

   rule rl_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      Bool err = False;
      if (rg_wr_rsps_pending == 0) begin
	 rg_wr_error <= True;

	 $display ("%0d: %m.rl_write_rsp: ERROR not expecting any write-response:", cur_cycle);
	 $display ("    ", fshow (wr_resp));
      end
      else begin
	 rg_wr_rsps_pending <= rg_wr_rsps_pending - 1;
	 if (wr_resp.bresp != axi4_resp_okay) begin
	    rg_wr_error <= True;
	    if (verbosity >= 1) begin
	       $display ("%0d: %m.rl_write_rsp: FABRIC RESPONSE ERROR", cur_cycle);
	       $display ("    ", fshow (wr_resp));
	    end
	 end
	 else if (verbosity >= 1) begin
	    $display ("%0d: %m.rl_write_rsp: pending=%0d, ",
		      cur_cycle, rg_wr_rsps_pending, fshow (wr_resp));
	 end
      end
   endrule: rl_write_rsp

   // ****************************************************************
   // BEHAVIOR: Write data (for both line and single)

   // Regs holding state during write-burst
   Reg #(Bit #(1))               rg_wr_client_id     <- mkRegU;
   Reg #(Bit #(8))               rg_wr_pending_beats <- mkReg (0);
   Reg #(CLine)                  rg_wr_data_buf      <- mkRegU;
   Reg #(Bit #(Bytes_per_CLine)) rg_wr_strb_buf      <- mkRegU;

   rule rl_write_data (rg_wr_pending_beats != 0);
      Bool last = (rg_wr_pending_beats == 1);
      if (last) begin
	 if (rg_wr_client_id == client_id_line)
	    f_cline_reqs.deq;
	 else
	    f_single_reqs.deq;
      end
      rg_wr_pending_beats <= rg_wr_pending_beats - 1;

      CLine                   cline = rg_wr_data_buf;
      Bit #(Bytes_per_CLine)  strb  = rg_wr_strb_buf;

      // Invariant: cline and strb are properly aligned in lsbs

      // Send AXI write-data
      Bit #(Wd_Data)             wdata = truncate (cline);
      Bit #(TDiv #(Wd_Data, 8))  wstrb = truncate (strb);
      let mem_req_wr_data = AXI4_Wr_Data {wdata:  wdata,
					  wstrb:  wstrb,
					  wlast:  last,
					  wuser:  fabric_default_user};
      master_xactor.i_wr_data.enq (mem_req_wr_data);

      // Prepare for next beat
      rg_wr_data_buf <= (cline >> valueOf (Wd_Data));
      rg_wr_strb_buf <= (strb  >> valueOf (TDiv #(Wd_Data, 8)));

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_write_data: pending beats %0d", cur_cycle, rg_wr_pending_beats);
	 $display ("    ", fshow (mem_req_wr_data));
      end
   endrule: rl_write_data

   // ****************************************************************
   // BEHAVIOR: Line-write requests

   rule rl_line_write_req ((! f_cline_reqs.first.is_read)
			   && (rg_wr_pending_beats == 0)
			   && (rg_rd_rsps_pending == 0)
			   && (rg_wr_rsps_pending < '1));
      let req = f_cline_reqs.first;
      rg_wr_data_buf <= req.cline;
      rg_wr_strb_buf <= '1;
      
      dynamicAssert (fv_is_line_aligned (req.addr), "rl_line_write_req addr misaligned");

      let addr = fv_align_to_line (req.addr);

      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (addr);
      AXI4_Size   fabric_size = ((valueOf (Wd_Data) == 32) ? axsize_4 : axsize_8);

      // Note: AXI4 codes a burst length of 'n' as 'n-1'
      Integer   num_beats  = bytes_per_cline / bytes_per_fabric_data;
      AXI4_Len  fabric_len = fromInteger (num_beats - 1);

      let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
					  awaddr:   fabric_addr,
					  awlen:    fabric_len,
					  awsize:   fabric_size,
					  awburst:  axburst_incr,
					  awlock:   fabric_default_lock,
					  awcache:  fabric_default_awcache,
					  awprot:   fabric_default_prot,
					  awqos:    fabric_default_qos,
					  awregion: fabric_default_region,
					  awuser:   fabric_default_user};
      master_xactor.i_wr_addr.enq (mem_req_wr_addr);

      rg_wr_client_id     <= client_id_line;
      rg_wr_pending_beats <= fromInteger (num_beats);

      // Debugging
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_line_write_req: AXI4_Wr_Addr{awaddr %0h awlen %0d burst-length %0d ",
		   cur_cycle,
		   fabric_addr,
		   fabric_len, num_beats,
		   fshow_AXI4_Size (fabric_size),
		   " incr}");
   endrule: rl_line_write_req

   // ****************************************************************
   // Scheduling

   (* descending_urgency = "rl_read_data,       rl_write_rsp" *)
   (* descending_urgency = "rl_write_rsp,       rl_line_read_req" *)
   (* descending_urgency = "rl_line_read_req,   rl_line_write_req" *)
   (* descending_urgency = "rl_line_write_req,  rl_write_data" *)
   (* descending_urgency = "rl_write_data,      rl_single_read_req" *)
   (* descending_urgency = "rl_single_read_req, rl_single_write_req" *)

   // The following should be implied by transitivity, but bsc still cites them
   (* descending_urgency = "rl_read_data,       rl_line_read_req" *)
   (* descending_urgency = "rl_line_read_req,   rl_single_read_req" *)
   (* descending_urgency = "rl_line_write_req,  rl_single_read_req" *)
   (* descending_urgency = "rl_write_rsp,       rl_line_write_req" *)
   (* descending_urgency = "rl_line_write_req,  rl_single_write_req" *)
   (* descending_urgency = "rl_line_read_req,   rl_single_write_req" *)

   // ****************************************************************
   // BEHAVIOR: Single write requests (not a burst)

   rule rl_single_write_req ((! f_single_reqs.first.is_read)
			     && (rg_wr_pending_beats == 0)
			     && (rg_rd_rsps_pending == 0)
			     && (rg_wr_rsps_pending < '1));
      let req = f_single_reqs.first;

      // Single writes: data is in lsbs
      Bit #(64) data64 = req.data;
      Bit #(8)  strb = case (req.size_code)
			  2'b00: 8'h_01;
			  2'b01: 8'h_03;
			  2'b10: 8'h_0F;
			  2'b11: 8'h_FF;
		       endcase;

      // adjust alignment of data and strobe for AXI4 data bus
      if (valueOf (Wd_Data) == 32) begin
	 data64 = (data64 << req.addr [1:0]);
	 strb   = (strb   << req.addr [1:0]);
      end
      else if (valueOf (Wd_Data) == 64) begin
	 data64 = (data64 << req.addr [2:0]);
	 strb   = (strb   << req.addr [2:0]);
      end
      else begin
	 $display ("%0d: ERROR: %m.rl_write_data", cur_cycle);
	 $display ("    Unsupported fabric width %0d", valueOf (Wd_Data));
	 $finish (1);
      end
      rg_wr_data_buf <= { ?, data64 };
      rg_wr_strb_buf <= { ?, strb };

      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
      AXI4_Size   fabric_size = fv_size_code_to_AXI4_Size (req.size_code);

      // Only size D in 32-bit fabrics needs 2 beats
      Integer num_beats = 1;
      if ((valueOf (Wd_Data) == 32) && (req.size_code == 2'b11)) begin
	 fabric_size = axsize_4;
	 num_beats   = 2; 
      end
      // Note: AXI4 codes a burst length of 'n' as 'n-1'
      AXI4_Len  fabric_len = fromInteger (num_beats - 1);

      // AXI4 Write-Address channel
      let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
					  awaddr:   fabric_addr,
					  awlen:    fabric_len,
					  awsize:   fabric_size,
					  awburst:  fabric_default_burst,
					  awlock:   fabric_default_lock,
					  awcache:  fabric_default_awcache,
					  awprot:   fabric_default_prot,
					  awqos:    fabric_default_qos,
					  awregion: fabric_default_region,
					  awuser:   fabric_default_user};
      master_xactor.i_wr_addr.enq (mem_req_wr_addr);

      rg_wr_client_id     <= client_id_single;
      rg_wr_pending_beats <= fromInteger (num_beats);

      // Debugging
      $display ("%0d: %m.rl_single_write_req: AXI4_Wr_Addr{awaddr %0h awlen %0d burst-length %0d ",
		cur_cycle,
		fabric_addr,
		fabric_len, num_beats,
		fshow_AXI4_Size (fabric_size),
		" incr}");
   endrule: rl_single_write_req

   // ================================================================
   // INTERFACE

   // ----------------
   // interface for cache-line read/write client

   interface Server cline_server = toGPServer (f_cline_reqs, f_cline_rsps);

   // ----------------
   // interface for word/sub-word read/write client

   interface Server mmio_server = toGPServer (f_single_reqs, f_single_rsps);

   // ----------------
   // Write-error from memory

   method Bool mv_write_error = rg_wr_error;

   // ----------------
   // Fabric master interface
   interface mem_master = master_xactor.axi_side;
endmodule

// ================================================================

endpackage
