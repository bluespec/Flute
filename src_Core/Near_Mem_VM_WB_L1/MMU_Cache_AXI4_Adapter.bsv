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

   interface Put #(Line_Req)  p_mem_line_req;
   interface Put #(Bit #(64)) p_mem_line_write_data;
   interface Get #(Read_Data) g_mem_line_read_data;

   // ----------------
   // interface for word/sub-word read/write client

   interface Put #(Single_Req) p_mem_single_req;
   interface Put #(Bit #(64))  p_mem_single_write_data;
   interface Get #(Read_Data)  g_mem_single_read_data;

   // ----------------
   // Write-error from memory
   (* always_ready *)
   method Bool mv_write_error;

   // ----------------
   // Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;

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
   FIFOF #(Line_Req)   f_line_reqs       <- mkFIFOF;
   FIFOF #(Bit #(64))  f_line_write_data <- mkFIFOF;
   FIFOF #(Read_Data)  f_line_read_data  <- mkFIFOF;

   // FIFOs for single-word client
   FIFOF #(Single_Req) f_single_reqs       <- mkFIFOF;
   FIFOF #(Bit #(64))  f_single_write_data <- mkFIFOF;
   FIFOF #(Read_Data)  f_single_read_data  <- mkFIFOF;

   // Identifiers for requestor client
   Bit #(1) client_id_line   = 0;
   Bit #(1) client_id_single = 1;

   // Limit the number of reads/writes outstanding to 15
   // TODO: change these to concurrent up/down counters?
   Reg #(Bit #(4)) rg_rd_rsps_pending <- mkReg (0);
   Reg #(Bit #(4)) rg_wr_rsps_pending <- mkReg (0);

   Reg #(Bool) rg_ddr4_ready <- mkReg (False);

   // Record errors on write-responses from mem
   Reg #(Bool) rg_write_error <- mkReg (False);

   // AXI4 fabric request/response
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;

   // ****************************************************************
   // BEHAVIOR: READ RESPONSES (for line and single clients)
   // The following registers identify the client, the beat, etc.

   Reg #(Bit #(1)) rg_rd_client_id <- mkRegU;

   // TODO: change mkFIFOF to mkSizedFIFOF (allowing multiple outstanding reads)
   // may reads can be outstanding
   FIFOF #(Tuple3 #(Bit #(2),     // size_code
		    Bit #(1),     // addr bit [2]
		    Bit #(8)))    // Num beats read-data
         f_rd_rsp_control <- mkFIFOF;
   match { .rd_req_size_code, .rd_req_addr_bit_2, .rd_req_beats } = f_rd_rsp_control.first;

   // Count beats in a single transaction.
   // Note: beat-count is for fabric, not for client-side data.
   // Former is 2 x latter for Fabric32 for 64b line data and 64b single data.

   Reg #(Bit #(8)) rg_rd_beat <- mkRegU;

   // The following are needed for FABRIC32 during each burst response
   // to assemble lower and upper 32b into a 64b response to client
   Reg #(Bool)       rg_rd_data_lower32_ok <- mkRegU;
   Reg #(Bit #(32))  rg_rd_data_lower32    <- mkRegU;

   rule rl_read_data (rg_rd_beat < rd_req_beats);
      rg_rd_beat <= rg_rd_beat + 1;
      Bool last_beat = (rg_rd_beat == (rd_req_beats - 1));
      if (last_beat) begin
	 f_rd_rsp_control.deq;
	 rg_rd_rsps_pending <= rg_rd_rsps_pending - 1;
      end

      let rd_data <- pop_o (master_xactor.o_rd_data);

      Bool      ok   = (rd_data.rresp == axi4_resp_okay);
      Bit #(64) data = zeroExtend (rd_data.rdata);

      Bool do_enq    = True;
      Bool even_beat = (rg_rd_beat [0] == 1'b0);

      // FABRIC32 adjustments
      if (valueOf (Wd_Data) == 32) begin
	 if (rd_req_size_code != 2'b11) begin
	    // B, H, W: only 1 beat
	    Bool in_upper32 = (rd_req_addr_bit_2 == 1'b1);
	    if (in_upper32)
	       data = { data [31:0], 32'b0 };
	    do_enq = True;
	 end
	 else if (even_beat) begin // D, even beat
	    // Just save lower 32b; enq response only after next beat
	    rg_rd_data_lower32_ok <= ok;
	    rg_rd_data_lower32    <= data [31:0];
	    do_enq = False;
	 end
	 else begin // D, odd beat
	    ok   = (rg_rd_data_lower32_ok && ok);
	    data = { data [31:0], rg_rd_data_lower32 };
	    do_enq = True;
	 end
      end	       

      if (do_enq) begin
	 let rsp = Read_Data {ok: ok, data: data};
	 if (rg_rd_client_id == client_id_line)
	    f_line_read_data.enq (rsp);
	 else
	    f_single_read_data.enq (rsp);
      end

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_read_data: ", cur_cycle);
	 if (rg_rd_client_id == client_id_line) $write ("    line (beat %0d)", rg_rd_beat);
	 else                                   $write ("     single");
	 $write (" ok %0d data %0h", pack (ok), data);
	 if ((valueOf (Wd_Data) == 32) && even_beat) $write (" (lower 32b of 64b)");
	 $display ("");
      end
   endrule: rl_read_data

   // ****************************************************************
   // BEHAVIOR: LINE READ REQUESTS

   rule rl_line_read_req (f_line_reqs.first.is_read
			  && rg_ddr4_ready
			  && (rg_rd_rsps_pending < '1)
			  && (rg_wr_rsps_pending == 0));
      let req <- pop (f_line_reqs);
      
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

      f_rd_rsp_control.enq (tuple3 (2'b11, 1'b0, fromInteger (num_beats)));
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
   endrule

   // ****************************************************************
   // BEHAVIOR: Single read requests (not a burst)

   rule rl_single_read_req (f_single_reqs.first.is_read
			    && rg_ddr4_ready
			    && (rg_rd_rsps_pending < '1)
			    && (rg_wr_rsps_pending == 0));
      let         req        <- pop (f_single_reqs);
      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
      AXI4_Size   fabric_size = fv_size_code_to_AXI4_Size (req.size_code);

      if (verbosity >= 1)
	 $display ("%0d: %m.rl_single_read_req:\n    AXI4_Rd_Addr{araddr %0h arlen 0 (burst length 1) ",
		   cur_cycle, fabric_addr,  fshow_AXI4_Size (fabric_size), "}");

      Bit #(8)    num_beats   = 1;

      // FABRIC32 adjustments
      if ((valueOf (Wd_Data) == 32) && (req.size_code == 2'b11)) begin
	 fabric_size = axsize_4;
	 num_beats   = 2;
      end
      // Note: AXI4 codes a burst length of 'n' as 'n-1'
      AXI4_Len fabric_len = num_beats - 1;

      let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					  araddr:   fabric_addr,
					  arlen:    0,           // burst len = arlen+1
					  arsize:   fabric_size,
					  arburst:  fabric_default_burst,
					  arlock:   fabric_default_lock,
					  arcache:  fabric_default_arcache,
					  arprot:   fabric_default_prot,
					  arqos:    fabric_default_qos,
					  arregion: fabric_default_region,
					  aruser:   fabric_default_user};
      master_xactor.i_rd_addr.enq (mem_req_rd_addr);

      f_rd_rsp_control.enq (tuple3 (req.size_code, req.addr [2], num_beats));
      rg_rd_client_id    <= client_id_single;
      rg_rd_beat         <= 0;
      rg_rd_rsps_pending <= rg_rd_rsps_pending + 1;
   endrule

   // ****************************************************************
   // BEHAVIOR: WRITE RESPONSES (for line and single clients)
   // The following register identifies the client

   rule rl_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      Bool err = False;
      if (rg_wr_rsps_pending == 0) begin
	 rg_write_error <= True;

	 $display ("%0d: %m.rl_write_rsp: ERROR not expecting any write-response:", cur_cycle);
	 $display ("    ", fshow (wr_resp));
      end
      else begin
	 rg_wr_rsps_pending <= rg_wr_rsps_pending - 1;
	 if (wr_resp.bresp != axi4_resp_okay) begin
	    rg_write_error <= True;
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
   endrule

   // ****************************************************************
   // BEHAVIOR: Write data (for both line and single)
   // Assume that data is already lane-aligned for 64b data width.

   FIFOF #(Tuple4 #(Bit #(1),     // client_id
		    Bit #(2),     // size_code
		    Bit #(3),     // addr lsbs
		    Bit #(8)))    // Num beats in write-data
         f_wr_data_control <- mkFIFOF;
   match {.wr_client_id,
	  .wr_req_size_code,
	  .wr_req_addr_lsbs,
	  .wr_req_beats } = f_wr_data_control.first;

   // Count beats in a single transaction
   // Note: beat-count is for fabric, not for client-side data.
   // Former is 2 x latter for Fabric32 for 64b line data and 64b single data.
   Reg #(Bit #(8)) rg_wr_beat <- mkReg (0);

   rule rl_write_data (rg_ddr4_ready && (rg_wr_beat < wr_req_beats));
      Bool last = (rg_wr_beat == (wr_req_beats - 1));
      if (last) begin
	 f_wr_data_control.deq;
	 rg_wr_beat <= 0;
      end
      else
	 rg_wr_beat <= rg_wr_beat + 1;

      Bit #(64) data = ((wr_client_id == client_id_line)
			? f_line_write_data.first
			: f_single_write_data.first);

      // Compute strobe from size and address
      Bit #(8)  strb = case (wr_req_size_code)
			  2'b00: 8'h_01;
			  2'b01: 8'h_03;
			  2'b10: 8'h_0F;
			  2'b11: 8'h_FF;
		       endcase;
      strb = (strb << wr_req_addr_lsbs);
      
      Bool do_deq    = True;
      Bool even_beat = (rg_wr_beat [0] == 1'b0);

      // FABRIC32 adjustments
      if (valueOf (Wd_Data) == 32) begin
	 if (wr_req_size_code != 2'b11) begin
	    // B, H, W: only 1 beat
	    Bool in_upper32 = (wr_req_addr_lsbs [2] == 1'b1);
	    if (in_upper32) begin
	       data = { 32'b0, data [63:32] };
	       strb = {  4'b0, strb [7:4] };
	    end
	    do_deq = True;
	 end
	 else if (even_beat) begin  // D, even beat
	    do_deq = False;
	 end
	 else begin // D, odd beat
	    data   = { 32'b0, data [63:32] };
	    strb   = '1;
	    do_deq = True;
	 end
      end

      let mem_req_wr_data = AXI4_Wr_Data {wdata:  truncate (data),
					  wstrb:  truncate (strb),
					  wlast:  last,
					  wuser:  fabric_default_user};
      master_xactor.i_wr_data.enq (mem_req_wr_data);

      if (do_deq) begin
	 if (wr_client_id == client_id_line)
	    f_line_write_data.deq;
	 else
	    f_single_write_data.deq;
      end

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_write_data: beat %0d/%0d", cur_cycle, rg_wr_beat, wr_req_beats);
	 $display ("    AXI4_Wr_Data{%0h strb %0h last %0d}", data, strb, pack (last));
      end
   endrule

   // ****************************************************************
   // BEHAVIOR: Line-write requests

   rule rl_line_write_req ((! f_line_reqs.first.is_read)
			   && rg_ddr4_ready
			   && (rg_rd_rsps_pending == 0)
			   && (rg_wr_rsps_pending < '1));
      let req <- pop (f_line_reqs);
      
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

      f_wr_data_control.enq (tuple4 (client_id_line,
				     2'b11,        // size_code D
				     0,            // req.addr lsbs
				     fromInteger (num_beats)));
      rg_wr_rsps_pending <= rg_wr_rsps_pending + 1;

      // Debugging
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_line_write_req: AXI4_Wr_Addr{awaddr %0h awlen %0d burst-length %0d ",
		   cur_cycle,
		   fabric_addr,
		   fabric_len, num_beats,
		   fshow_AXI4_Size (fabric_size),
		   " incr}");
   endrule

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
			     && rg_ddr4_ready
			     && (rg_rd_rsps_pending == 0)
			     && (rg_wr_rsps_pending < '1));
      let req <- pop (f_single_reqs);

      Fabric_Addr fabric_addr = fv_Addr_to_Fabric_Addr (req.addr);
      AXI4_Size   fabric_size = fv_size_code_to_AXI4_Size (req.size_code);
      Bit #(8)    num_beats   = 1;

      // FABRIC32 adjustments
      if ((valueOf (Wd_Data) == 32) && (req.size_code == 2'b11)) begin
	 fabric_size = axsize_4;
	 num_beats   = 2; 
      end
      // Note: AXI4 codes a burst length of 'n' as 'n-1'
      AXI4_Len fabric_len = num_beats - 1;

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

      f_wr_data_control.enq (tuple4 (client_id_single,
				     req.size_code,
				     req.addr [2:0],
				     num_beats));
      rg_wr_rsps_pending <= rg_wr_rsps_pending + 1;

      // Debugging
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_single_write_req: AXI4_Wr_Addr{awaddr %0h awlen %0d burst-length %0d ",
		   cur_cycle,
		   fabric_addr,
		   fabric_len, num_beats,
		   fshow_AXI4_Size (fabric_size),
		   " incr}");
   endrule

   // ================================================================
   // INTERFACE

   // ----------------
   // interface for cache-line read/write client

   interface Put p_mem_line_req        = toPut (f_line_reqs);
   interface Put p_mem_line_write_data = toPut (f_line_write_data);
   interface Get g_mem_line_read_data  = toGet (f_line_read_data);

   // ----------------
   // interface for word/sub-word read/write client

   interface Put p_mem_single_req        = toPut (f_single_reqs);
   interface Put p_mem_single_write_data = toPut (f_single_write_data);
   interface Get g_mem_single_read_data  = toGet (f_single_read_data);

   // ----------------
   // Write-error from memory

   method Bool mv_write_error = rg_write_error;

   // ----------------
   // Fabric master interface
   interface mem_master = master_xactor.axi_side;

   // ----------------------------------------------------------------
   // Misc. control and status

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;
      rg_ddr4_ready <= True;
      $display ("%0d: %m: Enabling memory accesses", cur_cycle);
   endmethod

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status;
      return (rg_write_error ? 1 : 0);
   endmethod

endmodule

// ================================================================

endpackage
