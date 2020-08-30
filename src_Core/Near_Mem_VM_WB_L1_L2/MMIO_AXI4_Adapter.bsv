// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package MMIO_AXI4_Adapter;

// ================================================================
// Adapter converting read/write requests/responses, upto 64b-wide,
// from N upstream MMIO clients, into an AXI4 bus master.

// On the client-side:
// - Data is always aligned to LSBs (like CPU registers)
// - There are no write-responses.  AXI4-side write-responses are
//     collected and discarded, after checking for errors.
//     Write-errors are remembered and reported in a separate method
//     that can be used, for example, to trigger an interrupt.

// On the AXI4 side:
// - The bus master can be used with 32b or 64b buses.  This adapter
//     manages byte-lane alignment, number of beats in a burst,
//     write-strobes, etc.

// This module avoids interleaving read and write requests, i.e., it
// launches a read request only when no write-responses from the AXI4
// fabric are pending, and launches a write request only when no read
// reponses are pending.

// ================================================================

export  MMIO_AXI4_Adapter_IFC (..);
export  mkMMIO_AXI4_Adapter;      // parameterized by # of clients
export  mkMMIO_AXI4_Adapter_2;    // 2 clients, synthesized

// ================================================================
// BSV lib imports

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import MMU_Cache_Common :: *;    // For Single_Req, Single_Rsp types

import AXI4_Types  :: *;
import Fabric_Defs :: *;

// ================================================================
// MODULE INTERFACE

interface MMIO_AXI4_Adapter_IFC #(numeric type num_clients_t);
   // ----------------
   // Client-side
   interface Vector #(num_clients_t,
		      Server #(Single_Req, Single_Rsp))  v_mmio_server;

   // ----------------
   // AXI-side
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;

   // ----------------
   // Write-error from memory
   (* always_ready *)
   method Bool mv_write_error;
endinterface

// ================================================================
// Misc. help functions

// ----------------------------------------------------------------
// Convert size code into AXI4_Size code (number of bytes in a beat).
// It just so happens that our coding coincides with AXI4's coding.

function AXI4_Size  fv_size_code_to_AXI4_Size (Bit #(2) size_code);
   return { 1'b0, size_code };
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

module mkMMIO_AXI4_Adapter #(parameter Bit #(3) verbosity)
                           (MMIO_AXI4_Adapter_IFC #(num_clients_t))
   provisos (NumAlias #(TLog #(num_clients_t), log_num_clients_t));

   // Verbosity: 0=quiet, 1 = rule firings

   Integer num_clients = valueOf (num_clients_t);

   Vector #(num_clients_t, FIFOF #(Single_Req)) v_f_reqs <- replicateM (mkFIFOF);
   Vector #(num_clients_t, FIFOF #(Single_Rsp)) v_f_rsps <- replicateM (mkFIFOF);

   // Limit the number of reads/writes outstanding to 15
   // TODO: change these to concurrent up/down counters?
   Reg #(Bit #(4)) rg_rd_rsps_pending <- mkReg (0);
   Reg #(Bit #(4)) rg_wr_rsps_pending <- mkReg (0);

   // Record errors on write-responses from mem
   Reg #(Bool) rg_wr_error <- mkReg (False);

   // AXI4 fabric request/response
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)
      master_xactor <- mkAXI4_Master_Xactor;

   // ****************************************************************
   // BEHAVIOR: read requests

   // ----------------
   // Control info for read-responses

   // TODO: change mkFIFOF to mkSizedFIFOF (allowing multiple outstanding reads)
   // many reads can be outstanding
   FIFOF #(Tuple4 #(Bit #(log_num_clients_t),    // client_id
		    AXI4_Size,
		    Bit #(3),                    // addr lsbs
		    AXI4_Len))                   // arlen (= # of beats - 1)
         f_rd_rsp_control <- mkFIFOF;

   Reg #(AXI4_Len) rg_rd_beat <- mkReg (0);    // Beat counter

   // ----------------
   // Read requests

   function Action fa_rd_req (Integer j, FIFOF #(Single_Req) f_reqs_j);
      action
	 let         req   <- pop (f_reqs_j);
	 Fabric_Addr araddr = fv_Addr_to_Fabric_Addr (req.addr);
	 AXI4_Size   arsize = fv_size_code_to_AXI4_Size (req.size_code);

	 // Note: AXI4 codes a burst length of 'n' as 'n-1'.
	 // Only size D in 32-bit fabrics needs 2 beats.
	 AXI4_Len  arlen = 0;    // 1 beat
	 if ((valueOf (Wd_Data) == 32) && (req.size_code == 2'b11)) begin
	    arsize = axsize_4;
	    arlen  = 1;    // 2 beats
	 end

	 if (verbosity >= 1) begin
	    $display ("%0d: %m.rl_rd_req", cur_cycle);
	    $display ("    AXI4_Rd_Addr {araddr %0h arlen %d ",
		      araddr,  arlen, fshow_AXI4_Size (arsize), "}");
	 end

	 let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					     araddr:   araddr,
					     arlen:    arlen,
					     arsize:   arsize,
					     arburst:  fabric_default_burst,
					     arlock:   fabric_default_lock,
					     arcache:  fabric_default_arcache,
					     arprot:   fabric_default_prot,
					     arqos:    fabric_default_qos,
					     arregion: fabric_default_region,
					     aruser:   fabric_default_user};
	 master_xactor.i_rd_addr.enq (mem_req_rd_addr);

	 f_rd_rsp_control.enq (tuple4 (fromInteger (j), arsize, req.addr [2:0], arlen));
	 rg_rd_beat         <= 0;
	 rg_rd_rsps_pending <= rg_rd_rsps_pending + 1;
      endaction
   endfunction

   for (Integer j = 0; j < num_clients; j = j + 1)
      rule rl_rd_req (v_f_reqs [j].first.is_read
		      && (rg_rd_rsps_pending < '1)
		      && (rg_wr_rsps_pending == 0));
	 fa_rd_req (j, v_f_reqs [j]);
      endrule: rl_rd_req

   // ----------------
   // Read responses

   match {.rd_client_id,
	  .rd_arsize,
	  .rd_addr_lsbs, 
	  .rd_arlen      } = f_rd_rsp_control.first;

   Reg #(Bit #(64)) rg_rd_data_buf <- mkRegU;    // accumulate data across beats

   rule rl_rd_data (rg_rd_beat <= rd_arlen);
      // Get read-data response from AXI4
      let       rd_data <- pop_o (master_xactor.o_rd_data);
      Bool      ok      = (rd_data.rresp == axi4_resp_okay);

      // Accumulate beats into word64 and rg_rd_data
      Bit #(64) word64 = rg_rd_data_buf;
      if (rg_rd_beat == 0)
	 word64 = zeroExtend (rd_data.rdata);
      else if (rg_rd_beat == 1)
	 // 2nd beat is only possible in 32-bit fabrics
	 word64 = { rd_data.rdata [31:0], word64 [31:0] };
      else begin
	 $display ("%0d: INTERNAL ERROR: %m.rl_rd_data", cur_cycle);
	 $display ("    Unexpected beat number %0d (can only be 0 or 1)", rg_rd_beat);
	 $finish (1);
      end
      rg_rd_data_buf <= word64;

      // If last beat, deliver to CPU-side client
      if (rg_rd_beat == rd_arlen) begin
	 f_rd_rsp_control.deq;
	 rg_rd_rsps_pending <= rg_rd_rsps_pending - 1;

	 // Adjust alignment of B,H,W data
	 // addr [1:0] is non-zero only for B, H, W (so, single-beat, so data is in [31:0])
	 if (rd_arsize != axsize_8) begin
	    Bit #(6) shamt_bits = ?;
	    if (valueOf (Wd_Data) == 32)
	       shamt_bits = { 1'b0, rd_addr_lsbs [1:0], 3'b000 };
	    else if (valueOf (Wd_Data) == 64)
	       shamt_bits = { rd_addr_lsbs [2:0], 3'b000 };
	    else begin
	       $display ("%0d: INTERNAL ERROR: %m.rl_rd_data", cur_cycle);
	       $display ("    Unsupported fabric width %0d", valueOf (Wd_Data));
	       $finish (1);
	    end
	    word64 = (word64 >> shamt_bits);
	 end
	 let rsp = Single_Rsp {ok: ok, data: word64};
	 v_f_rsps [rd_client_id].enq (rsp);

	 // Reset beat counter for next transaction
	 rg_rd_beat <= 0;
      end
      else
	 rg_rd_beat <= rg_rd_beat + 1;

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_rd_data: ", cur_cycle);
	 $display ("    [%0d] beat %0d data %0h", rd_client_id, rg_rd_beat, word64);
      end
   endrule: rl_rd_data

   // ****************************************************************
   // BEHAVIOR: write requests

   // Regs holding state during write-data burst
   Reg #(Bit #(log_num_clients_t))  rg_wr_client_id <- mkRegU;
   Reg #(Bit #(8))                  rg_awlen        <- mkReg (0);
   Reg #(Bit #(64))                 rg_wr_data_buf  <- mkRegU;
   Reg #(Bit #(8))                  rg_wr_strb_buf  <- mkRegU;

   // Beat counter: There are pending beats when rg_wr_beat <= rg_awlen
   Reg #(Bit #(8))  rg_wr_beat <- mkReg (1);

   function Action fa_wr_req (Integer j, FIFOF #(Single_Req) f_reqs_j);
      action
	 let req = f_reqs_j.first;    // Don't deq it until data beats sent

	 // Data is in lsbs
	 Bit #(64) word64 = req.data;
	 Bit #(8)  strb   = case (req.size_code)
			       2'b00: 8'h_01;
			       2'b01: 8'h_03;
			       2'b10: 8'h_0F;
			       2'b11: 8'h_FF;
			    endcase;

	 Fabric_Addr awaddr = fv_Addr_to_Fabric_Addr (req.addr);
	 AXI4_Size   awsize = fv_size_code_to_AXI4_Size (req.size_code);
	 AXI4_Len    awlen  = 0;    // 1 beat

	 // Adjustments for AXI4 data bus widths of 32-bit and 64-bit
	 if (awsize == axsize_8) begin
	    if (valueOf (Wd_Data) == 32) begin
	       awsize = axsize_4;
	       awlen  = 1;     // 2 beats
	    end
	 end
	 else begin
	    if (valueOf (Wd_Data) == 32) begin
	       word64 = (word64 << ({ req.addr [1:0], 3'b0 }));
	       strb   = (strb   << req.addr [1:0]);
	    end
	    else if (valueOf (Wd_Data) == 64) begin
	       word64 = (word64 << ({ req.addr [2:0], 3'b0 }));
	       strb   = (strb   << req.addr [2:0]);
	    end
	    else begin
	       $display ("%0d: ERROR: %m.rl_wr_data", cur_cycle);
	       $display ("    Unsupported fabric width %0d", valueOf (Wd_Data));
	       $finish (1);
	    end
	 end
	 rg_wr_client_id <= fromInteger (j);
	 rg_awlen        <= awlen;
	 rg_wr_data_buf  <= word64;
	 rg_wr_strb_buf  <= strb;
	 rg_wr_beat      <= 0;

	 // AXI4 Write-Address channel
	 let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
					     awaddr:   awaddr,
					     awlen:    awlen,
					     awsize:   awsize,
					     awburst:  fabric_default_burst,
					     awlock:   fabric_default_lock,
					     awcache:  fabric_default_awcache,
					     awprot:   fabric_default_prot,
					     awqos:    fabric_default_qos,
					     awregion: fabric_default_region,
					     awuser:   fabric_default_user};
	 master_xactor.i_wr_addr.enq (mem_req_wr_addr);
	 rg_wr_rsps_pending <= rg_wr_rsps_pending + 1;

	 // Debugging
	 if (verbosity >= 1) begin
	    $display ("%0d: %m.rl_wr_req", cur_cycle);
	    $display ("    AXI4_Wr_Addr{awaddr %0h awlen %0d ",
		      awaddr, awlen,
		      fshow_AXI4_Size (awsize),
		      " incr}");
	 end
      endaction
   endfunction

   for (Integer j = 0; j < num_clients; j = j + 1)
      rule rl_wr_req ((! v_f_reqs [j].first.is_read)
		      && (rg_wr_beat > rg_awlen)
		      && (rg_rd_rsps_pending == 0)
		      && (rg_wr_rsps_pending < '1));
	 fa_wr_req (j, v_f_reqs [j]);
      endrule: rl_wr_req

   // ----------------
   // Write data (multiple beats if fabric data width < 64b)

   rule rl_wr_data (rg_wr_beat <= rg_awlen);
      Bool last = (rg_wr_beat == rg_awlen);
      if (last)
	 v_f_reqs [rg_wr_client_id].deq;
      rg_wr_beat <= rg_wr_beat + 1;

      // Send AXI write-data
      Bit #(Wd_Data)             wdata = truncate (rg_wr_data_buf);
      Bit #(TDiv #(Wd_Data, 8))  wstrb = truncate (rg_wr_strb_buf);
      let wr_data = AXI4_Wr_Data {wdata:  wdata,
				  wstrb:  wstrb,
				  wlast:  last,
				  wuser:  fabric_default_user};
      master_xactor.i_wr_data.enq (wr_data);

      // Prepare for next beat
      rg_wr_data_buf <= (rg_wr_data_buf >> valueOf (Wd_Data));
      rg_wr_strb_buf <= (rg_wr_strb_buf >> valueOf (TDiv #(Wd_Data, 8)));

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_wr_data", cur_cycle);
	 $display ("    beat %0d/%0d", rg_wr_beat, rg_awlen);
	 $display ("    ", fshow (wr_data));
      end
   endrule: rl_wr_data

   // ----------------
   // Write responses: discard, but accumulate sticky error

   rule rl_wr_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      Bool err = False;
      if (rg_wr_rsps_pending == 0) begin
	 rg_wr_error <= True;

	 $display ("%0d: %m.rl_wr_rsp: ERROR write-response when not expecting any",
		   cur_cycle);
	 $display ("    ", fshow (wr_resp));
      end
      else begin
	 rg_wr_rsps_pending <= rg_wr_rsps_pending - 1;
	 if (wr_resp.bresp != axi4_resp_okay) begin
	    rg_wr_error <= True;
	    if (verbosity >= 1) begin
	       $display ("%0d: %m.rl_wr_rsp: ERROR", cur_cycle);
	       $display ("    ", fshow (wr_resp));
	    end
	 end
	 else if (verbosity >= 1) begin
	    $display ("%0d: %m.rl_wr_rsp: pending=%0d, ",
		      cur_cycle, rg_wr_rsps_pending, fshow (wr_resp));
	 end
      end
   endrule: rl_wr_rsp

   // ================================================================
   // INTERFACE

   function Server #(Single_Req, Single_Rsp) mkServer_ifc (Integer j);
      return toGPServer (v_f_reqs [j], v_f_rsps [j]);
   endfunction

   // ----------------
   // Client-side
   interface v_mmio_server = genWith (mkServer_ifc);

   // ----------------
   // AXI-side
   interface mem_master = master_xactor.axi_side;

   // ----------------
   // Write-error from memory
   method Bool mv_write_error = rg_wr_error;
endmodule

// ================================================================

(* synthesize *)
module mkMMIO_AXI4_Adapter_2 #(parameter Bit #(3) verbosity)
                             (MMIO_AXI4_Adapter_IFC #(2));
   let ifc <- mkMMIO_AXI4_Adapter (verbosity);
   return ifc;
endmodule

// ================================================================

endpackage
