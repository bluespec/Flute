// Copyright (c) 2019 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AXI4_Mem_Model;

// ================================================================
// A memory-model to be used as a slave on an AXI4 bus.
// Only partical functionality; will be gradually improved over time.
// Current status:
//     Address and Data bus widths: 64b
//     Bursts:      'fixed' and 'incr' only
//     Size:        Full 64-bit width reads/writes only
//     Strobes:     Not yet handled
//     memory size: See 'mem_size_word64' definition below

// ================================================================
// Exports

export AXI4_Mem_Model_IFC (..);
export mkAXI4_Mem_Model;

// ================================================================
// Bluespec library imports

import RegFile      :: *;
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

import AXI4_Types :: *;

// ================================================================
// INTERFACE

interface AXI4_Mem_Model_IFC #(numeric type  wd_id,
			       numeric type  wd_addr,
			       numeric type  wd_data,
			       numeric type  wd_user);

   method Action init (Bit #(wd_addr) addr_map_base, Bit #(wd_addr) addr_map_lim);

   interface AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user) slave;

endinterface

// ================================================================
// IMPLEMENTATION

Integer mem_size_word64 = 'h100_0000;    // 16M x 64b words = 128MiB

function Bool fn_addr_ok (Bit #(64) base, Bit #(64) lim, Bit #(64) addr, AXI4_Size size);
   let aligned  = fn_addr_is_aligned (addr, size);
   let in_range = ((base <= addr) && (addr < lim));
   return (aligned && in_range);
endfunction

// ----------------

module mkAXI4_Mem_Model (AXI4_Mem_Model_IFC #(wd_id, wd_addr, wd_data, wd_user))
   provisos (NumAlias #(wd_addr, 64),
	     NumAlias #(wd_data, 64));

   // 0 = quiet; 1 = show mem transactions
   Integer verbosity = 1;

   Reg #(Bool) rg_initialized <- mkReg (False);

   Reg #(Bit #(wd_addr)) rg_addr_map_base <- mkRegU;
   Reg #(Bit #(wd_addr)) rg_addr_map_lim  <- mkRegU;

   AXI4_Slave_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user) xactor <- mkAXI4_Slave_Xactor;

   RegFile #(Bit #(wd_addr), Bit #(wd_data)) rf <- mkRegFile (0, fromInteger (mem_size_word64));

   // ================================================================
   // Read requests
   // TODO: does a bad addr return 'burst-len' err responses or just 1?

   Reg #(Bit #(8)) rg_rd_beat <- mkReg (0);

   // Recv request on RD_ADDR bus
   // Send burst responses on RD_DATA bus
   rule rl_read (rg_initialized);
      let rd_addr  = xactor.o_rd_addr.first;
      let rf_index = ((rd_addr.araddr - rg_addr_map_base) >> 3);
      if (rd_addr.arburst == axburst_incr)
	 rf_index = rf_index + zeroExtend (rg_rd_beat);
      let last = (rg_rd_beat == rd_addr.arlen);

      let addr_ok = fn_addr_ok (rg_addr_map_base, rg_addr_map_lim, rd_addr.araddr, rd_addr.arsize);

      let data = (addr_ok ? rf.sub (rf_index) : 0);

      AXI4_Rd_Data #(wd_id, wd_data, wd_user)
      rd_data = AXI4_Rd_Data {rid:   rd_addr.arid,
			      rdata: data,
			      rresp: (addr_ok ? axi4_resp_okay : axi4_resp_slverr),
			      rlast: last,
			      ruser: rd_addr.aruser};
      xactor.i_rd_data.enq (rd_data);

      if (last) begin
	 xactor.o_rd_addr.deq;
	 rg_rd_beat <= 0;
      end
      else
	 rg_rd_beat <= rg_rd_beat + 1;

      if (verbosity != 0) begin
	 $write ("%0d: %m.rl_read: ", cur_cycle);
	 $write (fshow_Rd_Addr (rd_addr));
	 $write (fshow_Rd_Data (rd_data));
	 if (addr_ok)
	    $display (" beat %0d rf_index 0x%0h", rg_rd_beat, rf_index);
	 else
	    $display (" beat 0x%0h BAD ADDR", rg_rd_beat);
      end
   endrule

   // ================================================================
   // Write requests

   Reg #(Bit #(8)) rg_wr_beat <- mkReg (0);

   // Recv request on WR_ADDR bus and burst data on WR_DATA bus,
   // send final response on WR_RESP bus
   rule rl_write (rg_initialized);
      let wr_addr = xactor.o_wr_addr.first;
      let wr_data <- pop_o (xactor.o_wr_data);
      let rf_index   = ((wr_addr.awaddr - rg_addr_map_base) >> 3);
      if (wr_addr.awburst == axburst_incr)
	 rf_index = rf_index + zeroExtend (rg_wr_beat);
      let last = (rg_wr_beat == wr_addr.awlen);

      let addr_ok = fn_addr_ok (rg_addr_map_base, rg_addr_map_lim, wr_addr.awaddr, wr_addr.awsize);

      if (addr_ok)
	 rf.upd (rf_index, wr_data.wdata);

      if (verbosity != 0) begin
	 $write ("%0d: %m.rl_write: ", cur_cycle);
	 $write (fshow_Wr_Data (wr_data));
	 $write ("  ", fshow_Wr_Addr (wr_addr));
	 if (addr_ok)
	    $display (" beat %0d rf_index %0h", rg_wr_beat, rf_index);
	 else
	    $display (" beat %0d BAD ADDR", rg_wr_beat);
      end

      if (last) begin
	 AXI4_Wr_Resp #(wd_id, wd_user) wr_resp = ?;
	 wr_resp = AXI4_Wr_Resp {bid:   wr_addr.awid,
				 bresp: (addr_ok ? axi4_resp_okay : axi4_resp_slverr),
				 buser: wr_addr.awuser};
	 xactor.i_wr_resp.enq (wr_resp);
	 xactor.o_wr_addr.deq;
	 rg_wr_beat <= 0;
	 if (verbosity != 0)
	    $display ("    ", fshow_Wr_Resp (wr_resp));
      end
      else
	 rg_wr_beat <= rg_wr_beat + 1;
   endrule

   // ================================================================
   // INTERFACE

   method Action init (Bit #(wd_addr) addr_map_base, Bit #(wd_addr) addr_map_lim);
      if (addr_map_base [2:0] != 3'b0)
	 $display ("%0d: %m.init: ERROR: unaligned addr_map_base 0x%0h", cur_cycle, addr_map_base);
      else if (addr_map_lim [2:0] != 3'b0)
	 $display ("%0d: %m.init: ERROR: unaligned addr_map_lim 0x%0h", cur_cycle, addr_map_lim);
      else if (addr_map_lim <= addr_map_base)
	 $display ("%0d: %m.init: ERROR: addr_map_base 0x%0h > addr_map_lim 0x%0h",
		   cur_cycle,
		   addr_map_base,
		   addr_map_lim);
      else if ((addr_map_lim - addr_map_base) > fromInteger (mem_size_word64 * 8))
	 $display ("%0d: %m.init: ERROR: mem size (base 0x%0h, lim 0x%0h) > max (0x%0h)",
		   cur_cycle,
		   addr_map_base,
		   addr_map_lim,
		   fromInteger (mem_size_word64 * 8));
      else begin
	 xactor.reset;
	 rg_addr_map_base <= addr_map_base;
	 rg_addr_map_lim  <= addr_map_lim;
	 rg_initialized   <= True;
	 $display ("%0d: %m.init: addr_map_base 0x%0h, addr_map_lim 0x%0h",
		   cur_cycle,
		   addr_map_base,
		   addr_map_lim);
      end
   endmethod

   interface slave = xactor.axi_side;
endmodule

// ================================================================

endpackage
