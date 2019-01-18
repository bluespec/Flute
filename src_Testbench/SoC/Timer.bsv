// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package Timer;

// ================================================================
// This package implements a slave IP with two unrelated pieces of
// RISC-V functionality:
//
// - real-time timer:
//     Two 64-bit memory-mapped registers (rg_time and rg_timecmp).
//     Delivers an external interrupt whenever rg_timecmp >= rg_time.
//     The timer is cleared when rg_timecmp is written.
//     Can be used for the RISC-V v1.10 Privilege Spec 'mtime' and
//     'mtimecmp', and provides a memory-mapped API to access them.
//
//     Offset/Size        Name        Function
//     'h_4000/8 Bytes    mtimecmp    R/W the hart0 mtimecmp  register
//     'h_BFF8/8 Bytes    mtime       R/W the mtime     register
//
// - Memory-mapped location for software interrupts.
//
//     Offset/Size        Name        Function
//     'h_0000/8 Bytes    msip        R/W Writing LSB=1 generates a software interrupt to hart0
//
// ----------------
// This slave IP can be attached to fabrics with 32b- or 64b-wide data channels.
//    (NOTE: this is the width of the fabric, which can be chosen
//      independently of the native width of a CPU master on the
//      fabric (such as RV32/RV64 for a RISC-V CPU).
// When attached to 32b-wide fabric, 64-bit locations must be
// read/written in two 32b transaction, once for the lower 32b and
// once for the upper 32b.
//
// Some of the 'truncate()'s and 'zeroExtend()'s below are no-ops but
// necessary to satisfy type-checking.
// ================================================================

export Timer_IFC (..), mkTimer;

// ================================================================
// BSV library imports

import  Vector        :: *;
import  FIFOF         :: *;
import  GetPut        :: *;
import  ClientServer  :: *;
import  ConfigReg     :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;
import ByteLane   :: *;

// ================================================================
// Project imports

import Fabric_Defs     :: *;
import AXI4_Lite_Types :: *;

// ================================================================
// Local constants and types

// Module state
typedef enum {MODULE_STATE_START, MODULE_STATE_READY } Module_State
deriving (Bits, Eq, FShow);

// ================================================================
// Interface

interface Timer_IFC;
   // Reset
   interface Server #(Bit #(0), Bit #(0))  server_reset;

   // set_addr_map should be called after this module's reset
   method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);

   // Main Fabric Reqs/Rsps
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) slave;

   // Timer interrupt
   // True/False = set/clear interrupt-pending in CPU's MTIP
   interface Get #(Bool)  get_timer_interrupt_req;

   // Software interrupt
   interface Get #(Bool)  get_sw_interrupt_req;
endinterface

// ================================================================

(* synthesize *)
module mkTimer (Timer_IFC);

   // Verbosity: 0: quiet; 1: reset; 2: timer interrupts, all reads and writes
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (0);

   Reg #(Module_State) rg_state     <- mkReg (MODULE_STATE_START);
   Reg #(Fabric_Addr)  rg_addr_base <- mkRegU;
   Reg #(Fabric_Addr)  rg_addr_lim  <- mkRegU;

   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

   // ----------------
   // Connector to fabric

   AXI4_Lite_Slave_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Lite_Slave_Xactor;

   // ----------------
   // Timer registers

   Reg #(Bit #(64)) crg_time [2]        <- mkCReg (2, 1);
   Reg #(Bit #(64)) crg_timecmp [2]     <- mkCReg (2, 0);

   Reg #(Bool) rg_mtip <- mkReg (True);

   // Timer interrupt queue
   FIFOF #(Bool) f_timer_interrupt_req <- mkFIFOF;

   // ----------------
   // Software-interrupt registers

   Reg #(Bool) rg_msip <- mkRegU;

   // Software interrupt queue
   FIFOF #(Bool) f_sw_interrupt_req <- mkFIFOF;

   // ================================================================
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Reset

   // ns: 06/12/17 -- GDB reset bug fix
   // The explicit condition was preventing the Timer from being reset by GDB
   // after the initial hardware reset. This meant that on issuing a reset
   // command from GDB, control was never returned by hardware. The explicit
   // condition is not necessary as on a GDB reset, it's okay if the Timer
   // returns to its reset state irrespective of its current state
   // rule rl_reset (rg_state == MODULE_STATE_START);
   rule rl_reset;
      f_reset_reqs.deq;
      slave_xactor.reset;
      f_timer_interrupt_req.clear;
      f_sw_interrupt_req.clear;

      rg_state            <= MODULE_STATE_READY;
      crg_time [1]        <= 1;
      crg_timecmp [1]     <= 0;
      rg_mtip             <= True;
      rg_msip             <= False;

      f_reset_rsps.enq (?);

      if (cfg_verbosity != 0)
	 $display ("%0d: Timer.rl_reset", cur_cycle);
   endrule

   // ----------------------------------------------------------------
   // Keep time and generate interrupt

   // Increment time, but saturate, do not wrap-around
   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_tick_timer ((rg_state == MODULE_STATE_READY) && (crg_time [0] != '1));
      crg_time [0] <= crg_time [0] + 1;
   endrule

   // Compare and generate timer interrupt request
   Bool new_mtip = (crg_time [0] >= crg_timecmp [0]);
   rule rl_compare ((rg_state == MODULE_STATE_READY)
		    && (rg_mtip != new_mtip));
      rg_mtip <= new_mtip;
      f_timer_interrupt_req.enq (new_mtip);
      if (cfg_verbosity > 1)
	 $display ("%0d: Timer.rl_compare: new MTIP = %0d, time = %0d, timecmp = %0d",
		   cur_cycle, new_mtip, crg_time [0], crg_timecmp [0]);
   endrule

   // ----------------------------------------------------------------
   // Handle fabric read requests

   rule rl_process_rd_req (rg_state == MODULE_STATE_READY);
      let rda <- pop_o (slave_xactor.o_rd_addr);
      if (cfg_verbosity > 1) begin
	 $display ("%0d: Timer.rl_process_rd_req: rg_mtip = %0d", cur_cycle, rg_mtip);
	 $display ("    ", fshow (rda));
      end

      let byte_addr = rda.araddr - rg_addr_base;

      Bit #(Wd_Data) rdata  = 0;
      AXI4_Lite_Resp rresp = AXI4_LITE_OKAY;

      case (byte_addr)
	 'h_0000: rdata = zeroExtend (rg_msip ? 1'b1 : 1'b0);
	 'h_4000: rdata = truncate (crg_timecmp [0]);    // truncates for 32b fabrics
	 'h_BFF8: rdata = truncate (crg_time [0]);       // truncates for 32b fabrics

	 // The following ALIGN4B reads are only needed for 32b fabrics
	 'h_0004: rdata = 0;
	 'h_4004: rdata = zeroExtend (crg_timecmp [0] [63:32]);    // extends for 64b fabrics
	 'h_BFFC: rdata = zeroExtend (crg_time    [0] [63:32]);    // extends for 64b fabrics
	 default: begin
		     rresp = AXI4_LITE_SLVERR;

		     $display ("%0d: ERROR: Timer.rl_process_rd_req: unrecognized addr", cur_cycle);
		     $display ("            ", fshow (rda));
		  end
      endcase

      let rdr = AXI4_Lite_Rd_Data {rresp: rresp, rdata: rdata, ruser: rda.aruser};
      slave_xactor.i_rd_data.enq (rdr);

      if (cfg_verbosity > 1) begin
	 $display ("    <= ", fshow (rdr));
      end
   endrule

   // ----------------------------------------------------------------
   // Handle fabric write requests

   rule rl_process_wr_req (rg_state == MODULE_STATE_READY);
      let wra <- pop_o (slave_xactor.o_wr_addr);
      let wrd <- pop_o (slave_xactor.o_wr_data);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: Timer.rl_process_wr_req: rg_mtip = %0d", cur_cycle, rg_mtip);
	 $display ("    ", fshow (wra));
	 $display ("    ", fshow (wrd));
      end

      let byte_addr = wra.awaddr - rg_addr_base;

      AXI4_Lite_Resp bresp = AXI4_LITE_OKAY;

      case (byte_addr)
	 'h_0000: begin
	             Bool new_msip = (wrd.wdata [0] == 1'b1);
		     if (rg_msip != new_msip) begin
			rg_msip <= new_msip;
			f_sw_interrupt_req.enq (new_msip);
			if (cfg_verbosity > 1)
			   $display ("    new MSIP = %0d", new_msip);
		     end
		  end
	 'h_4000: begin
		     Bit #(64) old_timecmp = crg_timecmp [1];
		     Bit #(64) new_timecmp = fn_update_strobed_bytes (old_timecmp,
								      zeroExtend (wrd.wdata),
								      zeroExtend (wrd.wstrb));
		     crg_timecmp [1] <= new_timecmp;

		     if (cfg_verbosity > 1) begin
			$display ("    Writing MTIMECMP");
			$display ("        old MTIMECMP         = 0x%0h", old_timecmp);
			$display ("        new MTIMECMP         = 0x%0h", new_timecmp);
			$display ("        cur MTIME            = 0x%0h", crg_time [1]);
			$display ("        new MTIMECMP - MTIME = 0x%0h", new_timecmp - crg_time [1]);
		     end
		  end
	 'h_BFF8: begin
		     Bit #(64) old_time = crg_time [1];
		     Bit #(64) new_time = fn_update_strobed_bytes (old_time,
								   zeroExtend (wrd.wdata),
								   zeroExtend (wrd.wstrb));
		     crg_time [1] <= new_time;

		     if (cfg_verbosity > 1) begin
			$display ("    Writing MTIME");
			$display ("        old MTIME = 0x%0h", old_time);
			$display ("        new MTIME = 0x%0h", new_time);
		     end
		  end

	 // The following ALIGN4B writes are only needed for 32b fabrics
	 'h_0004: noAction;
	 'h_4004: begin
		     Bit #(64) old_timecmp = crg_timecmp [1];
		     Bit #(64) new_timecmp = fn_update_strobed_bytes (old_timecmp,
								      { wrd.wdata [31:0], 32'h0 },
								      { wrd.wstrb [3:0], 4'h0 });
		     crg_timecmp [1] <= new_timecmp;

		     if (cfg_verbosity > 1) begin
			$display ("    Writing MTIMECMP");
			$display ("        old MTIMECMP         = 0x%0h", old_timecmp);
			$display ("        new MTIMECMP         = 0x%0h", new_timecmp);
			$display ("        cur MTIME            = 0x%0h", crg_time [1]);
			$display ("        new MTIMECMP - MTIME = 0x%0h", new_timecmp - crg_time [1]);
		     end
		  end
	 'h_BFFC: begin
		     Bit #(64) old_time = crg_time [1];
		     Bit #(64) new_time = fn_update_strobed_bytes (old_time,
								   { wrd.wdata [31:0], 32'h0 },
								   { wrd.wstrb [3:0], 4'h0 });
		     crg_time [1] <= new_time;

		     if (cfg_verbosity > 1) begin
			$display ("    Writing MTIME");
			$display ("        old MTIME = 0x%0h", old_time);
			$display ("        new MTIME = 0x%0h", new_time);
		     end
		  end

	 default: begin
		     $display ("%0d: ERROR: Timer.rl_process_wr_req: unrecognized addr", cur_cycle);
		     $display ("            ", fshow (wra));
		     $display ("            ", fshow (wrd));
		     bresp  = AXI4_LITE_SLVERR;
		  end
      endcase

      let wrr = AXI4_Lite_Wr_Resp {bresp: bresp, buser: wra.awuser};
			 
      slave_xactor.i_wr_resp.enq (wrr);

      if (cfg_verbosity > 1) begin
	 $display ("    <= ", fshow (wrr));
      end
   endrule

   // ================================================================
   // INTERFACE

   // Reset
   interface  server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // set_addr_map should be called after this module's reset
   method Action  set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);
      if (addr_base [1:0] != 0)
	 $display ("%0d: WARNING: Timer.set_addr_map: addr_base 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_base);

      if (addr_lim [1:0] != 0)
	 $display ("%0d: WARNING: Timer.set_addr_map: addr_lim 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_lim);

      rg_addr_base <= addr_base;
      rg_addr_lim  <= addr_lim;
   endmethod

   // Main Fabric Reqs/Rsps
   interface  slave = slave_xactor.axi_side;

   // External interrupt
   interface Get get_timer_interrupt_req;
     method ActionValue#(Bool) get();
       let x <- toGet (f_timer_interrupt_req).get;
       if (cfg_verbosity > 1)
          $display ("%0d: Timer: get_timer_interrupt_req: %x", cur_cycle, x);
       return x;
     endmethod
   endinterface

   // Software interrupt
   interface Get get_sw_interrupt_req;
     method ActionValue#(Bool) get();
       let x <- toGet (f_sw_interrupt_req).get;
       if (cfg_verbosity > 1)
          $display ("%0d: Timer: get_sw_interrupt_req: %x", cur_cycle, x);
       return x;
     endmethod
   endinterface
endmodule

// ================================================================

endpackage
