// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package Near_Mem_IO;

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

export  Near_Mem_IO_Req (..),
        Near_Mem_IO_Rsp (..),
        Near_Mem_IO_IFC (..),
        mkNear_Mem_IO;

// ================================================================
// BSV library imports

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

// None

// ================================================================
// Local constants and types

// Module state
typedef enum {MODULE_STATE_START, MODULE_STATE_READY } Module_State
deriving (Bits, Eq, FShow);

// ================================================================
// Near_Mem_IO requests and responses

typedef struct {
   Bool       read_not_write;
   Bit #(64)  addr;
   Bit #(64)  wdata;    // write-data (not relevant for reads)
   Bit #(8)   wstrb;    // byte-enable strobe (for write-data)
   } Near_Mem_IO_Req
deriving (Bits, FShow);

typedef struct {
   Bool       read_not_write;
   Bool       ok;
   Bit #(64)  rdata;
   } Near_Mem_IO_Rsp
deriving (Bits, FShow);

// ================================================================
// Interface

interface Near_Mem_IO_IFC;
   // Reset
   interface Server #(Bit #(0), Bit #(0))  server_reset;

   // set_addr_map should be called after this module's reset
   method Action set_addr_map (Bit #(64) addr_base, Bit #(64) addr_lim);

   // Memory-mapped Reqs/Rsps
   interface Server #(Near_Mem_IO_Req, Near_Mem_IO_Rsp)  server;

   // Timer interrupt
   // True/False = set/clear interrupt-pending in CPU's MTIP
   interface Get #(Bool)  get_timer_interrupt_req;

   // Software interrupt
   interface Get #(Bool)  get_sw_interrupt_req;
endinterface

// ================================================================

(* synthesize *)
module mkNear_Mem_IO (Near_Mem_IO_IFC);

   // Verbosity: 0: quiet; 1: reset; 2: timer interrupts, all reads and writes
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (1);

   Reg #(Module_State) rg_state     <- mkReg (MODULE_STATE_START);

   Reg #(Bit #(64)) rg_addr_base <- mkRegU;
   Reg #(Bit #(64)) rg_addr_lim  <- mkRegU;

   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

   FIFOF #(Near_Mem_IO_Req) f_reqs <- mkFIFOF;
   FIFOF #(Near_Mem_IO_Rsp) f_rsps <- mkFIFOF;

   // ----------------
   // Timer registers

   Reg #(Bit #(64)) crg_time [2]    <- mkCReg (2, 1);
   Reg #(Bit #(64)) crg_timecmp [2] <- mkCReg (2, 0);

   Reg #(Bool) rg_mtip <- mkReg (True);

   // Timer-interrupt queue
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

   rule rl_reset (rg_state == MODULE_STATE_START);
      f_reset_reqs.deq;

      f_reqs.clear;
      f_rsps.clear;
      f_timer_interrupt_req.clear;
      f_sw_interrupt_req.clear;

      rg_state        <= MODULE_STATE_READY;
      crg_time [1]    <= 1;
      crg_timecmp [1] <= 0;
      rg_mtip         <= True;
      rg_msip         <= False;

      f_reset_rsps.enq (?);

      if (cfg_verbosity != 0)
	 $display ("%0d: Near_Mem_IO.rl_reset", cur_cycle);
   endrule

   rule rl_soft_reset (f_reset_reqs.notEmpty);
      rg_state <= MODULE_STATE_START;
   endrule

   // ----------------------------------------------------------------
   // Keep time and generate interrupt

   // Increment time, but saturate, do not wrap-around
   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_tick_timer (   (rg_state == MODULE_STATE_READY)
		       && (crg_time [0] != '1)
		       && (! f_reset_reqs.notEmpty));

      crg_time [0] <= crg_time [0] + 1;
   endrule

   // Compare and generate timer interrupt request

   Bool new_mtip = (crg_time [0] >= crg_timecmp [0]);

   rule rl_compare ((rg_state == MODULE_STATE_READY)
		    && (rg_mtip != new_mtip)
		    && (! f_reset_reqs.notEmpty));

      rg_mtip <= new_mtip;
      f_timer_interrupt_req.enq (new_mtip);
      if (cfg_verbosity > 1)
	 $display ("%0d: Near_Mem_IO.rl_compare: new MTIP = %0d, time = %0d, timecmp = %0d",
		   cur_cycle, new_mtip, crg_time [0], crg_timecmp [0]);
   endrule

   // ----------------------------------------------------------------
   // Handle 'memory'-read requests

   rule rl_process_rd_req (   (rg_state == MODULE_STATE_READY)
			   && (! f_reset_reqs.notEmpty)
			   && f_reqs.first.read_not_write);
      let req <- pop (f_reqs);
      if (cfg_verbosity > 1) begin
	 $display ("%0d: Near_Mem_IO.rl_process_rd_req: rg_mtip = %0d", cur_cycle, rg_mtip);
	 $display ("    ", fshow (req));
      end

      let byte_addr = req.addr - rg_addr_base;

      Bit #(64) rdata = 0;
      Bool      rok   = True;

      case (byte_addr)
	 'h_0000: rdata = zeroExtend (rg_msip ? 1'b1 : 1'b0);
	 'h_4000: rdata = truncate (crg_timecmp [0]);    // truncates for 32b fabrics
	 'h_BFF8: rdata = truncate (crg_time [0]);       // truncates for 32b fabrics

	 // The following ALIGN4B reads are only needed for 32b fabrics
	 'h_0004: rdata = 0;
	 'h_4004: rdata = zeroExtend (crg_timecmp [0] [63:32]);    // extends for 64b fabrics
	 'h_BFFC: rdata = zeroExtend (crg_time    [0] [63:32]);    // extends for 64b fabrics
	 default: begin
		     rok = False;

		     $display ("%0d: ERROR: Near_Mem_IO.rl_process_rd_req: unrecognized addr", cur_cycle);
		     $display ("            ", fshow (req));
		  end
      endcase

      let rsp = Near_Mem_IO_Rsp {read_not_write: True, ok: rok, rdata: rdata};
      f_rsps.enq (rsp);

      if (cfg_verbosity > 1) begin
	 $display ("    <= ", fshow (rsp));
      end
   endrule

   // ----------------------------------------------------------------
   // Handle 'memory'-write requests

   rule rl_process_wr_req (   (rg_state == MODULE_STATE_READY)
			   && (! f_reset_reqs.notEmpty)
			   && (! f_reqs.first.read_not_write));
      let req <- pop (f_reqs);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: Near_Mem_IO.rl_process_wr_req: rg_mtip = %0d", cur_cycle, rg_mtip);
	 $display ("    ", fshow (req));
      end

      let byte_addr = req.addr - rg_addr_base;
      let wok = True;

      case (byte_addr)
	 'h_0000: begin
	             Bool new_msip = (req.wdata [0] == 1'b1);
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
								      zeroExtend (req.wdata),
								      zeroExtend (req.wstrb));
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
								   zeroExtend (req.wdata),
								   zeroExtend (req.wstrb));
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
								      { req.wdata [31:0], 32'h0 },
								      { req.wstrb [3:0], 4'h0 });
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
								   { req.wdata [31:0], 32'h0 },
								   { req.wstrb [3:0], 4'h0 });
		     crg_time [1] <= new_time;

		     if (cfg_verbosity > 1) begin
			$display ("    Writing MTIME");
			$display ("        old MTIME = 0x%0h", old_time);
			$display ("        new MTIME = 0x%0h", new_time);
		     end
		  end

	 default: begin
		     $display ("%0d: ERROR: Near_Mem_IO.rl_process_wr_req: unrecognized addr", cur_cycle);
		     $display ("            ", fshow (req));
		     wok = False;
		  end
      endcase

      let rsp = Near_Mem_IO_Rsp {read_not_write: False, ok: wok, rdata: ?};
      f_rsps.enq (rsp);
			 
      if (cfg_verbosity > 1) begin
	 $display ("    <= ", fshow (rsp));
      end
   endrule

   // ================================================================
   // INTERFACE

   // Reset
   interface  server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // set_addr_map should be called after this module's reset
   method Action  set_addr_map (Bit #(64) addr_base, Bit #(64) addr_lim);
      if (addr_base [1:0] != 0)
	 $display ("%0d: WARNING: Near_Mem_IO.set_addr_map: addr_base 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_base);

      if (addr_lim [1:0] != 0)
	 $display ("%0d: WARNING: Near_Mem_IO.set_addr_map: addr_lim 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_lim);

      rg_addr_base <= addr_base;
      rg_addr_lim  <= addr_lim;
      $display ("%0d: Near_Mem_IO.set_addr_map: addr_base 0x%0h addr_lim 0x%0h",
		cur_cycle, addr_base, addr_lim);
   endmethod

   // Memory-mapped Reqs/Rsps
   interface  server = toGPServer (f_reqs, f_rsps);

   // Timer interrupt
   interface Get get_timer_interrupt_req;
     method ActionValue#(Bool) get();
       let x <- toGet (f_timer_interrupt_req).get;
       if (cfg_verbosity > 1)
          $display ("%0d: Near_Mem_IO: get_timer_interrupt_req: %x", cur_cycle, x);
       return x;
     endmethod
   endinterface

   // Software interrupt
   interface Get get_sw_interrupt_req;
     method ActionValue#(Bool) get();
       let x <- toGet (f_sw_interrupt_req).get;
       if (cfg_verbosity > 1)
          $display ("%0d: Near_Mem_IO: get_sw_interrupt_req: %x", cur_cycle, x);
       return x;
     endmethod
   endinterface
endmodule

// ================================================================

endpackage
