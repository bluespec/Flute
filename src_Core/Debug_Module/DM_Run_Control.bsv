// Copyright (c) 2017-2019 Bluespec, Inc. All Rights Reserved.

package DM_Run_Control;

// ================================================================
// This package implements the 'Run Control' part of the RISC-V Debug
// Module, i.e., reset platform, reset hart, run/halt hart

// ================================================================
// BSV library imports

import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;
import DM_Common :: *;

// ================================================================
// Interface

interface DM_Run_Control_IFC;
   method Bool   dmactive;
   method Action reset;

   // ----------------
   // DMI facing GDB/host
   method ActionValue #(DM_Word) av_read  (DM_Addr dm_addr);
   method Action  write (DM_Addr dm_addr, DM_Word dm_word);

   // ----------------
   // Facing a hart: reset and run-control
   interface Get #(Token)         hart0_get_reset_req;
   interface Client #(Bool, Bool) hart0_client_run_halt;
   interface Get #(Bit #(4))      hart0_get_other_req;

   // ----------------
   // Facing Platform: Non-Debug-Module Reset (reset all except DM)
   interface Get #(Token) get_ndm_reset_req;
endinterface

// ================================================================

(* synthesize *)
module mkDM_Run_Control (DM_Run_Control_IFC);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   // ----------------------------------------------------------------
   // NDM Reset

   FIFOF #(Token) f_ndm_reset_reqs <- mkFIFOF;

   // ----------------------------------------------------------------
   // Hart0 run control

   Reg #(Bool) rg_hart0_running <- mkRegU;

   // Reset requests to hart
   FIFOF #(Token) f_hart0_reset_reqs <- mkFIFOF;

   // Run/halt requests to hart and responses
   FIFOF #(Bool)  f_hart0_run_halt_reqs   <- mkFIFOF;
   FIFOF #(Bool)  f_hart0_run_halt_rsps   <- mkFIFOF;

   // Non-standard requests to hart and responses
   // Currently only verbosity
   FIFOF #(Bit #(4)) f_hart0_other_reqs <- mkFIFOF;

   // ----------------------------------------------------------------

   Bit #(32) haltregion0 = { 31'h0, pack (! rg_hart0_running) };
   Bit #(32) haltsum     = { 31'h0, pack (! rg_hart0_running) };

   // ----------------------------------------------------------------
   // rg_dmstatus
   // Since we currently support only 1 hart,
   //     'anyXX' = 'allXX'
   //     'allrunning' = NOT 'allhalted'

   Reg#(Bool) rg_dmstatus_allresumeack <- mkRegU;

   Bool dmstatus_allresumeack   = rg_dmstatus_allresumeack;
   Bool dmstatus_anyresumeack   = rg_dmstatus_allresumeack;

   Bool dmstatus_allnonexistent = False;
   Bool dmstatus_anynonexistent = dmstatus_allnonexistent;

   Bool dmstatus_allunavail     = False;
   Bool dmstatus_anyunavail     = dmstatus_allunavail;

   Bool dmstatus_allrunning     = rg_hart0_running;
   Bool dmstatus_anyrunning     = dmstatus_allrunning;

   Bool dmstatus_allhalted      = (! rg_hart0_running);
   Bool dmstatus_anyhalted      = dmstatus_allhalted;

   DM_Word virt_rg_dmstatus = {14'b0,
			       pack (dmstatus_allresumeack),
			       pack (dmstatus_anyresumeack),
			       pack (dmstatus_allnonexistent),
			       pack (dmstatus_anynonexistent),
			       pack (dmstatus_allunavail),
			       pack (dmstatus_anyunavail),
			       pack (dmstatus_allrunning),
			       pack (dmstatus_anyrunning),
			       pack (dmstatus_allhalted),
			       pack (dmstatus_anyhalted),
			       pack (True),                     // authenticated
			       pack (False),                    // authbusy
			       1'b0,
			       pack (False),                    // devtreevalid
			       4'h2};                           // version

   // ----------------------------------------------------------------
   // rg_dmcontrol

   Reg #(Bool)  rg_dmcontrol_haltreq   <- mkRegU;
   // resumereq is a W1 field, no need for a register
   Reg #(Bool)  rg_dmcontrol_hartreset <- mkRegU;
   Reg #(Bool)  rg_dmcontrol_ndmreset  <- mkRegU;
   Reg #(Bool)  rg_dmcontrol_dmactive  <- mkReg (False);

   DM_Word virt_rg_dmcontrol = {2'b0,  // haltreq, resumereq (w-o)
				pack (rg_dmcontrol_hartreset),
				2'b0,
				pack (False),    // hasel
				10'b0,           // hartsel
				14'b0,
				pack (rg_dmcontrol_ndmreset),
				pack (rg_dmcontrol_dmactive)};

   function Action fa_rg_dmcontrol_write (DM_Word dm_word);
      action
	 let haltreq   = fn_dmcontrol_haltreq   (dm_word);
	 let resumereq = fn_dmcontrol_resumereq (dm_word);
	 let hartreset = fn_dmcontrol_hartreset (dm_word);
	 let hasel     = fn_dmcontrol_hasel     (dm_word);
	 let hartsel   = fn_dmcontrol_hartsel   (dm_word);
	 let ndmreset  = fn_dmcontrol_ndmreset  (dm_word);
	 let dmactive  = fn_dmcontrol_dmactive  (dm_word);

	 rg_dmcontrol_haltreq   <= haltreq;
	 rg_dmcontrol_hartreset <= hartreset;
	 rg_dmcontrol_ndmreset  <= ndmreset;
	 rg_dmcontrol_dmactive  <= dmactive;

	 // Debug Module reset
	 if (! dmactive) begin
	    // Reset the DM module itself
	    $display ("DM_Run_Control.write: dmcontrol 0x%08h (dmactive=0): resetting Debug Module",
		      dm_word);

	    // Error-checking
	    if (ndmreset) begin
	       $display ("DM_Run_Control.write: WARNING: in word written to dmcontrol (0x%08h):",
			 dm_word);
	       $display ("    [1] (ndmreset) and [0] (dmactive) both asserted");
	       $display ("    dmactive has priority; ignoring ndmreset");
	    end
	    if (hartreset) begin
	       $display ("DM_Run_Control.write: WARNING: in word written to dmcontrol (0x%08h):",
			 dm_word);
	       $display ("    [29] (hartreset) and [0] (dmactive) both asserted");
	       $display ("    dmactive has priority; ignoring hartreset");
	    end

	    // No action here; other rules will fire (see method dmactive, Debug_Module.rl_reset)
	    noAction;
	 end

	 // Platform reset (non-Debug Module)
	 else if (ndmreset) begin
	    $display ("DM_Run_Control.write: dmcontrol 0x%08h: ndmreset=1: resetting platform",
		      dm_word);
	    f_ndm_reset_reqs.enq (?);
	    rg_hart0_running <= True;    // Must be same as run/halt state of CPU after hart_reset!

	    // Error-checking
	    if (hartreset) begin
	       $display ("DM_Run_Control.write: WARNING: in word written to dmcontrol (0x%08h):",
			 dm_word);
	       $display ("    Both ndmreset (bit 1) and hartreset (bit 29) are asserted");
	       $display ("    ndmreset has priority; ignoring hartreset");
	    end
	 end
	 else begin
	    // Deassert platform reset
	    if ((verbosity != 0) && rg_dmcontrol_ndmreset)
	       $display ("DM_Run_Control.write: dmcontrol 0x%08h: clearing ndmreset", dm_word);

	    // Hart reset
	    if (hartreset) begin
	       if (verbosity != 0)
		  $display ("DM_Run_Control.write: dmcontrol 0x%08h: hartreset=1: resetting hart",
			    dm_word);
	       f_hart0_reset_reqs.enq (?);
	       rg_hart0_running <= True;    // Must be same as run/halt state of CPU after hart_reset!
	    end
	    else begin
	       // Deassert hart reset
	       if ((verbosity != 0) && rg_dmcontrol_hartreset)
		  $display ("DM_Run_Control.write: dmcontrol 0x%08h: clearing hartreset", dm_word);

	       if (hasel)
		  $display ("DM_Run_Control.write: ERROR: dmcontrol 0x%08h: 'hasel' is not supported",
			    dm_word);

	       if (hartsel != 0)
		  $display ("DM_Run_Control.write: ERROR: dmcontrol 0x%08h: hartsel 0x%0h not supported",
			    dm_word, hartsel);

	       if (haltreq && resumereq) begin
		  $display ("DM_Run_Control.write: ERROR: dmcontrol 0x%08h: haltreq=1 and resumereq=1",
			    dm_word);
		  $display ("    This behavior is 'undefined' in the spec; ignoring");
	       end
	       // Resume hart(s) if not running
	       else if (resumereq && (! rg_hart0_running)) begin
		  f_hart0_run_halt_reqs.enq (True);
		  rg_dmstatus_allresumeack <= False;
		  $display ("DM_Run_Control.write: hart0 resume request");
	       end
	       // Halt hart(s)
	       else if (haltreq && rg_hart0_running) begin
		  f_hart0_run_halt_reqs.enq (False);
		  $display ("DM_Run_Control.write: hart0 halt request");
	       end
	    end
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Unimplemented

   // dm_addr_hartinfo
   // dm_addr_hawindowsel
   // dm_addr_hawindow
   // dm_addr_devtreeaddr0..3
   // dm_addr_authdata
   // dm_addr_haltregion1..31

   // ----------------------------------------------------------------
   // rg_verbosity: non-standard

   Reg #(Bit #(4)) rg_verbosity <- mkRegU;

   // ----------------------------------------------------------------

   rule rl_hart0_run_rsp;
      let x = f_hart0_run_halt_rsps.first;
      f_hart0_run_halt_rsps.deq;

      rg_hart0_running <= x;
      if (x) begin
	 rg_dmstatus_allresumeack <= True;
	 $display ("DM_Run_Control: hart0 running");
      end
      else begin
	 $display ("DM_Run_Control: hart0 halted");
      end
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Bool dmactive;
      return rg_dmcontrol_dmactive;
   endmethod

   method Action reset;
      f_ndm_reset_reqs.clear;

      rg_hart0_running <= True;    // Must be same as run/halt state of CPU after hart_reset!
      f_hart0_reset_reqs.clear;
      f_hart0_run_halt_reqs.clear;
      f_hart0_run_halt_rsps.clear;

      rg_dmcontrol_haltreq   <= False;
      rg_dmcontrol_hartreset <= False;
      rg_dmcontrol_ndmreset  <= False;
      rg_dmcontrol_dmactive  <= True;    // DM module is now active

      rg_dmstatus_allresumeack <= False;

      rg_verbosity <= 0;

      if (verbosity != 0)
	 $display ("DM_Run_Control: reset");
   endmethod

   // ----------------
   // DMI facing GDB/host

   method ActionValue #(DM_Word) av_read (DM_Addr dm_addr);
      actionvalue
	 DM_Word dm_word = case (dm_addr)
			      dm_addr_dmcontrol:   virt_rg_dmcontrol;
			      dm_addr_dmstatus:    virt_rg_dmstatus;
			      dm_addr_haltsum:     haltsum;
			      dm_addr_haltregion0: haltregion0;
			      dm_addr_verbosity:   extend (rg_verbosity);
			   endcase;

	 if (verbosity != 0)
	    $display ("DM_Run_Control.av_read: [", fshow_dm_addr (dm_addr), "] => 0x%08h", dm_word);

	 return dm_word;
      endactionvalue
   endmethod

   method Action write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 if (verbosity != 0)
	    $display ("DM_Run_Control.write: [", fshow_dm_addr (dm_addr), "] <= 0x%08h", dm_word);

	 case (dm_addr)
	    dm_addr_dmcontrol: fa_rg_dmcontrol_write (dm_word);
	    dm_addr_verbosity: begin
				  rg_verbosity <= truncate (dm_word);
				  f_hart0_other_reqs.enq (truncate (dm_word));
			       end
	    default: noAction;
	 endcase
      endaction
   endmethod

   // ----------------
   // Facing Hart: Reset, Run-control, etc.
   interface Get    hart0_get_reset_req   = toGet (f_hart0_reset_reqs);
   interface Client hart0_client_run_halt = toGPClient (f_hart0_run_halt_reqs, f_hart0_run_halt_rsps);
   interface Get    hart0_get_other_req   = toGet (f_hart0_other_reqs);

   // ----------------
   // Facing Platform: Non-Debug-Module Reset (reset all except DM)
   interface Get get_ndm_reset_req = toGet (f_ndm_reset_reqs);
endmodule

// ================================================================

endpackage
