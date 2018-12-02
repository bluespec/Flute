// Copyright (c) 2016-2018 Bluespec, Inc. All Rights Reserved

package CPU_Stage3;

// ================================================================
// This is Stage 3 of the "Flute" CPU.
// It is the WB ("Write Back") stage:
// - Writes back a GPR register value (if the instr has an Rd)
// - Updates CSR INSTRET
//     Note: this instr cannot be a CSRRx updating INSTRET, since
//           CSRRx is done completely in Stage1.


// Note: $displays are indented by (stage num x 4) spaces.
// for traditional pipeline display
//     IF
//         DM
//             WB
// i.e., 12 spaces for this stage.

// ================================================================
// Exports

export
CPU_Stage3_IFC (..),
mkCPU_Stage3;

// ================================================================
// BSV library imports

import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle :: *;

// ================================================================
// Project imports

import ISA_Decls   :: *;
import GPR_RegFile :: *;
import CSR_RegFile :: *;
import CPU_Globals :: *;

// ================================================================
// Interface

interface CPU_Stage3_IFC;
   // ---- Reset
   interface Server #(Token, Token) server_reset;

   // ---- Output
   (* always_ready *)
   method Output_Stage3  out;

   (* always_ready *)
   method Action deq;

   // ---- Input
   (* always_ready *)
   method Action enq (Data_Stage2_to_Stage3 x);

   (* always_ready *)
   method Action set_full (Bool full);

   // ---- Debugging
   method Action show_state;
endinterface

// ================================================================
// Module

module mkCPU_Stage3 #(Bit #(4)         verbosity,
		      GPR_RegFile_IFC  gpr_regfile,
		      CSR_RegFile_IFC  csr_regfile)
                    (CPU_Stage3_IFC);

   FIFOF #(Token) f_reset_reqs <- mkFIFOF;
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   Reg #(Bool)                  rg_full   <- mkReg (False);
   Reg #(Data_Stage2_to_Stage3) rg_stage3 <- mkRegU;    // From Stage 2

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset;
      f_reset_reqs.deq;
      rg_full <= False;
      f_reset_rsps.enq (?);
   endrule

   // ----------------
   // Combinational output function

   function Output_Stage3 fv_out;
      return Output_Stage3 {ostatus: (rg_full ? OSTATUS_PIPE : OSTATUS_EMPTY),
			    bypass:  Bypass {bypass_state: ((rg_full && rg_stage3.rd_valid)
							    ? BYPASS_RD_RDVAL
							    : BYPASS_RD_NONE),
					     rd:           rg_stage3.rd,
					     rd_val:       rg_stage3.rd_val }};
   endfunction

   // ----------------
   // Actions on 'deq': writeback Rd and update CSR INSTRET

   function Action fa_deq;
      action
	 // Writeback Rd if valid
	 if (rg_stage3.rd_valid) begin
	    gpr_regfile.write_rd (rg_stage3.rd, rg_stage3.rd_val);
	    if (verbosity > 1)
	       $display ("    S3.fa_deq: write Rd 0x%0h, rd_val 0x%0h",
			 rg_stage3.rd, rg_stage3.rd_val);
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // INTERFACE

   // ---- Reset
   interface server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // ---- Output

   method Output_Stage3  out;
      return fv_out;
   endmethod

   method Action deq;
      fa_deq;
   endmethod

   // ---- Input
   method Action enq (Data_Stage2_to_Stage3 x);
      rg_stage3 <= x;

      if (verbosity > 1)
	 $display ("    S3.enq: ", fshow (x));
   endmethod

   method Action set_full (Bool full);
      rg_full <= full;
   endmethod

   // ---- Debugging
   method Action show_state;
      if (rg_full)
	 $display ("    CPU_Stage3 state: ", fshow (rg_stage3));
      else
	 $display ("    CPU_Stage3 state: empty");
   endmethod
endmodule

// ================================================================

endpackage
