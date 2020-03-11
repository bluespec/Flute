// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved

package FPR_RegFile;

// ================================================================
// FPR (Floating Point Register) Register File

// ================================================================
// Exports

export FPR_RegFile_IFC (..), mkFPR_RegFile;

// ================================================================
// BSV library imports

import ConfigReg    :: *;
import RegFile      :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// BSV additional libs

import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;

// ================================================================

interface FPR_RegFile_IFC;
   // Reset
   interface Server #(Token, Token) server_reset;

   // FPR read
   (* always_ready *)
   method WordFL read_rs1 (RegName rs1);
   (* always_ready *)
   method WordFL read_rs1_port2 (RegName rs1);    // For debugger access only
   (* always_ready *)
   method WordFL read_rs2 (RegName rs2);
   (* always_ready *)
   method WordFL read_rs3 (RegName rs3);

   // FPR write
   (* always_ready *)
   method Action write_rd (RegName rd, WordFL rd_val);

endinterface

// ================================================================
// Major states of mkFPR_RegFile module

typedef enum { RF_RESET_START, RF_RESETTING, RF_RUNNING } RF_State
deriving (Eq, Bits, FShow);

// ================================================================

(* synthesize *)
module mkFPR_RegFile (FPR_RegFile_IFC);

   Reg #(RF_State) rg_state      <- mkReg (RF_RESET_START);

   // Reset
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   // Floating Point Registers
   // Unlike GPRs, all registers in the FPR are regular registers (no r0)
   RegFile #(RegName, WordFL) regfile <- mkRegFileFull;

   // ----------------------------------------------------------------
   // Reset.
   // This loop initializes all FPRs to 0.
   // The spec does not require this, but it's useful for debugging
   // and tandem verification

`ifdef INCLUDE_TANDEM_VERIF
   Reg #(RegName) rg_j <- mkRegU;    // reset loop index
`endif

   rule rl_reset_start (rg_state == RF_RESET_START);
      rg_state <= RF_RESETTING;
`ifdef INCLUDE_TANDEM_VERIF
      rg_j <= 1;
`endif
   endrule

   rule rl_reset_loop (rg_state == RF_RESETTING);
`ifdef INCLUDE_TANDEM_VERIF
      regfile.upd (rg_j, 0);
      rg_j <= rg_j + 1;
      if (rg_j == 31)
         rg_state <= RF_RUNNING;
`else
      rg_state <= RF_RUNNING;
`endif
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   // Reset
   interface Server server_reset;
      interface Put request;
         method Action put (Token token);
            rg_state <= RF_RESET_START;

            // This response is placed here, and not in rl_reset_loop, because
            // reset_loop can happen on power-up, where no response is expected.
            f_reset_rsps.enq (?);
         endmethod
      endinterface
      interface Get response;
         method ActionValue #(Token) get if (rg_state == RF_RUNNING);
            let token <- pop (f_reset_rsps);
            return token;
         endmethod
      endinterface
   endinterface

   // FPR read
   method WordFL read_rs1 (RegName rs1);
      return (regfile.sub (rs1));
   endmethod

   // FPR read
   method WordFL read_rs1_port2 (RegName rs1);        // For debugger access only
      return (regfile.sub (rs1));
   endmethod

   method WordFL read_rs2 (RegName rs2);
      return (regfile.sub (rs2));
   endmethod

   method WordFL read_rs3 (RegName rs3);
      return (regfile.sub (rs3));
   endmethod

   // FPR write
   method Action write_rd (RegName rd, WordFL rd_val);
      regfile.upd (rd, rd_val);
   endmethod

endmodule

// ================================================================

endpackage
