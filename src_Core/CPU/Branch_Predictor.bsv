// Copyright (c) 2013-2021 Bluespec, Inc. All Rights Reserved

package Branch_Predictor;

// ================================================================
// Branch Predictor for RISC-V CPU
//

// This is a "null" branch predictor, for purposes of comparision with
// a real branch predictor.
// It just predicts PC+4 (or PC+2 for 'C' instrs).

// ================================================================
// Exports

export Branch_Predictor_IFC (..), mkBranch_Predictor;

// ================================================================
// BSV library imports

import FIFOF     :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;

// ================================================================
// Project imports

import ISA_Decls   :: *;
import CPU_Globals :: *;

// ================================================================

interface Branch_Predictor_IFC;
   method Action  reset;

   // ----------------
   // Request prediction for given pc; available on 'predict_rsp' on next cycle

   method Action  predict_req (WordXL pc);

   // ----------------
   // Response for 'predict_req' from an earlier cycle.
   // Args describe current instruction just now fetched in Fetch stage,
   // and are used to choose RAS actions if any, and size of
   // fall-through PC if no prediction.

   (* always_ready *)
   method WordXL  predict_rsp (Bool is_i32_not_i16, Instr instr);

   // ----------------
   // Train BTB and RAS.
   // First 3 args are from current fetch, to train RAS.
   // cf_info arg is from Exec stage (downpipe), from an earlier (older) instruction.

   method Action bp_train (WordXL   pc,
			   Bool     is_i32_not_i16,
			   Instr    instr,
			   CF_Info  cf_info);
endinterface

// ================================================================

(* synthesize *)
module mkBranch_Predictor (Branch_Predictor_IFC);

   // This reg holds the PC being predicted (currently probing the btb)
   Reg #(WordXL)  rg_pc <- mkRegU;

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      noAction;
   endmethod

   // ----------------
   // Request prediction for given pc; available on 'predict_rsp' on next cycle

   method Action  predict_req (WordXL pc);
      rg_pc <= pc;
   endmethod

   // ----------------
   // Response for 'predict_req' from an earlier cycle.
   // Args describe current instruction just now fetched in Fetch stage,
   // and are used to choose RAS actions if any, and size of
   // fall-through PC if no prediction.

   method WordXL  predict_rsp (Bool is_i32_not_i16, Instr instr);
      let pred_pc = rg_pc + (is_i32_not_i16 ? 4 : 2);
      return pred_pc;
   endmethod

   // ----------------
   // Train BTB and RAS.
   // First 3 args are from current fetch, to train RAS.
   // cf_info arg is from Exec stage (downpipe), from an earlier (older) instruction.

   method Action bp_train (WordXL   pc,
			   Bool     is_i32_not_i16,
			   Instr    instr,
			   CF_Info  cf_info);
      noAction;
   endmethod

endmodule

// ================================================================

endpackage
