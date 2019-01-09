// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CPU_StageD;

// ================================================================
// This is Stage D ("decode") of the "Flute_V3" CPU.

// ================================================================
// Exports

export
CPU_StageD_IFC (..),
mkCPU_StageD;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import ConfigReg    :: *;

// ----------------
// BSV additional libs

import Cur_Cycle :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import CPU_Globals      :: *;
import Near_Mem_IFC     :: *;

// ================================================================
// Interface

interface CPU_StageD_IFC;
   // ---- Reset
   interface Server #(Token, Token) server_reset;

   // ---- Output
   (* always_ready *)
   method Output_StageD out;

   (* always_ready *)
   method Action deq;

   // ---- Input
   (* always_ready *)
   method Action enq (Data_StageF_to_StageD  data_stageF_to_stageD);

   (* always_ready *)
   method Action set_full (Bool full);
endinterface

// ================================================================
// Implementation module

module mkCPU_StageD #(Bit #(4)  verbosity)
                    (CPU_StageD_IFC);

   FIFOF #(Token)  f_reset_reqs <- mkFIFOF;
   FIFOF #(Token)  f_reset_rsps <- mkFIFOF;

   Wire #(Bool)  dw_redirecting <- mkDWire (False);

   Reg #(Bool)                   rg_full <- mkReg (False);
   Reg #(Data_StageF_to_StageD)  rg_data <- mkRegU;

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset;
      f_reset_reqs.deq;
      rg_full <= False;
      f_reset_rsps.enq (?);
   endrule

   // ----------------
   // Combinational output function

   function Output_StageD fv_out;
      let decoded_instr = fv_decode (rg_data.instr);

      Output_StageD  output_stageD = ?;

      // This stage is empty
      if (! rg_full) begin
	 output_stageD.ostatus = OSTATUS_EMPTY;
      end
      else begin
	 output_stageD.ostatus        = OSTATUS_PIPE;
	 output_stageD.data_to_stage1 = Data_StageD_to_Stage1 {pc:            rg_data.pc,
							       priv:          rg_data.priv,
							       epoch:         rg_data.epoch,
							       exc:           rg_data.exc,
							       exc_code:      rg_data.exc_code,
							       instr:         rg_data.instr,
							       pred_pc:       rg_data.pred_pc,
							       decoded_instr: decoded_instr};
      end

      return output_stageD;
   endfunction: fv_out

   // ================================================================
   // INTERFACE

   // ---- Reset
   interface server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // ---- Output
   method Output_StageD out;
      return fv_out;
   endmethod

   method Action deq ();
      noAction;
   endmethod

   // ---- Input
   method Action enq (Data_StageF_to_StageD  data);
      rg_data <= data;
      if (verbosity > 1)
	 $display ("    CPU_StageD.enq (Data_StageF_to_StageD)");
   endmethod

   method Action set_full (Bool full);
      rg_full <= full;
   endmethod
endmodule

// ================================================================

endpackage
