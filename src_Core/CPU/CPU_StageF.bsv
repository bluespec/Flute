// Copyright (c) 2016-2021 Bluespec, Inc. All Rights Reserved

package CPU_StageF;

// ================================================================
// This is Stage F ("fetch") of the "Flute_V3" CPU.

// ================================================================
// Exports

export
CPU_StageF_IFC (..),
mkCPU_StageF;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle :: *;

// ================================================================
// Project imports

import ISA_Decls         :: *;
import CPU_Globals       :: *;
import Near_Mem_IFC      :: *;
import Branch_Predictor  :: *;

// ================================================================
// Interface

interface CPU_StageF_IFC;
   // ---- Reset
   interface Server #(Token, Token) server_reset;

   // ---- Output
   (* always_ready *)
   method Output_StageF out;

   (* always_ready *)
   method Action deq;

   // ---- Input
   (* always_ready *)
   method Action enq (Epoch            epoch,
		      WordXL           pc,
		      Priv_Mode        priv,
		      Bit #(1)         sstatus_SUM,
		      Bit #(1)         mstatus_MXR,
		      WordXL           satp);

   (* always_ready *)
   method Action bp_train (WordXL   pc,
			   Bool     is_i32_not_i16,
			   Instr    instr,
			   CF_Info  cf_info);

   (* always_ready *)
   method Action set_full (Bool full);
endinterface

// ================================================================
// Implementation module

module mkCPU_StageF #(Bit #(4)  verbosity,
		      IMem_IFC  imem)
                    (CPU_StageF_IFC);

   FIFOF #(Token)  f_reset_reqs <- mkFIFOF;
   FIFOF #(Token)  f_reset_rsps <- mkFIFOF;

   Wire #(Bool)      dw_redirecting <- mkDWire (False);

   Reg #(Bool)       rg_full  <- mkReg (False);
   Reg #(Epoch)      rg_epoch <- mkReg (0);               // Toggles on redirections
   Reg #(Priv_Mode)  rg_priv  <- mkRegU;

   Branch_Predictor_IFC branch_predictor <- mkBranch_Predictor;

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset;
      f_reset_reqs.deq;
      rg_full  <= False;
      rg_epoch <= 0;
      f_reset_rsps.enq (?);
   endrule

   // ----------------
   // Combinational output function

   function Output_StageF fv_out;
      let pred_pc = branch_predictor.predict_rsp (imem.is_i32_not_i16, imem.instr);
      let d = Data_StageF_to_StageD {pc:              imem.pc,
				     epoch:           rg_epoch,
				     priv:            rg_priv,
				     is_i32_not_i16:  imem.is_i32_not_i16,
				     exc:             imem.exc,
				     exc_code:        imem.exc_code,
				     tval:            imem.tval,
				     instr:           imem.instr,
				     pred_pc:         pred_pc};

      let ostatus = (  (! rg_full) ? OSTATUS_EMPTY
		     : (  (! imem.valid) ? OSTATUS_BUSY
			: OSTATUS_PIPE));                    // instr or exception

      let output_stageF = Output_StageF {ostatus: ostatus, data_to_stageD: d};

      return output_stageF;
   endfunction: fv_out

   // ================================================================
   // INTERFACE

   // ---- Reset
   interface server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // ---- Output
   method Output_StageF out;
      return fv_out;
   endmethod

   method Action deq ();
      noAction;
   endmethod

   // ---- Input
   method Action enq (Epoch            epoch,
		      WordXL           pc,
		      Priv_Mode        priv,
		      Bit #(1)         sstatus_SUM,
		      Bit #(1)         mstatus_MXR,
		      WordXL           satp);
      if (verbosity > 1) begin
	 $write ("    %m.CPU_StageF.ma_enq:  pc:0x%0h  epoch:%0d  priv:%0d", pc, epoch, priv);
	 $write ("  sstatus_SUM:%0d  mstatus_MXR:%0d  satp:0x%0h",
		 sstatus_SUM, mstatus_MXR, satp);
	 $display ("");
      end

      imem.req (f3_LW, pc, priv, sstatus_SUM, mstatus_MXR, satp);
      branch_predictor.predict_req (pc);    // TODO: ASID.VA vs PA?

      rg_epoch <= epoch;
      rg_priv  <= priv;
   endmethod

   method Action bp_train (WordXL   pc,
			   Bool     is_i32_not_i16,
			   Instr    instr,
			   CF_Info  cf_info);
      branch_predictor.bp_train (pc, is_i32_not_i16, instr, cf_info);
   endmethod

   method Action set_full (Bool full);
      rg_full <= full;
   endmethod
endmodule

// ================================================================

endpackage
