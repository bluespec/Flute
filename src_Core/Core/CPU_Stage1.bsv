// Copyright (c) 2016-2018 Bluespec, Inc. All Rights Reserved

package CPU_Stage1;

// ================================================================
// This is Stage 1 of the "Flute" CPU.
// It contains the EX functionality.
// EX: "Execute"

// ================================================================
// Exports

export
CPU_Stage1_IFC (..),
mkCPU_Stage1;

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
import GPR_RegFile      :: *;
import CSR_RegFile      :: *;
import EX_ALU_functions :: *;

// ================================================================
// Interface

interface CPU_Stage1_IFC;
   // ---- Reset
   interface Server #(Token, Token) server_reset;

   // ---- Output
   (* always_ready *)
   method Output_Stage1 out;

   (* always_ready *)
   method Action deq;

   // ---- Input
   (* always_ready *)
   method Action enq (Data_StageD_to_Stage1  data);

   (* always_ready *)
   method Action set_full (Bool full);
endinterface

// ================================================================
// Implementation module

module mkCPU_Stage1 #(Bit #(4)         verbosity,
		      GPR_RegFile_IFC  gpr_regfile,
`ifdef ISA_F
		      FPR_RegFile_IFC  fpr_regfile,
`endif
		      CSR_RegFile_IFC  csr_regfile,
		      Bypass           bypass_from_stage2,
		      Bypass           bypass_from_stage3,
		      Epoch            cur_epoch,
		      Priv_Mode        cur_priv)
                    (CPU_Stage1_IFC);

   FIFOF #(Token) f_reset_reqs <- mkFIFOF;
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   Reg #(Bool)                  rg_full        <- mkReg (False);
   Reg #(Data_StageD_to_Stage1) rg_stage_input <- mkRegU;

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset;
      f_reset_reqs.deq;
      rg_full <= False;
      f_reset_rsps.enq (?);
   endrule

   // ----------------
   // ALU

   let decoded_instr = rg_stage_input.decoded_instr;
   let funct3        = decoded_instr.funct3;

   // Register rs1 read and bypass
   let rs1 = decoded_instr.rs1;
   let rs1_val = gpr_regfile.read_rs1 (rs1);
   match { .busy1a, .rs1a } = fn_gpr_bypass (bypass_from_stage3, rs1, rs1_val);
   match { .busy1b, .rs1b } = fn_gpr_bypass (bypass_from_stage2, rs1, rs1a);
   Bool rs1_busy = (busy1a || busy1b);
   Word rs1_val_bypassed = ((rs1 == 0) ? 0 : rs1b);

   // Register rs2 read and bypass
   let rs2 = decoded_instr.rs2;
   let rs2_val = gpr_regfile.read_rs2 (rs2);
   match { .busy2a, .rs2a } = fn_gpr_bypass (bypass_from_stage3, rs2, rs2_val);
   match { .busy2b, .rs2b } = fn_gpr_bypass (bypass_from_stage2, rs2, rs2a);
   Bool rs2_busy = (busy2a || busy2b);
   Word rs2_val_bypassed = ((rs2 == 0) ? 0 : rs2b);

   // ALU function
   let alu_inputs = ALU_Inputs {cur_priv:       cur_priv,
				pc:             rg_stage_input.pc,
				instr:          rg_stage_input.instr,
				decoded_instr:  rg_stage_input.decoded_instr,
				rs1_val:        rs1_val_bypassed,
				rs2_val:        rs2_val_bypassed,
				mstatus:        csr_regfile.read_mstatus,
				misa:           csr_regfile.read_misa};

   let alu_outputs = fv_ALU (alu_inputs);

   // ----------------
   // Combinational output function

   function Output_Stage1 fv_out;
      Output_Stage1 output_stage1 = ?;

      // This stage is empty
      if (! rg_full) begin
	 output_stage1.ostatus = OSTATUS_EMPTY;
      end

      // Wrong branch-prediction epoch: discard instruction (convert into a NOOP)
      else if (rg_stage_input.epoch != cur_epoch) begin
	 output_stage1.ostatus        = OSTATUS_PIPE;
	 output_stage1.control        = CONTROL_DISCARD;

	 // For debugging only
	 output_stage1.data_to_stage2 = Data_Stage1_to_Stage2 {priv:      cur_priv,
							       pc:        rg_stage_input.pc,
							       instr:     rg_stage_input.instr,
							       op_stage2: OP_Stage2_ALU,
							       rd:        0,
							       addr:      ?,
							       val1:      ?,
							       val2:      ? };
      end

      // Stall if bypass pending for rs1 or rs2
      else if (rs1_busy || rs2_busy) begin
	 output_stage1.ostatus = OSTATUS_BUSY;
      end

      // Trap on fetch-exception
      else if (rg_stage_input.exc) begin
	 output_stage1.ostatus   = OSTATUS_NONPIPE;
	 output_stage1.control   = CONTROL_TRAP;
	 output_stage1.trap_info = Trap_Info {epc:      rg_stage_input.pc,
					      exc_code: rg_stage_input.exc_code,
					      badaddr:  rg_stage_input.pc};    // TODO: '?', perhaps?
      end

      // ALU outputs: pipe (straight/branch)
      // and non-pipe (CSRR_W, CSRR_S_or_C, FENCE.I, FENCE, SFENCE_VMA, xRET, WFI, TRAP)
      else begin
	 let ostatus = (  (   (alu_outputs.control == CONTROL_STRAIGHT)
			   || (alu_outputs.control == CONTROL_BRANCH))
			? OSTATUS_PIPE
			: OSTATUS_NONPIPE);

	 // TODO: change name 'badaddr' to 'tval'
	 let badaddr = 0;
	 if (alu_outputs.exc_code == exc_code_ILLEGAL_INSTRUCTION)
	    badaddr = zeroExtend (rg_stage_input.instr);
	 else if (alu_outputs.exc_code == exc_code_INSTR_ADDR_MISALIGNED)
	    badaddr = alu_outputs.addr;    // branch target pc
	 let trap_info = Trap_Info {epc:      rg_stage_input.pc,
				    exc_code: alu_outputs.exc_code,
				    badaddr:  badaddr};  // v1.10 - mtval

	 let next_pc = ((alu_outputs.control == CONTROL_BRANCH) ? alu_outputs.addr : rg_stage_input.pc + 4);
	 let redirect = (next_pc != rg_stage_input.pred_pc);

	 let data_to_stage2 = Data_Stage1_to_Stage2 {priv:      cur_priv,
						     pc:        rg_stage_input.pc,
						     instr:     rg_stage_input.instr,
						     op_stage2: alu_outputs.op_stage2,
						     rd:        alu_outputs.rd,
						     addr:      alu_outputs.addr,
						     val1:      alu_outputs.val1,
						     val2:      alu_outputs.val2 };

	 output_stage1.ostatus        = ostatus;
	 output_stage1.trap_info      = trap_info;
	 output_stage1.control        = alu_outputs.control;
	 output_stage1.redirect       = redirect;
	 output_stage1.next_pc        = next_pc;
	 output_stage1.data_to_stage2 = data_to_stage2;
      end

      return output_stage1;
   endfunction: fv_out

   // ================================================================
   // INTERFACE

   // ---- Reset
   interface server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // ---- Output
   method Output_Stage1 out;
      return fv_out;
   endmethod

   method Action deq ();
   endmethod

   // ---- Input
   method Action enq (Data_StageD_to_Stage1  data);
      rg_stage_input <= data;
      if (verbosity > 1)
	 $display ("    CPU_Stage1.enq: 0x%08x", data.pc);
   endmethod

   method Action set_full (Bool full);
      rg_full <= full;
   endmethod
endmodule

// ================================================================

endpackage
