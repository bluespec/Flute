// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

package Branch_Predictor;

// ================================================================
// Branch Predictor for RISC-V CPU
//
// BTB (Branch Target Buffer) with 2-bit FSM for hysteresis.
//         Direct-mapped.
// RAS (Return-Address Stack)

// ================================================================
// Exports

export Branch_Predictor_IFC (..), mkBranch_Predictor;

// ================================================================
// BSV library imports

import Vector    :: *;
import RegFile   :: *;
import BRAMCore  :: *;
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
// BTB parameters and functions.
// Ignore PCs[0] since PCs are at least 2-byte aligned (word-aligned).

typedef  512  BTB_Num_Sets;
Integer  btb_num_sets = valueOf (BTB_Num_Sets);

// Split a PC into a btb index and a btb tag
// Bit #(XLEN) pc = { tag, index, 1'b0 }

typedef TLog #(BTB_Num_Sets)  BTB_Index_Sz;
Integer btb_index_sz  = valueOf (BTB_Index_Sz);

typedef Bit #(BTB_Index_Sz)                           BTB_Index;

typedef Bit #(TSub #(TSub #(XLEN, BTB_Index_Sz), 1))  BTB_Tag;

// ----------------
// Pick out PC bits to use as index into BTB.

function BTB_Index  fn_pc_to_index (WordXL pc);
   return pc [btb_index_sz : 1];
endfunction

function BTB_Tag  fn_pc_to_tag (WordXL pc);
   return pc [(valueOf (XLEN) - 1) : btb_index_sz + 1];
endfunction

function Word_Addr  fn_pc_to_word_addr (WordXL pc);
   return pc [(valueOf (XLEN) - 1) : 1];
endfunction

// ----------------
// BTB entries

typedef Bit #(TSub #(XLEN, 1)) Word_Addr;

typedef struct {
   Bool       valid;
   BTB_Tag    pc_tag;
   Word_Addr  predicted_pc;
   } BTB_Entry
deriving (Bits, FShow);

// ================================================================
// RAS constants and functions

typedef  16  RAS_Capacity;

// ----------------
// Compute actions on RAS.
// Results are do_pop, do_push, and ret_PC (for push)

function Tuple3 #(Bool, Bool, WordXL) fn_ras_actions (WordXL  pc,
						      Bool    is_i32_not_i16,
						      Instr   instr);
   // Classify instr, for 32-bit instuctions
   WordXL ret_pc        = pc + 4;
   Bool   rd_is_link    = fn_reg_is_link (instr_rd  (instr));
   Bool   rs1_is_link   = fn_reg_is_link (instr_rs1 (instr));
   Bool   rs1_eq_rd     = (instr_rd  (instr) == instr_rs1 (instr));
   Bool   instr_is_JAL  = (instr_opcode (instr) == op_JAL);
   Bool   instr_is_JALR = (instr_opcode (instr) == op_JALR);

`ifdef ISA_C
   // Classify instr, for 16-bit instructions ('C' extension)
   if (! (is_i32_not_i16)) begin
      ret_pc = pc + 2;
      let funct3 = instr [15:13];
      let funct4 = instr [15:12];
      let rs1    = instr [11:7];
      let rs2    = instr [6:2];
      let op     = instr [1:0];
      if ((funct3 == funct3_C_J) && (op == opcode_C1)) begin
	 // C.J = JAL x0, offset
	 rd_is_link    = False;    // rd is x0
	 instr_is_JAL  = True;
	 instr_is_JALR = False;
      end
      else if ((funct3 == funct3_C_JAL) && (op == opcode_C1) && (xlen == 32)) begin
	 // C.JAL = JAL x1, offset
	 rd_is_link    = True;    // rd is x1
	 instr_is_JAL  = True;
	 instr_is_JALR = False;
      end
      else if ((funct4 == funct4_C_JR) && (rs1 != 0) && (rs2 == 0) && (op == opcode_C2)) begin
	 // C.JR = JALR x0, 0(rs1)
	 rd_is_link    = False;    // rd is x0
	 rs1_is_link   = fn_reg_is_link (rs1);
	 rs1_eq_rd     = False;
	 instr_is_JAL  = False;
	 instr_is_JALR = True;
      end
      else if ((funct4 == funct4_C_JALR) && (rs1 != 0) && (rs2 == 0) && (op == opcode_C2)) begin
	 // C.JALR = JALR x1, 0(rs1)
	 rd_is_link    = True;    // rd is x1
	 rs1_is_link   = fn_reg_is_link (rs1);
	 rs1_eq_rd     = (rs1 == x1);
	 instr_is_JAL  = False;
	 instr_is_JALR = True;
      end
   end
`endif

   // Compute RAS actions based on classification.
   // (cf. RISC-V Unprivileged ISA Manual, Sec 2.5, Control Transfer Instructions/ Unconditional Jumps)
   Bool do_pop  = False;
   Bool do_push = False;
   if (instr_is_JAL && rd_is_link)
      do_push = True;
   else if (instr_is_JALR) begin
      if ((! rd_is_link) && rs1_is_link)
	 do_pop = True;
      else if (rd_is_link && (! rs1_is_link))
	 do_push = True;
      else if (rd_is_link && rs1_is_link) begin
	 do_push = True;
	 if (! rs1_eq_rd)
	    do_pop  = True;
      end
   end
   return tuple3 (do_pop, do_push, ret_pc);
endfunction

// ================================================================

(* synthesize *)
module mkBranch_Predictor (Branch_Predictor_IFC);

   Integer verbosity = 0;
   WordXL  bogus_PC  = '1;

   // Branch Target Table (BTB)
   Bool bram_has_output_reg = False;
   BRAM_DUAL_PORT #(BTB_Index, BTB_Entry) btb_bramcore2 <- mkBRAMCore2 (btb_num_sets,
									bram_has_output_reg);

   // 2-bit FSMs for BTB hysteresis.
   //     00 = strong not-taken
   //     01 = weak   not-taken
   //     10 = weak   taken
   //     11 = strong taken
   // This is a regfile separate from btb_bramcore2 to allow combinational lookup,
   // to enable read-modify-write in a cycle.
   RegFile #(BTB_Index, Bit #(2)) rf_btb_fsms <- mkRegFileFull;

   // Return Address Stack (RAS). Top of stack is always at [0]. Push/pop by shifting.
   Reg #(Vector #(RAS_Capacity, WordXL)) rg_ras  <- mkReg (replicate (bogus_PC));

   // This reg holds the PC being predicted (currently probing the btb)
   Reg #(WordXL)  rg_pc <- mkRegU;

   // For reset
   Reg #(Bool)       rg_resetting <- mkReg (True);
   Reg #(BTB_Index)  rg_index     <- mkReg (0);

   // ----------------------------------------------------------------
   // BEHAVIOR

   // Reset loop: invalidate each BTB entry and set each hysteresis FSM to 0.

   rule rl_reset (rg_resetting);
      let btb_entry = BTB_Entry {valid: False,
				 pc_tag: 0,
				 predicted_pc: 0};
      Bool op_write_not_read = True;
      btb_bramcore2.b.put (op_write_not_read, rg_index, btb_entry);
      rf_btb_fsms.upd (rg_index, 2'b00);    // strong not-taken

      if (rg_index == '1) begin
	 rg_resetting <= False;
	 if (verbosity > 1)
	    $display ("%0d: %m.rl_reset: reset complete", cur_cycle);
      end
      rg_index <= rg_index + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      rg_index     <= 0;
      rg_resetting <= True;
   endmethod

   // ----------------
   // Request prediction for given pc; available on 'predict_rsp' on next cycle

   method Action  predict_req (WordXL pc) if (! rg_resetting);
      if (verbosity > 1)
	 $display ("%0d: %m.predict_req (pc 0x%0h)", cur_cycle, pc);

      let index_a = fn_pc_to_index (pc);
      Bool op_write_not_read = False;
      btb_bramcore2.a.put (op_write_not_read, index_a, ?);
      rg_pc <= pc;
   endmethod

   // ----------------
   // Response for 'predict_req' from an earlier cycle.
   // Args describe current instruction just now fetched in Fetch stage,
   // and are used to choose RAS actions if any, and size of
   // fall-through PC if no prediction.

   method WordXL  predict_rsp (Bool is_i32_not_i16, Instr instr);
      WordXL pred_pc   = bogus_PC;    // default: no prediction, invalid PC
      let    btb_entry = btb_bramcore2.a.read;

      // Check if prediction is from RAS pop (JALR returns)
      match { .do_pop, .*, .* } = fn_ras_actions (bogus_PC, is_i32_not_i16, instr);
      if (do_pop)
	 pred_pc = rg_ras [0];    // Top of RAS (will be popped in bp_train())

      // Look for BTB hit if no result from RAS
      if (   (pred_pc == bogus_PC)
	  && (btb_entry.valid)
	  && (btb_entry.pc_tag == fn_pc_to_tag (rg_pc))) begin
	 pred_pc = { btb_entry.predicted_pc, 1'b0 };
      end

      // Default prediction: fallthrough
      if (pred_pc == bogus_PC)
	 pred_pc = rg_pc + (is_i32_not_i16 ? 4 : 2);

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
      if (verbosity > 2)
	 $display ("%0d: %m.bp_train: pc %0x  is_i32 %0d  instr %0x ",
		   cur_cycle, pc, is_i32_not_i16, instr, fshow (cf_info));

      // ----------------
      // RAS training using (pc, is_i32_not_i16, instr) which are from currently fetched instr
      // Top-of-stack is at ras [0]

      match { .do_pop, .do_push, .ret_pc } = fn_ras_actions (pc, is_i32_not_i16, instr);
      let ras_val  = rg_ras;
      if (do_pop)  ras_val = shiftInAtN (ras_val, bogus_PC);
      if (do_push) ras_val = shiftInAt0 (ras_val, ret_pc);
      rg_ras       <= ras_val;

      // ----------------
      // BTB training using cf_info which is feedback from downpipe
      // exec stage (several instructions earlier)

      WordXL    pred_PC = bogus_PC;
      BTB_Index index_b = fn_pc_to_index (cf_info.from_PC);
      Bit #(2)  fsm_val = rf_btb_fsms.sub (index_b);

      if (cf_info.cf_op == CF_BR) begin
	 if (cf_info.taken)
	    case (fsm_val)
	       2'b00: begin fsm_val = 2'b01; pred_PC  = cf_info.fallthru_PC; end
	       2'b01: begin fsm_val = 2'b10; pred_PC  = cf_info.taken_PC;    end
	       2'b10: begin fsm_val = 2'b11; pred_PC  = cf_info.taken_PC;    end
	       2'b11: begin fsm_val = 2'b11; pred_PC  = cf_info.taken_PC;    end
	    endcase
	 else
	    case (fsm_val)
	       2'b00: begin fsm_val = 2'b00; pred_PC  = cf_info.fallthru_PC; end
	       2'b01: begin fsm_val = 2'b00; pred_PC  = cf_info.fallthru_PC; end
	       2'b10: begin fsm_val = 2'b01; pred_PC  = cf_info.fallthru_PC; end
	       2'b11: begin fsm_val = 2'b10; pred_PC  = cf_info.taken_PC;    end
	    endcase
      end
      else if ((cf_info.cf_op == CF_JAL) || (cf_info.cf_op == CF_JALR)) begin
	 fsm_val = 2'b11;    // strong taken
	 pred_PC = cf_info.taken_PC;
      end
      if (pred_PC != bogus_PC) begin
	 rf_btb_fsms.upd (index_b, fsm_val);
	 let btb_entry = BTB_Entry {valid:        True,
				    pc_tag:       fn_pc_to_tag (cf_info.from_PC),
				    predicted_pc: fn_pc_to_word_addr (pred_PC) };
	 Bool op_write_not_read = True;
	 btb_bramcore2.b.put (op_write_not_read, index_b, btb_entry);
	 if (verbosity > 2)
	    $display ("    insert prediction [0x%0h] <= (%0h -> %0h)",
		      index_b, cf_info.from_PC, pred_PC);
      end
   endmethod

endmodule

// ================================================================

endpackage
