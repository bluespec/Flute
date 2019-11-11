// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

package Branch_Predictor;

// ================================================================
// Branch Predictor for RISC-V CPU
//
// This is implemented as a simple direct-mapped cache.

// The 'insert' method overwrites the previous entry in the cache
//     (there is no 'writeback' of the victim)

// The 'predict' method requests a prediction.
// The 'prediction' method returns (and consumes) the response
// and is available from the next cycle onwards.
//    On a 'hit', returns 'predicted_pc'
//    On a 'miss', returns 'pc+4' (there is no 'cache refill')

// If there was no previous 'predict' request, then 'prediction'
// returns an invalid PC ('1') (no prediction).

// ================================================================
// Exports

export Branch_Predictor_IFC (..), mkBranch_Predictor;

// ================================================================
// BSV library imports

import Vector    :: *;
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

   method Action  predict_req (WordXL pc);

   (* always_ready *)
   method WordXL  predict_rsp (Bool is_i32_not_i16, Instr instr);

   method Action bp_train (WordXL   pc,
			   Bool     is_i32_not_i16,
			   Instr    instr,
			   CF_Info  cf_info);
endinterface

// ================================================================
// Cache parameters.
// PCs are at least 2-byte aligned, so we can ignore the lsb (always 0)

typedef  512  Cache_Num_Sets;
Integer  cache_num_sets = valueOf (Cache_Num_Sets);

// Splitting a PC into a cache index and a cache tag
// Bit #(XLEN) pc = { tag, index, 1'b0 }

typedef TLog #(Cache_Num_Sets)  Cache_Index_sz;
Integer cache_index_sz  = valueOf (Cache_Index_sz);

typedef Bit #(Cache_Index_sz)                           Cache_Index;

typedef Bit #(TSub #(TSub #(XLEN, Cache_Index_sz), 1))  Cache_Tag;

typedef Bit #(TSub #(XLEN, 1)) WordXL_Addr;

typedef struct {
   Bool         valid;
   Cache_Tag    pc_tag;
   WordXL_Addr  predicted_pc;
   } Cache_Entry
deriving (Bits, FShow);

typedef  16  RAS_Capacity;

// ================================================================
// Compute actions on RAS

function Tuple3 #(Bool, Bool, WordXL) fn_ras_actions (WordXL  pc,
						      Bool    is_i32_not_i16,
						      Instr   instr);
   // For 32-bit instuctions
   WordXL ret_pc      = pc + 4;
   Bool rd_is_link    = fn_reg_is_link (instr_rd  (instr));
   Bool rs1_is_link   = fn_reg_is_link (instr_rs1 (instr));
   Bool rs1_eq_rd     = (instr_rd  (instr) == instr_rs1 (instr));
   Bool instr_is_JAL  = (instr_opcode (instr) == op_JAL);
   Bool instr_is_JALR = (instr_opcode (instr) == op_JALR);

`ifdef ISA_C
   // For 16-bit instructions ('C' extension)
   if (! (is_i32_not_i16)) begin
      ret_pc = pc + 2;
      let rs1 = instr [11:7];
      let rs2 = instr [6:2];
      if ((instr [15:13] == funct3_C_J) && (instr [1:0] == opcode_C1)) begin
	 // C.J = JAL x0, offset
	 rd_is_link    = False;    // rd is x0
	 instr_is_JAL  = True;
	 instr_is_JALR = False;
      end
      else if ((instr [15:13] == funct3_C_JAL) && (instr [1:0] == opcode_C1) && (xlen == 32)) begin
	 // C.JAL = JAL x1, offset
	 rd_is_link    = True;    // rd is x1
	 instr_is_JAL  = True;
	 instr_is_JALR = False;
      end
      else if (   (instr [15:12] == funct4_C_JR)
	       && (instr [11:7] != 0)                // rs1
	       && (instr [6:2] == 0)
	       && (instr [1:0] == opcode_C2)) begin
	 // C.JR = JALR x0, 0(rs1)
	 rd_is_link    = False;    // rd is x0
	 rs1_is_link   = fn_reg_is_link (instr [11:7]);
	 rs1_eq_rd     = False;
	 instr_is_JAL  = False;
	 instr_is_JALR = True;
      end
      else if (   (instr [15:12] == funct4_C_JALR)
	       && (instr [11:7] != 0)                // rs1
	       && (instr [6:2] == 0)
	       && (instr [1:0] == opcode_C2)) begin
	 // C.JALR = JALR x1, 0(rs1)
	 rd_is_link    = True;    // rd is x1
	 rs1_is_link   = fn_reg_is_link (instr [11:7]);
	 rs1_eq_rd     = (instr [11:7] == x1);
	 instr_is_JAL  = False;
	 instr_is_JALR = True;
      end
   end
`endif

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
   BRAM_DUAL_PORT #(Cache_Index, Cache_Entry) btb_bramcore2
      <- mkBRAMCore2 (cache_num_sets, /* hasOutputRegister */ False);

   // Return Address Stack (RAS). Top of stack is at [0].
   Reg #(Vector #(RAS_Capacity, WordXL)) rg_ras       <- mkReg (replicate (bogus_PC));

   // This FIFO holds the PC being predicted (currently probing the cache)
   Reg #(WordXL)  rg_pc <- mkRegU;

   // For reset
   Reg #(Bool)         rg_resetting <- mkReg (True);
   Reg #(Cache_Index)  rg_index     <- mkReg (0);

   // ----------------------------------------------------------------
   // Pick out bits from the PC to use as index into the table
   // Ignore bottom 2 lsbs since addresses are 4-byte-aligned.

   function Cache_Index  fn_pc_to_index (WordXL pc);
      return pc [cache_index_sz : 1];
   endfunction

   function Cache_Tag  fn_pc_to_tag (WordXL pc);
      return pc [(valueOf (XLEN) - 1) : cache_index_sz + 1];
   endfunction

   function WordXL_Addr  fn_pc_to_word_addr (WordXL pc);
      return pc [(valueOf (XLEN) - 1) : 1];
   endfunction

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset (rg_resetting);
      let data = Cache_Entry {valid: False,
			      pc_tag: 0,
			      predicted_pc: 0};
      btb_bramcore2.b.put (/* write */ True, rg_index, data);
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

   method Action  predict_req (WordXL pc) if (! rg_resetting);
      if (verbosity > 1)
	 $display ("%0d: %m.predict_req (pc 0x%0h)", cur_cycle, pc);

      let index_a = fn_pc_to_index (pc);
      Bool op_write_not_read = False;
      btb_bramcore2.a.put (op_write_not_read, index_a, ?);
      rg_pc <= pc;
   endmethod

   method WordXL  predict_rsp (Bool is_i32_not_i16, Instr instr);
      WordXL pred_pc = bogus_PC;    // default: no prediction, invalid PC

      let cache_entry = btb_bramcore2.a.read;
      match { .do_pop, .*, .* } = fn_ras_actions (bogus_PC, is_i32_not_i16, instr);

      if (do_pop) begin
	 pred_pc = rg_ras [0];    // Top of RAS (will be popped in bp_train())
      end

      // Lookup BTB if no result from RAS
      if (   (pred_pc == bogus_PC)
	  && (cache_entry.valid)
	  && (cache_entry.pc_tag == fn_pc_to_tag (rg_pc))) begin
	 // BTB Hit
	 pred_pc = { cache_entry.predicted_pc, 1'b0 };
      end

      // Default
      if (pred_pc == bogus_PC)
	 pred_pc = rg_pc + (is_i32_not_i16 ? 4 : 2);

      return pred_pc;
   endmethod

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
      let ras_val   = rg_ras;
      if (do_pop)  ras_val = shiftInAtN (ras_val, bogus_PC);
      if (do_push) ras_val = shiftInAt0 (ras_val, ret_pc);
      rg_ras       <= ras_val;

      // ----------------
      // BTB training using cf_info which is feedback from downpipe
      // exec stage (several instructions earlier)

      WordXL pred_PC = bogus_PC;

      if (cf_info.cf_op == CF_BR)
	 pred_PC = (cf_info.taken ? cf_info.taken_PC : cf_info.fallthru_PC);

      else if ((cf_info.cf_op == CF_JAL) || (cf_info.cf_op == CF_JALR))
	 pred_PC = cf_info.taken_PC;

      if (pred_PC != bogus_PC) begin
	 Cache_Index index_b = fn_pc_to_index (cf_info.from_PC);
	 let data = Cache_Entry {valid:        True,
				 pc_tag:       fn_pc_to_tag (cf_info.from_PC),
				 predicted_pc: fn_pc_to_word_addr (pred_PC) };
         Bool op_write_not_read = True;
	 btb_bramcore2.b.put (op_write_not_read, index_b, data);
	 if (verbosity > 2)
	    $display ("    insert prediction [0x%0h] <= (%0h -> %0h)",
		      index_b, cf_info.from_PC, pred_PC);
      end
   endmethod

endmodule

// ================================================================

endpackage
