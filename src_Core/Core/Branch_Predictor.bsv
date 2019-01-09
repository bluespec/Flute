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

import ConfigReg :: *;
import BRAMCore  :: *;
import FIFOF     :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;

// ================================================================

interface Branch_Predictor_IFC;
   method Action  reset;

   method Action  predict_req (WordXL pc, Maybe #(WordXL) m_old_pc);

   (* always_ready *)
   method WordXL  predict_rsp;
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

// ================================================================

(* synthesize *)
module mkBranch_Predictor (Branch_Predictor_IFC);

   Reg #(int)  cfg_verbosity <- mkConfigReg (0);

   BRAM_DUAL_PORT #(Cache_Index, Cache_Entry) bramcore2
      <- mkBRAMCore2 (cache_num_sets, /* hasOutputRegister */ False);

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
      bramcore2.b.put (/* write */ True, rg_index, data);
      if (rg_index == '1) begin
	 rg_resetting <= False;
	 if (cfg_verbosity > 1)
	    $display ("%0d: Branch Predictor: reset complete", cur_cycle);
      end
      rg_index <= rg_index + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      rg_index     <= 0;
      rg_resetting <= True;
   endmethod

   method Action predict_req (WordXL pc, Maybe #(WordXL) m_old_pc) if (! rg_resetting);
      if (cfg_verbosity > 1)
	 $display ("    Branch_Predictor.predict_req (pc 0x%0h)", pc);

      if (m_old_pc matches tagged Valid .old_pc) begin
	 Cache_Index index_b = fn_pc_to_index (old_pc);
	 let data = Cache_Entry {valid:        True,
				 pc_tag:       fn_pc_to_tag (old_pc),
				 predicted_pc: fn_pc_to_word_addr (pc) };
	 bramcore2.b.put (/* write */ True, index_b, data);
	 if (cfg_verbosity > 1)
	    $display ("        insert prediction [0x%0h] <= (from pc 0x%0h, to pc 0x%0h)",
		      index_b, old_pc, pc);
      end

      let index_a = fn_pc_to_index (pc);
      bramcore2.a.put (/* write */ False, index_a, ?);
      rg_pc <= pc;
   endmethod

   method WordXL  predict_rsp ();
      WordXL pred_pc = 1;    // default: no prediction, invalid PC

      let cache_entry = bramcore2.a.read;

      if ((cache_entry.valid) && (cache_entry.pc_tag == fn_pc_to_tag (rg_pc))) begin
	 // Hit
	 pred_pc = { cache_entry.predicted_pc, 1'b0 };
      end
      else begin
	 // Miss: do default prediction
	 pred_pc = rg_pc + 4;
      end
      return pred_pc;
   endmethod

endmodule

// ================================================================

endpackage
