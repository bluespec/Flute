// Copyright (c) 2016-2018 Bluespec, Inc. All Rights Reserved

package Shifter_Box;

// ================================================================
// This contains a module that executes the RISC-V instructions:
//  SLL/SLLI    shift left  logical
//  SRL/SRLI    shift right logical
//  SRA/SRAI    shift right arithmetic
// using a serial shifter.  For shift amounts of 31 (63) bits, it can
// take up to 31 (63) ticks to compute the result, which is slow, but
// it's much cheaper in hardware resources than a 1-tick barrel
// shifter.

// ================================================================
// Exports

export
Shifter_Box_IFC (..),
mkShifter_Box;

// ================================================================
// BSV Library imports

// None

// ----------------
// BSV additional libs

// None

// ================================================================
// Project imports

import ISA_Decls  :: *;

// ================================================================
// MBox interface

interface Shifter_Box_IFC;
   // request
   // 'right' specifies right or left shift
   // 'v1' is the operand to be shifted
   // 'v2' has the shift amount ([4:0] for RV32, [5:0] for RV64)
   //      and whether right-shifts are logical ([5]=0) or arithmetic ([5]=1)
   (* always_ready *)
   method Action  req (Bool right, Word v1, Word v2);

   // response
   (* always_ready *)   method Bool  valid;
   (* always_ready *)   method Word  word;
endinterface

// ================================================================

(* synthesize *)
module mkShifter_Box (Shifter_Box_IFC);
   Reg #(Bool)                rg_right       <- mkRegU;
   Reg #(Word)                rg_v1          <- mkRegU;
   Reg #(Bit #(TLog #(XLEN))) rg_shamt       <- mkRegU;
   Reg #(Bool)                rg_arith_shift <- mkRegU;

   // The 'execution_order' attibs below are to override the compiler
   // default which schedules 'req' before rl_sll/sra/srl.  But 'req',
   // 'valid' and 'word' are called in the same rule in the main
   // pipeline, and so should not have 'sll/sra/srl' in between them.

   // Left shifts
   (* execution_order = "req, rl_sll" *)
   rule rl_sll ((! rg_right) && (rg_shamt != 0));
      rg_v1    <= (rg_v1 << 1);
      rg_shamt <= rg_shamt - 1;
   endrule

   // Arithmetic right-shifts
   (* execution_order = "req, rl_sra" *)
   rule rl_sra (rg_right && rg_arith_shift && (rg_shamt != 0));
      Word_S s_val = unpack (rg_v1);    // Signed value
      rg_v1 <= pack (s_val >> 1);
      rg_shamt <= rg_shamt - 1;
   endrule

   // Logical right shifts
   (* execution_order = "req, rl_srl" *)
   rule rl_srl (rg_right && (! rg_arith_shift) && (rg_shamt != 0));
      rg_v1 <= (rg_v1 >> 1);
      rg_shamt <= rg_shamt - 1;
   endrule

   // ================================================================
   // INTERFACE

   // MBox interface: request
   method Action  req (Bool right, Word v1, Word v2);
      rg_right       <= right;
      rg_v1          <= v1;
      rg_shamt       <= truncate (v2);
      rg_arith_shift <= unpack (v2 [7]);
   endmethod

   // MBox interface: response
   method Bool  valid;
      return (rg_shamt == 0);
   endmethod

   method Word  word;
      return rg_v1;
   endmethod
endmodule

// ================================================================

endpackage
