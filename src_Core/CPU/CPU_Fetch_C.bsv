// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CPU_Fetch_C;

// ================================================================
// mkCPU_Fetch_C is an IMem_IFC->Module#(IMem_IFC) wrapper.

// The input parameter interface is for 32-bit-aligned 32-bit instruction-fetches.

// This module completely encapsulates the logic needed to extend it to
// 16-bit "compressed" instructions ('C' extension)
// which then also includes 16-bit-aligned 32-bit instructions.

// ================================================================
// Exports

export mkCPU_Fetch_C;

// ================================================================
// BSV lib imports

import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle :: *;

// ================================================================
// Project imports

import ISA_Decls    :: *;
import Near_Mem_IFC :: *;

// ================================================================

module mkCPU_Fetch_C #(IMem_IFC  imem32) (IMem_IFC);

   // Assert: imem32.is_32b_not_16b == True

   Integer verbosity = 0;

   // This register holds the actual PC value (which can be +/-2 from
   // the address given to the ICache.
   Reg #(WordXL)  rg_pc  <- mkRegU;

   // This register holds [15:0] of a 32b instr that was in the
   // upper-half of a word from the ICache.
   Reg #(Bit #(16))  rg_instr_15_0  <- mkRegU;

   // The following hold args for the mmu-cache that are needed when
   // doing the second 32-bit fetch of an odd-aligned 32b instr
   Reg #(Bit #(3))  rg_f3          <- mkRegU;
   Reg #(Priv_Mode) rg_priv        <- mkRegU;
   Reg #(Bit #(1))  rg_sstatus_SUM <- mkRegU;
   Reg #(Bit #(1))  rg_mstatus_MXR <- mkRegU;
   Reg #(WordXL)    rg_satp        <- mkRegU;

   // ----------------------------------------------------------------

   function Bool is_even_h_pc (WordXL pc);
      return (pc [1:0] == 2'b00);
   endfunction

   function Bool is_odd_h_pc (WordXL pc);
      return (pc [1:0] != 2'b00);
   endfunction

   function Bool is_32b_instr (Bit #(n) instr);
      return (instr [1:0] == 2'b11);
   endfunction

   function Bool is_16b_instr (Bit #(n) instr);
      return (instr [1:0] != 2'b11);
   endfunction

   Bool eqw_rg_pc_imem_pc = ((imem32.pc >> 2) == (rg_pc >> 2));

   // ----------------------------------------------------------------
   // Conditions for selecting 16b and 32b instruction

   // Condition: 32b instr from {imem [15:0], rg_instr_15_0}
   Bool cond_i32_odd = (imem32.pc == rg_pc + 2);    // Assert: is_odd_h_pc (rg_pc)

   // Condition: 32b instr from imem [31:0]
   Bool cond_i32_even = (   eqw_rg_pc_imem_pc
			 && is_even_h_pc (rg_pc)
			 && (is_32b_instr (imem32.instr)));

   // Condition: 16b instr from imem [31:16]
   Bool cond_i16_odd  = (   eqw_rg_pc_imem_pc
			 && is_odd_h_pc (rg_pc)
			 && (is_16b_instr (imem32.instr [31:16])));

   // Condition: 16b instr from imem [15:0]
   Bool cond_i16_even = (   eqw_rg_pc_imem_pc
			 && is_even_h_pc (rg_pc)
			 && (is_16b_instr (imem32.instr)));

   // Condition: 32b instr from { ...next imem [15:0], imem [31:16] }
   Bool cond_i32_odd_fetch_next = (   eqw_rg_pc_imem_pc
				   && is_odd_h_pc (rg_pc)
				   && (is_32b_instr (imem32.instr [31:16])));

   // ================================================================
   // Assert: The underlying imem32 only knows about 32-bit aligned words

   rule rl_assert_fail (! imem32.is_i32_not_i16);
      $display ("ERROR: CPU_Fetch_C: imem32.is_i32_not_i16 is False");
      $finish (1);
   endrule

   rule rl_debug_conds (False && (verbosity != 0) && imem32.valid);
      $display ("    imem32.pc 0x%0h  pc 0x%0h  rg_f3 %0d  imem32.instr 0x%08h  rg_instr_15_0 0x%04h",
		imem32.pc,  rg_pc,  rg_f3,  imem32.instr,  rg_instr_15_0);
      $display ("    cond_i32_odd  = ", fshow (cond_i32_odd));
      $display ("    cond_i32_even = ", fshow (cond_i32_even));
      $display ("    cond_i16_odd  = ", fshow (cond_i16_odd));
      $display ("    cond_i16_even = ", fshow (cond_i16_even));
      $display ("    cond_i32_odd_fetch_next             = ", fshow (cond_i32_odd_fetch_next));
      $display ("    eqw_rg_pc_imem_pc                   = ", fshow (eqw_rg_pc_imem_pc));
      $display ("    is_odd_h_pc (rg_pc)                 = ", fshow (is_odd_h_pc (rg_pc)));
      $display ("    is_32b_instr (imem32.instr [31:16]) = ", fshow (is_32b_instr (imem32.instr [31:16])));
   endrule

   // ================================================================
   // On 32b odd instr in imem32.instr [31:15], fetch next 32 bits

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_fetch_next_32b (imem32.valid && cond_i32_odd_fetch_next);
      Addr next_word_addr = rg_pc + 2;
      imem32.req (rg_f3, next_word_addr, rg_priv, rg_sstatus_SUM, rg_mstatus_MXR, rg_satp);
      rg_instr_15_0 <= imem32.instr [31:16];
      if (verbosity != 0)
	 $display ("CPU_Fetch_C.rl_fetch_next_32b:  imem32.pc 0x%0h  next_word_addr 0x%0h",
		   imem32.pc, next_word_addr);
   endrule

   // ----------------
   // Compose the 'instr' 32b output

   Instr instr_out = imem32.instr;
   Bool  is_32b_not_16b = True;
   if (cond_i32_odd)
      instr_out = { imem32.instr [15:0], rg_instr_15_0 };

   else if (cond_i16_even) begin
      instr_out = { 16'b0, imem32.instr [15:0] };
      is_32b_not_16b = False;
   end

   else if (cond_i16_odd) begin
      instr_out = { 16'b0, imem32.instr [31:16] };
      is_32b_not_16b = False;
   end

   // ================================================================
   // INTERFACE

   // CPU side: IMem request
   method Action  req (Bit #(3) f3,
		       WordXL addr,
		       // The following  args for VM
		       Priv_Mode  priv,
		       Bit #(1)   sstatus_SUM,
		       Bit #(1)   mstatus_MXR,
		       WordXL     satp               // { VM_Mode, ASID, PPN_for_page_table }
		       ) if (! cond_i32_odd_fetch_next);
      rg_f3          <= f3;
      rg_pc          <= addr;
      rg_priv        <= priv;
      rg_sstatus_SUM <= sstatus_SUM;
      rg_mstatus_MXR <= mstatus_MXR;
      rg_satp        <= satp;
      WordXL even_addr = { addr [xlen-1:2], 2'b_00 };
      imem32.req (f3, even_addr, priv, sstatus_SUM, mstatus_MXR, satp);
      if (verbosity > 0) begin
	 $display ("CPU_Fetch_C.req: addr 0x%0h, even_addr 0x%0h", addr, even_addr);
      end
   endmethod

   // CPU side: IMem response
   method Bool     valid    = (imem32.valid && (   cond_i32_odd
						|| cond_i32_even
						|| cond_i16_odd
						|| cond_i16_even));

   method Bool     is_i32_not_i16 = (cond_i32_odd || cond_i32_even);

   method WordXL   pc       = rg_pc;
   method Instr    instr    = instr_out;
   method Bool     exc      = imem32.exc;
   method Exc_Code exc_code = imem32.exc_code;
   method WordXL   tval     = imem32.pc;        // Can be different from rg_pc

endmodule

// ================================================================

endpackage
