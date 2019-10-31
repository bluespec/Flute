// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CPU_Fetch_C;

// ================================================================
// mkCPU_Fetch_C is an IMem_IFC->Module#(IMem_IFC) wrapper.

// The input parameter interface is for 32-bit-aligned 32-bit instruction-fetches.

// This module completely encapsulates the logic needed to extend it to
// 16-bit "compressed" instructions ('C' extension)
// which then also includes 32-bit instructions which may be only 16-bit-aligned,
// since 32b and 16b instructions can be freely mixed.

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
// Functions concerning address-alignment of instructions.
// 'Even': aligned to an even 16b boundary (so, also aligned to 32b boundary)
// 'Odd':  aligned to an odd  16b boundary (so, not  aligned to 32b boundary)

function Bool is_addr_even16 (WordXL pc);
   return (pc [1:0] == 2'b00);
endfunction

function Bool is_addr_odd16 (WordXL pc);
   return (pc [1:0] != 2'b00);
endfunction

function WordXL fn_to_b32_addr (WordXL addr);
   return ((addr >> 2) << 2);
endfunction

// Equality of 32-bit aligned addresses containing a1 and a2 (i.e., ignoring [1:0])
function Bool eq_b32_addr (WordXL a1, WordXL a2) = ((a1 >> 2) == (a2 >> 2));

// ================================================================
// Functions concerning whether an instr is 32b (RV) or 16b (RVC)

function Bool is_32b_instr (Bit #(n) instr);
   return (instr [1:0] == 2'b11);
endfunction

function Bool is_16b_instr (Bit #(n) instr);
   return (instr [1:0] != 2'b11);
endfunction

// ================================================================
// Wrapper: wraps 32-bit aligned IMem to allow 16-bit instrs and 32-bit instrs,
// aligned to 16b or 32b boundaries.

module mkCPU_Fetch_C #(IMem_IFC  imem32) (IMem_IFC);

   // imem32 is the underlying 32-bit-aligned memory.
   // imem32.req is only given 32-bit aligned addrs,
   // and returns the 32b at that addr.
   // imem32.pc (output pc) is always the last requested address, always 32b-aligned.
   // ( Assert: imem32.is_32b_not_16b == True )

   Integer verbosity = 0;

   // This register holds the actual PC value (which can be +/-2 from
   // the address given to the ICache.
   Reg #(WordXL)  rg_pc  <- mkRegU;

   // These registers caches the last output of imem32 (imem32.pc, and imem32.instr [31:16])
   Reg #(WordXL)    rg_cache_addr <- mkReg ('1);
   Reg #(Bit #(16)) rg_cache_b16  <- mkRegU;

   // The following hold args of the 'req' method.
   Reg #(Bit #(3))  rg_f3          <- mkRegU;
   Reg #(Priv_Mode) rg_priv        <- mkRegU;
   Reg #(Bit #(1))  rg_sstatus_SUM <- mkRegU;
   Reg #(Bit #(1))  rg_mstatus_MXR <- mkRegU;
   Reg #(WordXL)    rg_satp        <- mkRegU;

   // Holds a faulting address
   Reg #(WordXL)    rg_tval        <- mkRegU;

   // ----------------------------------------------------------------
   // Conditions for selecting 16b and 32b instruction

   // Condition: 32b instr from {imem [15:0], rg_cache_b16}
   Bool cond_i32_odd = (   is_addr_odd16 (rg_pc)
			&& eq_b32_addr (rg_pc, rg_cache_addr)
			&& (imem32.pc == rg_pc + 2)
			&& is_32b_instr (rg_cache_b16));

   // Condition: 32b instr from imem [31:0]
   Bool cond_i32_even = (   is_addr_even16 (rg_pc)
			 && eq_b32_addr (rg_pc, imem32.pc)
			 && is_32b_instr (imem32.instr));

   // Condition: 16b instr from imem [31:16]
   Bool cond_i16_odd  = (   is_addr_odd16 (rg_pc)
			 && (   (   eq_b32_addr (rg_pc, imem32.pc)
				 && is_16b_instr (imem32.instr [31:16]))
			     || (   eq_b32_addr (rg_pc, rg_cache_addr)
				 && is_16b_instr (rg_cache_b16))));

   // Condition: 16b instr from imem [15:0]
   Bool cond_i16_even = (   is_addr_even16 (rg_pc)
			 && eq_b32_addr (rg_pc, imem32.pc)
			 && is_16b_instr (imem32.instr));

   // Condition: 32b instr from { ...next imem [15:0], imem [31:16] }
   Bool cond_i32_odd_fetch_next = (   is_addr_odd16 (rg_pc)
				   && imem32.valid
				   && eq_b32_addr (rg_pc, imem32.pc)
				   && is_32b_instr (imem32.instr [31:16]));

   // ----------------
   // Compose the 32b output 'instr' (either a 32b instr, or { 16'b0, 16b instr })

   function Bit #(32) fn_instr_out ();
      Bit #(32) instr_out = imem32.instr;
      if (cond_i32_odd)
	 instr_out = { imem32.instr [15:0], rg_cache_b16 };

      else if (cond_i16_even)
	 instr_out = { 16'b0, imem32.instr [15:0] };

      else if (cond_i16_odd) begin
	 if (eq_b32_addr (rg_pc, imem32.pc))
	    instr_out = { 16'b0, imem32.instr [31:16] };
	 else
	    instr_out = { 16'b0, rg_cache_b16 };
      end
      return instr_out;
   endfunction

   // ================================================================
   // Assert: The underlying imem32 only knows about 32-bit aligned words

   rule rl_assert_fail (! imem32.is_i32_not_i16);
      $display ("%0d: ERROR: CPU_Fetch_C: imem32.is_i32_not_i16 is False", cur_cycle);
      $finish (1);
   endrule

   rule rl_debug_conds (False && (verbosity != 0) && imem32.valid);
      $display ("%0d: imem32.pc 0x%0h  pc 0x%0h  rg_f3 %0d  imem32.instr 0x%08h  rg_cache_b16 0x%04h",
		cur_cycle, imem32.pc,  rg_pc,  rg_f3,  imem32.instr,  rg_cache_b16);
      $display ("    cond_i32_odd  = ", fshow (cond_i32_odd));
      $display ("    cond_i32_even = ", fshow (cond_i32_even));
      $display ("    cond_i16_odd  = ", fshow (cond_i16_odd));
      $display ("    cond_i16_even = ", fshow (cond_i16_even));
      $display ("    cond_i32_odd_fetch_next             = ", fshow (cond_i32_odd_fetch_next));
      $display ("    eq_b32_addr (rg_pc, imem32.pc)      = ", fshow (eq_b32_addr (rg_pc, imem32.pc)));
      $display ("    is_addr_odd16 (rg_pc)               = ", fshow (is_addr_odd16 (rg_pc)));
      $display ("    is_32b_instr (imem32.instr [31:16]) = ", fshow (is_32b_instr (imem32.instr [31:16])));
   endrule

   // ================================================================
   // When imem32.instr [31:15] has lower half of 32b instr, cache it and fetch next 32 bits

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_fetch_next_32b (imem32.valid && cond_i32_odd_fetch_next);
      Addr next_b32_addr = imem32.pc + 4;
      imem32.req (rg_f3, next_b32_addr, rg_priv, rg_sstatus_SUM, rg_mstatus_MXR, rg_satp);
      rg_cache_addr <= imem32.pc;
      rg_cache_b16  <= imem32.instr [31:16];
      rg_tval       <= next_b32_addr;

      if (verbosity != 0)
	 $display ("%0d: CPU_Fetch_C.rl_fetch_next_32b:  imem32.pc 0x%0h  next_b32_addr 0x%0h",
		   cur_cycle, imem32.pc, next_b32_addr);
   endrule

   // ================================================================
   // INTERFACE

   // CPU side: IMem request
   method Action  req (Bit #(3)   f3,
		       WordXL     addr,
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
      rg_tval        <= addr;

      // Cache the previous output, if valid
      if (imem32.valid) begin
	 rg_cache_addr     <= imem32.pc;
	 rg_cache_b16 <= imem32.instr [31:16];
      end

      WordXL addr_of_b32 = fn_to_b32_addr (addr);

      // Fetch next 32b word if request is odd-16b aligned, we've already got those 16b, and it's a 32b instr
      // (the 16b we've already got is saved in r16_cache_b16).
      // Note: since we know it's a 32b instr, this next fetch is not speculative, so ok if it page faults.
      if (   is_addr_odd16 (addr)
	  && imem32.valid
	  && (addr_of_b32 == imem32.pc)
	  && is_32b_instr (imem32.instr [31:16]))
	 begin
	    addr_of_b32 = addr_of_b32 + 4;
	 end

      imem32.req (f3, addr_of_b32, priv, sstatus_SUM, mstatus_MXR, satp);
      if (verbosity > 0) begin
	 $display ("CPU_Fetch_C.req: addr 0x%0h, addr_of_b32 0x%0h", addr, addr_of_b32);
      end
   endmethod

   // CPU side: IMem response
   method Bool     valid    = (imem32.valid && (   cond_i32_odd
						|| cond_i32_even
						|| cond_i16_odd
						|| cond_i16_even));

   method Bool     is_i32_not_i16 = (cond_i32_odd || cond_i32_even);

   method WordXL   pc       = rg_pc;
   method Instr    instr    = fn_instr_out ();
   method Bool     exc      = imem32.exc;
   method Exc_Code exc_code = imem32.exc_code;
   method WordXL   tval     = rg_tval;        // Can be different from rg_pc

endmodule

// ================================================================

endpackage
