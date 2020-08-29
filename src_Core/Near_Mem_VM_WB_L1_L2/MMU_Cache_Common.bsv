// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package MMU_Cache_Common;

// ================================================================
// Types etc. shared by multiple modules in MMU_Cache complex.

// ================================================================
// BSV lib imports

import Vector :: *;

// ================================================================
// Project imports

import ISA_Decls   :: *;
import Fabric_Defs :: *;
import Cache_Decls :: *;

// ================================================================
// Near_Mem opcodes

typedef enum {  CACHE_LD
	      , CACHE_ST
`ifdef ISA_A
	      , CACHE_AMO
`endif
   } CacheOp
deriving (Bits, Eq, FShow);

// ================================================================
// Requests from CPU to MMU_Cache

typedef struct {CacheOp    op;
		Bit #(3)   f3;
		WordXL     va;
		Bit #(64)  st_value;

`ifdef ISA_A
		Bit #(7)   amo_funct7;
`endif
`ifdef ISA_PRIV_S
		// The following are needed/used for VM translation only
		Priv_Mode  priv;
		Bit #(1)   sstatus_SUM;
		Bit #(1)   mstatus_MXR;
		WordXL     satp;           // = { VM_Mode, ASID, PPN_for_page_table }
`endif
   } MMU_Cache_Req
deriving (Bits, FShow);

function Fmt fshow_MMU_Cache_Req (MMU_Cache_Req req);
   Fmt fmt = $format ("MMU_Cache_Req{", fshow (req.op), " f3 %3b", req.f3);

`ifdef ISA_A
   if (req.op == CACHE_AMO) begin
      fmt = fmt + $format (" ", fshow_f5_AMO_op (req.amo_funct7 [6:2]));
      fmt = fmt + $format (" aqrl %2b", req.amo_funct7 [1:0]);
   end
`endif
   fmt = fmt + $format (" va %0h", req.va);

   Bool show_st_val = (req.op == CACHE_ST);
`ifdef ISA_A
   if ((req.op == CACHE_AMO) && (! fv_is_AMO_LR (req)))
      show_st_val = True;
`endif
   if (show_st_val) fmt = fmt + $format (" st_val %0h", req.st_value);

`ifdef ISA_PRIV_S
   fmt = fmt + $format (" priv %0d sstatus_SUM %0d mstatus_MXR %0d satp %0h",
			req.priv, req.sstatus_SUM, req.mstatus_MXR, req.satp);
`endif
   fmt = fmt + $format ("}");
   return fmt;
endfunction

// ================================================================
// Final result of VM translation.

// There are two versions below: the actual version when we support S
// Privilege Mode and a 'dummy' version when we don't.

`ifdef ISA_PRIV_S

typedef enum { VM_XLATE_OK, VM_XLATE_TLB_MISS, VM_XLATE_EXCEPTION } VM_Xlate_Outcome
deriving (Bits, Eq, FShow);

typedef struct {
   VM_Xlate_Outcome  outcome;
   PA                pa;            // phys addr, if VM_XLATE_OK
   Exc_Code          exc_code;      // if VM_XLATE_EXC

   // Information needed to write back updated PTE (A,D bits) to TLB and mem
   Bool              pte_modified;  // if VM_XLATE_OK and pte's A or D bits were modified
   PTE               pte;           // PTE (with possible A,D updates)
   Bit #(2)          pte_level;     // Level of leaf PTE for this translation
   PA                pte_pa;        // PA from which PTE was loaded
   } VM_Xlate_Result
deriving (Bits, FShow);

function Fmt fshow_VM_Xlate_Result (VM_Xlate_Result  r);
   Fmt fmt = $format ("VM_Xlate_Result{");
   fmt = fmt + fshow (r.outcome);
   if (r.outcome == VM_XLATE_OK) begin
      fmt = fmt + $format (" pa:%0h", r.pa);
      if (r.pte_modified)
	 fmt = fmt + $format (" pte (modified) %0h", r.pte);
   end
   else if (r.outcome == VM_XLATE_TLB_MISS) begin
   end
   else // exception
      fmt = fmt + $format (" ", fshow_trap_Exc_Code (r.exc_code));
   fmt = fmt + $format ("}");
   return fmt;
endfunction

// ----------------

`else // of ifdef ISA_PRIV_S

typedef enum { VM_XLATE_OK } VM_Xlate_Outcome
deriving (Bits, Eq, FShow);

typedef struct {
   VM_Xlate_Outcome   outcome;
   PA                 pa;            // phys addr, if VM_XLATE_OK
   } VM_Xlate_Result
deriving (Bits, FShow);

function Fmt fshow_VM_Xlate_Result (VM_Xlate_Result  r);
   Fmt fmt = $format ("VM_Xlate_Result{VM_XLATE_OK, pa:%0h}", r.pa);
   return fmt;
endfunction

`endif

// ================================================================
// Construct a PA from fields

function PA fn_CTag_and_CSet_to_CLine_PA (CTag  tag, CSet_in_Cache  cset);
   Byte_in_CLine  byte_in_cline = 0;
   return { tag, cset, byte_in_cline };
endfunction

// ================================================================
// Check if addr is aligned

function Bool fn_is_aligned (Bit #(2) size_code, Bit #(n) addr);
   return (    (size_code == 2'b00)                                // B
	   || ((size_code == 2'b01) && (addr [0] == 1'b0))         // H
	   || ((size_code == 2'b10) && (addr [1:0] == 2'b00))      // W
	   || ((size_code == 2'b11) && (addr [2:0] == 3'b000))     // D
	   );
endfunction

// ================================================================
// Convert width of an address from PA to Fabric_Addr

function Fabric_Addr fv_PA_to_Fabric_Addr (PA pa);
   Bit #(TAdd #(Wd_Addr, PA_sz)) fa = zeroExtend (pa);
   Integer hi = valueOf (Wd_Addr) - 1;
   return fa [hi:0];
endfunction

// ================================================================
// Classify AMO ops into LR, SC and the rest (read-modify-write ops)

function Bool fv_is_AMO_LR (MMU_Cache_Req req);
`ifdef ISA_A
   return ((req.op == CACHE_AMO) && (req.amo_funct7 [6:2] == f5_AMO_LR));
`else
   return False;
`endif
endfunction

function Bool fv_is_AMO_SC (MMU_Cache_Req req);
`ifdef ISA_A
   return ((req.op == CACHE_AMO) && (req.amo_funct7 [6:2] == f5_AMO_SC));
`else
   return False;
`endif
endfunction

function Bool fv_is_AMO_RMW (MMU_Cache_Req req);
`ifdef ISA_A
   return ((req.op == CACHE_AMO)
	   && (req.amo_funct7 [6:2] != f5_AMO_LR)
	   && (req.amo_funct7 [6:2] != f5_AMO_SC));
`else
   return False;
`endif
endfunction

// ================================================================
// Cache-line states (also used in coherence protocol): MESI

typedef enum { META_INVALID, META_SHARED, META_EXCLUSIVE, META_MODIFIED } Meta_State
deriving (Bits, Eq);

instance FShow #(Meta_State);
   function Fmt fshow (Meta_State s);
      Fmt fmt = $format ("Meta_State_UNKNOWN");
      case (s)
	 META_INVALID:   $format ("INVALID");
	 META_SHARED:    $format ("SHARED");
	 META_EXCLUSIVE: $format ("EXCLUSIVE");
	 META_MODIFIED:  $format ("MODIFIED");
      endcase
   endfunction
endinstance

instance Ord #(Meta_State);
   function Bool \< (Meta_State x, Meta_State y)          = (pack (x) < pack (y));
   function Bool \<= (Meta_State x, Meta_State y)         = (pack (x) <= pack (y));
   function Bool \> (Meta_State x, Meta_State y)          = (pack (x) > pack (y));
   function Bool \>= (Meta_State x, Meta_State y)         = (pack (x) >= pack (y));
   function Ordering compare (Meta_State x, Meta_State y) = compare (pack (x), pack (y));
   function Meta_State min (Meta_State x, Meta_State y)   = ((pack (x) <= pack (y)) ? x : y);
   function Meta_State max (Meta_State x, Meta_State y)   = ((pack (x) <= pack (y)) ? y : x);
endinstance

// ================================================================
// Requests from L1 to next-level cache/memory, and responses
// For now: address is line-aligned; data starts from offset 0.
// TODO: 'wrapping' bursts, starting with actual line-offset of 'addr'

// L1_to_L2_Req corresponds to L2's CRqMsg

typedef struct {
   Bit #(64)   addr;
   Meta_State  from_state;
   Meta_State  to_state;       // Upgrade requested
   Bool        can_up_to_E;
   // Bool       is_read;    // TODO DELETE
   // CLine      cline;      // TODO DELETE For requests where is_read is False (i.e., write request)
   } L1_to_L2_Req
deriving (Bits, FShow);

function Fmt fshow_L1_to_L2_Req (L1_to_L2_Req  req);
   Fmt fmt = $format ("L1_to_L2_Req {%0h", req.addr);
   fmt = fmt + $format (" ", fshow (req.from_state), "->", fshow (req.to_state));
   if (req.can_up_to_E)
      fmt = fmt + $format (" can_up_to_E");
   fmt = fmt + $format ("}");
   return fmt;
endfunction

// L2_to_L1_Rsp corresponds to L2's PRqRsMsg.PRs

typedef struct {
   Bit #(64)       addr;
   Meta_State      to_state;   // Upgraded state
   Maybe #(CLine)  m_cline;    // possible write-back data
   // id                       // Future (when L1 becomes non-blocking, out-of-order

   // Bool       ok;       // TODO DELETE
   // CLine      cline;    // TODO DELETE For requests where is_read is True (i.e., read request)
   } L2_to_L1_Rsp
deriving (Bits, FShow);

function Fmt fshow_L2_to_L1_Rsp (L2_to_L1_Rsp rsp);
   Fmt fmt = $format ("L2_to_L1_Rsp %0h -> ", rsp.addr, fshow (rsp.to_state));
   if (rsp.m_cline matches tagged Valid .cline) begin
      Vector #(CWords_per_CLine, Bit #(64)) v_cword = unpack (cline);
      for (Integer j = 0; j < cwords_per_cline; j = j + 1)
	 fmt = fmt + $format ("\n        [%0d]  %016h", j, v_cword [j]);
   end
   else
      fmt = fmt + $format (" <no line>");
   return fmt;
endfunction

// ================================================================
// Requests from next-level cache/memory to L1, and responses
// These are downgrade requests (M/E/S -> S/I)
// If downgrading from M, response is write-back cache line

// L2_to_L1_Req corresponds to L2's PRqRsMsg.PRq

typedef struct {
   Bit #(64)   addr;
   Meta_State  to_state;    // Downgrade demanded
   // id                    // TODO: Future (when L1 becomes non-blocking, out-of-order
   } L2_to_L1_Req
deriving (Bits, FShow);

function Fmt fshow_L2_to_L1_Req (L2_to_L1_Req  req);
   Fmt fmt = $format ("L2_to_L1_Req {%0h", req.addr);
   fmt = fmt + $format ("->", fshow (req.to_state));
   fmt = fmt + $format ("}");
   return fmt;
endfunction

// L1_to_L2_Rsp corresponds to L2's CRsMsg

typedef struct {
   Bit #(64)       addr;
   Meta_State      to_state;    // Downgrade result
   Maybe #(CLine)  m_cline;
   } L1_to_L2_Rsp
deriving (Bits, FShow);

function Fmt fshow_L1_to_L2_Rsp (L1_to_L2_Rsp rsp);
   Fmt fmt = $format ("L1_to_L2_Rsp %0h -> ", rsp.addr, fshow (rsp.to_state));
   if (rsp.m_cline matches tagged Valid .cline) begin
      Vector #(CWords_per_CLine, Bit #(64)) v_cword = unpack (cline);
      for (Integer j = 0; j < cwords_per_cline; j = j + 1)
	 fmt = fmt + $format ("\n        [%0d]  %016h", j, v_cword [j]);
   end
   else
      fmt = fmt + $format (" <no line>");
   return fmt;
endfunction

// ================================================================
// Requests and responses between:
//     MMIO <-> MMU_Cache_AXI4_Adapter

// Single requests are from MMIO for 1, 2, 4 or 8 bytes.
typedef struct {
   Bool       is_read;
   Bit #(64)  addr;
   Bit #(2)   size_code;    // 2'b00=1 (B), 01=2 (H), 10=4 (W), 11=8 (D) bytes
   Bit #(64)  data;         // For requests where is_read is False (i.e., write request)
   } Single_Req
deriving (Bits, FShow);

// Response (for a Single_Req write-request, there's no response, i.e., 'fire-and-forget')

typedef struct {
   Bool       ok;
   Bit #(64)  data;         // For requests where is_read is True (i.e., read request)
   } Single_Rsp
deriving (Bits, FShow);

// ================================================================
// Functions to/from lsb-justified data to fabric-lane-aligned data

function Bit #(64) fv_size_code_to_mask (Bit #(2) size_code);
   Bit #(64) mask = case (size_code)
		       2'b00: 'h_0000_0000_0000_00FF;
		       2'b01: 'h_0000_0000_0000_FFFF;
		       2'b10: 'h_0000_0000_FFFF_FFFF;
		       2'b11: 'h_FFFF_FFFF_FFFF_FFFF;
		    endcase;
   return mask;
endfunction

function Bit #(64) fv_from_byte_lanes (Bit #(64)  addr,
				       Bit #(2)   size_code,
				       Bit #(64)  data);
   Bit #(6)  shamt = { addr [2:0], 3'b0 };
   Bit #(64) data1 = (data >> shamt);

   return (data1 & fv_size_code_to_mask (size_code));
endfunction

function Bit #(64) fv_extend (Bit #(3) f3, Bit #(64) data);
   Bit #(64) mask     = fv_size_code_to_mask (f3 [1:0]);
   Bit #(1)  sign_bit = case (f3 [1:0])
			   2'b00: data  [7];
			   2'b01: data [15];
			   2'b10: data [31];
			   2'b11: data [63];
			endcase;
   Bit #(64) result;
   if ((f3 [2] == 1'b0) && (sign_bit == 1'b1))
      result = data | (~ mask);    // sign extend
   else
      result = data & mask;        // zero extend

   return result;
endfunction

// ================================================================
// ALU for AMO ops.
// Args: ld_val (64b from mem) and st_val (64b from CPU reg Rs2)
// Result: (final_ld_val, final_st_val)
//
// All args and results are in LSBs (i.e., not lane-aligned).
// final_ld_val includes sign-extension (if necessary).
// final_st_val is output of the binary AMO op

function Tuple2 #(Bit #(64),
		  Bit #(64)) fv_amo_op (Bit #(2)   size_code, // 2'b10=W, 11=D
					Bit #(5)   funct5,    // encodes the AMO op
					Bit #(64)  ld_val,    // 64b value loaded from mem
					Bit #(64)  st_val);   // 64b value from CPU reg Rs2
   Bit #(64) w1     = ld_val;
   Bit #(64) w2     = st_val;
   Int #(64) i1     = unpack (w1);    // Signed, for signed ops
   Int #(64) i2     = unpack (w2);    // Signed, for signed ops
   if (size_code == 2'b10) begin
      w1 = zeroExtend (w1 [31:0]);
      w2 = zeroExtend (w2 [31:0]);
      i1 = unpack (signExtend (w1 [31:0]));
      i2 = unpack (signExtend (w2 [31:0]));
   end
   Bit #(64) final_st_val = ?;
   case (funct5)
      f5_AMO_SWAP: final_st_val = w2;
      f5_AMO_ADD:  final_st_val = pack (i1 + i2);
      f5_AMO_XOR:  final_st_val = w1 ^ w2;
      f5_AMO_AND:  final_st_val = w1 & w2;
      f5_AMO_OR:   final_st_val = w1 | w2;
      f5_AMO_MINU: final_st_val = ((w1 < w2) ? w1 : w2);
      f5_AMO_MAXU: final_st_val = ((w1 > w2) ? w1 : w2);
      f5_AMO_MIN:  final_st_val = ((i1 < i2) ? w1 : w2);
      f5_AMO_MAX:  final_st_val = ((i1 > i2) ? w1 : w2);
   endcase

   if (size_code == 2'b10)
      final_st_val = zeroExtend (final_st_val [31:0]);

   return tuple2 (truncate (pack (i1)), final_st_val);
endfunction: fv_amo_op

// ================================================================

endpackage
