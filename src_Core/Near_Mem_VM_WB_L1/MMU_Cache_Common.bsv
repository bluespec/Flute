// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package MMU_Cache_Common;

// ================================================================
// Types etc. shared by multiple modules in MMU_Cache complex.

// ================================================================
// Project imports

import ISA_Decls   :: *;
import Fabric_Defs :: *;

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

typedef enum { META_INVALID, META_SHARED, META_MODIFIED } Meta_State
deriving (Bits, Eq);

instance FShow #(Meta_State);
   function Fmt fshow (Meta_State s);
      Fmt fmt = $format ("Meta_State_UNKNOWN");
      case (s)
	 META_INVALID:   $format ("INVALID");
	 META_SHARED:    $format ("SHARED");
	 META_MODIFIED:  $format ("MODIFIED");
      endcase
   endfunction
endinstance

// ================================================================
// Requests and responses between:
//     L1_Cache <-> L2_Cache/MMU_Cache_AXI4_Adapter
//     MMIO     <-> MMU_Cache_AXI4_Adapter

// Line requests' addr represent a full line read/write.
// For now: we line-align the address, expect write data to start from
// offset 0, and return read-data from offset 0.
// TODO: 'wrapping' bursts, starting with actual line-offset of 'addr'

typedef struct {
   Bool       is_read;
   Bit #(64)  addr;
   } Line_Req
deriving (Bits, FShow);

// Single requests are from MMIO for 1, 2, 4 or 8 bytes.
typedef struct {
   Bool       is_read;
   Bit #(64)  addr;
   Bit #(2)   size_code;    // 2'b00=1 (B), 01=2 (H), 10=4 (W), 11=8 (D) bytes
   } Single_Req
deriving (Bits, FShow);

// Response from L2

typedef struct {
   Bool       ok;
   Bit #(64)  data;
   } Read_Data
deriving (Bits, FShow);

// Write-data is just Bit #(64) data; no need for a new type decl

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

function Bit #(64) fv_to_byte_lanes (Bit #(64) addr, Bit #(2) size_code, Bit #(64) data);
   Bit #(64) data1 = (data & fv_size_code_to_mask (size_code));
   return data1;
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
