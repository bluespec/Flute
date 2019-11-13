// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

// ================================================================
// Definition of Tandem Verifier Packets.
// The CPU sends out such a packet for each instruction retired.
// A Tandem Verifier contains a "golden model" simulator of the RISC-V
// ISA, and verifies that the information in the packet is correct,
// instruction by instruction.

// ================================================================

package TV_Info;

// ================================================================
// Bluespec library imports

import DefaultValue :: *;
import Vector       :: *;

// ================================================================
// Project imports

import ISA_Decls    :: *;

// ================================================================

typedef enum {// These are not from instruction flow and do not have a PC or instruction
	      TRACE_RESET,
	      TRACE_GPR_WRITE,
	      TRACE_FPR_WRITE,
	      TRACE_CSR_WRITE,
	      TRACE_MEM_WRITE,

	      // These are from instruction flow and have a PC and instruction
	      TRACE_OTHER,
	      TRACE_I_RD,   TRACE_F_GRD,   TRACE_F_FRD,
 	      TRACE_I_LOAD,  TRACE_F_LOAD,
	      TRACE_I_STORE, TRACE_F_STORE,
	      TRACE_AMO,
	      TRACE_TRAP,
	      TRACE_RET,
	      TRACE_CSRRX,

	      // These are from an interrupt and has a PC but no instruction
	      TRACE_INTR
	      } Trace_Op
deriving (Bits, Eq, FShow);

typedef struct {
   Trace_Op   op;
   WordXL     pc;
   ISize      instr_sz;
   Bit #(32)  instr;
   RegName    rd;
   WordXL     word1;
   WordXL     word2;
   Bit #(64)  word3;    // Wider than WordXL because can contain paddr (in RV32, paddr can be 34 bits)
   WordXL     word4;
`ifdef ISA_F
   WordFL     word5;
`endif
   } Trace_Data
deriving (Bits);

// RESET
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x
function Trace_Data mkTrace_RESET ();
   Trace_Data td = ?;
   td.op       = TRACE_RESET;
   return td;
endfunction

// GPR_WRITE
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x                                 x    rdval
function Trace_Data mkTrace_GPR_WRITE (RegName rd, WordXL rdval);
   Trace_Data td = ?;
   td.op       = TRACE_GPR_WRITE;
   td.rd       = rd;
   td.word1    = rdval;
   return td;
endfunction

// FPR_WRITE
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x                                 x    rdval
function Trace_Data mkTrace_FPR_WRITE (RegName rd, WordXL rdval);
   Trace_Data td = ?;
   td.op       = TRACE_FPR_WRITE;
   td.rd       = rd;
   td.word1    = rdval;
   return td;
endfunction

// CSR_WRITE
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x                                                        csraddr  csrval
function Trace_Data mkTrace_CSR_WRITE (CSR_Addr csraddr, WordXL csrval);
   Trace_Data td = ?;
   td.op       = TRACE_CSR_WRITE;
   td.word3    = zeroExtend (csraddr);
   td.word4    = csrval;
   return td;
endfunction

// MEM_WRITE
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x                                         sz    stval    paddr
function Trace_Data mkTrace_MEM_WRITE (MemReqSize sz, WordXL stval, Bit #(64) paddr);
   Trace_Data td = ?;
   td.op       = TRACE_MEM_WRITE;
   td.word1    = zeroExtend (sz);
   td.word2    = stval;
   td.word3    = paddr;
   return td;
endfunction

// OTHER
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x
function Trace_Data mkTrace_OTHER (WordXL pc, ISize isize, Bit #(32) instr);
   Trace_Data td = ?;
   td.op       = TRACE_OTHER;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   return td;
endfunction

// I_RD
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x        x     rdval
function Trace_Data mkTrace_I_RD (WordXL pc, ISize isize, Bit #(32) instr, RegName rd, WordXL rdval);
   Trace_Data td = ?;
   td.op       = TRACE_I_RD;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = rd;
   td.word1    = rdval;
   return td;
endfunction

`ifdef ISA_F
// F_FRD
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4    word5
// x     x     x           x        x              fflags            mstatus  rdval
function Trace_Data mkTrace_F_FRD (WordXL pc, ISize isize, Bit #(32) instr, RegName rd, WordFL rdval, Bit#(5) fflags, WordXL mstatus);
   Trace_Data td = ?;
   td.op       = TRACE_F_FRD;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = rd;
   td.word2    = extend (fflags);
   td.word4    = mstatus;
   td.word5    = rdval;
   return td;
endfunction

// F_GRD
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4    word5
// x     x     x           x        x     rdval    fflags            mstatus
function Trace_Data mkTrace_F_GRD (WordXL pc, ISize isize, Bit #(32) instr, RegName rd, WordXL rdval, Bit#(5) fflags, WordXL mstatus);
   Trace_Data td = ?;
   td.op       = TRACE_F_GRD;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = rd;
   td.word1    = rdval;
   td.word2    = extend (fflags);
   td.word4    = mstatus;
   return td;
endfunction
`endif

// I_LOAD
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x        x     rdval             eaddr
function Trace_Data mkTrace_I_LOAD (WordXL pc, ISize isize, Bit #(32) instr, RegName rd, WordXL rdval, WordXL eaddr);
   Trace_Data td = ?;
   td.op       = TRACE_I_LOAD;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = rd;
   td.word1    = rdval;
   td.word3    = zeroExtend (eaddr);
   return td;
endfunction

// STORE
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x              funct3   stval    eaddr
function Trace_Data mkTrace_I_STORE (WordXL pc, Bit #(3) funct3, ISize isize, Bit #(32) instr, WordXL stval, WordXL eaddr);
   Trace_Data td = ?;
   td.op       = TRACE_I_STORE;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.word1    = zeroExtend (funct3);
   td.word2    = stval;
   td.word3    = zeroExtend (eaddr);
   return td;
endfunction

`ifdef ISA_F
// F_LOAD
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4    word5
// x     x     x           x        x                       eaddr    mstatus  rdval
function Trace_Data mkTrace_F_LOAD (WordXL pc, ISize isize, Bit #(32) instr, RegName rd, WordFL rdval, WordXL eaddr, WordXL mstatus);
   Trace_Data td = ?;
   td.op       = TRACE_F_LOAD;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = rd;
   td.word3    = zeroExtend (eaddr);
   td.word4    = mstatus;
   td.word5    = rdval;
   return td;
endfunction

// F_STORE
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4    word5
// x     x     x           x              funct3            eaddr             stval
function Trace_Data mkTrace_F_STORE (WordXL pc, Bit #(3) funct3, ISize isize, Bit #(32) instr, WordFL stval, WordXL eaddr);
   Trace_Data td = ?;
   td.op       = TRACE_F_STORE;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.word3    = zeroExtend (eaddr);
   td.word5    = stval;
   return td;
endfunction

function Trace_Data fv_trace_update_mstatus_fs (Trace_Data td, Bit #(2) fs);
   let ntd = td;
   ntd.word4 = fv_assign_bits (td.word4, fromInteger (mstatus_fs_bitpos), fs);
   return (ntd);
endfunction

function Trace_Data fv_trace_update_fcsr_fflags (Trace_Data td, Bit #(5) fflags);
   let ntd = td;
   ntd.word2 = (td.word2 | extend (fflags));
   return (ntd);
endfunction
`endif

// AMO
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x        x     rdval    stval    eaddr    funct3
function Trace_Data mkTrace_AMO (WordXL pc, Bit #(3) funct3, ISize isize, Bit #(32) instr,
				 RegName rd, WordXL rdval, WordXL stval, WordXL eaddr);
   Trace_Data td = ?;
   td.op       = TRACE_AMO;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = rd;
   td.word1    = rdval;
   td.word2    = stval;
   td.word3    = zeroExtend (eaddr);
   td.word4    = zeroExtend (funct3);
   return td;
endfunction

// TRAP
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x        priv  mstatus  mcause   mepc     mtval
function Trace_Data mkTrace_TRAP (WordXL pc, ISize isize, Bit #(32) instr,
				  Priv_Mode  priv, WordXL mstatus, WordXL mcause, WordXL mepc, WordXL mtval);
   Trace_Data td = ?;
   td.op       = TRACE_TRAP;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = zeroExtend (priv);
   td.word1    = mstatus;
   td.word2    = mcause;
   td.word3    = zeroExtend (mepc);
   td.word4    = mtval;
   return td;
endfunction

// RET
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x        priv  mstatus
function Trace_Data mkTrace_RET (WordXL pc, ISize isize, Bit #(32) instr, Priv_Mode  priv, WordXL mstatus);
   Trace_Data td = ?;
   td.op       = TRACE_RET;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = zeroExtend (priv);
   td.word1    = mstatus;
   return td;
endfunction

// CSRRX
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x     x           x        x     rdval    csrvalid csraddr  csrval
function Trace_Data mkTrace_CSRRX (WordXL pc, ISize isize, Bit #(32) instr,
				   RegName rd, WordXL rdval, Bool csrvalid, CSR_Addr csraddr, WordXL csrval);
   Trace_Data td = ?;
   td.op       = TRACE_CSRRX;
   td.pc       = pc;
   td.instr_sz = isize;
   td.instr    = instr;
   td.rd       = rd;
   td.word1    = rdval;
   td.word2    = (csrvalid ? 1 : 0);
   td.word3    = zeroExtend (csraddr);
   td.word4    = csrval;
   return td;
endfunction

// INTR
// op    pc    instr_sz    instr    rd    word1    word2    word3    word4
// x     x                          priv  mstatus  mcause   mepc     mtval
function Trace_Data mkTrace_INTR (WordXL pc,
				  Priv_Mode  priv, WordXL mstatus, WordXL mcause, WordXL mepc, WordXL mtval);
   Trace_Data td = ?;
   td.op       = TRACE_INTR;
   td.pc       = pc;
   td.rd       = zeroExtend (priv);
   td.word1    = mstatus;
   td.word2    = mcause;
   td.word3    = zeroExtend (mepc);
   td.word4    = mtval;
   return td;
endfunction

// ================================================================
// Display of Trace_Data for debugging

instance FShow #(Trace_Data);
   function Fmt fshow (Trace_Data td);
      Fmt fmt = $format ("Trace_Data{", fshow (td.op));

      if (td.op == TRACE_RESET) begin
      end

      else if ((td.op == TRACE_GPR_WRITE) || (td.op == TRACE_FPR_WRITE))
	 fmt = fmt + $format (" rd %0d  rdval %0h", td.rd, td.word1);

      else if (td.op == TRACE_CSR_WRITE)
	 fmt = fmt + $format (" csraddr %0h  csrval %0h", td.word3, td.word4);

      else if (td.op == TRACE_MEM_WRITE)
	 fmt = fmt + $format (" sz %0d  stval %0h  paddr %0h", td.word1, td.word2, td.word3);

      else begin
	 fmt = fmt + $format (" pc %0h", td.pc);

	 if (td.op != TRACE_INTR)
	    fmt = fmt + $format (" instr.%0d %0h:", pack (td.instr_sz), td.instr);

	 if (td.op == TRACE_I_RD)
	    fmt = fmt + $format (" rd %0d  rdval %0h", td.rd, td.word1);
`ifdef ISA_F
	 else if (td.op == TRACE_F_FRD)
	    fmt = fmt + $format (" rd %0d  rdval %0h  fflags %05b", td.rd, td.word5, td.word2);

	 else if (td.op == TRACE_F_GRD)
	    fmt = fmt + $format (" rd %0d  rdval %0h  fflags %05b", td.rd, td.word1, td.word2);

	 else if (td.op == TRACE_F_LOAD)
	    fmt = fmt + $format (" rd %0d  rdval %0h  eaddr %0h",
				 td.rd, td.word5, td.word3);

	 else if (td.op == TRACE_F_STORE)
	    fmt = fmt + $format (" stval %0h  eaddr %0h", td.word5, td.word3);
`endif
	 else if (td.op == TRACE_I_LOAD)
	    fmt = fmt + $format (" rd %0d  rdval %0h  eaddr %0h",
				 td.rd, td.word1, td.word3);

	 else if (td.op == TRACE_I_STORE)
	    fmt = fmt + $format (" stval %0h  eaddr %0h", td.word2, td.word3);

	 else if (td.op == TRACE_AMO)
	    fmt = fmt + $format (" rd %0d  rdval %0h  stval %0h  eaddr %0h",
				 td.rd, td.word1, td.word2, td.word3);

	 else if (td.op == TRACE_CSRRX)
	    fmt = fmt + $format (" rd %0d  rdval %0h  csraddr %0h  csrval %0h",
				 td.rd, td.word1, td.word3, td.word4);

	 else if ((td.op == TRACE_TRAP) || (td.op == TRACE_INTR))
	    fmt = fmt + $format (" priv %0d  mstatus %0h  mcause %0h  mepc %0h  mtval %0h",
				 td.rd, td.word1, td.word2, td.word3, td.word4);

	 else if (td.op == TRACE_RET)
	    fmt = fmt + $format (" priv %0d  mstatus %0h", td.rd, td.word1);
      end

      fmt = fmt + $format ("}");
      return fmt;
   endfunction
endinstance

// ================================================================
// Trace_Data is encoded in module mkTV_Encode into vectors of bytes,
// which are eventually streamed out to an on-line tandem verifier/
// analyzer (or to a file for off-line tandem-verification/analysis).

// Various 'transactions' produce a Trace_Data struct (e.g., reset,
// each instruction retirement, each GDB write to registers or memory,
// etc.).  Each struct is encoded into a vector of bytes; the number
// of bytes depends on the kind of transaction and various encoding
// choices.

typedef 72   TV_VB_SIZE;    // max bytes needed for each transaction
typedef  Vector #(TV_VB_SIZE, Byte)  TV_Vec_Bytes;

// ================================================================

typedef struct {
   Bit #(32)     num_bytes;
   TV_Vec_Bytes  vec_bytes;
} Info_CPU_to_Verifier deriving (Bits, FShow);

// ================================================================

endpackage
