// Copyright (c) 2013-2022 Bluespec, Inc. All Rights Reserved

// ================================================================
// ISA defs for UC Berkeley RISC V
//
// References (from riscv.org):
//     The RISC-V Instruction Set Manual
//     Volume I: Unprivileged ISA
//     Document Version 20181106-Base-Ratification
//     November 6, 2018
//
//     The RISC-V Instruction Set Manual
//     Volume II: Privileged Architecture
//     Document Version 20181203-Base-Ratification
//     December 3, 2018
//
// ================================================================

package ISA_Decls;

// ================================================================
// BSV library imports

import DefaultValue :: *;
import Vector       :: *;
import BuildVector  :: *;

// ================================================================
// BSV project imports

// None

// ================================================================

typedef 3 NO_OF_PRIVMODES;

// ================================================================
// XLEN and related constants

`ifdef RV32

typedef 32 XLEN;

`elsif RV64

typedef 64 XLEN;

`endif

typedef TMul #(2, XLEN)  XLEN_2;      // Double-width for multiplications
typedef TSub #(XLEN, 2)  XLEN_MINUS_2;// XLEN-2 for MTVEC base width

Integer xlen = valueOf (XLEN);

typedef enum { RV32, RV64 } RV_Version deriving (Eq, Bits);

RV_Version rv_version = ( (valueOf (XLEN) == 32) ? RV32 : RV64 );

// ----------------
// We're evolving the code to use WordXL/IntXL instead of Word/Word_S
// because of the widespread and inconsistent use of 'word' in the field.
// All existing uses of 'Word/Word_S' should migrate towards WordXL/IntXL.
// All new code should only use WordXL/IntXL
// Eventually, we should remove Word and Word_S

typedef  Bit #(XLEN)  WordXL;    // Raw (unsigned) register data
typedef  Int #(XLEN)  IntXL;     // Signed register data

typedef  Bit #(XLEN)  Word;      // Raw (unsigned) register data    // OLD: migrate to WordXL
typedef  Int #(XLEN)  Word_S;    // Signed register data            // OLD: migrate to IntXL

typedef  WordXL       Addr;      // addresses/pointers

// ----------------

typedef  8                                     Bits_per_Byte;
typedef  Bit #(Bits_per_Byte)                  Byte;

typedef  XLEN                                  Bits_per_Word;            // REDUNDANT to XLEN

typedef  TDiv #(Bits_per_Word, Bits_per_Byte)  Bytes_per_Word;           // OLD ('WordXL')
typedef  TLog #(Bytes_per_Word)                Bits_per_Byte_in_Word;    // OLD ('WordXL')
typedef  Bit #(Bits_per_Byte_in_Word)          Byte_in_Word;             // OLD ('WordXL')
typedef  Vector #(Bytes_per_Word, Byte)        Word_B;                   // OLD ('WordXL')

typedef  TDiv #(XLEN, Bits_per_Byte)           Bytes_per_WordXL;
typedef  TLog #(Bytes_per_WordXL)              Bits_per_Byte_in_WordXL;
typedef  Bit #(Bits_per_Byte_in_WordXL)        Byte_in_WordXL;
typedef  Vector #(Bytes_per_WordXL, Byte)      WordXL_B;

typedef  XLEN                                  Bits_per_Addr;
typedef  TDiv #(Bits_per_Addr, Bits_per_Byte)  Bytes_per_Addr;

Integer  bits_per_byte           = valueOf (Bits_per_Byte);

Integer  bytes_per_wordxl        = valueOf (Bytes_per_WordXL);
Integer  bits_per_byte_in_wordxl = valueOf (Bits_per_Byte_in_WordXL);

Integer  addr_lo_byte_in_wordxl = 0;
Integer  addr_hi_byte_in_wordxl = addr_lo_byte_in_wordxl + bits_per_byte_in_wordxl - 1;

function  Byte_in_Word  fn_addr_to_byte_in_wordxl (Addr a);
   return a [addr_hi_byte_in_wordxl : addr_lo_byte_in_wordxl ];
endfunction

// ================================================================
// FLEN and related constants, for floating point data
// Can have one or two fpu sizes (should they be merged sooner than later ?).

// ISA_D => ISA_F (ISA_D implies ISA_F)
// The combination ISA_D and !ISA_F is not permitted

// ISA_F - 32 bit FPU
// ISA_D - 64 bit FPU

`ifdef ISA_F

`ifdef ISA_D
typedef 64 FLEN;
Bool hasFpu32 = False;
Bool hasFpu64 = True;
`else
typedef 32 FLEN;
Bool hasFpu32 = True;
Bool hasFpu64 = False;
`endif

typedef  Bit #(FLEN) FP_Value;
typedef  Bit #(FLEN)  WordFL;    // Floating point data

typedef  TDiv #(FLEN, Bits_per_Byte)           Bytes_per_WordFL;
typedef  TLog #(Bytes_per_WordFL)              Bits_per_Byte_in_WordFL;
typedef  Bit #(Bits_per_Byte_in_WordFL)        Byte_in_WordFL;
typedef  Vector #(Bytes_per_WordFL, Byte)      WordFL_B;

`endif

// ================================================================
// Tokens are used for signalling/synchronization, and have no payload

typedef Bit #(0) Token;

// ================================================================
// Instruction fields

// This is used for encoding Tandem Verifier traces
typedef enum { ISIZE16BIT, ISIZE32BIT
   } ISize deriving (Bits, Eq, FShow);

typedef  Bit #(32)  Instr;
typedef  Bit #(7)   Opcode;
typedef  Bit #(5)   RegName;       // 32 registers, 0..31
typedef  32         NumRegs;
Integer  numRegs = valueOf (NumRegs);

Instr  illegal_instr = 32'h0000_0000;

function  Opcode     instr_opcode   (Instr x); return x [6:0]; endfunction

function  Bit #(2)   instr_funct2   (Instr x); return x [26:25]; endfunction
function  Bit #(3)   instr_funct3   (Instr x); return x [14:12]; endfunction
function  Bit #(5)   instr_funct5   (Instr x); return x [31:27]; endfunction
function  Bit #(7)   instr_funct7   (Instr x); return x [31:25]; endfunction
function  Bit #(10)  instr_funct10  (Instr x); return { x [31:25], x [14:12] }; endfunction
function  Bit #(2)   instr_fmt      (Instr x); return x [26:25]; endfunction

function  RegName    instr_rd       (Instr x); return x [11:7]; endfunction
function  RegName    instr_rs1      (Instr x); return x [19:15]; endfunction
function  RegName    instr_rs2      (Instr x); return x [24:20]; endfunction
function  RegName    instr_rs3      (Instr x); return x [31:27]; endfunction
function  CSR_Addr   instr_csr      (Instr x); return unpack(x [31:20]); endfunction

function  Bit #(12)  instr_I_imm12  (Instr x);
   return x [31:20];
endfunction

function  Bit #(12)  instr_S_imm12  (Instr x);
   return { x [31:25], x [11:7] };
endfunction

function  Bit #(13)  instr_SB_imm13 (Instr x);
   return { x [31], x [7], x [30:25], x [11:8], 1'b0 };
endfunction

function  Bit #(20)  instr_U_imm20  (Instr x);
   return x [31:12];
endfunction

function  Bit #(21)  instr_UJ_imm21 (Instr x);
   return { x [31], x [19:12], x [20], x [30:21], 1'b0 };
endfunction

// For FENCE decode
function  Bit #(4)   instr_pred (Instr x); return x [27:24]; endfunction
function  Bit #(4)   instr_succ (Instr x); return x [23:20]; endfunction

// For AMO decode
function  Bit #(2)   instr_aqrl (Instr x); return x [26:25]; endfunction

// ----------------
// Decoded instructions

typedef struct {
   Opcode    opcode;

   RegName   rd;
   RegName   rs1;
   RegName   rs2;
   RegName   rs3;
   CSR_Addr  csr;

   Bit #(3)  funct3;
   Bit #(5)  funct5;
   Bit #(7)  funct7;
   Bit #(10) funct10;

   Bit #(12) imm12_I;
   Bit #(12) imm12_S;
   Bit #(13) imm13_SB;
   Bit #(20) imm20_U;
   Bit #(21) imm21_UJ;

   Bit #(4)  pred;
   Bit #(4)  succ;

   Bit #(2)  aqrl;
   } Decoded_Instr
deriving (FShow, Bits);

function Decoded_Instr fv_decode (Instr instr);
   return Decoded_Instr {opcode:    instr_opcode (instr),

			 rd:        instr_rd       (instr),
			 rs1:       instr_rs1      (instr),
			 rs2:       instr_rs2      (instr),
			 rs3:       instr_rs3      (instr),
			 csr:       instr_csr      (instr),

			 funct3:    instr_funct3   (instr),
			 funct5:    instr_funct5   (instr),
			 funct7:    instr_funct7   (instr),
			 funct10:   instr_funct10  (instr),

			 imm12_I:   instr_I_imm12  (instr),
			 imm12_S:   instr_S_imm12  (instr),
			 imm13_SB:  instr_SB_imm13 (instr),
			 imm20_U:   instr_U_imm20  (instr),
			 imm21_UJ:  instr_UJ_imm21 (instr),

			 pred:      instr_pred     (instr),
			 succ:      instr_succ     (instr),

			 aqrl:      instr_aqrl     (instr)
			 };
endfunction

// Decodes if we need to read the GPR register file. This step becomes necessary
// on integrating the FPU as certain instruction now do not require the GPR
// anymore
//                IsFP, GPRRd
// function Tuple2# (Bool, Bool) fv_decode_gpr_read (Decoded_Instr di);
// `ifdef ISA_F
//    // FP_LD and FP_ST are treated as non-FP operation as far as GPR reads
//    // are concerned
//    if (di.opcode != op_FP) begin
//       return (tuple2 (False, True));   // Regular op with GPR read
//    end
// 
//    // This is an FP operation. The following f5 values would work for F and
//    // D subsets
//    else begin
//       if (   (di.funct5 == f5_FCVT_F_X)
//           || (di.funct5 == f5_FMV_W_X))
//          return (tuple2 (True, True)); // FP op with GPR read
//       else
//          return (tuple2 (True, False));// FP op with no GPR read
//    end
// `else
//    return (tuple2 (False, True));      // Regular op with GPR read
// `endif
// endfunction

// ================================================================
// Instruction constructors
// Used in 'C' decode to construct equivalent 32-bit instructions

// R-type
function Instr  mkInstr_R_type (Bit #(7) funct7, RegName rs2, RegName rs1, Bit #(3) funct3, RegName rd, Bit #(7) opcode);
   let instr = { funct7, rs2, rs1, funct3, rd, opcode };
   return instr;
endfunction

// I-type
function Instr  mkInstr_I_type (Bit #(12) imm12, RegName rs1, Bit #(3) funct3, RegName rd, Bit #(7) opcode);
   let instr = { imm12, rs1, funct3, rd, opcode };
   return instr;
endfunction

// S-type

function Instr  mkInstr_S_type (Bit #(12) imm12, RegName rs2, RegName rs1, Bit #(3) funct3, Bit #(7) opcode);
   let instr = { imm12 [11:5], rs2, rs1, funct3, imm12 [4:0], opcode };
   return instr;
endfunction

// B-type
function Instr  mkInstr_B_type (Bit #(13) imm13, RegName rs2, RegName rs1, Bit #(3) funct3, Bit #(7) opcode);
   let instr = { imm13 [12], imm13 [10:5], rs2, rs1, funct3, imm13 [4:1], imm13 [11], opcode };
   return instr;
endfunction

// U-type
function Instr  mkInstr_U_type (Bit #(20) imm20, RegName rd, Bit #(7) opcode);
   let instr = { imm20, rd, opcode };
   return instr;
endfunction

// J-type
function Instr  mkInstr_J_type (Bit #(21) imm21, RegName rd, Bit #(7) opcode);
   let instr = { imm21 [20], imm21 [10:1], imm21 [11], imm21 [19:12], rd, opcode };
   return instr;
endfunction

// ================================================================
// Symbolic register names

RegName x0  =  0;    RegName x1  =  1;    RegName x2  =  2;    RegName x3  =  3;
RegName x4  =  4;    RegName x5  =  5;    RegName x6  =  6;    RegName x7  =  7;
RegName x8  =  8;    RegName x9  =  9;    RegName x10 = 10;    RegName x11 = 11;
RegName x12 = 12;    RegName x13 = 13;    RegName x14 = 14;    RegName x15 = 15;
RegName x16 = 16;    RegName x17 = 17;    RegName x18 = 18;    RegName x19 = 19;
RegName x20 = 20;    RegName x21 = 21;    RegName x22 = 22;    RegName x23 = 23;
RegName x24 = 24;    RegName x25 = 25;    RegName x26 = 26;    RegName x27 = 27;
RegName x28 = 28;    RegName x29 = 29;    RegName x30 = 30;    RegName x31 = 31;

// Register names used in calling convention

RegName reg_zero =  0;
RegName reg_ra   =  1;
RegName reg_sp   =  2;
RegName reg_gp   =  3;
RegName reg_tp   =  4;

RegName reg_t0   =  5; RegName reg_t1  =  6; RegName reg_t2 =  7;
RegName reg_fp   =  8;
RegName reg_s0   =  8; RegName reg_s1  =  9;

RegName reg_a0   = 10; RegName reg_a1  = 11;
RegName reg_v0   = 10; RegName reg_v1  = 11;

RegName reg_a2   = 12; RegName reg_a3  = 13; RegName reg_a4 = 14; RegName reg_a5 = 15;
RegName reg_a6   = 16; RegName reg_a7  = 17;

RegName reg_s2   = 18; RegName reg_s3  = 19; RegName reg_s4 = 20; RegName reg_s5 = 21;
RegName reg_s6   = 22; RegName reg_s7  = 23; RegName reg_s8 = 24; RegName reg_s9 = 25;
RegName reg_s10  = 26; RegName reg_s11 = 27;

RegName reg_t3   = 28; RegName reg_t4  = 29; RegName reg_t5 = 30; RegName reg_t6 = 31;

// ----------------
// Is 'r' a standard register for PC save/restore on call/return?
// This function is used in branch-predictors for managing the return-address stack.

function Bool fn_reg_is_link (RegName  r);
   return ((r == x1) || (r == x5));
endfunction

// ================================================================
// Kinds of memory access (excluding AMOs)

typedef enum { Access_RWX_R, Access_RWX_W, Access_RWX_X } Access_RWX
deriving (Eq, Bits, FShow);

// ================================================================
// Data sizes for LOAD/STORE

typedef enum {BITS8,
	      BITS16,
	      BITS32,
	      BITS64    // Even in RV32, to allow for Double (floating point)
   } Mem_Data_Size
deriving (Eq, Bits, FShow);

// ================================================================
// LOAD/STORE instructions

typedef Bit #(2) MemReqSize;

MemReqSize f3_SIZE_B = 2'b00;
MemReqSize f3_SIZE_H = 2'b01;
MemReqSize f3_SIZE_W = 2'b10;
MemReqSize f3_SIZE_D = 2'b11;

// ----------------
// Load instructions

Opcode op_LOAD = 7'b00_000_11;

Bit #(3) f3_LB  = 3'b000;
Bit #(3) f3_LH  = 3'b001;
Bit #(3) f3_LW  = 3'b010;
Bit #(3) f3_LD  = 3'b011;
Bit #(3) f3_LBU = 3'b100;
Bit #(3) f3_LHU = 3'b101;
Bit #(3) f3_LWU = 3'b110;

// ----------------
// Store instructions

Opcode op_STORE = 7'b01_000_11;

Bit #(3) f3_SB  = 3'b000;
Bit #(3) f3_SH  = 3'b001;
Bit #(3) f3_SW  = 3'b010;
Bit #(3) f3_SD  = 3'b011;

// ================================================================
// Memory Model

Opcode op_MISC_MEM = 7'b00_011_11;

Bit #(3) f3_FENCE   = 3'b000;
Bit #(3) f3_FENCE_I = 3'b001;

typedef struct {
   // Predecessors
   Bool pi;    // IO reads
   Bool po;    // IO writes
   Bool pr;    // Mem reads
   Bool pw;    // Mem writes
   // Successors
   Bool si;
   Bool so;
   Bool sr;
   Bool sw;
   } Fence_Ordering
deriving (FShow);

instance Bits #(Fence_Ordering, 8);
   function Bit #(8) pack (Fence_Ordering fo);
      return {pack (fo.pi),
	      pack (fo.po),
	      pack (fo.pr),
	      pack (fo.pw),
	      pack (fo.si),
	      pack (fo.so),
	      pack (fo.sr),
	      pack (fo.sw) };
   endfunction
   function Fence_Ordering unpack (Bit #(8) b8);
      return Fence_Ordering {pi: unpack (b8 [7]),
			     po: unpack (b8 [6]),
			     pr: unpack (b8 [5]),
			     pw: unpack (b8 [4]),
			     si: unpack (b8 [3]),
			     so: unpack (b8 [2]),
			     sr: unpack (b8 [1]),
			     sw: unpack (b8 [0]) };
   endfunction
endinstance

function Bit #(4) fv_instr_to_fence_fm (Bit #(32) instr) = instr [31:28];

Bit #(4) fence_fm_none = 4'b0000;
Bit #(4) fence_fm_TSO  = 4'b1000;

// ================================================================
// Atomic Memory Operation Instructions

Opcode op_AMO = 7'b01_011_11;

// NOTE: bit [4] for aq, and [3] for rl, are here set to zero

Bit #(3)    f3_AMO_W     = 3'b010;
Bit #(3)    f3_AMO_D     = 3'b011;

Bit #(5)    f5_AMO_LR     = 5'b00010;
Bit #(5)    f5_AMO_SC     = 5'b00011;
Bit #(5)    f5_AMO_ADD    = 5'b00000;
Bit #(5)    f5_AMO_SWAP   = 5'b00001;
Bit #(5)    f5_AMO_XOR    = 5'b00100;
Bit #(5)    f5_AMO_AND    = 5'b01100;
Bit #(5)    f5_AMO_OR     = 5'b01000;
Bit #(5)    f5_AMO_MIN    = 5'b10000;
Bit #(5)    f5_AMO_MAX    = 5'b10100;
Bit #(5)    f5_AMO_MINU   = 5'b11000;
Bit #(5)    f5_AMO_MAXU   = 5'b11100;

function Fmt fshow_f5_AMO_op (Bit #(5) op);
   Fmt fmt = case (op)
		f5_AMO_LR: $format ("LR");
		f5_AMO_SC: $format ("SC");
		f5_AMO_ADD: $format ("ADD");
		f5_AMO_SWAP: $format ("SWAP");
		f5_AMO_XOR: $format ("XOR");
		f5_AMO_AND: $format ("AND");
		f5_AMO_OR: $format ("OR");
		f5_AMO_MIN: $format ("MIN");
		f5_AMO_MAX: $format ("MAX");
		f5_AMO_MINU: $format ("MINU");
		f5_AMO_MAXU: $format ("MAXU");
	     endcase;
   return fmt;
endfunction

Bit #(10) f10_LR_W       = 10'b00010_00_010;
Bit #(10) f10_SC_W       = 10'b00011_00_010;
Bit #(10) f10_AMOADD_W   = 10'b00000_00_010;
Bit #(10) f10_AMOSWAP_W  = 10'b00001_00_010;
Bit #(10) f10_AMOXOR_W   = 10'b00100_00_010;
Bit #(10) f10_AMOAND_W   = 10'b01100_00_010;
Bit #(10) f10_AMOOR_W    = 10'b01000_00_010;
Bit #(10) f10_AMOMIN_W   = 10'b10000_00_010;
Bit #(10) f10_AMOMAX_W   = 10'b10100_00_010;
Bit #(10) f10_AMOMINU_W  = 10'b11000_00_010;
Bit #(10) f10_AMOMAXU_W  = 10'b11100_00_010;

Bit #(10) f10_LR_D       = 10'b00010_00_011;
Bit #(10) f10_SC_D       = 10'b00011_00_011;
Bit #(10) f10_AMOADD_D   = 10'b00000_00_011;
Bit #(10) f10_AMOSWAP_D  = 10'b00001_00_011;
Bit #(10) f10_AMOXOR_D   = 10'b00100_00_011;
Bit #(10) f10_AMOAND_D   = 10'b01100_00_011;
Bit #(10) f10_AMOOR_D    = 10'b01000_00_011;
Bit #(10) f10_AMOMIN_D   = 10'b10000_00_011;
Bit #(10) f10_AMOMAX_D   = 10'b10100_00_011;
Bit #(10) f10_AMOMINU_D  = 10'b11000_00_011;
Bit #(10) f10_AMOMAXU_D  = 10'b11100_00_011;

// ================================================================
// Integer Register-Immediate Instructions

Opcode op_OP_IMM = 7'b00_100_11;

Bit #(3) f3_ADDI  = 3'b000;
Bit #(3) f3_SLLI  = 3'b001;
Bit #(3) f3_SLTI  = 3'b010;
Bit #(3) f3_SLTIU = 3'b011;
Bit #(3) f3_XORI  = 3'b100;
Bit #(3) f3_SRxI  = 3'b101; Bit #(3) f3_SRLI  = 3'b101; Bit #(3) f3_SRAI  = 3'b101;
Bit #(3) f3_ORI   = 3'b110;
Bit #(3) f3_ANDI  = 3'b111;

// ================================================================
// Integer Register-Immediate 32b Instructions for RV64

Opcode op_OP_IMM_32 = 7'b00_110_11;

Bit #(3) f3_ADDIW = 3'b000;
Bit #(3) f3_SLLIW = 3'b001;
Bit #(3) f3_SRxIW = 3'b101; Bit #(3) f3_SRLIW = 3'b101; Bit #(3) f3_SRAIW = 3'b101;

// OP_IMM.SLLI/SRLI/SRAI for RV32
Bit #(7)  msbs7_SLLI = 7'b_000_0000;
Bit #(7)  msbs7_SRLI = 7'b_000_0000;
Bit #(7)  msbs7_SRAI = 7'b_010_0000;

// OP_IMM.SLLI/SRLI/SRAI for RV64
Bit #(6)  msbs6_SLLI = 6'b_00_0000;
Bit #(6)  msbs6_SRLI = 6'b_00_0000;
Bit #(6)  msbs6_SRAI = 6'b_01_0000;

// ================================================================
// Integer Register-Register Instructions

Opcode op_OP = 7'b01_100_11;

Bit #(10) f10_ADD    = 10'b000_0000_000;
Bit #(10) f10_SUB    = 10'b010_0000_000;
Bit #(10) f10_SLL    = 10'b000_0000_001;
Bit #(10) f10_SLT    = 10'b000_0000_010;
Bit #(10) f10_SLTU   = 10'b000_0000_011;
Bit #(10) f10_XOR    = 10'b000_0000_100;
Bit #(10) f10_SRL    = 10'b000_0000_101;
Bit #(10) f10_SRA    = 10'b010_0000_101;
Bit #(10) f10_OR     = 10'b000_0000_110;
Bit #(10) f10_AND    = 10'b000_0000_111;

Bit #(7) funct7_ADD  = 7'b_000_0000;    Bit #(3) funct3_ADD = 3'b_000;
Bit #(7) funct7_SUB  = 7'b_010_0000;    Bit #(3) funct3_SUB = 3'b_000;
Bit #(7) funct7_XOR  = 7'b_000_0000;    Bit #(3) funct3_XOR = 3'b_100;
Bit #(7) funct7_OR   = 7'b_000_0000;    Bit #(3) funct3_OR  = 3'b_110;
Bit #(7) funct7_AND  = 7'b_000_0000;    Bit #(3) funct3_AND = 3'b_111;

// ----------------
// MUL/DIV/REM family

Bit #(7) f7_MUL_DIV_REM = 7'b000_0001;

function Bool f7_is_OP_MUL_DIV_REM (Bit #(7) f7);
   return (f7 == f7_MUL_DIV_REM);
endfunction

Bit #(3) f3_MUL    = 3'b000;
Bit #(3) f3_MULH   = 3'b001;
Bit #(3) f3_MULHSU = 3'b010;
Bit #(3) f3_MULHU  = 3'b011;
Bit #(3) f3_DIV    = 3'b100;
Bit #(3) f3_DIVU   = 3'b101;
Bit #(3) f3_REM    = 3'b110;
Bit #(3) f3_REMU   = 3'b111;

Bit #(10) f10_MUL    = 10'b000_0001_000;
Bit #(10) f10_MULH   = 10'b000_0001_001;
Bit #(10) f10_MULHSU = 10'b000_0001_010;
Bit #(10) f10_MULHU  = 10'b000_0001_011;
Bit #(10) f10_DIV    = 10'b000_0001_100;
Bit #(10) f10_DIVU   = 10'b000_0001_101;
Bit #(10) f10_REM    = 10'b000_0001_110;
Bit #(10) f10_REMU   = 10'b000_0001_111;

// ================================================================
// Integer Register-Register 32b Instructions for RV64

Opcode op_OP_32 = 7'b01_110_11;

Bit #(10) f10_ADDW   = 10'b000_0000_000;
Bit #(10) f10_SUBW   = 10'b010_0000_000;
Bit #(10) f10_SLLW   = 10'b000_0000_001;
Bit #(10) f10_SRLW   = 10'b000_0000_101;
Bit #(10) f10_SRAW   = 10'b010_0000_101;

Bit #(7) funct7_ADDW = 7'b_000_0000;    Bit #(3) funct3_ADDW  = 3'b_000;
Bit #(7) funct7_SUBW = 7'b_010_0000;    Bit #(3) funct3_SUBW  = 3'b_000;

Bit #(10) f10_MULW   = 10'b000_0001_000;
Bit #(10) f10_DIVW   = 10'b000_0001_100;
Bit #(10) f10_DIVUW  = 10'b000_0001_101;
Bit #(10) f10_REMW   = 10'b000_0001_110;
Bit #(10) f10_REMUW  = 10'b000_0001_111;

function Bool is_OP_32_MUL_DIV_REM (Bit #(10) f10);
   return (   (f10 == f10_MULW)
	   || (f10 == f10_DIVW)
	   || (f10 == f10_DIVUW)
	   || (f10 == f10_REMW)
	   || (f10 == f10_REMUW));
endfunction

// ================================================================
// LUI, AUIPC

Opcode op_LUI   = 7'b01_101_11;
Opcode op_AUIPC = 7'b00_101_11;

// ================================================================
// Control transfer

Opcode  op_BRANCH = 7'b11_000_11;

Bit #(3) f3_BEQ   = 3'b000;
Bit #(3) f3_BNE   = 3'b001;
Bit #(3) f3_BLT   = 3'b100;
Bit #(3) f3_BGE   = 3'b101;
Bit #(3) f3_BLTU  = 3'b110;
Bit #(3) f3_BGEU  = 3'b111;

Opcode op_JAL  = 7'b11_011_11;

Opcode op_JALR = 7'b11_001_11;
Bit #(3) funct3_JALR = 3'b000;

`ifdef ISA_F
// ================================================================
// Floating Point Instructions

// ----------------------------------------------------------------
// TODO: the following are FPU implementation choices; shouldn't be in ISA_Decls
// Enumeration of floating point opcodes for decode within the FPU
typedef enum {FPAdd,
	      FPSub,
	      FPMul,
	      FPDiv,
	      FPSqrt,
	      FPMAdd,
	      FPMSub,
	      FPNMAdd,
	      FPNMSub
   } FpuOp
   deriving (Bits, Eq, FShow);

// Enumeration of rounding modes
typedef enum {Rnd_Nearest_Even,
	      Rnd_Zero,
	      Rnd_Minus_Inf,
	      Rnd_Plus_Inf,
	      Rnd_Nearest_Max_Mag
   } RoundMode
   deriving (Bits, Eq, FShow);

// ----------------------------------------------------------------
// Floating point Load-Store

Opcode   op_LOAD_FP  = 7'b_00_001_11;
Opcode   op_STORE_FP = 7'b_01_001_11;

Bit #(3) f3_FSW = 3'b010;
Bit #(3) f3_FLW = 3'b010;

Bit #(3) f3_FSD = 3'b011;
Bit #(3) f3_FLD = 3'b011;

// ----------------------------------------------------------------
// Fused FP Multiply Add/Sub instructions (FM/FNM)

Opcode   op_FMADD  = 7'b10_00_011;
Opcode   op_FMSUB  = 7'b10_00_111;
Opcode   op_FNMSUB = 7'b10_01_011;
Opcode   op_FNMADD = 7'b10_01_111;

Bit #(2) f2_S = 2'b00;
Bit #(2) f2_D = 2'b01;
Bit #(2) f2_Q = 2'b11;

// ----------------------------------------------------------------
// All other FP intructions

Opcode  op_FP = 7'b10_10_011;

// ----------------
// RV32F

Bit #(7) f7_FADD_S      = 7'h0 ;
Bit #(7) f7_FSUB_S      = 7'h4 ;
Bit #(7) f7_FMUL_S      = 7'h8 ;
Bit #(7) f7_FDIV_S      = 7'hC ;
Bit #(7) f7_FSQRT_S     = 7'h2C; Bit #(5) rs2_FSQRT_S   = 5'b00000;

Bit #(7) f7_FSGNJ_S     = 7'h10;                                    Bit #(3) funct3_FSGNJ_S  = 3'b000;
Bit #(7) f7_FSGNJN_S    = 7'h10;                                    Bit #(3) funct3_FSGNJN_S = 3'b001;
Bit #(7) f7_FSGNJX_S    = 7'h10;                                    Bit #(3) funct3_FSGNJX_S = 3'b010;

Bit #(7) f7_FMIN_S      = 7'h14;                                    Bit #(3) funct3_FMIN_S   = 3'b000;
Bit #(7) f7_FMAX_S      = 7'h14;                                    Bit #(3) funct3_FMAX_S   = 3'b001;

Bit #(7) f7_FCVT_W_S    = 7'h60; Bit #(5) rs2_FCVT_W_S  = 5'b00000;
Bit #(7) f7_FCVT_WU_S   = 7'h60; Bit #(5) rs2_FCVT_WU_S = 5'b00001;
Bit #(7) f7_FMV_X_W     = 7'h70; Bit #(5) rs2_FMV_X_W   = 5'b00000; Bit #(3) funct3_FMV_X_W  = 3'b000;

Bit #(7) f7_FCMP_S      = 7'h50;
Bit #(7) f7_FEQ_S       = 7'h50;                                    Bit #(3) funct3_FEQ_S    = 3'b010;
Bit #(7) f7_FLT_S       = 7'h50;                                    Bit #(3) funct3_FLT_S    = 3'b001;
Bit #(7) f7_FLE_S       = 7'h50;                                    Bit #(3) funct3_FLE_S    = 3'b000;

Bit #(7) f7_FCLASS_S    = 7'h70; Bit #(5) rs2_FCLASS_S  = 5'b00000; Bit #(3) funct3_FCLASS_S = 3'b001;
Bit #(7) f7_FCVT_S_W    = 7'h68; Bit #(5) rs2_FCVT_S_W  = 5'b00000;
Bit #(7) f7_FCVT_S_WU   = 7'h68; Bit #(5) rs2_FCVT_S_WU = 5'b00001;
Bit #(7) f7_FMV_W_X     = 7'h78; Bit #(5) rs2_FMV_W_X   = 5'b00000; Bit #(3) funct3_FMV_W_X  = 3'b000;

// ----------------
// RV64F

Bit #(7) f7_FCVT_L_S    = 7'h60; Bit #(5) rs2_FCVT_L_S  = 5'b00010;
Bit #(7) f7_FCVT_LU_S   = 7'h60; Bit #(5) rs2_FCVT_LU_S = 5'b00011;
Bit #(7) f7_FCVT_S_L    = 7'h68; Bit #(5) rs2_FCVT_S_L  = 5'b00010;
Bit #(7) f7_FCVT_S_LU   = 7'h68; Bit #(5) rs2_FCVT_S_LU = 5'b00011;

// ----------------
// RV32D

Bit #(7) f7_FADD_D      = 7'h1 ;
Bit #(7) f7_FSUB_D      = 7'h5 ;
Bit #(7) f7_FMUL_D      = 7'h9 ;
Bit #(7) f7_FDIV_D      = 7'hD ;
Bit #(7) f7_FSQRT_D     = 7'h2D; Bit #(5) rs2_FSQRT_D  = 5'b00000;

Bit #(7) f7_FSGNJ_D     = 7'h11;                                    Bit #(3) funct3_FSGNJ_D  = 3'b000;
Bit #(7) f7_FSGNJN_D    = 7'h11;                                    Bit #(3) funct3_FSGNJN_D = 3'b001;
Bit #(7) f7_FSGNJX_D    = 7'h11;                                    Bit #(3) funct3_FSGNJX_D = 3'b010;

Bit #(7) f7_FMIN_D      = 7'h15;                                    Bit #(3) funct3_FMIN_D   = 3'b000;
Bit #(7) f7_FMAX_D      = 7'h15;                                    Bit #(3) funct3_FMAX_D   = 3'b001;

Bit #(7) f7_FCVT_S_D    = 7'h20; Bit #(5) rs2_FCVT_S_D = 5'b00001;
Bit #(7) f7_FCVT_D_S    = 7'h21; Bit #(5) rs2_FCVT_D_S = 5'b00000;

Bit #(7) f7_FCMP_D      = 7'h51;
Bit #(7) f7_FEQ_D       = 7'h51;                                    Bit #(3) funct3_FEQ_D    = 3'b010;
Bit #(7) f7_FLT_D       = 7'h51;                                    Bit #(3) funct3_FLT_D    = 3'b001;
Bit #(7) f7_FLE_D       = 7'h51;                                    Bit #(3) funct3_FLE_D    = 3'b000;

Bit #(7) f7_FCLASS_D    = 7'h71; Bit #(5) rs2_FCLASS_D  = 5'b00000; Bit #(3) funct3_FCLASS_D = 3'b001;
Bit #(7) f7_FCVT_W_D    = 7'h61; Bit #(5) rs2_FCVT_W_D  = 5'b00000;
Bit #(7) f7_FCVT_WU_D   = 7'h61; Bit #(5) rs2_FCVT_WU_D = 5'b00001;
Bit #(7) f7_FCVT_D_W    = 7'h69; Bit #(5) rs2_FCVT_D_W  = 5'b00000;
Bit #(7) f7_FCVT_D_WU   = 7'h69; Bit #(5) rs2_FCVT_D_WU = 5'b00001;

// ----------------
// RV64D

Bit #(7) f7_FCVT_L_D    = 7'h61; Bit #(5) rs2_FCVT_L_D  = 5'b00010;
Bit #(7) f7_FCVT_LU_D   = 7'h61; Bit #(5) rs2_FCVT_LU_D = 5'b00011;
Bit #(7) f7_FMV_X_D     = 7'h71; Bit #(5) rs2_FMV_X_D   = 5'b00000; Bit #(3) funct3_FMV_X_D = 3'b000;
Bit #(7) f7_FCVT_D_L    = 7'h69; Bit #(5) rs2_FCVT_D_L  = 5'b00010;
Bit #(7) f7_FCVT_D_LU   = 7'h69; Bit #(5) rs2_FCVT_D_LU = 5'b00011;
Bit #(7) f7_FMV_D_X     = 7'h79; Bit #(5) rs2_FMV_D_X   = 5'b00000; Bit #(3) funct3_FMV_D_X = 3'b000;

// ----------------------------------------------------------------
// fv_is_rd_in_GPR: Checks if the request generates a result which
// should be written into the GPR
function Bool fv_is_rd_in_GPR (Bit #(7) funct7, RegName rs2);

`ifdef ISA_D
    let is_FCVT_W_D  =    (funct7 == f7_FCVT_W_D)
                       && (rs2 == 0);
    let is_FCVT_WU_D =    (funct7 == f7_FCVT_WU_D)
                       && (rs2 == 1);
`ifdef RV64
    let is_FCVT_L_D  =    (funct7 == f7_FCVT_L_D)
                       && (rs2 == 2);
    let is_FCVT_LU_D =    (funct7 == f7_FCVT_LU_D)
                       && (rs2 == 3);

`endif
   // FCLASS.D also maps to this -- both write to GPR
   let is_FMV_X_D    =    (funct7 == f7_FMV_X_D);
   // FEQ.D, FLE.D, FLT.D map to this
   let is_FCMP_D     =    (funct7 == f7_FCMP_D);
`endif

    let is_FCVT_W_S  =    (funct7 == f7_FCVT_W_S)
                       && (rs2 == 0);
    let is_FCVT_WU_S =    (funct7 == f7_FCVT_WU_S)
                       && (rs2 == 1);
`ifdef RV64
    let is_FCVT_L_S  =    (funct7 == f7_FCVT_L_S)
                       && (rs2 == 2);
    let is_FCVT_LU_S =    (funct7 == f7_FCVT_LU_S)
                       && (rs2 == 3);
`endif

   // FCLASS.S also maps to this -- both write to GPR
   let is_FMV_X_W    =    (funct7 == f7_FMV_X_W);

   // FEQ.S, FLE.S, FLT.S map to this
   let is_FCMP_S     =    (funct7 == f7_FCMP_S);

    return (
          False
`ifdef ISA_D
       || is_FCVT_W_D
       || is_FCVT_WU_D
`ifdef RV64
       || is_FCVT_L_D
       || is_FCVT_LU_D
`endif
       || is_FMV_X_D
       || is_FCMP_D
`endif
`ifdef RV64
       || is_FCVT_L_S
       || is_FCVT_LU_S
`endif
       || is_FCVT_W_S
       || is_FCVT_WU_S
       || is_FMV_X_W
       || is_FCMP_S
    );
endfunction

// Check if a rounding mode value in the FCSR.FRM is valid
function Bool fv_fcsr_frm_valid (Bit #(3) frm);
   return (   (frm != 3'b101) 
           && (frm != 3'b110)
           && (frm != 3'b111)
          );
endfunction 

// Check if a rounding mode value in the instr is valid
function Bool fv_inst_frm_valid (Bit #(3) frm);
   return (   (frm != 3'b101) 
           && (frm != 3'b110)
          );
endfunction

// fv_rounding_mode_check
// Returns the correct rounding mode considering the values in the
// FCSR and the instruction and checks legality
function Tuple2# (Bit #(3), Bool) fv_rmode_check (
   Bit #(3) inst_frm, Bit #(3) fcsr_frm);
   let rm = (inst_frm == 3'h7) ? fcsr_frm : inst_frm;
   let rm_is_legal  = (inst_frm == 3'h7) ? fv_fcsr_frm_valid (fcsr_frm)
                                         : fv_inst_frm_valid (inst_frm);
   return (tuple2 (rm, rm_is_legal));
endfunction

// TODO: Check misa.f and misa.d
function Bool fv_is_fp_instr_legal (Bit #(7) funct7,
				    Bit #(3) rm,
				    RegName  rs2,
				    Opcode   opcode);
   // These compile-time constants (which will be optimized out) avoid ugly ifdefs later
   Bool rv64 = False;
`ifdef RV64
   rv64 = True;
`endif

   Bool isa_F = False;
   Bool isa_D = False;
`ifdef ISA_F
   isa_F = True;
`ifdef ISA_D
   isa_D = True;
`endif
`endif

   // ----------------
   // For FM.../FNM... check funct7 [1:0] (i.e., instr[26:25])
   Bool ok_instr_26_25 = (isa_D    // Both SP and DP are legal
			  ? ((funct7 [1:0] == f2_S) || (funct7 [1:0] == f2_D))
			  : (isa_F    // Only SP is legal
			     ? (funct7 [1:0] == f2_S)
			     : False));
   Bool is_legal_FM_FNM = (   ok_instr_26_25
			   && (   (opcode == op_FMADD )
			       || (opcode == op_FMSUB )
			       || (opcode == op_FNMADD)
			       || (opcode == op_FNMSUB)));
   // ----------------
   Bool is_legal_other_RV32F
   = (   isa_F
      && (opcode == op_FP)
      && (   (funct7== f7_FADD_S)
	  || (funct7== f7_FSUB_S)
	  || (funct7== f7_FMUL_S)
`ifdef INCLUDE_FDIV
	  || (funct7== f7_FDIV_S)
`endif
`ifdef INCLUDE_FSQRT
	  || ((funct7== f7_FSQRT_S)   && (rs2 == rs2_FSQRT_S))
`endif
	  || ((funct7== f7_FSGNJ_S)                              && (rm == funct3_FSGNJ_S))
	  || ((funct7== f7_FSGNJN_S)                             && (rm == funct3_FSGNJN_S))
	  || ((funct7== f7_FSGNJX_S)                             && (rm == funct3_FSGNJX_S))
	  || ((funct7== f7_FMIN_S)                               && (rm == funct3_FMIN_S))
	  || ((funct7== f7_FMAX_S)                               && (rm == funct3_FMAX_S))
	  || ((funct7== f7_FCVT_W_S)  && (rs2 == rs2_FCVT_W_S))
	  || ((funct7== f7_FCVT_WU_S) && (rs2 == rs2_FCVT_WU_S))
	  || ((funct7== f7_FMV_X_W)   && (rs2 == rs2_FMV_X_W)    && (rm == funct3_FMV_X_W))
	  || ((funct7== f7_FEQ_S)     &&                            (rm == funct3_FEQ_S))
	  || ((funct7== f7_FLT_S)     &&                            (rm == funct3_FLT_S))
	  || ((funct7== f7_FLE_S)     &&                            (rm == funct3_FLE_S))
	  || ((funct7== f7_FCLASS_S)  && (rs2 == rs2_FCLASS_S)   && (rm == funct3_FCLASS_S))
	  || ((funct7== f7_FCVT_S_W)  && (rs2 == rs2_FCVT_S_W))
	  || ((funct7== f7_FCVT_S_WU) && (rs2 == rs2_FCVT_S_WU))
	  || ((funct7== f7_FMV_W_X)   && (rs2 == rs2_FMV_W_X)    && (rm == funct3_FMV_W_X))
	 ));

   // ----------------
   Bool is_legal_other_RV64F
   = (   isa_F
      && (opcode == op_FP)
      && rv64
      && (   ((funct7== f7_FCVT_L_S)  && (rs2 == rs2_FCVT_L_S))
	  || ((funct7== f7_FCVT_LU_S) && (rs2 == rs2_FCVT_LU_S))
	  || ((funct7== f7_FCVT_S_L)  && (rs2 == rs2_FCVT_S_L))
	  || ((funct7== f7_FCVT_S_LU) && (rs2 == rs2_FCVT_S_LU))
	  ));

   // ----------------
   Bool is_legal_other_RV32D
   = (   isa_D
      && (opcode == op_FP)
      && (   (funct7== f7_FADD_D)
	  || (funct7== f7_FSUB_D)
	  || (funct7== f7_FMUL_D)
`ifdef INCLUDE_FDIV
	  || (funct7== f7_FDIV_D)
`endif
`ifdef INCLUDE_FSQRT
	  || ((funct7== f7_FSQRT_D)   && (rs2 == rs2_FSQRT_D))
`endif
	  || ((funct7== f7_FSGNJ_D)                              && (rm == funct3_FSGNJ_D))
	  || ((funct7== f7_FSGNJN_D)                             && (rm == funct3_FSGNJN_D))
	  || ((funct7== f7_FSGNJX_D)                             && (rm == funct3_FSGNJX_D))
	  || ((funct7== f7_FMIN_D)                               && (rm == funct3_FMIN_D))
	  || ((funct7== f7_FMAX_D)                               && (rm == funct3_FMAX_D))
	  || ((funct7== f7_FCVT_S_D)  && (rs2 == rs2_FCVT_S_D))
	  || ((funct7== f7_FCVT_D_S)  && (rs2 == rs2_FCVT_D_S))
	  || ((funct7== f7_FEQ_D)                                && (rm == funct3_FEQ_D))
	  || ((funct7== f7_FLT_D)                                && (rm == funct3_FLT_D))
	  || ((funct7== f7_FLE_D)                                && (rm == funct3_FLE_D))
	  || ((funct7== f7_FCLASS_D)  && (rs2 == rs2_FCLASS_D))  && (rm == funct3_FCLASS_D)
	  || ((funct7== f7_FCVT_W_D)  && (rs2 == rs2_FCVT_W_D))
	  || ((funct7== f7_FCVT_WU_D) && (rs2 == rs2_FCVT_WU_D))
	  || ((funct7== f7_FCVT_D_W)  && (rs2 == rs2_FCVT_D_W))
	  || ((funct7== f7_FCVT_D_WU) && (rs2 == rs2_FCVT_D_WU))
	  ));

   // ----------------
   Bool is_legal_other_RV64D
   = (   isa_D
      && (opcode == op_FP)
      && rv64
      && (   ((funct7== f7_FCVT_L_D)  && (rs2 == rs2_FCVT_L_D))
	  || ((funct7== f7_FCVT_LU_D) && (rs2 == rs2_FCVT_LU_D))
	  || ((funct7== f7_FMV_X_D)   && (rs2 == rs2_FMV_X_D))   && (rm == funct3_FMV_X_D)
	  || ((funct7== f7_FCVT_D_L)  && (rs2 == rs2_FCVT_D_L))
	  || ((funct7== f7_FCVT_D_LU) && (rs2 == rs2_FCVT_D_LU))
	  || ((funct7== f7_FMV_D_X)   && (rs2 == rs2_FMV_D_X))   && (rm == funct3_FMV_D_X)
	  ));

   // ----------------
   return (   is_legal_FM_FNM
	   || is_legal_other_RV32F
	   || is_legal_other_RV64F
	   || is_legal_other_RV32D
	   || is_legal_other_RV64D);
endfunction

/* DELETE (OLD): radical rewrite (with more error-checking) above
// A D instruction requires misa.f to be set as well as misa.d
function Bool fv_is_fp_instr_legal (
   Bit #(7) f7, Bit #(3) rm, RegName rs2, Opcode fopc);
   Bit #(2) f2 = f7[1:0];
   Bool is_legal = True;
   if (   (fopc == op_FMADD )
       || (fopc == op_FMSUB )
       || (fopc == op_FNMADD)
       || (fopc == op_FNMSUB))
`ifdef ISA_D
      return ((f2 == f2_S) || (f2 == f2_D)); // Both SP and DP are legal
`else
      return (f2 == f2_S);                   // Only SP is legal
`endif
   else
      if (    (f7 == f7_FADD_S)  
          ||  (f7 == f7_FSUB_S)  
          ||  (f7 == f7_FMUL_S)  
`ifdef INCLUDE_FDIV
          ||  (f7 == f7_FDIV_S)  
`endif
`ifdef INCLUDE_FSQRT
          ||  (f7 == f7_FSQRT_S) 
`endif
          || ((f7 == f7_FSGNJ_S)  && ( rm == 0))
          || ((f7 == f7_FSGNJ_S)  && ( rm == 1))
          || ((f7 == f7_FSGNJ_S)  && ( rm == 2))
          || ((f7 == f7_FCVT_W_S) && (rs2 == 0))
          || ((f7 == f7_FCVT_WU_S)&& (rs2 == 1))
`ifdef RV64
          || ((f7 == f7_FCVT_L_S) && (rs2 == 2))
          || ((f7 == f7_FCVT_LU_S)&& (rs2 == 3))
`endif                            
          || ((f7 == f7_FCVT_S_W) && (rs2 == 0))
          || ((f7 == f7_FCVT_S_WU)&& (rs2 == 1))             
`ifdef RV64                       
          || ((f7 == f7_FCVT_S_L) && (rs2 == 2))
          || ((f7 == f7_FCVT_S_LU)&& (rs2 == 3))
`endif
          || ((f7 == f7_FMIN_S)   && ( rm == 0))
          || ((f7 == f7_FMAX_S)   && ( rm == 1))
          || ((f7 == f7_FCMP_S)   && ( rm == 0))
          || ((f7 == f7_FCMP_S)   && ( rm == 1))
          || ((f7 == f7_FCMP_S)   && ( rm == 2))
          || ((f7 == f7_FMV_X_W)  && ( rm == 0))
          || ((f7 == f7_FMV_W_X)  && ( rm == 0))
          || ((f7 == f7_FCLASS_S) && ( rm == 1))
`ifdef ISA_D
          ||  (f7 == f7_FADD_D)  
          ||  (f7 == f7_FSUB_D)  
          ||  (f7 == f7_FMUL_D)  
`ifdef INCLUDE_FDIV
          ||  (f7 == f7_FDIV_D)  
`endif
`ifdef INCLUDE_FSQRT
          ||  (f7 == f7_FSQRT_D) 
`endif
          || ((f7 == f7_FSGNJ_D)  && ( rm == 0))
          || ((f7 == f7_FSGNJ_D)  && ( rm == 1))
          || ((f7 == f7_FSGNJ_D)  && ( rm == 2))
          || ((f7 == f7_FCVT_W_D) && (rs2 == 0))
          || ((f7 == f7_FCVT_WU_D)&& (rs2 == 1))
`ifdef RV64
          || ((f7 == f7_FCVT_L_D) && (rs2 == 2))
          || ((f7 == f7_FCVT_LU_D)&& (rs2 == 3))
`endif                            
          || ((f7 == f7_FCVT_D_W) && (rs2 == 0))
          || ((f7 == f7_FCVT_D_WU)&& (rs2 == 1))             
`ifdef RV64                       
          || ((f7 == f7_FCVT_D_L) && (rs2 == 2))
          || ((f7 == f7_FCVT_D_LU)&& (rs2 == 3))
`endif
          || ((f7 == f7_FCVT_D_S) && (rs2 == 0))
          || ((f7 == f7_FCVT_S_D) && (rs2 == 1))
          || ((f7 == f7_FMIN_D)   && ( rm == 0))
          || ((f7 == f7_FMAX_D)   && ( rm == 1))
          || ((f7 == f7_FCMP_D)   && ( rm == 0))
          || ((f7 == f7_FCMP_D)   && ( rm == 1))
          || ((f7 == f7_FCMP_D)   && ( rm == 2))
          || ((f7 == f7_FMV_X_D)  && ( rm == 0))
          || ((f7 == f7_FMV_D_X)  && ( rm == 0))
          || ((f7 == f7_FCLASS_D) && ( rm == 1))
`endif
         ) return True;
      else return False;
endfunction
*/

// Returns True if the first operand (val1) should be taken from the GPR
// instead of the FPR for a FP opcode
function Bool fv_fp_val1_from_gpr (Opcode opcode, Bit#(7) f7, RegName rs2);
   return (
         (opcode == op_FP)
      && (   False
`ifdef ISA_D
          || ((f7 == f7_FCVT_D_W)  && (rs2 == 0))
          || ((f7 == f7_FCVT_D_WU) && (rs2 == 1))
`ifdef RV64
          || ((f7 == f7_FCVT_D_L)  && (rs2 == 2))
          || ((f7 == f7_FCVT_D_LU) && (rs2 == 3))
`endif
          || ((f7 == f7_FMV_D_X))
`endif
          || ((f7 == f7_FCVT_S_W)  && (rs2 == 0))
          || ((f7 == f7_FCVT_S_WU) && (rs2 == 1))
`ifdef RV64
          || ((f7 == f7_FCVT_S_L)  && (rs2 == 2))
          || ((f7 == f7_FCVT_S_LU) && (rs2 == 3))
`endif
          || ((f7 == f7_FMV_W_X))
          )
   );
endfunction
`endif

// ================================================================
// System Instructions
Opcode op_SYSTEM = 7'b11_100_11;

// sub-opcodes: (in funct3 field)
Bit #(3)   f3_PRIV           = 3'b000;
Bit #(3)   f3_CSRRW          = 3'b001;
Bit #(3)   f3_CSRRS          = 3'b010;
Bit #(3)   f3_CSRRC          = 3'b011;
Bit #(3)   f3_SYSTEM_ILLEGAL = 3'b100;
Bit #(3)   f3_CSRRWI         = 3'b101;
Bit #(3)   f3_CSRRSI         = 3'b110;
Bit #(3)   f3_CSRRCI         = 3'b111;

// sub-sub-opcodes for f3_PRIV

Bit #(12) f12_ECALL     = 12'b_0000_0000_0000;
Bit #(12) f12_EBREAK    = 12'b_0000_0000_0001;

Bit #(12) f12_URET      = 12'b_0000_0000_0010;
Bit #(12) f12_SRET      = 12'b_0001_0000_0010;
Bit #(12) f12_HRET      = 12'b_0010_0000_0010;
Bit #(12) f12_MRET      = 12'b_0011_0000_0010;
Bit #(12) f12_WFI       = 12'b_0001_0000_0101;

// v1.10 sub-sub-opcode for SFENCE_VMA
Bit #(7)  f7_SFENCE_VMA = 7'b_0001_001;

Instr break_instr = { f12_EBREAK, 5'b00000, 3'b000, 5'b00000, op_SYSTEM };

function Bool fn_instr_is_csrrx (Instr  instr);
   let decoded_instr = fv_decode (instr);
   let opcode        = decoded_instr.opcode;
   let funct3        = decoded_instr.funct3;
   let csr           = decoded_instr.csr;
   return ((opcode == op_SYSTEM) && f3_is_CSRR_any (funct3));
endfunction

function Bool f3_is_CSRR_any (Bit #(3) f3);
   return (f3_is_CSRR_W (f3) || f3_is_CSRR_S_or_C (f3));
endfunction

function Bool f3_is_CSRR_W (Bit #(3) f3);
   return ((f3 == f3_CSRRW) || (f3 == f3_CSRRWI));
endfunction

function Bool f3_is_CSRR_S_or_C (Bit #(3) f3);
   return ((f3 == f3_CSRRS) || (f3 == f3_CSRRSI) ||
	   (f3 == f3_CSRRC) || (f3 == f3_CSRRCI));
endfunction

// ================================================================
// Privilege Modes

typedef 4 Num_Priv_Modes;

typedef Bit #(2) Priv_Mode;

Priv_Mode         u_Priv_Mode = 2'b00;
Priv_Mode         s_Priv_Mode = 2'b01;
Priv_Mode  reserved_Priv_Mode = 2'b10;
Priv_Mode         m_Priv_Mode = 2'b11;

function Fmt fshow_Priv_Mode (Priv_Mode pm);
   return case (pm)
	     u_Priv_Mode: $format ("U");
	     s_Priv_Mode: $format ("S");
	     m_Priv_Mode: $format ("M");
	     default: $format ("RESERVED");
	  endcase;
endfunction

// ================================================================
// Control/Status registers

typedef Bit #(12) CSR_Addr;

function Bool fn_csr_addr_can_write (CSR_Addr csr_addr);
   return (csr_addr [11:10] != 2'b11);
endfunction

function Bool fn_csr_addr_priv_ok (CSR_Addr csr_addr, Priv_Mode priv_mode);
   return (priv_mode >= csr_addr [9:8]);
endfunction

// ----------------
// User-level CSR addresses

CSR_Addr   csr_addr_ustatus        = 12'h000;    // User status
CSR_Addr   csr_addr_uie            = 12'h004;    // User interrupt-enable
CSR_Addr   csr_addr_utvec          = 12'h005;    // User trap handler base address

CSR_Addr   csr_addr_uscratch       = 12'h040;    // Scratch register for trap handlers
CSR_Addr   csr_addr_uepc           = 12'h041;    // User exception program counter
CSR_Addr   csr_addr_ucause         = 12'h042;    // User trap cause
CSR_Addr   csr_addr_utval          = 12'h043;    // User bad address or instruction
CSR_Addr   csr_addr_uip            = 12'h044;    // User interrupt pending

CSR_Addr   csr_addr_fflags         = 12'h001;    // Floating-point accrued exceptions
CSR_Addr   csr_addr_frm            = 12'h002;    // Floating-point Dynamic Rounding Mode
CSR_Addr   csr_addr_fcsr           = 12'h003;    // Floating-point Control and Status Register (frm + fflags)

CSR_Addr   csr_addr_cycle          = 12'hC00;    // Cycle counter for RDCYCLE
CSR_Addr   csr_addr_time           = 12'hC01;    // Timer for RDTIME
CSR_Addr   csr_addr_instret        = 12'hC02;    // Instructions retired counter for RDINSTRET

CSR_Addr   csr_addr_hpmcounter3    = 12'hC03;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter4    = 12'hC04;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter5    = 12'hC05;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter6    = 12'hC06;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter7    = 12'hC07;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter8    = 12'hC08;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter9    = 12'hC09;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter10   = 12'hC0A;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter11   = 12'hC0B;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter12   = 12'hC0C;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter13   = 12'hC0D;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter14   = 12'hC0E;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter15   = 12'hC0F;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter16   = 12'hC10;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter17   = 12'hC11;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter18   = 12'hC12;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter19   = 12'hC13;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter20   = 12'hC14;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter21   = 12'hC15;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter22   = 12'hC16;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter23   = 12'hC17;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter24   = 12'hC18;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter25   = 12'hC19;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter26   = 12'hC1A;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter27   = 12'hC1B;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter28   = 12'hC1C;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter29   = 12'hC1D;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter30   = 12'hC1E;    // Performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter31   = 12'hC1F;    // Performance-monitoring counter

CSR_Addr   csr_addr_cycleh         = 12'hC80;    // Upper 32 bits of csr_cycle (RV32I only)
CSR_Addr   csr_addr_timeh          = 12'hC81;    // Upper 32 bits of csr_time (RV32I only)
CSR_Addr   csr_addr_instreth       = 12'hC82;    // Upper 32 bits of csr_instret (RV32I only)

CSR_Addr   csr_addr_hpmcounter3h   = 12'hC83;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter4h   = 12'hC84;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter5h   = 12'hC85;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter6h   = 12'hC86;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter7h   = 12'hC87;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter8h   = 12'hC88;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter9h   = 12'hC89;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter10h  = 12'hC8A;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter11h  = 12'hC8B;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter12h  = 12'hC8C;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter13h  = 12'hC8D;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter14h  = 12'hC8E;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter15h  = 12'hC8F;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter16h  = 12'hC90;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter17h  = 12'hC91;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter18h  = 12'hC92;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter19h  = 12'hC93;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter20h  = 12'hC94;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter21h  = 12'hC95;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter22h  = 12'hC96;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter23h  = 12'hC97;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter24h  = 12'hC98;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter25h  = 12'hC99;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter26h  = 12'hC9A;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter27h  = 12'hC9B;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter28h  = 12'hC9C;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter29h  = 12'hC9D;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter30h  = 12'hC9E;    // Upper 32 bits of performance-monitoring counter
CSR_Addr   csr_addr_hpmcounter31h  = 12'hC9F;    // Upper 32 bits of performance-monitoring counter

// Information from the CSR on a new trap. 
typedef struct {
   Addr        pc;
   WordXL      mstatus;
   WordXL      mcause;
   Priv_Mode   priv;
} Trap_Info deriving (Bits, Eq, FShow);

// ================================================================
// 'C' Extension ("compressed" instructions)

`include "ISA_Decls_C.bsv"

// ================================================================
// Supervisor-Level ISA defs

`include "ISA_Decls_Priv_S.bsv"

// ================================================================
// Hypervisor-Level ISA defs

// `include "ISA_Decls_Priv_H.bsv"

// ================================================================
// Machine-Level ISA defs

`include "ISA_Decls_Priv_M.bsv"

// ================================================================

endpackage
