// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved

// ================================================================
// This is an 'include' file, not a separate BSV package
//
// Contains RISC-V ISA defs for the 'C' ("compressed") extension
// i.e., 16-bit instructions
//
// ================================================================
// Instruction field encodings

typedef  Bit #(16)  Instr_C;

Bit #(2) opcode_C0 = 2'b00;
Bit #(2) opcode_C1 = 2'b01;
Bit #(2) opcode_C2 = 2'b10;

Bit #(3) funct3_C_LWSP     = 3'b_010;
Bit #(3) funct3_C_LDSP     = 3'b_011;     // RV64 and RV128
Bit #(3) funct3_C_LQSP     = 3'b_001;     // RV128
Bit #(3) funct3_C_FLWSP    = 3'b_011;     // RV32FC
Bit #(3) funct3_C_FLDSP    = 3'b_001;     // RV32DC, RV64DC

Bit #(3) funct3_C_SWSP     = 3'b_110;

Bit #(3) funct3_C_SQSP     = 3'b_101;     // RV128
Bit #(3) funct3_C_FSDSP    = 3'b_101;     // RV32DC, RV64DC

Bit #(3) funct3_C_SDSP     = 3'b_111;     // RV64 and RV128
Bit #(3) funct3_C_FSWSP    = 3'b_111;     // RV32FC

Bit #(3) funct3_C_LQ       = 3'b_001;     // RV128
Bit #(3) funct3_C_FLD      = 3'b_001;     // RV32DC, RV64DC

Bit #(3) funct3_C_LW       = 3'b_010;

Bit #(3) funct3_C_LD       = 3'b_011;     // RV64 and RV128
Bit #(3) funct3_C_FLW      = 3'b_011;     // RV32FC

Bit #(3) funct3_C_FSD      = 3'b_101;     // RV32DC, RV64DC
Bit #(3) funct3_C_SQ       = 3'b_101;     // RV128

Bit #(3) funct3_C_SW       = 3'b_110;

Bit #(3) funct3_C_SD       = 3'b_111;     // RV64 and RV128
Bit #(3) funct3_C_FSW      = 3'b_111;     // RV32FC

Bit #(3) funct3_C_JAL      = 3'b_001;     // RV32
Bit #(3) funct3_C_J        = 3'b_101;
Bit #(3) funct3_C_BEQZ     = 3'b_110;
Bit #(3) funct3_C_BNEZ     = 3'b_111;

Bit #(4) funct4_C_JR       = 4'b_1000;
Bit #(4) funct4_C_JALR     = 4'b_1001;

Bit #(3) funct3_C_LI       = 3'b_010;
Bit #(3) funct3_C_LUI      = 3'b_011;     // RV64 and RV128

Bit #(3) funct3_C_NOP      = 3'b_000;
Bit #(3) funct3_C_ADDI     = 3'b_000;
Bit #(3) funct3_C_ADDIW    = 3'b_001;
Bit #(3) funct3_C_ADDI16SP = 3'b_011;
Bit #(3) funct3_C_ADDI4SPN = 3'b_000;
Bit #(3) funct3_C_SLLI     = 3'b_000;

Bit #(3) funct3_C_SRLI     = 3'b_100;
Bit #(2) funct2_C_SRLI     = 2'b_00;

Bit #(3) funct3_C_SRAI     = 3'b_100;
Bit #(2) funct2_C_SRAI     = 2'b_01;

Bit #(3) funct3_C_ANDI     = 3'b_100;
Bit #(2) funct2_C_ANDI     = 2'b_10;

Bit #(4) funct4_C_MV       = 4'b_1000;
Bit #(4) funct4_C_ADD      = 4'b_1001;

Bit #(6) funct6_C_AND      = 6'b_100_0_11;
Bit #(2) funct2_C_AND      = 2'b_11;

Bit #(6) funct6_C_OR       = 6'b_100_0_11;
Bit #(2) funct2_C_OR       = 2'b_10;

Bit #(6) funct6_C_XOR      = 6'b_100_0_11;
Bit #(2) funct2_C_XOR      = 2'b_01;

Bit #(6) funct6_C_SUB      = 6'b_100_0_11;
Bit #(2) funct2_C_SUB      = 2'b_00;

Bit #(6) funct6_C_ADDW     = 6'b_100_1_11;
Bit #(2) funct2_C_ADDW     = 2'b_01;

Bit #(6) funct6_C_SUBW     = 6'b_100_1_11;
Bit #(2) funct2_C_SUBW     = 2'b_00;

Bit #(4) funct4_C_EBREAK   = 4'b_1001;

// ================================================================
// Functions to extract instruction fields from 'C' (compressed) instructions

function Tuple4 #(Bit #(4), RegName, RegName, Bit #(2))  fv_ifields_CR_type (Instr_C  instr);
   let funct4 = instr [15:12];
   let rd_rs1 = instr [11: 7];
   let rs2    = instr [ 6: 2];
   let op     = instr [ 1: 0];
   return tuple4 (funct4, rd_rs1, rs2, op);
endfunction

function Tuple5 #(Bit #(3), Bit #(1), Bit #(5), Bit #(5), Bit #(2))  fv_ifields_CI_type (Instr_C  instr);
   let funct3     = instr [15:13];
   let imm_at_12  = instr [12:12];
   let rd_rs1     = instr [11: 7];
   let imm_at_6_2 = instr [ 6: 2];
   let op         = instr [ 1: 0];
   return tuple5 (funct3, imm_at_12, rd_rs1, imm_at_6_2, op);
endfunction

function Tuple4 #(Bit #(3), Bit #(6), RegName, Bit #(2))  fv_ifields_CSS_type (Instr_C  instr);
   let funct3      = instr [15:13];
   let imm_at_12_7 = instr [12: 7];
   let rs2         = instr [ 6: 2];
   let op          = instr [ 1: 0];
   return tuple4 (funct3, imm_at_12_7, rs2, op);
endfunction

function Tuple4 #(Bit #(3), Bit #(8), RegName, Bit #(2))  fv_ifields_CIW_type (Instr_C  instr);
   let funct3      = instr [15:13];
   let imm_at_12_5 = instr [12: 5];
   let rd          = {2'b01, instr [4:2]};
   let op          = instr [ 1: 0];
   return tuple4 (funct3, imm_at_12_5, rd, op);
endfunction

function Tuple6 #(Bit #(3), Bit #(3), RegName, Bit #(2), RegName, Bit #(2))  fv_ifields_CL_type (Instr_C  instr);
   let funct3       = instr [15:13];
   let imm_at_12_10 = instr [12:10];
   let rs1          = {2'b01, instr [9:7]};
   let imm_at_6_5   = instr [ 6: 5];
   let rd           = {2'b01, instr [4:2]};
   let op           = instr [ 1: 0];
   return tuple6 (funct3, imm_at_12_10, rs1, imm_at_6_5, rd, op);
endfunction

function Tuple6 #(Bit #(3), Bit #(3), RegName, Bit #(2), RegName, Bit #(2))  fv_ifields_CS_type (Instr_C  instr);
   let funct3       = instr [15:13];
   let imm_at_12_10 = instr [12:10];
   let rs1          = {2'b01, instr [9:7]};
   let imm_at_6_5   = instr [ 6: 5];
   let rs2          = {2'b01, instr [4:2]};
   let op           = instr [ 1: 0];
   return tuple6 (funct3, imm_at_12_10, rs1, imm_at_6_5, rs2, op);
endfunction

function Tuple5 #(Bit #(6), RegName, Bit #(2), RegName, Bit #(2))  fv_ifields_CA_type (Instr_C  instr);
   let funct6       = instr [15:10];
   let rd_rs1       = {2'b01, instr [9:7]};
   let funct2       = instr [ 6: 5];
   let rs2          = {2'b01, instr [4:2]};
   let op           = instr [ 1: 0];
   return tuple5 (funct6, rd_rs1, funct2, rs2, op);
endfunction

function Tuple5 #(Bit #(3), Bit #(3), RegName, Bit #(5), Bit #(2))  fv_ifields_CB_type (Instr_C  instr);
   let funct3       = instr [15:13];
   let imm_at_12_10 = instr [12:10];
   let rs1          = {2'b01, instr [9:7]};
   let imm_at_6_2   = instr [ 6: 2];
   let op           = instr [ 1: 0];
   return tuple5 (funct3, imm_at_12_10, rs1, imm_at_6_2, op);
endfunction

function Tuple3 #(Bit #(3), Bit #(11), Bit #(2))  fv_ifields_CJ_type (Instr_C  instr);
   let funct3       = instr [15:13];
   let imm_at_12_2  = instr [12: 2];
   let op           = instr [ 1: 0];
   return tuple3 (funct3, imm_at_12_2, op);
endfunction

// ================================================================
