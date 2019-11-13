// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package EX_ALU_functions;

// ================================================================
// These are the "ALU" functions in the EX stage of the "Piccolo" CPU.
// EX stands for "Execution".

// ================================================================
// Exports

export
ALU_Inputs (..),
ALU_Outputs (..),
fv_ALU;

// ================================================================
// BSV library imports

import Vector :: *;

// ----------------
// BSV additional libs

// None

// ================================================================
// Project imports

import ISA_Decls   :: *;
import CPU_Globals :: *;
import TV_Info     :: *;

// ================================================================
// ALU inputs

typedef struct {
   Priv_Mode      cur_priv;
   Addr           pc;
   Bool           is_i32_not_i16;
   Instr          instr;
`ifdef ISA_C
   Instr_C        instr_C;
`endif
   Decoded_Instr  decoded_instr;
   WordXL         rs1_val;
   WordXL         rs2_val;
   WordXL         mstatus;
`ifdef ISA_F
   Bit #(3)       frm;
   WordFL         frs1_val;
   WordFL         frs2_val;
   WordFL         frs3_val;
`ifdef INCLUDE_TANDEM_VERIF
   Bit #(5)       fflags;
`endif
`endif
   MISA           misa;
   } ALU_Inputs
deriving (Bits, FShow);

// ----------------
// These functions pick the instruction size and instruction bits to
// be sent in the trace to a tandem verifier

function ISize  fv_trace_isize (ALU_Inputs  inputs);
   return (inputs.is_i32_not_i16 ? ISIZE32BIT : ISIZE16BIT);
endfunction

function Bit #(32)  fv_trace_instr (ALU_Inputs  inputs);
   Bit #(32) result = inputs.instr;
`ifdef ISA_C
   if (! inputs.is_i32_not_i16)
      result = zeroExtend (inputs.instr_C);
`endif
   return result;
endfunction

// ================================================================
// ALU outputs

typedef struct {
   Control    control;
   Exc_Code   exc_code;        // Relevant if control == CONTROL_TRAP

   Op_Stage2  op_stage2;
   RegName    rd;
   Addr       addr;           // Branch, jump: newPC
		              // Mem ops and AMOs: mem addr
   WordXL     val1;           // OP_Stage2_ALU: result for Rd (ALU ops: result, JAL/JALR: return PC)
                              // CSRRx: rs1_val
                              // OP_Stage2_M: arg1
                              // OP_Stage2_AMO: funct7

   WordXL     val2;           // Branch: branch target (for Tandem Verification)
		              // OP_Stage2_ST: store-val
                              // OP_Stage2_M: arg2
`ifdef ISA_F
   WordFL     fval1;          // OP_Stage2_FD: arg1
   WordFL     fval2;          // OP_Stage2_FD: arg2
   WordFL     fval3;          // OP_Stage2_FD: arg3
   Bool       rd_in_fpr;      // result to be written to fpr
   Bool       rs_frm_fpr;     // src register is in fpr (for stores)
   Bool       val1_frm_gpr;   // first operand is in gpr (for some FP instrns)
   Bit #(3)   rm;             // rounding mode
`endif

   CF_Info    cf_info;        // For redirection and branch predictor

`ifdef INCLUDE_TANDEM_VERIF
   Trace_Data trace_data;
`endif
   } ALU_Outputs
deriving (Bits, FShow);

CF_Info cf_info_base = CF_Info {cf_op       : CF_None,
				from_PC     : ?,
				taken       : ?,
				fallthru_PC : ?,
				taken_PC    : ?};

ALU_Outputs alu_outputs_base
= ALU_Outputs {control     : CONTROL_STRAIGHT,
	       exc_code    : exc_code_ILLEGAL_INSTRUCTION,
	       op_stage2   : ?,
	       rd          : ?,
	       addr        : ?,
	       val1        : ?,
	       val2        : ?,
`ifdef ISA_F
	       fval1       : ?,
	       fval2       : ?,
	       fval3       : ?,
	       rd_in_fpr   : False,
	       rs_frm_fpr  : False,
	       val1_frm_gpr: False,
	       rm          : ?,
`endif
	       cf_info     : cf_info_base

`ifdef INCLUDE_TANDEM_VERIF
	     , trace_data  : ?
`endif
};

// ================================================================
// The fall-through PC is PC+4 for normal 32b instructions,
// and PC+2 for 'C' (16b compressed) instructions.

function Addr fall_through_pc (ALU_Inputs  inputs);
   Addr next_pc = inputs.pc + 4;
`ifdef ISA_C
   if (! inputs.is_i32_not_i16)
      next_pc = inputs.pc + 2;
`endif
   return next_pc;
endfunction

// ================================================================
// Alternate implementation of shifts using multiplication in DSPs

// ----------------------------------------------------------------
/* TODO: DELETE? 'factor' RegFile for shift ops

// ----------------------------------------------------------------
// The following is a lookup table of multiplication factors used by the "shift" ops
RegFile #(Bit #(TLog #(XLEN)), Bit #(XLEN))  rf_sh_factors <- mkRegFileFull;
// The following is used during reset to initialize rf_sh_factors
Reg #(Bool)                                  rg_resetting  <- mkReg (False);
Reg #(Bit #(TAdd #(1, TLog #(XLEN))))        rg_j          <- mkRegU;
Reg #(WordXL)                                rg_factor     <- mkRegU;
*/

// ----------------------------------------------------------------
// The following functions implement the 'shift' operators SHL, SHRL and SHRA
// using multiplication instead of actual shifts,
// thus using DSPs (multiplication) and LUTRAMs (rf_sh_factors) instead of LUTs

// Shift-left
// Instead of '>>' operator, uses '*', using DSPs instead of LUTs.
// To SHL(n), do a multiplication by 2^n.
// The 2^n factor is looked up in a RegFile (used as a ROM), which uses a LUTRAM instead of LUTs
function WordXL fn_shl (WordXL x, Bit #(TLog #(XLEN)) shamt);
   IntXL  x_signed = unpack (x);

   // IntXL y_signed = unpack (rf_sh_factors.sub (shamt));
   IntXL  y_signed = unpack ('b1 << shamt);

   IntXL  z_signed = x_signed * y_signed;
   WordXL z        = pack (z_signed);
   return z;
endfunction

// Shift-right-arithmetic
// Instead of '>>' operator, uses '*', using DSPs instead of LUTs
// To SHR(n), do a 2*XLEN-wide multiplication by 2^(32-n), and take upper XLEN bits
// The 2^(32-n) factor is looked up in a RegFile (used as a ROM), which uses a LUTRAM instead of LUTs
function WordXL fn_shra (WordXL x, Bit #(TLog #(XLEN)) shamt);
   // Bit #(TAdd #(1, XLEN)) y = { reverseBits (rf_sh_factors.sub (shamt)), 1'b0 };
   Bit #(TAdd #(1, XLEN)) y = { reverseBits ('b1 << shamt), 1'b0 };

   Int #(XLEN_2) xx_signed = extend (unpack (x));
   Int #(XLEN_2) yy_signed = unpack (extend (y));
   Int #(XLEN_2) zz_signed = xx_signed * yy_signed;
   Bit #(XLEN_2) zz        = pack (zz_signed);
   WordXL        z         = truncateLSB (zz);
   return z;
endfunction

// Shift-right-logical
// Instead of '>>' operator, uses '*', using DSPs instead of LUTs
// To SHR(n), do a 2*XLEN-wide multiplication by 2^(32-n), and take upper XLEN bits
// The 2^(32-n) factor is looked up in a RegFile (used as a ROM), which uses a LUTRAM instead of LUTs
function WordXL fn_shrl (WordXL x, Bit #(TLog #(XLEN)) shamt);
   // Bit #(TAdd #(1, XLEN)) y = { reverseBits (rf_sh_factors.sub (shamt)), 1'b0 };
   Bit #(TAdd #(1, XLEN)) y = { reverseBits ('b1 << shamt), 1'b0 };

   Bit #(XLEN_2) xx = extend (x);
   Bit #(XLEN_2) yy = extend (y);
   Bit #(XLEN_2) zz = xx * yy;
   WordXL        z  = truncateLSB (zz);
   return z;
endfunction

// ================================================================
// BRANCH

function ALU_Outputs fv_BRANCH (ALU_Inputs inputs);
   let rs1_val = inputs.rs1_val;
   let rs2_val = inputs.rs2_val;

   // Signed versions of rs1_val and rs2_val
   IntXL s_rs1_val = unpack (inputs.rs1_val);
   IntXL s_rs2_val = unpack (inputs.rs2_val);

   IntXL offset        = extend (unpack (inputs.decoded_instr.imm13_SB));
   Addr  branch_target = pack (unpack (inputs.pc) + offset);
   Bool  branch_taken  = False;
   Bool  trap          = False;

   let funct3 = inputs.decoded_instr.funct3;
   if      (funct3 == f3_BEQ)  branch_taken = (rs1_val  == rs2_val);
   else if (funct3 == f3_BNE)  branch_taken = (rs1_val  != rs2_val);
   else if (funct3 == f3_BLT)  branch_taken = (s_rs1_val <  s_rs2_val);
   else if (funct3 == f3_BGE)  branch_taken = (s_rs1_val >= s_rs2_val);
   else if (funct3 == f3_BLTU) branch_taken = (rs1_val  <  rs2_val);
   else if (funct3 == f3_BGEU) branch_taken = (rs1_val  >= rs2_val);
   else                        trap = True;

   Bool misaligned_target = (branch_target [1] == 1'b1);
`ifdef ISA_C
   misaligned_target = False;
`endif

   Exc_Code exc_code = exc_code_ILLEGAL_INSTRUCTION;
   if ((! trap) && branch_taken && misaligned_target) begin
      trap = True;
      exc_code = exc_code_INSTR_ADDR_MISALIGNED;
   end

   let cf_info   = CF_Info {cf_op       : CF_BR,
			    from_PC     : inputs.pc,
			    taken       : branch_taken,
			    fallthru_PC : fall_through_pc (inputs),
			    taken_PC    : branch_target };

   let alu_outputs = alu_outputs_base;
   let next_pc     = (branch_taken ? branch_target : fall_through_pc (inputs));
   alu_outputs.control   = (trap ? CONTROL_TRAP : (branch_taken ? CONTROL_BRANCH : CONTROL_STRAIGHT));
   alu_outputs.exc_code  = exc_code;
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = 0;
   alu_outputs.addr      = next_pc;
   alu_outputs.val2      = extend (branch_target);    // For tandem verifier only

   alu_outputs.cf_info   = cf_info;
`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_OTHER (next_pc,
					   fv_trace_isize (inputs),
					   fv_trace_instr (inputs));
`endif
   return alu_outputs;
endfunction

// ----------------------------------------------------------------
// JAL

function ALU_Outputs fv_JAL (ALU_Inputs inputs);
   IntXL offset  = extend (unpack (inputs.decoded_instr.imm21_UJ));
   Addr  next_pc = pack (unpack (inputs.pc) + offset);
   Addr  ret_pc  = fall_through_pc (inputs);

   Bool misaligned_target = (next_pc [1] == 1'b1);
`ifdef ISA_C
   misaligned_target = False;
`endif

   let cf_info   = CF_Info {cf_op       : CF_JAL,
			    from_PC     : inputs.pc,
			    taken       : True,
			    fallthru_PC : ret_pc,
			    taken_PC    : next_pc };

   let alu_outputs = alu_outputs_base;
   alu_outputs.control   = (misaligned_target ? CONTROL_TRAP : CONTROL_BRANCH);
   alu_outputs.exc_code  = exc_code_INSTR_ADDR_MISALIGNED;
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.addr      = next_pc;
   alu_outputs.val1      = extend (ret_pc);
   alu_outputs.cf_info   = cf_info;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (next_pc,
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  ret_pc);
`endif
   return alu_outputs;
endfunction

// ----------------------------------------------------------------
// JALR

function ALU_Outputs fv_JALR (ALU_Inputs inputs);
   let rs1_val = inputs.rs1_val;
   let rs2_val = inputs.rs2_val;

   // Signed versions of rs1_val and rs2_val
   IntXL s_rs1_val = unpack (rs1_val);
   IntXL s_rs2_val = unpack (rs2_val);
   IntXL offset    = extend (unpack (inputs.decoded_instr.imm12_I));
   Addr  next_pc   = pack (s_rs1_val + offset);
   Addr  ret_pc    = fall_through_pc (inputs);

   // next_pc [0] should be cleared
   next_pc [0] = 1'b0;

   Bool misaligned_target = (next_pc [1] == 1'b1);
`ifdef ISA_C
   misaligned_target = False;
`endif

   let cf_info   = CF_Info {cf_op       : CF_JALR,
			    from_PC     : inputs.pc,
			    taken       : True,
			    fallthru_PC : ret_pc,
			    taken_PC    : next_pc };

   let alu_outputs = alu_outputs_base;
   alu_outputs.control   = (misaligned_target ? CONTROL_TRAP : CONTROL_BRANCH);
   alu_outputs.exc_code  = exc_code_INSTR_ADDR_MISALIGNED;
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.addr      = next_pc;
   alu_outputs.val1      = extend (ret_pc);
   alu_outputs.cf_info   = cf_info;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (next_pc,
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  ret_pc);
`endif
   return alu_outputs;
endfunction

// ----------------------------------------------------------------
// Integer Register-Register and Register-Immediate Instructions

// ----------------
// Shifts (funct3 == f3_SLLI/ f3_SRLI/ f3_SRAI)

function ALU_Outputs fv_OP_and_OP_IMM_shifts (ALU_Inputs inputs);
   let rs1_val = inputs.rs1_val;
   let rs2_val = inputs.rs2_val;

   IntXL s_rs1_val = unpack (rs1_val);    // Signed version of rs1, for SRA

   Bit #(TLog #(XLEN)) shamt = (  (inputs.decoded_instr.opcode == op_OP_IMM)
				? truncate (inputs.decoded_instr.imm12_I)
				: truncate (rs2_val));

   WordXL   rd_val    = ?;
   let      funct3    = inputs.decoded_instr.funct3;
   Bit #(1) instr_b30 = inputs.instr [30];

`ifdef SHIFT_BARREL
   // Shifts implemented by Verilog synthesis,
   // mapping to barrel shifters
   if (funct3 == f3_SLLI)
      rd_val = (rs1_val << shamt);
   else begin // assert: (funct3 == f3_SRxI)
      if (instr_b30 == 1'b0)
	 // SRL/SRLI
	 rd_val = (rs1_val >> shamt);
      else
	 // SRA/SRAI
	 rd_val = pack (s_rs1_val >> shamt);
   end
`endif

`ifdef SHIFT_MULT
   // Shifts implemented using multiplication by 2^shamt,
   // mapping to DSPs in FPGA
   if (funct3 == f3_SLLI)
      rd_val = fn_shl (rs1_val, shamt);  // in LUTRAMs/DSPs
   else begin // assert: (funct3 == f3_SRxI)
      if (instr_b30 == 1'b0) begin
	 // SRL/SRLI
	 rd_val = fn_shrl (rs1_val, shamt);  // in LUTRAMs/DSPs
      else
	 // SRA/SRAI
	 rd_val = fn_shra (rs1_val, shamt);     // in LUTRAMs/DSPs
   end
`endif

   // Trap in RV32 if shamt > 31, i.e., if imm12_I [5] is 1
   Bool trap = ((rv_version == RV32) && (inputs.decoded_instr.imm12_I [5] == 1));

   let alu_outputs       = alu_outputs_base;
   alu_outputs.control   = (trap ? CONTROL_TRAP : CONTROL_STRAIGHT);
   alu_outputs.rd        = inputs.decoded_instr.rd;

`ifndef SHIFT_SERIAL
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.val1      = rd_val;
`else
   // Will be executed in serial Shifter_Box later
   alu_outputs.op_stage2 = OP_Stage2_SH;
   alu_outputs.val1      = rs1_val;
   // Encode 'arith-shift' in bit [7] of val2
   WordXL val2 = extend (shamt);
   val2 = (val2 | { 0, instr_b30, 7'b0});
   alu_outputs.val2 = val2;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  rd_val);
`endif
   return alu_outputs;
endfunction: fv_OP_and_OP_IMM_shifts

// ----------------
// Remaining OP and OP_IMM (excluding shifts, M ops MUL/DIV/REM)

function ALU_Outputs fv_OP_and_OP_IMM (ALU_Inputs inputs);
   let rs1_val = inputs.rs1_val;
   let rs2_val = inputs.rs2_val;

   // Signed versions of rs1_val and rs2_val
   IntXL  s_rs1_val = unpack (rs1_val);
   IntXL  s_rs2_val = unpack (rs2_val);

   IntXL  s_rs2_val_local = s_rs2_val;
   WordXL rs2_val_local   = rs2_val;

   Bit #(1) instr_b30  = inputs.instr [30];
   Bool     subtract   = ((inputs.decoded_instr.opcode == op_OP) && (instr_b30 == 1'b1));

   if (inputs.decoded_instr.opcode == op_OP_IMM) begin
      s_rs2_val_local = extend (unpack (inputs.decoded_instr.imm12_I));
      rs2_val_local   = pack (s_rs2_val_local);
   end

   let  funct3 = inputs.decoded_instr.funct3;
   Bool trap   = False;
   WordXL rd_val = ?;

   if      ((funct3 == f3_ADDI) && (! subtract)) rd_val = pack (s_rs1_val + s_rs2_val_local);
   else if ((funct3 == f3_ADDI) && (subtract))   rd_val = pack (s_rs1_val - s_rs2_val_local);

   else if (funct3 == f3_SLTI)  rd_val = ((s_rs1_val < s_rs2_val_local) ? 1 : 0);
   else if (funct3 == f3_SLTIU) rd_val = ((rs1_val  < rs2_val_local)  ? 1 : 0);
   else if (funct3 == f3_XORI)  rd_val = pack (s_rs1_val ^ s_rs2_val_local);
   else if (funct3 == f3_ORI)   rd_val = pack (s_rs1_val | s_rs2_val_local);
   else if (funct3 == f3_ANDI)  rd_val = pack (s_rs1_val & s_rs2_val_local);
   else
      trap = True;

   let alu_outputs       = alu_outputs_base;
   alu_outputs.control   = (trap ? CONTROL_TRAP : CONTROL_STRAIGHT);
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.val1      = rd_val;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  rd_val);
`endif
   return alu_outputs;
endfunction: fv_OP_and_OP_IMM

// ----------------
// OP_IMM_32 (ADDIW, SLLIW, SRxIW)

function ALU_Outputs fv_OP_IMM_32 (ALU_Inputs inputs);
   WordXL   rs1_val     = inputs.rs1_val;
   IntXL    s_rs1_val   = unpack (rs1_val);

   Bit #(5) shamt       = truncate (inputs.decoded_instr.imm12_I);
   Bool     shamt5_is_0 = (inputs.instr [25] == 1'b0);

   let    funct3 = inputs.decoded_instr.funct3;
   Bool   trap   = False;
   WordXL rd_val = ?;

   if (funct3 == f3_ADDIW) begin
      IntXL  s_rs2_val = extend (unpack (inputs.decoded_instr.imm12_I));
      IntXL  sum       = s_rs1_val + s_rs2_val;
      WordXL tmp       = pack (sum);
      rd_val           = signExtend (tmp [31:0]);
   end
   else if ((funct3 == f3_SLLIW) && shamt5_is_0) begin
      Bit #(32) tmp = truncate (rs1_val);
      rd_val = signExtend (tmp << shamt);
   end
   else if ((funct3 == f3_SRxIW) && shamt5_is_0) begin
      Bit #(1) instr_b30 = inputs.instr [30];
      if (instr_b30 == 1'b0) begin
	 // SRLIW
	 Bit #(32) tmp = truncate (rs1_val);
	 rd_val = signExtend (tmp >> shamt);
      end
      else begin
	 // SRAIW
	 Int #(32) s_tmp = unpack (rs1_val [31:0]);
	 Bit #(32) tmp   = pack (s_tmp >> shamt);
	 rd_val = signExtend (tmp);
      end
   end
   else
      trap = True;

   let alu_outputs       = alu_outputs_base;
   alu_outputs.control   = (trap ? CONTROL_TRAP : CONTROL_STRAIGHT);
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.val1      = rd_val;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  rd_val);
`endif
   return alu_outputs;
endfunction: fv_OP_IMM_32

// ----------------
// OP_32 (excluding 'M' ops: MULW/ DIVW/ DIVUW/ REMW/ REMUW)

function ALU_Outputs fv_OP_32 (ALU_Inputs inputs);
   Bit #(32) rs1_val = inputs.rs1_val [31:0];
   Bit #(32) rs2_val = inputs.rs2_val [31:0];

   // Signed version of rs1_val and rs2_val
   Int #(32) s_rs1_val = unpack (rs1_val);
   Int #(32) s_rs2_val = unpack (rs2_val);

   let    funct10 = inputs.decoded_instr.funct10;
   Bool   trap   = False;
   WordXL rd_val = ?;

   if      (funct10 == f10_ADDW) begin
      rd_val = pack (signExtend (s_rs1_val + s_rs2_val));
   end
   else if (funct10 == f10_SUBW) begin
      rd_val = pack (signExtend (s_rs1_val - s_rs2_val));
   end
   else if (funct10 == f10_SLLW) begin
      rd_val = pack (signExtend (rs1_val << (rs2_val [4:0])));
   end
   else if (funct10 == f10_SRLW) begin
      rd_val = pack (signExtend (rs1_val >> (rs2_val [4:0])));
   end
   else if (funct10 == f10_SRAW) begin
      rd_val = pack (signExtend (s_rs1_val >> (rs2_val [4:0])));
   end
   else
      trap = True;

   let alu_outputs       = alu_outputs_base;
   alu_outputs.control   = (trap ? CONTROL_TRAP : CONTROL_STRAIGHT);
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.val1      = rd_val;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  rd_val);
`endif
   return alu_outputs;
endfunction: fv_OP_32

// ----------------------------------------------------------------
// Upper Immediates

function ALU_Outputs fv_LUI (ALU_Inputs inputs);
   Bit #(32)  v32    = { inputs.decoded_instr.imm20_U, 12'h0 };
   IntXL      iv     = extend (unpack (v32));
   let        rd_val = pack (iv);

   let alu_outputs       = alu_outputs_base;
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.val1      = rd_val;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  rd_val);
`endif
   return alu_outputs;
endfunction

function ALU_Outputs fv_AUIPC (ALU_Inputs inputs);
   IntXL  iv     = extend (unpack ({ inputs.decoded_instr.imm20_U, 12'b0}));
   IntXL  pc_s   = unpack (inputs.pc);
   WordXL rd_val = pack (pc_s + iv);

   let alu_outputs       = alu_outputs_base;
   alu_outputs.op_stage2 = OP_Stage2_ALU;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.val1      = rd_val;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
					  fv_trace_isize (inputs),
					  fv_trace_instr (inputs),
					  inputs.decoded_instr.rd,
					  rd_val);
`endif
   return alu_outputs;
endfunction

// ----------------------------------------------------------------
// LOAD

function ALU_Outputs fv_LD (ALU_Inputs inputs);
   // Signed versions of rs1_val and rs2_val
   let opcode = inputs.decoded_instr.opcode;
   IntXL s_rs1_val = unpack (inputs.rs1_val);
   IntXL s_rs2_val = unpack (inputs.rs2_val);

   IntXL  imm_s = extend (unpack (inputs.decoded_instr.imm12_I));
   WordXL eaddr = pack (s_rs1_val + imm_s);

   let funct3 = inputs.decoded_instr.funct3;

   Bool legal_LD = (   (funct3 == f3_LB) || (funct3 == f3_LBU)
		    || (funct3 == f3_LH) || (funct3 == f3_LHU)
		    || (funct3 == f3_LW)
`ifdef RV64
		    || (funct3 == f3_LWU)
		    || (funct3 == f3_LD)
`endif
`ifdef ISA_F
		    || (funct3 == f3_FLW)
`endif
`ifdef ISA_D
		    || (funct3 == f3_FLD)
`endif
		    );

   // FP loads are not legal unless the MSTATUS.FS bit is set
   Bool legal_FP_LD = True;
`ifdef ISA_F
   if (opcode == op_LOAD_FP)
      legal_FP_LD = (fv_mstatus_fs (inputs.mstatus) != fs_xs_off);
`endif

   let alu_outputs = alu_outputs_base;

   alu_outputs.control   = ((legal_LD && legal_FP_LD) ? CONTROL_STRAIGHT
                                                      : CONTROL_TRAP);
   alu_outputs.op_stage2 = OP_Stage2_LD;
   alu_outputs.rd        = inputs.decoded_instr.rd;
   alu_outputs.addr      = eaddr;
`ifdef ISA_F
   // note that the destination register for this load is in the FPR
   alu_outputs.rd_in_fpr = (opcode == op_LOAD_FP);
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
`ifdef ISA_F
   if (alu_outputs.rd_in_fpr)
      alu_outputs.trace_data = mkTrace_F_LOAD (fall_through_pc (inputs),
					       fv_trace_isize (inputs),
					       fv_trace_instr (inputs),
					       inputs.decoded_instr.rd,
					       ?,
					       eaddr,
                                               inputs.mstatus);
   else
`endif
      alu_outputs.trace_data = mkTrace_I_LOAD (fall_through_pc (inputs),
					       fv_trace_isize (inputs),
					       fv_trace_instr (inputs),
					       inputs.decoded_instr.rd,
					       ?,
					       eaddr);
`endif
   return alu_outputs;
endfunction

// ----------------------------------------------------------------
// STORE

function ALU_Outputs fv_ST (ALU_Inputs inputs);
   // Signed version of rs1_val
   IntXL  s_rs1_val = unpack (inputs.rs1_val);
   IntXL  imm_s     = extend (unpack (inputs.decoded_instr.imm12_S));
   WordXL eaddr     = pack (s_rs1_val + imm_s);

   let opcode = inputs.decoded_instr.opcode;
   let funct3 = inputs.decoded_instr.funct3;
   Bool legal_ST = (   (funct3 == f3_SB)
		    || (funct3 == f3_SH)
		    || (funct3 == f3_SW)
`ifdef RV64
		    || (funct3 == f3_SD)
`endif
`ifdef ISA_F
		    || (funct3 == f3_FSW)
`endif
`ifdef ISA_D
		    || (funct3 == f3_FSD)
`endif
		    );

   let alu_outputs = alu_outputs_base;

   // FP stores are not legal unless the MSTATUS.FS bit is set
   Bool legal_FP_ST = True;
`ifdef ISA_F
   if (opcode == op_STORE_FP) begin
      legal_FP_ST = (fv_mstatus_fs (inputs.mstatus) != fs_xs_off);

      // note that the source data register for this store is in the FPR
      alu_outputs.rs_frm_fpr = True;
   end
`endif

   alu_outputs.control   = ((legal_ST && legal_FP_ST) ? CONTROL_STRAIGHT
                                                      : CONTROL_TRAP);
   alu_outputs.op_stage2 = OP_Stage2_ST;
   alu_outputs.addr      = eaddr;

   alu_outputs.val2      = inputs.rs2_val;

`ifdef ISA_F
   alu_outputs.fval2     = inputs.frs2_val;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
`ifdef ISA_F
   if (opcode == op_STORE_FP)
      alu_outputs.trace_data = mkTrace_F_STORE (fall_through_pc (inputs),
						funct3,
						fv_trace_isize (inputs),
						fv_trace_instr (inputs),
						alu_outputs.fval2,
						eaddr);
   else
`endif
      alu_outputs.trace_data = mkTrace_I_STORE (fall_through_pc (inputs),
						funct3,
						fv_trace_isize (inputs),
						fv_trace_instr (inputs),
						(alu_outputs.val2),
						eaddr);
`endif
   return alu_outputs;
endfunction

// ----------------------------------------------------------------
// MISC_MEM (FENCE and FENCE.I)
// No-ops, for now

function ALU_Outputs fv_MISC_MEM (ALU_Inputs inputs);
   let alu_outputs = alu_outputs_base;
   alu_outputs.control  = (  (inputs.decoded_instr.funct3 == f3_FENCE_I)
			   ? CONTROL_FENCE_I
			   : (  (inputs.decoded_instr.funct3 == f3_FENCE)
			      ? CONTROL_FENCE
			      : CONTROL_TRAP));

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_OTHER (fall_through_pc (inputs),
					   fv_trace_isize (inputs),
					   fv_trace_instr (inputs));
`endif
   return alu_outputs;
endfunction

// ----------------------------------------------------------------
// System instructions

function ALU_Outputs fv_SYSTEM (ALU_Inputs inputs);
   let funct3      = inputs.decoded_instr.funct3;
   let alu_outputs = alu_outputs_base;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_OTHER (fall_through_pc (inputs),
					   fv_trace_isize (inputs),
					   fv_trace_instr (inputs));
`endif

   if (funct3  == f3_PRIV) begin
`ifdef ISA_PRIV_S
      // SFENCE.VMA instruction
      if (   (inputs.decoded_instr.rd  == 0)
	  && (   (inputs.cur_priv == m_Priv_Mode)
	      || (   (inputs.cur_priv == s_Priv_Mode)
		  && (inputs.mstatus [mstatus_tvm_bitpos] == 0)))
	  && (inputs.decoded_instr.funct7 == f7_SFENCE_VMA))
	 begin
	    alu_outputs.control = CONTROL_SFENCE_VMA;
	 end
      else
`endif
      if (   (inputs.decoded_instr.rd  == 0)
	  && (inputs.decoded_instr.rs1 == 0))
	 begin
	    // ECALL instructions
	    if (inputs.decoded_instr.imm12_I == f12_ECALL) begin
	       alu_outputs.control  = CONTROL_TRAP;
	       alu_outputs.exc_code = ((inputs.cur_priv == u_Priv_Mode)
				       ? exc_code_ECALL_FROM_U
				       : ((inputs.cur_priv == s_Priv_Mode)
					  ? exc_code_ECALL_FROM_S
					  : exc_code_ECALL_FROM_M));
	    end

	    // EBREAK instruction
	    else if (inputs.decoded_instr.imm12_I == f12_EBREAK) begin
	       alu_outputs.control  = CONTROL_TRAP;
	       alu_outputs.exc_code = exc_code_BREAKPOINT;
	    end

	    // MRET instruction
	    else if (   (inputs.cur_priv >= m_Priv_Mode)
		     && (inputs.decoded_instr.imm12_I == f12_MRET))
	       begin
		  alu_outputs.control = CONTROL_MRET;
	       end

	    // SRET instruction
	    else if (   (   (inputs.cur_priv == m_Priv_Mode)
			 || (   (inputs.cur_priv == s_Priv_Mode)
			     && (inputs.mstatus [mstatus_tsr_bitpos] == 0)))
		     && (inputs.decoded_instr.imm12_I == f12_SRET))
	       begin
		  alu_outputs.control = CONTROL_SRET;
	       end


	    /*
	    // URET instruction (future: Piccolo does not support 'N' extension)
	    else if (   (inputs.cur_priv >= u_Priv_Mode)
		     && (inputs.decoded_instr.imm12_I == f12_URET))
	       begin
		  alu_outputs.control = CONTROL_URET;
	       end
	    */

	    // WFI instruction
	    else if (   (   (inputs.cur_priv == m_Priv_Mode)
			 || (   (inputs.cur_priv == s_Priv_Mode)
			     && (inputs.mstatus [mstatus_tw_bitpos] == 0))
			 || (   (inputs.cur_priv == u_Priv_Mode)
			     && (inputs.misa.n == 1)))
		     && (inputs.decoded_instr.imm12_I == f12_WFI))
	       begin
		  alu_outputs.control = CONTROL_WFI;
	       end

	    else begin
	       alu_outputs.control = CONTROL_TRAP;
	    end
	 end

      else begin
	 alu_outputs.control = CONTROL_TRAP;
      end
   end    // funct3 is f3_PRIV

   // CSRRW, CSRRWI
   else if (f3_is_CSRR_W (funct3)) begin
      WordXL rs1_val = (  (funct3 [2] == 1)
			? extend (inputs.decoded_instr.rs1)    // Immediate zimm
			: inputs.rs1_val);                     // From rs1 reg

      alu_outputs.control   = CONTROL_CSRR_W;
      alu_outputs.val1      = rs1_val;
   end

   // CSRRS, CSRRSI, CSRRC, CSRRCI
   else if (f3_is_CSRR_S_or_C (funct3)) begin
      WordXL rs1_val = (  (funct3 [2] == 1)
			? extend (inputs.decoded_instr.rs1)    // Immediate zimm
			: inputs.rs1_val);                     // From rs1 reg

      alu_outputs.control   = CONTROL_CSRR_S_or_C;
      alu_outputs.val1      = rs1_val;
   end

   // funct3 is not f3_PRIV
   else begin // (funct3 == f3_SYSTEM_ILLEGAL)
      alu_outputs.control = CONTROL_TRAP;
   end

   return alu_outputs;
endfunction: fv_SYSTEM

// ----------------------------------------------------------------
// FP Ops
// Just pass through to the FP stage

`ifdef ISA_F
function ALU_Outputs fv_FP (ALU_Inputs inputs);
   let opcode = inputs.decoded_instr.opcode;
   let funct3 = inputs.decoded_instr.funct3;
   let funct7 = inputs.decoded_instr.funct7;
   let rs2    = inputs.decoded_instr.rs2;

   // Check instruction legality
   // Is the rounding mode legal
   match {.rm, .rm_is_legal} = fv_rmode_check  (funct3, inputs.frm);

   // Is the instruction legal -- if MSTATUS.FS = fs_xs_off, FP instructions
   // are always illegal
   let inst_is_legal = (  (fv_mstatus_fs (inputs.mstatus) == fs_xs_off)
			? False
			: fv_is_fp_instr_legal (funct7,
						rm,
						rs2,
						opcode));

   let alu_outputs         = alu_outputs_base;
   alu_outputs.control     = ((inst_is_legal && rm_is_legal) ? CONTROL_STRAIGHT
			                                     : CONTROL_TRAP);
   alu_outputs.op_stage2   = OP_Stage2_FD;
   alu_outputs.rd          = inputs.decoded_instr.rd;
   alu_outputs.rm          = rm;

   // Operand values
   // The first operand may be from the FPR or GPR
   alu_outputs.val1_frm_gpr= fv_fp_val1_from_gpr (opcode, funct7, rs2);


   // Just copy the rs1_val values from inputs to outputs this covers cases
   // whenever val1 is from GPR
   alu_outputs.val1     = inputs.rs1_val;

   // Just copy the frs*_val values from inputs to outputs
   alu_outputs.fval1     = inputs.frs1_val;
   alu_outputs.fval2     = inputs.frs2_val;
   alu_outputs.fval3     = inputs.frs3_val;

   alu_outputs.rd_in_fpr = !fv_is_rd_in_GPR (funct7, rs2);

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   if (alu_outputs.rd_in_fpr)
      alu_outputs.trace_data = mkTrace_F_FRD (fall_through_pc (inputs),
					      fv_trace_isize (inputs),
					      fv_trace_instr (inputs),
					      inputs.decoded_instr.rd,
					      ?,
					      inputs.fflags,
					      inputs.mstatus);
   else
      alu_outputs.trace_data = mkTrace_F_GRD (fall_through_pc (inputs),
					      fv_trace_isize (inputs),
					      fv_trace_instr (inputs),
					      inputs.decoded_instr.rd,
					      ?,
					      inputs.fflags,
					      inputs.mstatus);
`endif
   return alu_outputs;
endfunction
`endif

// ----------------------------------------------------------------
// AMO
// Just pass through to the memory stage

`ifdef ISA_A
function ALU_Outputs fv_AMO (ALU_Inputs inputs);
   let funct3 = inputs.decoded_instr.funct3;
   let funct5 = inputs.decoded_instr.funct5;
   let funct7 = inputs.decoded_instr.funct7;

   Bool legal_f5 = (   (funct5 == f5_AMO_LR)   || (funct5 == f5_AMO_SC)

		    || (funct5 == f5_AMO_ADD)
		    || (funct5 == f5_AMO_SWAP)

		    || (funct5 == f5_AMO_AND)  || (funct5 == f5_AMO_OR) || (funct5 == f5_AMO_XOR)

		    || (funct5 == f5_AMO_MIN)  || (funct5 == f5_AMO_MINU)
		    || (funct5 == f5_AMO_MAX)  || (funct5 == f5_AMO_MAXU));

   Bool legal_width = (   (funct3 == f3_AMO_W)
		       || ((xlen == 64) && (funct3 == f3_AMO_D)) );

   let eaddr = inputs.rs1_val;

   let alu_outputs = alu_outputs_base;
   alu_outputs.control   = ((legal_f5 && legal_width) ? CONTROL_STRAIGHT : CONTROL_TRAP);
   alu_outputs.op_stage2 = OP_Stage2_AMO;
   alu_outputs.addr      = eaddr;
   alu_outputs.val1      = zeroExtend (inputs.decoded_instr.funct7);
   alu_outputs.val2      = inputs.rs2_val;

`ifdef INCLUDE_TANDEM_VERIF
   // Normal trace output (if no trap)
   alu_outputs.trace_data = mkTrace_AMO (fall_through_pc (inputs),
					 funct3,
					 fv_trace_isize (inputs),
					 fv_trace_instr (inputs),
					 inputs.decoded_instr.rd, ?,
					 inputs.rs2_val,
					 eaddr);
`endif
   return alu_outputs;
endfunction
`endif

// ----------------------------------------------------------------
// Top-level ALU function

function ALU_Outputs fv_ALU (ALU_Inputs inputs);
   let alu_outputs = alu_outputs_base;

   if (inputs.decoded_instr.opcode == op_BRANCH)
      alu_outputs = fv_BRANCH (inputs);

   else if (inputs.decoded_instr.opcode == op_JAL)
      alu_outputs = fv_JAL (inputs);

   else if (inputs.decoded_instr.opcode == op_JALR)
      alu_outputs = fv_JALR (inputs);

`ifdef ISA_M
   // OP 'M' ops MUL/ MULH/ MULHSU/ MULHU/ DIV/ DIVU/ REM/ REMU
   else if (   (inputs.decoded_instr.opcode == op_OP)
	    && f7_is_OP_MUL_DIV_REM (inputs.decoded_instr.funct7))
      begin
	 // Will be executed in MBox in next stage
	 alu_outputs.op_stage2 = OP_Stage2_M;
	 alu_outputs.rd        = inputs.decoded_instr.rd;
	 alu_outputs.val1      = inputs.rs1_val;
	 alu_outputs.val2      = inputs.rs2_val;

`ifdef INCLUDE_TANDEM_VERIF
	 // Normal trace output (if no trap)
	 alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
						fv_trace_isize (inputs),
						fv_trace_instr (inputs),
						inputs.decoded_instr.rd,
						?);
`endif
      end

`ifdef RV64
   // OP 'M' ops MULW/ DIVW/ DIVUW/ REMW/ REMUW
   else if (   (inputs.decoded_instr.opcode == op_OP_32)
	    && f7_is_OP_MUL_DIV_REM (inputs.decoded_instr.funct7))
      begin
	 // Will be executed in MBox in next stage
	 alu_outputs.op_stage2 = OP_Stage2_M;
	 alu_outputs.rd        = inputs.decoded_instr.rd;
	 alu_outputs.val1      = inputs.rs1_val;
	 alu_outputs.val2      = inputs.rs2_val;

`ifdef INCLUDE_TANDEM_VERIF
	 // Normal trace output (if no trap)
	 alu_outputs.trace_data = mkTrace_I_RD (fall_through_pc (inputs),
						fv_trace_isize (inputs),
						fv_trace_instr (inputs),
						inputs.decoded_instr.rd,
						?);
`endif
      end
`endif
`endif

   // OP_IMM and OP (shifts)
   else if (   (   (inputs.decoded_instr.opcode == op_OP_IMM)
		|| (inputs.decoded_instr.opcode == op_OP))
	    && (   (inputs.decoded_instr.funct3 == f3_SLLI)
		|| (inputs.decoded_instr.funct3 == f3_SRLI)
		|| (inputs.decoded_instr.funct3 == f3_SRAI)))
      alu_outputs = fv_OP_and_OP_IMM_shifts (inputs);

   // Remaining OP_IMM and OP (excluding shifts and 'M' ops MUL/DIV/REM)
   else if (   (inputs.decoded_instr.opcode == op_OP_IMM)
	    || (inputs.decoded_instr.opcode == op_OP))
      alu_outputs = fv_OP_and_OP_IMM (inputs);

`ifdef RV64
   else if (inputs.decoded_instr.opcode == op_OP_IMM_32)
      alu_outputs = fv_OP_IMM_32 (inputs);

   // Remaining op_OP_32 (excluding 'M' ops)
   else if (inputs.decoded_instr.opcode == op_OP_32)
      alu_outputs = fv_OP_32 (inputs);
`endif

   else if (inputs.decoded_instr.opcode == op_LUI)
      alu_outputs = fv_LUI (inputs);

   else if (inputs.decoded_instr.opcode == op_AUIPC)
      alu_outputs = fv_AUIPC (inputs);

   else if (inputs.decoded_instr.opcode == op_LOAD)
      alu_outputs = fv_LD (inputs);

   else if (inputs.decoded_instr.opcode == op_STORE)
      alu_outputs = fv_ST (inputs);

   else if (inputs.decoded_instr.opcode == op_MISC_MEM)
      alu_outputs = fv_MISC_MEM (inputs);

   else if (inputs.decoded_instr.opcode == op_SYSTEM)
      alu_outputs = fv_SYSTEM (inputs);

`ifdef ISA_A
   else if (inputs.decoded_instr.opcode == op_AMO)
      alu_outputs = fv_AMO (inputs);
`endif

`ifdef ISA_F
   else if (   (inputs.decoded_instr.opcode == op_LOAD_FP))
      alu_outputs = fv_LD (inputs);

   else if (   (inputs.decoded_instr.opcode == op_STORE_FP))
      alu_outputs = fv_ST (inputs);

   else if (   (inputs.decoded_instr.opcode == op_FP)
            || (inputs.decoded_instr.opcode == op_FMADD)
            || (inputs.decoded_instr.opcode == op_FMSUB)
            || (inputs.decoded_instr.opcode == op_FNMSUB)
            || (inputs.decoded_instr.opcode == op_FNMADD))
      alu_outputs = fv_FP (inputs);
`endif

   else begin
      alu_outputs.control = CONTROL_TRAP;

`ifdef INCLUDE_TANDEM_VERIF
      // Normal trace output (if no trap)
      alu_outputs.trace_data = mkTrace_TRAP (fall_through_pc (inputs),
					     fv_trace_isize (inputs),
					     fv_trace_instr (inputs),
					     ?,
					     ?,
					     ?,
					     ?,
					     ?);
`endif
   end

   return alu_outputs;
endfunction

// ================================================================

endpackage
