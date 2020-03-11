// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved

package CPU_Globals;

// ================================================================
// Types common to multiple CPU stages,
// including types communicated from stage to stage.

// ================================================================
// BSV library imports

// None

// ----------------
// BSV additional libs

// None

// ================================================================
// Project imports

import ISA_Decls :: *;

import TV_Info   :: *;

// ================================================================
// Output status of each stage

// EMPTY:   Stage has nothing in its input register
// BUSY:    Stage has input, but output is not ready
// PIPE:    Stage has input; driving normal output for pipeline
// NONPIPE: (In some stages) Stage has input; driving output is handled specially
//                (such as traps, CSR access, ...)

typedef enum {OSTATUS_EMPTY,
	      OSTATUS_BUSY,
	      OSTATUS_PIPE,
	      OSTATUS_NONPIPE
   } Stage_OStatus
deriving (Eq, Bits, FShow);

// ================================================================
// Branch-prediction info

// ----------------
// Epoch is a wrap-around counter that ticks every time there is a
// control-flow redirection due (e.g., due to a mispredict).
// Number of bits should be large enough to accommodate wrap-around.

typedef Bit #(2)  Epoch;

// ----------------
// CF_Info ("control flow information") is sent from the execute stage
// on all control-flow instructions and is used by the fetch stage to
// improve branch prediction.

// cf. RISC-V Unprivileged ISA Spec Section 2.5 on JAL/JALR for
// interpretation of the 'link' fields below.

typedef enum {CF_BR,
	      CF_JAL,
	      CF_JALR,
	      // TODO: extend to ECALL/traps and xRET?
	      CF_None
   } CF_Op
deriving (Eq, Bits, FShow);

typedef struct {
   CF_Op   cf_op;
   WordXL  from_PC;
   Bool    taken;            // Relevant for BR
   WordXL  fallthru_PC;      // for BR; return-PC for JAL/JALR
   WordXL  taken_PC;         // target PC for taken BR and for JAL/JALR
   } CF_Info
deriving (Bits);

CF_Info cf_info_none = CF_Info{cf_op:       CF_None,
			       from_PC:     ?,
			       taken:       ?,
			       fallthru_PC: ?,
			       taken_PC:    ?};

instance FShow #(CF_Info);
   function Fmt fshow (CF_Info x);
      Fmt fmt = $format ("{");

      if (x.cf_op == CF_None)
	 fmt = fmt + $format ("CF_None");
      else if (x.cf_op == CF_BR) begin
	 fmt = fmt + $format ("BR ");
	 fmt = fmt + $format (x.taken ? "taken " : "fallthru ");
	 fmt = fmt + $format ("[%h->%h %h]", x.from_PC, x.fallthru_PC, x.taken_PC);
      end
      else if (x.cf_op == CF_JAL)
	 fmt = fmt + $format ("JAL [%h->%h/%h]", x.from_PC, x.taken_PC, x.fallthru_PC);
      else if (x.cf_op == CF_JALR)
	 fmt = fmt + $format ("JALR [%h->%h/%h]", x.from_PC, x.taken_PC, x.fallthru_PC);

      fmt = fmt + $format ("}");
      return fmt;
   endfunction
endinstance

// ================================================================
// Bypass information
// From later to earlier stages.

// For an instruction's Rd (output GPR), a stage may:
// - have no Rd output
// - have Rd output, Rd is known but RdVal unknown
// - have Rd output, Rd is known and RdVal is known
// Note: a bypass has to stall if Rd matches and RdVal is unknown

typedef enum { BYPASS_RD_NONE, BYPASS_RD, BYPASS_RD_RDVAL } Bypass_State
deriving (Eq, Bits, FShow);

// We do not bypass CSR values, since we stall on CSRRxy insructions.

typedef struct {
   Bypass_State  bypass_state;
   RegName       rd;
   Word          rd_val;
   } Bypass
deriving (Bits);

instance FShow #(Bypass);
   function Fmt fshow (Bypass x);
      let fmt0 = $format ("Bypass {");
      let fmt1 = ((x.bypass_state == BYPASS_RD_NONE)
		  ? $format ("Rd -")
		  : $format ("Rd %0d ", x.rd) + ((x.bypass_state == BYPASS_RD)
						 ? $format ("-")
						 : $format ("rd_val:%h", x.rd_val)));
      let fmt2 = $format ("}");
      return fmt0 + fmt1 + fmt2;
   endfunction
endinstance

`ifdef ISA_F
typedef struct {
   Bypass_State  bypass_state;
   RegName       rd;
   WordFL        rd_val;
   } FBypass
deriving (Bits);

instance FShow #(FBypass);
   function Fmt fshow (FBypass x);
      let fmt0 = $format ("FBypass {");
      let fmt1 = ((x.bypass_state == BYPASS_RD_NONE)
		  ? $format ("FRd -")
		  : $format ("FRd %0d ", x.rd) + ((x.bypass_state == BYPASS_RD)
						 ? $format ("-")
						 : $format ("frd_val:%h", x.rd_val)));
      let fmt2 = $format ("}");
      return fmt0 + fmt1 + fmt2;
   endfunction
endinstance
`endif

// ----------------
// Baseline bypass info

Bypass no_bypass = Bypass {bypass_state: BYPASS_RD_NONE,
			   rd: ?,
			   rd_val: ? };

`ifdef ISA_F
FBypass no_fbypass = FBypass {bypass_state: BYPASS_RD_NONE,
			      rd: ?,
			      rd_val: ? };
`endif

// ----------------
// Bypass functions for GPRs
// Returns '(busy, val)'
// 'busy' means that the RegName is valid and matches, but the value is not available yet

function Tuple2 #(Bool, Word) fn_gpr_bypass (Bypass bypass, RegName rd, Word rd_val);
   Bool   busy = ((bypass.bypass_state == BYPASS_RD) && (bypass.rd == rd));
   WordXL val  = (  ((bypass.bypass_state == BYPASS_RD_RDVAL) && (bypass.rd == rd))
		  ? bypass.rd_val
		  : rd_val);
   return tuple2 (busy, val);
endfunction

`ifdef ISA_F
// FBypass functions for FPRs
// Returns '(busy, val)'
// 'busy' means that the RegName is valid and matches, but the value is not available yet

function Tuple2 #(Bool, WordFL) fn_fpr_bypass (FBypass bypass, RegName rd, WordFL rd_val);
   Bool busy = ((bypass.bypass_state == BYPASS_RD) && (bypass.rd == rd));
   WordFL val= (  ((bypass.bypass_state == BYPASS_RD_RDVAL) && (bypass.rd == rd))
		? bypass.rd_val
		: rd_val);
   return tuple2 (busy, val);
endfunction
`endif

// ================================================================
// Trap information

typedef struct {
   Addr      epc;
   Exc_Code  exc_code;
   Addr      tval;
   } Trap_Info
deriving (Bits, FShow);

// ================================================================
// Output from Stage F

typedef struct {
   Stage_OStatus          ostatus;

   // feedforward data
   Data_StageF_to_StageD  data_to_stageD;
   } Output_StageF
deriving (Bits);

instance FShow #(Output_StageF);
   function Fmt fshow (Output_StageF x);
      Fmt fmt = $format ("Output_StageF");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY: pc:%h", x.data_to_stageD.pc);
      else if (x.ostatus == OSTATUS_NONPIPE)
	 fmt = fmt + $format (" NONPIPE: pc:%h [***** IMPOSSIBLE! *****]", x.data_to_stageD.pc);
      else
	 fmt = fmt + $format (" PIPE: ", fshow (x.data_to_stageD));
      return fmt;
   endfunction
endinstance
// ----------------
// Data_StageF_to_StageD

typedef struct {
   Addr       pc;
   Epoch      epoch;              // Branch prediction epoch
   Priv_Mode  priv;               // Priv at which instr was fetched
   Bool       is_i32_not_i16;     // True if a regular 32b instr, not a compressed (16b) instr
   Bool       exc;                // True if exc in icache access
   Exc_Code   exc_code;
   WordXL     tval;               // Trap value; can be different from PC, with 'C' extension
   Instr      instr;              // Valid if no exception
   WordXL     pred_pc;            // Predicted next pc
   } Data_StageF_to_StageD
deriving (Bits);

instance FShow #(Data_StageF_to_StageD);
   function Fmt fshow (Data_StageF_to_StageD x);
      Fmt fmt = $format ("data_to_StageD {pc:%h  priv:%0d  epoch:%0d", x.pc, x.priv, x.epoch);
      if (x.exc)
	 fmt = fmt + $format ("  ", fshow_trap_Exc_Code (x.exc_code));
      else
	 fmt = fmt + $format ("  instr:%h  pred_pc:%h", x.instr, x.pred_pc);
      fmt = fmt + $format ("}");
      return fmt;
   endfunction
endinstance

// ================================================================
// Output from Stage D
// Just adds decoded instr info

typedef struct {
   Stage_OStatus          ostatus;

   // feedforward data
   Data_StageD_to_Stage1  data_to_stage1;
   } Output_StageD
deriving (Bits);

instance FShow #(Output_StageD);
   function Fmt fshow (Output_StageD x);
      Fmt fmt = $format ("Output_StageD");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY: pc:%h", x.data_to_stage1.pc);
      else if (x.ostatus == OSTATUS_NONPIPE)
	 fmt = fmt + $format (" NONPIPE: pc:%h [***** IMPOSSIBLE! *****]", x.data_to_stage1.pc);
      else
	 fmt = fmt + $format (" PIPE: ", fshow (x.data_to_stage1));
      return fmt;
   endfunction
endinstance

// ----------------
// Data_StageD_to_Stage1

typedef struct {
   Addr           pc;
   Priv_Mode      priv;               // Priv at which instr was fetched
   Epoch          epoch;              // Branch prediction epoch

   Bool           is_i32_not_i16;     // True if a regular 32b instr, not a compressed (16b) instr

   Bool           exc;                // True if exc in icache access
   Exc_Code       exc_code;
   WordXL         tval;               // Trap value; can be different from PC, with 'C' extension

   Instr          instr;              // Valid if no exception
   Instr_C        instr_C;            // Valid if no exception; original compressed instruction
   WordXL         pred_pc;            // Predicted next pc
   Decoded_Instr  decoded_instr;
   } Data_StageD_to_Stage1
deriving (Bits);

instance FShow #(Data_StageD_to_Stage1);
   function Fmt fshow (Data_StageD_to_Stage1 x);
      Fmt fmt = $format ("data_to_Stage1 {pc:%0h  priv:%0d  epoch:%0d", x.pc, x.priv, x.epoch);
      if (x.exc)
	 fmt = fmt + $format ("  ", fshow_trap_Exc_Code (x.exc_code), " tval %0h", x.tval);
      else begin
	 if (x.is_i32_not_i16)
	    fmt = fmt + $format ("  instr_C:%0h", x.instr_C);
	 fmt = fmt + $format ("  instr:%0h  pred_pc:%0h", x.instr, x.pred_pc);
      end
      fmt = fmt + $format ("}");
      return fmt;
   endfunction
endinstance

// ================================================================
// Output from Stage 1

// Outputs from Stage1 to pipeline control
typedef enum {  CONTROL_DISCARD
	      , CONTROL_STRAIGHT
	      , CONTROL_BRANCH
	      , CONTROL_CSRR_W
	      , CONTROL_CSRR_S_or_C
	      , CONTROL_FENCE
	      , CONTROL_FENCE_I
	      , CONTROL_SFENCE_VMA
	      , CONTROL_MRET
	      , CONTROL_SRET
	      , CONTROL_URET
	      , CONTROL_WFI
	      , CONTROL_TRAP
   } Control
deriving (Eq, Bits, FShow);

typedef struct {
   Stage_OStatus          ostatus;

   Control                control;

   Trap_Info              trap_info;

   // feedback
   Bool                   redirect;
   WordXL                 next_pc;
   CF_Info                cf_info;

   // feedforward data
   Data_Stage1_to_Stage2  data_to_stage2;
   } Output_Stage1
deriving (Bits);

instance FShow #(Output_Stage1);
   function Fmt fshow (Output_Stage1 x);
      Fmt fmt = $format ("Output_Stage1");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY pc:%h", x.data_to_stage2.pc);
      else begin
	 if (x.ostatus == OSTATUS_NONPIPE) begin
	    fmt = fmt + $format (" NONPIPE: pc:%h", x.data_to_stage2.pc);
	    fmt = fmt + $format (" ", fshow (x.control));
	    fmt = fmt + $format (" ", fshow (x.trap_info));
	 end
	 else begin
	    fmt = fmt + $format (" PIPE: ", fshow (x.control), " ", fshow (x.cf_info), fshow (x.data_to_stage2));
	 end

	 if (x.redirect)
	    fmt = fmt + $format ("\n        redirect next_pc:%h", x.next_pc);
      end
      return fmt;
   endfunction
endinstance

// ================================================================
// Data_Stage1_to_Stage2: Data output from Stage1 stage, input to DM stage

// Stage1 stage forwards, to DM, one of these 'opcodes'
// - ALU result (all non-mem, M and FD insructions)
// - DM request (Data Memory LD/ST/...)
// - Shifter Box request (SLL/SLLI, SRL/SRLI, SRA/SRAI)
// - MBox request (integer multiply/divide)
// - FDBox request (floating point ops)

typedef enum {  OP_Stage2_ALU         // Pass-through (non mem, M, FD, AMO)
	      , OP_Stage2_LD
	      , OP_Stage2_ST

`ifdef SHIFT_SERIAL
	      , OP_Stage2_SH
`endif

`ifdef ISA_M
	      , OP_Stage2_M
`endif

`ifdef ISA_A
	      , OP_Stage2_AMO
`endif

`ifdef ISA_F
	      , OP_Stage2_FD
`endif
   } Op_Stage2
deriving (Eq, Bits, FShow);

typedef struct {
   Priv_Mode  priv;
   Addr       pc;
   Instr      instr;             // For debugging. Just funct3, funct7 are
                                 // enough for functionality.
   Op_Stage2  op_stage2;
   RegName    rd;
   Addr       addr;              // Branch, jump: newPC
                                 // Mem ops and AMOs: mem addr
   WordXL     val1;              // OP_Stage2_ALU: rd_val
                                 // OP_Stage2_M

   WordXL     val2;              // OP_Stage2_ST: store-val;
                                 // OP_Stage2_M and OP_Stage2_FD: arg2

`ifdef ISA_F
   // Floating point fields
   WordFL     fval1;             // OP_Stage2_FD: arg1
   WordFL     fval2;             // OP_Stage2_FD: arg2
   WordFL     fval3;             // OP_Stage2_FD: arg3
   Bool       rd_in_fpr;         // The rd should update into FPR
   Bool       rs_frm_fpr;        // The rs is from FPR (FP stores)
   Bool       val1_frm_gpr;      // The val1 is from GPR for a FP instruction
   Bit #(3)   rounding_mode;     // rounding mode from fcsr_frm or instr.rm
`endif

`ifdef INCLUDE_TANDEM_VERIF
   Trace_Data  trace_data;
`endif
   } Data_Stage1_to_Stage2
deriving (Bits);

instance FShow #(Data_Stage1_to_Stage2);
   function Fmt fshow (Data_Stage1_to_Stage2 x);
      Fmt fmt =   $format ("data_to_Stage 2 {pc:%h  instr:%h  priv:%0d\n", x.pc, x.instr, x.priv);
      fmt = fmt + $format ("            op_stage2:", fshow (x.op_stage2), "  rd:%0d\n", x.rd);
      fmt = fmt + $format ("            addr:%h  val1:%h  val2:%h}",
			   x.addr, x.val1, x.val2);
`ifdef ISA_F
      fmt = fmt + $format ("\n");
      fmt = fmt + $format ("            fval1:%h  fval2:%h  fval3:%h}",
			   x.fval1, x.fval2, x.fval3);
`endif
      return fmt;
   endfunction
endinstance

// ================================================================
// Output from Stage 2

typedef struct {
   Stage_OStatus          ostatus;
   Trap_Info              trap_info;    // relevant if ostatus == OSTATUS_NONPIPE

   // feedback
   Bypass                 bypass;
`ifdef ISA_F
   FBypass                fbypass;
`endif

   // feedforward data
   Data_Stage2_to_Stage3  data_to_stage3;
   } Output_Stage2
deriving (Bits);

instance FShow #(Output_Stage2);
   function Fmt fshow (Output_Stage2 x);
      Fmt fmt = $format ("Output_Stage2");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY: pc:%0h", x.data_to_stage3.pc);
      else if (x.ostatus == OSTATUS_NONPIPE) begin
	 fmt = fmt + $format (" NONPIPE: ") + fshow (x.trap_info);
	 fmt = fmt + $format (" ") + fshow (x.trap_info);
      end
      else
	 fmt = fmt + $format (" PIPE: ") + fshow (x.data_to_stage3);
      return fmt;
   endfunction
endinstance

// ================================================================
// Data communicated from stage 2 to stage 3

typedef struct {
   Addr      pc;            // For debugging only
   Instr     instr;         // For debugging only
   Priv_Mode priv;

   Bool      rd_valid;
   RegName   rd;
   WordXL    rd_val;

`ifdef ISA_F
   Bool      upd_flags;
   Bool      rd_in_fpr;
   Bit #(5)  fpr_flags;
   WordFL    frd_val;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   Trace_Data             trace_data;
`endif
   } Data_Stage2_to_Stage3
deriving (Bits);

instance FShow #(Data_Stage2_to_Stage3);
   function Fmt fshow (Data_Stage2_to_Stage3 x);
      Fmt fmt =   $format ("data_to_Stage3 {pc:%h  instr:%h  priv:%0d\n", x.pc, x.instr, x.priv);
      fmt = fmt + $format ("        rd_valid:", fshow (x.rd_valid));

`ifdef ISA_F
      if (x.upd_flags)
         fmt = fmt + $format ("  fflags: %05b", fshow (x.fpr_flags));

      if (x.rd_in_fpr)
         fmt = fmt + $format ("  frd:%0d  rd_val:%h\n", x.rd, x.frd_val);
      else
`endif
         fmt = fmt + $format ("  grd:%0d  rd_val:%h\n", x.rd, x.rd_val);
      return fmt;
   endfunction
endinstance

// ================================================================
// Output from Stage 3

typedef struct {
   Stage_OStatus  ostatus;
   Bypass         bypass;
`ifdef ISA_F
   FBypass        fbypass;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   Trace_Data     trace_data;
`endif
   } Output_Stage3
deriving (Bits);

instance FShow #(Output_Stage3);
   function Fmt fshow (Output_Stage3 x);
      Fmt fmt = $format ("Output_Stage3");
      if (x.ostatus == OSTATUS_EMPTY)
	 fmt = fmt + $format (" EMPTY");
      else if (x.ostatus == OSTATUS_BUSY)
	 fmt = fmt + $format (" BUSY");
      else if (x.ostatus == OSTATUS_PIPE)
	 fmt = fmt + $format (" PIPE");
      else if (x.ostatus == OSTATUS_NONPIPE)
	 fmt = fmt + $format (" NONPIPE");
      return fmt;
   endfunction
endinstance

// ================================================================

endpackage
