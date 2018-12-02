// Copyright (c) 2016-2018 Bluespec, Inc. All Rights Reserved

package CPU_Stage2;

// ================================================================
// This is Stage 2 of the "Flute" CPU.
// It is the "DM" stage ("Data Memory"), which is the main function.

// However, this stage also contains all other (potentially) long-latency
// operations:
//    MBox ("M" extension ops, integer multiply/divide)
//    FDBox ("FD" extension ops, single and double precision floating point)

// This stage sends out Tandem Verifier information for pipelined instructions

// Note: $displays are indented by (stage num x 4) spaces.
// for traditional pipeline display
//     IF
//         DM
//             WB
// i.e., 8 spaces for this stage.

// ================================================================
// Exports

export
CPU_Stage2_IFC (..),
mkCPU_Stage2;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import ConfigReg    :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;

// ================================================================
// Project imports

import ISA_Decls     :: *;

import TV_Info       :: *;

import CPU_Globals   :: *;
import Near_Mem_IFC  :: *;
import CSR_RegFile   :: *;    // For SATP, SSTATUS, MSTATUS

`ifdef SHIFT_SERIAL
import Shifter_Box  :: *;
`endif

`ifdef ISA_M
import RISCV_MBox  :: *;
`endif

`ifdef ISA_FD
import RISCV_FBox  :: *;
`endif

// ================================================================
// Interface

interface CPU_Stage2_IFC;
   // ---- Reset
   interface Server #(Token, Token) server_reset;

   // ---- Output
   (* always_ready *)
   method Output_Stage2  out;

   (* always_ready *)
   method Action deq;

   // ---- Input
   (* always_ready *)
   method Action enq (Data_Stage1_to_Stage2 x);

   (* always_ready *)
   method Action set_full (Bool full);
endinterface

// ================================================================
// Implementation module

module mkCPU_Stage2 #(Bit #(4)         verbosity,
		      CSR_RegFile_IFC  csr_regfile,    // for SATP and SSTATUS: TODO carry in Data_Stage1_to_Stage2
		      DMem_IFC         dcache)
                    (CPU_Stage2_IFC);

   FIFOF #(Token) f_reset_reqs <- mkFIFOF;
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   Reg #(Bool)                  rg_full   <- mkReg (False);
   Reg #(Data_Stage1_to_Stage2) rg_stage2 <- mkRegU;    // From Stage 1

   // ----------------
   // Serial shifter box

`ifdef SHIFT_SERIAL
   Shifter_Box_IFC shifter_box <- mkShifter_Box;
`endif

   // ----------------
   // Integer multiply/divide box

`ifdef ISA_M
   RISCV_MBox_IFC mbox <- mkRISCV_MBox;
`endif

   // ----------------
   // Floating point box

`ifdef ISA_FD
   RISCV_FBox_IFC fbox <- mkRISCV_FBox;
`endif

   // ----------------

   let bypass_base = Bypass {bypass_state: BYPASS_RD_NONE,
			     rd:           rg_stage2.rd,
			     rd_val:       rg_stage2.val1 };

   let data_to_stage3_base = Data_Stage2_to_Stage3 {priv:      rg_stage2.priv,
						    pc:        rg_stage2.pc,
						    instr:     rg_stage2.instr,
						    rd_valid:  False,
						    rd:        rg_stage2.rd,
						    rd_val:    rg_stage2.val1};

   let  trap_info_dmem = Trap_Info {epc:      rg_stage2.pc,
				    exc_code: dcache.exc_code,
				    tval:     rg_stage2.addr };

`ifdef ISA_FD
   let  trap_info_fbox = Trap_Info {epc:      rg_stage2.pc,
				    exc_code: fbox.exc_code,
				    tval:     0 };
`endif

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset;
      f_reset_reqs.deq;
      rg_full <= False;
      f_reset_rsps.enq (?);
   endrule

   // ----------------
   // Combinational output function

   function Output_Stage2 fv_out;
      Output_Stage2 output_stage2 = ?;

      // This stage is empty
      if (! rg_full) begin
	 output_stage2 = Output_Stage2 {ostatus:         OSTATUS_EMPTY,
					trap_info:       ?,
					data_to_stage3:  ?,
					bypass:          no_bypass,
					trace_data:      ?
					};
      end

      // This stage is just relaying ALU results from previous stage to next stage
      else if (rg_stage2.op_stage2 == OP_Stage2_ALU) begin
	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = True;

	 let bypass = bypass_base;
	 bypass.bypass_state = BYPASS_RD_RDVAL;

	 let trace_data   = ?;
`ifdef INCLUDE_TANDEM_VERIF
	 trace_data = rg_stage2.trace_data;
`endif

	 output_stage2 = Output_Stage2 {ostatus:         OSTATUS_PIPE,
					trap_info:       ?,
					data_to_stage3:  data_to_stage3,
					bypass:          bypass,
					trace_data:      trace_data};
      end

      // This stage is doing a LOAD or AMO
      else if (   (rg_stage2.op_stage2 == OP_Stage2_LD)
`ifdef ISA_A
	       || (rg_stage2.op_stage2 == OP_Stage2_AMO)
`endif
	       )
	 begin
	    let ostatus = (  (! dcache.valid)
			   ? OSTATUS_BUSY
			   : (  dcache.exc
			      ? OSTATUS_NONPIPE
			      : OSTATUS_PIPE));

	    WordXL result = truncate (dcache.word64);

	    let data_to_stage3 = data_to_stage3_base;
	    data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	    data_to_stage3.rd_val   = result;

	    let bypass = bypass_base;
	    if (rg_stage2.rd != 0) begin    // TODO: is this test necessary?
	       bypass.bypass_state = ((ostatus == OSTATUS_PIPE) ? BYPASS_RD_RDVAL : BYPASS_RD);
	       bypass.rd_val       = result;
	    end

	    let trace_data   = ?;
`ifdef INCLUDE_TANDEM_VERIF
	    trace_data   = rg_stage2.trace_data;
`endif
	    trace_data.word1 = result;

	    output_stage2 = Output_Stage2 {ostatus:         ostatus,
					   trap_info:       trap_info_dmem,
					   data_to_stage3:  data_to_stage3,
					   bypass:          bypass,
					   trace_data:      trace_data};
	 end

      // This stage is doing a STORE
      else if (rg_stage2.op_stage2 == OP_Stage2_ST) begin
	 let ostatus = (  (! dcache.valid)
			     ? OSTATUS_BUSY
			     : (  dcache.exc
				? OSTATUS_NONPIPE
				: OSTATUS_PIPE));

	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	 data_to_stage3.rd       = 0;
	 data_to_stage3.rd_val   = ?;

	 let trace_data   = ?;
`ifdef INCLUDE_TANDEM_VERIF
	 trace_data   = rg_stage2.trace_data;
`endif

	 output_stage2 = Output_Stage2 {ostatus:        ostatus,
					trap_info:      trap_info_dmem,
					data_to_stage3: data_to_stage3,
					bypass:         no_bypass,
					trace_data:     trace_data};
      end

`ifdef SHIFT_SERIAL
      // This stage is doing a serial shift
      else if (rg_stage2.op_stage2 == OP_Stage2_SH) begin
	 let ostatus = ((! shifter_box.valid) ? OSTATUS_BUSY : OSTATUS_PIPE);

	 let result = shifter_box.word;

	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	 data_to_stage3.rd_val   = result;

	 let bypass = bypass_base;
	 bypass.bypass_state = ((ostatus == OSTATUS_PIPE) ? BYPASS_RD_RDVAL : BYPASS_RD);
	 bypass.rd_val       = result;

	 let trace_data   = ?;
`ifdef INCLUDE_TANDEM_VERIF
	 trace_data   = rg_stage2.trace_data;
`endif
	 trace_data.word1 = result;

	 output_stage2 = Output_Stage2 {ostatus:         ostatus,
					trap_info:       ?,
					data_to_stage3:  data_to_stage3,
					bypass:          bypass,
					trace_data:      trace_data};
      end
`endif

`ifdef ISA_M
      // This stage is doing an integer multiply/divide
      else if (rg_stage2.op_stage2 == OP_Stage2_M) begin
	 let ostatus = ((! mbox.valid) ? OSTATUS_BUSY : OSTATUS_PIPE);

	 let result = mbox.word;

	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	 data_to_stage3.rd_val   = result;

	 let bypass = bypass_base;
	 bypass.bypass_state = ((ostatus == OSTATUS_PIPE) ? BYPASS_RD_RDVAL : BYPASS_RD);
	 bypass.rd_val       = result;

	 let trace_data   = ?;
`ifdef INCLUDE_TANDEM_VERIF
	 trace_data   = rg_stage2.trace_data;
`endif
	 trace_data.word1 = result;

	 output_stage2 = Output_Stage2 {ostatus:         ostatus,
					trap_info:       ?,
					data_to_stage3:  data_to_stage3,
					bypass:          bypass,
					trace_data:      trace_data};
      end
`endif

`ifdef ISA_FD
      // This stage is doing a floating point op
      else if (rg_stage2.op_stage2 == OP_Stage2_FD) begin
	 let ostatus = (  (! fbox.valid)
			? OSTATUS_BUSY
			: (  fbox.exc
			   ? OSTATUS_NONPIPE
			   : OSTATUS_PIPE));

	 let result = fbox.word;

	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	 data_to_stage3.rd_val   = result;

	 let bypass = bypass_base;
	 bypass.bypass_state = ((ostatus == OSTATUS_PIPE) ? BYPASS_RD_RDVAL : BYPASS_RD);
	 bypass.rd_val       = result;

	 let trace_data   = ?;
`ifdef INCLUDE_TANDEM_VERIF
	 trace_data   = rg_stage2.trace_data;
`endif
	 trace_data.word1 = result;

	 output_stage2 = Output_Stage2 {ostatus:         ostatus,
					trap_info:       trap_info_fbox,
					data_to_stage3:  data_to_stage3,
					bypass:          bypass,
					trace_data:      trace_data};
      end
`endif

      return output_stage2;
   endfunction

   // ----------------
   // Initiate DM, Shifter box, MBox or FBox op

   function Action fa_enq (Data_Stage1_to_Stage2 x);
      action
	 rg_stage2  <= x;

	 let funct3 = instr_funct3 (x.instr);

	 // If DMem access, initiate it
`ifdef ISA_A
	 Bool op_stage2_amo = (x.op_stage2 == OP_Stage2_AMO);
	 Bit #(7) amo_funct7 = x.val1 [6:0];
`else
	 Bool op_stage2_amo = False;
	 Bit #(7) amo_funct7 = 0;
`endif
	 if ((x.op_stage2 == OP_Stage2_LD) || (x.op_stage2 == OP_Stage2_ST) || op_stage2_amo) begin
	    WordXL   mstatus     = csr_regfile.read_mstatus;
`ifdef ISA_PRIV_S
	    Bit #(1) sstatus_SUM = (csr_regfile.read_sstatus) [18];
`else
	    Bit #(1) sstatus_SUM = 0;
`endif
	    Bit #(1) mstatus_MXR = mstatus [19];
	    Priv_Mode  mem_priv = x.priv;
	    if (mstatus [17] == 1'b1) begin
	       mem_priv = mstatus [12:11];
	       // $display ("    S2.fa_enq: mem_priv %0d => %0d (mstatus.MPP) due to mstatus.MPRV", x.priv, mem_priv);
	    end

	    CacheOp cache_op = ?;
	    if      (x.op_stage2 == OP_Stage2_LD)  cache_op = CACHE_LD;
	    else if (x.op_stage2 == OP_Stage2_ST)  cache_op = CACHE_ST;
`ifdef ISA_A
	    else if (x.op_stage2 == OP_Stage2_AMO) cache_op = CACHE_AMO;
`endif

	    dcache.req (cache_op,
			instr_funct3 (x.instr),
`ifdef ISA_A
			amo_funct7,
`endif
			x.addr,
			zeroExtend (x.val2),
			mem_priv,
			sstatus_SUM,
			mstatus_MXR,
			csr_regfile.read_satp);
	 end

`ifdef SHIFT_SERIAL
	 // If Shifter box op, initiate it
	 else if (x.op_stage2 == OP_Stage2_SH)
	    shifter_box.req (unpack (funct3 [2]), x.val1, x.val2);
`endif

`ifdef ISA_M
	 // If MBox op, initiate it
	 else if (x.op_stage2 == OP_Stage2_M) begin
	    Bool is_OP_not_OP_32 = (x.instr [3] == 1'b0);
	    mbox.req (is_OP_not_OP_32, funct3, x.val1, x.val2);
	 end
`endif

`ifdef ISA_FD
	 // If FBox op, initiate it
	 else if (x.op_stage2 == OP_Stage2_FD)
	    fbox.req (funct3, x.val1, x.val2);
`endif
      endaction
   endfunction

   // ----------------------------------------------------------------
   // INTERFACE

   // ---- Reset
   interface server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // ---- Output
   method Output_Stage2  out;
      return fv_out;
   endmethod

   method Action deq ();
      noAction;
   endmethod

   // ---- Input
   method Action enq (Data_Stage1_to_Stage2 x);
      fa_enq (x);

      if (verbosity > 1)
	 $display ("    CPU_Stage2.enq (Data_Stage1_to_Stage2)");
   endmethod

   method Action set_full (Bool full);
      rg_full <= full;
   endmethod
endmodule

// ================================================================

endpackage
