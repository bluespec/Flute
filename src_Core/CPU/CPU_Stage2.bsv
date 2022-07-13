// Copyright (c) 2016-2022 Bluespec, Inc. All Rights Reserved

package CPU_Stage2;

// ================================================================
// This is Stage 2 of the CPU.
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

// Below,
//   Option 1 is the normal bypass of an Stage2 output back to Stage 1:
//       BYPASS_RD_RDVAL = bypass carries both RD and RDVAL
//       This is a longer critical path.
//   Option 2 delays the bypass to Stage 3
//       BYPASS_RD_RDVAL = bypass carries RD but not RDVAL
//       This is a shorter critical path.

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

import TV_Trace_Data :: *;

import CPU_Globals      :: *;
import Near_Mem_IFC     :: *;
import MMU_Cache_Common :: *;    // for CacheOp
import CSR_RegFile      :: *;    // For SATP, SSTATUS, MSTATUS

`ifdef SHIFT_SERIAL
import Shifter_Box  :: *;
`endif

`ifdef ISA_M
import RISCV_MBox  :: *;
`endif

`ifdef ISA_F
import FBox_Top    :: *;
import FBox_Core   :: *;   // For fv_nanbox function
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

   Reg #(Bool)                  rg_resetting  <- mkReg (False);
   Reg #(Bool)                  rg_full       <- mkReg (False);
   Reg #(Data_Stage1_to_Stage2) rg_stage2     <- mkRegU;    // From Stage 1

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

`ifdef ISA_F
   FBox_Top_IFC fbox <- mkFBox_Top (0);
`endif

   // ----------------

   let bypass_base = Bypass {bypass_state: BYPASS_RD_NONE,
			     rd:           rg_stage2.rd,
			     rd_val:       rg_stage2.val1
			     };

`ifdef ISA_F
   let fbypass_base = FBypass {bypass_state: BYPASS_RD_NONE,
			       rd:           rg_stage2.rd,
			       rd_val:       rg_stage2.fval1
			       };
`endif

   let data_to_stage3_base = Data_Stage2_to_Stage3 {priv:       rg_stage2.priv,
						    pc:         rg_stage2.pc,
						    instr:      rg_stage2.instr,

						    rd_valid:   False,
						    rd:         rg_stage2.rd,
						    rd_val:     rg_stage2.val1
`ifdef ISA_F
						    , rd_in_fpr:  False,
						    upd_flags:  False,
						    fpr_flags:  0,
						    frd_val:    rg_stage2.fval1
`endif
`ifdef INCLUDE_TANDEM_VERIF
						    , trace_data: rg_stage2.trace_data
`endif
						    };

   let  trap_info_dmem = Trap_Info {epc:      rg_stage2.pc,
				    exc_code: dcache.exc_code,
				    tval:     rg_stage2.addr };

`ifdef ISA_F
   // The FBox can only generate ILLEGAL Instruction exceptions
   let  trap_info_fbox = Trap_Info {epc:      rg_stage2.pc,
				    exc_code: exc_code_ILLEGAL_INSTRUCTION,
				    tval:     0 };
`endif

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset_begin;
      f_reset_reqs.deq;
      rg_full <= False;
      rg_resetting <= True;
`ifdef ISA_F
      fbox.server_reset.request.put (?);
`endif
   endrule

   rule rl_reset_end (rg_resetting);
      rg_resetting <= False;

`ifdef ISA_F
      let res <- fbox.server_reset.response.get;
`endif

      f_reset_rsps.enq (?);
   endrule

   // ----------------
   // Combinational output function

   function Output_Stage2 fv_out;
      Output_Stage2 output_stage2 = ?;

      // This stage is empty
      if (! rg_full) begin
	 output_stage2 = Output_Stage2 {ostatus         : OSTATUS_EMPTY,
					trap_info       : ?,
					data_to_stage3  : ?,
					bypass          : no_bypass
`ifdef ISA_F
					, fbypass       : no_fbypass
`endif
					};
      end

      // ----------------
      // Path: No-op (just relaying ALU results from previous stage to next stage)
      else if (rg_stage2.op_stage2 == OP_Stage2_ALU) begin
	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = True;

	 let bypass = bypass_base;

	 // Option 1: longer critical path: data bypassed back to previous stage
	 bypass.bypass_state = BYPASS_RD_RDVAL;

	 // Option 2: shorter critical path: data not bypassed to previous stage
	 // bypass.bypass_state = BYPASS_RD;

	 output_stage2 = Output_Stage2 {ostatus         : OSTATUS_PIPE,
					trap_info       : ?,
					data_to_stage3  : data_to_stage3,
					bypass          : bypass
`ifdef ISA_F
					, fbypass       : no_fbypass
`endif
					};
      end

      // ----------------
      // Path: DMem (LOADs and AMOs)
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

            let funct3 = instr_funct3 (rg_stage2.instr);

	    let data_to_stage3 = data_to_stage3_base;
	    data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);

`ifdef ISA_F
            data_to_stage3.rd_in_fpr = rg_stage2.rd_in_fpr;
            // A FPR load
            if (rg_stage2.rd_in_fpr) begin
`ifdef ISA_D
               // Both FLW and FLD are legal instructions
               // A FLW result
               if (funct3 == f3_FLW)
                  // needs nan-boxing when destined for a DP register file
                  data_to_stage3.frd_val = fv_nanbox (dcache.word64);

               // A FLD result
               else
                  data_to_stage3.frd_val = dcache.word64;
`else
               // Only FLW is a legal instruction
               data_to_stage3.frd_val = truncate (dcache.word64);
`endif
            end
`endif
            // GPR loads
	    data_to_stage3.rd_val = result;

            // Update the bypass channel, if not trapping (NONPIPE)
	    let bypass = bypass_base;
`ifdef ISA_F
	    let fbypass = fbypass_base;
`endif

	    if (ostatus != OSTATUS_NONPIPE) begin
`ifdef ISA_F
               // Bypassing FPR value.
               if (rg_stage2.rd_in_fpr) begin
		  // Option 1: longer critical path: data bypassed back to previous stage
		  fbypass.bypass_state = ((ostatus == OSTATUS_PIPE)
					  ? BYPASS_RD_RDVAL : BYPASS_RD);
		  fbypass.rd_val       = data_to_stage3.frd_val;

		  // Option 2: shorter critical path: data not bypassed to previous stage
		  // fbypass.bypass_state = BYPASS_RD;
               end
`endif

               // Bypassing GPR values
               if (rg_stage2.rd != 0) begin    // TODO: is this test necessary?
		  // Option 1: longer critical path: data bypassed back to previous stage
		  bypass.bypass_state = ((ostatus == OSTATUS_PIPE)
					 ? BYPASS_RD_RDVAL : BYPASS_RD);
		  bypass.rd_val       = result;

		  // Option 2: shorter critical path: data not bypassed to previous stage
		  // bypass.bypass_state = BYPASS_RD;
	       end
	    end

`ifdef INCLUDE_TANDEM_VERIF
	    let trace_data = rg_stage2.trace_data;
`ifdef ISA_F
            if (rg_stage2.rd_in_fpr) begin
               trace_data.word5 = data_to_stage3.frd_val;

               // Update MSTATUS.FS in trace packet
	       let new_mstatus = csr_regfile.mv_update_mstatus_fs (fs_xs_dirty);
               trace_data = fv_trace_update_mstatus_fs (trace_data, new_mstatus);
            end else
`endif
               trace_data.word1 = data_to_stage3.rd_val;

            data_to_stage3.trace_data = trace_data;
`endif

            output_stage2 = Output_Stage2 {ostatus         : ostatus,
					   trap_info       : trap_info_dmem,
					   data_to_stage3  : data_to_stage3,
					   bypass          : bypass
`ifdef ISA_F
					   , fbypass       : fbypass
`endif
					   };
	 end

      // ----------------
      // Path: DMem (STOREs)
      else if (rg_stage2.op_stage2 == OP_Stage2_ST) begin
	 let ostatus = (  (! dcache.valid)
			     ? OSTATUS_BUSY
			     : (  dcache.exc
				? OSTATUS_NONPIPE
				: OSTATUS_PIPE));

	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	 data_to_stage3.rd       = 0;

	 output_stage2 = Output_Stage2 {ostatus         : ostatus,
					trap_info       : trap_info_dmem,
					data_to_stage3  : data_to_stage3,
					bypass          : no_bypass
`ifdef ISA_F
					, fbypass       : no_fbypass
`endif
					};
      end

`ifdef SHIFT_SERIAL
      // ----------------
      // Path: Serial shifter
      else if (rg_stage2.op_stage2 == OP_Stage2_SH) begin
	 let ostatus = ((! shifter_box.valid) ? OSTATUS_BUSY : OSTATUS_PIPE);

	 let result = shifter_box.word;

	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	 data_to_stage3.rd_val   = result;

	 let bypass = bypass_base;
	 // Option 1: longer critical path: data bypassed back to previous stage
	 bypass.bypass_state = ((ostatus == OSTATUS_PIPE)
				? BYPASS_RD_RDVAL : BYPASS_RD);
	 bypass.rd_val       = result;

	 // Option 2: shorter critical path: data not bypassed to previous stage
	 // bypass.bypass_state = BYPASS_RD;

`ifdef INCLUDE_TANDEM_VERIF
	 let trace_data            = rg_stage2.trace_data;
	 trace_data.word1          = result;
	 data_to_stage3.trace_data = trace_data;
`endif

	 output_stage2 = Output_Stage2 {ostatus         : ostatus,
					trap_info       : ?,
					data_to_stage3  : data_to_stage3,
					bypass          : bypass
`ifdef ISA_F
					, fbypass         : no_fbypass
`endif
					};
      end
`endif

`ifdef ISA_M
      // ----------------
      // Path: integer multiply/divide
      else if (rg_stage2.op_stage2 == OP_Stage2_M) begin
	 let ostatus = ((! mbox.valid) ? OSTATUS_BUSY : OSTATUS_PIPE);

	 let result = mbox.word;

	 let data_to_stage3 = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
	 data_to_stage3.rd_val   = result;

	 let bypass = bypass_base;
	 // Option 1: longer critical path: data bypassed back to previous stage
	 bypass.bypass_state = ((ostatus == OSTATUS_PIPE) ?
				BYPASS_RD_RDVAL : BYPASS_RD);
	 bypass.rd_val       = result;

	 // Option 2: shorter critical path: data not bypassed to previous stage
	 // bypass.bypass_state = BYPASS_RD;

`ifdef INCLUDE_TANDEM_VERIF
	 let trace_data            = rg_stage2.trace_data;
	 trace_data.word1          = result;
	 data_to_stage3.trace_data = trace_data;
`endif

	 output_stage2 = Output_Stage2 {ostatus         : ostatus,
					trap_info       : ?,
					data_to_stage3  : data_to_stage3,
					bypass          : bypass
`ifdef ISA_F
					, fbypass         : no_fbypass
`endif
					};
      end
`endif

`ifdef ISA_F
      // ----------------
      // Path: floating point op
      else if (rg_stage2.op_stage2 == OP_Stage2_FD) begin
	 let ostatus = ((! fbox.valid) ? OSTATUS_BUSY : OSTATUS_PIPE);

         // Extract fields from FBOX result
	 match {.value, .fflags} = fbox.word;

	 let data_to_stage3      = data_to_stage3_base;
	 data_to_stage3.rd_valid = (ostatus == OSTATUS_PIPE);
         data_to_stage3.rd_in_fpr= rg_stage2.rd_in_fpr;
         data_to_stage3.upd_flags= True;
         data_to_stage3.fpr_flags= fflags;

`ifdef ISA_D
	 data_to_stage3.frd_val  = value;
`else
	 data_to_stage3.frd_val  = truncate (value);
`endif
`ifdef RV64
         data_to_stage3.rd_val = value;
`else
         data_to_stage3.rd_val = truncate (value);
`endif

         // result is meant for a FPR
	 let bypass  = bypass_base;
         let fbypass = fbypass_base;
         if (rg_stage2.rd_in_fpr) begin
	    // Option 1: longer critical path: data bypassed back to previous stage
            fbypass.bypass_state = ((ostatus==OSTATUS_PIPE)
				    ? BYPASS_RD_RDVAL : BYPASS_RD);
`ifdef ISA_D
            fbypass.rd_val       = value;
`else
            fbypass.rd_val       = truncate (value);
`endif
	    // Option 2: shorter critical path: data not bypassed to previous stage
	    // fbypass.bypass_state = BYPASS_RD;
         end

         // result is meant for a GPR
         else begin
	    // Option 1: longer critical path: data bypassed back to previous stage
            bypass.bypass_state = ((ostatus==OSTATUS_PIPE)
				   ? BYPASS_RD_RDVAL : BYPASS_RD);
`ifdef RV64
            bypass.rd_val       = (value);
`else
            bypass.rd_val       = truncate (value);
`endif
	    // Option 2: shorter critical path: data not bypassed to previous stage
	    // bypass.bypass_state = BYPASS_RD;
         end

         // ----------------
`ifdef INCLUDE_TANDEM_VERIF
	 let trace_data = rg_stage2.trace_data;

         if (rg_stage2.rd_in_fpr) begin
            trace_data.word5 = data_to_stage3.frd_val;
         end else begin
            trace_data.word1 = data_to_stage3.rd_val;
         end

	 data_to_stage3.trace_data = trace_data;
`endif

	 output_stage2 = Output_Stage2 {ostatus         : ostatus,
					trap_info       : trap_info_fbox,
					data_to_stage3  : data_to_stage3,
					bypass          : bypass
`ifdef ISA_F
					, fbypass       : fbypass
`endif
         };
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
	 if ((x.op_stage2 == OP_Stage2_LD)
	     || (x.op_stage2 == OP_Stage2_ST)
	     || op_stage2_amo) begin
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

            // Prepare the store value
`ifdef RV64
            Bit# (64) wdata_from_gpr = x.val2;
`else
            Bit# (64) wdata_from_gpr = zeroExtend (x.val2);
`endif

`ifdef ISA_F
`ifdef ISA_D
            Bit# (64) wdata_from_fpr = x.fval2;
`else
            Bit# (64) wdata_from_fpr = zeroExtend (x.fval2);
`endif
`endif
	    dcache.req (cache_op,
			instr_funct3 (x.instr),
`ifdef ISA_A
			amo_funct7,
`endif
			x.addr,
`ifdef ISA_F
			(x.rs_frm_fpr ? wdata_from_fpr : wdata_from_gpr),
`else
			wdata_from_gpr,
`endif
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
            // Instr fields required for decode for F/D opcodes
	    Bool is_OP_not_OP_32 = (x.instr [3] == 1'b0);
            mbox.req (is_OP_not_OP_32, funct3, x.val1, x.val2);
	 end
`endif

`ifdef ISA_F
	 // If FBox op, initiate it
	 else if (x.op_stage2 == OP_Stage2_FD) begin
	    // Instr fields required for decode for F/D opcodes
            let opcode = instr_opcode (x.instr);
	    let funct7 = instr_funct7 (x.instr);
            let rs2    = instr_rs2    (x.instr);
            Bit #(64) val1 = x.val1_frm_gpr ? extend (x.val1)
                                            : extend (x.fval1);

	    fbox.req (opcode,
		      funct7,
		      x.rounding_mode,   // rm
		      rs2,
		      val1,
		      extend (x.fval2),
		      extend (x.fval3));
         end
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
	 $display ("    CPU_Stage2.enq (Data_Stage1_to_Stage2) ", fshow (x));
   endmethod

   method Action set_full (Bool full);
      rg_full <= full;
   endmethod
endmodule

// ================================================================

endpackage
