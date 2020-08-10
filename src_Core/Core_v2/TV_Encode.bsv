// Copyright (c) 2013-2020 Bluespec, Inc. All Rights Reserved.

package TV_Encode;

// ================================================================
// module mkTV_Encode is a transforming FIFO
// converting Trace_Data into encoded byte vectors

// ================================================================
// BSV lib imports

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls  :: *;
import TV_Info    :: *;

// ================================================================

interface TV_Encode_IFC;
   method Action reset;

   // This module receives Trace_Data structs from the CPU and Debug Module
   interface Put #(Trace_Data)  trace_data_in;

   // This module produces tuples (n,vb),
   // where 'vb' is a vector of bytes
   // with relevant bytes in locations [0]..[n-1]
   interface Get #(Tuple2 #(Bit #(32), TV_Vec_Bytes)) tv_vb_out;
endinterface

// ================================================================

(* synthesize *)
module mkTV_Encode (TV_Encode_IFC);

   Integer verbosity = 0;    // For debugging

   Reg #(Bool) rg_reset_done <- mkReg (True);

   // Keep track of last PC for more efficient encoding of incremented PCs
   // TODO: currently always sending full PC
   Reg #(WordXL) rg_last_pc <- mkReg (0);

   FIFOF #(Trace_Data)                        f_trace_data <- mkFIFOF;
   FIFOF #(Tuple2 #(Bit #(32), TV_Vec_Bytes)) f_vb         <- mkFIFOF;

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_log_trace_RESET (rg_reset_done && (f_trace_data.first.op == TRACE_RESET));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_byte (te_op_hart_reset);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nnN, .xN } = vsubst (nn1, x1,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_GPR_WRITE (rg_reset_done && (f_trace_data.first.op == TRACE_GPR_WRITE));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_byte (te_op_state_init);
      match { .n2, .vb2 } = encode_reg (fv_gpr_regnum (td.rd), td.word1);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nnN, .xN } = vsubst (nn2, x2,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_FPR_WRITE (rg_reset_done && (f_trace_data.first.op == TRACE_FPR_WRITE));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_byte (te_op_state_init);
      match { .n2, .vb2 } = encode_reg (fv_fpr_regnum (td.rd), td.word1);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nnN, .xN } = vsubst (nn2, x2,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_CSR_WRITE (rg_reset_done && (f_trace_data.first.op == TRACE_CSR_WRITE));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_byte (te_op_state_init);
      match { .n2, .vb2 } = encode_reg (fv_csr_regnum (truncate (td.word3)), td.word4);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nnN, .xN } = vsubst (nn2, x2,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_MEM_WRITE (rg_reset_done && (f_trace_data.first.op == TRACE_MEM_WRITE));
      let td <- pop (f_trace_data);

      Bit #(2)  mem_req_size        = td.word1 [1:0];
      Byte      size_and_mem_req_op = { 2'b0, mem_req_size, te_mem_req_op_Store };
      Byte      result_and_size     = { te_mem_result_success, 2'b0, mem_req_size };

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_byte (te_op_state_init);
      match { .n2, .vb2 } = encode_byte (te_op_mem_req);
      match { .n3, .vb3 } = encode_mlen (td.word3);
      match { .n4, .vb4 } = encode_byte (size_and_mem_req_op);
      match { .n5, .vb5 } = encode_mdata (mem_req_size, td.word2);
      //match { .n6, .vb6 } = encode_byte (te_op_mem_rsp);
      //match { .n7, .vb7 } = encode_byte (result_and_size);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      //match { .nn6, .x6 } = vsubst (nn5, x5,  n6, vb6);
      //match { .nn7, .x7 } = vsubst (nn6, x6,  n7, vb7);
      //match { .nnN, .xN } = vsubst (nn7, x7,  nN, vbN);
      match { .nnN, .xN } = vsubst (nn5, x5,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_OTHER (rg_reset_done && (f_trace_data.first.op == TRACE_OTHER));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nnN, .xN } = vsubst (nn2, x2,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));

      if (verbosity != 0)
	 $display ("%0d: %m.rl_log_trace_OTHER, pc = %0h", cur_cycle, td.pc);
   endrule

   rule rl_log_trace_I_RD (rg_reset_done && (f_trace_data.first.op == TRACE_I_RD));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_reg (fv_gpr_regnum (td.rd), td.word1);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nnN, .xN } = vsubst (nn3, x3,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));

      if (verbosity != 0)
	 $display ("%0d: %m.rl_log_trace_I_RD, pc = %0h", cur_cycle, td.pc);
   endrule

`ifdef ISA_F
   // New opcode to track GPR updates due to F/D instructions. Also updates
   // the CSR FFLAGS
   rule rl_log_trace_F_GRD (rg_reset_done && (f_trace_data.first.op == TRACE_F_GRD));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_reg (fv_gpr_regnum (td.rd), td.word1);
      match { .n4, .vb4 } = encode_reg (fv_csr_regnum (csr_addr_fflags), td.word2);
      match { .n5, .vb5 } = encode_reg (fv_csr_regnum (csr_addr_mstatus), td.word4);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      match { .nnN, .xN } = vsubst (nn5, x5,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   // New opcode to track FPR updates due to F/D instructions. Also updates
   // the CSRs FFLAGS and MSTATUS
   rule rl_log_trace_F_FRD (rg_reset_done && (f_trace_data.first.op == TRACE_F_FRD));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_fpr (fv_fpr_regnum (td.rd), td.word5);
      match { .n4, .vb4 } = encode_reg (fv_csr_regnum (csr_addr_fflags), td.word2);
      match { .n5, .vb5 } = encode_reg (fv_csr_regnum (csr_addr_mstatus), td.word4);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      match { .nnN, .xN } = vsubst (nn5, x5,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule
`endif

   rule rl_log_trace_I_LOAD (rg_reset_done && (f_trace_data.first.op == TRACE_I_LOAD));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_reg (fv_gpr_regnum (td.rd), td.word1);
      match { .n4, .vb4 } = encode_eaddr (truncate (td.word3));
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nnN, .xN } = vsubst (nn4, x4,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

`ifdef ISA_F
   rule rl_log_trace_F_LOAD (rg_reset_done && (f_trace_data.first.op == TRACE_F_LOAD));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_fpr (fv_fpr_regnum (td.rd), td.word5);
      match { .n4, .vb4 } = encode_eaddr (truncate (td.word3));
      match { .n5, .vb5 } = encode_reg (fv_csr_regnum (csr_addr_mstatus), td.word4);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      match { .nnN, .xN } = vsubst (nn5, x5,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule
`endif

   rule rl_log_trace_I_STORE (rg_reset_done && (f_trace_data.first.op == TRACE_I_STORE));
      let td <- pop (f_trace_data);

      let mem_req_size = td.word1 [1:0];    // funct3

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_stval (mem_req_size, td.word2);
      match { .n4, .vb4 } = encode_eaddr (truncate (td.word3));
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nnN, .xN } = vsubst (nn4, x4,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

`ifdef ISA_F
   rule rl_log_trace_F_STORE (rg_reset_done && (f_trace_data.first.op == TRACE_F_STORE));
      let td <- pop (f_trace_data);

      let mem_req_size = td.word1 [1:0];    // funct3

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_fstval (mem_req_size, td.word5);
      match { .n4, .vb4 } = encode_eaddr (truncate (td.word3));
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nnN, .xN } = vsubst (nn4, x4,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule
`endif

   rule rl_log_trace_AMO (rg_reset_done && (f_trace_data.first.op == TRACE_AMO));
      let td <- pop (f_trace_data);

      let mem_req_size = td.word4 [1:0];    // funct3

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_reg (fv_gpr_regnum (td.rd), td.word1);
      match { .n4, .vb4 } = encode_stval (mem_req_size, td.word2);
      match { .n5, .vb5 } = encode_eaddr (truncate (td.word3));
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      match { .nnN, .xN } = vsubst (nn5, x5,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));

      if (verbosity != 0)
	 $display ("%0d: %m.rl_log_trace_AMO, pc = %0h", cur_cycle, td.pc);
   endrule

   rule rl_log_trace_CSRRX (rg_reset_done && (f_trace_data.first.op == TRACE_CSRRX));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_reg (fv_gpr_regnum (td.rd), td.word1);
      Bool csr_written = (td.word2 [0] == 1'b1);
      match { .n4, .vb4 } = (csr_written
			     ? encode_reg (fv_csr_regnum (truncate (td.word3)), td.word4)
			     : tuple2 (0, ?));
`ifdef ISA_F
      // MSTATUS.FS and .SD also updated if CSR instr wrote FFLAGS, FRM or FCSR
      Bool mstatus_written = (td.word2 [1] == 1'b1);
      match { .n5, .vb5 } = (mstatus_written
			     ? encode_reg (fv_csr_regnum (csr_addr_mstatus), td.word5)
			     : tuple2 (0, ?));
`endif
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
`ifdef ISA_F
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      match { .nnN, .xN } = vsubst (nn5, x5,  nN, vbN);
`else
      match { .nnN, .xN } = vsubst (nn4, x4,  nN, vbN);
`endif

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_TRAP (rg_reset_done && (f_trace_data.first.op == TRACE_TRAP));
      let td <- pop (f_trace_data);

      // Use new priv mode to decide which trap regs are updated (M, S or U priv)
      Priv_Mode priv            = truncate (td.rd);
      CSR_Addr  csr_addr_status = csr_addr_mstatus;
      CSR_Addr  csr_addr_cause  = csr_addr_mcause;
      CSR_Addr  csr_addr_epc    = csr_addr_mepc;
      CSR_Addr  csr_addr_tval   = csr_addr_mtval;
      if (priv == s_Priv_Mode) begin
	 csr_addr_status = csr_addr_sstatus;
	 csr_addr_cause  = csr_addr_scause;
	 csr_addr_epc    = csr_addr_sepc;
	 csr_addr_tval   = csr_addr_stval;
      end
      else if (priv == u_Priv_Mode) begin
	 csr_addr_status = csr_addr_ustatus;
	 csr_addr_cause  = csr_addr_ucause;
	 csr_addr_epc    = csr_addr_uepc;
	 csr_addr_tval   = csr_addr_utval;
      end

      // Omit the instruction if cause is instruction fault since the instruction is then bogus
      Bool is_instr_fault = (   (truncate (td.word2) == exc_code_INSTR_ACCESS_FAULT)
			     || (truncate (td.word2) == exc_code_INSTR_PAGE_FAULT));

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = (is_instr_fault
			     ? tuple2 (0, ?)
			     : encode_instr (td.instr_sz, td.instr));
      match { .n3, .vb3 } = encode_priv (td.rd);
      match { .n4, .vb4 } = encode_reg (fv_csr_regnum (csr_addr_status), td.word1);
      match { .n5, .vb5 } = encode_reg (fv_csr_regnum (csr_addr_cause),  td.word2);
      match { .n6, .vb6 } = encode_reg (fv_csr_regnum (csr_addr_epc),    truncate (td.word3));
      match { .n7, .vb7 } = encode_reg (fv_csr_regnum (csr_addr_tval),   td.word4);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      match { .nn6, .x6 } = vsubst (nn5, x5,  n6, vb6);
      match { .nn7, .x7 } = vsubst (nn6, x6,  n7, vb7);
      match { .nnN, .xN } = vsubst (nn7, x7,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_INTR (rg_reset_done && (f_trace_data.first.op == TRACE_INTR));
      let td <- pop (f_trace_data);

      // Use new priv mode to decide which trap regs are updated (M, S or U priv)
      Priv_Mode priv            = truncate (td.rd);
      CSR_Addr  csr_addr_status = csr_addr_mstatus;
      CSR_Addr  csr_addr_cause  = csr_addr_mcause;
      CSR_Addr  csr_addr_epc    = csr_addr_mepc;
      CSR_Addr  csr_addr_tval   = csr_addr_mtval;
      if (priv == s_Priv_Mode) begin
	 csr_addr_status = csr_addr_sstatus;
	 csr_addr_cause  = csr_addr_scause;
	 csr_addr_epc    = csr_addr_sepc;
	 csr_addr_tval   = csr_addr_stval;
      end
      else if (priv == u_Priv_Mode) begin
	 csr_addr_status = csr_addr_ustatus;
	 csr_addr_cause  = csr_addr_ucause;
	 csr_addr_epc    = csr_addr_uepc;
	 csr_addr_tval   = csr_addr_utval;
      end

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_priv (td.rd);
      match { .n3, .vb3 } = encode_reg (fv_csr_regnum (csr_addr_status), td.word1);
      match { .n4, .vb4 } = encode_reg (fv_csr_regnum (csr_addr_cause),  td.word2);
      match { .n5, .vb5 } = encode_reg (fv_csr_regnum (csr_addr_epc),    truncate (td.word3));
      match { .n6, .vb6 } = encode_reg (fv_csr_regnum (csr_addr_tval),   td.word4);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nn5, .x5 } = vsubst (nn4, x4,  n5, vb5);
      match { .nn6, .x6 } = vsubst (nn5, x5,  n6, vb6);
      match { .nnN, .xN } = vsubst (nn6, x6,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   rule rl_log_trace_RET (rg_reset_done && (f_trace_data.first.op == TRACE_RET));
      let td <- pop (f_trace_data);

      // Encode components of td into byte vecs
      match { .n0, .vb0 } = encode_byte (te_op_begin_group);
      match { .n1, .vb1 } = encode_pc (td.pc);
      match { .n2, .vb2 } = encode_instr (td.instr_sz, td.instr);
      match { .n3, .vb3 } = encode_priv (td.rd);
      match { .n4, .vb4 } = encode_reg (fv_csr_regnum (csr_addr_mstatus), td.word1);
      match { .nN, .vbN } = encode_byte (te_op_end_group);

      // Concatenate components into a single byte vec
      match { .nn0, .x0 } = vsubst (  0,  ?,  n0, vb0);
      match { .nn1, .x1 } = vsubst (nn0, x0,  n1, vb1);
      match { .nn2, .x2 } = vsubst (nn1, x1,  n2, vb2);
      match { .nn3, .x3 } = vsubst (nn2, x2,  n3, vb3);
      match { .nn4, .x4 } = vsubst (nn3, x3,  n4, vb4);
      match { .nnN, .xN } = vsubst (nn4, x4,  nN, vbN);

      f_vb.enq (tuple2 (nnN, xN));
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset ();
   endmethod

   interface Put trace_data_in = toPut (f_trace_data);
   interface Get tv_vb_out     = toGet (f_vb);
endmodule

// ****************************************************************
// ****************************************************************
// ****************************************************************
// Encoding Trace_Data into Byte vectors

// ================================================================
// Encodings
// cf. "Trace Protocol Specification Version 2018-09-12, Darius Rad, Bluespec, Inc."

Bit #(8) te_op_begin_group     = 1;
Bit #(8) te_op_end_group       = 2;
Bit #(8) te_op_incr_pc         = 3;
Bit #(8) te_op_full_reg        = 4;
Bit #(8) te_op_incr_reg        = 5;
Bit #(8) te_op_incr_reg_OR     = 6;
Bit #(8) te_op_addl_state      = 7;
Bit #(8) te_op_mem_req         = 8;
Bit #(8) te_op_mem_rsp         = 9;
Bit #(8) te_op_hart_reset      = 10;
Bit #(8) te_op_state_init      = 11;
Bit #(8) te_op_16b_instr       = 16;
Bit #(8) te_op_32b_instr       = 17;

Bit #(4) te_mem_req_size_8     = 0;
Bit #(4) te_mem_req_size_16    = 1;
Bit #(4) te_mem_req_size_32    = 2;
Bit #(4) te_mem_req_size_64    = 3;

Bit #(4) te_mem_req_op_Load       = 0;
Bit #(4) te_mem_req_op_Store      = 1;
Bit #(4) te_mem_req_op_LR         = 2;
Bit #(4) te_mem_req_op_SC         = 3;
Bit #(4) te_mem_req_op_AMO_swap   = 4;
Bit #(4) te_mem_req_op_AMO_add    = 5;
Bit #(4) te_mem_req_op_AMO_xor    = 6;
Bit #(4) te_mem_req_op_AMO_and    = 7;
Bit #(4) te_mem_req_op_AMO_or     = 8;
Bit #(4) te_mem_req_op_AMO_min    = 9;
Bit #(4) te_mem_req_op_AMO_max    = 10;
Bit #(4) te_mem_req_op_AMO_minu   = 11;
Bit #(4) te_mem_req_op_AMO_maxu   = 12;
Bit #(4) te_mem_req_op_ifetch     = 13;

Bit #(4) te_mem_result_success    = 0;
Bit #(4) te_mem_result_failure    = 1;

Bit #(8) te_op_addl_state_priv     = 1;
Bit #(8) te_op_addl_state_paddr    = 2;
Bit #(8) te_op_addl_state_eaddr    = 3;
Bit #(8) te_op_addl_state_data8    = 4;
Bit #(8) te_op_addl_state_data16   = 5;
Bit #(8) te_op_addl_state_data32   = 6;
Bit #(8) te_op_addl_state_data64   = 7;
Bit #(8) te_op_addl_state_mtime    = 8;
Bit #(8) te_op_addl_state_pc_paddr = 9;
Bit #(8) te_op_addl_state_pc       = 10;

// ================================================================
// Architectural register address encodings
// cf. "RISC-V External Debug Support"
//      2018-10-02_riscv_debug_spec_v0.13_DRAFT_f2873e71
//     "Table 3.3 Abstract Register Numbers"

function Bit #(16) fv_csr_regnum (CSR_Addr  csr_addr);
   return zeroExtend (csr_addr);
endfunction

function Bit #(16) fv_gpr_regnum (RegName  gpr_addr);
   return 'h1000 + zeroExtend (gpr_addr);
endfunction

function Bit #(16) fv_fpr_regnum (RegName  fpr_addr);
   return 'h1020 + zeroExtend (fpr_addr);
endfunction

// ================================================================
// vsubst substitutes vb1[j1:j1+j2-1] with vb2[0:j2-1]

function Tuple2 #(Bit #(32),
		  Vector #(TV_VB_SIZE, Byte))
   vsubst (Bit #(32) j1, Vector #(TV_VB_SIZE, Byte) vb1,
	   Bit #(32) j2, Vector #(m, Byte)          vb2);

   function Byte f (Integer j);
      Byte      x  = vb1 [j];
      Bit #(32) jj = fromInteger (j);
      if ((j1 <= jj) && (jj < j1 + j2))
	 x = vb2 [jj - j1];
      return x;
   endfunction

   let v = genWith (f);
   let n = j1 + j2;

   return tuple2 (n, v);
endfunction

// ================================================================
// Encoding of Trace_Data into byte vectors
// Every function below returns:
//     (n, vb) :: Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte))
// where vb is a vector of bytes with relevant bytes in vb[0]..vb[n-1]

// ================================================================

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_byte (Byte x);
   return tuple2 (1, replicate (x));
endfunction

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_mlen (Bit #(64) word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   Bit #(32)            n;
   vb [0] = word[7:0];
   vb [1] = word [15:8];
   vb [2] = word [23:16];
   vb [3] = word [31:24];
   vb [4] = word [39:32];
   vb [5] = word [47:40];
   vb [6] = word [55:48];
   vb [7] = word [63:56];
`ifdef RV32
   n = 4;    // MLEN = 32
`ifdef SV32
   n = 5;    // MLEN = 34
`endif
`else
   n = 8;    // MLEN = 64
`endif
   return tuple2 (n, vb);
endfunction

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_mdata (MemReqSize mem_req_size, WordXL word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   vb [0] = word[7:0];
   vb [1] = word [15:8];
   vb [2] = word [23:16];
   vb [3] = word [31:24];
`ifdef RV64
   vb [4] = word [39:32];
   vb [5] = word [47:40];
   vb [6] = word [55:48];
   vb [7] = word [63:56];
`endif
   Bit #(32) n = (1 << pack(mem_req_size));
   return tuple2 (n, vb);
endfunction

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_instr (ISize isize, Bit #(32) instr);

   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   Bit #(32)           n  = ((isize == ISIZE16BIT) ? 3 : 5);
   vb [0] = ((isize == ISIZE16BIT) ? te_op_16b_instr : te_op_32b_instr);
   vb [1] = instr [7:0];
   vb [2] = instr [15:8];
   vb [3] = instr [23:16];
   vb [4] = instr [31:24];
   return tuple2 (n, vb);
endfunction

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_reg (Bit #(16) regnum, WordXL word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   Bit #(32) n = 0;
   vb [0] = te_op_full_reg;
   vb [1] = regnum [7:0];
   vb [2] = regnum [15:8];
   vb [3] = word[7:0];
   vb [4] = word [15:8];
   vb [5] = word [23:16];
   vb [6] = word [31:24];
   n = 7;
`ifdef RV64
   vb [7] = word [39:32];
   vb [8] = word [47:40];
   vb [9] = word [55:48];
   vb [10] = word [63:56];
   n = 11;
`endif
   if (regnum == fv_gpr_regnum (0)) n = 0;
   return tuple2 (n, vb);
endfunction

`ifdef ISA_F
function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_fpr (Bit #(16) regnum, WordFL word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   Bit #(32) n = 0;
   vb [0] = te_op_full_reg;
   vb [1] = regnum [7:0];
   vb [2] = regnum [15:8];
   vb [3] = word[7:0];
   vb [4] = word [15:8];
   vb [5] = word [23:16];
   vb [6] = word [31:24];
   n = 7;
`ifdef ISA_D
   vb [7] = word [39:32];
   vb [8] = word [47:40];
   vb [9] = word [55:48];
   vb [10] = word [63:56];
   n = 11;
`endif
   return tuple2 (n, vb);
endfunction
`endif

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_priv (Bit #(5) priv);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   vb [0] = te_op_addl_state;
   vb [1] = te_op_addl_state_priv;
   vb [2] = zeroExtend (priv);
   return tuple2 (3, vb);
endfunction

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_pc (WordXL word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   Bit #(32) n;
   vb [0] = te_op_addl_state;
   vb [1] = te_op_addl_state_pc;
   vb [2] = word [7:0];
   vb [3] = word [15:8];
   vb [4] = word [23:16];
   vb [5] = word [31:24];
   n = 6;
`ifdef RV64
   vb [6] = word [39:32];
   vb [7] = word [47:40];
   vb [8] = word [55:48];
   vb [9] = word [63:56];
   n = 10;
`endif
   return tuple2 (n, vb);
endfunction

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_eaddr (WordXL word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   Bit #(32)            n;
   vb [0] = te_op_addl_state;
   vb [1] = te_op_addl_state_eaddr;
   vb [2] = word [7:0];
   vb [3] = word [15:8];
   vb [4] = word [23:16];
   vb [5] = word [31:24];
   n = 6;
`ifdef RV64
   vb [6] = word [39:32];
   vb [7] = word [47:40];
   vb [8] = word [55:48];
   vb [9] = word [63:56];
   n = 10;
`endif
   return tuple2 (n, vb);
endfunction

function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_stval (MemReqSize mem_req_size, WordXL word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   vb [0] = te_op_addl_state;
   vb [1] = case (mem_req_size)
	       f3_SIZE_B: te_op_addl_state_data8;
	       f3_SIZE_H: te_op_addl_state_data16;
	       f3_SIZE_W: te_op_addl_state_data32;
	       f3_SIZE_D: te_op_addl_state_data64;
	    endcase;
   vb [2] = word [7:0];
   vb [3] = word [15:8];
   vb [4] = word [23:16];
   vb [5] = word [31:24];
`ifdef RV64
   vb [6] = word [39:32];
   vb [7] = word [47:40];
   vb [8] = word [55:48];
   vb [9] = word [63:56];
`endif
   Bit #(32) n = (1 << pack(mem_req_size)) + 2;
   return tuple2 (n, vb);
endfunction

`ifdef ISA_F
function Tuple2 #(Bit #(32), Vector #(TV_VB_SIZE, Byte)) encode_fstval (MemReqSize mem_req_size, WordFL word);
   Vector #(TV_VB_SIZE, Byte) vb = newVector;
   vb [0] = te_op_addl_state;
   vb [1] = case (mem_req_size)
	       f3_SIZE_B: te_op_addl_state_data8;  // not possible
	       f3_SIZE_H: te_op_addl_state_data16; // not possible
	       f3_SIZE_W: te_op_addl_state_data32;
	       f3_SIZE_D: te_op_addl_state_data64;
	    endcase;
   vb [2] = word [7:0];
   vb [3] = word [15:8];
   vb [4] = word [23:16];
   vb [5] = word [31:24];
`ifdef ISA_D
   vb [6] = word [39:32];
   vb [7] = word [47:40];
   vb [8] = word [55:48];
   vb [9] = word [63:56];
`endif
   Bit #(32) n = (1 << pack(mem_req_size)) + 2;
   return tuple2 (n, vb);
endfunction
`endif

// ================================================================

endpackage
