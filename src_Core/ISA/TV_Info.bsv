// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved

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

// ================================================================
// The struct fields represent the following, for each instruction:
// dut_exc_taken      True if the DUT took an exception (interrupt/trap), False otherwise
//
// dut_pc             PC of the current instruction
//                    Will be same as new CSR_MEPC on exceptions
//                    Set to pc_tv_cmd for debug module activity (reset, mem/reg writes)
//
// dut_addr           New PC for exception branch, jump, xRET: new PC
//                    Mem effective addr for mem ops and AMOs
//                    CSR addr for CSRR{S,C,W}
//
// dut_data1          New rd_val for ALU ops, LOAD, LOAD_FP, LR, SC, CSRR{S,C,W}
//                    New csr_mstatus for exceptions and xRET
//                    RV32: New rd_val [31:0] for FP Instructions (lower 32b for 'double' results)
//                    RV64: New rd_val for FP Instructions
//
// dut_data2          New csr_cause for exceptions
//                    New mem data for STORE, STORE_FP, SC, AMO
//                    New CSR data for CSRR{S,C,W}
//                    RV32: New rd_val [63:32] for FP Instructions (upper 32b for 'double' results)
//
// dut_instr_valid    True if the next argument 'dut_instr' is valid, False otherwise
// dut_instr          The instruction executed by the DUT

// ================================================================
// For RV32 CPUs.
// TODO: change name to Info_RV32_... for symmetry with RV64

`ifdef RV32

typedef struct {
   Bool       exc_taken;
   Bit #(32)  pc;
   Bit #(32)  addr;
   Bit #(32)  data1;
   Bit #(32)  data2;
   Bool       instr_valid;
   Bit #(32)  instr;
} Info_CPU_to_Verifier deriving (Bits, FShow);

instance DefaultValue #(Info_CPU_to_Verifier);
   defaultValue = Info_CPU_to_Verifier {
        exc_taken    : False
      , pc           : 0
      , addr         : 0
      , data1        : 0
      , data2        : 0
      , instr_valid  : False
      , instr        : 0
   };
endinstance

`endif

// ================================================================
// For RV64 CPUs.

`ifdef RV64

typedef struct {
   Bool       exc_taken;
   Bit #(64)  pc;
   Bit #(64)  addr;
   Bit #(64)  data1;
   Bit #(64)  data2;
   Bool       instr_valid;
   Bit #(32)  instr;
} Info_CPU_to_Verifier deriving (Bits, FShow);

instance DefaultValue #(Info_CPU_to_Verifier);
   defaultValue = Info_CPU_to_Verifier {
        exc_taken    : False
      , pc           : 0
      , addr         : 0
      , data1        : 0
      , data2        : 0
      , instr_valid  : False
      , instr        : 0
   };
endinstance

`endif

Integer pc_tv_cmd = 'hffffffff;
Integer tv_cmd_reset = 0;
Integer tv_cmd_mem_write8 = 1;
Integer tv_cmd_mem_write16 = 2;
Integer tv_cmd_mem_write32 = 3;
Integer tv_cmd_mem_write64 = 4;
Integer tv_cmd_reg_write = 5;
Integer tv_cmd_mip = 6;

// ================================================================

endpackage
