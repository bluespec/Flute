// Copyright (c) 2016-2018 Bluespec, Inc. All Rights Reserved

package CPU_IFC;

// ================================================================
// BSV library imports

import Memory       :: *;
import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls       :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info         :: *;
`endif

import Fabric_Defs     :: *;
import AXI4_Lite_Types :: *;

// ================================================================
// CPU interface

interface CPU_IFC;
   // Reset
   interface Server #(Token, Token)  hart0_server_reset;

   // ----------------
   // SoC fabric connections

   // IMem to Fabric master interface
   interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User)  imem_master;

   // DMem to Fabric master interface
   interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User)  dmem_master;

   // Back-door slave interface from fabric into Near_Mem
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User)  near_mem_slave;

   // ----------------
   // Interrupts

   method Action  external_interrupt_req (Bool set_not_clear);
   method Action  timer_interrupt_req (Bool set_not_clear);
   method Action  software_interrupt_req (Bool set_not_clear);

   // ----------------
   // Set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

   // ----------------
   // Optional interface to Tandem Verifier

`ifdef INCLUDE_TANDEM_VERIF
   interface Get #(Trace_Data)  trace_data_out;
`endif

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL
   // run-control, other
   interface Server #(Bool, Bool)  hart0_server_run_halt;
   interface Put #(Bit #(4))       hart0_put_other_req;

   // GPR access
   interface MemoryServer #(5,  XLEN)  hart0_gpr_mem_server;

   // CSR access
   interface MemoryServer #(12, XLEN)  hart0_csr_mem_server;
`endif

endinterface

// ================================================================

endpackage
