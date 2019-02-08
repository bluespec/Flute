// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

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

import AXI4_Types  :: *;
import Fabric_Defs :: *;

// ================================================================
// CPU interface

interface CPU_IFC;
   // Reset
   interface Server #(Token, Token)  hart0_server_reset;

   // ----------------
   // SoC fabric connections

   // IMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  imem_master;

   // DMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  dmem_master;

   // Back-door slave interface from fabric into Near_Mem
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  near_mem_slave;

   // ----------------
   // External interrupts

   (* always_ready, always_enabled *)
   method Action  external_interrupt_req (Bool set_not_clear);

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

`ifdef ISA_F
   // FPR access
   interface MemoryServer #(5,  FLEN)  hart0_fpr_mem_server;
`endif

   // CSR access
   interface MemoryServer #(12, XLEN)  hart0_csr_mem_server;
`endif

endinterface

// ================================================================

endpackage
