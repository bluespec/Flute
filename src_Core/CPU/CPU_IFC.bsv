// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CPU_IFC;

// ================================================================
// BSV library imports

import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls       :: *;

import AXI4_Types  :: *;
import Fabric_Defs :: *;

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import DM_CPU_Req_Rsp :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info         :: *;
`endif

// ================================================================
// CPU interface

interface CPU_IFC;
   // Reset
   interface Server #(Bool, Bool)  hart0_server_reset;

   // ----------------
   // SoC fabric connections

   // IMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  imem_master;

   // DMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  dmem_master;

   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

`ifdef INCLUDE_DMEM_SLAVE
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User)  dmem_slave;
`endif

   // ----------------
   // External interrupts

   (* always_ready, always_enabled *)
   method Action  m_external_interrupt_req (Bool set_not_clear);

   (* always_ready, always_enabled *)
   method Action  s_external_interrupt_req (Bool set_not_clear);

   // ----------------
   // Software and timer interrupts (from Near_Mem_IO/CLINT)

   (* always_ready, always_enabled *)
   method Action  software_interrupt_req (Bool set_not_clear);

   (* always_ready, always_enabled *)
   method Action  timer_interrupt_req    (Bool set_not_clear);

   // ----------------
   // Non-maskable interrupt

   (* always_ready, always_enabled *)
   method Action  nmi_req (Bool set_not_clear);

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
   interface Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) hart0_gpr_mem_server;

`ifdef ISA_F
   // FPR access
   interface Server #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(FLEN)) hart0_fpr_mem_server;
`endif

   // CSR access
   interface Server #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN)) hart0_csr_mem_server;
`endif

endinterface

// ================================================================

endpackage
