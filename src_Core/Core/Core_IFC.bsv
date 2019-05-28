// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package Core_IFC;

// ================================================================
// This package defines the interface of a Core module which
// contains:
//     - mkCPU (the RISC-V CPU)
//     - mkFabric_2x3
//     - mkNear_Mem_IO_AXI4
//     - mkPLIC_16_2_7
//     - mkTV_Encode          (Tandem-Verification logic, optional: INCLUDE_TANDEM_VERIF)
//     - mkDebug_Module       (RISC-V Debug Module, optional: INCLUDE_GDB_CONTROL)

// ================================================================
// BSV library imports

import Vector        :: *;
import GetPut        :: *;
import ClientServer  :: *;

// ================================================================
// Project imports

// Main fabric
import AXI4_Types   :: *;
import Fabric_Defs  :: *;

// External interrupt request interface
import PLIC  :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info  :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module  :: *;
`endif

// ================================================================
// The Core interface

interface Core_IFC #(numeric type t_n_interrupt_sources);

   // ----------------------------------------------------------------
   // Debugging: set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

   // ----------------------------------------------------------------
   // Soft reset
   // 'Bool' is initial 'running' state

   interface Server #(Bool, Bool)  cpu_reset_server;

   // ----------------------------------------------------------------
   // AXI4 Fabric interfaces

   // CPU IMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) cpu_imem_master;

   // CPU DMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) cpu_dmem_master;

   // ----------------------------------------------------------------
   // External interrupt sources

   interface Vector #(t_n_interrupt_sources, PLIC_Source_IFC)  core_external_interrupt_sources;

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   (* always_ready, always_enabled *)
   method Action nmi_req (Bool set_not_clear);

   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface output tuples (n,vb),
   // where 'vb' is a vector of bytes
   // with relevant bytes in locations [0]..[n-1]

`ifdef INCLUDE_TANDEM_VERIF
   interface Get #(Info_CPU_to_Verifier)  tv_verifier_info_get;
`endif

   // ----------------------------------------------------------------
   // Optional Debug Module interfaces

`ifdef INCLUDE_GDB_CONTROL
   // ----------------
   // DMI (Debug Module Interface) facing remote debugger

   interface DMI dm_dmi;

   // ----------------
   // Facing Platform
   // Non-Debug-Module Reset (reset all except DM)
   // Bool indicates 'running' hart state.

   interface Client #(Bool, Bool) ndm_reset_client;
`endif
endinterface

// ================================================================

endpackage
