// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package Core_IFC;

// ================================================================
// This package defines the interface of the Core module which
// contains:
// - The RISC-V CPU with Tandem Verifier (TV) logic
// - RISC-V Debug Module

// ================================================================
// BSV library imports

import GetPut        :: *;
import ClientServer  :: *;

// ================================================================
// Project imports

// Main fabric
import AXI4_Types   :: *;
import Fabric_Defs  :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info  :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module  :: *;
`endif

// ================================================================
// The Core interface

interface Core_IFC;

   // ----------------------------------------------------------------
   // Core CPU interfaces

   // Reset
   interface Server #(Bit #(0), Bit #(0))  cpu_reset_server;

   // CPU IMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) cpu_imem_master;

   // CPU DMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) cpu_dmem_master;

   // CPU Back-door slave interface from fabric
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) cpu_slave;

   // External interrupts
   (* always_ready, always_enabled *)
   method Action cpu_external_interrupt_req (Bool set_not_clear);

   // ----------------------------------------------------------------
   // Set core's verbosity
   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface output tuples (n,vb),
   // where 'vb' is a vector of bytes
   // with relevant bytes in locations [0]..[n-1]

   interface Get #(Info_CPU_to_Verifier)  tv_verifier_info_get;
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // Optional Debug Module interfaces

   // ----------------
   // DMI (Debug Module Interface) facing remote debugger

   interface DMI dm_dmi;

   // ----------------
   // Facing Platform

   // Non-Debug-Module Reset (reset all except DM)
   interface Get #(Bit #(0)) dm_ndm_reset_req_get;

   // Read/Write RISC-V memory
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) dm_master;
`endif
endinterface

// ================================================================

endpackage
