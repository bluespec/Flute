// Copyright (c) 2018 Bluespec, Inc. All Rights Reserved.

package BRVF_Core_IFC;

// ================================================================
// This package defines the interface of the BRVF_Core module which
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
import AXI4_Lite_Types  :: *;
import AXI4_Lite_Fabric :: *;
import Fabric_Defs      :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module   :: *;
`endif

// ================================================================
// The BRVF_Core interface

interface BRVF_Core_IFC;

   // ----------------------------------------------------------------
   // Core CPU interfaces

   // Reset
   interface Server #(Bit #(0), Bit #(0))  cpu_reset_server;

   // CPU IMem to Fabric master interface
   interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) cpu_imem_master;

   // CPU DMem to Fabric master interface
   interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) cpu_dmem_master;

   // CPU Back-door slave interface from fabric
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) cpu_slave;

   // Interrupts
   method Action cpu_external_interrupt_req (Bool set_not_clear);
   method Action cpu_timer_interrupt_req (Bool set_not_clear);
   method Action cpu_software_interrupt_req (Bool set_not_clear);

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface

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
   interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) dm_master;
`endif
endinterface

// ================================================================

endpackage
