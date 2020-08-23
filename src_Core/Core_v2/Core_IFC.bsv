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

import Near_Mem_IFC :: *;    // For Wd_{Id,Addr,Data,User}_Dma

// Main fabric
import AXI4_Types   :: *;
import Fabric_Defs  :: *;

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

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
   // Soft reset
   // 'Bool' is initial 'running' state

   interface Server #(Bool, Bool)  cpu_reset_server;

   // ----------------------------------------------------------------
   // AXI4 Fabric interfaces

   // CPU IMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) cpu_imem_master;

   // Fabric master interface to memory
   interface Near_Mem_Fabric_IFC  core_mem_master;

   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

`ifdef INCLUDE_DMEM_SLAVE
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) cpu_dmem_slave;
`endif

   // ----------------------------------------------------------------
   // Interface to 'coherent DMA' port of optional L2 cache

   interface AXI4_Slave_IFC #(Wd_Id_Dma, Wd_Addr_Dma, Wd_Data_Dma, Wd_User_Dma)  dma_server;

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

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // Debugging: set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
   method Bit #(64) mv_tohost_value;
`endif

   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;

endinterface

// ================================================================

endpackage
