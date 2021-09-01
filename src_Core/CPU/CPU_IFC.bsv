// Copyright (c) 2016-2021 Bluespec, Inc. All Rights Reserved

package CPU_IFC;

// ================================================================
// BSV library imports

import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;

import AXI4_Types   :: *;
import AXI_Widths   :: *;    // For Wd_Id/Addr/Data/User_Dma
import Fabric_Defs  :: *;
import Near_Mem_IFC :: *;

`ifdef FABRIC_AHBL
import AHBL_Types  :: *;
import AHBL_Defs   :: *;
`endif

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Interfaces :: *;
import DM_CPU_Req_Rsp   :: *;
`endif

`ifdef INCLUDE_PC_TRACE
import PC_Trace :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info         :: *;
`endif

import Near_Mem_IFC    :: *;

// ================================================================
// CPU interface

interface CPU_IFC;
   // ----------------
   // SoC fabric connections
`ifdef Near_Mem_TCM     // TCM based Near Mem
`ifdef FABRIC_AXI4
`ifdef DUAL_FABRIC

   // Fabric side (MMIO initiator interface)
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) nmio_master;

`else    // (!DUAL_FABRIC && FABRIC_AXI4)

   // Fabric side (MMIO initiator interface)
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) dmem_master;

`endif
`endif

`ifdef FABRIC_AHBL
   // Fabric side (MMIO initiator interface)
   interface AHBL_Master_IFC #(AHB_Wd_Data) dmem_master;
`endif


`else                   // Cache-based Near Mem
   // IMem to Fabric master interface is present in all cases except when a ITCM is present
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) imem_master;

   // Fabric master interface to memory
   interface Near_Mem_Fabric_IFC  mem_master;
`endif

    // DMA and back-door interfaces
`ifdef Near_Mem_TCM
   // AXI4 DMA target interface (for backdoor loading of TCMs in debug mode)
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  imem_dma_server;
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  dmem_dma_server;
`else
   // ----------------
   // Interface to 'coherent DMA' port of optional L2 cache
   interface AXI4_Slave_IFC #(Wd_Id_Dma, Wd_Addr_Dma, Wd_Data_Dma, Wd_User_Dma)  dma_server;

`endif


`ifdef INCLUDE_DMEM_SLAVE
   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface
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
   // Optional PC trace interface
   // Outputs a stream of 2-tuples: (cycle_count, pc)

`ifdef INCLUDE_PC_TRACE
   interface Get #(PC_Trace)  g_pc_trace;
`endif

   // ----------------
   // Optional interface to Tandem Verifier

`ifdef INCLUDE_TANDEM_VERIF
   interface Get #(Trace_Data)  trace_data_out;
`endif

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL
   interface CPU_DM_Ifc debug;
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
   interface Server #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN)) hart_csr_mem_server;
`else
   // Reset
   interface Server #(Bool, Bool)  hart_reset_server;
`endif

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // Set core's verbosity
   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);

`ifdef Near_Mem_TCM

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
   method Bit #(64) mv_tohost_value;
`endif
`endif

`ifndef Near_Mem_TCM
   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;
`endif
endinterface

// ================================================================

endpackage
