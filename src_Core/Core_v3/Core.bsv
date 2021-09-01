// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package Core;

// ================================================================
// This package defines:
//     Core_IFC
//     mkCore #(Core_IFC)
//
// mkCore instantiates:
//     - mkCPU (the RISC-V CPU)
//     - mkFabric_2x3
//     - mkNear_Mem_IO_AXI4
//     - mkPLIC_16_1_7
//     - mkTV_Encode          (Tandem-Verification logic, optional: INCLUDE_TANDEM_VERIF)
//     - mkDebug_Module       (RISC-V Debug Module, optional: INCLUDE_GDB_CONTROL)
// and connects them all up.

// ================================================================
// BSV library imports

import Vector        :: *;
import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

// Main fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User
import SoC_Map      :: *;

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Interfaces   :: *;
import Debug_Triage       :: *;
`endif

import Core_IFC          :: *;
import CPU_IFC           :: *;
import CPU               :: *;

import Local_Fabric      :: *;

import Near_Mem_IFC      :: *;    // For Wd_{Id,Addr,Data,User}_Dma
import Near_Mem_IO_AXI4  :: *;
import PLIC              :: *;
import PLIC_16_1_7       :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info   :: *;
import TV_Encode :: *;
`endif

// TV_Taps needed when both GDB_CONTROL and TANDEM_VERIF are present
`ifdef INCLUDE_GDB_CONTROL
`ifdef INCLUDE_TANDEM_VERIF
import TV_Taps :: *;
`endif
`endif

// ================================================================

// This function is needed for the debug module's sbus client port,
// which sometimes requests partial words.

function AXI4_Master_IFC#(i,a,d,u) fn_8byte_align(AXI4_Master_IFC#(i,a,d,u) ifc);
   let nbytes = valueof(d)/8;
   let isize  = valueof(TSub#(TLog#(d),3));
   Bit#(a) mask = ~(fromInteger(nbytes - 1));
   return ( interface AXI4_Master_IFC;
	       method m_awvalid = ifc.m_awvalid;
	       method m_awid = ifc.m_awid;
	       method m_awaddr = (ifc.m_awaddr & mask);
	       method m_awlen = ifc.m_awlen;
	       method m_awsize = fromInteger(isize); //ifc.m_awsize;
	       method m_awburst = ifc.m_awburst;
	       method m_awlock = ifc.m_awlock;
	       method m_awcache = ifc.m_awcache;
	       method m_awprot = ifc.m_awprot;
	       method m_awqos = ifc.m_awqos;
	       method m_awregion = ifc.m_awregion;
	       method m_awuser = ifc.m_awuser;
	       method m_awready = ifc.m_awready;
	       method m_wvalid = ifc.m_wvalid;
	       method m_wdata = ifc.m_wdata;
	       method m_wstrb = ifc.m_wstrb;
	       method m_wlast = ifc.m_wlast;
	       method m_wuser = ifc.m_wuser;
	       method m_wready = ifc.m_wready;
	       method m_bvalid = ifc.m_bvalid;
	       method m_bready = ifc.m_bready;
	       method m_arvalid = ifc.m_arvalid;
	       method m_arid = ifc.m_arid;
	       method m_araddr = (ifc.m_araddr & mask);
	       method m_arlen = ifc.m_arlen;
	       method m_arsize = fromInteger(isize); //ifc.m_arsize;
	       method m_arburst = ifc.m_arburst;
	       method m_arlock = ifc.m_arlock;
	       method m_arcache = ifc.m_arcache;
	       method m_arprot = ifc.m_arprot;
	       method m_arqos = ifc.m_arqos;
	       method m_arregion = ifc.m_arregion;
	       method m_aruser = ifc.m_aruser;
	       method m_arready = ifc.m_arready;
	       method m_rvalid = ifc.m_rvalid;
	       method m_rready = ifc.m_rready;
	   endinterface );
endfunction

// ================================================================
// The Core module

(* synthesize *)
module mkCore (Core_IFC #(N_External_Interrupt_Sources));

   // ================================================================
   // STATE

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // The CPU
   // CPU resets after por are controlled by the reset server.
   CPU_IFC  cpu <- mkCPU;

   // A fabric for connecting local components {CPU, Debug_Module} to {memory, Near_Mem_IO,
   // PLIC, ITCM backdoor, DTCM backdoor}. The configuration depends on an array of compile macros.
   // The only case we do not have a local fabric is if FABRIC_AHBL && !DUAL_FABRIC. Currently,
   // both these macros are only used by TCM based near-mems but may be included in cache based
   // implementations as well in the future.

`ifdef DUAL_FABRIC
   Local_Fabric_IFC  local_fabric <- mkLocal_Fabric;
`else
`ifndef FABRIC_AHBL
   Local_Fabric_IFC  local_fabric <- mkLocal_Fabric;
`endif
`endif

   // Near_Mem_IO
   Near_Mem_IO_AXI4_IFC  clint <- mkNear_Mem_IO_AXI4;

   // PLIC (Platform-Level Interrupt Controller)
   PLIC_IFC_16_1_7  plic <- mkPLIC_16_1_7;

   // Reset requests from SoC and responses to SoC
   // 'Bool' is 'running' state
   FIFOF #(Bool) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bool) f_reset_rsps <- mkFIFOF;

`ifdef INCLUDE_TANDEM_VERIF
   // The TV encoder transforms Trace_Data structures produced by the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier
   TV_Encode_IFC tv_encode <- mkTV_Encode;
`endif

   // ================================================================
   // RESET
   // There are two sources of reset requests to the CPU: externally
   // from the SoC and, optionally, the DM.  When both requestors are
   // present (i.e., DM is present), we merge the reset requests into
   // the CPU, and we remember which one was the requestor in
   // f_reset_requestor, so that we know whome to respond to.

   Bit #(1) reset_requestor_dm  = 0;
   Bit #(1) reset_requestor_soc = 1;
   FIFOF #(Bit #(1)) f_reset_requestor <- mkFIFOF;
`ifdef INCLUDE_GDB_CONTROL
   Server #(Bool, Bool)  hart_reset_server = cpu.debug.hart_reset_server;
`else
   Server #(Bool, Bool)  hart_reset_server = cpu.hart_reset_server;
`endif

   // Reset-hart0 request from SoC
   rule rl_cpu_hart0_reset_from_soc_start;
      let running <- pop (f_reset_reqs);

      hart_reset_server.request.put (running);   // CPU
      clint.server_reset.request.put (?);        // Near_Mem_IO
      plic.server_reset.request.put (?);         // PLIC
      local_fabric.reset;                        // Local Fabric

      // Remember the requestor, so we can respond to it
      f_reset_requestor.enq (reset_requestor_soc);
      $display ("%0d: Core.rl_cpu_hart0_reset_from_soc_start", cur_cycle);
   endrule


`ifdef INCLUDE_GDB_CONTROL
   // Reset-hart0 from Debug Module
   Server #(Bool, Bool) dm_reset_server = ( interface Server;
      interface Put request;
	 method Action put(running);
	    hart_reset_server.request.put (running);    // CPU
	    clint.server_reset.request.put (?);        // Near_Mem_IO
	    plic.server_reset.request.put (?);               // PLIC
	    local_fabric.reset;                                // Local 2x3 fabric

	    // Remember the requestor, so we can respond to it
	    f_reset_requestor.enq (reset_requestor_dm);
	    $display ("%0d: Core.rl_cpu_hart0_reset_from_dm_start", cur_cycle);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue#(Bool) get() if (f_reset_requestor.first == reset_requestor_dm);
	    let running <- hart_reset_server.response.get;     // CPU
	    let rsp2    <- clint.server_reset.response.get;    // Near_Mem_IO
	    let rsp3    <- plic.server_reset.response.get;     // PLIC

	    clint.set_addr_map (zeroExtend (soc_map.m_clint_addr_base), zeroExtend (soc_map.m_clint_addr_lim));
	    plic.set_addr_map (zeroExtend (soc_map.m_plic_addr_base), zeroExtend (soc_map.m_plic_addr_lim));

	    f_reset_requestor.deq;
	    $display ("%0d: Core.rl_cpu_hart0_reset_complete", cur_cycle);
	    return running;
	 endmethod
      endinterface
					   endinterface );
`endif

   rule rl_cpu_hart0_reset_complete  (f_reset_requestor.first == reset_requestor_soc);
      let running <- hart_reset_server.response.get;     // CPU
      let rsp2    <- clint.server_reset.response.get;    // Near_Mem_IO
      let rsp3    <- plic.server_reset.response.get;     // PLIC

      clint.set_addr_map (zeroExtend (soc_map.m_clint_addr_base), zeroExtend (soc_map.m_clint_addr_lim));
      plic.set_addr_map (zeroExtend (soc_map.m_plic_addr_base), zeroExtend (soc_map.m_plic_addr_lim));

      f_reset_rsps.enq (running);
      f_reset_requestor.deq;
      $display ("%0d: Core.rl_cpu_hart0_reset_complete", cur_cycle);
   endrule

   // ================================================================
   // Other CPU/DM/TV connections
   // (depends on whether DM, TV or both are present)

`ifdef INCLUDE_GDB_CONTROL
`ifdef INCLUDE_TANDEM_VERIF
   // BEGIN SECTION: GDB and TV
   // ----------------------------------------------------------------
   // DM and TV both present. We instantiate 'taps' into connections
   // where the DM writes CPU GPRs, CPU FPRs, CPU CSRs, and main memory,
   // in order to produce corresponding writes for the Tandem Verifier.
   // Then, we merge the Trace_Data from these three taps with the
   // Trace_Data produced by the CPU.

   FIFOF #(Trace_Data) f_trace_data_merged <- mkFIFOF;

   // Connect merged trace data to trace encoder
   mkConnection (toGet (f_trace_data_merged), tv_encode.trace_data_in);

   // Merge-in CPU's trace data.
   // This is equivalent to:  mkConnection (cpu.trace_data_out, toPut (f_trace_data_merged))
   // but using a rule allows us to name it in scheduling attributes.
   rule merge_cpu_trace_data;
      let tmp <- cpu.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

   // Create a tap for DM's GPR writes to the CPU, and merge-in the trace data.
   DM_GPR_Tap_IFC  dm_gpr_tap_ifc <- mkDM_GPR_Tap;
   //mkConnection (debug_module.hart0.hart_gpr_mem_client, dm_gpr_tap_ifc.server);
   mkConnection (dm_gpr_tap_ifc.client, cpu.hart0_gpr_mem_server);

   rule merge_dm_gpr_trace_data;
      let tmp <- dm_gpr_tap_ifc.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

`ifdef ISA_F
   // Create a tap for DM's FPR writes to the CPU, and merge-in the trace data.
   DM_FPR_Tap_IFC  dm_fpr_tap_ifc <- mkDM_FPR_Tap;
   //mkConnection (debug_module.hart0.hart_fpr_mem_client, dm_fpr_tap_ifc.server);
   mkConnection (dm_fpr_tap_ifc.client, cpu.hart0_fpr_mem_server);

   rule merge_dm_fpr_trace_data;
      let tmp <- dm_fpr_tap_ifc.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule
`endif

   // Create a tap for DM's CSR writes, and merge-in the trace data.
   DM_CSR_Tap_IFC  dm_csr_tap <- mkDM_CSR_Tap;
   //mkConnection(debug_module.hart0.hart_csr_mem_client, dm_csr_tap.server);
   mkConnection(dm_csr_tap.client, cpu.hart0_csr_mem_server);

   rule merge_dm_csr_trace_data;
      let tmp <- dm_csr_tap.trace_data_out.get;
      f_trace_data_merged.enq(tmp);
   endrule

   // Instantiate debug module's stub
   let dm_stub <- mkDebugTriage(dm_reset_server,
				cpu.debug.hart_server_run_halt,
				dm_gpr_tap.server,
				dm_csr_tap.server);

   // Create a tap for DM's memory-writes to the bus, and merge-in the trace data.
   DM_Mem_Tap_IFC dm_mem_tap <- mkDM_Mem_Tap;
   mkConnection (fn_8byte_align(dm_stub.client), dm_mem_tap.slave);
   let dm_master_local = dm_mem_tap.master;

`ifdef ISA_F
   (* descending_urgency = "merge_dm_fpr_trace_data, merge_dm_gpr_trace_data" *)
`endif
   (* descending_urgency = "merge_dm_gpr_trace_data, merge_dm_csr_trace_data" *)
   (* descending_urgency = "merge_dm_csr_trace_data, merge_dm_mem_trace_data" *)
   (* descending_urgency = "merge_dm_mem_trace_data, merge_cpu_trace_data"    *)
   rule merge_dm_mem_trace_data;
      let tmp <- dm_mem_tap.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

   // END SECTION: GDB and TV
`else
   // for ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // BEGIN SECTION: GDB and no TV
   let dm_stub <- mkDebugTriage(dm_reset_server,
				cpu.debug.hart_server_run_halt,
				cpu.debug.hart_gpr_mem_server,
				cpu.debug.hart_csr_mem_server);

   let dm_master_local = fn_8byte_align(dm_stub.client);
   // END SECTION: GDB and no TV
`endif
   // for ifdef INCLUDE_TANDEM_VERIF

`else
   // for ifdef INCLUDE_GDB_CONTROL
   // BEGIN SECTION: no GDB

   // No DM, so 'DM bus master' is dummy
   AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)
   dm_master_local = dummy_AXI4_Master_ifc;

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // BEGIN SECTION: no GDB, TV

   // Connect CPU's TV out directly to TV encoder
   mkConnection (cpu.trace_data_out, tv_encode.trace_data_in);
   // END SECTION: no GDB, TV
`endif
`endif
   // for ifdef INCLUDE_GDB_CONTROL

   // ================================================================
   // Local fabric connections
   // Connect the local fabric

`ifdef DUAL_FABRIC
   // Connect the CPU master
   mkConnection (cpu.nmio_master,  local_fabric.v_from_masters [cpu_dmem_master_num]);

`ifdef Near_Mem_TCM
`ifdef INCLUDE_GDB_CONTROL
   // Connect the Debug Module
   mkConnection (dm_master_local, local_fabric.v_from_masters [debug_module_sba_master_num]);
`endif
`else
   // Cache based near-mem -- always connect debug model, even if it is a stub
   mkConnection (dm_master_local, local_fabric.v_from_masters [debug_module_sba_master_num]);
`endif

`else
`ifndef FABRIC_AHBL
   // Connect the CPU master
   mkConnection (cpu.dmem_master,  local_fabric.v_from_masters [cpu_dmem_master_num]);

`ifdef Near_Mem_TCM
`ifdef INCLUDE_GDB_CONTROL
   // Connect the Debug Module
   mkConnection (dm_master_local, local_fabric.v_from_masters [debug_module_sba_master_num]);
`endif
`else
   // Cache based near-mem -- always connect debug model, even if it is a stub
   mkConnection (dm_master_local, local_fabric.v_from_masters [debug_module_sba_master_num]);
`endif
`endif
`endif

   // --------
   // Slave connections
`ifdef DUAL_FABRIC
   mkConnection (local_fabric.v_to_slaves [clint_slave_num], clint.axi4_slave);
   mkConnection (local_fabric.v_to_slaves [plic_slave_num],  plic.axi4_slave);

`else
`ifndef FABRIC_AHBL
   mkConnection (local_fabric.v_to_slaves [clint_slave_num], clint.axi4_slave);
   mkConnection (local_fabric.v_to_slaves [plic_slave_num],  plic.axi4_slave);

`endif
`endif

   // TCM DMA server connections
`ifdef Near_Mem_TCM
`ifdef INCLUDE_GDB_CONTROL
   // GDB with TCM - backdoor present
   mkConnection (local_fabric.v_to_slaves [imem_dma_slave_num], cpu.imem_dma_server);
   mkConnection (local_fabric.v_to_slaves [dmem_dma_slave_num], cpu.dmem_dma_server);
`else
`ifdef TCM_LOADER
   // No GDB, TCM Loader - backdoor present
   mkConnection (local_fabric.v_to_slaves [imem_dma_slave_num], cpu.imem_dma_server);
   mkConnection (local_fabric.v_to_slaves [dmem_dma_slave_num], cpu.dmem_dma_server);
`else
   // No GDB. No TCM Loader. No backdoor.
   AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) idma_dummy_master = dummy_AXI4_Master_ifc;
   AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) ddma_dummy_master = dummy_AXI4_Master_ifc;
   mkConnection (idma_dummy_master, cpu.imem_dma_server);
   mkConnection (ddma_dummy_master, cpu.dmem_dma_server);
`endif
`endif
`endif

   // ================================================================
   // Connect interrupt lines from clint and PLIC to CPU

   rule rl_relay_sw_interrupts;    // from Near_Mem_IO (CLINT)
      Bool x <- clint.get_sw_interrupt_req.get;
      cpu.software_interrupt_req (x);
      // $display ("%0d: Core.rl_relay_sw_interrupts: relaying: %d", cur_cycle, pack (x));
   endrule

   rule rl_relay_timer_interrupts;    // from Near_Mem_IO (CLINT)
      Bool x <- clint.get_timer_interrupt_req.get;
      cpu.timer_interrupt_req (x);

      // $display ("%0d: Core.rl_relay_timer_interrupts: relaying: %d", cur_cycle, pack (x));
   endrule

   rule rl_relay_external_interrupts;    // from PLIC
      Bool meip = plic.v_targets [0].m_eip;
      cpu.m_external_interrupt_req (meip);

      //Bool seip = plic.v_targets [1].m_eip;  -- no supervisor mode in this version
      cpu.s_external_interrupt_req (False);

      // $display ("%0d: Core.rl_relay_external_interrupts: relaying: %d", cur_cycle, pack (x));
   endrule

   // ================================================================
   // INTERFACE

   // ----------------------------------------------------------------
   // Soft reset

   interface Server  cpu_reset_server = toGPServer (f_reset_reqs, f_reset_rsps);

   // ----------------------------------------------------------------
   // AXI4 Fabric interfaces

`ifndef Near_Mem_TCM
   // IMem to Fabric master interface
   interface AXI4_Master_IFC  cpu_imem_master = cpu.imem_master;
`endif

`ifdef FABRIC_AHBL
   interface AHBL_Master_IFC  cpu_dmem_master = cpu.dmem_master;
`else
   // DMem to Fabric master interface
   interface AXI4_Master_IFC  cpu_dmem_master = local_fabric.v_to_slaves [default_slave_num];
`endif

   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

`ifdef INCLUDE_DMEM_SLAVE
   interface AXI4_Lite_Slave_IFC  cpu_dmem_slave = cpu.dmem_slave;
`endif

   // ----------------------------------------------------------------
   // External interrupt sources

   interface core_external_interrupt_sources = plic.v_sources;

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   method Action nmi_req (Bool set_not_clear);
      cpu.nmi_req (set_not_clear);
   endmethod

   // ----------------------------------------------------------------
   // Optional TV interface

`ifdef INCLUDE_TANDEM_VERIF
   interface Get tv_verifier_info_get;
      method ActionValue #(Info_CPU_to_Verifier) get();
         match { .n, .v } <- tv_encode.tv_vb_out.get;
         return (Info_CPU_to_Verifier { num_bytes: n, vec_bytes: v });
      endmethod
   endinterface
`endif

   // ----------------------------------------------------------------
   // Optional DM interfaces

`ifdef INCLUDE_GDB_CONTROL
   // ----------------
   // DMI (Debug Module Interface) facing remote debugger
   interface debug = dm_stub.server;
`endif

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // Debugging: set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);
      cpu.set_verbosity (verbosity, logdelay);
   endmethod

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr

`ifdef Near_Mem_TCM
`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
      cpu.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Bit #(64) mv_tohost_value = cpu.mv_tohost_value;
`endif
`ifdef TCM_LOADER
   interface loader_slave = local_fabric.v_from_masters [loader_master_num];
`endif
`endif

endmodule: mkCore

endpackage
