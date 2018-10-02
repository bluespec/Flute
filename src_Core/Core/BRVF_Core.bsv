// Copyright (c) 2018 Bluespec, Inc. All Rights Reserved.

package BRVF_Core;

// ================================================================
// This package defines the BRVF_Core module that combines the core
// RISC-V CPU with Tandem-Verification (TV) logic and the RISC-V Debug
// Module.

// ================================================================
// BSV library imports

import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;

// ================================================================
// Project imports

// Main fabric
import AXI4_Lite_Types  :: *;
import AXI4_Lite_Fabric :: *;
import Fabric_Defs      :: *;

`ifdef INCLUDE_GDB_CONTROL
import External_Control :: *;    // Control requests/responses from HSFE
import Debug_Module     :: *;
`endif

import CPU_IFC        :: *;
import CPU            :: *;
import BRVF_Core_IFC  :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info        :: *;
import TV_Extra       :: *;
`endif

// ================================================================
// The BRVF_Core module

(* synthesize *)
module mkBRVF_Core #(parameter Bit #(64)  pc_reset_value)  (BRVF_Core_IFC);
   // The CPU itself
   CPU_IFC  cpu <- mkCPU (pc_reset_value);

   // CPU can be reset either by the Debug Module or by the SoC
   // This FIFO keeps track of who is the reset requestor.
   Bit #(1) reset_client_dm  = 0;
   Bit #(1) reset_client_soc = 1;
   FIFOF #(Bit #(1)) f_reset_client <- mkFIFOF;

   // Reset requests from SoC and responses to SoC
   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

`ifdef INCLUDE_GDB_CONTROL
   // ================================================================
   // Optional Debug Module

   Debug_Module_IFC  debug_module <- mkDebug_Module;

   // Connect debug_module to CPU hart0 run-control

   mkConnection (debug_module.hart0_client_run_halt, cpu.hart0_server_run_halt);
   mkConnection (debug_module.hart0_get_other_req,   cpu.hart0_put_other_req);

   // ----------------
   // Connect debug_module to CPU hart0 GPR/CSR access

`ifndef INCLUDE_TANDEM_VERIF
   mkConnection (debug_module.hart0_gpr_mem_client, cpu.hart0_gpr_mem_server);
   mkConnection (debug_module.hart0_csr_mem_client, cpu.hart0_csr_mem_server);
`endif

   // Reset hart0 from Debug Module

   rule rl_cpu_hart0_reset_from_dm_start;
      let req <- debug_module.hart0_get_reset_req.get;
      cpu.hart0_server_reset.request.put (?);
      f_reset_client.enq (reset_client_dm);
   endrule

   rule rl_cpu_hart0_reset_from_dm_complete (f_reset_client.first == reset_client_dm);
      f_reset_client.deq;
      let rsp <- cpu.hart0_server_reset.response.get;
   endrule

`endif

   // ================================================================
   // Reset hart0 from SoC

   rule rl_cpu_hart0_reset_from_soc_start;
      let req = f_reset_reqs.first;
      f_reset_reqs.deq;

      cpu.hart0_server_reset.request.put (?);
      f_reset_client.enq (reset_client_soc);
   endrule

   rule rl_cpu_hart0_reset_from_cpu_complete (f_reset_client.first == reset_client_soc);
      f_reset_client.deq;
      let rsp <- cpu.hart0_server_reset.response.get;
      f_reset_rsps.enq (?);
   endrule

`ifdef INCLUDE_TANDEM_VERIF
`ifdef INCLUDE_GDB_CONTROL
   let memtv <- mkMemoryTV;
   mkConnection(debug_module.master, memtv.slave);

   Register_TV_IFC#(5) gprtv <- mkRegisterTV('h1000);
   mkConnection(debug_module.hart0_gpr_mem_client, gprtv.server);
   mkConnection(gprtv.client, cpu.hart0_gpr_mem_server);

   Register_TV_IFC#(12) csrtv <- mkRegisterTV(0);
   mkConnection(debug_module.hart0_csr_mem_client, csrtv.server);
   mkConnection(csrtv.client, cpu.hart0_csr_mem_server);

   FIFOF#(Info_CPU_to_Verifier) f_tv_merged <- mkFIFOF;

   rule merge_tv_cpu;
      let tmp <- cpu.to_verifier.get;
      f_tv_merged.enq(tmp);
   endrule

   rule merge_tv_mem;
      let tmp <- memtv.to_verifier.get;
      f_tv_merged.enq(tmp);
   endrule

   rule merge_tv_gpr;
      let tmp <- gprtv.to_verifier.get;
      f_tv_merged.enq(tmp);
   endrule

   (* descending_urgency = "merge_tv_csr, merge_tv_gpr, merge_tv_mem, merge_tv_cpu" *)
   rule merge_tv_csr;
      let tmp <- csrtv.to_verifier.get;
      f_tv_merged.enq(tmp);
   endrule
`endif
`endif

   // ================================================================
   // INTERFACE

   // Reset
   interface Server  cpu_reset_server = toGPServer (f_reset_reqs, f_reset_rsps);

   // ----------------
   // SoC fabric connections

   // IMem to Fabric master interface
   interface AXI4_Lite_Master_IFC  cpu_imem_master = cpu.imem_master;

   // DMem to Fabric master interface
   interface AXI4_Lite_Master_IFC  cpu_dmem_master = cpu.dmem_master;

   // Back-door slave interface from fabric
   interface AXI4_Lite_Slave_IFC  cpu_slave = cpu.near_mem_slave;

   // ----------------
   // Interrupts

   method Action  cpu_external_interrupt_req (x) = cpu.external_interrupt_req (x);
   method Action  cpu_software_interrupt_req (x) = cpu.software_interrupt_req (x);
   method Action  cpu_timer_interrupt_req (x)    = cpu.timer_interrupt_req (x);

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------
   // Optional Tandem Verifier interface

`ifdef INCLUDE_GDB_CONTROL
   interface Get  tv_verifier_info_get = toGet(f_tv_merged);
`else
   interface Get  tv_verifier_info_get = cpu.to_verifier;
`endif
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // Optional Debug Module interfaces

   // ----------------
   // DMI (Debug Module Interface) facing remote debugger

   interface DMI  dm_dmi = debug_module.dmi;

   // ----------------
   // Facing Platform

   // Non-Debug-Module Reset (reset all except DM)
   interface Get  dm_ndm_reset_req_get = debug_module.get_ndm_reset_req;

   // Read/Write RISC-V memory
`ifdef INCLUDE_TANDEM_VERIF
   interface AXI4_Lite_Master_IFC  dm_master = memtv.master;
`else
   interface AXI4_Lite_Master_IFC  dm_master = debug_module.master;
`endif
`endif

endmodule

// ================================================================

endpackage
