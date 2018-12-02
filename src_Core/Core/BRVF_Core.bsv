// Copyright (c) 2018 Bluespec, Inc. All Rights Reserved.

package BRVF_Core;

// ================================================================
// This package defines the BRVF_Core module that combines:
// - the core RISC-V CPU
// - Tandem-Verification (TV) logic (optional: INCLUDE_TANDEM_VERIF)
// - a RISC-V Debug Module          (optional: INCLUDE_GDB_CONTROL)

// ================================================================
// BSV library imports

import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;

// ----------------
// BSV additional libs

import GetPut_Aux :: *;

// ================================================================
// Project imports

// Main fabric
import AXI4_Lite_Types  :: *;
import AXI4_Lite_Fabric :: *;
import Fabric_Defs      :: *;

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module     :: *;
`endif

import CPU_IFC        :: *;
import CPU            :: *;
import BRVF_Core_IFC  :: *;

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info        :: *;
import TV_Encode      :: *;

`ifdef INCLUDE_GDB_CONTROL
import TV_Taps        :: *;
`endif
`endif

// ================================================================
// The BRVF_Core module

(* synthesize *)
module mkBRVF_Core #(parameter Bit #(64)  pc_reset_value)  (BRVF_Core_IFC);

   // ================================================================
   // STATE

   // The CPU and queues for reset reqs and rsps from SoC
   CPU_IFC  cpu <- mkCPU (pc_reset_value);

   // Reset requests from SoC and responses to SoC
   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

`ifdef INCLUDE_TANDEM_VERIF
   // The TV encoder transforms Trace_Data structures produced by the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier
   TV_Encode_IFC tv_encode <- mkTV_Encode;
`endif

`ifdef INCLUDE_GDB_CONTROL
   // Debug Module
   Debug_Module_IFC  debug_module <- mkDebug_Module;
`endif

   // ================================================================
   // RESET
   // There are two sources of reset requests to the CPU: externally
   // from the SoC and, optionally, the DM.  The SoC requires a
   // response, the DM does not.  When both requestors are present
   // (i.e., DM is present), we merge the reset requests into the CPU,
   // and we remember which one was the requestor in
   // f_reset_requestor, so that we know whether or not to respond to
   // the SoC.

   Bit #(1) reset_requestor_dm  = 0;
   Bit #(1) reset_requestor_soc = 1;
`ifdef INCLUDE_GDB_CONTROL
   FIFOF #(Bit #(1)) f_reset_requestor <- mkFIFOF;
`endif

   // Reset-hart0 request from SoC
   rule rl_cpu_hart0_reset_from_soc_start;
      let req <- pop (f_reset_reqs);

      // Reset the hart
      cpu.hart0_server_reset.request.put (?);
`ifdef INCLUDE_GDB_CONTROL
      // Remember the requestor, so we can respond to it
      f_reset_requestor.enq (reset_requestor_soc);
`endif
   endrule

`ifdef INCLUDE_GDB_CONTROL
   // Reset-hart0 from Debug Module
   rule rl_cpu_hart0_reset_from_dm_start;
      let req <- debug_module.hart0_get_reset_req.get;
      // Reset the hart
      cpu.hart0_server_reset.request.put (?);
      // Remember the requestor, so we can respond to it
      f_reset_requestor.enq (reset_requestor_dm);
   endrule
`endif

   rule rl_cpu_hart0_reset_complete;
      let rsp <- cpu.hart0_server_reset.response.get;

      Bit #(1) requestor = reset_requestor_soc;
`ifdef INCLUDE_GDB_CONTROL
      requestor <- pop (f_reset_requestor);
`endif
      if (requestor == reset_requestor_soc)
	 f_reset_rsps.enq (?);
   endrule

   // ================================================================
   // Direct DM-to-CPU connections

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // DM to CPU connections for run-control and other misc requests
   mkConnection (debug_module.hart0_client_run_halt, cpu.hart0_server_run_halt);
   mkConnection (debug_module.hart0_get_other_req,   cpu.hart0_put_other_req);
`endif

   // ================================================================
   // Other CPU/DM/TV connections
   // (depends on whether DM, TV or both are present)

`ifdef INCLUDE_GDB_CONTROL
`ifdef INCLUDE_TANDEM_VERIF
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

   // Create a tap for DM's memory-writes to the bus, and merge-in the trace data.
   DM_Mem_Tap_IFC dm_mem_tap <- mkDM_Mem_Tap;
   mkConnection (debug_module.master, dm_mem_tap.slave);
   let dm_master_local = dm_mem_tap.master;

   rule merge_dm_mem_trace_data;
      let tmp <- dm_mem_tap.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

   // Create a tap for DM's GPR writes to the CPU, and merge-in the trace data.
   DM_GPR_Tap_IFC  dm_gpr_tap_ifc <- mkDM_GPR_Tap;
   mkConnection (debug_module.hart0_gpr_mem_client, dm_gpr_tap_ifc.server);
   mkConnection (dm_gpr_tap_ifc.client, cpu.hart0_gpr_mem_server);

   rule merge_dm_gpr_trace_data;
      let tmp <- dm_gpr_tap_ifc.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

`ifdef ISA_F_OR_D
   // Create a tap for DM's FPR writes to the CPU, and merge-in the trace data.
   DM_FPR_Tap_IFC  dm_fpr_tap_ifc <- mkDM_FPR_Tap;
   mkConnection (debug_module.hart0_fpr_mem_client, dm_fpr_tap_ifc.server);
   mkConnection (dm_fpr_tap_ifc.client, cpu.hart0_fpr_mem_server);

   rule merge_dm_fpr_trace_data;
      let tmp <- dm_fpr_tap_ifc.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule
`endif

   // Create a tap for DM's CSR writes, and merge-in the trace data.
   DM_CSR_Tap_IFC  dm_csr_tap <- mkDM_CSR_Tap;
   mkConnection(debug_module.hart0_csr_mem_client, dm_csr_tap.server);
   mkConnection(dm_csr_tap.client, cpu.hart0_csr_mem_server);

`ifdef ISA_F_OR_D
   (* descending_urgency = "merge_dm_fpr_trace_data, merge_dm_gpr_trace_data" *)
`endif
   (* descending_urgency = "merge_dm_gpr_trace_data, merge_dm_csr_trace_data" *)
   (* descending_urgency = "merge_dm_csr_trace_data, merge_dm_mem_trace_data" *)
   (* descending_urgency = "merge_dm_mem_trace_data, merge_cpu_trace_data"    *)
   rule merge_dm_csr_trace_data;
      let tmp <- dm_csr_tap.trace_data_out.get;
      f_trace_data_merged.enq(tmp);
   endrule

`else
   // ----------------------------------------------------------------
   // DM present, no TV

   // Connect DM's GPR interface directly to CPU
   mkConnection (debug_module.hart0_gpr_mem_client, cpu.hart0_gpr_mem_server);

`ifdef ISA_F_OR_D
   // Connect DM's FPR interface directly to CPU
   mkConnection (debug_module.hart0_fpr_mem_client, cpu.hart0_fpr_mem_server);
`endif

   // Connect DM's CSR interface directly to CPU
   mkConnection (debug_module.hart0_csr_mem_client, cpu.hart0_csr_mem_server);

   // DM's bus master is directly the bus master
   let dm_master_local = debug_module.master;
`endif

`else    // not INCLUDE_GDB_CONTROL

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // TV present, no DM

   // Connect CPU's TV out directly to TV encoder
   mkConnection (cpu.trace_data_out, tv_encode.trace_data_in);
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

   // ----------------------------------------------------------------
   // Set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);
      cpu.set_verbosity (verbosity, logdelay);
   endmethod

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------
   // Optional TV interface

   interface Get tv_verifier_info_get;
      method ActionValue #(Info_CPU_to_Verifier) get();
         match { .n, .v } <- tv_encode.tv_vb_out.get;
         return (Info_CPU_to_Verifier { num_bytes: n, vec_bytes: v });
      endmethod
   endinterface
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // Optional DM interfaces

   // ----------------
   // DMI (Debug Module Interface) facing remote debugger

   interface DMI  dm_dmi = debug_module.dmi;

   // ----------------
   // Facing Platform

   // Non-Debug-Module Reset (reset all except DM)
   interface Get  dm_ndm_reset_req_get = debug_module.get_ndm_reset_req;
   interface AXI4_Lite_Master_IFC  dm_master = dm_master_local;
`endif

endmodule

// ================================================================

endpackage
