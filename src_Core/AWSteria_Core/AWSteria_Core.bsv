// Copyright (c) 2021-2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AWSteria_Core;

// ================================================================
// This package defines mkAWSteria_core, which is a top-level
// "substitutable core" in AWSteria; it is instantiated by the fixed
// AWSteria_System.

// This module has three subsystems:
//
//     mkAWSteria_Core
//         mkHost_Control_Status
//         mkDebug_Module
//         mkAWSteria_Core_Inner

// Most of the core components are in mkAWSteria_Core_Inner (CPU with
// caches and MMUs, Boot ROM, PLIC, Near-Mem-IO/CLINT).

// mkAWSteria_Core_Inner is a separate clock and reset domain.
// It typically runs at a slower clock than the default clock.

// Either Host_Control_Status or Debug_Module can reset AWSteria_Core_Inner.

// ================================================================
// Lib imports

// BSV libs
import Clocks       :: *;

import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ----------------
// AXI

import AXI4_Types     :: *;

// ================================================================
// Project imports

import AWSteria_Core_IFC             :: *;

// Host Control and Status
import Host_Control_Status           :: *;

// Debug Module
import DM_Common      :: *;    // For Server_DMI
import Debug_Module   :: *;
import DM_CPU_Req_Rsp :: *;

// Inner core
import AWSteria_Core_Inner_Reclocked :: *;
import AWSteria_Core_Inner           :: *;

// ================================================================
// Interface specialization to non-polymorphic type.
// These parameter values are used in AWSteria_RISCV_Virtio
// with Flute or Toooba CPUs.

typedef AWSteria_Core_IFC #(// AXI widths for Mem
			    AXI4_Wd_Id, AXI4_Wd_Addr, AXI4_Wd_Data_A, AXI4_Wd_User,

			    // AXI widths for MMIO
			    AXI4_Wd_Id, AXI4_Wd_Addr, AXI4_Wd_Data_B, AXI4_Wd_User,

			    // AXI widths for DMA port
			    AXI4_Wd_Id, AXI4_Wd_Addr, AXI4_Wd_Data_A, AXI4_Wd_User,

			    // UART, virtio 1,2,3,4
			    N_Core_External_Interrupt_Sources
			    ) AWSteria_Core_IFC_Specialized;

// ================================================================
// mkAWSteria_Core is thin wrapper around mkAWSteria_Core_Inner,
// optionally selecting a slower clock for it.  If we use a slower
// clock, mkAWSteria_Core_Inner_Reclocked contains all the
// clock-crossings.

(* synthesize *)
module mkAWSteria_Core #(Clock clk1,        // extra clock
			 Clock clk2,        // extra clock
			 Clock clk3,        // extra clock
			 Clock clk4,        // extra clock
			 Clock clk5)        // extra clock
                       (AWSteria_Core_IFC_Specialized);

   let clk_cur  <- exposeCurrentClock;
   let rstn_cur <- exposeCurrentReset;

   // ================================================================
   // Host Control-Status
   Host_Control_Status_IFC host_cs <- mkHost_Control_Status;

   // ================================================================
   // Debug module
   // TODO: ndm_reset should be a signal, not a token (event).
   // First fix this in Debug Module, then plumb it through to here.

`ifdef INCLUDE_GDB_CONTROL
   Debug_Module_IFC  debug_module <- mkDebug_Module;

   // NDM reset from Debug Module
   Reg #(Bool) rg_debug_module_ndm_reset <- mkReg (False);

   rule rl_ndm_reset;
      $display ("AWSteria_Core: Debug Module requesting NDM reset (non-Debug-Module)");
      Bool b <- debug_module.ndm_reset_client.request.get;
      debug_module.ndm_reset_client.response.put (b);
      rg_debug_module_ndm_reset <= True;
   endrule
`endif

   // ================================================================
   // Defines 'core_inner' interface to mkCore_Inner, with or without reclocking

`ifndef INCLUDE_AWSTERIA_CORE_IFC_CLOCK_CROSSING
   // ----------------
   // Instantiate inner core with with controllable reset on current clock

   messageM ("\nINFO: mkAWSteria_System --> AWSteria_Core: no clock crossing.");

   MakeResetIfc innerRstIfc <- mkReset (2,           // # of delay stages
					True,        // start in reset
					clk_cur);    // for which this is a reset

   let innerReset = innerRstIfc.new_rst;

   // Inner core on current clock, resettable
   AWSteria_Core_Inner_IFC
   core_inner <- mkAWSteria_Core_Inner (reset_by  innerReset);

`else
   // ----------------
   // Instantiate inner core with with controllable reset and slower clock

   messageM ("\nINFO: mkAWSteria_System --> AWSteria_Core: with clock crossings.");

   // Choose clock, depending on target platform.
   // One of these must be defined.
`ifdef PLATFORM_AWSF1
   let innerCLK = clk4;    //  75 MHz
   messageM ("\nINFO: Core clock is clk4");
`endif
`ifdef PLATFORM_VCU118
   // let innerCLK = clk2;    // 100 MHz
   // messageM ("\nINFO: Core clock is clk2");
   let innerCLK = clk3;    // 50 MHz
   messageM ("\nINFO: Core clock is clk3");
`endif

   MakeResetIfc innerRstIfc <- mkReset (2,            // # of delay stages
					True,         // start in reset
					innerCLK);    // for which this is a reset

   let innerReset = innerRstIfc.new_rst;

   // Inner core on inner clock, resettable
   AWSteria_Core_Inner_IFC
   core_inner_reclocked <- mkAWSteria_Core_Inner (clocked_by innerCLK,
						  reset_by   innerReset);

   // Clock crossings into inner core
   AWSteria_Core_Inner_IFC
   core_inner <- mkAWSteria_Core_Inner_Reclocked (clk_cur,  rstn_cur,
						  innerCLK, innerReset,
						  core_inner_reclocked);
`endif

   // ================================================================
   // Assert inner reset if commanded by host_cs or by Debug Module's NDM reset
   // TODO: debug_module_ndm_reset should also be a level, not a token

   Reg #(Bool) rg_core_reset_message_displayed <- mkReg (False);

   rule rl_assert_reset_for_inner_core (host_cs.mv_assert_core_reset || rg_debug_module_ndm_reset);
      if (host_cs.mv_assert_core_reset) begin
	 if (! rg_core_reset_message_displayed) begin
	    $display ("AWSteria_Core: asserting Core_Inner reset due to host-control");
	    rg_core_reset_message_displayed <= True;
	 end
      end
      else
	 $display ("AWSteria_Core: asserting Core_Inner reset due to NDM reset from Debug Module");
      innerRstIfc.assertReset();
      rg_debug_module_ndm_reset <= False;
   endrule

   rule rl_on_deassert_core_reset (rg_core_reset_message_displayed
				   && (! host_cs.mv_assert_core_reset));
      $display ("AWSteria_Core: de-asserting Core_Inner reset due to host-control");
      // Prepare for next core reset
      rg_core_reset_message_displayed <= False;
   endrule

   // ================================================================
   // Connect host controls to inner core

   // PC trace
   mkConnection (host_cs.fo_pc_trace_control, core_inner.fi_pc_trace_control);

   // Simulation verbosity
   mkConnection (host_cs.fo_verbosity_control, core_inner.fi_verbosity_control);

   // Host control of 'watch tohost'
   mkConnection (host_cs.fo_watch_tohost_control, core_inner.fi_watch_tohost_control);

   // Relay tohost value to host
   mkConnection (core_inner.fo_tohost_value, host_cs.fi_tohost_value);

   // ================================================================
   // Connect Debug module to inner core

   // GPR access
   mkConnection (debug_module.hart0_gpr_mem_client, core_inner.hart0_gpr_mem_server);
`ifdef ISA_F
   // FPR access
   mkConnection (debug_module.hart0_fpr_mem_client, core_inner.hart0_fpr_mem_server);
`endif
   // CSR access
   mkConnection (debug_module.hart0_csr_mem_client, core_inner.hart0_csr_mem_server);
   // System Bus access
   mkConnection (debug_module.master,               core_inner.sba_S);

   // Adapter from debug module's dmi to server DMI
   FIFOF #(DMI_Req) f_dmi_reqs <- mkFIFOF;
   FIFOF #(DMI_Rsp) f_dmi_rsps <- mkFIFOF;

   rule rl_dmi_req;
      let req <- pop (f_dmi_reqs);
      if (req.is_read)
	 debug_module.dmi.read_addr (req.addr);
      else
	 debug_module.dmi.write (req.addr, req.wdata);
   endrule

   rule rl_dmi_rsp;
      let x <- debug_module.dmi.read_data;
      let rsp = DMI_Rsp { rdata: x };
      f_dmi_rsps.enq (rsp);
   endrule


   // ================================================================
   // INTERFACE

   // ----------------------------------------------------------------
   // Interfaces that go directly out to AWSteria_Core_IFC

   // ----------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = core_inner.mem_M;
   interface AXI4_Master_IFC mmio_M = core_inner.mmio_M;
   interface AXI4_Slave_IFC  dma_S  = core_inner.dma_S;

   // ----------------
   // External interrupt sources

   method ext_interrupts = core_inner.ext_interrupts;

   // ----------------
   // Non-maskable interrupt request

   interface fi_nmi = core_inner.fi_nmi;

   // ----------------
   // Misc I/O streams

   interface fo_misc = core_inner.fo_misc;
   interface fi_misc = core_inner.fi_misc;

   // ----------------
   // Tandem Verification output

   interface fo_tv_info = core_inner.fo_tv_info;

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // DMI (Debug Module Interface) facing remote debugger

   interface Server_Semi_FIFOF se_dmi = fifofs_to_Server_Semi_FIFOF (f_dmi_reqs, f_dmi_rsps);

   // Non-Debug-Module Reset (reset "all" except DM)
   // These Bit#(0) values are just tokens for signaling 'reset request' and 'reset done'

   // TODO; we stub this out for now. In future, we could use this to
   // reset modules in AWSteria_System.

   interface Client_Semi_FIFOF cl_ndm_reset = dummy_Client_Semi_FIFOF;

   // ----------------------------------------------------------------
   // Host control and status

   interface Server_Semi_FIFOF se_control_status = host_cs.se_control_status;
endmodule

// ================================================================

endpackage
