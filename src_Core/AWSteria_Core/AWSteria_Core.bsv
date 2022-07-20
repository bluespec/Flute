// Copyright (c) 2021-2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AWSteria_Core;

// ================================================================
// This package defines mkAWSteria_core, located as follows:
//
//     mkAWSteria_Core
//         mkHost_Control_Status
//         mkAWSteria_Core_Inner

// Most of the core components are in mkAWSteria_Core_Inner
// These may be run at a slower clock
// mkAWSteria Core optionally instantiates Core_Inner at a slower clock
// mkAWSteria_Core can reset Core_Inner based on a command from mkHost_Control_Status

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
import Host_Control_Status           :: *;

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
// mkAWSteria_Core is an optional thin wrapper around
// mkAWSteria_Core_Single_Clock, selecting a slower clock at which to
// run the inner module 'mkAWSteria_Core_Single_Clock'.

(* synthesize *)
module mkAWSteria_Core #(Clock clk1,        // extra clock
			 Clock clk2,        // extra clock
			 Clock clk3,        // extra clock
			 Clock clk4,        // extra clock
			 Clock clk5)        // extra clock
                       (AWSteria_Core_IFC_Specialized);

   let clk_cur  <- exposeCurrentClock;
   let rstn_cur <- exposeCurrentReset;

   // Control-Status decoder
   Host_Control_Status_IFC host_cs <- mkHost_Control_Status;

`ifndef INCLUDE_AWSTERIA_CORE_IFC_CLOCK_CROSSING
   // ----------------------------------------------------------------
   // Instantiate inner core with with controllable reset on current clock

   messageM ("\nINFO: mkAWSteria_System --> AWSteria_Core: no clock crossing.");

   MakeResetIfc innerRstIfc <- mkReset (2,           // # of delay stages
					True,        // start in reset
					clk_cur);    // for which this is a reset

   // Assert inner reset if commanded by host_cs
   rule assertInnerReset (host_cs.mv_assert_core_reset);
      innerRstIfc.assertReset();
   endrule

   let innerReset = innerRstIfc.new_rst;

   AWSteria_Core_Inner_IFC
   core_inner <- mkAWSteria_Core_Inner (reset_by  innerReset);

`else
   // ----------------------------------------------------------------
   // Instantiate inner core with with controllable reset and slower clock

   messageM ("\nINFO: mkAWSteria_System --> AWSteria_Core: with clock crossings.");

   // Choose clock, depending on target platform.
   // One of these must be defined.
`ifdef PLATFORM_AWSF1
   let innerCLK = clk4;    //  75 MHz
   messageM ("\nINFO: Core clock is clk4");
`endif
`ifdef PLATFORM_VCU118
   let innerCLK = clk2;    // 100 MHz
   messageM ("\nINFO: Core clock is clk2");
`endif

   MakeResetIfc innerRstIfc <- mkReset (2,            // # of delay stages
					True,         // start in reset
					innerCLK);    // for which this is a reset
   // Assert inner reset if commanded by host_cs
   rule assertInnerReset (host_cs.mv_assert_core_reset);
      innerRstIfc.assertReset();
   endrule

   let innerReset = innerRstIfc.new_rst;

   AWSteria_Core_Inner_IFC
   core_inner_reclocked <- mkAWSteria_Core_Inner (clocked_by innerCLK,
						  reset_by   innerReset);

   AWSteria_Core_Inner_IFC
   core_inner <- mkAWSteria_Core_Inner_Reclocked (clk_cur,  rstn_cur,
						  innerCLK, innerReset,
						  core_inner_reclocked);
`endif

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
   // INTERFACE
   // Most are taken from core_inner,
   // plus host_cs.se_control_status

   // ----------------------------------------------------------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = core_inner.mem_M;
   interface AXI4_Master_IFC mmio_M = core_inner.mmio_M;
   interface AXI4_Slave_IFC  dma_S  = core_inner.dma_S;

   // ----------------------------------------------------------------
   // External interrupt sources

   method ext_interrupts = core_inner.ext_interrupts;

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   interface fi_nmi = core_inner.fi_nmi;

   // ----------------------------------------------------------------
   // Misc stream I/O, and Tandem Verification output

   interface fo_misc = core_inner.fo_misc;
   interface fi_misc = core_inner.fi_misc;

   interface fo_tv_info = core_inner.fo_tv_info;

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // DMI (Debug Module Interface) facing remote debugger

   interface Server_DMI se_dmi = core_inner.se_dmi;

   // Non-Debug-Module Reset (reset "all" except DM)
   // These Bit#(0) values are just tokens for signaling 'reset request' and 'reset done'

   interface cl_ndm_reset = core_inner.cl_ndm_reset;

   // ----------------------------------------------------------------
   // Host control and status

   interface Server_Semi_FIFOF se_control_status = host_cs.se_control_status;
endmodule

// ================================================================

endpackage
