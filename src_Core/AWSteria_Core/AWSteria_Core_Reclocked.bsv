// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AWSteria_Core_Reclocked;

// ================================================================
// This package defines a 'reclocking' module, i.e., a function
//
//    Clock -> Clock -> AWSteria_Core_IFC -> Module (AWSteria_Core_IFC)
//
// where the input and output AWSteria_Core_IFC interfaces
// run on different clocks

// ================================================================
// Lib imports

// from BSV library
import Vector      :: *;
import Connectable :: *;
import Clocks      :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;

// AXI
import AXI4_Types         :: *;
import AXI4_ClockCrossing :: *;

// ================================================================
// Project imports

import AWSteria_Core_IFC :: *;

// Debug Module interface
import DM_Common :: *;

// Trace and Tandem Verification
import PC_Trace :: *;
import TV_Info  :: *;

// ================================================================

function FIFOF_O #(t) fn_SyncFIFOIfc_to_FIFOF_O (SyncFIFOIfc #(t) syncfifo);
   return interface FIFOF_O;
	     method first    = syncfifo.first;
	     method deq      = syncfifo.deq;
	     method notEmpty = syncfifo.notEmpty;
	  endinterface;
endfunction

function FIFOF_I #(t) fn_SyncFIFOIfc_to_FIFOF_I (SyncFIFOIfc #(t) syncfifo);
   return interface FIFOF_I;
	     method enq     = syncfifo.enq;
	     method notFull = syncfifo.notFull;
	  endinterface;
endfunction

// ================================================================

module mkAWSteria_Core_Reclocked
   #(Clock clk_fast, Reset rst_fast,
     Clock clk_slow, Reset rst_slow,
     AWSteria_Core_IFC #(wd_id_mem,  wd_addr_mem,  wd_data_mem,  wd_user_mem,
			 wd_id_mmio, wd_addr_mmio, wd_data_mmio, wd_user_mmio,
			 wd_id_dma,  wd_addr_dma,  wd_data_dma,  wd_user_dma,
			 t_n_interrupt_sources) core)

   (AWSteria_Core_IFC #(wd_id_mem,  wd_addr_mem,  wd_data_mem,  wd_user_mem,
			wd_id_mmio, wd_addr_mmio, wd_data_mmio, wd_user_mmio,
			wd_id_dma,  wd_addr_dma,  wd_data_dma,  wd_user_dma,
			t_n_interrupt_sources));

   Integer depth = 2;    // For SyncFIFOs

   // ----------------------------------------------------------------
   // AXI4 interface: Core (M) to DDR (S)

   let ddr_AXI4_clock_crossing <- mkAXI4_ClockCrossing (clk_slow, rst_slow,
							clk_fast, rst_fast);
   mkConnection (core.mem_M, ddr_AXI4_clock_crossing.from_M);

   // ----------------------------------------------------------------
   // AXI4 interface: Core (M) to MMIO (S)

   let mmio_AXI4_clock_crossing <- mkAXI4_ClockCrossing (clk_slow, rst_slow,
							 clk_fast, rst_fast);
   mkConnection (core.mmio_M, mmio_AXI4_clock_crossing.from_M);

   // ----------------------------------------------------------------
   // AXI4 interface: System (M) to Core (S)

   let dma_AXI4_clock_crossing <- mkAXI4_ClockCrossing (clk_fast, rst_fast,
							clk_slow, rst_slow);
   mkConnection (dma_AXI4_clock_crossing.to_S, core.dma_S);

   // ----------------------------------------------------------------
   // External interrupt sources

   // Register Wire (bus) driven by 'ext_interrupts' method
   Reg #(Bit #(t_n_interrupt_sources)) rg_irqs <- mkReg (0);

   // Clock-crossing for wire (bus)
   ReadOnly #(Bit #(t_n_interrupt_sources)) ro_sync_irqs
   <- mkNullCrossingWire (clocked_by clk_fast, clk_slow, rg_irqs);

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_relay_interrupts;
      core.ext_interrupts (ro_sync_irqs);
   endrule

   // ----------------------------------------------------------------
   // Non-maskable interrupt

   SyncFIFOIfc #(Bool) f_nmi <- mkSyncFIFO (depth, clk_fast, rst_fast, clk_slow);

   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_nmi), core.fi_nmi);

   // ----------------
   // Trace and Tandem Verification output

   SyncFIFOIfc #(PC_Trace) f_pc_trace <- mkSyncFIFO (depth, clk_slow, rst_slow,
						     clk_fast);

   mkConnection (core.fo_pc_trace, fn_SyncFIFOIfc_to_FIFOF_I (f_pc_trace));

   SyncFIFOIfc #(TV_Info)  f_tv_info  <- mkSyncFIFO (depth, clk_slow, rst_slow,
						     clk_fast);

   mkConnection (core.fo_tv_info, fn_SyncFIFOIfc_to_FIFOF_I (f_tv_info));

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // DMI (Debug Module Interface) facing remote debugger

   SyncFIFOIfc #(DMI_Req) f_dmi_req <- mkSyncFIFO (depth, clk_fast, rst_fast, clk_slow);
   SyncFIFOIfc #(DMI_Rsp) f_dmi_rsp <- mkSyncFIFO (depth, clk_slow, rst_slow, clk_fast);

   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_dmi_req), core.se_dmi.request);
   mkConnection (fn_SyncFIFOIfc_to_FIFOF_I (f_dmi_rsp), core.se_dmi.response);

   // Non-Debug-Module Reset (reset "all" except DM)
   // These Bit#(0) values are just tokens for signaling 'reset request' and 'reset done'

   SyncFIFOIfc #(Bit #(0)) f_ndm_reset_req <- mkSyncFIFO (depth, clk_slow, rst_slow,
							  clk_fast);
   SyncFIFOIfc #(Bit #(0)) f_ndm_reset_rsp <- mkSyncFIFO (depth, clk_fast, rst_fast,
							  clk_slow);

   mkConnection (fn_SyncFIFOIfc_to_FIFOF_I (f_ndm_reset_req), core.cl_ndm_reset.request);
   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_ndm_reset_rsp), core.cl_ndm_reset.response);

   // ----------------------------------------------------------------
   // Misc. control and status

   SyncFIFOIfc #(Bit #(32)) f_control_status_req <- mkSyncFIFO (depth,
								clk_fast, rst_fast,
								clk_slow);
   SyncFIFOIfc #(Bit #(32)) f_control_status_rsp <- mkSyncFIFO (depth,
								clk_slow, rst_slow,
								clk_fast);

   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_control_status_req),
		 core.se_control_status.request);
   mkConnection (fn_SyncFIFOIfc_to_FIFOF_I (f_control_status_rsp),
		 core.se_control_status.response);

   // ----------------------------------------------------------------
   // INTERFACE

   // ----------------
   // AXI4 interfaces for DDR memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = ddr_AXI4_clock_crossing.to_S;
   interface AXI4_Master_IFC mmio_M = mmio_AXI4_clock_crossing.to_S;
   interface AXI4_Master_IFC dma_S  = dma_AXI4_clock_crossing.from_M;

   // ----------------
   // External interrupt sources

   method Action ext_interrupts (Bit #(t_n_interrupt_sources) x);
      rg_irqs <= x;
   endmethod

   // ----------------
   // Non-maskable interrupt request

   interface fi_nmi = fn_SyncFIFOIfc_to_FIFOF_I (f_nmi);

   // ----------------
   // Trace and Tandem Verification output

   interface fo_pc_trace = fn_SyncFIFOIfc_to_FIFOF_O (f_pc_trace);
   interface fo_tv_info  = fn_SyncFIFOIfc_to_FIFOF_O (f_tv_info);

   // ----------------
   // Debug Module interfaces

   // DMI (Debug Module Interface) facing remote debugger

   interface Server_DMI se_dmi;
      interface request  = fn_SyncFIFOIfc_to_FIFOF_I (f_dmi_req);
      interface response = fn_SyncFIFOIfc_to_FIFOF_O (f_dmi_rsp);
   endinterface

   // Non-Debug-Module Reset (reset "all" except DM)
   // These Bit#(0) values are just tokens for signaling 'reset request' and 'reset done'

   interface Client_Semi_FIFOF cl_ndm_reset;
      interface request  = fn_SyncFIFOIfc_to_FIFOF_O (f_ndm_reset_req);
      interface response = fn_SyncFIFOIfc_to_FIFOF_I (f_ndm_reset_rsp);
   endinterface

   // ----------------
   // Misc. control and status
   // The interpretation of these 32-bit values is left up to the specific Core

   interface Server_Semi_FIFOF se_control_status;
      interface request  = fn_SyncFIFOIfc_to_FIFOF_I (f_control_status_req);
      interface response = fn_SyncFIFOIfc_to_FIFOF_O (f_control_status_rsp);
   endinterface

endmodule

// ================================================================

endpackage
