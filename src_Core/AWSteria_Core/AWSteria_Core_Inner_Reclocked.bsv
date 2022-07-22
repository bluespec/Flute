// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AWSteria_Core_Inner_Reclocked;

// ================================================================
// This package defines a 'reclocking' module, i.e., a function
//
//    Clock -> Clock -> AWSteria_Core_Inner_IFC -> Module (AWSteria_Core_Inner_IFC)
//
// where the input and output AWSteria_Core_Inner_IFC interfaces
// run on different clocks

// ================================================================
// Lib imports

// from BSV library
import Vector       :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import Clocks       :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;

// AXI
import AXI4_Types         :: *;
import AXI4_ClockCrossing :: *;

// ================================================================
// Project imports

import AWSteria_Core_IFC   :: *;
import AWSteria_Core_Inner :: *;

import ISA_Decls :: *;

// Debug Module interface
import DM_Common      :: *;
import DM_CPU_Req_Rsp :: *;

// Tandem Verification
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

module mkAWSteria_Core_Inner_Reclocked
   #(Clock clk_fast, Reset rst_fast,
     Clock clk_slow, Reset rst_slow,
     AWSteria_Core_Inner_IFC  core_inner) (AWSteria_Core_Inner_IFC);

   Integer depth = 4;    // For SyncFIFOs

   // ----------------------------------------------------------------
   // Interfaces that go directly out to AWSteria_Core_IFC

   // ----------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   let ddr_AXI4_clock_crossing <- mkAXI4_ClockCrossing (clk_slow, rst_slow,
							clk_fast, rst_fast);
   mkConnection (core_inner.mem_M, ddr_AXI4_clock_crossing.from_M);

   // ----
   // AXI4 interface: Core (M) to MMIO (S)

   let mmio_AXI4_clock_crossing <- mkAXI4_ClockCrossing (clk_slow, rst_slow,
							 clk_fast, rst_fast);
   mkConnection (core_inner.mmio_M, mmio_AXI4_clock_crossing.from_M);

   // ----
   // AXI4 interface: System (M) to Core (S)

   let dma_AXI4_clock_crossing <- mkAXI4_ClockCrossing (clk_fast, rst_fast,
							clk_slow, rst_slow);
   mkConnection (dma_AXI4_clock_crossing.to_S, core_inner.dma_S);

   // ----------------
   // External interrupt sources

   // Register Wire (bus) driven by 'ext_interrupts' method
   Reg #(Bit #(N_Core_External_Interrupt_Sources)) rg_irqs <- mkReg (0);

   // Clock-crossing for wire (bus)
   ReadOnly #(Bit #(N_Core_External_Interrupt_Sources)) ro_sync_irqs
   <- mkNullCrossingWire (clocked_by clk_fast, clk_slow, rg_irqs);

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_relay_interrupts;
      core_inner.ext_interrupts (ro_sync_irqs);
   endrule

   // ----------------
   // Non-maskable interrupt

   SyncFIFOIfc #(Bool) f_nmi <- mkSyncFIFO (depth, clk_fast, rst_fast, clk_slow);

   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_nmi), core_inner.fi_nmi);

   // ----------------
   // Misc IO streams

   // Input stream
   SyncFIFOIfc #(Bit #(32)) f_misc_from_host <- mkSyncFIFO (depth, clk_fast, rst_fast,
							    clk_slow);
   mkConnection (core_inner.fi_misc, fn_SyncFIFOIfc_to_FIFOF_O (f_misc_from_host));

   // Output stream
   SyncFIFOIfc #(Bit #(32)) f_misc_to_host <- mkSyncFIFO (depth, clk_slow, rst_slow,
							  clk_fast);
   mkConnection (core_inner.fo_misc, fn_SyncFIFOIfc_to_FIFOF_I (f_misc_to_host));

   // ----------------
   // Tandem Verification output

   SyncFIFOIfc #(TV_Info)  f_tv_info  <- mkSyncFIFO (depth, clk_slow, rst_slow,
						     clk_fast);

   mkConnection (core_inner.fo_tv_info, fn_SyncFIFOIfc_to_FIFOF_I (f_tv_info));

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // GPR access
   SyncFIFOIfc #(DM_CPU_Req #(5,  XLEN)) f_gpr_req <- mkSyncFIFO (depth, clk_fast,rst_fast,clk_slow);
   SyncFIFOIfc #(DM_CPU_Rsp #(XLEN))     f_gpr_rsp <- mkSyncFIFO (depth, clk_slow,rst_slow,clk_fast);
   mkConnection (toGet (f_gpr_req), core_inner.hart0_gpr_mem_server.request);
   mkConnection (toPut (f_gpr_rsp), core_inner.hart0_gpr_mem_server.response);

`ifdef ISA_F
   // FPR access
   SyncFIFOIfc #(DM_CPU_Req #(5,  XLEN)) f_fpr_req <- mkSyncFIFO (depth, clk_fast,rst_fast,clk_slow);
   SyncFIFOIfc #(DM_CPU_Rsp #(XLEN))     f_fpr_rsp <- mkSyncFIFO (depth, clk_slow,rst_slow,clk_fast);
   mkConnection (toGet (f_fpr_req), core_inner.hart0_fpr_mem_server.request);
   mkConnection (toPut (f_fpr_rsp), core_inner.hart0_fpr_mem_server.response);
`endif

   // CSR access
   SyncFIFOIfc #(DM_CPU_Req #(12, XLEN)) f_csr_req <- mkSyncFIFO (depth, clk_fast,rst_fast,clk_slow);
   SyncFIFOIfc #(DM_CPU_Rsp #(XLEN))     f_csr_rsp <- mkSyncFIFO (depth, clk_slow,rst_slow,clk_fast);
   mkConnection (toGet (f_csr_req), core_inner.hart0_csr_mem_server.request);
   mkConnection (toPut (f_csr_rsp), core_inner.hart0_csr_mem_server.response);

   // System Bus access: AXI4 interface: Debug Module (M) to Inner Core (S)

   let sba_AXI4_clock_crossing <- mkAXI4_ClockCrossing (clk_fast, rst_fast,
							clk_slow, rst_slow);
   mkConnection (sba_AXI4_clock_crossing.to_S, core_inner.sba_S);

   // ================================================================
   // Interfaces for mkHost_Control_Status

   // ----------------------------------------------------------------
   // Misc. control and status

   // PC Trace control
   SyncFIFOIfc #(Tuple2 #(Bool, Bit #(64))) f_pc_trace_control <- mkSyncFIFO (depth,
									      clk_fast, rst_fast,
									      clk_slow);
   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_pc_trace_control),
		 core_inner.fi_pc_trace_control);

   // Set core's verbosity and logdelay
   SyncFIFOIfc #(Tuple2 #(Bit #(4), Bit #(64))) f_verbosity_control <- mkSyncFIFO (depth,
										   clk_fast, rst_fast,
										   clk_slow);
   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_verbosity_control),
		 core_inner.fi_verbosity_control);



`ifdef WATCH_TOHOST
   // Set watch-tohost on/off with tohost address
   SyncFIFOIfc #(Tuple2 #(Bool, Bit #(64))) f_watch_tohost_control <- mkSyncFIFO (depth,
										  clk_fast, rst_fast,
										  clk_slow);
   mkConnection (fn_SyncFIFOIfc_to_FIFOF_O (f_watch_tohost_control),
		 core_inner.fi_watch_tohost_control);

   // Get tohost value
   SyncFIFOIfc #(Bit #(64)) f_tohost_value <- mkSyncFIFO (depth,
							  clk_slow, rst_slow,
							  clk_fast);
   mkConnection (fn_SyncFIFOIfc_to_FIFOF_I (f_tohost_value),
		 core_inner.fo_tohost_value);

   // ================================================================
   // INTERFACE

   // ----------------------------------------------------------------
   // Interfaces that go directly out to AWSteria_Core_IFC

   // ----------------
   // AXI4 interfaces for DDR memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = ddr_AXI4_clock_crossing.to_S;
   interface AXI4_Master_IFC mmio_M = mmio_AXI4_clock_crossing.to_S;
   interface AXI4_Master_IFC dma_S  = dma_AXI4_clock_crossing.from_M;

   // ----------------
   // External interrupt sources

   method Action ext_interrupts (Bit #(N_Core_External_Interrupt_Sources) x);
      rg_irqs <= x;
   endmethod

   // ----------------
   // Non-maskable interrupt request

   interface fi_nmi = fn_SyncFIFOIfc_to_FIFOF_I (f_nmi);

   // ----------------
   // Misc I/O stream

   interface fi_misc = fn_SyncFIFOIfc_to_FIFOF_I (f_misc_from_host);
   interface fo_misc = fn_SyncFIFOIfc_to_FIFOF_O (f_misc_to_host);

   // ----------------
   // Tandem Verification output

   interface fo_tv_info  = fn_SyncFIFOIfc_to_FIFOF_O (f_tv_info);

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // GPR access
   interface Server hart0_gpr_mem_server = toGPServer (f_gpr_req, f_gpr_rsp);
`ifdef ISA_F
   // FPR access
   interface Server hart0_fpr_mem_server = toGPServer (f_fpr_req, f_fpr_rsp);
`endif
   // CSR access
   interface Server hart0_csr_mem_server = toGPServer (f_csr_req, f_csr_rsp);

   // System Bus (Mem) access
   interface AXI4_Slave_IFC sba_S        = sba_AXI4_clock_crossing.from_M;

   // ================================================================
   // plus interfaces fro mkHost_Control_Status

   // PC Trace control
   interface FIFOF_I fi_pc_trace_control = fn_SyncFIFOIfc_to_FIFOF_I (f_pc_trace_control);

   // Set core's verbosity and logdelay
   interface FIFOF_I fi_verbosity_control = fn_SyncFIFOIfc_to_FIFOF_I (f_verbosity_control);

`ifdef WATCH_TOHOST
   // Set watch-tohost on/off with tohost address
   interface FIFOF_I fi_watch_tohost_control = fn_SyncFIFOIfc_to_FIFOF_I (f_watch_tohost_control);
   // Get tohost value
   interface FIFOF_O fo_tohost_value  = fn_SyncFIFOIfc_to_FIFOF_O (f_tohost_value);
`endif
endmodule

// ================================================================

endpackage
