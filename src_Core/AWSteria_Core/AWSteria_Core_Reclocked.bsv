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
import AXI4_Types      :: *;
import AXI4_Lite_Types :: *;
import AXI_SyncBuffer  :: *;

// ================================================================
// Project imports

import AWSteria_Core_IFC :: *;

// Debug Module interface
import DMI :: *;

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

   Integer depth = 2;

   // ----------------------------------------------------------------
   // AXI4 interface for memory

   // Towards AWSteria_Core
   AXI4_Slave_Xactor_IFC #(wd_id_mem,  wd_addr_mem,  wd_data_mem,  wd_user_mem)
   mem_axi4_S_xactor <- mkAXI4_Slave_Xactor (clocked_by clk_slow,
					     reset_by   rst_slow);

   AXI_SyncBuffer_IFC #(AXI4_Wr_Addr #(wd_id_mem, wd_addr_mem, wd_user_mem),
			AXI4_Wr_Data #(wd_data_mem, wd_user_mem),
			AXI4_Wr_Resp #(wd_id_mem, wd_user_mem),
			AXI4_Rd_Addr #(wd_id_mem, wd_addr_mem, wd_user_mem),
			AXI4_Rd_Data #(wd_id_mem, wd_data_mem, wd_user_mem))
   mem_axi4_syncbuf <- mkAXI_SyncBuffer (depth,
					 clk_slow, rst_slow,
					 clk_fast, rst_fast);

   // Towards AWSteria_System
   AXI4_Master_Xactor_IFC #(wd_id_mem,  wd_addr_mem,  wd_data_mem,  wd_user_mem)
   mem_axi4_M_xactor <- mkAXI4_Master_Xactor;

   // ----------------

   mkConnection (core.mem_M, mem_axi4_S_xactor.axi_side);

   mkConnection (mem_axi4_S_xactor.o_wr_addr, mem_axi4_syncbuf.from_M.i_aw);
   mkConnection (mem_axi4_S_xactor.o_wr_data, mem_axi4_syncbuf.from_M.i_w);
   mkConnection (mem_axi4_S_xactor.i_wr_resp, mem_axi4_syncbuf.from_M.o_b);
   mkConnection (mem_axi4_S_xactor.o_rd_addr, mem_axi4_syncbuf.from_M.i_ar);
   mkConnection (mem_axi4_S_xactor.i_rd_data, mem_axi4_syncbuf.from_M.o_r);

   mkConnection (mem_axi4_syncbuf.to_S.o_aw, mem_axi4_M_xactor.i_wr_addr);
   mkConnection (mem_axi4_syncbuf.to_S.o_w,  mem_axi4_M_xactor.i_wr_data);
   mkConnection (mem_axi4_syncbuf.to_S.i_b,  mem_axi4_M_xactor.o_wr_resp);
   mkConnection (mem_axi4_syncbuf.to_S.o_ar, mem_axi4_M_xactor.i_rd_addr);
   mkConnection (mem_axi4_syncbuf.to_S.i_r,  mem_axi4_M_xactor.o_rd_data);

   // ----------------------------------------------------------------
   // AXI4 interface for MMIO

   // Towards AWSteria_Core
   AXI4_Slave_Xactor_IFC #(wd_id_mmio,  wd_addr_mmio,  wd_data_mmio,  wd_user_mmio)
   mmio_axi4_S_xactor <- mkAXI4_Slave_Xactor (clocked_by clk_slow,
					      reset_by   rst_slow);

   AXI_SyncBuffer_IFC #(AXI4_Wr_Addr #(wd_id_mmio, wd_addr_mmio, wd_user_mmio),
			AXI4_Wr_Data #(wd_data_mmio, wd_user_mmio),
			AXI4_Wr_Resp #(wd_id_mmio, wd_user_mmio),
			AXI4_Rd_Addr #(wd_id_mmio, wd_addr_mmio, wd_user_mmio),
			AXI4_Rd_Data #(wd_id_mmio, wd_data_mmio, wd_user_mmio))
   mmio_axi4_syncbuf <- mkAXI_SyncBuffer (depth,
					  clk_slow, rst_slow,
					  clk_fast, rst_fast);

   // Towards AWSteria_System
   AXI4_Master_Xactor_IFC #(wd_id_mmio,  wd_addr_mmio,  wd_data_mmio,  wd_user_mmio)
   mmio_axi4_M_xactor <- mkAXI4_Master_Xactor;

   // ----------------

   mkConnection (core.mmio_M, mmio_axi4_S_xactor.axi_side);

   mkConnection (mmio_axi4_S_xactor.o_wr_addr, mmio_axi4_syncbuf.from_M.i_aw);
   mkConnection (mmio_axi4_S_xactor.o_wr_data, mmio_axi4_syncbuf.from_M.i_w);
   mkConnection (mmio_axi4_S_xactor.i_wr_resp, mmio_axi4_syncbuf.from_M.o_b);
   mkConnection (mmio_axi4_S_xactor.o_rd_addr, mmio_axi4_syncbuf.from_M.i_ar);
   mkConnection (mmio_axi4_S_xactor.i_rd_data, mmio_axi4_syncbuf.from_M.o_r);

   mkConnection (mmio_axi4_syncbuf.to_S.o_aw, mmio_axi4_M_xactor.i_wr_addr);
   mkConnection (mmio_axi4_syncbuf.to_S.o_w,  mmio_axi4_M_xactor.i_wr_data);
   mkConnection (mmio_axi4_syncbuf.to_S.i_b,  mmio_axi4_M_xactor.o_wr_resp);
   mkConnection (mmio_axi4_syncbuf.to_S.o_ar, mmio_axi4_M_xactor.i_rd_addr);
   mkConnection (mmio_axi4_syncbuf.to_S.i_r,  mmio_axi4_M_xactor.o_rd_data);

   // ----------------------------------------------------------------
   // AXI4 interface for DMA

   // Towards AWSteria_Core
   AXI4_Master_Xactor_IFC #(wd_id_dma,  wd_addr_dma,  wd_data_dma,  wd_user_dma)
   dma_axi4_M_xactor <- mkAXI4_Master_Xactor (clocked_by clk_slow,
					      reset_by   rst_slow);

   AXI_SyncBuffer_IFC #(AXI4_Wr_Addr #(wd_id_dma, wd_addr_dma, wd_user_dma),
			AXI4_Wr_Data #(wd_data_dma, wd_user_dma),
			AXI4_Wr_Resp #(wd_id_dma, wd_user_dma),
			AXI4_Rd_Addr #(wd_id_dma, wd_addr_dma, wd_user_dma),
			AXI4_Rd_Data #(wd_id_dma, wd_data_dma, wd_user_dma))
   dma_axi4_syncbuf <- mkAXI_SyncBuffer (depth,
					 clk_fast, rst_fast,
					 clk_slow, rst_slow);

   // Towards AWSteria_System
   AXI4_Slave_Xactor_IFC #(wd_id_dma,  wd_addr_dma,  wd_data_dma,  wd_user_dma)
   dma_axi4_S_xactor <- mkAXI4_Slave_Xactor;

   // ----------------

   mkConnection (dma_axi4_M_xactor.axi_side, core.dma_S);

   mkConnection (dma_axi4_M_xactor.i_wr_addr, dma_axi4_syncbuf.to_S.o_aw);
   mkConnection (dma_axi4_M_xactor.i_wr_data, dma_axi4_syncbuf.to_S.o_w);
   mkConnection (dma_axi4_M_xactor.o_wr_resp, dma_axi4_syncbuf.to_S.i_b);
   mkConnection (dma_axi4_M_xactor.i_rd_addr, dma_axi4_syncbuf.to_S.o_ar);
   mkConnection (dma_axi4_M_xactor.o_rd_data, dma_axi4_syncbuf.to_S.i_r);

   mkConnection (dma_axi4_syncbuf.from_M.i_aw, dma_axi4_S_xactor.o_wr_addr);
   mkConnection (dma_axi4_syncbuf.from_M.i_w,  dma_axi4_S_xactor.o_wr_data);
   mkConnection (dma_axi4_syncbuf.from_M.o_b,  dma_axi4_S_xactor.i_wr_resp);
   mkConnection (dma_axi4_syncbuf.from_M.i_ar, dma_axi4_S_xactor.o_rd_addr);
   mkConnection (dma_axi4_syncbuf.from_M.o_r,  dma_axi4_S_xactor.i_rd_data);

   // ----------------------------------------------------------------
   // External interrupt sources

   // Wire (bus) driven by 'ext_interrupts' method
   Wire #(Bit #(t_n_interrupt_sources)) w_irqs <- mkBypassWire;

   // Clock-crossing for wire (bus)
   ReadOnly #(Bit #(t_n_interrupt_sources)) ro_sync_irqs
   <- mkNullCrossingWire (clocked_by clk_fast, clk_slow, w_irqs);

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
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = mem_axi4_M_xactor.axi_side;
   interface AXI4_Master_IFC mmio_M = mmio_axi4_M_xactor.axi_side;
   interface AXI4_Master_IFC dma_S  = dma_axi4_S_xactor.axi_side;

   // ----------------
   // External interrupt sources

   method Action ext_interrupts (Bit #(t_n_interrupt_sources) x);
      w_irqs <= x;
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
