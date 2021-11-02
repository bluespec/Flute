// Copyright (c) 2021 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AWSteria_Core;

// ================================================================
// This package defines a 'mkAWSteria_Core' module containing 
// - one or more Bluespec CPUs (Flute, Toooba, ...)
// - a PLIC (RISC-V platform level interrupt controller or other)
// - a Debug Module (RISC-V Debug Module or other)
// - a TV encoder (RISC-V or other)

// ================================================================
// Lib imports

// BSV libs
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import Vector       :: *;
import Clocks       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import Semi_FIFOF :: *;
import GetPut_Aux :: *;

// ----------------
// AXI

import AXI4_Types      :: *;
import AXI4_Deburster  :: *;

// ================================================================
// Project imports

import SoC_Map :: *;

// ----------------
// AWSteria_Core interface and related defs

import AWSteria_Core_IFC :: *;
import Interrupt_Defs    :: *;
import AXI_Param_Defs    :: *;
import DMI               :: *;
import PC_Trace          :: *;
import TV_Info           :: *;

// ----------------
// RISC-V Core and related interfaces

// Core (containing RISC-V CPU, Near_Mem_IO (CLINT), PLIC, Debug Module, TV encoder)
import Core_IFC     :: *;
import Core         :: *;
import PLIC         :: *;    // required for Core's interrupt interface
import Debug_Module :: *;    // required for Core's debug interface

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
			    N_External_Interrupt_Sources
			    ) AWSteria_Core_IFC_Specialized;

// ================================================================
// Local types and constants

typedef enum {SOC_START,
	      SOC_RESETTING,
`ifdef INCLUDE_GDB_CONTROL
	      SOC_RESETTING_NDM,
`endif
	      SOC_IDLE} SoC_State
deriving (Bits, Eq, FShow);

// Tags on Control commands arriving on Control/Status channels
// (se_control_status.request)

Bit #(4) tag_ddr4_is_loaded  = 0;    // ddr4 has been loaded from host
Bit #(4) tag_verbosity       = 1;    // set verbosity
Bit #(4) tag_no_watch_tohost = 2;    // set 'watch_tohost' to False
Bit #(4) tag_watch_tohost    = 3;    // set 'watch_tohost' to True
Bit #(4) tag_shutdown        = 4;    // stop simulation
Bit #(4) tag_pc_trace        = 5;    // set pc trace subsampling interval
Bit #(4) tag_env_ready       = 6;    // DDRs are ready for interaction

// ================================================================
// The extra clocks are typically slower clocks for some components
// that may need them.

(* synthesize *)
module mkAWSteria_Core #(Reset dm_reset,                // reset for Debug Module
			 Clock b_CLK, Reset b_RST_N,    // extra clock b
			 Clock c_CLK, Reset c_RST_N)    // extra clock c
                       (AWSteria_Core_IFC_Specialized);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   // Initialization FSM state
   Reg #(SoC_State) rg_state <- mkReg (SOC_START);

   // Core: CPU + Near_Mem_IO (CLINT) + PLIC + Debug module (optional) + TV (optional)
   Core_IFC #(PLIC_16_2_7::N_External_Interrupt_Sources)
   core <- mkCore (dm_reset);

   // AXI4 Deburster in front of core's coherent DMA port
   AXI4_Deburster_IFC #(AXI4_Wd_Id,
			AXI4_Wd_Addr,
			AXI4_Wd_Data_A,
			AXI4_Wd_User)   dma_server_axi4_deburster <- mkAXI4_Deburster_A;
   mkConnection (dma_server_axi4_deburster.to_slave, core.dma_server);

   // External interrupts
   Vector #(N_External_Interrupt_Sources, FIFOF #(Bool))
   v_f_ext_intrs <- replicateM (mkFIFOF);

   Vector #(N_External_Interrupt_Sources, Reg #(Bool))
   v_rg_ext_intrs <- replicateM (mkReg (False));

   // Non-maskable interrupts
   FIFOF #(Bool) f_nmi  <- mkFIFOF;
   Reg #(Bool)   rg_nmi <- mkReg (False);

   // PC Trace
   FIFOF #(PC_Trace) f_pc_trace               <- mkFIFOF;
   Reg #(Bool)       rg_pc_trace_on           <- mkReg (False);
   Reg #(Bit #(64))  rg_pc_trace_interval_ctr <- mkReg (0);
   Reg #(Bit #(64))  rg_pc_trace_interval_max <- mkRegU;

   // TV Info
   FIFOF # (TV_Info) f_tv_info <- mkFIFOF;

   // State for control and status
   Reg #(Bool) rg_env_ready         <- mkReg (False);    // AWSteria_Infra env is ready
   Reg #(Bool) rg_ddr4_is_loaded    <- mkReg (False);    // DDR4 contents have been loaded from host
   Reg #(Bool) rg_shutdown_received <- mkReg (False);    // For simulation shutdown

   // ================================================================
   // INITIALIZATION

   rule rl_init_start_initial (rg_ddr4_is_loaded && (rg_state == SOC_START));
      Bool running = True;
      core.cpu_reset_server.request.put (running);
      rg_state <= SOC_RESETTING;

      $display ("%0d: %m.rl_init_start_initial ...", cur_cycle);
   endrule

   rule rl_init_complete_initial (rg_state == SOC_RESETTING);
      let cpu_rsp <- core.cpu_reset_server.response.get;
      rg_state <= SOC_IDLE;

      $display ("%0d: %m.rl_init_complete_initial", cur_cycle);
   endrule

   Reg #(Bool) rg_TEMPORARY_DONE <- mkReg (False);
   rule rl_TEMPORARY (! rg_TEMPORARY_DONE);
      $display ("%0d: %m.rl_TEMPORARY: -> core.ma_ddr4_ready", cur_cycle);
      core.ma_ddr4_ready;
      rg_TEMPORARY_DONE <= True;
   endrule

   // ================================================================
   // External interrupts

   // Register interrupt set/clear requests
   for (Integer j = 0; j < valueOf (N_External_Interrupt_Sources); j = j + 1)
      rule rl_register_interrupt;
	 Bool b <- pop (v_f_ext_intrs [j]);
	 v_rg_ext_intrs [j] <= b;
      endrule

   // Drive Core's interrupt lines
   for (Integer j = 0; j < valueOf (N_External_Interrupt_Sources); j = j + 1)
      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_drive_interrupt;
	 core.core_external_interrupt_sources [j].m_interrupt_req (v_rg_ext_intrs [j]);
      endrule

   // Tie-off Core's unused interrupts
   for (Integer j = valueOf (N_External_Interrupt_Sources); j < 16; j = j + 1)
      rule rl_drive_no_interrupt;
	 core.core_external_interrupt_sources [j].m_interrupt_req (False);
      endrule

   // ================================================================
   // Non-maskable interrupts (NMI)

   // Register NMI set/clear requests
   rule rl_register_interrupt;
      Bool b <- pop (f_nmi);
      rg_nmi <= b;
   endrule

   // Drive Core's NMI line
   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_interrupt;
      core.nmi_req (rg_nmi);
   endrule

   // ================================================================
   // Host-to-HW Control channel
   // Writes are coded as follows: (ad hoc; we may evolve this as needed)
   // wdata [3:0] is a tag; [31:4] gives more info

   FIFOF #(Bit #(32)) f_host_to_hw_control <- mkFIFOF;
   FIFOF #(Bit #(32)) f_hw_to_host_status  <- mkFIFOF;

   rule rl_host_to_hw_control;
      Bit #(32) data <- pop (f_host_to_hw_control);
      Bit #(4)  tag = data [3:0];
      if (tag == tag_ddr4_is_loaded) begin
	 // data [31:4] ignored
	 $display ("%0d: %m.rl_host_to_hw_control: ddr4 loaded", cur_cycle);
	 rg_ddr4_is_loaded <= True;
      end
      else if (tag == tag_env_ready) begin
	 // data [31:4] ignored
	 $display ("%0d: %m.rl_host_to_hw_control: env ready (ddr4s ready)", cur_cycle);
	 Bool env_ready = (data [4] == 1);
	 rg_env_ready <= env_ready;
	 if (env_ready)
	    core.ma_ddr4_ready;
      end
      else if (tag == tag_verbosity) begin
	 // data [31:8]  = logdelay, [7:4] = verbosity
	 Bit #(4)  verbosity = data [7:4];
	 Bit #(64) logdelay  = zeroExtend (data [31:8]);
	 $display ("%0d: %m.rl_host_to_hw_control: verbosity %0d, logdelay %0h",
		   cur_cycle, verbosity, logdelay);
	 core.set_verbosity (verbosity, logdelay);
      end
      else if (tag == tag_no_watch_tohost) begin
	 // data [31:4] ignored
	 $display ("%0d: %m.rl_host_to_hw_control: do not watch tohost", cur_cycle);
	 core.set_watch_tohost (False, ?);
      end
      else if (tag == tag_watch_tohost) begin
	 // (data [31:4] << 4) = tohost_addr
	 Bit #(64) tohost_addr = zeroExtend ({ data [31:4], 4'b00 });
	 $display ("%0d: %m.rl_host_to_hw_control: watch tohost at addr %0h",
		   cur_cycle, tohost_addr);
	 core.set_watch_tohost (True, tohost_addr);
      end
      else if (tag == tag_shutdown) begin
	 // data [31:4] ignored
	 $display ("%0d: %m.rl_host_to_hw_control: SHUTDOWN", cur_cycle);
	 rg_shutdown_received <= True;
      end
      else if (tag == tag_pc_trace) begin
	 // data [7:4]  = (0 ? switch off PC tracing : switch on)
	 // data [31:8] = max of interval countdown (0 means every instruction)
	 if (data [7:4] == 0) begin
	    rg_pc_trace_on <= False;
	    $display ("%0d: %m.rl_host_to_hw_control: set PC trace off", cur_cycle);
	 end
	 else begin
	    rg_pc_trace_on <= True;
	    rg_pc_trace_interval_max <= zeroExtend (data [31:8]);
	    $display ("%0d: %m.rl_host_to_hw_control: set PC trace on, interval max = %0h",
		      cur_cycle, data [31:8]);
	 end
      end
      else begin
	 $display ("%0d: %m.rl_host_to_hw_control: ERROR: unrecognized control command %0h",
		   cur_cycle, data);
      end
   endrule

   // Return hw status to host
   // Encoding: { 16'tohost_value,
   //             4'ddr4_ready, 2'b0, 1'ddr4_is_loaded, 1'initialized_2, 8'soc_status}
   rule rl_hw_to_host_status;
      Bit #(32) status = zeroExtend(core.mv_status);
      if (rg_state == SOC_IDLE) status = status | (1 << 8);
      if (rg_ddr4_is_loaded)    status = status | (1 << 9);
      if (rg_env_ready)         status = status | (1 << 12);

      let tohost_value = core.mv_tohost_value;
      status = status | { tohost_value [15:0], 16'h0 };
      f_hw_to_host_status.enq (status);
      if ((verbosity > 0) && ((tohost_value != 0) || (status [7:0] != 0)))
	 $display ("%0d: %m.rl_hw_to_host_status: %0h", cur_cycle, status);
   endrule

   // ================================================================
   // Debug module requests and responses

`ifdef INCLUDE_GDB_CONTROL
   FIFOF #(DMI_Req) f_dmi_reqs <- mkFIFOF;
   FIFOF #(DMI_Rsp) f_dmi_rsps <- mkFIFOF;

   rule rl_dmi_req;
      let req <- pop (f_dmi_reqs);
      if (req.is_read)
	 core.dm_dmi.read_addr (req.addr);
      else
	 core.dm_dmi.write (req.addr, req.wdata);
   endrule

   rule rl_dmi_rsp;
      DM_Common::DMI core_dm_dmi = core.dm_dmi;

      let x <- core.dm_dmi.read_data;
      let rsp = DMI_Rsp { rdata: x };
      f_dmi_rsps.enq (rsp);
   endrule
`else
   FIFOF #(DMI_Req) f_dmi_reqs = dummy_FIFOF;
   FIFOF #(DMI_Rsp) f_dmi_rsps = dummy_FIFOF;
`endif

   // ================================================================
   // NDM (non-debug-module) reset (requested from Debug Module)
   // Request argument indicates if CPU comes up running or halted

`ifdef INCLUDE_GDB_CONTROL
   FIFOF #(Bit #(0)) f_ndm_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_ndm_reset_rsps <- mkFIFOF;
   Reg #(Bool) rg_running <- mkRegU;

   rule rl_ndm_reset_start (rg_state == SOC_IDLE);
      let running <- core.ndm_reset_client.request.get;
      rg_running <= running;

      core.cpu_reset_server.request.put (running);
      rg_state <= SOC_RESETTING_NDM;

      $display ("%0d: %m.rl_ndm_reset_start (non-debug-module) running = ",
		cur_cycle, fshow (running));
   endrule

   rule rl_ndm_reset_complete (rg_state == SOC_RESETTING_NDM);
      let cpu_rsp <- core.cpu_reset_server.response.get;
      rg_state <= SOC_IDLE;

      core.ndm_reset_client.response.put (rg_running);

      $display ("%0d: %m.rl_ndm_reset_complete (non-debug-module) running = ",
		cur_cycle, fshow (rg_running));
   endrule
`else
   FIFOF #(Bit #(0)) f_ndm_reset_reqs = dummy_FIFOF;
   FIFOF #(Bit #(0)) f_ndm_reset_rsps = dummy_FIFOF;
`endif

   // =================================================================
   // PC trace output

   rule rl_pc_trace;
      PC_Trace x <- core.g_pc_trace.get;

      if (rg_pc_trace_on && (rg_pc_trace_interval_ctr == 0)) begin
	 f_pc_trace.enq (x);
	 rg_pc_trace_interval_ctr <= rg_pc_trace_interval_max;
      end
      else begin
	 // Discard the sample
	 rg_pc_trace_interval_ctr <= rg_pc_trace_interval_ctr - 1;
      end
   endrule

   // ================================================================
   // INTERFACE

   // ----------------------------------------------------------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = core.core_mem_master;
   interface AXI4_Master_IFC mmio_M = core.cpu_imem_master;
   interface AXI4_Slave_IFC  dma_S  = dma_server_axi4_deburster.from_master;

   // ----------------------------------------------------------------
   // External interrupt sources

   interface v_fi_external_interrupt_reqs = map (to_FIFOF_I, v_f_ext_intrs);

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   interface fi_nmi = to_FIFOF_I (f_nmi);

   // ----------------------------------------------------------------
   // Trace and Tandem Verification output

   interface fo_pc_trace = to_FIFOF_O (f_pc_trace);
   interface fo_tv_info  = to_FIFOF_O (f_tv_info);

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // DMI (Debug Module Interface) facing remote debugger

   interface Server_DMI se_dmi = fifofs_to_Server_Semi_FIFOF (f_dmi_reqs, f_dmi_rsps);

   // Non-Debug-Module Reset (reset "all" except DM)
   // These Bit#(0) values are just tokens for signaling 'reset request' and 'reset done'

   interface cl_ndm_reset = fifofs_to_Client_Semi_FIFOF (f_ndm_reset_reqs, f_ndm_reset_rsps);

   // ----------------------------------------------------------------
   // Misc. control and status

   interface Server_Semi_FIFOF se_control_status
   = fifofs_to_Server_Semi_FIFOF (f_host_to_hw_control, f_hw_to_host_status);
endmodule

// ****************************************************************
// Specialization of parameterized AXI4 Debursters for this SoC.

(* synthesize *)
module mkAXI4_Deburster_A (AXI4_Deburster_IFC #(AXI4_Wd_Id,
						AXI4_Wd_Addr,
						AXI4_Wd_Data_A,
						AXI4_Wd_User));
   let m <- mkAXI4_Deburster;
   return m;
endmodule

// ================================================================

endpackage
