// Copyright (c) 2021-2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AWSteria_Core;

// ================================================================
// This package defines a 'mkAWSteria_Core' module containing 
//     - mkCPU (the RISC-V CPU)
//     - mkCore_MMIO_Fabric
//     - mkNear_Mem_IO_AXI4   (memory-mapped MTIME, MTIMECMP, MSIP etc.)
//     - mkPLIC_16_2_7        (RISC-V platform level interrupt controller or other)
//     - mkDebug_Module       (RISC-V Debug Module, optional: INCLUDE_GDB_CONTROL)
//     - mkTV_Encode          (Tandem-Verification logic, optional: INCLUDE_TANDEM_VERIF)
// and their connecting logic

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
import AXI4_Fabric    :: *;
import AXI4_Deburster :: *;

// ================================================================
// Project imports

import SoC_Map     :: *;

// ----------------
// AWSteria_Core interface and related defs

import AWSteria_Core_IFC :: *;
import DM_Common         :: *;    // Debug Module interface etc.

`ifdef INCLUDE_PC_TRACE
import PC_Trace          :: *;    // Lightweight PC trace info
`endif

import TV_Info           :: *;    // Tandem Verification info

import AWSteria_Core_Reclocked :: *;

// ----------------
// RISC-V CPU and related IPs

import CPU_IFC     :: *;
import CPU         :: *;

import Core_MMIO_Fabric :: *;    // CPU MMIO to Fabric, Near_Mem_IO, PLIC, Boot ROM

import PLIC        :: *;
import PLIC_16_2_7 :: *;

import Boot_ROM :: *;

import DM_TV       :: *;    // Encapsulation of Debug Module and Tandem Verifier Encoder

import Near_Mem_IO_AXI4  :: *;

// se_control_status decoder
import Host_Control_Status :: *;

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
// This module is an optional thin wrapper around
// mkAWSteria_Core_Single_Clock, selecting a slower clock at which to
// run the inner module 'mkAWSteria_Core_Single_Clock'.

`ifndef INCLUDE_AWSTERIA_CORE_IFC_CLOCK_CROSSING
//----------------------------------------------------------------
// No clock crossing

(* synthesize *)
module mkAWSteria_Core #(Clock clk1,        // extra clock
			 Clock clk2,        // extra clock
			 Clock clk3,        // extra clock
			 Clock clk4,        // extra clock
			 Clock clk5)        // extra clock
                       (AWSteria_Core_IFC_Specialized);

   messageM ("\n    INFO: mkAWSteria_System --> AWSteria_Core: no clock crossing.");

   AWSteria_Core_IFC_Specialized
   core_single_clock <- mkAWSteria_Core_Single_Clock;
   return core_single_clock;
endmodule

//----------------------------------------------------------------
`else
//----------------------------------------------------------------
// With clock crossings
// Please see:  BlueCloudV_Infra/Doc/Clocks_in_AWSteria_HW_IFC.txt
// for details about frequencies of these incoming clocks.

(* synthesize *)
module mkAWSteria_Core #(Clock clk1,        // extra clock
			 Clock clk2,        // extra clock
			 Clock clk3,        // extra clock
			 Clock clk4,        // extra clock
			 Clock clk5)        // extra clock
                       (AWSteria_Core_IFC_Specialized);

   messageM ("\nINFO: mkAWSteria_Core: Using clk2 for Core.");

   let clk_cur  <- exposeCurrentClock;
   let rstn_cur <- exposeCurrentReset;

   // Choose clock, depending on target platform.
   // One of these must be defined.
`ifdef PLATFORM_AWSF1
   let clk_core = clk1;    // 125 MHz
   messageM ("    Core clock is clk1");
`endif
`ifdef PLATFORM_VCU118
   let clk_core = clk2;    // 100 MHz
   messageM ("    Core clock is clk2");
`endif

   MakeResetIfc reset_for_core <- mkReset (5,            // stages
					   True,         // startInRst
					   clk_core);    // for which this is a reset
   // interface MakeResetIfc;
   //     method Action assertReset();
   //     method Bool isAsserted();
   //     interface Reset new_rst;
   // endinterface

   let rstn_core = reset_for_core.new_rst;

   AWSteria_Core_IFC_Specialized
   core_single_clock <- mkAWSteria_Core_Single_Clock (clocked_by clk_core,
						      reset_by   rstn_core);

   AWSteria_Core_IFC_Specialized
   core_reclocked <- mkAWSteria_Core_Reclocked (clk_cur,  rstn_cur,
						clk_core, rstn_core,
						core_single_clock);

   return core_reclocked;
endmodule

//----------------------------------------------------------------
`endif

// ================================================================
// The extra clocks are typically slower clocks for some components
// that may need them.

typedef enum {
   MODULE_STATE_0A,       // start initialization actions on power-up
   MODULE_STATE_1A,       // finish initialization actions on power-up

   MODULE_STATE_1B,       // finish (re-) initialization actions

   MODULE_STATE_READY
   } Module_State
deriving (Bits, Eq, FShow);

(* synthesize *)
module mkAWSteria_Core_Single_Clock (AWSteria_Core_IFC_Specialized);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   Reg #(Module_State) rg_module_state <- mkReg (MODULE_STATE_0A);

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // The CPU
   CPU_IFC  cpu <- mkCPU;

   // A fabric for connecting CPU to {System, Near_Mem_IO, PLIC, Boot ROM}
   Core_MMIO_Fabric_IFC  mmio_fabric <- mkCore_MMIO_Fabric;

   // Near_Mem_IO
   Near_Mem_IO_AXI4_IFC  near_mem_io <- mkNear_Mem_IO_AXI4;

   // PLIC (Platform-Level Interrupt Controller)
   PLIC_IFC_16_2_7  plic <- mkPLIC_16_2_7;

   // The Boot ROM
   Boot_ROM_IFC  boot_rom <- mkBoot_ROM;

   // AXI4 Deburster in front of core's coherent DMA port
   AXI4_Deburster_IFC #(AXI4_Wd_Id,
			AXI4_Wd_Addr,
			AXI4_Wd_Data_A,
			AXI4_Wd_User)   dma_server_axi4_deburster <- mkAXI4_Deburster_A;
   mkConnection (dma_server_axi4_deburster.to_slave, cpu.dma_server);

   // Control-Status decoder
   Host_Control_Status_IFC host_cs <- mkHost_Control_Status;

   // Debug Module and Tandem Verifier complex
   let dm_tv_param = DM_TV_Param {
      cpu_dma_server:            dma_server_axi4_deburster.from_master,
`ifdef INCLUDE_TANDEM_VERIF
      cpu_trace_data_out:        cpu.trace_data_out,
`else
      cpu_trace_data_out:        getstub,
`endif
      cpu_hart0_put_other_req:   cpu.hart0_put_other_req,
      cpu_hart0_server_run_halt: cpu.hart0_server_run_halt,
      cpu_hart0_csr_mem_server:  cpu.hart0_csr_mem_server,
`ifdef ISA_F
      cpu_hart0_fpr_mem_server:  cpu.hart0_fpr_mem_server,
`endif
      cpu_hart0_gpr_mem_server:	 cpu.hart0_gpr_mem_server,
      cs_cl_hart0_reset:         host_cs.cl_cpu_reset,
      cs_cl_run_halt:            host_cs.cl_run_halt,
      cs_cl_csr_rw:              host_cs.cl_csr_rw};

   DM_TV_IFC dm_tv <- mkDM_TV (dm_tv_param);

   // External interrupts
   Reg #(Bit #(N_External_Interrupt_Sources)) rg_ext_intrs <- mkReg (0);

   // Non-maskable interrupts
   FIFOF #(Bool) f_nmi  <- mkFIFOF;
   Reg #(Bool)   rg_nmi <- mkReg (False);

   // ================================================================
   // Initialization actions
   
   function Action fa_initialization_start (Bool running);
      action
	 cpu.hart0_server_reset.request.put (running);
	 near_mem_io.server_reset.request.put (?);
	 plic.server_reset.request.put (?);
	 mmio_fabric.reset;
      endaction
   endfunction

   function ActionValue #(Bool) fav_initialization_finish;
      actionvalue
	 let running <- cpu.hart0_server_reset.response.get;
	 let rsp2    <- near_mem_io.server_reset.response.get;
	 let rsp3    <- plic.server_reset.response.get;
	 cpu.ma_ddr4_ready;    // TODO: get rid of this

	 near_mem_io.set_addr_map (zeroExtend (soc_map.m_near_mem_io_addr_base),
				   zeroExtend (soc_map.m_near_mem_io_addr_lim));

	 plic.set_addr_map (zeroExtend (soc_map.m_plic_addr_base),
			    zeroExtend (soc_map.m_plic_addr_lim));

	 boot_rom.set_addr_map (zeroExtend (soc_map.m_boot_rom_addr_base),
				zeroExtend (soc_map.m_boot_rom_addr_lim));

	 return running;
      endactionvalue
   endfunction

   // ================================================================
   // Initialization on power-up

   rule rl_first_init_start (rg_module_state == MODULE_STATE_0A);
      Bool running = True;
      fa_initialization_start (running);
      rg_module_state <= MODULE_STATE_1A;

      $display ("AWSteria_Core: Initialization start ...");
      $display ("    %m");
      $display ("    %0d: rule rl_first_init_start", cur_cycle);
   endrule

   rule rl_first_init_finish (rg_module_state == MODULE_STATE_1A);
      let running <- fav_initialization_finish;
      rg_module_state <= MODULE_STATE_READY;

      $display ("AWSteria_Core: Initialization finished ...");
      $display ("    %m");
      $display ("    %0d: rule rl_first_init_start", cur_cycle);
   endrule

   // ================================================================
   // Post-power-up (re-)initialization on Host-Control or Debug Module

   rule rl_reinitialization_start (rg_module_state == MODULE_STATE_READY);
      let running <- dm_tv.cl_reset.request.get;
      fa_initialization_start (running);
      rg_module_state <= MODULE_STATE_1B;

      $display ("AWSteria_Core: Re-initialization start ...");
      $display ("    %m");
      $display ("    %0d: rule rl_reinitialization_start", cur_cycle);
   endrule

   rule rl_reinitialization_finish (rg_module_state == MODULE_STATE_1B);
      let running <- fav_initialization_finish;
      rg_module_state <= MODULE_STATE_READY;
      dm_tv.cl_reset.response.put (running);

      $display ("AWSteria_Core: Re-initialization finished ...");
      $display ("    %m");
      $display ("    %0d: rule rl_reinitialization_start", cur_cycle);
   endrule

   // ================================================================
   // Connect CPU to mmio fabric, and fabric to Near_Mem_IO, PLIC and Boot ROM

   // Initiators on mmio fabric
   mkConnection (cpu.imem_master, mmio_fabric.v_from_masters [cpu_mmio_master_num]);

   // Targets on mmio fabric
   // default target is taken out directly viao Core interface to System
   mkConnection (mmio_fabric.v_to_slaves [near_mem_io_target_num], near_mem_io.axi4_slave);
   mkConnection (mmio_fabric.v_to_slaves [plic_target_num],        plic.axi4_slave);
   mkConnection (mmio_fabric.v_to_slaves [boot_rom_target_num],    boot_rom.slave);

   // ================================================================
   // Connect MTIME from near_mem_io to csr_regfile in CPU

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_time;
      cpu.ma_set_csr_time (near_mem_io.mv_read_mtime);
   endrule

   // ================================================================
   // Connect various interrupts to CPU

   // ----------------
   // SW interrupt from Near_Mem_IO (CLINT)

   Reg #(Bool) rg_sw_interrupt <- mkReg (False);

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_sw_interrupt;
      cpu.software_interrupt_req (rg_sw_interrupt);
   endrule

   rule rl_relay_sw_interrupt (rg_module_state == MODULE_STATE_READY);
      Bool x <- near_mem_io.get_sw_interrupt_req.get;
      rg_sw_interrupt <= x;
      // $display ("%0d: Core.rl_relay_sw_interrupt: %d", cur_cycle, pack (x));
   endrule

   // ----------------
   // Timer interrupt from Near_Mem_IO (CLINT)

   Reg #(Bool) rg_timer_interrupt <- mkReg (False);

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_timer_interrupt;
      cpu.timer_interrupt_req (rg_timer_interrupt);
   endrule

   rule rl_relay_timer_interrupt (rg_module_state == MODULE_STATE_READY);
      Bool x <- near_mem_io.get_timer_interrupt_req.get;
      rg_timer_interrupt <= x;
      // $display ("%0d: Core.rl_relay_timer_interrupt: %d", cur_cycle, pack (x));
   endrule

   // ----------------
   // External interrupts from PLIC

   Reg #(Bool) rg_m_external_interrupt <- mkReg (False);
   Reg #(Bool) rg_s_external_interrupt <- mkReg (False);

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_external_interrupt;
      cpu.m_external_interrupt_req (rg_m_external_interrupt);
      cpu.s_external_interrupt_req (rg_s_external_interrupt);
   endrule

   rule rl_relay_external_interrupt (rg_module_state == MODULE_STATE_READY);
      Bool meip = plic.v_targets [0].m_eip;
      rg_m_external_interrupt <= meip;

      Bool seip = plic.v_targets [1].m_eip;
      rg_s_external_interrupt <= seip;

      // $display ("%0d: AWSteria_Core.rl_relay_external_interrupt: %d", ur_cycle, pack (x));
   endrule

   // ================================================================
   // Connect external interrupts to PLIC

   // Drive PLIC's interrupt lines
   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_interrupt;
      for (Integer j = 0; j < valueOf (N_External_Interrupt_Sources); j = j + 1)
	 plic.v_sources [j].m_interrupt_req (unpack (rg_ext_intrs [j]));
   endrule

   // ================================================================
   // Non-maskable interrupts (NMI)

   // Register NMI set/clear requests
   rule rl_register_nmi (rg_module_state == MODULE_STATE_READY);
      Bool b <- pop (f_nmi);
      rg_nmi <= b;
   endrule

   // Drive CPU's NMI line
   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_nmi;
      cpu.nmi_req (rg_nmi);
   endrule

   // =================================================================
   // Misc. to host and from host

   FIFOF #(Bit #(32)) f_misc_from_host = dummy_FIFOF;

`ifdef INCLUDE_PC_TRACE
   FIFOF #(Bit #(32)) f_misc_to_host <- mkFIFOF;

   // Use f_misc_to_host for sampled PC trace
   Reg #(PC_Trace)   rg_pc_trace              <- mkRegU;
   Reg #(Bit #(64))  rg_pc_trace_interval_ctr <- mkReg (0);
   Reg #(Bit #(3))   rg_pc_trace_serialize_state <- mkReg (0);

   rule rl_pc_trace_0 (rg_pc_trace_serialize_state == 0);
      // Drain pc trace packet from CPU
      PC_Trace pc_trace <- cpu.g_pc_trace.get;
      rg_pc_trace <= pc_trace;

      match { .pc_trace_on, .pc_trace_interval } = host_cs.mv_pc_trace;

      if (pc_trace_on && (rg_pc_trace_interval_ctr == 0)) begin
	 // Serialize sample to f_misc_to_host in next several rules, 32b at a time
	 f_misc_to_host.enq (pc_trace.cycle [31:0]);
	 rg_pc_trace_interval_ctr    <= pc_trace_interval;
	 rg_pc_trace_serialize_state <= 1;
	 $display ("%0d: %m.rl_pc trace_0: cycle %0d  instret %0d  pc %0h (sending)",
		   cur_cycle, pc_trace.cycle, pc_trace.instret, pc_trace.pc);
      end
      else begin
	 // Discard the sample and countdown the interval
	 rg_pc_trace_interval_ctr <= rg_pc_trace_interval_ctr - 1;
      end
   endrule

   rule rl_pc_trace_1 (rg_pc_trace_serialize_state == 1);
      f_misc_to_host.enq (rg_pc_trace.cycle [63:32]);
      rg_pc_trace_serialize_state <= 2;
   endrule

   rule rl_pc_trace_2 (rg_pc_trace_serialize_state == 2);
      f_misc_to_host.enq (rg_pc_trace.instret [31:0]);
      rg_pc_trace_serialize_state <= 3;
   endrule

   rule rl_pc_trace_3 (rg_pc_trace_serialize_state == 3);
      f_misc_to_host.enq (rg_pc_trace.instret [63:32]);
      rg_pc_trace_serialize_state <= 4;
   endrule

   rule rl_pc_trace_4 (rg_pc_trace_serialize_state == 4);
      f_misc_to_host.enq (rg_pc_trace.pc [31:0]);
      rg_pc_trace_serialize_state <= 5;
   endrule

   rule rl_pc_trace_5 (rg_pc_trace_serialize_state == 5);
      f_misc_to_host.enq (rg_pc_trace.pc [63:32]);
      rg_pc_trace_serialize_state <= 0;
   endrule
`else
   FIFOF #(Bit #(32)) f_misc_to_host = dummy_FIFOF;
`endif

   // =================================================================
   // Misc CPU control/status

   rule rl_set_verbosity (rg_module_state == MODULE_STATE_READY);
      match { .verbosity, .log_delay } <- host_cs.g_verbosity.get;
      cpu.set_verbosity (verbosity, log_delay);
      $display ("AWSteria_Core: setting verbosity %0d log_delay %0d",
		verbosity, log_delay);
   endrule

   rule rl_watch_thost (rg_module_state == MODULE_STATE_READY);
      match { .watch, .tohost_addr } <- host_cs.g_watch_tohost.get;
      cpu.set_watch_tohost (watch, tohost_addr);
      $display ("AWSteria_Core: setting tohost watch %0d addr %0h",
		watch, tohost_addr);
   endrule

   rule rl_send_tohost_value;
      host_cs.ma_tohost_value (cpu.mv_tohost_value);
   endrule

   // ================================================================
   // INTERFACE

   // ----------------------------------------------------------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = cpu.mem_master;
   interface AXI4_Master_IFC mmio_M = mmio_fabric.v_to_slaves [default_target_num];
   interface AXI4_Slave_IFC  dma_S  = dm_tv.dma_S;

   // ----------------------------------------------------------------
   // External interrupt sources

   method Action ext_interrupts (Bit #(N_Core_External_Interrupt_Sources) x);
      rg_ext_intrs <= zeroExtend (x);
   endmethod

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   interface fi_nmi = to_FIFOF_I (f_nmi);

   // ----------------------------------------------------------------
   // Misc stream I/O, and Tandem Verification output

   interface fo_misc = to_FIFOF_O (f_misc_to_host);
   interface fi_misc = to_FIFOF_I (f_misc_from_host);

   interface fo_tv_info = dm_tv.fo_tv_info;

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // DMI (Debug Module Interface) facing remote debugger

   interface Server_DMI se_dmi = dm_tv.se_dmi;

   // Non-Debug-Module Reset (reset "all" except DM)
   // These Bit#(0) values are just tokens for signaling 'reset request' and 'reset done'

   interface cl_ndm_reset = dm_tv.cl_ndm_reset;

   // ----------------------------------------------------------------
   // Misc. control and status

   interface Server_Semi_FIFOF se_control_status = host_cs.se_control_status;
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
