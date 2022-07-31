// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AWSteria_Core_Inner;

// ================================================================
// This package defines module 'mkAWSteria_Core_Inner' that is
// located as follows:
//
//     mkAWSteria_Core
//         mkHost_Control_Status
//         mkAWSteria_Core_Inner
// 
// The inner core can be run at a slower clock.
// The inner core can be reset by mkHost_Control_Status.

// The inner core contains:
//     - mkCPU (the RISC-V CPU)
//     - mkCore_MMIO_Fabric
//     - mkNear_Mem_IO_AXI4     (memory-mapped MTIME, MTIMECMP, MSIP etc.)
//     - mkPLIC_16_2_7          (RISC-V platform level interrupt controller or other)
//     - mkDebug_Module         (RISC-V Debug Module, optional: INCLUDE_GDB_CONTROL)
//     - mkTV_Encode            (Tandem-Verification logic, optional: INCLUDE_TANDEM_VERIF)
// and their connecting logic.

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
// Defs used in AWSteria_Core_Inner_IFC and related defs

import AWSteria_Core_IFC :: *;    // For AXI4_Wd_{Id,Addr,Data_A,Data_B,_User}
                                  // and N_Core_External_Interrupt_Sources

import ISA_Decls         :: *;
import Fabric_Defs       :: *;    // for Wd_{Id,Addr,Data,User}

import DM_Common         :: *;    // Debug Module interface etc.
import DM_CPU_Req_Rsp    :: *;

`ifdef INCLUDE_PC_TRACE
import PC_Trace          :: *;    // Lightweight PC trace info
`endif

import TV_Info           :: *;    // Tandem Verification info

// ----------------
// RISC-V CPU and related IPs

import CPU_IFC     :: *;
import CPU         :: *;

import Core_MMIO_Fabric :: *;    // CPU MMIO to Fabric, Near_Mem_IO, PLIC, Boot ROM

import PLIC        :: *;
import PLIC_16_2_7 :: *;

import Boot_ROM :: *;

import TVE_Wrapper :: *;    // Encapsulation of Tandem Verifier Encoder

import Near_Mem_IO_AXI4  :: *;

// ================================================================
// Core_Inner's interface:
//     mostly AWSteria_Core_IFC, minus 'se_control_status' since mkHost_Control_Status
//         is outside and controls a reset for Core_Inner
//     plus other control interfaces from mkHost_Control_Status to Core_Inner

interface AWSteria_Core_Inner_IFC;
   // ----------------------------------------------------------------
   // Interfaces that go directly out to AWSteria_Core_IFC

   // ----------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC #(AXI4_Wd_Id, AXI4_Wd_Addr, AXI4_Wd_Data_A, AXI4_Wd_User) mem_M;
   interface AXI4_Master_IFC #(AXI4_Wd_Id, AXI4_Wd_Addr, AXI4_Wd_Data_B, AXI4_Wd_User) mmio_M;
   interface AXI4_Slave_IFC  #(AXI4_Wd_Id, AXI4_Wd_Addr, AXI4_Wd_Data_A, AXI4_Wd_User) dma_S;

   // ----------------
   // External interrupt sources

   method Action ext_interrupts (Bit #(N_Core_External_Interrupt_Sources) x);

   // ----------------
   // Non-maskable interrupt request

   interface FIFOF_I #(Bool) fi_nmi;

   // ----------------
   // Misc I/O streams

   interface FIFOF_O #(Bit #(32)) fo_misc;
   interface FIFOF_I #(Bit #(32)) fi_misc;

   // ----------------
   // Tandem Verification output

   interface FIFOF_O #(TV_Info)  fo_tv_info;

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // GPR access
   interface Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) hart0_gpr_mem_server;
`ifdef ISA_F
   // FPR access
   interface Server #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(FLEN)) hart0_fpr_mem_server;
`endif
   // CSR access
   interface Server #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN)) hart0_csr_mem_server;

   // System Bus (Mem) access
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) sba_S;

   // ----------------------------------------------------------------
   // Interfaces for mkHost_Control_Status

   // PC Trace control
   interface FIFOF_I #(Tuple2 #(Bool, Bit #(64))) fi_pc_trace_control;

   // Set core's verbosity and logdelay
   interface FIFOF_I #(Tuple2 #(Bit #(4), Bit #(64))) fi_verbosity_control;

`ifdef WATCH_TOHOST
   // Set watch-tohost on/off with tohost address
   interface FIFOF_I #(Tuple2 #(Bool, Bit #(64))) fi_watch_tohost_control;
   // Get tohost value
   interface FIFOF_O #(Bit #(64)) fo_tohost_value;
`endif
endinterface

// ================================================================
// AWSteria_Core: single clocked, resettable by Debug_Module or
// Host_Control_Status module

typedef enum {
   MODULE_STATE_INIT_0,       // start post-reset initializations
   MODULE_STATE_INIT_1,       // finish post-reset initializations

   MODULE_STATE_READY
   } Module_State
deriving (Bits, Eq, FShow);

(* synthesize *)
module mkAWSteria_Core_Inner (AWSteria_Core_Inner_IFC);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   Reg #(Module_State) rg_module_state <- mkReg (MODULE_STATE_INIT_0);

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

   // Tandem Verification Encoder wrapper
   // and its connections to CPU
   let tve_wrapper_param = TVE_Wrapper_Param {
`ifdef INCLUDE_TANDEM_VERIF
      cpu_trace_data_out:        cpu.trace_data_out,
`else
      cpu_trace_data_out:        getstub,
`endif
`ifdef INCLUDE_GDB_CONTROL
      cpu_hart0_gpr_mem_server:	 cpu.hart0_gpr_mem_server,
`ifdef ISA_F
      cpu_hart0_fpr_mem_server:  cpu.hart0_fpr_mem_server,
`endif
      cpu_hart0_csr_mem_server:  cpu.hart0_csr_mem_server,
`endif
      cpu_dma_server:            dma_server_axi4_deburster.from_master
      };

   TVE_Wrapper_IFC tve_wrapper <- mkTVE_Wrapper (tve_wrapper_param);

   // ================================================================
   // Post-reset initialization

   rule rl_first_init_start (rg_module_state == MODULE_STATE_INIT_0);
      Bool running = True;

      cpu.hart0_server_reset.request.put (running);
      near_mem_io.server_reset.request.put (?);
      plic.server_reset.request.put (?);
      mmio_fabric.reset;

      rg_module_state <= MODULE_STATE_INIT_1;

      $display ("AWSteria_Core_Inner: start post-reset initializations ...");
      $display ("    %m");
      $display ("    %0d: rule rl_first_init_start", cur_cycle);
   endrule

   rule rl_first_init_finish (rg_module_state == MODULE_STATE_INIT_1);
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

      rg_module_state <= MODULE_STATE_READY;

      $display ("AWSteria_Core_Inner: finish post-reeest Initializations ...");
      $display ("    %m");
      $display ("    %0d: rule rl_first_init_start", cur_cycle);
   endrule

   // ================================================================
   // Connect CPU to mmio fabric, and mmio fabric to Near_Mem_IO, PLIC and Boot ROM

   // Initiators on mmio fabric
   mkConnection (cpu.imem_master, mmio_fabric.v_from_masters [cpu_mmio_master_num]);

   // Targets on mmio fabric
   mkConnection (mmio_fabric.v_to_slaves [near_mem_io_target_num], near_mem_io.axi4_slave);
   mkConnection (mmio_fabric.v_to_slaves [plic_target_num],        plic.axi4_slave);
   mkConnection (mmio_fabric.v_to_slaves [boot_rom_target_num],    boot_rom.slave);
   //            mmio_fabric.v_to_slaves [default_target_num] lifted to module interface

   // ================================================================
   // Connect MTIME from near_mem_io to csr_regfile in CPU,
   // where it is used for user-level TIMEH, TIME

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

   // External interrupts
   Reg #(Bit #(N_External_Interrupt_Sources)) rg_ext_intrs <- mkReg (0);

   // Drive PLIC's interrupt lines
   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_drive_interrupt;
      for (Integer j = 0; j < valueOf (N_External_Interrupt_Sources); j = j + 1)
	 plic.v_sources [j].m_interrupt_req (unpack (rg_ext_intrs [j]));
   endrule

   // ================================================================
   // Non-maskable interrupts (NMI)

   // Non-maskable interrupts
   FIFOF #(Bool) f_nmi  <- mkFIFOF;
   Reg #(Bool)   rg_nmi <- mkReg (False);

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
   // fo_misc: if INCLUDE_PC_TRACE, send PC trace out through this, else unused
   // fi_misc: unused

   FIFOF #(Bit #(32)) f_misc_from_host = dummy_FIFOF;    // unused

`ifdef INCLUDE_PC_TRACE
   Reg #(Tuple2 #(Bool, Bit #(64))) rg_pc_trace_control <- mkReg (tuple2 (False, ?));
   FIFOF #(Bit #(32))               f_misc_to_host      <- mkFIFOF;

   Reg #(PC_Trace)   rg_pc_trace                 <- mkRegU;
   Reg #(Bit #(64))  rg_pc_trace_interval_ctr    <- mkReg (0);
   Reg #(Bit #(3))   rg_pc_trace_serialize_state <- mkReg (0);

   rule rl_pc_trace_0 (rg_pc_trace_serialize_state == 0);
      // Drain pc trace packet from CPU
      PC_Trace pc_trace <- cpu.g_pc_trace.get;
      rg_pc_trace <= pc_trace;

      match { .pc_trace_on, .pc_trace_interval } = rg_pc_trace_control;

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
   FIFOF #(Bit #(32)) f_misc_to_host = dummy_FIFOF;    // unused
`endif

   // ================================================================
   // To_Host value
   // Enqueue each changed value

   FIFOF #(Bit #(64)) f_tohost_value <- mkFIFOF;
   Reg #(Bit #(64))   rg_prev_tohost_value <- mkReg (0);

   rule rl_tohost_value;
      Bit #(64) tohost_value = cpu.mv_tohost_value;
      if (tohost_value != rg_prev_tohost_value) begin
	 f_tohost_value.enq (tohost_value);
	 rg_prev_tohost_value <= tohost_value;
	 // $display ("AWSteria_Core_Inner: new tohost_value = 0x%0h", tohost_value);
      end
   endrule

   // ================================================================
   // INTERFACE

   // ----------------------------------------------------------------
   // Interfaces that go directly out to AWSteria_Core_IFC

   // ----------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC mem_M  = cpu.mem_master;
   interface AXI4_Master_IFC mmio_M = mmio_fabric.v_to_slaves [default_target_num];
   interface AXI4_Slave_IFC  dma_S  = tve_wrapper.dma_S;

   // ----------------
   // External interrupt sources

   method Action ext_interrupts (Bit #(N_Core_External_Interrupt_Sources) x);
      rg_ext_intrs <= zeroExtend (x);
   endmethod

   // ----------------
   // Non-maskable interrupt request

   interface fi_nmi = to_FIFOF_I (f_nmi);

   // ----------------
   // Misc I/O streams

   interface fo_misc = to_FIFOF_O (f_misc_to_host);
   interface fi_misc = to_FIFOF_I (f_misc_from_host);

   // ----------------
   // Tandem Verification output

   interface fo_tv_info = tve_wrapper.fo_tv_info;

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // GPR access
   interface Server hart0_gpr_mem_server = tve_wrapper.hart0_gpr_mem_server;
`ifdef ISA_F
   // FPR access
   interface Server hart0_fpr_mem_server = tve_wrapper.hart0_fpr_mem_server;
`endif
   // CSR access
   interface Server hart0_csr_mem_server = tve_wrapper.hart0_csr_mem_server;

   // System Bus (Mem) access
   interface AXI4_Master_IFC sba_S = tve_wrapper.sba_S;

   // ----------------------------------------------------------------
   // Interfaces for mkHost_Control_Status

`ifdef INCLUDE_PC_TRACE
   // PC Trace control
   interface FIFOF_I fi_pc_trace_control;
      method Action enq (Tuple2 #(Bool, Bit #(64)) x);
	 rg_pc_trace_control <= x;
      endmethod
      method notFull = True;
   endinterface
`else
   interface FIFOF_I fi_pc_trace_control = dummy_FIFOF_I;
`endif

   // ----------------------------------------------------------------
   // To CPU

   // ----------------
   // Set core's verbosity and logdelay

   interface FIFOF_I fi_verbosity_control;
      method Action enq (Tuple2 #(Bit #(4), Bit #(64)) xy);
	 match { .verbosity, .logdelay } = xy;
	 cpu.set_verbosity (verbosity, logdelay);
      endmethod
      method notFull = True;
   endinterface

   // ----------------
   // Set watch-tohost on/off with tohost address

`ifdef WATCH_TOHOST
   interface FIFOF_I fi_watch_tohost_control;
      method Action enq (Tuple2 #(Bool, Bit #(64)) xy);
	 match { .watch_tohost, .tohost_addr } = xy;
	 cpu.set_watch_tohost (watch_tohost, tohost_addr);
      endmethod
      method notFull = True;
   endinterface

   // Get tohost value
   interface FIFOF_O fo_tohost_value = to_FIFOF_O (f_tohost_value);
`endif

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
