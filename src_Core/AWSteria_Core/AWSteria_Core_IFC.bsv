// Copyright (c) 2021-2022 Bluespec, Inc. All Rights Reserved.

package AWSteria_Core_IFC;

// ================================================================
// This package defines the interface for an 'mkAWSteria_Core' module
// which may contain one or more of the following:
//   - one or more CPUs (of RISC-V ISA or other ISA)
//   - a PLIC (RISC-V platform level interrupt controller or other)
//   - a Debug Module (RISC-V Debug Module or other)
//   - a TV encoder (RISC-V or other)

// To produce a specific 'mkAWSteria_Core' module (containing specific CPUs etc.):
// - Either write it directly in BSV
// - Or compile AWSteria_Core_Empty.bsv to produce an 'empty'
//       mkAWSteria_Core.v with the correct interface,
//       then fill in the contents with RTL.

// ================================================================

// from BSV library
import Vector :: *;

// from should-be-in-BSV-library
import Semi_FIFOF :: *;

// from AXI library
import AXI4_Types :: *;

// Local: Debug Module interface
import DM_Common :: *;    // for Server_DMI

// Local: Tandem Verification
import TV_Info  :: *;

// ================================================================
// Interrupt definitions

// Number of external interrupts

typedef  5  N_Core_External_Interrupt_Sources;
Integer  n_core_external_interrupt_sources_I = valueOf (N_Core_External_Interrupt_Sources);

// Interrupt request numbers

Integer irq_num_uart16550_0   = 0;

Integer irq_num_host_to_hw_1  = 1;
Integer irq_num_host_to_hw_2  = 2;
Integer irq_num_host_to_hw_3  = 3;
Integer irq_num_host_to_hw_4  = 4;

Integer irq_num_max = n_core_external_interrupt_sources_I - 1;

// ================================================================
// AXI4 widths (coherent DMA and DDR paths)

typedef 16   AXI4_Wd_Id;

typedef 64   AXI4_Wd_Addr;

typedef 512  AXI4_Wd_Data_A;    // For coherent DMA interfaces
typedef 64   AXI4_Wd_Data_B;    // For MMIO interfaces

typedef 0    AXI4_Wd_User;

// ================================================================
// The Core interface

// NOTE: there are also extra incoming clocks and resets, which are not captured in this IFC.
//       See the module paramters in AWSteria_Core_Empty.bsv

// Not all subinterfaces need be used.
// A minimal core may only use mem_M and mmio_M, and tie-off all the rest.

interface AWSteria_Core_IFC #(numeric type wd_id_mem,
			      numeric type wd_addr_mem,
			      numeric type wd_data_mem,
			      numeric type wd_user_mem,

			      numeric type wd_id_mmio,
			      numeric type wd_addr_mmio,
			      numeric type wd_data_mmio,
			      numeric type wd_user_mmio,

			      numeric type wd_id_dma,
			      numeric type wd_addr_dma,
			      numeric type wd_data_dma,
			      numeric type wd_user_dma,

			      numeric type t_n_interrupt_sources);

   // ----------------------------------------------------------------
   // AXI4 interfaces for memory, MMIO, and DMA
   // Note: DMA may or may not be coherent, depending on internal Core architecture.

   interface AXI4_Master_IFC #(wd_id_mem,  wd_addr_mem,  wd_data_mem,  wd_user_mem)  mem_M;
   interface AXI4_Master_IFC #(wd_id_mmio, wd_addr_mmio, wd_data_mmio, wd_user_mmio) mmio_M;
   interface AXI4_Slave_IFC  #(wd_id_dma,  wd_addr_dma,  wd_data_dma,  wd_user_dma)  dma_S;

   // ----------------------------------------------------------------
   // External interrupt sources

   method Action ext_interrupts (Bit #(t_n_interrupt_sources) x);

   // ----------------------------------------------------------------
   // Non-maskable interrupt request

   interface FIFOF_I #(Bool) fi_nmi;

   // ----------------------------------------------------------------
   // Misc I/O streams

   interface FIFOF_O #(Bit #(32)) fo_misc;
   interface FIFOF_I #(Bit #(32)) fi_misc;

   // ----------------------------------------------------------------
   // Tandem Verification output

   interface FIFOF_O #(TV_Info)  fo_tv_info;

   // ----------------------------------------------------------------
   // Debug Module interfaces

   // DMI (Debug Module Interface) facing remote debugger

   interface Server_DMI se_dmi;

   // Non-Debug-Module Reset (reset "all" except DM)
   // These Bit#(0) values are just tokens for signaling 'reset request' and 'reset done'

   interface Client_Semi_FIFOF #(Bit #(0), Bit #(0)) cl_ndm_reset;

   // ----------------------------------------------------------------
   // Misc. control and status
   // The interpretation of these 32-bit values is left up to the specific Core

   interface Server_Semi_FIFOF #(Bit #(32), Bit #(32)) se_control_status;
endinterface

// ================================================================

endpackage
