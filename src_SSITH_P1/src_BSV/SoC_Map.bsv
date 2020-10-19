// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

package SoC_Map;

// ================================================================
// This module defines the overall 'address map' of the SoC, showing
// the addresses serviced by each slave IP, and which addresses are
// memory vs. I/O.

// ***** WARNING! WARNING! WARNING! *****

// During system integration, this address map should be identical to
// the system interconnect settings (e.g., routing of requests between
// masters and slaves).  This map is also needed by software so that
// it knows how to address various IPs.

// This module contains no state; it just has constants, and so can be
// freely instantiated at multiple places in the SoC module hierarchy
// at no hardware cost.  It allows this map to be defined in one
// place and shared across the SoC.

// ================================================================
// This version of SoC_Map is for the DARPA SSITH GFE

// Our "Near_Mem_IO" corresponds to "CLINT" in Rocket

// ================================================================
// Exports

export  SoC_Map_IFC (..), mkSoC_Map;

export  Num_Masters;
export  imem_master_num;
export  dmem_master_num;
export  accel0_master_num;

export  Num_Slaves;
export  boot_rom_slave_num;
export  mem0_controller_slave_num;
export  uart0_slave_num;
export  gpio0_slave_num;
export  quad_spi_0_mem_slave_num;
export  quad_spi_0_reg_slave_num;
export  accel0_slave_num;

export  N_External_Interrupt_Sources;
export  n_external_interrupt_sources;
export  irq_num_uart16550_0;
export  irq_num_accel0;
export  irq_num_quad_spi_0;
export  irq_num_gpio0;

// ================================================================
// Bluespec library imports

// None

// ================================================================
// Project imports

import Fabric_Defs :: *;    // Only for type Fabric_Addr

// ================================================================
// Interface and module for the address map

interface SoC_Map_IFC;
   (* always_ready *)   method  Fabric_Addr  m_plic_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_plic_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_plic_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_quad_spi_0_mem_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_quad_spi_0_mem_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_quad_spi_0_mem_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_quad_spi_0_reg_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_quad_spi_0_reg_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_quad_spi_0_reg_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_ethernet_0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_ethernet_0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_ethernet_0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_dma_0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_dma_0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_dma_0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_uart16550_0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_uart16550_0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_uart16550_0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_gpio0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_gpio0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_gpio0_addr_lim;

`ifdef INCLUDE_ACCEL0
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_lim;
`endif

   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_uncached_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_uncached_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_uncached_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_cached_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_cached_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_cached_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_lim;

   (* always_ready *)
   method  Bool  m_is_mem_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_IO_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr);

   (* always_ready *)   method  Bit #(64)  m_pc_reset_value;
   (* always_ready *)   method  Bit #(64)  m_mtvec_reset_value;
   (* always_ready *)   method  Bit #(64)  m_nmivec_reset_value;
endinterface

// ================================================================

(* synthesize *)
module mkSoC_Map (SoC_Map_IFC);

   // ----------------------------------------------------------------
   // PLIC

   Fabric_Addr plic_addr_base = 'h_0C00_0000;
   Fabric_Addr plic_addr_size = 'h_0040_0000;    // 4M
   Fabric_Addr plic_addr_lim  = plic_addr_base + plic_addr_size;

   function Bool fn_is_plic_addr (Fabric_Addr addr);
      return ((plic_addr_base <= addr) && (addr < plic_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Near_Mem_IO (CLINT)

   Fabric_Addr near_mem_io_addr_base = 'h_1000_0000;
   Fabric_Addr near_mem_io_addr_size = 'h_0001_0000;    // 64K
   Fabric_Addr near_mem_io_addr_lim  = near_mem_io_addr_base + near_mem_io_addr_size;

   function Bool fn_is_near_mem_io_addr (Fabric_Addr addr);
      return ((near_mem_io_addr_base <= addr) && (addr < near_mem_io_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Quad_spi_0 Mem

   Fabric_Addr quad_spi_0_mem_addr_base = 'h_4000_0000;
   Fabric_Addr quad_spi_0_mem_addr_size = 'h_0100_0000;    // 16M
   Fabric_Addr quad_spi_0_mem_addr_lim  = quad_spi_0_mem_addr_base + quad_spi_0_mem_addr_size;

   function Bool fn_is_quad_spi_0_mem_addr (Fabric_Addr addr);
      return ((quad_spi_0_mem_addr_base <= addr) && (addr < quad_spi_0_mem_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Quad_spi_0 Reg

   Fabric_Addr quad_spi_0_reg_addr_base = 'h_6240_0000;
   Fabric_Addr quad_spi_0_reg_addr_size = 'h_0900_1000;    // 4KB
   Fabric_Addr quad_spi_0_reg_addr_lim  = quad_spi_0_reg_addr_base + quad_spi_0_reg_addr_size;

   function Bool fn_is_quad_spi_0_reg_addr (Fabric_Addr addr);
      return ((quad_spi_0_reg_addr_base <= addr) && (addr < quad_spi_0_reg_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Ethernet 0

   Fabric_Addr ethernet_0_addr_base = 'h_6210_0000;
   Fabric_Addr ethernet_0_addr_size = 'h_0004_0000;    // 256K
   Fabric_Addr ethernet_0_addr_lim  = ethernet_0_addr_base + ethernet_0_addr_size;

   function Bool fn_is_ethernet_0_addr (Fabric_Addr addr);
      return ((ethernet_0_addr_base <= addr) && (addr < ethernet_0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // DMA 0

   Fabric_Addr dma_0_addr_base = 'h_6220_0000;
   Fabric_Addr dma_0_addr_size = 'h_0001_0000;    // 64K
   Fabric_Addr dma_0_addr_lim  = dma_0_addr_base + dma_0_addr_size;

   function Bool fn_is_dma_0_addr (Fabric_Addr addr);
      return ((dma_0_addr_base <= addr) && (addr < dma_0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // UART 0

   Fabric_Addr uart16550_0_addr_base = 'h_6230_0000;
   Fabric_Addr uart16550_0_addr_size = 'h_0000_1000;    // 4K
   Fabric_Addr uart16550_0_addr_lim  = uart16550_0_addr_base + uart16550_0_addr_size;

   function Bool fn_is_uart16550_0_addr (Fabric_Addr addr);
      return ((uart16550_0_addr_base <= addr) && (addr < uart16550_0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // ACCEL 0

`ifdef INCLUDE_ACCEL0
   Fabric_Addr accel0_addr_base = 'h6240_0000;
   Fabric_Addr accel0_addr_size = 'h0000_1000;    // 4K
   Fabric_Addr accel0_addr_lim  = accel0_addr_base + accel0_addr_size;

   function Bool fn_is_accel0_addr (Fabric_Addr addr);
      return ((accel0_addr_base <= addr) && (addr < accel0_addr_lim));
   endfunction
`endif

   // ----------------------------------------------------------------
   // GPIO 0

   Fabric_Addr gpio0_addr_base = 'h_6FFF_0000;
   Fabric_Addr gpio0_addr_size = 'h_0001_0000;    // 64K
   Fabric_Addr gpio0_addr_lim  = gpio0_addr_base + gpio0_addr_size;

   function Bool fn_is_gpio0_addr (Fabric_Addr addr);
      return ((gpio0_addr_base <= addr) && (addr < gpio0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Boot ROM

   Fabric_Addr boot_rom_addr_base = 'h_7000_0000;
   Fabric_Addr boot_rom_addr_size = 'h_0000_1000;    // 4K
   Fabric_Addr boot_rom_addr_lim  = boot_rom_addr_base + boot_rom_addr_size;

   function Bool fn_is_boot_rom_addr (Fabric_Addr addr);
      return ((boot_rom_addr_base <= addr) && (addr < boot_rom_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // DDR memory 0 uncached

   Fabric_Addr ddr4_0_uncached_addr_base = 'h_CC00_0000;
   Fabric_Addr ddr4_0_uncached_addr_size = 'h_0400_0000;    //
   Fabric_Addr ddr4_0_uncached_addr_lim  = ddr4_0_uncached_addr_base + ddr4_0_uncached_addr_size;

   function Bool fn_is_ddr4_0_uncached_addr (Fabric_Addr addr);
      return ((ddr4_0_uncached_addr_base <= addr) && (addr < ddr4_0_uncached_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // DDR memory 0 cached

   Fabric_Addr ddr4_0_cached_addr_base = 'h_C000_0000;
   Fabric_Addr ddr4_0_cached_addr_size = 'h_0C00_0000;
   Fabric_Addr ddr4_0_cached_addr_lim  = ddr4_0_cached_addr_base + ddr4_0_cached_addr_size;

   function Bool fn_is_ddr4_0_cached_addr (Fabric_Addr addr);
      return ((ddr4_0_cached_addr_base <= addr) && (addr < ddr4_0_cached_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Memory address predicate
   // Identifies memory addresses in the Fabric.
   // (Caches needs this information to cache these addresses.)

   function Bool fn_is_mem_addr (Fabric_Addr addr);
      return (   fn_is_ddr4_0_cached_addr (addr)
	      );
   endfunction

   // ----------------------------------------------------------------
   // I/O address predicate
   // Identifies I/O addresses in the Fabric.
   // (Caches needs this information to avoid cacheing these addresses.)

   function Bool fn_is_IO_addr (Fabric_Addr addr);
      return (   fn_is_plic_addr (addr)
	      || fn_is_near_mem_io_addr (addr)
	   // || fn_is_pcie_ecam_slave_bridge_addr (addr)
	      || fn_is_quad_spi_0_mem_addr (addr)
	   // || fn_is_pcie_block_registers_addr (addr)
	      || fn_is_ethernet_0_addr (addr)
	      || fn_is_dma_0_addr (addr)
	      || fn_is_uart16550_0_addr  (addr)
`ifdef INCLUDE_ACCEL0
	      || fn_is_accel0_addr  (addr)
`endif
	      || fn_is_gpio0_addr (addr)
	      || fn_is_boot_rom_addr (addr)
	      || fn_is_ddr4_0_uncached_addr (addr)
	      );
   endfunction

   // ----------------------------------------------------------------
   // PC, MTVEC and NMIVEC reset values

   Bit #(64) pc_reset_value     = boot_rom_addr_base;
   Bit #(64) mtvec_reset_value  = 'h1000;    // TODO
   Bit #(64) nmivec_reset_value = ?;         // TODO

   // ================================================================
   // INTERFACE

   method  Fabric_Addr  m_plic_addr_base = plic_addr_base;
   method  Fabric_Addr  m_plic_addr_size = plic_addr_size;
   method  Fabric_Addr  m_plic_addr_lim  = plic_addr_lim;

   method  Fabric_Addr  m_near_mem_io_addr_base = near_mem_io_addr_base;
   method  Fabric_Addr  m_near_mem_io_addr_size = near_mem_io_addr_size;
   method  Fabric_Addr  m_near_mem_io_addr_lim  = near_mem_io_addr_lim;

   method  Fabric_Addr  m_quad_spi_0_mem_addr_base = quad_spi_0_mem_addr_base;
   method  Fabric_Addr  m_quad_spi_0_mem_addr_size = quad_spi_0_mem_addr_size;
   method  Fabric_Addr  m_quad_spi_0_mem_addr_lim  = quad_spi_0_mem_addr_lim;

   method  Fabric_Addr  m_quad_spi_0_reg_addr_base = quad_spi_0_reg_addr_base;
   method  Fabric_Addr  m_quad_spi_0_reg_addr_size = quad_spi_0_reg_addr_size;
   method  Fabric_Addr  m_quad_spi_0_reg_addr_lim  = quad_spi_0_reg_addr_lim;

   method  Fabric_Addr  m_ethernet_0_addr_base = ethernet_0_addr_base;
   method  Fabric_Addr  m_ethernet_0_addr_size = ethernet_0_addr_size;
   method  Fabric_Addr  m_ethernet_0_addr_lim  = ethernet_0_addr_lim;

   method  Fabric_Addr  m_dma_0_addr_base = dma_0_addr_base;
   method  Fabric_Addr  m_dma_0_addr_size = dma_0_addr_size;
   method  Fabric_Addr  m_dma_0_addr_lim  = dma_0_addr_lim;

   method  Fabric_Addr  m_uart16550_0_addr_base = uart16550_0_addr_base;
   method  Fabric_Addr  m_uart16550_0_addr_size = uart16550_0_addr_size;
   method  Fabric_Addr  m_uart16550_0_addr_lim  = uart16550_0_addr_lim;

`ifdef INCLUDE_ACCEL0
   method  Fabric_Addr  m_accel0_addr_base = accel0_addr_base;
   method  Fabric_Addr  m_accel0_addr_size = accel0_addr_size;
   method  Fabric_Addr  m_accel0_addr_lim  = accel0_addr_lim;
`endif

   method  Fabric_Addr  m_gpio0_addr_base = gpio0_addr_base;
   method  Fabric_Addr  m_gpio0_addr_size = gpio0_addr_size;
   method  Fabric_Addr  m_gpio0_addr_lim  = gpio0_addr_lim;

   method  Fabric_Addr  m_boot_rom_addr_base = boot_rom_addr_base;
   method  Fabric_Addr  m_boot_rom_addr_size = boot_rom_addr_size;
   method  Fabric_Addr  m_boot_rom_addr_lim  = boot_rom_addr_lim;

   method  Fabric_Addr  m_ddr4_0_uncached_addr_base = ddr4_0_uncached_addr_base;
   method  Fabric_Addr  m_ddr4_0_uncached_addr_size = ddr4_0_uncached_addr_size;
   method  Fabric_Addr  m_ddr4_0_uncached_addr_lim  = ddr4_0_uncached_addr_lim;

   method  Fabric_Addr  m_ddr4_0_cached_addr_base = ddr4_0_cached_addr_base;
   method  Fabric_Addr  m_ddr4_0_cached_addr_size = ddr4_0_cached_addr_size;
   method  Fabric_Addr  m_ddr4_0_cached_addr_lim  = ddr4_0_cached_addr_lim;

   method  Fabric_Addr  m_mem0_controller_addr_base = ddr4_0_cached_addr_base;
   method  Fabric_Addr  m_mem0_controller_addr_size = ddr4_0_cached_addr_size;
   method  Fabric_Addr  m_mem0_controller_addr_lim  = ddr4_0_cached_addr_lim;

   method  Bool  m_is_mem_addr (Fabric_Addr addr) = fn_is_mem_addr (addr);

   method  Bool  m_is_IO_addr (Fabric_Addr addr) = fn_is_IO_addr (addr);

   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr) = fn_is_near_mem_io_addr (addr);

   method  Bit #(64)  m_pc_reset_value     = pc_reset_value;
   method  Bit #(64)  m_mtvec_reset_value  = mtvec_reset_value;
   method  Bit #(64)  m_nmivec_reset_value = nmivec_reset_value;
endmodule

// ================================================================
// Count and master-numbers of masters in the fabric.

Integer imem_master_num   = 0;
Integer dmem_master_num   = 1;
Integer accel0_master_num = 2;

`ifdef INCLUDE_ACCEL0

typedef 3 Num_Masters;

`else

typedef 2 Num_Masters;

`endif

// ================================================================
// Count and slave-numbers of slaves in the fabric.

`ifdef INCLUDE_ACCEL0

typedef 7 Num_Slaves;

`else

typedef 6 Num_Slaves;

`endif

Integer boot_rom_slave_num        = 0;
Integer mem0_controller_slave_num = 1;
Integer uart0_slave_num           = 2;
Integer quad_spi_0_mem_slave_num  = 3;
Integer quad_spi_0_reg_slave_num  = 4;
Integer gpio0_slave_num           = 5;
Integer accel0_slave_num          = 6;

// ================================================================
// Interrupt request numbers (== index in to vector of
// interrupt-request lines in Core)

typedef  16  N_External_Interrupt_Sources;
Integer  n_external_interrupt_sources = valueOf (N_External_Interrupt_Sources);

Integer irq_num_uart16550_0  = 0;
Integer irq_num_quad_spi_0   = 1;
Integer irq_num_gpio0        = 2;
Integer irq_num_accel0       = 3;

// ================================================================

endpackage
