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

// ================================================================
// Bluespec library imports

// None

// ================================================================
// Project imports

import Fabric_Defs :: *;    // Only for type Fabric_Addr

// ================================================================
// Interface and module for the address map

interface SoC_Map_IFC;
   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_plic_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_plic_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_plic_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_pcie_ecam_slave_bridge_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_pcie_ecam_slave_bridge_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_pcie_ecam_slave_bridge_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_flash_mem_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_flash_mem_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_flash_mem_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_pcie_block_registers_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_pcie_block_registers_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_pcie_block_registers_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_ethernet0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_ethernet0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_ethernet0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_dma0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_dma0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_dma0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_ddr4_0_addr_lim;

   (* always_ready *)
   method  Bool  m_is_mem_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_IO_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr);

   (* always_ready *)   method  Bit #(64)  m_pc_reset_value;

   (* always_ready *)   method  Bit #(64)  m_nmi_vector;
endinterface

// ================================================================

(* synthesize *)
module mkSoC_Map (SoC_Map_IFC);

   // ----------------------------------------------------------------
   // Near_Mem_IO (CLINT)

   Fabric_Addr near_mem_io_addr_base = 'h_1000_0000;
   Fabric_Addr near_mem_io_addr_size = 'h_0001_0000;    // 64K
   Fabric_Addr near_mem_io_addr_lim  = near_mem_io_addr_base + near_mem_io_addr_size;

   function Bool fn_is_near_mem_io_addr (Fabric_Addr addr);
      return ((near_mem_io_addr_base <= addr) && (addr < near_mem_io_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // PLIC

   Fabric_Addr plic_addr_base = 'h_1100_0000;
   Fabric_Addr plic_addr_size = 'h_0010_0000;    // 1M
   Fabric_Addr plic_addr_lim  = plic_addr_base + plic_addr_size;

   function Bool fn_is_plic_addr (Fabric_Addr addr);
      return ((plic_addr_base <= addr) && (addr < plic_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // PCIE_ECAM_SLAVE_BRIDGE

   Fabric_Addr pcie_ecam_slave_bridge_addr_base = 'h_2000_0000;
   Fabric_Addr pcie_ecam_slave_bridge_addr_size = 'h_2000_0000;    // 512M
   Fabric_Addr pcie_ecam_slave_bridge_addr_lim  = (  pcie_ecam_slave_bridge_addr_base
						   + pcie_ecam_slave_bridge_addr_size);

   function Bool fn_is_pcie_ecam_slave_bridge_addr (Fabric_Addr addr);
      return ((pcie_ecam_slave_bridge_addr_base <= addr) && (addr < pcie_ecam_slave_bridge_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Flash Mem

   Fabric_Addr flash_mem_addr_base = 'h_4000_0000;
   Fabric_Addr flash_mem_addr_size = 'h_1000_0000;    // 256M
   Fabric_Addr flash_mem_addr_lim  = flash_mem_addr_base + flash_mem_addr_size;

   function Bool fn_is_flash_mem_addr (Fabric_Addr addr);
      return ((flash_mem_addr_base <= addr) && (addr < flash_mem_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // PCIe Block Registers

   Fabric_Addr pcie_block_registers_addr_base = 'h_6000_0000;
   Fabric_Addr pcie_block_registers_addr_size = 'h_0001_0000;    // 64K
   Fabric_Addr pcie_block_registers_addr_lim  = ( pcie_block_registers_addr_base
						 + pcie_block_registers_addr_size);

   function Bool fn_is_pcie_block_registers_addr (Fabric_Addr addr);
      return ((pcie_block_registers_addr_base <= addr) && (addr < pcie_block_registers_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Ethernet 0

   Fabric_Addr ethernet0_addr_base = 'h_6100_0000;
   Fabric_Addr ethernet0_addr_size = 'h_0004_0000;    // 256K
   Fabric_Addr ethernet0_addr_lim  = ethernet0_addr_base + ethernet0_addr_size;

   function Bool fn_is_ethernet0_addr (Fabric_Addr addr);
      return ((ethernet0_addr_base <= addr) && (addr < ethernet0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // DMA 0

   Fabric_Addr dma0_addr_base = 'h_6200_0000;
   Fabric_Addr dma0_addr_size = 'h_0001_0000;    // 64K
   Fabric_Addr dma0_addr_lim  = dma0_addr_base + dma0_addr_size;

   function Bool fn_is_dma0_addr (Fabric_Addr addr);
      return ((dma0_addr_base <= addr) && (addr < dma0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // UART 0

   Fabric_Addr uart0_addr_base = 'h_6230_0000;
   Fabric_Addr uart0_addr_size = 'h_0000_1000;    // 4K
   Fabric_Addr uart0_addr_lim  = uart0_addr_base + uart0_addr_size;

   function Bool fn_is_uart0_addr (Fabric_Addr addr);
      return ((uart0_addr_base <= addr) && (addr < uart0_addr_lim));
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
   // DDR memory 0

   Fabric_Addr ddr4_0_addr_base = 'h_8000_0000;
   Fabric_Addr ddr4_0_addr_size = 'h_8000_0000;    // 2G
   Fabric_Addr ddr4_0_addr_lim  = ddr4_0_addr_base + ddr4_0_addr_size;

   function Bool fn_is_ddr4_0_addr (Fabric_Addr addr);
      return ((ddr4_0_addr_base <= addr) && (addr < ddr4_0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Memory address predicate
   // Identifies memory addresses in the Fabric.
   // (Caches needs this information to cache these addresses.)

   function Bool fn_is_mem_addr (Fabric_Addr addr);
      return (   fn_is_ddr4_0_addr (addr)
	      );
   endfunction

   // ----------------------------------------------------------------
   // I/O address predicate
   // Identifies I/O addresses in the Fabric.
   // (Caches needs this information to avoid cacheing these addresses.)

   function Bool fn_is_IO_addr (Fabric_Addr addr);
      return (   fn_is_near_mem_io_addr (addr)
	      || fn_is_plic_addr (addr)
	      || fn_is_pcie_ecam_slave_bridge_addr (addr)
	      || fn_is_flash_mem_addr (addr)
	      || fn_is_pcie_block_registers_addr (addr)
	      || fn_is_ethernet0_addr (addr)
	      || fn_is_dma0_addr (addr)
	      || fn_is_uart0_addr  (addr)
	      || fn_is_boot_rom_addr (addr)
	      );
   endfunction

   // ----------------------------------------------------------------
   // PC reset value

   Bit #(64) pc_reset_value = boot_rom_addr_base;

   // ----------------------------------------------------------------
   // Non-maskable Interrupt vector

   Bit #(64) nmi_vector = ?;    // TODO

   // ================================================================
   // INTERFACE

   method  Fabric_Addr  m_near_mem_io_addr_base = near_mem_io_addr_base;
   method  Fabric_Addr  m_near_mem_io_addr_size = near_mem_io_addr_size;
   method  Fabric_Addr  m_near_mem_io_addr_lim  = near_mem_io_addr_lim;

   method  Fabric_Addr  m_plic_addr_base = plic_addr_base;
   method  Fabric_Addr  m_plic_addr_size = plic_addr_size;
   method  Fabric_Addr  m_plic_addr_lim  = plic_addr_lim;

   method  Fabric_Addr  m_pcie_ecam_slave_bridge_addr_base = pcie_ecam_slave_bridge_addr_base;
   method  Fabric_Addr  m_pcie_ecam_slave_bridge_addr_size = pcie_ecam_slave_bridge_addr_size;
   method  Fabric_Addr  m_pcie_ecam_slave_bridge_addr_lim  = pcie_ecam_slave_bridge_addr_lim;

   method  Fabric_Addr  m_flash_mem_addr_base = flash_mem_addr_base;
   method  Fabric_Addr  m_flash_mem_addr_size = flash_mem_addr_size;
   method  Fabric_Addr  m_flash_mem_addr_lim  = flash_mem_addr_lim;

   method  Fabric_Addr  m_pcie_block_registers_addr_base = pcie_block_registers_addr_base;
   method  Fabric_Addr  m_pcie_block_registers_addr_size = pcie_block_registers_addr_size;
   method  Fabric_Addr  m_pcie_block_registers_addr_lim  = pcie_block_registers_addr_lim;

   method  Fabric_Addr  m_ethernet0_addr_base = ethernet0_addr_base;
   method  Fabric_Addr  m_ethernet0_addr_size = ethernet0_addr_size;
   method  Fabric_Addr  m_ethernet0_addr_lim  = ethernet0_addr_lim;

   method  Fabric_Addr  m_dma0_addr_base = dma0_addr_base;
   method  Fabric_Addr  m_dma0_addr_size = dma0_addr_size;
   method  Fabric_Addr  m_dma0_addr_lim  = dma0_addr_lim;

   method  Fabric_Addr  m_uart0_addr_base = uart0_addr_base;
   method  Fabric_Addr  m_uart0_addr_size = uart0_addr_size;
   method  Fabric_Addr  m_uart0_addr_lim  = uart0_addr_lim;

   method  Fabric_Addr  m_boot_rom_addr_base = boot_rom_addr_base;
   method  Fabric_Addr  m_boot_rom_addr_size = boot_rom_addr_size;
   method  Fabric_Addr  m_boot_rom_addr_lim  = boot_rom_addr_lim;

   method  Fabric_Addr  m_ddr4_0_addr_base = ddr4_0_addr_base;
   method  Fabric_Addr  m_ddr4_0_addr_size = ddr4_0_addr_size;
   method  Fabric_Addr  m_ddr4_0_addr_lim  = ddr4_0_addr_lim;

   method  Bool  m_is_mem_addr (Fabric_Addr addr) = fn_is_mem_addr (addr);

   method  Bool  m_is_IO_addr (Fabric_Addr addr) = fn_is_IO_addr (addr);

   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr) = fn_is_near_mem_io_addr (addr);

   method  Bit #(64)  m_pc_reset_value = pc_reset_value;

   method  Bit #(64)  m_nmi_vector     = nmi_vector;
endmodule

// ================================================================

endpackage
