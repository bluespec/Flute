// THIS IS A PROGRAM-GENERATED FILE; DO NOT EDIT!
// See: https://github.com/rsnikhil/Misc_Utilities    /Generate_SoC_Map

package SoC_Map;

// ================================================================
// This module defines the overall 'address map' of the SoC, showing
// the addresses serviced by each server IP, and which addresses are
// memory vs. I/O., etc.

// ***** WARNING! WARNING! WARNING! *****

// During system integration, this address map should be identical to
// the system interconnect settings (e.g., routing of requests between
// clients and servers).  This map is also needed by software so that
// it knows how to address various IPs.

// This module contains no state; it just has constants, and so can be
// freely instantiated at multiple places in the SoC module hierarchy
// at no hardware cost.  It allows this map to be defined in one
// place and shared across the SoC.

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
// Interface for the address map module

interface SoC_Map_IFC;
   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_boot_rom_addr_base;
   (* always_ready *) method Fabric_Addr  m_boot_rom_addr_size;
   (* always_ready *) method Fabric_Addr  m_boot_rom_addr_lim;

   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_near_mem_io_addr_base;
   (* always_ready *) method Fabric_Addr  m_near_mem_io_addr_size;
   (* always_ready *) method Fabric_Addr  m_near_mem_io_addr_lim;

   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_plic_addr_base;
   (* always_ready *) method Fabric_Addr  m_plic_addr_size;
   (* always_ready *) method Fabric_Addr  m_plic_addr_lim;

   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_host_access_addr_base;
   (* always_ready *) method Fabric_Addr  m_host_access_addr_size;
   (* always_ready *) method Fabric_Addr  m_host_access_addr_lim;

   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_uart16550_0_addr_base;
   (* always_ready *) method Fabric_Addr  m_uart16550_0_addr_size;
   (* always_ready *) method Fabric_Addr  m_uart16550_0_addr_lim;

   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_msi_addr_base;
   (* always_ready *) method Fabric_Addr  m_msi_addr_size;
   (* always_ready *) method Fabric_Addr  m_msi_addr_lim;

   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_gpio_addr_base;
   (* always_ready *) method Fabric_Addr  m_gpio_addr_size;
   (* always_ready *) method Fabric_Addr  m_gpio_addr_lim;

   // ---------------- IO region
   (* always_ready *) method Fabric_Addr  m_sram_addr_base;
   (* always_ready *) method Fabric_Addr  m_sram_addr_size;
   (* always_ready *) method Fabric_Addr  m_sram_addr_lim;

   // ---------------- MEM region
   (* always_ready *) method Fabric_Addr  m_ddr4_0_cached_addr_base;
   (* always_ready *) method Fabric_Addr  m_ddr4_0_cached_addr_size;
   (* always_ready *) method Fabric_Addr  m_ddr4_0_cached_addr_lim;

   // ---------------- Predicates ----------------
   (* always_ready *)
   method  Bool  m_is_mem_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_IO_addr (Fabric_Addr addr);

   // ---------------- Constants ----------------
   (* always_ready *)  method  Bit #(64)  m_pc_reset_value;
   (* always_ready *)  method  Bit #(64)  m_mtvec_reset_value;
   (* always_ready *)  method  Bit #(64)  m_nmivec_reset_value;
endinterface

// ================================================================
// The address map module

module mkSoC_Map (SoC_Map_IFC);

   messageM ("\nINFO: SoC Map generated from spec file: SoC_Map_Spec_Catamaran.txt");

   // ---------------- IO region
   Fabric_Addr boot_rom_addr_base = 'h_0001_0000;
   Fabric_Addr boot_rom_addr_size = 'h_0001_0000;
   Fabric_Addr boot_rom_addr_lim  = boot_rom_addr_base + boot_rom_addr_size;

   function Bool fn_is_boot_rom_addr (Fabric_Addr addr);
      return ((boot_rom_addr_base <= addr) && (addr < boot_rom_addr_lim));
   endfunction

   // ---------------- IO region
   Fabric_Addr near_mem_io_addr_base = 'h_0200_0000;
   Fabric_Addr near_mem_io_addr_size = 'h_0001_0000;
   Fabric_Addr near_mem_io_addr_lim  = near_mem_io_addr_base + near_mem_io_addr_size;

   function Bool fn_is_near_mem_io_addr (Fabric_Addr addr);
      return ((near_mem_io_addr_base <= addr) && (addr < near_mem_io_addr_lim));
   endfunction

   // ---------------- IO region
   Fabric_Addr plic_addr_base = 'h_0c00_0000;
   Fabric_Addr plic_addr_size = 'h_0400_0000;
   Fabric_Addr plic_addr_lim  = plic_addr_base + plic_addr_size;

   function Bool fn_is_plic_addr (Fabric_Addr addr);
      return ((plic_addr_base <= addr) && (addr < plic_addr_lim));
   endfunction

   // ---------------- IO region
   Fabric_Addr host_access_addr_base = 'h_4000_0000;
   Fabric_Addr host_access_addr_size = 'h_0000_4000;
   Fabric_Addr host_access_addr_lim  = host_access_addr_base + host_access_addr_size;

   function Bool fn_is_host_access_addr (Fabric_Addr addr);
      return ((host_access_addr_base <= addr) && (addr < host_access_addr_lim));
   endfunction

   // ---------------- IO region
   Fabric_Addr uart16550_0_addr_base = 'h_6010_0000;
   Fabric_Addr uart16550_0_addr_size = 'h_0000_1000;
   Fabric_Addr uart16550_0_addr_lim  = uart16550_0_addr_base + uart16550_0_addr_size;

   function Bool fn_is_uart16550_0_addr (Fabric_Addr addr);
      return ((uart16550_0_addr_base <= addr) && (addr < uart16550_0_addr_lim));
   endfunction

   // ---------------- IO region
   Fabric_Addr msi_addr_base = 'h_6020_0000;
   Fabric_Addr msi_addr_size = 'h_0000_0004;
   Fabric_Addr msi_addr_lim  = msi_addr_base + msi_addr_size;

   function Bool fn_is_msi_addr (Fabric_Addr addr);
      return ((msi_addr_base <= addr) && (addr < msi_addr_lim));
   endfunction

   // ---------------- IO region
   Fabric_Addr gpio_addr_base = 'h_6fff_0000;
   Fabric_Addr gpio_addr_size = 'h_0001_0000;
   Fabric_Addr gpio_addr_lim  = gpio_addr_base + gpio_addr_size;

   function Bool fn_is_gpio_addr (Fabric_Addr addr);
      return ((gpio_addr_base <= addr) && (addr < gpio_addr_lim));
   endfunction

   // ---------------- IO region
   Fabric_Addr sram_addr_base = 'h_7000_0000;
   Fabric_Addr sram_addr_size = 'h_0040_0000;
   Fabric_Addr sram_addr_lim  = sram_addr_base + sram_addr_size;

   function Bool fn_is_sram_addr (Fabric_Addr addr);
      return ((sram_addr_base <= addr) && (addr < sram_addr_lim));
   endfunction

   // ---------------- MEM region
   Fabric_Addr ddr4_0_cached_addr_base = 'h_8000_0000;
   Fabric_Addr ddr4_0_cached_addr_size = 'h_8000_0000;
   Fabric_Addr ddr4_0_cached_addr_lim  = ddr4_0_cached_addr_base + ddr4_0_cached_addr_size;

   function Bool fn_is_ddr4_0_cached_addr (Fabric_Addr addr);
      return ((ddr4_0_cached_addr_base <= addr) && (addr < ddr4_0_cached_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Memory address predicate

   function Bool fn_is_mem_addr (Fabric_Addr addr);
      return (   False
              || fn_is_ddr4_0_cached_addr (addr)
      );
   endfunction

   // ----------------------------------------------------------------
   // IO address predicate

   function Bool fn_is_IO_addr (Fabric_Addr addr);
      return (   False
              || fn_is_boot_rom_addr (addr)
              || fn_is_near_mem_io_addr (addr)
              || fn_is_plic_addr (addr)
              || fn_is_host_access_addr (addr)
              || fn_is_uart16550_0_addr (addr)
              || fn_is_msi_addr (addr)
              || fn_is_gpio_addr (addr)
              || fn_is_sram_addr (addr)
      );
   endfunction

   // ----------------------------------------------------------------
   // Constants

   Bit #(64) pc_reset_value = 'h_8000_0000;
   Bit #(64) mtvec_reset_value = 'h_0000_1000;
   Bit #(64) nmivec_reset_value = 'h_0000_0000;

   // ----------------------------------------------------------------
   // INTERFACE

   method Fabric_Addr m_boot_rom_addr_base = boot_rom_addr_base;
   method Fabric_Addr m_boot_rom_addr_size = boot_rom_addr_size;
   method Fabric_Addr m_boot_rom_addr_lim  = boot_rom_addr_lim;

   method Fabric_Addr m_near_mem_io_addr_base = near_mem_io_addr_base;
   method Fabric_Addr m_near_mem_io_addr_size = near_mem_io_addr_size;
   method Fabric_Addr m_near_mem_io_addr_lim  = near_mem_io_addr_lim;

   method Fabric_Addr m_plic_addr_base = plic_addr_base;
   method Fabric_Addr m_plic_addr_size = plic_addr_size;
   method Fabric_Addr m_plic_addr_lim  = plic_addr_lim;

   method Fabric_Addr m_host_access_addr_base = host_access_addr_base;
   method Fabric_Addr m_host_access_addr_size = host_access_addr_size;
   method Fabric_Addr m_host_access_addr_lim  = host_access_addr_lim;

   method Fabric_Addr m_uart16550_0_addr_base = uart16550_0_addr_base;
   method Fabric_Addr m_uart16550_0_addr_size = uart16550_0_addr_size;
   method Fabric_Addr m_uart16550_0_addr_lim  = uart16550_0_addr_lim;

   method Fabric_Addr m_msi_addr_base = msi_addr_base;
   method Fabric_Addr m_msi_addr_size = msi_addr_size;
   method Fabric_Addr m_msi_addr_lim  = msi_addr_lim;

   method Fabric_Addr m_gpio_addr_base = gpio_addr_base;
   method Fabric_Addr m_gpio_addr_size = gpio_addr_size;
   method Fabric_Addr m_gpio_addr_lim  = gpio_addr_lim;

   method Fabric_Addr m_sram_addr_base = sram_addr_base;
   method Fabric_Addr m_sram_addr_size = sram_addr_size;
   method Fabric_Addr m_sram_addr_lim  = sram_addr_lim;

   method Fabric_Addr m_ddr4_0_cached_addr_base = ddr4_0_cached_addr_base;
   method Fabric_Addr m_ddr4_0_cached_addr_size = ddr4_0_cached_addr_size;
   method Fabric_Addr m_ddr4_0_cached_addr_lim  = ddr4_0_cached_addr_lim;

   method  Bool  m_is_mem_addr (Fabric_Addr addr) = fn_is_mem_addr (addr);
   method  Bool  m_is_IO_addr (Fabric_Addr addr) = fn_is_IO_addr (addr);

   method  Bit #(64)  m_pc_reset_value = pc_reset_value;
   method  Bit #(64)  m_mtvec_reset_value = mtvec_reset_value;
   method  Bit #(64)  m_nmivec_reset_value = nmivec_reset_value;
endmodule

// ================================================================

endpackage
