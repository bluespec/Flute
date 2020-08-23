// Copyright (c) 2013-2020 Bluespec, Inc. All Rights Reserved

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
// Exports

export  SoC_Map_IFC (..), mkSoC_Map;

// export  fn_addr_in_range;

export  Num_Masters;
export  imem_master_num;
export  dmem_master_num;
export  accel0_master_num;

export  Num_Slaves;
export  boot_rom_slave_num;
export  mem0_controller_slave_num;
export  uart0_slave_num;
export  accel0_slave_num;

export  N_External_Interrupt_Sources;
export  n_external_interrupt_sources;
export  irq_num_uart0;
export  irq_num_accel0;

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

   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_lim;

`ifdef INCLUDE_ACCEL0
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_lim;
`endif

   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_tcm_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_tcm_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_tcm_addr_lim;

   (* always_ready *)
   method  Bool  m_is_mem_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_IO_addr (Fabric_Addr addr);

   (* always_ready *)
   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr);

   (* always_ready *)   method  Bit #(64)  m_pc_reset_value;
   (* always_ready *)   method  Bit #(64)  m_mtvec_reset_value;

   // Non-maskable interrupt vector
   (* always_ready *)   method  Bit #(64)  m_nmivec_reset_value;
endinterface

// ================================================================

(* synthesize *)
module mkSoC_Map (SoC_Map_IFC);

   // ----------------------------------------------------------------
   // Near_Mem_IO (including CLINT, the core-local interruptor)

   Fabric_Addr near_mem_io_addr_base = 'h_0200_0000;
   Fabric_Addr near_mem_io_addr_size = 'h_0000_C000;    // 48K
   Fabric_Addr near_mem_io_addr_lim  = near_mem_io_addr_base + near_mem_io_addr_size;

   function Bool fn_is_near_mem_io_addr (Fabric_Addr addr);
      return ((near_mem_io_addr_base <= addr) && (addr < near_mem_io_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // PLIC

   Fabric_Addr plic_addr_base = 'h_0C00_0000;
   Fabric_Addr plic_addr_size = 'h_0040_0000;    // 4M
   Fabric_Addr plic_addr_lim  = plic_addr_base + plic_addr_size;

   function Bool fn_is_plic_addr (Fabric_Addr addr);
      return ((plic_addr_base <= addr) && (addr < plic_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // UART 0

   Fabric_Addr uart0_addr_base = 'hC000_0000;
   Fabric_Addr uart0_addr_size = 'h0000_0080;    // 128
   Fabric_Addr uart0_addr_lim  = uart0_addr_base + uart0_addr_size;

   function Bool fn_is_uart0_addr (Fabric_Addr addr);
      return ((uart0_addr_base <= addr) && (addr < uart0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // ACCEL 0

`ifdef INCLUDE_ACCEL0
   Fabric_Addr accel0_addr_base = 'hC000_2000;
   Fabric_Addr accel0_addr_size = 'h0000_1000;    // 4K
   Fabric_Addr accel0_addr_lim  = accel0_addr_base + accel0_addr_size;

   function Bool fn_is_accel0_addr (Fabric_Addr addr);
      return ((accel0_addr_base <= addr) && (addr < accel0_addr_lim));
   endfunction
`endif

   // ----------------------------------------------------------------
   // Boot ROM

   Fabric_Addr boot_rom_addr_base = 'h_0000_1000;
   Fabric_Addr boot_rom_addr_size = 'h_0000_1000;    // 4K
   Fabric_Addr boot_rom_addr_lim  = boot_rom_addr_base + boot_rom_addr_size;

   function Bool fn_is_boot_rom_addr (Fabric_Addr addr);
      return ((boot_rom_addr_base <= addr) && (addr < boot_rom_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Main Mem Controller 0

   Fabric_Addr mem0_controller_addr_base = 'h_8000_0000;
   Fabric_Addr mem0_controller_addr_size = 'h_1000_0000;    // 256 MB
   Fabric_Addr mem0_controller_addr_lim  = mem0_controller_addr_base + mem0_controller_addr_size;

   function Bool fn_is_mem0_controller_addr (Fabric_Addr addr);
      return ((mem0_controller_addr_base <= addr) && (addr < mem0_controller_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Tightly-coupled memory ('TCM'; optional)

`ifdef Near_Mem_TCM
// Integer kB_per_TCM = 'h4;         // 4KB
// Integer kB_per_TCM = 'h40;     // 64KB
// Integer kB_per_TCM = 'h80;     // 128KB
// Integer kB_per_TCM = 'h400;    // 1 MB
   Integer kB_per_TCM = 'h4000;    // 16 MB
`else
   Integer kB_per_TCM = 0;
`endif
   Integer bytes_per_TCM = kB_per_TCM * 'h400;

   Fabric_Addr tcm_addr_base = 'h_0000_0000;
   Fabric_Addr tcm_addr_size = fromInteger (bytes_per_TCM);
   Fabric_Addr tcm_addr_lim  = tcm_addr_base + tcm_addr_size;

   function Bool fn_is_tcm_addr (Fabric_Addr addr);
      return ((tcm_addr_base <= addr) && (addr < tcm_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Memory address predicate
   // Identifies memory addresses in the Fabric.
   // (Caches need this information to cache these addresses.)

   function Bool fn_is_mem_addr (Fabric_Addr addr);
      return (   fn_is_boot_rom_addr (addr)
	      || fn_is_mem0_controller_addr (addr)
	      || fn_is_tcm_addr (addr)
	      );
   endfunction

   // ----------------------------------------------------------------
   // I/O address predicate
   // Identifies I/O addresses in the Fabric.
   // (Caches need this information to avoid cacheing these addresses.)

   function Bool fn_is_IO_addr (Fabric_Addr addr);
      return (   fn_is_near_mem_io_addr (addr)
	      || fn_is_plic_addr (addr)
	      || fn_is_uart0_addr  (addr)
`ifdef INCLUDE_ACCEL0
	      || fn_is_accel0_addr  (addr)
`endif
	      );
   endfunction

   // ----------------------------------------------------------------
   // PC, MTVEC and NMIVEC reset values

   Bit #(64) pc_reset_value     = boot_rom_addr_base;
   Bit #(64) mtvec_reset_value  = 'h1000;    // TODO

   // Non-maskable interrupt vector
   Bit #(64) nmivec_reset_value = ?;         // TODO

   // ================================================================
   // INTERFACE

   method  Fabric_Addr  m_near_mem_io_addr_base = near_mem_io_addr_base;
   method  Fabric_Addr  m_near_mem_io_addr_size = near_mem_io_addr_size;
   method  Fabric_Addr  m_near_mem_io_addr_lim  = near_mem_io_addr_lim;

   method  Fabric_Addr  m_plic_addr_base = plic_addr_base;
   method  Fabric_Addr  m_plic_addr_size = plic_addr_size;
   method  Fabric_Addr  m_plic_addr_lim  = plic_addr_lim;

   method  Fabric_Addr  m_uart0_addr_base = uart0_addr_base;
   method  Fabric_Addr  m_uart0_addr_size = uart0_addr_size;
   method  Fabric_Addr  m_uart0_addr_lim  = uart0_addr_lim;

`ifdef INCLUDE_ACCEL0
   method  Fabric_Addr  m_accel0_addr_base = accel0_addr_base;
   method  Fabric_Addr  m_accel0_addr_size = accel0_addr_size;
   method  Fabric_Addr  m_accel0_addr_lim  = accel0_addr_lim;
`endif

   method  Fabric_Addr  m_boot_rom_addr_base = boot_rom_addr_base;
   method  Fabric_Addr  m_boot_rom_addr_size = boot_rom_addr_size;
   method  Fabric_Addr  m_boot_rom_addr_lim  = boot_rom_addr_lim;

   method  Fabric_Addr  m_mem0_controller_addr_base = mem0_controller_addr_base;
   method  Fabric_Addr  m_mem0_controller_addr_size = mem0_controller_addr_size;
   method  Fabric_Addr  m_mem0_controller_addr_lim  = mem0_controller_addr_lim;

   method  Fabric_Addr  m_tcm_addr_base = tcm_addr_base;
   method  Fabric_Addr  m_tcm_addr_size = tcm_addr_size;
   method  Fabric_Addr  m_tcm_addr_lim  = tcm_addr_lim;

   method  Bool  m_is_mem_addr (Fabric_Addr addr) = fn_is_mem_addr (addr);

   method  Bool  m_is_IO_addr (Fabric_Addr addr) = fn_is_IO_addr (addr);

   method  Bool  m_is_near_mem_IO_addr (Fabric_Addr addr) = fn_is_near_mem_io_addr (addr);

   method  Bit #(64)  m_pc_reset_value     = pc_reset_value;
   method  Bit #(64)  m_mtvec_reset_value  = mtvec_reset_value;

   // Non-maskable interrupt vector
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

typedef 4 Num_Slaves;

`else

typedef 3 Num_Slaves;

`endif


Integer boot_rom_slave_num        = 0;
Integer mem0_controller_slave_num = 1;
Integer uart0_slave_num           = 2;
Integer accel0_slave_num          = 3;

// ================================================================
// Interrupt request numbers (== index in to vector of
// interrupt-request lines in Core)

typedef  16  N_External_Interrupt_Sources;
Integer  n_external_interrupt_sources = valueOf (N_External_Interrupt_Sources);

Integer irq_num_uart0  = 0;
Integer irq_num_accel0 = 1;

// ================================================================

endpackage
