// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved

package SoC_Map;

// ================================================================
// This package defines the overall 'map' of the SoC:
//  - Features of the central fabric interconnecting all master and
//      slave IPs (fabric payload field widths, # masters, # slaves,
//      address-decode function mapping address to slave-number
//  - The memory map showing the addresses serviced by each slave IP,
//        and which addresses are memory vs. I/O
// and related utilities.

// Note: This map should be made available to software.

// This package should be edited if we change the map in any way
// (add/remove IPs, choose different addrs or sizes, etc.).

// ================================================================
// Bluespec library imports

// None

// ================================================================
// Project imports

import Fabric_Defs      :: *;

// ================================================================
// Exports

export  SoC_Map_IFC (..), mkSoC_Map;

export  fn_addr_in_range;

export  bytes_per_TCM;

export  Num_Masters;
export  debug_module_master_num;
export  imem_master_num;
export  dmem_master_num;

export  Num_Slaves;
export  boot_rom_slave_num;
export  tcm_back_door_slave_num;
export  mem0_controller_slave_num;
export  uart0_slave_num;
export  timer0_slave_num;

`ifdef INCLUDE_ACCEL0
export  accel0_master_num;
export  accel0_slave_num;
`endif

`ifdef HTIF_MEMORY
export  bytes_per_htif;
export  htif_slave_num;
`endif

// ================================================================
// Local definitions

// ----------------
// Boot ROM

Integer bytes_per_boot_rom = 'h1000;    // 4096

// ----------------
// TCM

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

// ----------------
// Memory

Integer mB_per_mem0    = 'h100;                      // 256 MB
Integer kB_per_mem0    = mB_per_mem0 * 'h400;
Integer bytes_per_mem0 = kB_per_mem0 * 'h400;

// ----------------
// UART0

Integer bytes_per_uart0 = 'h80;

// ----------------
// TIMER0

Integer bytes_per_timer0 = 'hc000;

// ----------------
// ACCEL0

`ifdef INCLUDE_ACCEL0
Integer bytes_per_accel0 = 'h40;
`endif

`ifdef HTIF_MEMORY
Integer bytes_per_htif = 128;
`endif

// ================================================================
// Interface and module for the address map

interface SoC_Map_IFC;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_boot_rom_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_tcm_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_tcm_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_tcm_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_mem0_controller_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_timer0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_timer0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_timer0_addr_lim;

`ifdef INCLUDE_ACCEL0
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_accel0_addr_lim;
`endif

`ifdef HTIF_MEMORY
   (* always_ready *)   method  Fabric_Addr  m_htif_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_htif_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_htif_addr_lim;
`endif

   (* always_ready *)
   method  Bool  m_is_IO_addr (Fabric_Addr addr);
endinterface

// ----------------

(* synthesize *)
module mkSoC_Map (SoC_Map_IFC);

   // ----------------------------------------------------------------
   // Boot ROM

   Fabric_Addr boot_rom_addr_size = fromInteger (bytes_per_boot_rom);
   Fabric_Addr boot_rom_addr_base = 'h_0000_1000;
   Fabric_Addr boot_rom_addr_lim  = boot_rom_addr_base + boot_rom_addr_size;

   function Bool is_boot_rom_addr (Fabric_Addr addr);
      return ((boot_rom_addr_base <= addr) && (addr < boot_rom_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Tightly-coupled memory (optional)

   Fabric_Addr tcm_addr_size = fromInteger (bytes_per_TCM);
   Fabric_Addr tcm_addr_base = 0;
   Fabric_Addr tcm_addr_lim  = tcm_addr_base + tcm_addr_size;

   function Bool is_tcm_addr (Fabric_Addr addr);
      return ((tcm_addr_base <= addr) && (addr < tcm_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Main Mem Controller 0

   Fabric_Addr mem0_controller_addr_size = fromInteger (bytes_per_mem0);
   Fabric_Addr mem0_controller_addr_base = 'h_8000_0000;
   Fabric_Addr mem0_controller_addr_lim  = mem0_controller_addr_base + mem0_controller_addr_size;

   function Bool is_mem0_controller_addr (Fabric_Addr addr);
      return ((mem0_controller_addr_base <= addr) && (addr < mem0_controller_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // UART 0

   Fabric_Addr uart0_addr_size = fromInteger (bytes_per_uart0);
   Fabric_Addr uart0_addr_base = 'hC000_0000;
   Fabric_Addr uart0_addr_lim  = uart0_addr_base + uart0_addr_size;

   function Bool is_uart0_addr (Fabric_Addr addr);
      return ((uart0_addr_base <= addr) && (addr < uart0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Timer 0

   Fabric_Addr timer0_addr_size = fromInteger (bytes_per_timer0);
   Fabric_Addr timer0_addr_base = 'h_0200_0000;
   Fabric_Addr timer0_addr_lim  = timer0_addr_base + timer0_addr_size;

   function Bool is_timer0_addr (Fabric_Addr addr);
      return ((timer0_addr_base <= addr) && (addr < timer0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Accel 0

`ifdef INCLUDE_ACCEL0
   Fabric_Addr accel0_addr_size = fromInteger (bytes_per_accel0);
   Fabric_Addr accel0_addr_base = 'hC000_0200;
   Fabric_Addr accel0_addr_lim  = accel0_addr_base + accel0_addr_size;

   function Bool is_accel0_addr (Fabric_Addr addr);
      return ((accel0_addr_base <= addr) && (addr < accel0_addr_lim));
   endfunction
`endif

`ifdef HTIF_MEMORY
   Fabric_Addr htif_addr_size = fromInteger (bytes_per_htif);
   Fabric_Addr htif_addr_base = 'h_6000_0000;
   Fabric_Addr htif_addr_lim  = htif_addr_base + htif_addr_size;

   function Bool is_htif_addr (Fabric_Addr addr);
      return ((htif_addr_base <= addr) && (addr < htif_addr_lim));
   endfunction
`endif

   // ----------------------------------------------------------------
   // I/O address predicate
   // Identifies I/O addresses in the Fabric.
   // (Caches needs this information to avoid cacheing these addresses.)

   function Bool fn_is_IO_addr (Fabric_Addr addr);
      return (   is_uart0_addr  (addr)
	      || is_timer0_addr (addr)
`ifdef INCLUDE_ACCEL0
	      || is_accel0_addr (addr)
`endif
`ifdef HTIF_MEMORY
	      || is_htif_addr (addr)
`endif
	      );
   endfunction

   // ----------------------------------------------------------------
   // INTERFACE

   method  Fabric_Addr  m_boot_rom_addr_size = boot_rom_addr_size;
   method  Fabric_Addr  m_boot_rom_addr_base = boot_rom_addr_base;
   method  Fabric_Addr  m_boot_rom_addr_lim  = boot_rom_addr_lim;

   method  Fabric_Addr  m_tcm_addr_size = tcm_addr_size;
   method  Fabric_Addr  m_tcm_addr_base = tcm_addr_base;
   method  Fabric_Addr  m_tcm_addr_lim  = tcm_addr_lim;

   method  Fabric_Addr  m_mem0_controller_addr_size = mem0_controller_addr_size;
   method  Fabric_Addr  m_mem0_controller_addr_base = mem0_controller_addr_base;
   method  Fabric_Addr  m_mem0_controller_addr_lim  = mem0_controller_addr_lim;

   method  Fabric_Addr  m_uart0_addr_size = uart0_addr_size;
   method  Fabric_Addr  m_uart0_addr_base = uart0_addr_base;
   method  Fabric_Addr  m_uart0_addr_lim  = uart0_addr_lim;

   method  Fabric_Addr  m_timer0_addr_size = timer0_addr_size;
   method  Fabric_Addr  m_timer0_addr_base = timer0_addr_base;
   method  Fabric_Addr  m_timer0_addr_lim  = timer0_addr_lim;

`ifdef INCLUDE_ACCEL0
   method  Fabric_Addr  m_accel0_addr_size = accel0_addr_size;
   method  Fabric_Addr  m_accel0_addr_base = accel0_addr_base;
   method  Fabric_Addr  m_accel0_addr_lim  = accel0_addr_lim;
`endif

`ifdef HTIF_MEMORY
   method  Fabric_Addr  m_htif_addr_size = htif_addr_size;
   method  Fabric_Addr  m_htif_addr_base = htif_addr_base;
   method  Fabric_Addr  m_htif_addr_lim  = htif_addr_lim;
`endif

   method  Bool  m_is_IO_addr (Fabric_Addr addr) = fn_is_IO_addr (addr);
endmodule

// ================================================================

function Bool fn_addr_in_range (Fabric_Addr base, Fabric_Addr addr, Fabric_Addr lim);
   return ((base <= addr) && (addr < lim));
endfunction

// ================================================================
// Count and master-numbers of masters in the fabric.

`ifdef INCLUDE_ACCEL0
typedef 4 Num_Masters;
`else
typedef 3 Num_Masters;
`endif

Integer debug_module_master_num = 0;
Integer imem_master_num         = 1;
Integer dmem_master_num         = 2;

`ifdef INCLUDE_ACCEL0
Integer accel0_master_num       = 3;
`endif

// ================================================================
// Count and slave-numbers of slaves in the fabric.

`ifdef HTIF_MEMORY
`ifdef INCLUDE_ACCEL0
typedef 7 Num_Slaves;
`else
typedef 6 Num_Slaves;
`endif
`else
`ifdef INCLUDE_ACCEL0
typedef 6 Num_Slaves;
`else
typedef 5 Num_Slaves;
`endif
`endif

Integer tcm_back_door_slave_num   = 0;
Integer boot_rom_slave_num        = 1;
Integer mem0_controller_slave_num = 2;
Integer uart0_slave_num           = 3;
Integer timer0_slave_num          = 4;

`ifdef INCLUDE_ACCEL0
Integer accel0_slave_num          = 5;
`endif

`ifdef HTIF_MEMORY
`ifdef INCLUDE_ACCEL0
Integer htif_slave_num            = 6;
`else
Integer htif_slave_num            = 5;
`endif
`endif

// ================================================================

endpackage
