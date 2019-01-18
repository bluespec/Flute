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
// Exports

export  SoC_Map_IFC (..), mkSoC_Map;

// export  fn_addr_in_range;

export  Num_Masters;
export  debug_module_master_num;
export  imem_master_num;
export  dmem_master_num;

export  Num_Slaves;
export  boot_rom_slave_num;
export  tcm_back_door_slave_num;
export  mem0_controller_slave_num;
export  uart0_slave_num;

// ================================================================
// Bluespec library imports

// None

// ================================================================
// Project imports

import Fabric_Defs :: *;    // Only for type Fabric_Addr

// ================================================================
// Interface and module for the address map

interface SoC_Map_IFC;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_uart0_addr_lim;

   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_base;
   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_size;
   (* always_ready *)   method  Fabric_Addr  m_near_mem_io_addr_lim;

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

   (* always_ready *)   method  Bit #(64)    m_pc_reset_value;
endinterface

// ================================================================

(* synthesize *)
module mkSoC_Map (SoC_Map_IFC);

   // ----------------------------------------------------------------
   // UART 0

   Fabric_Addr uart0_addr_base = 'hC000_0000;
   Fabric_Addr uart0_addr_size = 'h0000_0080;    // 128
   Fabric_Addr uart0_addr_lim  = uart0_addr_base + uart0_addr_size;

   function Bool fn_is_uart0_addr (Fabric_Addr addr);
      return ((uart0_addr_base <= addr) && (addr < uart0_addr_lim));
   endfunction

   // ----------------------------------------------------------------
   // Near_Mem_IO (CLINT)

   Fabric_Addr near_mem_io_addr_base = 'h_6300_0000;
   Fabric_Addr near_mem_io_addr_size = 'h_0001_0000;    // 64K
   Fabric_Addr near_mem_io_addr_lim  = near_mem_io_addr_base + near_mem_io_addr_size;

   function Bool fn_is_near_mem_io_addr (Fabric_Addr addr);
      return ((near_mem_io_addr_base <= addr) && (addr < near_mem_io_addr_lim));
   endfunction

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
   Fabric_Addr mem0_controller_addr_size = 'h_0FFF_FFFF;    // 256 MB
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
   // (Caches needs this information to cache these addresses.)

   function Bool fn_is_mem_addr (Fabric_Addr addr);
      return (   fn_is_boot_rom_addr (addr)
	      || fn_is_mem0_controller_addr (addr)
	      || fn_is_tcm_addr (addr)
	      );
   endfunction

   // ----------------------------------------------------------------
   // I/O address predicate
   // Identifies I/O addresses in the Fabric.
   // (Caches needs this information to avoid cacheing these addresses.)

   function Bool fn_is_IO_addr (Fabric_Addr addr);
      return (   fn_is_uart0_addr  (addr)
	      || fn_is_near_mem_io_addr (addr)
	      );
   endfunction

   // ----------------------------------------------------------------
   // PC reset value

   Bit #(64) pc_reset_value = boot_rom_addr_base;

   // ================================================================
   // INTERFACE

   method  Fabric_Addr  m_uart0_addr_base = uart0_addr_base;
   method  Fabric_Addr  m_uart0_addr_size = uart0_addr_size;
   method  Fabric_Addr  m_uart0_addr_lim  = uart0_addr_lim;

   method  Fabric_Addr  m_near_mem_io_addr_base = near_mem_io_addr_base;
   method  Fabric_Addr  m_near_mem_io_addr_size = near_mem_io_addr_size;
   method  Fabric_Addr  m_near_mem_io_addr_lim  = near_mem_io_addr_lim;

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

   method  Bit #(64)    m_pc_reset_value = pc_reset_value;
endmodule

// ================================================================
// Count and master-numbers of masters in the fabric.

typedef 3 Num_Masters;

Integer debug_module_master_num = 0;
Integer imem_master_num         = 1;
Integer dmem_master_num         = 2;

// ================================================================
// Count and slave-numbers of slaves in the fabric.

typedef 4 Num_Slaves;

Integer tcm_back_door_slave_num   = 0;
Integer boot_rom_slave_num        = 1;
Integer mem0_controller_slave_num = 2;
Integer uart0_slave_num           = 3;

// ================================================================

endpackage
