// Copyright (c) 2018-2022 Bluespec, Inc. All Rights Reserved.

package Core_MMIO_Fabric;

// ================================================================
// Defines a specialization of AXI4 fabric used inside the Core to
// connect:
//     Initiators: CPU MMIO
//     Targets: System Interconnect, PLIC, Near_Mem_IO (CLINT), Boot_ROM

// ================================================================
// Project imports

// Main fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User
import SoC_Map      :: *;

// ================================================================
// Fabric port numbers for Initiators

typedef 1  Core_MMIO_Fabric_Num_Initiators;

typedef Bit #(TLog #(Core_MMIO_Fabric_Num_Initiators))  Core_MMIO_Fabric_Initiator_Num;

Core_MMIO_Fabric_Initiator_Num  cpu_mmio_master_num = ?;

// ----------------
// Fabric port numbers for targets

typedef 4  Core_MMIO_Fabric_Num_Targets;

typedef Bit #(TLog #(Core_MMIO_Fabric_Num_Targets))  Core_MMIO_Fabric_Target_Num;

Core_MMIO_Fabric_Target_Num  default_target_num     = 0;
Core_MMIO_Fabric_Target_Num  near_mem_io_target_num = 1;    // memory-mapped MTIME, MTIMECMP, MSIP etc.
Core_MMIO_Fabric_Target_Num  plic_target_num        = 2;
Core_MMIO_Fabric_Target_Num  boot_rom_target_num    = 3;

// ----------------
// Specialization of parameterized AXI4 fabric for 1x3 Core fabric

typedef AXI4_Fabric_IFC #(Core_MMIO_Fabric_Num_Initiators,
			  Core_MMIO_Fabric_Num_Targets,
			  Wd_Id,
			  Wd_Addr,
			  Wd_Data,
			  Wd_User)  Core_MMIO_Fabric_IFC;

// ----------------

(* synthesize *)
module mkCore_MMIO_Fabric (Core_MMIO_Fabric_IFC);

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // ----------------
   // Target address decoder
   // Any addr is legal, and there is only one target to service it.

   function Tuple2 #(Bool, Core_MMIO_Fabric_Target_Num) fn_addr_to_target_num_1x3  (Fabric_Addr addr);
      // Near_Mem_IO (CLINT)
      if (   (soc_map.m_near_mem_io_addr_base <= addr)
	  && (addr < soc_map.m_near_mem_io_addr_lim))
	 return tuple2 (True, near_mem_io_target_num);

      // PLIC
      else if (   (soc_map.m_plic_addr_base <= addr)
	       && (addr < soc_map.m_plic_addr_lim))
	 return tuple2 (True, plic_target_num);

      // Boot ROM
      else if (   (soc_map.m_boot_rom_addr_base <= addr)
	       && (addr < soc_map.m_boot_rom_addr_lim))
	 return tuple2 (True, boot_rom_target_num);

      // Default: to System
      else
	 return tuple2 (True, default_target_num);
   endfunction

   AXI4_Fabric_IFC #(Core_MMIO_Fabric_Num_Initiators,
		     Core_MMIO_Fabric_Num_Targets,
		     Wd_Id, Wd_Addr, Wd_Data, Wd_User)
       fabric <- mkAXI4_Fabric (fn_addr_to_target_num_1x3);

   return fabric;
endmodule: mkCore_MMIO_Fabric

// ================================================================

endpackage
