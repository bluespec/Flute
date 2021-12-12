// Copyright (c) 2018-2020 Bluespec, Inc. All Rights Reserved.

package Fabric_1x3;

// ================================================================
// Defines a specialization of AXI4 fabric used inside the Core to
// connect:
//     Initiators: CPU MMIO
//     Targets: System Interconnect, PLIC, Near_Mem_IO

// ================================================================
// Project imports

// Main fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User
import SoC_Map      :: *;

// ================================================================
// Fabric port numbers for Initiators

typedef 1  Num_Initiators_1x3;

typedef Bit #(TLog #(Num_Initiators_1x3))  Initiator_Num_1x3;

Initiator_Num_1x3  cpu_mmio_master_num = ?;

// ----------------
// Fabric port numbers for targets

typedef 3  Num_Targets_1x3;

typedef Bit #(TLog #(Num_Targets_1x3))  Target_Num_1x3;

Target_Num_1x3  default_target_num     = 0;
Target_Num_1x3  near_mem_io_target_num = 1;    // memory-mapped MTIME, MTIMECMP, MSIP etc.
Target_Num_1x3  plic_target_num        = 2;

// ----------------
// Specialization of parameterized AXI4 fabric for 1x3 Core fabric

typedef AXI4_Fabric_IFC #(Num_Initiators_1x3,
			  Num_Targets_1x3,
			  Wd_Id,
			  Wd_Addr,
			  Wd_Data,
			  Wd_User)  Fabric_1x3_IFC;

// ----------------

(* synthesize *)
module mkFabric_1x3 (Fabric_1x3_IFC);

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // ----------------
   // Target address decoder
   // Any addr is legal, and there is only one target to service it.

   function Tuple2 #(Bool, Target_Num_1x3) fn_addr_to_target_num_1x3  (Fabric_Addr addr);
      if (   (soc_map.m_near_mem_io_addr_base <= addr)
	  && (addr < soc_map.m_near_mem_io_addr_lim))
	 return tuple2 (True, near_mem_io_target_num);

      else if (   (soc_map.m_plic_addr_base <= addr)
	       && (addr < soc_map.m_plic_addr_lim))
	 return tuple2 (True, plic_target_num);

      else
	 return tuple2 (True, default_target_num);
   endfunction

   AXI4_Fabric_IFC #(Num_Initiators_1x3, Num_Targets_1x3, Wd_Id, Wd_Addr, Wd_Data, Wd_User)
       fabric <- mkAXI4_Fabric (fn_addr_to_target_num_1x3);

   return fabric;
endmodule: mkFabric_1x3

// ================================================================

endpackage
