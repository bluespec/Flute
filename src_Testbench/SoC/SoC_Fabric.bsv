// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

package SoC_Fabric;

// ================================================================
// Defines a SoC Fabric that is a specialization of AXI4_Lite_Fabric
// for this particular SoC.

// ================================================================
// Project imports

import AXI4_Types  :: *;
import AXI4_Fabric :: *;

import Fabric_Defs :: *;    // for Wd_Addr, Wd_Data, Wd_User
import SoC_Map     :: *;    // for Num_Masters, Num_Slaves

// ================================================================
// Slave address decoder
// Identifies whether a given addr is legal and, if so, which  slave services it.

typedef Bit #(TLog #(Num_Slaves))  Slave_Num;

// ================================================================
// Specialization of parameterized AXI4 fabric for this SoC.

typedef AXI4_Fabric_IFC #(Num_Masters,
			  Num_Slaves,
			  Wd_Id,
			  Wd_Addr,
			  Wd_Data,
			  Wd_User)  Fabric_AXI4_IFC;

// ----------------

(* synthesize *)
module mkFabric_AXI4 (Fabric_AXI4_IFC);

   SoC_Map_IFC soc_map <- mkSoC_Map;

   function Tuple2 #(Bool, Slave_Num) fn_addr_to_slave_num  (Fabric_Addr addr);

      // Main Mem
      if (   (soc_map.m_mem0_controller_addr_base <= addr)
	  && (addr < soc_map.m_mem0_controller_addr_lim))
	 return tuple2 (True, fromInteger (mem0_controller_slave_num));

      // Boot ROM
      else if (   (soc_map.m_boot_rom_addr_base <= addr)
	  && (addr < soc_map.m_boot_rom_addr_lim))
	 return tuple2 (True, fromInteger (boot_rom_slave_num));

`ifdef Near_Mem_TCM
      // TCM DMA Server
      else if (   (soc_map.m_itcm_addr_base <= addr)
	       && (addr < soc_map.m_itcm_addr_lim))
	 return tuple2 (True, fromInteger (dma_server_num));
`endif

      // UART
      else if (   (soc_map.m_uart0_addr_base <= addr)
	       && (addr < soc_map.m_uart0_addr_lim))
	 return tuple2 (True, fromInteger (uart0_slave_num));

`ifdef HTIF_MEMORY
      else if (   (soc_map.m_htif_addr_base <= addr)
	       && (addr < soc_map.m_htif_addr_lim))
	 return tuple2 (True, fromInteger (htif_slave_num));
`endif

`ifdef INCLUDE_ACCEL0
      // Accelerator 0
      else if (   (soc_map.m_accel0_addr_base <= addr)
	       && (addr < soc_map.m_accel0_addr_lim))
	 return tuple2 (True, fromInteger (accel0_slave_num));
`endif

      else
	 return tuple2 (False, ?);
   endfunction

   AXI4_Fabric_IFC #(Num_Masters, Num_Slaves, Wd_Id, Wd_Addr, Wd_Data, Wd_User)
       fabric <- mkAXI4_Fabric (fn_addr_to_slave_num);

   return fabric;
endmodule

// ================================================================

endpackage
