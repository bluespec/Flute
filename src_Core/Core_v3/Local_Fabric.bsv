// Copyright (c) 2018-2020 Bluespec, Inc. All Rights Reserved.

package Local_Fabric;

// ================================================================

// Defines the fabric (specialization of AXI4_Fabric) used inside the
// Core to interconnect
//     masters: CPU and Debug Module
//     slaves:  System Fabric, CLINT, PLIC, ITCM, DTCM

// ================================================================
// Project imports

// Main fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User
import SoC_Map      :: *;

// ================================================================
// HERE BE DRAGONS! Configuration of several different fabrics based on an array of switches:
// NEAR_MEM_TCM, DUAL_FABRIC, INCLUDE_GDB_CONTROL

`ifdef DUAL_FABRIC
`ifdef INCLUDE_GDB_CONTROL
`ifdef TCM_LOADER

// tcm assumed.

// 3x4 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access, TCM Loader
// Slaves: PLIC, Near_Mem_IO, IMem, DMem
typedef 3  Num_Masters;
typedef 4  Num_Slaves;

`else
// Dual fabric, gdb, no tcm loader

// 2x4 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access
// Slaves: PLIC, Near_Mem_IO, IMem, DMem
typedef 2  Num_Masters;
typedef 4  Num_Slaves;

`endif
`else
// Dual fabric, no gdb
`ifdef TCM_LOADER
// tcm assumed.

// 2x4 Fabric for the Core
// Masters: CPU DMem, TCM Loader
// Slaves: PLIC, Near_Mem_IO, IMem, DMem
typedef 2  Num_Masters;
typedef 4  Num_Slaves;

`else
// Dual fabric, no gdb, no tcm loader, tcm assumed

// 1x2 Fabric for the Core
// Masters: CPU DMem
// Slaves: PLIC, Near_Mem_IO
typedef 1  Num_Masters;
typedef 2  Num_Slaves;

`endif
`endif
`else
// No dual fabric
`ifdef NEAR_MEM_TCM
`ifdef INCLUDE_GDB_CONTROL
`ifdef TCM_LOADER
// tcm assumed.

// 3x5 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access, TCM Loader
// Slaves: PLIC, Near_Mem_IO, Interconnect, IMem, DMem
typedef 3  Num_Masters;
typedef 5  Num_Slaves;

`else
// No dual fabric, gdb, no tcm loader

// 2x5 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access
// Slaves: PLIC, Near_Mem_IO, Interconnect, IMem, DMem
typedef 2  Num_Masters;
typedef 5  Num_Slaves;

`endif
`else
// No dual fabric, no gdb
`ifdef TCM_LOADER

// 2x5 Fabric for the Core
// Masters: CPU DMem, TCM Loader
// Slaves: PLIC, Near_Mem_IO, Interconnect, IMem, DMem
typedef 2  Num_Masters;
typedef 5  Num_Slaves;

`else
// No dual fabric, tcm, no gdb, no tcm loader

// 1x3 Fabric for the Core
// Masters: CPU DMem
// Slaves: PLIC, Near_Mem_IO, Interconnect
typedef 1  Num_Masters;
typedef 3  Num_Slaves;

`endif
`endif
`else
// no tcm
`ifdef INCLUDE_GDB_CONTROL
// No dual fabric, no tcm, gdb

// 2x3 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access
// Slaves: PLIC, Near_Mem_IO, Interconnect
typedef 2  Num_Masters;
typedef 3  Num_Slaves;

`else
// No dual fabric, no gdb
// No dual fabric, no tcm, no gdb

// 1x3 Fabric for the Core
// Masters: CPU DMem
// Slaves: PLIC, Near_Mem_IO, Interconnect
typedef 1  Num_Masters;
typedef 3  Num_Slaves;

`endif
`endif
`endif

typedef Bit #(TLog #(Num_Masters))  Master_Num;
typedef Bit #(TLog #(Num_Slaves))  Slave_Num;

`ifdef DUAL_FABRIC
`ifdef INCLUDE_GDB_CONTROL
`ifdef TCM_LOADER

// tcm assumed.

// 3x4 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access, TCM Loader
// Slaves: PLIC, Near_Mem_IO, IMem, DMem
Master_Num  cpu_dmem_master_num         = 0;
Master_Num  debug_module_sba_master_num = 1;
Master_Num  loader_master_num           = 2;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  imem_dma_slave_num    = 2;
Slave_Num  dmem_dma_slave_num    = 3;

`else
// Dual fabric, gdb, no tcm loader

// 2x4 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access
// Slaves: PLIC, Near_Mem_IO, IMem, DMem
Master_Num  cpu_dmem_master_num         = 0;
Master_Num  debug_module_sba_master_num = 1;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  imem_dma_slave_num    = 2;
Slave_Num  dmem_dma_slave_num    = 3;

`endif
`else
// Dual fabric, no gdb
`ifdef TCM_LOADER
// tcm assumed.

// 2x4 Fabric for the Core
// Masters: CPU DMem, TCM Loader
// Slaves: PLIC, Near_Mem_IO, IMem, DMem
Master_Num  cpu_dmem_master_num         = 0;
Master_Num  loader_master_num           = 1;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  imem_dma_slave_num    = 2;
Slave_Num  dmem_dma_slave_num    = 3;

`else
// Dual fabric, no gdb, no tcm loader, tcm assumed

// 1x2 Fabric for the Core
// Masters: CPU DMem
// Slaves: PLIC, Near_Mem_IO
Master_Num  cpu_dmem_master_num         = 0;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;

`endif
`endif
`else
// No dual fabric
`ifdef NEAR_MEM_TCM
`ifdef INCLUDE_GDB_CONTROL
`ifdef TCM_LOADER
// tcm assumed.

// 3x5 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access, TCM Loader
// Slaves: PLIC, Near_Mem_IO, Interconnect, IMem, DMem
Master_Num  cpu_dmem_master_num         = 0;
Master_Num  debug_module_sba_master_num = 1;
Master_Num  loader_master_num           = 2;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  default_slave_num     = 2; // for interconnect
Slave_Num  imem_dma_slave_num    = 3;
Slave_Num  dmem_dma_slave_num    = 4;

`else
// No dual fabric, gdb, no tcm loader

// 2x5 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access
// Slaves: PLIC, Near_Mem_IO, Interconnect, IMem, DMem
Master_Num  cpu_dmem_master_num         = 0;
Master_Num  debug_module_sba_master_num = 1;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  default_slave_num     = 2; // for interconnect
Slave_Num  imem_dma_slave_num    = 3;
Slave_Num  dmem_dma_slave_num    = 4;

`endif
`else
// No dual fabric, no gdb
`ifdef TCM_LOADER

// 2x5 Fabric for the Core
// Masters: CPU DMem, TCM Loader
// Slaves: PLIC, Near_Mem_IO, Interconnect, IMem, DMem
Master_Num  cpu_dmem_master_num         = 0;
Master_Num  loader_master_num           = 1;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  default_slave_num     = 2; // for interconnect
Slave_Num  imem_dma_slave_num    = 3;
Slave_Num  dmem_dma_slave_num    = 4;

`else
// No dual fabric, tcm, no gdb, no tcm loader

// 1x3 Fabric for the Core
// Masters: CPU DMem
// Slaves: PLIC, Near_Mem_IO, Interconnect
Master_Num  cpu_dmem_master_num         = 0;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  default_slave_num     = 2; // for interconnect

`endif
`endif
`else
// no tcm
`ifdef INCLUDE_GDB_CONTROL
// No dual fabric, no tcm, gdb

// 2x3 Fabric for the Core
// Masters: CPU DMem, Debug Module System Bus Access
// Slaves: PLIC, Near_Mem_IO, Interconnect
Master_Num  cpu_dmem_master_num         = 0;
Master_Num  debug_module_sba_master_num = 1;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  default_slave_num     = 2; // for interconnect

`else
// No dual fabric, no gdb
// No dual fabric, no tcm, no gdb

// 1x3 Fabric for the Core
// Masters: CPU DMem
// Slaves: PLIC, Near_Mem_IO, Interconnect
Master_Num  cpu_dmem_master_num         = 0;
Slave_Num  plic_slave_num        = 0;
Slave_Num  clint_slave_num       = 1;
Slave_Num  default_slave_num     = 2; // for interconnect

`endif
`endif
`endif

// ================================================================

// ----------------
// Specialization of parameterized AXI4 fabric for different local core fabrics

typedef AXI4_Fabric_IFC #(Num_Masters,
			  Num_Slaves,
			  Wd_Id,
			  Wd_Addr,
			  Wd_Data,
			  Wd_User)  Local_Fabric_IFC;


// ----------------

(* synthesize *)
module mkLocal_Fabric (Local_Fabric_IFC);

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // ----------------
   // Slave address decoder
   // Any addr is legal, and there is only one slave to service it.

   function Tuple2 #(Bool, Slave_Num) fn_addr_to_slave_num  (Fabric_Addr addr);
      Slave_Num selected_target = ?;
      if (   (soc_map.m_clint_addr_base <= addr)
	  && (addr < soc_map.m_clint_addr_lim))
          selected_target = clint_slave_num;

      else if (   (soc_map.m_plic_addr_base <= addr)
	       && (addr < soc_map.m_plic_addr_lim))
	 selected_target = plic_slave_num;

`ifdef DUAL_FABRIC
`ifdef INCLUDE_GDB_CONTROL
      // DUAL_FABRIC with GDB_CONTROL
      // All addresses must be captured in these two else cases. There is no bound checking in
      // TCMs so, behaviour is undefined if a wrong address makes it to the TCM. It is the
      // responsibility of the SoC Map to place all code and data regions within the TCM bounds
      else if (   (soc_map.m_itcm_addr_base <= addr)
	       && (addr < soc_map.m_itcm_addr_lim))
	 selected_target = imem_dma_slave_num;

      else if (   (soc_map.m_dtcm_addr_base <= addr)
	       && (addr < soc_map.m_dtcm_addr_lim))
	 selected_target = dmem_dma_slave_num;

`endif
`else
      // !DUAL_FABRIC (a single AXI4 fabric for all targets)
`ifdef Near_Mem_TCM
`ifdef INCLUDE_GDB_CONTROL
      // TCM with GDB_CONTROL (three targets)
      else if (   (soc_map.m_itcm_addr_base <= addr)
	       && (addr < soc_map.m_itcm_addr_lim))
	 selected_target = imem_dma_slave_num;

      else if (   (soc_map.m_dtcm_addr_base <= addr)
	       && (addr < soc_map.m_dtcm_addr_lim))
	 selected_target = dmem_dma_slave_num;

      else
	 selected_target = default_slave_num;

`else
      // TCM with no GDB_CONTROL (one - default target)
      else
	 selected_target = default_slave_num;

`endif
`else
      // !TCM (one - default target)
      else
	 selected_target = default_slave_num;

`endif
`endif
      return tuple2 (True, selected_target);
   endfunction

   AXI4_Fabric_IFC #(Num_Masters, Num_Slaves, Wd_Id, Wd_Addr, Wd_Data, Wd_User)
       fabric <- mkAXI4_Fabric (fn_addr_to_slave_num);

   return fabric;
endmodule

// ================================================================

endpackage
