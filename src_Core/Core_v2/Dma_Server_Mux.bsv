// Copyright (c) 2020-2021 Bluespec, Inc. All Rights Reserved.

package Dma_Server_Mux;

// ================================================================
// This package defines a Mux into the Dma_Server of the coherent L2
// (which may be lifted through Core.CPU.Near_Mem.LLCache).
// The Dma_Server interface is: AXI4_Slave_IFC_Id6_Addr64_Data512_User0.

// This Mux connects that to two clients:
//   An AXI4_Slave_IFC_Id6_Addr64_Data512_User0 (for external DMA)
//   An AXI4_Slave_IFC_Id4_Addr64_Data64_User0  (for Debug Module)

// ================================================================
// BSV library imports

import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import AXI4_Types   :: *;
import AXI_Widths   :: *;
import AXI4_Fabric  :: *;
import AXI4_Widener :: *;

import Fabric_Defs  :: *;    // for Wd_{Id,Addr,Data,User}
import Near_Mem_IFC :: *;    // for Wd_{Id,Addr,Data,User}_Dma

// ================================================================

export Dma_Server_Mux_IFC (..), mkDma_Server_Mux;

// ================================================================
// INTERFACE

interface Dma_Server_Mux_IFC;
   interface AXI4_Slave_IFC  #(Wd_Id_Dma, Wd_Addr_Dma, Wd_Data_Dma, Wd_User_Dma)  initiator_A_server;
   interface AXI4_Slave_IFC  #(Wd_Id,     Wd_Addr,     Wd_Data,     Wd_User)      initiator_B_server;
   interface AXI4_Master_IFC #(Wd_Id_Dma, Wd_Addr_Dma, Wd_Data_Dma, Wd_User_Dma)  target_client;
endinterface

// ================================================================
// IMPLEMENTATION MODULE

(* synthesize *)
module mkDma_Server_Mux (Dma_Server_Mux_IFC);

   AXI4_Widener_IFC #(Wd_Id_Dma,
		      Wd_Addr_Dma,
		      Wd_Data,        // narrower
		      Wd_Data_Dma,    // wider
		      Wd_User_Dma) widener <- mkAXI4_Widener;

   Fabric_2x1_IFC fabric_2x1 <- mkDma_Server_Mux_Fabric;

   // ----------------------------------------------------------------
   // Connect widener to one of the fabric ports

   mkConnection (widener.to_slave, fabric_2x1.v_from_masters [1]);

   // ----------------------------------------------------------------
   // INTERFACE

   interface initiator_A_server = fabric_2x1.v_from_masters [0];
   interface initiator_B_server = widener.from_master;
   interface target_client      = fabric_2x1.v_to_slaves [0];
endmodule

// ****************************************************************
// ****************************************************************
// 2x1 Fabric to mux the DMA and Debug Module into the CPU.
// Masters: External DMA into memory, Debug Module System Bus Access
// Slave: mkCPU.dma_server

// ----------------
// Fabric port numbers for masters

typedef 2  Num_Masters_2x1;

// ----------------
// Fabric port numbers for slaves

typedef 1  Num_Slaves_2x1;

typedef Bit #(TLog #(Num_Slaves_2x1))  Slave_Num_2x1;

Slave_Num_2x1  default_slave_num     = 0;

// ----------------
// Specialization of parameterized AXI4 fabric for 2x1 Core fabric

typedef AXI4_Fabric_IFC #(Num_Masters_2x1,
			  Num_Slaves_2x1,
			  Wd_Id_Dma,
			  Wd_Addr_Dma,
			  Wd_Data_Dma,
			  Wd_User_Dma)  Fabric_2x1_IFC;

// ----------------

(* synthesize *)
module mkDma_Server_Mux_Fabric (Fabric_2x1_IFC);

   // ----------------
   // Slave address decoder
   // Any addr is legal, and there is only one slave to service it.

   function Tuple2 #(Bool, Slave_Num_2x1) fn_addr_to_slave_num_2x1  (Fabric_Addr addr);
      return tuple2 (True, default_slave_num);
   endfunction

   Fabric_2x1_IFC fabric <- mkAXI4_Fabric (fn_addr_to_slave_num_2x1);

   return fabric;
endmodule: mkDma_Server_Mux_Fabric

// ================================================================

endpackage
