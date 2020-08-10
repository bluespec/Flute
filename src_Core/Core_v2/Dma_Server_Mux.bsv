// Copyright (c) 2020 Bluespec, Inc. All Rights Reserved.

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

// Main fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_{Id,Addr,Data,User}
import Near_Mem_IFC :: *;    // for Wd_{Id,Addr,Data,User}_Dma

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

   AXI4_Slave_Xactor_IFC  #(Wd_Id_Dma, Wd_Addr_Dma, Wd_Data_Dma, Wd_User_Dma) slave_xactor_A <- mkAXI4_Slave_Xactor;
   AXI4_Slave_Xactor_IFC  #(Wd_Id,     Wd_Addr,     Wd_Data,     Wd_User)     slave_xactor_B <- mkAXI4_Slave_Xactor;
   AXI4_Master_Xactor_IFC #(Wd_Id_Dma, Wd_Addr_Dma, Wd_Data_Dma, Wd_User_Dma) master_xactor  <- mkAXI4_Master_Xactor;

   // ----------------------------------------------------------------
   // BEHAVIOR

   Reg #(Bool) rg_done <- mkReg (False);

   // TODO: fill in.
   rule rl_WARNING (! rg_done);
      $display ("%0d: %m.rl_WARNING", cur_cycle);
      $display ("    WARNING WARNING WARNING");
      $display ("    TBD: the body of this module needs to be implemented");
      $display ("    Missing Debug Module connectivity to memory");
      rg_done <= True;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   interface initiator_A_server = slave_xactor_A.axi_side;
   interface initiator_B_server = slave_xactor_B.axi_side;
   interface target_client      = master_xactor.axi_side;
endmodule

// ================================================================

endpackage
