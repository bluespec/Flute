// Copyright (c) 2020 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AXI4_Accel_IFC;

// ================================================================
// Canonical interface for a accelerator:
//   'init'   method to:
//              - intialize the accelerator
//              - set its place in the address map
//              - set an AXI4 TID to be used by the master interface
//   'slave'  interface that is used to program the accelerator
//   'master' interface used by the accelerator to read/write memory

// ================================================================
// Bluespec library imports

import RegFile      :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import AXI4_Types  :: *;
import Fabric_Defs :: *;

// ================================================================
// Interface

interface AXI4_Accel_IFC;
   method Action init (Bit# (Wd_Id) axi4_id, Bit #(Wd_Addr) addr_base, Bit #(Wd_Addr) addr_lim);

   interface AXI4_Slave_IFC  #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  slave;

   method Bool interrupt_req;

   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  master;
endinterface

// ================================================================

endpackage
