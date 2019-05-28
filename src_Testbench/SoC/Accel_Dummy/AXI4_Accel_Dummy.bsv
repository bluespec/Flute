// Copyright (c) 2019 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package AXI4_Accel_Dummy;

// ================================================================
// A dummy 'accelerator' for testing that software can read/write
// accelerator registers.

// The slave interface just implements a memory model.
// The master interface is a dummy.

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

import AXI4_Types     :: *;
import AXI4_Mem_Model :: *;
import Fabric_Defs    :: *;
import AXI4_Accel_IFC :: *;

// ================================================================

(* synthesize *)
module mkAXI4_Accel_Dummy (AXI4_Accel_IFC);

   AXI4_Mem_Model_IFC #(Wd_Id,
			Wd_Addr,
			Wd_Data,
			Wd_User) mem_model <- mkAXI4_Mem_Model;

   method Action init (Bit# (Wd_Id) axi4_id, Bit #(Wd_Addr) addr_base, Bit #(Wd_Addr) addr_lim);
      mem_model.init (addr_base, addr_lim);
   endmethod

   interface slave  = mem_model.slave;
   interface master = dummy_AXI4_Master_ifc;
endmodule

// ================================================================

endpackage
