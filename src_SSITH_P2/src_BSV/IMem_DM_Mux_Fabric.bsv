// Copyright (c) 2019 Bluespec, Inc. All Rights Reserved

package IMem_DM_Mux_Fabric;

// ================================================================
// Defines a 2x1 Fabric that is a specialization of AXI4_Lite_Fabric
// that is used only within mkP1_Core to multiplex two AXI4-Lite
// master interfaces (from the IMem and Debug Module) into a single
// AXI4-Lite master interface.

// ================================================================

export Fabric_2x1_IFC (..), mkFabric_2x1;

// ================================================================
// Project imports

import AXI4_Lite_Types  :: *;
import AXI4_Lite_Fabric :: *;

import Fabric_Defs      :: *;    // for Wd_Addr, Wd_Data, Wd_User

// ================================================================
// Slave address decoder
// Any addr is legal, and there is only one slave to service it.

typedef Bit #(0)  Slave_Num_2x1;

function Tuple2 #(Bool, Slave_Num_2x1) fn_addr_to_slave_num_2x1  (Fabric_Addr addr);
   return tuple2 (True, ?);
endfunction

// ================================================================
// Specialization of parameterized AXI4_Lite fabric for this case.

typedef AXI4_Lite_Fabric_IFC #(2,         // Num_Masters
			       1,         // Num_Slaves
			       Wd_Addr,
			       Wd_Data,
			       Wd_User)  Fabric_2x1_IFC;

// ----------------

(* synthesize *)
module mkFabric_2x1 (Fabric_2x1_IFC);

   AXI4_Lite_Fabric_IFC #(2, 1, Wd_Addr, Wd_Data, Wd_User)
       fabric <- mkAXI4_Lite_Fabric (fn_addr_to_slave_num_2x1);

   return fabric;
endmodule

// ================================================================

endpackage
