// Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved

package PLIC_16_2_7;

// ================================================================
// Instantiation of parameterized PLIC to specific parameter values.
//
// ================================================================
// Bluespec lib imports

// None

// ----------------
// BSV additional libs

// None

// ================================================================
// Project imports

import SoC_Map :: *;    // For N_External_Interrupt_Sources
import PLIC    :: *;    // For PLIC_IFC, mkPLIC

// ================================================================
// PLIC for this core

typedef  2  PLIC_N_Targets;
typedef  7  PLIC_Max_Priority;

typedef  PLIC_IFC #(N_External_Interrupt_Sources,
		    PLIC_N_Targets,
		    PLIC_Max_Priority)             PLIC_IFC_16_2_7;

(* synthesize *)
module mkPLIC_16_2_7 (PLIC_IFC_16_2_7);
   let m <- mkPLIC;
   return m;
endmodule

// ================================================================

endpackage
