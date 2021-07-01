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

//import SoC_Map :: *;    // For N_External_Interrupt_Sources
import PLIC    :: *;    // For PLIC_IFC, mkPLIC
import Vector  :: *;

// ================================================================
// PLIC for this core

typedef 16  N_External_Interrupt_Sources;
typedef  2  PLIC_N_Targets;
typedef  7  PLIC_Max_Priority;

typedef TAdd #(N_External_Interrupt_Sources, 1) T_n_sources;
typedef TLog #(TAdd #(PLIC_Max_Priority, 1))    T_wd_priority;


typedef  PLIC_IFC #(N_External_Interrupt_Sources,
		    PLIC_N_Targets,
		    PLIC_Max_Priority)             PLIC_IFC_16_2_7;

(* noinline *)
function Tuple2 #(Bit #(T_wd_priority), Bit #(TLog #(T_n_sources)))
   fn_target_max_prio_and_max_id0 (Vector #(T_n_sources, Bool)                           vrg_source_ip,
				   Vector #(PLIC_N_Targets, Vector #(T_n_sources, Bool)) vvrg_ie,
				   Vector #(T_n_sources, Bit #(T_wd_priority))           vrg_source_prio,

				   Bit #(T_wd_target_id)  target_id);

   Bit #(T_wd_priority)       max_prio = 0;
   Bit #(TLog #(T_n_sources)) max_id   = 0;

   // Note: source_ids begin at 1, not 0.
   for (Integer source_id = 1; source_id < valueof(T_n_sources); source_id = source_id + 1)
      if (   vrg_source_ip [source_id]
	  && (vrg_source_prio [source_id] > max_prio)
	  && (vvrg_ie [target_id][source_id])) begin
						  max_id   = fromInteger (source_id);
						  max_prio = vrg_source_prio [source_id];
					       end
   // Assert: if any interrupt is pending (max_id > 0), then prio > 0
   return tuple2 (max_prio, max_id);
endfunction


(* synthesize *)
module mkPLIC_16_2_7 (PLIC_IFC_16_2_7);
   let m <- mkPLIC(fn_target_max_prio_and_max_id0);
   return m;
endmodule

// ================================================================

endpackage
