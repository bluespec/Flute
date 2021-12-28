// Copyright (c) 2013-2022 Bluespec, Inc. All Rights Reserved.

package Cur_Cycle;

// ================================================================
// A convenience function to return the current cycle number during BSV simulations

ActionValue #(Bit #(32)) cur_cycle = actionvalue
					Bit #(32) t <- $stime;
					return t / 10;
				     endactionvalue;

// ================================================================
// fa_debug_show_location
// Shows module hierarchy and current cycle

// Note: each invocation looks like this:
//     fa_debug_show_location; if (verbosity != 0) $display ("<invocation location>");

// Why not define the function as:
//     function Action fa_debug_show_location (Integer verbosity, String location_s);
// and just print the location as part of this function?

// This is a workaround, because there's some bug in Verilog codegen
// and/or Verilator, where that version core dumps.

function Action fa_debug_show_location (Integer verbosity);
   action
      if (verbosity != 0) begin
	 $display ("    %m");
	 $write ("    %0d: ", cur_cycle);
      end
   endaction
endfunction

// ================================================================

endpackage
