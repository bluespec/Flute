// Copyright (c) 2015-2017 Bluespec, Inc., All Rights Reserved
// Author: Rishiyur S. Nikhil

// A "credit counter" which can be incremented and decremented concurrently.

package CreditCounter;

// ================================================================
// BSV library imports

import Cur_Cycle :: *;

// ================================================================
// Interface

interface CreditCounter_IFC #(numeric type w);
   // Current value of internal count
   method UInt #(w) value;

   // Increment internal count
   method Action incr;

   // Decrement internal count
   method Action decr;

   // Clear internal count to 0
   method Action clear;
endinterface

// ================================================================
// Module implementation
// Scheduling: value < incr < decr < clear

module mkCreditCounter (CreditCounter_IFC #(w));

   Reg #(UInt #(w)) crg [3] <- mkCReg (3, 0);

   method UInt #(w) value = crg [1];

   method Action incr;
      if (crg [0] == maxBound) begin
	 $display ("%0d: ERROR: CreditCounter: overflow", cur_cycle);
	 $finish (1);    // Assertion failure
      end
      crg [0] <= crg [0] + 1;
   endmethod

   method Action decr () if (crg [1] != 0);
      if (crg [1] == 0) begin
	 $display ("%0d: ERROR: CreditCounter: underflow", cur_cycle);
	 $finish (1);    // Assertion failure
      end
      crg [1] <= crg [1] - 1;
   endmethod

   method Action clear;
      crg [2] <= 0;
   endmethod
endmodule

// ================================================================

endpackage
