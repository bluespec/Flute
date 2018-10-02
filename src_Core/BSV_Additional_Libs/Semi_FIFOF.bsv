// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package Semi_FIFOF;

// ================================================================
// Separate interfaces for input-side and output-side of FIFOF.
// Conversion functions to these, from FIFOF interfaces.

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

// ================================================================
// Semi-FIFOF interfaces

interface FIFOF_I #(type t);
   method Action  enq (t x);
   method Bool    notFull ();
endinterface

interface FIFOF_O #(type t);
   method t       first ();
   method Action  deq ();
   method Bool    notEmpty ();
endinterface

// ================================================================
// Converters from FIFOF

function FIFOF_I #(t) to_FIFOF_I (FIFOF #(t) f);
   return interface FIFOF_I;
	     method enq (x) = f.enq (x);
	     method notFull = f.notFull;
	  endinterface;
endfunction

function FIFOF_O #(t) to_FIFOF_O (FIFOF #(t) f);
   return interface FIFOF_O;
	     method first    = f.first;
	     method deq      = f.deq;
	     method notEmpty = f.notEmpty;
	  endinterface;
endfunction

// ================================================================
// Connections

// ----------------
// FIFOF_O to FIFOF_I

instance Connectable #(FIFOF_O #(t), FIFOF_I #(t));
   module mkConnection #(FIFOF_O #(t) fo, FIFOF_I #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ----------------
// FIFOF_I to FIFOF_O

instance Connectable #(FIFOF_I #(t), FIFOF_O #(t));
   module mkConnection #(FIFOF_I #(t) fi, FIFOF_O #(t) fo) (Empty);
      mkConnection (fo, fi);
   endmodule
endinstance

// ----------------
// FIFOF_O to FIFOF

instance Connectable #(FIFOF_O #(t), FIFOF #(t));
   module mkConnection #(FIFOF_O #(t) fo, FIFOF #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ----------------
// FIFOF to FIFOF_I

instance Connectable #(FIFOF #(t), FIFOF_I #(t));
   module mkConnection #(FIFOF #(t) fo, FIFOF_I #(t) fi) (Empty);
      rule rl_connect;
	 fi.enq (fo.first);
	 fo.deq;
      endrule
   endmodule
endinstance

// ================================================================
// Convenience function combining first/enq

function ActionValue #(t) pop_o (FIFOF_O #(t) f);
   actionvalue
      f.deq;
      return f.first;
   endactionvalue
endfunction

// ================================================================
// Dummy tie-off interfaces

// dummy_FIFO_I that never accepts anything (always "full")

FIFOF_I #(t) dummy_FIFOF_I = interface FIFOF_I;
				method Action enq (x) if (False);
				   noAction;
				endmethod
				method notFull;
				   return False;
				endmethod
			     endinterface;

// Dummy FIFO_O that never yields anything (always "empty")

FIFOF_O #(t) dummy_FIFOF_O = interface FIFOF_O;
				method first () if (False);
				   return ?;
				endmethod

				method Action deq () if (False);
				   noAction;
				endmethod

				method notEmpty;
				   return False;
				endmethod
			     endinterface;

// ================================================================

endpackage
