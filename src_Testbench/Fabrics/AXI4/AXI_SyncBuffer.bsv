// Copyright (c) 2021 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI_SyncBuffer;

// ================================================================
// This package defines an clock-domain-crossing buffer for an AXI bus
// (parameterized, so can be used for AXI4 and AXI4 Lite).

// The interfaces are FIFOF-like (not raw AXI4 signals).
// The module merely contains 5 SyncFIFOs for each of the 5 AXI4 or
// AXI4 Lite channels

// ================================================================
// Bluespec library imports

import Clocks      :: *;
import Connectable :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

// -- none

// ================================================================
// The interface for the fabric module

interface AXI_M_IFC #(type aw, type w, type b, type ar, type r);
   interface FIFOF_O #(aw)  o_aw;
   interface FIFOF_O #(w)   o_w;
   interface FIFOF_I #(b)   i_b;
   interface FIFOF_O #(ar)  o_ar;
   interface FIFOF_I #(r)   i_r;
endinterface

interface AXI_S_IFC #(type aw, type w, type b, type ar, type r);
   interface FIFOF_I #(aw)  i_aw;
   interface FIFOF_I #(w)   i_w;
   interface FIFOF_O #(b)   o_b;
   interface FIFOF_I #(ar)  i_ar;
   interface FIFOF_O #(r)   o_r;
endinterface

instance Connectable #(AXI_M_IFC #(aw, w, b, ar, r),
		       AXI_S_IFC #(aw, w, b, ar, r));

   module mkConnection #(AXI_M_IFC #(aw, w, b, ar, r) m,
			 AXI_S_IFC #(aw, w, b, ar, r) s)  (Empty);
      mkConnection (m.o_aw, s.i_aw);
      mkConnection (m.o_w,  s.i_w);
      mkConnection (m.i_b,  s.o_b);
      mkConnection (m.o_ar, s.i_ar);
      mkConnection (m.i_r,  s.o_r);
   endmodule
endinstance

// ================================================================
// The SyncBuffer interface

interface AXI_SyncBuffer_IFC #(type aw, type w, type b, type ar, type r);
   interface AXI_S_IFC #(aw, w, b, ar, r) from_M;
   interface AXI_M_IFC #(aw, w, b, ar, r) to_S;
endinterface

// ================================================================
// The SyncBuffer module
// Implements an AXI (AXI4 or AXI4 Lite) clock-crossing

module mkAXI_SyncBuffer #(Integer depth,
			  Clock sClkIn, Reset sRstIn,
			  Clock dClkIn, Reset dRstIn)
                        (AXI_SyncBuffer_IFC #(aw, w, b, ar, r))
   provisos (Bits #(aw, _size_aw_t),
	     Bits #(w,  _size_w_t),
	     Bits #(b,  _size_b_t),
	     Bits #(ar, _size_ar_t),
	     Bits #(r,  _size_r_t));

   SyncFIFOIfc #(aw) f_aw <- mkSyncFIFO (depth, sClkIn, sRstIn, dClkIn); // enq|=>|deq
   SyncFIFOIfc #(w)  f_w  <- mkSyncFIFO (depth, sClkIn, sRstIn, dClkIn); // enq|=>|deq
   SyncFIFOIfc #(b)  f_b  <- mkSyncFIFO (depth, dClkIn, dRstIn, sClkIn); // deq|<=|enq

   SyncFIFOIfc #(ar) f_ar <- mkSyncFIFO (depth, sClkIn, sRstIn, dClkIn); // enq|=>|deq
   SyncFIFOIfc #(r)  f_r  <- mkSyncFIFO (depth, dClkIn, dRstIn, sClkIn); // deq|<=|enq

   // ----------------------------------------------------------------
   // Help functions

   function FIFOF_I #(t) syncFIFO_to_FIFOF_I (SyncFIFOIfc #(t) sf);
      return interface FIFOF_I;
		method Action enq (t x) = sf.enq (x);
		method Bool notFull     = sf.notFull;
	     endinterface;
   endfunction

   function FIFOF_O #(t) syncFIFO_to_FIFOF_O (SyncFIFOIfc #(t) sf);
      return interface FIFOF_O;
		method t      first    = sf.first;
		method Action deq      = sf.deq;
		method Bool   notEmpty = sf.notEmpty;
	     endinterface;
   endfunction

   // ----------------------------------------------------------------
   // INTERFACE

   interface from_M = interface AXI_S_IFC;
			 interface FIFOF_I i_aw = syncFIFO_to_FIFOF_I (f_aw);
			 interface FIFOF_I i_w  = syncFIFO_to_FIFOF_I (f_w);
			 interface FIFOF_O o_b  = syncFIFO_to_FIFOF_O (f_b);
			 interface FIFOF_I i_ar = syncFIFO_to_FIFOF_I (f_ar);
			 interface FIFOF_O o_r  = syncFIFO_to_FIFOF_O (f_r);
		      endinterface;

   interface to_S   = interface AXI_M_IFC;
			 interface FIFOF_O o_aw = syncFIFO_to_FIFOF_O (f_aw);
			 interface FIFOF_O o_w  = syncFIFO_to_FIFOF_O (f_w);
			 interface FIFOF_I i_b  = syncFIFO_to_FIFOF_I (f_b);
			 interface FIFOF_O o_ar = syncFIFO_to_FIFOF_O (f_ar);
			 interface FIFOF_I i_r  = syncFIFO_to_FIFOF_I (f_r);
		      endinterface;
endmodule

// ================================================================

endpackage
