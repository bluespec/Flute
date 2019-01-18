// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved.

// EdgeFIFOFs are 1-element FIFOFs used as generic interfaces by IPs
// where they connect to interconnect fabrics.  The overall interface
// is a standard FIFOF interface.  The IP-side is guarded.  The
// Fabric-side is unguarded, so that it's easy to attach transactors
// for some particular bus interface such as AXI4-Lite, AHB-Lite, etc.


package EdgeFIFOFs;

// ================================================================
// BSV library imports

import FIFOF :: *;

// ================================================================
// FIFOFs for Master IPs.
// enq (IP-side) is guarded, deq (Fabric-side) is not.

module mkMaster_EdgeFIFOF (FIFOF #(t))
   provisos (Bits #(t, tsz));

   Integer port_deq   = 0;
   Integer port_enq   = 1;
   Integer port_clear = 2;

   Array #(Reg #(Bool)) crg_full   <- mkCReg (3, False);
   Reg #(t)             rg_payload <- mkRegU;

   // ----------------
   // Clear

   method Action clear;
      crg_full [port_clear] <= False;
   endmethod

   // ----------------
   // Enq side (IP-side; guarded)

   method Bool notFull ();
      return (! crg_full [port_enq]);
   endmethod

   method Action  enq (t x) if (! crg_full [port_enq]);
      crg_full [port_enq] <= True;
      rg_payload          <= x;
   endmethod

   // ----------------
   // Deq side (Fabric-side; unguarded)

   method Bool notEmpty ();
      return crg_full [port_deq];
   endmethod

   method t first ();    // unguarded
      return rg_payload;
   endmethod

   method Action deq ();    // unguarded
      crg_full [port_deq] <= False;
   endmethod

endmodule

// ================================================================
// For Slave IPs.
// enq (Fabric-side) is unguarded, deq (IP-side) is guarded.

module mkSlave_EdgeFIFOF (FIFOF #(t))
   provisos (Bits #(t, tsz));

   Integer port_deq   = 0;
   Integer port_enq   = 1;
   Integer port_clear = 2;

   Array #(Reg #(Bool)) crg_full   <- mkCReg (3, False);
   Reg #(t)             rg_payload <- mkRegU;

   // ----------------
   // Clear

   method Action clear;
      crg_full [port_clear] <= False;
   endmethod

   // ----------------
   // Enq side (IP-side; unguarded)

   method Bool notFull ();
      return (! crg_full [port_enq]);
   endmethod

   method Action  enq (t x);    // unguarded
      crg_full [port_enq] <= True;
      rg_payload          <= x;
   endmethod

   // ----------------
   // Deq side (Fabric-side; guarded)

   method Bool notEmpty ();
      return crg_full [port_deq];
   endmethod

   method t first () if (crg_full [port_deq]);
      return rg_payload;
   endmethod

   method Action deq () if (crg_full [port_deq]);
      crg_full [port_deq] <= False;
   endmethod

endmodule

// ================================================================

endpackage
