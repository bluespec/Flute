// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package CB;

import CBus::*;
import Counter::*;
import FIFO::*;
import FShow::*;
import GetPut::*;
import Vector::*;
import RegFile::*;

////////////////////////////////////////////////////////////////////////////////
/// A modified version of the completion buffer for use with
/// out-of-order response streams
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {TokenRequest#(a, ln) Request;
		      TokenRange#(a, ln)   Range;
		      } TokenRR#(type a, numeric type ln) deriving(Eq, Bits, Bounded);

typedef struct {a          label;
		Bit#(ln)   token;
		Bit#(ln)   size;
		} TokenRange#(type a, numeric type ln) deriving (Eq, Bits, Bounded);

typedef struct {a          label;
		Bit#(ln)   size;
		} TokenRequest#(type a, numeric type ln) deriving (Eq, Bits, Bounded);

instance FShow#(TokenRange#(a, ln))
   provisos(Bits#(a, sa));
   function Fmt fshow (TokenRange#(a, ln) t);
      return $format("<TokenRange Label: %0d Token: %0d Size: %0d >", t.label, t.token, t.size);
   endfunction
endinstance

instance FShow#(TokenRequest#(a, ln))
   provisos(Bits#(a, sa));
   function Fmt fshow (TokenRequest#(a, ln) t);
      return $format("<TokenRequest Label: %0d Size: %0d >", t.label, t.size);
   endfunction
endinstance

instance FShow#(TokenRR#(a, ln))
   provisos(Bits#(a, sa));
   function Fmt fshow (TokenRR#(a, ln) rr);
      Fmt f = ?;
      if (rr matches tagged Range .r)
	 f = fshow(r);
      if (rr matches tagged Request .r)
	 f = fshow(r);
      return f;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface CompletionBuffer #(numeric type ln, type a, type b);
   // Request "size" tokens with Id "label"
   method Put#(TokenRequest#(a, ln)) request;
   // Grab the requested token range
   method Get#(TokenRange#(a, ln))   reserve;
   // Put (one at a time) a token/object pair;
   method Put#(Tuple2#(Bit#(ln), b)) complete;
   // Get the next ordered object (blocks until available)
   method Get#(b) drain();
endinterface

module mkCompletionBuffer(CompletionBuffer#(ln, a, b))
   provisos (Bits#(a, sa), Bits#(b, sb), Add#(1, ln, ln1), FShow#(b));

   let hi = fromInteger(valueOf(TExp#(ln)) - 1);

   Reg#(Bool) initialized <- mkReg(False);

   RegFile#(Bit#(ln), Maybe#(b)) bufx <- mkRegFileWCF(0, hi);
   Reg#(Bit#(ln))                i    <- mkReg(0);
   Reg#(Bit#(ln))                o    <- mkReg(0);
   Reg#(Bit#(ln))                oi   <- mkReg(0);
   Reg#(Bit#(ln))                f    <- mkReg(0);
   Counter#(ln1)                 n    <- mkCounter(0);

   FIFO#(TokenRequest#(a, ln)) request_fifo <- mkLFIFO;

   Bool up_to_date = (oi == f || initialized);

   rule initialize (!up_to_date);
      bufx.upd(oi, tagged Invalid);
      oi <= oi + 1;
//      $display("(%0d) INIT: %d ", $time, oi);
      initialized <= oi == hi;
   endrule

   interface Put request;
      method Action put (value);
	 f <= f + value.size + 1;
	 request_fifo.enq(value);
      endmethod
   endinterface

   interface Get reserve;
      method get() if ((n.value + extendNP(request_fifo.first.size) + 1) <= hi && up_to_date) ;
	 actionvalue
	    i <= i + request_fifo.first.size + 1;
	    request_fifo.deq;
	    n.inc(extendNP(request_fifo.first.size) + 1);
	    return TokenRange { label: request_fifo.first.label, token: i, size: request_fifo.first.size};
	 endactionvalue
      endmethod
   endinterface

   interface Put complete;
      method Action put (pair) if (up_to_date);
	 match {.t,.a} = pair;
	 bufx.upd(t, tagged Valid a);
//	 $display("(%0d) FILLING: %d ", $time, t, fshow(a));
    endmethod
   endinterface

  interface Get drain;
    method get() if (bufx.sub(o) matches tagged Valid .x &&& n.value > 0 &&& up_to_date);
       actionvalue
//	  $display("(%0d) DRAINING: %d prev: ", $time, o, fshow(x));
       	  bufx.upd(o, tagged Invalid);
	  o <= o + 1;
	  n.down;
	return(x);
      endactionvalue
    endmethod
  endinterface

endmodule

endpackage

