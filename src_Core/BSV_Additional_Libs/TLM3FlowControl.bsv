// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3FlowControl;

import TLM3Defines::*;
import FIFOF::*;
import GetPut::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass FlowPayload#(type a);
   function Bool lastOne  (a payload);
   function Bool canStall(a payload);
endtypeclass

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance FlowPayload#(TLMRequest#(`TLM_PRM));
   function Bool lastOne (TLMRequest#(`TLM_PRM) request);
      let is_last = False;
      if (request matches tagged Descriptor .d)
	 is_last = d.b_length == 0 || d.command == READ;
      if (request matches tagged Data .d)
	 is_last = d.is_last;
      return is_last;
   endfunction
   function Bool canStall (TLMRequest#(`TLM_PRM) request);
      let can_stall = False;
      if (request matches tagged Descriptor .d)
	 can_stall = d.cntrl_flow && d.command == WRITE;
      if (request matches tagged Data .d)
	 can_stall = False;
      return can_stall;
   endfunction
endinstance

instance FlowPayload#(TLMResponse#(`TLM_PRM));
   function Bool lastOne (TLMResponse#(`TLM_PRM) response);
      return response.is_last;
   endfunction
   function Bool canStall (TLMResponse#(`TLM_PRM) response);
      return False;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface TLMFlow#(type a);
   interface Get#(a) tx;
   interface Put#(a) rx;
endinterface

module mkTLMFlow#(parameter UInt#(32) depth) (TLMFlow#(a))
   provisos(FlowPayload#(a), Bits#(a, sa));

   let d = max(3, depth);

   FIFOF#(a) fifo <- mkSafeDepthParamFIFOF(d);

   FIFOF#(void)  cntr <- mkSafeDepthParamFIFOF(paramLog(d));

   Bool can_deq = !canStall(fifo.first) || !fifo.notFull || cntr.notEmpty || depth == 0;

   interface Get tx;
      method ActionValue#(a) get if (can_deq);
	 if (lastOne(fifo.first)) cntr.deq;
	 fifo.deq;
	 return fifo.first;
      endmethod
   endinterface

   interface Put rx;
      method Action put (a value);
	 fifo.enq(value);
	 if (lastOne(value)) cntr.enq(?);
      endmethod
   endinterface

endmodule

function UInt#(32) paramLog (UInt#(32) n);
   UInt#(32) result = ?;
   if (n <= 4)          result = (2 + 1);
   else if (n <= 16)    result = (4 + 1);
   else if (n <= 256)   result = (8 + 1);
   else if (n <= 65536) result = (16 + 1);
   else                 result = 32;
   return result;
endfunction

endpackage
