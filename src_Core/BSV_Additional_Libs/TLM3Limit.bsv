// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3Limit;

import FIFO::*;
import GetPut::*;
import TLM3Defines::*;


`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
/// log_max indicates the max length burst the associated fabric can handle
/// (log_max == 4 for instance indicates a max burst of 16 etc). The mkTLMLimitP
/// module breaks long bursts into shorter ones to satisfy the
/// limit. A log_max < 0 indicates there is no limit (i.e. the burst
/// is passed through unmodified).
////////////////////////////////////////////////////////////////////////////////

module mkTLMLimitP#(Integer log_max, parameter UInt#(32) max_flight) (TLMTransformIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1));

   TLMTransformIFC#(req_t, resp_t) _ifc = ?;
   if (log_max < 0) 
      _ifc <- mkTLMUnlimited;
   else
      _ifc <- mkTLMLimitPInternal(log_max, max_flight);
   return _ifc;
endmodule   

module mkTLMLimitPInternal#(Integer log_max, parameter UInt#(32) max_flight) (TLMTransformIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1));

   let depth = max_flight + max_flight; // 2 x max_flight

   SFIFO#(Bool, TLMBLength#(`TLM_PRM)) length_fifo <- mkSafeDepthParamSFIFO(depth);

   FIFO#(req_t)             f_rx;

   if (log_max == 0)
      f_rx <- mkTLMLimitRequestFIFO0(fromSFIFO(length_fifo));
   else
      f_rx <- mkTLMLimitRequestFIFO(log_max, fromSFIFO(length_fifo));
   FIFO#(resp_t)            f_tx   <- mkLFIFO;
   Reg#(Bool)               first  <- mkReg(True);

   interface TLMRecvIFC in;
      interface Get tx = toGet(f_tx);
      interface Put rx = toPut(f_rx);
   endinterface

   interface TLMSendIFC out;
      interface Get tx = toGet(f_rx);
      interface Put rx;
	 method Action put (value);
	    let tlm_response = toTLMResponse(value);
	    resp_t rtn = value;
	    first <= tlm_response.is_last;
	    if (tlm_response.is_last)
	       begin
		  let keep = length_fifo.first;
		  length_fifo.deq;
		  if (!keep)
		     begin
			tlm_response.is_last = False;
			rtn = fromTLMResponse(tlm_response);
		     end
	       end
	    if (tlm_response.command == READ || tlm_response.is_last)
	       f_tx.enq(rtn);
	 endmethod
      endinterface
   endinterface
endmodule

module mkTLMUnlimited (TLMTransformIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
   	    Bits#(resp_t, s1));

   FIFO#(req_t)             f_rx   <- mkLFIFO;
   FIFO#(resp_t)            f_tx   <- mkLFIFO;

   interface TLMRecvIFC in;
      interface Get tx = toGet(f_tx);
      interface Put rx = toPut(f_rx);
   endinterface

   interface TLMSendIFC out;
      interface Get tx = toGet(f_rx);
      interface Put rx = toPut(f_tx);
   endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkTLMLimitRequestFIFO#(Integer log_max,
			      FIFO#(Bool) length_fifo) (FIFO#(a))
   provisos (Bits#(a, sa),
	     TLMRequestTC#(a, `TLM_PRM));

   Reg#(Maybe#(a))             writeReg <- mkReg(tagged Invalid);
   Reg#(Maybe#(a))             readReg  <- mkReg(tagged Invalid);
   FIFO#(a)                    fifo     <- mkLFIFO;
   Reg#(TLMBLength#(`TLM_PRM)) count    <- mkReg(0);
   Reg#(TLMBLength#(`TLM_PRM)) x_count  <- mkReg(0);

   TLMBLength#(`TLM_PRM) length_max = 1 << fromInteger(log_max);

   rule add_read_requests (readReg matches tagged Valid .r
			   &&& toTLMRequest(r) matches tagged Descriptor .d);
      if (x_count == 1)
	 begin
	    fifo.enq(r);
	    length_fifo.enq(True);
	    readReg <= tagged Invalid;
	 end
      else
	 begin
	    let dd = d;
	    dd.b_length = length_max - 1;
	    fifo.enq(fromTLMRequest(tagged Descriptor dd));
	    length_fifo.enq(False);
	    dd = incrTLMAddrN(log_max, alignAddress(d));
	    readReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	    x_count <= x_count - 1;
	 end
   endrule

   method Action enq(request) if (readReg matches tagged Invalid);
      if (toTLMRequest(request) matches tagged Descriptor .d
	 &&& d.command == READ)
	 begin
	    Bool x_long = (d.b_length >> log_max) != 0;
	    if (x_long && (d.burst_mode == INCR || d.burst_mode == CNST))
	       begin
		  x_count <= d.b_length >> log_max;
		  let rest = d.b_length << (valueOf(length_size) - log_max);
		  rest = rest >> (valueOf(length_size) - log_max);
		  let dd = d;
		  dd.b_length = length_max - 1;
		  fifo.enq(fromTLMRequest(tagged Descriptor dd));
		  length_fifo.enq(False);
		  dd.b_length = rest;
		  dd = incrTLMAddrN(log_max, alignAddress(dd));
		  readReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	       end
	    else
	       begin
		  length_fifo.enq(True);
		  fifo.enq(request);
	       end
	 end
      if (toTLMRequest(request) matches tagged Descriptor .d
	 &&& d.command == WRITE)
	 begin
	    Bool x_long = (d.b_length >> log_max) != 0;
	    if (x_long && (d.burst_mode == INCR || d.burst_mode == CNST))
	       begin
		  x_count <= d.b_length >> log_max;
		  let rest = d.b_length << (valueOf(length_size) - log_max);
		  rest = rest >> (valueOf(length_size) - log_max);
		  let dd = d;
		  dd.b_length = length_max - 1;
		  count <= dd.b_length;
		  fifo.enq(fromTLMRequest(tagged Descriptor dd));
		  length_fifo.enq(False);
		  dd.b_length = rest;
		  dd = incrTLMAddrN(log_max, alignAddress(dd));
		  writeReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	       end
	    else
	       begin
		  count <= d.b_length;
		  fifo.enq(request);
		  length_fifo.enq(True);
	       end
	 end
      if (toTLMRequest(request) matches tagged Data .dr
	  &&& count == 0
	  &&& writeReg matches tagged Valid .w
	  &&& toTLMRequest(w) matches tagged Descriptor .d)
	 if (x_count == 1)
	    begin
	       let dd = d;
	       dd.data = dr.data;
	       dd.user = dr.user;
	       dd.byte_enable = dr.byte_enable;
	       count <= dd.b_length;
	       fifo.enq(fromTLMRequest(tagged Descriptor dd));
	       length_fifo.enq(True);
	       writeReg <= tagged Invalid;
	    end
	 else
	    begin
	       let dd = d;
	       dd.b_length = length_max - 1;
	       dd.data = dr.data;
	       dd.user = dr.user;
	       dd.byte_enable = dr.byte_enable;
	       count <= dd.b_length;
	       fifo.enq(fromTLMRequest(tagged Descriptor dd));
	       length_fifo.enq(False);
	       dd = incrTLMAddrN(log_max, d);
	       writeReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	       x_count <= x_count - 1;
	    end
      if (toTLMRequest(request) matches tagged Data .dr
	  &&& count != 0)
	 begin
	    count <= count - 1;
	    fifo.enq(request);
	 end
   endmethod
   method first = fifo.first;
   method deq   = fifo.deq;
   method clear = fifo.clear;

endmodule


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkTLMLimitRequestFIFO0#(FIFO#(Bool) length_fifo) (FIFO#(a))
   provisos (Bits#(a, sa),
	     TLMRequestTC#(a, `TLM_PRM));

   Reg#(Maybe#(a))             writeReg <- mkReg(tagged Invalid);
   Reg#(Maybe#(a))             readReg  <- mkReg(tagged Invalid);
   FIFO#(a)                    fifo     <- mkLFIFO;
   Reg#(TLMBLength#(`TLM_PRM)) count    <- mkReg(0);

   rule add_read_requests (readReg matches tagged Valid .r
			   &&& toTLMRequest(r) matches tagged Descriptor .d);
      let dd = d;
      dd.b_length = 0;
      fifo.enq(fromTLMRequest(tagged Descriptor dd));
      if (count == 0)
	 begin
	    length_fifo.enq(True);
	    readReg <= tagged Invalid;
	 end
      else
	 begin
	    length_fifo.enq(False);
	    dd = incrTLMAddr(alignAddress(d));
	    readReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	    count <= count - 1;
	 end
   endrule

   method Action enq(request) if (readReg matches tagged Invalid);
      if (toTLMRequest(request) matches tagged Descriptor .d
	 &&& d.command == READ)
	 if (d.b_length != 0)
	    begin
	       let remaining = d.b_length - 1;
	       count <= remaining;
	       let dd = d;
	       dd.b_length = 0;
	       fifo.enq(fromTLMRequest(tagged Descriptor dd));
	       length_fifo.enq(False);
	       dd = incrTLMAddr(alignAddress(d));
	       readReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	    end
	 else
	    begin
	       length_fifo.enq(True);
	       fifo.enq(request);
	    end
      if (toTLMRequest(request) matches tagged Descriptor .d
	 &&& d.command == WRITE)
	 if (d.b_length != 0)
	    begin
	       let remaining = d.b_length - 1;
	       count <= remaining;
	       let dd = d;
	       dd.b_length = 0;
	       fifo.enq(fromTLMRequest(tagged Descriptor dd));
	       length_fifo.enq(False);
	       dd = incrTLMAddr(alignAddress(d));
	       writeReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	    end
	 else
	    begin
	       fifo.enq(request);
	       length_fifo.enq(True);
	    end
      if (toTLMRequest(request) matches tagged Data .dr
	  &&& writeReg matches tagged Valid .w
	  &&& toTLMRequest(w) matches tagged Descriptor .d)
	 begin
	    let dd = d;
	    dd.data = dr.data;
	    dd.user = dr.user;
	    dd.byte_enable = dr.byte_enable;
	    dd.b_length = 0;
	    fifo.enq(fromTLMRequest(tagged Descriptor dd));
	    if (count == 0)
	       begin
		  length_fifo.enq(True);
		  writeReg <= tagged Invalid;
	       end
	    else
	       begin
		  length_fifo.enq(False);
		  count <= count - 1;
		  dd = incrTLMAddr(alignAddress(d));
		  writeReg <= tagged Valid (fromTLMRequest(tagged Descriptor dd));
	       end
	 end
   endmethod
   method first = fifo.first;
   method deq   = fifo.deq;
   method clear = fifo.clear;

endmodule

endpackage