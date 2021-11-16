// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TokenBuffer;

import CB::*;
import Vector::*;

import FIFO::*;
import FIFOF::*;
import FShow::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface TokenBuffer#(numeric type m, numeric type ln, type a);
   method Action                    store   (TokenRange#(a, ln) value);
   method Action                    request (TokenRR#(a, ln) value);
   method ActionValue#(Bit#(ln))    get;
endinterface

module mkTokenBuffer (TokenBuffer#(m, ln, a))
   provisos (Bits#(a, sa), Eq#(a), Add#(m, 1, m1));

   let size = valueOf(m);

   Reg#(Vector#(m, Maybe#(TokenRange#(a, ln)))) store_buf <- mkReg(replicate(tagged Invalid));
   Reg#(Vector#(m1, Maybe#(TokenRR#(a, ln))))   req_buf   <- mkReg(replicate(tagged Invalid));

   Vector#(m,  Maybe#(TokenRange#(a, ln))) store_buf_0 = ?;
   Vector#(m1, Maybe#(TokenRR#(a, ln))) req_buf_0  = ?;
   Vector#(m,  Maybe#(TokenRange#(a, ln))) store_buf_1 = ?;
   Vector#(m1, Maybe#(TokenRR#(a, ln))) req_buf_1  = ?;
   Vector#(m,  Maybe#(TokenRange#(a, ln))) store_buf_2 = ?;
   Vector#(m1, Maybe#(TokenRR#(a, ln))) req_buf_2  = ?;

   Wire#(Vector#(m1, Maybe#(TokenRR#(a, ln)))) req_wire_0  <- mkBypassWire;
   Wire#(Vector#(m1, Maybe#(TokenRR#(a, ln)))) req_wire_1  <- mkBypassWire;
   Wire#(Vector#(m1, Maybe#(TokenRR#(a, ln)))) req_wire_2  <- mkBypassWire;

   Wire#(Maybe#(TokenRR#(a, ln)))               req_wire   <- mkDWire(tagged Invalid);
   Wire#(Maybe#(TokenRange#(a, ln)))            store_wire <- mkDWire(tagged Invalid);

   Reg#(Maybe#(TokenRR#(a, ln)))                req_last   <- mkDWire(req_buf[size]);

   Reg#(Bool) stored <- mkReg(False);
   PulseWire                                    store_pw   <- mkPulseWire;

   Bool room_for_req  = False;
   if (req_buf[size] matches tagged Invalid)
      if (req_buf[size - 1] matches tagged Valid (tagged Request .r))
	 room_for_req  = False;
      else
	 room_for_req  = True;

   Bool room_for_str  = !isValid(store_buf[0]);

   ////////////////////////////////////////////////////////////////////////////////
   /// First calculate updates based on matches between the store buffer and the
   /// request buffer.
   ////////////////////////////////////////////////////////////////////////////////

   req_buf_0[size] = req_last;
   for (Integer x = 0; x < size; x = x + 1)
      begin
	 store_buf_0[x]  = store_buf[x];
	 req_buf_0[x]    = req_buf[x];
	 if (req_buf[x] matches tagged Valid .r)
	    if (store_buf[x] matches tagged Valid .s)
	       if (r matches tagged Request .rq
		   &&& rq.label == s.label)
		  if (rq.size == 0)
		     begin
			let ns = s;
			ns.size  = s.size  - 1;
			ns.token = s.token + 1;
			store_buf_0[x]  = (s.size == 0) ? tagged Invalid : tagged Valid ns;
			req_buf_0[x]    = tagged Valid (tagged Range TokenRange {label: rq.label, token: s.token, size: 0});
		     end
		  else
		     begin
			store_buf_0[x]  = tagged Invalid;
			req_buf_0[x]  = tagged Valid (tagged Range s);
//			req_buf_0[x]  = tagged Valid (tagged Range TokenRange {label: rq.label, token: s.token, size: s.size});
		     end
      end


   req_buf_1[size] = req_buf_0[size];
   store_buf_1[0]  = store_buf_0[0];
   for (Integer x = 0; x < size; x = x + 1)
      begin
	 req_buf_1[x]    = req_buf_0[x];
	 if (x < (size - 1))
	    begin
	       store_buf_1[x + 1]  = store_buf_0[x + 1];
	       if (req_buf_0[x] matches tagged Valid .r)
		  if (store_buf_0[x + 1] matches tagged Valid .s)
		     if (r matches tagged Request .rq
			 &&& rq.label == s.label)
			if (rq.size == 0)
			   begin
			      let ns = s;
			      ns.size  = s.size  - 1;
			      ns.token = s.token + 1;
			      store_buf_1[x + 1]  = (s.size == 0) ? tagged Invalid : tagged Valid ns;
			      req_buf_1[x]    = tagged Valid (tagged Range TokenRange {label: rq.label, token: s.token, size: 0});
			   end
			else
			   begin
			      store_buf_1[x + 1]  = tagged Invalid;
			      req_buf_1[x]  = tagged Valid (tagged Range s);
//			      req_buf_1[x]  = tagged Valid (tagged Range TokenRange {label: rq.label, token: s.token, size: s.size});
			   end
	    end
      end


   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   if (room_for_req)
      begin
	 req_buf_2[0] = req_wire;
	 for (Integer x = 1; x <= size; x = x + 1)
	    begin
	       req_buf_2[x] = req_buf_1[x - 1];
	    end
      end
   else
      for (Integer x = 0; x <= size; x = x + 1)
	 begin
	    req_buf_2[x] = req_buf_1[x];
	 end

   if (store_pw)
      begin
	 store_buf_2[size - 1] = store_wire;
	 for (Integer x = 0; x < (size - 1); x = x + 1)
	    begin
	       store_buf_2[x] = store_buf_1[x + 1];
	    end
      end
   else
      for (Integer x = 0; x < size; x = x + 1)
	 begin
	    store_buf_2[x] = store_buf_1[x];
	 end

   rule update_state;
      store_buf <= store_buf_2;
      req_buf <= req_buf_2;
   endrule

   rule zzz;
      req_wire_0 <= req_buf_0;
      req_wire_1 <= req_buf_1;
      req_wire_2 <= req_buf_2;
   endrule


   method Action store (TokenRange#(a, ln) value) if (room_for_str);
      stored <= True;
      store_pw.send;
      store_wire <= tagged Valid value;
   endmethod

   method Action request (value) if (room_for_req && stored);
      req_wire <= tagged Valid value;
   endmethod

   method ActionValue#(Bit#(ln)) get if (req_buf[size] matches tagged Valid (tagged Range .r));
      let n = r;
      n.token = r.token + 1;
      n.size  = r.size - 1;
      req_last <= (r.size == 0) ? tagged Invalid : tagged Valid (tagged Range n);
      return r.token;
   endmethod

endmodule



endpackage