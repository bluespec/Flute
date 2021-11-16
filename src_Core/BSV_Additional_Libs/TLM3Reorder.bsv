// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3Reorder;

import CB::*;
import CBus::*;
import FIFO::*;
import FShow::*;
import GetPut::*;
import TLM3Defines::*;
import TokenBuffer::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface TLMReorder#(numeric type m, numeric type ln, `TLM_RR_DCL);
   interface TLMRecvIFC#(req_t, resp_t) in;
   interface TLMSendIFC#(req_t, resp_t) out;
endinterface

typedef TLMReorder#(TExp#(ld), TMax#(8, TAdd#(lm, ld)), `TLM_RR)
        TLMReorderBuffer#(numeric type lm, numeric type ld, `TLM_RR_DCL);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkTLMReorder1 (TLMReorder#(1, ln, `TLM_RR))
   provisos (Bits#(req_t, s0),
	     Bits#(resp_t, s1));

   FIFO#(req_t ) fifo_req  <- mkLFIFO;
   FIFO#(resp_t) fifo_resp <- mkLFIFO;

   interface TLMRecvIFC in;
      interface Get tx = toGet(fifo_resp);
      interface Put rx = toPut(fifo_req);
   endinterface

   interface TLMSendIFC out;
      interface Get tx = toGet(fifo_req);
      interface Put rx = toPut(fifo_resp);
   endinterface

endmodule

module mkTLMReorderP (TLMReorder#(m, ln, `TLM_RR))
   provisos (Bits#(req_t, s0),
	     Bits#(resp_t, s1),
	     TLMRequestTC#(req_t, `TLM_PRM),
	     TLMResponseTC#(resp_t, `TLM_PRM),
	     FShow#(resp_t));

   CompletionBuffer#(ln,  TLMId#(`TLM_PRM), resp_t) cb     <- mkCompletionBuffer;
   TokenBuffer#(m, ln, TLMId#(`TLM_PRM))            buffer <- mkTokenBuffer;
   FIFO#(req_t )                                    fifo_out_tx   <- mkLFIFO;
   FIFO#(resp_t)                                    response_fifo <- mkSizedFIFO(10); //FIX THIS!!!!!!

   rule send_reservations;
      let t <- cb.reserve.get;
      buffer.store(t);
//      $display("(%0d) Sending reservation: ", $time, fshow(t));
   endrule

   rule complete_responses;
      let response = response_fifo.first;
      response_fifo.deq;
      let token <- buffer.get;
      cb.complete.put(tuple2(token, response));
   endrule

   interface TLMRecvIFC in;
      interface Get tx;
	 method ActionValue#(resp_t) get;
	    let response <- cb.drain.get;
	    return response;
	 endmethod
      endinterface

      interface Put rx;
	 method Action put(req_t request);
	    if (toTLMRequest(request) matches tagged Descriptor .d
		&&& d.command == WRITE)
	       begin
		  TokenRequest#(TLMId#(`TLM_PRM), ln) tr = TokenRequest { label: d.transaction_id, size: 0};
		  cb.request.put(tr);
//		  $display("(%0d) Sending CB request: ", $time, fshow(tr));
	       end
	    if (toTLMRequest(request) matches tagged Descriptor .d
		&&& d.command == READ)
	       begin
		  TokenRequest#(TLMId#(`TLM_PRM), ln) tr = TokenRequest { label: d.transaction_id, size: extendNP(pack(d.b_length))};
		  cb.request.put(tr);
//		  $display("(%0d) Sending CB request: ", $time, fshow(tr));
	       end
	    fifo_out_tx.enq(request);
	 endmethod
      endinterface
   endinterface

   interface TLMSendIFC out;
      interface Get tx = toGet(fifo_out_tx);
      interface Put rx;
	 method Action put(resp_t response);
	    TLMResponse#(`TLM_PRM) tlm_resp = toTLMResponse(response);
//	    $display("(%0d) [%0d] Response is (PRE): ", $time, id, fshow(tlm_resp));
//	    $display("(%0d) Response: ", $time, fshow(tlm_resp));
//	    first <= toTLMResponse(response).is_last;
	    TokenRequest#(TLMId#(`TLM_PRM), ln) tr = TokenRequest { label: tlm_resp.transaction_id, size: 0};
	    buffer.request(tagged Request tr);
//	    $display("(%0d) Sending buffer request: ", $time, fshow(tr));
	    response_fifo.enq(response);
	 endmethod
      endinterface
   endinterface

endmodule

endpackage