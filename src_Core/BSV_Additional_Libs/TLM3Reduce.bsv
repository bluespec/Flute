// Copyright (c) 2007--2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3Reduce;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import CBus::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import SpecialFIFOs::*;
import TLM3Defines::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
/// Convert a stream of (arbitrary) TLM ops into a stream with only single
/// reads and single writes.
////////////////////////////////////////////////////////////////////////////////

module mkTLMReducer (TLMTransformIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1));

   let _ifc <- mkTLMReducerP(False, False);
   return _ifc;
endmodule

module mkTLMReducerP#(Bool all_responses,
		      Bool big_endian) (TLMTransformIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(1, SizeOf#(TLMBLength#(`TLM_PRM)), n));

   Reg#(req_t)          req_reg      <- mkReg(?);
   Reg#(TLMBLength#(`TLM_PRM)) count        <- mkReg(0);

//   SFIFO#(resp_t, Bit#(9)) fifo_in_tx  <- mkSizedSFIFO(20);
   FIFO#(req_t)        fifo_in_rx  <- mkFIFO;
   FIFO#(req_t)        fifo_out_tx <- mkFIFO;
   FIFO#(resp_t)       fifo_out_rx <- mkFIFO;

   let rx_in = toTLMRequest(fifo_in_rx.first);

   Reg#(Bool)          first_marked <- mkReg(True);
   SFIFO#(Bool, TLMBLength#(`TLM_PRM)) fifo_marked <- mkSafeDepthParamSFIFO(5);

   function Bool isUncountedResponse (TLMResponse#(`TLM_PRM) tlm_response);
      TLMErrorCode code = unpack(truncateNP(tlm_response.data));
      let uncounted = tlm_response.status == ERROR &&
      (code == RETRY || code == SPLIT || code == SPLIT_CONTINUE);
      return uncounted;
   endfunction

   function Bool keepResponse (TLMResponse#(`TLM_PRM) tlm_response);
      Bool keep = tlm_response.is_last || tlm_response.command == READ || all_responses;
      if (isUncountedResponse(tlm_response))
	 keep = True;
      if (fifo_marked.first)
	 keep = True;
      return keep;
   endfunction

   (* preempts = "(marked_op, marked_op_error), (read_op_first, read_op_rest, write_op_first, write_op_rest)" *)
   rule marked_op (rx_in matches tagged Descriptor .d
		   &&& d.b_length == 0
		   &&& d.mark != OPEN);
      fifo_out_tx.enq(fifo_in_rx.first);
      fifo_in_rx.deq;
      if (first_marked) fifo_marked.enq(True);
      first_marked <= d.mark == LAST;
   endrule

   rule marked_op_error (rx_in matches tagged Descriptor .d
			 &&& d.b_length != 0
			 &&& d.mark != OPEN);
      $display("(%0d) ERROR! Marked requests must be of length 1", $time);
   endrule

   rule read_op_first (rx_in matches tagged Descriptor .d
		       &&& d.command == READ
		       &&& d.mark == OPEN
		       &&& (count == 0));
      let remaining = d.b_length;
      count <= remaining;
      let desc_current = d;
      desc_current.b_length = 0;
      desc_current.mark = (remaining == 0) ? LAST : NOT_LAST;
      fifo_out_tx.enq(fromTLMRequest(tagged Descriptor desc_current));
      req_reg <= fromTLMRequest(tagged Descriptor incrTLMAddr(alignAddress(d)));
      if (remaining == 0) fifo_in_rx.deq;
      fifo_marked.enq(False);
   endrule

   rule read_op_rest (rx_in matches tagged Descriptor .d
		      &&& d.command == READ
		      &&& d.mark == OPEN
		      &&& (count > 0)
		      &&& toTLMRequest(req_reg) matches tagged Descriptor .dr);
      let remaining = count - 1;
      count <= remaining;
      let desc_current = dr;
      desc_current.b_length = 0;
      desc_current.mark = (remaining == 0) ? LAST : NOT_LAST;
      fifo_out_tx.enq(fromTLMRequest(tagged Descriptor desc_current));
      req_reg <= fromTLMRequest(tagged Descriptor incrTLMAddr(dr));
      if (remaining == 0) fifo_in_rx.deq;
   endrule

   rule write_op_first (rx_in matches tagged Descriptor .d
			&&& d.command == WRITE
			&&& d.mark == OPEN
			&&& (count == 0));
      let remaining = d.b_length;
      count <= remaining;
      let desc_current = d;
      desc_current.b_length = 0;
      desc_current.mark = (remaining == 0) ? LAST : NOT_LAST;
      if (desc_current.byte_enable matches tagged Specify .be)
	 fifo_out_tx.enq(fromTLMRequest(tagged Descriptor desc_current));
      else
	 fifo_out_tx.enq(fromTLMRequest(tagged Descriptor addByteEnableU(big_endian, desc_current)));
      req_reg <= fromTLMRequest(tagged Descriptor incrTLMAddr(alignAddress(d)));
      fifo_in_rx.deq;
      fifo_marked.enq(False);
   endrule

   rule write_op_rest (rx_in matches tagged Data .d
		       &&& toTLMRequest(req_reg) matches tagged Descriptor .dr);
      let remaining = count - 1;
      count <= remaining;
      let desc_current = dr;
      desc_current.b_length = 0;
      desc_current.data = d.data;
      desc_current.byte_enable = d.byte_enable;
      desc_current.mark = (remaining == 0) ? LAST : NOT_LAST;
      if (desc_current.byte_enable matches tagged Specify .be)
	 fifo_out_tx.enq(fromTLMRequest(tagged Descriptor desc_current));
      else
	 fifo_out_tx.enq(fromTLMRequest(tagged Descriptor addByteEnableU(big_endian, desc_current)));
      req_reg <= fromTLMRequest(tagged Descriptor incrTLMAddr(dr));
      fifo_in_rx.deq;
   endrule

   let tlm_response = toTLMResponse(fifo_out_rx.first);

   // just drop it on the floor.
   rule grab_response(!keepResponse(tlm_response));
      if (tlm_response.is_last) fifo_marked.deq;
      fifo_out_rx.deq;
   endrule

   interface TLMRecvIFC in;
      interface Get tx;
	 method ActionValue#(resp_t) get if (keepResponse(tlm_response));
	    if (tlm_response.is_last && !isUncountedResponse(tlm_response))
	       fifo_marked.deq;
	    let value = fifo_out_rx.first;
	    fifo_out_rx.deq;
	    return value;
	 endmethod
      endinterface
      interface Put rx = toPut(fifo_in_rx);
   endinterface

   interface TLMSendIFC out;
      interface Get tx = toGet(fifo_out_tx);
      interface Put rx = toPut(fifo_out_rx);
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkTLMSplit#(parameter UInt#(32) max_flight,
		   TLMSendIFC#(`TLM_RR) send) (TLMReadWriteSendIFC#(`TLM_RR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1));

   FIFO#(req_t)      fifo_tx     <- mkLFIFO;
   Wire#(req_t)      rd_req_wire <- mkWire;
   Wire#(req_t)      wr_req_wire <- mkWire;
   Wire#(TLMCommand) cmd_wire    <- mkWire;

   FIFOF#(resp_t) fifo_rd_rx <- mkSizedFIFOF(16);
   FIFOF#(resp_t) fifo_wr_rx <- mkLFIFOF;

   Reg#(TLMCommand) mode <- mkReg(READ);

   FIFO#(TLMCommand) fifo_cmd <- mkSafeDepthParamFIFO(max_flight);

   ////////////////////////////////////////////////////////////////////////////////
   /// Request handling
   ////////////////////////////////////////////////////////////////////////////////

   rule grab_all_requests;
      let value <- send.tx.get;
      fifo_tx.enq(value);
   endrule

   rule grab_rd_requests(toTLMRequest(fifo_tx.first) matches tagged Descriptor .d
			 &&& d.command == READ);
      rd_req_wire <= fifo_tx.first;
      cmd_wire <= READ;
   endrule

   rule grab_wr_requests(toTLMRequest(fifo_tx.first) matches tagged Descriptor .d
			 &&& d.command == WRITE);
      wr_req_wire <= fifo_tx.first;
      cmd_wire <= WRITE;
   endrule

   rule grab_data_requests(toTLMRequest(fifo_tx.first) matches tagged Data .d);
      wr_req_wire <= fifo_tx.first;
      cmd_wire <= WRITE;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Response handling
   ////////////////////////////////////////////////////////////////////////////////

   rule grab_read_response (fifo_cmd.first == READ);
      let value = fifo_rd_rx.first;
      let response = toTLMResponse(value);
      send.rx.put(value);
      fifo_rd_rx.deq;
      if (response.is_last)
	 fifo_cmd.deq;
   endrule

   rule grab_write_response (fifo_cmd.first == WRITE);
      let value = fifo_wr_rx.first;
      let response = toTLMResponse(value);
      send.rx.put(value);
      fifo_wr_rx.deq;
      if (response.is_last)
	 fifo_cmd.deq;
   endrule

   interface TLMSendIFC read;
      interface Get tx;
	 method ActionValue#(req_t) get if (cmd_wire == READ);
	    fifo_tx.deq;
	    if (toTLMRequest(rd_req_wire) matches tagged Descriptor .d)
	       fifo_cmd.enq(cmd_wire);
	    return rd_req_wire;
	 endmethod
      endinterface

      interface rx = toPut(fifo_rd_rx);
   endinterface

      interface TLMSendIFC write;
      interface Get tx;
	 method ActionValue#(req_t) get if (cmd_wire == WRITE);
	    fifo_tx.deq;
	    if (toTLMRequest(wr_req_wire) matches tagged Descriptor .d)
	       fifo_cmd.enq(cmd_wire);
	    return wr_req_wire;
	 endmethod
      endinterface

      interface rx = toPut(fifo_wr_rx);
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {Rd, Wr, RdOrWr} MD deriving (Eq, Bits, Bounded);

module mkTLMJoin#(TLMRecvIFC#(`TLM_RR) recv) (TLMReadWriteRecvIFC#(`TLM_RR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1));

   FIFOF#(req_t) fifo_rd_rx   <- mkLFIFOF;
   FIFOF#(req_t) fifo_wr_rx   <- mkLFIFOF;

   FIFO#(resp_t)     fifo_rx      <- mkLFIFO;
   Wire#(resp_t)     rd_resp_wire <- mkWire;
   Wire#(resp_t)     wr_resp_wire <- mkWire;
   Wire#(TLMCommand) cmd_wire     <- mkWire;

   Reg#(TLMBLength#(`TLM_PRM)) count     <- mkReg(0);

   Reg#(MD) mode <- mkReg(RdOrWr);

   ////////////////////////////////////////////////////////////////////////////////
   /// Request handling
   ////////////////////////////////////////////////////////////////////////////////

   rule grab_write_data_request (toTLMRequest(fifo_wr_rx.first) matches tagged Data .d
				 &&& count != 0);
      recv.rx.put(fifo_wr_rx.first);
      fifo_wr_rx.deq;
      let remaining = count - 1;
      count <= remaining;
      if (remaining == 0)
      mode <= (fifo_rd_rx.notEmpty) ? Rd : RdOrWr;
   endrule
   (* preempts = "grab_read_request, grab_write_request" *)
   rule grab_read_request (toTLMRequest(fifo_rd_rx.first) matches tagged Descriptor .d
			   &&& count == 0
			   &&& mode != Wr);
      let value = fifo_rd_rx.first;
      recv.rx.put(value);
      fifo_rd_rx.deq;
      mode <= (d.mark == NOT_LAST) ? Rd : ((fifo_wr_rx.notEmpty) ? Wr : RdOrWr);
   endrule

   rule grab_write_request (toTLMRequest(fifo_wr_rx.first) matches tagged Descriptor .d
			    &&& count == 0
			    &&& mode != Rd);
      let value = fifo_wr_rx.first;
      recv.rx.put(value);
      fifo_wr_rx.deq;
      let remaining = d.b_length;
      count <= remaining;
      if (remaining == 0)
      mode <= (d.mark == NOT_LAST) ? Wr : ((fifo_rd_rx.notEmpty) ? Rd : RdOrWr);
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Response handling
   ////////////////////////////////////////////////////////////////////////////////

    rule grab_all_responses;
      let value <- recv.tx.get;
      fifo_rx.enq(value);
    endrule

   rule grab_rd_responses(toTLMResponse(fifo_rx.first).command == READ);
      rd_resp_wire <= fifo_rx.first;
      cmd_wire <= READ;
   endrule

   rule grab_wr_responses(toTLMResponse(fifo_rx.first).command == WRITE);
      wr_resp_wire <= fifo_rx.first;
      cmd_wire <= WRITE;
   endrule

   interface TLMRecvIFC read;
      interface Get tx;
	 method ActionValue#(resp_t) get if (cmd_wire == READ);
	    fifo_rx.deq;
	    return rd_resp_wire;
	 endmethod
      endinterface
      interface rx = toPut(fifo_rd_rx);
   endinterface

   interface TLMRecvIFC write;
      interface Get tx;
	 method ActionValue#(resp_t) get if (cmd_wire == WRITE);
	    fifo_rx.deq;
	    return wr_resp_wire;
	 endmethod
      endinterface
      interface rx = toPut(fifo_wr_rx);
   endinterface
endmodule

endpackage
