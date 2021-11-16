// Copyright (c) 2007--2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3CBusAdapter;

import CBus::*;
import DefaultValue::*;
import FIFO::*;
import GetPut::*;
import TLM3Defines::*;
import BUtils::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef CBus#(caddr_size, data_size) TLMCBus#(`TLM_PRM_DCL, numeric type caddr_size);
typedef ModWithCBus#(caddr_size, data_size, i) ModWithTLMCBus#(`TLM_PRM_DCL, numeric type caddr_size, type i);
typedef CRAddr#(caddr_size, data_size) TLMCRAddr#(`TLM_PRM_DCL, numeric type caddr_size);

module mkTLMCBusAdapter#(function Bit#(caddr_size) mapTLMAddr(Bit#(addr_size) addr),
			 TLMCBus#(`TLM_PRM, caddr_size) cfg) (TLMRecvIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
	    Bits#(TLMRequest#(`TLM_PRM),  s0),
	    Bits#(resp_t, s1));

//	    Add#(ignore, caddr_size, addr_size));

   // Wire is OK since CBus never blocks request.
   Wire#(TLMRequest#(`TLM_PRM))  tlm_wire_in   <- mkWire;

   FIFO#(resp_t) tlm_fifo_out <- mkFIFO;

   rule read_op (tlm_wire_in matches tagged Descriptor .d
		 &&& d.command matches READ
		 &&& d.b_length == 0);


      TLMResponse#(`TLM_PRM) response = defaultValue;
      Bit#(caddr_size) addr = mapTLMAddr(d.addr);
      let data <- cfg.read(addr);
      response.data = data;
      response.status = SUCCESS;
      response.command = READ;
      response.transaction_id = d.transaction_id;
      tlm_fifo_out.enq(fromTLMResponse(response));
      // $display("(%0d) READ OP Addr: %h Data: %h", $time, addr, data);
   endrule

   rule write_op (tlm_wire_in matches tagged Descriptor .d
		  &&& d.command matches WRITE
		  &&& d.b_length == 0);
      TLMResponse#(`TLM_PRM) response = defaultValue;
      Bit#(caddr_size) addr = mapTLMAddr(d.addr);
      cfg.write(addr, d.data);
      response.status = SUCCESS;
      response.command = WRITE;
      response.transaction_id = d.transaction_id;
      tlm_fifo_out.enq(fromTLMResponse(response));
      // $display("(%0d) WRITE OP Addr: %h Data: %h", $time, addr, d.data);
   endrule

   rule error_op (tlm_wire_in matches tagged Descriptor .d
		  &&& (d.b_length != 0));
      $display("(%5d) ERROR: TLMCbusAdapter (cant handle ops with burst length > 1).", $time);
   endrule

   interface Get tx = toGet(tlm_fifo_out);
   interface Put rx;
      method Action put (x);
	 tlm_wire_in <= toTLMRequest(x);
      endmethod
   endinterface

endmodule

module mkTLMCBusAdapterToReadWrite#(function Bit#(caddr_size) mapTLMAddr(Bit#(addr_size) addr),
				    TLMCBus#(`TLM_PRM, caddr_size) cfg)
				       (TLMReadWriteRecvIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
	    Bits#(TLMRequest#(`TLM_PRM),  s0),
	    Bits#(resp_t, s1));
   
//	    Add#(ignore, caddr_size, addr_size));

   // Wire is OK since CBus never blocks request.
   Wire#(TLMRequest#(`TLM_PRM))  read_wire_in   <- mkWire;
   Wire#(TLMRequest#(`TLM_PRM))  write_wire_in  <- mkWire;

   FIFO#(resp_t) read_fifo_out <- mkFIFO;
   FIFO#(resp_t) write_fifo_out <- mkFIFO;

   rule read_op (read_wire_in matches tagged Descriptor .d
		 &&& d.command matches READ
		 &&& d.b_length == 0);


      TLMResponse#(`TLM_PRM) response = defaultValue;
      Bit#(caddr_size) addr = mapTLMAddr(d.addr);
      let data <- cfg.read(addr);
      response.data    = data;
      response.status  = SUCCESS;
      response.command = READ;
      response.transaction_id = d.transaction_id;
      read_fifo_out.enq(fromTLMResponse(response));
   endrule

   rule write_op (write_wire_in matches tagged Descriptor .d
		  &&& d.command matches WRITE
		  &&& d.b_length == 0);
      TLMResponse#(`TLM_PRM) response = defaultValue;
      Bit#(caddr_size) addr = mapTLMAddr(d.addr);
      cfg.write(addr, d.data);
      response.status = SUCCESS;
      response.command = WRITE;
      response.transaction_id = d.transaction_id;
      write_fifo_out.enq(fromTLMResponse(response));
   endrule

   rule read_error_op (read_wire_in matches tagged Descriptor .d
		       &&& (d.b_length != 0));
      $display("[%0d] ERROR: TLMCbusAdapter (cant handle ops with burst length > 1).", $time);
   endrule

   rule write_error_op (read_wire_in matches tagged Descriptor .d
			&&& (d.b_length != 0));
      $display("[%0d] ERROR: TLMCbusAdapter (cant handle ops with burst length > 1).", $time);
   endrule

   interface TLMRecvIFC read;
      interface Get tx = toGet(read_fifo_out);
      interface Put rx;
	 method Action put (x);
	    read_wire_in <= toTLMRequest(x);
	 endmethod
      endinterface
   endinterface

   interface TLMRecvIFC write;
      interface Get tx = toGet(write_fifo_out);
      interface Put rx;
	 method Action put (x);
	    write_wire_in <= toTLMRequest(x);
	 endmethod
      endinterface
   endinterface

endmodule

endpackage
