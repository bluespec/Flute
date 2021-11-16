// Copyright (c) 2007--2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3Ram;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import BUtils::*;
import DefaultValue::*;
import FIFO::*;
import GetPut::*;
import RegFile::*;
import TLM3Defines::*;
import TLM3Utils::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkTLMRam_unbuffered#(parameter Bit#(4) id, Bool verbose) (TLMRecvIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
	    Bits#(TLMRequest#(`TLM_PRM),  s0),
	    Bits#(TLMResponse#(`TLM_PRM), s1));

   Wire#(TLMRequest#(`TLM_PRM))  in_wire   <- mkWire;
   Wire#(TLMResponse#(`TLM_PRM)) out_wire  <- mkWire;

   RegFile#(Bit#(8), Bit#(data_size)) ram <- mkRegFileLoad("ram_init.text", 0, 255);

   rule read_op (in_wire matches tagged Descriptor .d
		 &&& d.command matches READ
		 &&& d.b_length == 0);

      TLMResponse#(`TLM_PRM) response = defaultValue;
      Bit#(10) addr = zExtend(d.addr);
      Bit#(8) mem_addr = grab_left(addr);
      TLMData#(`TLM_PRM) data = ram.sub(mem_addr);
      response.data = maskTLMData(d.byte_enable, data);
      response.status = SUCCESS;
      response.transaction_id = d.transaction_id;
      response.command = READ;

      out_wire <= response;

      if (verbose) $display("(%0d) TM (%0d) Read Op %h %h", $time, id, d.addr, response.data);

   endrule


   rule write_op (in_wire matches tagged Descriptor .d
		  &&& d.command matches WRITE
		  &&& d.b_length == 0);

      Bit#(10) addr = zExtend(d.addr);
      Bit#(8) mem_addr = grab_left(addr);
      TLMData#(`TLM_PRM) data_orig = ram.sub(mem_addr);
      TLMData#(`TLM_PRM) data_new  = overwriteTLMData(d.byte_enable, data_orig, d.data);
      ram.upd(mem_addr, data_new);

      TLMResponse#(`TLM_PRM) response = defaultValue;
      response.status = SUCCESS;
      response.transaction_id = d.transaction_id;
      response.command = WRITE;

      out_wire <= response;

      if (verbose) $display("(%0d) TM (%0d) Write Op %h %h", $time, id, d.addr, d.data);

   endrule

   rule error_op (in_wire matches tagged Descriptor .d
		  &&& (d.b_length != 0));
      $display("(%0d) ERROR: TLMRAM (%0d) (cannot handle ops with burst length > 1).", $time, id);
   endrule

   interface Get tx;
      method get;
	 actionvalue
            return fromTLMResponse(out_wire);
	 endactionvalue
      endmethod
   endinterface
   interface Put rx;
      method Action put (x);
	 in_wire <= toTLMRequest(x);
      endmethod
   endinterface

endmodule

(* deprecate = "Replaced by mkTLMRam" *)
module mkTLMRamX#(parameter Bit#(4) id, Bool verbose) (TLMRecvIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
	    Bits#(req_t, s0),
	    Bits#(TLMResponse#(`TLM_PRM), s1));

   (*hide*)
   let _i <- mkTLMRam(id, verbose);
   return _i;
endmodule


module mkTLMRam#(parameter Bit#(4) id, Bool verbose) (TLMRecvIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
	    Bits#(req_t, s0),
	    Bits#(TLMResponse#(`TLM_PRM), s1));

   FIFO#(req_t) fifo_in <- mkLFIFO;

   RegFile#(Bit#(8), Bit#(data_size)) ram <- mkRegFileLoad("ram_init.text", 0, 255);

   rule error_op (toTLMRequest(fifo_in.first) matches tagged Descriptor .d
		  &&& (d.b_length != 0));
      fifo_in.deq;
      $display("(%0d) ERROR: TLMRAMX (%0d) (cannot handle ops with burst length > 1).", $time, id);
   endrule

   interface Get tx;
      method get if (toTLMRequest(fifo_in.first) matches tagged Descriptor .d &&&
		     d.b_length == 0);
	 actionvalue
	    resp_t value = ?;
	    if (d.command == WRITE)
	       begin
		  Bit#(10) addr = zExtend(d.addr);
		  Bit#(8) mem_addr = grab_left(addr);
		  TLMData#(`TLM_PRM) data_orig = ram.sub(mem_addr);
		  TLMData#(`TLM_PRM) data_new  = overwriteTLMData(d.byte_enable, data_orig, d.data);
		  ram.upd(mem_addr, data_new);

		  TLMResponse#(`TLM_PRM) response = defaultValue;
		  response.status = SUCCESS;
		  response.transaction_id = d.transaction_id;
		  response.command = WRITE;
		  response.is_last = d.mark == LAST;

		  fifo_in.deq;

		  if (verbose) $display("(%0d) TM (%0d) Write Op %h %h", $time, id, d.addr, d.data);
		  value = fromTLMResponse(response);
	       end
	    if (d.command == READ)
	       begin
		  TLMResponse#(`TLM_PRM) response = defaultValue;
		  Bit#(10) addr = zExtend(d.addr);
		  Bit#(8) mem_addr = grab_left(addr);
		  TLMData#(`TLM_PRM) data = ram.sub(mem_addr);
		  response.data = maskTLMData(d.byte_enable, data);
		  response.status = SUCCESS;
		  response.transaction_id = d.transaction_id;
		  response.command = READ;
		  response.is_last = d.mark == LAST;

		  fifo_in.deq;

		  if (verbose) $display("(%0d) TM (%0d) Read Op %h %h", $time, id, d.addr, response.data);
		  value = fromTLMResponse(response);
	       end
	    return value;
	 endactionvalue
      endmethod
   endinterface
   interface Put rx = toPut(fifo_in);

endmodule

endpackage
