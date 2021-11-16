// Copyright (c) 2007--2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3ReadWriteRam;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import BUtils::*;
import DefaultValue::*;
import FShow::*;
import GetPut::*;
import Probe::*;
import RegFile::*;
import TLM3Defines::*;
import TLM3Utils::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkTLMReadWriteRam#(parameter Bit#(4) id, Bool verbose) (TLMReadWriteRecvIFC#(req_t, resp_t))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
            Bits#(TLMRequest#(`TLM_PRM),  s0),
	    Bits#(TLMResponse#(`TLM_PRM), s1),
	    FShow#(TLMRequest#(`TLM_PRM)),
	    FShow#(TLMResponse#(`TLM_PRM)));

   Wire#(TLMRequest#(`TLM_PRM))  read_in_wire   <- mkWire;
   Wire#(TLMResponse#(`TLM_PRM)) read_out_wire  <- mkWire;
   Wire#(TLMRequest#(`TLM_PRM))  write_in_wire  <- mkWire;
   Wire#(TLMResponse#(`TLM_PRM)) write_out_wire <- mkWire;

   RegFile#(Bit#(8), Bit#(data_size)) ram <- mkRegFileLoad("ram_init.text", 0, 255);

   rule read_op (read_in_wire matches tagged Descriptor .d
		 &&& d.command == READ
		 &&& d.b_length == 0);

      TLMResponse#(`TLM_PRM) response = defaultValue;
      Bit#(10) addr = zExtend(d.addr);
      Bit#(8) mem_addr = grab_left(addr);
      TLMData#(`TLM_PRM) data = ram.sub(mem_addr);
      response.data = maskTLMData(d.byte_enable, data);
      response.status = SUCCESS;
      response.transaction_id = d.transaction_id;
      response.command = READ;

      read_out_wire <= response;

      if (verbose) $display("(%0d) TM (%0d) %0d Read  Op %h %h", $time, id, d.transaction_id, d.addr, response.data);

   endrule

   rule write_op (write_in_wire matches tagged Descriptor .d
		  &&& d.command == WRITE
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

      write_out_wire <= response;

      if (verbose) $display("(%0d) TM (%0d) %0d Write Op %h %h", $time, id, d.transaction_id, d.addr, d.data);

   endrule

   rule read_error_op (read_in_wire matches tagged Descriptor .d
		       &&& (d.b_length != 0));
      $display("(%0d) ERROR: TLMReadWriteRAM (%0d) (cannot handle ops with burst length > 1).", $time, id);
   endrule

   rule write_error_op (write_in_wire matches tagged Descriptor .d
			&&& (d.b_length != 0));
      $display("(%0d) ERROR: TLMReadWriteRAM (%0d) (cannot handle ops with burst length > 1).", $time, id);
   endrule

   interface TLMRecvIFC read;
      interface Get tx;
	 method get;
	    actionvalue
               return fromTLMResponse(read_out_wire);
	    endactionvalue
	 endmethod
      endinterface
      interface Put rx;
	 method Action put (x);
	    read_in_wire <= toTLMRequest(x);
	 endmethod
      endinterface
   endinterface

   interface TLMRecvIFC write;
      interface Get tx;
	 method get;
	    actionvalue
               return fromTLMResponse(write_out_wire);
	    endactionvalue
	 endmethod
      endinterface
      interface Put rx;
	 method Action put (x);
	    write_in_wire <= toTLMRequest(x);
	 endmethod
      endinterface
   endinterface


endmodule

endpackage
