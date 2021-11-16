// Copyright (c) 2007 - 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3BRAM;

import BRAM::*;
import CBus::*;
import ClientServer::* ;
import DefaultValue::* ;
import FIFOF::*;
import FShow::*;
import GetPut::*;
import TLM3Defines::*;

`include "TLM.defines"

module mkTLMBRAM (BRAMServer#(Bit#(anx), Bit#(dn)) bramifc, TLMRecvIFC#(reqt, respt) ifc)
   provisos(Bits#(respt, sr),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
	    Div#(data_size, 8, byte_size), // byte_size needs to be a power of 2 (i.e. 1, 2, 4 ..)
            Div#(data_size,8,TDiv#(data_size,8)),
	    TLMRequestTC#(reqt,   `TLM_PRM),
	    TLMResponseTC#(respt, `TLM_PRM),
	    FShow#(TLMRequest#(`TLM_PRM)));

   BRAMServerBE#(Bit#(anx), Bit#(dn), TDiv#(dn,8)) bram_be = toBRAMServerBE(bramifc);
   let _z <- mkTLMBRAMBE(bram_be);
   return _z;

endmodule

// A module which provides a TLMRecv interface, built on any module that
// provides a BRAM1Port interface for example a mkBRAM module.
module mkTLMBRAMBE (BRAMServerBE#(Bit#(anx), Bit#(dn), nn) bramifc, TLMRecvIFC#(req_t, resp_t) ifc)
   provisos(Bits#(resp_t, sr),
	    DefaultValue#(TLMResponse#(`TLM_PRM)),
	    Div#(data_size, 8, byte_size), // byte_size needs to be a power of 2 (i.e. 1, 2, 4 ..)
	    TLMRequestTC#(req_t,   `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    FShow#(TLMRequest#(`TLM_PRM))
      );

   BRAMServerBE#(TLMAddr#(`TLM_PRM), TLMData#(`TLM_PRM), byte_size) bram = convertBRAMType (bramifc);

   FIFOF#(Tuple2#(TLMCommand, TLMId#(`TLM_PRM))) fifo_op <- mkFIFOF;

   interface Get tx;
      method ActionValue#(resp_t) get () ;
         let val <- bram.response.get;
	 match {.cmd, .id} = fifo_op.first;
         fifo_op.deq;
         TLMResponse#(`TLM_PRM) response = defaultValue ;
	 response.data = val;
         response.command = cmd;
	 response.transaction_id = id;
         response.status  = SUCCESS; // Assume always OK if we get a response from the BRAM
	 response.data = extendNP((cmd == WRITE) ? 0 : val);
	 response.is_last = True;
         return fromTLMResponse(response);
      endmethod
   endinterface
   interface Put rx;
      method Action put (req_t req);
         case (toTLMRequest(req))  matches
            tagged Descriptor .d : begin
	       let byte_enable = '1;
	       if (d.byte_enable matches tagged Specify .b)
		  byte_enable = b;
               case (d.command)
                  READ: begin
			   TLMAddr#(`TLM_PRM) addr = extendNP(d.addr >> valueOf(TLog#(byte_size)));
                           bram.request.put( BRAMRequestBE {writeen    :0,
                                                            responseOnWrite : True,
                                                            address  :addr,
			                                    datain   :0} );
			   fifo_op.enq(tuple2(READ, d.transaction_id));
                        end
                  WRITE: begin
			    TLMAddr#(`TLM_PRM) addr = extendNP(d.addr >> valueOf(TLog#(byte_size)));
                            bram.request.put( BRAMRequestBE {writeen    : byte_enable,
                                                             responseOnWrite : True,
			                                     address  :addr,
			                                     datain   :d.data} );
			    fifo_op.enq(tuple2(WRITE, d.transaction_id));
                         end
               endcase
               // Bursts of Length 1 are supported.
               if (d.b_length != 0)
                  $display( "ERROR: %m, burst length > 1 not supported ", fshow(d));
            end
            tagged Data .d : begin
               $display( "ERROR: data stream sent: %m, not supported", fshow(d));
            end
         endcase
      endmethod
   endinterface

endmodule


function BRAMServerBE#(TLMAddr#(`TLM_PRM), TLMData#(`TLM_PRM), n)
         convertBRAMType (BRAMServerBE#(Bit#(an), Bit#(dn), nn) ifcin);
   return
   (interface Server;
       interface Put request;
	  method Action put (reqin);
	     let req = (valueOf(dn) > valueOf(data_size))
	     ? BRAMRequestBE {writeen   : extendNP(reqin.writeen),
			      responseOnWrite: True, // TLM has write response
			      address : truncateNP(reqin.address),
			      datain  : extendNP(reqin.datain)}
	     : BRAMRequestBE {writeen   : truncateNP(reqin.writeen),
			      responseOnWrite: True, // TLM has write response
			      address : truncateNP(reqin.address),
			      datain  : truncateNP(reqin.datain)};
	     ifcin.request.put (req);
          endmethod
       endinterface
       interface Get response;
          method ActionValue#(TLMData#(`TLM_PRM)) get;
	     let value <-  ifcin.response.get;
	     let rtn = (valueOf(dn) > valueOf(data_size)) ? truncateNP(value) : extendNP(value);
	     return rtn;
	  endmethod
       endinterface
    endinterface
    );
endfunction


typeclass ToBRAMSeverBETC #(type a, type addr, type data, numeric type n)
   dependencies (a determines (addr, data, n));
   function BRAMServerBE#(addr, data, n) toBRAMServerBE (a ifc);
endtypeclass

instance ToBRAMSeverBETC #(BRAMServerBE#(addr, data, n), addr, data, n);
   function toBRAMServerBE = id ;
endinstance

instance ToBRAMSeverBETC #(BRAMServer#(addr, data), addr, data, n);
   function BRAMServerBE#(addr, data, n) toBRAMServerBE ( BRAMServer#(addr, data) ifcin );
      return
      (interface Server;
          interface Put request;
             method Action put ( BRAMRequestBE#(addr, data, n) reqin);
                if ( (reqin.writeen != '0) && (reqin.writeen != '1) )
                   $display ("Converting from a BRAM Server to BRAM Server BE with invalid Byte enable %b",
                             reqin.writeen );
                let req = BRAMRequest {write    : reqin.writeen != 0,
                                       responseOnWrite: reqin.responseOnWrite,
	                               address : reqin.address,
	                               datain  : reqin.datain } ;
                ifcin.request.put (req);
       endmethod
          endinterface
          interface Get response;
             method ActionValue#(data) get;
	        let value <-  ifcin.response.get;
	        return(value);
       endmethod
          endinterface
       endinterface);
   endfunction
endinstance



endpackage
