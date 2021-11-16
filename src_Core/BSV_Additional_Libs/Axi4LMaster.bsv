// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi4LMaster;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Axi4Defines::*;
import Axi4LDefines::*;
import AxiDefines::*;
import Bus::*;
import Connectable::*;
import FIFO::*;
import GetPut::*;
import TLM3::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LRdMasterIFC#(BusSend#(Axi4LAddrCmd#(`TLM_PRM))  request_addr,
			   BusRecv#(Axi4LRdResp#(`TLM_PRM))    response) (Axi4LRdMaster#(`TLM_PRM));

   Wire#(AxiData#(`TLM_PRM)) rDATA_wire <- mkBypassWire;
   Wire#(AxiResp)            rRESP_wire <- mkBypassWire;

   rule every ;
      let value = Axi4LRdResp {data: rDATA_wire,
			       resp: rRESP_wire};
      response.data(value);
   endrule

   // Address Outputs
   method arADDR  = request_addr.data.addr;
   method arPROT  = request_addr.data.prot;
   method arVALID = request_addr.valid;

   // Address Inputs
   method arREADY = request_addr.ready;

   // Response Outputs
   method rREADY  = response.ready;

   // Response Inputs
   method rDATA   = rDATA_wire._write;
   method rRESP   = rRESP_wire._write;
   method rVALID  = response.valid;

endmodule

module mkAxi4LWrMasterIFC#(BusSend#(Axi4LAddrCmd#(`TLM_PRM)) request_addr,
			   BusSend#(Axi4LWrData#(`TLM_PRM))  request_data,
			   BusRecv#(AxiResp)                 response)
                          (Axi4LWrMaster#(`TLM_PRM));

   Wire#(AxiResp) bRESP_wire  <- mkBypassWire;

   rule every ;
      let value = bRESP_wire;
      response.data(value);
   endrule

   // Address Outputs
   method awADDR  = request_addr.data.addr;
   method awPROT  = request_addr.data.prot;
   method awVALID = request_addr.valid;

   // Address Inputs
   method awREADY = request_addr.ready;

   // Data Outputs
   method wDATA   = request_data.data.data;
   method wSTRB   = request_data.data.strb;
   method wVALID  = request_data.valid;

   // Data Inputs
   method wREADY  = request_data.ready;

   // Response Outputs
   method bREADY  = response.ready;

   // Response Inputs
   method bRESP   = bRESP_wire._write;
   method bVALID  = response.valid;

endmodule

module mkAxi4LRdBusMasterIFC#(Axi4LRdMaster#(`TLM_PRM) ifc) (Axi4LRdBusMaster#(`TLM_PRM));

   interface BusSend addr;
      method Axi4LAddrCmd#(`TLM_PRM) data;
	 let addr =  Axi4LAddrCmd {prot:  ifc.arPROT,
				   addr:  ifc.arADDR};
	 return addr;
      endmethod
      method valid = ifc.arVALID;
      method ready = ifc.arREADY;
   endinterface
   interface BusRecv resp;
      method Action data(Axi4LRdResp#(`TLM_PRM) value);
	 ifc.rDATA(value.data);
	 ifc.rRESP(value.resp);
      endmethod
      method valid = ifc.rVALID;
      method ready = ifc.rREADY;
   endinterface

endmodule

module mkAxi4LWrBusMasterIFC#(Axi4LWrMaster#(`TLM_PRM) ifc) (Axi4LWrBusMaster#(`TLM_PRM));

   interface BusSend addr;
      method Axi4LAddrCmd#(`TLM_PRM) data;
	 let addr =  Axi4LAddrCmd {prot:  ifc.awPROT,
				   addr:  ifc.awADDR};
	 return addr;
      endmethod
      method valid = ifc.awVALID;
      method ready = ifc.awREADY;
   endinterface
   interface BusSend data;
      method Axi4LWrData#(`TLM_PRM) data;
	 let out = Axi4LWrData {data: ifc.wDATA,
				strb: ifc.wSTRB};
	 return out;
      endmethod
      method valid = ifc.wVALID;
      method ready = ifc.wREADY;
   endinterface
   interface BusRecv resp;
      method Action data(AxiResp value);
	 ifc.bRESP(value);
      endmethod
      method valid = ifc.bVALID;
      method ready = ifc.bREADY;
   endinterface

endmodule


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LRdMaster#(parameter UInt#(32) max_flight) (Axi4LRdMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusSender#(Axi4LAddrCmd#(`TLM_PRM))  rd_addr_fifo <- mkBypassBusSender(unpack(0));
   BusReceiver#(Axi4LRdResp#(`TLM_PRM)) rd_resp_fifo <- mkBypassBusReceiver;

   FIFO#(req_t)                       fifo_rx <- mkLFIFO;
   FIFO#(resp_t)                      fifo_tx <- mkLFIFO;
   Reg#(TLMBLength#(`TLM_PRM))        count   <- mkReg(0);

   FIFO#(TLMId#(`TLM_PRM)) id_fifo <- mkSafeDepthParamFIFO(max_flight);

   let _ifc <- mkAxi4LRdMasterIFC(rd_addr_fifo.out, rd_resp_fifo.in);

   let rx_first = toTLMRequest(fifo_rx.first);

   rule send_read (rx_first matches tagged Descriptor .d
		   &&& d.command matches READ);
      let addr_cmd = getAxi4LAddrCmd(d);
      rd_addr_fifo.in.enq(addr_cmd);
      id_fifo.enq(d.transaction_id);
      fifo_rx.deq;
   endrule

   rule grab_response;
      let axi_response = rd_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxi4LRdResp(axi_response);
      response.transaction_id = id_fifo.first;
      id_fifo.deq;
      rd_resp_fifo.out.deq;
      fifo_tx.enq(fromTLMResponse(response));
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Error cases:
   ////////////////////////////////////////////////////////////////////////////////

   (* preempts = "start_write, grab_response" *)
   rule start_write (rx_first matches tagged Descriptor .d
		     &&& d.command matches WRITE);
      $display("(%0d) ERROR: Axi4LRdMaster cannot handle WRITE ops!", $time);
      fifo_rx.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   interface TLMRecvIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface Axi4LRdFabricMaster fabric;
      interface Axi4LRdMaster bus = _ifc;
   endinterface


endmodule



////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LWrMaster#(parameter UInt#(32) max_flight,
			parameter Bool big_endian) (Axi4LWrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusSender#(Axi4LAddrCmd#(`TLM_PRM)) wr_addr_fifo <- mkBypassBusSender(unpack(0));
   BusSender#(Axi4LWrData#(`TLM_PRM))  wr_data_fifo <- mkBypassBusSender(unpack(0));

   let _ifc <- mkAxi4LWrMasterP(max_flight, big_endian, wr_addr_fifo, wr_data_fifo);
   return _ifc;

endmodule

module mkAxi4LWrMasterP#(parameter UInt#(32) max_flight,
			 parameter Bool big_endian,
			 BusSender#(Axi4LAddrCmd#(`TLM_PRM)) wr_addr_fifo,
			 BusSender#(Axi4LWrData#(`TLM_PRM))  wr_data_fifo)
                        (Axi4LWrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusReceiver#(AxiResp)                wr_resp_fifo <- mkBypassBusReceiver;

   FIFO#(req_t)                         fifo_rx      <- mkLFIFO;
   FIFO#(resp_t)                        fifo_tx      <- mkLFIFO;

   FIFO#(TLMId#(`TLM_PRM)) id_fifo <- mkSafeDepthParamFIFO(max_flight);

   let _ifc <- mkAxi4LWrMasterIFC(wr_addr_fifo.out, wr_data_fifo.out, wr_resp_fifo.in);

   let rx_first = toTLMRequest(fifo_rx.first);

   rule send_write (rx_first matches tagged Descriptor .dd
		    &&& dd.command == WRITE);
      let d = dd;
      if (d.byte_enable matches tagged Calculate)
	 d = addByteEnableU(big_endian, d);
      let addr_cmd = getAxi4LAddrCmd(d);
      let wr_data  = getAxi4LWrData(d);
      id_fifo.enq(d.transaction_id);
      wr_addr_fifo.in.enq(addr_cmd);
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
   endrule

   rule grab_response;
      let axi_response = wr_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxiResp(axi_response);
      response.transaction_id = id_fifo.first;
      id_fifo.deq;
      fifo_tx.enq(fromTLMResponse(response));
      wr_resp_fifo.out.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Error cases:
   ////////////////////////////////////////////////////////////////////////////////

   (* preempts = "start_read, grab_response" *)
   rule start_read (rx_first matches tagged Descriptor .d &&&
		    d.command matches READ);
      $display("(%0d) ERROR: Axi4LWrMaster cannot handle READ ops!", $time);
      fifo_rx.deq;
   endrule

   (* preempts = "data_write, grab_response" *)
   rule data_write (rx_first matches tagged Data .d);
      $display("(%0d) ERROR: Axi4LWrMaster cannot handle bursts!", $time);
      fifo_rx.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   interface TLMRecvIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface Axi4LWrFabricMaster fabric;
      interface Axi4LWrMaster bus = _ifc;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LRdWrMaster#(parameter UInt#(32) max_flight,
			 parameter Bool big_endian)
                        (Axi4LRdWrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   Axi4LRdMasterXActorIFC#(`TLM_XTR) master_rd_xactor <- mkAxi4LRdMaster(max_flight);
   Axi4LWrMasterXActorIFC#(`TLM_XTR) master_wr_xactor <- mkAxi4LWrMaster(max_flight, big_endian);

   FIFO#(req_t)  request_fifo  <- mkLFIFO;
   FIFO#(resp_t) response_fifo <- mkLFIFO;


   TLMSendIFC#(`TLM_RR) ifc = (interface TLMSendIFC
				  interface tx = toGet(request_fifo);
				  interface rx = toPut(response_fifo);
			       endinterface);

   let split <- mkTLMSplit(max_flight, ifc);

   mkConnection(split.read,  master_rd_xactor.tlm);
   mkConnection(split.write, master_wr_xactor.tlm);

   interface TLMRecvIFC tlm;
      interface tx = toGet(response_fifo);
      interface rx = toPut(request_fifo);
   endinterface

   interface read   = master_rd_xactor.fabric;
   interface write  = master_wr_xactor.fabric;

endmodule

endpackage
