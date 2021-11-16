// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi4LSlave;

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

module mkAxi4LRdSlaveIFC#(BusRecv#(Axi4LAddrCmd#(`TLM_PRM)) request_addr,
			  BusSend#(Axi4LRdResp#(`TLM_PRM))  response) (Axi4LRdSlave#(`TLM_PRM));

   Wire#(AxiProt)            arPROT_wire   <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_PRM)) arADDR_wire   <- mkBypassWire;

   rule every;
      let addr_value = Axi4LAddrCmd {prot:      arPROT_wire,
				     addr:      arADDR_wire};
      request_addr.data(addr_value);
   endrule

   // Address Inputs
   method arADDR   = arADDR_wire._write;
   method arPROT   = arPROT_wire._write;
   method arVALID  = request_addr.valid;

   // Address Outputs
   method arREADY  = request_addr.ready;

   // Response Inputs
   method rREADY  = response.ready;

   // Response Outputs
   method rDATA   = response.data.data;
   method rRESP   = response.data.resp;
   method rVALID  = response.valid;

endmodule

module mkAxi4LWrSlaveIFC#(BusRecv#(Axi4LAddrCmd#(`TLM_PRM)) request_addr,
			  BusRecv#(Axi4LWrData#(`TLM_PRM))  request_data,
			  BusSend#(AxiResp)                 response)
                         (Axi4LWrSlave#(`TLM_PRM));

   Wire#(AxiProt)            awPROT_wire  <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_PRM)) awADDR_wire  <- mkBypassWire;

   Wire#(AxiData#(`TLM_PRM))   wDATA_wire  <- mkBypassWire;
   Wire#(AxiByteEn#(`TLM_PRM)) wSTRB_wire  <- mkBypassWire;

   rule every;
      let addr_value = Axi4LAddrCmd {prot:  awPROT_wire,
				     addr:  awADDR_wire};
      request_addr.data(addr_value);
      let data_value = Axi4LWrData {data:  wDATA_wire,
				    strb:  wSTRB_wire};
      request_data.data(data_value);
   endrule

   // Address Inputs
   method awADDR   = awADDR_wire._write;
   method awPROT   = awPROT_wire._write;
   method awVALID  = request_addr.valid;

   // Address Outputs
   method awREADY  = request_addr.ready;

   // Data Inputs
   method wDATA    = wDATA_wire._write;
   method wSTRB    = wSTRB_wire._write;
   method wVALID   = request_data.valid;

   // Data Outputs
   method wREADY   = request_data.ready;

   // Response Inputs
   method bREADY  = response.ready;

   // Response Outputs
   method bRESP   = response.data;
   method bVALID  = response.valid;

endmodule

module mkAxi4LRdBusSlaveIFC#(Axi4LRdSlave#(`TLM_PRM) ifc) (Axi4LRdBusSlave#(`TLM_PRM));

   interface BusRecv addr;
      method Action data(Axi4LAddrCmd#(`TLM_PRM) value);
	 ifc.arADDR(value.addr);
	 ifc.arPROT(value.prot);
      endmethod
      method valid = ifc.arVALID;
      method ready = ifc.arREADY;
   endinterface
   interface BusSend resp;
      method Axi4LRdResp#(`TLM_PRM) data;
	 let resp = Axi4LRdResp {data: ifc.rDATA,
				 resp: ifc.rRESP};
	 return resp;
      endmethod
      method valid = ifc.rVALID;
      method ready = ifc.rREADY;
   endinterface

endmodule

module mkAxi4LWrBusSlaveIFC#(Axi4LWrSlave#(`TLM_PRM) ifc) (Axi4LWrBusSlave#(`TLM_PRM));

   interface BusRecv addr;
      method Action data(Axi4LAddrCmd#(`TLM_PRM) value);
	 ifc.awADDR(value.addr);
	 ifc.awPROT(value.prot);
      endmethod
      method valid = ifc.awVALID;
      method ready = ifc.awREADY;
   endinterface
   interface BusRecv data;
      method Action data(Axi4LWrData#(`TLM_PRM) value);
	 ifc.wDATA(value.data);
	 ifc.wSTRB(value.strb);
      endmethod
      method valid = ifc.wVALID;
      method ready = ifc.wREADY;
   endinterface
   interface BusSend resp;
      method AxiResp data;
	 return ifc.bRESP;
      endmethod
      method valid = ifc.bVALID;
      method ready = ifc.bREADY;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LRdSlave#(function Bool addr_match(AxiAddr#(`TLM_PRM) addr))
		      (Axi4LRdSlaveXActorIFC#(`TLM_XTR))
   provisos (TLMRequestTC#(req_t, `TLM_PRM),
	     TLMResponseTC#(resp_t, `TLM_PRM),
	     Bits#(req_t, s0),
	     Bits#(resp_t, s1),
	     Add#(_1, SizeOf#(TLMErrorCode), data_size));

   let _ifc <- mkAxi4LRdSlaveSynth;

   interface TLMSendIFC tlm = _ifc.tlm;
   interface Axi4LRdFabricSlave fabric;
      interface Axi4LRdSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule

module mkAxi4LRdSlaveSynth (Axi4LRdSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   BusReceiver#(Axi4LAddrCmd#(`TLM_PRM)) rd_addr_fifo <- mkBypassBusReceiver;
   BusSender#(Axi4LRdResp#(`TLM_PRM))    rd_resp_fifo <- mkBypassBusSender(unpack(0));

   FIFO#(req_t)        fifo_tx      <- mkLFIFO;
   FIFO#(resp_t)       fifo_rx      <- mkLFIFO;

   Reg#(RequestDescriptor#(`TLM_PRM)) desc_prev    <- mkReg(?);
   Reg#(Bool)                         just_one     <- mkReg(False);

   let _ifc <- mkAxi4LRdSlaveIFC(rd_addr_fifo.in, rd_resp_fifo.out);

   rule grab_addr;
      let value = rd_addr_fifo.out.first;
      let desc = fromAxi4LAddrCmd(value);
      desc.command = READ;
      rd_addr_fifo.out.deq;
      fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
   endrule

   rule grab_tlm_response;
      let response =  toTLMResponse(fifo_rx.first);
      fifo_rx.deq;
      Axi4LRdResp#(`TLM_PRM) axi_response;
      axi_response.resp = getAxiResp(response.status, response.data);
      axi_response.data = response.data;
      rd_resp_fifo.in.enq(axi_response);
   endrule

   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface Axi4LRdFabricSlave fabric;
      interface Axi4LRdSlave bus = _ifc;
      method Bool addrMatch(AxiAddr#(`TLM_PRM) value) = False;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LWrSlave#(function Bool addr_match(AxiAddr#(`TLM_PRM) addr))
                      (Axi4LWrSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   let _ifc <- mkAxi4LWrSlaveSynth;

   interface TLMSendIFC tlm = _ifc.tlm;
   interface Axi4LWrFabricSlave fabric;
      interface Axi4LWrSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule

module mkAxi4LWrSlaveSynth (Axi4LWrSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   BusReceiver#(Axi4LAddrCmd#(`TLM_PRM)) wr_addr_fifo <- mkBypassBusReceiver;
   BusReceiver#(Axi4LWrData#(`TLM_PRM))  wr_data_fifo <- mkBypassBusReceiver;
   let _ifc <- mkAxi4LWrSlaveSynthP(wr_addr_fifo, wr_data_fifo);
   return _ifc;

endmodule

module mkAxi4LWrSlaveSynthP#(BusReceiver#(Axi4LAddrCmd#(`TLM_PRM)) wr_addr_fifo,
			     BusReceiver#(Axi4LWrData#(`TLM_PRM))  wr_data_fifo) (Axi4LWrSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   BusSender#(AxiResp)    wr_resp_fifo <- mkBusSender(unpack(0));

   FIFO#(req_t)        fifo_tx      <- mkLFIFO;
   FIFO#(resp_t)       fifo_rx      <- mkLFIFO;

   let _ifc <- mkAxi4LWrSlaveIFC(wr_addr_fifo.in, wr_data_fifo.in, wr_resp_fifo.out);

   rule grab_addr;
      let value  = wr_addr_fifo.out.first;
      let dvalue = wr_data_fifo.out.first;
      wr_addr_fifo.out.deq;
      wr_data_fifo.out.deq;
      let desc = fromAxi4LAddrCmd(value);
      desc.command = WRITE;
      desc.data = dvalue.data;
      desc.byte_enable = tagged Specify dvalue.strb;
      fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
   endrule

   rule grab_tlm_response;
      let response = toTLMResponse(fifo_rx.first);
      fifo_rx.deq;
      AxiResp axi_response = getAxiResp(response.status, response.data);
      wr_resp_fifo.in.enq(axi_response);
   endrule

   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface Axi4LWrFabricSlave fabric;
      interface Axi4LWrSlave bus = _ifc;
      method Bool addrMatch(AxiAddr#(`TLM_PRM) value) = False;
   endinterface

endmodule

module mkAxi4LRdWrSlave#(function Bool addr_match(Bit#(addr_size) addr))
                        (Axi4LRdWrSlaveXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   Axi4LRdSlaveXActorIFC#(`TLM_XTR) slave_rd_xactor <- mkAxi4LRdSlave(addr_match);
   Axi4LWrSlaveXActorIFC#(`TLM_XTR) slave_wr_xactor <- mkAxi4LWrSlave(addr_match);

   FIFO#(req_t)  request_fifo  <- mkLFIFO;
   FIFO#(resp_t) response_fifo <- mkLFIFO;

   TLMRecvIFC#(`TLM_RR) ifc = (interface TLMRecvIFC
				  interface tx = toGet(response_fifo);
				  interface rx = toPut(request_fifo);
			       endinterface);

   let joiner <- mkTLMJoin(ifc);

   mkConnection(joiner.read,  slave_rd_xactor.tlm);
   mkConnection(joiner.write, slave_wr_xactor.tlm);

   interface TLMSendIFC tlm;
      interface tx = toGet(request_fifo);
      interface rx = toPut(response_fifo);
   endinterface

   interface read   = slave_rd_xactor.fabric;
   interface write  = slave_wr_xactor.fabric;

endmodule

/* -----\/----- EXCLUDED -----\/-----


module mkAxi4RdSlaveDummy(Axi4RdFabricSlave#(`TLM_PRM))
   provisos(Add#(SizeOf#(Axi4Len), 1, n));

   BusReceiver#(Axi4AddrCmd#(`TLM_PRM)) rd_addr_fifo <- mkBypassBusReceiver;
   BusSender#(Axi4RdResp#(`TLM_PRM))    rd_resp_fifo <- mkBypassBusSender(unpack(0));

   Reg#(UInt#(n))                         count        <- mkReg(0);
   Reg#(AxiId#(`TLM_PRM))                 id_prev      <- mkReg(?);

   let _ifc <- mkAxi4RdSlaveIFC(rd_addr_fifo.in, rd_resp_fifo.out);

   rule grab_addr (count == 0);
      let value = rd_addr_fifo.out.first;
      UInt#(n) remaining = extend(value.len) + 1;
      count <= remaining;
      id_prev <= value.id;
      rd_addr_fifo.out.deq;
   endrule

   rule do_read (count != 0);
      let remaining = count - 1;
      let last = (remaining == 0);
      count <= remaining;
      Axi4RdResp#(`TLM_PRM) axi_response = unpack(0);
      axi_response.id = id_prev;
      axi_response.resp = DECERR;
      axi_response.data = 0;
      axi_response.last = remaining == 0;
      rd_resp_fifo.in.enq(axi_response);
   endrule

   interface Axi4RdSlave bus = _ifc;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value) = False;

endmodule

module mkAxi4WrSlaveDummy(Axi4WrFabricSlave#(`TLM_PRM))
   provisos(Add#(SizeOf#(Axi4Len), 1, n));

   BusReceiver#(Axi4AddrCmd#(`TLM_PRM)) wr_addr_fifo <- mkBypassBusReceiver;
   BusReceiver#(Axi4WrData#(`TLM_PRM))  wr_data_fifo <- mkBypassBusReceiver;
   BusSender#(Axi4WrResp#(`TLM_PRM))    wr_resp_fifo <- mkBusSender(unpack(0));

   Reg#(UInt#(n))                       count        <- mkReg(0);
   Reg#(AxiId#(`TLM_PRM))               id_prev      <- mkReg(?);

   let _ifc <- mkAxi4WrSlaveIFC(wr_addr_fifo.in, wr_data_fifo.in, wr_resp_fifo.out);

   rule grab_addr (count == 0);
      let value  = wr_addr_fifo.out.first;
      let dvalue = wr_data_fifo.out.first;
      wr_addr_fifo.out.deq;
      wr_data_fifo.out.deq;
      UInt#(n) remaining = extend(value.len);
      count <= remaining;
      id_prev <= value.id;
      Axi4WrResp#(`TLM_PRM) axi_response = unpack(0);
      axi_response.id = value.id;
      axi_response.resp = DECERR;
      if (remaining == 0) wr_resp_fifo.in.enq(axi_response);
   endrule

   rule grab_data (count != 0);
      let value = wr_data_fifo.out.first;
      wr_data_fifo.out.deq;
      let remaining =  count - 1;
      count <= remaining;
      Axi4WrResp#(`TLM_PRM) axi_response = unpack(0);
      axi_response.id = id_prev;
      axi_response.resp = DECERR;
      if (remaining == 0) wr_resp_fifo.in.enq(axi_response);
   endrule

   interface Axi4WrSlave bus = _ifc;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value) = False;

endmodule

module mkAxi4WrSlaveDummyX(Axi4WrFabricSlave#(`TLM_PRM));

   Reg#(AxiId#(`TLM_PRM))  rAWId         <- mkRegU;
   Reg#(Bool)              rwLast        <- mkReg(False);

   Wire#(Bool)             awValid       <- mkBypassWire;
   Wire#(Bool)             wValid        <- mkBypassWire;
   Reg#(Bool)              rBReady       <- mkReg(False);

   function Action nop(a ignore);
      return noAction;
   endfunction

   interface Axi4WrSlave bus;
      method awID     = rAWId._write;
      method awADDR   = nop;
      method awREGION = nop;
      method awUSER   = nop;
      method awLEN    = nop;
      method awSIZE   = nop;
      method awBURST  = nop;
      method awLOCK   = nop;
      method awCACHE  = nop;
      method awPROT   = nop;
      method awQOS    = nop;
      method awVALID  = awValid._write;
      method awREADY  = True;
      method wDATA    = nop;
      method wUSER    = nop;
      method wSTRB    = nop;
      method wLAST    = rwLast._write;
      method wVALID   = wValid._write;
      method wREADY   = True;

      method bREADY   = rBReady._write;
      method bID      = rAWId;
      method bRESP    = DECERR;
      method bUSER    = 0;
      method bVALID   = True;
   endinterface

   method Bool addrMatch(value);
      return False;
   endmethod

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

 -----/\----- EXCLUDED -----/\----- */

endpackage
