// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi4Slave;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Axi4Defines::*;
import AxiDefines::*;
import Bus::*;
import CBus::*;
import Connectable::*;
import FIFO::*;
import FIFOF::*;
import GetPut::*;
import SpecialFIFOs::*;
import TLM3::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4RdSlaveIFC#(BusRecv#(Axi4AddrCmd#(`TLM_PRM)) request_addr,
			BusSend#(Axi4RdResp#(`TLM_PRM))  response) (Axi4RdSlave#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM))   arID_wire     <- mkBypassWire;
   Wire#(Axi4Len)            arLEN_wire    <- mkBypassWire;
   Wire#(AxiSize)            arSIZE_wire   <- mkBypassWire;
   Wire#(AxiBurst)           arBURST_wire  <- mkBypassWire;
   Wire#(Axi4Lock)           arLOCK_wire   <- mkBypassWire;
   Wire#(Axi4Cache)          arCACHE_wire  <- mkBypassWire;
   Wire#(AxiProt)            arPROT_wire   <- mkBypassWire;
   Wire#(Axi4Qos)            arQOS_wire    <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_PRM)) arADDR_wire   <- mkBypassWire;
   Wire#(Axi4Region)         arREGION_wire <- mkBypassWire;
   Wire#(TLMUser#(`TLM_PRM)) arUSER_wire   <- mkBypassWire;

   rule every;
      let addr_value = Axi4AddrCmd {id:        arID_wire,
				    len:       arLEN_wire,
				    size:      arSIZE_wire,
				    burst:     arBURST_wire,
				    lock:      arLOCK_wire,
				    cache:     arCACHE_wire,
				    prot:      arPROT_wire,
				    qos:       arQOS_wire,
				    addr:      arADDR_wire,
				    region:    arREGION_wire,
				    user_addr: arUSER_wire};
      request_addr.data(addr_value);
   endrule

   // Address Inputs
   method arID     = arID_wire._write;
   method arADDR   = arADDR_wire._write;
   method arREGION = arREGION_wire._write;
   method arUSER   = arUSER_wire._write;
   method arLEN    = arLEN_wire._write;
   method arSIZE   = arSIZE_wire._write;
   method arBURST  = arBURST_wire._write;
   method arLOCK   = arLOCK_wire._write;
   method arCACHE  = arCACHE_wire._write;
   method arPROT   = arPROT_wire._write;
   method arQOS    = arQOS_wire._write;
   method arVALID  = request_addr.valid;

   // Address Outputs
   method arREADY  = request_addr.ready;

   // Response Inputs
   method rREADY  = response.ready;

   // Response Outputs
   method rID     = response.data.id;
   method rDATA   = response.data.data;
   method rRESP   = response.data.resp;
   method rUSER   = response.data.user;
   method rLAST   = response.data.last;
   method rVALID  = response.valid;

endmodule

module mkAxi4WrSlaveIFC#(BusRecv#(Axi4AddrCmd#(`TLM_PRM)) request_addr,
			 BusRecv#(Axi4WrData#(`TLM_PRM))  request_data,
			 BusSend#(Axi4WrResp#(`TLM_PRM))  response) (Axi4WrSlave#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM))   awID_wire    <- mkBypassWire;
   Wire#(Axi4Len)            awLEN_wire   <- mkBypassWire;
   Wire#(AxiSize)            awSIZE_wire  <- mkBypassWire;
   Wire#(AxiBurst)           awBURST_wire <- mkBypassWire;
   Wire#(Axi4Lock)           awLOCK_wire  <- mkBypassWire;
   Wire#(Axi4Cache)          awCACHE_wire <- mkBypassWire;
   Wire#(AxiProt)            awPROT_wire  <- mkBypassWire;
   Wire#(Axi4Qos)            awQOS_wire   <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_PRM)) awADDR_wire  <- mkBypassWire;
   Wire#(Axi4Region)         awREGION_wire <- mkBypassWire;
   Wire#(TLMUser#(`TLM_PRM)) awUSER_wire  <- mkBypassWire;

   Wire#(AxiData#(`TLM_PRM))   wDATA_wire  <- mkBypassWire;
   Wire#(TLMUser#(`TLM_PRM))   wUSER_wire  <- mkBypassWire;
   Wire#(AxiByteEn#(`TLM_PRM)) wSTRB_wire  <- mkBypassWire;
   Wire#(Bool)                 wLAST_wire  <- mkBypassWire;


   rule every;
      let addr_value = Axi4AddrCmd {id:    awID_wire,
				    len:   awLEN_wire,
				    size:  awSIZE_wire,
				    burst: awBURST_wire,
				    lock:  awLOCK_wire,
				    cache: awCACHE_wire,
				    prot:  awPROT_wire,
				    qos:   awQOS_wire,
				    addr:  awADDR_wire,
				    region:    awREGION_wire,
				    user_addr: awUSER_wire};
      request_addr.data(addr_value);
      let data_value = Axi4WrData {data:  wDATA_wire,
				   user:  wUSER_wire,
				   strb:  wSTRB_wire,
				   last:  wLAST_wire};
      request_data.data(data_value);
   endrule

   // Address Inputs
   method awID     = awID_wire._write;
   method awADDR   = awADDR_wire._write;
   method awREGION = awREGION_wire._write;
   method awUSER   = awUSER_wire._write;
   method awLEN    = awLEN_wire._write;
   method awSIZE   = awSIZE_wire._write;
   method awBURST  = awBURST_wire._write;
   method awLOCK   = awLOCK_wire._write;
   method awCACHE  = awCACHE_wire._write;
   method awPROT   = awPROT_wire._write;
   method awQOS    = awQOS_wire._write;
   method awVALID  = request_addr.valid;

   // Address Outputs
   method awREADY  = request_addr.ready;

   // Data Inputs
   method wDATA    = wDATA_wire._write;
   method wUSER    = wUSER_wire._write;
   method wSTRB    = wSTRB_wire._write;
   method wLAST    = wLAST_wire._write;
   method wVALID   = request_data.valid;

   // Data Outputs
   method wREADY   = request_data.ready;

   // Response Inputs
   method bREADY  = response.ready;

   // Response Outputs
   method bID     = response.data.id;
   method bRESP   = response.data.resp;
   method bUSER   = response.data.user;
   method bVALID  = response.valid;

endmodule

module mkAxi4RdBusSlaveIFC#(Axi4RdSlave#(`TLM_PRM) ifc) (Axi4RdBusSlave#(`TLM_PRM));

   interface BusRecv addr;
      method Action data(Axi4AddrCmd#(`TLM_PRM) value);
	 ifc.arID(value.id);
	 ifc.arADDR(value.addr);
	 ifc.arREGION(value.region);
	 ifc.arLEN(value.len);
	 ifc.arSIZE(value.size);
	 ifc.arBURST(value.burst);
	 ifc.arLOCK(value.lock);
	 ifc.arCACHE(value.cache);
	 ifc.arPROT(value.prot);
	 ifc.arQOS(value.qos);
      endmethod
      method valid = ifc.arVALID;
      method ready = ifc.arREADY;
   endinterface
   interface BusSend resp;
      method Axi4RdResp#(`TLM_PRM) data;
	 let resp = Axi4RdResp {id:   ifc.rID,
				data: ifc.rDATA,
				resp: ifc.rRESP,
				user: ifc.rUSER,
				last: ifc.rLAST};
	 return resp;
      endmethod
      method valid = ifc.rVALID;
      method ready = ifc.rREADY;
   endinterface

endmodule

module mkAxi4WrBusSlaveIFC#(Axi4WrSlave#(`TLM_PRM) ifc) (Axi4WrBusSlave#(`TLM_PRM));

   interface BusRecv addr;
      method Action data(Axi4AddrCmd#(`TLM_PRM) value);
	 ifc.awID(value.id);
	 ifc.awADDR(value.addr);
	 ifc.awREGION(value.region);
	 ifc.awLEN(value.len);
	 ifc.awSIZE(value.size);
	 ifc.awBURST(value.burst);
	 ifc.awLOCK(value.lock);
	 ifc.awCACHE(value.cache);
	 ifc.awPROT(value.prot);
	 ifc.awQOS(value.qos);
      endmethod
      method valid = ifc.awVALID;
      method ready = ifc.awREADY;
   endinterface
   interface BusRecv data;
      method Action data(Axi4WrData#(`TLM_PRM) value);
	 ifc.wDATA(value.data);
	 ifc.wSTRB(value.strb);
	 ifc.wLAST(value.last);
      endmethod
      method valid = ifc.wVALID;
      method ready = ifc.wREADY;
   endinterface
   interface BusSend resp;
      method Axi4WrResp#(`TLM_PRM) data;
	 let resp = Axi4WrResp {id:   ifc.bID,
				resp: ifc.bRESP,
				user: ifc.bUSER};
	 return resp;
      endmethod
      method valid = ifc.bVALID;
      method ready = ifc.bREADY;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4RdSlave#(parameter Bool keep_bursts,
		     function Bool addr_match(AxiAddr#(`TLM_PRM) addr))
		    (Axi4RdSlaveXActorIFC#(`TLM_XTR))
   provisos (TLMRequestTC#(req_t, `TLM_PRM),
	     TLMResponseTC#(resp_t, `TLM_PRM),
	     Bits#(req_t, s0),
	     Bits#(resp_t, s1),
	     Add#(_1, SizeOf#(TLMErrorCode), data_size));

   let _ifc <- mkAxi4RdSlaveSynth(keep_bursts);

   interface TLMSendIFC tlm = _ifc.tlm;
   interface Axi4RdFabricSlave fabric;
      interface Axi4RdSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule

module mkAxi4RdSlaveSynth#(parameter Bool keep_bursts) (Axi4RdSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(SizeOf#(Axi4Len), 1, n),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));


   BusReceiver#(Axi4AddrCmd#(`TLM_PRM)) rd_addr_fifo <- mkBypassBusReceiver;
   BusSender#(Axi4RdResp#(`TLM_PRM))    rd_resp_fifo <- mkBypassBusSender(unpack(0));

   FIFO#(req_t)        fifo_tx      <- mkLFIFO;
   FIFO#(resp_t)       fifo_rx      <- mkLFIFO;

   Reg#(UInt#(n))                     count        <- mkReg(0);
   Reg#(RequestDescriptor#(`TLM_PRM)) desc_prev    <- mkReg(?);
   Reg#(Bool)                         just_one     <- mkReg(False);

   let _ifc <- mkAxi4RdSlaveIFC(rd_addr_fifo.in, rd_resp_fifo.out);

   rule grab_addr (count == 0);
      let value = rd_addr_fifo.out.first;
      UInt#(n) remaining = extend(value.len) + 1;
      let desc = fromAxi4AddrCmd(value);
      let keep = desc.b_length != 0 && keep_bursts;
      desc.command = READ;
      count <= remaining;
      desc_prev <= desc;
      rd_addr_fifo.out.deq;
      just_one <= keep;
   endrule

   rule do_read (count != 0);
      let desc = desc_prev;
      let keep = desc.b_length != 0 && keep_bursts;
      let remaining = count - 1;
      let last = (remaining == 0);
      count <= remaining;
      if (!keep)
	 begin
	    desc_prev <= incrTLMAddr(alignAddress(desc));
	    desc.b_length = 0;
	    desc.mark = (remaining == 0) ? LAST : NOT_LAST;
	    fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
	 end
      if (keep && just_one)
	 begin
	    just_one <= False;
	    fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
	 end
   endrule

   rule grab_tlm_response;
      let response =  toTLMResponse(fifo_rx.first);
      let id = response.transaction_id;
      fifo_rx.deq;
      Axi4RdResp#(`TLM_PRM) axi_response;
      axi_response.id = getAxiId(id);
      axi_response.resp = getAxiResp(response.status, response.data);
      axi_response.data = response.data;
      axi_response.user = response.user;
      axi_response.last = response.is_last;
      rd_resp_fifo.in.enq(axi_response);
   endrule

   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx;
	 method Action put (value);
	    fifo_rx.enq(value);
	 endmethod
      endinterface
   endinterface

   interface Axi4RdFabricSlave fabric;
      interface Axi4RdSlave bus = _ifc;
      method Bool addrMatch(AxiAddr#(`TLM_PRM) value) = False;
   endinterface

endmodule


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4WrSlave#(parameter Bool keep_bursts,
		     function Bool addr_match(AxiAddr#(`TLM_PRM) addr))
                    (Axi4WrSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   let _ifc <- mkAxi4WrSlaveSynth(keep_bursts);

   interface TLMSendIFC tlm = _ifc.tlm;
   interface Axi4WrFabricSlave fabric;
      interface Axi4WrSlave bus = _ifc.fabric.bus;
      method addrMatch = addr_match;
   endinterface
endmodule

module mkAxi4WrSlaveSynth#(parameter Bool keep_bursts) (Axi4WrSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(SizeOf#(Axi4Len), 1, n),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   BusReceiver#(Axi4AddrCmd#(`TLM_PRM)) wr_addr_fifo <- mkBypassBusReceiver;
   BusReceiver#(Axi4WrData#(`TLM_PRM))  wr_data_fifo <- mkBypassBusReceiver;
   let _ifc <- mkAxi4WrSlaveSynthP(keep_bursts, wr_addr_fifo, wr_data_fifo);
   return _ifc;

endmodule

module mkAxi4WrSlaveSynthP#(parameter Bool keep_bursts,
			   BusReceiver#(Axi4AddrCmd#(`TLM_PRM)) wr_addr_fifo,
			   BusReceiver#(Axi4WrData#(`TLM_PRM))  wr_data_fifo) (Axi4WrSlaveXActorIFC#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(SizeOf#(Axi4Len), 1, n),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   BusSender#(Axi4WrResp#(`TLM_PRM))    wr_resp_fifo <- mkBusSender(unpack(0));

   FIFO#(req_t)        fifo_tx      <- mkLFIFO;
   FIFO#(resp_t)       fifo_rx      <- mkLFIFO;

   let _ifc <- mkAxi4WrSlaveIFC(wr_addr_fifo.in, wr_data_fifo.in, wr_resp_fifo.out);

   Reg#(UInt#(n))                      count        <- mkReg(0);
   Reg#(RequestDescriptor#(`TLM_PRM))  desc_prev    <- mkReg(?);

   rule grab_addr (count == 0);
      let value  = wr_addr_fifo.out.first;
      let dvalue = wr_data_fifo.out.first;
      wr_addr_fifo.out.deq;
      wr_data_fifo.out.deq;
      UInt#(n) remaining = extend(value.len);
      count <= remaining;
      let desc = fromAxi4AddrCmd(value);
      desc.command = WRITE;
      desc.data = dvalue.data;
      desc.user = dvalue.user;
      desc.byte_enable = tagged Specify dvalue.strb;
      let keep = desc.b_length != 0 && keep_bursts;
      desc_prev <= (keep) ? desc : incrTLMAddr(alignAddress(desc));
      if (!keep)
	 begin
	    desc.b_length = 0;
	    desc.mark = (dvalue.last) ? LAST : NOT_LAST;
	 end
      fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
   endrule

   rule grab_data (count != 0);
      let value = wr_data_fifo.out.first;
      wr_data_fifo.out.deq;
      let remaining =  count - 1;
      count <= remaining;
      let desc = desc_prev;
      desc.data = value.data;
      desc.user = value.user;
      let keep = desc.b_length != 0 && keep_bursts;
      desc_prev <= (keep) ? desc : incrTLMAddr(desc);
      let rd = ?;
      if (keep)
	 begin
	    RequestData#(`TLM_PRM) data = ?;
	    data.data = desc.data;
	    data.user = desc.user;
	    data.transaction_id = desc.transaction_id;
	    data.byte_enable = tagged Specify value.strb;
	    rd = tagged Data data;
	 end
      else
	 begin
	    desc.byte_enable = tagged Specify value.strb;
	    desc.b_length = 0;
	    desc.mark = (value.last) ? LAST : NOT_LAST;
	    rd = tagged Descriptor desc;
	 end
      fifo_tx.enq(fromTLMRequest(rd));
   endrule

   rule grab_tlm_response (toTLMResponse(fifo_rx.first).is_last);
      let response = toTLMResponse(fifo_rx.first);
      fifo_rx.deq;
      Axi4WrResp#(`TLM_PRM) axi_response = unpack(0);
      axi_response.id = response.transaction_id;
      axi_response.resp = getAxiResp(response.status, response.data);
      axi_response.user = response.user;
      wr_resp_fifo.in.enq(axi_response);
   endrule

   rule grab_tlm_response_skip (!toTLMResponse(fifo_rx.first).is_last);
      fifo_rx.deq;
   endrule

   interface TLMSendIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface Axi4WrFabricSlave fabric;
      interface Axi4WrSlave bus = _ifc;
      method Bool addrMatch(AxiAddr#(`TLM_PRM) value) = False;
   endinterface

endmodule
/* -----\/----- EXCLUDED -----\/-----

interface SlaveSlice#(`TLM_XTR_DCL);
   interface GetS#(Axi4AddrCmd#(`TLM_PRM)) addr_f;
   interface GetS#(Axi4WrData#(`TLM_PRM))  data_f;
   interface ReadOnly#(Maybe#(TLMId#(`TLM_PRM))) id;
endinterface

module mkSlaveSlice#(parameter Bool keep_bursts,
              GetS#(Axi4AddrCmd#(`TLM_PRM)) addr_fifo,
	      GetS#(Axi4WrData#(`TLM_PRM))  data_fifo,
	      FIFO#(req_t) fifo_tx) (SlaveSlice#(`TLM_XTR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Add#(SizeOf#(Axi4Len), 1, n));

   Reg#(UInt#(n))                       count        <- mkReg(0);
   Reg#(RequestDescriptor#(`TLM_PRM))   desc_prev    <- mkReg(?);
   Reg#(Maybe#(TLMId#(`TLM_PRM)))       m_id         <- mkReg(tagged Invalid);

   Bool data_match = False;
   if(m_id matches tagged Valid .i &&&
      data_fifo.first.id == i)
      data_match = True;

   Bool addr_mismatch = False;
   if(m_id matches tagged Valid .i &&&
      addr_fifo.first.id != i)
      addr_mismatch = True;

   Bool data_mismatch = False;
   if(m_id matches tagged Valid .i &&&
      data_fifo.first.id != i)
      data_mismatch = True;

   rule grab_addr (count == 0 &&
		   !isValid(m_id) &&
		   addr_fifo.first.id == data_fifo.first.id);
      let value  = addr_fifo.first;
      let dvalue = data_fifo.first;
      addr_fifo.deq;
      data_fifo.deq;
      UInt#(n) remaining = extend(value.len);
      count <= remaining;
      let desc = fromAxi4AddrCmd(value);
      desc.command = WRITE;
      desc.data = dvalue.data;
      desc.byte_enable = tagged Specify dvalue.strb;
      let keep = desc.b_length != 0 && keep_bursts;
      desc_prev <= (keep) ? desc : incrTLMAddr(alignAddress(desc));
      if (!keep)
	 begin
	    desc.b_length = 0;
	    desc.mark = (dvalue.last) ? LAST : NOT_LAST;
	 end
      m_id <= (dvalue.last) ? (tagged Invalid) : tagged Valid addr_fifo.first.id;
      fifo_tx.enq(fromTLMRequest(tagged Descriptor desc));
   endrule

   rule grab_data (count != 0 && data_match);
      let value = data_fifo.first;
      data_fifo.deq;
      let remaining =  count - 1;
      count <= remaining;
      let desc = desc_prev;
      desc.data = value.data;
      let keep = desc.b_length != 0 && keep_bursts;
      desc_prev <= (keep) ? desc : incrTLMAddr(desc);
      let rd = ?;
      if (keep)
	 begin
	    RequestData#(`TLM_PRM) data = ?;
	    data.data = desc.data;
	    data.transaction_id = desc.transaction_id;
	    data.byte_enable = tagged Specify value.strb;
	    rd = tagged Data data;
	 end
      else
	 begin
	    desc.byte_enable = tagged Specify value.strb;
	    desc.b_length = 0;
	    desc.mark = (value.last) ? LAST : NOT_LAST;
	    rd = tagged Descriptor desc;
	 end
      m_id <= (value.last) ? (tagged Invalid) : m_id;
      fifo_tx.enq(fromTLMRequest(rd));
   endrule

   interface GetS addr_f;
      method Action deq if (addr_mismatch);
	 addr_fifo.deq;
      endmethod
      method first if (addr_mismatch);
	 return addr_fifo.first;
      endmethod
   endinterface

   interface GetS data_f;
      method Action deq if(data_mismatch);
	 data_fifo.deq;
      endmethod
      method first if(data_mismatch);
	 return data_fifo.first;
      endmethod
   endinterface

   interface ReadOnly id;
      method _read = m_id;
   endinterface

endmodule
 -----/\----- EXCLUDED -----/\----- */

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

   method addrMatch(value);
      return False;
   endmethod

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4RdWrSlave#(parameter Bool keep_bursts,
		       function Bool addr_match(Bit#(addr_size) addr))
                      (Axi4RdWrSlaveXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Add#(_1, SizeOf#(TLMErrorCode), data_size));

   Axi4RdSlaveXActorIFC#(`TLM_XTR) slave_rd_xactor <- mkAxi4RdSlave(keep_bursts, addr_match);
   Axi4WrSlaveXActorIFC#(`TLM_XTR) slave_wr_xactor <- mkAxi4WrSlave(keep_bursts, addr_match);

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

endpackage
