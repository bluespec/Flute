// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi4Master;

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

module mkAxi4RdMasterIFC#(BusSend#(Axi4AddrCmd#(`TLM_PRM))  request_addr,
			  BusRecv#(Axi4RdResp#(`TLM_PRM))   response) (Axi4RdMaster#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM))   rID_wire   <- mkBypassWire;
   Wire#(AxiData#(`TLM_PRM)) rDATA_wire <- mkBypassWire;
   Wire#(AxiResp)            rRESP_wire <- mkBypassWire;
   Wire#(TLMUser#(`TLM_PRM)) rUSER_wire <- mkBypassWire;
   Wire#(Bool)               rLAST_wire <- mkBypassWire;

   rule every ;
      let value = Axi4RdResp {id:   rID_wire,
			      data: rDATA_wire,
			      resp: rRESP_wire,
			      user: rUSER_wire,
			      last: rLAST_wire};
      response.data(value);
   endrule

   // Address Outputs
   method arID    = request_addr.data.id;
   method arADDR  = request_addr.data.addr;
   method arUSER  = request_addr.data.user_addr;
   method arLEN   = request_addr.data.len;
   method arSIZE  = request_addr.data.size;
   method arBURST = request_addr.data.burst;
   method arLOCK  = request_addr.data.lock;
   method arCACHE = request_addr.data.cache;
   method arPROT  = request_addr.data.prot;
   method arQOS   = request_addr.data.qos;
   method arVALID = request_addr.valid;

   // Address Inputs
   method arREADY = request_addr.ready;

   // Response Outputs
   method rREADY  = response.ready;

   // Response Inputs
   method rID     = rID_wire._write;
   method rDATA   = rDATA_wire._write;
   method rRESP   = rRESP_wire._write;
   method rUSER   = rUSER_wire._write;
   method rLAST   = rLAST_wire._write;
   method rVALID  = response.valid;

endmodule

module mkAxi4WrMasterIFC#(BusSend#(Axi4AddrCmd#(`TLM_PRM))  request_addr,
			  BusSend#(Axi4WrData#(`TLM_PRM))   request_data,
			  BusRecv#(Axi4WrResp#(`TLM_PRM))   response) (Axi4WrMaster#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM))   bID_wire    <- mkBypassWire;
   Wire#(AxiResp)            bRESP_wire  <- mkBypassWire;
   Wire#(TLMUser#(`TLM_PRM)) bUSER_wire  <- mkBypassWire;

   rule every ;
      let value = Axi4WrResp { id: bID_wire, resp: bRESP_wire, user: bUSER_wire};
      response.data(value);
   endrule

   // Address Outputs
   method awID    = request_addr.data.id;
   method awADDR  = request_addr.data.addr;
   method awUSER  = request_addr.data.user_addr;
   method awLEN   = request_addr.data.len;
   method awSIZE  = request_addr.data.size;
   method awBURST = request_addr.data.burst;
   method awLOCK  = request_addr.data.lock;
   method awCACHE = request_addr.data.cache;
   method awPROT  = request_addr.data.prot;
   method awQOS   = request_addr.data.qos;
   method awVALID = request_addr.valid;

   // Address Inputs
   method awREADY = request_addr.ready;

   // Data Outputs
   method wDATA   = request_data.data.data;
   method wUSER   = request_data.data.user;
   method wSTRB   = request_data.data.strb;
   method wLAST   = request_data.data.last;
   method wVALID  = request_data.valid;

   // Data Inputs
   method wREADY  = request_data.ready;

   // Response Outputs
   method bREADY  = response.ready;

   // Response Inputs
   method bID     = bID_wire._write;
   method bRESP   = bRESP_wire._write;
   method bUSER   = bUSER_wire._write;
   method bVALID  = response.valid;

endmodule

module mkAxi4RdBusMasterIFC#(Axi4RdMaster#(`TLM_PRM) ifc) (Axi4RdBusMaster#(`TLM_PRM));

   interface BusSend addr;
      method Axi4AddrCmd#(`TLM_PRM) data;
	 let addr =  Axi4AddrCmd {id:    ifc.arID,
				  len:   ifc.arLEN,
				  size:  ifc.arSIZE,
				  burst: ifc.arBURST,
				  lock:  ifc.arLOCK,
				  cache: ifc.arCACHE,
				  prot:  ifc.arPROT,
				  qos:   ifc.arQOS,
				  addr:  ifc.arADDR,
				  region: unpack(0),
				  user_addr: ifc.arUSER};
	 return addr;
      endmethod
      method valid = ifc.arVALID;
      method ready = ifc.arREADY;
   endinterface
   interface BusRecv resp;
      method Action data(Axi4RdResp#(`TLM_PRM) value);
	 ifc.rID(value.id);
	 ifc.rDATA(value.data);
	 ifc.rRESP(value.resp);
	 ifc.rLAST(value.last);
      endmethod
      method valid = ifc.rVALID;
      method ready = ifc.rREADY;
   endinterface

endmodule

module mkAxi4WrBusMasterIFC#(Axi4WrMaster#(`TLM_PRM) ifc) (Axi4WrBusMaster#(`TLM_PRM));

   interface BusSend addr;
      method Axi4AddrCmd#(`TLM_PRM) data;
	 let addr =  Axi4AddrCmd {id:    ifc.awID,
				  len:   ifc.awLEN,
				  size:  ifc.awSIZE,
				  burst: ifc.awBURST,
				  lock:  ifc.awLOCK,
				  cache: ifc.awCACHE,
				  prot:  ifc.awPROT,
				  qos:   ifc.awQOS,
				  addr:  ifc.awADDR,
				  region: unpack(0),
				  user_addr: ifc.awUSER};
	 return addr;
      endmethod
      method valid = ifc.awVALID;
      method ready = ifc.awREADY;
   endinterface
   interface BusSend data;
      method Axi4WrData#(`TLM_PRM) data;
	 let out = Axi4WrData {data: ifc.wDATA,
			       user: ifc.wUSER,
			       strb: ifc.wSTRB,
			       last: ifc.wLAST};
	 return out;
      endmethod
      method valid = ifc.wVALID;
      method ready = ifc.wREADY;
   endinterface
   interface BusRecv resp;
      method Action data(Axi4WrResp#(`TLM_PRM) value);
	 ifc.bID(value.id);
	 ifc.bRESP(value.resp);
	 ifc.bUSER(value.user);
      endmethod
      method valid = ifc.bVALID;
      method ready = ifc.bREADY;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4RdMaster#(parameter UInt#(32) max_flight) (Axi4RdMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusSender#(Axi4AddrCmd#(`TLM_PRM)) rd_addr_fifo <- mkBypassBusSender(unpack(0));
   BusReceiver#(Axi4RdResp#(`TLM_PRM)) rd_resp_fifo <- mkBypassBusReceiver;

   FIFO#(req_t)                       fifo_rx <- mkLFIFO;
   FIFO#(resp_t)                      fifo_tx <- mkLFIFO;
   Reg#(TLMBLength#(`TLM_PRM))        count   <- mkReg(0);

   let _ifc <- mkAxi4RdMasterIFC(rd_addr_fifo.out, rd_resp_fifo.in);

   let rx_first = toTLMRequest(fifo_rx.first);

   rule send_read (rx_first matches tagged Descriptor .d
		   &&& d.command matches READ);
      let addr_cmd = getAxi4AddrCmd(d);
      rd_addr_fifo.in.enq(addr_cmd);
      fifo_rx.deq;
   endrule

   rule grab_response;
      let axi_response = rd_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxi4RdResp(axi_response);
      rd_resp_fifo.out.deq;
      fifo_tx.enq(fromTLMResponse(response));
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Error cases:
   ////////////////////////////////////////////////////////////////////////////////
   (* preempts = "start_write, grab_response" *)
   rule start_write (rx_first matches tagged Descriptor .d
		     &&& d.command matches WRITE);
      $display("(%0d) ERROR: Axi4RdMaster cannot handle WRITE ops!", $time);
      fifo_rx.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   interface TLMRecvIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface Axi4RdFabricMaster fabric;
      interface Axi4RdMaster bus = _ifc;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4WrMaster#(parameter UInt#(32) max_flight,
		       parameter Bool big_endian)
                     (Axi4WrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusSender#(Axi4AddrCmd#(`TLM_PRM)) wr_addr_fifo <- mkBypassBusSender(unpack(0));
   BusSender#(Axi4WrData#(`TLM_PRM))  wr_data_fifo <- mkBypassBusSender(unpack(0));

   let _ifc <- mkAxi4WrMasterP(max_flight, big_endian, wr_addr_fifo, wr_data_fifo);
   return _ifc;

endmodule

module mkAxi4WrMasterP#(parameter UInt#(32) max_flight,
			parameter Bool big_endian,
			BusSender#(Axi4AddrCmd#(`TLM_PRM)) wr_addr_fifo,
			BusSender#(Axi4WrData#(`TLM_PRM))  wr_data_fifo) (Axi4WrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusReceiver#(Axi4WrResp#(`TLM_PRM))   wr_resp_fifo <- mkBypassBusReceiver;

   FIFO#(req_t)                         fifo_rx      <- mkLFIFO;
   FIFO#(resp_t)                        fifo_tx      <- mkLFIFO;

   Reg#(TLMBLength#(`TLM_PRM))          count        <- mkReg(0);
   Reg#(RequestDescriptor#(`TLM_PRM))   descriptor   <- mkReg(?);

   let _ifc <- mkAxi4WrMasterIFC(wr_addr_fifo.out, wr_data_fifo.out, wr_resp_fifo.in);

   let rx_first = toTLMRequest(fifo_rx.first);

   rule send_write (rx_first matches tagged Descriptor .dd
		    &&& dd.command == WRITE);
      let d = dd;
      if (d.byte_enable matches tagged Calculate)
	 d = addByteEnableU(big_endian, d);
      let addr_cmd = getAxi4AddrCmd(d);
      let wr_data  = getAxi4WrData(d);
      count <= d.b_length;
      wr_addr_fifo.in.enq(addr_cmd);
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
      descriptor <= addByteEnable(big_endian, incrTLMAddr(alignAddress(d)));
   endrule

   rule data_write (rx_first matches tagged Data .dd);
      let d = dd;
      d.transaction_id = descriptor.transaction_id;
      let desc = descriptor;
      if (dd.byte_enable matches tagged Specify .be)
	 desc.byte_enable = dd.byte_enable;
      let wr_data = getAxi4WrData(desc);
      wr_data.data = d.data;
      wr_data.user = d.user;
      wr_data.last = (count == 1);
      count <= count - 1;
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
      descriptor <= addByteEnable(big_endian, incrTLMAddr(descriptor));
   endrule

   rule grab_response;
      let axi_response = wr_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxi4WrResp(axi_response);
      fifo_tx.enq(fromTLMResponse(response));
      wr_resp_fifo.out.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Error cases:
   ////////////////////////////////////////////////////////////////////////////////
   (* preempts = "start_read, grab_response" *)
   rule start_read (rx_first matches tagged Descriptor .d &&&
		    d.command matches READ);
      $display("(%0d) ERROR: Axi4WrMaster cannot handle READ OPS!", $time);
      fifo_rx.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   interface TLMRecvIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface Axi4WrFabricMaster fabric;
      interface Axi4WrMaster bus = _ifc;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxi4RdWrMaster#(parameter UInt#(32) max_flight,
			 parameter Bool big_endian)
                        (Axi4RdWrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   Axi4RdMasterXActorIFC#(`TLM_XTR) master_rd_xactor <- mkAxi4RdMaster(max_flight);
   Axi4WrMasterXActorIFC#(`TLM_XTR) master_wr_xactor <- mkAxi4WrMaster(max_flight, big_endian);

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
