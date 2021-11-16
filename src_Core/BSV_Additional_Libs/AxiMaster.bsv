// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package AxiMaster;


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import AxiDefines::*;
import Bus::*;
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

module mkAxiRdMasterIFC#(BusSend#(AxiAddrCmd#(`TLM_PRM))  request_addr,
			 BusRecv#(AxiRdResp#(`TLM_PRM))   response) (AxiRdMaster#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM))   rID_wire   <- mkBypassWire;
   Wire#(AxiData#(`TLM_PRM)) rDATA_wire <- mkBypassWire;
   Wire#(AxiResp)              rRESP_wire <- mkBypassWire;
   Wire#(Bool)                 rLAST_wire <- mkBypassWire;

   rule every ;
      let value = AxiRdResp {id:   rID_wire,
			     data: rDATA_wire,
			     resp: rRESP_wire,
			     last: rLAST_wire};
      response.data(value);
   endrule

   // Address Outputs
   method arID    = request_addr.data.id;
   method arADDR  = request_addr.data.addr;
   method arLEN   = request_addr.data.len;
   method arSIZE  = request_addr.data.size;
   method arBURST = request_addr.data.burst;
   method arLOCK  = request_addr.data.lock;
   method arCACHE = request_addr.data.cache;
   method arPROT  = request_addr.data.prot;
   method arVALID = request_addr.valid;

   // Address Inputs
   method arREADY = request_addr.ready;

   // Response Outputs
   method rREADY  = response.ready;

   // Response Inputs
   method rID     = rID_wire._write;
   method rDATA   = rDATA_wire._write;
   method rRESP   = rRESP_wire._write;
   method rLAST   = rLAST_wire._write;
   method rVALID  = response.valid;

endmodule

module mkAxiWrMasterIFC#(BusSend#(AxiAddrCmd#(`TLM_PRM))  request_addr,
			 BusSend#(AxiWrData#(`TLM_PRM))   request_data,
			 BusRecv#(AxiWrResp#(`TLM_PRM))   response) (AxiWrMaster#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM)) bID_wire    <- mkBypassWire;
   Wire#(AxiResp)            bRESP_wire  <- mkBypassWire;

   rule every ;
      let value = AxiWrResp { id: bID_wire, resp: bRESP_wire};
      response.data(value);
   endrule

   // Address Outputs
   method awID    = request_addr.data.id;
   method awADDR  = request_addr.data.addr;
   method awLEN   = request_addr.data.len;
   method awSIZE  = request_addr.data.size;
   method awBURST = request_addr.data.burst;
   method awLOCK  = request_addr.data.lock;
   method awCACHE = request_addr.data.cache;
   method awPROT  = request_addr.data.prot;
   method awVALID = request_addr.valid;

   // Address Inputs
   method awREADY = request_addr.ready;

   // Data Outputs
   method wID     = request_data.data.id;
   method wDATA   = request_data.data.data;
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
   method bVALID  = response.valid;

endmodule

module mkAxiRdBusMasterIFC#(AxiRdMaster#(`TLM_PRM) ifc) (AxiRdBusMaster#(`TLM_PRM));

   interface BusSend addr;
      method AxiAddrCmd#(`TLM_PRM) data;
	 let addr =  AxiAddrCmd {id:    ifc.arID,
				 len:   ifc.arLEN,
				 size:  ifc.arSIZE,
				 burst: ifc.arBURST,
				 lock:  ifc.arLOCK,
				 cache: ifc.arCACHE,
				 prot:  ifc.arPROT,
				 addr:  ifc.arADDR};
	 return addr;
      endmethod
      method valid = ifc.arVALID;
      method ready = ifc.arREADY;
   endinterface
   interface BusRecv resp;
      method Action data(AxiRdResp#(`TLM_PRM) value);
	 ifc.rID(value.id);
	 ifc.rDATA(value.data);
	 ifc.rRESP(value.resp);
	 ifc.rLAST(value.last);
      endmethod
      method valid = ifc.rVALID;
      method ready = ifc.rREADY;
   endinterface

endmodule

module mkAxiWrBusMasterIFC#(AxiWrMaster#(`TLM_PRM) ifc) (AxiWrBusMaster#(`TLM_PRM));

   interface BusSend addr;
      method AxiAddrCmd#(`TLM_PRM) data;
	 let addr =  AxiAddrCmd {id:    ifc.awID,
				 len:   ifc.awLEN,
				 size:  ifc.awSIZE,
				 burst: ifc.awBURST,
				 lock:  ifc.awLOCK,
				 cache: ifc.awCACHE,
				 prot:  ifc.awPROT,
				 addr:  ifc.awADDR};
	 return addr;
      endmethod
      method valid = ifc.awVALID;
      method ready = ifc.awREADY;
   endinterface
   interface BusSend data;
      method AxiWrData#(`TLM_PRM) data;
	 let out = AxiWrData {id:   ifc.wID,
			      data: ifc.wDATA,
			      strb: ifc.wSTRB,
			      last: ifc.wLAST};
	 return out;
      endmethod
      method valid = ifc.wVALID;
      method ready = ifc.wREADY;
   endinterface
   interface BusRecv resp;
      method Action data(AxiWrResp#(`TLM_PRM) value);
	 ifc.bID(value.id);
	 ifc.bRESP(value.resp);
      endmethod
      method valid = ifc.bVALID;
      method ready = ifc.bREADY;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function Bool isLocked (TLMLock value);
   return (value == LOCKED);
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiRdMaster#(parameter UInt#(32) max_flight) (AxiRdMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusSender#(AxiAddrCmd#(`TLM_PRM))  rd_addr_fifo <- mkBypassBusSender(unpack(0));
   BusReceiver#(AxiRdResp#(`TLM_PRM)) rd_resp_fifo <- mkBypassBusReceiver;

   FIFO#(req_t)                       fifo_rx <- mkLFIFO;
   FIFO#(resp_t)                      fifo_tx <- mkLFIFO;
   Reg#(TLMBLength#(`TLM_PRM))        count   <- mkReg(0);

   let _ifc <- mkAxiRdMasterIFC(rd_addr_fifo.out, rd_resp_fifo.in);

   let rx_first = toTLMRequest(fifo_rx.first);

   FIFOF#(TLMId#(`TLM_PRM)) id_fifo <- mkSafeDepthParamFIFOF(max_flight);

   Reg#(Bool)             must_retire <- mkReg(True);
   Reg#(Bool)             lock_mode   <- mkReg(False);
   Reg#(TLMId#(`TLM_PRM)) locked_id   <- mkReg(?);

   PulseWire              out_incr <- mkPulseWire;
   PulseWire              out_decr <- mkPulseWire;

   Reg#(UInt#(8))         out_count <- mkReg(0);

   rule incr_out (out_incr && !out_decr);
      out_count <= out_count + 1;
   endrule

   rule decr_out (!out_incr && out_decr);
      out_count <= out_count - 1;
   endrule

   rule first_locked_read (rx_first matches tagged Descriptor .d
			   &&& d.command matches READ
			   &&& isLocked(d.lock)
			   &&& !lock_mode
			   &&& out_count == 0);
      let addr_cmd = getAxiAddrCmd(d);
      lock_mode <= True;
      locked_id <= addr_cmd.id;
      rd_addr_fifo.in.enq(addr_cmd);
      fifo_rx.deq;
      id_fifo.enq(d.transaction_id);
      out_incr.send;
   endrule

   rule continue_locked_read (rx_first matches tagged Descriptor .dd
			      &&& dd.command matches READ
			      &&& isLocked(dd.lock)
			      &&& lock_mode);
      id_fifo.enq(dd.transaction_id);
      let d = dd;
      d.transaction_id = locked_id;
      let addr_cmd = getAxiAddrCmd(d);
      rd_addr_fifo.in.enq(addr_cmd);
      fifo_rx.deq;
      out_incr.send;
   endrule

   rule last_locked_read (rx_first matches tagged Descriptor .dd
			  &&& dd.command matches READ
			  &&& !isLocked(dd.lock)
			  &&& lock_mode
			  &&& out_count == 0);
      id_fifo.enq(dd.transaction_id);
      let d = dd;
      d.transaction_id = locked_id;
      let addr_cmd = getAxiAddrCmd(d);
      lock_mode <= False;
      must_retire <= True;
      rd_addr_fifo.in.enq(addr_cmd);
      fifo_rx.deq;
      out_incr.send;
   endrule

   rule send_unlocked_read (rx_first matches tagged Descriptor .d
			    &&& d.command matches READ
			    &&& !isLocked(d.lock)
			    &&& !lock_mode
			    &&& (!must_retire || out_count == 0));
      let addr_cmd = getAxiAddrCmd(d);
      must_retire <= False;
      rd_addr_fifo.in.enq(addr_cmd);
      fifo_rx.deq;
      out_incr.send;
   endrule

   (* preempts = "grab_response_locked, grab_response_unlocked" *)
   rule grab_response_locked;
      let axi_response = rd_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxiRdResp(axi_response);
      rd_resp_fifo.out.deq;
      let id = id_fifo.first;
      response.transaction_id = id;
      if (response.is_last)
	 begin
	    id_fifo.deq;
	    out_decr.send;
	 end
      fifo_tx.enq(fromTLMResponse(response));
   endrule

   rule grab_response_unlocked;
      let axi_response = rd_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxiRdResp(axi_response);
      rd_resp_fifo.out.deq;
      if (response.is_last) out_decr.send;
      fifo_tx.enq(fromTLMResponse(response));
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Error cases:
   ////////////////////////////////////////////////////////////////////////////////
   (* preempts = "start_write, (grab_response_locked, grab_response_unlocked)" *)
   rule start_write (rx_first matches tagged Descriptor .d
		     &&& d.command matches WRITE);
      $display("(%0d) ERROR: AxiRdMaster cannot handle WRITE ops!", $time);
      fifo_rx.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   interface TLMRecvIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface AxiRdFabricMaster fabric;
      interface AxiRdMaster bus = _ifc;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiWrMaster#(parameter UInt#(32) max_flight,
		      parameter Bool big_endian)
                     (AxiWrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusSender#(AxiAddrCmd#(`TLM_PRM)) wr_addr_fifo <- mkBypassBusSender(unpack(0));
   BusSender#(AxiWrData#(`TLM_PRM))  wr_data_fifo <- mkBypassBusSender(unpack(0));

   let _ifc <- mkAxiWrMasterP(max_flight, big_endian, wr_addr_fifo, wr_data_fifo);
   return _ifc;

endmodule

module mkAxiWrMasterP#(parameter UInt#(32) max_flight,
		       parameter Bool big_endian,
		       BusSender#(AxiAddrCmd#(`TLM_PRM)) wr_addr_fifo,
		       BusSender#(AxiWrData#(`TLM_PRM))  wr_data_fifo) (AxiWrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   BusReceiver#(AxiWrResp#(`TLM_PRM))   wr_resp_fifo <- mkBypassBusReceiver;

   FIFO#(req_t)                         fifo_rx      <- mkLFIFO;
   FIFO#(resp_t)                        fifo_tx      <- mkLFIFO;

   Reg#(TLMBLength#(`TLM_PRM))          count        <- mkReg(0);
   Reg#(RequestDescriptor#(`TLM_PRM))   descriptor   <- mkReg(?);

   let _ifc <- mkAxiWrMasterIFC(wr_addr_fifo.out, wr_data_fifo.out, wr_resp_fifo.in);

   let rx_first = toTLMRequest(fifo_rx.first);

   FIFOF#(TLMId#(`TLM_PRM)) id_fifo <- mkSafeDepthParamFIFOF(max_flight);

   Reg#(Bool)             must_retire <- mkReg(True);
   Reg#(Bool)             lock_mode   <- mkReg(False);
   Reg#(TLMId#(`TLM_PRM)) locked_id   <- mkReg(?);

   PulseWire              out_incr <- mkPulseWire;
   PulseWire              out_decr <- mkPulseWire;

   Reg#(UInt#(8))         out_count <- mkReg(0);

   rule incr_out (out_incr && !out_decr);
      out_count <= out_count + 1;
   endrule

   rule decr_out (!out_incr && out_decr);
      out_count <= out_count - 1;
   endrule

   rule first_locked_write (rx_first matches tagged Descriptor .dd &&&
			    dd.command == WRITE
			    &&& isLocked(dd.lock)
			    &&& !lock_mode
			    &&& out_count == 0);

      id_fifo.enq(dd.transaction_id);
      let d = dd;
      if (d.byte_enable matches tagged Calculate)
	 d = addByteEnableU(big_endian, d);
      let addr_cmd = getAxiAddrCmd(d);
      let wr_data  = getAxiWrData(d);
      lock_mode <= True;
      locked_id <= addr_cmd.id;
      count <= d.b_length;
      wr_addr_fifo.in.enq(addr_cmd);
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
      descriptor <= addByteEnable(big_endian, incrTLMAddr(alignAddress(d)));
      out_incr.send;
   endrule

   rule continue_locked_write (rx_first matches tagged Descriptor .dd
			       &&& dd.command == WRITE
			       &&& isLocked(dd.lock)
			       &&& lock_mode);

      id_fifo.enq(dd.transaction_id);
      let d = dd;
      if (d.byte_enable matches tagged Calculate)
	 d = addByteEnableU(big_endian, d);
      d.transaction_id = locked_id;
      let addr_cmd = getAxiAddrCmd(d);
      let wr_data  = getAxiWrData(d);
      count <= d.b_length;
      wr_addr_fifo.in.enq(addr_cmd);
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
      descriptor <= addByteEnable(big_endian, incrTLMAddr(alignAddress(d)));
      out_incr.send;
   endrule


   rule last_locked_write (rx_first matches tagged Descriptor .dd
			   &&& dd.command == WRITE
			   &&& !isLocked(dd.lock)
			   &&& lock_mode
			   &&& out_count == 0);
      id_fifo.enq(dd.transaction_id);
      let d = dd;
      if (d.byte_enable matches tagged Calculate)
	 d = addByteEnableU(big_endian, d);
      d.transaction_id = locked_id;
      let addr_cmd = getAxiAddrCmd(d);
      lock_mode <= False;
      must_retire <= True;
      let wr_data  = getAxiWrData(d);
      count <= d.b_length;
      wr_addr_fifo.in.enq(addr_cmd);
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
      descriptor <= addByteEnable(big_endian, incrTLMAddr(alignAddress(d)));
      out_incr.send;
   endrule

   rule send_unlocked_write (rx_first matches tagged Descriptor .dd
			     &&& dd.command == WRITE
			     &&& !isLocked(dd.lock)
			     &&& !lock_mode
			     &&& (!must_retire || out_count == 0));
      let d = dd;
      if (d.byte_enable matches tagged Calculate)
	 d = addByteEnableU(big_endian, d);
      let addr_cmd = getAxiAddrCmd(d);
      must_retire <= False;
      let wr_data  = getAxiWrData(d);
      count <= d.b_length;
      wr_addr_fifo.in.enq(addr_cmd);
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
      descriptor <= addByteEnable(big_endian, incrTLMAddr(alignAddress(d)));
      out_incr.send;
   endrule

   rule data_write (rx_first matches tagged Data .dd);
      let d = dd;
      d.transaction_id = descriptor.transaction_id;
      let desc = descriptor;
      if (dd.byte_enable matches tagged Specify .be)
	 desc.byte_enable = dd.byte_enable;
      let wr_data = getAxiWrData(desc);
      wr_data.data = d.data;
      wr_data.last = (count == 1);
      count <= count - 1;
      wr_data_fifo.in.enq(wr_data);
      fifo_rx.deq;
      descriptor <= addByteEnable(big_endian, incrTLMAddr(descriptor));
   endrule

   (* preempts = "grab_response_locked, grab_response_unlocked" *)
   rule grab_response_locked;
      let axi_response = wr_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxiWrResp(axi_response);
      let id = id_fifo.first;
      response.transaction_id = id;
      fifo_tx.enq(fromTLMResponse(response));
      wr_resp_fifo.out.deq;
      id_fifo.deq;
      out_decr.send;
   endrule

   rule grab_response_unlocked;
      let axi_response = wr_resp_fifo.out.first;
      TLMResponse#(`TLM_PRM) response = fromAxiWrResp(axi_response);
      fifo_tx.enq(fromTLMResponse(response));
      wr_resp_fifo.out.deq;
      out_decr.send;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Error cases:
   ////////////////////////////////////////////////////////////////////////////////
   (* preempts = "start_read, (grab_response_locked, grab_response_unlocked)" *)
   rule start_read (rx_first matches tagged Descriptor .d &&&
		    d.command matches READ);
      $display("(%0d) ERROR: AxiWrMaster cannot handle READ OPS!", $time);
      fifo_rx.deq;
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   interface TLMRecvIFC tlm;
      interface Get tx = toGet(fifo_tx);
      interface Put rx = toPut(fifo_rx);
   endinterface

   interface AxiWrFabricMaster fabric;
      interface AxiWrMaster bus = _ifc;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiRdWrMaster#(parameter UInt#(32) max_flight,
			 parameter Bool big_endian)
			(AxiRdWrMasterXActorIFC#(`TLM_XTR))
   provisos(Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM));

   AxiRdMasterXActorIFC#(`TLM_XTR) master_rd_xactor <- mkAxiRdMaster(max_flight);
   AxiWrMasterXActorIFC#(`TLM_XTR) master_wr_xactor <- mkAxiWrMaster(max_flight, big_endian);

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
