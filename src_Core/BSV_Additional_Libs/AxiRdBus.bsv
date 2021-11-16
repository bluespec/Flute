// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package AxiRdBus;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Arbiter::*;
import AxiDefines::*;
import AxiExtend::*;
import AxiMaster::*;
import AxiSlave::*;
import BUtils::*;
import Bus::*;
import CBus::*;
import Connectable::*;
import FIFO::*;
import FIFOF::*;
import List::*;
import SpecialFIFOs::*;
import TLM3::*;
import Vector::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkAxiRdBus#(Vector#(master_count, AxiRdFabricMaster#(`TLM_PRM)) masters,
		    Vector#(slv_count,  AxiRdFabricSlave#(`TLM_PRM))  slvs) (Empty)
   provisos(Add#(slv_count, 1, slave_count));

   function Bit#(slave_count) select_slave (TLMAddr#(`TLM_PRM) addr);
      Bit#(slv_count) value = pack(map(addrMatch(addr), slvs));
      if (value == 0)
	 return {value, 1'b1};
      else
	 return {value, 1'b0};
   endfunction

   Vector#(slv_count,
	   AxiRdFabricSlave#(TAdd#(TLog#(master_count), id_size), `TLM_PRM_REST))
   slvs_ext <- mapM(extendIds(10), slvs);

   Vector#(master_count,
	   AxiRdFabricMaster#(TAdd#(TLog#(master_count), id_size), `TLM_PRM_REST))
   masters_ext <- zipWithM(extendIds, map(fromInteger, genList), masters);

   let _ifc <- mkAxiRdBusS(select_slave, masters_ext, slvs_ext);

   return _ifc;

endmodule

module mkAxiRdBusS#(function Bit#(slave_count) select_slave(Bit#(addr_size) addr),
                   Vector#(master_count, AxiRdFabricMaster#(`TLM_PRM)) masters,
		   Vector#(slv_count,  AxiRdFabricSlave#(`TLM_PRM))  slvs) (Empty)
   provisos(Add#(slv_count, 1, slave_count));

   Wire#(Bool)                             fixed    <- mkDWire(True);
   Reg#(Maybe#(Bit#(TLog#(master_count)))) lock_num <- mkReg(tagged Invalid);

   function AxiRdMaster#(`TLM_PRM) master_bus_ifc(AxiRdFabricMaster#(`TLM_PRM) ifc) = ifc.bus;
   function AxiRdSlave#(`TLM_PRM)  slave_bus_ifc(AxiRdFabricSlave#(`TLM_PRM) ifc)   = ifc.bus;

   AxiRdFabricSlave#(`TLM_PRM) dummy <- mkAxiRdSlaveDummy;
   let slaves = cons(dummy, slvs);

   let master_vector <- mapM(mkAxiRdBusMasterIFC, map(master_bus_ifc, masters));
   let slave_vector  <- mapM(mkAxiRdBusSlaveIFC,  map(slave_bus_ifc, slaves));

   Arbiter_IFC#(master_count) arbiter <- mkArbiter(fixed);
   Vector#(master_count, ArbiterRequest_IFC) requests <- mapM(mkArbiterRequest, master_vector);

   zipWithM(mkConnection, arbiter.clients, requests);

   FIFOF#(BusSwitchPath#(`TLM_PRM)) addr_path_fifo <- mkBypassFIFOF;
   FIFO#(BusSwitchPath#(`TLM_PRM))  resp_path_fifo <- mkBypassFIFO;

   ////////////////////////////////////////////////////////////////////////////////
   /// A switch for the address phase.
   ////////////////////////////////////////////////////////////////////////////////

   let addr_sends = map(getAxiRdMasterAddr, master_vector);
   let addr_recvs = map(getAxiRdSlaveAddr,  slave_vector);

   BusSwitch#(`TLM_PRM) addr_switch <- mkBusSwitch(addr_sends, addr_recvs, False);

   ////////////////////////////////////////////////////////////////////////////////
   /// A switch for the response phase.
   ////////////////////////////////////////////////////////////////////////////////

   let resp_recvs = map(getAxiRdMasterResp, master_vector);
   let resp_sends = map(getAxiRdSlaveResp,  slave_vector);

   BusSwitch#(`TLM_PRM)  resp_switch <- mkBusSwitch(resp_sends, resp_recvs, False);

   let requests_pending = (pack(map(getRequest, requests)) != 0);

   rule pre_select_path (lock_num matches tagged Invalid
			 &&& requests_pending
			 &&& addr_path_fifo.notFull);
      fixed <= False;
   endrule

   rule select_path (requests_pending);
      let master_port = ?;
      if (lock_num matches tagged Valid .num)
	 master_port = num;
      else
	 master_port  = arbiter.grant_id;
      let master      = master_vector[master_port];
      let addr        = master.addr.data.addr;
      let lock        = master.addr.data.lock;
      lock_num <= (lock == LOCKED) ? tagged Valid master_port : tagged Invalid;
      let zow         = map(addrMatch(addr), slaves);
//      let slave_port  = getIndex(map(addrMatch(addr), slaves));
      Bit#(slave_count) one_hot = select_slave(addr);
      let slave_port  = getIndex(unpack(one_hot));
      BusSwitchPath#(`TLM_PRM) path = BusSwitchPath {send_port: extendNP(master_port),
						     recv_port: extendNP(slave_port),
						     send_id:   getId(master.addr.data),
						     recv_id:   getId(master.addr.data)};


      addr_path_fifo.enq(path);
   endrule

   rule set_addr_path;
      addr_switch.set_path(addr_path_fifo.first);
   endrule

   rule set_resp_path;
      resp_switch.set_path(resp_path_fifo.first);
   endrule

   rule finish_addr (addr_switch.done);
      addr_switch.ack;
      let resp_path = reverseBusSwitchPath(addr_path_fifo.first);
      resp_path_fifo.enq(resp_path);
      addr_path_fifo.deq;
   endrule

   rule finish_resp (resp_switch.done);
      resp_switch.ack;
      resp_path_fifo.deq;
   endrule

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function BusSend#(AxiAddrCmd#(`TLM_PRM)) getAxiRdMasterAddr (AxiRdBusMaster#(`TLM_PRM) master);
   return master.addr;
endfunction

function BusRecv#(AxiRdResp#(`TLM_PRM)) getAxiRdMasterResp (AxiRdBusMaster#(`TLM_PRM) master);
   return master.resp;
endfunction

function BusRecv#(AxiAddrCmd#(`TLM_PRM)) getAxiRdSlaveAddr (AxiRdBusSlave#(`TLM_PRM) slave);
   return slave.addr;
endfunction

function BusSend#(AxiRdResp#(`TLM_PRM)) getAxiRdSlaveResp (AxiRdBusSlave#(`TLM_PRM) slave);
   return slave.resp;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function Bool getRequest(ArbiterRequest_IFC ifc);
   return ifc.request;
endfunction

function Bool getGrant(ArbiterClient_IFC ifc);
   return ifc.grant;
endfunction

function Bool addrMatch(AxiAddr#(`TLM_PRM) addr, AxiRdFabricSlave#(`TLM_PRM) ifc);
   return ifc.addrMatch(addr);
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage
