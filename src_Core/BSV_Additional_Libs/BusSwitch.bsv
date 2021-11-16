// Copyright (c) 2007--2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$
package BusSwitch;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import BusDefines::*;
import DReg::*;
import TLM3Defines::*;
import BUtils::*;
import Vector::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef struct {
		TLMId#(`TLM_PRM) send_port;
		TLMId#(`TLM_PRM) recv_port;
		TLMId#(`TLM_PRM) send_id;
		TLMId#(`TLM_PRM) recv_id;
		} BusSwitchPath#(`TLM_PRM_DCL) `dv;

function BusSwitchPath#(`TLM_PRM) reverseBusSwitchPath(BusSwitchPath#(`TLM_PRM) path);
   let out = BusSwitchPath {send_port: path.recv_port,
			    recv_port: path.send_port,
			    send_id:   path.recv_id,
			    recv_id:   path.send_id};
   return out;
endfunction

interface BusSwitch#(`TLM_PRM_DCL);
   method Action set_path (BusSwitchPath#(`TLM_PRM) path);
   method Bool   done ();
   method Action ack();
endinterface

module mkBusSwitch#(Vector#(send_count, BusSend#(a)) sends,
		    Vector#(recv_count, BusRecv#(a)) recvs,
		    Bool setLast) (BusSwitch#(`TLM_PRM))
   provisos(Bits#(a, sa), BusPayload#(a, TLMId#(`TLM_PRM)));
   
   let isend_count = valueOf(send_count);
   let irecv_count = valueOf(recv_count);
   
   Wire#(Bool)                           handoff_wire <- mkDWire(False);
   Wire#(Bool)                           last_wire    <- mkBypassWire;
   Wire#(BusSwitchPath#(`TLM_PRM))       path_wire    <- mkWire;
   Wire#(Bool)                           ack_wire     <- mkDWire(False);
   Reg#(Bool)                            done_reg     <- mkDReg(False);
   
   Wire#(TLMId#(`TLM_PRM))             send_port    <- mkDWire(fromInteger(isend_count));
   Wire#(TLMId#(`TLM_PRM))             recv_port    <- mkDWire(fromInteger(irecv_count));
   
   Wire#(a)                              data_wire    <-  mkDWire(unpack(0));
   
   let send_connected = sends[path_wire.send_port];
   let recv_connected = recvs[path_wire.recv_port];
   
   rule update_data;
//      data_wire <= setId(send_connected.data, path_wire.send_id);
      data_wire <= send_connected.data;
   endrule
   
   rule update_handoff;
      handoff_wire <= recv_connected.ready && send_connected.valid; 
   endrule
   
   
   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_confirm;
      
      for (Integer x = 0; x < isend_count; x = x + 1)
	 begin
	    let send = sends[x];
	    let ready = ((fromInteger(x) == send_port) && (recv_port < fromInteger(irecv_count)))
	                ? (recvs[recv_port].ready && !done_reg) : False;
	    send.ready(ready);
	 end
      
   endrule
   
   (* fire_when_enabled, no_implicit_conditions *)
   rule connect_data;
      
      for (Integer y = 0; y < irecv_count; y = y + 1)
	 begin
	    let recv  = recvs[y];
	    let data  = (fromInteger(y) == recv_port) ? data_wire : ?;
	    let valid = ((fromInteger(y) == recv_port) && (send_port < fromInteger(isend_count)))
	                ? (sends[send_port].valid && !done_reg) : False;
	    recv.data(data);
	    recv.valid(valid);
	 end
      
      last_wire    <= isLast(data_wire) || setLast;
      
   endrule
   
   let done_value = (handoff_wire && last_wire) || done_reg;
   
   rule keep_done (done_value && !ack_wire);
      done_reg <= True;
   endrule
   
   method Action set_path (path);
      path_wire <= path;
      send_port <= path.send_port;
      recv_port <= path.recv_port;
   endmethod
   
   method Bool done ();
      return done_value;
   endmethod
   
   method Action ack ();
      ack_wire <= True;
   endmethod

endmodule


endpackage
