// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi4LDefines;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Axi4Defines::*;
import AxiDefines::*;
import Bus::*;
import CBus::*;
import Connectable::*;
import DefaultValue::*;
import TLM3::*;

`include "TLM.defines"

typedef struct {
		AxiProt              prot;
                AxiAddr#(`TLM_PRM)   addr;
		} Axi4LAddrCmd#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(Axi4LAddrCmd#(`TLM_PRM));
   defaultValue = Axi4LAddrCmd {
      prot:   unpack(0),
      addr:   ?
      };
endinstance


typedef struct {
		AxiData#(`TLM_PRM)   data;
		AxiByteEn#(`TLM_PRM) strb;
		} Axi4LWrData#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(Axi4LWrData#(`TLM_PRM));
   defaultValue = Axi4LWrData {
      data:   ?,
      strb:   '1
      };
endinstance

typedef struct {AxiData#(`TLM_PRM)   data;
		AxiResp              resp;
		} Axi4LRdResp#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(Axi4LRdResp#(`TLM_PRM));
   defaultValue = Axi4LRdResp {
      data:   ?,
      resp:   OKAY
      };
endinstance

function Action nop(a ignore);
   return noAction;
endfunction

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface Axi4LWrMaster#(`TLM_PRM_DCL);

   // Address Outputs
   (* result = "AWADDR" *)
   method AxiAddr#(`TLM_PRM)   awADDR;
   (* result = "AWPROT" *)
   method AxiProt              awPROT;
   (* result = "AWVALID" *)
   method Bool                 awVALID;

   // Address Inputs
   (* prefix = "", result = "unusedwm0" *)
   method Action awREADY((* port = "AWREADY" *) Bool value);

   // Data Outputs
   (* result = "WDATA" *)
   method AxiData#(`TLM_PRM)     wDATA;
   (* result = "WSTRB" *)
   method AxiByteEn#(`TLM_PRM) wSTRB;
   (* result = "WVALID" *)
   method Bool                   wVALID;

   // Data Inputs
   (* prefix = "", result = "unusedwm1" *)
   method Action wREADY((* port = "WREADY" *) Bool value);

   // Response Outputs
   (* result = "BREADY" *)
   method Bool                   bREADY;

   // Response Inputs
   (* prefix = "", result = "unusedwm3" *)
   method Action bRESP((* port = "BRESP" *) AxiResp value);
   (* prefix = "", result = "unusedwm2" *)
   method Action bVALID((* port = "BVALID" *) Bool value);

endinterface

(* always_ready, always_enabled *)
interface Axi4LRdMaster#(`TLM_PRM_DCL);

   // Address Outputs
   (* result = "ARADDR" *)
   method AxiAddr#(`TLM_PRM)     arADDR;
   (* result = "ARPROT" *)
   method AxiProt              arPROT;
   (* result = "ARVALID" *)
   method Bool                 arVALID;

   // Address Inputs
   (* prefix = "", result = "unusedrm0" *)
   method Action arREADY((* port = "ARREADY" *) Bool value);

   // Response Outputs
   (* result = "RREADY" *)
   method Bool                   rREADY;

   // Response Inputs
   (* prefix = "", result = "unusedrm2" *)
   method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm3" *)
   method Action rRESP((* port = "RRESP" *) AxiResp value);
   (* prefix = "", result = "unusedrm6" *)
   method Action rVALID((* port = "RVALID" *) Bool value);

endinterface


(* always_ready, always_enabled *)
interface Axi4LWrSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedws1" *)
   method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws8" *)
   method Action awPROT((* port = "AWPROT" *) AxiProt value);
   (* prefix = "", result = "unusedws10" *)
   method Action awVALID((* port = "AWVALID" *) Bool value);

   // Address Outputs
   (* result = "AWREADY" *)
   method Bool                  awREADY;

   // Data Inputs
   (* prefix = "", result = "unusedws11" *)
   method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws13" *)
   method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws15" *)
   method Action wVALID((* port = "WVALID" *) Bool value);

   // Data Ouptuts
   (* result = "WREADY" *)
   method Bool                   wREADY;

   // Response Inputs
   (* prefix = "", result = "unusedws16" *)
   method Action bREADY((* port = "BREADY" *) Bool value);

   // Response Outputs
   (* result = "BRESP" *)
   method AxiResp                bRESP;
   (* result = "BVALID" *)
   method Bool                   bVALID;

endinterface

(* always_ready, always_enabled *)
interface Axi4LRdSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedrs1" *)
   method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs8" *)
   method Action arPROT((* port = "ARPROT" *) AxiProt value);
   (* prefix = "", result = "unusedrs10" *)
   method Action arVALID((* port = "ARVALID" *) Bool value);

   // Address Outputs
   (* result = "ARREADY" *)
   method Bool                  arREADY;

   // Response Inputs
   (* prefix = "", result = "unusedrs11" *)
   method Action rREADY((* port = "RREADY" *) Bool value);

   // Response Outputs
   (* result = "RDATA" *)
   method AxiData#(`TLM_PRM)     rDATA;
   (* result = "RRESP" *)
   method AxiResp                rRESP;
   (* result = "RVALID" *)
   method Bool                   rVALID;

endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Axi4LRdBusMaster#(`TLM_PRM_DCL);
   interface BusSend#(Axi4LAddrCmd#(`TLM_PRM)) addr;
   interface BusRecv#(Axi4LRdResp#(`TLM_PRM))  resp;
endinterface

interface Axi4LWrBusMaster#(`TLM_PRM_DCL);
   interface BusSend#(Axi4LAddrCmd#(`TLM_PRM)) addr;
   interface BusSend#(Axi4LWrData#(`TLM_PRM))  data;
   interface BusRecv#(AxiResp)                 resp;
endinterface

interface Axi4LRdBusSlave#(`TLM_PRM_DCL);
   interface BusRecv#(Axi4LAddrCmd#(`TLM_PRM)) addr;
   interface BusSend#(Axi4LRdResp#(`TLM_PRM))  resp;
endinterface

interface Axi4LWrBusSlave#(`TLM_PRM_DCL);
   interface BusRecv#(Axi4LAddrCmd#(`TLM_PRM)) addr;
   interface BusRecv#(Axi4LWrData#(`TLM_PRM))  data;
   interface BusSend#(AxiResp)                 resp;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Axi4LRdFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdMaster#(`TLM_PRM) bus;
endinterface

interface Axi4LRdFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

interface Axi4LWrFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LWrMaster#(`TLM_PRM) bus;
endinterface

interface Axi4LWrFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LWrSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

interface Axi4LRdMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface Axi4LRdFabricMaster#(`TLM_PRM) fabric;
endinterface

interface Axi4LWrMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface Axi4LWrFabricMaster#(`TLM_PRM) fabric;
endinterface

interface Axi4LRdSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)       tlm;
   (* prefix = "" *)
   interface Axi4LRdFabricSlave#(`TLM_PRM) fabric;
endinterface

interface Axi4LWrSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)       tlm;
   (* prefix = "" *)
   interface Axi4LWrFabricSlave#(`TLM_PRM) fabric;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Axi4LRdWrMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)         tlm;
   (* prefix = "" *)
   interface Axi4LRdFabricMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4LWrFabricMaster#(`TLM_PRM) write;
endinterface

interface Axi4LRdWrSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface Axi4LRdFabricSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4LWrFabricSlave#(`TLM_PRM) write;
endinterface

interface Axi4LRdWrMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4LWrMaster#(`TLM_PRM) write;
endinterface

interface Axi4LRdWrSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4LRdSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4LWrSlave#(`TLM_PRM) write;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// TLM conversion functions
/// TLM to AXI4L:
////////////////////////////////////////////////////////////////////////////////

function Axi4LAddrCmd#(`TLM_PRM) getAxi4LAddrCmd (RequestDescriptor#(`TLM_PRM) tlm_descriptor);

   AxiProt prot = ?;
   prot.access    = tlm_descriptor.access;
   prot.security  = tlm_descriptor.security;
   prot.privilege = tlm_descriptor.privilege;

   Axi4LAddrCmd#(`TLM_PRM) addr_cmd;
   addr_cmd.prot  = prot;
   addr_cmd.addr  = tlm_descriptor.addr;

   return addr_cmd;

endfunction

function Axi4LWrData#(`TLM_PRM) getAxi4LWrData (RequestDescriptor#(`TLM_PRM) tlm_descriptor);
   Axi4LWrData#(`TLM_PRM) wr_data = ?;
   wr_data.data = tlm_descriptor.data;
   wr_data.strb = '1;
   if (tlm_descriptor.byte_enable matches tagged Specify .be)
      wr_data.strb = be;
   return wr_data;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function RequestDescriptor#(`TLM_PRM) fromAxi4LAddrCmd (Axi4LAddrCmd#(`TLM_PRM) addr_cmd)
   provisos(Bits#(RequestDescriptor#(`TLM_PRM), s0));

   RequestDescriptor#(`TLM_PRM) desc = unpack(0);
   desc.mode            = REGULAR;
   desc.addr            = addr_cmd.addr;
   desc.region          = 0;
   desc.b_length        = 0;
   desc.burst_mode      = INCR;
   desc.b_size          = getMaxBSize(valueOf(data_size));
   desc.prty            = 0;
   desc.lock            = NORMAL;
   desc.transaction_id  = 0;

   desc.access          = addr_cmd.prot.access;
   desc.security        = addr_cmd.prot.security;
   desc.privilege       = addr_cmd.prot.privilege;

   desc.command         = READ; // added later
   desc.data            = 0;    // added later
   desc.byte_enable     = Calculate;

   return desc;
endfunction

function TLMResponse#(`TLM_PRM) fromAxi4LRdResp(Axi4LRdResp#(`TLM_PRM) value)
   provisos(DefaultValue#(TLMResponse#(`TLM_PRM)));

   TLMResponse#(`TLM_PRM) response = defaultValue;
   response.command        = READ;
   response.transaction_id = 0;
   response.data           = value.data;
   response.is_last        = True;
   case (value.resp)
      OKAY: begin
	       response.status = SUCCESS;
	    end
      EXOKAY: begin
		 response.status = EXOKAY;
	      end
      SLVERR: begin
      		 TLMErrorCode code = SLVERR;
		 response.status = ERROR;
		 response.data   = extendNP(pack(code));
	      end
      DECERR: begin
      		 TLMErrorCode code = DECERR;
		 response.status = ERROR;
		 response.data   = extendNP(pack(code));
	      end
   endcase
   return response;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass ToAxi4L#(type a, type b)
   dependencies (a determines b);
   module toAxi4L(a ifc, b ignore);
endtypeclass

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance BusPayload#(Axi4LAddrCmd#(`TLM_PRM), TLMId#(`TLM_PRM));
   function isLast (payload);
      return True;
   endfunction
   function getId(payload);
      return 0;
   endfunction
   function setId(payload, value);
      return payload;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(Axi4LWrMaster#(`TLM_PRM), Axi4LWrSlave#(`TLM_PRM));
   module mkConnection#(Axi4LWrMaster#(`TLM_PRM) m, Axi4LWrSlave#(`TLM_PRM) s )(Empty);

      rule master_to_slave_addr_data;
	 // Address Signals
	 s.awADDR(m.awADDR);
	 s.awPROT(m.awPROT);
	 s.awVALID(m.awVALID);
	 // Data Signals
	 s.wDATA(m.wDATA);
	 s.wSTRB(m.wSTRB);
	 s.wVALID(m.wVALID);
      endrule

      rule master_to_slave_response;
	 // Response Signals
	 s.bREADY(m.bREADY);
      endrule

      rule slave_to_master_addr_data;
	 // Address Signals
	 m.awREADY(s.awREADY);
	 // Data Signals
	 m.wREADY(s.wREADY);
      endrule

      rule slave_to_master_response;
	 // Response Signals
	 m.bRESP(s.bRESP);
	 m.bVALID(s.bVALID);
      endrule

   endmodule
endinstance
instance Connectable#( Axi4LWrSlave#(`TLM_PRM), Axi4LWrMaster#(`TLM_PRM));
   module mkConnection#( Axi4LWrSlave#(`TLM_PRM) s, Axi4LWrMaster#(`TLM_PRM) m)(Empty);
   (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance

instance Connectable#(Axi4LRdMaster#(`TLM_PRM), Axi4LRdSlave#(`TLM_PRM));
   module mkConnection#(Axi4LRdMaster#(`TLM_PRM) m, Axi4LRdSlave#(`TLM_PRM) s )(Empty);

      rule master_to_slave_addr;
	 // Address Signals
	 s.arADDR(m.arADDR);
	 s.arPROT(m.arPROT);
	 s.arVALID(m.arVALID);
      endrule

      rule master_to_slave_response;
	 // Response Signals
	 s.rREADY(m.rREADY);
      endrule

      rule slave_to_master_addr;
	 // Address Signals
	 m.arREADY(s.arREADY);
      endrule

      rule slave_to_master_response;
	 // Response Signals
	 m.rDATA(s.rDATA);
	 m.rRESP(s.rRESP);
	 m.rVALID(s.rVALID);
      endrule

   endmodule
endinstance
instance Connectable#( Axi4LRdSlave#(`TLM_PRM), Axi4LRdMaster#(`TLM_PRM));
   module mkConnection#( Axi4LRdSlave#(`TLM_PRM) s, Axi4LRdMaster#(`TLM_PRM) m)(Empty);
   (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance

instance Connectable#(Axi4LRdFabricMaster#(`TLM_PRM), Axi4LRdFabricSlave#(`TLM_PRM));
   module mkConnection#(Axi4LRdFabricMaster#(`TLM_PRM) m, Axi4LRdFabricSlave#(`TLM_PRM) s )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(Axi4LRdFabricSlave#(`TLM_PRM), Axi4LRdFabricMaster#(`TLM_PRM));
   module mkConnection#(Axi4LRdFabricSlave#(`TLM_PRM) s, Axi4LRdFabricMaster#(`TLM_PRM) m )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(Axi4LWrFabricMaster#(`TLM_PRM), Axi4LWrFabricSlave#(`TLM_PRM));
   module mkConnection#(Axi4LWrFabricMaster#(`TLM_PRM) m, Axi4LWrFabricSlave#(`TLM_PRM) s )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(Axi4LWrFabricSlave#(`TLM_PRM), Axi4LWrFabricMaster#(`TLM_PRM));
   module mkConnection#(Axi4LWrFabricSlave#(`TLM_PRM) s, Axi4LWrFabricMaster#(`TLM_PRM) m )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

endpackage
