// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package AxiDefines;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Arbiter::*;
import Bus::*;
import CBus::*;
import Connectable::*;
import DefaultValue::*;
import FShow::*;
import TLM3::*;
import BUtils::*;
import Vector::*;
import TieOff::*;

`include "TLM.defines"

 ////////////////////////////////////////////////////////////////////////////////
/// Data Structures
////////////////////////////////////////////////////////////////////////////////

typedef Bit#(addr_size)                  AxiAddr#(`TLM_PRM_DCL);
typedef Bit#(data_size)                  AxiData#(`TLM_PRM_DCL);
typedef Bit#(TDiv#(data_size, 8))        AxiByteEn#(`TLM_PRM_DCL);

typedef Bit#(id_size)                        AxiId#(`TLM_PRM_DCL); // Unique id
typedef UInt#(4)                             AxiLen;  // 1 - 16
typedef TLMBSize AxiSize;

typedef struct {
		TLMAccess    access;
		TLMSecurity  security;
		TLMPrivilege privilege;
		} AxiProt deriving (Eq, Bits, Bounded);

typedef struct {
		TLMAllocate  write_allocate;
		TLMAllocate  read_allocate;
		TLMCache     cache;
		TLMBuffer    buffer;
		} AxiCache deriving (Eq, Bits, Bounded);

typedef enum {FIXED, INCR, WRAP}             AxiBurst deriving (Bits, Eq, Bounded);
typedef TLMLock  AxiLock;
typedef enum {OKAY, EXOKAY, SLVERR, DECERR } AxiResp deriving (Bits, Eq, Bounded);


typedef struct {
                AxiId#(`TLM_PRM)     id;
                AxiLen               len;
                AxiSize              size;
                AxiBurst             burst;
                AxiLock              lock;
                AxiCache             cache;
                AxiProt              prot;
                AxiAddr#(`TLM_PRM)   addr;
		} AxiAddrCmd#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(AxiAddrCmd#(`TLM_PRM));
   defaultValue = AxiAddrCmd {
      id:     0,
      len:    0,
      size:   BITS8,
      burst:  INCR,
      lock:   NORMAL,
      cache:  unpack(0),
      prot:   unpack(0),
      addr:   ?
      };
endinstance

typedef struct {
		AxiId#(`TLM_PRM)     id;
		AxiData#(`TLM_PRM)   data;
		AxiByteEn#(`TLM_PRM) strb;
		Bool                 last;
		} AxiWrData#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(AxiWrData#(`TLM_PRM));
   defaultValue = AxiWrData {
      id:     0,
      data:   ?,
      strb:   maxBound,
      last:   True
      };
endinstance

typedef struct {
		AxiId#(`TLM_PRM)     id;
		AxiData#(`TLM_PRM)   data;
		AxiResp              resp;
		Bool                 last;
		} AxiRdResp#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(AxiRdResp#(`TLM_PRM));
   defaultValue = AxiRdResp {
      id:     0,
      data:   ?,
      resp:   OKAY,
      last:   True
      };
endinstance

typedef struct {
		AxiId#(`TLM_PRM)     id;
		AxiResp              resp;
		} AxiWrResp#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(AxiWrResp#(`TLM_PRM));
   defaultValue = AxiWrResp {
      id:     0,
      resp:   OKAY
      };
endinstance


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance BusPayload#(AxiAddrCmd#(`TLM_PRM), TLMId#(`TLM_PRM));
   function isLast (payload);
      return True;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

instance BusPayload#(AxiWrData#(`TLM_PRM), TLMId#(`TLM_PRM));
   function isLast (payload);
      return payload.last;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

instance BusPayload#(AxiWrResp#(`TLM_PRM), TLMId#(`TLM_PRM));
   function isLast (payload);
      return True;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

instance BusPayload#(AxiRdResp#(`TLM_PRM), TLMId#(`TLM_PRM));
   function isLast (payload);
      return payload.last;
   endfunction
   function getId(payload);
      return fromAxiId(payload.id);
   endfunction
   function setId(payload, value);
      payload.id = getAxiId(value);
      return payload;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface AxiWrMaster#(`TLM_PRM_DCL);

   // Address Outputs
   (* result = "AWID" *)
   method AxiId#(`TLM_PRM)   awID;
   (* result = "AWADDR" *)
   method AxiAddr#(`TLM_PRM) awADDR;
   (* result = "AWLEN" *)
   method AxiLen               awLEN;
   (* result = "AWSIZE" *)
   method AxiSize              awSIZE;
   (* result = "AWBURST" *)
   method AxiBurst             awBURST;
   (* result = "AWLOCK" *)
   method AxiLock              awLOCK;
   (* result = "AWCACHE" *)
   method AxiCache             awCACHE;
   (* result = "AWPROT" *)
   method AxiProt              awPROT;
   (* result = "AWVALID" *)
   method Bool                 awVALID;

   // Address Inputs
   (* prefix = "", result = "unusedwm0" *)
   method Action awREADY((* port = "AWREADY" *) Bool value);

   // Data Outputs
   (* result = "WID" *)
   method AxiId#(`TLM_PRM)     wID;
   (* result = "WDATA" *)
   method AxiData#(`TLM_PRM)   wDATA;
   (* result = "WSTRB" *)
   method AxiByteEn#(`TLM_PRM) wSTRB;
   (* result = "WLAST" *)
   method Bool                   wLAST;
   (* result = "WVALID" *)
   method Bool                   wVALID;

   // Data Inputs
   (* prefix = "", result = "unusedwm1" *)
   method Action wREADY((* port = "WREADY" *) Bool value);

   // Response Outputs
   (* result = "BREADY" *)
   method Bool                   bREADY;

   // Response Inputs
   (* prefix = "", result = "unusedwm2" *)
   method Action bID((* port = "BID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedwm3" *)
   method Action bRESP((* port = "BRESP" *) AxiResp value);
   (* prefix = "", result = "unusedwm4" *)
   method Action bVALID((* port = "BVALID" *) Bool value);

endinterface

(* always_ready, always_enabled *)
interface AxiRdMaster#(`TLM_PRM_DCL);

   // Address Outputs
   (* result = "ARID" *)
   method AxiId#(`TLM_PRM)   arID;
   (* result = "ARADDR" *)
   method AxiAddr#(`TLM_PRM) arADDR;
   (* result = "ARLEN" *)
   method AxiLen               arLEN;
   (* result = "ARSIZE" *)
   method AxiSize              arSIZE;
   (* result = "ARBURST" *)
   method AxiBurst             arBURST;
   (* result = "ARLOCK" *)
   method AxiLock              arLOCK;
   (* result = "ARCACHE" *)
   method AxiCache             arCACHE;
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
   (* prefix = "", result = "unusedrm1" *)
   method Action rID((* port = "RID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm2" *)
   method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm3" *)
   method Action rRESP((* port = "RRESP" *) AxiResp value);
   (* prefix = "", result = "unusedrm4" *)
   method Action rLAST((* port = "RLAST" *) Bool value);
   (* prefix = "", result = "unusedrm5" *)
   method Action rVALID((* port = "RVALID" *) Bool value);

endinterface


(* always_ready, always_enabled *)
interface AxiWrSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedws0" *)
   method Action awID((* port = "AWID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1" *)
   method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws2" *)
   method Action awLEN((* port = "AWLEN" *) AxiLen value);
   (* prefix = "", result = "unusedws3" *)
   method Action awSIZE((* port = "AWSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedws4" *)
   method Action awBURST((* port = "AWBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedws5" *)
   method Action awLOCK((* port = "AWLOCK" *) AxiLock value);
   (* prefix = "", result = "unusedws6" *)
   method Action awCACHE((* port = "AWCACHE" *) AxiCache value);
   (* prefix = "", result = "unusedws7" *)
   method Action awPROT((* port = "AWPROT" *) AxiProt value);
   (* prefix = "", result = "unusedws8" *)
   method Action awVALID((* port = "AWVALID" *) Bool value);

   // Address Outputs
   (* result = "AWREADY" *)
   method Bool                  awREADY;

   // Data Inputs
   (* prefix = "", result = "unusedws9" *)
   method Action wID((* port = "WID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws10" *)
   method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws11" *)
   method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws12" *)
   method Action wLAST((* port = "WLAST" *) Bool value);
   (* prefix = "", result = "unusedws13" *)
   method Action wVALID((* port = "WVALID" *) Bool value);

   // Data Ouptuts
   (* result = "WREADY" *)
   method Bool                   wREADY;

   // Response Inputs
   (* prefix = "", result = "unusedws14" *)
   method Action bREADY((* port = "BREADY" *) Bool value);

   // Response Outputs
   (* result = "BID" *)
   method AxiId#(`TLM_PRM)     bID;
   (* result = "BRESP" *)
   method AxiResp                bRESP;
   (* result = "BVALID" *)
   method Bool                   bVALID;

endinterface

(* always_ready, always_enabled *)
interface AxiRdSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedrs0" *)
   method Action arID((* port = "ARID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs1" *)
   method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs2" *)
   method Action arLEN((* port = "ARLEN" *) AxiLen value);
   (* prefix = "", result = "unusedrs3" *)
   method Action arSIZE((* port = "ARSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedrs4" *)
   method Action arBURST((* port = "ARBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedrs5" *)
   method Action arLOCK((* port = "ARLOCK" *) AxiLock value);
   (* prefix = "", result = "unusedrs6" *)
   method Action arCACHE((* port = "ARCACHE" *) AxiCache value);
   (* prefix = "", result = "unusedrs7" *)
   method Action arPROT((* port = "ARPROT" *) AxiProt value);
   (* prefix = "", result = "unusedrs8" *)
   method Action arVALID((* port = "ARVALID" *) Bool value);

   // Address Outputs
   (* result = "ARREADY" *)
   method Bool                  arREADY;

   // Response Inputs
   (* prefix = "", result = "unusedrs9" *)
   method Action rREADY((* port = "RREADY" *) Bool value);

   // Response Outputs
   (* result = "RID" *)
   method AxiId#(`TLM_PRM)     rID;
   (* result = "RDATA" *)
   method AxiData#(`TLM_PRM)   rDATA;
   (* result = "RRESP" *)
   method AxiResp                rRESP;
   (* result = "RLAST" *)
   method Bool                   rLAST;
   (* result = "RVALID" *)
   method Bool                   rVALID;

endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface AxiRdBusMaster#(`TLM_PRM_DCL);
   interface BusSend#(AxiAddrCmd#(`TLM_PRM)) addr;
   interface BusRecv#(AxiRdResp#(`TLM_PRM))  resp;
endinterface

interface AxiWrBusMaster#(`TLM_PRM_DCL);
   interface BusSend#(AxiAddrCmd#(`TLM_PRM)) addr;
   interface BusSend#(AxiWrData#(`TLM_PRM))  data;
   interface BusRecv#(AxiWrResp#(`TLM_PRM))  resp;
endinterface

interface AxiRdBusSlave#(`TLM_PRM_DCL);
   interface BusRecv#(AxiAddrCmd#(`TLM_PRM)) addr;
   interface BusSend#(AxiRdResp#(`TLM_PRM))  resp;
endinterface

interface AxiWrBusSlave#(`TLM_PRM_DCL);
   interface BusRecv#(AxiAddrCmd#(`TLM_PRM)) addr;
   interface BusRecv#(AxiWrData#(`TLM_PRM))  data;
   interface BusSend#(AxiWrResp#(`TLM_PRM))  resp;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface AxiRdFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdMaster#(`TLM_PRM) bus;
endinterface

interface AxiRdFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

interface AxiWrFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiWrMaster#(`TLM_PRM) bus;
endinterface

interface AxiWrFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiWrSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

instance TieOff#(AxiRdFabricSlave#(`TLM_PRM));
   module mkTieOff#(AxiRdFabricSlave#(`TLM_PRM) ifc)(Empty);
      rule tie_off;
         ifc.bus.rREADY(False);
//         ifc.bus.arID(?);
         ifc.bus.arADDR(0);
         ifc.bus.arLEN(?);
         ifc.bus.arSIZE(BITS8);
         ifc.bus.arBURST(FIXED);
         ifc.bus.arLOCK(unpack(0));
         ifc.bus.arCACHE(unpack(0));
         ifc.bus.arPROT(unpack(0));
         ifc.bus.arVALID(False);
      endrule
   endmodule
endinstance

instance TieOff#(AxiWrFabricSlave#(`TLM_PRM));
   module mkTieOff#(AxiWrFabricSlave#(`TLM_PRM) ifc) (Empty);
      rule tie_off;
//         ifc.bus.awID(0);
         ifc.bus.awADDR(0);
         ifc.bus.awLEN(0);
         ifc.bus.awSIZE(BITS8);
         ifc.bus.awBURST(FIXED);
         ifc.bus.awLOCK(unpack(0));
         ifc.bus.awCACHE(unpack(0));
         ifc.bus.awPROT(unpack(0));
         ifc.bus.awVALID(False);
         ifc.bus.wID(0);
         ifc.bus.wDATA(0);
         ifc.bus.wSTRB(0);
         ifc.bus.wLAST(False);
         ifc.bus.wVALID(False);
      endrule
   endmodule
endinstance

interface AxiRdMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface AxiRdFabricMaster#(`TLM_PRM) fabric;
endinterface

interface AxiWrMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface AxiWrFabricMaster#(`TLM_PRM) fabric;
endinterface

interface AxiRdSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)       tlm;
   (* prefix = "" *)
   interface AxiRdFabricSlave#(`TLM_PRM) fabric;
endinterface

interface AxiWrSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)       tlm;
   (* prefix = "" *)
   interface AxiWrFabricSlave#(`TLM_PRM) fabric;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface AxiRdWrMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)         tlm;
   (* prefix = "" *)
   interface AxiRdFabricMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrFabricMaster#(`TLM_PRM) write;
endinterface

interface AxiRdWrSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface AxiRdFabricSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrFabricSlave#(`TLM_PRM) write;
endinterface

interface AxiRdWrMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrMaster#(`TLM_PRM) write;
endinterface

interface AxiRdWrSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface AxiRdSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface AxiWrSlave#(`TLM_PRM) write;
endinterface

instance Connectable#(AxiRdWrMaster#(`TLM_PRM), AxiRdWrSlave#(`TLM_PRM));
   module mkConnection#(AxiRdWrMaster#(`TLM_PRM) s, AxiRdWrSlave#(`TLM_PRM) m) (Empty);
      let rdConn <- mkConnection(m.read,s.read);
      let wrConn <- mkConnection(m.write,s.write);
   endmodule
endinstance
instance Connectable#(AxiRdWrSlave#(`TLM_PRM), AxiRdWrMaster#(`TLM_PRM));
   module mkConnection#(AxiRdWrSlave#(`TLM_PRM) s, AxiRdWrMaster#(`TLM_PRM) m) (Empty);
      (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface AxiLowPower;
   (* prefix = "", result = "unusedwm0" *)
   method Action cSYSREQ((* port = "CSYSREQ" *) Bool value);
   (* result = "CSYSACK" *)
   method Bool                 cSYSACK;
   (* result = "CACTIVE" *)
   method Bool                 cACTIVE;
endinterface

(* synthesize *)
module mkAxiLowPower (AxiLowPower);

   Reg#(Bool) req_reg <- mkReg(True);

   method Action cSYSREQ(Bool value);
      req_reg <= value;
   endmethod

   method Bool cSYSACK;
      return req_reg;
   endmethod

   method cACTIVE = True;

endmodule

instance TieOff#(AxiLowPower);
   module mkTieOff#(AxiLowPower ifc)(Empty);
      rule tie_off;
         ifc.cSYSREQ(False);
      endrule
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(AxiWrMaster#(`TLM_PRM), AxiWrSlave#(`TLM_PRM));
   module mkConnection#(AxiWrMaster#(`TLM_PRM) m, AxiWrSlave#(`TLM_PRM) s )(Empty);

      rule master_to_slave_addr_data;
	 // Address Signals
	 s.awID(m.awID);
	 s.awADDR(m.awADDR);
	 s.awLEN(m.awLEN);
	 s.awSIZE(m.awSIZE);
	 s.awBURST(m.awBURST);
	 s.awLOCK(m.awLOCK);
	 s.awCACHE(m.awCACHE);
	 s.awPROT(m.awPROT);
	 s.awVALID(m.awVALID);
	 // Data Signals
	 s.wID(m.wID);
	 s.wDATA(m.wDATA);
	 s.wSTRB(m.wSTRB);
	 s.wLAST(m.wLAST);
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
	 m.bID(s.bID);
	 m.bRESP(s.bRESP);
	 m.bVALID(s.bVALID);
      endrule

   endmodule
endinstance
instance Connectable#( AxiWrSlave#(`TLM_PRM), AxiWrMaster#(`TLM_PRM));
   module mkConnection#( AxiWrSlave#(`TLM_PRM) s, AxiWrMaster#(`TLM_PRM) m)(Empty);
   (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance

instance Connectable#(AxiRdMaster#(`TLM_PRM), AxiRdSlave#(`TLM_PRM));
   module mkConnection#(AxiRdMaster#(`TLM_PRM) m, AxiRdSlave#(`TLM_PRM) s )(Empty);

      rule master_to_slave_addr;
	 // Address Signals
	 s.arID(m.arID);
	 s.arADDR(m.arADDR);
	 s.arLEN(m.arLEN);
	 s.arSIZE(m.arSIZE);
	 s.arBURST(m.arBURST);
	 s.arLOCK(m.arLOCK);
	 s.arCACHE(m.arCACHE);
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
	 m.rID(s.rID);
	 m.rDATA(s.rDATA);
	 m.rRESP(s.rRESP);
	 m.rLAST(s.rLAST);
	 m.rVALID(s.rVALID);
      endrule

   endmodule
endinstance
instance Connectable#( AxiRdSlave#(`TLM_PRM), AxiRdMaster#(`TLM_PRM));
   module mkConnection#( AxiRdSlave#(`TLM_PRM) s, AxiRdMaster#(`TLM_PRM) m)(Empty);
   (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance

instance Connectable#(AxiRdFabricMaster#(`TLM_PRM), AxiRdFabricSlave#(`TLM_PRM));
   module mkConnection#(AxiRdFabricMaster#(`TLM_PRM) m, AxiRdFabricSlave#(`TLM_PRM) s )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(AxiRdFabricSlave#(`TLM_PRM), AxiRdFabricMaster#(`TLM_PRM));
   module mkConnection#(AxiRdFabricSlave#(`TLM_PRM) s, AxiRdFabricMaster#(`TLM_PRM) m )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(AxiWrFabricMaster#(`TLM_PRM), AxiWrFabricSlave#(`TLM_PRM));
   module mkConnection#(AxiWrFabricMaster#(`TLM_PRM) m, AxiWrFabricSlave#(`TLM_PRM) s )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(AxiWrFabricSlave#(`TLM_PRM), AxiWrFabricMaster#(`TLM_PRM));
   module mkConnection#(AxiWrFabricSlave#(`TLM_PRM) s, AxiWrFabricMaster#(`TLM_PRM) m )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
/// TLM conversion functions
/// TLM to AXI:
////////////////////////////////////////////////////////////////////////////////

function AxiAddrCmd#(`TLM_PRM) getAxiAddrCmd (RequestDescriptor#(`TLM_PRM) tlm_descriptor);

   AxiCache cache = ?;
   cache.write_allocate = tlm_descriptor.write_allocate;
   cache.read_allocate  = tlm_descriptor.read_allocate;
   cache.cache  = tlm_descriptor.cache;
   cache.buffer = tlm_descriptor.buffer;

   AxiProt prot = ?;
   prot.access    = tlm_descriptor.access;
   prot.security  = tlm_descriptor.security;
   prot.privilege = tlm_descriptor.privilege;

   AxiAddrCmd#(`TLM_PRM) addr_cmd = unpack(0);
   addr_cmd.id    = getAxiId(tlm_descriptor.transaction_id);
   addr_cmd.len   = getAxiLen(tlm_descriptor.b_length);
   addr_cmd.size  = getAxiSize(tlm_descriptor.b_size);
   addr_cmd.burst = getAxiBurst(tlm_descriptor.burst_mode);
   addr_cmd.lock  = getAxiLock(tlm_descriptor.lock);
   addr_cmd.cache = cache;
   addr_cmd.prot  = prot;
   addr_cmd.addr  = tlm_descriptor.addr;

   return addr_cmd;

endfunction

function AxiWrData#(`TLM_PRM) getAxiWrData (RequestDescriptor#(`TLM_PRM) tlm_descriptor);
   AxiWrData#(`TLM_PRM) wr_data = unpack(0);
   wr_data.id   = getAxiId(tlm_descriptor.transaction_id);
   wr_data.data = tlm_descriptor.data;
   wr_data.strb = '1;
   if (tlm_descriptor.byte_enable matches tagged Specify .be)
      wr_data.strb = be;
   wr_data.last = (tlm_descriptor.b_length == 0);
   return wr_data;
endfunction

function AxiLen getAxiLen(TLMBLength#(`TLM_PRM) burst_length);
   AxiLen length = truncateNP(burst_length);
   return length;
endfunction

function AxiSize getAxiSize(TLMBSize size);
   return size;
endfunction

function AxiBurst getAxiBurst(TLMBurstMode burst_mode);
   case (burst_mode)
      INCR: return INCR;
      CNST: return FIXED;
      WRAP: return WRAP;
   endcase
endfunction

function AxiLock getAxiLock(TLMLock lock);
   return lock;
endfunction

function AxiId#(`TLM_PRM) getAxiId(TLMId#(`TLM_PRM) transaction_id);
   return cExtend(transaction_id);
endfunction

function AxiResp getAxiResp(TLMStatus status, Bit#(n) data)
   provisos (Add#(_1, SizeOf#(TLMErrorCode), n));

   TLMErrorCode code = unpack(truncate(data));
   case (status)
      SUCCESS:     return OKAY;
      ERROR:       return (code == DECERR ? DECERR : SLVERR);
      EXOKAY:      return EXOKAY;
      UNKNOWN:     return SLVERR;
     endcase
endfunction

////////////////////////////////////////////////////////////////////////////////
/// AXI to TLM:
////////////////////////////////////////////////////////////////////////////////

function RequestDescriptor#(`TLM_PRM) fromAxiAddrCmd (AxiAddrCmd#(`TLM_PRM) addr_cmd)
   provisos(Bits#(RequestDescriptor#(`TLM_PRM), s0));

   RequestDescriptor#(`TLM_PRM) desc = unpack(0);
   desc.mode            = REGULAR;
   desc.addr            = addr_cmd.addr;
   desc.b_length        = fromAxiLen(addr_cmd.len);
   desc.burst_mode      = fromAxiBurst(addr_cmd.burst);
   desc.b_size          = fromAxiSize(addr_cmd.size);
   desc.prty            = 0;
   desc.lock            = fromAxiLock(addr_cmd.lock);
//   desc.thread_id       = 0;
   desc.transaction_id  = fromAxiId(addr_cmd.id);
//   desc.export_id       = 0;
   desc.read_allocate   = addr_cmd.cache.read_allocate;
   desc.write_allocate  = addr_cmd.cache.write_allocate;
   desc.cache     = addr_cmd.cache.cache;
   desc.buffer    = addr_cmd.cache.buffer;
   desc.access    = addr_cmd.prot.access;
   desc.security  = addr_cmd.prot.security;
   desc.privilege = addr_cmd.prot.privilege;
   desc.mark      = OPEN;

   desc.command         = READ; // added later
   desc.data            = 0;    // added later
   desc.byte_enable     = Calculate;

   return desc;
endfunction

function TLMBLength#(`TLM_PRM) fromAxiLen(AxiLen len);
   return extendNP(len);
endfunction

function TLMBurstMode fromAxiBurst(AxiBurst burst);
   case (burst)
      INCR:  return INCR;
      FIXED: return CNST;
      WRAP:  return WRAP;
   endcase
endfunction

function TLMBSize fromAxiSize(AxiSize size);
   return size;
endfunction

function TLMLock fromAxiLock(AxiLock lock);
   return lock;
endfunction

function TLMId#(`TLM_PRM) fromAxiId(AxiId#(`TLM_PRM) id);
   return cExtend(id);
endfunction

function TLMResponse#(`TLM_PRM) fromAxiResp(AxiResp value)
   provisos(DefaultValue#(TLMResponse#(`TLM_PRM)));

   TLMResponse#(`TLM_PRM) response = defaultValue;
   response.command        = WRITE;
   response.transaction_id = 0;
   response.data           = 0;
   response.is_last        = True;
   case (value)
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

function TLMResponse#(`TLM_PRM) fromAxiRdResp(AxiRdResp#(`TLM_PRM) value)
   provisos(DefaultValue#(TLMResponse#(`TLM_PRM)));

   TLMResponse#(`TLM_PRM) response = defaultValue;
   response.command        = READ;
   response.transaction_id = value.id;
   response.data           = value.data;
   response.is_last        = value.last;
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

function TLMResponse#(`TLM_PRM) fromAxiWrResp(AxiWrResp#(`TLM_PRM) value)
   provisos(DefaultValue#(TLMResponse#(`TLM_PRM)));

   TLMResponse#(`TLM_PRM) response = defaultValue;
   response.command        = WRITE;
   response.transaction_id = value.id;
   response.data           = 0;
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
      		 TLMErrorCode code = UNMAPPED;
		 response.status = ERROR;
		 response.data   = extendNP(pack(code));
	      end
   endcase
   return response;
endfunction


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Arbitable#(AxiWrBusMaster#(`TLM_PRM));
   module mkArbiterRequest#(AxiWrBusMaster#(`TLM_PRM) master) (ArbiterRequest_IFC);

      let addr_req <- mkArbiterRequest(master.addr);

      method Bool request;
	 return addr_req.request;
      endmethod
      method Bool lock;
	 return master.addr.data.lock == LOCKED;
      endmethod
      method Action grant;
	 dummyAction;
      endmethod

   endmodule
endinstance


instance Arbitable#(AxiRdBusMaster#(`TLM_PRM));
   module mkArbiterRequest#(AxiRdBusMaster#(`TLM_PRM) master) (ArbiterRequest_IFC);

      let addr_req <- mkArbiterRequest(master.addr);

      method Bool request;
	 return addr_req.request;
      endmethod
      method Bool lock;
	 return master.addr.data.lock == LOCKED;
      endmethod
      method Action grant;
	 dummyAction;
      endmethod

   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance FShow#(AxiAddrCmd#(`TLM_PRM));
   function Fmt fshow (AxiAddrCmd#(`TLM_PRM) cmd);
      return fshow(fromAxiAddrCmd(cmd));
   endfunction
endinstance


instance FShow#(AxiRdResp#(`TLM_PRM));
   function Fmt fshow (AxiRdResp#(`TLM_PRM) rsp);
      return fshow(fromAxiRdResp(rsp));
   endfunction
endinstance


instance FShow#(AxiBurst);
   function Fmt fshow (AxiBurst label);
      case (label)
	 FIXED: return fshow("FIXED");
	 INCR:  return fshow("INCR");
	 WRAP:  return fshow("WRAP");
      endcase
   endfunction
endinstance

instance FShow#(AxiResp);
   function Fmt fshow (AxiResp label);
      case (label)
	 OKAY:   return fshow("OKAY");
	 EXOKAY: return fshow("EXOKAY");
	 SLVERR: return fshow("SLVERR");
	 DECERR: return fshow("DECERR");
      endcase
   endfunction
endinstance


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage
