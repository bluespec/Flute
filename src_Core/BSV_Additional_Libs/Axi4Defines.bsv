// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi4Defines;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import Axi::*;
import Bus::*;
import CBus::*;
import Connectable::*;
import DefaultValue::*;
import FIFO::*;
import TLM3::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
/// Data Structures
////////////////////////////////////////////////////////////////////////////////

typedef UInt#(8)                         Axi4Len;  // 1 - 256
typedef TLMQos                           Axi4Qos;
typedef TLMRegion                        Axi4Region;

typedef struct {
		TLMAllocate  allocate;
		TLMAllocate  other_allocate;
		TLMModify    modify;
		TLMBuffer    buffer;
		} Axi4Cache deriving (Eq, Bits, Bounded);

typedef enum {NORMAL, EXCLUSIVE}  Axi4Lock    deriving(Bounded, Bits, Eq);

typedef struct {
                AxiId#(`TLM_PRM)     id;
                Axi4Len              len;
                AxiSize              size;
                AxiBurst             burst;
                Axi4Lock             lock;
                Axi4Cache            cache;
		AxiProt              prot;
		Axi4Qos              qos;
		AxiAddr#(`TLM_PRM)   addr;
		Axi4Region           region;
		TLMUser#(`TLM_PRM)   user_addr;
		} Axi4AddrCmd#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(Axi4AddrCmd#(`TLM_PRM));
   defaultValue = Axi4AddrCmd {
      id:     0,
      len:    0,
      size:   BITS8,
      burst:  INCR,
      lock:   NORMAL,
      cache:  unpack(0),
      prot:   unpack(0),
      qos:    0,
      addr:   ?,
      region: unpack(0),
      user_addr: 0
      };
endinstance

typedef struct {
		AxiData#(`TLM_PRM)   data;
		TLMUser#(`TLM_PRM)   user;
		AxiByteEn#(`TLM_PRM) strb;
		Bool                 last;
		} Axi4WrData#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(Axi4WrData#(`TLM_PRM));
   defaultValue = Axi4WrData {
      data:   ?,
      user:   0,
      strb:   '1,
      last:   True
      };
endinstance

typedef struct {
		AxiId#(`TLM_PRM)     id;
		AxiData#(`TLM_PRM)   data;
		AxiResp              resp;
		TLMUser#(`TLM_PRM)   user;
		Bool                 last;
		} Axi4RdResp#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(Axi4RdResp#(`TLM_PRM));
   defaultValue = Axi4RdResp {
      id:     0,
      data:   ?,
      resp:   OKAY,
      user:   0,
      last:   True
      };
endinstance

typedef struct {
		AxiId#(`TLM_PRM)     id;
		AxiResp              resp;
		TLMUser#(`TLM_PRM)   user;
		} Axi4WrResp#(`TLM_PRM_DCL) `dv;

instance DefaultValue#(Axi4WrResp#(`TLM_PRM));
   defaultValue = Axi4WrResp {
      id:     0,
      resp:   OKAY,
      user:   0
      };
endinstance


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function TLMResponse#(`TLM_PRM) fromAxi4RdResp(Axi4RdResp#(`TLM_PRM) value)
   provisos(DefaultValue#(TLMResponse#(`TLM_PRM)));

   TLMResponse#(`TLM_PRM) response = defaultValue;
   response.command        = READ;
   response.transaction_id = value.id;
   response.data           = value.data;
   response.user           = value.user;
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


function TLMResponse#(`TLM_PRM) fromAxi4WrResp(Axi4WrResp#(`TLM_PRM) value)
   provisos(DefaultValue#(TLMResponse#(`TLM_PRM)));

   TLMResponse#(`TLM_PRM) response = defaultValue;
   response.command        = WRITE;
   response.transaction_id = value.id;
   response.data           = 0;
   response.user           = value.user;
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
/// Interfaces
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface Axi4WrMaster#(`TLM_PRM_DCL);

   // Address Outputs
   (* result = "AWID" *)
   method AxiId#(`TLM_PRM)     awID;
   (* result = "AWADDR" *)
   method AxiAddr#(`TLM_PRM)   awADDR;
   (* result = "AWUSER" *)
   method TLMUser#(`TLM_PRM)   awUSER;
   (* result = "AWLEN" *)
   method Axi4Len              awLEN;
   (* result = "AWSIZE" *)
   method AxiSize              awSIZE;
   (* result = "AWBURST" *)
   method AxiBurst             awBURST;
   (* result = "AWLOCK" *)
   method Axi4Lock             awLOCK;
   (* result = "AWCACHE" *)
   method Axi4Cache            awCACHE;
   (* result = "AWPROT" *)
   method AxiProt              awPROT;
   (* result = "AWQOS" *)
   method Axi4Qos              awQOS;
   (* result = "AWVALID" *)
   method Bool                 awVALID;

   // Address Inputs
   (* prefix = "", result = "unusedwm0" *)
   method Action awREADY((* port = "AWREADY" *) Bool value);

   // Data Outputs
   (* result = "WDATA" *)
   method AxiData#(`TLM_PRM)     wDATA;
   (* result = "WUSER" *)
   method TLMUser#(`TLM_PRM)     wUSER;
   (* result = "WSTRB" *)
   method AxiByteEn#(`TLM_PRM)   wSTRB;
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
   method Action bUSER((* port = "BUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedwm5" *)
   method Action bVALID((* port = "BVALID" *) Bool value);

endinterface

(* always_ready, always_enabled *)
interface Axi4RdMaster#(`TLM_PRM_DCL);

   // Address Outputs
   (* result = "ARID" *)
   method AxiId#(`TLM_PRM)     arID;
   (* result = "ARADDR" *)
   method AxiAddr#(`TLM_PRM)   arADDR;
   (* result = "ARUSER" *)
   method TLMUser#(`TLM_PRM)   arUSER;
   (* result = "ARLEN" *)
   method Axi4Len              arLEN;
   (* result = "ARSIZE" *)
   method AxiSize              arSIZE;
   (* result = "ARBURST" *)
   method AxiBurst             arBURST;
   (* result = "ARLOCK" *)
   method Axi4Lock             arLOCK;
   (* result = "ARCACHE" *)
   method Axi4Cache            arCACHE;
   (* result = "ARPROT" *)
   method AxiProt              arPROT;
   (* result = "ARQOS" *)
   method Axi4Qos              arQOS;
   (* result = "ARVALID" *)
   method Bool                 arVALID;

   // Address Inputs
   (* prefix = "", result = "unusedrm0" *)
   method Action arREADY((* port = "ARREADY" *) Bool value);

   // Response Outputs
   (* result = "RREADY" *)
   method Bool                 rREADY;

   // Response Inputs
   (* prefix = "", result = "unusedrm1" *)
   method Action rID((* port = "RID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm2" *)
   method Action rDATA((* port = "RDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm3" *)
   method Action rRESP((* port = "RRESP" *) AxiResp value);
   (* prefix = "", result = "unusedrm4" *)
   method Action rUSER((* port = "RUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm5" *)
   method Action rLAST((* port = "RLAST" *) Bool value);
   (* prefix = "", result = "unusedrm6" *)
   method Action rVALID((* port = "RVALID" *) Bool value);

endinterface


(* always_ready, always_enabled *)
interface Axi4WrSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedws0" *)
   method Action awID((* port = "AWID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1" *)
   method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1A" *)
   method Action awREGION((* port = "AWREGION" *) Axi4Region value);
   (* prefix = "", result = "unusedws2" *)
   method Action awUSER((* port = "AWUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws3" *)
   method Action awLEN((* port = "AWLEN" *) Axi4Len value);
   (* prefix = "", result = "unusedws4" *)
   method Action awSIZE((* port = "AWSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedws5" *)
   method Action awBURST((* port = "AWBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedws6" *)
   method Action awLOCK((* port = "AWLOCK" *) Axi4Lock value);
   (* prefix = "", result = "unusedws7" *)
   method Action awCACHE((* port = "AWCACHE" *) Axi4Cache value);
   (* prefix = "", result = "unusedws8" *)
   method Action awPROT((* port = "AWPROT" *) AxiProt value);
   (* prefix = "", result = "unusedws9" *)
   method Action awQOS((* port = "AWQOS" *) Axi4Qos value);
   (* prefix = "", result = "unusedws10" *)
   method Action awVALID((* port = "AWVALID" *) Bool value);

   // Address Outputs
   (* result = "AWREADY" *)
   method Bool                   awREADY;

   // Data Inputs
   (* prefix = "", result = "unusedws11" *)
   method Action wDATA((* port = "WDATA" *) AxiData#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws12" *)
   method Action wUSER((* port = "WUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws13" *)
   method Action wSTRB((* port = "WSTRB" *) AxiByteEn#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws14" *)
   method Action wLAST((* port = "WLAST" *) Bool value);
   (* prefix = "", result = "unusedws15" *)
   method Action wVALID((* port = "WVALID" *) Bool value);

   // Data Ouptuts
   (* result = "WREADY" *)
   method Bool                   wREADY;

   // Response Inputs
   (* prefix = "", result = "unusedws16" *)
   method Action bREADY((* port = "BREADY" *) Bool value);

   // Response Outputs
   (* result = "BID" *)
   method AxiId#(`TLM_PRM)       bID;
   (* result = "BRESP" *)
   method AxiResp                bRESP;
   (* result = "BUSER" *)
   method TLMUser#(`TLM_PRM)     bUSER;
   (* result = "BVALID" *)
   method Bool                   bVALID;

endinterface

(* always_ready, always_enabled *)
interface Axi4RdSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedrs0" *)
   method Action arID((* port = "ARID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs1" *)
   method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1A" *)
   method Action arREGION((* port = "ARREGION" *) Axi4Region value);
   (* prefix = "", result = "unusedws2" *)
   method Action arUSER((* port = "ARUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs3" *)
   method Action arLEN((* port = "ARLEN" *) Axi4Len value);
   (* prefix = "", result = "unusedrs4" *)
   method Action arSIZE((* port = "ARSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedrs5" *)
   method Action arBURST((* port = "ARBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedrs6" *)
   method Action arLOCK((* port = "ARLOCK" *) Axi4Lock value);
   (* prefix = "", result = "unusedrs7" *)
   method Action arCACHE((* port = "ARCACHE" *) Axi4Cache value);
   (* prefix = "", result = "unusedrs8" *)
   method Action arPROT((* port = "ARPROT" *) AxiProt value);
   (* prefix = "", result = "unusedws9" *)
   method Action arQOS((* port = "ARQOS" *) Axi4Qos value);
   (* prefix = "", result = "unusedrs10" *)
   method Action arVALID((* port = "ARVALID" *) Bool value);

   // Address Outputs
   (* result = "ARREADY" *)
   method Bool                   arREADY;

   // Response Inputs
   (* prefix = "", result = "unusedrs11" *)
   method Action rREADY((* port = "RREADY" *) Bool value);

   // Response Outputs
   (* result = "RID" *)
   method AxiId#(`TLM_PRM)       rID;
   (* result = "RDATA" *)
   method AxiData#(`TLM_PRM)     rDATA;
   (* result = "RRESP" *)
   method AxiResp                rRESP;
   (* result = "RUSER" *)
   method TLMUser#(`TLM_PRM)     rUSER;
   (* result = "RLAST" *)
   method Bool                   rLAST;
   (* result = "RVALID" *)
   method Bool                   rVALID;

endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Axi4RdBusMaster#(`TLM_PRM_DCL);
   interface BusSend#(Axi4AddrCmd#(`TLM_PRM)) addr;
   interface BusRecv#(Axi4RdResp#(`TLM_PRM))  resp;
endinterface

interface Axi4WrBusMaster#(`TLM_PRM_DCL);
   interface BusSend#(Axi4AddrCmd#(`TLM_PRM)) addr;
   interface BusSend#(Axi4WrData#(`TLM_PRM))  data;
   interface BusRecv#(Axi4WrResp#(`TLM_PRM))  resp;
endinterface

interface Axi4RdBusSlave#(`TLM_PRM_DCL);
   interface BusRecv#(Axi4AddrCmd#(`TLM_PRM)) addr;
   interface BusSend#(Axi4RdResp#(`TLM_PRM))  resp;
endinterface

interface Axi4WrBusSlave#(`TLM_PRM_DCL);
   interface BusRecv#(Axi4AddrCmd#(`TLM_PRM)) addr;
   interface BusRecv#(Axi4WrData#(`TLM_PRM))  data;
   interface BusSend#(Axi4WrResp#(`TLM_PRM))  resp;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Axi4RdFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdMaster#(`TLM_PRM) bus;
endinterface

interface Axi4RdFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

interface Axi4WrFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4WrMaster#(`TLM_PRM) bus;
endinterface

interface Axi4WrFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4WrSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

interface Axi4RdMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface Axi4RdFabricMaster#(`TLM_PRM) fabric;
endinterface

interface Axi4WrMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface Axi4WrFabricMaster#(`TLM_PRM) fabric;
endinterface

interface Axi4RdSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)       tlm;
   (* prefix = "" *)
   interface Axi4RdFabricSlave#(`TLM_PRM) fabric;
endinterface

interface Axi4WrSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)       tlm;
   (* prefix = "" *)
   interface Axi4WrFabricSlave#(`TLM_PRM) fabric;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Axi4RdWrMasterXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMRecvIFC#(`TLM_RR)         tlm;
   (* prefix = "" *)
   interface Axi4RdFabricMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4WrFabricMaster#(`TLM_PRM) write;
endinterface

interface Axi4RdWrSlaveXActorIFC#(`TLM_RR_DCL, `TLM_PRM_DCL);
   interface TLMSendIFC#(`TLM_RR)        tlm;
   (* prefix = "" *)
   interface Axi4RdFabricSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4WrFabricSlave#(`TLM_PRM) write;
endinterface

interface Axi4RdWrMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdMaster#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4WrMaster#(`TLM_PRM) write;
endinterface

interface Axi4RdWrSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi4RdSlave#(`TLM_PRM) read;
   (* prefix = "" *)
   interface Axi4WrSlave#(`TLM_PRM) write;
endinterface

instance Connectable#(Axi4RdWrMaster#(`TLM_PRM), Axi4RdWrSlave#(`TLM_PRM));
   module mkConnection#(Axi4RdWrMaster#(`TLM_PRM) s, Axi4RdWrSlave#(`TLM_PRM) m) (Empty);
      let rdConn <- mkConnection(m.read,s.read);
      let wrConn <- mkConnection(m.write,s.write);
   endmodule
endinstance
instance Connectable#(Axi4RdWrSlave#(`TLM_PRM), Axi4RdWrMaster#(`TLM_PRM));
   module mkConnection#(Axi4RdWrSlave#(`TLM_PRM) s, Axi4RdWrMaster#(`TLM_PRM) m) (Empty);
      (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(Axi4WrMaster#(`TLM_PRM), Axi4WrSlave#(`TLM_PRM));
   module mkConnection#(Axi4WrMaster#(`TLM_PRM) m, Axi4WrSlave#(`TLM_PRM) s )(Empty);

      rule master_to_slave_addr_data;
	 // Address Signals
	 s.awID(m.awID);
	 s.awADDR(m.awADDR);
	 s.awREGION(unpack(0));
	 s.awLEN(m.awLEN);
	 s.awSIZE(m.awSIZE);
	 s.awBURST(m.awBURST);
	 s.awLOCK(m.awLOCK);
	 s.awCACHE(m.awCACHE);
	 s.awPROT(m.awPROT);
	 s.awQOS(m.awQOS);
	 s.awUSER(m.awUSER);
	 s.awVALID(m.awVALID);
	 // Data Signals
	 s.wDATA(m.wDATA);
	 s.wSTRB(m.wSTRB);
	 s.wLAST(m.wLAST);
	 s.wUSER(m.wUSER);
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
	 m.bUSER(s.bUSER);
	 m.bVALID(s.bVALID);
      endrule

   endmodule
endinstance
instance Connectable#(Axi4WrSlave#(`TLM_PRM), Axi4WrMaster#(`TLM_PRM));
   module mkConnection#(Axi4WrSlave#(`TLM_PRM) s, Axi4WrMaster#(`TLM_PRM) m )(Empty);
   (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance

instance Connectable#(Axi4RdMaster#(`TLM_PRM), Axi4RdSlave#(`TLM_PRM));
   module mkConnection#(Axi4RdMaster#(`TLM_PRM) m, Axi4RdSlave#(`TLM_PRM) s )(Empty);

      rule master_to_slave_addr;
	 // Address Signals
	 s.arID(m.arID);
	 s.arADDR(m.arADDR);
	 s.arREGION(unpack(0));
	 s.arUSER(m.arUSER);
	 s.arLEN(m.arLEN);
	 s.arSIZE(m.arSIZE);
	 s.arBURST(m.arBURST);
	 s.arLOCK(m.arLOCK);
	 s.arCACHE(m.arCACHE);
	 s.arPROT(m.arPROT);
	 s.arQOS(m.arQOS);
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
	 m.rUSER(s.rUSER);
	 m.rLAST(s.rLAST);
	 m.rVALID(s.rVALID);
      endrule

   endmodule
endinstance
instance Connectable#( Axi4RdSlave#(`TLM_PRM), Axi4RdMaster#(`TLM_PRM));
   module mkConnection#( Axi4RdSlave#(`TLM_PRM) s, Axi4RdMaster#(`TLM_PRM) m)(Empty);
   (*hide*) let _i <- mkConnection(m,s);
   endmodule
endinstance

instance Connectable#(Axi4RdFabricMaster#(`TLM_PRM), Axi4RdFabricSlave#(`TLM_PRM));
   module mkConnection#(Axi4RdFabricMaster#(`TLM_PRM) m, Axi4RdFabricSlave#(`TLM_PRM) s )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(Axi4RdFabricSlave#(`TLM_PRM), Axi4RdFabricMaster#(`TLM_PRM));
   module mkConnection#(Axi4RdFabricSlave#(`TLM_PRM) s, Axi4RdFabricMaster#(`TLM_PRM) m )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(Axi4WrFabricMaster#(`TLM_PRM), Axi4WrFabricSlave#(`TLM_PRM));
   module mkConnection#(Axi4WrFabricMaster#(`TLM_PRM) m, Axi4WrFabricSlave#(`TLM_PRM) s )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

instance Connectable#(Axi4WrFabricSlave#(`TLM_PRM), Axi4WrFabricMaster#(`TLM_PRM));
   module mkConnection#(Axi4WrFabricSlave#(`TLM_PRM) s, Axi4WrFabricMaster#(`TLM_PRM) m )(Empty);
      mkConnection(m.bus, s.bus);
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
/// TLM conversion functions
/// TLM to AXI4:
////////////////////////////////////////////////////////////////////////////////

function Axi4AddrCmd#(`TLM_PRM) getAxi4AddrCmd (RequestDescriptor#(`TLM_PRM) tlm_descriptor);

   Axi4Cache cache = ?;
   cache.allocate = tlm_descriptor.write_allocate;
   cache.other_allocate = tlm_descriptor.read_allocate;
   cache.modify  = unpack(pack(tlm_descriptor.cache));
   cache.buffer = tlm_descriptor.buffer;

   AxiProt prot = ?;
   prot.access    = tlm_descriptor.access;
   prot.security  = tlm_descriptor.security;
   prot.privilege = tlm_descriptor.privilege;

   Axi4AddrCmd#(`TLM_PRM) addr_cmd = unpack(0);
   addr_cmd.id    = getAxiId(tlm_descriptor.transaction_id);
   addr_cmd.len   = getAxi4Len(tlm_descriptor.b_length);
   addr_cmd.size  = getAxiSize(tlm_descriptor.b_size);
   addr_cmd.burst = getAxiBurst(tlm_descriptor.burst_mode);
   addr_cmd.lock  = getAxi4Lock(tlm_descriptor.lock);
   addr_cmd.cache = cache;
   addr_cmd.prot  = prot;
   addr_cmd.qos   = tlm_descriptor.prty;
   addr_cmd.addr  = tlm_descriptor.addr;
   addr_cmd.region = tlm_descriptor.region;
   addr_cmd.user_addr = tlm_descriptor.user_addr;

   return addr_cmd;

endfunction

function Axi4WrData#(`TLM_PRM) getAxi4WrData (RequestDescriptor#(`TLM_PRM) tlm_descriptor);
   Axi4WrData#(`TLM_PRM) wr_data = ?;
   wr_data.data = tlm_descriptor.data;
   wr_data.user = tlm_descriptor.user;
   wr_data.strb = '1;
   if (tlm_descriptor.byte_enable matches tagged Specify .be)
      wr_data.strb = be;
   wr_data.last = (tlm_descriptor.b_length == 0);
   return wr_data;
endfunction

function Axi4Len getAxi4Len(TLMBLength#(`TLM_PRM) burst_length);
   return unpack(truncateNP(pack(burst_length)));
endfunction

function Axi4Lock getAxi4Lock(TLMLock lock);
   return unpack(pack(lock)[0]);
endfunction

////////////////////////////////////////////////////////////////////////////////
/// AXI4 to TLM:
////////////////////////////////////////////////////////////////////////////////

function RequestDescriptor#(`TLM_PRM) fromAxi4AddrCmd (Axi4AddrCmd#(`TLM_PRM) addr_cmd)
   provisos(Bits#(RequestDescriptor#(`TLM_PRM), s0));

   RequestDescriptor#(`TLM_PRM) desc = unpack(0);
   desc.mode            = REGULAR;
   desc.addr            = addr_cmd.addr;
   desc.user_addr       = addr_cmd.user_addr;
   desc.region          = addr_cmd.region;
   desc.b_length        = fromAxi4Len(addr_cmd.len);
   desc.burst_mode      = fromAxiBurst(addr_cmd.burst);
   desc.b_size          = fromAxiSize(addr_cmd.size);
   desc.prty            = fromAxi4Qos(addr_cmd.qos);
   desc.lock            = fromAxi4Lock(addr_cmd.lock);
//   desc.thread_id       = 0;
   desc.transaction_id  = fromAxiId(addr_cmd.id);
//   desc.export_id       = 0;
   desc.read_allocate   = addr_cmd.cache.other_allocate;
   desc.write_allocate  = addr_cmd.cache.allocate;
   desc.cache     = unpack(pack(addr_cmd.cache.modify));
   desc.buffer    = addr_cmd.cache.buffer;
   desc.access    = addr_cmd.prot.access;
   desc.security  = addr_cmd.prot.security;
   desc.privilege = addr_cmd.prot.privilege;

   desc.command         = READ; // added later
   desc.data            = 0;    // added later
   desc.byte_enable     = Calculate;

   return desc;
endfunction

function TLMBLength#(`TLM_PRM) fromAxi4Len(Axi4Len len);
   return unpack(extendNP(pack(len)));
endfunction

function TLMQos fromAxi4Qos(Axi4Qos qos);
   return qos;
endfunction

function TLMLock fromAxi4Lock(Axi4Lock lock);
   return ((lock == EXCLUSIVE) ? EXCLUSIVE : NORMAL);
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass ToAxi4#(type a, type b)
   dependencies (a determines b);
   module toAxi4(a ifc, b ignore);
endtypeclass

instance ToAxi4#(AxiWrMaster#(`TLM_PRM), Axi4WrMaster#(`TLM_PRM));
   module toAxi4#(AxiWrMaster#(`TLM_PRM) ifc) (Axi4WrMaster#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4WrMaster
		 // Addr Outputs
		 method awID    = ifc.awID;
		 method awADDR  = ifc.awADDR;
		 method awUSER  = 0;
		 method awLEN   = unpack(extendNP(pack(ifc.awLEN)));
		 method awSIZE  = ifc.awSIZE;
		 method awBURST = ifc.awBURST;
		 method awLOCK  = (ifc.awLOCK == EXCLUSIVE) ? EXCLUSIVE : NORMAL;
		 method awCACHE = unpack(pack(ifc.awCACHE));
		 method awPROT  = ifc.awPROT;
		 method awQOS   = unpack(0);
		 method awVALID = ifc.awVALID;

		 // Addr Inputs
		 method awREADY = ifc.awREADY;

		 // Data Outputs
		 method wDATA   = ifc.wDATA;
		 method wUSER   = 0;
		 method wSTRB   = ifc.wSTRB;
		 method wLAST   = ifc.wLAST;
		 method wVALID  = ifc.wVALID;

		 // Data Inputs
		 method wREADY = ifc.wREADY;

		 // Response Outputs
		 method bREADY = ifc.bREADY;

		 // Response Inputs
		 method bID    = ifc.bID;
		 method bRESP  = ifc.bRESP;
		 method bUSER  = nop;
		 method bVALID  = ifc.bVALID;
	 endinterface);
   endmodule
endinstance

instance ToAxi4#(AxiRdMaster#(`TLM_PRM), Axi4RdMaster#(`TLM_PRM));
   module toAxi4#(AxiRdMaster#(`TLM_PRM) ifc) (Axi4RdMaster#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4RdMaster
		 // Addr Outputs
		 method arID    = ifc.arID;
		 method arADDR  = ifc.arADDR;
		 method arUSER  = 0;
		 method arLEN   = unpack(extendNP(pack(ifc.arLEN)));
		 method arSIZE  = ifc.arSIZE;
		 method arBURST = ifc.arBURST;
		 method arLOCK  = (ifc.arLOCK == EXCLUSIVE) ? EXCLUSIVE : NORMAL;
	 	 method arCACHE = unpack(pack(ifc.arCACHE));
		 method arPROT  = ifc.arPROT;
		 method arQOS   = unpack(0);
		 method arVALID = ifc.arVALID;

		 // Addr Inputs
		 method arREADY = ifc.arREADY;

		 // Response Outputs
		 method rREADY = ifc.rREADY;

		 // Response Inputs
		 method rID    = ifc.rID;
	 	 method rDATA  = ifc.rDATA;
		 method rRESP  = ifc.rRESP;
		 method rUSER  = nop;
		 method rLAST   = ifc.rLAST;
	 	 method rVALID  = ifc.rVALID;
	      endinterface);
   endmodule
endinstance

instance ToAxi4#(AxiWrSlave#(`TLM_PRM), Axi4WrSlave#(`TLM_PRM));
   module toAxi4#(AxiWrSlave#(`TLM_PRM) ifc) (Axi4WrSlave#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      FIFO#(TLMId#(`TLM_PRM)) id_fifo   <- mkSizedFIFO(3);
      Wire#(TLMId#(`TLM_PRM)) id_wire   <- mkDWire(unpack(0));


      Wire#(Bool)             aw_valid  <- mkBypassWire;
      Wire#(TLMId#(`TLM_PRM)) aw_id     <- mkBypassWire;
      Wire#(Bool)             w_valid   <- mkBypassWire;
      Wire#(Bool)             w_last    <- mkBypassWire;

      rule every;
	 id_wire <= id_fifo.first;
      endrule

      rule set_wid;
	 ifc.wID(id_wire);
      endrule

      rule enq_id (ifc.awREADY && aw_valid);
	 id_fifo.enq(aw_id);
      endrule

      rule deq_id (ifc.wREADY && w_valid && w_last);
	 id_fifo.deq;
      endrule

      return (interface Axi4WrSlave
		 // Addr Inputs;
		 method Action awID (value);
		    ifc.awID(value);
		    aw_id <= value;
		 endmethod
		 method awADDR   = ifc.awADDR;
	 	 method awREGION = nop;
		 method awUSER   = nop;
		 method Action awLEN (value);
		    ifc.awLEN(unpack(truncateNP(pack(value))));
		 endmethod
	 	 method awSIZE  = ifc.awSIZE;
		 method awBURST = ifc.awBURST;
		 method Action awLOCK (value);
		    ifc.awLOCK (unpack(extendNP(pack(value))));
		 endmethod
		 method Action awCACHE (value);
		    ifc.awCACHE (unpack(pack(value)));
		 endmethod
		 method awPROT  = ifc.awPROT;
		 method awQOS   = nop;
		 method Action awVALID (Bool value);
		    ifc.awVALID(value);
		    aw_valid <= value;
		 endmethod

		 // Addr Outputs
		 method awREADY = ifc.awREADY;

		 // Data Inputs
	 	 method wDATA  = ifc.wDATA;
	      	 method wUSER   = nop;
		 method wSTRB   = ifc.wSTRB;
		 method Action wLAST (Bool value);
		    ifc.wLAST(value);
		    w_last <= value;
		 endmethod
	 	 method Action wVALID (Bool value);
		    ifc.wVALID(value);
		    w_valid <= value;
		 endmethod

		 // Data Outputs;
		 method wREADY  = ifc.wREADY;

		 // Response Inputs
		 method bREADY  = ifc.bREADY;

		 // Response Outputs
		 method bID     = ifc.bID;
		 method bRESP   = ifc.bRESP;
		 method bUSER   = 0;
		 method bVALID  = ifc.bVALID;
	      endinterface);
   endmodule
endinstance


instance ToAxi4#(AxiRdSlave#(`TLM_PRM), Axi4RdSlave#(`TLM_PRM));
   module toAxi4#(AxiRdSlave#(`TLM_PRM) ifc) (Axi4RdSlave#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4RdSlave
		 // Addr Inputs;
		 method arID    = ifc.arID;
		 method arADDR  = ifc.arADDR;
	 	 method arREGION = nop;
		 method arUSER   = nop;
		 method Action arLEN (value);
		    ifc.arLEN(unpack(truncateNP(pack(value))));
		 endmethod
	 	 method arSIZE  = ifc.arSIZE;
		 method arBURST = ifc.arBURST;
		 method Action arLOCK (value);
		    ifc.arLOCK (unpack(extendNP(pack(value))));
		 endmethod
	 	 method Action arCACHE (value);
		    ifc.arCACHE (unpack(pack(value)));
		 endmethod
		 method arPROT  = ifc.arPROT;
		 method arQOS   = nop;
		 method arVALID = ifc.arVALID;

		 // Addr Outputs
		 method arREADY = ifc.arREADY;

		 // Response Inputs
		 method rREADY = ifc.rREADY;

		 // Response Outputs
		 method rID    = ifc.rID;
	 	 method rDATA  = ifc.rDATA;
		 method rRESP  = ifc.rRESP;
		 method rUSER  = 0;
		 method rLAST  = ifc.rLAST;
	 	 method rVALID = ifc.rVALID;
	      endinterface);
   endmodule
endinstance


instance ToAxi4#(AxiWrFabricMaster#(`TLM_PRM), Axi4WrFabricMaster#(`TLM_PRM));
   module toAxi4#(AxiWrFabricMaster#(`TLM_PRM) ifc) (Axi4WrFabricMaster#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus = _ifc;

   endmodule
endinstance


instance ToAxi4#(AxiRdFabricMaster#(`TLM_PRM), Axi4RdFabricMaster#(`TLM_PRM));
   module toAxi4#(AxiRdFabricMaster#(`TLM_PRM) ifc) (Axi4RdFabricMaster#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus = _ifc;

   endmodule
endinstance

instance ToAxi4#(AxiWrFabricSlave#(`TLM_PRM), Axi4WrFabricSlave#(`TLM_PRM));
   module toAxi4#(AxiWrFabricSlave#(`TLM_PRM) ifc) (Axi4WrFabricSlave#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus       = _ifc;
      interface addrMatch = ifc.addrMatch;

   endmodule
endinstance

instance ToAxi4#(AxiRdFabricSlave#(`TLM_PRM), Axi4RdFabricSlave#(`TLM_PRM));
   module toAxi4#(AxiRdFabricSlave#(`TLM_PRM) ifc) (Axi4RdFabricSlave#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus       = _ifc;
      interface addrMatch = ifc.addrMatch;

   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage
