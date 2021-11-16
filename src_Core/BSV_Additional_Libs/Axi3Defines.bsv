// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi3Defines;

import Axi4Defines::*;
import Axi4LDefines::*;
import AxiDefines::*;
import CBus::*;
import FIFO::*;
import FIFOF::*;
import TLM3::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

(* always_ready, always_enabled *)
interface Axi3WrMaster#(`TLM_PRM_DCL);

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
   (* result = "WUSER" *)
   method TLMUser#(`TLM_PRM)   wUSER;
   (* result = "WSTRB" *)
   method AxiByteEn#(`TLM_PRM) wSTRB;
   (* result = "WLAST" *)
   method Bool                 wLAST;
   (* result = "WVALID" *)
   method Bool                 wVALID;

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
interface Axi3RdMaster#(`TLM_PRM_DCL);

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
   method AxiLock             arLOCK;
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
   method Action rUSER((* port = "RUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrm5" *)
   method Action rLAST((* port = "RLAST" *) Bool value);
   (* prefix = "", result = "unusedrm6" *)
   method Action rVALID((* port = "RVALID" *) Bool value);

endinterface


(* always_ready, always_enabled *)
interface Axi3WrSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedws0" *)
   method Action awID((* port = "AWID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws1" *)
   method Action awADDR((* port = "AWADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws2" *)
   method Action awUSER((* port = "AWUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws3" *)
   method Action awLEN((* port = "AWLEN" *) Axi4Len value);
   (* prefix = "", result = "unusedws4" *)
   method Action awSIZE((* port = "AWSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedws5" *)
   method Action awBURST((* port = "AWBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedws6" *)
   method Action awLOCK((* port = "AWLOCK" *) AxiLock value);
   (* prefix = "", result = "unusedws7" *)
   method Action awCACHE((* port = "AWCACHE" *) AxiCache value);
   (* prefix = "", result = "unusedws8" *)
   method Action awPROT((* port = "AWPROT" *) AxiProt value);
   (* prefix = "", result = "unusedws9" *)
   method Action awVALID((* port = "AWVALID" *) Bool value);

   // Address Outputs
   (* result = "AWREADY" *)
   method Bool                  awREADY;

   // Data Inputs
   (* prefix = "", result = "unusedws10" *)
   method Action wID((* port = "WID" *)     AxiId#(`TLM_PRM) value);
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
   method AxiId#(`TLM_PRM)     bID;
   (* result = "BRESP" *)
   method AxiResp                bRESP;
   (* result = "BUSER" *)
   method TLMUser#(`TLM_PRM)      bUSER;
   (* result = "BVALID" *)
   method Bool                   bVALID;

endinterface

(* always_ready, always_enabled *)
interface Axi3RdSlave#(`TLM_PRM_DCL);

   // Address Inputs
   (* prefix = "", result = "unusedrs0" *)
   method Action arID((* port = "ARID" *) AxiId#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs1" *)
   method Action arADDR((* port = "ARADDR" *) AxiAddr#(`TLM_PRM) value);
   (* prefix = "", result = "unusedws2" *)
   method Action arUSER((* port = "ARUSER" *) TLMUser#(`TLM_PRM) value);
   (* prefix = "", result = "unusedrs3" *)
   method Action arLEN((* port = "ARLEN" *) Axi4Len value);
   (* prefix = "", result = "unusedrs4" *)
   method Action arSIZE((* port = "ARSIZE" *) AxiSize value);
   (* prefix = "", result = "unusedrs5" *)
   method Action arBURST((* port = "ARBURST" *) AxiBurst value);
   (* prefix = "", result = "unusedrs6" *)
   method Action arLOCK((* port = "ARLOCK" *) AxiLock value);
   (* prefix = "", result = "unusedrs7" *)
   method Action arCACHE((* port = "ARCACHE" *) AxiCache value);
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
   (* result = "RID" *)
   method AxiId#(`TLM_PRM)        rID;
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

interface Axi3RdFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi3RdMaster#(`TLM_PRM) bus;
endinterface

interface Axi3RdFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi3RdSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

interface Axi3WrFabricMaster#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi3WrMaster#(`TLM_PRM) bus;
endinterface

interface Axi3WrFabricSlave#(`TLM_PRM_DCL);
   (* prefix = "" *)
   interface Axi3WrSlave#(`TLM_PRM) bus;
   method Bool addrMatch(AxiAddr#(`TLM_PRM) value);
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance ToAxi4#(Axi3WrMaster#(`TLM_PRM), Axi4WrMaster#(`TLM_PRM));
   module toAxi4#(Axi3WrMaster#(`TLM_PRM) ifc) (Axi4WrMaster#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4WrMaster
		 // Addr Outputs
		 method awID    = ifc.awID;
		 method awADDR  = ifc.awADDR;
		 method awUSER  = ifc.awUSER;
	 	 method awLEN   = ifc.awLEN;
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
		 method wUSER   = ifc.wUSER;
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
		 method bUSER  = ifc.bUSER;
		 method bVALID = ifc.bVALID;
	      endinterface);
   endmodule
endinstance

instance ToAxi4#(Axi3RdMaster#(`TLM_PRM), Axi4RdMaster#(`TLM_PRM));
   module toAxi4#(Axi3RdMaster#(`TLM_PRM) ifc) (Axi4RdMaster#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4RdMaster
		 // Addr Outputs
		 method arID    = ifc.arID;
		 method arADDR  = ifc.arADDR;
		 method arUSER  = ifc.arUSER;
	 	 method arLEN   = ifc.arLEN;
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
		 method rUSER  = ifc.rUSER;
		 method rLAST   = ifc.rLAST;
	 	 method rVALID  = ifc.rVALID;
	      endinterface);
   endmodule
endinstance

instance ToAxi4#(Axi3WrSlave#(`TLM_PRM), Axi4WrSlave#(`TLM_PRM));
   module toAxi4#(Axi3WrSlave#(`TLM_PRM) ifc) (Axi4WrSlave#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      FIFOF#(TLMId#(`TLM_PRM)) id_fifo   <- mkSizedFIFOF(4);
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
		 method awUSER  = ifc.awUSER;
	 	 method awLEN   = ifc.awLEN;
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
		    ifc.awVALID(value && id_fifo.notFull);
		    aw_valid <= value;
		 endmethod

		 // Addr Outputs
		 method awREADY = ifc.awREADY;

		 // Data Inputs
		 method wDATA  = ifc.wDATA;
		 method wUSER  = ifc.wUSER;
		 method wSTRB  = ifc.wSTRB;
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
		 method bUSER   = ifc.bUSER;
		 method bVALID  = ifc.bVALID;
	       endinterface);
   endmodule
endinstance


instance ToAxi4#(Axi3RdSlave#(`TLM_PRM), Axi4RdSlave#(`TLM_PRM));
   module toAxi4#(Axi3RdSlave#(`TLM_PRM) ifc) (Axi4RdSlave#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4RdSlave
		 // Addr Inputs;
		 method arID     = ifc.arID;
		 method arADDR   = ifc.arADDR;
	 	 method arREGION = nop;
		 method arUSER   = ifc.arUSER;
	 	 method arLEN    = ifc.arLEN;
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
		 method rUSER  = ifc.rUSER;
		 method rLAST  = ifc.rLAST;
	 	 method rVALID = ifc.rVALID;
	      endinterface);
   endmodule
endinstance

instance ToAxi4#(Axi3WrFabricMaster#(`TLM_PRM), Axi4WrFabricMaster#(`TLM_PRM));
   module toAxi4#(Axi3WrFabricMaster#(`TLM_PRM) ifc) (Axi4WrFabricMaster#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus = _ifc;

   endmodule
endinstance


instance ToAxi4#(Axi3RdFabricMaster#(`TLM_PRM), Axi4RdFabricMaster#(`TLM_PRM));
   module toAxi4#(Axi3RdFabricMaster#(`TLM_PRM) ifc) (Axi4RdFabricMaster#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus = _ifc;

   endmodule
endinstance

instance ToAxi4#(Axi3WrFabricSlave#(`TLM_PRM), Axi4WrFabricSlave#(`TLM_PRM));
   module toAxi4#(Axi3WrFabricSlave#(`TLM_PRM) ifc) (Axi4WrFabricSlave#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus       = _ifc;
      interface addrMatch = ifc.addrMatch;

   endmodule
endinstance

instance ToAxi4#(Axi3RdFabricSlave#(`TLM_PRM), Axi4RdFabricSlave#(`TLM_PRM));
   module toAxi4#(Axi3RdFabricSlave#(`TLM_PRM) ifc) (Axi4RdFabricSlave#(`TLM_PRM));

      let _ifc <- toAxi4(ifc.bus);

      interface bus       = _ifc;
      interface addrMatch = ifc.addrMatch;

   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance ToAxi4L#(Axi3WrMaster#(`TLM_PRM), Axi4LWrMaster#(`TLM_PRM));
   module toAxi4L#(Axi3WrMaster#(`TLM_PRM) ifc) (Axi4LWrMaster#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      FIFOF#(TLMId#(`TLM_PRM)) id_fifo   <- mkSizedFIFOF(4);
      Wire#(TLMId#(`TLM_PRM)) id_wire   <- mkDWire(unpack(0));

      Wire#(Bool)             aw_ready  <- mkBypassWire;
      Wire#(Bool)             b_valid   <- mkBypassWire;

      rule every;
	 id_wire <= id_fifo.first;
      endrule

      rule set_rid;
	 ifc.bID(id_wire);
      endrule

      rule enq_id (ifc.awVALID && aw_ready);
	 id_fifo.enq(ifc.awID);
      endrule

      rule deq_id (ifc.bREADY && b_valid);
	 id_fifo.deq;
      endrule

      return (interface Axi4LWrMaster
		 // Addr Outputs
		 method awADDR  = ifc.awADDR;
		 method awPROT  = ifc.awPROT;
		 method awVALID = ifc.awVALID;

		 // Addr Inputs
	 	 method Action awREADY (value);
		    ifc.awREADY(value && id_fifo.notFull);
		    aw_ready <= value;
		 endmethod

		 // Data Outputs
		 method wDATA   = ifc.wDATA;
		 method wSTRB   = ifc.wSTRB;
		 method wVALID  = ifc.wVALID;

		 // Data Inputs
		 method wREADY = ifc.wREADY;

		 // Response Outputs
		 method bREADY = ifc.bREADY;

		 // Response Inputs
		 method Action bRESP (value);
		    ifc.bRESP(value);
	 	    ifc.bUSER(0);
		 endmethod
		 method Action bVALID (value);
		    ifc.bVALID(value);
		    b_valid <= value;
		 endmethod
	      endinterface);
   endmodule
endinstance

instance ToAxi4L#(Axi3RdMaster#(`TLM_PRM), Axi4LRdMaster#(`TLM_PRM));
   module toAxi4L#(Axi3RdMaster#(`TLM_PRM) ifc) (Axi4LRdMaster#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      FIFOF#(TLMId#(`TLM_PRM)) id_fifo   <- mkSizedFIFOF(4);
      Wire#(TLMId#(`TLM_PRM)) id_wire    <- mkDWire(unpack(0));

      Wire#(Bool)             ar_ready  <- mkBypassWire;
      Wire#(Bool)             r_valid   <- mkBypassWire;

      rule every;
	 id_wire <= id_fifo.first;
      endrule

      rule set_rid;
	 ifc.rID(id_wire);
      endrule

      rule enq_id (ifc.arVALID && ar_ready);
	 id_fifo.enq(ifc.arID);
      endrule

      rule deq_id (ifc.rREADY && r_valid);
	 id_fifo.deq;
      endrule

      return (interface Axi4LRdMaster
		 // Addr Outputs
		 method arADDR  = ifc.arADDR;
		 method arPROT  = ifc.arPROT;
		 method arVALID = ifc.arVALID;

		 // Addr Inputs
		 method Action arREADY (value);
		    ifc.arREADY(value && id_fifo.notFull);
		    ar_ready <= value;
		 endmethod

		 // Response Outputs
		 method rREADY = ifc.rREADY;

		 // Response Inputs
		 method Action rDATA (value);
		    ifc.rDATA(value);
	 	    ifc.rUSER(0);
	 	    ifc.rLAST(True);
		 endmethod
		 method rRESP  = ifc.rRESP;
	 	 method Action rVALID (value);
		    ifc.rVALID(value);
		    r_valid <= value;
		 endmethod
	      endinterface);
   endmodule
endinstance

instance ToAxi4L#(Axi3WrSlave#(`TLM_PRM), Axi4LWrSlave#(`TLM_PRM));
   module toAxi4L#(Axi3WrSlave#(`TLM_PRM) ifc) (Axi4LWrSlave#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4LWrSlave
		 // Addr Inputs;
		 method Action awADDR (value);
		    ifc.awADDR(value);
	 	    ifc.awID(0);
	 	    ifc.awUSER(0);
	 	    ifc.awLEN(0);
	 	    ifc.awBURST(INCR);
	 	    ifc.awLOCK(NORMAL);
	 	    ifc.awCACHE(unpack(0));
		    ifc.awSIZE(getMaxBSize(valueOf(data_size)));
		 endmethod
		 method awPROT  = ifc.awPROT;
		 method awVALID = ifc.awVALID;

		 // Addr Outputs
		 method awREADY = ifc.awREADY;

		 // Data Inputs
		 method Action wDATA (value);
		    ifc.wDATA(value);
	 	    ifc.wID(0);
	 	    ifc.wUSER(0);
	 	    ifc.wLAST(True);
		 endmethod
		 method wSTRB   = ifc.wSTRB;
		 method wVALID  = ifc.wVALID;

		 // Data Outputs;
		 method wREADY  = ifc.wREADY;

		 // Response Inputs
		 method bREADY  = ifc.bREADY;

		 // Response Outputs
		 method bRESP   = ifc.bRESP;
		 method bVALID  = ifc.bVALID;
	      endinterface);
   endmodule
endinstance

instance ToAxi4L#(Axi3RdSlave#(`TLM_PRM), Axi4LRdSlave#(`TLM_PRM));
   module toAxi4L#(Axi3RdSlave#(`TLM_PRM) ifc) (Axi4LRdSlave#(`TLM_PRM));

      function Action nop(a ignore);
	 return noAction;
      endfunction

      return (interface Axi4LRdSlave
		 // Addr Inputs;
		 method Action arADDR (value);
		    ifc.arADDR(value);
	 	    ifc.arID(0);
	 	    ifc.arUSER(0);
	 	    ifc.arLEN(0);
	 	    ifc.arBURST(INCR);
	 	    ifc.arLOCK(NORMAL);
	 	    ifc.arCACHE(unpack(0));
	 	    ifc.arSIZE(getMaxBSize(valueOf(data_size)));
		 endmethod
		 method arPROT  = ifc.arPROT;
	 	 method arVALID = ifc.arVALID;

		 // Addr Outputs
		 method arREADY = ifc.arREADY;

		 // Response Inputs
		 method rREADY = ifc.rREADY;

		 // Response Outputs
	 	 method rDATA  = ifc.rDATA;
		 method rRESP  = ifc.rRESP;
	 	 method rVALID = ifc.rVALID;
	      endinterface);
   endmodule
endinstance

instance ToAxi4L#(Axi3WrFabricMaster#(`TLM_PRM), Axi4LWrFabricMaster#(`TLM_PRM));
   module toAxi4L#(Axi3WrFabricMaster#(`TLM_PRM) ifc) (Axi4LWrFabricMaster#(`TLM_PRM));

      let _ifc <- toAxi4L(ifc.bus);

      interface bus = _ifc;

   endmodule
endinstance

instance ToAxi4L#(Axi3RdFabricMaster#(`TLM_PRM), Axi4LRdFabricMaster#(`TLM_PRM));
   module toAxi4L#(Axi3RdFabricMaster#(`TLM_PRM) ifc) (Axi4LRdFabricMaster#(`TLM_PRM));

      let _ifc <- toAxi4L(ifc.bus);

      interface bus = _ifc;

   endmodule
endinstance

instance ToAxi4L#(Axi3WrFabricSlave#(`TLM_PRM), Axi4LWrFabricSlave#(`TLM_PRM));
   module toAxi4L#(Axi3WrFabricSlave#(`TLM_PRM) ifc) (Axi4LWrFabricSlave#(`TLM_PRM));

      let _ifc <- toAxi4L(ifc.bus);

      interface bus       = _ifc;
      interface addrMatch = ifc.addrMatch;

   endmodule
endinstance

instance ToAxi4L#(Axi3RdFabricSlave#(`TLM_PRM), Axi4LRdFabricSlave#(`TLM_PRM));
   module toAxi4L#(Axi3RdFabricSlave#(`TLM_PRM) ifc) (Axi4LRdFabricSlave#(`TLM_PRM));

      let _ifc <- toAxi4L(ifc.bus);

      interface bus       = _ifc;
      interface addrMatch = ifc.addrMatch;

   endmodule
endinstance

endpackage