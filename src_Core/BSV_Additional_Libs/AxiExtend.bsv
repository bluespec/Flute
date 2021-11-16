// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package AxiExtend;

import AxiDefines::*;
import TLM3::*;
import FIFOF::*;
import CBus::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass Extendable#(type ifc_t, type ifc_ext_t);
   module extendIds#(parameter UInt#(32) prm , ifc_t ifc) (ifc_ext_t);
endtypeclass

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Extendable#(AxiRdFabricSlave#(`TLM_PRM), AxiRdFabricSlave#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) max_flight,
		  AxiRdFabricSlave#(`TLM_PRM) ifc) (AxiRdFabricSlave#(`TLM_PRM_ID_EXT));

      let ifc_ext <- extendIds(max_flight, ifc.bus);

      interface AxiRdSlave bus = ifc_ext;
      method addrMatch = ifc.addrMatch;
   endmodule
endinstance

instance Extendable#(AxiRdSlave#(`TLM_PRM), AxiRdSlave#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) max_flight,
		     AxiRdSlave#(`TLM_PRM) ifc) (AxiRdSlave#(`TLM_PRM_ID_EXT));

      FIFOF#(Bit#(x_size)) x_fifo       <- mkSafeDepthParamFIFOF(max_flight);
      Wire#(Bit#(x_size))  x_wire       <- mkBypassWire;
      Wire#(Bool)          arVALID_wire <- mkBypassWire;
      Wire#(Bool)          rREADY_wire  <- mkBypassWire;
      Wire#(Bit#(x_size))  x_first      <- mkDWire(0);

      rule set_first;
	 x_first <= x_fifo.first;
      endrule

      rule enq_extra (ifc.arREADY && arVALID_wire);
	 x_fifo.enq(x_wire);
      endrule

      rule deq_extra (ifc.rVALID && rREADY_wire && ifc.rLAST);
	 x_fifo.deq;
      endrule

      // Address Inputs
      method Action arID (AxiId#(`TLM_PRM_ID_EXT) value);
	 x_wire <= truncateLSB(value);
	 ifc.arID(truncate(value));
      endmethod
      method arADDR  = ifc.arADDR;
      method arLEN   = ifc.arLEN;
      method arSIZE  = ifc.arSIZE;
      method arBURST = ifc.arBURST;
      method arLOCK  = ifc.arLOCK;
      method arCACHE = ifc.arCACHE;
      method arPROT  = ifc.arPROT;

      method Action arVALID (Bool value);
	 arVALID_wire <= value;
	 ifc.arVALID(value);
      endmethod

      // Address Outputs
      method arREADY = ifc.arREADY && x_fifo.notFull;

      // Response Inputs
      method Action rREADY(Bool value);
	 rREADY_wire <= value;
	 ifc.rREADY(value);
      endmethod

      // Response Outputs
      method TLMId#(`TLM_PRM_ID_EXT) rID();
	 return{x_first, ifc.rID};
      endmethod
      method rDATA  = ifc.rDATA;
      method rRESP  = ifc.rRESP;
      method rLAST  = ifc.rLAST;
      method rVALID = ifc.rVALID && x_fifo.notEmpty;

   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Extendable#(AxiRdFabricMaster#(`TLM_PRM), AxiRdFabricMaster#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) num,
		  AxiRdFabricMaster#(`TLM_PRM) ifc) (AxiRdFabricMaster#(`TLM_PRM_ID_EXT));

      let ifc_ext <- extendIds(num, ifc.bus);

      interface AxiRdMaster bus = ifc_ext;
   endmodule
endinstance

instance Extendable#(AxiRdMaster#(`TLM_PRM), AxiRdMaster#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) num,
		  AxiRdMaster#(`TLM_PRM) ifc) (AxiRdMaster#(`TLM_PRM_ID_EXT));

      Bit#(x_size) prefix = truncateNP(pack(num));

      // Address Outputs
      method AxiId#(`TLM_PRM_ID_EXT) arID;
	 return unpack({prefix, pack(ifc.arID)});
      endmethod
      method arADDR  = ifc.arADDR;
      method arLEN   = ifc.arLEN;
      method arSIZE  = ifc.arSIZE;
      method arBURST = ifc.arBURST;
      method arLOCK  = ifc.arLOCK;
      method arCACHE = ifc.arCACHE;
      method arPROT  = ifc.arPROT;
      method arVALID = ifc.arVALID;

      // Address Inputs;
      method arREADY = ifc.arREADY;

      // Response Outputs
      method rREADY = ifc.rREADY;

      // Response Inputs
      method Action rID (AxiId#(`TLM_PRM_ID_EXT) value);
	 ifc.rID(truncate(value));
      endmethod
      method rDATA = ifc.rDATA;
      method rRESP = ifc.rRESP;
      method rLAST = ifc.rLAST;
      method rVALID = ifc.rVALID;
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Extendable#(AxiWrFabricSlave#(`TLM_PRM), AxiWrFabricSlave#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) max_flight,
		  AxiWrFabricSlave#(`TLM_PRM) ifc) (AxiWrFabricSlave#(`TLM_PRM_ID_EXT));

      let ifc_ext <- extendIds(max_flight, ifc.bus);

      interface AxiWrSlave bus = ifc_ext;
      method addrMatch = ifc.addrMatch;
   endmodule
endinstance

instance Extendable#(AxiWrSlave#(`TLM_PRM), AxiWrSlave#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) max_flight,
		  AxiWrSlave#(`TLM_PRM) ifc) (AxiWrSlave#(`TLM_PRM_ID_EXT));

      FIFOF#(Bit#(x_size)) x_fifo       <- mkSafeDepthParamFIFOF(max_flight);
      Wire#(Bit#(x_size))  x_wire       <- mkBypassWire;
      Wire#(Bool)          awVALID_wire <- mkBypassWire;
      Wire#(Bool)          bREADY_wire  <- mkBypassWire;
      Wire#(Bit#(x_size))  x_first      <- mkDWire(0);

      rule set_first;
	 x_first <= x_fifo.first;
      endrule

      rule enq_extra (ifc.awREADY && awVALID_wire);
	 x_fifo.enq(x_wire);
      endrule

      rule deq_extra (ifc.bVALID && bREADY_wire);
	 x_fifo.deq;
      endrule

      // Address Inputs
      method Action awID (AxiId#(`TLM_PRM_ID_EXT) value);
	 x_wire <= truncateLSB(value);
	 ifc.awID(truncate(value));
      endmethod
      method awADDR  = ifc.awADDR;
      method awLEN   = ifc.awLEN;
      method awSIZE  = ifc.awSIZE;
      method awBURST = ifc.awBURST;
      method awLOCK  = ifc.awLOCK;
      method awCACHE = ifc.awCACHE;
      method awPROT  = ifc.awPROT;

      method Action awVALID (Bool value);
	 awVALID_wire <= value;
	 ifc.awVALID(value);
      endmethod

      // Address Outputs
      method awREADY = ifc.awREADY && x_fifo.notFull;

      // Data inputs
      method Action wID (AxiId#(`TLM_PRM_ID_EXT) value);
	 ifc.wID(truncate(value));
      endmethod
      method wDATA = ifc.wDATA;
      method wSTRB = ifc.wSTRB;
      method wLAST = ifc.wLAST;
      method wVALID = ifc.wVALID;

      // Data Outputs
      method wREADY = ifc.wREADY;

      // Response Inputs
      method Action bREADY(Bool value);
	 bREADY_wire <= value;
	 ifc.bREADY(value);
      endmethod

      // Response Outputs
      method TLMId#(`TLM_PRM_ID_EXT) bID();
	 return{x_first, ifc.bID};
      endmethod
      method bRESP  = ifc.bRESP;
      method bVALID = ifc.bVALID && x_fifo.notEmpty;

   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Extendable#(AxiWrFabricMaster#(`TLM_PRM), AxiWrFabricMaster#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) num,
		  AxiWrFabricMaster#(`TLM_PRM) ifc) (AxiWrFabricMaster#(`TLM_PRM_ID_EXT));

      let ifc_ext <- extendIds(num, ifc.bus);

      interface AxiWrMaster bus = ifc_ext;
   endmodule
endinstance

instance Extendable#(AxiWrMaster#(`TLM_PRM), AxiWrMaster#(`TLM_PRM_ID_EXT))
   provisos(Add#(id_size, x_size, id_size_ext));
   module extendIds#(parameter UInt#(32) num,
		  AxiWrMaster#(`TLM_PRM) ifc) (AxiWrMaster#(`TLM_PRM_ID_EXT));

      Bit#(x_size) prefix = truncateNP(pack(num));

      // Address Outputs
      method AxiId#(`TLM_PRM_ID_EXT) awID;
	 return unpack({prefix, pack(ifc.awID)});
      endmethod
      method awADDR  = ifc.awADDR;
      method awLEN   = ifc.awLEN;
      method awSIZE  = ifc.awSIZE;
      method awBURST = ifc.awBURST;
      method awLOCK  = ifc.awLOCK;
      method awCACHE = ifc.awCACHE;
      method awPROT  = ifc.awPROT;
      method awVALID = ifc.awVALID;

      // Address Inputs;
      method awREADY = ifc.awREADY;

      // Data Outputs
      method AxiId#(`TLM_PRM_ID_EXT) wID;
	 return unpack({prefix, pack(ifc.wID)});
      endmethod
      method wDATA  = ifc.wDATA;
      method wSTRB  = ifc.wSTRB;
      method wLAST  = ifc.wLAST;
      method wVALID = ifc.wVALID;

      // Data Inputs
      method wREADY = ifc.wREADY;


      // Response Outputs
      method bREADY = ifc.bREADY;

      // Response Inputs
      method Action bID (AxiId#(`TLM_PRM_ID_EXT) value);
	 ifc.bID(truncate(value));
      endmethod
      method bRESP  = ifc.bRESP;
      method bVALID = ifc.bVALID;


   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

endpackage