// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package Axi4LPC;

import Axi4LDefines::*;
import AxiDefines::*;
import DefaultValue::*;
import TLM3::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
/// Read Bus Checkers
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LRdMasterPC#(Axi4LPCParams params, Axi4LRdFabricMaster#(`TLM_PRM) master) (Axi4LRdFabricMaster#(`TLM_PRM));


   Wire#(Bool)               arREADY_wire <- mkBypassWire;

   Wire#(Bool)               rVALID_wire  <- mkBypassWire;
   Wire#(AxiData#(`TLM_PRM)) rDATA_wire   <- mkBypassWire;
   Wire#(AxiResp)            rRESP_wire   <- mkBypassWire;

   if (genVerilog)
      begin

	 Axi4LPC_Ifc#(`TLM_PRM) checker <- mkAxi4LPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_prot (unpack(0));
	    checker.aw_addr (unpack(0));
	    checker.aw_valid(unpack(0));
	    checker.aw_ready(unpack(0));

	    checker.w_data  (unpack(0));
	    checker.w_strb  (unpack(0));
	    checker.w_valid (unpack(0));
	    checker.w_ready (unpack(0));

	    checker.b_resp  (unpack(0));
	    checker.b_valid (unpack(0));
	    checker.b_ready (unpack(0));

	    checker.ar_prot (master.bus.arPROT);
	    checker.ar_addr (master.bus.arADDR);
	    checker.ar_valid(master.bus.arVALID);
	    checker.ar_ready(arREADY_wire);

	    checker.r_data  (rDATA_wire);
	    checker.r_resp  (rRESP_wire);
	    checker.r_valid (rVALID_wire);
	    checker.r_ready (master.bus.rREADY);

	 endrule
      end

   interface Axi4LRdMaster bus;

      // Address Outputs
      method arADDR  = master.bus.arADDR;
      method arPROT  = master.bus.arPROT;
      method arVALID = master.bus.arVALID;

      // Address Input
      method Action arREADY(Bool value);
	   master.bus.arREADY(value);
	   arREADY_wire <= value;
      endmethod

      method Action rRESP(value);
	 master.bus.rRESP(value);
	 rRESP_wire <= value;
      endmethod

      method Action rVALID(value);
	    master.bus.rVALID(value);
	    rVALID_wire <= value;
      endmethod

      method Action rDATA(value);
	    master.bus.rDATA(value);
	    rDATA_wire <= value;
      endmethod

      // Response Output
      method rREADY = master.bus.rREADY;
endinterface

endmodule


module mkAxi4LRdSlavePC#(Axi4LPCParams params, Axi4LRdFabricSlave#(`TLM_PRM) slave) (Axi4LRdFabricSlave#(`TLM_PRM));

   Wire#(AxiAddr#(`TLM_PRM)) arADDR_wire   <- mkBypassWire;
   Wire#(AxiProt)            arPROT_wire  <- mkBypassWire;
   Wire#(Bool)               arVALID_wire <- mkBypassWire;
   Wire#(Bool)               rREADY_wire  <- mkBypassWire;

   if (genVerilog) begin

	 Axi4LPC_Ifc#(`TLM_PRM) checker <- mkAxi4LPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_prot (unpack(0));
	    checker.aw_addr (unpack(0));
	    checker.aw_valid(unpack(0));

	    checker.aw_ready(unpack(0));

	    checker.w_data  (unpack(0));
	    checker.w_strb  (unpack(0));
	    checker.w_valid (unpack(0));
	    checker.w_ready (unpack(0));

	    checker.b_resp  (unpack(0));
	    checker.b_valid (unpack(0));
	    checker.b_ready (unpack(0));

	    checker.ar_prot (arPROT_wire);
	    checker.ar_addr (arADDR_wire);
	    checker.ar_valid(arVALID_wire);
	    checker.ar_ready(slave.bus.arREADY);

	    checker.r_data  (slave.bus.rDATA);
	    checker.r_resp  (slave.bus.rRESP);
	    checker.r_valid (slave.bus.rVALID);
	    checker.r_ready (rREADY_wire);

	 endrule
      end

   interface Axi4LRdSlave bus;

      // Address Inputs
      method Action arADDR (value);
	 slave.bus.arADDR(value);
	 arADDR_wire <= value;
      endmethod
      method Action arPROT (value);
	 slave.bus.arPROT(value);
	 arPROT_wire <= value;
      endmethod
      method Action arVALID (value);
	 slave.bus.arVALID(value);
	 arVALID_wire <= value;
      endmethod

      // Address Outputs
      method arREADY = slave.bus.arREADY;

      // Response Inputs
      method Action rREADY (value);
	    slave.bus.rREADY(value);
	    rREADY_wire <= value;
      endmethod

      // Response Outputs
      method rRESP  = slave.bus.rRESP;
      method rDATA  = slave.bus.rDATA;
      method rVALID = slave.bus.rVALID;

   endinterface

   method addrMatch = slave.addrMatch;

endmodule

////////////////////////////////////////////////////////////////////////////////
/// Write Bus Checkers
////////////////////////////////////////////////////////////////////////////////

module mkAxi4LWrMasterPC#(Axi4LPCParams params, Axi4LWrFabricMaster#(`TLM_PRM) master) (Axi4LWrFabricMaster#(`TLM_PRM));


   Wire#(Bool)               awREADY_wire <- mkBypassWire;
   Wire#(Bool)               wREADY_wire  <- mkBypassWire;

   Wire#(AxiResp)            bRESP_wire   <- mkBypassWire;
   Wire#(Bool)               bVALID_wire  <- mkBypassWire;


   if (genVerilog)
      begin

	 Axi4LPC_Ifc#(`TLM_PRM) checker <- mkAxi4LPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_prot(master.bus.awPROT);
	    checker.aw_addr(master.bus.awADDR);
	    checker.aw_valid(master.bus.awVALID);
	    checker.aw_ready(awREADY_wire);

	    checker.w_data(master.bus.wDATA);
	    checker.w_strb(master.bus.wSTRB);
	    checker.w_valid(master.bus.wVALID);
	    checker.w_ready(wREADY_wire);

	    checker.b_resp(bRESP_wire);
	    checker.b_valid(bVALID_wire);
	    checker.b_ready(master.bus.bREADY);

	    checker.ar_prot(unpack(0));
	    checker.ar_addr(unpack(0));
	    checker.ar_valid(unpack(0));
	    checker.ar_ready(unpack(0));

	    checker.r_data(unpack(0));
	    checker.r_resp(unpack(0));
	    checker.r_valid(unpack(0));
	    checker.r_ready(unpack(0));

	 endrule
      end

   interface Axi4LWrMaster bus;

      // Address Outputs
      method awADDR  = master.bus.awADDR;
      method awPROT  = master.bus.awPROT;
      method awVALID = master.bus.awVALID;

      method Action awREADY(Bool value);
	 master.bus.awREADY(value);
	 awREADY_wire <= value;
      endmethod


      // Data Outputs
      method wDATA  = master.bus.wDATA;
      method wSTRB  = master.bus.wSTRB;
      method wVALID = master.bus.wVALID;

      // Data Inputs
      method Action wREADY(Bool value);
	 master.bus.wREADY(value);
	 wREADY_wire <= value;
      endmethod

      method bREADY = master.bus.bREADY;

      // Response Inputs
      method Action bRESP(value);
	 master.bus.bRESP(value);
	 bRESP_wire <= value;
      endmethod
      method Action bVALID(value);
	 master.bus.bVALID(value);
	 bVALID_wire <= value;
      endmethod
endinterface

endmodule


module mkAxi4LWrSlavePC#(Axi4LPCParams params, Axi4LWrFabricSlave#(`TLM_PRM) slave) (Axi4LWrFabricSlave#(`TLM_PRM));

   Wire#(AxiAddr#(`TLM_PRM))   awADDR_wire   <- mkBypassWire;
   Wire#(AxiProt)              awPROT_wire  <- mkBypassWire;
   Wire#(Bool)                 awVALID_wire <- mkBypassWire;
   Wire#(AxiData#(`TLM_PRM))   wDATA_wire   <- mkBypassWire;
   Wire#(AxiByteEn#(`TLM_PRM)) wSTRB_wire   <- mkBypassWire;
   Wire#(Bool)                 wVALID_wire  <- mkBypassWire;
   Wire#(Bool)                 bREADY_wire  <- mkBypassWire;


   if (genVerilog)
      begin

	 Axi4LPC_Ifc#(`TLM_PRM) checker <- mkAxi4LPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_prot(awPROT_wire);
	    checker.aw_addr(awADDR_wire);
	    checker.aw_valid(awVALID_wire);

	    checker.aw_ready(slave.bus.awREADY);

	    checker.w_data(wDATA_wire);
	    checker.w_strb(wSTRB_wire);
	    checker.w_valid(wVALID_wire);
	    checker.w_ready(slave.bus.wREADY);

	    checker.b_resp(slave.bus.bRESP);
	    checker.b_valid(slave.bus.bVALID);
	    checker.b_ready(bREADY_wire);

	    checker.ar_prot(unpack(0));
	    checker.ar_addr(unpack(0));
	    checker.ar_valid(unpack(0));
	    checker.ar_ready(unpack(0));

	    checker.r_data(unpack(0));
	    checker.r_resp(unpack(0));
	    checker.r_valid(unpack(0));
	    checker.r_ready(unpack(0));

	 endrule
      end

   interface Axi4LWrSlave bus;

      // Address Inputs
      method Action awADDR (value);
	 slave.bus.awADDR(value);
	 awADDR_wire <= value;
      endmethod
      method Action awPROT (value);
	 slave.bus.awPROT(value);
	 awPROT_wire <= value;
      endmethod
      method Action awVALID (value);
	 slave.bus.awVALID(value);
	 awVALID_wire <= value;
      endmethod

      // Address Outputs
      method awREADY = slave.bus.awREADY;

      // Data Inputs
      method Action wDATA (value);
	 slave.bus.wDATA(value);
	 wDATA_wire <= value;
      endmethod
      method Action wSTRB (value);
	 slave.bus.wSTRB(value);
	 wSTRB_wire <= value;
      endmethod
      method Action wVALID (value);
	 slave.bus.wVALID(value);
	 wVALID_wire <= value;
      endmethod

      // Data Ouptuts
      method wREADY = slave.bus.wREADY;

      // Response Inputs
      method Action bREADY (value);
	 slave.bus.bREADY(value);
	 bREADY_wire <= value;
      endmethod

      // Response Outputs
      method bRESP = slave.bus.bRESP;
      method bVALID = slave.bus.bVALID;

   endinterface

   method addrMatch = slave.addrMatch;

endmodule


////////////////////////////////////////////////////////////////////////////////
/// Read Bus Checkers
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface Axi4LPC_Ifc#(`TLM_PRM_DCL);
   method Action   aw_prot   (AxiProt value);
   method Action   aw_addr   (AxiAddr#(`TLM_PRM) value);
   method Action   aw_valid  (Bool value);
   method Action   aw_ready  (Bool value);

   method Action   w_data    (AxiData#(`TLM_PRM) value);
   method Action   w_strb    (AxiByteEn#(`TLM_PRM) value);
   method Action   w_valid   (Bool value);
   method Action   w_ready   (Bool value);

   method Action   b_resp    (AxiResp value);
   method Action   b_valid   (Bool value);
   method Action   b_ready   (Bool value);

   method Action   ar_prot   (AxiProt value);
   method Action   ar_addr   (AxiAddr#(`TLM_PRM) value);
   method Action   ar_valid  (Bool value);
   method Action   ar_ready  (Bool value);

   method Action   r_data    (AxiData#(`TLM_PRM) value);
   method Action   r_resp    (AxiResp value);
   method Action   r_valid   (Bool value);
   method Action   r_ready   (Bool value);
endinterface


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef struct {Int#(32) maxwaits;
		} Axi4LPCParams deriving (Eq, Bits, Bounded);


instance DefaultValue #(Axi4LPCParams);
   function defaultValue ();
      Axi4LPCParams params;
      params.maxwaits    = 256;
      return params;
   endfunction
endinstance

import "BVI" Axi4LitePC =
module mkAxi4LPC#(Axi4LPCParams params) (Axi4LPC_Ifc#(`TLM_PRM));

   parameter DATA_WIDTH  = valueOf(data_size);
   parameter MAXWAITS    = pack(params.maxwaits);

   // a few extras for address width
   parameter ADDR_WIDTH   = valueOf(addr_size);

   default_clock clk( ACLK);
   default_reset rst(ARESETn);

   method    aw_prot   (AWPROT )enable((*inhigh*)IGNORE06);
   method    aw_addr   (AWADDR )enable((*inhigh*)IGNORE07);
   method    aw_valid  (AWVALID)enable((*inhigh*)IGNORE08);
   method    aw_ready  (AWREADY)enable((*inhigh*)IGNORE09);
   method    w_data    (WDATA  )enable((*inhigh*)IGNORE11);
   method    w_strb    (WSTRB  )enable((*inhigh*)IGNORE12);
   method    w_valid   (WVALID )enable((*inhigh*)IGNORE14);
   method    w_ready   (WREADY )enable((*inhigh*)IGNORE15);

   method    b_resp    (BRESP  )enable((*inhigh*)IGNORE17);
   method    b_valid   (BVALID )enable((*inhigh*)IGNORE18);
   method    b_ready   (BREADY )enable((*inhigh*)IGNORE19);

   method    ar_prot   (ARPROT )enable((*inhigh*)IGNORE27);
   method    ar_addr   (ARADDR )enable((*inhigh*)IGNORE28);
   method    ar_valid  (ARVALID)enable((*inhigh*)IGNORE29);
   method    ar_ready  (ARREADY)enable((*inhigh*)IGNORE30);

   method    r_data    (RDATA  )enable((*inhigh*)IGNORE32);
   method    r_resp    (RRESP  )enable((*inhigh*)IGNORE33);
   method    r_valid   (RVALID )enable((*inhigh*)IGNORE35);
   method    r_ready   (RREADY )enable((*inhigh*)IGNORE36);

      schedule (aw_valid, w_valid, aw_prot, aw_addr, aw_ready, w_data, w_strb, w_ready, b_resp, b_valid, b_ready, ar_prot, ar_addr, ar_valid, ar_ready, r_data, r_resp, r_valid, r_ready) CF (aw_valid, w_valid, aw_prot, aw_addr, aw_ready, w_data, w_strb, w_ready, b_resp, b_valid, b_ready, ar_prot, ar_addr, ar_valid, ar_ready, r_data, r_resp, r_valid, r_ready);

endmodule

endpackage



