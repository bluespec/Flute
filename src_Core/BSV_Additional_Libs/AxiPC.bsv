// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package AxiPC;

import AxiDefines::*;
import DefaultValue::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
/// Read Bus Checkers
////////////////////////////////////////////////////////////////////////////////

module mkAxiRdMasterPC#(AxiPCParams params, AxiRdFabricMaster#(`TLM_PRM) master) (AxiRdFabricMaster#(`TLM_PRM));


   Wire#(Bool)               arREADY_wire <- mkBypassWire;

   Wire#(Bool)               rVALID_wire  <- mkBypassWire;
   Wire#(Bool)               rLAST_wire   <- mkBypassWire;
   Wire#(AxiId#(`TLM_PRM))   rID_wire     <- mkBypassWire;
   Wire#(AxiData#(`TLM_PRM)) rDATA_wire   <- mkBypassWire;
   Wire#(AxiResp)            rRESP_wire   <- mkBypassWire;


   if (genVerilog)
      begin

	 AxiPC_Ifc#(`TLM_PRM) checker <- mkAxiPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_id   (unpack(0));
	    checker.aw_len  (unpack(0));
	    checker.aw_size (unpack(0));
	    checker.aw_burst(unpack(0));
	    checker.aw_lock (unpack(0));
	    checker.aw_cache(unpack(0));
	    checker.aw_prot (unpack(0));
	    checker.aw_addr (unpack(0));
	    checker.aw_valid(unpack(0));
	    checker.aw_ready(unpack(0));

	    checker.w_id    (unpack(0));
	    checker.w_data  (unpack(0));
	    checker.w_strb  (unpack(0));
	    checker.w_last  (unpack(0));
	    checker.w_valid (unpack(0));
	    checker.w_ready (unpack(0));

	    checker.b_id    (unpack(0));
	    checker.b_resp  (unpack(0));
	    checker.b_valid (unpack(0));
	    checker.b_ready (unpack(0));

	    checker.ar_id   (master.bus.arID);
	    checker.ar_len  (master.bus.arLEN);
	    checker.ar_size (master.bus.arSIZE);
	    checker.ar_burst(master.bus.arBURST);
	    checker.ar_lock (master.bus.arLOCK);
	    checker.ar_cache(master.bus.arCACHE);
	    checker.ar_prot (master.bus.arPROT);
	    checker.ar_addr (master.bus.arADDR);
	    checker.ar_valid(master.bus.arVALID);
	    checker.ar_ready(arREADY_wire);

	    checker.r_id    (rID_wire);
	    checker.r_data  (rDATA_wire);
	    checker.r_resp  (rRESP_wire);
	    checker.r_last  (rLAST_wire);
	    checker.r_valid (rVALID_wire);
	    checker.r_ready (master.bus.rREADY);

	    checker.c_active(unpack(0));
	    checker.c_sysreq(unpack(0));
	    checker.c_sysack(unpack(0));

	 endrule
      end

   interface AxiRdMaster bus;

      // Address Outputs
      method arID    = master.bus.arID;
      method arADDR  = master.bus.arADDR;
      method arLEN   = master.bus.arLEN;
      method arSIZE  = master.bus.arSIZE;
      method arBURST = master.bus.arBURST;
      method arLOCK  = master.bus.arLOCK;
      method arCACHE = master.bus.arCACHE;
      method arPROT  = master.bus.arPROT;
      method arVALID = master.bus.arVALID;

      // Address Input
      method Action arREADY(Bool value);
	   master.bus.arREADY(value);
	   arREADY_wire <= value;
      endmethod


      // Response Inputs
      method Action rLAST(Bool value);
        master.bus.rLAST(value);
	    rLAST_wire <= value;
      endmethod

      method Action rID(value);
	    master.bus.rID(value);
	    rID_wire <= value;
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


module mkAxiRdSlavePC#(AxiPCParams params, AxiRdFabricSlave#(`TLM_PRM) slave) (AxiRdFabricSlave#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM))   arID_wire    <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_PRM)) arADDR_wire  <- mkBypassWire;
   Wire#(AxiLen)             arLEN_wire   <- mkBypassWire;
   Wire#(AxiSize)            arSIZE_wire  <- mkBypassWire;
   Wire#(AxiBurst)           arBURST_wire <- mkBypassWire;
   Wire#(AxiLock)            arLOCK_wire  <- mkBypassWire;
   Wire#(AxiCache)           arCACHE_wire <- mkBypassWire;
   Wire#(AxiProt)            arPROT_wire  <- mkBypassWire;
   Wire#(Bool)               arVALID_wire <- mkBypassWire;
   Wire#(Bool)               rREADY_wire  <- mkBypassWire;

   if (genVerilog) begin

	 AxiPC_Ifc#(`TLM_PRM) checker <- mkAxiPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_id   (unpack(0));
	    checker.aw_len  (unpack(0));
	    checker.aw_size (unpack(0));
	    checker.aw_burst(unpack(0));
	    checker.aw_lock (unpack(0));
	    checker.aw_cache(unpack(0));
	    checker.aw_prot (unpack(0));
	    checker.aw_addr (unpack(0));
	    checker.aw_valid(unpack(0));

	    checker.aw_ready(unpack(0));

	    checker.w_id    (unpack(0));
	    checker.w_data  (unpack(0));
	    checker.w_strb  (unpack(0));
	    checker.w_last  (unpack(0));
	    checker.w_valid (unpack(0));
	    checker.w_ready (unpack(0));

	    checker.b_id    (unpack(0));
	    checker.b_resp  (unpack(0));
	    checker.b_valid (unpack(0));
	    checker.b_ready (unpack(0));

	    checker.ar_id   (arID_wire);
	    checker.ar_len  (arLEN_wire);
	    checker.ar_size (arSIZE_wire);
	    checker.ar_burst(arBURST_wire);
	    checker.ar_lock (arLOCK_wire);
	    checker.ar_cache(arCACHE_wire);
	    checker.ar_prot (arPROT_wire);
	    checker.ar_addr (arADDR_wire);
	    checker.ar_valid(arVALID_wire);
	    checker.ar_ready(slave.bus.arREADY);

	    checker.r_id    (slave.bus.rID);
	    checker.r_data  (slave.bus.rDATA);
	    checker.r_resp  (slave.bus.rRESP);
	    checker.r_last  (slave.bus.rLAST);
	    checker.r_valid (slave.bus.rVALID);
	    checker.r_ready (rREADY_wire);

	    checker.c_active(unpack(0));
	    checker.c_sysreq(unpack(0));
	    checker.c_sysack(unpack(0));

	 endrule
      end

   interface AxiRdSlave bus;

      // Address Inputs
      method Action arID (value);
	 slave.bus.arID(value);
	 arID_wire <= value;
      endmethod
      method Action arADDR (value);
	 slave.bus.arADDR(value);
	 arADDR_wire <= value;
      endmethod
      method Action arLEN (value);
	 slave.bus.arLEN(value);
	 arLEN_wire <= value;
      endmethod
      method Action arSIZE (value);
	 slave.bus.arSIZE(value);
	 arSIZE_wire <= value;
      endmethod
      method Action arBURST (value);
	 slave.bus.arBURST(value);
	 arBURST_wire <= value;
      endmethod
      method Action arLOCK (value);
	 slave.bus.arLOCK(value);
	 arLOCK_wire <= value;
      endmethod
      method Action arPROT (value);
	 slave.bus.arPROT(value);
	 arPROT_wire <= value;
      endmethod
      method Action arCACHE (value);
	 slave.bus.arCACHE(value);
	 arCACHE_wire <= value;
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
      method rID = slave.bus.rID;
      method rRESP = slave.bus.rRESP;
      method rLAST = slave.bus.rLAST;
      method rDATA = slave.bus.rDATA;
      method rVALID = slave.bus.rVALID;

   endinterface

   method addrMatch = slave.addrMatch;

endmodule

////////////////////////////////////////////////////////////////////////////////
/// Write Bus Checkers
////////////////////////////////////////////////////////////////////////////////

module mkAxiWrMasterPC#(AxiPCParams params, AxiWrFabricMaster#(`TLM_PRM) master) (AxiWrFabricMaster#(`TLM_PRM));


   Wire#(Bool)             awREADY_wire <- mkBypassWire;
   Wire#(Bool)             wREADY_wire  <- mkBypassWire;

   Wire#(AxiId#(`TLM_PRM)) bID_wire     <- mkBypassWire;
   Wire#(AxiResp)          bRESP_wire   <- mkBypassWire;
   Wire#(Bool) bVALID_wire   <- mkBypassWire;


   if (genVerilog)
      begin

	 AxiPC_Ifc#(`TLM_PRM) checker <- mkAxiPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_id(master.bus.awID);
	    checker.aw_len(master.bus.awLEN);
	    checker.aw_size(master.bus.awSIZE);
	    checker.aw_burst(master.bus.awBURST);
	    checker.aw_lock(master.bus.awLOCK);
	    checker.aw_cache(master.bus.awCACHE);
	    checker.aw_prot(master.bus.awPROT);
	    checker.aw_addr(master.bus.awADDR);
	    checker.aw_valid(master.bus.awVALID);
	    checker.aw_ready(awREADY_wire);

	    checker.w_id(master.bus.wID);
	    checker.w_data(master.bus.wDATA);
	    checker.w_strb(master.bus.wSTRB);
	    checker.w_last(master.bus.wLAST);
	    checker.w_valid(master.bus.wVALID);
	    checker.w_ready(wREADY_wire);

	    checker.b_id(bID_wire);
	    checker.b_resp(bRESP_wire);
	    checker.b_valid(bVALID_wire);
	    checker.b_ready(master.bus.bREADY);

	    checker.ar_id(unpack(0));
	    checker.ar_len(unpack(0));
	    checker.ar_size(unpack(0));
	    checker.ar_burst(unpack(0));
	    checker.ar_lock(unpack(0));
	    checker.ar_cache(unpack(0));
	    checker.ar_prot(unpack(0));
	    checker.ar_addr(unpack(0));
	    checker.ar_valid(unpack(0));
	    checker.ar_ready(unpack(0));

	    checker.r_id(unpack(0));
	    checker.r_data(unpack(0));
	    checker.r_resp(unpack(0));
	    checker.r_last(unpack(0));
	    checker.r_valid(unpack(0));
	    checker.r_ready(unpack(0));

	    checker.c_active(unpack(0));
	    checker.c_sysreq(unpack(0));
	    checker.c_sysack(unpack(0));

	 endrule
      end

   interface AxiWrMaster bus;

      // Address Outputs
      method awID    = master.bus.awID;
      method awADDR  = master.bus.awADDR;
      method awLEN   = master.bus.awLEN;
      method awSIZE  = master.bus.awSIZE;
      method awBURST = master.bus.awBURST;
      method awLOCK  = master.bus.awLOCK;
      method awCACHE = master.bus.awCACHE;
      method awPROT  = master.bus.awPROT;
      method awVALID = master.bus.awVALID;

      method Action awREADY(Bool value);
	 master.bus.awREADY(value);
	 awREADY_wire <= value;
      endmethod


      // Data Outputs
      method wID    = master.bus.wID;
      method wDATA  = master.bus.wDATA;
      method wSTRB  = master.bus.wSTRB;
      method wLAST  = master.bus.wLAST;
      method wVALID = master.bus.wVALID;

      // Data Inputs
      method Action wREADY(Bool value);
	 master.bus.wREADY(value);
	 wREADY_wire <= value;
      endmethod

      method bREADY = master.bus.bREADY;

      // Response Inputs
      method Action bID(value);
	 master.bus.bID(value);
	 bID_wire <= value;
      endmethod
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


module mkAxiWrSlavePC#(AxiPCParams params, AxiWrFabricSlave#(`TLM_PRM) slave) (AxiWrFabricSlave#(`TLM_PRM));

   Wire#(AxiId#(`TLM_PRM))   awID_wire    <- mkBypassWire;
   Wire#(AxiAddr#(`TLM_PRM)) awADDR_wire  <- mkBypassWire;
   Wire#(AxiLen)             awLEN_wire   <- mkBypassWire;
   Wire#(AxiSize)            awSIZE_wire  <- mkBypassWire;
   Wire#(AxiBurst)           awBURST_wire <- mkBypassWire;
   Wire#(AxiLock)            awLOCK_wire  <- mkBypassWire;
   Wire#(AxiCache)           awCACHE_wire <- mkBypassWire;
   Wire#(AxiProt)            awPROT_wire  <- mkBypassWire;
   Wire#(Bool)               awVALID_wire <- mkBypassWire;
   Wire#(AxiId#(`TLM_PRM))   wID_wire     <- mkBypassWire;
   Wire#(AxiData#(`TLM_PRM))   wDATA_wire  <- mkBypassWire;
   Wire#(AxiByteEn#(`TLM_PRM)) wSTRB_wire  <- mkBypassWire;
   Wire#(Bool)                 wLAST_wire  <- mkBypassWire;
   Wire#(Bool)                 wVALID_wire <- mkBypassWire;
   Wire#(Bool)                 bREADY_wire <- mkBypassWire;


   if (genVerilog)
      begin

	 AxiPC_Ifc#(`TLM_PRM) checker <- mkAxiPC(params);

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_id(awID_wire);
	    checker.aw_len(awLEN_wire);
	    checker.aw_size(awSIZE_wire);
	    checker.aw_burst(awBURST_wire);
	    checker.aw_lock(awLOCK_wire);
	    checker.aw_cache(awCACHE_wire);
	    checker.aw_prot(awPROT_wire);
	    checker.aw_addr(awADDR_wire);
	    checker.aw_valid(awVALID_wire);

	    checker.aw_ready(slave.bus.awREADY);

	    checker.w_id(wID_wire);
	    checker.w_data(wDATA_wire);
	    checker.w_strb(wSTRB_wire);
	    checker.w_last(wLAST_wire);
	    checker.w_valid(wVALID_wire);
	    checker.w_ready(slave.bus.wREADY);

	    checker.b_id(slave.bus.bID);
	    checker.b_resp(slave.bus.bRESP);
	    checker.b_valid(slave.bus.bVALID);
	    checker.b_ready(bREADY_wire);

	    checker.ar_id(unpack(0));
	    checker.ar_len(unpack(0));
	    checker.ar_size(unpack(0));
	    checker.ar_burst(unpack(0));
	    checker.ar_lock(unpack(0));
	    checker.ar_cache(unpack(0));
	    checker.ar_prot(unpack(0));
	    checker.ar_addr(unpack(0));
	    checker.ar_valid(unpack(0));
	    checker.ar_ready(unpack(0));

	    checker.r_id(unpack(0));
	    checker.r_data(unpack(0));
	    checker.r_resp(unpack(0));
	    checker.r_last(unpack(0));
	    checker.r_valid(unpack(0));
	    checker.r_ready(unpack(0));

	    checker.c_active(unpack(0));
	    checker.c_sysreq(unpack(0));
	    checker.c_sysack(unpack(0));

	 endrule
      end

   interface AxiWrSlave bus;

      // Address Inputs
      method Action awID (value);
	 slave.bus.awID(value);
	 awID_wire <= value;
      endmethod
      method Action awADDR (value);
	 slave.bus.awADDR(value);
	 awADDR_wire <= value;
      endmethod
      method Action awLEN (value);
	 slave.bus.awLEN(value);
	 awLEN_wire <= value;
      endmethod
      method Action awSIZE (value);
	 slave.bus.awSIZE(value);
	 awSIZE_wire <= value;
      endmethod
      method Action awBURST (value);
	 slave.bus.awBURST(value);
	 awBURST_wire <= value;
      endmethod
      method Action awLOCK (value);
	 slave.bus.awLOCK(value);
	 awLOCK_wire <= value;
      endmethod
      method Action awPROT (value);
	 slave.bus.awPROT(value);
	 awPROT_wire <= value;
      endmethod
      method Action awCACHE (value);
	 slave.bus.awCACHE(value);
	 awCACHE_wire <= value;
      endmethod
      method Action awVALID (value);
	 slave.bus.awVALID(value);
	 awVALID_wire <= value;
      endmethod

      // Address Outputs
      method awREADY = slave.bus.awREADY;

      // Data Inputs
      method Action wID (value);
	 slave.bus.wID(value);
	 wID_wire <= value;
      endmethod
      method Action wDATA (value);
	 slave.bus.wDATA(value);
	 wDATA_wire <= value;
      endmethod
      method Action wSTRB (value);
	 slave.bus.wSTRB(value);
	 wSTRB_wire <= value;
      endmethod
      method Action wLAST (value);
	 slave.bus.wLAST(value);
	 wLAST_wire <= value;
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
      method bID = slave.bus.bID;
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

interface AxiPC_Ifc#(`TLM_PRM_DCL);
   method Action   aw_id     (AxiId#(`TLM_PRM) value);
   method Action   aw_len    (AxiLen aw_len);
   method Action   aw_size   (AxiSize value);
   method Action   aw_burst  (AxiBurst value);
   method Action   aw_lock   (AxiLock value);
   method Action   aw_cache  (AxiCache value);
   method Action   aw_prot   (AxiProt value);
   method Action   aw_addr   (AxiAddr#(`TLM_PRM) value);
   method Action   aw_valid  (Bool value);
   method Action   aw_ready  (Bool value);

   method Action   w_id      (AxiId#(`TLM_PRM) value);
   method Action   w_data    (AxiData#(`TLM_PRM) value);
   method Action   w_strb    (AxiByteEn#(`TLM_PRM) value);
   method Action   w_last    (Bool value);
   method Action   w_valid   (Bool value);
   method Action   w_ready   (Bool value);

   method Action   b_id      (AxiId#(`TLM_PRM) value);
   method Action   b_resp    (AxiResp value);
   method Action   b_valid   (Bool value);
   method Action   b_ready   (Bool value);

   method Action   ar_id     (AxiId#(`TLM_PRM) value);
   method Action   ar_len    (AxiLen value);
   method Action   ar_size   (AxiSize value);
   method Action   ar_burst  (AxiBurst value);
   method Action   ar_lock   (AxiLock value);
   method Action   ar_cache  (AxiCache value);
   method Action   ar_prot   (AxiProt value);
   method Action   ar_addr   (AxiAddr#(`TLM_PRM) value);
   method Action   ar_valid  (Bool value);
   method Action   ar_ready  (Bool value);

   method Action   r_id      (AxiId#(`TLM_PRM) value);
   method Action   r_data    (AxiData#(`TLM_PRM) value);
   method Action   r_resp    (AxiResp value);
   method Action   r_last    (Bool value);
   method Action   r_valid   (Bool value);
   method Action   r_ready   (Bool value);

   // unused
   method Action   c_active  (Bool value);
   method Action   c_sysreq  (Bool value);
   method Action   c_sysack  (Bool value);

endinterface


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef struct {UInt#(32) exmon_width;
		UInt#(32) maxwaits;
		} AxiPCParams deriving (Eq, Bits, Bounded);


instance DefaultValue #(AxiPCParams);
   function defaultValue ();
      AxiPCParams params;
      params.exmon_width = 8;
      params.maxwaits    = 256;
      return params;
   endfunction
endinstance

import "BVI" AxiPC =
module mkAxiPC#(AxiPCParams params) (AxiPC_Ifc#(`TLM_PRM));

   parameter ID_WIDTH    = valueOf(id_size);
   parameter DATA_WIDTH  = valueOf(data_size);
   parameter MAXWAITS    = pack(params.maxwaits);
   parameter EXMON_WIDTH = pack(params.exmon_width);

   // a few extras for address width
   parameter ADDR_WIDTH   = valueOf(addr_size);
   parameter WUSER_WIDTH  = valueOf(addr_size);
   parameter BUSER_WIDTH  = valueOf(addr_size);
   parameter ARUSER_WIDTH = valueOf(addr_size);
   parameter RUSER_WIDTH  = valueOf(addr_size);

   default_clock clk( ACLK);
   default_reset rst(ARESETn);
   method    aw_id     (AWID   )enable((*inhigh*)IGNORE00);
   method    aw_len    (AWLEN  )enable((*inhigh*)IGNORE01);
   method    aw_size   (AWSIZE )enable((*inhigh*)IGNORE02);
   method    aw_burst  (AWBURST)enable((*inhigh*)IGNORE03);
   method    aw_lock   (AWLOCK )enable((*inhigh*)IGNORE04);
   method    aw_cache  (AWCACHE)enable((*inhigh*)IGNORE05);
   method    aw_prot   (AWPROT )enable((*inhigh*)IGNORE06);
   method    aw_addr   (AWADDR )enable((*inhigh*)IGNORE07);
   method    aw_valid  (AWVALID)enable((*inhigh*)IGNORE08);
   method    aw_ready  (AWREADY)enable((*inhigh*)IGNORE09);
   method    w_id      (WID    )enable((*inhigh*)IGNORE10);
   method    w_data    (WDATA  )enable((*inhigh*)IGNORE11);
   method    w_strb    (WSTRB  )enable((*inhigh*)IGNORE12);
   method    w_last    (WLAST  )enable((*inhigh*)IGNORE13);
   method    w_valid   (WVALID )enable((*inhigh*)IGNORE14);
   method    w_ready   (WREADY )enable((*inhigh*)IGNORE15);

   method    b_id      (BID    )enable((*inhigh*)IGNORE16);
   method    b_resp    (BRESP  )enable((*inhigh*)IGNORE17);
   method    b_valid   (BVALID )enable((*inhigh*)IGNORE18);
   method    b_ready   (BREADY )enable((*inhigh*)IGNORE19);

   method    ar_id     (ARID   )enable((*inhigh*)IGNORE20);
   method    ar_len    (ARLEN  )enable((*inhigh*)IGNORE21);
   method    ar_size   (ARSIZE )enable((*inhigh*)IGNORE22);
   method    ar_burst  (ARBURST)enable((*inhigh*)IGNORE24);
   method    ar_lock   (ARLOCK )enable((*inhigh*)IGNORE25);
   method    ar_cache  (ARCACHE)enable((*inhigh*)IGNORE26);
   method    ar_prot   (ARPROT )enable((*inhigh*)IGNORE27);
   method    ar_addr   (ARADDR )enable((*inhigh*)IGNORE28);
   method    ar_valid  (ARVALID)enable((*inhigh*)IGNORE29);
   method    ar_ready  (ARREADY)enable((*inhigh*)IGNORE30);

   method    r_id      (RID    )enable((*inhigh*)IGNORE31);
   method    r_data    (RDATA  )enable((*inhigh*)IGNORE32);
   method    r_resp    (RRESP  )enable((*inhigh*)IGNORE33);
   method    r_last    (RLAST  )enable((*inhigh*)IGNORE34);
   method    r_valid   (RVALID )enable((*inhigh*)IGNORE35);
   method    r_ready   (RREADY )enable((*inhigh*)IGNORE36);

   method    c_active  (CACTIVE)enable((*inhigh*)IGNORE37);
   method    c_sysreq  (CSYSREQ)enable((*inhigh*)IGNORE38);
   method    c_sysack  (CSYSACK)enable((*inhigh*)IGNORE39);

   schedule (aw_len,aw_id,aw_size,aw_burst,aw_lock,aw_cache,aw_prot,aw_addr,aw_valid , aw_ready, w_id , w_data , w_strb, w_last , w_valid , w_ready, b_id , b_resp, b_valid, b_ready, ar_id , ar_len , ar_size , ar_burst, ar_lock, ar_cache, ar_prot, ar_addr , ar_valid, ar_ready , r_id  , r_data, r_resp, r_last, r_valid , r_ready, c_active, c_sysreq , c_sysack) CF (aw_len,aw_id,aw_size,aw_burst,aw_lock,aw_cache,aw_prot,aw_addr,aw_valid , aw_ready, w_id , w_data , w_strb, w_last , w_valid , w_ready, b_id , b_resp, b_valid, b_ready, ar_id , ar_len , ar_size , ar_burst, ar_lock, ar_cache, ar_prot, ar_addr , ar_valid, ar_ready , r_id  , r_data, r_resp, r_last, r_valid , r_ready, c_active, c_sysreq , c_sysack);

endmodule

endpackage



