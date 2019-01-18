// Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved

package AXI4_Types;

// ================================================================
// Facilities for ARM AXI4, consisting of 5 independent channels:
//   Write Address, Write Data, Write Response, Read Address and Read Data

// Ref: ARM document:
//    AMBA AXI and ACE Protocol Specification
//    AXI3, AXI4, and AXI4-Lite
//    ACE and ACE-Lite
//    ARM IHI 0022E (ID022613)
//    Issue E, 22 Feb 2013

// See export list below

// ================================================================
// Exports

export

// RTL-level interfaces (signals/buses)
AXI4_Master_IFC (..);

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;
import EdgeFIFOFs :: *;

// ****************************************************************
// ****************************************************************
// Section: RTL-level interfaces
// ****************************************************************
// ****************************************************************

// ================================================================
// These are the signal-level interfaces for an AXI4 master.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Master_IFC #(numeric type wd_id,
			    numeric type wd_addr,
			    numeric type wd_data,
			    numeric type wd_user);
   // ----------------
   // Wr Addr channel
   (* always_ready, result="awvalid" *)   method Bool           m_awvalid;     // out

   (* always_ready, result="awid" *)      method Bit #(wd_id)   m_awid;        // out
   (* always_ready, result="awaddr" *)    method Bit #(wd_addr) m_awaddr;      // out
   (* always_ready, result="awlen" *)     method Bit #(8)       m_awlen;       // out
   (* always_ready, result="awsize" *)    method Bit #(3)       m_awsize;      // out
   (* always_ready, result="awburst" *)   method Bit #(2)       m_awburst;     // out
   (* always_ready, result="awlock" *)    method Bit #(1)       m_awlock;      // out
   (* always_ready, result="awcache" *)   method Bit #(4)       m_awcache;     // out
   (* always_ready, result="awprot" *)    method Bit #(3)       m_awprot;      // out
   (* always_ready, result="awqos" *)     method Bit #(4)       m_awqos;       // out
   (* always_ready, result="awregion" *)  method Bit #(4)       m_awregion;    // out
   (* always_ready, result="awuser" *)    method Bit #(wd_user) m_awuser;      // out

   (* always_ready, always_enabled, prefix="" *)
   method Action m_awready ((* port="awready" *) Bool awready);                // in

   // ----------------
   // Wr Data channel
   (* always_ready, result="wvalid" *)  method Bool                      m_wvalid;    // out

   (* always_ready, result="wdata" *)   method Bit #(wd_data)            m_wdata;     // out
   (* always_ready, result="wstrb" *)   method Bit #(TDiv #(wd_data, 8)) m_wstrb;     // out
   (* always_ready, result="wlast" *)   method Bool                      m_wlast;     // out
   (* always_ready, result="wuser" *)   method Bit #(wd_user)            m_wuser;     // out

   (* always_ready, always_enabled, prefix = "" *)
   method Action m_wready ((* port="wready" *)  Bool wready);                         // in

   // ----------------
   // Wr Response channel
   (* always_ready, always_enabled, prefix = "" *)
   method Action m_bvalid ((* port="bvalid" *)  Bool           bvalid,    // in
			   (* port="bid"    *)  Bit #(wd_id)   bid,       // in
			   (* port="bresp"  *)  Bit #(2)       bresp,     // in
			   (* port="buser"  *)  Bit #(wd_user) buser);    // in

   (* always_ready, prefix = "", result="bready" *)
   method Bool m_bready;                                                  // out

   // ----------------
   // Rd Addr channel
   (* always_ready, result="arvalid" *)   method Bool            m_arvalid;     // out

   (* always_ready, result="arid" *)      method Bit #(wd_id)    m_arid;        // out
   (* always_ready, result="araddr" *)    method Bit #(wd_addr)  m_araddr;      // out
   (* always_ready, result="arlen" *)     method Bit #(8)        m_arlen;       // out
   (* always_ready, result="arsize" *)    method Bit #(3)        m_arsize;      // out
   (* always_ready, result="arburst" *)   method Bit #(2)        m_arburst;     // out
   (* always_ready, result="arlock" *)    method Bit #(1)        m_arlock;      // out
   (* always_ready, result="arcache" *)   method Bit #(4)        m_arcache;     // out
   (* always_ready, result="arprot" *)    method Bit #(3)        m_arprot;      // out
   (* always_ready, result="arqos" *)     method Bit #(4)        m_arqos;       // out
   (* always_ready, result="arregion" *)  method Bit #(4)        m_arregion;    // out
   (* always_ready, result="aruser" *)    method Bit #(wd_user)  m_aruser;      // out

   (* always_ready, always_enabled, prefix="" *)
   method Action m_arready ((* port="arready" *) Bool arready);    // in

   // ----------------
   // Rd Data channel
   (* always_ready, always_enabled, prefix = "" *)
   method Action m_rvalid ((* port="rvalid" *)  Bool           rvalid,    // in
			   (* port="rid"    *)  Bit #(wd_id)   rid,       // in
			   (* port="rdata" *)   Bit #(wd_data) rdata,     // in
			   (* port="rresp" *)   Bit #(2)       rresp,     // in
			   (* port="rlast" *)   Bool           rlast,     // in
			   (* port="ruser" *)   Bit #(wd_user) ruser);    // in

   (* always_ready, result="rready" *)
   method Bool m_rready;                                                  // out
endinterface: AXI4_Master_IFC

// ================================================================

endpackage
