// Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved

package AXI4_AXI4_Lite_Adapters;

// ================================================================
// Adapters for interconnecting AXI4 and AXI4_Lite.

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

fn_AXI4_Lite_Master_IFC_to_AXI4_Master_IFC;

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;
import EdgeFIFOFs :: *;

// ================================================================
// Project imports

import AXI4_Lite_Types         :: *;
import AXI4_Types              :: *;

// ================================================================
// Compute the encoding of AWSIZE/ARSIZE

function Bit #(3) wd_data_to_axsize (Integer wd_data_i);
   Bit #(3) axsize = (  (wd_data_i == 32)
		      ? 3'b_010
		      : (  (wd_data_i == 64)
			 ? 3'b_011
			 : 3'b_000));
   return axsize;
endfunction

// ================================================================

function AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)
         fn_AXI4_Lite_Master_IFC_to_AXI4_Master_IFC
         (AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user)  axi4_lite);

   return
   interface AXI4_Master_IFC;

      // ----------------
      // Wr Addr channel
      // output buses
      method Bool           m_awvalid = axi4_lite.m_awvalid;

      method Bit #(wd_id)   m_awid     = 0;
      method Bit #(wd_addr) m_awaddr   = axi4_lite.m_awaddr;
      method Bit #(8)       m_awlen    = 0;                       // burst length = awlen+1
      method Bit #(3)       m_awsize   = wd_data_to_axsize (valueOf (wd_data));
      method Bit #(2)       m_awburst  = 2'b_00;                  // FIXED
      method Bit #(1)       m_awlock   = 0;                       // NORMAL
      method Bit #(4)       m_awcache  = 4'b_0000;                // Device Non-Bufferable
      method Bit #(3)       m_awprot   = axi4_lite.m_awprot;
      method Bit #(4)       m_awqos    = 4'b_0000;
      method Bit #(4)       m_awregion = 4'b_0000;
      method Bit #(wd_user) m_awuser   = 0;

      // input buses
      method Action m_awready (Bool awready) = axi4_lite.m_awready (awready);

      // ----------------
      // Wr Data channel
      // output buses
      method Bool                      m_wvalid = axi4_lite.m_wvalid;

      method Bit #(wd_id)              m_wid    = 0;
      method Bit #(wd_data)            m_wdata  = axi4_lite.m_wdata;
      method Bit #(TDiv #(wd_data, 8)) m_wstrb  = axi4_lite.m_wstrb;
      method Bool                      m_wlast  = True;
      method Bit #(wd_user)            m_wuser  = 0;

      // input buses
      method Action m_wready (Bool wready) = axi4_lite.m_wready (wready);

      // ----------------
      // Wr Response channel
      // input buses
      method Action m_bvalid (Bool           bvalid,
			      Bit #(wd_id)   bid,
			      Bit #(2)       bresp,
			      Bit #(wd_user) buser) = axi4_lite.m_bvalid (bvalid,
									  bresp,
									  0);

      // output buses
      method Bool m_bready = axi4_lite.m_bready;

      // ----------------
      // Rd Addr channel
      // output buses
      method Bool            m_arvalid = axi4_lite.m_arvalid;

      method Bit #(wd_id)    m_arid     = 0;
      method Bit #(wd_addr)  m_araddr   = axi4_lite.m_araddr;
      method Bit #(8)        m_arlen    = 0;                       // burst length = awlen+1
      method Bit #(3)        m_arsize   = wd_data_to_axsize (valueOf (wd_data));
      method Bit #(2)        m_arburst  = 2'b_00;                  // FIXED
      method Bit #(1)        m_arlock   = 0;                       // NORMAL
      method Bit #(4)        m_arcache  = 4'b_0000;                // Device Non-Bufferable
      method Bit #(3)        m_arprot   = axi4_lite.m_arprot;
      method Bit #(4)        m_arqos    = 4'b_0000;
      method Bit #(4)        m_arregion = 4'b_0000;
      method Bit #(wd_user)  m_aruser   = axi4_lite.m_aruser;

      // input buses
      method Action m_arready (Bool arready) = axi4_lite.m_arready (arready);

      // ----------------
      // Rd Data channel
      // input buses
      method Action m_rvalid (Bool           rvalid,
			      Bit #(wd_id)   rid,
			      Bit #(wd_data) rdata,
			      Bit #(2)       rresp,
			      Bool           rlast,
			      Bit #(wd_user) ruser) = axi4_lite.m_rvalid (rvalid,
									  rresp,
									  rdata,
									  0);

      // output buses
      method Bool m_rready = axi4_lite.m_rready;

   endinterface;
endfunction

// ================================================================

endpackage
