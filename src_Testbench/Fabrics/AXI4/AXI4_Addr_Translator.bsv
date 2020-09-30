// Copyright (c) 2020 Bluespec, Inc. All Rights Reserved

package AXI4_Addr_Translator;

// ================================================================
// This package defines an AXI4-slave-to-AXI4-slave 'address-translator' function.
// That just adds/subtracts a fixed constant from addresses.

// ================================================================
// Bluespec library imports

// none

// ----------------
// BSV additional libs

// none

// ================================================================
// Project imports

import AXI4_Types :: *;

// ================================================================
// Master-to-Master interface transformer with address translation

function AXI4_Master_IFC  #(wd_id, wd_addr, wd_data, wd_user)
   fv_AXI4_Master_Address_Translator (Bool            add_not_sub,
				      Bit #(wd_addr)  addr_delta,
				      AXI4_Master_IFC  #(wd_id, wd_addr, wd_data, wd_user)  ifc);

   function Bit #(wd_addr) fv_addr_translate (Bit #(wd_addr)  addr);
      return (add_not_sub ? addr + addr_delta : addr - addr_delta);
   endfunction

   return interface AXI4_Master_IFC
	     // Wr Addr channel
	     method Bool            m_awvalid  = ifc.m_awvalid;                       // out
	     method Bit #(wd_id)    m_awid     = ifc.m_awid;                          // out
	     method Bit #(wd_addr)  m_awaddr   = fv_addr_translate (ifc.m_awaddr);    // out
	     method Bit #(8)        m_awlen    = ifc.m_awlen;                         // out
	     method AXI4_Size       m_awsize   = ifc.m_awsize;                        // out
	     method Bit #(2)        m_awburst  = ifc.m_awburst;                       // out
	     method Bit #(1)        m_awlock   = ifc.m_awlock;                        // out
	     method Bit #(4)        m_awcache  = ifc.m_awcache;                       // out
	     method Bit #(3)        m_awprot   = ifc.m_awprot;                        // out
	     method Bit #(4)        m_awqos    = ifc.m_awqos;                         // out
	     method Bit #(4)        m_awregion = ifc.m_awregion;                      // out
	     method Bit #(wd_user)  m_awuser   = ifc.m_awuser;                        // out
	     method Action m_awready (Bool awready) = ifc.m_awready (awready);        // in

		// Wr Data channel
	     method Bool                       m_wvalid = ifc.m_wvalid;       // out
	     method Bit #(wd_data)             m_wdata  = ifc.m_wdata;        // out
	     method Bit #(TDiv #(wd_data, 8))  m_wstrb  = ifc.m_wstrb;        // out
	     method Bool                       m_wlast  = ifc.m_wlast;        // out
	     method Bit #(wd_user)             m_wuser  = ifc.m_wuser;        // out

	     method Action m_wready (Bool wready) = ifc.m_wready (wready);    // in

	     // Wr Response channel
	     method Action m_bvalid (Bool            bvalid,    // in
				     Bit #(wd_id)    bid,       // in
				     Bit #(2)        bresp,     // in
				     Bit #(wd_user)  buser);    // in
		ifc.m_bvalid (bvalid, bid, bresp, buser);
	     endmethod
	     method Bool m_bready = ifc.m_bready;               // out

	     // Rd Addr channel
	     method Bool            m_arvalid  = ifc.m_arvalid;                       // out
	     method Bit #(wd_id)    m_arid     = ifc.m_arid;                          // out
	     method Bit #(wd_addr)  m_araddr   = fv_addr_translate (ifc.m_araddr);    // out
	     method Bit #(8)        m_arlen    = ifc.m_arlen;                         // out
	     method AXI4_Size       m_arsize   = ifc.m_arsize;                        // out
	     method Bit #(2)        m_arburst  = ifc.m_arburst;                       // out
	     method Bit #(1)        m_arlock   = ifc.m_arlock;                        // out
	     method Bit #(4)        m_arcache  = ifc.m_arcache;                       // out
	     method Bit #(3)        m_arprot   = ifc.m_arprot;                        // out
	     method Bit #(4)        m_arqos    = ifc.m_arqos;                         // out
	     method Bit #(4)        m_arregion = ifc.m_arregion;                      // out
	     method Bit #(wd_user)  m_aruser   = ifc.m_aruser;                        // out
	     method Action m_arready (Bool arready) = ifc.m_arready (arready);        // in

	     // Rd Data channel
	     method Action m_rvalid (Bool            rvalid,    // in
				     Bit #(wd_id)    rid,       // in
				     Bit #(wd_data)  rdata,     // in
				     Bit #(2)        rresp,     // in
				     Bool            rlast,     // in
				     Bit #(wd_user)  ruser);    // in
		ifc.m_rvalid (rvalid, rid, rdata, rresp, rlast, ruser);
	     endmethod
	     method Bool m_rready = ifc.m_rready;               // out
	  endinterface;
endfunction

// ================================================================
// Slave-to-Slave interface transformer with address translation

function AXI4_Slave_IFC  #(wd_id, wd_addr, wd_data, wd_user)
   fv_AXI4_Slave_Address_Translator (Bool            add_not_sub,
				     Bit #(wd_addr)  addr_delta,
				     AXI4_Slave_IFC  #(wd_id, wd_addr, wd_data, wd_user)  ifc);

   function Bit #(wd_addr) fv_addr_translate (Bit #(wd_addr)  addr);
      return (add_not_sub ? addr + addr_delta : addr - addr_delta);
   endfunction

   return interface AXI4_Slave_IFC 
	     // Wr Addr channel
	     method Action m_awvalid (Bool            awvalid,
				      Bit #(wd_id)    awid,
				      Bit #(wd_addr)  awaddr,
				      Bit #(8)        awlen,
				      AXI4_Size       awsize,
				      Bit #(2)        awburst,
				      Bit #(1)        awlock,
				      Bit #(4)        awcache,
				      Bit #(3)        awprot,
				      Bit #(4)        awqos,
				      Bit #(4)        awregion,
				      Bit #(wd_user)  awuser);
		ifc.m_awvalid (awvalid, awid,
			       fv_addr_translate (awaddr),
			       awlen, awsize, awburst, awlock, awcache, awprot, awqos, awregion, awuser);
	     endmethod
	     method Bool m_awready = ifc.m_awready;

	     // Wr Data channel
	     method Action m_wvalid (Bool                       wvalid,
				     Bit #(wd_data)             wdata,
				     Bit #(TDiv #(wd_data, 8))  wstrb,
				     Bool                       wlast,
				     Bit #(wd_user)             wuser);
		ifc.m_wvalid (wvalid, wdata, wstrb, wlast, wuser);
	     endmethod
	     method Bool m_wready = ifc.m_wready;

	     // Wr Response channel
	     method Bool           m_bvalid = ifc.m_bvalid;
	     method Bit #(wd_id)   m_bid    = ifc.m_bid;
	     method Bit #(2)       m_bresp  = ifc.m_bresp;
	     method Bit #(wd_user) m_buser  = ifc.m_buser;
	     method Action         m_bready (Bool bready) = ifc.m_bready (bready);

	     // Rd Addr channel
	     method Action m_arvalid (Bool            arvalid,
				      Bit #(wd_id)    arid,
				      Bit #(wd_addr)  araddr,
				      Bit #(8)        arlen,
				      AXI4_Size       arsize,
				      Bit #(2)        arburst,
				      Bit #(1)        arlock,
				      Bit #(4)        arcache,
				      Bit #(3)        arprot,
				      Bit #(4)        arqos,
				      Bit #(4)        arregion,
				      Bit #(wd_user)  aruser);
		ifc.m_arvalid (arvalid, arid,
			       fv_addr_translate (araddr),
			       arlen, arsize, arburst, arlock, arcache, arprot, arqos, arregion, aruser);
	     endmethod
	     method Bool m_arready = ifc.m_arready;

	     // Rd Data channel
	     method Bool           m_rvalid = ifc.m_rvalid;
	     method Bit #(wd_id)   m_rid    = ifc.m_rid;
	     method Bit #(wd_data) m_rdata  = ifc.m_rdata;
	     method Bit #(2)       m_rresp  = ifc.m_rresp;
	     method Bool           m_rlast  = ifc.m_rlast;
	     method Bit #(wd_user) m_ruser  = ifc.m_ruser;
	     method Action m_rready  (Bool rready);
		ifc.m_rready (rready);
	     endmethod
	  endinterface;
endfunction

// ================================================================

endpackage: AXI4_Addr_Translator
