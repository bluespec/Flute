// ================================================================
// Transformer to get AXI4_Lite Slave interface from an AXI4 Slave interface

function AXI4_Lite_Slave_IFC #(wd_addr, wd_data, wd_user)
         fv_AXI4_Slave_IFC_to_AXI4_Lite_Slave_IFC
         (AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi4);

   return
   interface AXI4_Lite_Slave_IFC;

      // ----------------
      // Wr Addr channel
      // input buses
      method Action m_awvalid (Bool           awvalid,    // in
			       Bit #(wd_addr) awaddr,     // in
			       Bit #(3)       awprot,     // in
			       Bit #(wd_user) awuser);    // in
	 axi4.m_awvalid (awvalid,
			 0,                     // awid
			 awaddr,
			 0,                     // awlen (= burst len 1)
			 wd_data_to_axsize (valueOf (wd_data));
			 axburst_fixed,
			 axlock_normal,
			 awcache_dev_nonbuf,
			 awprot,
			 0,                     // qos
			 0,                     // region
			 awuser);
      endmethod

      // output buses
      method Bool m_awready = axi4.m_awready;

      // ----------------
      // Wr Data channel
      // input buses
      method Action m_wvalid (Bool                     wvalid,    // in
			      Bit #(wd_data)           wdata,     // in
			      Bit #(TDiv #(wd_data,8)) wstrb);    // in
	 axi4.m_wvalid(wvalid,
		       wdata,
		       wstrb,
		       True,    // wlast
		       0);      // wuser
      endmethod

      // output buses
      method Bool m_wready = axi4.m_wready;

      // ----------------
      // Wr Response channel
      // output buses
      method Bool m_bvalid = axi4.m_bvalid;
      method Bool m_bresp  = axi4.m_bresp;
      method Bool m_buser  = axi4.m_buser;

      // input buses
      method Action m_bready  (Bool bready);    // in
	 axi4.m_bready (bready);
      endmethod

      // ----------------
      // Rd Addr channel
      // input buses
      method Action m_arvalid (Bool           arvalid,    // in
			       Bit #(wd_addr) araddr,     // in
			       Bit #(3)       arprot,     // in
			       Bit #(wd_user) aruser);    // in
	 axi4.m_arvalid (arvalid,
			 0,                     // arid
			 araddr,
			 0,                     // arlen (= burst len 1)
			 wd_data_to_axsize (valueOf (wd_data));
			 axburst_fixed,
			 axlock_normal,
			 arcache_dev_nonbuf,
			 arprot,
			 0,                     // qos
			 0,                     // region
			 aruser);
      endmethod

      // output buses
      method Bool m_arready = axi4.m_arready;

      // ----------------
      // Rd Data channel
      // input buses
      method Bool           m_rvalid = axi4.m_rvalid;    // out
      method Bit #(2)       m_rresp  = axi4.m_rresp;     // out
      method Bit #(wd_data) m_rdata  = axi4.m_rdata;     // out
      method Bit #(wd_user) m_ruser  = axi4.m_ruser;     // out

      method Action m_rready  (Bool rready);    // in
	 axi4.m_rready (rready);
      endmethod
   endinterface;
endfunction
