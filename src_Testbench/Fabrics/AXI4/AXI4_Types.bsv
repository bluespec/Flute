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
// Fixed-width AXI4 buses

typedef Bit #(8)  AXI4_Len;

// AxSIZE
typedef Bit #(3)  AXI4_Size;

AXI4_Size  axsize_1   = 3'b_000;
AXI4_Size  axsize_2   = 3'b_001;
AXI4_Size  axsize_4   = 3'b_010;
AXI4_Size  axsize_8   = 3'b_011;
AXI4_Size  axsize_16  = 3'b_100;
AXI4_Size  axsize_32  = 3'b_101;
AXI4_Size  axsize_64  = 3'b_110;
AXI4_Size  axsize_128 = 3'b_111;

function Bit #(8) fv_AXI4_Size_to_num_bytes (AXI4_Size  axi4_size);
   return (1 << axi4_size);
endfunction

function AXI4_Size fv_num_bytes_to_AXI4_Size (Bit #(8) num_bytes);
   return (case (num_bytes)
	      1:   axsize_1;
	      2:   axsize_2;
	      4:   axsize_4;
	      8:   axsize_8;
	      16:  axsize_16;
	      32:  axsize_32;
	      64:  axsize_64;
	      128: axsize_128;
	      default: axsize_128;    // Bogus
	   endcase);
endfunction

// AxBURST
typedef Bit #(2)  AXI4_Burst;

AXI4_Burst  axburst_fixed = 2'b_00;
AXI4_Burst  axburst_incr  = 2'b_01;
AXI4_Burst  axburst_wrap  = 2'b_10;

// AxLOCK
typedef Bit #(1)  AXI4_Lock;

AXI4_Lock  axlock_normal    = 1'b_0;
AXI4_Lock  axlock_exclusive = 1'b_1;

// ARCACHE
typedef Bit #(4)  AXI4_Cache;

AXI4_Cache  arcache_dev_nonbuf           = 'b_0000;
AXI4_Cache  arcache_dev_buf              = 'b_0001;

AXI4_Cache  arcache_norm_noncache_nonbuf = 'b_0010;
AXI4_Cache  arcache_norm_noncache_buf    = 'b_0011;

AXI4_Cache  arcache_wthru_no_alloc       = 'b_1010;
AXI4_Cache  arcache_wthru_r_alloc        = 'b_1110;
AXI4_Cache  arcache_wthru_w_alloc        = 'b_1010;
AXI4_Cache  arcache_wthru_r_w_alloc      = 'b_1110;

AXI4_Cache  arcache_wback_no_alloc       = 'b_1011;
AXI4_Cache  arcache_wback_r_alloc        = 'b_1111;
AXI4_Cache  arcache_wback_w_alloc        = 'b_1011;
AXI4_Cache  arcache_wback_r_w_alloc      = 'b_1111;

// AWCACHE
AXI4_Cache  awcache_dev_nonbuf           = 'b_0000;
AXI4_Cache  awcache_dev_buf              = 'b_0001;

AXI4_Cache  awcache_norm_noncache_nonbuf = 'b_0010;
AXI4_Cache  awcache_norm_noncache_buf    = 'b_0011;

AXI4_Cache  awcache_wthru_no_alloc       = 'b_0110;
AXI4_Cache  awcache_wthru_r_alloc        = 'b_0110;
AXI4_Cache  awcache_wthru_w_alloc        = 'b_1110;
AXI4_Cache  awcache_wthru_r_w_alloc      = 'b_1110;

AXI4_Cache  awcache_wback_no_alloc       = 'b_0111;
AXI4_Cache  awcache_wback_r_alloc        = 'b_0111;
AXI4_Cache  awcache_wback_w_alloc        = 'b_1111;
AXI4_Cache  awcache_wback_r_w_alloc      = 'b_1111;

// PROT
typedef Bit #(3)  AXI4_Prot;

Bit #(1)  axprot_0_unpriv     = 0;    Bit #(1) axprot_0_priv       = 1;
Bit #(1)  axprot_1_secure     = 0;    Bit #(1) axprot_1_non_secure = 1;
Bit #(1)  axprot_2_data       = 0;    Bit #(1) axprot_2_instr      = 1;

// QoS
typedef Bit #(4)  AXI4_QoS;

// REGION
typedef Bit #(4)  AXI4_Region;

// RESP
typedef Bit #(2)  AXI4_Resp;

AXI4_Resp  axi4_resp_okay   = 2'b_00;
AXI4_Resp  axi4_resp_exokay = 2'b_01;
AXI4_Resp  axi4_resp_slverr = 2'b_10;
AXI4_Resp  axi4_resp_decerr = 2'b_11;

// ================================================================
// Function to check address-alignment

function Bool fn_addr_is_aligned (Bit #(wd_addr) addr, AXI4_Size size);
   return (    (size == axsize_1)
	   || ((size == axsize_2)   && (addr [0]   == 1'b0))
	   || ((size == axsize_4)   && (addr [1:0] == 2'b0))
	   || ((size == axsize_8)   && (addr [2:0] == 3'b0))
	   || ((size == axsize_16)  && (addr [3:0] == 4'b0))
	   || ((size == axsize_32)  && (addr [4:0] == 5'b0))
	   || ((size == axsize_64)  && (addr [5:0] == 6'b0))
	   || ((size == axsize_128) && (addr [6:0] == 7'b0)));
endfunction

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
   (* always_ready, result="awsize" *)    method AXI4_Size      m_awsize;      // out
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
   (* always_ready, result="arsize" *)    method AXI4_Size       m_arsize;      // out
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
			   (* port="rdata"  *)  Bit #(wd_data) rdata,     // in
			   (* port="rresp"  *)  Bit #(2)       rresp,     // in
			   (* port="rlast"  *)  Bool           rlast,     // in
			   (* port="ruser"  *)  Bit #(wd_user) ruser);    // in

   (* always_ready, result="rready" *)
   method Bool m_rready;                                                  // out
endinterface: AXI4_Master_IFC

// ================================================================
// These are the signal-level interfaces for an AXI4-Lite slave.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Slave_IFC #(numeric type wd_id,
			   numeric type wd_addr,
			   numeric type wd_data,
			   numeric type wd_user);
   // Wr Addr channel
   (* always_ready, always_enabled, prefix = "" *)
   method Action m_awvalid ((* port="awvalid" *)   Bool            awvalid,     // in
			    (* port="awid" *)      Bit #(wd_id)    awid,        // in
			    (* port="awaddr" *)    Bit #(wd_addr)  awaddr,      // in
			    (* port="awlen" *)     Bit #(8)        awlen,       // in
			    (* port="awsize" *)    AXI4_Size       awsize,      // in
			    (* port="awburst" *)   Bit #(2)        awburst,     // in
			    (* port="awlock" *)    Bit #(1)        awlock,      // in
			    (* port="awcache" *)   Bit #(4)        awcache,     // in
			    (* port="awprot" *)    Bit #(3)        awprot,      // in
			    (* port="awqos" *)     Bit #(4)        awqos,       // in
			    (* port="awregion" *)  Bit #(4)        awregion,    // in
			    (* port="awuser" *)    Bit #(wd_user)  awuser);     // in
   (* always_ready, result="awready" *)
   method Bool m_awready;                                                       // out

   // Wr Data channel
   (* always_ready, always_enabled, prefix = "" *)
   method Action m_wvalid ((* port="wvalid" *) Bool                      wvalid,    // in
			   (* port="wdata" *)  Bit #(wd_data)            wdata,     // in
			   (* port="wstrb" *)  Bit #(TDiv #(wd_data,8))  wstrb,     // in
			   (* port="wlast" *)  Bool                      wlast,     // in
			   (* port="wuser" *)  Bit #(wd_user)            wuser);    // in
   (* always_ready, result="wready" *)
   method Bool m_wready;                                                           // out

   // Wr Response channel
   (* always_ready, result="bvalid" *)  method Bool            m_bvalid;    // out
   (* always_ready, result="bid" *)     method Bit #(wd_id)    m_bid;       // out
   (* always_ready, result="bresp" *)   method Bit #(2)        m_bresp;     // out
   (* always_ready, result="buser" *)   method Bit #(wd_user)  m_buser;     // out
   (* always_ready, always_enabled, prefix="" *)
   method Action m_bready  ((* port="bready" *)   Bool bready);            // in

   // Rd Addr channel
   (* always_ready, always_enabled, prefix = "" *)
   method Action m_arvalid ((* port="arvalid" *)   Bool            arvalid,     // in
			    (* port="arid" *)      Bit #(wd_id)    arid,        // in
			    (* port="araddr" *)    Bit #(wd_addr)  araddr,      // in
			    (* port="arlen" *)     Bit #(8)        arlen,       // in
			    (* port="arsize" *)    AXI4_Size       arsize,      // in
			    (* port="arburst" *)   Bit #(2)        arburst,     // in
			    (* port="arlock" *)    Bit #(1)        arlock,      // in
			    (* port="arcache" *)   Bit #(4)        arcache,     // in
			    (* port="arprot" *)    Bit #(3)        arprot,      // in
			    (* port="arqos" *)     Bit #(4)        arqos,       // in
			    (* port="arregion" *)  Bit #(4)        arregion,    // in
			    (* port="aruser" *)    Bit #(wd_user)  aruser);     // in
   (* always_ready, result="arready" *)
   method Bool m_arready;                                                       // out

   // Rd Data channel
   (* always_ready, result="rvalid" *)  method Bool            m_rvalid;    // out
   (* always_ready, result="rid" *)     method Bit #(wd_id)    m_rid;       // out
   (* always_ready, result="rdata" *)   method Bit #(wd_data)  m_rdata;     // out
   (* always_ready, result="rresp" *)   method Bit #(2)        m_rresp;     // out
   (* always_ready, result="rlast" *)   method Bool            m_rlast;     // out
   (* always_ready, result="ruser" *)   method Bit #(wd_user)  m_ruser;     // out
   (* always_ready, always_enabled, prefix="" *)
   method Action m_rready  ((* port="rready" *)   Bool rready);             // in
endinterface: AXI4_Slave_IFC

// ================================================================
// Connecting signal-level interfaces

instance Connectable #(AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user),
		       AXI4_Slave_IFC  #(wd_id, wd_addr, wd_data, wd_user));

   module mkConnection #(AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user) axim,
			 AXI4_Slave_IFC  #(wd_id, wd_addr, wd_data, wd_user) axis)
		       (Empty);

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_wr_addr_channel;
	 axis.m_awvalid (axim.m_awvalid,
			 axim.m_awid,
			 axim.m_awaddr,
			 axim.m_awlen,
			 axim.m_awsize,
			 axim.m_awburst,
			 axim.m_awlock,
			 axim.m_awcache,
			 axim.m_awprot,
			 axim.m_awqos,
			 axim.m_awregion,
			 axim.m_awuser);
	 axim.m_awready (axis.m_awready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_wr_data_channel;
	 axis.m_wvalid (axim.m_wvalid,
			axim.m_wdata,
			axim.m_wstrb,
			axim.m_wlast,
			axim.m_wuser);
	 axim.m_wready (axis.m_wready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_wr_response_channel;
	 axim.m_bvalid (axis.m_bvalid,
			axis.m_bid,
			axis.m_bresp,
			axis.m_buser);
	 axis.m_bready (axim.m_bready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_rd_addr_channel;
	 axis.m_arvalid (axim.m_arvalid,
			 axim.m_arid,
			 axim.m_araddr,
			 axim.m_arlen,
			 axim.m_arsize,
			 axim.m_arburst,
			 axim.m_arlock,
			 axim.m_arcache,
			 axim.m_arprot,
			 axim.m_arqos,
			 axim.m_arregion,
			 axim.m_aruser);
	 axim.m_arready (axis.m_arready);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_rd_data_channel;
	 axim.m_rvalid (axis.m_rvalid,
			axis.m_rid,
			axis.m_rdata,
			axis.m_rresp,
			axis.m_rlast,
			axis.m_ruser);
	 axis.m_rready (axim.m_rready);
      endrule
   endmodule
endinstance

// ================================================================
// AXI4 dummy master: never produces requests, never accepts responses

AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)
    dummy_AXI4_Master_ifc = interface AXI4_Master_IFC
			       // Wr Addr channel
			       method Bool            m_awvalid  = False;              // out
			       method Bit #(wd_id)    m_awid     = ?;                  // out
			       method Bit #(wd_addr)  m_awaddr   = ?;                  // out
			       method Bit #(8)        m_awlen    = ?;                  // out
			       method AXI4_Size       m_awsize   = ?;                  // out
			       method Bit #(2)        m_awburst  = ?;                  // out
			       method Bit #(1)        m_awlock   = ?;                  // out
			       method Bit #(4)        m_awcache  = ?;                  // out
			       method Bit #(3)        m_awprot   = ?;                  // out
			       method Bit #(4)        m_awqos    = ?;                  // out
			       method Bit #(4)        m_awregion = ?;                  // out
			       method Bit #(wd_user)  m_awuser   = ?;                  // out
			       method Action m_awready (Bool awready) = noAction;      // in

			       // Wr Data channel
			       method Bool                       m_wvalid = False;     // out
			       method Bit #(wd_data)             m_wdata  = ?;         // out
			       method Bit #(TDiv #(wd_data, 8))  m_wstrb  = ?;         // out
			       method Bool                       m_wlast  = ?;         // out
			       method Bit #(wd_user)             m_wuser  = ?;         // out

			       method Action m_wready (Bool wready) = noAction;        // in

			       // Wr Response channel
			       method Action m_bvalid (Bool            bvalid,    // in
						       Bit #(wd_id)    bid,       // in
						       Bit #(2)        bresp,     // in
						       Bit #(wd_user)  buser);    // in
				  noAction;
			       endmethod
			       method Bool m_bready = False;                     // out

			       // Rd Addr channel
			       method Bool            m_arvalid  = False;             // out
			       method Bit #(wd_id)    m_arid     = ?;                 // out
			       method Bit #(wd_addr)  m_araddr   = ?;                 // out
			       method Bit #(8)        m_arlen    = ?;                 // out
			       method AXI4_Size       m_arsize   = ?;                 // out
			       method Bit #(2)        m_arburst  = ?;                 // out
			       method Bit #(1)        m_arlock   = ?;                 // out
			       method Bit #(4)        m_arcache  = ?;                 // out
			       method Bit #(3)        m_arprot   = ?;                 // out
			       method Bit #(4)        m_arqos    = ?;                 // out
			       method Bit #(4)        m_arregion = ?;                 // out
			       method Bit #(wd_user)  m_aruser   = ?;                 // out
			       method Action m_arready (Bool arready) = noAction;     // in

			       // Rd Data channel
			       method Action m_rvalid (Bool            rvalid,    // in
						       Bit #(wd_id)    rid,       // in
						       Bit #(wd_data)  rdata,     // in
						       Bit #(2)        rresp,     // in
						       Bool            rlast,     // in
						       Bit #(wd_user)  ruser);    // in
				  noAction;
			       endmethod
			       method Bool m_rready = False;                     // out
			    endinterface;

// ================================================================
// AXI4 dummy slave: never accepts requests, never produces responses

AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user)
   dummy_AXI4_Slave_ifc = interface AXI4_Slave_IFC 
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
				noAction;
			     endmethod

			     method Bool m_awready;
				return False;
			     endmethod

			     // Wr Data channel
			     method Action m_wvalid (Bool                       wvalid,
						     Bit #(wd_data)             wdata,
						     Bit #(TDiv #(wd_data, 8))  wstrb,
						     Bool                       wlast,
						     Bit #(wd_user)             wuser);
				noAction;
			     endmethod

			     method Bool m_wready;
				return False;
			     endmethod

			     // Wr Response channel
			     method Bool m_bvalid;
				return False;
			     endmethod

			     method Bit #(wd_id) m_bid;
				return ?;
			     endmethod

			     method Bit #(2) m_bresp;
				return 0;
			     endmethod

			     method Bit #(wd_user) m_buser;
				return ?;
			     endmethod

			     method Action m_bready  (Bool bready);
				noAction;
			     endmethod

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
				noAction;
			     endmethod

			     method Bool m_arready;
				return False;
			     endmethod

			     // Rd Data channel
			     method Bool m_rvalid;
				return False;
			     endmethod

			     method Bit #(wd_id) m_rid;
				return 0;
			     endmethod

			     method Bit #(wd_data) m_rdata;
				return 0;
			     endmethod

			     method Bit #(2) m_rresp;
				return 0;
			     endmethod

			     method Bool  m_rlast;
				return True;
			     endmethod

			     method Bit #(wd_user) m_ruser;
				return ?;
			     endmethod

			     method Action m_rready  (Bool rready);
				noAction;
			     endmethod
			  endinterface;

// ****************************************************************
// ****************************************************************
// Section: Higher-level FIFO-like interfaces and transactors
// ****************************************************************
// ****************************************************************

// ================================================================
// Help function: fn_crg_and_rg_to_FIFOF_I
// In the modules below, we use a crg_full and a rg_data to represent a fifo.
// These functions convert these to FIFOF_I and FIFOF_O interfaces.

function FIFOF_I #(t) fn_crg_and_rg_to_FIFOF_I (Reg #(Bool) rg_full, Reg #(t) rg_data);
   return interface FIFOF_I;
	     method Action enq (t x) if (! rg_full);
		rg_full <= True;
		rg_data <= x;
	     endmethod
	     method Bool notFull;
		return (! rg_full);
	     endmethod
	  endinterface;
endfunction

function FIFOF_O #(t) fn_crg_and_rg_to_FIFOF_O (Reg #(Bool) rg_full, Reg #(t) rg_data);
   return interface FIFOF_O;
	     method t first () if (rg_full);
		return rg_data;
	     endmethod
	     method Action deq () if (rg_full);
		rg_full <= False;
	     endmethod
	     method notEmpty;
		return rg_full;
	     endmethod
	  endinterface;
endfunction

// ================================================================
// Higher-level types for payloads (rather than just bits)

// Write Address channel

typedef struct {
   Bit #(wd_id)    awid;
   Bit #(wd_addr)  awaddr;
   Bit #(8)        awlen;
   AXI4_Size       awsize;
   Bit #(2)        awburst;
   Bit #(1)        awlock;
   Bit #(4)        awcache;
   Bit #(3)        awprot;
   Bit #(4)        awqos;
   Bit #(4)        awregion;
   Bit #(wd_user)  awuser;
   } AXI4_Wr_Addr #(numeric type wd_id,
		    numeric type wd_addr,
		    numeric type wd_user)
deriving (Bits, FShow);

// Write Data channel

typedef struct {
   Bit #(wd_data)             wdata;
   Bit #(TDiv #(wd_data, 8))  wstrb;
   Bool                       wlast;
   Bit #(wd_user)             wuser;
   } AXI4_Wr_Data #(numeric type wd_data,
		    numeric type wd_user)
deriving (Bits, FShow);

// Write Response channel

typedef struct {
   Bit #(wd_id)    bid;
   Bit #(2)        bresp;
   Bit #(wd_user)  buser;
   } AXI4_Wr_Resp #(numeric type wd_id,
		    numeric type wd_user)
deriving (Bits, FShow);

// Read Address channel

typedef struct {
   Bit #(wd_id)    arid;
   Bit #(wd_addr)  araddr;
   Bit #(8)        arlen;
   AXI4_Size       arsize;
   Bit #(2)        arburst;
   Bit #(1)        arlock;
   Bit #(4)        arcache;
   Bit #(3)        arprot;
   Bit #(4)        arqos;
   Bit #(4)        arregion;
   Bit #(wd_user)  aruser;
   } AXI4_Rd_Addr #(numeric type wd_id,
		    numeric type wd_addr,
		    numeric type wd_user)
deriving (Bits, FShow);

// Read Data channel

typedef struct {
   Bit #(wd_id)    rid;
   Bit #(wd_data)  rdata;
   Bit #(2)        rresp;
   Bool            rlast;
   Bit #(wd_user)  ruser;
   } AXI4_Rd_Data #(numeric type wd_id,
		    numeric type wd_data,
		    numeric type wd_user)
deriving (Bits, FShow);

// ================================================================
// The following are specialized 'fshow' functions for AXI4 bus
// payloads: the most common fields, and more compact.

function Fmt fshow_AXI4_Size (AXI4_Size  size);
   Fmt result = ?;
   if      (size == axsize_1)   result = $format ("sz1");
   else if (size == axsize_2)   result = $format ("sz2");
   else if (size == axsize_4)   result = $format ("sz4");
   else if (size == axsize_8)   result = $format ("sz8");
   else if (size == axsize_16)  result = $format ("sz16");
   else if (size == axsize_32)  result = $format ("sz32");
   else if (size == axsize_64)  result = $format ("sz64");
   else if (size == axsize_128) result = $format ("sz128");
   return result;
endfunction

function Fmt fshow_AXI4_Burst (AXI4_Burst  burst);
   Fmt result = ?;
   if      (burst == axburst_fixed)  result = $format ("fixed");
   else if (burst == axburst_incr)   result = $format ("incr");
   else if (burst == axburst_wrap)   result = $format ("wrap");
   else                              result = $format ("burst:%0d", burst);
   return result;
endfunction

function Fmt fshow_AXI4_Resp (AXI4_Resp  resp);
   Fmt result = ?;
   if      (resp == axi4_resp_okay)    result = $format ("okay");
   else if (resp == axi4_resp_exokay)  result = $format ("exokay");
   else if (resp == axi4_resp_slverr)  result = $format ("slverr");
   else if (resp == axi4_resp_decerr)  result = $format ("decerr");
   return result;
endfunction

// ----------------

function Fmt fshow_Wr_Addr (AXI4_Wr_Addr #(wd_id, wd_addr, wd_user) x);
   Fmt result = ($format ("{awaddr:%0h,", x.awaddr)
		 + $format ("awlen:%0d", x.awlen)
		 + $format (",")
		 + fshow_AXI4_Size (x.awsize)
		 + $format (",")
		 + fshow_AXI4_Burst (x.awburst)
		 + $format ("}"));
   return result;
endfunction

function Fmt fshow_Wr_Data (AXI4_Wr_Data #(wd_data, wd_user) x);
   let result = ($format ("{wdata:%0h,wstrb:%0h", x.wdata, x.wstrb)
		 + (x.wlast ? $format (",wlast") : $format (",.."))
		 + $format ("}"));
   return result;
endfunction

function Fmt fshow_Wr_Resp (AXI4_Wr_Resp #(wd_id, wd_user) x);
   Fmt result = ($format ("{bresp:")
		 + fshow_AXI4_Resp (x.bresp)
		 + $format ("}"));
   return result;
endfunction

function Fmt fshow_Rd_Addr (AXI4_Rd_Addr #(wd_id, wd_addr, wd_user) x);
   Fmt result = ($format ("{araddr:%0h", x.araddr)
		 + $format (",arlen:%0d", x.arlen)
		 + $format (",")
		 + fshow_AXI4_Size (x.arsize)
		 + $format (",")
		 + fshow_AXI4_Burst (x.arburst)
		 + $format ("}"));
   return result;
endfunction

function Fmt fshow_Rd_Data (AXI4_Rd_Data #(wd_id, wd_data, wd_user) x);
   Fmt result = ($format ("{rresp:")
		 + fshow_AXI4_Resp (x.rresp)
		 + $format (",rdata:%0h", x.rdata)
		 + (x.rlast ? $format (",rlast") : $format (",.."))
		 + $format ("}"));
   return result;
endfunction

// ================================================================
// AXI4 buffer

// ----------------
// Server-side interface accepts requests and yields responses

interface AXI4_Server_IFC  #(numeric type wd_id,
			     numeric type wd_addr,
			     numeric type wd_data,
			     numeric type wd_user);

   interface FIFOF_I #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  i_wr_addr;
   interface FIFOF_I #(AXI4_Wr_Data #(wd_data, wd_user))         i_wr_data;
   interface FIFOF_O #(AXI4_Wr_Resp #(wd_id, wd_user))           o_wr_resp;

   interface FIFOF_I #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  i_rd_addr;
   interface FIFOF_O #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  o_rd_data;
endinterface

// ----------------
// Client-side interface yields requests and accepts responses

interface AXI4_Client_IFC  #(numeric type wd_id,
			     numeric type wd_addr,
			     numeric type wd_data,
			     numeric type wd_user);

   interface FIFOF_O #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  o_wr_addr;
   interface FIFOF_O #(AXI4_Wr_Data #(wd_data, wd_user))         o_wr_data;
   interface FIFOF_I #(AXI4_Wr_Resp #(wd_id, wd_user))           i_wr_resp;

   interface FIFOF_O #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  o_rd_addr;
   interface FIFOF_I #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  i_rd_data;
endinterface

// ----------------
// A Buffer has a server-side and a client-side, and a reset

interface AXI4_Buffer_IFC  #(numeric type wd_id,
			     numeric type wd_addr,
			     numeric type wd_data,
			     numeric type wd_user);
   method Action reset;
   interface AXI4_Server_IFC #(wd_id, wd_addr, wd_data, wd_user) server_side;
   interface AXI4_Client_IFC #(wd_id, wd_addr, wd_data, wd_user) client_side;
endinterface

// ----------------------------------------------------------------

module mkAXI4_Buffer (AXI4_Buffer_IFC #(wd_id, wd_addr, wd_data, wd_user));

   FIFOF #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  f_wr_addr <- mkFIFOF;
   FIFOF #(AXI4_Wr_Data #(wd_data, wd_user))         f_wr_data <- mkFIFOF;
   FIFOF #(AXI4_Wr_Resp #(wd_id, wd_user))           f_wr_resp <- mkFIFOF;

   FIFOF #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  f_rd_addr <- mkFIFOF;
   FIFOF #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  f_rd_data <- mkFIFOF;

   method Action reset;
      f_wr_addr.clear;
      f_wr_data.clear;
      f_wr_resp.clear;

      f_rd_addr.clear;
      f_rd_data.clear;
   endmethod

   interface AXI4_Server_IFC server_side;
      interface i_wr_addr = to_FIFOF_I (f_wr_addr);
      interface i_wr_data = to_FIFOF_I (f_wr_data);
      interface o_wr_resp = to_FIFOF_O (f_wr_resp);

      interface i_rd_addr = to_FIFOF_I (f_rd_addr);
      interface o_rd_data = to_FIFOF_O (f_rd_data);
   endinterface

   interface AXI4_Client_IFC client_side;
      interface o_wr_addr = to_FIFOF_O (f_wr_addr);
      interface o_wr_data = to_FIFOF_O (f_wr_data);
      interface i_wr_resp = to_FIFOF_I (f_wr_resp);

      interface o_rd_addr = to_FIFOF_O (f_rd_addr);
      interface i_rd_data = to_FIFOF_I (f_rd_data);
   endinterface
endmodule

module mkAXI4_Buffer_2 (AXI4_Buffer_IFC #(wd_id, wd_addr, wd_data, wd_user));

   FIFOF #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  f_wr_addr <- mkMaster_EdgeFIFOF;
   FIFOF #(AXI4_Wr_Data #(wd_data, wd_user))         f_wr_data <- mkMaster_EdgeFIFOF;
   FIFOF #(AXI4_Wr_Resp #(wd_id, wd_user))           f_wr_resp <- mkSlave_EdgeFIFOF;

   FIFOF #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  f_rd_addr <- mkMaster_EdgeFIFOF;
   FIFOF #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  f_rd_data <- mkSlave_EdgeFIFOF;

   method Action reset;
      f_wr_addr.clear;
      f_wr_data.clear;
      f_wr_resp.clear;

      f_rd_addr.clear;
      f_rd_data.clear;
   endmethod

   interface AXI4_Server_IFC server_side;
      interface i_wr_addr = to_FIFOF_I (f_wr_addr);
      interface i_wr_data = to_FIFOF_I (f_wr_data);
      interface o_wr_resp = to_FIFOF_O (f_wr_resp);

      interface i_rd_addr = to_FIFOF_I (f_rd_addr);
      interface o_rd_data = to_FIFOF_O (f_rd_data);
   endinterface

   interface AXI4_Client_IFC client_side;
      interface o_wr_addr = to_FIFOF_O (f_wr_addr);
      interface o_wr_data = to_FIFOF_O (f_wr_data);
      interface i_wr_resp = to_FIFOF_I (f_wr_resp);

      interface o_rd_addr = to_FIFOF_O (f_rd_addr);
      interface i_rd_data = to_FIFOF_I (f_rd_data);
   endinterface
endmodule

// ================================================================
// Master transactor interface

interface AXI4_Master_Xactor_IFC #(numeric type wd_id,
				   numeric type wd_addr,
				   numeric type wd_data,
				   numeric type wd_user);
   method Action reset;

   // AXI side
   interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)  axi_side;

   // FIFOF side
   interface FIFOF_I #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  i_wr_addr;
   interface FIFOF_I #(AXI4_Wr_Data #(wd_data, wd_user))         i_wr_data;
   interface FIFOF_O #(AXI4_Wr_Resp #(wd_id, wd_user))           o_wr_resp;

   interface FIFOF_I #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  i_rd_addr;
   interface FIFOF_O #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  o_rd_data;
endinterface: AXI4_Master_Xactor_IFC

// ----------------------------------------------------------------
// Master transactor
// This version uses FIFOFs for total decoupling.

module mkAXI4_Master_Xactor (AXI4_Master_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  f_wr_addr <- mkGFIFOF (guarded, unguarded);
   FIFOF #(AXI4_Wr_Data #(wd_data, wd_user))         f_wr_data <- mkGFIFOF (guarded, unguarded);
   FIFOF #(AXI4_Wr_Resp #(wd_id, wd_user))           f_wr_resp <- mkGFIFOF (unguarded, guarded);

   FIFOF #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  f_rd_addr <- mkGFIFOF (guarded, unguarded);
   FIFOF #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  f_rd_data <- mkGFIFOF (unguarded, guarded);

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      f_wr_addr.clear;
      f_wr_data.clear;
      f_wr_resp.clear;
      f_rd_addr.clear;
      f_rd_data.clear;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Master_IFC;
			   // Wr Addr channel
			   method Bool            m_awvalid  = f_wr_addr.notEmpty;
			   method Bit #(wd_id)    m_awid     = f_wr_addr.first.awid;
			   method Bit #(wd_addr)  m_awaddr   = f_wr_addr.first.awaddr;
			   method Bit #(8)        m_awlen    = f_wr_addr.first.awlen;
			   method AXI4_Size       m_awsize   = f_wr_addr.first.awsize;
			   method Bit #(2)        m_awburst  = f_wr_addr.first.awburst;
			   method Bit #(1)        m_awlock   = f_wr_addr.first.awlock;
			   method Bit #(4)        m_awcache  = f_wr_addr.first.awcache;
			   method Bit #(3)        m_awprot   = f_wr_addr.first.awprot;
			   method Bit #(4)        m_awqos    = f_wr_addr.first.awqos;
			   method Bit #(4)        m_awregion = f_wr_addr.first.awregion;
			   method Bit #(wd_user)  m_awuser   = f_wr_addr.first.awuser;
			   method Action m_awready (Bool awready);
			      if (f_wr_addr.notEmpty && awready) f_wr_addr.deq;
			   endmethod

			   // Wr Data channel
			   method Bool                       m_wvalid = f_wr_data.notEmpty;
			   method Bit #(wd_data)             m_wdata  = f_wr_data.first.wdata;
			   method Bit #(TDiv #(wd_data, 8))  m_wstrb  = f_wr_data.first.wstrb;
			   method Bool                       m_wlast  = f_wr_data.first.wlast;
			   method Bit #(wd_user)             m_wuser  = f_wr_data.first.wuser;
			   method Action m_wready (Bool wready);
			      if (f_wr_data.notEmpty && wready) f_wr_data.deq;
			   endmethod

			   // Wr Response channel
			   method Action m_bvalid (Bool           bvalid,
						   Bit #(wd_id)   bid,
						   Bit #(2)       bresp,
						   Bit #(wd_user) buser);
			      if (bvalid && f_wr_resp.notFull)
				 f_wr_resp.enq (AXI4_Wr_Resp {bid:   bid,
							      bresp: bresp,
							      buser: buser});
			   endmethod

			   method Bool m_bready;
			      return f_wr_resp.notFull;
			   endmethod

			   // Rd Addr channel
			   method Bool            m_arvalid  = f_rd_addr.notEmpty;
			   method Bit #(wd_id)    m_arid     = f_rd_addr.first.arid;
			   method Bit #(wd_addr)  m_araddr   = f_rd_addr.first.araddr;
			   method Bit #(8)        m_arlen    = f_rd_addr.first.arlen;
			   method AXI4_Size       m_arsize   = f_rd_addr.first.arsize;
			   method Bit #(2)        m_arburst  = f_rd_addr.first.arburst;
			   method Bit #(1)        m_arlock   = f_rd_addr.first.arlock;
			   method Bit #(4)        m_arcache  = f_rd_addr.first.arcache;
			   method Bit #(3)        m_arprot   = f_rd_addr.first.arprot;
			   method Bit #(4)        m_arqos    = f_rd_addr.first.arqos;
			   method Bit #(4)        m_arregion = f_rd_addr.first.arregion;
			   method Bit #(wd_user)  m_aruser   = f_rd_addr.first.aruser;

			   method Action m_arready (Bool arready);
			      if (f_rd_addr.notEmpty && arready) f_rd_addr.deq;
			   endmethod

			   // Rd Data channel
			   method Action m_rvalid (Bool           rvalid,    // in
						   Bit #(wd_id)   rid,       // in
						   Bit #(wd_data) rdata,     // in
						   Bit #(2)       rresp,     // in
						   Bool           rlast,     // in
						   Bit #(wd_user) ruser);    // in
			      if (rvalid && f_rd_data.notFull)
				 f_rd_data.enq (AXI4_Rd_Data {rid:   rid,
							      rdata: rdata,
							      rresp: rresp,
							      rlast: rlast,
							      ruser: ruser});
			   endmethod

			   method Bool m_rready;
			      return f_rd_data.notFull;
			   endmethod

			endinterface;

   // FIFOF side
   interface i_wr_addr = to_FIFOF_I (f_wr_addr);
   interface i_wr_data = to_FIFOF_I (f_wr_data);
   interface o_wr_resp = to_FIFOF_O (f_wr_resp);

   interface i_rd_addr = to_FIFOF_I (f_rd_addr);
   interface o_rd_data = to_FIFOF_O (f_rd_data);
endmodule: mkAXI4_Master_Xactor

// ----------------------------------------------------------------
// Master transactor
// This version uses crgs and regs instead of FIFOFs.
// This uses 1/2 the resources, but introduces scheduling dependencies.

module mkAXI4_Master_Xactor_2 (AXI4_Master_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user));

   // Each crg_full, rg_data pair below represents a 1-element fifo.

   Array #(Reg #(Bool))                            crg_wr_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  rg_wr_addr <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_data_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Data #(wd_data, wd_user))         rg_wr_data <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_resp_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Resp #(wd_id, wd_user))           rg_wr_resp <- mkRegU;

   Array #(Reg #(Bool))                            crg_rd_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  rg_rd_addr <- mkRegU;

   Array #(Reg #(Bool))                            crg_rd_data_full <- mkCReg (3, False);
   Reg #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  rg_rd_data <- mkRegU;

   // The following CReg port indexes specify the relative scheduling of:
   //     {first,deq,notEmpty}    {enq,notFull}    clear

   // TODO: 'deq/enq/clear = 1/2/0' is unusual, but eliminates a
   // scheduling cycle in Piccolo's DCache.  Normally should be 0/1/2.

   Integer port_deq   = 1;
   Integer port_enq   = 2;
   Integer port_clear = 0;

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      crg_wr_addr_full [port_clear] <= False;
      crg_wr_data_full [port_clear] <= False;
      crg_wr_resp_full [port_clear] <= False;
      crg_rd_addr_full [port_clear] <= False;
      crg_rd_data_full [port_clear] <= False;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Master_IFC;
			   // Wr Addr channel
			   method Bool           m_awvalid  = crg_wr_addr_full [port_deq];
			   method Bit #(wd_id)   m_awid     = rg_wr_addr.awid;
			   method Bit #(wd_addr) m_awaddr   = rg_wr_addr.awaddr;
			   method Bit #(8)       m_awlen    = rg_wr_addr.awlen;
			   method AXI4_Size      m_awsize   = rg_wr_addr.awsize;
			   method Bit #(2)       m_awburst  = rg_wr_addr.awburst;
			   method Bit #(1)       m_awlock   = rg_wr_addr.awlock;
			   method Bit #(4)       m_awcache  = rg_wr_addr.awcache;
			   method Bit #(3)       m_awprot   = rg_wr_addr.awprot;
			   method Bit #(4)       m_awqos    = rg_wr_addr.awqos;
			   method Bit #(4)       m_awregion = rg_wr_addr.awregion;
			   method Bit #(wd_user) m_awuser   = rg_wr_addr.awuser;
			   method Action m_awready (Bool awready);
			      if (crg_wr_addr_full [port_deq] && awready)
				 crg_wr_addr_full [port_deq] <= False;    // deq
			   endmethod

			   // Wr Data channel
			   method Bool                       m_wvalid = crg_wr_data_full [port_deq];
			   method Bit #(wd_data)             m_wdata  = rg_wr_data.wdata;
			   method Bit #(TDiv #(wd_data, 8))  m_wstrb  = rg_wr_data.wstrb;
			   method Bool                       m_wlast  = rg_wr_data.wlast;
			   method Bit #(wd_user)             m_wuser  = rg_wr_data.wuser;
			   method Action m_wready (Bool wready);
			      if (crg_wr_data_full [port_deq] && wready)
				 crg_wr_data_full [port_deq] <= False;
			   endmethod

			   // Wr Response channel
			   method Action m_bvalid (Bool            bvalid,
						   Bit #(wd_id)    bid,
						   Bit #(2)        bresp,
						   Bit #(wd_user)  buser);
			      if (bvalid && (! (crg_wr_resp_full [port_enq]))) begin
				 crg_wr_resp_full [port_enq] <= True;
				 rg_wr_resp <= AXI4_Wr_Resp {bid:   bid,
							     bresp: bresp,
							     buser: buser};
			      end
			   endmethod

			   method Bool m_bready;
			      return (! (crg_wr_resp_full [port_enq]));
			   endmethod

			   // Rd Addr channel
			   method Bool            m_arvalid = crg_rd_addr_full [port_deq];
			   method Bit #(wd_id)    m_arid     = rg_rd_addr.arid;
			   method Bit #(wd_addr)  m_araddr   = rg_rd_addr.araddr;
			   method Bit #(8)        m_arlen    = rg_rd_addr.arlen;
			   method AXI4_Size       m_arsize   = rg_rd_addr.arsize;
			   method Bit #(2)        m_arburst  = rg_rd_addr.arburst;
			   method Bit #(1)        m_arlock   = rg_rd_addr.arlock;
			   method Bit #(4)        m_arcache  = rg_rd_addr.arcache;
			   method Bit #(3)        m_arprot   = rg_rd_addr.arprot;
			   method Bit #(4)        m_arqos    = rg_rd_addr.arqos;
			   method Bit #(4)        m_arregion = rg_rd_addr.arregion;
			   method Bit #(wd_user)  m_aruser   = rg_rd_addr.aruser;
			   method Action m_arready (Bool arready);
			      if (crg_rd_addr_full [port_deq] && arready)
				 crg_rd_addr_full [port_deq] <= False;    // deq
			   endmethod

			   // Rd Data channel
			   method Action m_rvalid (Bool            rvalid,
						   Bit #(wd_id)    rid,
						   Bit #(wd_data)  rdata,
						   Bit #(2)        rresp,
						   Bool            rlast,
						   Bit #(wd_user)  ruser);
			      if (rvalid && (! (crg_rd_data_full [port_enq])))
				 crg_rd_data_full [port_enq] <= True;
				 rg_rd_data <= (AXI4_Rd_Data {rid:   rid,
							      rdata: rdata,
							      rresp: rresp,
							      rlast: rlast,
							      ruser: ruser});
			   endmethod

			   method Bool m_rready;
			      return (! (crg_rd_data_full [port_enq]));
			   endmethod

			endinterface;

   // FIFOF side
   interface i_wr_addr = fn_crg_and_rg_to_FIFOF_I (crg_wr_addr_full [port_enq], rg_wr_addr);
   interface i_wr_data = fn_crg_and_rg_to_FIFOF_I (crg_wr_data_full [port_enq], rg_wr_data);
   interface o_wr_resp = fn_crg_and_rg_to_FIFOF_O (crg_wr_resp_full [port_deq], rg_wr_resp);

   interface i_rd_addr = fn_crg_and_rg_to_FIFOF_I (crg_rd_addr_full [port_enq], rg_rd_addr);
   interface o_rd_data = fn_crg_and_rg_to_FIFOF_O (crg_rd_data_full [port_deq], rg_rd_data);
endmodule: mkAXI4_Master_Xactor_2

// ================================================================
// Slave transactor interface

interface AXI4_Slave_Xactor_IFC #(numeric type wd_id,
				  numeric type wd_addr,
				  numeric type wd_data,
				  numeric type wd_user);
   method Action reset;

   // AXI side
   interface AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user) axi_side;

   // FIFOF side
   interface FIFOF_O #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  o_wr_addr;
   interface FIFOF_O #(AXI4_Wr_Data #(wd_data, wd_user))         o_wr_data;
   interface FIFOF_I #(AXI4_Wr_Resp #(wd_id, wd_user))           i_wr_resp;

   interface FIFOF_O #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  o_rd_addr;
   interface FIFOF_I #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  i_rd_data;
endinterface: AXI4_Slave_Xactor_IFC

// ----------------------------------------------------------------
// Slave transactor
// This version uses FIFOFs for total decoupling.

module mkAXI4_Slave_Xactor (AXI4_Slave_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  f_wr_addr <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Wr_Data #(wd_data, wd_user))         f_wr_data <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Wr_Resp #(wd_id, wd_user))           f_wr_resp <- mkGFIFOF (guarded, unguarded);

   FIFOF #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  f_rd_addr <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  f_rd_data <- mkGFIFOF (guarded, unguarded);

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      f_wr_addr.clear;
      f_wr_data.clear;
      f_wr_resp.clear;
      f_rd_addr.clear;
      f_rd_data.clear;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Slave_IFC;
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
			      if (awvalid && f_wr_addr.notFull)
				 f_wr_addr.enq (AXI4_Wr_Addr {awid:     awid,
							      awaddr:   awaddr,
							      awlen:    awlen,
							      awsize:   awsize,
							      awburst:  awburst,
							      awlock:   awlock,
							      awcache:  awcache,
							      awprot:   awprot,
							      awqos:    awqos,
							      awregion: awregion,
							      awuser:   awuser});
			   endmethod

			   method Bool m_awready;
			      return f_wr_addr.notFull;
			   endmethod

			   // Wr Data channel
			   method Action m_wvalid (Bool                       wvalid,
						   Bit #(wd_data)             wdata,
						   Bit #(TDiv #(wd_data, 8))  wstrb,
						   Bool                       wlast,
						   Bit #(wd_user)             wuser);
			      if (wvalid && f_wr_data.notFull)
				 f_wr_data.enq (AXI4_Wr_Data {wdata: wdata,
							      wstrb: wstrb,
							      wlast: wlast,
							      wuser: wuser});
			   endmethod

			   method Bool m_wready;
			      return f_wr_data.notFull;
			   endmethod

			   // Wr Response channel
			   method Bool           m_bvalid = f_wr_resp.notEmpty;
			   method Bit #(wd_id)   m_bid    = f_wr_resp.first.bid;
			   method Bit #(2)       m_bresp  = f_wr_resp.first.bresp;
			   method Bit #(wd_user) m_buser  = f_wr_resp.first.buser;
			   method Action m_bready (Bool bready);
			      if (bready && f_wr_resp.notEmpty)
				 f_wr_resp.deq;
			   endmethod

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
			      if (arvalid && f_rd_addr.notFull)
				 f_rd_addr.enq (AXI4_Rd_Addr {arid:     arid,
							      araddr:   araddr,
							      arlen:    arlen,
							      arsize:   arsize,
							      arburst:  arburst,
							      arlock:   arlock,
							      arcache:  arcache,
							      arprot:   arprot,
							      arqos:    arqos,
							      arregion: arregion,
							      aruser:   aruser});
			   endmethod

			   method Bool m_arready;
			      return f_rd_addr.notFull;
			   endmethod

			   // Rd Data channel
			   method Bool           m_rvalid = f_rd_data.notEmpty;
			   method Bit #(wd_id)   m_rid    = f_rd_data.first.rid;
			   method Bit #(wd_data) m_rdata  = f_rd_data.first.rdata;
			   method Bit #(2)       m_rresp  = f_rd_data.first.rresp;
			   method Bool           m_rlast  = f_rd_data.first.rlast;
			   method Bit #(wd_user) m_ruser  = f_rd_data.first.ruser;
			   method Action m_rready (Bool rready);
			      if (rready && f_rd_data.notEmpty)
				 f_rd_data.deq;
			   endmethod
			endinterface;

   // FIFOF side
   interface o_wr_addr = to_FIFOF_O (f_wr_addr);
   interface o_wr_data = to_FIFOF_O (f_wr_data);
   interface i_wr_resp = to_FIFOF_I (f_wr_resp);

   interface o_rd_addr = to_FIFOF_O (f_rd_addr);
   interface i_rd_data = to_FIFOF_I (f_rd_data);
endmodule: mkAXI4_Slave_Xactor

// ----------------------------------------------------------------
// Slave transactor
// This version uses crgs and regs instead of FIFOFs.
// This uses 1/2 the resources, but introduces scheduling dependencies.

module mkAXI4_Slave_Xactor_2 (AXI4_Slave_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user));

   // Each crg_full, rg_data pair below represents a 1-element fifo.

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   Array #(Reg #(Bool))                            crg_wr_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user))  rg_wr_addr <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_data_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Data #(wd_data, wd_user))         rg_wr_data <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_resp_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Resp #(wd_id, wd_user))           rg_wr_resp <- mkRegU;

   Array #(Reg #(Bool))                            crg_rd_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Rd_Addr #(wd_id, wd_addr, wd_user))  rg_rd_addr <- mkRegU;

   Array #(Reg #(Bool))                            crg_rd_data_full <- mkCReg (3, False);
   Reg #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  rg_rd_data <- mkRegU;

   // The following CReg port indexes specify the relative scheduling of:
   //     {first,deq,notEmpty}    {enq,notFull}    clear
   Integer port_deq   = 0;
   Integer port_enq   = 1;
   Integer port_clear = 2;

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      crg_wr_addr_full [port_clear] <= False;
      crg_wr_data_full [port_clear] <= False;
      crg_wr_resp_full [port_clear] <= False;
      crg_rd_addr_full [port_clear] <= False;
      crg_rd_data_full [port_clear] <= False;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Slave_IFC;
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

			      if (awvalid && (! crg_wr_addr_full [port_enq])) begin
				 crg_wr_addr_full [port_enq] <= True;    // enq
				 rg_wr_addr <= AXI4_Wr_Addr {awid:     awid,
							     awaddr:   awaddr,
							     awlen:    awlen,
							     awsize:   awsize,
							     awburst:  awburst,
							     awlock:   awlock,
							     awcache:  awcache,
							     awprot:   awprot,
							     awqos:    awqos,
							     awregion: awregion,
							     awuser:   awuser};
			      end
			   endmethod

			   method Bool m_awready;
			      return (! crg_wr_addr_full [port_enq]);
			   endmethod

			   // Wr Data channel
			   method Action m_wvalid (Bool                       wvalid,
						   Bit #(wd_data)             wdata,
						   Bit #(TDiv #(wd_data, 8))  wstrb,
						   Bool                       wlast,
						   Bit #(wd_user)             wuser);
			      if (wvalid && (! crg_wr_data_full [port_enq])) begin
				 crg_wr_data_full [port_enq] <= True;    // enq
				 rg_wr_data <= AXI4_Wr_Data {wdata: wdata,
							     wstrb: wstrb,
							     wlast: wlast,
							     wuser: wuser};
			      end
			   endmethod

			   method Bool m_wready;
			      return (! crg_wr_data_full [port_enq]);
			   endmethod

			   // Wr Response channel
			   method Bool           m_bvalid = crg_wr_resp_full [port_deq];
			   method Bit #(wd_id)   m_bid    = rg_wr_resp.bid;
			   method Bit #(2)       m_bresp  = rg_wr_resp.bresp;
			   method Bit #(wd_user) m_buser  = rg_wr_resp.buser;
			   method Action m_bready (Bool bready);
			      if (bready && crg_wr_resp_full [port_deq])
				 crg_wr_resp_full [port_deq] <= False;    // deq
			   endmethod

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
			      if (arvalid && (! crg_rd_addr_full [port_enq])) begin
				 crg_rd_addr_full [port_enq] <= True;    // enq
				 rg_rd_addr <= AXI4_Rd_Addr {arid:     arid,
							     araddr:   araddr,
							     arlen:    arlen,
							     arsize:   arsize,
							     arburst:  arburst,
							     arlock:   arlock,
							     arcache:  arcache,
							     arprot:   arprot,
							     arqos:    arqos,
							     arregion: arregion,
							     aruser:   aruser};
			      end
			   endmethod

			   method Bool m_arready;
			      return (! crg_rd_addr_full [port_enq]);
			   endmethod

			   // Rd Data channel
			   method Bool           m_rvalid = crg_rd_data_full [port_deq];
			   method Bit #(wd_id)   m_rid    = rg_rd_data.rid;
			   method Bit #(wd_data) m_rdata  = rg_rd_data.rdata;
			   method Bit #(2)       m_rresp  = rg_rd_data.rresp;
			   method Bool           m_rlast  = rg_rd_data.rlast;
			   method Bit #(wd_user) m_ruser  = rg_rd_data.ruser;
			   method Action m_rready (Bool rready);
			      if (rready && crg_rd_data_full [port_deq])
				 crg_rd_data_full [port_deq] <= False;    // deq
			   endmethod
			endinterface;

   // FIFOF side
   interface o_wr_addr = fn_crg_and_rg_to_FIFOF_O (crg_wr_addr_full [port_deq], rg_wr_addr);
   interface o_wr_data = fn_crg_and_rg_to_FIFOF_O (crg_wr_data_full [port_deq], rg_wr_data);
   interface i_wr_resp = fn_crg_and_rg_to_FIFOF_I (crg_wr_resp_full [port_enq], rg_wr_resp);

   interface o_rd_addr = fn_crg_and_rg_to_FIFOF_O (crg_rd_addr_full [port_deq], rg_rd_addr);
   interface i_rd_data = fn_crg_and_rg_to_FIFOF_I (crg_rd_data_full [port_enq], rg_rd_data);
endmodule: mkAXI4_Slave_Xactor_2

// ================================================================

endpackage
