// Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved

package AXI4_Stream;

// ================================================================
// BSV library imports

import FIFOF       :: *;
import Connectable :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;
import EdgeFIFOFs :: *;
import AXI4_Types :: *;

// ================================================================
// These are the signal-level interfaces for an AXI4 stream master.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Stream_Master_IFC #(numeric type wd_id,
				   numeric type wd_dest,
				   numeric type wd_data,
				   numeric type wd_user);

   (* always_ready, result="tvalid" *)  method Bool                      m_tvalid;    // out
   (* always_ready, result="tid" *)     method Bit #(wd_id)              m_tid;       // out
   (* always_ready, result="tdata" *)   method Bit #(wd_data)            m_tdata;     // out
   (* always_ready, result="tstrb" *)   method Bit #(TDiv #(wd_data, 8)) m_tstrb;     // out
   (* always_ready, result="tkeep" *)   method Bit #(TDiv #(wd_data, 8)) m_tkeep;     // out
   (* always_ready, result="tlast" *)   method Bool                      m_tlast;     // out
   (* always_ready, result="tdest" *)   method Bit #(wd_dest)            m_tdest;     // out
   (* always_ready, result="tuser" *)   method Bit #(wd_user)            m_tuser;     // out

   (* always_ready, always_enabled, prefix = "" *)
   method Action m_tready ((* port="tready" *)  Bool tready);                         // in

endinterface: AXI4_Stream_Master_IFC

// ================================================================
// These are the signal-level interfaces for an AXI4 stream slave.
// The (*..*) attributes ensure that when bsc compiles this to Verilog,
// we get exactly the signals specified in the ARM spec.

interface AXI4_Stream_Slave_IFC #(numeric type wd_id,
				  numeric type wd_dest,
				  numeric type wd_data,
				  numeric type wd_user);
   (* always_ready, always_enabled, prefix = "" *)
   method Action m_tvalid ((* port="tvalid" *) Bool                      tvalid,    // in
			   (* port="tid" *)    Bit #(wd_id)              tid,       // in
			   (* port="tdata" *)  Bit #(wd_data)            tdata,     // in
			   (* port="tstrb" *)  Bit #(TDiv #(wd_data,8))  tstrb,     // in
			   (* port="tkeep" *)  Bit #(TDiv #(wd_data,8))  tkeep,     // in
			   (* port="tlast" *)  Bool                      tlast,     // in
			   (* port="tdest" *)  Bit #(wd_dest)            tdest,     // in
			   (* port="tuser" *)  Bit #(wd_user)            tuser);    // in
   (* always_ready, result="tready" *)
   method Bool m_tready;                                                           // out
endinterface: AXI4_Stream_Slave_IFC

// ================================================================
// Connecting signal-level interfaces

instance Connectable #(AXI4_Stream_Master_IFC #(wd_id, wd_dest, wd_data, wd_user),
		       AXI4_Stream_Slave_IFC  #(wd_id, wd_dest, wd_data, wd_user));

   module mkConnection #(AXI4_Stream_Master_IFC #(wd_id, wd_dest, wd_data, wd_user) axim,
			 AXI4_Stream_Slave_IFC  #(wd_id, wd_dest, wd_data, wd_user) axis)
		       (Empty);

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_data_channel;
	 axis.m_tvalid (axim.m_tvalid,
			axim.m_tid,
			axim.m_tdata,
			axim.m_tstrb,
			axim.m_tkeep,
			axim.m_tlast,
			axim.m_tdest,
			axim.m_tuser);
	 axim.m_tready (axis.m_tready);
      endrule
   endmodule
endinstance

instance Connectable #(AXI4_Stream_Slave_IFC  #(wd_id, wd_dest, wd_data, wd_user),
		       AXI4_Stream_Master_IFC #(wd_id, wd_dest, wd_data, wd_user));
   module mkConnection #(AXI4_Stream_Slave_IFC  #(wd_id, wd_dest, wd_data, wd_user) axis,
			 AXI4_Stream_Master_IFC #(wd_id, wd_dest, wd_data, wd_user) axim)
			 		       (Empty);
      mkConnection(axim, axis);
   endmodule
endinstance

// ================================================================
// AXI4 dummy master: never produces requests

AXI4_Stream_Master_IFC #(wd_id, wd_dest, wd_data, wd_user) axi4_stream_dummy_master
  = interface AXI4_Stream_Master_IFC
       method m_tvalid = False;     // out
       method m_tid    = ?;         // out
       method m_tdata  = ?;         // out
       method m_tstrb  = ?;         // out
       method m_tkeep  = ?;         // out
       method m_tlast  = ?;         // out
       method m_tdest  = ?;         // out
       method m_tuser  = ?;         // out

       method Action m_tready (wready) = noAction;        // in
    endinterface;

// ================================================================
// AXI4 dummy slave: always accepts requests

AXI4_Stream_Slave_IFC #(wd_id, wd_dest, wd_data, wd_user) axi4_stream_dummy_slave
   = interface AXI4_Stream_Slave_IFC
	method Action m_tvalid (wvalid,
				wid,
				wdata,
				wstrb,
				wkeep,
				wlast,
				wdest,
				wuser);
	   noAction;
	endmethod
	method Bool m_tready = True;
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

typedef struct {
   Bit #(wd_id)               tid;
   Bit #(wd_data)             tdata;
   Bit #(TDiv #(wd_data, 8))  tstrb;
   Bit #(TDiv #(wd_data, 8))  tkeep;
   Bool                       tlast;
   Bit #(wd_dest)             tdest;
   Bit #(wd_user)             tuser;
   } AXI4_Stream #(numeric type wd_id,
		   numeric type wd_dest,
		   numeric type wd_data,
		   numeric type wd_user)
deriving (Bits, FShow);

// ================================================================
// Master transactor interface

interface AXI4_Stream_Master_Xactor_IFC #(numeric type wd_id,
					  numeric type wd_dest,
					  numeric type wd_data,
					  numeric type wd_user);
   method Action reset;
   // AXI side
   interface AXI4_Stream_Master_IFC #(wd_id, wd_dest, wd_data, wd_user)  axi_side;
   // FIFOF side
   interface FIFOF_I #(AXI4_Stream #(wd_id, wd_dest, wd_data, wd_user))  i_stream;
endinterface: AXI4_Stream_Master_Xactor_IFC

// ----------------------------------------------------------------
// Master transactor
// This version uses FIFOFs for total decoupling.

module mkAXI4_Stream_Master_Xactor (AXI4_Stream_Master_Xactor_IFC #(wd_id, wd_dest, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // Guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Stream #(wd_id, wd_dest, wd_data, wd_user))  f_data <- mkGFIFOF (guarded, unguarded);


   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      f_data.clear;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Stream_Master_IFC;
			   method m_tvalid = f_data.notEmpty;
			   method m_tid    = f_data.first.tid;
			   method m_tdata  = f_data.first.tdata;
			   method m_tstrb  = f_data.first.tstrb;
			   method m_tkeep  = f_data.first.tkeep;
			   method m_tlast  = f_data.first.tlast;
			   method m_tdest  = f_data.first.tdest;
			   method m_tuser  = f_data.first.tuser;
			   method Action m_tready (Bool tready);
			      if (f_data.notEmpty && tready) f_data.deq;
			   endmethod
			endinterface;

   // FIFOF side
   interface i_stream = to_FIFOF_I (f_data);
endmodule: mkAXI4_Stream_Master_Xactor

// ================================================================
// Slave transactor interface

interface AXI4_Stream_Slave_Xactor_IFC #(numeric type wd_id,
					 numeric type wd_dest,
					 numeric type wd_data,
					 numeric type wd_user);
   method Action reset;
   // AXI side
   interface AXI4_Stream_Slave_IFC #(wd_id, wd_dest, wd_data, wd_user)  axi_side;
   // FIFOF side
   interface FIFOF_O #(AXI4_Stream #(wd_id, wd_dest, wd_data, wd_user)) o_stream;
endinterface: AXI4_Stream_Slave_Xactor_IFC

// ----------------------------------------------------------------
// Slave transactor
// This version uses FIFOFs for total decoupling.

module mkAXI4_Stream_Slave_Xactor (AXI4_Stream_Slave_Xactor_IFC #(wd_id, wd_dest, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // Guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Stream #(wd_id, wd_dest, wd_data, wd_user))  f_data <- mkGFIFOF (unguarded, guarded);

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      f_data.clear;
   endmethod

   // AXI side
   interface axi_side = interface AXI4_Stream_Slave_IFC;
			   method Action m_tvalid (Bool                       tvalid,
						   Bit #(wd_id)               tid,
						   Bit #(wd_data)             tdata,
						   Bit #(TDiv #(wd_data, 8))  tstrb,
						   Bit #(TDiv #(wd_data, 8))  tkeep,
						   Bool                       tlast,
						   Bit #(wd_dest)             tdest,
						   Bit #(wd_user)             tuser);
			      if (tvalid && f_data.notFull)
				 f_data.enq (AXI4_Stream {tid:   tid,
							  tdata: tdata,
							  tstrb: tstrb,
							  tkeep: tkeep,
							  tlast: tlast,
							  tdest: tdest,
							  tuser: tuser});
			   endmethod

			   method Bool m_tready;
			      return f_data.notFull;
			   endmethod
			endinterface;

   // FIFOF side
   interface o_stream  = to_FIFOF_O (f_data);
endmodule: mkAXI4_Stream_Slave_Xactor

/*
// ----------------------------------------------------------------
// Master transactor
// This version uses crgs and regs instead of FIFOFs.
// This uses 1/2 the resources, but introduces scheduling dependencies.

module mkAXI4_Master_Xactor_2 (AXI4_Master_Xactor_IFC #(wd_id, wd_dest, wd_data, wd_user));

   // Each crg_full, rg_data pair below represents a 1-element fifo.

   Array #(Reg #(Bool))                            crg_wr_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Addr #(wd_id, wd_dest, wd_user))  rg_wr_addr <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_data_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Data #(wd_id, wd_data, wd_user))  rg_wr_data <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_resp_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Resp #(wd_id, wd_user))           rg_wr_resp <- mkRegU;

   Array #(Reg #(Bool))                            crg_rd_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Rd_Addr #(wd_id, wd_dest, wd_user))  rg_rd_addr <- mkRegU;

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
			   method Bit #(wd_dest) m_awaddr   = rg_wr_addr.awaddr;
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
			   method Bit #(wd_id)               m_wid    = rg_wr_data.wid;
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
			   method Bit #(wd_dest)  m_araddr   = rg_rd_addr.araddr;
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
				  numeric type wd_dest,
				  numeric type wd_data,
				  numeric type wd_user);
   method Action reset;

   // AXI side
   interface AXI4_Slave_IFC #(wd_id, wd_dest, wd_data, wd_user) axi_side;

   // FIFOF side
   interface FIFOF_O #(AXI4_Wr_Addr #(wd_id, wd_dest, wd_user))  o_wr_addr;
   interface FIFOF_O #(AXI4_Wr_Data #(wd_id, wd_data, wd_user))  o_wr_data;
   interface FIFOF_I #(AXI4_Wr_Resp #(wd_id, wd_user))           i_wr_resp;

   interface FIFOF_O #(AXI4_Rd_Addr #(wd_id, wd_dest, wd_user))  o_rd_addr;
   interface FIFOF_I #(AXI4_Rd_Data #(wd_id, wd_data, wd_user))  i_rd_data;
endinterface: AXI4_Slave_Xactor_IFC

// ----------------------------------------------------------------
// Slave transactor
// This version uses FIFOFs for total decoupling.

module mkAXI4_Slave_Xactor (AXI4_Slave_Xactor_IFC #(wd_id, wd_dest, wd_data, wd_user));

   Bool unguarded = True;
   Bool guarded   = False;

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   FIFOF #(AXI4_Wr_Addr #(wd_id, wd_dest, wd_user))  f_wr_addr <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Wr_Data #(wd_id, wd_data, wd_user))  f_wr_data <- mkGFIFOF (unguarded, guarded);
   FIFOF #(AXI4_Wr_Resp #(wd_id, wd_user))           f_wr_resp <- mkGFIFOF (guarded, unguarded);

   FIFOF #(AXI4_Rd_Addr #(wd_id, wd_dest, wd_user))  f_rd_addr <- mkGFIFOF (unguarded, guarded);
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
						    Bit #(wd_dest)  awaddr,
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
						   Bit #(wd_id)               wid,
						   Bit #(wd_data)             wdata,
						   it #(TDiv #(wd_data, 8))  wstrb,
						   Bool                       wlast,
						   Bit #(wd_user)             wuser);
			      if (wvalid && f_wr_data.notFull)
				 f_wr_data.enq (AXI4_Wr_Data {wid:   wid,
							      wdata: wdata,
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
						    Bit #(wd_dest)  araddr,
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

module mkAXI4_Slave_Xactor_2 (AXI4_Slave_Xactor_IFC #(wd_id, wd_dest, wd_data, wd_user));

   // Each crg_full, rg_data pair below represents a 1-element fifo.

   // These FIFOs are guarded on BSV side, unguarded on AXI side
   Array #(Reg #(Bool))                            crg_wr_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Addr #(wd_id, wd_dest, wd_user))  rg_wr_addr <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_data_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Data #(wd_id, wd_data, wd_user))  rg_wr_data <- mkRegU;

   Array #(Reg #(Bool))                            crg_wr_resp_full <- mkCReg (3, False);
   Reg #(AXI4_Wr_Resp #(wd_id, wd_user))           rg_wr_resp <- mkRegU;

   Array #(Reg #(Bool))                            crg_rd_addr_full <- mkCReg (3, False);
   Reg #(AXI4_Rd_Addr #(wd_id, wd_dest, wd_user))  rg_rd_addr <- mkRegU;

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
						    Bit #(wd_dest)  awaddr,
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
						   Bit #(wd_id)               wid,
						   Bit #(wd_data)             wdata,
						   Bit #(TDiv #(wd_data, 8))  wstrb,
						   Bool                       wlast,
						   Bit #(wd_user)             wuser);
			      if (wvalid && (! crg_wr_data_full [port_enq])) begin
				 crg_wr_data_full [port_enq] <= True;    // enq
				 rg_wr_data <= AXI4_Wr_Data {wid:   wid,
							     wdata: wdata,
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
						    Bit #(wd_dest)  araddr,
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
*/
// ================================================================

endpackage
