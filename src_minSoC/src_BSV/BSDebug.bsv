// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package BSDebug;

// ================================================================
// This package defines the interface and implementation of the 'P1 Core'
// for the DARPA SSITH project.
// This P1 core contains:
//    - Piccolo CPU, including
//        - Near_Mem (ICache and DCache)
//        - Near_Mem_IO (Timer, Software-interrupt, and other mem-mapped-locations)
//        - External interrupt request lines
//        - 2 x AXI4 Master interfaces (from DM and ICache, and from DCache)
//    - RISC-V Debug Module (DM)
//    - JTAG TAP interface for DM
//    - Optional Tandem Verification trace stream output interface

// ================================================================
// BSV library imports

import Vector        :: *;
import FIFO          :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;
import Bus           :: *;
import Clocks        :: *;
import RegUInit      :: *;

// ----------------
// BSV additional libs

import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import SoC_Map  :: *;

// Main Fabric
import AXI4_Types   :: *;
import Fabric_Defs  :: *;

import Debug_Module :: *;
import Debug_Interfaces :: *;
import DM_CPU_Req_Rsp::*;
import Jtag         :: *;
import JtagTap      :: *;
import Giraffe_IFC  :: *;

// ================================================================
// Constant: cycles to hold SoC in reset for ndm reset:

UInt#(6) ndm_interval = 20;
UInt#(6) por_interval = 20;

// ================================================================
// The BSCore interface

interface BSDebug_IFC;
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) toCore;
   interface Reset ndm_resetn;
   interface JTAG_IFC jtag;
endinterface

// ================================================================

(* synthesize *)
// reset by power-on reset
module mkBSDebug ((*reset="dmi_reset"*)Reset dmi_reset, BSDebug_IFC _ifc);
//   let dmi_resetN <- mkResetInverter(dmi_reset); // dmi_reset is active-high
   let dmi_resetN <- exposeCurrentReset;

   // ----------------

   let debug_module <- mkDebug_Module;

   // ----------------
   // Instantiate JTAG TAP controller,
   // connect to core.dm_dmi;
   // and export its JTAG interface

   Wire#(Bit#(7)) w_dmi_req_addr <- mkDWire(0);
   Wire#(Bit#(32)) w_dmi_req_data <- mkDWire(0);
   Wire#(Bit#(2)) w_dmi_req_op <- mkDWire(0);

   Wire#(Bit#(32)) w_dmi_rsp_data <- mkDWire(0);
   Wire#(Bit#(2)) w_dmi_rsp_response <- mkDWire(0);

   BusReceiver#(Tuple3#(Bit#(7),Bit#(32),Bit#(2))) bus_dmi_req <- mkBusReceiver(reset_by dmi_resetN);
   BusSender#(Tuple2#(Bit#(32),Bit#(2))) bus_dmi_rsp <- mkBusSender(unpack(0), reset_by dmi_resetN);

   let jtagtap <- mkJtagTap(reset_by dmi_resetN);

   mkConnection(jtagtap.dmi.req_ready, pack(bus_dmi_req.in.ready), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_valid, compose(bus_dmi_req.in.valid, unpack), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_addr, w_dmi_req_addr._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_data, w_dmi_req_data._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_op, w_dmi_req_op._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_valid, pack(bus_dmi_rsp.out.valid), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_ready, compose(bus_dmi_rsp.out.ready, unpack), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_data, w_dmi_rsp_data, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_response, w_dmi_rsp_response, reset_by dmi_resetN);

   rule rl_dmi_req;
      bus_dmi_req.in.data(tuple3(w_dmi_req_addr, w_dmi_req_data, w_dmi_req_op));
   endrule

   rule rl_dmi_rsp;
      match {.data, .response} = bus_dmi_rsp.out.data;
      w_dmi_rsp_data <= data;
      w_dmi_rsp_response <= response;
   endrule

   (* preempts = "rl_dmi_req_cpu, rl_dmi_rsp_cpu" *)
   rule rl_dmi_req_cpu;
      match {.addr, .data, .op} = bus_dmi_req.out.first;
      bus_dmi_req.out.deq;
      case (op)
	 1: debug_module.dmi.read_addr(addr);
	 2: begin
	       debug_module.dmi.write(addr, data);
	       bus_dmi_rsp.in.enq(tuple2(?, 0));
	    end
	 default: bus_dmi_rsp.in.enq(tuple2(?, 2));
      endcase
   endrule

   rule rl_dmi_rsp_cpu;
      let data <- debug_module.dmi.read_data;
      bus_dmi_rsp.in.enq(tuple2(data, 0));
   endrule

   // ----------------
   // Reset behaviour:
   // ndm-reset resets hart and also asserts separate ndm_resetn signal.

   Bool isNDMreset  = True;
   Bool isHARTreset = !isNDMreset;

   FIFO #(Bool) req_FF   <- mkFIFO1;
   FIFO #(Bool) rsp_FF   <- mkFIFO1;
   FIFO #(Bool) which_FF <- mkFIFO1;

   let clk      <- exposeCurrentClock();
   let resetIfc <- mkReset(2, True, clk);
   let ndm_rstn  = resetIfc.new_rst;

   rule hart_reset_rl;
      let running <- debug_module.hart0.hart_reset_client.request.get();
      req_FF.enq(running);
      which_FF.enq(isHARTreset);
   endrule

   rule ndm_reset_rl;
      let running <- debug_module.ndm_reset_client.request.get();
      req_FF.enq(running);
      which_FF.enq(isNDMreset);
      resetIfc.assertReset;
   endrule

   rule reset_rsp_rl;
      let running <- toGet(rsp_FF).get();
      let which   <- toGet(which_FF).get();
      if (which == isNDMreset)
	 debug_module.ndm_reset_client.response.put(running);
      else
	 debug_module.hart0.hart_reset_client.response.put(running);
   endrule

   // ----------------

   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) coreXactor <- mkAXI4_Master_Xactor;
   AXI4_Slave_Xactor_IFC  #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) dmXactor   <- mkAXI4_Slave_Xactor;
   mkConnection(debug_module.master, dmXactor.axi_side);

   AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User) rdreqDefault = AXI4_Rd_Addr {arid: ?,
      									araddr: ?,
      									arlen: 0,
      									arsize: axsize_4,
      									arburst: axburst_incr,
      									arlock: axlock_normal,
      									arcache:arcache_dev_nonbuf,
      									arprot: 0,
      									arqos: 0,
      									arregion: 0,
									aruser: ?};
   AXI4_Wr_Addr #(Wd_Id, Wd_Addr, Wd_User) wrreqDefault = AXI4_Wr_Addr {awid: ?,
      									awaddr: ?,
      									awlen: 0,
      									awsize: axsize_4,
      									awburst: axburst_incr,
      									awlock: axlock_normal,
      									awcache:awcache_dev_nonbuf,
      									awprot: 0,
      									awqos: 0,
      									awregion: 0,
									awuser: ?};





   rule sbus_aradd_rl;
      let x <- toGet(dmXactor.o_rd_addr).get();
      coreXactor.i_rd_addr.enq(x);
   endrule

   Reg #(Bool) rg_bursting <- mkReg(False);
   Reg #(Bool) rg_busy     <- mkReg(False);

   rule sbus_awadd_rl (!rg_bursting && !rg_busy);
      let x <- toGet(dmXactor.o_wr_addr).get();
      coreXactor.i_wr_addr.enq(x);
      rg_bursting <= True;
      rg_busy     <= True;
   endrule

   rule sbus_wdata_rl (rg_bursting);
      let x <- toGet(dmXactor.o_wr_data).get();
      coreXactor.i_wr_data.enq(x);
      rg_bursting <= (!x.wlast);
   endrule

   rule reset_req_rl (!rg_busy);
      let running <- toGet(req_FF).get();
      AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User) rdreq = rdreqDefault;
      rdreq.arid = 1; // for reset requests
      rdreq.araddr = extend(pack(running));
      coreXactor.i_rd_addr.enq(rdreq);
      rg_busy     <= True;
   endrule

   rule runhalt_req_rl (!rg_busy);
      let running <- debug_module.hart0.hart_client_run_halt.request.get();
      AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User) rdreq = rdreqDefault;
      rdreq.arid = 2; // for run/halt requests
      rdreq.araddr = extend(pack(running));
      coreXactor.i_rd_addr.enq(rdreq);
   endrule

   rule gpr_req_rl (!rg_bursting && !rg_busy);
      let req <-  debug_module.hart0.hart_gpr_mem_client.request.get();
      AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User) rdreq = rdreqDefault;
      AXI4_Wr_Addr #(Wd_Id, Wd_Addr, Wd_User) wrreq = wrreqDefault;
      rdreq.arid = 3; wrreq.awid = 3; // for gpr requests
      rdreq.araddr = extend(req.address); wrreq.awaddr = extend(req.address);
      if (req.write) begin
	 AXI4_Wr_Data#(Wd_Data, Wd_User) dta =
	    AXI4_Wr_Data {wdata: extend(req.data),
			  wstrb: '1,
			  wlast: True,
			  wuser: (?)};
	 coreXactor.i_wr_data.enq(dta);
	 coreXactor.i_wr_addr.enq(wrreq);
      end
      else
	 coreXactor.i_rd_addr.enq(rdreq);
   endrule

   rule csr_req_rl (!rg_bursting && !rg_busy);
      let req <-  debug_module.hart0.hart_csr_mem_client.request.get();
      AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User) rdreq = rdreqDefault;
      AXI4_Wr_Addr #(Wd_Id, Wd_Addr, Wd_User) wrreq = wrreqDefault;
      rdreq.arid = 4; wrreq.awid = 4; // for csr requests
      rdreq.araddr = extend(req.address); wrreq.awaddr = extend(req.address);
      if (req.write) begin
	 AXI4_Wr_Data#(Wd_Data, Wd_User) dta =
	    AXI4_Wr_Data {wdata: extend(req.data),
			  wstrb: '1,
			  wlast: True,
			  wuser: (?)};
	 coreXactor.i_wr_data.enq(dta);
	 coreXactor.i_wr_addr.enq(wrreq);
      end
      else
	 coreXactor.i_rd_addr.enq(rdreq);
   endrule

   rule rd_responses_rl;
      let rsp <- toGet(coreXactor.o_rd_data).get();
      case (rsp.rid)
	 0: dmXactor.i_rd_data.enq(rsp);
	 1: rsp_FF.enq(unpack(truncate(rsp.rdata)));
	 2: debug_module.hart0.hart_client_run_halt.response.put(unpack(truncate(rsp.rdata)));
	 3: debug_module.hart0.hart_gpr_mem_client.response.put(DM_CPU_Rsp {ok: rsp.rresp == axi4_resp_okay,
									    data: rsp.rdata });
	 4: debug_module.hart0.hart_csr_mem_client.response.put(DM_CPU_Rsp {ok: rsp.rresp == axi4_resp_okay,
									    data: rsp.rdata });
	 default: begin
		     $display("rd_responses_rl: invalid response");
		     $finish(0);
		  end
      endcase
      rg_busy <= False;
   endrule

   rule wr_responses_rl;
      let rsp <- toGet(coreXactor.o_wr_resp).get();
      case (rsp.bid)
	 0: dmXactor.i_wr_resp.enq(rsp);
	 3: debug_module.hart0.hart_gpr_mem_client.response.put(DM_CPU_Rsp {ok: rsp.bresp == axi4_resp_okay,
									    data: ? });
	 4: debug_module.hart0.hart_csr_mem_client.response.put(DM_CPU_Rsp {ok: rsp.bresp == axi4_resp_okay,
									    data: ? });
	 default: begin
		     $display("wr_responses_rl: invalid response");
		     $finish(0);
		  end
      endcase
      rg_busy <= False;
     endrule

/*
   interface Get #(Bit #(4))       hart_get_other_req;
   interface Client #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) hart_gpr_mem_client;
   interface Client #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN)) hart_csr_mem_client;
*/


   interface toCore = coreXactor.axi_side;
   interface Reset ndm_resetn = ndm_rstn;
   interface JTAG_IFC jtag = jtagtap.jtag;
 endmodule

// ================================================================

(* synthesize *)
// reset by power-on reset
module mkDummyBSDebug (BSDebug_IFC);
   interface toCore = dummy_AXI4_Master_ifc;
      interface Reset ndm_resetn = noReset;
      interface JTAG_IFC jtag = interface JTAG_IFC;
				   method Action tdi(x) = noAction;
				   method Action tms(x) = noAction;
				   method Action tclk(x) = noAction;
				   method tdo = 0;
				   interface Clock tclk_out = noClock;
				endinterface ;

endmodule

// ================================================================

endpackage
