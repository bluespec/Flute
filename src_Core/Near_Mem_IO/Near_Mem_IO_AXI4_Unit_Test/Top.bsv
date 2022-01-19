// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package Top;

// ================================================================
// This is a unit-test for 'module mkNear_Mem_IO_AXI4'
// which is in file Near_Mem_IO_AXI4.bsv.

// ================================================================
// BSV library imports

import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

import StmtFSM :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import Semi_FIFOF :: *;

// ----------------
// Project imports

// AXI
import AXI4_Types   :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User

import Near_Mem_IO_AXI4 :: *;

// ================================================================
// Test both RV32 and RV64 versions

`ifdef RV32

typedef 32 XLEN;

function Bit #(Wd_Data) fn_cpu_to_fabric_width (Bit #(XLEN) x);
   return zeroExtend (x);
endfunction

function Bit #(XLEN) fn_fabric_to_cpu_width (Bit #(Wd_Data) x);
   return truncate (x);
endfunction

`else

typedef 64 XLEN;

function Bit #(Wd_Data) fn_cpu_to_fabric_width (Bit #(XLEN) x);
   return truncate (x);
endfunction

function Bit #(XLEN) fn_fabric_to_cpu_width (Bit #(Wd_Data) x);
   return zeroExtend (x);
endfunction

`endif

// ================================================================

Fabric_Addr addr_base = 'h0200_0000;
Fabric_Addr addr_lim  = 'h0200_C000;

Fabric_Addr addr_mtime    = 'h_BFF8;
Fabric_Addr addr_mtimecmp = 'h_4000;

// ================================================================

(* synthesize *)
module mkTop (Empty);

   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)
   master_xactor <- mkAXI4_Master_Xactor;

   Near_Mem_IO_AXI4_IFC near_mem_io <- mkNear_Mem_IO_AXI4;

   mkConnection (master_xactor.axi_side, near_mem_io.axi4_slave);

   // ----------------------------------------------------------------

   function Action fa_axi_read_req (Fabric_Addr addr);
      action
	 AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User)
	 ra = AXI4_Rd_Addr {arid:    0,
			    araddr:  addr,
			    arlen:   0,
			    arsize:  axsize_8,
			    arburst: axburst_incr,
			    arlock:  axlock_normal,
			    arcache: arcache_norm_noncache_nonbuf,
			    arprot:  {axprot_2_data,
				      axprot_1_non_secure,
				      axprot_0_priv},
			    arqos:    0,
			    arregion: 0,
			    aruser:   0};
	 master_xactor.i_rd_addr.enq (ra);
	 $display ("%0d: AXI4_Rd_Addr {addr %0h len %0h size %0h burst %0h}",
		   cur_cycle, ra.araddr, ra.arlen, ra.arsize, ra.arburst);
      endaction
   endfunction

   function ActionValue #(Bit #(XLEN)) fa_axi_read_rsp (Fabric_Addr addr);
      actionvalue
	 AXI4_Rd_Data #(Wd_Id, Wd_Data, Wd_User)
	 rd <- pop_o (master_xactor.o_rd_data);
	 $display ("%0d: axi_read response for addr %0h", cur_cycle, addr);
	 $display ("    AXI4_Rd_Data {rdata: %0h", rd.rdata,
		   " rresp: ", fshow_AXI4_Resp (rd.rresp),
		   " rlast: %0h}", rd.rlast);
	 if (rd.rresp != axi4_resp_okay) begin
	    $display ("ERROR RESPONSE\n");
	    $finish (1);
	 end
	 return fn_fabric_to_cpu_width (rd.rdata);
      endactionvalue
   endfunction

   function Action fa_axi_write_req (Fabric_Addr addr, Bit #(XLEN) data);
      action
	 AXI4_Wr_Addr #(Wd_Id, Wd_Addr, Wd_User)
	 wa = AXI4_Wr_Addr {awid:    0,
			    awaddr:  addr,
			    awlen:   0,
			    awsize:  axsize_8,
			    awburst: axburst_incr,
			    awlock:  axlock_normal,
			    awcache: awcache_norm_noncache_nonbuf,
			    awprot:  {axprot_2_data,
				      axprot_1_non_secure,
				      axprot_0_priv},
			    awqos:    0,
			    awregion: 0,
			    awuser:   0};

	 AXI4_Wr_Data #(Wd_Data, Wd_User)
	 wd = AXI4_Wr_Data {wdata: fn_cpu_to_fabric_width (data),
			    wstrb: '1,
			    wlast: True,
			    wuser: 0};
	 master_xactor.i_wr_addr.enq (wa);
	 master_xactor.i_wr_data.enq (wd);
	 $display ("%0d: AXI4_Wr_Addr {addr %0h len %0h size %0h burst %0h}",
		   cur_cycle, wa.awaddr, wa.awlen, wa.awsize, wa.awburst);
	 $display ("    AXI4_Wr_Data {wdata %0h wstrb %0h wlast %0h}",
		   wd.wdata, wd.wstrb, wd.wlast);
      endaction
   endfunction

   function Action fa_axi_write_rsp (Fabric_Addr addr);
      action
	 AXI4_Wr_Resp #(Wd_Id, Wd_User)
	 wr <- pop_o (master_xactor.o_wr_resp);
	 $display ("%0d: axi_write response for addr %0h", cur_cycle, addr);
	 $display ("    AXI4_Wr_Resp: {bresp ", fshow_AXI4_Resp (wr.bresp), "}");
	 if (wr.bresp != axi4_resp_okay) begin
	    $display ("ERROR RESPONSE\n");
	    $finish (1);
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------

   Reg #(Bit #(XLEN)) rg_data <- mkRegU;
   Reg #(Bit #(32)) rg_iter <- mkRegU;

   FSM fsm_stimulus <- mkFSM (
      seq
	 action
	    $display ("----------------");
	    $display ("%0d: Top: dut reset request", cur_cycle);
	    near_mem_io.server_reset.request.put (?);
	 endaction

	 action
	    $display ("----------------");
	    $display ("%0d: Top: dut reset response", cur_cycle);
	    let x <- near_mem_io.server_reset.response.get;
	 endaction

	 action
	    $display ("----------------");
	    $display ("%0d: Top: set dut addr map: %0h %0h",
		      cur_cycle, addr_base, addr_lim);
	    near_mem_io.set_addr_map (addr_base, addr_lim);
	 endaction

	 for (rg_iter <= 0; rg_iter < 5; rg_iter <= rg_iter + 1) seq
	    $display ("Iter %0d ================================", rg_iter);
	    action
	       $display ("%0d: Top: read-req mtime", cur_cycle);
	       fa_axi_read_req (addr_base + addr_mtime);
	    endaction
	    action
	       $display ("--------");
	       let rdata <- fa_axi_read_rsp (addr_base + addr_mtime);
	       $display ("%0d: Top: read-resp mtime: %0h", cur_cycle, rdata);
	       rg_data <= rdata;
	    endaction

	    action
	       $display ("----------------");
	       $display ("%0d: Top: write-req mtimecmp", cur_cycle);
	       fa_axi_write_req (addr_base + addr_mtimecmp, rg_data + 100);
	    endaction
	    action
	       $display ("--------");
	       $display ("%0d: Top: write-resp mtimecmp", cur_cycle);
	       fa_axi_write_rsp (addr_base + addr_mtimecmp);
	    endaction
	    $display ("%0d: 'Processing' for 1000 cycles", cur_cycle);
	    delay (1000); // longer than timer delay +  interrupt duration
	 endseq

	 $display ("----------------");
	 $display ("%0d: Exiting FSM", cur_cycle);
	 $finish (0);
      endseq);

   Reg #(Bool) rg_irq <- mkRegU;
   FSM fsm_irq <- mkFSM (
      seq
	 while (True) seq
	    action
	       let irq <- near_mem_io.get_timer_interrupt_req.get;
	       rg_irq <= irq;
	    endaction
	    if (rg_irq) seq
	       $display ("%0d: IRQ: Entering interrupt for 50 cycles", cur_cycle);
	       delay (50);
	       $display ("%0d: IRQ: Exitting interrupt after 50 cycles", cur_cycle);
	    endseq
	    else action
	       $display ("%0d: IRQ: resetting irq", cur_cycle);
	    endaction
	 endseq
      endseq);

   Reg #(Bool) rg_started <- mkReg (False);

   rule rl_start (! rg_started);
      fsm_stimulus.start;
      fsm_irq.start;
      rg_started <= True;
   endrule

endmodule

// ================================================================

endpackage
