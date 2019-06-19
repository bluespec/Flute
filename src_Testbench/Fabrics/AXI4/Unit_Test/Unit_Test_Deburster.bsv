// Copyright (c) 2019 Bluespec, Inc. All Rights Reserved

package Unit_Test_Deburster;

// ================================================================
// Standalone unit tester for AXI4_Deburster.bsv

// ================================================================
// Bluespec library imports

import FIFOF       :: *;
import Connectable :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;

// ================================================================
// Project imports

import Semi_FIFOF     :: *;
import AXI4_Types     :: *;
import AXI4_Deburster :: *;

// ================================================================
// Synthesized instance of Deburster

typedef  4 Wd_Id;
typedef 32 Wd_Addr;
typedef 64 Wd_Data;
typedef 10 Wd_User;

typedef AXI4_Deburster_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) AXI4_Deburster_IFC_Inst;

(* synthesize *)
module mkAXI4_Deburster_Inst (AXI4_Deburster_IFC_Inst);
   let m <- mkAXI4_Deburster;
   return m;
endmodule

// ================================================================

(* synthesize *)
module mkUnit_Test_Deburster (Empty);
   AXI4_Deburster_IFC_Inst deburster <- mkAXI4_Deburster_Inst;

   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master <- mkAXI4_Master_Xactor;
   AXI4_Slave_Xactor_IFC  #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave  <- mkAXI4_Slave_Xactor;

   mkConnection (master.axi_side,    deburster.from_master);
   mkConnection (deburster.to_slave, slave.axi_side);

   Reg #(Bit #(32)) rg_test       <- mkReg (20);    // Chooses which test to run

   FIFOF #(Bit #(8))  f_len         <- mkFIFOF;
   Reg #(Bit #(8))    rg_beat       <- mkReg (0);
   Reg #(Bit #(32))   rg_idle_count <- mkReg (0);

   // ================================================================
   // Help function to create AXI4 channel payloads

   function AXI4_Wr_Addr #(Wd_Id, Wd_Addr, Wd_User)
            fv_mk_wr_addr (Bit #(Wd_Id)    id,
			   Bit #(Wd_Addr)  addr,
			   Bit #(8)        len,
			   Bit #(2)        burst,
			   Bit #(Wd_User)  user);
      return AXI4_Wr_Addr {awid: id,
			   awaddr: addr,
			   awlen: len,
			   awsize: axsize_8,
			   awburst: burst,
			   awlock: 0,
			   awcache: 0,
			   awprot: 0,
			   awqos: 0,
			   awregion: 0,
			   awuser: user};
   endfunction

   function AXI4_Wr_Data #(Wd_Data, Wd_User)
            fv_mk_wr_data (Bit #(Wd_Data)  data,
			   Bit #(Wd_User)  user);
      Bool last = (rg_beat == f_len.first - 1);
      return AXI4_Wr_Data {wdata: data,
			   wstrb: 'hFF,
			   wlast: last,
			   wuser: user};
   endfunction

   function AXI4_Wr_Resp #(Wd_Id, Wd_User)
            fv_mk_wr_resp (AXI4_Wr_Addr #(Wd_Id, Wd_Addr, Wd_User) wa);
      return AXI4_Wr_Resp {bid:   wa.awid,
			   bresp: axi4_resp_okay,
			   buser: wa.awuser};
   endfunction

   function AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User)
            fv_mk_rd_addr (Bit #(Wd_Id)    id,
			   Bit #(Wd_Addr)  addr,
			   Bit #(8)        len,
			   Bit #(2)        burst,
			   Bit #(Wd_User)  user);
      return AXI4_Rd_Addr {arid: id,
			   araddr: addr,
			   arlen: len,
			   arsize: axsize_8,
			   arburst: burst,
			   arlock: 0,
			   arcache: 0,
			   arprot: 0,
			   arqos: 0,
			   arregion: 0,
			   aruser: user};
   endfunction

   function AXI4_Rd_Data #(Wd_Id, Wd_Data, Wd_User)
            fv_mk_rd_data (AXI4_Rd_Addr #(Wd_Id, Wd_Addr, Wd_User) ar);
      return AXI4_Rd_Data {rid:   ar.arid,
			   rdata: zeroExtend (ar.araddr + 'h10_000),
			   rresp: axi4_resp_okay,
			   rlast: True,
			   ruser: ar.aruser};
   endfunction

   // ================================================================
   // STIMULUS

   Bit #(Wd_Id)   id1   = 1;
   Bit #(Wd_User) user1 = 1;

   // ----------------
   // Write tests

   rule rl_wr_single (rg_test == 0);
      Bit #(8) len = 1;
      let wa = fv_mk_wr_addr (id1, 'h1000, (len - 1), axburst_fixed, user1);
      master.i_wr_addr.enq (wa);

      f_len.enq (len);
      rg_idle_count <= 0;
      rg_test       <= 100;

      $display ("%0d: master.rl_wr_single: ", cur_cycle);
      $display ("  ", fshow (wa));
   endrule

   rule rl_wr_burst_addr_0 (rg_test == 10);
      Bit #(8) len = 2;
      let wa = fv_mk_wr_addr (id1, 'h1000, (len - 1), axburst_incr, user1);
      master.i_wr_addr.enq (wa);

      f_len.enq (len);
      rg_idle_count <= 0;
      rg_test       <= 11;

      $display ("%0d: master.rl_wr_burst_addr_0: ", cur_cycle);
      $display ("  ", fshow (wa));
   endrule

   rule rl_wr_burst_addr_1 (rg_test == 11);
      Bit #(8) len = 4;
      let wa = fv_mk_wr_addr (id1, 'h2000, (len - 1), axburst_incr, user1);
      master.i_wr_addr.enq (wa);

      f_len.enq (len);
      rg_idle_count <= 0;
      rg_test       <= 100;

      $display ("%0d: master.rl_wr_burst_addr_1: ", cur_cycle);
      $display ("  ", fshow (wa));
   endrule

   rule rl_wr_data;
      let data = 'h1_0000 + zeroExtend (rg_beat);
      let wd = fv_mk_wr_data (data, user1);
      master.i_wr_data.enq (wd);
      rg_idle_count <= 0;
      
      if (rg_beat < f_len.first - 1)
	 rg_beat <= rg_beat + 1;
      else begin
	 rg_beat <= 0;
	 f_len.deq;

	 rg_test <= '1;
      end

      $display ("%0d: master.rl_wr_data: ", cur_cycle);
      $display ("  ", fshow (wd));
   endrule

   // ----------------
   // Read tests

   rule rl_rd_single (rg_test == 2);
      let ra = fv_mk_rd_addr (id1, 'h1000, 1, axburst_fixed, user1);
      master.i_rd_addr.enq (ra);
      rg_idle_count <= 0;
      rg_test <= '1;

      $display ("%0d: master.rd_single: ", cur_cycle);
      $display ("  ", fshow (ra));
   endrule

   rule rl_rd_burst_addr_0 (rg_test == 20);
      Bit #(8) len = 2;
      let ra = fv_mk_rd_addr (id1, 'h1000, (len - 1), axburst_incr, user1);
      master.i_rd_addr.enq (ra);

      rg_idle_count <= 0;
      rg_test       <= 21;

      $display ("%0d: master.rl_rd_burst_addr_0: ", cur_cycle);
      $display ("  ", fshow (ra));
   endrule

   rule rl_rd_burst_addr_1 (rg_test == 21);
      Bit #(8) len = 4;
      let ra = fv_mk_rd_addr (id1, 'h2000, (len - 1), axburst_incr, user1);
      master.i_rd_addr.enq (ra);

      rg_idle_count <= 0;
      rg_test       <= 100;

      $display ("%0d: master.rl_rd_burst_addr_1: ", cur_cycle);
      $display ("  ", fshow (ra));
   endrule

   // ================================================================
   // Drain and display responses received by master

   rule rl_wr_resps;
      let wr_resp <- pop_o (master.o_wr_resp);
      $display ("%0d: master: ", cur_cycle);
      $display ("  ", fshow (wr_resp));
      rg_idle_count <= 0;
   endrule

   rule rl_rd_resps;
      let rd_resp <- pop_o (master.o_rd_data);
      $display ("%0d: master: ", cur_cycle);
      $display ("  ", fshow (rd_resp));
      rg_idle_count <= 0;
   endrule

   // ================================================================
   // Slave: return functional responses
   // Note: we should not be receiving any bursts, since we're fronted by the Deburster.

   rule rl_slave_IP_model_writes;
      $display ("%0d: %m.rl_slave_IP_model_writes: ", cur_cycle);

      let wa <- pop_o (slave.o_wr_addr);
      let wd <- pop_o (slave.o_wr_data);

      let wr = fv_mk_wr_resp (wa);
      slave.i_wr_resp.enq (wr);
      $display ("  ", fshow (wa));
      $display ("  ", fshow (wd));
      $display ("  ", fshow (wr));
   endrule

   rule rl_slave_IP_model_rd_addr;
      let ra <- pop_o (slave.o_rd_addr);
      slave.i_rd_data.enq (fv_mk_rd_data (ra));

      $display ("%0d: slave: ", cur_cycle);
      $display ("  ", fshow (ra));
   endrule

   // ================================================================

   rule rl_idle_quit;
      if (rg_idle_count == 100) begin
	 $display ("%0d: UnitTest_Deburster: idle; quit", cur_cycle);
	 $finish (0);
      end
      else begin
	 rg_idle_count <= rg_idle_count + 1;
      end
   endrule

endmodule

// ================================================================

endpackage
