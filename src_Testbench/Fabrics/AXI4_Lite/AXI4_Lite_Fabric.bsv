// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

package AXI4_Lite_Fabric;

// ================================================================
// This package defines a fabric connecting CPUs, Memories and DMAs
// and other IP blocks.

// ================================================================
// Bluespec library imports

import Vector       :: *;
import FIFOF        :: *;
import SpecialFIFOs :: *;
import ConfigReg    :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;

// ================================================================
// Project imports

import Semi_FIFOF      :: *;
import AXI4_Lite_Types :: *;

// ================================================================
// The interface for the fabric module

interface AXI4_Lite_Fabric_IFC #(numeric type num_masters,
				 numeric type num_slaves,
				 numeric type wd_addr,
				 numeric type wd_data,
				 numeric type wd_user);
   method Action reset;
   method Action set_verbosity (Bit #(4) verbosity);

   // From masters
   interface Vector #(num_masters, AXI4_Lite_Slave_IFC #(wd_addr, wd_data, wd_user))  v_from_masters;

   // To slaves
   interface Vector #(num_slaves,  AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user)) v_to_slaves;
endinterface

// ================================================================
// The Fabric module
// The function parameter is an address-decode function, which returns
// returns (True,  slave-port-num)  if address is mapped to slave-port-num
//         (False, ?)               if address is unmapped to any port

module mkAXI4_Lite_Fabric #(function Tuple2 #(Bool, Bit #(TLog #(num_slaves)))
			             fn_addr_to_slave_num (Bit #(wd_addr) addr))
                          (AXI4_Lite_Fabric_IFC #(num_masters, num_slaves, wd_addr, wd_data, wd_user))

   provisos (Log #(num_masters, log_nm),
	     Log #(num_slaves,  log_ns),
	     Log #(TAdd #(num_masters, 1), log_nm_plus_1),
	     Log #(TAdd #(num_slaves,  1), log_ns_plus_1),
	     Add #(_dummy, TLog #(num_slaves), log_ns_plus_1));

   Reg #(Bit #(4)) cfg_verbosity  <- mkConfigReg (0);

   Reg #(Bool) rg_reset <- mkReg (True);

   // Transactors facing masters
   Vector #(num_masters, AXI4_Lite_Slave_Xactor_IFC  #(wd_addr, wd_data, wd_user))
      xactors_from_masters <- replicateM (mkAXI4_Lite_Slave_Xactor);

   // Transactors facing slaves
   Vector #(num_slaves,  AXI4_Lite_Master_Xactor_IFC #(wd_addr, wd_data, wd_user))
       xactors_to_slaves <- replicateM (mkAXI4_Lite_Master_Xactor);

   // FIFOs to keep track of which master originated a transaction, in
   // order to route corresponding responses back to that master.
   // Legal masters are 0..(num_masters-1)
   // The value of 'num_masters' is used for decode errors (no such slave)

   Vector #(num_masters, FIFOF #(Bit #(log_ns_plus_1))) v_f_wr_sjs      <- replicateM (mkSizedFIFOF (8));
   Vector #(num_masters, FIFOF #(Bit #(wd_user)))       v_f_wr_err_user <- replicateM (mkSizedFIFOF (8));
   Vector #(num_slaves,  FIFOF #(Bit #(log_nm_plus_1))) v_f_wr_mis      <- replicateM (mkSizedFIFOF (8));

   Vector #(num_masters, FIFOF #(Bit #(log_ns_plus_1))) v_f_rd_sjs      <- replicateM (mkSizedFIFOF (8));
   Vector #(num_masters, FIFOF #(Bit #(wd_user)))       v_f_rd_err_user <- replicateM (mkSizedFIFOF (8));
   Vector #(num_slaves,  FIFOF #(Bit #(log_nm_plus_1))) v_f_rd_mis      <- replicateM (mkSizedFIFOF (8));

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_reset (rg_reset);
      $display ("%0d: AXI4_Lite_Fabric.rl_reset", cur_cycle);
      for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1) begin
	 xactors_from_masters [mi].reset;

	 v_f_wr_sjs [mi].clear;
	 v_f_wr_err_user [mi].clear;

	 v_f_rd_sjs [mi].clear;
	 v_f_rd_err_user [mi].clear;
      end

      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1) begin
	 xactors_to_slaves [sj].reset;
	 v_f_wr_mis [sj].clear;
	 v_f_rd_mis [sj].clear;
      end
      rg_reset <= False;
   endrule

   // ----------------------------------------------------------------
   // Help functions for moving data from masters to slaves

   Integer num_slaves_i = valueOf (num_slaves);

   function Bool wr_move_from_mi_to_sj (Integer mi, Integer sj);
      let addr = xactors_from_masters [mi].o_wr_addr.first.awaddr;
      match { .legal, .slave_num } = fn_addr_to_slave_num (addr);
      return (legal
	      && (   (num_slaves_i == 1)
		  || (slave_num == fromInteger (sj))));
   endfunction

   function Bool wr_illegal_sj (Integer mi);
      let addr = xactors_from_masters [mi].o_wr_addr.first.awaddr;
      match { .legal, ._ } = fn_addr_to_slave_num (addr);
      return (! legal);
   endfunction

   function Bool rd_move_from_mi_to_sj (Integer mi, Integer sj);
      let addr = xactors_from_masters [mi].o_rd_addr.first.araddr;
      match { .legal, .slave_num } = fn_addr_to_slave_num (addr);
      return (legal
	      && (   (num_slaves_i == 1)
		  || (slave_num == fromInteger (sj))));
   endfunction

   function Bool rd_illegal_sj (Integer mi);
      let addr = xactors_from_masters [mi].o_rd_addr.first.araddr;
      match { .legal, ._ } = fn_addr_to_slave_num (addr);
      return (! legal);
   endfunction

   // ----------------
   // Wr requests from masters to slaves

   // Legal destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_wr_xaction_master_to_slave (wr_move_from_mi_to_sj (mi, sj));
	    AXI4_Lite_Wr_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_wr_addr);
	    AXI4_Lite_Wr_Data #(wd_data)          d <- pop_o (xactors_from_masters [mi].o_wr_data);

	    xactors_to_slaves [sj].i_wr_addr.enq (a);
	    xactors_to_slaves [sj].i_wr_data.enq (d);

	    v_f_wr_mis        [sj].enq (fromInteger (mi));
	    v_f_wr_sjs        [mi].enq (fromInteger (sj));

	    if (cfg_verbosity > 1) begin
	       $display ("%0d: AXI4_Lite_Fabric: wr master [%0d] -> slave [%0d]", cur_cycle, mi, sj);
	       $display ("        ", fshow (a));
	       $display ("        ", fshow (d));
	    end
	 endrule

   // Non-existent destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
	 rule rl_wr_xaction_no_such_slave (wr_illegal_sj (mi));
	    AXI4_Lite_Wr_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_wr_addr);
	    AXI4_Lite_Wr_Data #(wd_data)          d <- pop_o (xactors_from_masters [mi].o_wr_data);

	    v_f_wr_sjs        [mi].enq (fromInteger (valueOf (num_slaves)));
	    v_f_wr_err_user   [mi].enq (a.awuser);

	    if (cfg_verbosity > 1) begin
	       $display ("%0d: AXI4_Lite_Fabric: wr master [%0d] -> illegal addr", cur_cycle, mi);
	       $display ("        ", fshow (a));
	    end
	 endrule

   // ----------------
   // Rd requests from masters to slaves

   // Legal destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_rd_xaction_master_to_slave (rd_move_from_mi_to_sj (mi, sj));
	    AXI4_Lite_Rd_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_rd_addr);

	    xactors_to_slaves [sj].i_rd_addr.enq (a);

	    v_f_rd_mis [sj].enq (fromInteger (mi));
	    v_f_rd_sjs [mi].enq (fromInteger (sj));

	    if (cfg_verbosity > 1) begin
	       $display ("%0d: AXI4_Lite_Fabric: rd master [%0d] -> slave [%0d]", cur_cycle, mi, sj);
	       $display ("        ", fshow (a));
	    end
	 endrule

   // Non-existent destination slaves
   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
	 rule rl_rd_xaction_no_such_slave (rd_illegal_sj (mi));
	    AXI4_Lite_Rd_Addr #(wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_rd_addr);

	    v_f_rd_sjs      [mi].enq (fromInteger (valueOf (num_slaves)));
	    v_f_rd_err_user [mi].enq (a.aruser);

	    if (cfg_verbosity > 1) begin
	       $display ("%0d: AXI4_Lite_Fabric: rd master [%0d] -> illegal addr", cur_cycle, mi);
	       $display ("        ", fshow (a));
	    end
	 endrule

   // ----------------
   // Wr responses from slaves to masters

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_wr_resp_slave_to_master (   (v_f_wr_mis [sj].first == fromInteger (mi))
					  && (v_f_wr_sjs [mi].first == fromInteger (sj)));
	    v_f_wr_mis [sj].deq;
	    v_f_wr_sjs [mi].deq;
	    AXI4_Lite_Wr_Resp #(wd_user) b <- pop_o (xactors_to_slaves [sj].o_wr_resp);

	    xactors_from_masters [mi].i_wr_resp.enq (b);

	    if (cfg_verbosity > 1) begin
	       $display ("%0d: AXI4_Lite_Fabric: wr master [%0d] <- slave [%0d]", cur_cycle, mi, sj);
	       $display ("        ", fshow (b));
	    end
	 endrule

   // ----------------
   // Wr error responses to masters
   // v_f_wr_sjs [mi].first has value num_slaves (illegal value)
   // v_f_wr_err_user [mi].first contains the request's 'user' data

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)

      rule rl_wr_resp_err_to_master (v_f_wr_sjs [mi].first == fromInteger (valueOf (num_slaves)));
	 v_f_wr_sjs [mi].deq;
	 v_f_wr_err_user [mi].deq;

	 let b = AXI4_Lite_Wr_Resp {bresp: AXI4_LITE_DECERR, buser: v_f_wr_err_user [mi].first};

	 xactors_from_masters [mi].i_wr_resp.enq (b);

	 if (cfg_verbosity > 1) begin
	    $display ("%0d: AXI4_Lite_Fabric: wr master [%0d] <- error", cur_cycle, mi);
	    $display ("        ", fshow (b));
	 end
      endrule

   // ----------------
   // Rd responses from slaves to masters

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)
      for (Integer sj = 0; sj < valueOf (num_slaves); sj = sj + 1)

	 rule rl_rd_resp_slave_to_master (   (v_f_rd_mis [sj].first == fromInteger (mi))
					  && (v_f_rd_sjs [mi].first == fromInteger (sj)));
	    v_f_rd_mis [sj].deq;
	    v_f_rd_sjs [mi].deq;
	    AXI4_Lite_Rd_Data #(wd_data, wd_user) r <- pop_o (xactors_to_slaves [sj].o_rd_data);

	    xactors_from_masters [mi].i_rd_data.enq (r);

	    if (cfg_verbosity > 1) begin
	       $display ("%0d: AXI4_Lite_Fabric: rd master [%0d] <- slave [%0d]", cur_cycle, mi, sj);
	       $display ("        ", fshow (r));
	    end
	 endrule

   // ----------------
   // Rd error responses to masters
   // v_f_rd_sjs [mi].first has value num_slaves (illegal value)
   // v_f_rd_err_user [mi].first contains the request's 'user' data

   for (Integer mi = 0; mi < valueOf (num_masters); mi = mi + 1)

      rule rl_rd_resp_err_to_master (v_f_rd_sjs [mi].first == fromInteger (valueOf (num_slaves)));
	 v_f_rd_sjs [mi].deq;
	 v_f_rd_err_user [mi].deq;

	 Bit #(wd_data) data = 0;
	 let r = AXI4_Lite_Rd_Data {rresp: AXI4_LITE_DECERR, ruser: v_f_rd_err_user [mi].first, rdata: data};

	 xactors_from_masters [mi].i_rd_data.enq (r);

	 if (cfg_verbosity > 1) begin
	    $display ("%0d: AXI4_Lite_Fabric: rd master [%0d] <- error", cur_cycle, mi);
	    $display ("        ", fshow (r));
	 end
      endrule

   // ----------------------------------------------------------------
   // INTERFACE

   function AXI4_Lite_Slave_IFC  #(wd_addr, wd_data, wd_user) f1 (Integer j)
      = xactors_from_masters [j].axi_side;
   function AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user) f2 (Integer j)
      = xactors_to_slaves    [j].axi_side;

   method Action reset () if (! rg_reset);
      rg_reset <= True;
   endmethod

   method Action set_verbosity (Bit #(4) verbosity);
      cfg_verbosity <= verbosity;
   endmethod

   interface v_from_masters = genWith (f1);
   interface v_to_slaves    = genWith (f2);
endmodule

// ================================================================

endpackage: AXI4_Lite_Fabric
