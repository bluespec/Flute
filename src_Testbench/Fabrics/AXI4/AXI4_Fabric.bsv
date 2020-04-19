// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

package AXI4_Fabric;

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

import Semi_FIFOF :: *;
import AXI4_Types :: *;

// ================================================================
// The interface for the fabric module

interface AXI4_Fabric_IFC #(numeric type tn_num_masters,
			    numeric type tn_num_slaves,
			    numeric type wd_id,
			    numeric type wd_addr,
			    numeric type wd_data,
			    numeric type wd_user);
   method Action reset;
   method Action set_verbosity (Bit #(4) verbosity);

   // From masters
   interface Vector #(tn_num_masters, AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user))  v_from_masters;

   // To slaves
   interface Vector #(tn_num_slaves,  AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user)) v_to_slaves;
endinterface

// ================================================================
// The Fabric module
// The function parameter is an address-decode function, which
// returns (True,  slave-port-num)  if address is mapped to slave-port-num
//         (False, ?)               if address is unmapped to any slave port

module mkAXI4_Fabric #(function Tuple2 #(Bool, Bit #(TLog #(tn_num_slaves)))
			        fn_addr_to_slave_num (Bit #(wd_addr) addr))
		     (AXI4_Fabric_IFC #(tn_num_masters, tn_num_slaves, wd_id, wd_addr, wd_data, wd_user))

   provisos (Log #(tn_num_masters, log_nm),
	     Log #(tn_num_slaves,  log_ns),
	     Log #(TAdd #(tn_num_slaves,  1),  log_ns_plus_1),
	     Log #(TAdd #(tn_num_masters,  1), log_nm_plus_1));

   Integer num_masters = valueOf (tn_num_masters);
   Integer num_slaves  = valueOf (tn_num_slaves);

   // 0: quiet; 1: show transactions
   Reg #(Bit #(4)) cfg_verbosity  <- mkConfigReg (0);

   Reg #(Bool) rg_reset <- mkReg (True);

   // Transactors facing masters
   Vector #(tn_num_masters, AXI4_Slave_Xactor_IFC  #(wd_id, wd_addr, wd_data, wd_user))
      xactors_from_masters <- replicateM (mkAXI4_Slave_Xactor);

   // Transactors facing slaves
   Vector #(tn_num_slaves,  AXI4_Master_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user))
       xactors_to_slaves <- replicateM (mkAXI4_Master_Xactor);

   // ----------------------------------------------------------------
   // Book-keeping FIFOs and regs
   // - to keep track of which master originated a transaction, in order
   //       to route corresponding responses back to that master
   // - to manage wdata channel based on burst info in awaddr channel
   // - to manage requests that do not map to any of the slaves.
   // Legal slaves are 0..(num_slaves-1)
   //     The "illegal" value of 'num_slaves' is used for decode errors (no such slave).
   // num_masters could be 1 => Bit #(0) to identify a master, but
   //     equality on Bit #(0) is dicey, so we always use num_masters+1.
   // Size of SizedFIFOs is estimated: should cover round-trip latency to slave and back.

   // ----------------
   // Write-transaction book-keeping

   // On an mi->sj write-transaction, this fifo records sj for master mi
   Vector #(tn_num_masters, FIFOF #(Bit #(log_ns_plus_1))) v_f_wr_sjs <- replicateM (mkSizedFIFOF (8));

   // On an mi->sj write-transaction, this fifo records mi for slave sj
   Vector #(tn_num_slaves,  FIFOF #(Bit #(log_nm_plus_1))) v_f_wr_mis  <- replicateM (mkSizedFIFOF (8));

   // On an mi->sj write-transaction, this fifo records a task (sj, awlen) for W channel
   Vector #(tn_num_masters,
	    FIFOF #(Tuple2 #(Bit #(log_ns_plus_1),
			     AXI4_Len)))     v_f_wd_tasks <- replicateM (mkFIFOF);
   // On an mi->sj write-transaction, this register is the W-channel burst beat_count
   // (0 => ready for next burst)
   Vector #(tn_num_masters, Reg #(AXI4_Len)) v_rg_wd_beat_count <- replicateM (mkReg (0));

   // On a write-transaction to non-existent slave, record id and user for error response
   Vector #(tn_num_masters,
	    FIFOF #(Tuple2 #(Bit #(wd_id),
			     Bit #(wd_user))))  v_f_wr_err_info <- replicateM (mkSizedFIFOF (8));

   // ----------------
   // Read-transaction book-keeping

   // On an mi->sj read-transaction, records sj for master mi
   Vector #(tn_num_masters, FIFOF #(Bit #(log_ns_plus_1))) v_f_rd_sjs <- replicateM (mkSizedFIFOF (8));
   // On an mi->sj read-transaction, records (mi,arlen) for slave sj
   Vector #(tn_num_slaves,
	    FIFOF #(Tuple2 #(Bit #(log_nm_plus_1),
			     AXI4_Len)))            v_f_rd_mis <- replicateM (mkSizedFIFOF (8));
   // On an mi->sj read-transaction, this register is the R-channel burst beat_count
   // (0 => ready for next burst)
   Vector #(tn_num_slaves, Reg #(AXI4_Len)) v_rg_r_beat_count <- replicateM (mkReg (0));

   // On a read-transaction to non-exisitent slave, record id and user for error response
   Vector #(tn_num_masters,
	    FIFOF #(Tuple3 #(AXI4_Len,
			     Bit #(wd_id),
			     Bit #(wd_user)))) v_f_rd_err_info <- replicateM (mkSizedFIFOF (8));

   // On an mi->non-existent-slave read-transaction,
   // this register is the R-channel burst beat_count
   // (0 => ready for next burst)
   Vector #(tn_num_masters, Reg #(AXI4_Len)) v_rg_r_err_beat_count <- replicateM (mkReg (0));

   // ----------------------------------------------------------------
   // RESET

   rule rl_reset (rg_reset);
      if (cfg_verbosity > 0)
	 $display ("%0d: %m.rl_reset", cur_cycle);
      for (Integer mi = 0; mi < num_masters; mi = mi + 1) begin
	 xactors_from_masters [mi].reset;

	 v_f_wr_sjs [mi].clear;
	 v_f_wd_tasks [mi].clear;
	 v_rg_wd_beat_count [mi] <= 0;

	 v_f_wr_err_info [mi].clear;

	 v_f_rd_sjs [mi].clear;

	 v_f_rd_err_info [mi].clear;
      end

      for (Integer sj = 0; sj < num_slaves; sj = sj + 1) begin
	 xactors_to_slaves [sj].reset;
	 v_f_wr_mis [sj].clear;
	 v_f_rd_mis [sj].clear;
	 v_rg_r_beat_count [sj] <= 0;
      end
      rg_reset <= False;
   endrule

   // ----------------------------------------------------------------
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Predicates to check if master I has transaction for slave J

   function Bool fv_mi_has_wr_for_sj (Integer mi, Integer sj);
      let addr = xactors_from_masters [mi].o_wr_addr.first.awaddr;
      match { .legal, .slave_num } = fn_addr_to_slave_num (addr);
      return (legal
	      && (   (num_slaves == 1)
		  || (slave_num == fromInteger (sj))));
   endfunction

   function Bool fv_mi_has_wr_for_none (Integer mi);
      let addr = xactors_from_masters [mi].o_wr_addr.first.awaddr;
      match { .legal, ._ } = fn_addr_to_slave_num (addr);
      return (! legal);
   endfunction

   function Bool fv_mi_has_rd_for_sj (Integer mi, Integer sj);
      let addr = xactors_from_masters [mi].o_rd_addr.first.araddr;
      match { .legal, .slave_num } = fn_addr_to_slave_num (addr);
      return (legal
	      && (   (num_slaves == 1)
		  || (slave_num == fromInteger (sj))));
   endfunction

   function Bool fv_mi_has_rd_for_none (Integer mi);
      let addr = xactors_from_masters [mi].o_rd_addr.first.araddr;
      match { .legal, ._ } = fn_addr_to_slave_num (addr);
      return (! legal);
   endfunction

   // ================================================================
   // Wr requests (AW, W and B channels)

   // Wr requests to legal slaves (AW channel)
   for (Integer mi = 0; mi < num_masters; mi = mi + 1)
      for (Integer sj = 0; sj < num_slaves; sj = sj + 1)

	 rule rl_wr_xaction_master_to_slave (fv_mi_has_wr_for_sj (mi, sj));
	    // Move the AW transaction
	    AXI4_Wr_Addr #(wd_id, wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_wr_addr);
	    xactors_to_slaves [sj].i_wr_addr.enq (a);

	    // Enqueue a task for the W channel
	    v_f_wd_tasks      [mi].enq (tuple2 (fromInteger (sj), a.awlen));

	    // Book-keeping
	    v_f_wr_mis        [sj].enq (fromInteger (mi));
	    v_f_wr_sjs        [mi].enq (fromInteger (sj));

	    if (cfg_verbosity > 0) begin
	       $display ("%0d: %m.rl_wr_xaction_master_to_slave: m%0d -> s%0d",
			 cur_cycle, mi, sj);
	       $display ("    ", fshow (a));
	    end
	 endrule

   // Wr requests to non-existent slave (AW channel)
   for (Integer mi = 0; mi < num_masters; mi = mi + 1)
	 rule rl_wr_xaction_no_such_slave (fv_mi_has_wr_for_none (mi));
	    AXI4_Wr_Addr #(wd_id, wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_wr_addr);

	    // Special value 'num_slaves' (not a legal sj) means "no such slave"
	    v_f_wr_sjs        [mi].enq (fromInteger (num_slaves));
	    v_f_wr_err_info   [mi].enq (tuple2 (a.awid, a.awuser));

	    // Enqueue a task for the W channel (must consume the write-data burst)
	    v_f_wd_tasks      [mi].enq (tuple2 (fromInteger (num_slaves), a.awlen));

	    if (cfg_verbosity > 0) begin
	       $display ("%0d: %m.rl_wr_xaction_no_such_slave: m%0d -> ?",
			 cur_cycle, mi);
	       $display ("        ", fshow (a));
	    end
	 endrule

   // Wr data (W channel)
   for (Integer mi = 0; mi < num_masters; mi = mi + 1)

      // Handle W channel burst
      // Invariant: v_rg_wd_beat_count == 0 between bursts
      // Note: awlen is encoded as 0..255 for burst lengths of 1..256
      rule rl_wr_xaction_master_to_slave_data (v_f_wd_tasks [mi].first matches {.sj, .awlen});
	 AXI4_Wr_Data #(wd_data, wd_user) d <- pop_o (xactors_from_masters [mi].o_wr_data);

	 // If sj is a legal slave, send it the data beat, else drop it.
	 if (sj < fromInteger (num_slaves))
	    xactors_to_slaves [sj].i_wr_data.enq (d);

	 if (cfg_verbosity > 0) begin
	    $display ("%0d: %m.rl_wr_xaction_master_to_slave_data: m%0d -> s%0d, beat %0d/%0d",
		      cur_cycle, mi, sj, v_rg_wd_beat_count [mi], awlen);
	    $display ("    ", fshow (d));
	 end

	 if (v_rg_wd_beat_count [mi] == awlen) begin
	    // End of burst
	    v_f_wd_tasks [mi].deq;
	    v_rg_wd_beat_count [mi] <= 0;

	    // Simulation-only assertion-check (no action, just display assertion failure)
	    // Final beat must have WLAST = 1
	    // Rely on slave (which should also see this error) to return error response
	    if (! (d.wlast)) begin
	       $display ("%0d: %m.rl_wr_xaction_master_to_slave_data: ERROR: m%0d -> s%0d",
			 cur_cycle, mi, sj);
	       $display ("    WLAST not set on final data beat (awlen = %0d)", awlen);
	       $display ("    ", fshow (d));
	    end
	 end
	 else
	    v_rg_wd_beat_count [mi] <= v_rg_wd_beat_count [mi] + 1;
      endrule

   // Wr responses from slaves to masters (B channel)

   for (Integer mi = 0; mi < num_masters; mi = mi + 1)
      for (Integer sj = 0; sj < num_slaves; sj = sj + 1)

	 rule rl_wr_resp_slave_to_master (   (v_f_wr_mis [sj].first == fromInteger (mi))
					  && (v_f_wr_sjs [mi].first == fromInteger (sj)));
	    v_f_wr_mis [sj].deq;
	    v_f_wr_sjs [mi].deq;
	    AXI4_Wr_Resp #(wd_id, wd_user) b <- pop_o (xactors_to_slaves [sj].o_wr_resp);

	    xactors_from_masters [mi].i_wr_resp.enq (b);

	    if (cfg_verbosity > 0) begin
	       $display ("%0d: %m.rl_wr_resp_slave_to_master: m%0d <- s%0d",
			 cur_cycle, mi, sj);
	       $display ("        ", fshow (b));
	    end
	 endrule

   // Wr error responses to masters (B channel)
   // v_f_wr_sjs [mi].first has value num_slaves (illegal value)
   // v_f_wr_err_info [mi].first contains request fields 'awid' and 'awuser'

   for (Integer mi = 0; mi < num_masters; mi = mi + 1)

      rule rl_wr_resp_err_to_master (v_f_wr_sjs [mi].first == fromInteger (num_slaves));
	 v_f_wr_sjs [mi].deq;
	 v_f_wr_err_info [mi].deq;

	 match { .awid, .awuser } = v_f_wr_err_info [mi].first;

	 let b = AXI4_Wr_Resp {bid:   awid,
			       bresp: axi4_resp_decerr,
			       buser: awuser};

	 xactors_from_masters [mi].i_wr_resp.enq (b);

	 if (cfg_verbosity > 0) begin
	    $display ("%0d: %m.rl_wr_resp_err_to_master: m%0d <- err", cur_cycle, mi);
	    $display ("        ", fshow (b));
	 end
      endrule

   // ================================================================
   // Rd requests (AR and R channels)

   // Rd requests to legal slaves (AR channel)
   for (Integer mi = 0; mi < num_masters; mi = mi + 1)
      for (Integer sj = 0; sj < num_slaves; sj = sj + 1)

	 rule rl_rd_xaction_master_to_slave (fv_mi_has_rd_for_sj (mi, sj));
	    AXI4_Rd_Addr #(wd_id, wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_rd_addr);

	    xactors_to_slaves [sj].i_rd_addr.enq (a);

	    v_f_rd_mis [sj].enq (tuple2 (fromInteger (mi), a.arlen));
	    v_f_rd_sjs [mi].enq (fromInteger (sj));

	    if (cfg_verbosity > 0) begin
	       $display ("%0d: %m.rl_rd_xaction_master_to_slave: m%0d -> s%0d",
			 cur_cycle, mi, sj);
	       $display ("        ", fshow (a));
	    end
	 endrule

   // Rd requests to non-existent slave (AR channel)
   for (Integer mi = 0; mi < num_masters; mi = mi + 1)
	 rule rl_rd_xaction_no_such_slave (fv_mi_has_rd_for_none (mi));
	    AXI4_Rd_Addr #(wd_id, wd_addr, wd_user) a <- pop_o (xactors_from_masters [mi].o_rd_addr);

	    v_f_rd_sjs      [mi].enq (fromInteger (num_slaves));
	    v_f_rd_err_info [mi].enq (tuple3 (a.arlen, a.arid, a.aruser));

	    if (cfg_verbosity > 0) begin
	       $display ("%0d: %m.rl_rd_xaction_no_such_slave: m%0d -> ?",
			 cur_cycle, mi);
	       $display ("        ", fshow (a));
	    end
	 endrule

   // Rd responses from slaves to masters (R channel)

   for (Integer mi = 0; mi < num_masters; mi = mi + 1)
      for (Integer sj = 0; sj < num_slaves; sj = sj + 1)

	 rule rl_rd_resp_slave_to_master (v_f_rd_mis [sj].first matches { .mi2, .arlen }
					  &&& (mi2 == fromInteger (mi))
					  &&& (v_f_rd_sjs [mi].first == fromInteger (sj)));

	    AXI4_Rd_Data #(wd_id, wd_data, wd_user) r <- pop_o (xactors_to_slaves [sj].o_rd_data);

	    if (v_rg_r_beat_count [sj] == arlen) begin
	       // Final beat of burst
	       v_f_rd_mis [sj].deq;
	       v_f_rd_sjs [mi].deq;
	       v_rg_r_beat_count [sj] <= 0;

	       // Assertion-check
	       // Final beat must have RLAST = 1
	       // If not, and if RRESP is OK, set RRESP to AXI4_RESP_SLVERR
	       if ((r.rresp == axi4_resp_okay) && (! (r.rlast))) begin
		  r.rresp = axi4_resp_slverr;
		  $display ("%0d: %m.rl_rd_resp_slave_to_master: ERROR: m%0d <- s%0d",
			    cur_cycle, mi, sj);
		  $display ("    RLAST not set on final data beat (arlen = %0d)", arlen);
		  $display ("    ", fshow (r));
	       end
	    end
	    else
	       v_rg_r_beat_count [sj] <= v_rg_r_beat_count [sj] + 1;

	    xactors_from_masters [mi].i_rd_data.enq (r);

	    if (cfg_verbosity > 0) begin
	       $display ("%0d: %m.rl_rd_resp_slave_to_master: m%0d <- s%0d",
			 cur_cycle, mi, sj);
	       $display ("    r: ", fshow (r));
	    end
	 endrule

   // Rd error responses to masters (R channel)
   // v_f_rd_sjs [mi].first has value num_slaves (illegal value)
   // v_f_rd_err_info [mi].first contains request fields: 'arlen', 'arid', 'aruser'

   for (Integer mi = 0; mi < num_masters; mi = mi + 1)

      rule rl_rd_resp_err_to_master (v_f_rd_sjs [mi].first == fromInteger (num_slaves));
	 match { .arlen, .arid, .aruser } = v_f_rd_err_info [mi].first;

	 Bit #(wd_data) data = 0;
	 let r = AXI4_Rd_Data {rid:    arid,
			       rdata:  data,
			       rresp:  axi4_resp_decerr,
			       rlast:  (v_rg_r_err_beat_count [mi] == arlen),
			       ruser:  aruser};

	 xactors_from_masters [mi].i_rd_data.enq (r);

	 if (v_rg_r_err_beat_count [mi] == arlen) begin
	    // Last beat of burst
	    v_f_rd_sjs [mi].deq;
	    v_f_rd_err_info [mi].deq;
	    v_rg_r_err_beat_count [mi] <= 0;
	 end
	 else
	    v_rg_r_err_beat_count [mi] <= v_rg_r_err_beat_count [mi] + 1;

	 if (cfg_verbosity > 0) begin
	    $display ("%0d: %m.rl_rd_resp_err_to_master: m%0d <- err",
		      cur_cycle, mi);
	    $display ("    r: ", fshow (r));
	 end
      endrule

   // ================================================================
   // INTERFACE

   function AXI4_Slave_IFC  #(wd_id, wd_addr, wd_data, wd_user) f1 (Integer j)
      = xactors_from_masters [j].axi_side;
   function AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user) f2 (Integer j)
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

endpackage: AXI4_Fabric
