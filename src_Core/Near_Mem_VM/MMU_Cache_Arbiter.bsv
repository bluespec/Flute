package MMU_Cache_Arbiter;

// ================================================================
// An arbiter that splits a single MMU_Cache_IFC into N interface
// instances. We use a static priority for each master, with low index
// masters being higher priority. This reduces critical path complexity.

// ================================================================
// BSV lib imports

import Vector       :: *;
import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls    :: *;
import Near_Mem_IFC :: *;
import MMU_Cache    :: *;

// ================================================================
// Interface

interface MMU_Cache_Arbiter_IFC #(numeric type num_masters);
   interface Vector #(num_masters, MMU_Cache_IFC) v_from_masters;
endinterface

// ================================================================
// Implementation module

module mkMMU_Cache_Arbiter #(MMU_Cache_IFC cache)
			   (MMU_Cache_Arbiter_IFC #(num_masters))

   provisos (Add#(a__, 1, num_masters));

   // Whether there is currently a master in control of the cache. If so,
   // rg_master indicates which master this is.
   Reg #(Bool) rg_active <- mkReg (False);
   // One-hot encoding of the current cache master if active, or last master if
   // inactive (i.e. updated only on acquire, and not release).
   Reg #(Vector #(num_masters, Bool)) rg_master <- mkReg (unpack (zeroExtend (1'b1)));

   // Pulsed at the start of a cycle to indicate that a response has been
   // consumed and so the current active master is relinquishing control.
   PulseWire pw_released_early <- mkPulseWireOR;
   // Pulsed part way through a cyle to indicate that a master has submitted a
   // request to the cache and acquired control.
   PulseWire pw_acquired       <- mkPulseWireOR;
   // Pulsed part way through a cycle to cancel out pw_acquired for
   // single-cycle operations.
   PulseWire pw_released_late  <- mkPulseWireOR;

   // Whether the current cache master has an outstanding reset request.
   Reg #(Bool) rg_resetting <- mkReg (False);
   // Pulsed at the start of a cycle when the outstanding reset request has
   // completed.
   PulseWire pw_finished_reset <- mkPulseWire;
   // Pulsed part way through a cycle when a new reset request is issued.
   PulseWire pw_started_reset  <- mkPulseWire;

   // Whether the current cache master has an outstanding flush request.
   Reg #(Bool) rg_flushing <- mkReg (False);
   // Pulsed at the start of a cycle when the outstanding flush request has
   // completed.
   PulseWire pw_finished_flush <- mkPulseWire;
   // Pulsed part way through a cycle when a new flush request is issued.
   PulseWire pw_started_flush  <- mkPulseWire;

   // Summary bit for rg_resetting and rg_flushing.
   Reg #(Bool) rg_resetting_or_flushing <- mkReg (False);

   // Enforce priority order for taking control of the cache. Masters consult
   // every element preceding their PulseWire before they acquire.
   Vector #(num_masters, PulseWire) v_master_acquired <- replicateM (mkPulseWireOR);

   // Call when a master has consumed a response at the beginning of a cycle
   // and should release the cache.
   function Action fa_master_release_early;
      action
	 pw_released_early.send;
      endaction
   endfunction

   // Call when a master has issued a single-cycle request with no explicit
   // response signal, ensuring the cache will be released at the end of the
   // cycle despite also acquiring it.
   function Action fa_master_release_late;
      action
	 pw_released_late.send;
      endaction
   endfunction

   // Check whether the given master is awaiting a response.
   function Bool fn_master_is_active (Integer i);
      return (rg_active && rg_master [i]);
   endfunction

   // Check whether the given master is able to acquire the cache, taking into
   // account the static priority order.
   function Bool fn_master_can_acquire (Integer i);
      Bool preempted = False;
      for (Integer j = 0; j < i; j = j + 1)
	 preempted = preempted || v_master_acquired [j];
      return (   (   rg_active
                  && rg_master [i])
              || (   (! rg_active)
	          && (! preempted)));
   endfunction

   // Call when a master is issuing a request to the cache, ensuring nobody
   // else uses it until the response is available.
   function Action fa_master_acquire (Integer i);
      action
	 v_master_acquired [i].send;
	 pw_acquired.send;
      endaction
   endfunction

   // A bypassable buffered request for each master. Issued (and invalidated)
   // in the same cycle if the master can acquire the cache, otherwise buffered
   // until the first available opportunity.
   Vector #(num_masters, Array #(Reg #(Bool)))      v_req_valid       <- replicateM (mkCReg (2, False));
   Vector #(num_masters, Array #(Reg #(CacheOp)))   v_req_op          <- replicateM (mkCRegU (2));
   Vector #(num_masters, Array #(Reg #(Bit #(3))))  v_req_f3          <- replicateM (mkCRegU (2));
`ifdef ISA_A
   Vector #(num_masters, Array #(Reg #(Bit #(7))))  v_req_amo_funct7  <- replicateM (mkCRegU (2));
`endif
   Vector #(num_masters, Array #(Reg #(WordXL)))    v_req_addr        <- replicateM (mkCRegU (2));
   Vector #(num_masters, Array #(Reg #(Bit #(64)))) v_req_st_value    <- replicateM (mkCRegU (2));
   Vector #(num_masters, Array #(Reg #(Priv_Mode))) v_req_priv        <- replicateM (mkCRegU (2));
   Vector #(num_masters, Array #(Reg #(Bit #(1))))  v_req_sstatus_SUM <- replicateM (mkCRegU (2));
   Vector #(num_masters, Array #(Reg #(Bit #(1))))  v_req_mstatus_MXR <- replicateM (mkCRegU (2));
   Vector #(num_masters, Array #(Reg #(WordXL)))    v_req_satp        <- replicateM (mkCRegU (2));

   // Latched cache output signals for when we've switched to another
   // master.
   Vector #(num_masters, Reg #(Bool))      v_rsp_valid      <- replicateM (mkReg (False));
   Vector #(num_masters, Reg #(WordXL))    v_rsp_addr       <- replicateM (mkRegU);
   Vector #(num_masters, Reg #(Bit #(64))) v_rsp_word64     <- replicateM (mkRegU);
   Vector #(num_masters, Reg #(Bit #(64))) v_rsp_st_amo_val <- replicateM (mkRegU);
   Vector #(num_masters, Reg #(Bool))      v_rsp_exc        <- replicateM (mkReg (False));
   Vector #(num_masters, Reg #(Exc_Code))  v_rsp_exc_code   <- replicateM (mkRegU);

   Vector #(num_masters, PulseWire) v_latched_rsp    <- replicateM (mkPulseWireOR);
   Vector #(num_masters, PulseWire) v_invalidate_rsp <- replicateM (mkPulseWireOR);
   Vector #(num_masters, PulseWire) v_tlb_flush      <- replicateM (mkPulseWire);

   for (Integer i = 0; i < valueOf(num_masters); i = i + 1) begin
      // The first instance can fire when enabled, but the rules for subsequent
      // masters are blocked by earlier ones. Unlike the other rules, such an
      // assertion would only be for performance (or avoiding deadlock), not
      // for correctness, as we would just risk buffering the request for
      // longer here and appearing like the cache is stalling, rather than some
      // other rules in this module which would miss signals on wires.
      rule rl_send_req (fn_master_can_acquire (i) && v_req_valid [i][1]);
	 fa_master_acquire (i);
	 v_req_valid [i][1] <= False;
	 cache.req (v_req_op          [i][1],
		    v_req_f3          [i][1],
		    v_req_amo_funct7  [i][1],
		    v_req_addr        [i][1],
		    v_req_st_value    [i][1],
		    v_req_priv        [i][1],
		    v_req_sstatus_SUM [i][1],
		    v_req_mstatus_MXR [i][1],
		    v_req_satp        [i][1]);
      endrule

      (* no_implicit_conditions, fire_when_enabled *)
      rule rl_latch_rsp (fn_master_is_active (i) && cache.valid && (! rg_resetting_or_flushing));
	 fa_master_release_early;
	 v_rsp_addr        [i] <= cache.addr;
	 v_rsp_word64      [i] <= cache.word64;
	 v_rsp_st_amo_val  [i] <= cache.st_amo_val;
	 v_rsp_exc         [i] <= cache.exc;
	 v_rsp_exc_code    [i] <= cache.exc_code;
	 v_latched_rsp [i].send;
      endrule

      (* no_implicit_conditions, fire_when_enabled *)
      rule rl_tlb_flush (v_tlb_flush [i]);
	 cache.tlb_flush;
	 // Ensure we update the latched valid signal in case we're
	 // not the current master or will switch away.
	 v_invalidate_rsp [i].send;
      endrule

      (* no_implicit_conditions, fire_when_enabled *)
      rule rl_update_valid;
	 if (v_invalidate_rsp [i])
	    v_rsp_valid [i] <= False;
	 else if (v_latched_rsp [i])
	    v_rsp_valid [i] <= True;
      endrule
   end

   // Various bookkeeping for shared state

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_update_active;
      if (pw_released_late)
	 rg_active <= False;
      else if (pw_acquired)
	 rg_active <= True;
      else if (pw_released_early)
	 rg_active <= False;
   endrule

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_update_master;
      function Bool pw_read (PulseWire pw) = pw;
      if (pw_acquired)
	 rg_master <= map (pw_read, v_master_acquired);
   endrule

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_update_resetting_flushing;
      let resetting = rg_resetting;
      let flushing = rg_flushing;

      if (pw_started_reset)
	 resetting = True;
      else if (pw_finished_reset)
	 resetting = False;

      if (pw_started_flush)
	 flushing = True;
      else if (pw_finished_flush)
	 flushing = False;

      rg_resetting <= resetting;
      rg_flushing <= flushing;

      rg_resetting_or_flushing <= resetting || flushing;
   endrule

   function MMU_Cache_IFC gen (Integer i);
      return
      interface MMU_Cache_IFC;
	 method Action set_verbosity (Bit #(4) verbosity) if (fn_master_can_acquire (i));
	    fa_master_acquire (i);
	    cache.set_verbosity (verbosity);
	    // set_verbosity is a single-cycle synchronous operation.
	    fa_master_release_late;
	 endmethod

	 interface Server server_reset;
	    interface Put request;
	       method Action put (Token t) if (fn_master_can_acquire (i) && (! rg_resetting));
		  fa_master_acquire (i);
		  cache.server_reset.request.put (t);
		  pw_started_reset.send;
	       endmethod
	    endinterface
	    interface Get response;
	       method ActionValue #(Token) get if (fn_master_is_active (i) && rg_resetting);
		  let t <- cache.server_reset.response.get;
		  pw_finished_reset.send;
		  // Update the latched valid signal in case we switch master.
		  v_invalidate_rsp [i].send;
		  fa_master_release_early;
		  return t;
	       endmethod
	    endinterface
	 endinterface

	 method Action req (CacheOp   op,
			    Bit #(3)  f3,
`ifdef ISA_A
			    Bit #(7)  amo_funct7,
`endif
			    WordXL    addr,
			    Bit #(64) st_value,
			    Priv_Mode priv,
			    Bit #(1)  sstatus_SUM,
			    Bit #(1)  mstatus_MXR,
			    WordXL    satp);
	    v_req_valid       [i][0] <= True;
	    v_req_op          [i][0] <= op;
	    v_req_f3          [i][0] <= f3;
`ifdef ISA_A
	    v_req_amo_funct7  [i][0] <= amo_funct7;
`endif
	    v_req_addr        [i][0] <= addr;
	    v_req_st_value    [i][0] <= st_value;
	    v_req_priv        [i][0] <= priv;
	    v_req_sstatus_SUM [i][0] <= sstatus_SUM;
	    v_req_mstatus_MXR [i][0] <= mstatus_MXR;
	    v_req_satp        [i][0] <= satp;
	    // Update the latched valid signal in case we don't issue the
	    // request this cycle.
	    v_invalidate_rsp [i].send;
	 endmethod

	 // If this master was in control of the cache in the previous
	 // cycle then the current output of the cache corresponds to
	 // this master's actions (regardless of whether we've gone
	 // idle). Otherwise, use whatever was latched.

	 method Bool valid;
	    return rg_master [i] ? cache.valid : v_rsp_valid [i];
	 endmethod

	 method WordXL addr;
	    return rg_master [i] ? cache.addr : v_rsp_addr [i];
	 endmethod

	 method Bit #(64) word64;
	    return rg_master [i] ? cache.word64 : v_rsp_word64 [i];
	 endmethod

	 method Bit #(64) st_amo_val;
	    return rg_master [i] ? cache.st_amo_val : v_rsp_st_amo_val [i];
	 endmethod

	 method Bool exc;
	    return rg_master [i] ? cache.exc : v_rsp_exc [i];
	 endmethod

	 method Exc_Code exc_code;
	    return rg_master [i] ? cache.exc_code : v_rsp_exc_code [i];
	 endmethod

	 interface Server server_flush;
	    interface Put request;
	       method Action put (Token t) if (fn_master_can_acquire (i) && (! rg_flushing));
		  fa_master_acquire (i);
		  cache.server_flush.request.put (t);
		  pw_started_flush.send;
	       endmethod
	    endinterface
	    interface Get response;
	       method ActionValue #(Token) get if (fn_master_is_active (i) && rg_flushing);
		  let t <- cache.server_flush.response.get;
		  pw_finished_flush.send;
		  // Update the latched valid signal in case we switch master.
		  v_invalidate_rsp [i].send;
		  fa_master_release_early;
		  return t;
	       endmethod
	    endinterface
	 endinterface

	 method Action tlb_flush if (fn_master_can_acquire (i));
	    fa_master_acquire (i);
	    v_tlb_flush [i].send;
	    // Update the latched valid signal in case we switch master.
	    v_invalidate_rsp [i].send;
	    // TLB flush is a single-cycle synchronous operation.
	    fa_master_release_late;
	 endmethod

	 // NOTE: These are all the same, as the cache's master port is
	 //       unfortunately part of the interface we need to
	 //       provide. Consumers should only connect the first!
	 interface mem_master = cache.mem_master;
      endinterface;
   endfunction

   interface v_from_masters = genWith (gen);
endmodule

endpackage : MMU_Cache_Arbiter
