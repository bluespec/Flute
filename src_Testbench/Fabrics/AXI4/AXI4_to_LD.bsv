// Copyright (c) 2021 Rishiyur S. Nikhil and Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4_to_LD;

// ================================================================
// This package defines a component module for mkAXI4_to_LDST,
// that only handles the AXI4-Write to LOAD part.
// Pease see mkAXI4_to_LDST.bsv for general comments and terminology.

// ================================================================

export AXI4_to_LD_IFC (..);
export mkAXI4_to_LD;

// ================================================================
// Bluespec library imports

import Vector :: *;
import FIFOF  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import Semi_FIFOF :: *;

// ================
// Local imports

import AXI4_Types         :: *;
import AXI4_to_LDST_utils :: *;

// ================================================================
// Loads: Interface

interface AXI4_to_LD_IFC #(numeric type wd_addr_t,
			   numeric type wd_ldst_data_t);

   interface FIFOF_O #(Tuple2 #(Bit #(2),             // width B/H/W/D
				Bit #(wd_addr_t)))    // addr
             reqs;

   interface FIFOF_I #(Tuple2 #(Bool,                      // err <=> True
				Bit #(wd_ldst_data_t)))    // rdata
	     rsps;
endinterface

// Module state, verbosity

typedef enum {STATE_IDLE,
	      STATE_PARTIAL,
	      STATE_SLICE,
	      STATE_FINISH_REQ,
	      STATE_ILLEGAL_REQ}
    STATE
deriving (Bits, Eq, FShow);

typedef enum {RSP_NEW_SLICE,    // Create new slice with load-response
	      RSP_CUR_SLICE,    // OR load-response into current slice
	      RSP_NONE,         // No load-response, slice is 0
	      RSP_DONE}         // Done
    Rsp_Action
deriving (Bits, Eq, FShow);

Integer verbosity = 0;

// ================================================================
// Loads: Module

module mkAXI4_to_LD
   #(FIFOF_O #(AXI4_Rd_Addr #(wd_id_t, wd_addr_t, wd_user_t))  o_rd_addr,
     FIFOF_I #(AXI4_Rd_Data #(wd_id_t, wd_axi_data_t, wd_user_t))  i_rd_data)
   (AXI4_to_LD_IFC #(wd_addr_t, wd_ldst_data_t))

   provisos (Add #(a__,             8,                     wd_addr_t),
	     Mul #(wd_ldst_data_t,  slices_per_axi_data_t, wd_axi_data_t),

	     Mul #(wdB_axi_data_t,  8,                     wd_axi_data_t),
	     Mul #(wdB_ldst_data_t, 8,                     wd_ldst_data_t)
	     );

   // Integer bit-widths
   Integer wd_addr_I             = valueOf (wd_addr_t);
   Integer wd_axi_data_I         = valueOf (wd_axi_data_t);
   Integer wd_ldst_data_I        = valueOf (wd_ldst_data_t);
   Integer slices_per_axi_data_I = valueOf (slices_per_axi_data_t);

   // Integer byte-widths
   Integer wdB_axi_data_I  = valueOf (wdB_axi_data_t);
   Integer wdB_ldst_data_I = valueOf (wdB_ldst_data_t);

   // Compile-time checks that widths are legal
   static_error_check_widths (wd_addr_I, wd_axi_data_I, wd_ldst_data_I);

   // ----------------------------------------------------------------
   // STATE

   // Outgoing requests
   FIFOF #(Tuple2 #(Bit #(2),                  // width B/H/W/D
		    Bit #(wd_addr_t)))         // addr,
         f_reqs <- mkFIFOF;

   // Incoming err/ok responses
   FIFOF #(Tuple2 #(Bool,                      // True <=> err
		    Bit #(wd_ldst_data_t)))    // rdata
         f_rsps <- mkFIFOF;

   // ----------------
   // The following FIFOFs communicate to the load-response rules.

   // This fifo has info for every LB/LH/LW/LD issued
   // (we expect a response for each and accumulate errors).
   // These are followed by a sentinel indicating 'done' with this
   // axi transaction (this token is just a sentinel; no load response).
   FIFOF #(Tuple2 #(Rsp_Action,
		    Bit #(8)))    // left byte-shift amount for load-response data
   f_ld_rsp_info <- mkSizedFIFOF (16);

   // This fifo has info for the AXI response after seeing the last response
   // (illegal_req, rid, rdata, ruser)
   FIFOF #(Tuple3 #(Bool,
		    Bit #(wd_id_t),
		    Bit #(wd_user_t))) f_axi_rsp_info <- mkFIFOF;

   // ----------------

   Reg #(STATE) rg_state <- mkReg (STATE_IDLE);

   // Vector of slices of rdata, while iterating through slices.
   // [0] always has next slice to process
   Reg #(Vector #(slices_per_axi_data_t,
		  Bit #(wd_ldst_data_t))) rg_v_slice <- mkRegU;

   // Bytelane of current slice low byte
   Reg #(Bit #(8))              rg_bytelane_slice_lo <- mkRegU;

   // State while iterating partial slices within a slice
   Reg #(Bit #(wd_ldst_data_t)) rg_slice       <- mkRegU;
   Reg #(Bit #(8))              rg_bytelane_hi <- mkRegU;
   Reg #(Bit #(8))              rg_bytelane_lo <- mkRegU;

   Reg #(Bool) rg_cumulative_err <- mkReg (False);

   Reg #(Bit #(TLog #(TAdd #(1, slices_per_axi_data_t))))
   rg_remaining_slices <- mkReg (fromInteger (slices_per_axi_data_I));

   // ================================================================
   // Values derived from AXI4 request awaddr and awsize

   let rd_addr_S = o_rd_addr.first;

   Bit #(wd_addr_t) araddr           = rd_addr_S.araddr;
   // Address of byte lane [0] of AXI data bus containing araddr
   Bit #(wd_addr_t) addr_axi_bus_lo  = fn_addr_to_NAPOT (araddr,
							 fromInteger (wdB_axi_data_I));
   // Bytelane of araddr on AXI data
   Bit #(8)         addr_bytelane    = fn_addr_to_axi_data_bytelane (araddr, wdB_axi_data_I);

   // ARSIZE specifies a NAPOT window around araddr, ...
   Bit #(8)         wdB_szwindow_B   = fv_AXI4_Size_to_num_bytes (rd_addr_S.arsize);
   // Address of NAPOT ARSIZE window containing araddr
   Bit #(wd_addr_t) addr_szwindow_lo = fn_addr_to_NAPOT (araddr, wdB_szwindow_B);

   // Bytelanes of LSByte and MSByte of ARSIZE window
   Bit #(8) szwindow_bytelane_lo = fn_addr_to_axi_data_bytelane (addr_szwindow_lo,
								 wdB_axi_data_I);
   Bit #(8) szwindow_bytelane_hi = szwindow_bytelane_lo + (wdB_szwindow_B - 1);

   // ================================================================
   // BEHAVIOR

   // ----------------
   // For debug only

   function Action fa_show_axi_req_values ();
      action
	 $display ("  arid %0h araddr %0h  (bytelane %0h bus_lo %0h) szwindow: %0d bytes",
		   rd_addr_S.arid, araddr, addr_bytelane, addr_axi_bus_lo, wdB_szwindow_B);
	 $display ("  szwindow: bytelanes [%0h..%0h] addr %0h",
		   szwindow_bytelane_hi,
		   szwindow_bytelane_lo,
		   addr_szwindow_lo);
     endaction
   endfunction

   function Action fa_show_v_slices (Vector #(slices_per_axi_data_t,
					      Bit #(wd_ldst_data_t)) v_slices);
      action
	 for (Integer j = 0; j < valueOf (slices_per_axi_data_t); j = j + 1)
	    $display ("    %016h", v_slices [j]);
      endaction
   endfunction

   // ----------------
   // This function issues one aligned LB/LH/LW/LD request from the current slice.
   // Returns number of bytes remaining to be read for this slice.

   function ActionValue #(Bit #(8)) fav_load_req (Bool     new_slice,
						  Bit #(8) bytelane_hi,
						  Bit #(8) bytelane_lo,
						  Bit #(8) bytelane_slice_lo);
      actionvalue
	 Bit #(8) num_bytes = bytelane_hi - bytelane_lo + 1;
	 if (verbosity > 0)
	    $display ("    LOAD [%0h..%0h] (%0h bytes)",
		      bytelane_hi, bytelane_lo, num_bytes);

	 Bit #(wd_addr_t) op_addr = (addr_axi_bus_lo | zeroExtend (bytelane_lo));

	 Bit #(8) shift_bytes = bytelane_lo - bytelane_slice_lo;

	 Bit #(2) szcode;
	 Bit #(8) bytes_processed = 0;
	 if ((num_bytes == 1) || (bytelane_lo [0] == 1'b1)) begin
	    szcode          = ldst_b;
	    bytes_processed = 1;
	    if (verbosity > 0)
	       $display ("    LB addr %0h", op_addr);
	 end
	 else if ((num_bytes < 4) || (bytelane_lo [1:0] == 2'b10)) begin
	    szcode          = ldst_h;
	    bytes_processed = 2;
	    if (verbosity > 0)
	       $display ("    LH addr %0h", op_addr);
	 end
	 else if (num_bytes < 8) begin
	    szcode          = ldst_w;
	    bytes_processed = 4;
	    if (verbosity > 0)
	       $display ("    LW addr %0h", op_addr);
	 end
	 else begin
	    if (verbosity > 0)
	       $display ("    LD addr %0h", op_addr);
	    szcode          = ldst_d;
	    bytes_processed = 8;
	 end
	 f_reqs.enq (tuple2 (szcode, op_addr));
	 f_ld_rsp_info.enq (tuple2 ((new_slice ? RSP_NEW_SLICE : RSP_CUR_SLICE),
				    shift_bytes));
	 rg_bytelane_hi <= bytelane_hi;
	 rg_bytelane_lo <= bytelane_lo + bytes_processed;
	 return num_bytes - bytes_processed;
      endactionvalue
   endfunction

   // ----------------
   // This function starts a new slice:
   // Either ignore (if below araddr)
   // or issue aligned LB/LH/LW/LD.
   // Returns next-state:
   //     PARTIAL if there are remaining bytes in this slice
   //     SLICE if no remaining bytes in this slice, but more slices in szwindow
   //     DONE if no more slices in szwindow

   function ActionValue #(STATE)
            fav_start_slice (Bit #(8) bytelane_slice_lo,
			     Bit #(8) bytelane_slice_hi);
      actionvalue
	 if (verbosity > 0)
	    $display ("  fav_start_slice: bytelane_slice hi..lo: %0h..%0h",
		      bytelane_slice_hi, bytelane_slice_lo);

	 Bit #(8) bytelane_lo = max (bytelane_slice_lo, addr_bytelane);
	 Bit #(8) bytelane_hi = bytelane_slice_hi;
	 Bit #(8) rem_bytes   = 0;

	 if (bytelane_hi < bytelane_lo) begin
	    if (verbosity > 0) $display ("  ignore");
	    f_ld_rsp_info.enq (tuple2 (RSP_NONE, ?));
	 end
	 else begin
	    if (verbosity > 0)
	       $display ("  load slice [%0h..%0h] (%0h bytes)",
			 bytelane_hi, bytelane_lo, bytelane_hi - bytelane_lo + 1);
	    rem_bytes <- fav_load_req (True,        // new slice
				       bytelane_hi,
				       bytelane_lo,
				       bytelane_slice_lo);
	 end

	 STATE next_state = (  (rem_bytes != 0)
			     ? STATE_PARTIAL
			     : (  (bytelane_slice_hi < szwindow_bytelane_hi)
				? STATE_SLICE
				: STATE_FINISH_REQ));
	 if (verbosity > 0)
	    $display ("    rem_bytes = %0d, next_state = ",
		      rem_bytes, fshow (next_state));

	 return next_state;
      endactionvalue
   endfunction

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_start_xaction (rg_state == STATE_IDLE);
      if (verbosity > 0) begin
	 $display ("%0d: %m.AXI4_to_LD:rl_start_xaction ================", cur_cycle);
	 fa_show_axi_req_values();
      end

      // ----------------
      // Request legality checks
      Bool illegal_req = False;
      if (wdB_szwindow_B  > fromInteger (wdB_axi_data_I)) begin
	 if (verbosity == 0)
	    $display ("%0d: %m.AXI4_to_LD:rl_start_xaction ================", cur_cycle);
	 $display ("  ERROR: illegal AXI4 request");
	 $display ("    awsize 0x%0h bytes > axi data bus width 0x%0h bytes",
		   wdB_szwindow_B, wdB_axi_data_I);
	 illegal_req = True;
      end

      if (rd_addr_S.arlen != 0) begin
	 if (verbosity == 0)
	    $display ("%0d: %m.AXI4_to_LD:rl_start_xaction ================", cur_cycle);
	 $display ("  ERROR: illegal AXI4 request");
	 $display ("    arlen 0x%0h; only arlen 0 (1-beat bursts) supported",
		   rd_addr_S.arlen);
	 illegal_req = True;
      end

      if (illegal_req)
	 $display ("  Discarding: ", fshow (rd_addr_S));

      // ----------------
      STATE next_state;
      if (illegal_req) begin
	 next_state = STATE_ILLEGAL_REQ;
      end
      else begin
	 Bit #(8) bytelane_slice_lo = 0;
	 Bit #(8) bytelane_slice_hi = fromInteger (wdB_ldst_data_I - 1);

	 next_state <- fav_start_slice (bytelane_slice_lo, bytelane_slice_hi);

	 if (next_state != STATE_FINISH_REQ) begin
	    rg_bytelane_slice_lo <= ((next_state == STATE_PARTIAL)
				     ? bytelane_slice_lo
				     : bytelane_slice_hi + 1);
	 end
      end
      rg_state <= next_state;
      if (verbosity > 0) $display ("    => ", fshow (next_state));
   endrule

   // ----------------

   rule rl_next_slice (rg_state == STATE_SLICE);
      if (verbosity > 0)
	 $display ("%0d: %m.AXI4_to_LD:rl_next_slice", cur_cycle);

      Bit #(8) bytelane_slice_lo = rg_bytelane_slice_lo;
      Bit #(8) bytelane_slice_hi = rg_bytelane_slice_lo + fromInteger (wdB_ldst_data_I - 1);

      let next_state <- fav_start_slice (bytelane_slice_lo, bytelane_slice_hi);

      if (next_state == STATE_SLICE) begin
	 rg_bytelane_slice_lo <= bytelane_slice_hi + 1;
      end
      rg_state <= next_state;
      if (verbosity > 0) $display ("    => ", fshow (next_state));
   endrule

   // ----------------

   rule rl_partial (rg_state == STATE_PARTIAL);
      if (verbosity > 0) begin
	 $display ("%0d: %m.AXI4_to_LD:rl_partial", cur_cycle);
	 $display ("    rg_bytelane_hi..lo [%0h..%0h] rg_bytelane_slice_lo %0h",
		   rg_bytelane_hi, rg_bytelane_lo, rg_bytelane_slice_lo);
      end

      Bit #(8) rem_bytes <- fav_load_req (False,    // not new slice
					  rg_bytelane_hi,
					  rg_bytelane_lo,
					  rg_bytelane_slice_lo);

      Bit #(8) bytelane_slice_hi = rg_bytelane_slice_lo + fromInteger (wdB_ldst_data_I - 1);

      STATE next_state = (  (rem_bytes != 0)
			  ? STATE_PARTIAL
			  : (  (bytelane_slice_hi < szwindow_bytelane_hi)
			     ? STATE_SLICE
			     : STATE_FINISH_REQ));

      if (next_state == STATE_SLICE) begin
	 rg_bytelane_slice_lo <= bytelane_slice_hi + 1;
      end
      rg_state <= next_state;
      if (verbosity > 0) $display ("    => ", fshow (next_state));
   endrule

   // ----------------

   rule rl_finish_req (rg_state == STATE_FINISH_REQ);
      if (verbosity > 0)
	 $display ("%0d: %m.AXI4_to_LD:rl_finish_ld_req", cur_cycle);

      f_ld_rsp_info.enq (tuple2 (RSP_DONE, ?));    // 'done' sentinel
      f_axi_rsp_info.enq (tuple3 (False, rd_addr_S.arid, rd_addr_S.aruser));

      o_rd_addr.deq;
      rg_state <= STATE_IDLE;
   endrule

   // ----------------------------------------------------------------
   // Accumulate load responses and send final AXI response

   match { .rsp_op, .byte_shift_amt } = f_ld_rsp_info.first;

   rule rl_handle_ld_rsp ((rsp_op == RSP_NEW_SLICE) || (rsp_op == RSP_CUR_SLICE));
      f_ld_rsp_info.deq;
      match { .err, .ld_data } = f_rsps.first;
      f_rsps.deq;
      rg_cumulative_err <= (rg_cumulative_err || err);

      let slice   = fn_lshift_n_bytes (ld_data, byte_shift_amt);
      let v_slice = rg_v_slice;
      if (rsp_op == RSP_NEW_SLICE) begin
	 v_slice = shiftInAtN (v_slice, slice);
	 rg_remaining_slices <= rg_remaining_slices - 1;
      end
      else begin
	 let cur_slice = rg_v_slice [slices_per_axi_data_I - 1];
	 v_slice [slices_per_axi_data_I - 1] = (cur_slice | slice);
      end
      rg_v_slice <= v_slice;

      if (verbosity > 0) begin
	 $display ("%0d: %m.AXI4_to_LD:rl_handle_ld_rsp: err = %0h data %0h", cur_cycle,
		   err, ld_data);
	 $display ("  ShiftInAtN slice %016h; new v_slice", slice);
	 fa_show_v_slices (v_slice);
      end
   endrule

   // ----------------

   rule rl_handle_ld_slice_ignore (rsp_op == RSP_NONE);
      f_ld_rsp_info.deq;

      let v_slice = shiftInAtN (rg_v_slice, 0);
      rg_v_slice          <= v_slice;
      rg_remaining_slices <= rg_remaining_slices - 1;

      if (verbosity > 0) begin
	 $display ("%0d: %m.AXI4_to_LD:rl_handle_ld_slice_ignore: skipping slice", cur_cycle);
	 $display ("  new v_slice");
	 fa_show_v_slices (v_slice);
      end
   endrule

   // ----------------

   rule rl_shift_tail_slices ((rsp_op == RSP_DONE) && (rg_remaining_slices != 0));
      let v_slice = shiftInAtN (rg_v_slice, 0);
      rg_v_slice <= v_slice;
      rg_remaining_slices <= rg_remaining_slices - 1;

      if (verbosity > 0) begin
	 $display ("%0d: %m.AXI4_to_LD:rl_send_axi_response; shift only; new v_slice", cur_cycle);
	 fa_show_v_slices (v_slice);
      end
   endrule

   rule rl_send_axi_response ((rsp_op == RSP_DONE) && (rg_remaining_slices == 0));
      if (rg_remaining_slices == 0) begin
	 f_ld_rsp_info.deq;
	 match { .illegal_req, .rid, .ruser } = f_axi_rsp_info.first;
	 f_axi_rsp_info.deq;

	 Bit #(wd_axi_data_t) rdata = pack (rg_v_slice);

	 let rd_data_S = AXI4_Rd_Data {rid:   rid,
				       rresp: ((illegal_req || rg_cumulative_err)
					       ? axi4_resp_slverr
					       : axi4_resp_okay),
				       rdata: rdata,
				       ruser: ruser,
				       rlast: True};
	 i_rd_data.enq (rd_data_S);

	 // Get ready for next axi transaction
	 rg_remaining_slices <= fromInteger (slices_per_axi_data_I);

	 if (verbosity > 0) begin
	    $display ("%0d: %m.AXI4_to_LD:rl_send_axi_response", cur_cycle);
	    $display ("  AXI4_Rd_Data  rid %0h  rresp %0h  rlast %0d  ruser %0h",
		      rd_data_S.rid, rd_data_S.rresp, rd_data_S.rlast, rd_data_S.ruser);
	    fa_show_v_slices (rg_v_slice);
	 end
      end
      else begin
      end
   endrule

   // ----------------------------------------------------------------
   // Illegal AXI requests (bad axsize, burst length > 1, ...)
   // We send an AXI error response.

   rule rl_illegal_req (rg_state == STATE_ILLEGAL_REQ);
      if (verbosity > 0)
	 $display ("%0d: %m.AXI4_to_LD:rl_illegal_req", cur_cycle);

      f_ld_rsp_info.enq (tuple2 (RSP_DONE, ?));
      f_axi_rsp_info.enq (tuple3 (True, rd_addr_S.arid, rd_addr_S.aruser));
      o_rd_addr.deq;
      rg_state <= STATE_IDLE;
   endrule

   // ================================================================
   // INTERFACE

   interface reqs = to_FIFOF_O (f_reqs);
   interface rsps = to_FIFOF_I (f_rsps);
endmodule

// ================================================================

endpackage: AXI4_to_LD
