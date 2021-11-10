// Copyright (c) 2021 Rishiyur S. Nikhil and Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4_to_ST;

// ================================================================
// This package defines a component module for mkAXI4_to_LDST,
// that only handles the AXI4-Write to STORE part.
// Pease see mkAXI4_to_LDST.bsv for general comments and terminology.

// ================================================================

export AXI4_to_ST_IFC (..);
export mkAXI4_to_ST;

// ================================================================
// Bluespec library imports

import Vector       :: *;
import FIFOF        :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import Semi_FIFOF :: *;

// ================
// Local imports

import AXI4_Types         :: *;
import AXI4_to_LDST_utils :: *;

// ================================================================
// Stores: Interface

interface AXI4_to_ST_IFC #(numeric type wd_addr_t,
			   numeric type wd_ldst_data_t);

   interface FIFOF_O #(Tuple3 #(Bit #(2),                  // width B/H/W/D
				Bit #(wd_addr_t),          // addr,
				Bit #(wd_ldst_data_t)))    // wdata
             reqs;

   interface FIFOF_I #(Bool)    // True <=> err
             rsps;
endinterface

// ================================================================
// Stores: Module

// Module state, verbosity

typedef enum {STATE_IDLE,
	      STATE_PARTIAL,
	      STATE_SLICE,
	      STATE_FINISH_REQ,
	      STATE_ILLEGAL_REQ}
        STATE
deriving (Bits, Eq, FShow);

Integer verbosity = 0;

// ----------------
// Module

module mkAXI4_to_ST
   #(FIFOF_O #(AXI4_Wr_Addr #(wd_id_t, wd_addr_t, wd_user_t))  o_wr_addr,
     FIFOF_O #(AXI4_Wr_Data #(wd_axi_data_t, wd_user_t))       o_wr_data,
     FIFOF_I #(AXI4_Wr_Resp #(wd_id_t, wd_user_t))             i_wr_resp)
   (AXI4_to_ST_IFC #(wd_addr_t, wd_ldst_data_t))

   provisos (Add #(a__,             8,                                 wd_addr_t),
	     Mul #(wd_ldst_data_t,  slices_per_axi_data_t,             wd_axi_data_t),

	     Mul #(wdB_axi_data_t,  8,                                 wd_axi_data_t),
             Mul #(wdB_ldst_data_t, 8,                                 wd_ldst_data_t),

	     Add #(b__,             TLog #(TAdd #(1, wdB_ldst_data_t)), 8),

	     Bits #(Vector #(slices_per_axi_data_t,
			     Bit #(wdB_ldst_data_t)),  TDiv #(wd_axi_data_t, 8))
	     );

   // Integer bit-widths
   Integer wd_addr_I      = valueOf (wd_addr_t);
   Integer wd_axi_data_I  = valueOf (wd_axi_data_t);
   Integer wd_ldst_data_I = valueOf (wd_ldst_data_t);

   // Integer byte-widths
   Integer wdB_axi_data_I  = valueOf (wdB_axi_data_t);
   Integer wdB_ldst_data_I = valueOf (wdB_ldst_data_t);

   // Compile-time checks that widths are legal
   static_error_check_widths (wd_addr_I, wd_axi_data_I, wd_ldst_data_I);

   // ----------------------------------------------------------------
   // STATE

   // Outgoing requests
   FIFOF #(Tuple3 #(Bit #(2),                  // width B/H/W/D
		    Bit #(wd_addr_t),          // addr,
		    Bit #(wd_ldst_data_t)))    // wdata
         f_reqs <- mkFIFOF;

   // Incoming err/ok responses
   FIFOF #(Bool) f_rsps <- mkFIFOF;

   // ----------------
   // The following FIFOFs communicate to the store-response rules.

   // This fifo has a 0 token for every SB/SH/SW/SD issued
   // (we expect a response for each and accumulate errors).
   // These are followed by a sentinel indicating 'done' with this
   // axi transaction (this token is just a sentinel; no store response).
   FIFOF #(Bit #(1)) f_st_rsp_info <- mkSizedFIFOF (16);

   // This fifo has info for the AXI response after seeing the last response
   // (illegal_req, bid, buser)
   FIFOF #(Tuple3 #(Bool,
		    Bit #(wd_id_t),
		    Bit #(wd_user_t))) f_axi_rsp_info <- mkFIFOF;

   // ----------------

   Reg #(STATE) rg_state <- mkReg (STATE_IDLE);

   // Vector of slices of wdata, while iterating through slices.
   // [0] always has next slice to process
   Reg #(Vector #(slices_per_axi_data_t,
		  Bit #(wd_ldst_data_t))) rg_v_slice <- mkRegU;

   // Vector of slices of wstrb.
   // [0] always has next slice to process
   Reg #(Vector #(slices_per_axi_data_t,
		  Bit #(wdB_ldst_data_t))) rg_v_strb <- mkRegU;

   // Bytelane of current slice low byte
   Reg #(Bit #(8))              rg_bytelane_slice_lo <- mkRegU;

   // State while iterating partial slices within a slice
   Reg #(Bit #(wd_ldst_data_t)) rg_slice       <- mkRegU;
   Reg #(Bit #(8))              rg_bytelane_hi <- mkRegU;
   Reg #(Bit #(8))              rg_bytelane_lo <- mkRegU;

   // On erroneous requests, number of wr_data beats to consume (discard).
   // Starts as awlen, counts down to 0.
   Reg #(Bit #(8)) rg_discard_count <- mkRegU;

   // ================================================================
   // Values derived from AXI4 request awaddr and awsize

   let wr_addr_S = o_wr_addr.first;
   let wr_data_S = o_wr_data.first;

   Bit #(wd_addr_t) awaddr           = wr_addr_S.awaddr;
   // Address of byte lane [0] of AXI data bus containing awaddr
   Bit #(wd_addr_t) addr_axi_bus_lo  = fn_addr_to_NAPOT (awaddr,
							 fromInteger (wdB_axi_data_I));
   // Bytelane of awaddr on AXI data
   Bit #(8)         addr_bytelane    = fn_addr_to_axi_data_bytelane (awaddr, wdB_axi_data_I);

   // AWSIZE specifies a NAPOT window around awaddr, ...
   Bit #(8)         wdB_szwindow_B   = fv_AXI4_Size_to_num_bytes (wr_addr_S.awsize);
   // Address of NAPOT AWSIZE window containing awaddr
   Bit #(wd_addr_t) addr_szwindow_lo = fn_addr_to_NAPOT (awaddr, wdB_szwindow_B);

   // Bytelanes of LSByte and MSByte of AWSIZE window
   Bit #(8) szwindow_bytelane_lo = fn_addr_to_axi_data_bytelane (addr_szwindow_lo,
								 wdB_axi_data_I);
   Bit #(8) szwindow_bytelane_hi = szwindow_bytelane_lo + (wdB_szwindow_B - 1);

   Reg #(Bool) rg_cumulative_err <- mkReg (False);

   // ================================================================
   // BEHAVIOR

   // ----------------
   // For debug only

   function Action fa_show_values ();
      action
	 $display ("  awaddr %0h  (bytelane %0h bus_lo %0h) szwindow: %0d bytes",
		   awaddr, addr_bytelane, addr_axi_bus_lo, wdB_szwindow_B);
	 $display ("  szwindow: bytelanes [%0h..%0h] addr %0h",
		   szwindow_bytelane_hi,
		   szwindow_bytelane_lo,
		   addr_szwindow_lo);
     endaction
   endfunction

   // ----------------
   // This function issues one aligned SB/SH/SW/SD request from the current slice.
   // Returns number of bytes remaining to be written for this slice.

   function ActionValue #(Bit #(8))
            fav_store_req (Bit #(wd_ldst_data_t) slice,
			   Bit #(8)              bytelane_hi,
			   Bit #(8)              bytelane_lo,
			   Bit #(8)              bytelane_slice_lo);
      actionvalue
	 Bit #(8) num_bytes = bytelane_hi - bytelane_lo + 1;
	 if (verbosity > 0)
	    $display ("    STORE data %0h [%0h..%0h] (%0h bytes)",
		      slice, bytelane_hi, bytelane_lo, num_bytes);

	 Bit #(wd_addr_t) op_addr = (addr_axi_bus_lo | zeroExtend (bytelane_lo));

	 Bit #(8) shift_bytes = bytelane_lo - bytelane_slice_lo;
	 let shifted_slice = (slice >> (shift_bytes << 3));

	 Bit #(2) szcode;
	 Bit #(8) bytes_processed = 0;
	 if ((num_bytes == 1) || (bytelane_lo [0] == 1'b1)) begin
	    szcode          = ldst_b;
	    shifted_slice   = (shifted_slice & 'h_FF);
	    bytes_processed = 1;
	    if (verbosity > 0)
	       $display ("    SB addr %0h data %0h", op_addr, shifted_slice);
	 end
	 else if ((num_bytes < 4) || (bytelane_lo [1:0] == 2'b10)) begin
	    szcode          = ldst_h;
	    shifted_slice   = (shifted_slice & 'h_FF_FF);
	    bytes_processed = 2;
	    if (verbosity > 0)
	       $display ("    SH addr %0h data %0h", op_addr, shifted_slice);
	 end
	 else if (num_bytes < 8) begin
	    szcode          = ldst_w;
	    shifted_slice   = (shifted_slice & 'h_FF_FF_FF_FF);
	    bytes_processed = 4;
	    if (verbosity > 0)
	       $display ("    SW addr %0h data %0h", op_addr, shifted_slice);
	 end
	 else begin
	    szcode          = ldst_d;
	    bytes_processed = 8;
	    if (verbosity > 0)
	       $display ("    SD addr %0h data %0h", op_addr, shifted_slice);
	 end
	 f_reqs.enq (tuple3 (szcode, op_addr, shifted_slice));
	 f_st_rsp_info.enq (0);
	 rg_slice       <= slice;
	 rg_bytelane_hi <= bytelane_hi;
	 rg_bytelane_lo <= bytelane_lo + bytes_processed;
	 return num_bytes - bytes_processed;
      endactionvalue
   endfunction

   // ----------------
   // This function processes one slice:
   // Either ignore (if below awaddr or no wstrb)
   // or issue aligned SB/SH/SW/SD.
   // Returns next-state:
   //     PARTIAL if there are remaining bytes in this slice
   //     SLICE if no remaining bytes in this slice, but more slices in szwindow
   //     DONE if no more slices in szwindow

   function ActionValue #(STATE)
            fav_do_slice (Bit #(wd_ldst_data_t)  slice,
			  Bit #(wdB_ldst_data_t) slice_strb,
			  Bit #(8)               bytelane_slice_lo,
			  Bit #(8)               bytelane_slice_hi);
      actionvalue
	 if (verbosity > 0)
	    $display ("  fav_do_slice: slice %0h slice_strb %0b bytelane_slice hi..lo: %0h..%0h",
		      slice, slice_strb, bytelane_slice_hi, bytelane_slice_lo);

	 Bit #(8) num_lsb_zero_bytes = zeroExtend (pack (countZerosLSB (slice_strb)));
	 Bit #(8) num_msb_zero_bytes = zeroExtend (pack (countZerosMSB (slice_strb)));
	 if (verbosity > 0)
	    $display ("  num_lsb_zero_bytes %0h  num_msb_zero_bytes %0h",
		      num_lsb_zero_bytes, num_msb_zero_bytes);

	 Bit #(8) bytelane_lo = fn_max3 (bytelane_slice_lo,
					 bytelane_slice_lo + num_lsb_zero_bytes,
					 addr_bytelane);
	 Bit #(8) bytelane_hi = bytelane_slice_hi - num_msb_zero_bytes;
	 Bit #(8) rem_bytes   = 0;

	 if ((slice_strb == 0) || (bytelane_hi < bytelane_lo)) begin
	    if (verbosity > 0) $display ("  ignore");
	 end
	 else begin
	    if (verbosity > 0)
	       $display ("  store slice data %0h [%0h..%0h] (%0h bytes)",
			 slice, bytelane_hi, bytelane_lo, bytelane_hi - bytelane_lo + 1);
	    rem_bytes <- fav_store_req (slice,
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
	 $display ("%0d: %m.AXI4_to_ST:rl_start_xaction ================", cur_cycle);
	 fa_show_values();
      end

      // ----------------
      // Request legality checks
      Bool illegal_req = False;
      if (wdB_szwindow_B  > fromInteger (wdB_axi_data_I)) begin
	 if (verbosity == 0)
	    $display ("%0d: %m.AXI4_to_ST:rl_start_xaction ================", cur_cycle);
	 $display ("  ERROR: illegal AXI4 request");
	 $display ("    awsize 0x%0h bytes > axi data bus width 0x%0h bytes",
		   wdB_szwindow_B, wdB_axi_data_I);
	 illegal_req = True;
      end

      if (wr_addr_S.awlen != 0) begin
	 if (verbosity == 0)
	    $display ("%0d: %m.AXI4_to_ST:rl_start_xaction ================", cur_cycle);
	 $display ("  ERROR: illegal AXI4 request");
	 $display ("    awlen 0x%0h; only awlen 0 (1-beat bursts) supported",
		   wr_addr_S.awlen);
	 illegal_req = True;
      end

      if (! wr_data_S.wlast) begin
	 if (verbosity == 0)
	    $display ("%0d: %m.AXI4_to_ST:rl_start_xaction ================", cur_cycle);
	 $display ("  ERROR: illegal AXI4 request");
	 $display ("    wlast != 1; only 1-beat bursts supported");
	 illegal_req = True;
      end

      if (illegal_req)
	 $display ("  Discarding: ", fshow (wr_addr_S));

      // ----------------
      STATE next_state;
      if (illegal_req) begin
	 rg_discard_count <= wr_addr_S.awlen;
	 next_state = STATE_ILLEGAL_REQ;
      end
      else begin
	 Vector #(slices_per_axi_data_t,
		  Bit #(wd_ldst_data_t)) v_slice = unpack (wr_data_S.wdata);
	 Vector #(slices_per_axi_data_t,
		  Bit #(wdB_ldst_data_t)) v_strb = unpack (wr_data_S.wstrb);

	 Bit #(8) bytelane_slice_lo = 0;
	 Bit #(8) bytelane_slice_hi = fromInteger (wdB_ldst_data_I - 1);

	 next_state <- fav_do_slice (v_slice [0], v_strb [0],
				     bytelane_slice_lo, bytelane_slice_hi);

	 if (next_state != STATE_FINISH_REQ) begin
	    rg_v_slice           <= shiftInAtN (v_slice, 0);
	    rg_v_strb            <= shiftInAtN (v_strb,  0);
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
	 $display ("%0d: %m.AXI4_to_ST:rl_next_slice", cur_cycle);

      Vector #(slices_per_axi_data_t,
	       Bit #(wd_ldst_data_t))  v_slice = rg_v_slice;
      Vector #(slices_per_axi_data_t,
	       Bit #(wdB_ldst_data_t)) v_strb  = rg_v_strb;

      Bit #(8) bytelane_slice_lo = rg_bytelane_slice_lo;
      Bit #(8) bytelane_slice_hi = rg_bytelane_slice_lo + fromInteger (wdB_ldst_data_I - 1);

      let next_state <- fav_do_slice (v_slice [0], v_strb [0],
				      bytelane_slice_lo, bytelane_slice_hi);

      if (next_state == STATE_SLICE) begin
	 rg_v_slice           <= shiftInAtN (v_slice, 0);
	 rg_v_strb            <= shiftInAtN (v_strb,  0);
	 rg_bytelane_slice_lo <= bytelane_slice_hi + 1;
      end
      rg_state <= next_state;
      if (verbosity > 0) $display ("    => ", fshow (next_state));
   endrule

   // ----------------

   rule rl_partial (rg_state == STATE_PARTIAL);
      if (verbosity > 0) begin
	 $display ("%0d: %m.AXI4_to_ST:rl_partial", cur_cycle);
	 $display ("    rg_slice %0h  rg_bytelane_hi..lo [%0h..%0h] rg_bytelane_slice_lo %0h",
		   rg_slice, rg_bytelane_hi, rg_bytelane_lo, rg_bytelane_slice_lo);
      end

      Bit #(8) rem_bytes <- fav_store_req (rg_slice,
					   rg_bytelane_hi,
					   rg_bytelane_lo,
					   rg_bytelane_slice_lo);

      Vector #(slices_per_axi_data_t,
	       Bit #(wd_ldst_data_t))  v_slice = rg_v_slice;
      Vector #(slices_per_axi_data_t,
	       Bit #(wdB_ldst_data_t)) v_strb  = rg_v_strb;
      Bit #(8) bytelane_slice_hi = rg_bytelane_slice_lo + fromInteger (wdB_ldst_data_I - 1);

      STATE next_state = (  (rem_bytes != 0)
			  ? STATE_PARTIAL
			  : (  (bytelane_slice_hi < szwindow_bytelane_hi)
			     ? STATE_SLICE
			     : STATE_FINISH_REQ));

      if (next_state == STATE_SLICE) begin
	 rg_v_slice           <= shiftInAtN (v_slice, 0);
	 rg_v_strb            <= shiftInAtN (v_strb,  0);
	 rg_bytelane_slice_lo <= bytelane_slice_hi + 1;
      end
      rg_state <= next_state;
      if (verbosity > 0) $display ("    => ", fshow (next_state));
   endrule

   // ----------------

   rule rl_finish_req (rg_state == STATE_FINISH_REQ);
      if (verbosity > 0)
	 $display ("%0d: %m.AXI4_to_ST:rl_finish_req", cur_cycle);

      f_st_rsp_info.enq (1);    // 'done' sentinel
      f_axi_rsp_info.enq (tuple3 (False, wr_addr_S.awid, wr_addr_S.awuser));

      o_wr_addr.deq;
      o_wr_data.deq;
      rg_state <= STATE_IDLE;
   endrule

   // ----------------------------------------------------------------
   // Accumulate store responses and send final AXI response

   rule rl_handle_st_rsps (f_st_rsp_info.first == 1'b0);
      f_st_rsp_info.deq;
      Bool err = f_rsps.first;
      f_rsps.deq;
      rg_cumulative_err <= (rg_cumulative_err || err);

      if (verbosity > 0)
	 $display ("%0d: %m.AXI4_to_ST:rl_handle_st_rsps: err = %0d", cur_cycle, err);
   endrule

   // ----------------

   rule rl_send_axi_response (f_st_rsp_info.first == 1'b1);
      f_st_rsp_info.deq;
      match { .illegal_req, .bid, .buser } = f_axi_rsp_info.first;
      f_axi_rsp_info.deq;

      let wr_resp_S = AXI4_Wr_Resp {bid:   bid,
				    bresp: ((illegal_req || rg_cumulative_err)
					    ? axi4_resp_slverr
					    : axi4_resp_okay),
				    buser: buser};
      i_wr_resp.enq (wr_resp_S);

      if (verbosity > 0) begin
	 $display ("%0d: %m.AXI4_to_ST:rl_send_axi_response", cur_cycle);
	 $display ("    ", fshow (wr_resp_S));
      end
   endrule

   // ----------------------------------------------------------------
   // Illegal AXI requests (bad axsize, burst length > 1, ...)
   // rg_discard_count has been initialized to wr_addr.awlen
   // and is used here to count down while discarding wr_data beats.
   // Finally, we send an AXI error response.

   rule rl_illegal_req (rg_state == STATE_ILLEGAL_REQ);
      if (verbosity > 0) begin
	 $write ("%0d: %m.AXI4_to_ST:rl_illegal_req: rg_discard_count = %0h",
		 cur_cycle, rg_discard_count);
	 if (rg_discard_count == 0) $write (" (last)");
	 $display ("");
      end

      o_wr_data.deq;    // discard wr_data

      if (rg_discard_count != 0) begin
	 rg_discard_count <= rg_discard_count - 1;
      end
      else begin
	 f_st_rsp_info.enq (1);
	 f_axi_rsp_info.enq (tuple3 (True, wr_addr_S.awid, wr_addr_S.awuser));
	 o_wr_addr.deq;
	 rg_state <= STATE_IDLE;
      end
   endrule

   // ================================================================
   // INTERFACE

   interface reqs = to_FIFOF_O (f_reqs);
   interface rsps = to_FIFOF_I (f_rsps);
endmodule

// ================================================================

endpackage: AXI4_to_ST
