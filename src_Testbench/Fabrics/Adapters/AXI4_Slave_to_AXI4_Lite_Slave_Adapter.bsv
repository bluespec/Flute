// Copyright (c) 2020 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4_Slave_to_AXI4_Lite_Slave_Adapter;

// ================================================================
// This package defines an adapter that does two things:
// - AXI4 slave to AXI4-Lite slave
// - Wider to narrower

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
import AXI4_Types      :: *;

// ================================================================
// The interface for the adapter module

interface AXI4_Slave_to_AXI4_Lite_Slave_Adapter_IFC #(numeric type wd_id_AXI4_S,
						      numeric type wd_addr_AXI4_S,
						      numeric type wd_data_AXI4_S,
						      numeric type wd_user_AXI4_S,

						      numeric type wd_addr_AXI4L_M,
						      numeric type wd_data_AXI4L_M,
						      numeric type wd_user_AXI4L_M
						      );
   method Action reset;

   // From master
   interface AXI4_Slave_IFC #(wd_id_AXI4_S, wd_addr_AXI4_S, wd_data_AXI4_S, wd_user_AXI4_S) from_master;

   // To slave
   interface AXI4_Lite_Master_IFC #(wd_addr_AXI4L_M, wd_data_AXI4L_M, wd_user_AXI4L_M) to_slave;
endinterface

// ================================================================
// The adapter module

module mkAXI4_Slave_to_AXI4_Lite_Slave_Adapter
   (AXI4_Slave_to_AXI4_Lite_Slave_Adapter_IFC #(wd_id_AXI4_S, wd_addr_AXI4_S, wd_data_AXI4_S, wd_user_AXI4_S,
						wd_addr_AXI4L_M, wd_data_AXI4L_M, wd_user_AXI4L_M))
   provisos (Add #(wd_addr_AXI4L_M,          __a, wd_addr_AXI4_S),
	     Mul #(wd_data_AXI4L_M, x_downsize_t, wd_addr_AXI4_S),
	     Add #(wd_user_AXI4L_M,          __u, wd_addr_AXI4_S));

   Integer x_downsize_i       = ValueOf #(x_downsize_t);
   Integer bytes_per_word_out = ValueOf (wd_data_AXI4L_M);

   // 0 quiet; 1: display start of burst; 2: display all traffic
   Integer cfg_verbosity = 0;

   Reg #(Bool) rg_reset <- mkReg (True);

   // Transactor facing master
   AXI4_Slave_Xactor_IFC  #(wd_id_AXI4_S, wd_addr_AXI4_S, wd_data_AXI4_S, wd_user_AXI4_S)
       xactor_from_master <- mkAXI4_Slave_Xactor;

   // Transactor facing slave
   AXI4_Lite_Master_Xactor_IFC #(wd_addr_AXI4L_M, wd_data_AXI4L_M, wd_user_AXI4L_M)
       xactor_to_slave <- mkAXI4_Lite_Master_Xactor;

   // ----------------------------------------------------------------
   // Compute address for beat

// function ActionValue#(Bit #(wd_addr)) fv_addr_for_beat (Bit #(wd_addr) start_addr,
//      				     AXI4_Size      axsize,
//      				     AXI4_Burst     axburst,
//                                           AXI4_Len       axlen,
//      				     AXI4_Len       beat_count);
//
//    actionvalue
//    // For incrementing bursts this address is the next address
//    Bit #(wd_addr) addr = start_addr;
//    addr = start_addr + (1 << pack (axsize));
//
//    // The actual length of the burst is one more than indicated by axlen
//    Bit #(wd_addr) burst_len = zeroExtend (axlen) + 1;
//
//    // find the wrap boundary bit - this becomes the mask - will only work
//    // for burst lengths which are a power of two
//    Bit #(wd_addr) wrap_boundary = (burst_len << pack (axsize));
//
//    // For wrapping bursts the wrap_mask needs to be applied to check if the
//    // wrapping boundary has been reached
//    if (axburst == axburst_wrap) begin
//       $display ("%0d: %m::AXI4_Slave_to_AXI4_Lite_Slave_Adapter: wrapping burst. boundary: (%0x). addr: (%0x)", cur_cycle, wrap_boundary, addr);
//       // The wrapping condition
//       if ((addr % wrap_boundary) == 0) begin
//          // wrap the address - retain all bits except the wrap boundary bit
//          addr = addr & (~wrap_boundary);
//          $display ("%0d: %m::AXI4_Slave_to_AXI4_Lite_Slave_Adapter: wrapping burst. Wrapping: addr: (%0x)", cur_cycle, addr);
//       end
//    end
//    return addr;
//    endactionvalue
// endfunction

   function Bit #(wd_addr) fv_addr_for_beat (Bit #(wd_addr) start_addr,
					     AXI4_Size      axsize,
					     AXI4_Burst     axburst,
                                             AXI4_Len       axlen,
					     AXI4_Len       beat_count);

      // For incrementing bursts this address is the next address
      Bit #(wd_addr) addr = start_addr;
      addr = start_addr + (1 << pack (axsize));

      // The actual length of the burst is one more than indicated by axlen
      Bit #(wd_addr) burst_len = zeroExtend (axlen) + 1;

      // Compute the mask used to wrap the address, given that burst lenths are
      // always powers of two
      Bit #(wd_addr) wrap_mask = (burst_len << pack (axsize)) - 1;

      // For wrapping bursts the wrap_mask needs to be applied to wrap the
      // address round when it reaaches the boundary
      if (axburst == axburst_wrap) begin
         addr = (start_addr & (~ wrap_mask)) | (addr & wrap_mask);
      end
      return addr;
   endfunction

   // ----------------------------------------------------------------
   // RESET

   rule rl_reset (rg_reset);
      $display ("%0d: %m::AXI4_Slave_to_AXI4_Lite_Slave_Adapter.rl_reset", cur_cycle);
      xactor_from_master.reset;
      xactor_to_slave.reset;

      f_w_awlen.clear;
      rg_w_beat_count <= 0;
      rg_w_req_count  <= 0;

      rg_b_rsp_count  <= 0;
      rg_b_resp       <= axi4_resp_okay;

      f_r_arlen.clear;
      rg_ar_beat_count <= 0;
      rg_r_beat_count  <= 0;

      rg_reset <= False;
   endrule

   // ----------------------------------------------------------------
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Wr requests (AW and W channels)

   // AXI4 write-requests are expanded into multiple AXI4L
   // write-requests, due to bursts and due to data-downsizing

   // AXI4 W-channel burst beat count (0 => start of burst)
   Reg #(AXI4_Len)            rg_w_beat_count    <- mkReg (0);
   Reg #(Bit #(wd_addr_AXI4)) rg_last_beat_waddr <- mkRegU;

   // Index of AXI4L data word in AXI4 data word
   Reg #(AXI4_Len) rg_wdata_subword <- mkReg (0);

   // Record actions for write-responses from AXI4L slave:
   //     False = not-last; accumulate error response
   //     True  = last; send AXI4 B response
   // Size of FIFO should cover slave latency
   FIFOF #(Bool)  f_w_awlen <- mkSizedFIFOF (16);

   rule rl_wr_xaction_master_to_slave;
      AXI4_Wr_Addr #(wd_id_AXI4_S, wd_addr_AXI4_S, wd_user_AXI4_S) a_in = xactor_from_master.o_wr_addr.first;
      AXI4_Wr_Data #(wd_data_AXI4_S, wd_user_AXI4_S)               d_in = xactor_from_master.o_wr_data.first;

      // For the first AXI4 beat the address is same as address in the
      // input request; for remaining beats we have to update the
      // address based on the previous address used
      Bit #(wd_addr_AXI4) beat_waddr = ((rg_w_beat_count != 0)
					? a_in.awaddr
					: fv_addr_for_beat (rg_last_beat_waddr,
							    a_in.awsize,
							    a_in.awburst,
							    a_in.awlen,
							    rg_w_beat_count));
      rg_last_beat_waddr <= beat_waddr;

      // Output address
      Bit #(wd_addr_AXI4L) addr_out = truncate (beat_waddr + rg_wdata_subword * bytes_per_AXI4L_word)

      a_out.awaddr = beat_waddr.awaddr + fromInteger (bytes_per_word_out);


      // Construct output AXI4L AW item
      let a_out = AXI4_Lite_Wr_Addr {awaddr: beat_waddr,
				     awprot: a_in.awprot,
				     awuser: truncate (a_in.aw_user) };




      // Construct output AXI4L WD item
      // Set WLAST to true since this is always last beat of outgoing xaction (awlen=1)
      Vector #(x_downsize_t, Bit #(wd_data_AXI4L_S)) v_data = unpack (pack (d_in.wdata));
      Vector #(xstrb_t,      Bit #(wd_strb_AXI4L_S)) v_strb = unpack (pack (d_in.wstrb));
      let d_out   = AXI4_Lite_Wr_Data {v_data [rg_wdata_subword],
				       v_wstrb [rg_wdata_subword]};

      // Send to slave if non-zero wstrb
      xactor_to_slave.i_wr_addr.enq (a_out);
      xactor_to_slave.i_wr_data.enq (d_out);

      // Remember burst length so that individual responses from slave
      // can be combined into a single burst response to the master.
      if ((rg_w_beat_count == 0) && (rg_wdata_subword == 0))
	 f_w_awlen.enq (a_in.awlen);

      // Increment indexes
      Bool last_subword = (rg_wdata_subword == fromInteger (x_downsize_i - 1));
      Bool last_beat    = (rg_w_beat_count == a_in.awlen);

      if (! last_subword) begin
	 rg_wdata_subword <= rg_wdata_subword + 1;
      end
      else begin
	 // Last subword; done with wdata item
	 xactor_from_master.o_wr_data.deq;
	 rg_wdata_subword <= 0;

	 if  (! last beat) begin
	    rg_w_beat_count <= rg_w_beat_count + 1;
	 end
	 else begin
	    // Last beat of incoming burst; done with AW item
	    xactor_from_master.o_wr_addr.deq;
	    rg_w_beat_count <= 0;

	    // Simulation-only assertion-check (no action, just display assertion failure)
	    // Last incoming beat must have WLAST = 1
	    if (! d_in.wlast) begin
	       $display ("%0d: ERROR: %m.rl_wr_xaction_master_to_slave: m -> s", cur_cycle);
	       $display ("    WLAST not set on last data beat (awlen = %0d)", a_in.awlen);
	       $display ("    ", fshow (d_in));
	    end
	 end
      end

      // Debugging
      if (cfg_verbosity > 0) begin
	 $display ("%0d: %m.rl_wr_xaction_master_to_slave: m -> s, beat %0d", cur_cycle, rg_w_beat_count);
	 if (rg_w_beat_count == 0)
	    $display ("    a_in : ", fshow (a_in));
	 if ((rg_w_beat_count == 0) || (cfg_verbosity > 1)) begin
	    $display ("    d_in : ", fshow (d_in));
	    $display ("    a_out: ", fshow (a_out));
	    $display ("    d_out: ", fshow (d_out));
	 end
      end
   endrule: rl_wr_xaction_master_to_slave

   // ----------------------------------------------------------------
   // Wr responses (B channel): consume responses from slave until the
   // last response for a burst, then respond to master.  Remember if
   // any of them was not an 'okay' response.

   // On a burst write-transaction, all the individual slave responses
   // may not have the same 'resp' on the B channel. This register
   // remembers the first 'non-okay' resp (if any), to be returned to
   // the master in the burst response.
   Reg #(AXI4_Resp) rg_b_resp <- mkReg (axi4_resp_okay);

   rule rl_wr_resp_slave_to_master;
      AXI4_Wr_Resp #(wd_id, wd_user) b_in <- pop_o (xactor_to_slave.o_wr_resp);

      Bool last_rsp = (rg_b_rsp_count < ((f_w_awlen.first + 1) * x_downsize_i);

      if (! last_rsp) begin
	 // Remember first non-okay response (if any) of a burst in rg_b_resp
	 if ((rg_b_resp == axi4_resp_okay) && (b_in.bresp != axi4_resp_okay))
	    rg_b_resp <= b_in.bresp;

	 rg_b_rsp_count <= rg_b_rsp_count + 1;

	 if (cfg_verbosity > 1) begin
	    $display ("%0d: %m.rl_wr_resp_slave_to_master: m <- s, beat %0d", cur_cycle, rg_b_rsp_count);
	    $display ("    Consuming and discarding beat %0d", rg_b_rsp_count);
	    $display ("    ", fshow (b_in));
	 end
      end
      else begin
	 // Last response
	 let b_out = b_in;
	 if (rg_b_resp != axi4_resp_okay)
	    b_out.bresp = rg_b_resp;
	 xactor_from_master.i_wr_resp.enq (b_out);

	 f_w_awlen.deq;

	 // Get ready for next burst
	 rg_b_rsp_count <= 0;
	 rg_b_resp      <= axi4_resp_okay;

	 if (cfg_verbosity > 1) begin
	    $display ("%0d: %m.rl_wr_resp_slave_to_master: m <- s, beat %0d", cur_cycle, rg_b_rsp_count);
	    $display ("    b_in: ",  fshow (b_in));
	    $display ("    b_out: ", fshow (b_out));
	 end
      end
   endrule
 
   // ----------------------------------------------------------------
   // Rd requests (AR channel)
   // AXI4 Burst-read requests must be expanded into multiple AXI4L read requests.

   // The AR-channel burst beat count (# of reqs sent to AXI4L slave)
   // (0 => start of next burst)
   Reg #(AXI4_Len) rg_ar_beat_count <- mkReg (0);

   // Record arlen for read-responses from slave
   // Size of FIFO should cover slave latency
   FIFOF #(AXI4_Len)  f_r_arlen <- mkSizedFIFOF (16);

   Reg #(Bit #(wd_addr)) rg_last_beat_raddr <- mkRegU;
   rule rl_rd_xaction_master_to_slave;
      AXI4_Rd_Addr #(wd_id, wd_addr, wd_user) a_in = xactor_from_master.o_rd_addr.first;

      // Compute forwarded request for each beat, and send
      let a_out = a_in;

      // For the first beat the address is unchanged from the address in the
      // input request, for the remaining beats we have the update the address
      // based on the previous address used
      if (rg_ar_beat_count != 0) begin
         a_out.araddr = fv_addr_for_beat (rg_last_beat_raddr, a_in.arsize, a_in.arburst, a_in.arlen, rg_ar_beat_count);
      end

      a_out.arlen   = 0;
      a_out.arburst = axburst_fixed; // Not necessary when arlen=1, but slave may be finicky
      xactor_to_slave.i_rd_addr.enq (a_out);

      // On first beat, set up the response count
      if (rg_ar_beat_count == 0)
	 f_r_arlen.enq (a_in.arlen);

      if (rg_ar_beat_count < a_in.arlen) begin
	 rg_ar_beat_count <= rg_ar_beat_count + 1;
      end
      else begin
	 // Last beat sent; done with AR item
	 xactor_from_master.o_rd_addr.deq;
	 rg_ar_beat_count <= 0;
      end

      // Remember this beat's address for calculating the next beat address.
      // This is necessary to support wrapping bursts
      rg_last_beat_raddr <= a_out.araddr;

      // Debugging
      if (cfg_verbosity > 0) begin
	 $display ("%0d: %m.rl_rd_xaction_master_to_slave: m -> s, addr %08x beat %0d",
		   cur_cycle, a_out.araddr, rg_ar_beat_count);
	 if (rg_ar_beat_count == 0)
	    $display ("    a_in:  ", fshow (a_in));
	 if ((rg_ar_beat_count == 0) || (cfg_verbosity > 1))
	    $display ("    a_out: ", fshow (a_out));
      end

   endrule: rl_rd_xaction_master_to_slave

   // ----------------------------------------------------------------
   // Rd responses

   // The R-channel burst beat count
   // (0 => ready for next burst)
   Reg #(AXI4_Len) rg_r_beat_count <- mkReg (0);

   rule rl_rd_resp_slave_to_master;
      AXI4_Rd_Data #(wd_id, wd_data, wd_user) r_in <- pop_o (xactor_to_slave.o_rd_data);
      let arlen = f_r_arlen.first;

      let r_out = r_in;
      if (rg_r_beat_count < arlen) begin
	 // not last beat of burst
	 r_out.rlast = False;
	 rg_r_beat_count <= rg_r_beat_count + 1;
      end
      else begin
	 // Last beat of burst
	 rg_r_beat_count <= 0;
	 r_out.rlast = True;    // should be set already, but override if not
	 f_r_arlen.deq;
      end

      xactor_from_master.i_rd_data.enq (r_out);

      // Debugging
      if (cfg_verbosity > 0) begin
	 $display ("%0d: %m.rl_rd_resp_slave_to_master: m <- s, beat %0d", cur_cycle, rg_r_beat_count);
	 if ((rg_r_beat_count == 0) || (cfg_verbosity > 1)) begin
	    $display ("    r_in:  ", fshow (r_in));
	    $display ("    r_out: ", fshow (r_out));
	 end
      end
   endrule: rl_rd_resp_slave_to_master

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset () if (! rg_reset);
      rg_reset <= True;
   endmethod

   interface from_master = xactor_from_master.axi_side;
   interface to_slave    = xactor_to_slave   .axi_side;
endmodule

// ================================================================

endpackage: AXI4_Slave_to_AXI4_Lite_Slave_Adapter
