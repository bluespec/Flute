// Copyright (c) 2019 Bluespec, Inc. All Rights Reserved

package AXI4_Deburster;

// ================================================================
// This package defines a AXI4-slave-to-AXI4-slave conversion module.
// The parameter interface is an AXI4-slave that carries no burst transactions.
// The output interface is an AXI4-slave that carries burst transactions.

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

interface AXI4_Deburster_IFC #(numeric type wd_id,
			       numeric type wd_addr,
			       numeric type wd_data,
			       numeric type wd_user);
   method Action reset;

   // From master
   interface AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user) from_master;

   // To slave
   interface AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user) to_slave;
endinterface

// ================================================================
// The Deburster module
// The function parameter is an address-decode function, which
// returns (True,  slave-port-num)  if address is mapped to slave-port-num
//         (False, ?)               if address is unmapped to any slave port

module mkAXI4_Deburster (AXI4_Deburster_IFC #(wd_id, wd_addr, wd_data, wd_user))
   provisos (Add #(a__, 8, wd_addr));

   // 0 quiet; 1: display start of burst; 2: display all traffic
   Integer cfg_verbosity = 0;

   Reg #(Bool) rg_reset <- mkReg (True);

   // Transactor facing master
   AXI4_Slave_Xactor_IFC  #(wd_id, wd_addr, wd_data, wd_user)
      xactor_from_master <- mkAXI4_Slave_Xactor;

   // Transactor facing slave
   AXI4_Master_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user)
       xactor_to_slave <- mkAXI4_Master_Xactor;

   // On a write-transaction, this register is the W-channel burst beat count
   // (0 => start of burst)
   Reg #(AXI4_Len) rg_w_beat_count <- mkReg (0);

   // On a write-transaction, records awlen for slave
   // Size of FIFO should cover slave latency
   FIFOF #(AXI4_Len)  f_w_awlen <- mkSizedFIFOF (4);

   // On a write-transaction, this register is the B-channel burst beat count
   // which is the number of individual (non-burst) responses from the
   // slave to be combined into a single burst response to the master.
   // (0 => ready for next burst)
   Reg #(AXI4_Len) rg_b_beat_count <- mkReg (0);

   // On a burst write-transaction, all the individual slave responses
   // may not have the same 'resp' on the B channel. This register
   // remembers the first 'non-okay' resp (if any), to be returned to
   // the master in the burst response.
   Reg #(AXI4_Resp) rg_b_resp <- mkReg (axi4_resp_okay);

   // On a read-transaction, records arlen for slave
   // Size of FIFO should cover slave latency
   FIFOF #(AXI4_Len)  f_r_arlen <- mkSizedFIFOF (4);

   // On a read-transaction, this register is the AR-channel burst beat count
   // (0 => start of next burst)
   Reg #(AXI4_Len) rg_ar_beat_count <- mkReg (0);

   // On a read-transaction, this register is the R-channel burst beat count
   // (0 => ready for next burst)
   Reg #(AXI4_Len) rg_r_beat_count <- mkReg (0);

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
//       $display ("%0d: %m::AXI4_Deburster: wrapping burst. boundary: (%0x). addr: (%0x)", cur_cycle, wrap_boundary, addr);
//       // The wrapping condition
//       if ((addr % wrap_boundary) == 0) begin
//          // wrap the address - retain all bits except the wrap boundary bit
//          addr = addr & (~wrap_boundary);
//          $display ("%0d: %m::AXI4_Deburster: wrapping burst. Wrapping: addr: (%0x)", cur_cycle, addr);
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
      $display ("%0d: %m::AXI4_Deburster.rl_reset", cur_cycle);
      xactor_from_master.reset;
      xactor_to_slave.reset;

      f_w_awlen.clear;
      rg_w_beat_count <= 0;
      rg_b_beat_count <= 0;
      rg_b_resp       <= axi4_resp_okay;

      f_r_arlen.clear;
      rg_ar_beat_count <= 0;
      rg_r_beat_count  <= 0;

      rg_reset <= False;
   endrule

   // ----------------------------------------------------------------
   // BEHAVIOR
   Reg #(Bit #(wd_addr)) rg_last_beat_waddr <- mkRegU;

   // ----------------
   // Wr requests (AW and W channels)

   rule rl_wr_xaction_master_to_slave;
      AXI4_Wr_Addr #(wd_id, wd_addr, wd_user) a_in = xactor_from_master.o_wr_addr.first;
      AXI4_Wr_Data #(wd_data, wd_user)        d_in = xactor_from_master.o_wr_data.first;

      // Construct output AW item
      let a_out = a_in;
      // For the first beat the address is unchanged from the address in the
      // input request, for the remaining beats we have the update the address
      // based on the previous address used
      if (rg_w_beat_count != 0) begin
         a_out.awaddr = fv_addr_for_beat (rg_last_beat_waddr, a_in.awsize, a_in.awburst, a_in.awlen, rg_w_beat_count);
      end

      a_out.awlen   = 0;
      a_out.awburst = axburst_fixed; // Not necessary when awlen=1, but slave may be finicky

      // Set WLAST to true since this is always last beat of outgoing xaction (awlen=1)
      let d_out   = d_in;
      d_out.wlast = True;

      // Send to slave
      xactor_to_slave.i_wr_addr.enq (a_out);
      xactor_to_slave.i_wr_data.enq (d_out);

      xactor_from_master.o_wr_data.deq;

      // Remember burst length so that individual responses from slave
     // can be combined into a single burst response to the master.
      if (rg_w_beat_count == 0)
	 f_w_awlen.enq (a_in.awlen);

      if (rg_w_beat_count < a_in.awlen) begin
	 rg_w_beat_count <= rg_w_beat_count + 1;
      end
      else begin
	 // Last beat of incoming burst; done with AW item
	 xactor_from_master.o_wr_addr.deq;
	 rg_w_beat_count <= 0;

	 // Simulation-only assertion-check (no action, just display assertion failure)
	 // Last incoming beat must have WLAST = 1
	 if (! d_in.wlast) begin
	    $display ("%0d: ERROR: %m::AXI4_Deburster.rl_wr_xaction_master_to_slave: m -> s", cur_cycle);
	    $display ("    WLAST not set on last data beat (awlen = %0d)", a_in.awlen);
	    $display ("    ", fshow (d_in));
	 end
      end

      // Remember this beat's address for calculating the next beat address.
      // This is necessary to support wrapping bursts
      rg_last_beat_waddr <= a_out.awaddr;

      // Debugging
      if (cfg_verbosity > 0) begin
	 $display ("%0d: %m::AXI4_Deburster.rl_wr_xaction_master_to_slave: m -> s, beat %0d",
		   cur_cycle, rg_w_beat_count);
	 if (rg_w_beat_count == 0)
	    $display ("    a_in : ", fshow (a_in));
	 if ((rg_w_beat_count == 0) || (cfg_verbosity > 1)) begin
	    $display ("    d_in : ", fshow (d_in));
	    $display ("    a_out: ", fshow (a_out));
	    $display ("    d_out: ", fshow (d_out));
	 end
      end
   endrule: rl_wr_xaction_master_to_slave

   // ----------------
   // Wr responses (B channel): consume responses from slave until the
   // last response for a burst, then respond to master.  Remember if
   // any of them was not an 'okay' response.

   rule rl_wr_resp_slave_to_master;
      AXI4_Wr_Resp #(wd_id, wd_user) b_in <- pop_o (xactor_to_slave.o_wr_resp);

      if (rg_b_beat_count < f_w_awlen.first) begin
	 // Remember first non-okay response (if any) of a burst in rg_b_resp
	 if ((rg_b_resp == axi4_resp_okay) && (b_in.bresp != axi4_resp_okay))
	    rg_b_resp <= b_in.bresp;

	 // not last beat of burst
	 rg_b_beat_count <= rg_b_beat_count + 1;

	 if (cfg_verbosity > 1) begin
	    $display ("%0d: %m::AXI4_Deburster.rl_wr_resp_slave_to_master: m <- s, beat %0d",
		      cur_cycle, rg_b_beat_count);
	    $display ("    Consuming and discarding beat %0d", rg_b_beat_count);
	    $display ("    ", fshow (b_in));
	 end
      end
      else begin
	 // Last beat of burst
	 let b_out = b_in;
	 if (rg_b_resp != axi4_resp_okay)
	    b_out.bresp = rg_b_resp;
	 xactor_from_master.i_wr_resp.enq (b_out);

	 f_w_awlen.deq;

	 // Get ready for next burst
	 rg_b_beat_count <= 0;
	 rg_b_resp       <= axi4_resp_okay;

	 if (cfg_verbosity > 1) begin
	    $display ("%0d: %m::AXI4_Deburster.rl_wr_resp_slave_to_master: m <- s, beat %0d",
		      cur_cycle, rg_b_beat_count);
	    $display ("    b_in: ",  fshow (b_in));
	    $display ("    b_out: ", fshow (b_out));
	 end
      end
   endrule
 
  // ----------------
   // Rd requests (AR channel)

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
	 $display ("%0d: %m::AXI4_Deburster.rl_rd_xaction_master_to_slave: m -> s, addr %08x beat %0d",
		   cur_cycle, a_out.araddr, rg_ar_beat_count);
	 if (rg_ar_beat_count == 0)
	    $display ("    a_in:  ", fshow (a_in));
	 if ((rg_ar_beat_count == 0) || (cfg_verbosity > 1))
	    $display ("    a_out: ", fshow (a_out));
      end

   endrule: rl_rd_xaction_master_to_slave

   // ----------------
   // Rd responses

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
	 $display ("%0d: %m::AXI4_Deburster.rl_rd_resp_slave_to_master: m <- s, beat %0d",
		   cur_cycle, rg_r_beat_count);
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

endpackage: AXI4_Deburster
