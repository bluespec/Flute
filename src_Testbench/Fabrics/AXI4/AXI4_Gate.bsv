// Copyright (c) 2021 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4_Gate;

// ================================================================
// This package defines an AXI4-M-to-AXI4-S 'gate' module,
// that either allows or blocks the 5 AXI4 buses,
// depending on a Bool 'enable' input.

// ================================================================
// Bluespec library imports

import Vector       :: *;
import FIFOF        :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import AXI4_Types :: *;

// ================================================================
// The interface for the widener module

interface AXI4_Gate_IFC #(numeric type wd_id_t,
			  numeric type wd_addr_t,
			  numeric type wd_data_t,
			  numeric type wd_user_t);
   // From master
   interface AXI4_Slave_IFC  #(wd_id_t, wd_addr_t, wd_data_t, wd_user_t) axi4_S;
   // To slave
   interface AXI4_Master_IFC #(wd_id_t, wd_addr_t, wd_data_t, wd_user_t) axi4_M;

   // Enable control signal. Continuously driven with Bool arg.
   (* always_ready, always_enabled *)
   method Action m_enable (Bool enabled);
endinterface

// ================================================================
// The Gate module

module mkAXI4_Gate (AXI4_Gate_IFC #(wd_id_t, wd_addr_t, wd_data_t, wd_user_t));

   // ----------------
   // Transactor facing master
   AXI4_Slave_Xactor_IFC  #(wd_id_t, wd_addr_t, wd_data_t, wd_user_t)
      xactor_from_M <- mkAXI4_Slave_Xactor;

   // Transactor facing slave
   AXI4_Master_Xactor_IFC #(wd_id_t, wd_addr_t, wd_data_t, wd_user_t)
       xactor_to_S <- mkAXI4_Master_Xactor;

   Reg #(Bool) rg_enabled      <- mkReg (False);
   Reg #(Bool) rg_enabled_prev <- mkReg (False);

   // ----------------------------------------------------------------
   // BEHAVIOR
   // Each rule is basically a mkConnection controlled by 'enabled'

   rule rl_wr_addr (rg_enabled);
      let wra <- pop_o (xactor_from_M.o_wr_addr);
      xactor_to_S.i_wr_addr.enq (wra);
   endrule

   rule rl_wr_data (rg_enabled);
      let wrd <- pop_o (xactor_from_M.o_wr_data);
      xactor_to_S.i_wr_data.enq (wrd);
   endrule

   rule rl_wr_resp (rg_enabled);
      let wrr <- pop_o (xactor_to_S.o_wr_resp);
      xactor_from_M.i_wr_resp.enq (wrr);
   endrule
 
   rule rl_rd_addr (rg_enabled);
      let rda <- pop_o (xactor_from_M.o_rd_addr);
      xactor_to_S.i_rd_addr.enq (rda);
   endrule

   rule rl_rd_data (rg_enabled);
      let rdd <- pop_o (xactor_to_S.o_rd_data);
      xactor_from_M.i_rd_data.enq (rdd);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   interface axi4_S = xactor_from_M.axi_side;
   interface axi4_M = xactor_to_S  .axi_side;

   method Action m_enable (Bool enabled);
      if (enabled && (! rg_enabled))
	 $display ("%0d: %m: AXI4 ENABLING", cur_cycle);
      else if ((! enabled) && rg_enabled)
	 $display ("%0d: %m: AXI4 DISABLING", cur_cycle);

      rg_enabled      <= enabled;
   endmethod
endmodule

// ================================================================

endpackage: AXI4_Gate
