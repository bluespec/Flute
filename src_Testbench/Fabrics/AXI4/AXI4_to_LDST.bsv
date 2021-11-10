// Copyright (c) 2021 Rishiyur S. Nikhil and Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4_to_LDST;

// ================================================================
// This package defines module mkAXI4_to_LDST and its interface.
// The axi4_S sub-interface is an AXI4 subordinate for single (not burst) transactions.
// The ldst_M sub-interface is a simple memory Load/Store interface where
// - the op is LB, LH, LW, LD, SB, SH, SW, SD
//     for loads/stores of bytes (8b), halfwords (16b), words (32b), doublewords (64b),
//     but only up to the ldst_M data bus width
// - addresses are properly aligned (lsbs are 0 for H, 00 for W and 000 for D)
// - data are always in LSBS no matter the address (unlike AXI4's lane-alignment)
// - The axi4_S data bus width must be >= ldst_M bus width
// - This module 'slices' the axi4_S data (width wd_axi_data)
//     into slices of width wd_ldst_data for the load/store data bus.

// Terminology:
//   POT                    power of two
//   NAPOT                  naturally aligned power of two
//   wd_...                 width in bits
//   wdB_...                width in bytes
//   wd_..._t               width as a type (numeric kind)
//   wd_..._I               width as an Integer value
//   wd_..._B               width as a Bit #(n) value

//   wd_axi_data            width of AXI4 data bus
//   wd_ldst_data           width of load-store data bus
//   slice                  NAPOT slice of axi wdata/rdata, of width wd_ldst_data 
//   slices_per_axi_data    number of wd_ldst_data slices in wd_axi_data
//   szwindow               NAPOT window of size specified by AWSIZE, containing AWADDR

// ================================================================

export ldst_b, ldst_h, ldst_w, ldst_d;
export AXI4_to_LDST_IFC (..);
export mkAXI4_to_LDST;

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
import AXI4_to_LD         :: *;
import AXI4_to_ST         :: *;

// ================================================================
// The interface for the module

interface AXI4_to_LDST_IFC #(numeric type wd_id_t,
			     numeric type wd_addr_t,
			     numeric type wd_axi_data_t,
			     numeric type wd_user_t,
			     numeric type wd_ldst_data_t);

   interface AXI4_Slave_IFC #(wd_id_t, wd_addr_t, wd_axi_data_t, wd_user_t)
             axi4_S;

   // Stores
   interface FIFOF_O #(Tuple3 #(Bit #(2),                  // width B/H/W/D
				Bit #(wd_addr_t),          // addr
				Bit #(wd_ldst_data_t)))    // wdata
             st_reqs;

   interface FIFOF_I #(Bool)    // True <=> err
             st_rsps;

   // Loads
   interface FIFOF_O #(Tuple2 #(Bit #(2),             // width B/H/W/D
				Bit #(wd_addr_t)))    // addr
             ld_reqs;

   interface FIFOF_I #(Tuple2 #(Bool,                      // True <=> err
				Bit #(wd_ldst_data_t)))    // rdata
             ld_rsps;
endinterface

// ================================================================
// The module (uses separate modules, below, for Load and Store, respectively

module mkAXI4_to_LDST (AXI4_to_LDST_IFC #(wd_id_t,
					  wd_addr_t,
					  wd_axi_data_t,
					  wd_user_t,
					  wd_ldst_data_t))

   provisos (Add #(a__,             8,                     wd_addr_t),

	     Mul #(wd_ldst_data_t,  slices_per_axi_data_t, wd_axi_data_t),

	     Mul #(wdB_axi_data_t,  8,                     wd_axi_data_t),
	     // bsc demands this next proviso though it seems redundant
	     // (maybe not redundant due to integer div?)
	     Div #(wd_axi_data_t,   8,                     wdB_axi_data_t),

             Mul #(wdB_ldst_data_t, 8,                     wd_ldst_data_t),

	     // Redundant, but bsc doesn't work it out
	     Mul #(wdB_ldst_data_t, slices_per_axi_data_t, wdB_axi_data_t),

	     Add #(b__,             TLog #(TAdd #(1, wdB_ldst_data_t)), 8)
	     );

   // Transactor for axi4_S
   AXI4_Slave_Xactor_IFC  #(wd_id_t, wd_addr_t, wd_axi_data_t, wd_user_t)
      axi4_S_xactor <- mkAXI4_Slave_Xactor;

   // Store converter
   AXI4_to_ST_IFC #(wd_addr_t, wd_ldst_data_t)
   st_ifc <- mkAXI4_to_ST (axi4_S_xactor.o_wr_addr,
			   axi4_S_xactor.o_wr_data,
			   axi4_S_xactor.i_wr_resp);

   // Load converter
   AXI4_to_LD_IFC #(wd_addr_t, wd_ldst_data_t)
   ld_ifc <- mkAXI4_to_LD (axi4_S_xactor.o_rd_addr,
			   axi4_S_xactor.i_rd_data);

   // ----------------------------------------------------------------
   // INTERFACE

   interface axi4_S = axi4_S_xactor.axi_side;

   interface st_reqs = st_ifc.reqs;
   interface st_rsps = st_ifc.rsps;

   interface ld_reqs = ld_ifc.reqs;
   interface ld_rsps = ld_ifc.rsps;
endmodule

// ================================================================

endpackage: AXI4_to_LDST
