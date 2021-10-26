// Copyright (c) 2021 Rishiyur S. Nikhil and Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4_to_LDST_utils;

// ================================================================
// This package contains utilities used by
//     AXI4_to_LDST (and its components AXI4_to_ST and AXI4_to_LD)
// Please see AXI4_to_LDST for general comments, terminology etc.

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
// Bluespec library imports

import Vector :: *;

// ================================================================
// Encoding of LD/ST size on ldst_M side

Bit #(2) ldst_b =  2'b00;
Bit #(2) ldst_h =  2'b01;
Bit #(2) ldst_w =  2'b10;
Bit #(2) ldst_d =  2'b11;

// ================================================================
// Round down addr to NAPOT (naturally aligned power of two)
// where wdB is a power of two.

function Bit #(wd_addr_t) fn_addr_to_NAPOT (Bit #(wd_addr_t) addr, Bit #(8) wdB);
   let              wd_0_lsbs = countZerosLSB (wdB);
   Bit #(wd_addr_t) mask      = ('1 << wd_0_lsbs);
   return (addr & mask);
endfunction   

// ================================================================
// Compute byte lane for addr on a given AXI bus width

function Bit #(8) fn_addr_to_axi_data_bytelane (Bit #(wd_addr_t) addr,
					       Integer           wdB_axi_data_I);
   Integer          wd_0_lsbs = log2 (wdB_axi_data_I);
   Bit #(wd_addr_t) mask      = (1 << wd_0_lsbs) - 1;
   Bit #(wd_addr_t) bytelane  = (addr & mask);
   return bytelane [7:0];
endfunction   

// ================================================================
// Max of 3 numbers

function Bit #(n) fn_max3 (Bit #(n) x, Bit #(n) y, Bit #(n) z);
   return max (x, max (y, z));
endfunction

// ================================================================
// Shift by a number of bytes
// TODO: do these result in efficient HW?
//       i.e., exploit fact that it's in units of bytes, not bits?

function Bit #(nb) fn_lshift_n_bytes (Bit #(nb) x, Bit #(nsa) byte_shift_amt);
   return (x << (byte_shift_amt << 3));
endfunction

function Bit #(nb) fn_rshift_n_bytes (Bit #(nb) x, Bit #(nsa) byte_shift_amt);
   return (x >> (byte_shift_amt << 3));
endfunction

// ================================================================
// Check for legal supported widths
// and statically error-out if not ok.

module static_error_check_widths #(Integer wd_addr_I,
				   Integer wd_axi_data_I,
				   Integer wd_ldst_data_I)
   (Empty);

   function Bool fn_wd_addr_I_ok (Integer wd_addr_I);
      return ((wd_addr_I == 32) || (wd_addr_I == 64));
   endfunction

   function Bool fn_wd_data_I_ok (Integer wd_axi_data_I);
      return (   (wd_axi_data_I == 8)
	      || (wd_axi_data_I == 16)
	      || (wd_axi_data_I == 32)
	      || (wd_axi_data_I == 64)
	      || (wd_axi_data_I == 128)
	      || (wd_axi_data_I == 256)
	      || (wd_axi_data_I == 512)
	      || (wd_axi_data_I == 1024));
   endfunction

   if (! fn_wd_addr_I_ok (wd_addr_I))
      errorM ("wd_addr is not 32 or 64");

   if (! fn_wd_data_I_ok (wd_axi_data_I))
      errorM ("wd_axi_data is not power of 2, from 8 to 1024");

   if (! fn_wd_data_I_ok (wd_ldst_data_I))
      errorM ("wd_ldst_data is not power of 2, from 8 to 1024");

   if (wd_ldst_data_I > wd_axi_data_I)
      errorM ("wd_ldst_data is wider than wd_axi_data");
endmodule

// ================================================================

endpackage
