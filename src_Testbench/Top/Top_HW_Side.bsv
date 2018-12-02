// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved.

package Top_HW_Side;

// ================================================================
// mkTop_HW_Side is the top-level system for simulation.
// mkMem_Model is a memory model.

// **** CAVEAT FOR IVERILOG USERS: The 'C_Imports' sections below are
// disabled for IVerilog until we find a clean solution.  They depend
// on imported C which is non-trivial in IVerilog because IVerilog
// still depends on the older Verilog VPI standard instead of the
// newer DPI-C standard.  C-imported functions are used for:
//     UART input polling and character-reading
//     Writing tandem-verfication encoded trace data

// (Note: UART output does not depend on C-imported functions and so
// will work ok even in IVerilog)

// ================================================================
// BSV lib imports

import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls      :: *;
import TV_Info        :: *;
import SoC_Top        :: *;
import Mem_Controller :: *;
import Mem_Model      :: *;

`ifndef IVERILOG
import C_Imports      :: *;
`endif

// ================================================================
// Top-level module.
// Instantiates the SoC.
// Instantiates a memory model.

(* synthesize *)
module mkTop_HW_Side (Empty) ;

   SoC_Top_IFC    soc_top   <- mkSoC_Top;
   Mem_Model_IFC  mem_model <- mkMem_Model;

   // Connect SoC to raw memory
   let memCnx <- mkConnection (soc_top.to_raw_mem, mem_model.mem_server);

   // ----------------------------------------------------------------
   // BEHAVIOR

   Reg #(Bool) rg_banner_printed <- mkReg (False);

   // Display a banner
   rule rl_step0 (! rg_banner_printed);
      $display ("================================================================");
      $display ("Bluespec RISC-V standalone system simulation v1.2");
      $display ("Copyright (c) 2017-2018 Bluespec, Inc. All Rights Reserved.");
      $display ("================================================================");

      rg_banner_printed <= True;

      // Set CPU verbosity and logdelay (simulation only)
      Bool v1 <- $test$plusargs ("v1");
      Bool v2 <- $test$plusargs ("v2");
      Bit #(4)  verbosity = ((v2 ? 2 : (v1 ? 1 : 0)));
      Bit #(64) logdelay  = 0;    // # of instructions after which to set verbosity
      soc_top.set_verbosity  (verbosity, logdelay);

      // Note: see 'CAVEAT FOR IVERILOG USERS' above
`ifndef IVERILOG
      // Load tohost addr from symbol-table file
      Bool watch_tohost <- $test$plusargs ("tohost");
      Bit #(64) tohost_addr = 0;
      tohost_addr  <- c_get_symbol_val ("tohost");
      $display ("INFO: watch_tohost = %0d, tohost_addr = 0x%0h",
		pack (watch_tohost), tohost_addr);
      soc_top.set_watch_tohost (watch_tohost, tohost_addr);
`endif

`ifdef INCLUDE_TANDEM_VERIF

      // Note: see 'CAVEAT FOR IVERILOG USERS' above
`ifndef IVERILOG
      let success <- c_trace_file_open ('h_AA);
      if (success == 0) begin
	 $display ("ERROR: Top_HW_Side.rl_step0: error opening trace file.");
	 $finish (1);
      end
      else
	 $display ("Top_HW_Side.rl_step0: opened trace file.");
`else
      $display ("Warning: tandem verification output logs not available in IVerilog");
`endif

`endif
   endrule

   // ----------------
   // Tandem verifier: drain and output vectors of bytes

`ifdef INCLUDE_TANDEM_VERIF
   rule rl_tv_vb_out;
      let tv_info <- soc_top.tv_verifier_info_get.get;
      let n  = tv_info.num_bytes;
      let vb = tv_info.vec_bytes;

`ifndef IVERILOG
      Bit #(32) success = 1;

      for (Bit #(32) j = 0; j < fromInteger (valueOf (TV_VB_SIZE)); j = j + 8) begin
	 Bit #(64) w64 = { vb [j+7], vb [j+6], vb [j+5], vb [j+4], vb [j+3], vb [j+2], vb [j+1], vb [j] };
	 let success1 <- c_trace_file_load_word64_in_buffer (j, w64);
      end

      if (success == 0)
	 $display ("ERROR: Top_HW_Side.rl_tv_vb_out: error loading %0d bytes into buffer", n);
      else begin
	 // Send the data
	 success <- c_trace_file_write_buffer (n);
	 if (success == 0)
	    $display ("ERROR: Top_HW_Side.rl_tv_vb_out: error writing out bytevec data buffer (%0d bytes)", n);
      end

      if (success == 0) begin
	 $finish (1);
      end
`endif
   endrule
`endif

   // ----------------
   // UART console I/O

   // Relay system console output to terminal

   rule rl_relay_console_out;
      let ch <- soc_top.get_to_console.get;
      $write ("%c", ch);
      $fflush (stdout);
   endrule

   // Poll terminal input and relay any chars into system console input.
   // Note: rg_console_in_poll is used to poll only every N cycles, whenever it wraps around to 0.
   // Note: see 'CAVEAT FOR IVERILOG USERS' above for why this is ifdef'd out for iVerilog users.

`ifndef IVERILOG

   Reg #(Bit #(12)) rg_console_in_poll <- mkReg (0);

   rule rl_relay_console_in;
      if (rg_console_in_poll == 0) begin
	 Bit #(8) ch <- c_trygetchar (?);
	 if (ch != 0)
	    soc_top.put_from_console.put (ch);
      end
      rg_console_in_poll <= rg_console_in_poll + 1;
   endrule

`endif

   // ----------------------------------------------------------------
   // INTERFACE

   //  None (this is top-level)

endmodule

// ================================================================

endpackage: Top_HW_Side
