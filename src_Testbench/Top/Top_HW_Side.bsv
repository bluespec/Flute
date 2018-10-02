// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved.

package Top_HW_Side;

// ================================================================
// mkTop_HW_Side is the top-level system for simulation.
// mkMem_Model is a memory model.

// **** CAVEAT FOR IVERILOG USERS: The 'ConsoleIO' sections below are
// disabled for IVerilog.  Those sections concern polling for tty
// input for the SoC's UART.  They depend on imported C which is
// non-trivial in IVerilog because IVerilog still depends on the older
// Verilog VPI standard instead of the newer DPI-C standard.  Until we
// find a clean solution, IVerilog sim will not have access to UART
// input (it can still do UART output)

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
import SoC_Top        :: *;
import Mem_Controller :: *;
import Mem_Model      :: *;

`ifndef IVERILOG
import ConsoleIO      :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import Tandem_Verif_Out :: *;
`endif

// ================================================================
// Top-level module.
// Instantiates the SoC.
// Instantiates a memory model.

(* synthesize *)
module mkTop_HW_Side (Empty) ;

   SoC_Top_IFC    soc_top   <- mkSoC_Top;
   Mem_Model_IFC  mem_model <- mkMem_Model;

`ifdef INCLUDE_TANDEM_VERIF
   Tandem_Verif_Out_IFC tv_out <- mkTandem_Verif_Out;
`endif

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

`ifdef INCLUDE_TANDEM_VERIF
      tv_out.reset;
`endif
   endrule

   // ----------------
   // Tandem verifier: drain and output/discard packets

`ifdef INCLUDE_TANDEM_VERIF
   rule rl_drain_tandem;
      let tv_packet <- soc_top.verify_out.get;
      tv_out.tv_out.put (tv_packet);

      // $display ("%0d: Top_HW_Side.rl_drain_tandem: drained a TV packet", cur_cycle, fshow (tv_packet));
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
