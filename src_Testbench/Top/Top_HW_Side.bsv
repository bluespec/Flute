// Copyright (c) 2013-2020 Bluespec, Inc. All Rights Reserved.

package Top_HW_Side;

// ================================================================
// mkTop_HW_Side is the top-level system for simulation.
// mkMem_Model is a memory model.

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
import Fabric_Defs    :: *;
import PLIC           :: *;

import C_Imports        :: *;
import External_Control :: *;

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

   // ================================================================
   // BEHAVIOR

   Reg #(Bool) rg_banner_printed <- mkReg (False);

   // Display a banner
   rule rl_step0 (! rg_banner_printed);
      $display ("================================================================");
      $display ("Bluespec RISC-V standalone system simulation v1.2");
      $display ("Copyright (c) 2017-2019 Bluespec, Inc. All Rights Reserved.");
      $display ("================================================================");

      rg_banner_printed <= True;

      // Set CPU verbosity and logdelay (simulation only)
      Bool v1 <- $test$plusargs ("v1");
      Bool v2 <- $test$plusargs ("v2");
      Bit #(4)  verbosity = ((v2 ? 2 : (v1 ? 1 : 0)));
      Bit #(64) logdelay  = 0;    // # of instructions after which to set verbosity
      soc_top.set_verbosity  (verbosity, logdelay);

      // ----------------
      // Load tohost addr from symbol-table file
      Bool watch_tohost <- $test$plusargs ("tohost");
      let tha <- c_get_symbol_val ("tohost");
      Fabric_Addr tohost_addr = truncate (tha);
      $display ("INFO: watch_tohost = %0d, tohost_addr = 0x%0h",
		pack (watch_tohost), tohost_addr);
      soc_top.set_watch_tohost (watch_tohost, tohost_addr);

      // ----------------
      // Start timing the simulation
      Bit #(32) cycle_num <- cur_cycle;
      c_start_timing (zeroExtend (cycle_num));

      // ----------------
      // Open file for Tandem Verification trace output
`ifdef INCLUDE_TANDEM_VERIF
      let success <- c_trace_file_open ('h_AA);
      if (success == 0) begin
	 $display ("ERROR: Top_HW_Side.rl_step0: error opening trace file.");
	 $display ("    Aborting.");
	 $finish (1);
      end
      else
	 $display ("Top_HW_Side.rl_step0: opened trace file.");
`endif

      // ----------------
      // Open connection to remote debug client
`ifdef INCLUDE_GDB_CONTROL
      let dmi_status <- c_debug_client_connect (dmi_default_tcp_port);
      if (dmi_status != dmi_status_ok) begin
	 $display ("ERROR: Top_HW_Side.rl_step0: error opening debug client connection.");
	 $display ("    Aborting.");
	 $finish (1);
      end
`endif

   endrule: rl_step0

   // ================================================================
   // Terminate on any non-zero status

   rule rl_terminate (soc_top.status != 0);
      $display ("%0d: %m:.rl_terminate: soc_top status is 0x%0h (= 0d%0d)",
		cur_cycle, soc_top.status, soc_top.status);

      // End timing the simulation
      Bit #(32) cycle_num <- cur_cycle;
      c_end_timing (zeroExtend (cycle_num));
      $finish (0);
   endrule

   // ================================================================
   // Tandem verifier: drain and output vectors of bytes

`ifdef INCLUDE_TANDEM_VERIF
   rule rl_tv_vb_out;
      let tv_info <- soc_top.tv_verifier_info_get.get;
      let n  = tv_info.num_bytes;
      let vb = tv_info.vec_bytes;

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
   endrule
`endif

   // ================================================================
   // UART console I/O

   // Relay system console output to terminal

   rule rl_relay_console_out;
      let ch <- soc_top.get_to_console.get;
      $write ("%c", ch);
      $fflush (stdout);
   endrule

   // Poll terminal input and relay any chars into system console input.
   // Note: rg_console_in_poll is used to poll only every N cycles, whenever it wraps around to 0.

   Reg #(Bit #(12)) rg_console_in_poll <- mkReg (0);

   rule rl_relay_console_in;
      if (rg_console_in_poll == 0) begin
	 Bit #(8) ch <- c_trygetchar (?);
	 if (ch != 0) begin
	    soc_top.put_from_console.put (ch);
	    /*
	    $write ("%0d: Top_HW_Side.bsv.rl_relay_console: ch = 0x%0h", cur_cycle, ch);
	    if (ch >= 'h20) $write (" ('%c')", ch);
	    $display ("");
	    */
	 end
      end
      rg_console_in_poll <= rg_console_in_poll + 1;
   endrule

   // ================================================================
   // Interaction with remote debug client

`ifdef INCLUDE_GDB_CONTROL
   rule rl_debug_client_request_recv;
      Bit #(64) req <- c_debug_client_request_recv ('hAA);
      Bit #(8)  status = req [63:56];
      Bit #(32) data   = req [55:24];
      Bit #(16) addr   = req [23:8];
      Bit #(8)  op     = req [7:0];
      if (status == dmi_status_err) begin
	 $display ("%0d: Top_HW_Side.rl_debug_client_request_recv: receive error; aborting",
		   cur_cycle);
	 $finish (1);
      end
      else if (status == dmi_status_ok) begin
	 // $write ("%0d: Top_HW_Side.rl_debug_client_request_recv:", cur_cycle);
	 if (op == dmi_op_read) begin
	    // $display (" READ 0x%0h", addr);
	    let control_req = Control_Req {op: external_control_req_op_read_control_fabric,
					   arg1: zeroExtend (addr),
					   arg2: 0};
	    soc_top.server_external_control.request.put (control_req);
	 end
	 else if (op == dmi_op_write) begin
	    // $display (" WRITE 0x%0h 0x%0h", addr, data);
	    let control_req = Control_Req {op: external_control_req_op_write_control_fabric,
					   arg1: zeroExtend (addr),
					   arg2: zeroExtend (data)};
	    soc_top.server_external_control.request.put (control_req);
	 end
	 else if (op == dmi_op_shutdown) begin
	    $display ("Top_HW_Side.rl_debug_client_request_recv: SHUTDOWN");

	    // End timing the simulation and print simulation speed stats
	    Bit #(32) cycle_num <- cur_cycle;
	    c_end_timing (zeroExtend (cycle_num));
	    $finish (0);
	 end
	 else if (op == dmi_op_start_command) begin    // For debugging only
	    // $display (" START COMMAND ================================");
	 end
	 else
	    $display (" Top_HW_Side.rl_debug_client_request_recv: UNRECOGNIZED OP %0d; ignoring", op);
      end
   endrule

   rule rl_debug_client_response_send;
      let control_rsp <- soc_top.server_external_control.response.get;
      // $display ("Top_HW_Side.rl_debug_client_response_send: 0x%0h", control_rsp.result);
      let status <- c_debug_client_response_send (truncate (control_rsp.result));
      if (status == dmi_status_err) begin
	 $display ("%0d: Top_HW_Side.rl_debug_client_response_send: send error; aborting",
		   cur_cycle);
	 $finish (1);
      end
   endrule
`endif

   // ================================================================
   // INTERFACE

   //  None (this is top-level)

endmodule

// ================================================================

endpackage: Top_HW_Side
