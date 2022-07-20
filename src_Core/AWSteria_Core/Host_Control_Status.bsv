// Copyright (c) 2021-2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package Host_Control_Status;

// ================================================================
// This package defines module 'mkHost_Control_Status' and it interface.
// It decodes 32-bit words coming from host-side into individual
//     commands for the core.
// It encodes status information from the core into 32-bit status
//     words to be sent host-side.

// ================================================================
// Lib imports

// from BSV library
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Assert       :: *;

// from should-be-in-BSV-library
import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import ISA_Decls      :: *;    // for XLEN
import DM_CPU_Req_Rsp :: *;

// ================================================================

interface Host_Control_Status_IFC;
   // 32-bit control and status words to/from host-side
   interface Server_Semi_FIFOF #(Bit #(32), Bit #(32)) se_control_status;

   // ----------------
   // Decoded control/status actions

   method Bool mv_assert_core_reset;

   // Watch tohost 'on/off' and tohost addr
   interface FIFOF_O #(Tuple2 #(Bool, Bit #(64))) fo_watch_tohost_control;

   // Verbosity level and log-delay (during simulation)
   interface FIFOF_O #(Tuple2 #(Bit #(4), Bit #(64))) fo_verbosity_control;

   // PC trace on/off, and sampling interval (units: instructions)
   interface FIFOF_O #(Tuple2 #(Bool, Bit #(64))) fo_pc_trace_control;

   // Value written by RISC-V CPU to 'tohost' addr signalling end of test
   method FIFOF_I #(Bit #(64)) fi_tohost_value;
endinterface

// ================================================================
// Word 0 from host contains command and count of additional data words

typedef Bit #(5) Host_Cmd;

Host_Cmd cmd_ping                = 0;    // Supported
Host_Cmd cmd_core_reset          = 1;    // Supported

Host_Cmd cmd_CPU_run_control     = 2;    // Unsupported
Host_Cmd cmd_CPU_fence           = 3;    // Unsupported
Host_Cmd cmd_CSR_access          = 4;    // Unsupported

Host_Cmd cmd_watch_tohost        = 5;    // Supported
Host_Cmd cmd_read_tohost         = 6;    // Supported

Host_Cmd cmd_pc_trace            = 7;    // Supported

Host_Cmd cmd_set_sim_verbosity   = 8;    // Supported

// ================================================================
// Word 0 returned to host has this status in [7:0]

Bit #(32) status_ok          = 0;
Bit #(32) status_err         = 1;
Bit #(32) status_unsupported = 2;

// ================================================================
// Module FSM's state

typedef enum {STATE_REQ0,     // Expecting command word
	      STATE_REQ1,     // Expecting word 1
	      STATE_REQ2,     // Expecting word 2
	      STATE_REQ3,     // Expecting word 3

	      STATE_EXEC      // Execute command and send response word 0
	                      // Future commands: states for more response words
   } FSM_State
deriving (Bits, Eq, FShow);

// ================================================================

(* synthesize *)
module mkHost_Control_Status (Host_Control_Status_IFC);
   Integer verbosity = 0;

   // To/From host-side
   FIFOF #(Bit #(32)) f_host_to_hw <- mkFIFOF;
   FIFOF #(Bit #(32)) f_hw_to_host <- mkFIFOF;

   // Decoded commands/responses   
   Reg #(Bool)  rg_assert_core_reset <- mkReg (False);

   FIFOF #(Tuple2 #(Bool, Bit #(64))) f_watch_tohost <- mkFIFOF;

   Wire #(Bit #(16)) dw_tohost_value      <- mkDWire (0);
   Reg  #(Bit #(16)) rg_prev_tohost_value <- mkReg (0);

   FIFOF #(Tuple2 #(Bit #(4), Bit #(64))) f_verbosity <- mkFIFOF;

   FIFOF #(Tuple2 #(Bool,Bit #(64))) f_pc_trace_control <- mkFIFOF;

   Reg #(FSM_State) rg_state <- mkReg (STATE_REQ0);
   Reg #(Bit #(32)) rg_req0  <- mkRegU;
   Reg #(Bit #(32)) rg_req1  <- mkRegU;
   Reg #(Bit #(32)) rg_req2  <- mkRegU;
   Reg #(Bit #(32)) rg_req3  <- mkRegU;

   // ================================================================
   // FSM to receive a command and move to STATE_EXEC to execute it

   rule rl_req0 (rg_state == STATE_REQ0);
      Bit #(32) req0 <- pop (f_host_to_hw);
      rg_req0 <= req0;
      rg_state <= ((req0 [2:0] == 0) ? STATE_EXEC : STATE_REQ1);
   endrule

   rule rl_req1 (rg_state == STATE_REQ1);
      Bit #(32) req1 <- pop (f_host_to_hw);
      rg_req1 <= req1;
      rg_state <= ((req1 [2:0] == 1) ? STATE_EXEC : STATE_REQ2);
   endrule

   rule rl_req2 (rg_state == STATE_REQ2);
      Bit #(32) req2 <- pop (f_host_to_hw);
      rg_req2 <= req2;
      rg_state <= ((req2 [2:0] == 2) ? STATE_EXEC : STATE_REQ3);
   endrule

   rule rl_req3 (rg_state == STATE_REQ3);
      Bit #(32) req3 <- pop (f_host_to_hw);
      rg_req3 <= req3;
      rg_state <= STATE_EXEC;
   endrule

   // ================================================================
   // Execute command, send rsp0, return to idle state

   rule rl_exec (rg_state == STATE_EXEC);
      Host_Cmd  cmd  = rg_req0 [7:3];
      Bit #(32) rsp0 = status_ok;

      if (cmd == cmd_ping) begin
	 $display ("  mkHost_Control_Status: host_to_hw_req: ping/noop");
      end
      // ----------------
      else if (cmd == cmd_core_reset) begin
	 if (rg_req0 [31:8] != 0) begin
	    rg_assert_core_reset <= True;
	    $display ("  mkHost_Control_Status: host_to_hw_req: Assert Core Reset");
	 end
	 else begin
	    rg_assert_core_reset <= False;
	    $display ("  mkHost_Control_Status: host_to_hw_req: Deassert Core Reset");
	 end
      end
      // ----------------
      else if (cmd == cmd_watch_tohost) begin
	 if (rg_req0 [2:0] == 0) begin
	    // OFF
	    f_watch_tohost.enq (tuple2 (False, ?));
	    $display ("  mkHost_Control_Status: host_to_hw_req: watch_tohost_off");
	 end
	 else begin
	    // ON
	    Bit #(64) tohost_addr = { rg_req2, rg_req1 };
	    f_watch_tohost.enq (tuple2 (True, tohost_addr));
	    $display ("  mkHost_Control_Status: host_to_hw_req: watch_tohost_on, addr %0h",
		      tohost_addr);
	 end
      end
      // ----------------
      else if (cmd == cmd_read_tohost) begin
	 rsp0 = ({ dw_tohost_value, 16'h0 } | status_ok);
	 // Only display if host value changed
	 if (rg_prev_tohost_value != dw_tohost_value) begin
	    $display ("  mkHost_Control_Status: host_to_hw_req: read_tohost => %0h",
		      dw_tohost_value);
	    rg_prev_tohost_value <= dw_tohost_value;
	 end
      end
      // ----------------
      else if (cmd == cmd_pc_trace) begin
	 if (rg_req0 [2:0] == 0) begin
	    // OFF
	    f_pc_trace_control.enq (tuple2 (False, ?));
	    $display ("  mkHost_Control_Status: host_to_hw_req: PC trace off");
	 end
	 else begin
	    // ON
	    f_pc_trace_control.enq (tuple2 (True, zeroExtend (rg_req1)));
	    $display ("  mkHost_Control_Status: host_to_hw_req: PC trace on: interval %0h",
		      rg_req1);
	 end
      end
      // ----------------
      else if (cmd == cmd_set_sim_verbosity) begin
	 Bit #(4)  verbosity = rg_req0 [11:8];
	 Bit #(64) logdelay  = { rg_req2, rg_req1 };
	 f_verbosity.enq (tuple2 (verbosity, logdelay));
	 $display ("  mkHost_Control_Status: host_to_hw_req: set_sim_verbosity %0d logdelay %0h",
		   verbosity, logdelay);
      end
      // ----------------
      else begin
	 rsp0 = status_unsupported;
	 $display ("Host-to-HW control: unsupported command %0h; request word 0 is %0h",
		   cmd, rg_req0);
	 $display ("    %m");
      end

      f_hw_to_host.enq (rsp0);
      rg_state <= STATE_REQ0;
   endrule

   // ================================================================
   // INTERFACE

   interface se_control_status  = fifofs_to_Server_Semi_FIFOF (f_host_to_hw,
							       f_hw_to_host);

   method mv_assert_core_reset = rg_assert_core_reset;

   interface FIFOF_O fo_watch_tohost_control = to_FIFOF_O (f_watch_tohost);

   interface FIFOF_O fo_verbosity_control = to_FIFOF_O (f_verbosity);

   interface FIFOF_O fo_pc_trace_control = to_FIFOF_O (f_pc_trace_control);

   interface FIFOF_I fi_tohost_value;
      method Action enq (Bit #(64) tohost_value);
	 action
	    dw_tohost_value <= truncate (tohost_value);
	 endaction
      endmethod
      method notFull = True;
   endinterface
endmodule

// ================================================================

endpackage
