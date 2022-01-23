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
   // Decoded control/status commands

   interface Client #(Bool, Bool)              cl_cpu_reset;
   interface Client #(Bool, Bool)              cl_run_halt;
   interface Client #(DM_CPU_Req #(12, XLEN),
		      DM_CPU_Rsp #(XLEN))      cl_csr_rw;

   // Watch tohost 'on/off' and tohost addr
   interface Get #(Tuple2 #(Bool,
			    Bit #(64))) g_watch_tohost;

   // Verbosity level and log-delay (during simulation)
   interface Get #(Tuple2 #(Bit #(4),
			    Bit #(64))) g_verbosity;

   // PC trace on/off, and sampling interval (units: instructions)
   method Tuple2 #(Bool, Bit #(64)) mv_pc_trace;

   // Value written by RISC-V CPU to 'tohost' addr signalling end of test
   method Action ma_tohost_value (Bit #(64) tohost_value);
endinterface

// ================================================================
// 1st word from host has this command encoded in [7:0]

typedef Bit #(8) Host_Cmd;

Host_Cmd cmd_noop             = 0;

Host_Cmd cmd_CPU_stop         = 1;
Host_Cmd cmd_CPU_start        = 2;
Host_Cmd cmd_CPU_reset        = 3;
Host_Cmd cmd_CPU_fence        = 4;
Host_Cmd cmd_CSR_write        = 5;
Host_Cmd cmd_CSR_read         = 6;

Host_Cmd cmd_watch_tohost_off = 20;
Host_Cmd cmd_watch_tohost_on  = 21;
Host_Cmd cmd_read_tohost      = 22;

Host_Cmd cmd_pc_trace_off     = 25;
Host_Cmd cmd_pc_trace_on      = 26;

Host_Cmd cmd_set_verbosity    = 27;

// ================================================================
// 1st word returned to host has this status in LSBs

Bit #(32) status_ok           = 0;
Bit #(32) status_unrecognized = 1;
Bit #(32) status_err          = 2;

// ================================================================
// Module FSM's state

typedef enum {STATE_START,
	      STATE_WORD1,
	      STATE_EXEC,
	      STATE_CSR_WRITE_2,
	      STATE_CSR_READ_2, STATE_CSR_READ_3, STATE_CSR_READ_4
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
   FIFOF #(Bool)                   f_run_halt_reqs <- mkFIFOF;
   FIFOF #(Bool)                   f_run_halt_rsps <- mkFIFOF;

   FIFOF #(Bool)                   f_cpu_reset_reqs <- mkFIFOF;
   FIFOF #(Bool)                   f_cpu_reset_rsps <- mkFIFOF;

   FIFOF #(DM_CPU_Req #(12, XLEN)) f_csr_mem_reqs <- mkFIFOF;
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_csr_mem_rsps <- mkFIFOF;

   FIFOF #(Tuple2 #(Bool, Bit #(64))) f_watch_tohost <- mkFIFOF;

   Reg #(Bit #(16)) rg_prev_tohost_value <- mkReg (0);
   Wire #(Bit #(64)) dw_tohost_value <- mkDWire (0);

   FIFOF #(Tuple2 #(Bit #(4), Bit #(64))) f_verbosity <- mkFIFOF;

   Reg #(Bool)      rg_pc_trace_on       <- mkReg (False);
   Reg #(Bit #(64)) rg_pc_trace_interval <- mkRegU;

   Reg #(FSM_State) rg_state <- mkReg (STATE_START);

   Reg #(Bit #(32)) rg_word0 <- mkRegU;
   Reg #(Bit #(32)) rg_word1 <- mkRegU;

   let word0_cmd = truncate (rg_word0);

   Reg #(Bit #(64)) rg_rsp_data <- mkRegU;

   // ================================================================
   // FSM to receive commands and to respond

   rule rl_fsm_start (rg_state == STATE_START);
      Bit #(32) word0 <- pop (f_host_to_hw);
      rg_word0 <= word0;
      Host_Cmd cmd = truncate (word0);

      FSM_State next_state = STATE_START;

      if (cmd == cmd_noop) begin
	 f_hw_to_host.enq (status_ok);
	 $display ("  host_to_hw_req: noop");
      end
      else if (cmd == cmd_CPU_stop) begin
	 f_run_halt_reqs.enq (False);
	 next_state = STATE_EXEC;
	 $display ("  host_to_hw_req: CPU_stop");
      end
      else if (cmd == cmd_CPU_start) begin
	 f_run_halt_reqs.enq (True);
	 next_state = STATE_EXEC;
	 $display ("  host_to_hw_req: CPU_start");
      end
      else if (cmd == cmd_CPU_reset) begin
	 f_cpu_reset_reqs.enq (True);
	 next_state = STATE_EXEC;
	 $display ("  host_to_hw_req: CPU_reset");
      end
      else if (cmd == cmd_CPU_fence) begin
	 f_hw_to_host.enq (status_ok);
	 $display ("  host_to_hw_req: CPU_fence: no-op on this system (coherent access)");
      end
      else if (cmd == cmd_CSR_write) begin
	 next_state = STATE_WORD1;
	 $display ("  host_to_hw_req: CSR_write");
      end
      else if (cmd == cmd_CSR_read) begin
	 next_state = STATE_EXEC;
	 $display ("  host_to_hw_req: CSR_read");
      end
      else if (cmd == cmd_watch_tohost_off) begin
	 f_watch_tohost.enq (tuple2 (False, ?));
	 f_hw_to_host.enq (status_ok);
	 $display ("  host_to_hw_req: watch_tohost_off");
      end
      else if (cmd == cmd_watch_tohost_on) begin
	 next_state = STATE_WORD1;
	 $display ("  host_to_hw_req: watch_tohost_on");
      end
      else if (cmd == cmd_read_tohost) begin
	 let tohost_value = dw_tohost_value [15:0];
	 let status = ({ tohost_value, 16'h0 } | status_ok);
	 f_hw_to_host.enq (status);
	 rg_prev_tohost_value <= tohost_value;

	 // Only report changes to non-zero
	 if ((tohost_value != 0) && (tohost_value != rg_prev_tohost_value)) begin
	    $display ("  host_to_hw_req: read_tohost = %0h", tohost_value);
	    let test_num = (tohost_value >> 1);
	    if (test_num == 0) $display ("  = PASS");
	    else               $display ("  = FAIL on test %0d", test_num);
	 end
      end
      else if (cmd == cmd_pc_trace_off) begin
	 rg_pc_trace_on <= True;
	 f_hw_to_host.enq (status_ok);
	 $display ("  host_to_hw_req: PC trace off");
      end
      else if (cmd == cmd_pc_trace_on) begin
	 next_state = STATE_EXEC;
	 $display ("  host_to_hw_req: PC trace on");
      end
      else if (cmd == cmd_set_verbosity) begin
	 next_state = STATE_WORD1;
	 $display ("  host_to_hw_req: verbosity");
      end
      else begin
	 f_hw_to_host.enq (status_unrecognized);
	 $display ("  host_to_hw_req: ERROR: unrecognized command %0h", word0);
      end
      rg_state <= next_state;
   endrule

   // ----------------
   // Common rule for 3-word commands: collect word1, then go to state_exec

   rule rl_fsm_word1 (rg_state == STATE_WORD1);
      Bit #(32) word1 <- pop (f_host_to_hw);
      rg_word1 <= word1;
      rg_state <= STATE_EXEC;
   endrule

   // ----------------

   rule rl_reset ((rg_state == STATE_EXEC) && (word0_cmd == cmd_CPU_reset));
      Bool running <- pop (f_cpu_reset_rsps);
      dynamicAssert (running == True, "Reset: CPU came up stopped");
      f_hw_to_host.enq (status_ok);
      rg_state <= STATE_START;

      $display ("  host_to_hw_req: CPU_reset ... done. running = ", fshow (running));
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_reset");
   endrule

   // ----------------

   rule rl_stop ((rg_state == STATE_EXEC) && (word0_cmd == cmd_CPU_stop));
      Bool running <- pop (f_run_halt_rsps);
      dynamicAssert ((running == False), "Stop cmd but CPU still running");
      f_hw_to_host.enq ((running == False) ? status_ok : status_err);
      rg_state <= STATE_START;

      $display ("  host_to_hw_req: CPU_stop ... running = ", fshow (running));
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_stop");
   endrule

   // ----------------

   rule rl_start ((rg_state == STATE_EXEC) && (word0_cmd == cmd_CPU_start));
      Bool running <- pop (f_run_halt_rsps);
      dynamicAssert ((running == True), "Start cmd but CPU still stopped");
      f_hw_to_host.enq ((running == True) ? status_ok : status_err);
      rg_state <= STATE_START;

      $display ("  host_to_hw_req: CPU_start ... running = ", fshow (running));
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_start");
   endrule

   // ----------------

   rule rl_CSR_write ((rg_state == STATE_EXEC) && (word0_cmd == cmd_CSR_write));
      Bit #(32) word2 <- pop (f_host_to_hw);
      Bit #(12) csr_addr = rg_word0 [31:20];
      WordXL    csr_data = truncate ({ word2, rg_word1 });
      let req = DM_CPU_Req {write:   True,
			    address: csr_addr,
			    data:    csr_data};
      f_csr_mem_reqs.enq (req);
      rg_state <= STATE_CSR_WRITE_2;

      $display ("  host_to_hw_req: CSR_write: addr %0h data %0h", csr_addr, csr_data);
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_CSR_write");
   endrule

   rule rl_CSR_write_2 (rg_state == STATE_CSR_WRITE_2);
      let dm_cpu_rsp <- pop (f_csr_mem_rsps);
      f_hw_to_host.enq (dm_cpu_rsp.ok ? status_ok : status_err);
      rg_state <= STATE_START;

      $display ("  host_to_hw_req: CSR_write: ok = ", fshow (dm_cpu_rsp.ok ));
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_CSR_write_2");
   endrule

   // ----------------

   rule rl_CSR_read ((rg_state == STATE_EXEC) && (word0_cmd == cmd_CSR_read));
      Bit #(12) csr_addr = rg_word0 [31:20];
      let req = DM_CPU_Req {write:   False,
			    address: csr_addr,
			    data:    ?};
      f_csr_mem_reqs.enq (req);
      rg_state <= STATE_CSR_READ_2;

      $display ("  host_to_hw_req: CSR_read: addr %0h", csr_addr);
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_CSR_read");
   endrule

   rule rl_CSR_read_2 (rg_state == STATE_CSR_READ_2);
      let dm_cpu_rsp <- pop (f_csr_mem_rsps);
      rg_rsp_data <= zeroExtend (dm_cpu_rsp.data);
      f_hw_to_host.enq (dm_cpu_rsp.ok ? status_ok : status_err);
      rg_state <= (dm_cpu_rsp.ok ? STATE_CSR_READ_3 : STATE_START);

      $write ("  host_to_hw_req: CSR_read: ok = ", fshow (dm_cpu_rsp.ok ));
      if (dm_cpu_rsp.ok) $write (" data %0h", dm_cpu_rsp.data);
      $display ("");
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_CSR_read_2");
   endrule

   rule rl_CSR_read_3 (rg_state == STATE_CSR_READ_3);
      f_hw_to_host.enq (rg_rsp_data [31:0]);
      rg_state <= STATE_CSR_READ_4;
   endrule

   rule rl_CSR_read_4 (   (rg_state == STATE_CSR_READ_4)
		       && (word0_cmd == cmd_CSR_read));
      f_hw_to_host.enq (rg_rsp_data [63:32]);
      rg_state <= STATE_START;
   endrule

   // ----------------

   rule rl_watch_tohost_on (   (rg_state == STATE_EXEC)
			    && (word0_cmd == cmd_watch_tohost_on));
      Bit #(32) word2 <- pop (f_host_to_hw);
      Bit #(64) tohost_addr = { word2, rg_word1 };
      f_watch_tohost.enq (tuple2 (True, tohost_addr));
      f_hw_to_host.enq (status_ok);
      rg_state <= STATE_START;

      $display ("  host_to_hw_req: watch_tohost_on: tohost_addr = %0h", tohost_addr);
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_watch_tohost_on");
   endrule

   // ----------------

   rule rl_pc_trace_on (   (rg_state == STATE_EXEC)
			&& (word0_cmd == cmd_pc_trace_on));
      Bit #(32) word1 <- pop (f_host_to_hw);
      rg_pc_trace_on       <= True;
      rg_pc_trace_interval <= zeroExtend (word1);
      f_hw_to_host.enq (status_ok);
      rg_state <= STATE_START;

      $display ("  host_to_hw_req: PC trace on, interval = %0h", word1);
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_pc_trace_on");
   endrule

   // ----------------

   rule rl_verbosity (   (rg_state == STATE_EXEC)
		      && (word0_cmd == cmd_set_verbosity));
      Bit #(32) word2 <- pop (f_host_to_hw);
      Bit #(4)  verbosity = truncate (rg_word0 [31:8]);
      Bit #(64) logdelay  = { word2, rg_word1 };
      f_verbosity.enq (tuple2 (verbosity, logdelay));
      f_hw_to_host.enq (status_ok);
      rg_state <= STATE_START;

      $display ("  host_to_hw_req: set_verbosity %0d, delay %0h", verbosity, logdelay);
      $display ("    %0d: %m", cur_cycle);
      $display ("    Rule rl_verbosity");
   endrule

   // ================================================================
   // INTERFACE

   interface se_control_status  = fifofs_to_Server_Semi_FIFOF (f_host_to_hw,
							       f_hw_to_host);

   interface cl_cpu_reset       = toGPClient (f_cpu_reset_reqs,
					      f_cpu_reset_rsps);

   interface cl_run_halt        = toGPClient (f_run_halt_reqs,
					      f_run_halt_rsps);

   interface cl_csr_rw          = toGPClient (f_csr_mem_reqs,
					      f_csr_mem_rsps);

   interface Get g_watch_tohost = toGet (f_watch_tohost);

   interface Get g_verbosity    = toGet (f_verbosity);

   method mv_pc_trace = tuple2 (rg_pc_trace_on, rg_pc_trace_interval);

   method Action ma_tohost_value (Bit #(64) tohost_value);
      dw_tohost_value <= tohost_value;
   endmethod
endmodule

// ================================================================

endpackage
