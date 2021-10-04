// Copyright (c) 2016-2021 Bluespec, Inc. All Rights Reserved

package CPU;

// ================================================================
// This is the "Flute_V3" CPU, implementing the RISC-V ISA.
// - RV32/64, ACDFIMSU, 5-stage in order pipeline with branch prediction.
// - Optional Debug Module connection
// - Optional Tandem Verification connection.

`ifdef EXTERNAL_DEBUG_MODULE
`undef INCLUDE_GDB_CONTROL
`endif

// ================================================================
// Exports

export mkCPU;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import ConfigReg    :: *;
import Clocks       :: *;
// ----------------
// BSV additional libs

import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import AXI4_Types :: *;

`ifdef FABRIC_AHBL
import AHBL_Types  :: *;
import AHBL_Defs   :: *;
`endif

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

import ISA_Decls :: *;

import PC_Trace  :: *;
import TV_Info   :: *;

import GPR_RegFile :: *;
`ifdef ISA_F
import FPR_RegFile :: *;
`endif
import CSR_RegFile :: *;
import CPU_Globals :: *;
import CPU_IFC     :: *;

`ifdef ISA_C
// 'C' extension (16b compressed instructions)
import CPU_Fetch_C  :: *;
`endif

import CPU_StageF :: *;    // Fetch
import CPU_StageD :: *;    // Decode
import CPU_Stage1 :: *;    // Execute
import CPU_Stage2 :: *;    // Memory and long-latency ops
import CPU_Stage3 :: *;    // Writeback

import Near_Mem_IFC :: *;  // Caches or TCM

`ifdef Near_Mem_Caches
import Near_Mem_Caches :: *;
`endif

`ifdef Near_Mem_TCM
import Near_Mem_TCM :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
//import Debug_Module     :: *;
import DM_Common        :: *;
import Debug_Interfaces :: *;
import DM_CPU_Req_Rsp   :: *;
`endif

// System address map and pc_reset value
import SoC_Map :: *;

// ================================================================
// Major States of CPU

typedef enum {CPU_RESET1,
	      CPU_RESET2,

`ifdef INCLUDE_GDB_CONTROL
	      CPU_GDB_PAUSING,      // On GDB breakpoint, while waiting for fence completion
`endif
	      CPU_DEBUG_MODE,       // Stopped (normally for debugger)
	      CPU_RUNNING,          // Normal operation
	      CPU_TRAP,
	      CPU_START_TRAP_HANDLER,
	      CPU_CSRRW_2,
	      CPU_CSRR_S_or_C_2,
	      CPU_CSRRX_RESTART,    // Restart pipe after a CSRRX instruction
	      CPU_FENCE_I,          // While waiting for FENCE.I to complete in Near_Mem
	      CPU_FENCE,            // While waiting for FENCE to complete in Near_Mem
	      CPU_SFENCE_VMA,       // While waiting for FENCE.VMA to complete in Near_Mem

	      CPU_WFI_PAUSED        // On WFI pause
   } CPU_State
deriving (Eq, Bits, FShow);

function Bool fn_is_running (CPU_State  cpu_state);
   return (   (cpu_state != CPU_RESET1)
	   && (cpu_state != CPU_RESET2)
`ifdef INCLUDE_GDB_CONTROL
	   && (cpu_state != CPU_GDB_PAUSING)
	   && (cpu_state != CPU_DEBUG_MODE)
`endif
	   );
endfunction

// ================================================================

(* synthesize *)
module mkCPU (CPU_IFC);

   // ----------------
   // System address map and pc reset value
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // ----------------
   // General purpose registers and CSRs
   GPR_RegFile_IFC  gpr_regfile  <- mkGPR_RegFile;
`ifdef ISA_F
   FPR_RegFile_IFC  fpr_regfile  <- mkFPR_RegFile;
`endif

   CSR_RegFile_IFC  csr_regfile  <- mkCSR_RegFile;

   // ----------------
   // Some commonly used CSR values
   let mcycle   = csr_regfile.read_csr_mcycle;
   let mstatus  = csr_regfile.read_mstatus;
   let misa     = csr_regfile.read_misa;
   let minstret = csr_regfile.read_csr_minstret;

   // MSTATUS.MXR and SSTATUS.SUM for Virtual Memory access control
   Bit #(1) mstatus_MXR = mstatus [19];
`ifdef ISA_PRIV_S
   Bit #(1) sstatus_SUM = (csr_regfile.read_sstatus) [18];
`else
   Bit #(1) sstatus_SUM = 0;
`endif

   // ----------------
   // Near mem (caches or TCM, for example)
   Near_Mem_IFC  near_mem <- mkNear_Mem;

   // Take imem as is from near_mem, or use wrapper for 'C' extension
`ifdef ISA_C
   IMem_IFC imem <- mkCPU_Fetch_C (near_mem.imem);
`else
   IMem_IFC imem = near_mem.imem;
`endif

   // ----------------
   // For debugging

   // Verbosity: 0=quiet; 1=instruction trace; 2=more detail
   Reg #(Bit #(4))  cfg_verbosity <- mkConfigReg (1);

   // Verbosity is 0 as long as # of instrs retired is <= cfg_logdelay
   Reg #(Bit #(64))  cfg_logdelay <- mkConfigReg (0);

   // Current verbosity, taking into account log delay
   Bit #(4)  cur_verbosity = ((minstret < cfg_logdelay) ? 0 : cfg_verbosity);

   // ----------------
   // Major CPU states
   Reg #(CPU_State)  rg_state    <- mkReg (CPU_RESET1);
   Reg #(Priv_Mode)  rg_cur_priv <- mkReg (m_Priv_Mode);
   Reg #(Epoch)      rg_epoch    <- mkRegU;

   // These regs save info on a trap in Stage1 or Stage2
   Reg #(Trap_Info)  rg_trap_info       <- mkRegU;
   Reg #(Bool)       rg_trap_interrupt  <- mkRegU;
   Reg #(Instr)      rg_trap_instr      <- mkRegU;
`ifdef SHOW_COMPRESSED
   Reg #(Instr)      rg_trap_traced_instr <- mkRegU;
`endif
`ifdef INCLUDE_TANDEM_VERIF
   Reg #(Trace_Data) rg_trap_trace_data <- mkRegU;
`endif

   // rg_next_pc is used for redirections (branches, non-pipe
   // instructions, traps, interrupts)
   Reg #(WordXL) rg_next_pc <- mkRegU;

   // Save CSR info in CSRRx istrs to handle in separate rules
   Reg #(WordXL) rg_csr_pc   <- mkRegU;
   Reg #(WordXL) rg_csr_val1 <- mkRegU;

   // Save sstatus_SUM and mstatus_MXR to initiate fetches on an external
   // interrupt
   Reg #(Bit #(1)) rg_sstatus_SUM <- mkRegU;
   Reg #(Bit #(1)) rg_mstatus_MXR <- mkRegU;

   // ----------------
   // Pipeline stages

   CPU_Stage3_IFC stage3 <- mkCPU_Stage3 (cur_verbosity,
					  gpr_regfile,
`ifdef ISA_F
					  fpr_regfile,
`endif
					  csr_regfile);

   CPU_Stage2_IFC stage2 <- mkCPU_Stage2 (cur_verbosity, csr_regfile, near_mem.dmem);

   CPU_Stage1_IFC  stage1 <- mkCPU_Stage1 (cur_verbosity,
					   gpr_regfile,
					   stage2.out.bypass,
					   stage3.out.bypass,
`ifdef ISA_F
					   fpr_regfile,
					   stage2.out.fbypass,
					   stage3.out.fbypass,
`endif
					   csr_regfile,
					   rg_epoch,
					   rg_cur_priv);

   CPU_StageD_IFC  stageD <- mkCPU_StageD (cur_verbosity, misa);

   CPU_StageF_IFC  stageF <- mkCPU_StageF (cur_verbosity, imem);

   // ----------------
   // Interrupt pending based on current priv, mstatus.ie, mie and mip registers

   Bool interrupt_pending = (   isValid (csr_regfile.interrupt_pending (rg_cur_priv))
			     || csr_regfile.nmi_pending);

   // ----------------
   // Reset requests and responses

   FIFOF #(Bool)  f_reset_reqs <- mkFIFOF;
   FIFOF #(Bool)  f_reset_rsps <- mkFIFOF;

   // ----------------
   // Communication to/from External debug module

`ifdef INCLUDE_GDB_CONTROL

   // Debugger run-control
   FIFOF #(Bool)  f_run_halt_reqs <- mkFIFOF;
   FIFOF #(Bool)  f_run_halt_rsps <- mkFIFOF;

   // Stop-request from debugger (e.g., GDB ^C or Dsharp 'stop')
   Reg #(Bool) rg_stop_req <- mkReg (False);

   // Count instrs after step-request from debugger (via dcsr.step)
   Reg #(Bit #(1))  rg_step_count <- mkReg (0);

   // Debugger GPR read/write request/response
   FIFOF #(DM_CPU_Req #(5,  XLEN)) f_gpr_reqs <- mkFIFOF;
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_gpr_rsps <- mkFIFOF;

`ifdef ISA_F
   // Debugger FPR read/write request/response
   FIFOF #(DM_CPU_Req #(5,  FLEN)) f_fpr_reqs <- mkFIFOF;
   FIFOF #(DM_CPU_Rsp #(FLEN))     f_fpr_rsps <- mkFIFOF;
`endif

   // Debugger CSR read/write request/response
   FIFOF #(DM_CPU_Req #(12, XLEN)) f_csr_reqs <- mkFIFOF;
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_csr_rsps <- mkFIFOF;

`endif

   // ----------------
   // PC trace output

`ifdef INCLUDE_PC_TRACE
   FIFOF #(PC_Trace) f_pc_trace  <- mkFIFOF;
`endif

   // ----------------
   // Tandem Verification

`ifdef INCLUDE_TANDEM_VERIF
   FIFOF #(Trace_Data) f_trace_data  <- mkFIFOF;

   // State for deciding if a MIP update needs to be sent into the trace file
   Reg #(WordXL) rg_prev_mip <- mkReg (0);
`endif

   function Bool mip_cmd_needed ();
`ifdef INCLUDE_TANDEM_VERIF
      // If the MTIP, MSIP, or xEIP bits of MIP have changed, then send a MIP update
      WordXL new_mip = csr_regfile.csr_mip_read;
      Bool mip_has_changed = (new_mip != rg_prev_mip);
      return mip_has_changed;
`else
      return False;
`endif
   endfunction: mip_cmd_needed

   // ================================================================
   // Debugging: print instruction trace info

   function Action fa_emit_instr_trace (Bit #(64) instret, WordXL pc, Instr instr, Priv_Mode priv);
      action
	 if ((cur_verbosity >= 1) || ((instret & 'h_F_FFFF) == 0))
	    $display ("%8h (0x%8h)", pc, instr);

`ifdef INCLUDE_PC_TRACE
	 let pc_trace = PC_Trace {cycle: mcycle,  instret: instret, pc: pc};
	 f_pc_trace.enq (pc_trace);
`endif
      endaction
   endfunction

   // ================================================================
   // CPI measurement in each 'run' (from Debug Mode pause to Debug Mode pause)

   Reg #(Bit #(64))  rg_start_CPI_cycles <- mkRegU;
   Reg #(Bit #(64))  rg_start_CPI_instrs <- mkRegU;

   function Action fa_report_CPI;
      action
	 Bit #(64) delta_CPI_cycles = mcycle - rg_start_CPI_cycles;
	 Bit #(64) delta_CPI_instrs = minstret - rg_start_CPI_instrs;

	 // Make delta_CPI_instrs at least 1, to avoid divide-by-zero
	 if (delta_CPI_instrs == 0)
	    delta_CPI_instrs = delta_CPI_instrs + 1;

	 // Report CPI to 1 decimal place.
	 let x = (delta_CPI_cycles * 10) / delta_CPI_instrs;
	 let cpi     = x / 10;
	 let cpifrac = x % 10;
	 $display ("CPI: %0d.%0d = (%0d/%0d) since last 'continue'",
		   cpi, cpifrac, delta_CPI_cycles, delta_CPI_instrs);
      endaction
   endfunction

   // ================================================================
   // Actions to restart from Debug Mode (e.g., GDB 'continue' after a breakpoint)
   // We re-initialize CPI_instrs and CPI_cycles.

   function Action fa_stageF_redirect (Addr new_pc);
      action
	 // Update epoch
	 let new_epoch = rg_epoch + 1;
	 rg_epoch     <= new_epoch;

	 stageF.enq (new_epoch,
		     new_pc,
		     rg_cur_priv,
		     sstatus_SUM,
		     mstatus_MXR,
		     csr_regfile.read_satp);
	 stageF.set_full (True);
	 rg_state <= CPU_RUNNING;

	 if (cur_verbosity > 1)
	    $display ("    fa_stageF_redirect: minstret:%0d  new_pc:%0x  cur_priv:%0d, epoch %0d->%0d",
		      minstret, new_pc, rg_cur_priv, rg_epoch, new_epoch);
      endaction
   endfunction

   function Action fa_restart_from_halt (Addr resume_pc);
      action
	 stage3.set_full (False);
	 stage2.set_full (False);
	 stage1.set_full (False);
	 stageD.set_full (False);

	 fa_stageF_redirect (resume_pc);

	 rg_start_CPI_cycles <= mcycle;
	 rg_start_CPI_instrs <= minstret;
      endaction
   endfunction

   // ================================================================
   // Debug tracing: show pipe state

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_show_pipe (   (cur_verbosity > 1)
		      && fn_is_running (rg_state)
		      && (rg_state != CPU_WFI_PAUSED));
      $display ("================================================================");
      $display ("%0d: Pipeline State:  minstret:%0d  cur_priv:%0d  mstatus:%0x  epoch:%0d",
		mcycle, minstret, rg_cur_priv, mstatus, rg_epoch);
      $display ("    ", fshow_mstatus (misa, mstatus));

      $display ("    Stage3: ", fshow (stage3.out));
      $display ("        Bypass  to Stage1: ", fshow (stage3.out.bypass));
`ifdef ISA_F
      $display ("        FBypass to Stage1: ", fshow (stage3.out.fbypass));
`endif
      $display ("    Stage2: pc 0x%08h instr 0x%08h priv %0d",
		stage2.out.data_to_stage3.pc,
		stage2.out.data_to_stage3.instr,
		stage2.out.data_to_stage3.priv);
      $display ("        ", fshow (stage2.out));
      $display ("        Bypass  to Stage1: ", fshow (stage2.out.bypass));
`ifdef ISA_F
      $display ("        FBypass to Stage1: ", fshow (stage2.out.fbypass));
`endif

      $display ("    Stage1: pc 0x%08h instr 0x%08h priv %0d",
		stage1.out.data_to_stage2.pc,
		stage1.out.data_to_stage2.instr,
		stage1.out.data_to_stage2.priv);
      $display ("        ", fshow (stage1.out));

      $display ("    StageD: pc 0x%08h instr 0x%08h priv %0d epoch %0d",
		stageD.out.data_to_stage1.pc,
		stageD.out.data_to_stage1.instr,
		stageD.out.data_to_stage1.priv,
		stageD.out.data_to_stage1.epoch);
      $display ("        ", fshow (stageD.out));

      $display ("    StageF: pc 0x%08h instr 0x%08h priv %0d epoch %0d",
		stageF.out.data_to_stageD.pc,
		stageF.out.data_to_stageD.instr,
		stageF.out.data_to_stageD.priv,
		stageF.out.data_to_stageD.epoch);
      $display ("        ", fshow (stageF.out));
      $display ("----------------");
   endrule

   // ================================================================
   // Reset

   Reg #(Bool) rg_run_on_reset <- mkReg (False);

   rule rl_reset_start (rg_state == CPU_RESET1);
      let run_on_reset <- pop (f_reset_reqs);
      rg_run_on_reset <= run_on_reset;

      $display ("================================================================");
      $write   ("CPU: Bluespec  RISC-V  Flute  v3.0");
      if (rv_version == RV32)
	 $display (" (RV32)");
      else
	 $display (" (RV64)");
      $display ("Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.");
      $display ("================================================================");

      gpr_regfile.server_reset.request.put (?);
`ifdef ISA_F
      fpr_regfile.server_reset.request.put (?);
`endif
      csr_regfile.server_reset.request.put (?);
      near_mem.server_reset.request.put (?);

      stageF.server_reset.request.put (?);
      stageD.server_reset.request.put (?);
      stage1.server_reset.request.put (?);
      stage2.server_reset.request.put (?);
      stage3.server_reset.request.put (?);

      rg_cur_priv <= m_Priv_Mode;
      rg_state    <= CPU_RESET2;
      rg_epoch    <= 0;

`ifdef INCLUDE_GDB_CONTROL
      rg_stop_req   <= False;
      rg_step_count <= 0;
`endif

`ifdef INCLUDE_TANDEM_VERIF
      let trace_data = mkTrace_RESET;
      f_trace_data.enq (trace_data);

      rg_prev_mip <= 0;
`endif
   endrule: rl_reset_start

   // ----------------

`ifdef ISA_C
   // TODO: analyze this carefully; added to resolve a blockage.
   // imem_rl_fetch_next_32b is in CPU_Fetch_C.bsv, and calls imem32.req (near_mem.imem_req).
   // fa_stageF_redirect calls stageF.enq which also calls imem.req which calls imem32.req.
   // But cond_i32_odd_fetch_next should make these rules mutually exclusive; why doesn't bsc realize this?
   (* descending_urgency = "imem_rl_fetch_next_32b, rl_reset_complete" *)
`endif

   rule rl_reset_complete (rg_state == CPU_RESET2);
      let ack_gpr <- gpr_regfile.server_reset.response.get;
`ifdef ISA_F
      let ack_fpr <- fpr_regfile.server_reset.response.get;
`endif
      let ack_csr <- csr_regfile.server_reset.response.get;
      let ack_nm  <- near_mem.server_reset.response.get;

      let ackF <- stageF.server_reset.response.get;
      let ackD <- stageD.server_reset.response.get;
      let ack1 <- stage1.server_reset.response.get;
      let ack2 <- stage2.server_reset.response.get;
      let ack3 <- stage3.server_reset.response.get;

      WordXL dpc = truncate (soc_map.m_pc_reset_value);

      f_reset_rsps.enq (rg_run_on_reset);

      if (rg_run_on_reset) begin
	 $display ("%0d: %m.rl_reset_complete: restart at PC = 0x%0h", mcycle, dpc);
	 fa_restart_from_halt (dpc);
      end
      else begin
	 rg_state <= CPU_DEBUG_MODE;
`ifdef INCLUDE_GDB_CONTROL
	 csr_regfile.write_dcsr_cause_priv (DCSR_CAUSE_HALTREQ, m_Priv_Mode);
	 csr_regfile.write_dpc (dpc);
`endif
	 $display ("%0d: %m.rl_reset_complete: entering DEBUG_MODE", mcycle);
      end
   endrule: rl_reset_complete

   // ================================================================
   // Various conditions on the pipe

   Bool pipe_is_empty = (   (stage3.out.ostatus == OSTATUS_EMPTY)
			 && (stage2.out.ostatus == OSTATUS_EMPTY)
			 && (stage1.out.ostatus == OSTATUS_EMPTY)
			 && (stageD.out.ostatus == OSTATUS_EMPTY)
			 && (stageF.out.ostatus == OSTATUS_EMPTY));

   // The pipe is ready to execute a non-pipe if any stage has NONPIPE
   // and all stages downstream of that stage are EMPTY
   Bool pipe_has_nonpipe = (   (stage3.out.ostatus == OSTATUS_NONPIPE)
			    || (   (stage3.out.ostatus == OSTATUS_EMPTY)
				&& (stage2.out.ostatus == OSTATUS_NONPIPE))
			    || (   (stage3.out.ostatus == OSTATUS_EMPTY)
				&& (stage2.out.ostatus == OSTATUS_EMPTY)
				&& (stage1.out.ostatus == OSTATUS_NONPIPE)));

   // Stage 1 contains an architectural (not mis-predicted) instruction
   Bool stage1_has_arch_instr = (   (   (stage1.out.ostatus == OSTATUS_PIPE)
				     && (stage1.out.control != CONTROL_DISCARD))
				 || (stage1.out.ostatus == OSTATUS_NONPIPE));

`ifdef INCLUDE_GDB_CONTROL
   Bool stop_step_req = (   rg_stop_req
			 || rg_step_count == 1);
`else
   Bool stop_step_req = False;
`endif

   // Debugger stop and step should only happen on architectural instructions
   Bool stop_step_halt = stage1_has_arch_instr && stop_step_req;

   // Halting conditions
   Bool halting = (stop_step_halt || mip_cmd_needed || (interrupt_pending && stage1_has_arch_instr));
   // Stage1 can halt only when actually contains an instruction, downstream is
   // empty and, if a branch misprediction, StageF is able to be redirected.
   Bool stage1_halted = (   halting
			 && (   (stage1.out.ostatus == OSTATUS_PIPE)
			     || (stage1.out.ostatus == OSTATUS_NONPIPE))
			 && (   (! stage1.out.redirect)
			     || (stageF.out.ostatus != OSTATUS_BUSY))
			 && (stage2.out.ostatus == OSTATUS_EMPTY)
			 && (stage3.out.ostatus == OSTATUS_EMPTY));

   // Stage1 halt reasons, in decreasing priority order
   Bool stage1_send_mip_cmd   = stage1_halted && mip_cmd_needed;
   Bool stage1_take_interrupt = stage1_halted && (! mip_cmd_needed) && interrupt_pending && stage1_has_arch_instr;
   Bool stage1_stop           = stage1_halted && (! mip_cmd_needed) && (! (interrupt_pending && stage1_has_arch_instr));

   // ================================================================
   // Every time an instruction finishes stage 1
   //    (i.e., stage1.set_full () is invoked, Stage 1 has an architectural
   //    instruction and, if a branch misprediction, Stage F is able to be
   //    redirected)
   // this function checks if this is a 'stepped' instruction
   //    (i.e., dcsr.step is set and rg_step_count == 0)
   // If so, set rg_step_count <= 1 so the stage will halt on the next
   // architectural instruction.

   function Action fa_step_check;
      action
`ifdef INCLUDE_GDB_CONTROL
	 if (   stage1_has_arch_instr
	    && (   (! stage1.out.redirect)
		|| (stageF.out.ostatus != OSTATUS_BUSY))
	    && csr_regfile.read_dcsr_step
	    && (rg_step_count == 0)) begin

	    rg_step_count <= 1;
	 end
`endif
      endaction
   endfunction

   // ================================================================

`ifdef INCLUDE_TANDEM_VERIF
   rule rl_stage1_mip_cmd (   (rg_state == CPU_RUNNING)
			   && stage1_send_mip_cmd);
      WordXL new_mip = csr_regfile.csr_mip_read;
      rg_prev_mip <= new_mip;

      let trace_data = mkTrace_CSR_WRITE (csr_addr_mip, new_mip);
      f_trace_data.enq (trace_data);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_stage1_mip_cmd: MIP new 0x%0h, old 0x%0h", mcycle, new_mip, rg_prev_mip);
   endrule
`endif

   // ================================================================
   // PIPELINE BEHAVIOR (excluding nonpipe special instructions and exceptions)

   // We do not attempt to manage CSR values in the pipeline like GPRs
   // (read reg, writeback, bypassing) because of complexity: too many
   // CSRs can change simultaneously.  A CSRRx instruction in stage1
   // is stalled until downstream stages are empty. Then, we delay for
   // a cycle before restarting the pipe by re-fetching the next
   // instr, since the fetch may need the just-written CSR value.

`ifdef ISA_C
   // TODO: analyze this carefully; added to resolve a blockage
   // imem_rl_fetch_next_32b is in CPU_Fetch_C.bsv, and calls imem32.req (near_mem.imem_req).
   // fa_stageF_redirect calls stageF.enq which also calls imem.req which calls imem32.req.
   // But cond_i32_odd_fetch_next should make these rules mutually exclusive; why doesn't bsc realize this?
   (* descending_urgency = "imem_rl_fetch_next_32b, rl_pipe" *)
`endif

   rule rl_pipe (   (rg_state == CPU_RUNNING)
		 && (! pipe_is_empty)
		 && (! pipe_has_nonpipe)
		 && (! stage1_halted));

      if (cur_verbosity > 1) $display ("%0d: %m.rl_pipe", mcycle);

      Bool stage3_full = (stage3.out.ostatus != OSTATUS_EMPTY);
      Bool stage2_full = (stage2.out.ostatus != OSTATUS_EMPTY);
      Bool stage1_full = (stage1.out.ostatus != OSTATUS_EMPTY);
      Bool stageD_full = (stageD.out.ostatus != OSTATUS_EMPTY);
      Bool stageF_full = (stageF.out.ostatus != OSTATUS_EMPTY);
      Bool redirect    = False;

      // ----------------
      // Stage3 sink (does regfile writebacks)

      if (stage3.out.ostatus == OSTATUS_PIPE) begin
	 stage3.deq; stage3_full = False;

`ifdef INCLUDE_TANDEM_VERIF
	 // To Verifier
	 let trace_data = stage3.out.trace_data;
	 f_trace_data.enq (trace_data);
`endif
      end

      // ----------------
      // Move instruction from Stage2 to Stage3

      if ((! stage3_full) && (stage2.out.ostatus == OSTATUS_PIPE)) begin
	 stage3.enq (stage2.out.data_to_stage3);  stage3_full = True;
	 stage2.deq;                              stage2_full = False;

	 // Increment csr_INSTRET.
	 // Note: this instr cannot be a CSRRx updating INSTRET, since
	 // CSRRx is done off-pipe
	 csr_regfile.csr_minstret_incr;
`ifdef SHOW_COMPRESSED
	 fa_emit_instr_trace (minstret, stage2.out.data_to_stage3.pc, stage2.out.data_to_stage3.traced_instr, rg_cur_priv);
`else
	 fa_emit_instr_trace (minstret, stage2.out.data_to_stage3.pc, stage2.out.data_to_stage3.instr, rg_cur_priv);
`endif
      end

      // ----------------
      // Move instruction from Stage1 to Stage2, except:
      //  - Discard if in branch mispredict region
      //  - If a branch, proceed only if can redirect stage1

      if (   (! halting)
	  && (! stage2_full)
	  && (stage1.out.ostatus == OSTATUS_PIPE))
	 begin
	    if (stage1.out.control == CONTROL_DISCARD) begin
	       stage2_full = False;
	       stage1_full = False;
	       if (cur_verbosity > 1)
		  $display ("    rl_pipe: Discarding stage1 due to redirection");
	    end
	    else if ((! stage1.out.redirect) || (stageF.out.ostatus != OSTATUS_BUSY)) begin
	       stage2.enq (stage1.out.data_to_stage2);  stage2_full = True;
	       stage1.deq;                              stage1_full = False;

	       if (stage1.out.redirect) begin
		  rg_next_pc <= stage1.out.next_pc;
		  redirect    = True;
	       end
	    end
	 end

      // ----------------
      // Move instruction from StageD to Stage1
      if (   (! stage1_full)
	  && (stageD.out.ostatus == OSTATUS_PIPE))
	 begin
	    stage1.enq (stageD.out.data_to_stage1);  stage1_full = True;
	    stageD.deq;                              stageD_full = False;
	 end

      // ----------------
      // Move instruction from StageF to StageD
      if (   (! stageD_full)
	  && (stageF.out.ostatus == OSTATUS_PIPE))
	 begin
	    stageD.enq (stageF.out.data_to_stageD);  stageD_full = True;
	    stageF.deq;                              stageF_full = False;
	 end

      // ----------------
      // Feed Stage F
      if (   (! stageF_full)
	  && (stageF.out.ostatus == OSTATUS_PIPE))
	 begin
	    CF_Info cf_info = cf_info_none;
	    if (   (stage1.out.ostatus == OSTATUS_PIPE)
		&& (stage1.out.control != CONTROL_DISCARD))
	       cf_info = stage1.out.cf_info;

	    if (redirect)
	       rg_state <= CPU_START_TRAP_HANDLER;
	    else begin
	       stageF.enq (stageF.out.data_to_stageD.epoch,
			   stageF.out.data_to_stageD.pred_pc,
			   rg_cur_priv,
			   sstatus_SUM,
			   mstatus_MXR,
			   csr_regfile.read_satp);
	       stageF_full = True;
	    end

	    // Train branch predictor
	    stageF.bp_train (stageF.out.data_to_stageD.pc,
			     stageF.out.data_to_stageD.is_i32_not_i16,
			     stageF.out.data_to_stageD.instr,
			     cf_info);
	 end

      stage3.set_full (stage3_full);
      stage2.set_full (stage2_full);
      stage1.set_full (stage1_full);    fa_step_check;
      stageD.set_full (stageD_full);
      stageF.set_full (stageF_full);
   endrule: rl_pipe

   // ================================================================
   // Stage2: nonpipe special: all stage2 nonpipe behaviors are traps

   rule rl_stage2_nonpipe (   (rg_state == CPU_RUNNING)
			   && (stage3.out.ostatus == OSTATUS_EMPTY)
			   && (stage2.out.ostatus == OSTATUS_NONPIPE));
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_stage2_nonpipe -> CPU_TRAP", mcycle);

      // Just save relevant info and handle in next clock
      rg_trap_info       <= stage2.out.trap_info;
      rg_trap_interrupt  <= False;
      rg_trap_instr      <= stage2.out.data_to_stage3.instr;
`ifdef SHOW_COMPRESSED
      rg_trap_traced_instr <= stage2.out.data_to_stage3.traced_instr;
`endif
`ifdef INCLUDE_TANDEM_VERIF
      rg_trap_trace_data <= stage2.out.data_to_stage3.trace_data;
`endif

      rg_state           <= CPU_TRAP;
   endrule: rl_stage2_nonpipe

   // ================================================================
   // Stage1: nonpipe traps (except BREAKs that enter Debug Mode)

`ifdef INCLUDE_GDB_CONTROL
   Bool break_into_Debug_Mode = (   (stage1.out.trap_info.exc_code == exc_code_BREAKPOINT)
				 && csr_regfile.dcsr_break_enters_debug (rg_cur_priv));
`else
   Bool break_into_Debug_Mode = False;
`endif

   rule rl_stage1_trap (   (rg_state == CPU_RUNNING)
			&& (! halting)
			&& (stage3.out.ostatus == OSTATUS_EMPTY)
			&& (stage2.out.ostatus == OSTATUS_EMPTY)
			&& (stage1.out.ostatus == OSTATUS_NONPIPE)
			&& (stage1.out.control == CONTROL_TRAP)
			&& (! break_into_Debug_Mode)
			&& (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_trap", mcycle);

      // Just save relevant info and handle in next clock
      rg_trap_info       <= stage1.out.trap_info;
      rg_trap_interrupt  <= False;
      rg_trap_instr      <= stage1.out.data_to_stage2.instr;
`ifdef SHOW_COMPRESSED
      rg_trap_traced_instr <= stage1.out.data_to_stage2.traced_instr;
`endif
`ifdef INCLUDE_TANDEM_VERIF
      rg_trap_trace_data <= stage1.out.data_to_stage2.trace_data;
`endif

      rg_state           <= CPU_TRAP;
   endrule: rl_stage1_trap

   // ================================================================
   // Traps

   rule rl_trap ((rg_state == CPU_TRAP)
		 && (stageF.out.ostatus != OSTATUS_BUSY));
      let epc          = rg_trap_info.epc;
      let exc_code     = rg_trap_info.exc_code;
      let tval         = rg_trap_info.tval;
      let instr        = rg_trap_instr;
      let is_interrupt = rg_trap_interrupt;

      // Take trap, save trap information for next phase
      let trap_info <- csr_regfile.csr_trap_actions (rg_cur_priv,    // from priv
						     epc,
						     (is_interrupt && csr_regfile.nmi_pending),
						     (is_interrupt && (! csr_regfile.nmi_pending)),
						     exc_code,
						     tval);

      let next_pc    = trap_info.pc;
      let new_mstatus= trap_info.mstatus;
      let mcause     = trap_info.mcause;
      let new_priv   = trap_info.priv;

      // Save new privilege and pc for ifetch
      rg_cur_priv <= new_priv;
      rg_next_pc  <= next_pc;

      // Note old MSTATUS.MXR and SSTATUS.SUM for initiating FETCH in next phase
      rg_mstatus_MXR <= mstatus_MXR;
      rg_sstatus_SUM <= sstatus_SUM;

      rg_state <= CPU_START_TRAP_HANDLER;

      stageD.set_full (False);
      stage1.set_full (False);    fa_step_check;
      stage2.set_full (False);

      // Accounting    TODO: should traps be counted as retired insrs?
      // csr_regfile.csr_minstret_incr;

      // Tandem Verification and Debug related actions
`ifdef INCLUDE_TANDEM_VERIF
      // Trace data
      Trace_Data trace_data;
      if (is_interrupt)
	 trace_data = mkTrace_INTR (next_pc, new_priv, new_mstatus, mcause, epc, 0);
      else begin
	 trace_data = rg_trap_trace_data;
	 trace_data.op = TRACE_TRAP;
	 trace_data.pc = next_pc;
	 // trace_data.instr_sz    should already be set
	 // trace_data.instr       should already be set
	 trace_data.rd    = zeroExtend (new_priv);
	 trace_data.word1 = new_mstatus;
	 trace_data.word2 = mcause;
	 trace_data.word3 = zeroExtend (epc);
	 trace_data.word4 = tval;
      end
      f_trace_data.enq (trace_data);
`endif

`ifdef SHOW_COMPRESSED
      fa_emit_instr_trace (minstret, epc, rg_trap_traced_instr, rg_cur_priv);
`else
      fa_emit_instr_trace (minstret, epc, rg_trap_instr, rg_cur_priv);
`endif
      // Debug
      if (cur_verbosity > 1)
	 $display ("    mcause:0x%0h  epc 0x%0h  tval:0x%0h  next_pc 0x%0h, new_priv %0d new_mstatus 0x%0h",
		   mcause, epc, tval, next_pc, new_priv, new_mstatus);
   endrule: rl_trap

   // ================================================================
   // Stage1: nonpipe special: CSRRW and CSRRWI

   rule rl_stage1_CSRR_W (   (rg_state == CPU_RUNNING)
			  && (! halting)
			  && (stage3.out.ostatus == OSTATUS_EMPTY)
			  && (stage2.out.ostatus == OSTATUS_EMPTY)
			  && (stage1.out.ostatus == OSTATUS_NONPIPE)
			  && (stage1.out.control == CONTROL_CSRR_W));

      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_CSRR_W", mcycle);

      // Register required info and handle in next clock
      rg_csr_pc  <= stage1.out.data_to_stage2.pc;
      rg_next_pc <= stage1.out.next_pc;

`ifdef ISA_F
      // With FP, the val is always Bit #(64)
      // TODO: is this ifdef necessary? Can't we always use 'truncate'?
      rg_csr_val1 <= truncate (stage1.out.data_to_stage2.val1);
`else
      rg_csr_val1 <= stage1.out.data_to_stage2.val1;
`endif

      // In case of trap (illegal CSRRW)
      rg_trap_info         <= Trap_Info {epc:      stage1.out.data_to_stage2.pc,
					 exc_code: exc_code_ILLEGAL_INSTRUCTION,
					 tval:     stage1.out.trap_info.tval};
      rg_trap_interrupt    <= False;
      rg_trap_instr        <= stage1.out.data_to_stage2.instr; // Also used in successful CSSRW
`ifdef SHOW_COMPRESSED
      rg_trap_traced_instr <= stage1.out.data_to_stage2.instr; // These instructions never compressed
`endif
`ifdef INCLUDE_TANDEM_VERIF
      rg_trap_trace_data   <= stage1.out.data_to_stage2.trace_data;
`endif

      rg_state <= CPU_CSRRW_2;
   endrule: rl_stage1_CSRR_W

   // ----------------

   rule rl_stage1_CSRR_W_2 (rg_state == CPU_CSRRW_2);
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_CSRR_W_2", mcycle);

      let instr    = rg_trap_instr;
      let csr_addr = instr_csr    (instr);
      let rs1      = instr_rs1    (instr);
      let funct3   = instr_funct3 (instr);
      let rd       = instr_rd     (instr);

      let rs1_val  = (  (funct3 == f3_CSRRW)
		      ? rg_csr_val1                       // CSRRW
		      : extend (rs1));                    // CSRRWI

      Bool read_not_write = False;    // CSRRW always writes the CSR
      Bool permitted = csr_regfile.access_permitted_1 (rg_cur_priv, csr_addr, read_not_write);

      if (! permitted) begin
	 rg_state <= CPU_TRAP;

	 // Debug
	 if (cur_verbosity > 1) begin
	    $display ("    Trap on CSR permissions: Rs1 %0d Rs1_val 0x%0h csr 0x%0h Rd %0d",
		      rs1, rs1_val, csr_addr, rd);
	 end
      end
      else begin
	 // Read the CSR only if Rd is not 0
	 WordXL csr_val = ?;
	 if (rd != 0) begin
	    begin
	       // Note: csr_regfile.read should become ActionValue if it acquires side effects
	       let m_csr_val = csr_regfile.read_csr (csr_addr);
	       csr_val   = fromMaybe (?, m_csr_val);
	    end
	 end

	 // Writeback to GPR file
	 let new_rd_val = csr_val;
	 gpr_regfile.write_rd (rd, new_rd_val);

	 // Writeback to CSR file
	 let csr_write_result <- csr_regfile.mav_csr_write (csr_addr, rs1_val);
	 let new_csr_val       = csr_write_result.new_csr_value;
	 let m_new_mstatus     = csr_write_result.m_new_csr_value2;

	 // Accounting
	 csr_regfile.csr_minstret_incr;

	 // Restart the pipe
	 rg_state   <= CPU_CSRRX_RESTART;

`ifdef INCLUDE_TANDEM_VERIF
	 // Trace data
	 let trace_data = mkTrace_CSRRX (rg_trap_trace_data.pc,
					 rg_trap_trace_data.instr_sz,
					 rg_trap_trace_data.instr,
					 rd,
					 new_rd_val,
					 True,    // updated-CSR info is valid
					 csr_addr,
					 new_csr_val,
					 isValid (m_new_mstatus),
					 fromMaybe (?, m_new_mstatus));
	 f_trace_data.enq (trace_data);
`endif

	 // Debug
	 // Note: CSR instructions are never compressed.
	 fa_emit_instr_trace (minstret, rg_csr_pc, instr, rg_cur_priv);
	 if (cur_verbosity > 1) begin
	    $display ("    S1: write CSRRW/CSRRWI Rs1 %0d Rs1_val 0x%0h csr 0x%0h csr_val 0x%0h Rd %0d",
		      rs1, rs1_val, csr_addr, csr_val, rd);
	 end
      end
   endrule: rl_stage1_CSRR_W_2

   // ================================================================
   // Stage1: nonpipe special: CSRRS, CSRRSI, CSRRC, CSRRCI

   rule rl_stage1_CSRR_S_or_C (   (rg_state == CPU_RUNNING)
			       && (! halting)
			       && (stage3.out.ostatus == OSTATUS_EMPTY)
			       && (stage2.out.ostatus == OSTATUS_EMPTY)
			       && (stage1.out.ostatus == OSTATUS_NONPIPE)
			       && (stage1.out.control == CONTROL_CSRR_S_or_C));

      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_CSRR_S_or_C", mcycle);

      // Register required info and handle in next clock
      rg_csr_pc  <= stage1.out.data_to_stage2.pc;
      rg_next_pc <= stage1.out.next_pc;

`ifdef ISA_F
      // With FP, the val is always Bit #(64)
      // TODO: is this ifdef necessary? Can't we always use 'truncate'?
      rg_csr_val1 <= truncate (stage1.out.data_to_stage2.val1);
`else
      rg_csr_val1 <= stage1.out.data_to_stage2.val1;
`endif

      // In case of trap (illegal CSRRW)
      rg_trap_info      <= Trap_Info {epc:      stage1.out.data_to_stage2.pc,
                                      exc_code: exc_code_ILLEGAL_INSTRUCTION,
                                      tval:     stage1.out.trap_info.tval};
      rg_trap_interrupt <= False;
      rg_trap_instr     <= stage1.out.data_to_stage2.instr;    // TODO: this is also used for successful CSRRW
`ifdef INCLUDE_TANDEM_VERIF
      rg_trap_trace_data <= stage1.out.data_to_stage2.trace_data;    // TODO: this is also used for successful CSRRW
`endif

      rg_state <= CPU_CSRR_S_or_C_2;
   endrule: rl_stage1_CSRR_S_or_C

   // ----------------

   rule rl_stage1_CSRR_S_or_C_2 (rg_state == CPU_CSRR_S_or_C_2);
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_CSRR_S_or_C_2", mcycle);

      let instr    = rg_trap_instr;
      let csr_addr = instr_csr    (instr);
      let rs1      = instr_rs1    (instr);
      let funct3   = instr_funct3 (instr);
      let rd       = instr_rd     (instr);

      let rs1_val  = (  ((funct3 == f3_CSRRS) || (funct3 == f3_CSRRC))
		      ? rg_csr_val1                      // CSRRS,  CSRRC
		      : extend (rs1));                   // CSRRSI, CSRRCI

      Bool read_not_write = (rs1_val == 0);    // CSRR_S_or_C only reads, does not write CSR, if rs1_val == 0
      Bool permitted = csr_regfile.access_permitted_2 (rg_cur_priv, csr_addr, read_not_write);

      if (! permitted) begin
	 rg_state <= CPU_TRAP;

	 // Debug
	 if (cur_verbosity > 1) begin
	    $display ("    Trap on CSR permissions: Rs1 %0d Rs1_val 0x%0h csr 0x%0h Rd %0d",
		      rs1, rs1_val, csr_addr, rd);
	 end
      end
      else begin
	 // Read the CSR
	 WordXL csr_val = ?;
	 begin
	    // Note: csr_regfile.read should become ActionValue if it acquires side effects
	    let m_csr_val  = csr_regfile.read_csr (csr_addr);
	    csr_val = fromMaybe (?, m_csr_val);
	 end

	 // Writeback to GPR file
	 let new_rd_val = csr_val;
	 gpr_regfile.write_rd (rd, new_rd_val);

	 // Writeback to CSR file, but only if rs1 != 0
	 WordXL          new_csr_val = ?;
	 Maybe #(WordXL) m_new_mstatus = tagged Invalid;
	 if (rs1 != 0) begin
	    let x = (  ((funct3 == f3_CSRRS) || (funct3 == f3_CSRRSI))
		     ? (csr_val | rs1_val)                // CSRRS, CSRRSI
		     : csr_val & (~ rs1_val));            // CSRRC, CSRRCI

	    let csr_write_result <- csr_regfile.mav_csr_write (csr_addr, x);
	    new_csr_val           = csr_write_result.new_csr_value;
	    m_new_mstatus         = csr_write_result.m_new_csr_value2;
	 end

	 // Accounting
	 csr_regfile.csr_minstret_incr;

	 // Restart the pipe
	 rg_state   <= CPU_CSRRX_RESTART;

`ifdef INCLUDE_TANDEM_VERIF
	 // Trace data
	 let trace_data = mkTrace_CSRRX (rg_trap_trace_data.pc,
					 rg_trap_trace_data.instr_sz,
					 rg_trap_trace_data.instr,
					 rd,
					 new_rd_val,
					 (rs1 != 0),    // whether we've written csr or not
					 csr_addr,
					 new_csr_val,
					 isValid (m_new_mstatus),
					 fromMaybe (?, m_new_mstatus));
	 f_trace_data.enq (trace_data);
`endif

	 // Debug
	 // Note: CSR instructions are never compressed.
	 fa_emit_instr_trace (minstret, rg_csr_pc, instr, rg_cur_priv);
	 if (cur_verbosity > 1) begin
	    $display ("    S1: write CSRR_S_or_C: Rs1 %0d Rs1_val 0x%0h csr 0x%0h csr_val 0x%0h Rd %0d",
		      rs1, rs1_val, csr_addr, csr_val, rd);
	 end
      end
   endrule: rl_stage1_CSRR_S_or_C_2

   // ================================================================
   // Restart the pipe after a CSRRX stall
   // waiting for stageF to be non-busy (servicing a previous request)

   rule rl_stage1_restart_after_csrrx (   (rg_state == CPU_CSRRX_RESTART)
				       && (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_stage1_restart_after_csrrx", mcycle);

      stageD.set_full (False);
      stage1.set_full (False);    fa_step_check;
      fa_stageF_redirect (rg_next_pc);
   endrule

   // ================================================================
   // Stage1: nonpipe special: MRET/SRET/URET

   rule rl_stage1_xRET (   (rg_state == CPU_RUNNING)
			&& (! halting)
			&& (stage3.out.ostatus == OSTATUS_EMPTY)
			&& (stage2.out.ostatus == OSTATUS_EMPTY)
			&& (stage1.out.ostatus == OSTATUS_NONPIPE)
			&& (   (stage1.out.control == CONTROL_MRET)
			    || (stage1.out.control == CONTROL_SRET)
			    || (stage1.out.control == CONTROL_URET))
			&& (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_xRET", mcycle);

      // Return-from-exception actions on CSRs
      Priv_Mode from_priv = ((stage1.out.control == CONTROL_MRET) ?
			     m_Priv_Mode : ((stage1.out.control == CONTROL_SRET) ?
					    s_Priv_Mode : u_Priv_Mode));
      match { .next_pc, .new_priv, .new_mstatus } <- csr_regfile.csr_ret_actions (from_priv);
      // Save new privilege and pc for ifetch
      rg_cur_priv <= new_priv;
      rg_next_pc  <= next_pc;

      // Note old MSTATUS.MXR and SSTATUS.SUM for initiating FETCH in next phase
      rg_mstatus_MXR <= mstatus_MXR;
      rg_sstatus_SUM <= sstatus_SUM;

      rg_state <= CPU_START_TRAP_HANDLER;    // TODO: bad naming; this is not starting the trap handler

      stageD.set_full (False);
      stage1.set_full (False);    fa_step_check;

      // Accounting
      csr_regfile.csr_minstret_incr;

`ifdef INCLUDE_TANDEM_VERIF
      // Trace data
      let td  = stage1.out.data_to_stage2.trace_data;
      let td1 = mkTrace_RET (next_pc, td.instr_sz, td.instr, new_priv, new_mstatus);
      f_trace_data.enq (td1);
`endif

      // Debug
`ifdef SHOW_COMPRESSED
      fa_emit_instr_trace (minstret, stage1.out.data_to_stage2.pc, stage1.out.data_to_stage2.traced_instr, rg_cur_priv);
`else
      fa_emit_instr_trace (minstret, stage1.out.data_to_stage2.pc, stage1.out.data_to_stage2.instr, rg_cur_priv);
`endif
      if (cur_verbosity > 1)
	 $display ("    xRET: next_pc:0x%0h  new mstatus:0x%0h  new priv:%0d", next_pc, new_mstatus, new_priv);
   endrule: rl_stage1_xRET

   // ================================================================
   // Stage1: nonpipe special: FENCE.I

   rule rl_stage1_FENCE_I (   (rg_state== CPU_RUNNING)
			   && (! halting)
			   && (stage3.out.ostatus == OSTATUS_EMPTY)
			   && (stage2.out.ostatus == OSTATUS_EMPTY)
			   && (stage1.out.ostatus == OSTATUS_NONPIPE)
			   && (stage1.out.control == CONTROL_FENCE_I)
			   && (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_FENCE_I", mcycle);

      // Save stage1.out.next_pc since it can be destroyed by FENCE.I op
      rg_next_pc <= stage1.out.next_pc;
      near_mem.server_fence_i.request.put (?);
      rg_state <= CPU_FENCE_I;
   endrule

   // ----------------
   // Finish FENCE.I

   rule rl_finish_FENCE_I (rg_state == CPU_FENCE_I);
      if (cur_verbosity > 1) $display ("%0d: %m.rl_finish_FENCE_I", mcycle);

      // Await mem system FENCE.I completion
      let dummy <- near_mem.server_fence_i.response.get;

      // Accounting
      csr_regfile.csr_minstret_incr;
      // Debug
      fa_emit_instr_trace (minstret,
			   stage1.out.data_to_stage2.pc,
`ifdef SHOW_COMPRESSED
			   stage1.out.data_to_stage2.traced_instr,
`else
			   stage1.out.data_to_stage2.instr,
`endif
			   rg_cur_priv);
`ifdef INCLUDE_TANDEM_VERIF
      let trace_data = stage1.out.data_to_stage2.trace_data;
      f_trace_data.enq (trace_data);
`endif
      // Resume pipe
      stageD.set_full (False);
      stage1.set_full (False);    fa_step_check;
      fa_stageF_redirect (rg_next_pc);
   endrule: rl_finish_FENCE_I

   // ================================================================
   // Stage1: nonpipe special: FENCE

   rule rl_stage1_FENCE (   (rg_state== CPU_RUNNING)
			 && (! halting)
			 && (stage3.out.ostatus == OSTATUS_EMPTY)
			 && (stage2.out.ostatus == OSTATUS_EMPTY)
			 && (stage1.out.ostatus == OSTATUS_NONPIPE)
			 && (stage1.out.control == CONTROL_FENCE)
			 && (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_FENCE", mcycle);

      // Save stage1.out.next_pc since it can be destroyed by FENCE op
      rg_next_pc <= stage1.out.next_pc;
      near_mem.server_fence.request.put (?);
      rg_state <= CPU_FENCE;
   endrule: rl_stage1_FENCE

   // ----------------

   rule rl_finish_FENCE (rg_state == CPU_FENCE);
      if (cur_verbosity > 1) $display ("%0d: %m.rl_finish_FENCE", mcycle);

      // Await mem system FENCE completion
      let dummy <- near_mem.server_fence.response.get;

      // Accounting
      csr_regfile.csr_minstret_incr;
      // Debug
      fa_emit_instr_trace (minstret, stage1.out.data_to_stage2.pc,
`ifdef SHOW_COMPRESSED
			   stage1.out.data_to_stage2.traced_instr,
`else
			   stage1.out.data_to_stage2.instr,
`endif
			   rg_cur_priv);
`ifdef INCLUDE_TANDEM_VERIF
      // Trace data
      let trace_data = stage1.out.data_to_stage2.trace_data;
      f_trace_data.enq (trace_data);
`endif

      // Resume pipe
      stageD.set_full (False);
      stage1.set_full (False);    fa_step_check;
      fa_stageF_redirect (rg_next_pc);
   endrule: rl_finish_FENCE

   // ================================================================
   // Stage1: nonpipe special: SFENCE.VMA

`ifdef ISA_PRIV_S
`ifdef ISA_C
   // TODO: analyze this carefully; added to resolve a blockage
   // imem_rl_fetch_next_32b is in CPU_Fetch_C.bsv, and calls imem32.req (near_mem.imem_req).
   // fa_stageF_redirect calls stageF.enq which also calls imem.req which calls imem32.req.
   // But cond_i32_odd_fetch_next should make these rules mutually exclusive; why doesn't bsc realize this?
   (* descending_urgency = "imem_rl_fetch_next_32b, rl_stage1_SFENCE_VMA" *)
`endif

   rule rl_stage1_SFENCE_VMA (   (rg_state== CPU_RUNNING)
			      && (! halting)
			      && (stage3.out.ostatus == OSTATUS_EMPTY)
			      && (stage2.out.ostatus == OSTATUS_EMPTY)
			      && (stage1.out.ostatus == OSTATUS_NONPIPE)
			      && (stage1.out.control == CONTROL_SFENCE_VMA)
			      && (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_SFENCE_VMA", mcycle);

      // Save stage1.out.next_pc since it can be destroyed by FENCE op
      rg_next_pc <= stage1.out.next_pc;
      near_mem.sfence_vma_server.request.put (?);
      rg_state <= CPU_SFENCE_VMA;
   endrule: rl_stage1_SFENCE_VMA

   // ----------------

   rule rl_finish_SFENCE_VMA (rg_state == CPU_SFENCE_VMA);
      if (cur_verbosity > 1) $display ("%0d: %m.rl_finish_SFENCE_VMA", mcycle);

      // Await SFENCE.VMA completion
      let dummy <- near_mem.sfence_vma_server.response.get;

      // Accounting
      csr_regfile.csr_minstret_incr;
      // Debug
      fa_emit_instr_trace (minstret,
			   stage1.out.data_to_stage2.pc,
`ifdef SHOW_COMPRESSED
			   stage1.out.data_to_stage2.traced_instr,
`else
			   rg_cur_priv);
`endif
`ifdef INCLUDE_TANDEM_VERIF
      // Trace data
      let trace_data = stage1.out.data_to_stage2.trace_data;
      f_trace_data.enq (trace_data);
`endif
      // Resume pipe
      stageD.set_full (False);
      stage1.set_full (False);    fa_step_check;
      fa_stageF_redirect (rg_next_pc);
   endrule: rl_finish_SFENCE_VMA
`endif

   // ================================================================
   // Stage1: nonpipe special: WFI

   rule rl_stage1_WFI (   (rg_state== CPU_RUNNING)
		       && (! halting)
		       && (stage3.out.ostatus == OSTATUS_EMPTY)
		       && (stage2.out.ostatus == OSTATUS_EMPTY)
		       && (stage1.out.ostatus == OSTATUS_NONPIPE)
		       && (stage1.out.control == CONTROL_WFI)
		       && (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_WFI", mcycle);

      rg_next_pc <= stage1.out.next_pc;
      rg_state   <= CPU_WFI_PAUSED;

      stageD.set_full (False);
      stage1.set_full (False);    fa_step_check;

      // Accounting
      csr_regfile.csr_minstret_incr;

`ifdef INCLUDE_TANDEM_VERIF
      // Trace data
      let trace_data = stage1.out.data_to_stage2.trace_data;
      f_trace_data.enq (trace_data);
`endif

      // Debug
`ifdef SHOW_COMPRESSED
      fa_emit_instr_trace (minstret, stage1.out.data_to_stage2.pc, stage1.out.data_to_stage2.traced_instr, rg_cur_priv);
`else
      fa_emit_instr_trace (minstret, stage1.out.data_to_stage2.pc, stage1.out.data_to_stage2.instr, rg_cur_priv);
`endif
      if (cur_verbosity > 1)
	 $display ("    CPU.rl_stage1_WFI");
   endrule: rl_stage1_WFI

   // ----------------

   rule rl_WFI_resume (   (rg_state == CPU_WFI_PAUSED)
		       && (   csr_regfile.wfi_resume
			   || stop_step_req)
		       && (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_WFI_resume", mcycle);

      // Resume pipe (it will handle the interrupt, if one is pending)
      stageD.set_full (False);
      fa_stageF_redirect (rg_next_pc);
   endrule: rl_WFI_resume

   // ----------------
   rule rl_reset_from_WFI (   (rg_state == CPU_WFI_PAUSED)
			   && f_reset_reqs.notEmpty);
      if (cur_verbosity > 1) $display ("%0d: %m.rl_reset_from_WFI", mcycle);

      rg_state <= CPU_RESET1;
   endrule: rl_reset_from_WFI

   // ================================================================
   // Initiate instruction fetch from new_pc.
   // These actions were formerly part of the stage1 and stage2 trap,
   // external interrupt and RET rules. Separated to break long timing
   // paths from stage2 and stage3 status to IFetch

   rule rl_trap_fetch ((rg_state == CPU_START_TRAP_HANDLER)
		       && (stageF.out.ostatus != OSTATUS_BUSY));
      stageD.set_full (False);
      fa_stageF_redirect (rg_next_pc);
   endrule: rl_trap_fetch

   // ================================================================
   // Stage1: nonpipe trap: BREAK into Debug Mode when dcsr.ebreakm/s/u is set
   // Not setting tval, as we are breaking to the debugger.
   // TODO: Does the spec say anything about this?

`ifdef INCLUDE_GDB_CONTROL
   rule rl_trap_BREAK_to_Debug_Mode (   (rg_state == CPU_RUNNING)
				     && (! halting)
				     && (stage3.out.ostatus == OSTATUS_EMPTY)
				     && (stage2.out.ostatus == OSTATUS_EMPTY)
				     && (stage1.out.ostatus == OSTATUS_NONPIPE)
				     && (stage1.out.control == CONTROL_TRAP)
				     && (stageF.out.ostatus != OSTATUS_BUSY)
				     && break_into_Debug_Mode);
      if (cur_verbosity > 1) $display ("%0d: %m.rl_trap_BREAK_to_Debug_Mode", mcycle);

      let pc    = stage1.out.data_to_stage2.pc;
      let instr = stage1.out.data_to_stage2.instr;

      $display ("%0d: %m.rl_trap_BREAK_to_Debug_Mode: PC 0x%08h instr 0x%08h", mcycle, pc, instr);
      if (cur_verbosity > 1)
	 $display ("    Flushing caches");

      csr_regfile.write_dcsr_cause_priv (DCSR_CAUSE_EBREAK, rg_cur_priv);
      csr_regfile.write_dpc (pc);    // Where we'll resume on 'continue'
      rg_state <= CPU_GDB_PAUSING;

      // Flush both caches -- using the same interface as that used by FENCE_I
      near_mem.server_fence_i.request.put (?);

      // Notify debugger that we've halted
      f_run_halt_rsps.enq (False);
   endrule: rl_trap_BREAK_to_Debug_Mode

   // ----------------
   // Handle the flush responses from the caches when the flush was initiated
   // on entering CPU_GDB_PAUSING state

   rule rl_BREAK_cache_flush_finish (rg_state == CPU_GDB_PAUSING);
      let ack <- near_mem.server_fence_i.response.get;
      rg_state <= CPU_DEBUG_MODE;

      // Notify debugger that we've halted
      f_run_halt_rsps.enq (False);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_BREAK_cache_flush_finish", mcycle);
   endrule
`endif

   // ----------------
   // Reset from Debug Module (or SoC)

   rule rl_reset_from_Debug_Module (f_reset_reqs.notEmpty && (rg_state != CPU_RESET1));
      $display ("%0d: %m.rl_reset_from_Debug_Module", mcycle);
      rg_state <= CPU_RESET1;
   endrule

   // ================================================================
   // EXTERNAL and GDB INTERRUPTS while running
   // We take an interrupt when Stage1 is frozen
   // and Stage2 and Stage3 have drained,
   // encapsulated in condition 'stage1_take_interrupt'

   rule rl_stage1_interrupt (interrupt_pending
			     && (rg_state == CPU_RUNNING)
			     && stage1_take_interrupt
			     && (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_interrupt", mcycle);

      // Just save relevant info and handle in next clock
      Exc_Code exc_code = 0;    // "Unknown cause" for NMI
      if (csr_regfile.interrupt_pending (rg_cur_priv) matches tagged Valid .ec
	  &&& (! csr_regfile.nmi_pending))
	 exc_code = ec;

      rg_trap_info       <= Trap_Info {epc:      stage1.out.data_to_stage2.pc,
				       exc_code: exc_code,
				       tval:     0};
      rg_trap_interrupt  <= True;
      rg_trap_instr      <= stage1.out.data_to_stage2.instr;

`ifdef INCLUDE_TANDEM_VERIF
      // rg_trap_trace_data <= ?;    // Will be filled in in rl_trap
`endif

      rg_state           <= CPU_TRAP;
   endrule: rl_stage1_interrupt

   // ================================================================
   // Stage1: Handle debugger stop-request and dcsr.step step-request while running
   // and no interrupt pending.  Stage1 has an architectural instruction,
   // and stage2 and stage3 are empty, and stageF is not BUSY.

`ifdef INCLUDE_GDB_CONTROL
   rule rl_stage1_stop (   (rg_state== CPU_RUNNING)
			&& stage1_stop
			&& (stageF.out.ostatus != OSTATUS_BUSY));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_stage1_stop", mcycle);

      let pc    = stage1.out.data_to_stage2.pc;    // We'll retry this instruction on 'continue'
      let instr = stage1.out.data_to_stage2.instr;

      // Report CPI only stop-req, but not on step-req (where it's not very useful)
      if (rg_stop_req) begin
	 $display ("%0d: %m.rl_stage1_stop: Stop for debugger. minstret %0d priv %0d PC 0x%0h instr 0x%0h",
		   mcycle, minstret, rg_cur_priv, pc, instr);
	 fa_report_CPI;
      end
      else
	 $display ("%0d: %m.rl_stage1_stop: Stop after single-step. PC = 0x%08h", mcycle, pc);

      DCSR_Cause cause= (rg_stop_req ? DCSR_CAUSE_HALTREQ : DCSR_CAUSE_STEP);
      csr_regfile.write_dcsr_cause_priv (cause, rg_cur_priv);
      csr_regfile.write_dpc (pc);    // We'll retry this instruction on 'continue'
      rg_state      <= CPU_GDB_PAUSING;
      rg_stop_req   <= False;
      rg_step_count <= 0;

      // Flush both caches -- using the same interface as that used by FENCE_I
      near_mem.server_fence_i.request.put (?);

      // Accounting: none (instruction is abandoned)
   endrule: rl_stage1_stop
`endif

   // ================================================================
   // ================================================================
   // ================================================================
   // DEBUGGER ACCESS

   // ----------------
   // Debug Module Run/Halt control

`ifdef INCLUDE_GDB_CONTROL
   rule rl_debug_run ((f_run_halt_reqs.first == True) && (rg_state == CPU_DEBUG_MODE));
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_run", mcycle);

      f_run_halt_reqs.deq;

      // Debugger 'resume' request (e.g., GDB 'continue' command)
      let dpc = csr_regfile.read_dpc;
      fa_restart_from_halt (dpc);

      // Notify debugger that we've started running
      f_run_halt_rsps.enq (True);
   endrule

   (* descending_urgency = "rl_debug_run_redundant, rl_pipe" *)
   rule rl_debug_run_redundant ((f_run_halt_reqs.first == True) && fn_is_running (rg_state));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_debug_run_redundant", mcycle);

      f_run_halt_reqs.deq;

      // Notify debugger that we're running
      f_run_halt_rsps.enq (True);

      $display ("%0d: %m.debug_run_redundant: CPU already running.", mcycle);
   endrule

   (* descending_urgency = "rl_debug_halt, rl_pipe" *)
   rule rl_debug_halt ((f_run_halt_reqs.first == False) && fn_is_running (rg_state));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_debug_halt", mcycle);

      f_run_halt_reqs.deq;

      // Debugger 'halt' request (e.g., GDB '^C' command)
      rg_stop_req <= True;
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_halt", mcycle);
   endrule

   rule rl_debug_halt_redundant ((f_run_halt_reqs.first == False) && (! fn_is_running (rg_state)));
      if (cur_verbosity > 1) $display ("%0d: %m.rl_debug_halt_redundant", mcycle);

      f_run_halt_reqs.deq;

      // Notify debugger that we've 'halted'
      f_run_halt_rsps.enq (False);

      $display ("%0d: %m.rl_debug_halt_redundant: CPU already halted.", mcycle);
      $display ("    state = ", fshow (rg_state));
   endrule

   // ----------------
   // Debug Module GPR read/write

   rule rl_debug_read_gpr ((rg_state == CPU_DEBUG_MODE) && (! f_gpr_reqs.first.write));
      let req <- pop (f_gpr_reqs);
      Bit #(5) regname = req.address;
      let data = gpr_regfile.read_rs1_port2 (regname);
      let rsp = DM_CPU_Rsp {ok: True, data: data};
      f_gpr_rsps.enq (rsp);
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_read_gpr: reg %0d => 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_write_gpr ((rg_state == CPU_DEBUG_MODE) && f_gpr_reqs.first.write);
      let req <- pop (f_gpr_reqs);
      Bit #(5) regname = req.address;
      let data = req.data;
      gpr_regfile.write_rd (regname, data);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_gpr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_write_gpr: reg %0d <= 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_gpr_access_busy (rg_state != CPU_DEBUG_MODE);
      let req <- pop (f_gpr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_gpr_rsps.enq (rsp);

      if (cur_verbosity > 1) $display ("%0d: %m.rl_debug_gpr_access_busy", mcycle);
   endrule

   // ----------------
   // Debug Module FPR read/write

`ifdef ISA_F
   rule rl_debug_read_fpr ((rg_state == CPU_DEBUG_MODE) && (! f_fpr_reqs.first.write));
      let req <- pop (f_fpr_reqs);
      Bit #(5) regname = req.address;
      let data = fpr_regfile.read_rs1_port2 (regname);
      let rsp = DM_CPU_Rsp {ok: True, data: data};
      f_fpr_rsps.enq (rsp);
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_read_fpr: reg %0d => 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_write_fpr ((rg_state == CPU_DEBUG_MODE) && f_fpr_reqs.first.write);
      let req <- pop (f_fpr_reqs);
      Bit #(5) regname = req.address;
      let data = req.data;
      fpr_regfile.write_rd (regname, data);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_fpr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_write_fpr: reg %0d <= 0x%0h",
		   mcycle, regname, data);
   endrule

   rule rl_debug_fpr_access_busy (rg_state != CPU_DEBUG_MODE);
      let req <- pop (f_fpr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_fpr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_fpr_access_busy", mcycle);
   endrule
`endif

   // ----------------
   // Debug Module CSR read/write

   rule rl_debug_read_csr ((rg_state == CPU_DEBUG_MODE) && (! f_csr_reqs.first.write));
      let req <- pop (f_csr_reqs);
      Bit #(12) csr_addr = req.address;
      let m_data = csr_regfile.read_csr_port2 (csr_addr);
      let data = fromMaybe (?, m_data);
      let rsp = DM_CPU_Rsp {ok: True, data: data};
      f_csr_rsps.enq (rsp);
      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_read_csr: csr %0d => 0x%0h",
		   mcycle, csr_addr, data);
   endrule

   rule rl_debug_write_csr ((rg_state == CPU_DEBUG_MODE) && f_csr_reqs.first.write);
      let req <- pop (f_csr_reqs);
      Bit #(12) csr_addr = req.address;
      let data = req.data;
      let new_csr_val <- csr_regfile.mav_csr_write (csr_addr, data);

      let rsp = DM_CPU_Rsp {ok: True, data: ?};
      f_csr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_write_csr: csr 0x%0h 0x%0h <= 0x%0h",
		   mcycle, csr_addr, data, new_csr_val);
   endrule

   rule rl_debug_csr_access_busy (rg_state != CPU_DEBUG_MODE);
      let req <- pop (f_csr_reqs);
      let rsp = DM_CPU_Rsp {ok: False, data: ?};
      f_csr_rsps.enq (rsp);

      if (cur_verbosity > 1)
	 $display ("%0d: %m.rl_debug_csr_access_busy", mcycle);
   endrule
`endif

   // ================================================================
   // ================================================================
   // ================================================================
   // INTERFACE

   // ----------------
   // SoC fabric connections

`ifndef Near_Mem_TCM
   // IMem to fabric master interface
   interface  imem_master = near_mem.imem_master;
`endif

`ifdef Near_Mem_TCM
   // DMem to fabric master interface
   interface  dmem_master = near_mem.dmem_master;

`ifdef DUAL_FABRIC
   interface  nmio_master = near_mem.nmio_master;
`endif
`else
   // DMem to fabric master interface
   interface Near_Mem_Fabric_IFC  mem_master = near_mem.mem_master;
`endif

   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

`ifdef INCLUDE_DMEM_SLAVE
   interface  dmem_slave = near_mem.dmem_slave;
`endif

`ifdef Near_Mem_TCM
   // ----------------
   // Debug access to ITCM
   interface AXI4_Slave_IFC imem_dma_server = near_mem.imem_dma_server;

   // DMA/Debug access to DTCM
   interface AXI4_Slave_IFC dmem_dma_server = near_mem.dmem_dma_server;
`else
   // ----------------
   // Interface to 'coherent DMA' port of optional L2 cache

   interface AXI4_Slave_IFC dma_server = near_mem.dma_server;
`endif

   // ----------------------------------------------------------------
   // External interrupts

   method Action  m_external_interrupt_req (x) = csr_regfile.m_external_interrupt_req (x);
   method Action  s_external_interrupt_req (x) = csr_regfile.s_external_interrupt_req (x);

   // ----------------
   // Software and timer interrupts (from Near_Mem_IO/CLINT)

   method Action  software_interrupt_req (x) = csr_regfile.software_interrupt_req (x);
   method Action  timer_interrupt_req    (x) = csr_regfile.timer_interrupt_req    (x);

   // ----------------
   // Non-maskable interrupt

   method Action  nmi_req (x);
      csr_regfile.nmi_req (x);
   endmethod

   // ----------------
   // Optional PC trace interface
   // Outputs a stream of 2-tuples: (cycle_count, pc)

`ifdef INCLUDE_PC_TRACE
   interface Get  g_pc_trace = toGet (f_pc_trace);
`endif

   // ----------------
   // Optional interface to Tandem Verifier

`ifdef INCLUDE_TANDEM_VERIF
   interface Get  trace_data_out = toGet (f_trace_data);
`endif

   // ----------------
   // Optional interface to Debug Module

`ifdef INCLUDE_GDB_CONTROL
   interface CPU_DM_Ifc debug;
      // Reset
      interface Server  hart_reset_server = toGPServer (f_reset_reqs, f_reset_rsps);
      // run-control, other
      interface Server  hart_server_run_halt = toGPServer (f_run_halt_reqs, f_run_halt_rsps);

      interface Put  hart_put_other_req;
         method Action  put (Bit #(4) req);
            cfg_verbosity <= req;
         endmethod
      endinterface

      // GPR access
      interface Server  hart_gpr_mem_server = toGPServer (f_gpr_reqs, f_gpr_rsps);

`ifdef ISA_F
      // FPR access
      interface Server  hart_fpr_mem_server = toGPServer (f_fpr_reqs, f_fpr_rsps);
`endif

      // CSR access
      interface Server  hart_csr_mem_server = toGPServer (f_csr_reqs, f_csr_rsps);
   endinterface
`else
   // Reset
   interface Server  hart_reset_server = toGPServer (f_reset_reqs, f_reset_rsps);
`endif

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // Debugging: set core's verbosity

   method Action  set_verbosity (Bit #(4)  verbosity, Bit #(64)  logdelay);
      cfg_verbosity <= verbosity;
      cfg_logdelay  <= logdelay;
   endmethod

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
      near_mem.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Bit #(64) mv_tohost_value = near_mem.mv_tohost_value;
`endif

`ifndef Near_Mem_TCM
   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;
      near_mem.ma_ddr4_ready;
   endmethod

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status;
      return near_mem.mv_status;
   endmethod
`endif

endmodule: mkCPU

// ================================================================

endpackage
