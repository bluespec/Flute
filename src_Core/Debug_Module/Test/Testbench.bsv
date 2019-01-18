package Testbench;

// ================================================================
// Testbench for basic sanity-check testing of the Debug Module.


// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import StmtFSM      :: *;

// ----------------
// Other library imports

import Semi_FIFOF :: *;

// ================================================================

import ISA_Decls :: *;
import TRX       :: *;

import Debug_Module :: *;

// ================================================================

Integer csr_addr_dcsr      = 'h7b0;
Integer csr_addr_dpc       = 'h7b1;
Integer csr_addr_dscratch0 = 'h7b2;
Integer csr_addr_dscratch1 = 'h7b3;

// ================================================================

(* synthesize *)
module mkTestbench (Empty);

   // ================================================================
   // Cycle-counter and cycle-limit termination

   Reg #(Bit #(32)) rg_cycle <- mkReg (0);

   Integer cycle_limit = 100;

   rule rl_count_cycles;
      if (rg_cycle == fromInteger (100)) begin
	 $display ("Testench: stopping at cycle %0d", cycle_limit);
	 $finish (0);
      end
      rg_cycle <= rg_cycle +1;
   endrule

   // ================================================================
   // The Debug Module

   Debug_Module_IFC dm <- mkDebug_Module;

   // ================================================================
   // Model of a hart, and connections to dm

   Hart_DM_IFC hart0 <- mkHart_Model (0);

   // Reset
   mkConnection (dm.hart0_reset_req,   hart0.hart_reset_req);

   // Run-control
   mkConnection (dm.hart0_run_req_rsp, hart0.hart_run_req_rsp);

   // GPR access
   mkConnection (dm.master_for_gprs, hart0.slave_for_gprs);

   // CSR access
   mkConnection (dm.master_for_csrs, hart0.slave_for_csrs);

   // ================================================================
   // Over-simplified model of platform reset (all except DM)

   rule rl_ndm_reset;
      let x <- dm.ndm_reset_req.get;
      $display ("Testbench.rl_ndm_reset: Resetting all platform except Debug Module");
   endrule

   // ================================================================
   // Over-simplified model of system (RISC-V) memory
   // On reads, return addr + 2.

   rule rl_mem_read;
      let rda <- pop_o (dm.master.fo_rda);
      let data = rda.addr + 2;    // Bogus data, for now
      let rdr = TRX_RdR {trans_id: rda.trans_id,
			 status  : TRX_OKAY,
			 data    : data};
      dm.master.fi_rdr.enq (rdr);
      $display ("Testbench: memory read [0x%08h] => 0x%08h", rda.addr, data);
   endrule

   rule rl_mem_write;
      let wra <- pop_o (dm.master.fo_wra);
      let wrd <- pop_o (dm.master.fo_wrd);
      let wrr = TRX_WrR {trans_id: wra.trans_id,
			 status  : TRX_OKAY};
      dm.master.fi_wrr.enq (wrr);
      $display ("Testbench: memory write [0x%08h] <= 0x%08h", wra.addr, wrd.data);
   endrule

   // ================================================================
   // Abstract command sequences (read/write GPR/CSR)

   Reg #(Bit #(32)) rg_abstractcs <- mkRegU;

   // Read a register
   function Stmt fn_stmt_read_reg (Bit #(16) regno);
      return
      seq
	 $display ("----------------\nRead RISC-V reg");
	 // Clear any prior error status
	 dm.write (dm_addr_abstractcs, fn_mk_abstractcs (dm_cmderr_w1c));
	 // Perform the read
	 dm.write (dm_addr_command,
		   fn_mk_command_access_reg (DM_COMMAND_ACCESS_REG_SIZE_LOWER32,
					     False,                                   // postexec
					     True,                                    // transfer
					     False,                                   // write
					     regno));
	 // Read status to check no error
	 action
	    let x <- dm.av_read (dm_addr_abstractcs);
	    rg_abstractcs <= x;
	 endaction
	 while (fn_abstractcs_busy (rg_abstractcs)) seq
	    $display ("Testbench: read reg: busy");
	    action
	       let x <- dm.av_read (dm_addr_abstractcs);
	       rg_abstractcs <= x;
	    endaction
	 endseq
	 if (fn_abstractcs_cmderr (rg_abstractcs) != DM_ABSTRACTCS_CMDERR_NONE)
	    $display ("Testbench: read reg => ", fshow (fn_abstractcs_cmderr (rg_abstractcs)));
	 else action
	    let x <- dm.av_read (dm_addr_data0);
	    $display ("Testbench: read reg => 0x%08h", x);
	 endaction
      endseq;
   endfunction

   // Write a register
   function Stmt fn_stmt_write_reg (Bit #(16) regno, Bit #(32) data);
      return
      seq
	 $display ("----------------\nWrite RISC-V reg");
	 // Clear any prior error status
	 dm.write (dm_addr_abstractcs, fn_mk_abstractcs (dm_cmderr_w1c));
	 // Write data0
	 dm.write (dm_addr_data0, data);
	 // Perform the write
	 dm.write (dm_addr_command,
		   fn_mk_command_access_reg (DM_COMMAND_ACCESS_REG_SIZE_LOWER32,
					     False,                                   // postexec
					     True,                                    // transfer
					     True,                                    // write
					     regno));
	 // Read status to check no error
	 action
	    let x <- dm.av_read (dm_addr_abstractcs);
	    rg_abstractcs <= x;
	 endaction
	 while (fn_abstractcs_busy (rg_abstractcs)) seq
	    $display ("Testbench: write reg: busy");
	    action
	       let x <- dm.av_read (dm_addr_abstractcs);
	       rg_abstractcs <= x;
	    endaction
	 endseq
	 $display ("Testbench: write reg => ", fshow (fn_abstractcs_cmderr (rg_abstractcs)));
      endseq;
   endfunction

   // ================================================================
   // System Bus access sequences (read/write RISC-V memory)

   Reg #(Bool)      rg_busy <- mkRegU;

   Reg #(Bit #(32)) rg_j    <- mkRegU;
   Reg #(Bit #(32)) rg_addr <- mkRegU;
   Reg #(Bit #(32)) rg_data <- mkRegU;

   Stmt stmt_wait_for_sb_nonbusy = (
      seq
	 rg_busy <= True;
	 while (rg_busy) seq
	    delay (1);
	    action
	       let x <- dm.av_read (dm_addr_sbcs);
	       let sberror = fn_sbcs_sberror (x);
	       rg_busy <= (sberror == DM_SBERROR_BUSY_STALE);
	       if (   (sberror != DM_SBERROR_NONE)
		  && (sberror != DM_SBERROR_BUSY_STALE))
		  begin
		     $display ("Testbench: stmt_wait_for_sb_nonbusy: ", fshow (sberror));
		     $finish (1);
		  end
	    endaction
	 endseq
      endseq);

   // Do a single-read from memory
   Stmt stmt_mem_read_1 = (
      seq
	 dm.write (dm_addr_sbaddress0, 'h1_0000);
	 dm.write (dm_addr_sbcs, fn_mk_sbcs (True,                     // sbsingleread
					     DM_SBACCESS_32_BIT,
					     False,                    // sbautoincrement
					     False,                    // sbautoread
					     DM_SBERROR_UNDEF7_W1C));  // clear sberror
	 stmt_wait_for_sb_nonbusy;
	 action
	    let x <- dm.av_read (dm_addr_sbdata0);
	    $display ("stmt_mem_read_1: read-data = 0x%08h", x);
	 endaction
      endseq);

   // Do a multiple-read from memory
   Stmt stmt_mem_read_4 = (
      seq
	 dm.write (dm_addr_sbaddress0, 'h1_0000);
	 dm.write (dm_addr_sbcs, fn_mk_sbcs (True,                     // sbsingleread
					     DM_SBACCESS_32_BIT,
					     True,                     // sbautoincrement
					     True,                     // sbautoread
					     DM_SBERROR_UNDEF7_W1C));  // clear sberror
	 for (rg_j <= 0; rg_j < 3; rg_j <= rg_j + 1) seq
	    stmt_wait_for_sb_nonbusy;
	    action
	       let x <- dm.av_read (dm_addr_sbdata0);
	       $display ("stmt_mem_read_4: read-data [%0d] = 0x%08h", rg_j, x);
	    endaction
	 endseq
	 dm.write (dm_addr_sbcs, fn_mk_sbcs (False,                    // sbsingleread
					     DM_SBACCESS_32_BIT,
					     False,                    // sbautoincrement
					     False,                    // sbautoread
					     DM_SBERROR_UNDEF7_W1C));  // clear sberror
	 stmt_wait_for_sb_nonbusy;
	 action
	    let x <- dm.av_read (dm_addr_sbdata0);
	    $display ("stmt_mem_read_4: read-data [%0d] = 0x%08h", rg_j, x);
	 endaction
      endseq);

   // Do a single-write to memory
   Stmt stmt_mem_write_1 = (
      seq
	 dm.write (dm_addr_sbcs, fn_mk_sbcs (False,                    // sbsingleread
					     DM_SBACCESS_32_BIT,
					     False,                    // sbautoincrement
					     False,                    // sbautoread
					     DM_SBERROR_UNDEF7_W1C));  // clear sberror
	 stmt_wait_for_sb_nonbusy;
	 dm.write (dm_addr_sbaddress0, 'h1_0000);
	 dm.write (dm_addr_sbdata0,    'h_BEEF);
      endseq);

   // Do a multiple-write to memory
   Stmt stmt_mem_write_4 = (
      seq
	 dm.write (dm_addr_sbcs, fn_mk_sbcs (False,                    // sbsingleread
					     DM_SBACCESS_32_BIT,
					     True,                     // sbautoincrement
					     False,                    // sbautoread
					     DM_SBERROR_UNDEF7_W1C));  // clear sberror
	 stmt_wait_for_sb_nonbusy;
	 action
	    rg_addr <= 'h_2000;
	    rg_data <= 'h_DAFA_0000;
	 endaction
	 dm.write (dm_addr_sbaddress0, rg_addr);
	 for (rg_j <= 0; rg_j < 4; rg_j <= rg_j + 1) seq
	    stmt_wait_for_sb_nonbusy;
	    action
	       $display ("stmt_mem_write_4: [0x%08h] x = 0x%08h", rg_addr + rg_j, rg_data);
	       dm.write (dm_addr_sbdata0, rg_data);
	       rg_data <= rg_data + 1;
	    endaction
	 endseq
      endseq);

   // ================================================================
   // Run-control test sequences (reset, run, halt, single-step)

   let dmcontrol_dm_reset
   = fn_mk_dmcontrol (False,   // haltreq
		      False,   // resumereq
		      False,   // hartreset
		      False,   // hasel
		      0,       // hartsel,
		      False,   // ndmreset
		      False);  // dmactive; assert reset

   let dmcontrol_ndmreset
   = fn_mk_dmcontrol (False,   // haltreq
		      False,   // resumereq
		      False,   // hartreset
		      False,   // hasel,
		      0,       // hartsel
		      True,    // ndmreset
		      True);   // dmactive

   let dmcontrol_err_hasel
   = fn_mk_dmcontrol (False,   // haltreq
		      False,   // resumereq
		      False,   // hartreset
		      True,    // hasel,
		      0,       // hartsel
		      False,   // ndmreset
		      True);   // dmactive

   let dmcontrol_err_hartsel
   = fn_mk_dmcontrol (False,       // haltreq
		      False,       // resumereq
		      False,   // hartreset
		      False,   // hasel,
		      3,       // hartsel
		      False,   // ndmreset
		      True);   // dmactive

   let dmcontrol_hartreset
   = fn_mk_dmcontrol (False,       // haltreq
		      False,       // resumereq
		      True,    // hartreset
		      False,   // hasel,
		      0,       // hartsel
		      False,   // ndmreset
		      True);   // dmactive

   let dmcontrol_err_haltreq_resumereq
   = fn_mk_dmcontrol (True,    // haltreq
		      True,    // resumereq
		      False,   // hartreset
		      False,   // hasel,
		      0,       // hartsel
		      False,   // ndmreset
		      True);   // dmactive

   let dmcontrol_haltreq
   = fn_mk_dmcontrol (True,    // haltreq
		      False,   // resumereq
		      False,   // hartreset
		      False,   // hasel,
		      0,       // hartsel
		      False,   // ndmreset
		      True);   // dmactive

   let dmcontrol_resumereq
   = fn_mk_dmcontrol (False,   // haltreq
		      True,    // resumereq
		      False,   // hartreset
		      False,   // hasel,
		      0,       // hartsel
		      False,   // ndmreset
		      True);   // dmactive

   function Stmt fn_stmt_run_control (DM_Word dm_word);
      return seq
		dm.write (dm_addr_dmcontrol, dm_word);
		delay (5);
		// Check and show status
		action
		   let x <- dm.av_read (dm_addr_dmstatus);
		   $display ("  ", fshow_dmstatus (x));
		endaction
	     endseq;
   endfunction

   // ----------------
   // For single-step, set 'step' bit in DCSR, then run

   let dcsr_step = {4'h4,    // xdebugver
		    12'b0,
		    1'b0,    // ebreakm
		    1'b0,
		    1'b0,    // ebreaks
		    1'b0,    // ebreaku
		    1'b0,    // stepie
		    1'b0,    // stepcount
		    1'b0,    // steptime
		    3'b0,    // cause
		    3'b0,
		    1'b1,    // step
		    2'h3};

   Stmt stmt_single_step = (
      seq
	 // set 'step' in dcsr
	 fn_stmt_write_reg (fromInteger (dm_command_access_reg_regno_csr_0 + csr_addr_dcsr),
			    dcsr_step);  // priv
	 fn_stmt_run_control (dmcontrol_resumereq);
      endseq);

   // ================================================================
   // Top-level test.  Comment/Uncomment desired parts.

   Stmt test = seq
		  // Reset DM
		  $display ("----------------\n'Testbench: Reset DM'");
		  fn_stmt_run_control (dmcontrol_dm_reset);

		  /*
		  $display ("----------------\n'Testbench: Reset Platform'");
		  fn_stmt_run_control (dmcontrol_ndmreset);

		  $display ("----------------\n'Testbench: Err hasel'");
		  fn_stmt_run_control (dmcontrol_err_hasel);

		  $display ("----------------\n'Testbench: Err hartsel'");
		  fn_stmt_run_control (dmcontrol_err_hartsel);

		  $display ("----------------\n'Testbench: Reset hart'");
		  fn_stmt_run_control (dmcontrol_hartreset);

		  $display ("----------------\n'Testbench: Err haltreq and resumereq'");
		  fn_stmt_run_control (dmcontrol_err_haltreq_resumereq);

		  $display ("----------------\n'Testbench: Continue'");
		  fn_stmt_run_control (dmcontrol_resumereq);

		  $display ("----------------\n'Testbench: Halt'");
		  fn_stmt_run_control (dmcontrol_haltreq);

		  $display ("----------------\n'Testbench: Single step'");
		  stmt_single_step;
		  */

		  $display ("----------------\n'Testbench: Read GPR'");
		  fn_stmt_read_reg (fromInteger (dm_command_access_reg_regno_gpr_0 + 5));
		  $display ("----------------\n'Testbench: Read CSR'");
		  fn_stmt_read_reg (fromInteger (dm_command_access_reg_regno_csr_0 + 3));

		  $display ("----------------\n'Testbench: Write GPR'");
		  fn_stmt_write_reg (fromInteger (dm_command_access_reg_regno_gpr_0 + 5), 'h_AAAA_0005);
		  $display ("----------------\n'Testbench: Write CSR'");
		  fn_stmt_write_reg (fromInteger (dm_command_access_reg_regno_csr_0 + 3), 'h_CCCC_0003);

		  /*
		  $display ("----------------\n'Testbench: Read 1'");
		  stmt_mem_read_1;

		  $display ("----------------\n'Testbench: Write 1'");
		  stmt_mem_write_1;

		  $display ("----------------\n'Testbench: Read 4'");
		  stmt_mem_read_4;

		  $display ("----------------\n'Testbench: Write 4'");
		  stmt_mem_write_4;
		   */

		  await (False);
	       endseq;

   mkAutoFSM (test);

endmodule

// ================================================================
// Over-simplified model of a hart (reset, run/halt, read/write GPR/CSR)

interface Hart_DM_IFC;
   // Reset
   interface Put #(Token)         hart_reset_req;

   // Run-control
   interface Server #(Bool, Bool) hart_run_req_rsp;

   // GPR access
   interface TRX_Slave_IFC #(5,32,0)  slave_for_gprs;

   // CSR access
   interface TRX_Slave_IFC #(12,32,0) slave_for_csrs;
endinterface


(* synthesize *)
module mkHart_Model #(parameter Bit #(10) hart_id) (Hart_DM_IFC);

   Reg #(Bool) rg_hart_running <- mkReg (False);

   FIFOF #(Token) f_hart_reset_reqs <- mkFIFOF;

   FIFOF #(Bool) f_hart_run_reqs <- mkFIFOF;
   FIFOF #(Bool) f_hart_run_rsps <- mkFIFOF;

   // TRX interface to gprs
   TRX_Buffer_IFC #(5,32,0) trx_buf_gprs <- mkTRX_Buffer;

   // TRX interface to crs
   TRX_Buffer_IFC #(12,32,0) trx_buf_csrs <- mkTRX_Buffer;

   // ----------------------------------------------------------------
   // BEHAVIOR

   // ----------------
   // Reset

   rule rl_hart_reset;
      let x = f_hart_reset_reqs.first;
      f_hart_reset_reqs.deq;

      $display ("Testbench.hart [%0d]: reset", hart_id);
   endrule

   // ----------------
   // Run-control

   rule rl_resume_hart (f_hart_run_reqs.first);
      f_hart_run_reqs.deq;
      rg_hart_running <= True;

      if (rg_hart_running)
	 $display ("Testbench.hart [%0d].rl_resume_hart: already running", hart_id);
      else
	 $display ("Testbench.hart [%0d].rl_resume_hart: resuming", hart_id);

      f_hart_run_rsps.enq (True);
   endrule

   rule rl_halt_hart (! f_hart_run_reqs.first);
      f_hart_run_reqs.deq;
      rg_hart_running <= False;

      if (rg_hart_running)
	 $display ("Testbench.hart [%0d].rl_halt_hart: halting", hart_id);
      else
	 $display ("Testbench.hart [%0d].rl_halt_hart: already halted", hart_id);

      f_hart_run_rsps.enq (False);
   endrule

   // ----------------
   // GPR access

   rule rl_read_gpr;
      let rda <- pop_o (trx_buf_gprs.master.fo_rda);
      Bit #(32) data = extend (rda.addr) + 'h1000;
      let rdr = TRX_RdR {trans_id: rda.trans_id,
			 status:   TRX_OKAY,
			 data:     data};
      trx_buf_gprs.master.fi_rdr.enq (rdr);
      $display ("Testbench.hart [%0d]: Read GPR [%0h] => 0x%08h", hart_id, rda.addr, data);
   endrule

   rule rl_read_csr;
      let rda <- pop_o (trx_buf_csrs.master.fo_rda);
      Bit #(32) data = extend (rda.addr) + 'h2000;
      let rdr = TRX_RdR {trans_id: rda.trans_id,
			 status:   TRX_OKAY,
			 data:     data};
      trx_buf_csrs.master.fi_rdr.enq (rdr);
      $display ("Testbench.hart [%0d]: Read CSR [%0h] => 0x%08h", hart_id, rda.addr, data);
   endrule

   rule rl_write_gpr;
      let wra <- pop_o (trx_buf_gprs.master.fo_wra);
      let wrd <- pop_o (trx_buf_gprs.master.fo_wrd);
      let wrr = TRX_WrR {trans_id: wra.trans_id, status:   TRX_OKAY};
      trx_buf_gprs.master.fi_wrr.enq (wrr);
      $display ("Testbench.hart [%0d]: Write GPR [%0h] <= 0x%08h", hart_id, wra.addr, wrd.data);
   endrule

   rule rl_write_csr;
      let wra <- pop_o (trx_buf_csrs.master.fo_wra);
      let wrd <- pop_o (trx_buf_csrs.master.fo_wrd);
      let wrr = TRX_WrR {trans_id: wra.trans_id, status:   TRX_OKAY};
      trx_buf_csrs.master.fi_wrr.enq (wrr);
      $display ("Testbench.hart [%0d]: Write CSR [%0h] <= 0x%08h", hart_id, wra.addr, wrd.data);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   // Reset
   interface Put hart_reset_req = toPut (f_hart_reset_reqs);

   // Run-control
   interface Server hart_run_req_rsp = toGPServer (f_hart_run_reqs, f_hart_run_rsps);

   // GPR access
   interface TRX_Slave_IFC  slave_for_gprs = trx_buf_gprs.slave;

   // CSR access
   interface TRX_Slave_IFC  slave_for_csrs = trx_buf_csrs.slave;
endmodule

// ================================================================

endpackage
