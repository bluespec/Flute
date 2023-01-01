// Copyright (c) 2017-2023 Bluespec, Inc. All Rights Reserved.

package DM_Abstract_Commands;

// ================================================================
// This package implements the 'Abstract Command' part of the RISC-V
// Debug Module, i.e., read/write access to RISC-V GPRs, FPRs and CSRs.

// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// Other library imports

import GetPut_Aux :: *;
import Semi_FIFOF :: *;
import Cur_Cycle  :: *;

// ================================================================

import ISA_Decls      :: *;
import DM_Common      :: *;
import DM_CPU_Req_Rsp :: *;

// ================================================================
// Interface

interface DM_Abstract_Commands_IFC;
   method Action reset;

   // ----------------
   // DMI facing GDB/host
   method ActionValue #(DM_Word) av_read  (DM_Addr dm_addr);
   method Action  write (DM_Addr dm_addr, DM_Word dm_word);

   // ----------------
   // Facing CPU/hart
   interface Client #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) hart0_gpr_mem_client;
`ifdef ISA_F
   interface Client #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(FLEN)) hart0_fpr_mem_client;
`endif
   interface Client #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN)) hart0_csr_mem_client;
endinterface

// ================================================================

(* synthesize *)
module mkDM_Abstract_Commands (DM_Abstract_Commands_IFC);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   // ----------------------------------------------------------------

   Reg #(Bool) rg_start_reg_access <- mkReg (False);

   // FIFOs for request/response to access GPRs
   FIFOF #(DM_CPU_Req #(5,  XLEN)) f_hart0_gpr_reqs <- mkFIFOF;
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_hart0_gpr_rsps <- mkFIFOF;

   // FIFOs for request/response to access FPRs
`ifdef ISA_F
   FIFOF #(DM_CPU_Req #(5,  FLEN)) f_hart0_fpr_reqs <- mkFIFOF;
   FIFOF #(DM_CPU_Rsp #(FLEN))     f_hart0_fpr_rsps <- mkFIFOF;
`endif

   // FIFOs for request/response to access CSRs
   FIFOF #(DM_CPU_Req #(12, XLEN)) f_hart0_csr_reqs <- mkFIFOF;
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_hart0_csr_rsps <- mkFIFOF;

   // ----------------------------------------------------------------
   // rg_data0, rg_data1, rg_data2, rg_data3
   // Bluespec's RISCV_gdbstub only accesses up to rg_data1
   // OpenOCD seems to access up to rg_data3

   Reg #(DM_Word) rg_data0 <- mkRegU;
   Reg #(DM_Word) rg_data1 <- mkRegU;
   Reg #(DM_Word) rg_data2 <- mkRegU;
   Reg #(DM_Word) rg_data3 <- mkRegU;

   // ----------------------------------------------------------------
   // rg_data4..11:    not implemented
   // rg_abstractauto: not implemented
   // rg_progbuf0..15: not implemented
   // ----------------------------------------------------------------
   // rg_abstractcs

   Reg #(Bool)                 rg_abstractcs_busy   <- mkRegU;
   Reg #(DM_abstractcs_cmderr) rg_abstractcs_cmderr <- mkRegU;

   // Size of program buffer, in 32b words
   Bit #(5) abstractcs_progbufsize = 0;
   // Number of data registers implemented (rg_data0, rg_data1, rg_data2, rg_data3)
   Bit #(4) abstractcs_datacount = 4;

   DM_Word virt_rg_abstractcs = {3'b0,
				 abstractcs_progbufsize,
				 11'b0,
				 pack (rg_abstractcs_busy),
				 1'b0,
				 pack (rg_abstractcs_cmderr),
				 4'b0,
				 abstractcs_datacount};

   function Action fa_rg_abstractcs_write (DM_Word dm_word);
      action
	 if (rg_abstractcs_busy) begin
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_BUSY;
	    $display ("ERROR: Debug Module: Abstract Command write: [abstractcs] <= 0x%08h:",
		      dm_word);
	    $display ("    Debug Module is busy with a previous abstract command");
	 end
	 else if (fn_abstractcs_cmderr (dm_word) != DM_ABSTRACTCS_CMDERR_NONE) begin
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NONE;
	    if (verbosity != 0)
	       $display ("Debug Module: Abstract Commands write [abstractcs]: clearing cmderr");
	 end
	 else begin
	    if (verbosity != 0)
	       $display ("Debug Module: Abstract Commands write [abstractcs]: cmderr unchanged");
	 end
	 fa_debug_show_location (verbosity);
	 if (verbosity != 0) $display ("fa_rg_abstractcs_write");
      endaction
   endfunction

   // ----------------------------------------------------------------
   // rg_command
   // cmdtype  no register, since we only support 'access reg'
   // size     no register, since we only support 'lower 32b' for RV32
   //          and 'lower 64b' for RV64
   // postexec no register, since we don't support Program Buffer
   // transfer no register, since we always do transfers

   Reg #(Bool) rg_command_access_reg_write <- mkRegU;

   // regno: we only implement lower 13 bits of this 16-bit field
   Reg #(Bit #(13)) rg_command_access_reg_regno <- mkRegU;

   DM_Word virt_rg_command = fn_mk_command_access_reg (
        DM_COMMAND_ACCESS_SIZE_LOWER32
      , False     // postexec
      , True      // transfer
      , rg_command_access_reg_write
      , zeroExtend (rg_command_access_reg_regno));

   function Action fa_rg_command_write (DM_Word dm_word);
      action
	 // TODO: check that CPU is halted, else set cmderr = DM_ABSTRACTCS_CMDERR_HALT_RESUME

	 DM_abstractcs_cmderr cmderr = rg_abstractcs_cmderr;
	 let size = fn_command_access_size (dm_word);

	 // Ignore if 'cmderr' is non-zero
	 if (cmderr != DM_ABSTRACTCS_CMDERR_NONE) begin
	    $display ("ERROR: Debug Module: Abstract Command write: [command] <= 0x%08h",
		      dm_word);
	    $display ("    Ignoring since 'cmderr' is 0x%0h", cmderr);
	 end
	 else begin
	    if (rg_abstractcs_busy) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_BUSY;
	       $display ("ERROR: Debug Module: Abstract Command write: [command] <= 0x%08h",
			 dm_word);
	       $display ("    DM is busy with a previous abstract command");
	    end

	    // Only 'Access Reg' cmdtype is supported
	    else if (fn_command_cmdtype (dm_word) != DM_COMMAND_CMDTYPE_ACCESS_REG) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("ERROR: Debug Module: Abstract Command write: [command] <= 0x%08h",
			 dm_word);
	       $display ("    ", fshow (fn_command_cmdtype (dm_word)), " not supported");
	    end

`ifdef RV32
	    // Only lower 32-bit access is supported
	    else if (size != DM_COMMAND_ACCESS_SIZE_LOWER32) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("ERROR: Debug Module: Abstract Command write: [command] <= 0x%08h",
			 dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, ",
			 fshow (fn_command_access_size (dm_word)),
			 " not supported in RV32 mode");
	    end
`endif
`ifdef RV64
	    // Only lower 32-bit and 64-bit access is supported
	    else if (size != DM_COMMAND_ACCESS_SIZE_LOWER64)
	       begin
		  cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
		  $display ("ERROR: Debug Module: Abstract Command write: [command] <= 0x%08h",
			    dm_word);
		  $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, ",
			    fshow (fn_command_access_size (dm_word)),
			    " not supported in RV64 mode");
	       end
`endif

	    // 'postexec' is not supported
	    else if (fn_command_access_reg_postexec (dm_word) == True) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("ERROR: Debug Module: Abstract Command write: [command] <= 0x%08h",
			 dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, postexec not supported");
	    end

	    // non-'transfer' is not supported
	    else if (fn_command_access_reg_transfer (dm_word) == False) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("ERROR: Debug Module: Abstract Command write: [command] <= 0x%08h",
			 dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, no-transfer not supported");
	    end

	    else begin
	       Bool      is_write = fn_command_access_reg_write (dm_word);
	       Bit #(13) regno    = truncate (fn_command_access_reg_regno (dm_word));

	       rg_command_access_reg_write <= is_write;
	       rg_command_access_reg_regno <= regno;
	       rg_abstractcs_busy          <= True;
	       rg_start_reg_access         <= True;
	       cmderr = DM_ABSTRACTCS_CMDERR_NONE;
               if (verbosity != 0)
                  $display ("Debug Module: Abstract Command write: [command] <= 0x%08h: OKAY",
			    dm_word);
	    end
	    rg_abstractcs_cmderr <= cmderr;
	 end
	 fa_debug_show_location (verbosity);
	 if (verbosity != 0) $display ("fa_rg_command_write");
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Register reads and writes

   Bool is_csr
   = (   (fromInteger (dm_command_access_reg_regno_csr_0) <= rg_command_access_reg_regno)
      && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_csr_FFF)));

   Bool is_gpr
   = (   (fromInteger (dm_command_access_reg_regno_gpr_0) <= rg_command_access_reg_regno)
      && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_gpr_1F)));

`ifdef ISA_F
   Bool is_fpr
   = (   (fromInteger (dm_command_access_reg_regno_fpr_0) <= rg_command_access_reg_regno)
      && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_fpr_1F)));
`else
   Bool is_fpr = False;
`endif

   Bit #(12) csr_addr = truncate (rg_command_access_reg_regno
				  - fromInteger (dm_command_access_reg_regno_csr_0));

   Bit #(5)  gpr_addr = truncate (rg_command_access_reg_regno
				  - fromInteger (dm_command_access_reg_regno_gpr_0));

   Bit #(5)  fpr_addr = truncate (rg_command_access_reg_regno
				  - fromInteger (dm_command_access_reg_regno_fpr_0));

   // ----------------------------------------------------------------
   // Write CSR

   rule rl_csr_write_start (   rg_abstractcs_busy
			    && rg_start_reg_access
			    && rg_command_access_reg_write
			    && is_csr);
      let req = DM_CPU_Req {write:   True,
			    address: csr_addr,
`ifdef RV32
			    data:    rg_data0
`endif
`ifdef RV64
			    data:    {rg_data1, rg_data0}
`endif
			    };
      f_hart0_csr_reqs.enq (req);
      rg_start_reg_access <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: csr write start: ", fshow (req));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_csr_write_start");
   endrule

   // ----------------

   rule rl_csr_write_finish (rg_abstractcs_busy
			     && rg_command_access_reg_write
			     && is_csr);
      let rsp <- pop (f_hart0_csr_rsps);
      rg_abstractcs_cmderr <= (rsp.ok
			       ? DM_ABSTRACTCS_CMDERR_NONE
			       : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
      rg_abstractcs_busy   <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: csr write finish: ", fshow (rsp));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_csr_write_finish");
   endrule

   // ----------------------------------------------------------------
   // Read CSR

   rule rl_csr_read_start (   rg_abstractcs_busy
			   && rg_start_reg_access
			   && (! rg_command_access_reg_write)
			   && is_csr);
      Bit #(XLEN) data = ?;
      let req = DM_CPU_Req {write: False, address: csr_addr, data: data};
      f_hart0_csr_reqs.enq (req);
      rg_start_reg_access <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: csr read start: ", fshow (req));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_csr_read_start");
   endrule

   // ----------------

   rule rl_csr_read_finish (   rg_abstractcs_busy
			    && (! rg_command_access_reg_write)
			    && is_csr);
      let rsp <- pop (f_hart0_csr_rsps);
      rg_abstractcs_cmderr <= (rsp.ok
			       ? DM_ABSTRACTCS_CMDERR_NONE
			       : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
`ifdef RV32
      rg_data0 <= rsp.data;
`endif
`ifdef RV64
      rg_data0 <= truncate (rsp.data);
      rg_data1 <= rsp.data [63:32];
`endif
      rg_abstractcs_busy <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: CSR read finish: ", fshow (rsp));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_csr_read_finish");
   endrule

   // ----------------------------------------------------------------
   // Write GPR

   rule rl_gpr_write_start (   rg_abstractcs_busy
			    && rg_start_reg_access
			    && rg_command_access_reg_write
			    && is_gpr);
      let req = DM_CPU_Req {write:   True,
			    address: gpr_addr,
`ifdef RV32
			    data:    rg_data0
`endif
`ifdef RV64
			    data:    {rg_data1, rg_data0}
`endif
			    };
      f_hart0_gpr_reqs.enq (req);
      rg_start_reg_access <= False;
      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: GPR write start: ", fshow (req));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_gpr_write_start");
   endrule

   // ----------------

   rule rl_gpr_write_finish (   rg_abstractcs_busy
			     && rg_command_access_reg_write
			     && is_gpr);
      let rsp <- pop (f_hart0_gpr_rsps);
      rg_abstractcs_cmderr <= (rsp.ok
			       ? DM_ABSTRACTCS_CMDERR_NONE
			       : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
      rg_abstractcs_busy   <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: GPR write finish: ", fshow (rsp));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_gpr_write_finish");
   endrule

   // ----------------------------------------------------------------
   // Read GPR

   rule rl_gpr_read_start (   rg_abstractcs_busy
			   && rg_start_reg_access
			   && (! rg_command_access_reg_write)
			   && is_gpr);
      Bit #(XLEN) data = ?;
      let req = DM_CPU_Req {write: False, address: gpr_addr, data: data };
      f_hart0_gpr_reqs.enq (req);
      rg_start_reg_access <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: GPR read start: ", fshow (req));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_gpr_read_start");
   endrule

   // ----------------

   rule rl_gpr_read_finish (   rg_abstractcs_busy
			    && (! rg_command_access_reg_write)
			    && is_gpr);
      let rsp <- pop (f_hart0_gpr_rsps);
`ifdef RV32
      rg_data0 <= rsp.data;
`endif
`ifdef RV64
      rg_data0 <= truncate (rsp.data);
      rg_data1 <= rsp.data [63:32];
`endif
      rg_abstractcs_cmderr <= (rsp.ok
			       ? DM_ABSTRACTCS_CMDERR_NONE
			       : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
      rg_abstractcs_busy <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: GPR read finish: ", fshow (rsp));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_gpr_read_finish");
   endrule

   // ----------------------------------------------------------------
   // Write FPR

`ifdef ISA_F

   rule rl_fpr_write_start (   rg_abstractcs_busy
			    && rg_start_reg_access
			    && rg_command_access_reg_write
			    && is_fpr);
      let req = DM_CPU_Req {write:   True,
			    address: fpr_addr,
`ifdef ISA_D
			    data:    {rg_data1, rg_data0}
`else
			    data:    rg_data0
`endif
			    };
      f_hart0_fpr_reqs.enq (req);
      rg_start_reg_access <= False;
      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: FPR write start: ", fshow (req));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_fpr_write_start");
   endrule

   // ----------------

   rule rl_fpr_write_finish (   rg_abstractcs_busy
			     && rg_command_access_reg_write
			     && is_fpr);
      let rsp <- pop (f_hart0_fpr_rsps);
      rg_abstractcs_cmderr <= (rsp.ok
			       ? DM_ABSTRACTCS_CMDERR_NONE
			       : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
      rg_abstractcs_busy   <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: FPR write finish: ", fshow (rsp));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_fpr_write_finish");
   endrule

   // ----------------------------------------------------------------
   // Read FPR

   rule rl_fpr_read_start (   rg_abstractcs_busy
			   && rg_start_reg_access
			   && (! rg_command_access_reg_write)
			   && is_fpr);
      Bit #(FLEN) data = ?;
      let req = DM_CPU_Req {write: False, address: fpr_addr, data: data };
      f_hart0_fpr_reqs.enq (req);
      rg_start_reg_access <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: FPR read start: ", fshow (req));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_fpr_read_start");
   endrule

   // ----------------

   rule rl_fpr_read_finish (   rg_abstractcs_busy
			    && (! rg_command_access_reg_write)
			    && is_fpr);
      let rsp <- pop (f_hart0_fpr_rsps);

`ifdef ISA_D
      rg_data0 <= truncate (rsp.data);
      rg_data1 <= rsp.data [63:32];
`else
      rg_data0 <= rsp.data;
`endif
      rg_abstractcs_cmderr <= (rsp.ok
			       ? DM_ABSTRACTCS_CMDERR_NONE
			       : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
      rg_abstractcs_busy <= False;

      if (verbosity != 0)
	 $display ("Debug Module: Abstract Command: FPR read finish: ", fshow (rsp));
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_fpr_read_finish");
   endrule

`endif

   // ----------------------------------------------------------------
   // Read/Write unknown address

   rule rl_unknown_write_start (   rg_abstractcs_busy
				&& rg_start_reg_access
				&& rg_command_access_reg_write
				&& (! is_csr) && (! is_gpr) && (! is_fpr));
      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_OTHER;
      rg_start_reg_access  <= False;
      rg_abstractcs_busy   <= False;

      $display ("ERROR: Debug Module: Abstract Command Write: unknown regno [0x%0h] <= 0x%08h",
		rg_command_access_reg_regno, rg_data0);
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_unknown_write_start");
   endrule

   rule rl_unknown_read_start (   rg_abstractcs_busy
			       && rg_start_reg_access
			       && (! rg_command_access_reg_write)
			       && (! is_csr) && (! is_gpr) && (! is_fpr));
      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_OTHER;
      rg_start_reg_access  <= False;
      rg_abstractcs_busy   <= False;

      $display ("ERROR: Debug Module: Abstract Command READ: unknown regno [0x%0h]",
		rg_command_access_reg_regno);
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("rl_unknown_read_start");
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      rg_start_reg_access <= False;

      f_hart0_gpr_reqs.clear;
      f_hart0_gpr_rsps.clear;
      f_hart0_csr_reqs.clear;
      f_hart0_csr_rsps.clear;

      rg_abstractcs_busy   <= False;
      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NONE;

      rg_command_access_reg_write <= False;
      rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_gpr_0);

      rg_data0 <= 0;
      rg_data1 <= 0;
      rg_data2 <= 0;
      rg_data3 <= 0;

      if (verbosity != 0)
	 $display ("Debug Module: debug module reset");
      fa_debug_show_location (verbosity);
      if (verbosity != 0) $display ("ma_reset");
   endmethod

   // ----------------
   // Facing DMI/GDB

   method ActionValue #(DM_Word) av_read (DM_Addr dm_addr);
      actionvalue
	 let dm_addr_name = fshow_dm_addr (dm_addr);
	 DM_Word dm_word = case (dm_addr)
			      dm_addr_abstractcs:   virt_rg_abstractcs;
			      dm_addr_command:      virt_rg_command;
			      dm_addr_data0:        rg_data0;
			      dm_addr_data1:        rg_data1;
			      dm_addr_data2:        rg_data2;
			      dm_addr_data3:        rg_data3;

			      // dm_addr_data4..12
			      // dm_addr_abstractauto
			      // dm_addr_progbuf0..15
			   endcase;
	 if (verbosity != 0)
	    $display ("Debug Module: Abstract Command read: [", dm_addr_name, "]",
		      " => 0x%08h", dm_word);
	 fa_debug_show_location (verbosity);
	 if (verbosity != 0) $display ("mav_read");
	 return dm_word;
      endactionvalue
   endmethod

   method Action write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 let dm_addr_name = fshow_dm_addr (dm_addr);
	 if (verbosity != 0)
	    $display ("Debug Module: Abstract Command write: [", dm_addr_name, "]",
		      " <= 0x%08h", dm_word);
	 fa_debug_show_location (verbosity);
	 if (verbosity != 0) $display ("ma_write");

	 if (dm_addr == dm_addr_abstractcs)
	    fa_rg_abstractcs_write (dm_word);

	 else if (rg_abstractcs_cmderr != DM_ABSTRACTCS_CMDERR_NONE) begin
	    $display ("ERROR: Debug Module: Abstract Command write: [", dm_addr_name, "]",
		      " <= 0x%08h", dm_word);
	    $display ("    Ignoring due to previous cmderr ",
		      fshow (rg_abstractcs_cmderr));
	    fa_debug_show_location (verbosity);
	    if (verbosity != 0) $display ("ma_write");
	 end

	 else if (dm_addr == dm_addr_command) fa_rg_command_write (dm_word);
	 else if (dm_addr == dm_addr_data0)   rg_data0 <= dm_word;
	 else if (dm_addr == dm_addr_data1)   rg_data1 <= dm_word;
	 else if (dm_addr == dm_addr_data2)   rg_data2 <= dm_word;
	 else if (dm_addr == dm_addr_data3)   rg_data3 <= dm_word;
	 else begin
	    // dm_addr_data4..12
	    // dm_addr_abstractauto
	    // dm_addr_progbuf0..15
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;

	    $display ("Debug Module: DM_Abstract_Commands.write: [", dm_addr_name,
		      "] <= 0x%08h: ERROR: not supported", dm_word);
	 end
      endaction
   endmethod

   // ----------------
   // Facing CPU/hart
      interface Client hart0_gpr_mem_client = toGPClient (f_hart0_gpr_reqs, f_hart0_gpr_rsps);
`ifdef ISA_F
      interface Client hart0_fpr_mem_client = toGPClient (f_hart0_fpr_reqs, f_hart0_fpr_rsps);
`endif
      interface Client hart0_csr_mem_client = toGPClient (f_hart0_csr_reqs, f_hart0_csr_rsps);
endmodule

// ================================================================

endpackage
