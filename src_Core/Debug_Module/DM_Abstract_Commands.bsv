// Copyright (c) 2017-2019 Bluespec, Inc. All Rights Reserved.

package DM_Abstract_Commands;

// ================================================================
// This package implements the 'Abstract Command' part of the RISC-V
// Debug Module, i.e., read/write access to RISC-V GPRs, FPRs and CSRs.

// ================================================================
// BSV library imports

import Memory       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// Other library imports

import GetPut_Aux :: *;
import Semi_FIFOF :: *;
import Cur_Cycle  :: *;

// ================================================================

import ISA_Decls :: *;
import DM_Common :: *;

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
   interface MemoryClient #(5,  XLEN) hart0_gpr_mem_client;
   interface MemoryClient #(12, XLEN) hart0_csr_mem_client;
endinterface

// ================================================================

(* synthesize *)
module mkDM_Abstract_Commands (DM_Abstract_Commands_IFC);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   // ----------------------------------------------------------------

   Reg #(Bool) rg_start_reg_access <- mkReg (False);

   // FIFOs for request/response to access GPRs
   FIFOF #(MemoryRequest  #(5,  XLEN)) f_hart0_gpr_reqs <- mkFIFOF1;
   FIFOF #(MemoryResponse #(    XLEN)) f_hart0_gpr_rsps <- mkFIFOF1;

   // FIFOs for request/response to access CSRs
   FIFOF #(MemoryRequest  #(12, XLEN)) f_hart0_csr_reqs <- mkFIFOF1;
   FIFOF #(MemoryResponse #(    XLEN)) f_hart0_csr_rsps <- mkFIFOF1;

   // ----------------------------------------------------------------
   // rg_data0

   Reg #(DM_Word ) rg_data0 <- mkRegU;
`ifdef RV64
   Reg #(DM_Word ) rg_data1 <- mkRegU;
`endif

   // ----------------------------------------------------------------
   // rg_data2..11:    not implemented
   // rg_abstractauto: not implemented
   // rg_progbuf0..15: not implemented
   // ----------------------------------------------------------------
   // rg_abstractcs

   Reg #(Bool)                 rg_abstractcs_busy   <- mkRegU;
   Reg #(DM_abstractcs_cmderr) rg_abstractcs_cmderr <- mkRegU;

   Bit #(5) abstractcs_progsize  = 0;
   Bit #(5) abstractcs_datacount = 0;
   DM_Word virt_rg_abstractcs = {3'b0,
				 abstractcs_progsize,
				 11'b0,
				 pack (rg_abstractcs_busy),
				 1'b0,
				 pack (rg_abstractcs_cmderr),
				 3'b0,
				 abstractcs_datacount};

   function Action fa_rg_abstractcs_write (DM_Word dm_word);
      action
	 if (rg_abstractcs_busy) begin
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_BUSY;
	    $display ("(%0d): DM_Abstract_Commands.write: [abstractcs] <= 0x%08h: ERROR", cur_cycle, dm_word);
	    $display ("    DM is busy with a previous abstract command");
	 end
	 else if (fn_abstractcs_cmderr (dm_word) != DM_ABSTRACTCS_CMDERR_NONE) begin
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NONE;
	    if (verbosity != 0)
	       $display ("(%0d): DM_Abstract_Commands.write [abstractcs]: clearing cmderr", cur_cycle);
	 end
	 else begin
	    if (verbosity != 0)
	       $display ("(%0d): DM_Abstract_Commands.write [abstractcs]: cmderr unchanged", cur_cycle);
	 end
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
        DM_COMMAND_ACCESS_REG_SIZE_LOWER32
      , False     // postexec
      , True      // transfer
      , rg_command_access_reg_write
      , zeroExtend (rg_command_access_reg_regno));

   function Action fa_rg_command_write (DM_Word dm_word);
      action
	 // TODO: check that CPU is halted, else set cmderr = DM_ABSTRACTCS_CMDERR_HALT_RESUME

	 DM_abstractcs_cmderr cmderr = rg_abstractcs_cmderr;
	 let size = fn_command_access_reg_size (dm_word);

	 // Ignore if 'cmderr' is non-zero
	 if (cmderr != DM_ABSTRACTCS_CMDERR_NONE) begin
	    $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	    $display ("    Ignoring since 'cmderr' is 0x%0h", cmderr);
	 end
	 else begin
	    if (rg_abstractcs_busy) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_BUSY;
	       $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    DM is busy with a previous abstract command");
	    end

	    // Only 'Access Reg' cmdtype is supported
	    else if (fn_command_cmdtype (dm_word) != DM_COMMAND_CMDTYPE_ACCESS_REG) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    ", fshow (fn_command_cmdtype (dm_word)), " not supported");
	    end

`ifdef RV32
	    // Only lower 32-bit access is supported
	    else if (size != DM_COMMAND_ACCESS_REG_SIZE_LOWER32) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, ",
			 fshow (fn_command_access_reg_size (dm_word)), " not supported in RV32 mode");
	    end
`endif
`ifdef RV64
	    // Only lower 32-bit and 64-bit access is supported
	    else if (size != DM_COMMAND_ACCESS_REG_SIZE_LOWER64)
	       begin
		  cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
		  $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
		  $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, ",
			    fshow (fn_command_access_reg_size (dm_word)), " not supported in RV64 mode");
	       end
`endif

	    // 'postexec' is not supported
	    else if (fn_command_access_reg_postexec (dm_word) == True) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, postexec not supported");
	    end

	    // non-'transfer' is not supported
	    else if (fn_command_access_reg_transfer (dm_word) == False) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
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
                  $display ("(%0d): DM_Abstract_Commands.write: [command] <= 0x%08h: OKAY", cur_cycle, dm_word);
	    end
	    rg_abstractcs_cmderr <= cmderr;
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Start reads/writes

   Bool is_csr = (   (fromInteger (dm_command_access_reg_regno_csr_0) <= rg_command_access_reg_regno)
		  && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_csr_FFF)));

   Bool is_gpr = (   (fromInteger (dm_command_access_reg_regno_gpr_0) <= rg_command_access_reg_regno)
		  && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_gpr_1F)));

   Bit #(12) csr_addr = truncate (rg_command_access_reg_regno - fromInteger (dm_command_access_reg_regno_csr_0));
   Bit #(5)  gpr_addr = truncate (rg_command_access_reg_regno - fromInteger (dm_command_access_reg_regno_gpr_0));

   // ----------------------------------------------------------------
   // Read/Write CSR

   rule rl_start_write_csr (   rg_abstractcs_busy
			    && rg_start_reg_access
			    && rg_command_access_reg_write
			    && is_csr);
      if (verbosity != 0)
`ifdef RV32
	 $display ("(%0d): DM_Abstract_Commands.write [command]: write CSR [0x%0h] <= 0x%08h", cur_cycle,
		   csr_addr, rg_data0);
`endif
`ifdef RV64
	 $display ("(%0d): DM_Abstract_Commands.write [command]: write CSR [0x%0h] <= 0x%016h", cur_cycle,
		   csr_addr, {rg_data1, rg_data0});
`endif

      let req = MemoryRequest {write: True, byteen: '1, address: csr_addr,
`ifdef RV32
			       data: rg_data0
`endif
`ifdef RV64
			       data: {rg_data1, rg_data0}
`endif
			       };
      f_hart0_csr_reqs.enq (req);
      rg_start_reg_access <= False;
      rg_abstractcs_busy  <= False;
   endrule

   rule rl_start_read_csr (   rg_abstractcs_busy
			   && rg_start_reg_access
			   && (! rg_command_access_reg_write)
			   && is_csr);
      if (verbosity != 0)
	 $display ("(%0d): DM_Abstract_Commands.write [command]: read CSR [0x%0h]", cur_cycle, csr_addr);

      let req = MemoryRequest {write: False, byteen: '1, address: csr_addr, data: ?};
      f_hart0_csr_reqs.enq (req);
      rg_start_reg_access <= False;
   endrule

   rule rl_finish_csr_read (rg_abstractcs_busy);
      let rsp <- pop (f_hart0_csr_rsps);
`ifdef RV32
      rg_data0 <= rsp.data;
`endif
`ifdef RV64
      rg_data0 <= truncate (rsp.data);
      rg_data1 <= rsp.data[63:32];
`endif
      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NONE;
      if (verbosity != 0)
`ifdef RV32
	 $display ("(%0d): DM_Abstract_Commands: csr read data is 0x%08h", cur_cycle, rsp.data);
`endif
`ifdef RV64
	 $display ("(%0d): DM_Abstract_Commands: csr read data is 0x%016h", cur_cycle, rsp.data);
`endif

      rg_abstractcs_busy <= False;
   endrule

   // ----------------------------------------------------------------
   // Read/Write GPR

   rule rl_start_write_gpr (   rg_abstractcs_busy
			    && rg_start_reg_access
			    && rg_command_access_reg_write
			    && is_gpr);
      if (verbosity != 0)
`ifdef RV32
	 $display ("(%0d): DM_Abstract_Commands.write [command]: write GPR [0x%0h] <= 0x%08h", cur_cycle,
	    gpr_addr, rg_data0);
`endif
`ifdef RV64
	 $display ("(%0d): DM_Abstract_Commands.write [command]: write GPR [0x%0h] <= 0x%016h", cur_cycle,
	    gpr_addr, {rg_data1, rg_data0});
`endif

      let req = MemoryRequest {write: True,
			       byteen: '1,
			       address: gpr_addr,
`ifdef RV32
			       data: rg_data0
`endif
`ifdef RV64
			       data: {rg_data1, rg_data0}
`endif
			       };

      f_hart0_gpr_reqs.enq (req);
      rg_start_reg_access <= False;
      rg_abstractcs_busy  <= False;
   endrule

   rule rl_start_read_gpr (   rg_abstractcs_busy
			   && rg_start_reg_access
			   && (! rg_command_access_reg_write)
			   && is_gpr);
      if (verbosity != 0)
	 $display ("(%0d): DM_Abstract_Commands.write [command]: read GPR [0x%0h]", cur_cycle, gpr_addr);

      let req = MemoryRequest {
	   write: False
	 , byteen: '1
	 , address: gpr_addr
	 , data: ?
      };
      f_hart0_gpr_reqs.enq (req);
      rg_start_reg_access <= False;
   endrule

   rule rl_finish_gpr_read (rg_abstractcs_busy);
      let rsp <- pop (f_hart0_gpr_rsps);
`ifdef RV32
      rg_data0 <= rsp.data;
`endif
`ifdef RV64
      rg_data0 <= truncate (rsp.data);
      rg_data1 <= rsp.data[63:32];
`endif
      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NONE;
      if (verbosity != 0)
`ifdef RV32
	 $display ("(%0d): DM_Abstract_Commands: gpr read data is 0x%08h", cur_cycle, rsp.data);
`endif
`ifdef RV64
	 $display ("(%0d): DM_Abstract_Commands: gpr read data is 0x%016h", cur_cycle, rsp.data);
`endif
      rg_abstractcs_busy <= False;
   endrule

   // ----------------
   // Read/Write unknown address

   rule rl_start_write_unknown (   rg_abstractcs_busy
				&& rg_start_reg_access
				&& rg_command_access_reg_write
				&& (! is_csr) && (! is_gpr));
      if (verbosity != 0)
	 $display ("(%0d): DM_Abstract_Commands.write [command]: write unknown RISC-V regno [0x%0h] <= 0x%08h", cur_cycle,
		   rg_command_access_reg_regno, rg_data0);

      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_OTHER;
      rg_start_reg_access  <= False;
      rg_abstractcs_busy   <= False;
   endrule

   rule rl_start_read_unknown (   rg_abstractcs_busy
			       && rg_start_reg_access
			       && (! rg_command_access_reg_write)
			       && (! is_csr) && (! is_gpr));
      if (verbosity != 0)
	 $display ("(%0d): DM_Abstract_Commands.write [command]: read unknown RISC-V regno [0x%0h] => ...", cur_cycle,
		   rg_command_access_reg_regno);

      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_OTHER;
      rg_start_reg_access  <= False;
      rg_abstractcs_busy   <= False;
   endrule

   // ----------------------------------------------------------------
   // Finish CSR and GPR reads

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
`ifdef RV64
      rg_data1 <= 0;
`endif

      if (verbosity != 0)
	 $display ("(%0d): DM_Abstract_Commands: reset", cur_cycle);
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
`ifdef RV64
			      dm_addr_data1:        rg_data1;
`endif
			      // dm_addr_data2..data3
			      // dm_addr_abstractauto
			      // dm_addr_progbuf0..15
			   endcase;
	 if (verbosity != 0)
	    $display ("(%0d): DM_Abstract_Commands.av_read: [", cur_cycle, dm_addr_name, "] => 0x%08h", dm_word);
	 return dm_word;
      endactionvalue
   endmethod

   method Action write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 let dm_addr_name = fshow_dm_addr (dm_addr);

	 if (dm_addr == dm_addr_abstractcs)
	    fa_rg_abstractcs_write (dm_word);

	 else if (rg_abstractcs_cmderr != DM_ABSTRACTCS_CMDERR_NONE) begin
	    if (verbosity != 0) begin
	       $display ("(%0d): DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name, "] <= 0x%08h: ERROR", dm_word);
	       $display ("    Ignoring: previous cmderr ", fshow (rg_abstractcs_cmderr));
	    end
	 end

	 else if (dm_addr == dm_addr_command)
	    fa_rg_command_write (dm_word);

	 else if (dm_addr == dm_addr_data0) begin
	    rg_data0 <= dm_word;

	    if (verbosity != 0)
	       $display ("(%0d): DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name, "] <= 0x%08h", dm_word);
	 end
`ifdef RV64
	 else if (dm_addr == dm_addr_data1) begin
	    rg_data1 <= dm_word;

	    if (verbosity != 0)
	       $display ("(%0d): DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name, "] <= 0x%08h", dm_word);
	 end
`endif
	 else begin
	    // dm_addr_data2..12
	    // dm_addr_abstractauto
	    // dm_addr_progbuf0..15
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;

	    $display ("(%0d): DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name,
		      "] <= 0x%08h: ERROR: not supported", dm_word);
	 end
      endaction
   endmethod

   // ----------------
   // Facing CPU/hart
      interface MemoryClient hart0_gpr_mem_client = toGPClient (f_hart0_gpr_reqs, f_hart0_gpr_rsps);
      interface MemoryClient hart0_csr_mem_client = toGPClient (f_hart0_csr_reqs, f_hart0_csr_rsps);
endmodule

// ================================================================

endpackage
