// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CSR_RegFile_UM;

// ================================================================
// CSR (Control and Status Register) Register File

// This version has all the User- and Machine- privilege registers.

// ================================================================
// Exports

export  CSR_RegFile_IFC (..),  mkCSR_RegFile;

// ================================================================
// BSV library imports

import ConfigReg    :: *;
import RegFile      :: *;
import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// BSV additional libs

import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;

`ifdef INCLUDE_GDB_CONTROL
import DM_Common :: *;    // Debug Module defs
`endif

// ================================================================

interface CSR_RegFile_IFC;
   // Reset
   interface Server #(Token, Token) server_reset;

   // CSR read (w.o. side effect)
   (* always_ready *)
   method Maybe #(Word) read_csr (CSR_Addr csr_addr);
   (* always_ready *)
   method Maybe #(Word) read_csr_port2 (CSR_Addr csr_addr);

   // CSR read (w. side effect)
   (* always_ready *)
   method ActionValue #(Maybe #(Word)) mav_read_csr (CSR_Addr csr_addr);

   // CSR write
   (* always_ready *)
   method Action write_csr (CSR_Addr csr_addr, Word word);

`ifdef ISA_F
   // Read FCSR.FRM
   (* always_ready *)
   method Bit #(3) read_frm;

   // Update FCSR.FFLAGS
   (* always_ready *)
   method Action update_fcsr_fflags (Bit #(5) flags);
`endif

   // Read MISA
   (* always_ready *)
   method MISA read_misa;

   // Read MSTATUS
   (* always_ready *)
   method WordXL read_mstatus;

   // Read SSTATUS
   (* always_ready *)
   method WordXL read_sstatus;

   // Read SATP
   (* always_ready *)
   method WordXL read_satp;

   // CSR trap actions
   method ActionValue #(Trap_Info)
          csr_trap_actions (Priv_Mode  from_priv,
			    Word       pc,
			    Bool       interrupt,
			    Exc_Code   exc_code,
			    Word       xtval);

   // CSR RET actions (return from exception)
   method ActionValue #(Tuple3 #(Addr, Priv_Mode, Word)) csr_ret_actions (Priv_Mode from_priv);

   // Read MINSTRET
   (* always_ready *)
   method Bit #(64) read_csr_minstret;

   // Increment MINSTRET
   (* always_ready *)
   method Action csr_minstret_incr;

   // Read MCYCLE
   (* always_ready *)
   method Bit #(64) read_csr_mcycle;

   // Read MTIME
   (* always_ready *)
   method Bit #(64) read_csr_mtime;

   // Fault on reading counters?
   (* always_ready *)
   method Bool csr_counter_read_fault (Priv_Mode  priv, CSR_Addr  csr_addr);

   // Read MIP
   (* always_ready *)
   method MIP read_csr_mip;

   // Interrupts
   (* always_ready, always_enabled *)
   method Action external_interrupt_req (Bool set_not_clear);

   method Action timer_interrupt_req    (Bool set_not_clear);
   method Action software_interrupt_req (Bool set_not_clear);

   (* always_ready *)
   method Maybe #(Exc_Code) interrupt_pending (Priv_Mode cur_priv);

   // WFI ignores mstatus ies and ideleg regs
   (* always_ready *)
   method Bool wfi_resume;

   // ----------------
   // Methods when Debug Module is present

`ifdef INCLUDE_GDB_CONTROL
   // Read dpc
   method Word read_dpc ();

   // Update dpc
   method Action write_dpc (Addr pc);

   // Break should enter Debug Mode
   method Bool dcsr_break_enters_debug (Priv_Mode cur_priv);

   // Read dcsr.step
   method Bool read_dcsr_step ();

   // Update 'cause' and 'priv' in DCSR
   (* always_ready *)
   method Action write_dcsr_cause_priv (DCSR_Cause  cause, Priv_Mode  priv);

`endif

endinterface

// ================================================================
// 'misa' specifying RSIC-V features implemented.

function MISA misa_reset_value;
   MISA ms = unpack (0);

`ifdef RV32
   ms.mxl = misa_mxl_32;
`elsif RV64
   ms.mxl = misa_mxl_64;
`elsif RV128
   ms.mxl = misa_mxl_128;
`else
   ms.mxl = misa_mxl_default;
`endif

`ifdef ISA_PRIV_U
   // User Mode
   ms.u = 1'b1;
`ifdef ISA_N
   // User-level Interrupts
   ms.n = 1'b1;
`endif
`endif

`ifdef ISA_PRIV_S
   // Supervisor Mode
   ms.s = 1'b1;
`endif

   // Integer Base
   ms.i = 1'b1;

`ifdef ISA_M
   // Integer Multiply/Divide
   ms.m = 1'b1;
`endif

`ifdef ISA_FD
   // Single- and Double-precision Floating Point
   ms.f = 1'b1;
   ms.d = 1'b1;
`endif

`ifdef ISA_A
   // Atomic Memory Ops
   ms.a = 1'b1;
`endif

`ifdef ISA_C
   // Compressed Instructions
   ms.c = 1'b1;
`endif

   return ms;
endfunction

// ================================================================
// mtvec reset value    TODO: still relevant? No longer part of the spec?

Word mtvec_reset_value = 'h1000;    // TODO: this is no longer standard?

// ================================================================
// Major states of mkCSR_RegFile module

typedef enum { RF_RESET_START, RF_RUNNING } RF_State
deriving (Eq, Bits, FShow);

// ================================================================

(* synthesize *)
module mkCSR_RegFile (CSR_RegFile_IFC);

   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (0);
   Reg #(RF_State) rg_state      <- mkReg (RF_RESET_START);

   FIFOF #(Bool) f_ei_reqs   <- mkFIFOF;   // External interrupt requested
   FIFOF #(Bool) f_ti_reqs   <- mkFIFOF;   // Timer    interrupt requests
   FIFOF #(Bool) f_si_reqs   <- mkFIFOF;   // Software interrupt requested

   // Reset
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   // CSRs
   // User-mode CSRs
`ifdef ISA_FD
   Reg #(Bit #(5)) rg_fflags <- mkRegU;    // floating point flags
   Reg #(Bit #(3)) rg_frm    <- mkRegU;    // floating point rounding mode
`endif

   // Supervisor-mode CSRs
   Bit #(16)  sedeleg = 0;    // hardwired to 0
   Bit #(12)  sideleg = 0;    // hardwired to 0

`ifdef ISA_PRIV_S
   // sstatus is a restricted view of mstatus
   // sie     is a restricted view of mie
   // sip     is a restricted view of mip

   Reg #(MTVec)      rg_stvec     <- mkRegU;
   // scounteren hardwired to 0 for now

   Reg #(Word)       rg_sscratch  <- mkRegU;
   Reg #(Word)       rg_sepc      <- mkRegU;
   Reg #(MCause)     rg_scause    <- mkRegU;
   Reg #(Word)       rg_stval     <- mkRegU;

   Reg #(WordXL)     rg_satp      <- mkRegU;

   Reg #(Bit #(16))  rg_medeleg   <- mkRegU;    // TODO: also in M-U systems with user-level traps
   Reg #(Bit #(12))  rg_mideleg   <- mkRegU;    // TODO: also in M-U systems with user-level traps
`else
   Bit #(16)         rg_medeleg   = 0;
   Bit #(12)         rg_mideleg   = 0;
`endif

   // Machine-mode CSRs
   Word mvendorid   = 0;    // Not implemented
   Word marchid     = 0;    // Not implemented
   Word mimpid      = 0;    // Not implemented
   Word mhartid     = 0;

   Reg #(MStatus)    rg_mstatus    <- mkReg (mstatus_reset_value);
   MISA              misa          =  misa_reset_value;
   Reg #(MIE)        rg_mie        <- mkRegU;
   Reg #(MTVec)      rg_mtvec      <- mkRegU;
   Reg #(MCounteren) rg_mcounteren <- mkRegU;

   Reg #(Word)       rg_mscratch <- mkRegU;
   Reg #(Word)       rg_mepc     <- mkRegU;
   Reg #(MCause)     rg_mcause   <- mkRegU;
   Reg #(Word)       rg_mtval    <- mkRegU;
   Reg #(MIP)        rg_mip      <- mkRegU;

   // RegFile #(Bit #(2), WordXL)  rf_pmpcfg   <- mkRegFileFull;
   // Vector #(16, Reg #(WordXL))  vrg_pmpaddr <- replicateM (mkRegU);

   // mcycle is needed even for user-mode RDCYCLE instruction
   // It can be updated by a CSR instruction (in Priv_M), and by the clock
   Reg #(Bit #(64))   rg_mcycle <- mkReg (0);
   RWire #(Bit #(64)) rw_mcycle <- mkRWire;    // Driven on CSRRx write to mcycle

   // minstret is needed even for user-mode RDINSTRET instructions
   // It can be updated by a CSR instruction (in Priv_M), and by retirement of any other instruction
   Reg #(Bit #(64))   rg_minstret      <- mkReg (0);    // Needed even for user-mode instrs
   RWire #(Bit #(64)) rw_minstret      <- mkRWire;      // Driven on CSRRx write to minstret
   PulseWire          pw_minstret_incr <- mkPulseWire;

   // Debug/Trace
   Reg #(WordXL)    rg_tselect <- mkRegU;
   Reg #(WordXL)    rg_tdata1  <- mkRegU;
   Reg #(WordXL)    rg_tdata2  <- mkRegU;
   Reg #(WordXL)    rg_tdata3  <- mkRegU;

   // Debug
   Reg #(Bit #(32)) rg_dcsr      <- mkRegU;    // Is 32b even in RV64
   Reg #(WordXL)    rg_dpc       <- mkReg(mtvec_reset_value);
   Reg #(WordXL)    rg_dscratch0 <- mkRegU;
   Reg #(WordXL)    rg_dscratch1 <- mkRegU;

   // ----------------------------------------------------------------
   // Reset.
   // Initialize some CSRs.

   rule rl_reset_start (rg_state == RF_RESET_START);
      f_ei_reqs.clear;
      f_ti_reqs.clear;
      f_si_reqs.clear;

      // User-level CSRs
`ifdef ISA_FD
      rg_fflags <= 0;
      rg_frm    <= 0;
`endif

      // Supervisor-level CSRs
`ifdef ISA_PRIV_S
      rg_stvec    <= word_to_mtvec (mtvec_reset_value);
      rg_scause   <= word_to_mcause (0);    // Supposed to be the cause of the reset.
      rg_satp     <= 0;
      //rg_scounteren <= mcounteren_reset_value;
`endif

      // Machine-level CSRs
      rg_mstatus    <= mstatus_reset_value;
      rg_mie        <= mie_reset_value;
      rg_mtvec      <= word_to_mtvec (mtvec_reset_value);
      rg_mcause     <= word_to_mcause (0);    // Supposed to be the cause of the reset.
      rg_mip        <= mip_reset_value;
`ifdef ISA_PRIV_S
      rg_medeleg    <= 0;
      rg_mideleg    <= 0;
`endif
      rg_mcounteren <= mcounteren_reset_value;

      rw_minstret.wset (0);

`ifdef INCLUDE_GDB_CONTROL
      // rg_dpc  <= pc_reset_value;    // Should be set by GDB
      rg_dcsr <= zeroExtend ({4'h4,    // [31:28]  xdebugver
			      12'h0,   // [27:16]  reserved
			      1'h1,    // [15]     ebreakm
			      1'h0,    // [14]     reserved
			      1'h1,    // [13]     ebreaks
			      1'h1,    // [12]     ebreaku
			      1'h0,    // [11]     stepie
			      1'h0,    // [10]     stopcount
			      1'h0,    // [9]      stoptime
			      3'h0,    // [8:7]    cause    // WARNING: 0 is non-standard
			      1'h0,    // [5]      reserved
			      1'h1,    // [4]      mprven
			      1'h0,    // [3]      nmip
			      1'h0,    // [2]      step
			      2'h3}    // [1:0]    prv (machine mode)
			     );
`endif

      rg_state <= RF_RUNNING;
   endrule

   // ----------------------------------------------------------------
   // CYCLE counter

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_mcycle_incr;
      // Update due to CSRRx    TODO: fix this
      if (rw_mcycle.wget matches tagged Valid .v)
	 rg_mcycle <= rg_mcycle + 1;

      // Update due to clock
      else
	 rg_mcycle <= rg_mcycle + 1;
   endrule

   // ----------------------------------------------------------------
   // INSTRET

   (* descending_urgency = "rl_reset_start, rl_upd_minstret_csrrx" *)
   rule rl_upd_minstret_csrrx (rw_minstret.wget matches tagged Valid .v);
      rg_minstret <= v;
      // $display ("%0d: CSR_RegFile_UM.rl_upd_minstret_csrrx: new value is %0d", rg_mcycle, v);
   endrule

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_upd_minstret_incr ((! isValid (rw_minstret.wget)) && pw_minstret_incr);
      rg_minstret <= rg_minstret + 1;
      // $display ("%0d: CSR_RegFile_UM.rl_upd_minstret_incr: new value is %0d", rg_mcycle, rg_minstret + 1);
   endrule

   // ----------------------------------------------------------------
   // Help functions for interface methods

   // ----------------
   // CSR reads (no side effect)
   // Returns Invalid for invalid CSR addresses or access-mode violations

   function Maybe #(Word) fv_csr_read (CSR_Addr csr_addr);
      Maybe #(Word)  m_csr_value = tagged Invalid;

      if ((csr_hpmcounter3 <= csr_addr) && (csr_addr <= csr_hpmcounter31))
	 m_csr_value = tagged Valid 0;
`ifdef RV32
      else if ((csr_hpmcounter3h <= csr_addr) && (csr_addr <= csr_hpmcounter31h))
	 m_csr_value = tagged Valid 0;
`endif
      else if ((csr_mhpmcounter3 <= csr_addr) && (csr_addr <= csr_mhpmcounter31))
	 m_csr_value = tagged Valid 0;
`ifdef RV32
      else if ((csr_mhpmcounter3h <= csr_addr) && (csr_addr <= csr_mhpmcounter31h))
	 m_csr_value = tagged Valid 0;
`endif
      else if ((csr_mhpmevent3 <= csr_addr) && (csr_addr <= csr_mhpmevent31))
	 m_csr_value = tagged Valid 0;

      else begin
	 case (csr_addr)
	    // User mode csrs
`ifdef ISA_FD
	    csr_fflags:    m_csr_value = tagged Valid ({ 0, rg_fflags });
	    csr_frm:       m_csr_value = tagged Valid ({ 0, rg_frm });
	    csr_fcsr:      m_csr_value = tagged Valid ({ 0, rg_frm, rg_fflags });
`endif
	    csr_cycle:     m_csr_value = tagged Valid (truncate (rg_mcycle));

	    /*
	    // NOTE: CSR_TIME should be a 'shadow copy' of the MTIME
	    // mem-mapped location; but since both increment at the
	    // same rate, and MTIME is never written, this is ok.

	    csr_time:      m_csr_value = tagged Valid (truncate (rg_mcycle));
	    */

	    csr_instret:   m_csr_value = tagged Valid (truncate (rg_minstret));
`ifdef RV32
	    csr_cycleh:    m_csr_value = tagged Valid (rg_mcycle   [63:32]);
	    csr_timeh:     m_csr_value = tagged Invalid;
	    csr_instreth:  m_csr_value = tagged Valid (rg_minstret [63:32]);
`endif

`ifdef ISA_PRIV_S
	    // Supervisor mode csrs
	    csr_sstatus:   m_csr_value = tagged Valid (fn_read_sstatus (rg_mstatus));
	    csr_sedeleg:   m_csr_value = tagged Valid zeroExtend (sedeleg);
	    csr_sideleg:   m_csr_value = tagged Valid zeroExtend (sideleg);
	    csr_sie:       m_csr_value = tagged Valid (sie_to_word (rg_mie, rg_mideleg));
	    csr_stvec:     m_csr_value = tagged Valid (mtvec_to_word (rg_stvec));
	    csr_scounteren:m_csr_value = tagged Valid 0;

	    csr_sscratch:  m_csr_value = tagged Valid rg_sscratch;
	    csr_sepc:      m_csr_value = tagged Valid rg_sepc;
	    csr_scause:    m_csr_value = tagged Valid (mcause_to_word (rg_scause));
	    csr_stval:     m_csr_value = tagged Valid rg_stval;
	    csr_sip:       m_csr_value = tagged Valid (sip_to_word (rg_mip, rg_mideleg));

	    csr_satp:      m_csr_value = tagged Valid rg_satp;

	    csr_medeleg:   m_csr_value = tagged Valid zeroExtend (rg_medeleg);
	    csr_mideleg:   m_csr_value = tagged Valid zeroExtend (rg_mideleg);
`endif

	    // Machine mode csrs
	    csr_mvendorid: m_csr_value = tagged Valid mvendorid;
	    csr_marchid:   m_csr_value = tagged Valid marchid;
	    csr_mimpid:    m_csr_value = tagged Valid mimpid;
	    csr_mhartid:   m_csr_value = tagged Valid mhartid;

	    csr_mstatus:   m_csr_value = tagged Valid (mstatus_to_word (rg_mstatus));
	    csr_misa:      m_csr_value = tagged Valid (misa_to_word (misa));
	    csr_mie:       m_csr_value = tagged Valid (mie_to_word (rg_mie));
	    csr_mtvec:     m_csr_value = tagged Valid (mtvec_to_word (rg_mtvec));
	    csr_mcounteren:m_csr_value = tagged Valid (mcounteren_to_word (rg_mcounteren));

	    csr_mscratch:  m_csr_value = tagged Valid rg_mscratch;
	    csr_mepc:      m_csr_value = tagged Valid rg_mepc;
	    csr_mcause:    m_csr_value = tagged Valid (mcause_to_word (rg_mcause));
	    csr_mtval:     m_csr_value = tagged Valid rg_mtval;
	    csr_mip:       m_csr_value = tagged Valid (mip_to_word (rg_mip));

	    // TODO: Phys Mem Protection regs
	    // csr_pmpcfg0:   m_csr_value = tagged Valid rf_pmpcfg.sub (0);
	    // csr_pmpcfg1:   m_csr_value = tagged Valid rf_pmpcfg.sub (1);
	    // csr_pmpcfg2:   m_csr_value = tagged Valid rf_pmpcfg.sub (2);
	    // csr_pmpcfg3:   m_csr_value = tagged Valid rf_pmpcfg.sub (3);

	    // csr_pmpaddr0:   m_csr_value = tagged Valid vrg_pmpaddr [0];
	    // csr_pmpaddr1:   m_csr_value = tagged Valid vrg_pmpaddr [1];
	    // csr_pmpaddr2:   m_csr_value = tagged Valid vrg_pmpaddr [2];
	    // csr_pmpaddr3:   m_csr_value = tagged Valid vrg_pmpaddr [3];
	    // csr_pmpaddr4:   m_csr_value = tagged Valid vrg_pmpaddr [4];
	    // csr_pmpaddr5:   m_csr_value = tagged Valid vrg_pmpaddr [5];
	    // csr_pmpaddr6:   m_csr_value = tagged Valid vrg_pmpaddr [6];
	    // csr_pmpaddr7:   m_csr_value = tagged Valid vrg_pmpaddr [7];
	    // csr_pmpaddr8:   m_csr_value = tagged Valid vrg_pmpaddr [8];
	    // csr_pmpaddr9:   m_csr_value = tagged Valid vrg_pmpaddr [9];
	    // csr_pmpaddr10:  m_csr_value = tagged Valid vrg_pmpaddr [10];
	    // csr_pmpaddr11:  m_csr_value = tagged Valid vrg_pmpaddr [11];
	    // csr_pmpaddr12:  m_csr_value = tagged Valid vrg_pmpaddr [12];
	    // csr_pmpaddr13:  m_csr_value = tagged Valid vrg_pmpaddr [13];
	    // csr_pmpaddr14:  m_csr_value = tagged Valid vrg_pmpaddr [14];
	    // csr_pmpaddr15:  m_csr_value = tagged Valid vrg_pmpaddr [15];

	    csr_mcycle:        m_csr_value = tagged Valid (truncate (rg_mcycle));
	    csr_minstret:      m_csr_value = tagged Valid (truncate (rg_minstret));
`ifdef RV32
	    csr_mcycleh:        m_csr_value = tagged Valid (rg_mcycle [63:32]);
	    csr_minstreth:      m_csr_value = tagged Valid (rg_minstret [63:32]);
`endif

	    csr_addr_tselect:  m_csr_value = tagged Valid rg_tselect;
	    csr_addr_tdata1:   m_csr_value = tagged Valid rg_tdata1;
	    csr_addr_tdata2:   m_csr_value = tagged Valid rg_tdata2;
	    csr_addr_tdata3:   m_csr_value = tagged Valid rg_tdata3;

`ifdef INCLUDE_GDB_CONTROL
	    csr_addr_dcsr:       m_csr_value = tagged Valid zeroExtend (rg_dcsr);
	    csr_addr_dpc:        m_csr_value = tagged Valid rg_dpc;
	    csr_addr_dscratch0:  m_csr_value = tagged Valid rg_dscratch0;
	    csr_addr_dscratch1:  m_csr_value = tagged Valid rg_dscratch1;
`endif

	    default: m_csr_value = tagged Invalid;
	 endcase
      end

      return m_csr_value;
   endfunction

   // ----------------
   // CSR writes
   // Returns True if successful
   // If unsuccessful, should trap (illegal CSR).

   function Action fav_write_csr (CSR_Addr csr_addr, Word word);
      action
	 Bool success = True;
	 if ((csr_mhpmcounter3 <= csr_addr) && (csr_addr <= csr_mhpmcounter31))
	    noAction;
`ifdef RV32
	 else if ((csr_mhpmcounter3h <= csr_addr) && (csr_addr <= csr_mhpmcounter31h))
	    noAction;
`endif
	 else if ((csr_mhpmevent3 <= csr_addr) && (csr_addr <= csr_mhpmevent31))
	    noAction;
	 else
	    case (csr_addr)
	       // User mode csrs
`ifdef ISA_FD
	       csr_fflags:     rg_fflags <= word [4:0];
	       csr_frm:        rg_frm    <= word [7:5];
	       csr_fcsr:       begin
				  rg_fflags <= word [4:0];
				  rg_frm    <= word [7:5];
			       end
`endif

`ifdef ISA_PRIV_S
	       csr_sstatus:    rg_mstatus    <= fn_write_sstatus (misa, rg_mstatus, word);
	       csr_sedeleg:    noAction;               // Hardwired to 0 (no delegation)
	       csr_sideleg:    noAction;               // Hardwired to 0 (no delegation)
	       csr_sie:        rg_mie        <= word_to_sie (word, rg_mie, rg_mideleg);
	       csr_stvec:      rg_stvec      <= word_to_mtvec (word);
	       csr_scounteren: noAction;

	       csr_sscratch:   rg_sscratch <= word;
	       csr_sepc:       rg_sepc     <= word;
	       csr_scause:     rg_scause   <= word_to_mcause (word);
	       csr_stval:      rg_stval    <= word;
	       csr_sip:        rg_mip      <= word_to_sip (word, rg_mip, rg_mideleg);

	       csr_satp:       rg_satp <= word;

	       csr_medeleg:    rg_medeleg <= (truncate (word) & 'h_B3FF);  // 16 bits relevant and some are 0
	       csr_mideleg:    rg_mideleg <= (truncate (word) & 'h_0FFF);  // 12 bits relevant
`endif

	       // Machine mode
	       csr_mvendorid: noAction;
	       csr_marchid:   noAction;
	       csr_mimpid:    noAction;
	       csr_mhartid:   noAction;

	       csr_mstatus:   rg_mstatus    <= word_to_mstatus (misa, word);
	       csr_misa:      noAction;
	       csr_mie:       rg_mie        <= word_to_mie (word);
	       csr_mtvec:     rg_mtvec      <= word_to_mtvec (word);
	       csr_mcounteren:rg_mcounteren <= word_to_mcounteren(word);

	       csr_mscratch:  rg_mscratch <= word;
	       csr_mepc:      rg_mepc     <= word;
	       csr_mcause:    rg_mcause   <= word_to_mcause (word);
	       csr_mtval:     rg_mtval    <= word;
	       csr_mip:       rg_mip      <= word_to_mip (word, rg_mip);

	       // TODO: PMPs
	       // csr_pmpcfg0:   rf_pmpcfg.upd (0, word);
	       // csr_pmpcfg1:   rf_pmpcfg.upd (1, word);
	       // csr_pmpcfg2:   rf_pmpcfg.upd (2, word);
	       // csr_pmpcfg3:   rf_pmpcfg.upd (3, word);

	       // csr_pmpaddr0:  vrg_pmpaddr [0] <= word;
	       // csr_pmpaddr1:  vrg_pmpaddr [1] <= word;
	       // csr_pmpaddr2:  vrg_pmpaddr [2] <= word;
	       // csr_pmpaddr3:  vrg_pmpaddr [3] <= word;
	       // csr_pmpaddr4:  vrg_pmpaddr [4] <= word;
	       // csr_pmpaddr5:  vrg_pmpaddr [5] <= word;
	       // csr_pmpaddr6:  vrg_pmpaddr [6] <= word;
	       // csr_pmpaddr7:  vrg_pmpaddr [7] <= word;
	       // csr_pmpaddr8:  vrg_pmpaddr [8] <= word;
	       // csr_pmpaddr9:  vrg_pmpaddr [9] <= word;
	       // csr_pmpaddr10: vrg_pmpaddr [10] <= word;
	       // csr_pmpaddr11: vrg_pmpaddr [11] <= word;
	       // csr_pmpaddr12: vrg_pmpaddr [12] <= word;
	       // csr_pmpaddr13: vrg_pmpaddr [13] <= word;
	       // csr_pmpaddr14: vrg_pmpaddr [14] <= word;
	       // csr_pmpaddr15: vrg_pmpaddr [15] <= word;

`ifdef RV32
	       csr_mcycle:    rw_mcycle.wset   ({ rg_mcycle   [63:32], word });
	       csr_minstret:  rw_minstret.wset ({ rg_minstret [63:32], word });
	       csr_mcycleh:   rw_mcycle.wset   ({ word, rg_mcycle   [31:0] });
	       csr_minstreth: rw_minstret.wset ({ word, rg_minstret [31:0] });
`else
	       csr_mcycle:    rw_mcycle.wset   (word);
	       csr_minstret:  rw_minstret.wset (word);
`endif

	       csr_addr_tselect:  rg_tselect <= word;
	       csr_addr_tdata1:   rg_tdata1  <= word;
	       csr_addr_tdata2:   rg_tdata2  <= word;
	       csr_addr_tdata3:   rg_tdata3  <= word;

`ifdef INCLUDE_GDB_CONTROL
	       csr_addr_dcsr:       begin
				       Bit #(32) new_dcsr
				       = {rg_dcsr [31:28],   // xdebugver: read-only
					  rg_dcsr [27:16],   // reserved
					  wordxl  [15:12],   // ebreakm/s/u,
					  wordxl  [11:9],    // stepie, stopcount, stoptime
					  rg_dcsr [8:6],     // cause: read-only
					  rg_dcsr [5],       // reserved
					  wordxl  [4],       // mprvn
					  rg_dcsr [3],       // nmip: read-only
					  wordxl  [2],       // step
					  wordxl  [1:0]};    // prv
				       result   = zeroExtend (new_dcsr);
				       rg_dcsr <= new_dcsr;
				    end
	       csr_addr_dpc:        rg_dpc  <= word;
	       csr_addr_dscratch0:  rg_dscratch0  <= word;
	       csr_addr_dscratch1:  rg_dscratch1  <= word;
`endif

	       default:       success = False;
	    endcase

	 if ((! success) && (cfg_verbosity > 1))
	    $display ("%0d: ERROR: CSR-write addr 0x%0h val 0x%0h not successful", rg_mcycle,
		      csr_addr, word);
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Interrupt requests

   (* execution_order = "read_csr,  rl_record_external_interrupt" *)
   (* execution_order = "write_csr, rl_record_external_interrupt" *)
   rule rl_record_external_interrupt;
      let ei_req <- pop (f_ei_reqs);

      // Set or clear mip.mtip
      let old_mip = rg_mip;
      let new_mip = old_mip;
      new_mip.eips [m_Priv_Mode] = (ei_req ? 1'b1 : 1'b0);
      rg_mip <= new_mip;

      WordXL old_mip_w = mip_to_word (old_mip);
      WordXL new_mip_w = mip_to_word (new_mip);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: CSR_RegFile.rl_record_external_interrupt: mip: %0h -> %0h",
		   rg_mcycle, old_mip_w, new_mip_w);
	 $display ("    Current mie = %0h", mie_to_word (rg_mie));
      end
   endrule

   (* execution_order = "read_csr,  rl_record_timer_interrupt_req" *)
   (* execution_order = "write_csr, rl_record_timer_interrupt_req" *)
   rule rl_record_timer_interrupt_req;
      let ti_req <- pop (f_ti_reqs);

      // Set or clear mip.mtip
      let old_mip = rg_mip;
      let new_mip = old_mip;
      new_mip.tips [m_Priv_Mode] = (ti_req ? 1'b1 : 1'b0);
      rg_mip <= new_mip;

      WordXL old_mip_w = mip_to_word (old_mip);
      WordXL new_mip_w = mip_to_word (new_mip);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: CSR_RegFile.rl_record_timer_interrupt_req: mip: %0h -> %0h",
		   rg_mcycle, old_mip_w, new_mip_w);
	 $display ("    Current mie = %0h", mie_to_word (rg_mie));
      end
   endrule

   (* execution_order = "read_csr,  rl_record_software_interrupt" *)
   (* execution_order = "write_csr, rl_record_software_interrupt" *)
   rule rl_record_software_interrupt;
      let si_req <- pop (f_si_reqs);

      // Set or clear mip.msip
      let old_mip = rg_mip;
      let new_mip = old_mip;
      new_mip.sips [m_Priv_Mode] = (si_req ? 1'b1 : 1'b0);
      rg_mip <= new_mip;

      WordXL old_mip_w = mip_to_word (old_mip);
      WordXL new_mip_w = mip_to_word (new_mip);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: CSR_RegFile.rl_record_software_interrupt: mip: %0h -> %0h",
		   rg_mcycle, old_mip_w, new_mip_w);
	 $display ("    Current mie = %0h", mie_to_word (rg_mie));
      end
   endrule

   // ================================================================
   // For debugging

   function Action fa_show_trap_csrs (Priv_Mode priv,
				      MIP ip, MIE ie,
				      Bit #(16) edeleg, Bit #(12) ideleg,
				      MCause cause, MStatus status, MTVec tvec,
				      WordXL epc, WordXL tval);
      action
	 $write ("    priv %0d: ", priv);
	 $write (" ip: 0x%0h", mip_to_word (ip));
	 $write (" ie: 0x%0h", mie_to_word (ie));
	 $write (" edeleg: 0x%0h", edeleg);
	 $write (" ideleg: 0x%0h", ideleg);
	 $write (" cause:", fshow (cause));
	 $display ("");

	 $write ("        ");
	 $write (" status: 0x%0h", mstatus_to_word (status));
	 $write (" tvec: 0x%0h", mtvec_to_word (tvec));
	 $write (" epc: 0x%0h", epc);
	 $write (" tval: 0x%0h", tval);
	 $display ("");
      endaction
   endfunction

   // ================================================================
   // INTERFACE

   // Reset
   interface Server server_reset;
      interface Put request;
	 method Action put (Token token);
	    rg_state <= RF_RESET_START;

	    // This response is placed here, and not in rl_reset_loop, because
	    // reset_loop can happen on power-up, where no response is expected.
	    f_reset_rsps.enq (?);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue #(Token) get if (rg_state == RF_RUNNING);
	    let token <- pop (f_reset_rsps);
	    return token;
	 endmethod
      endinterface
   endinterface

   // CSR read (w.o. side effect)
   method Maybe #(Word) read_csr (CSR_Addr csr_addr);
      return fv_csr_read (csr_addr);
   endmethod

   // CSR read (w.o. side effect)
   method Maybe #(Word) read_csr_port2 (CSR_Addr csr_addr);
      return fv_csr_read (csr_addr);
   endmethod

   // CSR read (w. side effect)
   method ActionValue #(Maybe #(Word)) mav_read_csr (CSR_Addr csr_addr);
      return fv_csr_read (csr_addr);
   endmethod

   // CSR write
   method Action write_csr (CSR_Addr csr_addr, Word word);
      fav_write_csr (csr_addr, word);
   endmethod

   // Read MISA
   method MISA read_misa;
      return misa;
   endmethod

`ifdef ISA_F
   // Read FCSR.FRM
   method Bit# (3) read_frm;
      return rg_frm;
   endmethod

   // Update FCSR.FFLAGS
   method Action update_fcsr_fflags (Bit#(5) flags);
      rg_fflags <= rg_fflags | flags;
   endmethod
`endif

   // Read MSTATUS
   method WordXL read_mstatus;
      return  mstatus_to_word (rg_mstatus);
   endmethod

   // Read SSTATUS
   method WordXL read_sstatus;
      return  fn_read_sstatus (rg_mstatus);
   endmethod

   // Read SATP
   method WordXL read_satp;
`ifdef ISA_PRIV_S
      return  rg_satp;
`else
      return  ?;
`endif
   endmethod

   // CSR Trap actions
   method ActionValue #(Trap_Info)
          csr_trap_actions (Priv_Mode  from_priv,
			    Word       pc,
			    Bool       interrupt,
			    Exc_Code   exc_code,
			    Word       xtval);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: CSR_Regfile.csr_trap_actions:", rg_mcycle);
	 $display ("    from priv %0d  pc 0x%0h  interrupt %0d  exc_code %0d  xtval 0x%0h",
		   from_priv, pc, pack (interrupt), exc_code, xtval);
`ifdef ISA_PRIV_S
	 fa_show_trap_csrs (s_Priv_Mode, rg_mip, rg_mie, 0, 0, rg_scause,
			    word_to_mstatus (misa,  fn_read_sstatus (rg_mstatus)),
			    rg_stvec, rg_sepc, rg_stval);
`endif
	 fa_show_trap_csrs (m_Priv_Mode, rg_mip, rg_mie, rg_medeleg, rg_mideleg, rg_mcause,
			    rg_mstatus,
			    rg_mtvec, rg_mepc, rg_mtval);
      end

      let new_priv    = fn_new_priv_on_exception (from_priv,
						  interrupt,
						  exc_code,
						  rg_medeleg,
						  rg_mideleg,
						  sedeleg,
						  sideleg);
      let new_mstatus = fn_mstatus_upd_on_trap (rg_mstatus, from_priv, new_priv);
      rg_mstatus     <= new_mstatus;

      Reg #(Word)   rg_xepc   = rg_mepc;
      Reg #(MCause) rg_xcause = rg_mcause;
      Reg #(Word)   rg_xtval  = rg_mtval;
      Reg #(MTVec)  rg_xtvec  = rg_mtvec;
`ifdef ISA_PRIV_S
      if (new_priv != m_Priv_Mode) begin
         rg_xepc   = rg_sepc;
         rg_xcause = rg_scause;
         rg_xtval  = rg_stval;
         rg_xtvec  = rg_stvec;
      end
`endif

      rg_xepc        <= pc;
      let xcause      = MCause {interrupt: pack (interrupt), exc_code: exc_code};
      rg_xcause      <= xcause;
      rg_xtval       <= xtval;

      // Compute the exception PC based on the xTVEC mode bits
      Addr exc_pc     = (extend (rg_xtvec.base)) << 2;
      Addr vector_offset = (extend (exc_code)) << 2;
      if ((interrupt) && (rg_xtvec.mode == VECTORED))
	 exc_pc = exc_pc + vector_offset;

      if (cfg_verbosity > 1) begin
	 $write ("    Return: new pc 0x%0h  ", exc_pc);
	 $write (" new mstatus:", fshow (new_mstatus));
	 $write (" new xcause:", fshow (xcause));
	 $write (" new priv %0d", new_priv);
	 $display ("");
      end

      return (Trap_Info {pc       : exc_pc,                        // New PC
			 mstatus  : mstatus_to_word (new_mstatus), // New mstatus
			 mcause   : mcause_to_word  (xcause),      // New mcause
			 priv     : new_priv});                    // New priv
   endmethod

   // CSR RET actions (return from exception)
   method ActionValue #(Tuple3 #(Addr, Priv_Mode, Word)) csr_ret_actions (Priv_Mode from_priv);
      match { .new_mstatus, .to_priv } = fn_mstatus_upd_on_ret (rg_mstatus, from_priv);
      rg_mstatus  <= new_mstatus;
      Word next_pc = rg_mepc;
`ifdef ISA_PRIV_S
      if (from_priv != m_Priv_Mode)
	 next_pc = rg_sepc;
`endif
      return tuple3 (next_pc, to_priv, mstatus_to_word (new_mstatus));
   endmethod

   // Read MINSTRET
   method Bit #(64) read_csr_minstret;
      return rg_minstret;
   endmethod

   // Increment MINSTRET
   method Action csr_minstret_incr;
      pw_minstret_incr.send;
   endmethod

   // Read MCYCLE
   method Bit #(64) read_csr_mcycle;
      return rg_mcycle;
   endmethod

   // Read MTIME
   method Bit #(64) read_csr_mtime;
      // We use mcycle as a proxy for time
      return rg_mcycle;
   endmethod

   // Fault on reading counters?
   method Bool csr_counter_read_fault (Priv_Mode  priv, CSR_Addr  csr_addr);
      return (   ((priv == s_Priv_Mode) || (priv == u_Priv_Mode))
	      && (   ((csr_addr == csr_cycle)   && (rg_mcounteren.cy == 0))
		  || ((csr_addr == csr_time)    && (rg_mcounteren.tm == 0))
		  || ((csr_addr == csr_instret) && (rg_mcounteren.ir == 0))
		  || ((csr_hpmcounter3  <= csr_addr) && (csr_addr <= csr_hpmcounter31))
`ifdef RV32
		  || ((csr_hpmcounter3h <= csr_addr) && (csr_addr <= csr_hpmcounter31h))
`endif
		  ));
   endmethod

   // Read MIP
   method MIP read_csr_mip;
      return rg_mip;
   endmethod

   // Interrupts
   method Action external_interrupt_req (Bool set_not_clear);
      f_ei_reqs.enq (set_not_clear);
      if (cfg_verbosity > 1)
	 $display ("%0d: CSR_RegFile: external_interrupt_req: %x", rg_mcycle, set_not_clear);
   endmethod

   method Action timer_interrupt_req (Bool set_not_clear);
      f_ti_reqs.enq (set_not_clear);
      if (cfg_verbosity > 1)
	 $display ("%0d: CSR_RegFile: timer_interrupt_req: %x", rg_mcycle, set_not_clear);
   endmethod

   method Action software_interrupt_req (Bool set_not_clear);
      f_si_reqs.enq (set_not_clear);
      if (cfg_verbosity > 1)
	 $display ("%0d: CSR_RegFile: software_interrupt_req: %x", rg_mcycle, set_not_clear);
   endmethod

   method Maybe #(Exc_Code) interrupt_pending (Priv_Mode cur_priv);
      return fn_interrupt_pending (misa,
				   mstatus_to_word (rg_mstatus),
				   mip_to_word     (rg_mip),
				   mie_to_word     (rg_mie),
				   rg_mideleg,
				   sideleg,
				   cur_priv);
   endmethod

   // WFI ignores mstatus ies and ideleg regs
   method Bool wfi_resume;
      WordXL mip_w = mip_to_word (rg_mip);
      WordXL mie_w = mie_to_word (rg_mie);
      return ((mip_w & mie_w) != 0);
   endmethod

   // ----------------
   // Methods when Debug Module is present

`ifdef INCLUDE_GDB_CONTROL
   // Read dpc
   method Word read_dpc ();
      return rg_dpc;
   endmethod

   // Update dpc
   method Action write_dpc (Addr pc);
      rg_dpc <= pc;
   endmethod

   // Break should enter Debug Mode
   method Bool dcsr_break_enters_debug (Priv_Mode cur_priv);
      return case (cur_priv)
		m_Priv_Mode: (rg_dcsr [15] == 1'b1);
		s_Priv_Mode: (rg_dcsr [13] == 1'b1);
		u_Priv_Mode: (rg_dcsr [12] == 1'b1);
	     endcase;
   endmethod

   // Read dcsr.step
   method Bool read_dcsr_step ();
      return unpack (rg_dcsr [2]);
   endmethod

   // Update 'cause' and 'priv' in DCSR
   method Action write_dcsr_cause_priv (DCSR_Cause  cause, Priv_Mode  priv);
      Bit #(3) b3 = pack (cause);
      rg_dcsr <= { rg_dcsr [31:9], b3, rg_dcsr [5:2], priv };
   endmethod

`endif

endmodule

// ================================================================

endpackage
