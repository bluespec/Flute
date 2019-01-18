// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CSR_RegFile_MSU;

// ================================================================
// CSR (Control and Status Register) Register File

// Supports Machine, User and Superviser CSRs.

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

import CSR_MSTATUS :: *;
import CSR_MIP     :: *;
import CSR_MIE     :: *;

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

   // CSR write (returning new value)
   (* always_ready *)
   method ActionValue #(WordXL) mav_csr_write (CSR_Addr csr_addr, WordXL word);

`ifdef ISA_F
   // Read FRM
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

`ifdef ISA_PRIV_S
   // Read SSTATUS
   (* always_ready *)
   method WordXL read_sstatus;
`endif

`ifdef ISA_PRIV_U
   // Read USTATUS
   (* always_ready *)
   method WordXL read_ustatus;
`endif

   // Read SATP
   (* always_ready *)
   method WordXL read_satp;

   // CSR trap actions
   method ActionValue #(Tuple4 #(Addr, Word, Word, Priv_Mode))
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

   // Access permission
   (* always_ready *)
   method Bool access_permitted_1 (Priv_Mode  priv, CSR_Addr  csr_addr, Bool  read_not_write);
   (* always_ready *)
   method Bool access_permitted_2 (Priv_Mode  priv, CSR_Addr  csr_addr, Bool  read_not_write);

   // Fault on reading counters?
   (* always_ready *)
   method Bool csr_counter_read_fault (Priv_Mode  priv, CSR_Addr  csr_addr);

   // Read MIP
   (* always_ready *)
   method WordXL csr_mip_read;

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

   // Update 'cause' in DCSR
   (* always_ready *)
   method Action write_dcsr_cause (DCSR_Cause cause);

`endif

   // ----------------
   // Debugging this module

   method Action debug;
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
   ms.mxl = misa_mxl_zero;
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

`ifdef ISA_F
   // Single- and Double-precision Floating Point
   ms.f = 1'b1;
`endif

`ifdef ISA_D
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
// MTVEC reset value (our choice; not part of the spec)

Word mtvec_reset_value = 'h1000;

// ================================================================
// Major states of mkCSR_RegFile module

typedef enum { RF_RESET_START, RF_RUNNING } RF_State
deriving (Eq, Bits, FShow);

// ================================================================

(* synthesize *)
module mkCSR_RegFile (CSR_RegFile_IFC);

   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (0);
   Reg #(RF_State) rg_state      <- mkReg (RF_RESET_START);

   // Reset
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   // CSRs
   // User-mode CSRs
`ifdef ISA_F
   Reg #(Bit #(5)) rg_fflags <- mkRegU;    // floating point flags
   Reg #(Bit #(3)) rg_frm    <- mkRegU;    // floating point rounding mode
`endif

   // Supervisor-mode CSRs
   Bit #(16)  sedeleg = 0;    // hardwired to 0
   Bit #(12)  sideleg = 0;    // hardwired to 0

`ifdef ISA_PRIV_S
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

   CSR_MSTATUS_IFC  csr_mstatus <- mkCSR_MSTATUS (misa_reset_value);

   MISA              misa          =  misa_reset_value;
   CSR_MIE_IFC       csr_mie       <- mkCSR_MIE;
   CSR_MIP_IFC       csr_mip       <- mkCSR_MIP;

   Reg #(MTVec)      rg_mtvec      <- mkRegU;
   Reg #(MCounteren) rg_mcounteren <- mkRegU;

   Reg #(Word)       rg_mscratch <- mkRegU;
   Reg #(Word)       rg_mepc     <- mkRegU;
   Reg #(MCause)     rg_mcause   <- mkRegU;
   Reg #(Word)       rg_mtval    <- mkRegU;

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
      // User-level CSRs
`ifdef ISA_F
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
      csr_mstatus.reset (misa_reset_value);
      csr_mie.reset;
      csr_mip.reset;

      rg_mtvec      <= word_to_mtvec (mtvec_reset_value);
      rg_mcause     <= word_to_mcause (0);    // Supposed to be the cause of the reset.
`ifdef ISA_PRIV_S
      rg_medeleg    <= 0;
      rg_mideleg    <= 0;
`endif
      rg_mcounteren <= mcounteren_reset_value;
      rg_tdata1     <= 0;    // ISA test rv64mi-p-breakpoint assumes reset value 0.

      rw_minstret.wset (0);

`ifdef INCLUDE_GDB_CONTROL
      // rg_dpc  <= pc_reset_value;    // Should be set by GDB
      rg_dcsr <= zeroExtend ({4'h4,    // xdebugver
			      12'h0,   // reserved
			      1'h1,    // ebreakm
			      1'h0,    // reserved
			      1'h1,    // ebreaks
			      1'h1,    // ebreaku
			      1'h0,    // stepie
			      1'h0,    // stepcount
			      1'h0,    // steptime
			      3'h0,    // cause    // WARNING: 0 is non-standard
			      3'h0,    // reserved
			      1'h0,    // step
			      2'h0}    // prv
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
   // Test if CSR is supported

   function Bool fv_csr_exists (CSR_Addr csr_addr);
      Bool result = (   ((csr_addr_hpmcounter3 <= csr_addr) && (csr_addr <= csr_addr_hpmcounter31))
		     || ((csr_addr_mhpmcounter3 <= csr_addr) && (csr_addr <= csr_addr_mhpmcounter31))
`ifdef RV32
		     || ((csr_addr_hpmcounter3h <= csr_addr) && (csr_addr <= csr_addr_hpmcounter31h))
		     || ((csr_addr_mhpmcounter3h <= csr_addr) && (csr_addr <= csr_addr_mhpmcounter31h))
`endif
		     || ((csr_addr_mhpmevent3 <= csr_addr) && (csr_addr <= csr_addr_mhpmevent31))

		     // User mode csrs
`ifdef ISA_F
		     || (csr_addr == csr_addr_fflags)
		     || (csr_addr == csr_addr_frm)
		     || (csr_addr == csr_addr_fcsr)
`endif
		     || (csr_addr == csr_addr_cycle)

		     /*
		     // NOTE: CSR_TIME should be a 'shadow copy' of the MTIME
		     // mem-mapped location; but since both increment at the
		     // same rate, and MTIME is never written, this is ok.

		     || (csr_addr == csr_addr_time)
		     */

		     || (csr_addr == csr_addr_instret)
`ifdef RV32
		     || (csr_addr == csr_addr_cycleh)
		     || (csr_addr == csr_addr_timeh)
		     || (csr_addr == csr_addr_instreth)
`endif

`ifdef ISA_PRIV_S
		     // Supervisor mode csrs
		     || (csr_addr == csr_addr_sstatus)
		     || (csr_addr == csr_addr_sedeleg)
		     || (csr_addr == csr_addr_sideleg)
		     || (csr_addr == csr_addr_sie)
		     || (csr_addr == csr_addr_stvec)
		     || (csr_addr == csr_addr_scounteren)

		     || (csr_addr == csr_addr_sscratch)
		     || (csr_addr == csr_addr_sepc)
		     || (csr_addr == csr_addr_scause)
		     || (csr_addr == csr_addr_stval)
		     || (csr_addr == csr_addr_sip)

		     || (csr_addr == csr_addr_satp)

		     || (csr_addr == csr_addr_medeleg)
		     || (csr_addr == csr_addr_mideleg)
`endif

		     // Machine mode csrs
		     || (csr_addr == csr_addr_mvendorid)
		     || (csr_addr == csr_addr_marchid)
		     || (csr_addr == csr_addr_mimpid)
		     || (csr_addr == csr_addr_mhartid)

		     || (csr_addr == csr_addr_mstatus)
		     || (csr_addr == csr_addr_misa)
		     || (csr_addr == csr_addr_mie)
		     || (csr_addr == csr_addr_mtvec)
		     || (csr_addr == csr_addr_mcounteren)

		     || (csr_addr == csr_addr_mscratch)
		     || (csr_addr == csr_addr_mepc)
		     || (csr_addr == csr_addr_mcause)
		     || (csr_addr == csr_addr_mtval)
		     || (csr_addr == csr_addr_mip)

		     // TODO: Phys Mem Protection regs
		     // (csr_addr == csr_pmpcfg0)
		     // (csr_addr == csr_pmpcfg1)
		     // (csr_addr == csr_pmpcfg2)
		     // (csr_addr == csr_pmpcfg3)

		     // (csr_addr == csr_pmpaddr0)
		     // (csr_addr == csr_pmpaddr1)
		     // (csr_addr == csr_pmpaddr2)
		     // (csr_addr == csr_pmpaddr3)
		     // (csr_addr == csr_pmpaddr4)
		     // (csr_addr == csr_pmpaddr5)
		     // (csr_addr == csr_pmpaddr6)
		     // (csr_addr == csr_pmpaddr7)
		     // (csr_addr == csr_pmpaddr8)
		     // (csr_addr == csr_pmpaddr9)
		     // (csr_addr == csr_pmpaddr10)
		     // (csr_addr == csr_pmpaddr11)
		     // (csr_addr == csr_pmpaddr12)
		     // (csr_addr == csr_pmpaddr13)
		     // (csr_addr == csr_pmpaddr14)
		     // (csr_addr == csr_pmpaddr15)

		     || (csr_addr == csr_addr_mcycle)
		     || (csr_addr == csr_addr_minstret)
`ifdef RV32
		     || (csr_addr == csr_addr_mcycleh)
		     || (csr_addr == csr_addr_minstreth)
`endif

		     || (csr_addr == csr_addr_tselect)
		     || (csr_addr == csr_addr_tdata1)
		     || (csr_addr == csr_addr_tdata2)
		     || (csr_addr == csr_addr_tdata3)

`ifdef INCLUDE_GDB_CONTROL
		     || (csr_addr == csr_addr_dcsr)
		     || (csr_addr == csr_addr_dpc)
		     || (csr_addr == csr_addr_dscratch0)
		     || (csr_addr == csr_addr_dscratch1)
`endif
	 );
      return result;
   endfunction: fv_csr_exists

   // ----------------
   // CSR reads (no side effect)
   // Returns Invalid for invalid CSR addresses or access-mode violations

   function Maybe #(Word) fv_csr_read (CSR_Addr csr_addr);
      Maybe #(Word)  m_csr_value = tagged Invalid;

      if ((csr_addr_hpmcounter3 <= csr_addr) && (csr_addr <= csr_addr_hpmcounter31))
	 m_csr_value = tagged Valid 0;
`ifdef RV32
      else if ((csr_addr_hpmcounter3h <= csr_addr) && (csr_addr <= csr_addr_hpmcounter31h))
	 m_csr_value = tagged Valid 0;
`endif
      else if ((csr_addr_mhpmcounter3 <= csr_addr) && (csr_addr <= csr_addr_mhpmcounter31))
	 m_csr_value = tagged Valid 0;
`ifdef RV32
      else if ((csr_addr_mhpmcounter3h <= csr_addr) && (csr_addr <= csr_addr_mhpmcounter31h))
	 m_csr_value = tagged Valid 0;
`endif
      else if ((csr_addr_mhpmevent3 <= csr_addr) && (csr_addr <= csr_addr_mhpmevent31))
	 m_csr_value = tagged Valid 0;

      else begin
	 case (csr_addr)
	    // User mode csrs
`ifdef ISA_F
	    csr_addr_fflags:   m_csr_value = tagged Valid ({ 0, rg_fflags });
	    csr_addr_frm:      m_csr_value = tagged Valid ({ 0, rg_frm });
	    csr_addr_fcsr:     m_csr_value = tagged Valid ({ 0, rg_frm, rg_fflags });
`endif
	    csr_addr_cycle:    m_csr_value = tagged Valid (truncate (rg_mcycle));

	    /*
	    // NOTE: CSR_TIME should be a 'shadow copy' of the MTIME
	    // mem-mapped location; but since both increment at the
	    // same rate, and MTIME is never written, this is ok.

	    csr_addr_time:     m_csr_value = tagged Valid (truncate (rg_mcycle));
	    */

	    csr_addr_instret:  m_csr_value = tagged Valid (truncate (rg_minstret));
`ifdef RV32
	    csr_addr_cycleh:   m_csr_value = tagged Valid (rg_mcycle   [63:32]);
	    csr_addr_timeh:    m_csr_value = tagged Invalid;
	    csr_addr_instreth: m_csr_value = tagged Valid (rg_minstret [63:32]);
`endif

`ifdef ISA_PRIV_S
	    // Supervisor mode csrs
	    csr_addr_sstatus:    m_csr_value = tagged Valid (csr_mstatus.fv_sstatus_read);
	    csr_addr_sedeleg:    m_csr_value = tagged Valid zeroExtend (sedeleg);
	    csr_addr_sideleg:    m_csr_value = tagged Valid zeroExtend (sideleg);
	    csr_addr_sie:        m_csr_value = tagged Valid (csr_mie.fv_sie_read);
	    csr_addr_stvec:      m_csr_value = tagged Valid (mtvec_to_word (rg_stvec));
	    csr_addr_scounteren: m_csr_value = tagged Valid 0;

	    csr_addr_sscratch:   m_csr_value = tagged Valid rg_sscratch;
	    csr_addr_sepc:       m_csr_value = tagged Valid rg_sepc;
	    csr_addr_scause:     m_csr_value = tagged Valid (mcause_to_word (rg_scause));
	    csr_addr_stval:      m_csr_value = tagged Valid rg_stval;
	    csr_addr_sip:        m_csr_value = tagged Valid (csr_mip.fv_sip_read);

	    csr_addr_satp:       m_csr_value = tagged Valid rg_satp;

	    csr_addr_medeleg:    m_csr_value = tagged Valid zeroExtend (rg_medeleg);
	    csr_addr_mideleg:    m_csr_value = tagged Valid zeroExtend (rg_mideleg);
`endif

	    // Machine mode csrs
	    csr_addr_mvendorid:  m_csr_value = tagged Valid mvendorid;
	    csr_addr_marchid:    m_csr_value = tagged Valid marchid;
	    csr_addr_mimpid:     m_csr_value = tagged Valid mimpid;
	    csr_addr_mhartid:    m_csr_value = tagged Valid mhartid;

	    csr_addr_mstatus:    m_csr_value = tagged Valid (csr_mstatus.fv_read);
	    csr_addr_misa:       m_csr_value = tagged Valid (misa_to_word (misa));
	    csr_addr_mie:        m_csr_value = tagged Valid (csr_mie.fv_read);
	    csr_addr_mtvec:      m_csr_value = tagged Valid (mtvec_to_word (rg_mtvec));
	    csr_addr_mcounteren: m_csr_value = tagged Valid (mcounteren_to_word (rg_mcounteren));

	    csr_addr_mscratch:   m_csr_value = tagged Valid rg_mscratch;
	    csr_addr_mepc:       m_csr_value = tagged Valid rg_mepc;
	    csr_addr_mcause:     m_csr_value = tagged Valid (mcause_to_word (rg_mcause));
	    csr_addr_mtval:      m_csr_value = tagged Valid rg_mtval;
	    csr_addr_mip:        m_csr_value = tagged Valid (csr_mip.fv_read);

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

	    csr_addr_mcycle:    m_csr_value = tagged Valid (truncate (rg_mcycle));
	    csr_addr_minstret:  m_csr_value = tagged Valid (truncate (rg_minstret));
`ifdef RV32
	    csr_addr_mcycleh:   m_csr_value = tagged Valid (rg_mcycle [63:32]);
	    csr_addr_minstreth: m_csr_value = tagged Valid (rg_minstret [63:32]);
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

   // ----------------------------------------------------------------
   // CSR writes

   function ActionValue #(WordXL) fav_csr_write (CSR_Addr csr_addr, WordXL wordxl);
      actionvalue
	 Bool    success = True;
	 WordXL  result  = 0;

	 if ((csr_addr_mhpmcounter3 <= csr_addr) && (csr_addr <= csr_addr_mhpmcounter31))
	    noAction;
`ifdef RV32
	 else if ((csr_addr_mhpmcounter3h <= csr_addr) && (csr_addr <= csr_addr_mhpmcounter31h))
	    noAction;
`endif
	 else if ((csr_addr_mhpmevent3 <= csr_addr) && (csr_addr <= csr_addr_mhpmevent31))
	    noAction;
	 else
	    case (csr_addr)
	       // User mode csrs
`ifdef ISA_F
	       csr_addr_fflags:     begin
				       result     = zeroExtend (wordxl [4:0]);
				       rg_fflags <= wordxl [4:0];
				    end
	       csr_addr_frm:        begin
				       result  = zeroExtend (wordxl [2:0]);
				       rg_frm <= wordxl [2:0];
				    end
	       csr_addr_fcsr:       begin
				       result     = zeroExtend (wordxl [7:0]);
				       rg_fflags <= wordxl [4:0];
				       rg_frm    <= wordxl [7:5];
				    end
`endif

`ifdef ISA_PRIV_S
	       csr_addr_sstatus:    begin
				       result <- csr_mstatus.fav_sstatus_write (misa, wordxl);
				    end
	       csr_addr_sedeleg:    noAction;               // Hardwired to 0 (no delegation)
	       csr_addr_sideleg:    noAction;               // Hardwired to 0 (no delegation)
	       csr_addr_sie:        begin
				       result <- csr_mie.fav_sie_write (misa, wordxl);
				    end
	       csr_addr_stvec:      begin
				       let mtvec = word_to_mtvec (wordxl);
				       result    = mtvec_to_word (mtvec);
				       rg_stvec <= mtvec;
				    end
	       csr_addr_scounteren: noAction;

	       csr_addr_sscratch:   begin
				       result       = wordxl;
				       rg_sscratch <= result;
				    end
	       csr_addr_sepc:       begin
				       result       = wordxl;
				       rg_sepc     <= result;
				    end
	       csr_addr_scause:     begin
				       let mcause   = word_to_mcause (wordxl);
				       result       = mcause_to_word (mcause);
				       rg_scause   <= mcause;
				    end
	       csr_addr_stval:      begin
				       result    = wordxl;
				       rg_stval <= result;
				    end
	       csr_addr_sip:        begin
				       result <- csr_mip.fav_sip_write (misa, wordxl);
				    end
	       csr_addr_satp:       begin
				       result   = wordxl;
				       rg_satp <= result;
				    end
	       csr_addr_medeleg:    begin
				       result      = (wordxl & 'h_B3FF);  // 16 bits relevant and some are 0
				       rg_medeleg <= truncate (result);
				    end
	       csr_addr_mideleg:    begin
				       result      = (wordxl & 'h_0FFF);  // 12 bits relevant
				       rg_mideleg <= truncate (result);
				    end
`endif

	       // Machine mode
	       csr_addr_mvendorid:  noAction;
	       csr_addr_marchid:    noAction;
	       csr_addr_mimpid:     noAction;
	       csr_addr_mhartid:    noAction;
	       csr_addr_mstatus:    begin
				      result <- csr_mstatus.fav_write (misa, wordxl);
				    end
	       csr_addr_misa:       noAction;
	       csr_addr_mie:        begin
				       result <- csr_mie.fav_write (misa, wordxl);
				    end
	       csr_addr_mtvec:      begin
				       let mtvec = word_to_mtvec (wordxl);
				       result    = mtvec_to_word (mtvec);
				       rg_mtvec <= mtvec;
				    end
	       csr_addr_mcounteren: begin
				       let mcounteren = word_to_mcounteren(wordxl);
				       result         = mcounteren_to_word (mcounteren);
				       rg_mcounteren <= mcounteren;
				    end
	       csr_addr_mscratch:   begin
				       result       = wordxl;
				       rg_mscratch <= result;
				    end
	       csr_addr_mepc:       begin
				       result   = wordxl;
				       rg_mepc <= result;
				    end
	       csr_addr_mcause:     begin
				       let mcause = word_to_mcause (wordxl);
				       result     = mcause_to_word (mcause);
				       rg_mcause <= mcause;
				    end
	       csr_addr_mtval:      begin
				       result    = wordxl;
				       rg_mtval <= result;
				    end
	       csr_addr_mip:        begin
				       result <- csr_mip.fav_write (misa, wordxl);
				    end
	       // TODO: PMPs
	       // csr_pmpcfg0:   rf_pmpcfg.upd (0, wordxl);
	       // csr_pmpcfg1:   rf_pmpcfg.upd (1, wordxl);
	       // csr_pmpcfg2:   rf_pmpcfg.upd (2, wordxl);
	       // csr_pmpcfg3:   rf_pmpcfg.upd (3, wordxl);

	       // csr_pmpaddr0:  vrg_pmpaddr [0] <= wordxl;
	       // csr_pmpaddr1:  vrg_pmpaddr [1] <= wordxl;
	       // csr_pmpaddr2:  vrg_pmpaddr [2] <= wordxl;
	       // csr_pmpaddr3:  vrg_pmpaddr [3] <= wordxl;
	       // csr_pmpaddr4:  vrg_pmpaddr [4] <= wordxl;
	       // csr_pmpaddr5:  vrg_pmpaddr [5] <= wordxl;
	       // csr_pmpaddr6:  vrg_pmpaddr [6] <= wordxl;
	       // csr_pmpaddr7:  vrg_pmpaddr [7] <= wordxl;
	       // csr_pmpaddr8:  vrg_pmpaddr [8] <= wordxl;
	       // csr_pmpaddr9:  vrg_pmpaddr [9] <= wordxl;
	       // csr_pmpaddr10: vrg_pmpaddr [10] <= wordxl;
	       // csr_pmpaddr11: vrg_pmpaddr [11] <= wordxl;
	       // csr_pmpaddr12: vrg_pmpaddr [12] <= wordxl;
	       // csr_pmpaddr13: vrg_pmpaddr [13] <= wordxl;
	       // csr_pmpaddr14: vrg_pmpaddr [14] <= wordxl;
	       // csr_pmpaddr15: vrg_pmpaddr [15] <= wordxl;

`ifdef RV32
	       csr_addr_mcycle:     begin
				       result = wordxl;
				       rw_mcycle.wset   ({ rg_mcycle   [63:32], wordxl });
				    end
	       csr_addr_minstret:   begin
				       result = wordxl;
				       rw_minstret.wset ({ rg_minstret [63:32], wordxl });
				    end
	       csr_addr_mcycleh:    begin
				       result = wordxl;
				       rw_mcycle.wset   ({ wordxl, rg_mcycle   [31:0] });
				    end
	       csr_addr_minstreth:  begin
				       result = wordxl;
				       rw_minstret.wset ({ wordxl, rg_minstret [31:0] });
				    end
`else
	       csr_addr_mcycle:     begin
				       result = wordxl;
				       rw_mcycle.wset (result);
				    end
	       csr_addr_minstret:   begin
				       result = wordxl;
				       rw_minstret.wset (result);
				    end
`endif
	       csr_addr_tselect:   begin
				      result      = wordxl;
				      rg_tselect <= result;
				   end
	       csr_addr_tdata1:    begin
				      result     = wordxl;
				      rg_tdata1 <= result;
				   end
	       csr_addr_tdata2:    begin
				      result     = wordxl;
				      rg_tdata2 <= result;
				   end
	       csr_addr_tdata3:    begin
				      result     = wordxl;
				      rg_tdata3 <= result;
				   end

`ifdef INCLUDE_GDB_CONTROL
	       csr_addr_dcsr:       begin
				       Bit #(32) new_dcsr = {// xdebugver: read-only
							     rg_dcsr [31:28],
							     // ebreakm/s/u, stepie, stopcount, stoptime
							     wordxl [27:9],
							     // cause: read-only
							     rg_dcsr [8:6],
							     // step, prv
							     wordxl [5:0]};
				       result   = zeroExtend (new_dcsr);
				       rg_dcsr <= new_dcsr;
				    end
	       csr_addr_dpc:        begin
				       result  = wordxl;
				       rg_dpc <= result;
				    end
	       csr_addr_dscratch0:  begin
				       result        = wordxl;
				       rg_dscratch0 <= result;
				    end
	       csr_addr_dscratch1:  begin
				       result        = wordxl;
				       rg_dscratch1 <= result;
				    end
`endif

	       default: success = False;
	    endcase

	 if ((! success) && (cfg_verbosity > 1))
	    $display ("%0d: ERROR: CSR-write addr 0x%0h val 0x%0h not successful", rg_mcycle,
		      csr_addr, wordxl);

	 return result;
      endactionvalue
   endfunction

   // Access permission
   function Bool fv_access_permitted (Priv_Mode  priv, CSR_Addr  csr_addr,  Bool read_not_write);
      Bool exists  = fv_csr_exists (csr_addr);    // Is this CSR implemented?

      Bool priv_ok = priv >= csr_addr [9:8];      // Accessible at current privilege?

      // TVM fault: cannot access SATP if MSTATUS.TVM is set
      Bool tvm_fault = ((csr_addr == csr_addr_satp) && (csr_mstatus.fv_read [mstatus_tvm_bitpos] == 1'b1));

      // TODO: MxDELEG fault: MIDELEG and MEDELEG do not exist in
      //     systems with only m_Priv and systems with m_Priv and u_Priv but
      //     without support for U-mode traps

      Bool rw_ok = (read_not_write || (csr_addr [11:10] != 2'b11));

      return (exists && priv_ok && (! tvm_fault) && rw_ok);
   endfunction

   // ================================================================
   // For debugging

   function Action fa_show_trap_csrs (Priv_Mode priv,
				      WordXL ip, WordXL ie,
				      Bit #(16) edeleg, Bit #(12) ideleg,
				      MCause cause, WordXL status, MTVec tvec,
				      WordXL epc, WordXL tval);
      action
	 $write ("    priv %0d: ", priv);
	 $write (" ip: 0x%0h", ip);
	 $write (" ie: 0x%0h", ie);
	 $write (" edeleg: 0x%0h", edeleg);
	 $write (" ideleg: 0x%0h", ideleg);
	 $write (" cause:", fshow (cause));
	 $display ("");

	 $write ("        ");
	 $write (" status: 0x%0h", status);
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
   method ActionValue #(WordXL) mav_csr_write (CSR_Addr csr_addr, WordXL word);
      let result <- fav_csr_write (csr_addr, word);
      return result;
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
      return  csr_mstatus.fv_read;
   endmethod

`ifdef ISA_PRIV_S
   // Read SSTATUS
   method WordXL read_sstatus;
      return  csr_mstatus.fv_sstatus_read;
   endmethod
`endif

`ifdef ISA_PRIV_U
   // Read USTATUS
   method WordXL read_ustatus;
      return  csr_mstatus.fv_ustatus_read;
   endmethod
`endif

   // Read SATP
   method WordXL read_satp;
`ifdef ISA_PRIV_S
      return  rg_satp;
`else
      return  ?;
`endif
   endmethod

   // CSR Trap actions
   method ActionValue #(Tuple4 #(Addr,          // new PC
				 WordXL,        // new mstatus
				 WordXL,        // new mcause
				 Priv_Mode))    // new priv
          csr_trap_actions (Priv_Mode  from_priv,
			    WordXL     pc,
			    Bool       interrupt,
			    Exc_Code   exc_code,
			    WordXL     xtval);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: CSR_Regfile.csr_trap_actions:", rg_mcycle);
	 $display ("    from priv %0d  pc 0x%0h  interrupt %0d  exc_code %0d  xtval 0x%0h",
		   from_priv, pc, pack (interrupt), exc_code, xtval);
`ifdef ISA_PRIV_S
	 fa_show_trap_csrs (s_Priv_Mode, csr_mip.fv_read, csr_mie.fv_read, 0, 0, rg_scause,
			    csr_mstatus.fv_sstatus_read,
			    rg_stvec, rg_sepc, rg_stval);
`endif
	 fa_show_trap_csrs (m_Priv_Mode, csr_mip.fv_read, csr_mie.fv_read, rg_medeleg, rg_mideleg, rg_mcause,
			    csr_mstatus.fv_read,
			    rg_mtvec, rg_mepc, rg_mtval);
      end

      let new_priv    = fv_new_priv_on_exception (misa,
						  from_priv,
						  interrupt,
						  exc_code,
						  rg_medeleg,
						  rg_mideleg,
						  sedeleg,
						  sideleg);
      let new_mstatus  = fv_new_mstatus_on_exception (csr_mstatus.fv_read, from_priv, new_priv);
      let new_status  <- csr_mstatus.fav_write (misa, new_mstatus);

      let  xcause      = MCause {interrupt: pack (interrupt), exc_code: exc_code};
      let  is_vectored = (rg_mtvec.mode == VECTORED);
      Addr exc_pc      = (extend (rg_mtvec.base)) << 2;

      if (new_priv == m_Priv_Mode) begin
	 rg_mepc    <= pc;
	 rg_mcause  <= xcause;
	 rg_mtval   <= xtval;
      end
`ifdef ISA_PRIV_S
      else if (new_priv == s_Priv_Mode) begin
	 rg_sepc    <= pc;
	 rg_scause  <= xcause;
	 rg_stval   <= xtval;

	 is_vectored = (rg_stvec.mode == VECTORED);
	 new_status  = fv_mstatus_to_sstatus (new_status);
	 exc_pc      = (extend (rg_stvec.base)) << 2;
      end
`endif
      // TODO: if (new_priv == u_Priv_Mode)

      // Compute the exception PC based on the xTVEC mode bits
      Addr vector_offset = (extend (exc_code)) << 2;
      if (interrupt && is_vectored)
	 exc_pc = exc_pc + vector_offset;

      if (cfg_verbosity > 1) begin
	 $write ("    Return: new pc 0x%0h  ", exc_pc);
	 $write (" new mstatus:", fshow_mstatus (misa, new_status));
	 $write (" new xcause:", fshow (xcause));
	 $write (" new priv %0d", new_priv);
	 $display ("");
      end

      return tuple4 (exc_pc,                       // New PC
		     new_status,                   // New mstatus/sstatus/ustatus
		     mcause_to_word  (xcause),     // New mcause
		     new_priv);                    // New priv
   endmethod: csr_trap_actions

   // CSR RET actions (return from exception)
   method ActionValue #(Tuple3 #(Addr, Priv_Mode, Word)) csr_ret_actions (Priv_Mode from_priv);
      match { .new_mstatus, .to_priv } = fv_new_mstatus_on_ret (misa, csr_mstatus.fv_read, from_priv);
      csr_mstatus.fa_write (misa, new_mstatus);
      WordXL next_pc = rg_mepc;
`ifdef ISA_PRIV_S
      if (from_priv != m_Priv_Mode)
	 next_pc = rg_sepc;
`endif
      return tuple3 (next_pc, to_priv, new_mstatus);
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

   // Access permission
   method Bool access_permitted_1 (Priv_Mode  priv, CSR_Addr  csr_addr,  Bool read_not_write);
      return fv_access_permitted (priv, csr_addr, read_not_write);
   endmethod

   method Bool access_permitted_2 (Priv_Mode  priv, CSR_Addr  csr_addr,  Bool read_not_write);
      return fv_access_permitted (priv, csr_addr, read_not_write);
   endmethod

   // Fault on reading counters?
   method Bool csr_counter_read_fault (Priv_Mode  priv, CSR_Addr  csr_addr);
      return (   ((priv == s_Priv_Mode) || (priv == u_Priv_Mode))
	      && (   ((csr_addr == csr_addr_cycle)   && (rg_mcounteren.cy == 0))
		  || ((csr_addr == csr_addr_time)    && (rg_mcounteren.tm == 0))
		  || ((csr_addr == csr_addr_instret) && (rg_mcounteren.ir == 0))
		  || ((csr_addr_hpmcounter3  <= csr_addr) && (csr_addr <= csr_addr_hpmcounter31))
`ifdef RV32
		  || ((csr_addr_hpmcounter3h <= csr_addr) && (csr_addr <= csr_addr_hpmcounter31h))
`endif
		  ));
   endmethod

   // Read MIP
   method WordXL csr_mip_read;
      return csr_mip.fv_read;
   endmethod

   // Interrupts
   method Action external_interrupt_req (Bool set_not_clear);
      csr_mip.external_interrupt_req  (set_not_clear);     
      if (cfg_verbosity > 1)
	 $display ("%0d: CSR_RegFile: external_interrupt_req: %x", rg_mcycle, set_not_clear);
   endmethod

   method Action timer_interrupt_req (Bool set_not_clear);
      csr_mip.timer_interrupt_req  (set_not_clear);     
      if (cfg_verbosity > 1)
	 $display ("%0d: CSR_RegFile: timer_interrupt_req: %x", rg_mcycle, set_not_clear);
   endmethod

   method Action software_interrupt_req (Bool set_not_clear);
      csr_mip.software_interrupt_req (set_not_clear);
      if (cfg_verbosity > 1)
	 $display ("%0d: CSR_RegFile: software_interrupt_req: %x", rg_mcycle, set_not_clear);
   endmethod

   method Maybe #(Exc_Code) interrupt_pending (Priv_Mode cur_priv);
      return fv_interrupt_pending (misa,
				   csr_mstatus.fv_read,
				   csr_mip.fv_read,
				   csr_mie.fv_read,
				   rg_mideleg,
				   sideleg,
				   cur_priv);
   endmethod

   // WFI ignores mstatus ies and ideleg regs
   method Bool wfi_resume;
      WordXL mip = csr_mip.fv_read;
      WordXL mie = csr_mie.fv_read;
      return ((mip & mie) != 0);
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

   // Update 'cause' in DCSR
   method Action write_dcsr_cause (DCSR_Cause cause);
      Bit #(3) b3 = pack (cause);
      rg_dcsr <= { rg_dcsr [31:9], b3, rg_dcsr [5:0] };
   endmethod

`endif

   // ----------------
   // Debugging this module

   method Action debug;
      $display ("mstatus = 0x%0h", csr_mstatus.fv_read);
`ifdef ISA_PRIV_S
      $display ("sstatus = 0x%0h", csr_mstatus.fv_sstatus_read);
`endif
      $display ("mip     = 0x%0h", csr_mip.fv_read);
`ifdef ISA_PRIV_S
      $display ("sip     = 0x%0h", csr_mip.fv_sip_read);
`endif
      $display ("mie     = 0x%0h", csr_mie.fv_read);
`ifdef ISA_PRIV_S
      $display ("sie     = 0x%0h", csr_mie.fv_sie_read);
`endif
   endmethod      
endmodule

// ================================================================

endpackage
