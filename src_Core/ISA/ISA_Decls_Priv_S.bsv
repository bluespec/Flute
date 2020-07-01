// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

// ================================================================
// WARNING: this is an 'include' file, not a separate BSV package!
//
// Contains RISC-V Supervisor-Level ISA defs, based on:
//     The RISC-V Instruction Set Manual"
//     Volume II: Privileged Architecture
//     Privileged Architecture Version 1.10
//     Document Version 1.10
//     May 7, 2017
//
// ================================================================

// Invariants on ifdefs:
// - If RV32 is defined, we assume Sv32 for the VM system
// - If RV64 is defined, one of SV39 or SV48 must also be defined for the VM system

// ================================================================
// Supervisor-level CSRs

CSR_Addr   csr_addr_sstatus    = 12'h100;    // Supervisor status
CSR_Addr   csr_addr_sedeleg    = 12'h102;    // Supervisor exception delegation
CSR_Addr   csr_addr_sideleg    = 12'h103;    // Supervisor interrupt delegation
CSR_Addr   csr_addr_sie        = 12'h104;    // Supervisor interrupt enable
CSR_Addr   csr_addr_stvec      = 12'h105;    // Supervisor trap handler base address
CSR_Addr   csr_addr_scounteren = 12'h106;    // Supervisor counter enable

CSR_Addr   csr_addr_sscratch   = 12'h140;    // Scratch reg for supervisor trap handlers
CSR_Addr   csr_addr_sepc       = 12'h141;    // Supervisor exception program counter
CSR_Addr   csr_addr_scause     = 12'h142;    // Supervisor trap cause
CSR_Addr   csr_addr_stval      = 12'h143;    // Supervisor bad address or instruction
CSR_Addr   csr_addr_sip        = 12'h144;    // Supervisor interrupt pending

CSR_Addr   csr_addr_satp       = 12'h180;    // Supervisor address translation and protection

// ================================================================
// SSTATUS

function Bit #(1) fn_sstatus_sd    (WordXL sstatus_val); return sstatus_val [xlen-1]; endfunction

`ifdef RV64
function Bit #(2) fn_sstatus_UXL   (WordXL sstatus_val); return sstatus_val [33:32]; endfunction
`endif

function Bit #(1) fn_sstatus_SUM   (WordXL sstatus_val); return sstatus_val [19]; endfunction
function Bit #(1) fn_sstatus_MXR   (WordXL sstatus_val); return sstatus_val [18]; endfunction

function Bit #(2) fn_sstatus_xs    (WordXL sstatus_val); return sstatus_val [16:15]; endfunction
function Bit #(2) fn_sstatus_fs    (WordXL sstatus_val); return sstatus_val [14:13]; endfunction

function Bit #(1) fn_sstatus_spp   (WordXL sstatus_val); return sstatus_val [8]; endfunction

function Bit #(1) fn_sstatus_spie  (WordXL sstatus_val); return sstatus_val [5]; endfunction
function Bit #(1) fn_sstatus_upie  (WordXL sstatus_val); return sstatus_val [4]; endfunction

function Bit #(1) fn_sstatus_sie   (WordXL sstatus_val); return sstatus_val [1]; endfunction
function Bit #(1) fn_sstatus_uie   (WordXL sstatus_val); return sstatus_val [0]; endfunction

// ----------------
// SCAUSE (reason for exception)

function Bit #(1)              scause_interrupt      (WordXL scause_val); return scause_val [xlen-1];   endfunction
function Bit #(TSub #(XLEN,1)) scause_exception_code (WordXL scause_val); return scause_val [xlen-2:0]; endfunction

// ================================================================

`ifdef ISA_PRIV_S
// ================================================================
// SATP (supervisor address translation and protection)

// ----------------
`ifdef RV32

typedef Bit #(1) VM_Mode;
typedef Bit #(9) ASID;

function WordXL   fn_mk_satp_val (VM_Mode mode, ASID asid, PA pa) = { mode, asid, pa [33:12] };
function VM_Mode  fn_satp_to_VM_Mode  (Bit #(32) satp_val); return satp_val    [31]; endfunction
function ASID     fn_satp_to_ASID     (Bit #(32) satp_val); return satp_val [30:22]; endfunction
function PPN      fn_satp_to_PPN      (Bit #(32) satp_val); return satp_val [21: 0]; endfunction

Bit #(1)  satp_mode_RV32_bare = 1'h_0;
Bit #(1)  satp_mode_RV32_sv32 = 1'h_1;

`elsif RV64

typedef Bit #(4)  VM_Mode;
typedef Bit #(16) ASID;

function WordXL   fn_mk_satp_val (VM_Mode mode, ASID asid, PA pa) = { mode, asid, pa [55:12] };
function VM_Mode  fn_satp_to_VM_Mode  (Bit #(64) satp_val); return satp_val [63:60]; endfunction
function ASID     fn_satp_to_ASID     (Bit #(64) satp_val); return satp_val [59:44]; endfunction
function PPN      fn_satp_to_PPN      (Bit #(64) satp_val); return satp_val [43: 0]; endfunction

Bit #(4)  satp_mode_RV64_bare = 4'd__0;
Bit #(4)  satp_mode_RV64_sv39 = 4'd__8;
Bit #(4)  satp_mode_RV64_sv48 = 4'd__9;
Bit #(4)  satp_mode_RV64_sv57 = 4'd_10;
Bit #(4)  satp_mode_RV64_sv64 = 4'd_11;

`endif

// ----------------------------------------------------------------
// Virtual and Physical addresses, page numbers, offsets
// Page table (PT) fields and entries (PTEs)
// For Sv32 and Sv39


// ----------------
// RV32.Sv32

`ifdef RV32

// Virtual addrs
typedef  32  VA_sz;
typedef  20  VPN_sz;
typedef  10  VPN_J_sz;

// Physical addrs
typedef  34  PA_sz;
typedef  22  PPN_sz;
typedef  12  PPN_1_sz;
typedef  10  PPN_0_sz;

// Offsets within a page
typedef  12  Offset_sz;

// PTNodes (nodes in the page-table tree)
typedef  1024  PTNode_sz;    // # of PTEs in a PTNode

// VAs, VPN selectors
function VA    fn_mkVA (VPN_J vpn1, VPN_J vpn0, Bit #(Offset_sz) offset) = { vpn1, vpn0, offset };
function VPN   fn_Addr_to_VPN   (Bit #(n) addr) = addr [31:12];
function VPN_J fn_Addr_to_VPN_1 (Bit #(n) addr) = addr [31:22];
function VPN_J fn_Addr_to_VPN_0 (Bit #(n) addr) = addr [21:12];

// ----------------
// RV64.Sv39

// ifdef RV32
`elsif RV64

// ----------------
// RV64.Sv39

// ifdef RV32 .. elsif RV64
`ifdef SV39

// Virtual addrs
typedef   39  VA_sz;
typedef   27  VPN_sz;
typedef    9  VPN_J_sz;

// Physical addrs
typedef   64  PA_sz;        // need 56b in Sv39 mode and 64b in Bare mode
typedef   44  PPN_sz;
typedef   26  PPN_2_sz;
typedef    9  PPN_1_sz;
typedef    9  PPN_0_sz;

// Offsets within a page
typedef   12  Offset_sz;

// PTNodes (nodes in the page-table tree)
typedef  512  PTNode_sz;    // # of PTEs in a PTNode

// VAs, VPN selectors
function VA    fn_mkVA (VPN_J vpn2, VPN_J vpn1, VPN_J vpn0, Bit #(Offset_sz) offset) = { vpn2, vpn1, vpn0, offset };
function VPN   fn_Addr_to_VPN   (Bit #(n) addr) = addr [38:12];
function VPN_J fn_Addr_to_VPN_2 (Bit #(n) addr) = addr [38:30];
function VPN_J fn_Addr_to_VPN_1 (Bit #(n) addr) = addr [29:21];
function VPN_J fn_Addr_to_VPN_0 (Bit #(n) addr) = addr [20:12];

// ifdef RV32 .. elsif RV64 / ifdef SV39
`else

// TODO: RV64.SV48 definitions

// ifdef RV32 .. elsif RV64 / ifdef SV39 .. else
`endif

// ifdef RV32 .. elsif RV64
`endif

// ----------------
// Derived types and values

// Physical addrs
Integer  pa_sz = valueOf (PA_sz);  typedef Bit #(PA_sz)     PA;

function PA fn_WordXL_to_PA (WordXL  eaddr);
`ifdef RV32
   return extend (eaddr);
`elsif RV64
   return truncate (eaddr);
`endif
endfunction

// Virtual addrs -- derived types and values
Integer  va_sz = valueOf (VA_sz);  typedef Bit #(VA_sz)      VA;

function VA fn_WordXL_to_VA (WordXL  eaddr);
`ifdef RV32
   return eaddr;
`elsif RV64
   return truncate (eaddr);
`endif
endfunction

// Page offsets
function  Offset  fn_Addr_to_Offset (Bit #(n) addr);
   return addr [offset_sz - 1: 0];
endfunction

// VPNs
Integer  vpn_sz    = valueOf (VPN_sz);       typedef Bit #(VPN_sz)     VPN;
Integer  vpn_j_sz  = valueOf (VPN_J_sz);     typedef Bit #(VPN_J_sz)   VPN_J;
Integer  offset_sz = valueOf (Offset_sz);    typedef Bit #(Offset_sz)  Offset;

// PPNs
Integer  ppn_sz   = valueOf (PPN_sz);    typedef Bit #(PPN_sz)    PPN;
`ifdef RV64
Integer  ppn_2_sz = valueOf (PPN_2_sz);  typedef Bit #(PPN_2_sz)  PPN_2;
`endif
Integer  ppn_1_sz = valueOf (PPN_1_sz);  typedef Bit #(PPN_1_sz)  PPN_1;
Integer  ppn_0_sz = valueOf (PPN_0_sz);  typedef Bit #(PPN_0_sz)  PPN_0;

`ifdef RV32
typedef Bit #(PPN_1_sz)  PPN_MEGA;
`elsif RV64
typedef Bit #(TAdd #(PPN_2_sz, PPN_1_sz))  PPN_MEGA;
typedef Bit #(PPN_2_sz)                    PPN_GIGA;
`endif

function  PPN  fn_PA_to_PPN (PA pa);
   return pa [ppn_sz + offset_sz - 1: offset_sz];
endfunction

function PA fn_PPN_and_Offset_to_PA (PPN ppn, Offset offset);
`ifdef RV32
   return {ppn, offset};
`elsif RV64
   return zeroExtend ({ppn, offset});
`endif
endfunction

// ----------------
// PTNodes (nodes in the page-table tree)

Integer  ptnode_sz = valueOf (PTNode_sz);    // # of PTEs in a PTNode
typedef  TLog #(PTNode_sz)       PTNode_Index_sz;
typedef  Bit #(PTNode_Index_sz)  PTNode_Index;
Integer  ptnode_index_sz = valueOf (PTNode_Index_sz);

// ----------------
// PTEs (Page Table Entries in PTNodes)

typedef WordXL PTE;

Integer  pte_V_offset    = 0;    // Valid
Integer  pte_R_offset    = 1;    // Read permission
Integer  pte_W_offset    = 2;    // Write permission
Integer  pte_X_offset    = 3;    // Execute permission
Integer  pte_U_offset    = 4;    // Accessible-to-user-mode
Integer  pte_G_offset    = 5;    // Global mapping
Integer  pte_A_offset    = 6;    // Accessed
Integer  pte_D_offset    = 7;    // Dirty
Integer  pte_RSW_offset  = 8;    // Reserved for supervisor SW

`ifdef RV32
Integer  pte_PPN_0_offset  = 10;
Integer  pte_PPN_1_offset  = 20;
`elsif RV64
Integer  pte_PPN_0_offset  = 10;
Integer  pte_PPN_1_offset  = 19;
Integer  pte_PPN_2_offset  = 28;
`endif

function Bit #(1) fn_PTE_to_V (PTE pte);
   return pte [pte_V_offset];
endfunction

function Bit #(1) fn_PTE_to_R (PTE pte);
   return pte [pte_R_offset];
endfunction

function Bit #(1) fn_PTE_to_W (PTE pte);
   return pte [pte_W_offset];
endfunction

function Bit #(1) fn_PTE_to_X (PTE pte);
   return pte [pte_X_offset];
endfunction

function Bit #(1) fn_PTE_to_U (PTE pte);
   return pte [pte_U_offset];
endfunction

function Bit #(1) fn_PTE_to_G (PTE pte);
   return pte [pte_G_offset];
endfunction

function Bit #(1) fn_PTE_to_A (PTE pte);
   return pte [pte_A_offset];
endfunction

function Bit #(1) fn_PTE_to_D (PTE pte);
   return pte [pte_D_offset];
endfunction

function PPN fn_PTE_to_PPN (PTE pte);
   return pte [ppn_sz + pte_PPN_0_offset - 1 : pte_PPN_0_offset];
endfunction

function PPN_MEGA  fn_PTE_to_PPN_mega (PTE pte);
   return pte [ppn_sz + pte_PPN_0_offset - 1 : pte_PPN_1_offset];
endfunction

`ifdef RV64
function PPN_GIGA  fn_PTE_to_PPN_giga (PTE pte);
   return pte [ppn_sz + pte_PPN_0_offset - 1 : pte_PPN_2_offset];
endfunction
`endif

function PPN_0  fn_PTE_to_PPN_0 (PTE pte);
   return pte [pte_PPN_1_offset - 1 : pte_PPN_0_offset];
endfunction

function PPN_1  fn_PTE_to_PPN_1 (PTE pte);
   return pte [ppn_1_sz + pte_PPN_1_offset - 1 : pte_PPN_1_offset];
endfunction

`ifdef RV64
function PPN_2  fn_PTE_to_PPN_2 (PTE pte);
   return pte [ppn_2_sz + pte_PPN_2_offset - 1 : pte_PPN_2_offset];
endfunction
`endif

// ----------------
// Check if a PTE is invalid (V bit clear, or improper R/W bits)

function Bool is_invalid_pte (PTE pte);
   return (   (fn_PTE_to_V (pte) == 0)
	   || (   (fn_PTE_to_R (pte) == 0)
	       && (fn_PTE_to_W (pte) == 1)));
endfunction

// ----------------
// Check if PTE bits deny a virtual-mem access

function Bool is_pte_denial (Bool       dmem_not_imem,        // load-store or fetch?
			     Bool       read_not_write,
			     Priv_Mode  priv,
			     Bit #(1)   sstatus_SUM,
			     Bit #(1)   mstatus_MXR,
			     PTE        pte);

   let pte_u = fn_PTE_to_U (pte);
   let pte_x = fn_PTE_to_X (pte);
   let pte_w = fn_PTE_to_W (pte);
   let pte_r = fn_PTE_to_R (pte);

   Bool priv_deny = (   ((priv == u_Priv_Mode) && (pte_u == 1'b0))
		     || ((priv == s_Priv_Mode) && (pte_u == 1'b1) && (sstatus_SUM == 1'b0)));

   Bool access_fetch = ((! dmem_not_imem) && read_not_write);
   Bool access_load  = (dmem_not_imem && read_not_write);
   Bool access_store = (dmem_not_imem && (! read_not_write));

   let pte_r_mxr = (pte_r | (mstatus_MXR & pte_x));

   Bool access_ok = (   (access_fetch && (pte_x     == 1'b1))
		     || (access_load  && (pte_r_mxr == 1'b1))
		     || (access_store && (pte_w     == 1'b1)));

   
   return (priv_deny || (! access_ok));
endfunction

// ----------------
// Check PTE A and D bits
// Note: implementation choice to fault on PTA.A=0 or (store and PTE.D=0)

function Bool is_pte_A_D_fault (Bool read_not_write, PTE pte);
   return (   (fn_PTE_to_A (pte) == 0)
	   || ((! read_not_write) && (fn_PTE_to_D (pte) == 0)));
endfunction

// ----------------
// Choose particular kind of page fault

function Exc_Code  fn_page_fault_exc_code (Bool dmem_not_imem, Bool read_not_write);
   return ((! dmem_not_imem) ? exc_code_INSTR_PAGE_FAULT
	   :(read_not_write  ? exc_code_LOAD_PAGE_FAULT
	     :                 exc_code_STORE_AMO_PAGE_FAULT));
endfunction   

`else // ifdef ISA_PRIV_S
// The below definitions are valid for cases where there is no VM
// Physical addrs -- without VM, PA is same as WordXL
typedef XLEN PA_sz;

// Physical addrs
Integer  pa_sz = valueOf (PA_sz);  typedef Bit #(PA_sz)     PA;

function PA fn_WordXL_to_PA (WordXL  eaddr);
   return eaddr;
endfunction

`endif   // else-ifdef ISA_PRIV_S

// ----------------
// Choose particular kind of access fault

function Exc_Code  fn_access_exc_code (Bool dmem_not_imem, Bool read_not_write);
   return ((! dmem_not_imem) ? exc_code_INSTR_ACCESS_FAULT
	   :(read_not_write  ? exc_code_LOAD_ACCESS_FAULT
	     :                 exc_code_STORE_AMO_ACCESS_FAULT));
endfunction

// ================================================================
