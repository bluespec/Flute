// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved

// ================================================================
// This is an 'include' file, not a separate BSV package
//
// Contains RISC-V Machine-Level ISA defs
//
// ================================================================

// ================================================================
// Utility functions

// In these functions, 'bitpos' is Bit #(6) which is enough to index
// 64-bit words in RV64.

function Bit #(n) fv_assign_bit (Bit #(n) x, Bit #(6) bitpos, Bit #(1) b)
   provisos (Add #(a__, 1, n));
   Bit #(n) mask = (1          << bitpos);
   Bit #(n) val  = (extend (b) << bitpos);
   return ((x & (~ mask)) | val);
endfunction

function Bit #(n) fv_assign_bits (Bit #(n) x, Bit #(6) bitpos, Bit #(w) bs)
   provisos (Add #(a__, w, n));
   Bit #(n) mask = (((1 << valueOf (w)) - 1) << bitpos);
   Bit #(n) val  = (extend (bs)              << bitpos);
   return ((x & (~ mask)) | val);
endfunction

function Bit #(w) fv_get_bits (Bit #(n) x, Bit #(6) bitpos)
   provisos (Add #(a__, w, n));
   Bit #(n) mask = ((1 << valueOf (w)) - 1);
   return truncate ((x >> bitpos) & mask);
endfunction

// ================================================================
// Machine-level CSRs

CSR_Addr   csr_addr_mvendorid      = 12'hF11;    // Vendor ID
CSR_Addr   csr_addr_marchid        = 12'hF12;    // Architecture ID
CSR_Addr   csr_addr_mimpid         = 12'hF13;    // Implementation ID
CSR_Addr   csr_addr_mhartid        = 12'hF14;    // Hardware thread ID

CSR_Addr   csr_addr_mstatus        = 12'h300;    // Machine status
CSR_Addr   csr_addr_misa           = 12'h301;    // ISA and extensions
CSR_Addr   csr_addr_medeleg        = 12'h302;    // Machine exception delegation
CSR_Addr   csr_addr_mideleg        = 12'h303;    // Machine interrupt delegation
CSR_Addr   csr_addr_mie            = 12'h304;    // Machine interrupt-enable
CSR_Addr   csr_addr_mtvec          = 12'h305;    // Machine trap handler base address
CSR_Addr   csr_addr_mcounteren     = 12'h306;    // Machine counter enable

CSR_Addr   csr_addr_mscratch       = 12'h340;    // Scratch reg for machine trap handlers
CSR_Addr   csr_addr_mepc           = 12'h341;    // Machine exception program counter
CSR_Addr   csr_addr_mcause         = 12'h342;    // Machine trap cause
CSR_Addr   csr_addr_mtval          = 12'h343;    // Machine bad address
CSR_Addr   csr_addr_mip            = 12'h344;    // Machine interrupt pending

CSR_Addr   csr_addr_pmpcfg0        = 12'h3A0;    // PMP Config
CSR_Addr   csr_addr_pmpcfg1        = 12'h3A1;    // PMP Config
CSR_Addr   csr_addr_pmpcfg2        = 12'h3A2;    // PMP Config
CSR_Addr   csr_addr_pmpcfg3        = 12'h3A3;    // PMP Config
CSR_Addr   csr_addr_pmpaddr0       = 12'h3B0;    // PMP address register
CSR_Addr   csr_addr_pmpaddr1       = 12'h3B1;    // PMP address register
CSR_Addr   csr_addr_pmpaddr2       = 12'h3B2;    // PMP address register
CSR_Addr   csr_addr_pmpaddr3       = 12'h3B3;    // PMP address register
CSR_Addr   csr_addr_pmpaddr4       = 12'h3B4;    // PMP address register
CSR_Addr   csr_addr_pmpaddr5       = 12'h3B5;    // PMP address register
CSR_Addr   csr_addr_pmpaddr6       = 12'h3B6;    // PMP address register
CSR_Addr   csr_addr_pmpaddr7       = 12'h3B7;    // PMP address register
CSR_Addr   csr_addr_pmpaddr8       = 12'h3B8;    // PMP address register
CSR_Addr   csr_addr_pmpaddr9       = 12'h3B9;    // PMP address register
CSR_Addr   csr_addr_pmpaddr10      = 12'h3BA;    // PMP address register
CSR_Addr   csr_addr_pmpaddr11      = 12'h3BB;    // PMP address register
CSR_Addr   csr_addr_pmpaddr12      = 12'h3BC;    // PMP address register
CSR_Addr   csr_addr_pmpaddr13      = 12'h3BD;    // PMP address register
CSR_Addr   csr_addr_pmpaddr14      = 12'h3BE;    // PMP address register
CSR_Addr   csr_addr_pmpaddr15      = 12'h3BF;    // PMP address register

CSR_Addr   csr_addr_mcycle         = 12'hB00;    // Machine cycle counter
CSR_Addr   csr_addr_minstret       = 12'hB02;    // Machine Instructions retired counter

CSR_Addr   csr_addr_mhpmcounter3   = 12'hB03;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter4   = 12'hB04;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter5   = 12'hB05;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter6   = 12'hB06;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter7   = 12'hB07;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter8   = 12'hB08;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter9   = 12'hB09;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter10  = 12'hB0A;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter11  = 12'hB0B;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter12  = 12'hB0C;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter13  = 12'hB0D;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter14  = 12'hB0E;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter15  = 12'hB0F;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter16  = 12'hB10;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter17  = 12'hB11;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter18  = 12'hB12;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter19  = 12'hB13;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter20  = 12'hB14;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter21  = 12'hB15;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter22  = 12'hB16;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter23  = 12'hB17;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter24  = 12'hB18;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter25  = 12'hB19;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter26  = 12'hB1A;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter27  = 12'hB1B;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter28  = 12'hB1C;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter29  = 12'hB1D;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter30  = 12'hB1E;    // Machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter31  = 12'hB1F;    // Machine performance-monitoring counter

CSR_Addr   csr_addr_mcycleh        = 12'hB80;    // Upper 32 bits of csr_mcycle (RV32I only)
CSR_Addr   csr_addr_minstreth      = 12'hB82;    // Upper 32 bits of csr_minstret (RV32I only)

CSR_Addr   csr_addr_mhpmcounter3h  = 12'hB83;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter4h  = 12'hB84;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter5h  = 12'hB85;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter6h  = 12'hB86;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter7h  = 12'hB87;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter8h  = 12'hB88;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter9h  = 12'hB89;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter10h = 12'hB8A;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter11h = 12'hB8B;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter12h = 12'hB8C;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter13h = 12'hB8D;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter14h = 12'hB8E;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter15h = 12'hB8F;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter16h = 12'hB90;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter17h = 12'hB91;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter18h = 12'hB92;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter19h = 12'hB93;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter20h = 12'hB94;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter21h = 12'hB95;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter22h = 12'hB96;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter23h = 12'hB97;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter24h = 12'hB98;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter25h = 12'hB99;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter26h = 12'hB9A;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter27h = 12'hB9B;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter28h = 12'hB9C;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter29h = 12'hB9D;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter30h = 12'hB9E;    // Upper 32 bits of machine performance-monitoring counter
CSR_Addr   csr_addr_mhpmcounter31h = 12'hB9F;    // Upper 32 bits of machine performance-monitoring counter

CSR_Addr   csr_addr_mhpmevent3     = 12'h323;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent4     = 12'h324;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent5     = 12'h325;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent6     = 12'h326;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent7     = 12'h327;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent8     = 12'h328;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent9     = 12'h329;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent10    = 12'h32A;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent11    = 12'h32B;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent12    = 12'h32C;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent13    = 12'h32D;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent14    = 12'h32E;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent15    = 12'h32F;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent16    = 12'h330;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent17    = 12'h331;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent18    = 12'h332;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent19    = 12'h333;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent20    = 12'h334;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent21    = 12'h335;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent22    = 12'h336;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent23    = 12'h337;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent24    = 12'h338;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent25    = 12'h339;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent26    = 12'h33A;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent27    = 12'h33B;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent28    = 12'h33C;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent29    = 12'h33D;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent30    = 12'h33E;    // Machine performance-monitoring event selector
CSR_Addr   csr_addr_mhpmevent31    = 12'h33F;    // Machine performance-monitoring event selector

CSR_Addr   csr_addr_tselect   = 12'h7A0;    // Debug/Trace trigger register select
CSR_Addr   csr_addr_tdata1    = 12'h7A1;    // First Debug/Trace trigger data
CSR_Addr   csr_addr_tdata2    = 12'h7A2;    // Secont Debug/Trace trigger data
CSR_Addr   csr_addr_tdata3    = 12'h7A3;    // Third Debug/Trace trigger data

CSR_Addr   csr_addr_dcsr      = 12'h7B0;    // Debug control and status
CSR_Addr   csr_addr_dpc       = 12'h7B1;    // Debug PC
CSR_Addr   csr_addr_dscratch0 = 12'h7B2;    // Debug scratch0
CSR_Addr   csr_addr_dscratch1 = 12'h7B3;    // Debug scratch1

// ================================================================
// MISA

typedef struct {
   Bit #(2) mxl;
   Bit #(1) z;  Bit #(1) y;
   Bit #(1) x;  Bit #(1) w;  Bit #(1) v;  Bit #(1) u;  Bit #(1) t;  Bit #(1) s;  Bit #(1) r;  Bit #(1) q;
   Bit #(1) p;  Bit #(1) o;  Bit #(1) n;  Bit #(1) m;  Bit #(1) l;  Bit #(1) k;  Bit #(1) j;  Bit #(1) i;
   Bit #(1) h;  Bit #(1) g;  Bit #(1) f;  Bit #(1) e;  Bit #(1) d;  Bit #(1) c;  Bit #(1) b;  Bit #(1) a;
   } MISA
deriving (Bits);

Bit #(2) misa_mxl_zero  = 0;
Bit #(2) misa_mxl_32    = 1;
Bit #(2) misa_mxl_64    = 2;
Bit #(2) misa_mxl_128   = 3;

function WordXL misa_to_word (MISA ms);
   return {ms.mxl,
	   0,        // expands appropriately for RV32 and RV64
	   ms.z, ms.y,
	   ms.x, ms.w, ms.v, ms.u, ms.t, ms.s, ms.r, ms.q,
	   ms.p, ms.o, ms.n, ms.m, ms.l, ms.k, ms.j, ms.i,
	   ms.h, ms.g, ms.f, ms.e, ms.d, ms.c, ms.b, ms.a};
endfunction

function MISA word_to_misa (WordXL x);
   return MISA {mxl: x [xlen-1:xlen-2],
		z: x [25], y: x [24],
		x: x [23], w: x [22], v: x [21], u: x [20], t: x [19], s: x [18], r: x [17], q: x [16],
		p: x [15], o: x [14], n: x [13], m: x [12], l: x [11], k: x [10], j: x  [9], i: x  [8],
		h: x  [7], g: x  [6], f: x  [5], e: x  [4], d: x  [3], c: x  [2], b: x  [1], a: x  [0]};
endfunction

instance FShow #(MISA);
   function Fmt fshow (MISA misa);
      let fmt_mxl = case (misa.mxl)
			1: $format ("mxl 32");
			2: $format ("mxl 64");
			3: $format ("mxl 128");
			default: $format ("mxl unknown %0d", misa.mxl);
		     endcase;
      return (  fmt_mxl
	      + $format ((misa.z == 1'b1) ? "Z" : "")
	      + $format ((misa.y == 1'b1) ? "Y" : "")
	      + $format ((misa.x == 1'b1) ? "X" : "")
	      + $format ((misa.w == 1'b1) ? "W" : "")
	      + $format ((misa.v == 1'b1) ? "V" : "")
	      + $format ((misa.u == 1'b1) ? "U" : "")
	      + $format ((misa.t == 1'b1) ? "T" : "")
	      + $format ((misa.s == 1'b1) ? "S" : "")
	      + $format ((misa.r == 1'b1) ? "R" : "")
	      + $format ((misa.q == 1'b1) ? "Q" : "")
	      + $format ((misa.p == 1'b1) ? "P" : "")
	      + $format ((misa.o == 1'b1) ? "O" : "")
	      + $format ((misa.n == 1'b1) ? "N" : "")
	      + $format ((misa.m == 1'b1) ? "M" : "")
	      + $format ((misa.l == 1'b1) ? "L" : "")
	      + $format ((misa.k == 1'b1) ? "K" : "")
	      + $format ((misa.j == 1'b1) ? "J" : "")
	      + $format ((misa.i == 1'b1) ? "I" : "")
	      + $format ((misa.h == 1'b1) ? "H" : "")
	      + $format ((misa.g == 1'b1) ? "G" : "")
	      + $format ((misa.f == 1'b1) ? "F" : "")
	      + $format ((misa.d == 1'b1) ? "E" : "")
	      + $format ((misa.d == 1'b1) ? "D" : "")
	      + $format ((misa.c == 1'b1) ? "C" : "")
	      + $format ((misa.b == 1'b1) ? "B" : "")
	      + $format ((misa.a == 1'b1) ? "A" : ""));
   endfunction
endinstance

// ================================================================
// MSTATUS

Integer mstatus_sd_bitpos      = xlen - 1;

Integer mstatus_sxl_bitpos     = 34;
Integer mstatus_uxl_bitpos     = 32;

Integer mstatus_tsr_bitpos     = 22;
Integer mstatus_tw_bitpos      = 21;
Integer mstatus_tvm_bitpos     = 20;

Integer mstatus_mxr_bitpos     = 19;
Integer mstatus_sum_bitpos     = 18;
Integer mstatus_mprv_bitpos    = 17;

Integer mstatus_xs_bitpos      = 15;
Integer mstatus_fs_bitpos      = 13;

Integer mstatus_mpp_bitpos     = 11;
Integer mstatus_WPRI_9_bitpos  =  9;
Integer mstatus_spp_bitpos     =  8;

Integer mstatus_mpie_bitpos    =  7;
Integer mstatus_WPRI_6_bitpos  =  6;
Integer mstatus_spie_bitpos    =  5;
Integer mstatus_upie_bitpos    =  4;

Integer mstatus_mie_bitpos     =  3;
Integer mstatus_WPRI_2_bitpos  =  2;
Integer mstatus_sie_bitpos     =  1;
Integer mstatus_uie_bitpos     =  0;

// Values for FS and XS

Bit #(2) fs_xs_off      = 2'h0;
Bit #(2) fs_xs_initial  = 2'h1;
Bit #(2) fs_xs_clean    = 2'h2;
Bit #(2) fs_xs_dirty    = 2'h3;

// Extract MSTATUS.FS field
function Bit #(2) fv_mstatus_fs (WordXL mstatus);
   return (fv_get_bits (mstatus, fromInteger (mstatus_fs_bitpos)));
endfunction

// Virtual field SD is computed from FS and XS
function Bit #(1) fv_mstatus_sd (WordXL  mstatus);
   Bit #(2) xs = fv_get_bits (mstatus, fromInteger (mstatus_xs_bitpos));
   Bit #(2) fs = fv_get_bits (mstatus, fromInteger (mstatus_fs_bitpos));
   return (((fs == fs_xs_dirty) || (xs == fs_xs_dirty)) ? 1 : 0);
endfunction

function Fmt fshow_mstatus (MISA  misa, WordXL  mstatus);
   Bit #(2) sxl = ((misa.mxl == misa_mxl_64) ? fv_get_bits (mstatus, fromInteger (mstatus_sxl_bitpos)) : 0);
   Bit #(2) uxl = ((misa.mxl == misa_mxl_64) ? fv_get_bits (mstatus, fromInteger (mstatus_uxl_bitpos)) : 0);
   Bit #(2) xs  = fv_get_bits (mstatus, fromInteger (mstatus_xs_bitpos));
   Bit #(2) fs  = fv_get_bits (mstatus, fromInteger (mstatus_fs_bitpos));
   Bit #(2) mpp = fv_get_bits (mstatus, fromInteger (mstatus_mpp_bitpos));

   return (  $format ("MStatus{")
	   + $format ("sd:%0d", fv_mstatus_sd (mstatus))

	   + ((misa.mxl == misa_mxl_64) ? $format (" sxl:%0d uxl:%0d", sxl, uxl) : $format (""))

	   + $format (" tsr:%0d",  mstatus [mstatus_tsr_bitpos])
	   + $format (" tw:%0d",   mstatus [mstatus_tw_bitpos])
	   + $format (" tvm:%0d",  mstatus [mstatus_tvm_bitpos])
	   + $format (" mxr:%0d",  mstatus [mstatus_mxr_bitpos])
	   + $format (" sum:%0d",  mstatus [mstatus_sum_bitpos])
	   + $format (" mprv:%0d", mstatus [mstatus_mprv_bitpos])

	   + $format (" xs:%0d",   xs)
	   + $format (" fs:%0d",   fs)

	   + $format (" mpp:%0d",  mpp)
	   + $format (" spp:%0d",  mstatus [mstatus_spp_bitpos])

	   + $format (" pies:%0d_%0d%0d",
		      mstatus [mstatus_mpie_bitpos], mstatus [mstatus_spie_bitpos], mstatus [mstatus_upie_bitpos])

	   + $format (" ies:%0d_%0d%0d",
		      mstatus [mstatus_mie_bitpos], mstatus [mstatus_sie_bitpos], mstatus [mstatus_uie_bitpos])
	   + $format ("}")
	   );
endfunction

// ----------------
// Help functions to manipulate mstatus on traps and trap-returns

function Priv_Mode fv_new_priv_on_exception (MISA       misa,
					     Priv_Mode  from_priv,
					     Bool       interrupt,
					     Exc_Code   exc_code,
					     Bit #(16)  medeleg,
					     Bit #(12)  mideleg,
					     Bit #(16)  sedeleg,
					     Bit #(12)  sideleg);
   Priv_Mode to_priv = m_Priv_Mode;
   Bit #(1) deleg_bit = 1'b0;

   // If the current priv mode is M, it cannot be delegated.
   if (from_priv < m_Priv_Mode) begin
      // If S is supported
      if (misa.s == 1'b1) begin
	 // Look in medeleg/mideleg for the cause bit; if set, delegate.
	 if (interrupt)
	    deleg_bit = mideleg [exc_code];
	 else
	    deleg_bit = medeleg [exc_code];
	 if (deleg_bit == 1'b1) begin
	    // If the current priv mode is S, then delegate to S.
	    to_priv = s_Priv_Mode;
	    // If the current priv mode is U, and user mode traps are supported,
	    // then consult sedeleg/sideleg to determine if delegated to U mode.
	    if ((from_priv == u_Priv_Mode) && (misa.n == 1'b1)) begin
	       if (interrupt)
		  deleg_bit = sideleg [exc_code];
	       else
		  deleg_bit = sedeleg [exc_code];
	       if (deleg_bit == 1'b1)
	          to_priv = u_Priv_Mode;
	    end
	 end
      end
      else begin
	 // S is not supported
	 // If user mode traps are supported,
	 // then consult medele/mideleg to determine if delegated to U mode.
	 if (misa.n == 1'b1) begin
	    // Look in medeleg/mideleg for the cause bit; if set, delegate.
	    if (interrupt)
	       deleg_bit = mideleg [exc_code];
	    else
	       deleg_bit = medeleg [exc_code];
	    if (deleg_bit == 1'b1)
	       to_priv = u_Priv_Mode;
	 end
      end
   end

   return to_priv;
endfunction

function WordXL fv_new_mstatus_on_exception (WordXL mstatus, Priv_Mode from_y, Priv_Mode to_x);
   Bit #(6) ie_to_x  = extend (to_x);
   Bit #(6) pie_to_x = fromInteger (mstatus_upie_bitpos) + extend (to_x);
   // xPIE = xIE
   mstatus = fv_assign_bit (mstatus, pie_to_x, mstatus [ie_to_x]);
   // xIE = 0
   mstatus = fv_assign_bit (mstatus, ie_to_x, 1'b0);

   // xPP = y        Assert: (to_x == m_Priv_Mode) || (to_x == s_Priv_Mode)
   mstatus = (  (to_x == m_Priv_Mode)
	      ? fv_assign_bits (mstatus, fromInteger (mstatus_mpp_bitpos), from_y)
	      : fv_assign_bit (mstatus, fromInteger (mstatus_spp_bitpos), from_y [0]));
   return mstatus;
endfunction

function Tuple2 #(WordXL, Priv_Mode) fv_new_mstatus_on_ret (MISA       misa,
							    WordXL     mstatus,
							    Priv_Mode  from_x);
   Bit #(6) ie_from_x  = extend (from_x);
   Bit #(6) pie_from_x = fromInteger (mstatus_upie_bitpos) + extend (from_x);

   // Pop the interrupt-enable stack
   // (set xIE = xPIE)
   mstatus = fv_assign_bit (mstatus, ie_from_x, mstatus [pie_from_x]);

   // Enable interrupt at from_x
   // (set xPIE = 1)
   mstatus = fv_assign_bit (mstatus, pie_from_x, 1'b1);

   // Pop the previous privilege mode
   // which empties the one-element stack, revealing the default value
   // (set xPP to U -- or M if U is not supported)
   Priv_Mode to_y;
   Priv_Mode default_pp = ((misa.u == 1'b1) ? u_Priv_Mode : m_Priv_Mode);
   if (from_x == m_Priv_Mode) begin
      to_y = fv_get_bits (mstatus, fromInteger (mstatus_mpp_bitpos));
      mstatus = fv_assign_bits (mstatus, fromInteger (mstatus_mpp_bitpos), default_pp);
   end
   else begin //if (from_x == s_Priv_Mode)
      to_y = {1'b0, mstatus [mstatus_spp_bitpos]};
      mstatus = fv_assign_bit (mstatus, fromInteger (mstatus_spp_bitpos), default_pp [0]);
   end

   return tuple2 (mstatus, to_y);
endfunction

// ================================================================
// Logical view of csr_mtvec register

typedef enum {DIRECT, VECTORED} MTVEC_Mode
deriving (Bits, Eq, FShow);

typedef struct {
   Bit #(XLEN_MINUS_2) base;
   MTVEC_Mode mode;
} MTVec
deriving (Bits, FShow);

function WordXL mtvec_to_word (MTVec mv);
   return {mv.base,
           1'b0,
           pack (mv.mode)};
endfunction

function MTVec word_to_mtvec (WordXL x);
   return MTVec {base: truncate (x >> 2),
                 mode: unpack (x[0])};
endfunction

// ================================================================
// Logical view of csr_mcounteren register
typedef struct {
   Bit#(1) ir;
   Bit#(1) tm;
   Bit#(1) cy;
} MCounteren
deriving (Bits, FShow);

function WordXL mcounteren_to_word (MCounteren mc);
   return {0,
           mc.ir,
	   mc.tm,
	   mc.cy};
endfunction

function MCounteren word_to_mcounteren (WordXL x);
   return MCounteren {ir: x[2],
                      tm: x[1],
		      cy: x[0]};
endfunction

function MCounteren mcounteren_reset_value;
   return MCounteren {ir: 1'b0,
                      tm: 1'b0,
		      cy: 1'b0};
endfunction

// ================================================================
// MIP and MIE fields (interrupt pending, interrupt enable)

Integer mip_usip_bitpos =  0;
Integer mip_ssip_bitpos =  1;
Integer mip_msip_bitpos =  3;

Integer mip_utip_bitpos =  4;
Integer mip_stip_bitpos =  5;
Integer mip_mtip_bitpos =  7;

Integer mip_ueip_bitpos =  8;
Integer mip_seip_bitpos =  9;
Integer mip_meip_bitpos = 11;

// ================================================================
// MCAUSE (reason for exception)

typedef struct {
   Bit #(1)  interrupt;
   Exc_Code  exc_code;
   } MCause
deriving (Bits);

instance FShow #(MCause);
   function Fmt fshow (MCause mc);
      if (mc.interrupt == 1)
	 return fshow_interrupt_Exc_Code (mc.exc_code);
      else
	 return fshow_trap_Exc_Code (mc.exc_code);
   endfunction
endinstance

function WordXL mcause_to_word (MCause mc);
   return {mc.interrupt, 0, mc.exc_code};
endfunction

function MCause word_to_mcause (WordXL x);
   return MCause {interrupt: msb (x),
		  exc_code:  truncate (x)};
endfunction

// Exception Codes in mcause

typedef Bit #(4) Exc_Code;

// When Interrupt = 1 (interrupt)

Exc_Code  exc_code_USER_SW_INTERRUPT             = 0;
Exc_Code  exc_code_SUPERVISOR_SW_INTERRUPT       = 1;
Exc_Code  exc_code_HYPERVISOR_SW_INTERRUPT       = 2;
Exc_Code  exc_code_MACHINE_SW_INTERRUPT          = 3;

Exc_Code  exc_code_USER_TIMER_INTERRUPT          = 4;
Exc_Code  exc_code_SUPERVISOR_TIMER_INTERRUPT    = 5;
Exc_Code  exc_code_HYPERVISOR_TIMER_INTERRUPT    = 6;
Exc_Code  exc_code_MACHINE_TIMER_INTERRUPT       = 7;

Exc_Code  exc_code_USER_EXTERNAL_INTERRUPT       = 8;
Exc_Code  exc_code_SUPERVISOR_EXTERNAL_INTERRUPT = 9;
Exc_Code  exc_code_HYPERVISOR_EXTERNAL_INTERRUPT = 10;
Exc_Code  exc_code_MACHINE_EXTERNAL_INTERRUPT    = 11;

// When Interrupt = 0 (trap)

Exc_Code  exc_code_INSTR_ADDR_MISALIGNED         = 0;
Exc_Code  exc_code_INSTR_ACCESS_FAULT            = 1;
Exc_Code  exc_code_ILLEGAL_INSTRUCTION           = 2;
Exc_Code  exc_code_BREAKPOINT                    = 3;

Exc_Code  exc_code_LOAD_ADDR_MISALIGNED          = 4;
Exc_Code  exc_code_LOAD_ACCESS_FAULT             = 5;

Exc_Code  exc_code_STORE_AMO_ADDR_MISALIGNED     = 6;
Exc_Code  exc_code_STORE_AMO_ACCESS_FAULT        = 7;

Exc_Code  exc_code_ECALL_FROM_U                  = 8;
Exc_Code  exc_code_ECALL_FROM_S                  = 9;
Exc_Code  exc_code_RESERVED_10                   = 10;
Exc_Code  exc_code_ECALL_FROM_M                  = 11;

Exc_Code  exc_code_INSTR_PAGE_FAULT              = 12;
Exc_Code  exc_code_LOAD_PAGE_FAULT               = 13;
Exc_Code  exc_code_RESERVED_14                   = 14;
Exc_Code  exc_code_STORE_AMO_PAGE_FAULT          = 15;


function Fmt fshow_interrupt_Exc_Code (Exc_Code exc_code);
   return case (exc_code)
	     exc_code_USER_SW_INTERRUPT:             $format ("USER_SW_INTERRUPT");
	     exc_code_SUPERVISOR_SW_INTERRUPT:       $format ("SUPERVISOR_SW_INTERRUPT");
	     exc_code_HYPERVISOR_SW_INTERRUPT:       $format ("HYPERVISOR_SW_INTERRUPT");
	     exc_code_MACHINE_SW_INTERRUPT:          $format ("MACHINE_SW_INTERRUPT");

	     exc_code_USER_TIMER_INTERRUPT:          $format ("USER_TIMER_INTERRUPT");
	     exc_code_SUPERVISOR_TIMER_INTERRUPT:    $format ("SUPERVISOR_TIMER_INTERRUPT");
	     exc_code_HYPERVISOR_TIMER_INTERRUPT:    $format ("HYPERVISOR_TIMER_INTERRUPT");
	     exc_code_MACHINE_TIMER_INTERRUPT:       $format ("MACHINE_TIMER_INTERRUPT");

	     exc_code_USER_EXTERNAL_INTERRUPT:       $format ("USER_EXTERNAL_INTERRUPT");
	     exc_code_SUPERVISOR_EXTERNAL_INTERRUPT: $format ("SUPERVISOR_EXTERNAL_INTERRUPT");
	     exc_code_HYPERVISOR_EXTERNAL_INTERRUPT: $format ("HYPERVISOR_EXTERNAL_INTERRUPT");
	     exc_code_MACHINE_EXTERNAL_INTERRUPT:    $format ("MACHINE_EXTERNAL_INTERRUPT");
	     default:                                $format ("unknown interrupt Exc_Code %d", exc_code);
	  endcase;
endfunction

function Fmt fshow_trap_Exc_Code (Exc_Code exc_code);
   return case (exc_code)
	     exc_code_INSTR_ADDR_MISALIGNED:      $format ("INSTRUCTION_ADDR_MISALIGNED");
	     exc_code_INSTR_ACCESS_FAULT:         $format ("INSTRUCTION_ACCESS_FAULT");
	     exc_code_ILLEGAL_INSTRUCTION:        $format ("ILLEGAL_INSTRUCTION");
	     exc_code_BREAKPOINT:                 $format ("BREAKPOINT");

	     exc_code_LOAD_ADDR_MISALIGNED:       $format ("LOAD_ADDR_MISALIGNED");
	     exc_code_LOAD_ACCESS_FAULT:          $format ("LOAD_ACCESS_FAULT");

	     exc_code_STORE_AMO_ADDR_MISALIGNED:  $format ("STORE_AMO_ADDR_MISALIGNED");
	     exc_code_STORE_AMO_ACCESS_FAULT:     $format ("STORE_AMO_ACCESS_FAULT");

	     exc_code_ECALL_FROM_U:               $format ("ECALL_FROM_U");
	     exc_code_ECALL_FROM_S:               $format ("ECALL_FROM_S");
	     exc_code_ECALL_FROM_M:               $format ("ECALL_FROM_M");

	     exc_code_INSTR_PAGE_FAULT:           $format ("INSTRUCTION_PAGE_FAULT");
	     exc_code_LOAD_PAGE_FAULT:            $format ("LOAD_PAGE_FAULT");
	     exc_code_STORE_AMO_PAGE_FAULT:       $format ("STORE_AMO_PAGE_FAULT");

	     default:                             $format ("unknown trap Exc_Code %d", exc_code);
	  endcase;
endfunction

// ================================================================
// Function from various CSRs and current privilege to:
//     whether or not an interrupt is pending,
// and if so, corresponding exception code

function Maybe #(Exc_Code) fv_interrupt_pending (MISA       misa,
						 WordXL     mstatus,
						 WordXL     mip,
						 WordXL     mie,
						 Bit #(12)  mideleg,
						 Bit #(12)  sideleg,
						 Priv_Mode  cur_priv);

   function Maybe #(Exc_Code) fv_interrupt_i_pending (Exc_Code i);
      Bool intr_pending = ((mip [i] == 1) && (mie [i] == 1));
      Priv_Mode handler_priv;
      if (mideleg [i] == 1)
	 if (misa.u == 1)
	    if (misa.s == 1)
	       // System with M, S, U
	       if (sideleg [i] == 1)
		  if (misa.n == 1)
		     // M->S->U delegation
		     handler_priv = u_Priv_Mode;
		  else
		     // Error: SIDELEG [i] should not be 1 if MISA.N is 0
		     handler_priv = m_Priv_Mode;
	       else
                  // M->S delegation
		  handler_priv = s_Priv_Mode;
	    else
	       // System with M, U
	       if (misa.n == 1)
		  // M->U delegation
		  handler_priv = u_Priv_Mode;
	       else
		  // Error: MIDELEG [i] should not be 1 if MISA.N is 0
		  handler_priv = m_Priv_Mode;
	 else
	    // Error: System with M only; MIDELEG [i] should not be 1
	    handler_priv = m_Priv_Mode;
      else
	 // no delegation
	 handler_priv = m_Priv_Mode;

      Bool xie;
      if (cur_priv == u_Priv_Mode)
         xie = (mstatus [mstatus_uie_bitpos] == 1);
      else if (cur_priv == s_Priv_Mode)
         xie = (mstatus [mstatus_sie_bitpos] == 1);
      else if (cur_priv == m_Priv_Mode)
         xie = (mstatus [mstatus_mie_bitpos] == 1);
      else
         // Error: unexpected mode
	 xie = False;

      Bool glob_enabled = (   (cur_priv < handler_priv)
			   || ((cur_priv == handler_priv) && xie));

      return ((intr_pending && glob_enabled) ? (tagged Valid i) : (tagged Invalid));
   endfunction

   // Check all interrupts in the following decreasing priority order
   Maybe #(Exc_Code) m_ec;
   m_ec = fv_interrupt_i_pending (exc_code_MACHINE_EXTERNAL_INTERRUPT);
   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_MACHINE_SW_INTERRUPT);
   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_MACHINE_TIMER_INTERRUPT);

   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_SUPERVISOR_EXTERNAL_INTERRUPT);
   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_SUPERVISOR_SW_INTERRUPT);
   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_SUPERVISOR_TIMER_INTERRUPT);

   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_USER_EXTERNAL_INTERRUPT);
   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_USER_SW_INTERRUPT);
   if (m_ec matches tagged Invalid)
      m_ec = fv_interrupt_i_pending (exc_code_USER_TIMER_INTERRUPT);

   return m_ec;
endfunction

// ================================================================
