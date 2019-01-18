// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CSR_MIE;

// ================================================================
// CSR (Control and Status Register) Register MIE
// and its restricted views as SIE and UIE.

// ================================================================
// BSV library imports

// none

// BSV additional libs

// none

// ================================================================
// Project imports

import ISA_Decls :: *;

// ================================================================
// INTERFACE

interface CSR_MIE_IFC;
   (* always_ready *)
   method Action reset;

   (* always_ready *)
   method WordXL fv_read;

   // Fixup wordxl and write, and return actual value written
   (* always_ready *)
   method ActionValue #(WordXL) fav_write (MISA  misa, WordXL  wordxl);

`ifdef ISA_PRIV_S
   // SIE is a view of MIE, when 'S' extension is implemented.
   (* always_ready *)
   method WordXL fv_sie_read;
   (* always_ready *)
   method ActionValue #(WordXL) fav_sie_write (MISA  misa, WordXL  wordxl);
`endif

`ifdef ISA_N
   // UIE is a view of MIE, when 'N' extension is implemented.
   (* always_ready *)
   method WordXL fv_uie_read;
   (* always_ready *)
   method ActionValue #(WordXL) fav_uie_write (MISA  misa, WordXL  wordxl);
`endif
endinterface

// ================================================================
// IMPLEMENTATION

(* synthesize *)
module mkCSR_MIE (CSR_MIE_IFC);
   
   Reg #(Bit #(12)) rg_mie <- mkReg (mie_reset_value);

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      rg_mie <= mie_reset_value;
   endmethod

   method WordXL fv_read;
      return zeroExtend (rg_mie);
   endmethod

   method ActionValue #(WordXL) fav_write (MISA misa,  WordXL wordxl);
      let mie = fv_fixup_mie (misa, truncate (wordxl));
      rg_mie <= mie;
      return zeroExtend (mie);
   endmethod

`ifdef ISA_PRIV_S
   // SIE is a view of MIE, when 'S' extension is implemented.
   method WordXL fv_sie_read;
      return zeroExtend (fv_mie_to_sie (rg_mie));
   endmethod

   method ActionValue #(WordXL) fav_sie_write (MISA  misa, WordXL  wordxl);
      let new_mie = fv_fixup_mie (misa,
				  fv_sie_to_mie (misa,
						 rg_mie,
						 truncate (wordxl)));
      rg_mie <= new_mie;

      WordXL result = zeroExtend (fv_mie_to_sie (new_mie));
      return result;
   endmethod
`endif

`ifdef ISA_N
   // UIE is a view of MIE, when 'U' extension is implemented.
   method WordXL fv_uie_read;
      return zeroExtend (fv_mie_to_uie (rg_mie));
   endmethod

   method ActionValue #(WordXL) fav_uie_write (MISA misa,  WordXL wordxl);
      let new_mie = fv_fixup_mie (misa,
				  fv_uie_to_mie (misa,
						 rg_mie,
						 truncate (wordxl)));
      rg_mie <= new_mie;

      WordXL result = zeroExtend (fv_mie_to_uie (new_mie));
      return result;
   endmethod
`endif

endmodule

// ================================================================
// MIE reset value: 0 (all interrupts disabled)

Bit #(12) mie_reset_value = 0;

// ================================================================
// Restricted view of MIE as SIE

// Fields of mie that are visible in sie
Bit #(12) sie_mask = (  ('b1 << mip_seip_bitpos)
		      | ('b1 << mip_ueip_bitpos)
		      | ('b1 << mip_stip_bitpos)
		      | ('b1 << mip_utip_bitpos)
		      | ('b1 << mip_ssip_bitpos)
		      | ('b1 << mip_usip_bitpos));

// Hide mie fields not visible in sie
function Bit #(12) fv_mie_to_sie (Bit #(12) mie);
   return (mie & sie_mask);
endfunction

// Preserve WPRI fields from mie into sie
function Bit #(12)  fv_sie_to_mie (MISA misa, Bit #(12) mie, Bit #(12) sie);
   // Mask in new sie fields
   Bit #(12) sie_a = (sie & sie_mask);
   // Preserve remaining fields from mie
   Bit #(12) mie_a = (mie & (~ sie_mask));
   return (sie_a | mie_a);
endfunction

// ================================================================
// Restricted view of MIE as UIE

// Fields of mie that are visible in sie
Bit #(12) uie_mask = (  ('b1 << mip_ueip_bitpos)
		      | ('b1 << mip_utip_bitpos)
		      | ('b1 << mip_usip_bitpos));

// Hide mie fields not visible in uie
function Bit #(12) fv_mie_to_uie (Bit #(12) mie);
   return (mie & uie_mask);
endfunction

// Preserve WPRI fields from mie into uie
function Bit #(12)  fv_uie_to_mie (MISA misa, Bit #(12) mie, Bit #(12) uie);
   // Mask in new uie fields
   Bit #(12) uie_a = (uie & uie_mask);
   // Preserve remaining fields from mie
   Bit #(12) mie_a = (mie & (~ uie_mask));
   return (uie_a | mie_a);
endfunction

// ================================================================
// Fix up word to be written to mie according to specs for
// supported/ WPRI/ WLRL/ WARL fields.

function Bit #(12) fv_fixup_mie (MISA misa, Bit #(12)  mie);
   // Software-interrupt enables
   Bit #(1) usie = ((misa.n == 0) ? 0 : mie [mip_usip_bitpos]);
   Bit #(1) ssie = ((misa.s == 0) ? 0 : mie [mip_ssip_bitpos]);
   Bit #(1) msie = mie [mip_msip_bitpos];

   // Timer-interrupt enables
   Bit #(1) utie = ((misa.n == 0) ? 0 : mie [mip_utip_bitpos]);
   Bit #(1) stie = ((misa.s == 0) ? 0 : mie [mip_stip_bitpos]);
   Bit #(1) mtie = mie [mip_mtip_bitpos];

   // External-interrupt enables
   Bit #(1) ueie = ((misa.n == 0) ? 0 : mie [mip_ueip_bitpos]);
   Bit #(1) seie = ((misa.s == 0) ? 0 : mie [mip_seip_bitpos]);
   Bit #(1) meie = mie [mip_meip_bitpos];

   // Assemble fixed-up mie
   Bit #(12) new_mie = {meie, 1'b0, seie, ueie,
			mtie, 1'b0, stie, utie,
			msie, 1'b0, ssie, usie};

   return new_mie;
endfunction: fv_fixup_mie

// ================================================================

endpackage
