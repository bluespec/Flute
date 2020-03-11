// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved

package CSR_MSTATUS;

// ================================================================
// CSR (Control and Status Register) Register MSTATUS
// and its restricted views as SSTATUS and USTATUS.

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

interface CSR_MSTATUS_IFC;
   method Action reset (MISA misa_reset_value);

   method WordXL mv_read;

   // Fixup wordxl and write to reg
   method WordXL mv_write (MISA  misa, WordXL  wordxl);
   method Action ma_write (MISA  misa, WordXL  wordxl);

   // Fixup wordxl and write, and return actual value written
   method ActionValue #(WordXL) mav_write (MISA  misa, WordXL  wordxl);

`ifdef ISA_PRIV_U
   // UStatus is a view of MStatus, when 'U' extension is implemented.
   method WordXL mv_ustatus_read;
   method Action ma_ustatus_write (MISA  misa, WordXL  wordxl);
   method ActionValue #(WordXL) mav_ustatus_write (MISA  misa, WordXL  wordxl);
`endif

`ifdef ISA_PRIV_S
   // SStatus is a view of MStatus, when 'S' extension is implemented.
   method WordXL mv_sstatus_read;
   method Action ma_sstatus_write (MISA  misa, WordXL  wordxl);
   method ActionValue #(WordXL) mav_sstatus_write (MISA  misa, WordXL  wordxl);
`endif
endinterface

// ================================================================
// IMPLEMENTATION

// TODO: bsc Internal Error in Verilog gen due to parameter being an expression
//       Uncomment next line after bsc fix
// (* synthesize *)
module mkCSR_MSTATUS #(parameter MISA misa_reset_value) (CSR_MSTATUS_IFC);
   
   Reg #(WordXL) rg_mstatus <- mkReg (fv_mstatus_reset_value (misa_reset_value));

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset (MISA  misa_reset_value);
      rg_mstatus <= fv_mstatus_reset_value (misa_reset_value);
   endmethod

   method WordXL mv_read;
      return rg_mstatus;
   endmethod

   method WordXL mv_write (MISA  misa, WordXL  wordxl);
      return fv_fixup_mstatus (misa, wordxl);
   endmethod
   method Action ma_write (MISA  misa, WordXL  wordxl);
      rg_mstatus <= fv_fixup_mstatus (misa, wordxl);
   endmethod

   method ActionValue #(WordXL) mav_write (MISA misa,  WordXL wordxl);
      let wordxl1 = fv_fixup_mstatus (misa, wordxl);
      rg_mstatus <= wordxl1;
      return wordxl1;
   endmethod

`ifdef ISA_PRIV_S
   // SStatus is a view of MStatus, when 'S' extension is implemented.
   method WordXL mv_sstatus_read;
      return fv_mstatus_to_sstatus (rg_mstatus);
   endmethod

   method Action ma_sstatus_write (MISA  misa, WordXL  wordxl);
      rg_mstatus <= fv_fixup_mstatus (misa,
				      fv_sstatus_to_mstatus (misa,
							     rg_mstatus,
							     wordxl));
   endmethod

   method ActionValue #(WordXL) mav_sstatus_write (MISA misa,  WordXL wordxl);
      let new_mstatus = fv_fixup_mstatus (misa,
					  fv_sstatus_to_mstatus (misa,
								 rg_mstatus,
								 wordxl));
      rg_mstatus <= new_mstatus;
      return fv_mstatus_to_sstatus (new_mstatus);
   endmethod
`endif

`ifdef ISA_PRIV_U
   // UStatus is a view of MStatus, when 'U' extension is implemented.
   method WordXL mv_ustatus_read;
      return fv_mstatus_to_ustatus (rg_mstatus);
   endmethod

   method Action ma_ustatus_write (MISA  misa, WordXL  wordxl);
      rg_mstatus <= fv_fixup_mstatus (misa,
				      fv_ustatus_to_mstatus (misa,
							     rg_mstatus,
							     wordxl));
   endmethod

   method ActionValue #(WordXL) mav_ustatus_write (MISA misa,  WordXL wordxl);
      let wordxl1 = fv_fixup_mstatus (misa,
				      fv_ustatus_to_mstatus (misa,
							     rg_mstatus,
							     wordxl));
      rg_mstatus <= wordxl1;
      return wordxl1;
   endmethod
`endif

endmodule

// ================================================================
// MSTATUS reset value based on MISA

function WordXL fv_mstatus_reset_value (MISA misa);
   WordXL mstatus = 0;

   if (misa.mxl == misa_mxl_64) begin
      if (misa.s == 1)
	 mstatus = fv_assign_bits (mstatus, fromInteger (mstatus_sxl_bitpos), misa_mxl_64);
      if (misa.u == 1)
	 mstatus = fv_assign_bits (mstatus, fromInteger (mstatus_uxl_bitpos), misa_mxl_64);
   end

   mstatus = fv_assign_bits (mstatus, fromInteger (mstatus_mpp_bitpos), u_Priv_Mode);

`ifdef ISA_F
   // When F/D is present, FS bit needs to be set to initial value on reset
   mstatus = fv_assign_bits (mstatus, fromInteger (mstatus_fs_bitpos), fs_xs_initial);
`endif
   return mstatus;
endfunction

// ================================================================
// Restricted view of MSTATUS as SSTATUS

// Fields of mstatus that are visible in sstatus
WordXL sstatus_mask = (  ('b1  << mstatus_sd_bitpos)
		       | ('b11 << mstatus_uxl_bitpos)    // will be 0 in RV64
		       | ('b1  << mstatus_mxr_bitpos)
		       | ('b1  << mstatus_sum_bitpos)
		       | ('b11 << mstatus_xs_bitpos)
		       | ('b11 << mstatus_fs_bitpos)
		       | ('b1  << mstatus_spp_bitpos)
		       | ('b1  << mstatus_spie_bitpos)
		       | ('b1  << mstatus_upie_bitpos)
		       | ('b1  << mstatus_sie_bitpos)
		       | ('b1  << mstatus_uie_bitpos));

// Hide mstatus fields not visible in sstatus
function WordXL fv_mstatus_to_sstatus (WordXL mstatus);
   return (mstatus & sstatus_mask);
endfunction

// Preserve WPRI fields from mstatus into sstatus
function WordXL  fv_sstatus_to_mstatus (MISA misa, WordXL mstatus, WordXL sstatus);
   // Mask in new sstatus fields
   WordXL sstatus_a = (sstatus & sstatus_mask);
   // Preserve remaining fields from mstatus
   WordXL mstatus_a = (mstatus & (~ sstatus_mask));
   return (sstatus_a | mstatus_a);
endfunction

// ================================================================
// Restricted view of MSTATUS as USTATUS

// Fields of mstatus that are visible in ustatus
WordXL ustatus_mask = (  ('b1  << mstatus_upie_bitpos)
		       | ('b1  << mstatus_uie_bitpos));

// Hide mstatus fields not visible in ustatus
function WordXL fv_mstatus_to_ustatus (WordXL mstatus);
   return (mstatus & ustatus_mask);
endfunction

// Preserve WPRI fields from mstatus into ustatus
function WordXL  fv_ustatus_to_mstatus (MISA misa, WordXL mstatus, WordXL ustatus);
   // Mask in new ustatus fields
   WordXL ustatus_a = (ustatus & ustatus_mask);
   // Preserve remaining fields from mstatus
   WordXL mstatus_a = (mstatus & (~ ustatus_mask));
   return (ustatus_a | mstatus_a);
endfunction

// ================================================================
// Fix up word to be written to mstatus according to specs for
// supported/ WPRI/ WLRL/ WARL fields.

function WordXL fv_fixup_mstatus (MISA misa, WordXL  wordxl);
   // MIE, WPRI_2, SIE, UIE
   Bit #(1) uie    = ((misa.n == 0) ? 0 : wordxl [mstatus_uie_bitpos]);
   Bit #(1) wpri_2 = wordxl [mstatus_WPRI_2_bitpos];
   Bit #(1) sie    = ((misa.s == 0) ? 0 : wordxl [mstatus_sie_bitpos]);
   Bit #(1) mie    = wordxl [mstatus_mie_bitpos];

   // MPIE, WPRI_6, SPIE, UPIE
   Bit #(1) upie   = ((misa.n == 0) ? 0 : wordxl [mstatus_upie_bitpos]);
   Bit #(1) wpri_6 = wordxl [mstatus_WPRI_6_bitpos];
   Bit #(1) spie   = ((misa.s == 0) ? 0 : wordxl [mstatus_spie_bitpos]);
   Bit #(1) mpie   = wordxl [mstatus_mpie_bitpos];

   // SPP, WPRI_9, MPP
   Bit #(1) spp    = ((misa.s == 0) ? u_Priv_Mode [0] : wordxl [mstatus_spp_bitpos]);
   Bit #(2) wpri_9 = fv_get_bits (wordxl, fromInteger (mstatus_WPRI_9_bitpos));

   // MPP
   Bit #(2) mpp = fv_get_bits (wordxl, fromInteger (mstatus_mpp_bitpos));
   if (misa.u == 0) begin
      // Only M supported
      mpp = m_Priv_Mode;
   end
   else if (misa.s == 0) begin
      // Only M and U supported
      if (mpp != m_Priv_Mode)
	 mpp = u_Priv_Mode;
   end
   else begin
      // M, S, and U supported
      if (mpp == reserved_Priv_Mode)
	 mpp = s_Priv_Mode;
   end

   // FS: cf. Priv Arch 1.10, p.23: "In systems that do not
   // implement S-mode and do not have a floating-point unit,
   // the FS field is hardwired to zero."
   Bit #(2) fs = (  ((misa.s == 0) && (misa.f == 0) && (misa.d == 0))
		  ? 0
		  : fv_get_bits (wordxl, fromInteger (mstatus_fs_bitpos)));

   // XS
   Bit #(2) xs = (  (misa.x == 0)
		  ? 0
		  : fv_get_bits (wordxl, fromInteger (mstatus_xs_bitpos)));

   // UXL and SXL UXL (RV64 only)
   Bit #(2) uxl = ((misa.u == 1) ? misa_mxl_64 : 0);
   Bit #(2) sxl = ((misa.s == 1) ? misa_mxl_64 : 0);

   // TSR, TW, TVM, MXR, SUM, MPRV
   Bit #(1) tsr  = wordxl [mstatus_tsr_bitpos];
   Bit #(1) tw   = wordxl [mstatus_tw_bitpos];
   Bit #(1) tvm  = wordxl [mstatus_tvm_bitpos];
   Bit #(1) mxr  = wordxl [mstatus_mxr_bitpos];
   Bit #(1) sum  = wordxl [mstatus_sum_bitpos];
   Bit #(1) mprv = wordxl [mstatus_mprv_bitpos];

   Bit #(1) sd   = (((fs == fs_xs_dirty) || (xs == fs_xs_dirty)) ? 1 : 0);

   // Assemble fixed-up mstatus
   Bit #(23) fixed_up_val_23 = {tsr, tw, tvm, mxr, sum, mprv,
				xs, fs,
				mpp, wpri_9, spp,
				mpie, wpri_6, spie, upie,
				mie,  wpri_2, sie,  uie};

   WordXL fixed_up_val = (( zeroExtend (sd) << mstatus_sd_bitpos)
			  | zeroExtend (fixed_up_val_23));

   if (misa.mxl == misa_mxl_64)
      fixed_up_val = (  (zeroExtend (sxl) << mstatus_sxl_bitpos)
		      | (zeroExtend (uxl) << mstatus_uxl_bitpos)
		      | fixed_up_val);

   return fixed_up_val;
endfunction: fv_fixup_mstatus

// ================================================================

endpackage
