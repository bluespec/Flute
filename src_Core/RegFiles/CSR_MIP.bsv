// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package CSR_MIP;

// ================================================================
// CSR (Control and Status Register) Register MIP
// and its restricted views as SIP and UIP.

// ================================================================
// BSV library imports

// None

// BSV additional libs

// none

// ================================================================
// Project imports

import ISA_Decls :: *;

// ================================================================
// INTERFACE

interface CSR_MIP_IFC;
   (* always_ready *)
   method Action reset;

   (* always_ready *)
   method WordXL fv_read;

   // Fixup wordxl and write, and return actual value written
   (* always_ready *)
   method ActionValue #(WordXL) fav_write (MISA  misa, WordXL  wordxl);

`ifdef ISA_PRIV_S
   // SIP is a view of MIP, when 'S' extension is implemented.
   (* always_ready *)
   method WordXL fv_sip_read;
   (* always_ready *)
   method ActionValue #(WordXL) fav_sip_write (MISA  misa, WordXL  wordxl);
`endif

`ifdef ISA_N
   // UIP is a view of MIP, when 'N' extension is implemented.
   (* always_ready *)
   method WordXL fv_uip_read;
   (* always_ready *)
   method ActionValue #(WordXL) fav_uip_write (MISA  misa, WordXL  wordxl);
`endif

   (* always_ready, always_enabled *)
   method Action external_interrupt_req (Bool req);

   method Action software_interrupt_req (Bool req);
   method Action timer_interrupt_req (Bool req);
endinterface

// ================================================================
// IMPLEMENTATION

(* synthesize *)
module mkCSR_MIP (CSR_MIP_IFC);
   
   Reg #(Bit #(1)) rg_meip <- mkReg (0);
   Reg #(Bit #(1)) rg_seip <- mkReg (0);
   Reg #(Bit #(1)) rg_ueip <- mkReg (0);

   Reg #(Bit #(1)) rg_mtip <- mkReg (0);
   Reg #(Bit #(1)) rg_stip <- mkReg (0);
   Reg #(Bit #(1)) rg_utip <- mkReg (0);

   Reg #(Bit #(1)) rg_msip <- mkReg (0);
   Reg #(Bit #(1)) rg_ssip <- mkReg (0);
   Reg #(Bit #(1)) rg_usip <- mkReg (0);

   // FIFOF #(Bool) f_meip <- mkFIFOF;
   // FIFOF #(Bool) f_mtip <- mkFIFOF;
   // FIFOF #(Bool) f_msip <- mkFIFOF;

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset;
      rg_meip <= 0;    rg_seip <= 0;    rg_ueip <= 0;
      rg_mtip <= 0;    rg_stip <= 0;    rg_utip <= 0;
      rg_msip <= 0;    rg_ssip <= 0;    rg_usip <= 0;
   endmethod

   method WordXL fv_read;
      Bit #(12) new_mip = {rg_meip, 1'b0, rg_seip, rg_ueip,
			   rg_mtip, 1'b0, rg_stip, rg_utip,
			   rg_msip, 1'b0, rg_ssip, rg_usip};
      return zeroExtend (new_mip);
   endmethod

   method ActionValue #(WordXL) fav_write (MISA misa,  WordXL wordxl);
      // External-interrupt enables
      Bit #(1) seip = ((misa.s == 1) ? wordxl [mip_seip_bitpos] : 0);
      Bit #(1) ueip = ((misa.n == 1) ? wordxl [mip_ueip_bitpos] : 0);

      // Timer-interrupt enables
      Bit #(1) stip = ((misa.s == 1) ? wordxl [mip_stip_bitpos] : 0);
      Bit #(1) utip = ((misa.n == 1) ? wordxl [mip_utip_bitpos] : 0);

      // Software-interrupt enables
      Bit #(1) ssip = ((misa.s == 1) ? wordxl [mip_ssip_bitpos] : 0);
      Bit #(1) usip = ((misa.n == 1) ? wordxl [mip_usip_bitpos] : 0);

      // Note: meip, mtip and msip cannot be written from CSR instructions
      rg_seip <= seip;    rg_ueip <= ueip;
      rg_stip <= stip;    rg_utip <= utip;
      rg_ssip <= ssip;    rg_usip <= usip;

      Bit #(12) new_mip = {rg_meip, 1'b0, seip, ueip,
			   rg_mtip, 1'b0, stip, utip,
			   rg_msip, 1'b0, ssip, usip};
      return zeroExtend (new_mip);
   endmethod

`ifdef ISA_PRIV_S
   // SIP is a view of MIP, when 'S' extension is implemented.
   method WordXL fv_sip_read;
      Bit #(12) sip = {2'b0, rg_seip, rg_ueip,
		       2'b0, rg_stip, rg_utip,
		       2'b0, rg_ssip, rg_usip};
      return zeroExtend (sip);
   endmethod

   method ActionValue #(WordXL) fav_sip_write (MISA  misa, WordXL  wordxl);
      // Spec: "All bits besides SSIP, USIP and UEIP in the SIP register are read-only"
      let ueip = ((misa.n == 1) ? wordxl [mip_ueip_bitpos] : 0);
      let ssip = ((misa.s == 1) ? wordxl [mip_ssip_bitpos] : 0);
      let usip = ((misa.n == 1) ? wordxl [mip_usip_bitpos] : 0);

      rg_ueip <= ueip;
      rg_ssip <= ssip;
      rg_usip <= usip;

      Bit #(12) new_sip = {2'b0, rg_seip,    ueip,
			   2'b0, rg_stip, rg_utip,
			   2'b0, ssip,       usip};
      return zeroExtend (new_sip);
   endmethod
`endif

`ifdef ISA_N
   // UIP is a view of MIP, when 'U' extension is implemented.
   method WordXL fv_uip_read;
      return zeroExtend (fv_mip_to_uip (rg_mip));
   endmethod

   method ActionValue #(WordXL) fav_uip_write (MISA misa,  WordXL wordxl);
      // All UIP bits are read-only

      Bit #(12) new_uip = {3'b0, rg_ueip,
			   3'b0, rg_utip,
			   3'b0, rg_usip};
      return zeroExtend (new_uip);
   endmethod
`endif

   method Action external_interrupt_req (Bool req);
      rg_meip <= pack (req);
   endmethod

   method Action software_interrupt_req (Bool req);
      rg_msip <= pack (req);
   endmethod

   method Action timer_interrupt_req (Bool req);
      rg_mtip <= pack (req);
   endmethod
endmodule

// ================================================================

endpackage
