// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved.

package Tandem_Verif_Out;

// ================================================================
// module mkTandem_Verif_Out writes out tandem-verification logs to a file.


// ================================================================
// BSV lib imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls  :: *;
import TV_Info    :: *;

// ================================================================

String tv_out_filename = "tv.log";

// ================================================================

interface Tandem_Verif_Out_IFC;
   method Action reset;
   interface Put #(Info_CPU_to_Verifier) tv_out;
endinterface

// ================================================================

(* synthesize *)
module mkTandem_Verif_Out (Tandem_Verif_Out_IFC);

   Reg #(Bool) rg_reset_done <- mkReg (False);

   Reg #(File) rg_file <- mkRegU;

   Reg #(Bit #(64)) rg_inum <- mkReg (1);

   FIFOF #(Info_CPU_to_Verifier) f_tv_out <- mkFIFOF;

   // ----------------------------------------------------------------
   // BEHAVIOR

   rule rl_write_log (rg_reset_done);
      let tv_info  <- pop (f_tv_out);
      $fdisplay (rg_file, "%0d %0d %0h %0h %0d %0h %0h %0h",
		 rg_inum,
		 pack (tv_info.exc_taken),
		 tv_info.pc,
		 tv_info.instr,
		 pack (tv_info.instr_valid),
		 tv_info.addr,
		 tv_info.data1,
		 tv_info.data2);
      rg_inum <= rg_inum + 1;
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   method Action reset () if (! rg_reset_done);
      let fd <- $fopen (tv_out_filename, "w");
      rg_file  <= fd;
      $display ("Tandem_Verif_Out: opened file %s", tv_out_filename);
      rg_reset_done <= True;
   endmethod

   interface Put  tv_out = toPut (f_tv_out);
endmodule

// ================================================================

endpackage
