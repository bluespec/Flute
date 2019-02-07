// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package FBox_Top;

// ================================================================
// This package implements the FBox. The FBox executes the F and D
// subsets of the RISC-V ISA

// ================================================================
// BSV Library imports

import FIFOF         :: *;
import Assert        :: *;
import ConfigReg     :: *;
import FShow         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import DefaultValue  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;
import FBox_Core :: *;

// ================================================================
// FBox interface

interface FBox_Top_IFC;
   // ---- Reset
   interface Server #(Token, Token) server_reset;

   // FBox interface: request
   (* always_ready *)
   method Action req (
        Opcode                      opcode
      , Bit #(7)                    f7
      , Bit #(3)                    rm
      , RegName                     rs2
      , Bit #(64)                   v1
      , Bit #(64)                   v2
      , Bit #(64)                   v3
   );

   // FBox interface: response
   (* always_ready *)
   method Bool valid;
   (* always_ready *)
   method Tuple2 #(Bit #(64), Bit #(5)) word;
endinterface

// ================================================================

(* synthesize *)
module mkFBox_Top (FBox_Top_IFC);

   FBox_Core_IFC           fbox_core            <- mkFBox_Core;

   // =============================================================
   // INTERFACE
   // ---- Reset
   interface server_reset = fbox_core.server_reset;

   // FBox interface: request
   method Action req (
        Opcode    opcode
      , Bit #(7)  funct7
      , Bit #(3)  rounding_mode
      , Bit #(5)  rs2_name
      , Bit #(64) val1
      , Bit #(64) val2
      , Bit #(64) val3
   );
      // Legal instruction
      fbox_core.req (
           opcode
         , funct7
         , rounding_mode
         , rs2_name
         , val1
         , val2
         , val3);
   endmethod

   // FBox interface: response
   method Bool valid = fbox_core.valid;

   method Tuple2#(Bit#(64), Bit#(5)) word = fbox_core.word;

endmodule

// ================================================================

endpackage
