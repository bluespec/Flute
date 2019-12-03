// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package FBox_Core;

// ================================================================
// This package executes the FD instructions, and implements the
// FP core of the CPU

// ================================================================
// BSV Library imports

import FIFOF         :: *;
import Assert        :: *;
import ConfigReg     :: *;
import FShow         :: *;
import FloatingPoint :: *;
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
import FPU       :: *;

// ================================================================
// FBox interface

typedef struct {
   Bit #(64)   value;            // The result rd
   Bit #(5)    flags;            // FCSR.FFLAGS update value
} FBoxResult deriving (Bits, Eq, FShow);

typedef enum {
   FBOX_RST,                     // FBox is resetting
   FBOX_REQ,                     // FBox is accepting a request
   FBOX_BUSY,                    // FBox waiting for a response
   FBOX_RSP                      // FBox driving response
} FBoxState deriving (Bits, Eq, FShow);

interface FBox_Core_IFC;
   // ---- Reset
   interface Server #(Token, Token) server_reset;

   // FBox request
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

   // FBox response
   (* always_ready *)
   method Bool valid;
   (* always_ready *)
   method Tuple2 #(Bit #(64), Bit #(5)) word;
endinterface

// ================================================================
// Some helper function
// Definitions of Q-NaNs for single and double precision
Bit #(32) canonicalNaN32 = 32'h7fc00000;
Bit #(64) canonicalNaN64 = 64'h7ff8000000000000;

// Convert the rounding mode into the format understood by the FPU/PNU
function RoundMode fv_getRoundMode (Bit #(3) rm);
   case (rm)
      3'h0: return (Rnd_Nearest_Even);
      3'h1: return (Rnd_Zero);
      3'h2: return (Rnd_Minus_Inf);
      3'h3: return (Rnd_Plus_Inf);
      3'h4: return (Rnd_Nearest_Away_Zero); // XXX Is this RMM?
      default: return (Rnd_Nearest_Even);
   endcase
endfunction

// Converts the exception coming from the FPU/PNU to the format for the FCSR
function Bit #(5) exception_to_fcsr( FloatingPoint::Exception x );
   let nv  = x.invalid_op ? 1'b1 : 1'b0 ;
   let dz  = x.divide_0   ? 1'b1 : 1'b0 ;
   let of  = x.overflow   ? 1'b1 : 1'b0 ;
   let uf  = x.underflow  ? 1'b1 : 1'b0 ;
   let nx  = x.inexact    ? 1'b1 : 1'b0 ;
   return pack ({nv, dz, of, uf, nx});
endfunction

// Take a single precision value and nanboxes it to be able to write it to a
// 64-bit FPR register file. This is necessary if single precision operands
// used with a register file capable of holding double precision values
function Bit #(64) fv_nanbox (Bit #(64) x);
   Bit #(64) fill_bits = (64'h1 << 32) - 1;  // [31: 0] all ones
   Bit #(64) fill_mask = (fill_bits << 32);  // [63:32] all ones
   return (x | fill_mask);
endfunction

// Take a 64-bit value and check if it is properly nanboxed if operating in a DP
// capable environment. If not properly nanboxed, return canonicalNaN32
function FSingle fv_unbox (Bit #(64) x);
`ifdef ISA_D
   if (x [63:32] == 32'hffffffff)
      return (unpack (x [31:0]));
   else
      return (unpack (canonicalNaN32));
`else  
   return (unpack (x [31:0]));
`endif
endfunction

// Check if FSingle is a +0
function Bool fv_FSingleIsPositiveZero ( FSingle x );
   return ( isZero (x) && !(x.sign) );
endfunction

// Check if FDouble is a +0
function Bool fv_FDoubleIsPositiveZero ( FDouble x );
   return ( isZero (x) && !(x.sign) );
endfunction

// ================================================================

(* synthesize *)
module mkFBox_Core #(Bit #(4) verbosity) (FBox_Core_IFC);

   FIFOF #(Token)          resetReqsF           <- mkFIFOF;
   FIFOF #(Token)          resetRspsF           <- mkFIFOF;

   FIFOF #(Bool)           frmFpuF              <- mkFIFOF;

   Reg   #(FBoxState)      stateR               <- mkReg (FBOX_RST);

   Reg   #(Maybe #(Tuple7 #(
        Opcode
      , Bit #(7)
      , RegName 
      , Bit #(3)
      , Bit #(64)
      , Bit #(64)
      , Bit #(64))))       requestR             <- mkRegU;

   Reg   #(Bool)           dw_valid             <- mkDWire (False);
   Reg   #(Tuple2 #(
        Bit #(64)
      , Bit #(5)))         dw_result            <- mkDWire (?);

   Reg   #(Maybe #(Tuple2 #(
        Bit #(64)
      , Bit #(5))))        resultR              <- mkRegU;
   
   FPU_IFC                 fpu                  <- mkFPU;

   // =============================================================
   // Drive response to the pipeline
   function Action fa_driveResponse (Bit #(64) res, Bit #(5) flags);
      action
      dw_valid    <= True;
      dw_result   <= tuple2 (res, flags);
      endaction
   endfunction

   // =============================================================
   // Decode sub-opcodes (a direct lift from the spec)
   match {.opc, .f7, .rs2, .rm, .v1, .v2, .v3} = requestR.Valid;
   Bit #(2) f2 = f7[1:0];
`ifdef ISA_D
   let isFMADD_D     = (opc == op_FMADD)  && (f2 == 1);
   let isFMSUB_D     = (opc == op_FMSUB)  && (f2 == 1);
   let isFNMADD_D    = (opc == op_FNMADD) && (f2 == 1);
   let isFNMSUB_D    = (opc == op_FNMSUB) && (f2 == 1);
   let isFADD_D      = (opc == op_FP) && (f7 == f7_FADD_D); 
   let isFSUB_D      = (opc == op_FP) && (f7 == f7_FSUB_D);
   let isFMUL_D      = (opc == op_FP) && (f7 == f7_FMUL_D);
`ifdef ISA_FD_DIV
   let isFDIV_D      = (opc == op_FP) && (f7 == f7_FDIV_D);
   let isFSQRT_D     = (opc == op_FP) && (f7 == f7_FSQRT_D);
`endif
   let isFSGNJ_D     = (opc == op_FP) && (f7 == f7_FSGNJ_D) && (rm == 0);
   let isFSGNJN_D    = (opc == op_FP) && (f7 == f7_FSGNJ_D) && (rm == 1);
   let isFSGNJX_D    = (opc == op_FP) && (f7 == f7_FSGNJ_D) && (rm == 2);
   let isFCVT_W_D    = (opc == op_FP) && (f7 == f7_FCVT_W_D)  && (rs2 == 0);
   let isFCVT_WU_D   = (opc == op_FP) && (f7 == f7_FCVT_WU_D) && (rs2 == 1);
`ifdef RV64
   let isFCVT_L_D    = (opc == op_FP) && (f7 == f7_FCVT_L_D)  && (rs2 == 2);
   let isFCVT_LU_D   = (opc == op_FP) && (f7 == f7_FCVT_LU_D) && (rs2 == 3);
`endif
   let isFCVT_D_W    = (opc == op_FP) && (f7 == f7_FCVT_D_W)  && (rs2 == 0);
   let isFCVT_D_WU   = (opc == op_FP) && (f7 == f7_FCVT_D_WU) && (rs2 == 1);
`ifdef RV64
   let isFCVT_D_L    = (opc == op_FP) && (f7 == f7_FCVT_D_L)  && (rs2 == 2);
   let isFCVT_D_LU   = (opc == op_FP) && (f7 == f7_FCVT_D_LU) && (rs2 == 3);
`endif
   let isFCVT_D_S    = (opc == op_FP) && (f7 == f7_FCVT_D_S)  && (rs2 == 0);
   let isFCVT_S_D    = (opc == op_FP) && (f7 == f7_FCVT_S_D)  && (rs2 == 1);
   let isFMIN_D      = (opc == op_FP) && (f7 == f7_FMIN_D) && (rm == 0);
   let isFMAX_D      = (opc == op_FP) && (f7 == f7_FMAX_D) && (rm == 1);
   let isFLE_D       = (opc == op_FP) && (f7 == f7_FCMP_D) && (rm == 0);
   let isFLT_D       = (opc == op_FP) && (f7 == f7_FCMP_D) && (rm == 1);
   let isFEQ_D       = (opc == op_FP) && (f7 == f7_FCMP_D) && (rm == 2);
   let isFMV_X_D     = (opc == op_FP) && (f7 == f7_FMV_X_D) && (rm == 0);
   let isFMV_D_X     = (opc == op_FP) && (f7 == f7_FMV_D_X) && (rm == 0);
   let isFCLASS_D    = (opc == op_FP) && (f7 == f7_FCLASS_D) && (rm == 1);
`endif

   let isFMADD_S     = (opc == op_FMADD)  && (f2 == 0);
   let isFMSUB_S     = (opc == op_FMSUB)  && (f2 == 0);
   let isFNMADD_S    = (opc == op_FNMADD) && (f2 == 0);
   let isFNMSUB_S    = (opc == op_FNMSUB) && (f2 == 0);
   let isFADD_S      = (opc == op_FP) && (f7 == f7_FADD_S); 
   let isFSUB_S      = (opc == op_FP) && (f7 == f7_FSUB_S);
   let isFMUL_S      = (opc == op_FP) && (f7 == f7_FMUL_S);
`ifdef ISA_FD_DIV
   let isFDIV_S      = (opc == op_FP) && (f7 == f7_FDIV_S);
   let isFSQRT_S     = (opc == op_FP) && (f7 == f7_FSQRT_S);
`endif
   let isFSGNJ_S     = (opc == op_FP) && (f7 == f7_FSGNJ_S) && (rm == 0);
   let isFSGNJN_S    = (opc == op_FP) && (f7 == f7_FSGNJ_S) && (rm == 1);
   let isFSGNJX_S    = (opc == op_FP) && (f7 == f7_FSGNJ_S) && (rm == 2);
   let isFCVT_W_S    = (opc == op_FP) && (f7 == f7_FCVT_W_S)  && (rs2 == 0);
   let isFCVT_WU_S   = (opc == op_FP) && (f7 == f7_FCVT_WU_S) && (rs2 == 1);
`ifdef RV64
   let isFCVT_L_S    = (opc == op_FP) && (f7 == f7_FCVT_L_S)  && (rs2 == 2);
   let isFCVT_LU_S   = (opc == op_FP) && (f7 == f7_FCVT_LU_S) && (rs2 == 3);
`endif
   let isFCVT_S_W    = (opc == op_FP) && (f7 == f7_FCVT_S_W)  && (rs2 == 0);
   let isFCVT_S_WU   = (opc == op_FP) && (f7 == f7_FCVT_S_WU) && (rs2 == 1);
`ifdef RV64
   let isFCVT_S_L    = (opc == op_FP) && (f7 == f7_FCVT_S_L)  && (rs2 == 2);
   let isFCVT_S_LU   = (opc == op_FP) && (f7 == f7_FCVT_S_LU) && (rs2 == 3);
`endif
   let isFMIN_S      = (opc == op_FP) && (f7 == f7_FMIN_S) && (rm == 0);
   let isFMAX_S      = (opc == op_FP) && (f7 == f7_FMAX_S) && (rm == 1);
   let isFLE_S       = (opc == op_FP) && (f7 == f7_FCMP_S) && (rm == 0);
   let isFLT_S       = (opc == op_FP) && (f7 == f7_FCMP_S) && (rm == 1);
   let isFEQ_S       = (opc == op_FP) && (f7 == f7_FCMP_S) && (rm == 2);
   let isFMV_X_W     = (opc == op_FP) && (f7 == f7_FMV_X_W) && (rm == 0);
   let isFMV_W_X     = (opc == op_FP) && (f7 == f7_FMV_W_X) && (rm == 0);
   let isFCLASS_S    = (opc == op_FP) && (f7 == f7_FCLASS_S) && (rm == 1);

   // =============================================================
   // Prepare the operands. The operands come in as raw 64 bits. They need to be
   // type cast as FSingle of FDouble. This is also where the nanbox check needs
   // to be done. If we are executing in a DP capable environment, all SP 64-bit
   // rs values should be properly nanboxed. Otherwise, they will be treated as
   // as canonicalNaN32
   FSingle sV1, sV2, sV3;
   FDouble dV1, dV2, dV3;

   sV1 = fv_unbox (v1);
   sV2 = fv_unbox (v2);
   sV3 = fv_unbox (v3);

   dV1 = unpack (v1);
   dV2 = unpack (v2);
   dV3 = unpack (v3);

   let rmd = fv_getRoundMode (rm);

   // =============================================================
   // BEHAVIOR

   // Triggered on receiving a reset token -- necessary to start
   // operations
   rule rl_reset_begin;
      resetReqsF.deq;
      frmFpuF.clear;

      requestR <= tagged Invalid;
      resultR  <= tagged Invalid;
      stateR   <= FBOX_RST;

      fpu.server_reset.request.put (?);
   endrule
   
   // Complete the reset when response from the FPU is received
   rule rl_reset_end (stateR == FBOX_RST);
      stateR   <= FBOX_REQ;
      let res  <- fpu.server_reset.response.get;
      resetRspsF.enq (?);
   endrule

   // These rules execute the operations, either dispatch to the FPU/PNU or
   // locally here in the F-Box
   Bool validReq = isValid (requestR) && (stateR == FBOX_REQ) ;

   // Single precision operations
   let cmpres_s = compareFP ( sV1, sV2 );
   rule doFADD_S ( validReq && isFADD_S );
      if (verbosity > 1) 
         $display ("%0d: FBox_Core.doFADD: ", cur_cycle);
      fpu.server_core.request.put (tuple5 (tagged S sV1, tagged S sV2, ?, rmd, FPAdd));

      stateR <= FBOX_BUSY;
   endrule

   rule doFSUB_S ( validReq && isFSUB_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSUB: ", cur_cycle);
      fpu.server_core.request.put (tuple5 (tagged S sV1, tagged S sV2, ?, rmd, FPSub));
      stateR <= FBOX_BUSY;
   endrule

   rule doFMUL_S ( validReq && isFMUL_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMUL: ", cur_cycle);
      fpu.server_core.request.put (tuple5 (tagged S sV1, tagged S sV2, ?, rmd, FPMul));

      stateR <= FBOX_BUSY;
   endrule

   rule doFMADD_S ( validReq && isFMADD_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMADD_S ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged S sV1, tagged S sV2, tagged S sV3, rmd, FPMAdd ));
      stateR <= FBOX_BUSY;
   endrule

   rule doFMSUB_S ( validReq && isFMSUB_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMSUB_S ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged S sV1, tagged S sV2, tagged S sV3, rmd, FPMSub ));
      stateR <= FBOX_BUSY;
   endrule

   rule doFNMADD_S ( validReq && isFNMADD_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFNMADD_S ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged S sV1, tagged S sV2, tagged S sV3, rmd, FPNMAdd ));
      stateR <= FBOX_BUSY;
   endrule

   rule doFNMSUB_S ( validReq && isFNMSUB_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFNMSUB_S ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged S sV1, tagged S sV2, tagged S sV3, rmd, FPNMSub ));
      stateR <= FBOX_BUSY;
   endrule

`ifdef ISA_FD_DIV
   rule doFDIV_S ( validReq && isFDIV_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFDIV_S ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged S sV1, tagged S sV2, ?, rmd, FPDiv ));
      stateR <= FBOX_BUSY;
   endrule

   rule doFSQRT_S ( validReq && isFSQRT_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSQRT_S ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged S sV1, ?, ?, rmd, FPSqrt ));
      stateR <= FBOX_BUSY;
   endrule
`endif

   rule doFSGNJ_S ( validReq && isFSGNJ_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSGNJ_S ", cur_cycle);
      let r1 = FSingle {  sign: sV2.sign
                        , exp:  sV1.exp
                        , sfd:  sV1.sfd};
      Bit #(64) res = fv_nanbox (extend (pack (r1)));

      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFSGNJN_S ( validReq && isFSGNJN_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSGNJN_S ", cur_cycle);
      FSingle r1 = FSingle {sign: !sV2.sign,
                            exp:   sV1.exp,
                            sfd:   sV1.sfd};

      Bit #(64) res = fv_nanbox (extend (pack (r1)));
      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFSGNJX_S ( validReq && isFSGNJX_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSGNJX_S ", cur_cycle);
      FSingle r1 = FSingle {sign:  (sV1.sign != sV2.sign),
                            exp:   sV1.exp,
                            sfd:   sV1.sfd};
      Bit #(64) res = fv_nanbox (extend (pack (r1)));
      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

`ifdef RV64
   rule doFCVT_S_L ( validReq && isFCVT_S_L );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_S_L ", cur_cycle);

      Int#(64) v = unpack ( v1 );
      match {.f, .e} = Tuple2#(FSingle, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd));
      Bit #(64) res = fv_nanbox (extend (pack ( f )));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;

      if (verbosity > 2) begin
         $display ("v1 = %08x, rmd = ", v1, fshow (rmd));
         $display ("    Result: (%08x, %05b)", res, fcsr);
      end
   endrule

   rule doFCVT_S_LU ( validReq && isFCVT_S_LU );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_S_LU ", cur_cycle);
      UInt#(64) v = unpack ( v1 );
      match {.f, .e} = Tuple2#(FSingle, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd));
      Bit #(64) res = fv_nanbox (extend (pack ( f )));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule
`endif

   rule doFCVT_S_W ( validReq && isFCVT_S_W );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_S_W ", cur_cycle);
      Int#(32) v = unpack (truncate ( v1 ));
      match {.f, .e} = Tuple2#(FSingle, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd));
      Bit #(64) res = fv_nanbox (extend (pack ( f )));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_S_WU ( validReq && isFCVT_S_WU );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_S_WU ", cur_cycle);
      UInt#(32) v = unpack (truncate ( v1 ));
      match {.f, .e} = Tuple2#(FSingle, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd));
      Bit #(64) res = fv_nanbox (extend (pack ( f )));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

`ifdef RV64
   rule doFCVT_L_S ( validReq && isFCVT_L_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_L_S ", cur_cycle);
      FSingle f = sV1;
      match {.v, .e} = Tuple2#(Int#(64),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, f, rmd));

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = (1<<63) - 1;

      Bit #(64) res = ( pack (v) );
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_LU_S ( validReq && isFCVT_LU_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_LU_S ", cur_cycle);
      FSingle f = sV1;

      // Handle negative operands separately. Pass the absolute value to the
      // converter
      FSingle absf = f; absf.sign = False;
      match {.v, .e} = Tuple2#(UInt#(64),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, absf,rmd ));

      // Extra work if the operand is negative. The original convert function
      // prioritizes the sign of the operand before inexact checks
      if (f.sign) begin
         // Result is zero
         v = 0;
         // No exceptions were signalled, signal invalid exception, otherwise
         // let original exception remain
         if (pack (e) == 0)
            e.invalid_op = True;
      end

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = 64'hffffffffffffffff;

      Bit #(64) res = ( pack (v) );
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule
`endif

   rule doFCVT_W_S ( validReq && isFCVT_W_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_W_S ", cur_cycle);
      FSingle f = sV1;
      match {.v, .e} = Tuple2#(Int#(32),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, f, rmd ));

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = (1<<31) - 1;

      Bit #(64) res = signExtend (pack (v));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_WU_S ( validReq && isFCVT_WU_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_WU_S ", cur_cycle);
      FSingle f = sV1;

      // Handle negative operands separately. Pass the absolute value to the
      // converter
      FSingle absf = f; absf.sign = False;
      match {.v, .e} = Tuple2#(UInt#(32),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, absf, rmd ));

      // Extra work if the operand is negative. The original convert function
      // prioritizes the sign of the operand before inexact checks
      if (f.sign) begin
         // Result is zero
         v = 0;
         // No exceptions were signalled, signal invalid exception, otherwise
         // let original exception remain
         if (pack (e) == 0)
            e.invalid_op = True;
      end

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = 32'hffffffff;

      // This is meant for the GPR. If the GPR is 64-bit, the 32-bit result is
      // stored, sign-extended as per the v2.2 of the spec
      Bit #(64) res = signExtend(pack (v));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMIN_S ( validReq && isFMIN_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMIN_S ", cur_cycle);
      Bit #(64) res = ?;
      let rs1IsPos0 = fv_FSingleIsPositiveZero (sV1);
      let rs2IsPos0 = fv_FSingleIsPositiveZero (sV2);
      let rs1IsNeg0 = isNegativeZero (sV1);
      let rs2IsNeg0 = isNegativeZero (sV2);
      // One or both of the values are NaNs
      if ( isSNaN (sV1) && isSNaN (sV2) )
         res = fv_nanbox (extend (pack ( canonicalNaN32 )));
      else if ( isSNaN (sV1) )
         res = fv_nanbox (extend (pack ( sV2 )));
      else if ( isSNaN (sV2) )
         res = fv_nanbox (extend (pack ( sV1 )));
      else if ( isQNaN (sV1) && isQNaN (sV2) )
         res = fv_nanbox (extend (pack ( canonicalNaN32 )));
      else if ( isQNaN (sV1) )
         res = fv_nanbox (extend (pack ( sV2 )));
      else if ( isQNaN (sV2) )
         res = fv_nanbox (extend (pack ( sV1 )));
      else if ( rs1IsNeg0 && rs2IsPos0 )
         res = fv_nanbox (extend (pack ( sV1 )));
      else if ( rs2IsNeg0 && rs1IsPos0 )
         res = fv_nanbox (extend (pack ( sV2 )));
      else
         res = (cmpres_s == LT) ? fv_nanbox (extend (pack (sV1)))
                                : fv_nanbox (extend (pack (sV2)));

      // flag generation
      FloatingPoint::Exception e = defaultValue;
      if ( isSNaN (sV1) || isSNaN (sV2) ) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMAX_S ( validReq && isFMAX_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMAX_S ", cur_cycle);
      Bit #(64) res = ?;
      let rs1IsPos0 = fv_FSingleIsPositiveZero (sV1);
      let rs2IsPos0 = fv_FSingleIsPositiveZero (sV2);
      let rs1IsNeg0 = isNegativeZero (sV1);
      let rs2IsNeg0 = isNegativeZero (sV2);

      // One or both of the values are NaNs
      if ( isSNaN (sV1) && isSNaN (sV2) )
         res = fv_nanbox (extend (pack ( canonicalNaN32 )));
      else if ( isSNaN (sV1) )
         res = fv_nanbox (extend (pack ( sV2 )));
      else if ( isSNaN (sV2) )
         res = fv_nanbox (extend (pack ( sV1 )));
      else if ( isQNaN (sV1) && isQNaN (sV2) )
         res = fv_nanbox (extend (pack ( canonicalNaN32 )));
      else if ( isQNaN (sV1) )
         res = fv_nanbox (extend (pack ( sV2 )));
      else if ( isQNaN (sV2) )
         res = fv_nanbox (extend (pack ( sV1 )));
      else if ( rs1IsNeg0 && rs2IsPos0 )
         res = fv_nanbox (extend (pack ( sV2 )));
      else if ( rs2IsNeg0 && rs1IsPos0 )
         res = fv_nanbox (extend (pack ( sV1 )));
      else
         res = (cmpres_s == LT) ? fv_nanbox (extend (pack (sV2)))
                                : fv_nanbox (extend (pack (sV1)));

      // flag generation
      FloatingPoint::Exception e = defaultValue;
      if ( isSNaN (sV1) || isSNaN (sV2) ) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMV_W_X ( validReq && isFMV_W_X );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMV_W_X ", cur_cycle);
      Bit #(64) res = fv_nanbox (pack ( v1 ));
      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMV_X_W ( validReq && isFMV_X_W );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMV_X_W ", cur_cycle);
      // The FMV treats the data in the FPR and GPR as raw data and does not
      // interpret it. So for this instruction we use the raw bits coming from
      // the FPR
      Bit #(64) res = signExtend ( v1[31:0] );

      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFEQ_S ( validReq && isFEQ_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFEQ_S ", cur_cycle);
      // Generate the results
      Bit #(64) res = ?;
      
      if (  isSNaN (sV1)
         || isSNaN (sV2)
         || isQNaN (sV1)
         || isQNaN (sV2)) res = 0;
      else
         res = (cmpres_s == EQ) ? 1 : 0; 

      // Generate the flags
      FloatingPoint::Exception e = defaultValue;
      if (isSNaN(sV1) || isSNaN(sV2)) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFLT_S ( validReq && isFLT_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFLT_S ", cur_cycle);
      // Generate the results
      Bit #(64) res = ?;
      
      if (  isSNaN (sV1)
         || isSNaN (sV2)
         || isQNaN (sV1)
         || isQNaN (sV2)) res = 0;
      else
         res = (cmpres_s==LT) ? 1 : 0;

      // Generate the flags
      FloatingPoint::Exception e = defaultValue;
      if (isNaN(sV1) || isNaN(sV2)) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFLE_S ( validReq && isFLE_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFLE_S ", cur_cycle);
      // Generate the results
      Bit #(64) res = ?;
      
      if (  isSNaN (sV1)
         || isSNaN (sV2)
         || isQNaN (sV1)
         || isQNaN (sV2)) res = 0;
      else
         res = ((cmpres_s==LT) || (cmpres_s==EQ)) ? 1 : 0;

      // Generate the flags
      FloatingPoint::Exception e = defaultValue;
      if (isNaN(sV1) || isNaN(sV2)) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCLASS_S ( validReq && isFCLASS_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCLASS_S ", cur_cycle);
      Bit #(64) res = 1;
      if (isNaN(sV1)) begin
	 res = isQNaN(sV1) ? (res << 9) : (res << 8);
      end
      else if (isInfinity(sV1)) begin
	 res = sV1.sign ? res        : (res << 7);
      end
      else if (isZero(sV1)) begin
	 res = sV1.sign ? (res << 3) : (res << 4);
      end
      else if (isSubNormal(sV1)) begin
	 res = sV1.sign ? (res << 2) : (res << 5);
      end
      else begin
	 res = sV1.sign ? (res << 1) : (res << 6);
      end

      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule


`ifdef ISA_D
   // Double precision operations
   let cmpres_d = compareFP ( dV1, dV2 );
   rule doFADD_D ( validReq && isFADD_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFADD_D ", cur_cycle);
      fpu.server_core.request.put (tuple5 (tagged D dV1, tagged D dV2, ?, rmd, FPAdd));

      stateR <= FBOX_BUSY;
   endrule

   rule doFSUB_D ( validReq && isFSUB_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSUB_D ", cur_cycle);
      fpu.server_core.request.put (tuple5 (tagged D dV1, tagged D dV2, ?, rmd, FPSub));
      stateR <= FBOX_BUSY;
   endrule

   rule doFMUL_D ( validReq && isFMUL_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMUL_D ", cur_cycle);
      fpu.server_core.request.put (tuple5 (tagged D dV1, tagged D dV2, ?, rmd, FPMul));

      stateR <= FBOX_BUSY;
   endrule

   rule doFMADD_D ( validReq && isFMADD_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMADD_D ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged D dV1, tagged D dV2, tagged D dV3, rmd, FPMAdd ) );
      stateR <= FBOX_BUSY;
   endrule

   rule doFMSUB_D ( validReq && isFMSUB_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMSUB_D ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged D dV1, tagged D dV2, tagged D dV3, rmd, FPMSub ) );
      stateR <= FBOX_BUSY;
   endrule

   rule doFNMADD_D ( validReq && isFNMADD_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFNMADD_D ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged D dV1, tagged D dV2, tagged D dV3, rmd, FPNMAdd ) );
      stateR <= FBOX_BUSY;
   endrule

   rule doFNMSUB_D ( validReq && isFNMSUB_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFNMSUB_D ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged D dV1, tagged D dV2, tagged D dV3, rmd, FPNMSub ) );
      stateR <= FBOX_BUSY;
   endrule

`ifdef ISA_FD_DIV
   rule doFDIV_D ( validReq && isFDIV_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFDIV_D ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged D dV1, tagged D dV2, ?, rmd, FPDiv) );
      stateR <= FBOX_BUSY;
   endrule

   rule doFSQRT_D ( validReq && isFSQRT_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSQRT_D ", cur_cycle);
      fpu.server_core.request.put( tuple5( tagged D dV1, ?, ?, rmd, FPSqrt) );
      stateR <= FBOX_BUSY;
   endrule
`endif

   rule doFSGNJ_D ( validReq && isFSGNJ_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSGNJ_D ", cur_cycle);
      let r1 = FDouble {  sign: dV2.sign
                        , exp:  dV1.exp
                        , sfd:  dV1.sfd};
      Bit #(64) res = pack (r1);

      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFSGNJN_D ( validReq && isFSGNJN_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSGNJN_D ", cur_cycle);
      let r1 = FDouble {  sign: !dV2.sign
                        , exp:   dV1.exp
                        , sfd:   dV1.sfd};

      Bit #(64) res = pack (r1);
      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFSGNJX_D ( validReq && isFSGNJX_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFSGNJX_D ", cur_cycle);
      let r1 = FDouble {  sign:  (dV1.sign != dV2.sign)
                        , exp:   dV1.exp
                        , sfd:   dV1.sfd};
      Bit #(64) res = pack (r1);
      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_D_W ( validReq && isFCVT_D_W );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_D_W ", cur_cycle);
      Int#(32) v = unpack (truncate ( v1 ));
      match {.f, .e} = Tuple2#(FDouble, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd ));
      Bit #(64) res = pack ( f );
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_D_WU ( validReq && isFCVT_D_WU );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_D_WU ", cur_cycle);
      UInt#(32) v = unpack (truncate ( v1 ));
      match {.f, .e} = Tuple2#(FDouble, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd ));
      Bit #(64) res = pack ( f );
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_W_D ( validReq && isFCVT_W_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_W_D ", cur_cycle);
      FDouble f = dV1;
      match {.v, .e} = Tuple2#(Int#(32),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, f, rmd ));

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = (1<<31) - 1;

      Bit #(64) res = signExtend(pack (v));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_WU_D ( validReq && isFCVT_WU_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_WU_D ", cur_cycle);
      FDouble f = dV1;

      // Handle negative operands separately. Pass the absolute value to the
      // converter
      FDouble absf = f; absf.sign = False;
      match {.v, .e} = Tuple2#(UInt#(32),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, absf, rmd ));

      // Extra work if the operand is negative. The original convert function
      // prioritizes the sign of the operand before inexact checks
      if (f.sign) begin
         // Result is zero
         v = 0;
         // No exceptions were signalled, signal invalid exception, otherwise
         // let original exception remain
         if (pack (e) == 0)
            e.invalid_op = True;
      end

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = 32'hffffffff;

      // This is meant for the GPR. If the GPR is 64-bit, the 32-bit result is
      // stored, sign-extended as per the v2.2 of the spec
      Bit #(64) res = signExtend(pack(v));
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

`ifdef RV64
   rule doFCVT_D_L ( validReq && isFCVT_D_L );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_D_L ", cur_cycle);
      Int#(64) v = unpack ( v1 );
      match {.f, .e} = Tuple2#(FDouble, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd ));
      Bit #(64) res = pack ( f );
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_D_LU ( validReq && isFCVT_D_LU );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_D_LU ", cur_cycle);
      UInt#(64) v = unpack ( v1 );
      match {.f, .e} = Tuple2#(FDouble, FloatingPoint::Exception)'(vFixedToFloat( v, 6'd0, rmd ));
      Bit #(64) res = pack ( f );
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_L_D ( validReq && isFCVT_L_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_L_D ", cur_cycle);
      FDouble f = dV1;
      match {.v, .e} = Tuple2#(Int#(64),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, f, rmd ));

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = (1<<63) - 1;

      Bit #(64) res = pack (v);
      let fcsr = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_LU_D ( validReq && isFCVT_LU_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_LU_D ", cur_cycle);
      FDouble f = dV1;

      // Handle negative operands separately. Pass the absolute value to the
      // converter
      FDouble absf = f; absf.sign = False;
      match {.v, .e} = Tuple2#(UInt#(64),FloatingPoint::Exception)'(vFloatToFixed( 6'd0, absf,rmd ));

      // Extra work if the operand is negative. The original convert function
      // prioritizes the sign of the operand before inexact checks
      if (f.sign) begin
         // Result is zero
         v = 0;
         // No exceptions were signalled, signal invalid exception, otherwise
         // let original exception remain
         if (pack (e) == 0)
            e.invalid_op = True;
      end

      // Handle infinity and NaNs
      if (   (isNaN (f))
          || (!(f.sign) && (isInfinity (f)))) 
         v = 64'hffffffffffffffff;

      Bit #(64) res = pack (v);
      let fcsr    = exception_to_fcsr(e);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule
`endif

   rule doFCVT_S_D ( validReq && isFCVT_S_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_S_D ", cur_cycle);
      Bit #(64) res;
      Tuple2#(FSingle,FloatingPoint::Exception) f = convert( dV1 , rmd , False );

      // The convert function retains the original SFD bits in the conversion.
      // The RISC-V spec expects qNaNs to be the canonical NaNs. So, if the
      // output is a qNaN make it into a canonical NaN (zero out SFD, except q)
      if (isQNaN (tpl_1 (f)))
         res = fv_nanbox (extend ( canonicalNaN32 ));
      else
         res = fv_nanbox (extend (pack ( tpl_1(f) )));

      let fcsr = exception_to_fcsr( tpl_2(f) );
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCVT_D_S ( validReq && isFCVT_D_S );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCVT_D_S ", cur_cycle);
      Bit #(64) res;
      Tuple2#(FDouble,FloatingPoint::Exception) f = convert( sV1 , rmd , False );

      // The convert function retains the original SFD bits in the conversion.
      // The RISC-V spec expects qNaNs to be the canonical NaNs. So, if the
      // output is a qNaN make it into a canonical NaN (zero out SFD, except q)
      if (isQNaN (tpl_1 (f)))
         res = canonicalNaN64;
      else
         res = pack ( tpl_1(f) );

      let fcsr = exception_to_fcsr( tpl_2(f) );
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMIN_D ( validReq && isFMIN_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMIN_D ", cur_cycle);
      // One or both of the values are NaNs
      Bit #(64) res = ?;
      let rs1IsPos0 = fv_FDoubleIsPositiveZero (dV1);
      let rs2IsPos0 = fv_FDoubleIsPositiveZero (dV2);
      let rs1IsNeg0 = isNegativeZero (dV1);
      let rs2IsNeg0 = isNegativeZero (dV2);

      if ( isSNaN (dV1) && isSNaN (dV2) )
         res = pack (canonicalNaN64);
      else if ( isSNaN (dV1) )
         res = pack ( dV2 );
      else if ( isSNaN (dV2) )
         res = pack ( dV1 );
      else if ( isQNaN (dV1) && isQNaN (dV2) )
         res = pack (canonicalNaN64);
      else if ( isQNaN (dV1) )
         res = pack ( dV2 );
      else if ( isQNaN (dV2) )
         res = pack ( dV1 );
      else if ( rs1IsNeg0 && rs2IsPos0 )
         res = pack ( dV1 );
      else if ( rs2IsNeg0 && rs1IsPos0 )
         res = pack ( dV2 );
      else
         res = (cmpres_d == LT) ? pack (dV1) : pack (dV2);

      // flag generation
      FloatingPoint::Exception e = defaultValue;
      if ( isSNaN (dV1) || isSNaN (dV2) ) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMAX_D ( validReq && isFMAX_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMAX_D ", cur_cycle);
      // One or both of the values are NaNs
      Bit #(64) res = ?;
      let rs1IsPos0 = fv_FDoubleIsPositiveZero (dV1);
      let rs2IsPos0 = fv_FDoubleIsPositiveZero (dV2);
      let rs1IsNeg0 = isNegativeZero (dV1);
      let rs2IsNeg0 = isNegativeZero (dV2);

      if ( isSNaN (dV1) && isSNaN (dV2) )
         res = pack ( canonicalNaN64 );
      else if ( isSNaN (dV1) )
         res = pack ( dV2 );
      else if ( isSNaN (dV2) )
         res = pack ( dV1 );
      else if ( isQNaN (dV1) && isQNaN (dV2) )
         res = pack ( canonicalNaN64 );
      else if ( isQNaN (dV1) )
         res = pack ( dV2 );
      else if ( isQNaN (dV2) )
         res = pack ( dV1 );
      else if ( rs1IsNeg0 && rs2IsPos0 )
         res = pack ( dV2 );
      else if ( rs2IsNeg0 && rs1IsPos0 )
         res = pack ( dV1 );
      else
         res = (cmpres_d == LT) ? pack (dV2) : pack (dV1);

      // flag generation
      FloatingPoint::Exception e = defaultValue;
      if ( isSNaN (dV1) || isSNaN (dV2) ) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFEQ_D ( validReq && isFEQ_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFEQ_D ", cur_cycle);
      // Generate the results
      Bit #(64) res = ?;
      
      if (  isSNaN (dV1)
         || isSNaN (dV2)
         || isQNaN (dV1)
         || isQNaN (dV2)) res = 0;
      else
         res = (cmpres_d == EQ) ? 1 : 0; 

      // Generate the flags
      FloatingPoint::Exception e = defaultValue;
      if (isSNaN(dV1) || isSNaN(dV2)) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFLT_D ( validReq && isFLT_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFLT_D ", cur_cycle);
      // Generate the results
      Bit #(64) res = ?;
      
      if (  isSNaN (dV1)
         || isSNaN (dV2)
         || isQNaN (dV1)
         || isQNaN (dV2)) res = 0;
      else
         res = (cmpres_d==LT) ? 1 : 0;

      // Generate the flags
      FloatingPoint::Exception e = defaultValue;
      if (isNaN(dV1) || isNaN(dV2)) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFLE_D ( validReq && isFLE_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFLE_D ", cur_cycle);
      // Generate the results
      Bit #(64) res = ?;
      
      if (  isSNaN (dV1)
         || isSNaN (dV2)
         || isQNaN (dV1)
         || isQNaN (dV2)) res = 0;
      else
         res = ((cmpres_d==LT) || (cmpres_d==EQ)) ? 1 : 0;

      // Generate the flags
      FloatingPoint::Exception e = defaultValue;
      if (isNaN(dV1) || isNaN(dV2)) e.invalid_op = True;
      let fcsr = exception_to_fcsr(e);

      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMV_D_X ( validReq && isFMV_D_X );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMV_D_X ", cur_cycle);
      Bit #(64) res = pack ( v1 );
      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFMV_X_D ( validReq && isFMV_X_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFMV_X_D ", cur_cycle);
      Bit #(64) res = pack ( dV1 );
      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule

   rule doFCLASS_D ( validReq && isFCLASS_D );
      if (verbosity > 1)
         $display ("%0d: FBox_Core.doFCLASS_D ", cur_cycle);
      Bit #(64) res = 1;
      if (isNaN(dV1)) begin
	 res = isQNaN(dV1) ? (res << 9) : (res << 8);
      end
      else if (isInfinity(dV1)) begin
	 res = dV1.sign ? res        : (res << 7);
      end
      else if (isZero(dV1)) begin
	 res = dV1.sign ? (res << 3) : (res << 4);
      end
      else if (isSubNormal(dV1)) begin
	 res = dV1.sign ? (res << 2) : (res << 5);
      end
      else begin
	 res = dV1.sign ? (res << 1) : (res << 6);
      end

      resultR     <= tagged Valid (tuple2 (res, 0));
      stateR      <= FBOX_RSP;
   endrule
`endif

   // =============================================================

   // This rule collects the response from FPU for compute opcodes
   rule rl_get_fpu_result ((stateR == FBOX_BUSY));
      Fpu_Rsp r      <- fpu.server_core.response.get();
      match {.v, .e}  = r;
      Bit #(64) res = ?;

      if (v matches tagged S .out)
         if (isNaN (out))
            res = fv_nanbox (extend (pack (canonicalNaN32)));
         else
            res = fv_nanbox (extend (pack (out)));
      else if (v matches tagged D .out)
         if (isNaN (out))
            res = extend (pack (canonicalNaN64));
         else
            res = extend (pack (out));
      else
         res = 0;  // note: just ain't possible

      let fcsr    = exception_to_fcsr( e );
      fa_driveResponse (res, fcsr);
      resultR     <= tagged Valid (tuple2 (res, fcsr));
      stateR      <= FBOX_RSP;
   endrule

   // This rule drives the results from the FBox to the pipeline
   rule rl_drive_fpu_result (stateR == FBOX_RSP);
      dw_valid    <= isValid (resultR);
      dw_result   <= resultR.Valid;
   endrule

   // =============================================================
   // INTERFACE
   // ---- Reset
   interface server_reset = toGPServer (resetReqsF, resetRspsF);

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
      requestR <= tagged Valid (
         tuple7 (opcode, funct7, rs2_name, rounding_mode, val1, val2, val3));

      // Start processing the instruction
      resultR  <= tagged Invalid;
      stateR   <= FBOX_REQ;
   endmethod

   // MBox interface: response
   method Bool valid;
      return dw_valid;
   endmethod

   method Tuple2#(Bit#(64), Bit#(5)) word;
      return dw_result;
   endmethod

endmodule

// ================================================================

endpackage
