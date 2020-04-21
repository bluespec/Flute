// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved
package FPU;

import FIFOF :: *;
import RegFile :: *;
import GetPut :: *;
import ClientServer :: *;

import FloatingPoint :: *;
import Divide :: *;
import SquareRoot ::*;
import ISA_Decls :: *;

// ================================================================
// Type definitions
typedef FloatingPoint#(11,52) FDouble;
typedef FloatingPoint#(8,23)  FSingle;

typedef union tagged {
   FDouble D;
   FSingle S;
   } FloatU deriving(Bits,Eq);

typedef Tuple5#( FloatU,FloatU,FloatU,RoundMode,FpuOp) Fpu_Req;
typedef Tuple2#( FloatU, FloatingPoint::Exception )       Fpu_Rsp;

`ifdef ISA_D
typedef Tuple2#( FDouble, FloatingPoint::Exception )      FpuR;
`else
typedef Tuple2#( FSingle, FloatingPoint::Exception )      FpuR;
`endif

interface FPU_IFC;
   interface Server #( Fpu_Req, Fpu_Rsp ) server_core;

   // ---- Reset
   interface Server #(Token, Token) server_reset;
endinterface

(* synthesize *)
module mkFPU ( FPU_IFC );
`ifdef INCLUDE_FDIV
`ifdef ISA_D
   Server# (Tuple2# (UInt# (114), UInt# (57))
          , Tuple2# (UInt# (57) , UInt# (57))) _div <- mkNonPipelinedDivider(2);
   Server# (Tuple3#(FDouble, FDouble, RoundMode)
          , FpuR) fpu_div  <- mkFloatingPointDivider(_div);
`else
   Server# (Tuple2# (UInt #(56), UInt #(28))
          , Tuple2# (UInt #(28), UInt #(28))) _div <- mkNonPipelinedDivider(2);
   Server# (Tuple3# (FSingle, FSingle, RoundMode)
          , FpuR) fpu_div <- mkFloatingPointDivider(_div);
`endif
`endif

`ifdef INCLUDE_FSQRT
`ifdef ISA_D
   Server# (UInt# (116)
          , Tuple2# (UInt# (116), Bool)) _sqrt <- mkNonPipelinedSquareRooter(2);
   Server# (Tuple2# (FDouble, RoundMode)
          , FpuR) fpu_sqr <- mkFloatingPointSquareRooter(_sqrt);
`else
   Server# (UInt# (60)
          , Tuple2# (UInt# (60), Bool)) _sqrt <- mkNonPipelinedSquareRooter(2);
   Server# (Tuple2# (FSingle, RoundMode)
          , FpuR) fpu_sqr <- mkFloatingPointSquareRooter(_sqrt);
`endif
`endif

`ifdef ISA_D
   Server# (Tuple4# (Maybe# (FDouble), FDouble, FDouble, RoundMode)
          , FpuR ) fpu_madd <- mkFloatingPointFusedMultiplyAccumulate;
`else
   Server# (Tuple4# (Maybe# (FSingle), FSingle, FSingle, RoundMode)
          , FpuR ) fpu_madd <- mkFloatingPointFusedMultiplyAccumulate;
`endif

   FIFOF #(Token)          resetReqsF           <- mkFIFOF;
   FIFOF #(Token)          resetRspsF           <- mkFIFOF;

   FIFOF#( Fpu_Req )  iFifo        <- mkFIFOF; // TODO: bypass fifos?
   FIFOF#( Fpu_Rsp )  oFifo        <- mkFIFOF; // TODO: bypass fifos?
   FIFOF#( RoundMode ) rmdFifo     <- mkFIFOF; // TODO: bypass fifos?
`ifdef ISA_D
   FIFOF#( Bool )     isDoubleFifo <- mkFIFOF; // TODO: bypass fifos?
`endif
   FIFOF#( Bool )     isNegateFifo <- mkFIFOF; // TODO: bypass fifos?
   Reg#(FpuR)         resWire      <- mkWire;

   function FDouble toDouble( FloatU x, RoundMode rmode );
      if (x matches tagged S .val) begin
         let r = convert( val , rmode , True );
         return tpl_1(r);
      end
      else if (x matches tagged D .val)
         return val;
      else
         return unpack(0);
   endfunction

   let x    = iFifo.first();
   let op1  = tpl_1(x);
   let op2  = tpl_2(x);
   let op3  = tpl_3(x);
   let rmd  = tpl_4(x);
   let iop  = tpl_5(x);

`ifdef ISA_D
   FDouble opd1 = toDouble(op1, rmd);
   FDouble opd2 = toDouble(op2, rmd);
   FDouble opd3 = toDouble(op3, rmd);
`else
   FSingle opd1 = op1.S;
   FSingle opd2 = op2.S;
   FSingle opd3 = op3.S;
`endif

   rule start_op;
      iFifo.deq();
`ifdef ISA_D
      if (op1 matches tagged D .val) isDoubleFifo.enq(True);
      else                           isDoubleFifo.enq(False);
`endif

      if      (iop == FPNMAdd)    isNegateFifo.enq(True);
      else if (iop == FPNMSub)    isNegateFifo.enq(True);
      else                        isNegateFifo.enq(False);

      rmdFifo.enq(rmd);

      case ( iop )
         FPAdd:   fpu_madd.request.put (tuple4(Valid(opd1), opd2,         one(False), rmd) );
         FPSub:   fpu_madd.request.put (tuple4(Valid(opd1), negate(opd2), one(False), rmd) );
         FPMul:   fpu_madd.request.put (tuple4(Invalid,     opd1,         opd2,       rmd) );
`ifdef INCLUDE_FDIV
         FPDiv:   fpu_div.request.put (tuple3 (opd1, opd2, rmd) );
`endif
`ifdef INCLUDE_FSQRT
         FPSqrt:  fpu_sqr.request.put (tuple2(opd1, rmd) );
`endif
         FPMAdd:  fpu_madd.request.put(  tuple4(Valid(opd3),         opd1, opd2, rmd) );
         FPMSub:  fpu_madd.request.put(  tuple4(Valid(negate(opd3)), opd1, opd2, rmd) );
         FPNMAdd: fpu_madd.request.put(  tuple4(Valid(opd3),         opd1, opd2, rmd) );
         FPNMSub: fpu_madd.request.put(  tuple4(Valid(negate(opd3)), opd1, opd2, rmd) );
      endcase

   endrule

   // rule generator for handling responses from multi-cycle pipes
   function Rules fn_genMultCycResRules (Server #(req, FpuR) pipe);
      return (rules 
         rule getResFromPipe;
            let res <- pipe.response.get ();
            resWire <= res;
         endrule
      endrules);
   endfunction

   let rl_resRules = emptyRules;
   rl_resRules = rJoin (rl_resRules, fn_genMultCycResRules (fpu_madd));
`ifdef INCLUDE_FDIV
   rl_resRules = rJoinMutuallyExclusive (rl_resRules, fn_genMultCycResRules (fpu_div));
`endif
`ifdef INCLUDE_FSQRT
   rl_resRules = rJoinMutuallyExclusive (rl_resRules, fn_genMultCycResRules (fpu_sqr));
`endif
   addRules (rl_resRules);

   rule passResult;
`ifdef ISA_D
      isDoubleFifo.deq();
      let is64Bits = isDoubleFifo.first();
`endif

      isNegateFifo.deq();
      let negateResult = isNegateFifo.first();

      rmdFifo.deq();
      let rmode = rmdFifo.first();

`ifdef ISA_D
      if (is64Bits) begin
         FDouble   v  = tpl_1(resWire);
         FloatingPoint::Exception ex = tpl_2(resWire);
         if (negateResult)
            v = negate( v );
         FloatU res = tagged D v;
         oFifo.enq( tuple2( res, ex ) );
      end
      else begin
         Tuple2#(FSingle,FloatingPoint::Exception) sres = convert( tpl_1(resWire) , rmode , True );
         FSingle   v  = tpl_1(sres);
         FloatingPoint::Exception ex = tpl_2(resWire) | tpl_2(sres);
         if (negateResult)
            v = negate( v );
         FloatU res = tagged S v;
         oFifo.enq( tuple2( res, ex ) );
      end
`else
      FSingle   v  = tpl_1(resWire);
      FloatingPoint::Exception ex = tpl_2(resWire);
      if (negateResult)
         v = negate( v );
      FloatU res = tagged S v;
      oFifo.enq( tuple2( res, ex ) );
`endif
   endrule

   rule rl_reset;
      resetReqsF.deq;
      iFifo.clear;
      oFifo.clear;
      rmdFifo.clear;
`ifdef ISA_D
      isDoubleFifo.clear;
`endif
      isNegateFifo.clear;
      resetRspsF.enq (?);
   endrule


   // =============================================================
   // INTERFACE
   // ---- Reset
   interface server_reset = toGPServer (resetReqsF, resetRspsF);
   interface server_core = toGPServer ( iFifo, oFifo );
endmodule
endpackage
