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
typedef Server#( Fpu_Req, Fpu_Rsp )             FPU_IFC;

typedef Tuple2#( FDouble, FloatingPoint::Exception )      FpuR;

(* synthesize *)
module mkFPU ( FPU_IFC );
`ifdef ISA_FD_FDIV
   Server#(Tuple2#(UInt#(114),UInt#(57)),Tuple2#(UInt#(57),UInt#(57))) _div <- mkNonPipelinedDivider(2);
   Server#(UInt#(116),Tuple2#(UInt#(116),Bool)) _sqrt                       <- mkNonPipelinedSquareRooter(2);

   Server#( Tuple3#(FDouble,FDouble,RoundMode),         FpuR ) fpu_div64  <- mkFloatingPointDivider(_div);
   Server#( Tuple2#(FDouble,RoundMode),                 FpuR ) fpu_sqr64  <- mkFloatingPointSquareRooter(_sqrt);
`endif

   Server#( Tuple4#(Maybe#(FDouble),FDouble,FDouble,RoundMode), FpuR ) fpu_madd   <- mkFloatingPointFusedMultiplyAccumulate;

   FIFOF#( Fpu_Req )  iFifo        <- mkFIFOF; // TODO: bypass fifos?
   FIFOF#( Fpu_Rsp )  oFifo        <- mkFIFOF; // TODO: bypass fifos?
   FIFOF#( RoundMode ) rmdFifo     <- mkFIFOF; // TODO: bypass fifos?
   FIFOF#( Bool )     isDoubleFifo <- mkFIFOF; // TODO: bypass fifos?
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

   FDouble opd1 = toDouble(op1, rmd);
   FDouble opd2 = toDouble(op2, rmd);
   FDouble opd3 = toDouble(op3, rmd);

   rule start_op;
      iFifo.deq();
      if (op1 matches tagged D .val) isDoubleFifo.enq(True);
      else                           isDoubleFifo.enq(False);

      if      (iop == FPNMAdd)    isNegateFifo.enq(True);
      else if (iop == FPNMSub)    isNegateFifo.enq(True);
      else                        isNegateFifo.enq(False);

      rmdFifo.enq(rmd);

      case ( iop )
         FPAdd:   fpu_madd.request.put(  tuple4(Valid(opd1), opd2,         one(False), rmd) );
         FPSub:   fpu_madd.request.put(  tuple4(Valid(opd1), negate(opd2), one(False), rmd) );
         FPMul:   fpu_madd.request.put(  tuple4(Invalid,     opd1,         opd2,       rmd) );
`ifdef ISA_FD_FDIV
         FPDiv:   fpu_div64.request.put( tuple3(opd1, opd2,         rmd) );
         FPSqrt:  fpu_sqr64.request.put( tuple2(opd1,               rmd) );
`endif
         FPMAdd:  fpu_madd.request.put(  tuple4(Valid(opd3),         opd1, opd2, rmd) );
         FPMSub:  fpu_madd.request.put(  tuple4(Valid(negate(opd3)), opd1, opd2, rmd) );
         FPNMAdd: fpu_madd.request.put(  tuple4(Valid(opd3),         opd1, opd2, rmd) );
         FPNMSub: fpu_madd.request.put(  tuple4(Valid(negate(opd3)), opd1, opd2, rmd) );
      endcase

   endrule

`ifdef ISA_FD_FDIV
   (* mutually_exclusive = "getResDiv, getResSqr, getResMAdd" *)
   rule getResDiv;
      let res <- fpu_div64.response.get();
      resWire <= res;
   endrule

   rule getResSqr;
      let res <- fpu_sqr64.response.get();
      resWire <= res;
   endrule
`endif

   rule getResMAdd;
      let res <- fpu_madd.response.get();
      resWire <= res;
   endrule

   rule passResult;
      isDoubleFifo.deq();
      let is64Bits = isDoubleFifo.first();

      isNegateFifo.deq();
      let negateResult = isNegateFifo.first();

      rmdFifo.deq();
      let rmode = rmdFifo.first();

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
   endrule

   interface request  = toPut( iFifo );
   interface response = toGet( oFifo );

endmodule
endpackage
