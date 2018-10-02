// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved

package IntMulDiv;

// ================================================================
// This package implements a simple, iterative (unpipelined) divider
// and multiplier for signed and unsigned integers
// (Not intended as an example of high-performance arith circuits!)

// ================================================================
// Divider Interface
// - Data is always passed as a Bit#(w) quantity.
//   The accompanying Bools specify whether to interpret them as signed or not.
// - 'get_result' returns (err, quotient, remainder)
//    err is True if and only if denominator is 0

interface IntDiv_IFC #(numeric type w);
   (* always_ready *)
   method Action start (Bool num_is_signed, Bool den_is_signed);
   (* always_ready *)
   method Bool                          result_valid;
   (* always_ready *)
   method Tuple2 #(Bit #(w), Bit #(w))  result_value;
endinterface

// ================================================================
// Integer division

typedef enum { Div_RDY, Div_START, Div_LOOP1, Div_LOOP2, Div_DONE} DivState
   deriving (Eq, Bits, FShow);

module mkIntDiv #(Reg #(Bit #(w)) rg_numer,    // a.k.a. dividend, and final remainder
		  Reg #(Bit #(w)) rg_denom)    // a.k.a. divisor
                (IntDiv_IFC #(w));

   Reg #(DivState)  rg_state     <- mkReg (Div_RDY);

   Reg #(Bool)      rg_numer_is_signed  <- mkRegU;
   Reg #(Bool)      rg_denom_is_signed  <- mkRegU;
   Reg #(Bool)      rg_quoIsNeg  <- mkRegU;
   Reg #(Bool)      rg_remIsNeg  <- mkRegU;

   Reg #(Bit #(w))  rg_denom2    <- mkRegU;
   Reg #(Bit #(w))  rg_n         <- mkRegU;
   Reg #(Bit #(w))  rg_quo       <- mkRegU;

   // ----------------
   // RULES

   rule rl_start_div_by_zero ((rg_state == Div_START) && (rg_denom == 0));
      rg_quo   <= '1;        // all bits set
      // remainder is rg_numer
      rg_state <= Div_DONE;
   endrule

   Bit #(w) rep_most_neg = (1 << (valueOf (w) - 1));
   Bit #(w) rep_minus_1  = '1;

   Bool overflow = (   rg_numer_is_signed && (rg_numer == rep_most_neg)
		    && rg_denom_is_signed && (rg_denom == rep_minus_1));

   rule rl_start_overflow ((rg_state == Div_START) && overflow);
      rg_quo   <= rg_numer;
      rg_numer <= 0;         // remainder
      rg_state <= Div_DONE;
   endrule

   rule rl_start_s ((rg_state == Div_START) && (rg_denom != 0) && (! overflow));
      let numer = rg_numer;
      let denom = rg_denom;
      Int #(w) n_s = unpack (numer);
      Int #(w) d_s = unpack (denom);
      Bool quoIsNeg = False;
      Bool remIsNeg = False;

      if ((rg_numer_is_signed) && (rg_denom_is_signed)) begin
	 numer = pack (abs (n_s));
	 denom = pack (abs (d_s));
	 quoIsNeg = (msb (n_s) != msb (d_s));
	 remIsNeg = (msb (n_s) == 1);
      end
      else if (rg_numer_is_signed) begin
	 numer = pack (abs (n_s));
	 quoIsNeg = (msb (n_s) == 1);
	 remIsNeg = (msb (n_s) == 1);
      end
      else if (rg_denom_is_signed) begin
	 denom = pack (abs (d_s));
	 quoIsNeg = (msb (d_s) == 1);
      end

      rg_numer     <= numer;
      rg_denom     <= denom;
      rg_denom2    <= denom;
      rg_quoIsNeg  <= quoIsNeg;
      rg_remIsNeg  <= remIsNeg;
      rg_quo       <= 0;
      rg_n         <= 1;
      rg_state     <= Div_LOOP1;
   endrule

   // Keep doubling denom2 as long as it is < rem
   // Invariant: denom2 = denom * (2^(n-1))
   rule rl_loop1 (rg_state == Div_LOOP1);
      if (rg_denom2 <= (rg_numer >> 1)) begin
	 rg_denom2 <= rg_denom2 << 1;
	 rg_n <= rg_n << 1;
      end
      else
	 rg_state <= Div_LOOP2;
   endrule

   // Repeatedly subtract denom*(2^n) factors from rem
   rule rl_loop2 (rg_state == Div_LOOP2);
      if (rg_numer < rg_denom) begin
	 rg_state <= Div_DONE;
	 let quo = rg_quo;
	 if (rg_quoIsNeg) begin
	    Int #(w) quo_s = unpack (quo);
	    rg_quo <= pack (-quo_s);
	 end
	 let rem = rg_numer;
	 if (rg_remIsNeg) begin
	    Int #(w) rem_s = unpack (rem);
	    rg_numer <= pack (-rem_s);
	 end
      end
      else if (rg_numer >= rg_denom2) begin
	 rg_numer <= rg_numer - rg_denom2;
	 rg_quo <= rg_quo + rg_n;
      end
      else begin
	 rg_denom2 <= rg_denom2 >> 1;
	 rg_n <= rg_n >> 1;
      end
   endrule

   // ----------------
   // INTERFACE

   method Action start (Bool num_is_signed, Bool den_is_signed);
      rg_numer_is_signed <= num_is_signed;
      rg_denom_is_signed <= den_is_signed;
      rg_state           <= Div_START;
   endmethod

   method Bool result_valid;
      return (rg_state == Div_DONE);
   endmethod

   method Tuple2 #(Bit #(w), Bit #(w)) result_value;
      return tuple2 (rg_quo, rg_numer);
   endmethod
endmodule

// ================================================================
// Multiplier Interface
// - Arg data is always passed as a Bit#(w) quantity.
//   The accompanying Bools specify whether to interpret them as signed or not.
// - Result data is a Bit#(2w) quantity.

interface IntMul_IFC #(numeric type w);
   (* always_ready *)
   method Action put_args (Bool x_is_signed, Bit #(w) x,
			   Bool y_is_signed, Bit #(w) y);
   (* always_ready *)
   method Bool                result_valid;
   (* always_ready *)
   method Bit #(TAdd #(w,w))  result_value;
endinterface

// ================================================================
// Separately synthesized multiplier with 32b/64b inputs

(* synthesize *)
module mkIntMul_32 (IntMul_IFC #(32));
   let m <- mkIntMul;
   return m;
endmodule

(* synthesize *)
module mkIntMul_64 (IntMul_IFC #(64));
   let m <- mkIntMul;
   return m;
endmodule

// ================================================================
// Integer multiplication

typedef enum { Mul_RDY, Mul_BUSY} MulState
   deriving (Eq, Bits, FShow);

module mkIntMul (IntMul_IFC #(w));

   Reg #(MulState) rg_state <- mkReg (Mul_RDY);

   Reg #(Bit #(TAdd #(w,w)))  rg_xy     <- mkRegU;
   Reg #(Bit #(TAdd #(w,w)))  rg_x      <- mkRegU;
   Reg #(Bit #(w))            rg_y      <- mkRegU;
   Reg #(Bool)                rg_signed <- mkRegU;
   Reg #(Bool)                rg_isNeg  <- mkRegU;

   // ----------------
   // RULES

   rule compute ((rg_y != 0) && rg_state == Mul_BUSY) ;
      if (lsb (rg_y) == 1) rg_xy <= rg_xy + rg_x;
      rg_x <= rg_x << 1;
      rg_y <= rg_y >> 1;
   endrule

   // ----------------
   // INTERFACE

   method Action put_args (Bool x_is_signed, Bit #(w) x,
			   Bool y_is_signed, Bit #(w) y);//  if (! rg_busy);
      Int #(w) x_s = unpack (x);
      Int #(w) y_s = unpack (y);
      Bool isNeg   = False;

      if (x_is_signed && y_is_signed) begin
	 x = pack (abs (x_s));
	 y = pack (abs (y_s));
	 isNeg = ((x_s < 0) != (y_s < 0));
      end
      else if (x_is_signed) begin
	 x = pack (abs (x_s));
	 isNeg = (x_s < 0);
      end
      else if (y_is_signed) begin
	 y = pack (abs (y_s));
	 isNeg = (y_s < 0);
      end

      rg_x      <= { 0, x };
      rg_y      <= y;
      rg_isNeg  <= isNeg;
      rg_xy     <= 0;
      rg_state  <= Mul_BUSY;
      // $display ("DBG: IntMul: x = %h", x);
      // $display ("DBG: IntMul: y = %h", y);
   endmethod

   method result_valid = (rg_state == Mul_BUSY && (rg_y == 0));

   method result_value;
      let xy = rg_xy;
      if (rg_isNeg) begin
	 Int #(TAdd #(w,w)) xy_s = unpack (xy);
	 xy = pack (- xy_s);
      end
      return xy;
   endmethod
endmodule

// ================================================================

endpackage
