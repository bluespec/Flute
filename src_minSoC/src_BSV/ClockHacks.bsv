
package ClockHacks;

import Clocks ::*;
import Connectable ::*;

(* always_ready, always_enabled *)
interface OutputBit;
   method Bit#(1) out;
endinterface

import "BVI" ASSIGN1 =
module packClock#(Clock clk)(OutputBit);

   default_clock no_clock;
   default_reset no_reset;

   input_clock clk(IN) = clk;

   method OUT out;

   schedule (out) CF (out);

endmodule

import "BVI" ASSIGN1 =
module packReset#(Reset rst)(OutputBit);

   default_clock no_clock;
   default_reset no_reset;

   input_reset rst(IN) = rst;

   method OUT out reset_by(rst);

   schedule (out) CF (out);

endmodule

(* always_ready, always_enabled *)
interface UnpackedClock;
   interface Clock clk;
   method Action in(Bit#(1) x);
endinterface

import "BVI" ASSIGN1 =
module unpackClock(UnpackedClock);

   default_clock no_clock;
   default_reset no_reset;

   output_clock clk(OUT);

   method in(IN) enable((*inhigh*)en) clocked_by(clk);

   schedule (in) CF (in);

endmodule

(* always_ready, always_enabled *)
interface UnpackedReset;
   interface Reset rst;
   method Action in(Bit#(1) x);
endinterface

import "BVI" ASSIGN1 =
module unpackReset#(Clock clk)(UnpackedReset);

   default_clock no_clock;
   default_reset no_reset;

   input_clock clk() = clk;
   output_reset rst(OUT);

   method in(IN) enable((*inhigh*)en) clocked_by(clk) reset_by(rst);

   schedule (in) CF (in);

endmodule

////////////////////

interface BlockIfc;
   interface Clock clk;
   interface Reset rst;
endinterface

module mkBlock(BlockIfc);
   let default_clock <- exposeCurrentClock;
   let default_reset <- exposeCurrentReset;

   interface clk = default_clock;
   interface rst = default_reset;
endmodule

module mkTop(Empty);

   let clk_unpack <- unpackClock;
   let clk = clk_unpack.clk;

   let rst_unpack <- unpackReset(clk);
   let rst = rst_unpack.rst;

   let a <- mkBlock(clocked_by clk, reset_by rst);
   let b <- mkBlock(clocked_by clk, reset_by rst);

   let clk_pack <- packClock(b.clk, clocked_by clk);
   let rst_pack <- packReset(b.rst, clocked_by clk);

   mkConnection(clk_unpack.in, clk_pack.out);
   mkConnection(rst_unpack.in, rst_pack.out);

endmodule

////////////////////

endpackage : ClockHacks
