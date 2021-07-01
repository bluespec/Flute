package Jtag;

// This is the version for synthesis for a Xilinx FPGA.

import BVI_xilinx_jtag :: *;
import ClockHacks      :: *;
import Clocks          :: *;

(* always_ready, always_enabled *)
interface Jtag;
   interface Reset	reset;
   method Bit#(1) 	tck ();
   method Bit#(1) 	tdi ();
   method Action 	tdo (Bit#(1) x);
   method Bit#(1) 	tms ();
endinterface

(*synthesize*)
module mkJtag (Jtag);
   let j <- mkBVI_xilinx_jtag;

   let clk <- exposeCurrentClock;
   let jtagReset <- unpackReset(clk);
   (*no_implicit_conditions, fire_when_enabled *)
   rule setJtagReset;
      jtagReset.in(j.reset);
   endrule

   interface reset = jtagReset.rst;
   method tck   = j.tck;
   method tms   = j.tms;
   method tdi   = j.tdi;
   method tdo   = j.tdo;
endmodule



endpackage
