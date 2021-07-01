// BVI Wrapper created by: transactor generator on: Wed Dec  9 13:28:04 EST 2020

import Connectable::*;
import Clocks::*;


(* always_ready, always_enabled *)
interface BVI_xilinx_jtag_IFC;
  (*result="reset"*) method Bit#(1) 	reset ();
  (*result="sel1"*) method Bit#(1) 	sel1 ();
  (*result="sel2"*) method Bit#(1) 	sel2 ();
  (*result="tck"*) method Bit#(1) 	tck ();
  (*result="tdi"*) method Bit#(1) 	tdi ();
  (*prefix=""*) method Action 	tdo ((*port="tdo"*) Bit#(1) x);
  (*result="tms"*) method Bit#(1) 	tms ();
endinterface : BVI_xilinx_jtag_IFC

import "BVI" xilinx_jtag =
module mkBVI_xilinx_jtag (BVI_xilinx_jtag_IFC ifc
	);

  default_clock (clk, (*unused*)GATE);
  default_reset (rst_n);
  method /*Value*/ reset reset ();
  method /*Value*/ sel1 sel1 ();
  method /*Value*/ sel2 sel2 ();
  method /*Value*/ tck tck ();
  method /*Value*/ tdi tdi ();
  method /*Action*/ tdo (tdo) enable((*inhigh*) u0001);
  method /*Value*/ tms tms ();

  // Schedule for methods with clock: clk
  schedule (reset,sel1,sel2,tck,tdi,tms) CF (reset,sel1,sel2,tck,tdi,tms);
  schedule (reset,sel1,sel2,tck,tdi,tms) SB (tdo);
  schedule tdo C tdo;
endmodule: mkBVI_xilinx_jtag
