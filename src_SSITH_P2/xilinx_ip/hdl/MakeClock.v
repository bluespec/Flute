
// Copyright (c) 2000-2012 Bluespec, Inc.

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//
// $Revision$
// $Date$

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif


// Bluespec primitive module which allows creation of clocks
// with non-constant periods.  The CLK_IN and COND_IN inputs
// are registered and used to compute the CLK_OUT and
// CLK_GATE_OUT outputs.
module MakeClock ( CLK, RST,
                   CLK_IN, CLK_IN_EN,
                   COND_IN, COND_IN_EN,
                   CLK_VAL_OUT, COND_OUT,
                   CLK_OUT, CLK_GATE_OUT );

   parameter initVal = 0;
   parameter initGate = 1;

   input  CLK;
   input  RST;

   input  CLK_IN;
   input  CLK_IN_EN;
   input  COND_IN;
   input  COND_IN_EN;

   output CLK_VAL_OUT;
   output COND_OUT;
   (* CLOCK_SIGNAL = "YES" *)
   (* BUFFER_TYPE = "BUFG" *)
   output CLK_OUT;
   output CLK_GATE_OUT;

   (* KEEP = "TRUE" *)
   reg current_clk;
   reg CLK_VAL_OUT;
   reg current_gate;
   reg new_gate;

   // The use of blocking assignment within this block insures
   // that the clock generated from the generate clock (current_clK) occurs before any
   // LHS of nonblocking assigments also from CLKoccur.
   // Basically, this insures that CLK_OUT and CLK occur within
   // the same phase of the execution cycle,  before any state
   // updates occur. see
   // http://www.sunburst-design.com/papers/CummingsSNUG2002Boston_NBAwithDelays.pdf
   always @(posedge CLK or `BSV_RESET_EDGE RST)
   begin
     if (RST == `BSV_RESET_VALUE)
     begin
       current_clk = initVal;
     end
     else
     begin
       if (CLK_IN_EN)
         current_clk = CLK_IN;
     end
   end

   // Duplicate flop for DRC -- clocks cannot be used as data
   always @(posedge CLK or `BSV_RESET_EDGE RST)
   begin
     if (RST == `BSV_RESET_VALUE)
     begin
       CLK_VAL_OUT <=  `BSV_ASSIGNMENT_DELAY initVal;
     end
     else
     begin
       if (CLK_IN_EN)
         CLK_VAL_OUT <=  `BSV_ASSIGNMENT_DELAY CLK_IN;
     end
   end

   always @(posedge CLK or `BSV_RESET_EDGE RST)
   begin
     if (RST == `BSV_RESET_VALUE)
       new_gate   <=  `BSV_ASSIGNMENT_DELAY initGate;
     else
     begin
       if (COND_IN_EN)
         new_gate <=  `BSV_ASSIGNMENT_DELAY  COND_IN;
     end
   end


   // Use latch to avoid glitches
   // Gate can only change when clock is low
   // There remains a fundamental race condition in this design, which
   // is triggered when the current_clk rises and the the new_gate
   // changes.  We recommend to avoid changing the gate in the same
   // cycle when the clock rises.
   always @( current_clk or new_gate )
     begin
        if (current_clk == 1'b0)
          current_gate  <= `BSV_ASSIGNMENT_DELAY new_gate ;
     end

   assign CLK_OUT      = current_clk && current_gate;
   assign CLK_GATE_OUT = current_gate;
   assign COND_OUT     = new_gate;

`ifdef BSV_NO_INITIAL_BLOCKS
`else // not BSV_NO_INITIAL_BLOCKS
   // synopsys translate_off
   initial begin
      #0 ;
      current_clk  = 1'b0 ;
      current_gate = 1'b1 ;
      new_gate     = 1'b1 ;
      CLK_VAL_OUT  = 1'b0;
   end
   // synopsys translate_on
`endif // BSV_NO_INITIAL_BLOCKS

endmodule
