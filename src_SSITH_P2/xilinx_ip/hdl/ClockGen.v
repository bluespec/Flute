
// Copyright (c) 2000-2009 Bluespec, Inc.

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

// Bluespec primitive module which generates a periodic clock
// This module is not synthesizable
module ClockGen(CLK_OUT);

   parameter v1Width = 5;
   parameter v2Width  = 5;
   parameter initDelay = 5;
   parameter initValue = 1'b0;
   parameter otherValue = 1'b1 ;

   output    CLK_OUT ;
   reg       CLK_OUT ;

   // synopsys translate_off

   // Clock is set to initValue for initDelay, and
   // then repeats set to value1 for value1Width
   initial
      begin : clock_loop
         #0 ;
         CLK_OUT = initValue ;
         # initDelay ;
         forever
            begin
               CLK_OUT = otherValue ;
               # v1Width ;
               CLK_OUT = initValue ;
               # v2Width ;

            end // forever begin
      end // initial begin

   // Some assertions about parameter values
   initial
     begin : parameter_assertions
        integer ok ;
        ok = 1 ;

        if (! ( (( initValue == 1'b0 ) && ( otherValue == 1'b1 )) ||
             (( initValue == 1'b1 ) && ( otherValue == 1'b0 )) ) )
          begin
             ok = 0;
             $display ( "ERROR ClockGen.v: clock values must be complements" ) ;
          end // if ( (( initValue != 0 ) && ( otherValue != 1 )) ||...

        if ( ( v1Width <= 0 ) || ( v2Width <= 0 ))
          begin
             ok = 0;
             $display( "ERROR ClockGen.v: duty cycle must be greater then 0") ;
          end // if ( ( v1Width <= 0 ) || ( v2Width <= 0 ))

        if ( ok == 0 ) $finish ;

      end // initial begin
      // synopsys translate_on

endmodule // ClockGen

`ifdef testBluespec
module testClockGen1() ;

   wire clkout ;

   ClockGen#(8,24,16,1'b1,1'b0)  u1( clkout );

   initial
     begin
        $dumpfile("ClockGen.dump");
        $dumpvars(5) ;
        $dumpon ;
        #10000 $finish ;
     end

endmodule // testClockGen
`endif
