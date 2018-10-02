
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

`ifdef BSV_ASYNC_RESET
 `define BSV_ARESET_EDGE_META or `BSV_RESET_EDGE RST
`else
 `define BSV_ARESET_EDGE_META
`endif


// Depth N FIFO (N > 1)
module SizedFIFO0(CLK, RST, ENQ, FULL_N, DEQ, EMPTY_N, CLR);
   parameter p1depth = 2;
   parameter p2cntr_width = 2; // log2(p1depth+1)
   parameter guarded = 1;

   localparam truedepth = (p1depth >= 2) ? p1depth : 2;

   input     CLK;
   input     RST;
   input     CLR;
   input     ENQ;
   input     DEQ;
   output    FULL_N;
   output    EMPTY_N;

   reg       not_full;
   reg       not_empty;
   reg [p2cntr_width-1 : 0] count;

   assign                 EMPTY_N = not_empty;
   assign                 FULL_N = not_full;

`ifdef BSV_NO_INITIAL_BLOCKS
`else // not BSV_NO_INITIAL_BLOCKS
   // synopsys translate_off
   initial
     begin
        count      = 0 ;
        not_empty  = 1'b0;
        not_full   = 1'b1;
     end // initial begin
   // synopsys translate_on
`endif // BSV_NO_INITIAL_BLOCKS


   always @(posedge CLK `BSV_ARESET_EDGE_META)
     begin
        if (RST == `BSV_RESET_VALUE)
          begin
             count <= `BSV_ASSIGNMENT_DELAY 0 ;
             not_empty <= `BSV_ASSIGNMENT_DELAY 1'b0;
             not_full <= `BSV_ASSIGNMENT_DELAY 1'b1;
          end // if (RST == `BSV_RESET_VALUE)
        else
           begin
              if (CLR)
                begin
                   count <= `BSV_ASSIGNMENT_DELAY 0 ;
                   not_empty <= `BSV_ASSIGNMENT_DELAY 1'b0;
                   not_full <= `BSV_ASSIGNMENT_DELAY 1'b1;
                end // if (CLR)
              else begin
                 if (DEQ && ! ENQ && not_empty )
                   begin
                      not_full <= `BSV_ASSIGNMENT_DELAY 1'b1;
                      not_empty <= `BSV_ASSIGNMENT_DELAY  count != 'b01  ;
                      count <= `BSV_ASSIGNMENT_DELAY count - 1'b1 ;
                   end // if (DEQ && ! ENQ && not_empty )
                 else if (ENQ && ! DEQ && not_full )
                   begin
                      not_empty <= `BSV_ASSIGNMENT_DELAY 1'b1;
                      not_full <= `BSV_ASSIGNMENT_DELAY count != (truedepth - 1) ;
                      count <= `BSV_ASSIGNMENT_DELAY count + 1'b1 ;
                   end // if (ENQ && ! DEQ && not_full )
              end // else: !if(CLR)
           end // else: !if(RST == `BSV_RESET_VALUE)
     end // always @ (posedge CLK or `BSV_RESET_EDGE RST)

      // synopsys translate_off
   always@(posedge CLK)
     begin: error_checks
        reg deqerror, enqerror ;

        deqerror =  0;
        enqerror = 0;
        if (RST == ! `BSV_RESET_VALUE)
           begin
              if ( ! EMPTY_N && DEQ )
                begin
                   deqerror = 1 ;
                   $display( "Warning: SizedFIFO0: %m -- Dequeuing from empty fifo" ) ;
                end
              if ( ! FULL_N && ENQ && (!DEQ || guarded) )
                begin
                   enqerror =  1 ;
                   $display( "Warning: SizedFIFO0: %m -- Enqueuing to a full fifo" ) ;
                end
           end // if (RST == ! `BSV_RESET_VALUE)
     end // block: error_checks
   // synopsys translate_on

endmodule // SizedFIFO0
