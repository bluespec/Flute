
// Copyright (c) 2018 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Vector::*;
import Types::*;

// a timer to track latency of multiple misses

// To prevent assert failure, start() and done() methods should be called even
// when performance stats is not on

interface LatencyTimer#(numeric type num, numeric type timeWidth);
    method Action start(Bit#(TLog#(num)) idx);
    method ActionValue#(Bit#(timeWidth)) done(Bit#(TLog#(num)) idx);
endinterface

module mkLatencyTimer(LatencyTimer#(num, timeWidth)) provisos(
    //Add#(1, a__, num),
    Alias#(idxT, Bit#(TLog#(num))),
    Alias#(timeT, Bit#(timeWidth))
);
    Reg#(Vector#(num, timeT)) timer <- mkReg(replicate(0));
    Reg#(Vector#(num, Bool)) started <- mkReg(replicate(False)); // for checking purposes

    RWire#(idxT) startEn <- mkRWire;
    RWire#(idxT) doneEn <- mkRWire;

    (* fire_when_enabled, no_implicit_conditions *)
    rule canon;
        Vector#(num, timeT) timerNext = timer;
        Vector#(num, Bool) startedNext = started;
        // apply done
        if(doneEn.wget matches tagged Valid .i) begin
            startedNext[i] = False;
            doAssert(started[i], "timer must be valid");
        end
        // incr timer
        for(Integer i = 0; i < valueof(num); i = i+1) begin
            timeT t = timer[i];
            if(t < maxBound) begin
                timerNext[i] = t + 1;
            end
        end
        // apply start: new timer should be 1 (counting this cycle)
        if(startEn.wget matches tagged Valid .i) begin
            timerNext[i] = 1;
            startedNext[i] = True;
            // do not assert this, because things may be killed
            //doAssert(!started[i], "timer must be invalid");
        end
        // update states
        timer <= timerNext;
        started <= startedNext;
    endrule

    method Action start(idxT i);
        startEn.wset(i);
    endmethod

    method ActionValue#(timeT) done(idxT i);
        doneEn.wset(i);
        return timer[i];
    endmethod
endmodule
