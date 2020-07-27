
// Copyright (c) 2017 Massachusetts Institute of Technology
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

import Ehr::*;
import CCTypes::*;
import Vector::*;
import ProcTypes::*;

interface MshrDeadlockChecker#(numeric type num);
    method ActionValue#(Maybe#(Bit#(TLog#(num)))) getStuckIdx; // get deadlock MSHR idx
    method Action initEntry(Bit#(TLog#(num)) n); // new MSHR entry allocated
    method Action releaseEntry(Bit#(TLog#(num)) n); // existing MSHR entry released
endinterface

module mkMshrDeadlockChecker(MshrDeadlockChecker#(num)) provisos(
    Alias#(idxT, Bit#(TLog#(num)))
);
    Integer check_port = 0;
    Integer incr_port = 1;
    // timer for each entry to detect deadlock: being processed for 64M cycles
    Vector#(num, Ehr#(2, Maybe#(DeadlockTimer))) timer <- replicateM(mkEhr(Invalid));
    // when new entry is allocated, init timer
    Vector#(num, PulseWire) init <- replicateM(mkPulseWire);
    // when existing entry is released, end the timer
    Vector#(num, PulseWire) done <- replicateM(mkPulseWire);

    (* fire_when_enabled, no_implicit_conditions *)
    rule incrTimer;
        for(Integer i = 0; i < valueof(num); i = i+1) begin
            if(init[i]) begin
                timer[i][incr_port] <= Valid (0);
            end
            else if(done[i]) begin
                timer[i][incr_port] <= Invalid;
            end
            else if(timer[i][incr_port] matches tagged Valid .t &&& t != maxBound) begin
                timer[i][incr_port] <= Valid (t + 1);
            end
        end
    endrule

    method ActionValue#(Maybe#(idxT)) getStuckIdx;
        function Bool isDeadlock(Integer i);
            return timer[i][check_port] == Valid (maxBound);
        endfunction
        Vector#(num, Integer) idxVec = genVector;
        if(searchIndex(isDeadlock, idxVec) matches tagged Valid .n) begin
            timer[n][check_port] <= Valid (0);
            return Valid (n);
        end
        else begin
            return Invalid;
        end
    endmethod

    method Action initEntry(idxT n);
        init[n].send;
    endmethod

    method Action releaseEntry(idxT n);
        done[n].send;
    endmethod
endmodule
