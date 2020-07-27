
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


// counter that will block when overflow/underflow

interface SafeCounter#(type t);
    method Action incr(t v);
    method Action decr(t v);
    method t _read;
    method Action _write(t v);
endinterface

module mkSafeCounter#(t initVal)(SafeCounter#(t)) provisos(
    Alias#(t, Bit#(w))
);
    Ehr#(2, t) cnt <- mkEhr(initVal);

    Ehr#(2, Maybe#(t)) incr_req <- mkEhr(Invalid);
    Ehr#(2, Maybe#(t)) decr_req <- mkEhr(Invalid);

    (* fire_when_enabled, no_implicit_conditions *)
    rule con;
        t val = cnt[0];
        if(incr_req[1] matches tagged Valid .v) begin
            val = val + v;
        end
        if(decr_req[1] matches tagged Valid .v) begin
            val = val - v;
        end
        cnt[0] <= val;
        incr_req[1] <= Invalid;
        decr_req[1] <= Invalid;
    endrule

    method Action incr(t v);
        when(cnt[0] <= maxBound - v, action
            incr_req[0] <= Valid (v);
        endaction);
    endmethod

    method Action decr(t v);
        when(cnt[0] >= v, action
            decr_req[0] <= Valid (v);
        endaction);
    endmethod

    method t _read = cnt[0];

    method Action _write(t v);
        cnt[1] <= v;
    endmethod
endmodule
