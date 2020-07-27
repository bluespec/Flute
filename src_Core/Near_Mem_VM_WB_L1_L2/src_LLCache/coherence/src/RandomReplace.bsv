
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

import Vector::*;
import Fifos::*;
import CCTypes::*;
import RWBramCore::*;

// random replace does not need any bookkeeping
typedef void RandRepInfo;

function RandRepInfo randRepInitInfo;
    return ?;
endfunction

module mkRandRepRam(RWBramCore#(indexT, RandRepInfo));
    Fifo#(1, void) rdReqQ <- mkPipelineFifo;

    method Action wrReq(indexT a, RandRepInfo d);
        noAction;
    endmethod
    method Action rdReq(indexT a);
        rdReqQ.enq(?);
    endmethod
    method RandRepInfo rdResp if(rdReqQ.notEmpty);
        return ?;
    endmethod
    method rdRespValid = rdReqQ.notEmpty;
    method deqRdResp = rdReqQ.deq;
endmodule

interface RandomReplace#(numeric type wayNum);
    // find a way to replace, which is not locked
    // and Invalid way has priority
    method Maybe#(Bit#(TLog#(wayNum))) getReplaceWay(
        Vector#(wayNum, Bool) unlocked, 
        Vector#(wayNum, Bool) invalid
    );
endinterface

module mkRandomReplace(RandomReplace#(wayNum)) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum)))
);
    Reg#(wayT) randWay <- mkReg(0);

    rule tick;
        randWay <= randWay == fromInteger(valueOf(wayNum) - 1) ? 0 : randWay + 1;
    endrule
    
    method Maybe#(wayT) getReplaceWay(Vector#(wayNum, Bool) unlocked, Vector#(wayNum, Bool) invalid);
        // first search for invalid & unlocked way
        function Bool isInvUnlock(Integer i);
            return unlocked[i] && invalid[i];
        endfunction
        Vector#(wayNum, Integer) idxVec = genVector;
        Maybe#(wayT) repWay = searchIndex(isInvUnlock, idxVec);
        if(!isValid(repWay)) begin
            // check whether random way is unlocked
            if(unlocked[randWay]) begin
                repWay = Valid (randWay);
            end
            else begin
                // just find a unlocked way
                repWay = searchIndex(id, unlocked);
            end
        end
        return repWay;
    endmethod
endmodule

