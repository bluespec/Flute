
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
import GetPut::*;
import Connectable::*;
import CacheUtils::*;
import CCTypes::*;
import Types::*;
import FShow::*;
import Fifos::*;
import Ehr::*;

typedef struct {
    dstIdxT idx;
    dstDataT data;
} XBarDstInfo#(type dstIdxT, type dstDataT) deriving(Bits, Eq);

module mkXBar#(
    function XBarDstInfo#(dstIdxT, dstDataT) getDstInfo(srcIdxT idx, srcDataT data),
    Vector#(srcNum, Get#(srcDataT)) srcIfc,
    Vector#(dstNum, Put#(dstDataT)) dstIfc
)(Empty) provisos(
    Alias#(srcIdxT, Bit#(TLog#(srcNum))),
    Alias#(dstIdxT, Bit#(TLog#(dstNum))),
    Bits#(srcDataT, _srcDataSz),
    Bits#(dstDataT, _dstDataSz),
    FShow#(dstDataT)
);
   Bool verbose = False;

    // proposed data transfer by each src
    Vector#(srcNum, Ehr#(2, Maybe#(dstIdxT))) propDstIdx <- replicateM(mkEhr(Invalid));
    Vector#(srcNum, Ehr#(2, dstDataT)) propDstData <- replicateM(mkEhr(?));
    // enq command that should be carried out
    Vector#(dstNum, Ehr#(2, Maybe#(dstDataT))) enqDst <- replicateM(mkEhr(Invalid));

    // src propose data transfer when src is not empty 
    // and there is no unfinished src proposal
    for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
        rule srcPropose(!isValid(propDstIdx[i][0]));
            let d <- srcIfc[i].get;
            let info = getDstInfo(fromInteger(i), d);
            propDstIdx[i][0] <= Valid (info.idx);
            propDstData[i][0] <= info.data;
        endrule
    end

    // round-robin select src for each dst
    Vector#(dstNum, Reg#(srcIdxT)) srcRR <- replicateM(mkReg(0));

    // do selection for each dst & generate enq commands & deq src proposals
    // dst which has unfinished enq command cannot select src
    (* fire_when_enabled, no_implicit_conditions *)
    rule dstSelectSrc;
        // which src to deq (i.e. select) by each dst
        Vector#(dstNum, Maybe#(srcIdxT)) deqSrcByDst = replicate(Invalid);
        // each dst selects
        for(Integer dst = 0; dst < valueOf(dstNum); dst = dst + 1) begin
            // only select src to deq when dst has no unfinished enq command
            if(!isValid(enqDst[dst][0])) begin
                function Bool isFromSrc(srcIdxT src);
                    // src has proposed data to this dst or not
                    if(propDstIdx[src][1] matches tagged Valid .dstIdx &&& dstIdx == fromInteger(dst)) begin
                        return True;
                    end
                    else begin
                        return False;
                    end
                endfunction
                Maybe#(srcIdxT) whichSrc = Invalid; // the src to select
                if(isFromSrc(srcRR[dst])) begin
                    // first check the src with priority
                    whichSrc = Valid (srcRR[dst]);
                end
                else begin
                    // otherwise just get one valid src
                    Vector#(srcNum, srcIdxT) srcIdxVec = genWith(fromInteger);
                    whichSrc = searchIndex(isFromSrc, srcIdxVec);
                end
                if(whichSrc matches tagged Valid .src) begin
                    // can do enq & deq
                    deqSrcByDst[dst] = whichSrc;
                    enqDst[dst][0] <= Valid (propDstData[src][1]); // set enq command
                    // change round robin
                    srcRR[dst] <= srcRR[dst] == fromInteger(valueOf(srcNum) - 1) ? 0 : srcRR[dst] + 1;
                end
            end
        end

        // deq selected src
        function Bool isDeqSrc(srcIdxT src);
            function Bool isMatch(Maybe#(srcIdxT) deqIdx);
                return deqIdx matches tagged Valid .idx &&& idx == src ? True : False;
            endfunction
            return any(isMatch, deqSrcByDst);
        endfunction
        for(Integer i = 0; i < valueOf(srcNum); i = i+1) begin
            if(isDeqSrc(fromInteger(i))) begin
                propDstIdx[i][1] <= Invalid;
	       if (verbose)
                $display("%t XBar %m: deq src %d", $time, i);
                doAssert(isValid(propDstIdx[i][1]), "src must be proposing");
            end
        end
    endrule

    // enq dst & change round robin
    for(Integer i = 0; i < valueOf(dstNum); i = i+1) begin
        // XXX since dstIfc.put may conflict with other rules
        // the following rule should only fire when it can make progress
        // otherwise it may prevent other rules from firing forever
        rule doEnq(enqDst[i][1] matches tagged Valid .d);
            dstIfc[i].put(d);
            enqDst[i][1] <= Invalid; // reset enq command
	   if (verbose)
            $display("%t XBAR %m: enq dst %d ; ", $time, i, fshow(d));
        endrule
    end
endmodule

// XBar with latency added at src or dst (mainly for model timing)
interface XBarDelay#(numeric type srcLat, numeric type dstLat);
endinterface

module mkXBarDelay#(
    function XBarDstInfo#(dstIdxT, dstDataT) getDstInfo(srcIdxT idx, srcDataT data),
    Vector#(srcNum, Get#(srcDataT)) srcIfc,
    Vector#(dstNum, Put#(dstDataT)) dstIfc
)(XBarDelay#(srcLat, dstLat)) provisos(
    Alias#(srcIdxT, Bit#(TLog#(srcNum))),
    Alias#(dstIdxT, Bit#(TLog#(dstNum))),
    Bits#(srcDataT, _srcDataSz),
    Bits#(dstDataT, _dstDataSz),
    FShow#(dstDataT)
);
    // Add latency at src side
    Vector#(srcNum, Get#(srcDataT)) src = srcIfc;
    if(valueof(srcLat) > 0) begin
        for(Integer i = 0; i < valueof(srcNum); i = i+1) begin
            Vector#(srcLat, Fifo#(2, srcDataT)) delayQ <- replicateM(mkCFFifo);
            mkConnection(srcIfc[i], toPut(delayQ[0]));
            for(Integer j = 0; j < valueof(srcLat) - 1; j = j+1) begin
                mkConnection(toGet(delayQ[j]), toPut(delayQ[j + 1]));
            end
            src[i] = toGet(delayQ[valueof(srcLat) - 1]);
        end
    end

    // Add latency at dst side
    Vector#(dstNum, Put#(dstDataT)) dst = dstIfc;
    if(valueof(dstLat) > 0) begin
        for(Integer i = 0; i < valueof(dstNum); i = i+1) begin
            Vector#(dstLat, Fifo#(2, dstDataT)) delayQ <- replicateM(mkCFFifo);
            mkConnection(toGet(delayQ[0]), dstIfc[i]);
            for(Integer j = 0; j < valueof(dstLat) - 1; j = j+1) begin
                mkConnection(toGet(delayQ[j + 1]), toPut(delayQ[j]));
            end
            dst[i] = toPut(delayQ[valueof(dstLat) - 1]);
        end
    end

    mkXBar(getDstInfo, src, dst);
endmodule

interface CrossBar#(
    numeric type srcNum,
    numeric type srcLat,
    type srcDataT,
    numeric type dstNum,
    numeric type dstLat,
    type dstDataT
);
    interface Vector#(srcNum, FifoEnq#(srcDataT)) srcIfc;
    interface Vector#(dstNum, FifoDeq#(dstDataT)) dstIfc;
endinterface

module mkCrossBar#(
    function XBarDstInfo#(dstIdxT, dstDataT) getDstInfo(srcIdxT idx, srcDataT data)
)(
    CrossBar#(srcNum, srcLat, srcDataT, dstNum, dstLat, dstDataT)
) provisos(
    Alias#(srcIdxT, Bit#(TLog#(srcNum))),
    Alias#(dstIdxT, Bit#(TLog#(dstNum))),
    Bits#(srcDataT, _srcDataSz),
    Bits#(dstDataT, _dstDataSz),
    FShow#(dstDataT)
);

    // in/out FIFOs
    Vector#(srcNum, Fifo#(2, srcDataT)) srcQ <- replicateM(mkCFFifo);
    Vector#(dstNum, Fifo#(2, dstDataT)) dstQ <- replicateM(mkCFFifo);

    XBarDelay#(srcLat, dstLat) xbar <- mkXBarDelay(getDstInfo, map(toGet, srcQ), map(toPut, dstQ));

    interface srcIfc = map(toFifoEnq, srcQ);
    interface dstIfc = map(toFifoDeq, dstQ);
endmodule

