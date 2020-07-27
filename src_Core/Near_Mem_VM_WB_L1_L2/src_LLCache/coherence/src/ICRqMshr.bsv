
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
import RegFile::*;
import FIFO::*;
import FShow::*;
import Types::*;
import CCTypes::*;
import DefaultValue::*;
import Ehr::*;
import MshrDeadlockChecker::*;

// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// in L1, pRq is always handled immediately, so cRq never depends on pRq and vice versa

// CRq MSHR entry state
typedef enum {
    Empty,
    Init,
    WaitSt, // wait pRs/cRs to come
    Done, // resp is in index FIFO
    Depend
} ICRqState deriving(Bits, Eq, FShow);

// CRq info returned to outside
typedef struct {
    wayT way; // the way to occupy
    tagT repTag; // tag being replaced
    Bool waitP; // waiting for parent resp
} ICRqSlot#(type wayT, type tagT) deriving(Bits, Eq, FShow);

instance DefaultValue#(ICRqSlot#(wayT, tagT));
    defaultValue = ICRqSlot {
        way: ?,
        repTag: ?,
        waitP: False
    };
endinstance

typedef struct {
    reqT req;
    ICRqState state;
    Bool waitP;
} ICRqMshrStuck#(type reqT) deriving(Bits, Eq, FShow);

// MSHR data is purely for replacement resp to parent
// (resp to processor is done immediately, no data buffering needed)

// port for sendRsToP_cRq
interface ICRqMshr_sendRsToP_cRq#(
    numeric type cRqNum,
    type wayT,
    type tagT,
    type reqT
);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method ICRqSlot#(wayT, tagT) getSlot(Bit#(TLog#(cRqNum)) n);
endinterface

// port for sendRqToP
interface ICRqMshr_sendRqToP#(
    numeric type cRqNum,
    type wayT,
    type tagT,
    type reqT
);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method ICRqSlot#(wayT, tagT) getSlot(Bit#(TLog#(cRqNum)) n);
endinterface

// port for pipelineResp
interface ICRqMshr_pipelineResp#(
    numeric type cRqNum,
    type wayT,
    type tagT,
    type reqT,
    type resultT
);
    method ICRqState getState(Bit#(TLog#(cRqNum)) n);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method ICRqSlot#(wayT, tagT) getSlot(Bit#(TLog#(cRqNum)) n);

    method Action setResult(Bit#(TLog#(cRqNum)) n, resultT r); // set a valid result
    method Action setStateSlot(
        Bit#(TLog#(cRqNum)) n, 
        ICRqState state,
        ICRqSlot#(wayT, tagT) slot
    );
    // can only change state to NON-Empty state
    // cannot be used to release MSHR entry (use releaseSlot instead)

    method Maybe#(Bit#(TLog#(cRqNum))) getSucc(Bit#(TLog#(cRqNum)) n);
    method Action setSucc(Bit#(TLog#(cRqNum)) n, Maybe#(Bit#(TLog#(cRqNum))) succ);
    // index in setSucc is usually different from other getXXX methods

    // find existing cRq which has gone through pipeline, but not in Done state, and has not successor
    // i.e. search the end of dependency chain
    method Maybe#(Bit#(TLog#(cRqNum))) searchEndOfChain(Addr addr);
endinterface

interface ICRqMshr_sendRsToC#(
    numeric type cRqNum,
    type resultT
);
    method Action releaseEntry(Bit#(TLog#(cRqNum)) n);
    method Maybe#(resultT) getResult(Bit#(TLog#(cRqNum)) n);
endinterface

interface ICRqMshr#(
    numeric type cRqNum, 
    type wayT,
    type tagT,
    type reqT, // child req type
    type resultT // inst result type
);
    // port for cRqTransfer, initialization is done inside method
    method ActionValue#(Bit#(TLog#(cRqNum))) getEmptyEntryInit(reqT r); 

    // port for sendRsToC
    interface ICRqMshr_sendRsToC#(cRqNum, resultT) sendRsToC;

    // port for sendRsToP_cRq
    interface ICRqMshr_sendRsToP_cRq#(cRqNum, wayT, tagT, reqT) sendRsToP_cRq;

    // port for sendRqToP
    interface ICRqMshr_sendRqToP#(cRqNum, wayT, tagT, reqT) sendRqToP;

    // port for pipelineResp
    interface ICRqMshr_pipelineResp#(cRqNum, wayT, tagT, reqT, resultT) pipelineResp;

    // port for security flush
    method Bool emptyForFlush;

    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(ICRqMshrStuck#(reqT)) stuck;
endinterface


//////////////////
// safe version //
//////////////////
module mkICRqMshrSafe#(
    function Addr getAddrFromReq(reqT r)
)(
    ICRqMshr#(cRqNum, wayT, tagT, reqT, resultT)
) provisos (
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(slotT, ICRqSlot#(wayT, tagT)),
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(reqT, _reqSz),
    Bits#(resultT, _resultTSz)
);
   Bool verbose = False;

    // EHR ports
    // We put pipelineResp < transfer to cater for deq < enq of cache pipeline
    Integer cRqTransfer_port = 2;
    Integer sendRsToC_port = 1; // create a bypass behavior from pipelineResp to sendRsToC (to save a cycle)
    Integer pipelineResp_port = 0;
    Integer sendRqToP_port = 0; // sendRqToP is read only
    Integer sendRsToP_cRq_port = 0; // sendRsToP_cRq is read only
    Integer flush_port = 0; // flush port is read only

    // MSHR entry state
    Vector#(cRqNum, Ehr#(3, ICRqState)) stateVec <- replicateM(mkEhr(Empty));
    // cRq req contents
    Vector#(cRqNum, Ehr#(3, reqT)) reqVec <- replicateM(mkEhr(?));
    // cRq mshr slots
    Vector#(cRqNum, Ehr#(3, slotT)) slotVec <- replicateM(mkEhr(defaultValue));
    // result
    Vector#(cRqNum, Ehr#(3, Maybe#(resultT))) resultVec <- replicateM(mkEhr(Invalid));
    // successor valid bit
    Vector#(cRqNum, Ehr#(3, Bool)) succValidVec <- replicateM(mkEhr(False));
    // successor MSHR index
    RegFile#(cRqIndexT, cRqIndexT) succFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    // empty entry FIFO
    FIFO#(cRqIndexT) emptyEntryQ <- mkSizedFIFO(valueOf(cRqNum));

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(cRqIndexT) initIdx <- mkReg(0);

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(cRqNum) - 1)) begin
            inited <= True;
	   if (verbose)
            $display("%t ICRqMshrSafe %m: init empty entry done", $time);
        end
    endrule

`ifdef CHECK_DEADLOCK
    MshrDeadlockChecker#(cRqNum) checker <- mkMshrDeadlockChecker;
    FIFO#(ICRqMshrStuck#(reqT)) stuckQ <- mkFIFO1;

    (* fire_when_enabled *)
    rule checkDeadlock;
        let stuckIdx <- checker.getStuckIdx;
        if(stuckIdx matches tagged Valid .n) begin
            stuckQ.enq(ICRqMshrStuck {
                req: reqVec[n][0],
                state: stateVec[n][0],
                waitP: slotVec[n][0].waitP
            });
        end
    endrule
`endif

    method ActionValue#(cRqIndexT) getEmptyEntryInit(reqT r) if(inited);
        emptyEntryQ.deq;
        cRqIndexT n = emptyEntryQ.first;
        stateVec[n][cRqTransfer_port] <= Init;
        slotVec[n][cRqTransfer_port] <= defaultValue;
        resultVec[n][cRqTransfer_port] <= Invalid;
        succValidVec[n][cRqTransfer_port] <= False;
        reqVec[n][cRqTransfer_port] <= r;
`ifdef CHECK_DEADLOCK
        checker.initEntry(n);
`endif
        return n;
    endmethod

    interface ICRqMshr_sendRsToC sendRsToC;
        method Action releaseEntry(cRqIndexT n) if(inited);
            emptyEntryQ.enq(n);
            stateVec[n][sendRsToC_port] <= Empty;
`ifdef CHECK_DEADLOCK
            checker.releaseEntry(n);
`endif
        endmethod

        method Maybe#(resultT) getResult(Bit#(TLog#(cRqNum)) n);
            return resultVec[n][sendRsToC_port];
        endmethod
    endinterface

    interface ICRqMshr_sendRsToP_cRq sendRsToP_cRq;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][sendRsToP_cRq_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][sendRsToP_cRq_port];
        endmethod
    endinterface

    interface ICRqMshr_sendRqToP sendRqToP;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][sendRqToP_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][sendRqToP_port];
        endmethod
    endinterface

    interface ICRqMshr_pipelineResp pipelineResp;
        method ICRqState getState(cRqIndexT n);
            return stateVec[n][pipelineResp_port];
        endmethod

        method reqT getRq(cRqIndexT n);
            return reqVec[n][pipelineResp_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][pipelineResp_port];
        endmethod

        method Action setStateSlot(cRqIndexT n, ICRqState state, slotT slot);
            doAssert(state != Empty, "use releaseEntry to set state to Empty");
            stateVec[n][pipelineResp_port] <= state;
            slotVec[n][pipelineResp_port] <= slot;
        endmethod

        method Action setResult(cRqIndexT n, resultT r);
            resultVec[n][pipelineResp_port] <= Valid (r);
        endmethod

        method Maybe#(cRqIndexT) getSucc(cRqIndexT n);
            return succValidVec[n][pipelineResp_port] ? (Valid (succFile.sub(n))) : Invalid;
        endmethod

        method Action setSucc(cRqIndexT n, Maybe#(cRqIndexT) succ);
            succValidVec[n][pipelineResp_port] <= isValid(succ);
            succFile.upd(n, fromMaybe(?, succ));
        endmethod

        method Maybe#(cRqIndexT) searchEndOfChain(Addr addr);
            function Bool isEndOfChain(Integer i);
                // check entry i is end of chain or not
                ICRqState state = stateVec[i][pipelineResp_port];
                Bool notDone = state != Done;
                Bool processedOnce = state != Empty && state != Init;
                Bool addrMatch = getLineAddr(getAddrFromReq(reqVec[i][pipelineResp_port])) == getLineAddr(addr);
                Bool noSucc = !succValidVec[i][pipelineResp_port];
                return notDone && processedOnce && addrMatch && noSucc;
            endfunction
            Vector#(cRqNum, Integer) idxVec = genVector;
            return searchIndex(isEndOfChain, idxVec);
        endmethod
    endinterface

    method Bool emptyForFlush;
        function Bool isEmpty(Integer i) = stateVec[i][flush_port] == Empty;
        Vector#(cRqNum, Integer) idxVec = genVector;
        return all(isEmpty, idxVec);
    endmethod

`ifdef CHECK_DEADLOCK
    interface stuck = toGet(stuckQ);
`else
    interface stuck = nullGet;
`endif
endmodule

// exported version
module mkICRqMshr#(
    function Addr getAddrFromReq(reqT r)
)(
    ICRqMshr#(cRqNum, wayT, tagT, reqT, resultT)
) provisos (
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(reqT, _reqSz),
    Bits#(resultT, _resultTSz)
);
    let m <- mkICRqMshrSafe(getAddrFromReq);
    return m;
endmodule
