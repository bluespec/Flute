
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
import Fifos::*;
import MshrDeadlockChecker::*;

// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// in LLC, the head (h1) of a chain may be linked as the successor of the head (h2) of another chain
// when h2 is replacing the addr of h1
// h1 should be waken up and sent to pipeline when replacement is done (i.e. h2 gets to WaitSt)

// CRq MSHR entry state
typedef enum {
    Empty,
    Init,
    WaitOldTag,
    WaitSt,
    Done,
    Depend
} LLCRqState deriving(Bits, Eq, FShow);

// we split data from slot info
// because data may be used to buffer mem resp data
typedef struct {
    wayT way; // the way to occupy
    tagT repTag; // tag being replaced, used in sending down req to children
    Bool waitP; // wait parent resp
    dirPendT dirPend; // pending child downgrade
} LLCRqSlot#(type wayT, type tagT, type dirPendT) deriving(Bits, Eq, FShow);

typedef struct {
    reqT req;
    LLCRqState state;
    Bool waitP;
    dirPendT dirPend;
} LLCRqMshrStuck#(type dirPendT, type reqT) deriving(Bits, Eq, FShow);

// interface for cRq/mRs/cRsTransfer
interface LLCRqMshr_transfer#(
    numeric type cRqNum,
    type wayT,
    type tagT,
    type dirPendT,
    type reqT // child req type
);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method LLCRqSlot#(wayT, tagT, dirPendT) getSlot(Bit#(TLog#(cRqNum)) n);
    method ActionValue#(Bit#(TLog#(cRqNum))) getEmptyEntryInit(reqT r, Maybe#(Line) d);
    // check if any empty MSHR entry is available in order to get MSHR blocking
    // stats. The argument is not really used here, just in case some other
    // MSHR implementations may bank entries based on addr.
    method Bool hasEmptyEntry(reqT r);
endinterface

// interface for mRsDeq
interface LLCRqMshr_mRsDeq#(numeric type cRqNum);
    method Action setData(Bit#(TLog#(cRqNum)) n, Maybe#(Line) d);
endinterface

// interface for sendToM
interface LLCRqMshr_sendToM#(
    numeric type cRqNum,
    type wayT,
    type tagT,
    type dirPendT,
    type reqT // child req type
);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method LLCRqSlot#(wayT, tagT, dirPendT) getSlot(Bit#(TLog#(cRqNum)) n);
    method Maybe#(Line) getData(Bit#(TLog#(cRqNum)) n);
endinterface

// interface for sendRsToDma and sendRsToC
interface LLCRqMshr_sendRsToDmaC#(
    numeric type cRqNum,
    type reqT // child req type
);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method Maybe#(Line) getData(Bit#(TLog#(cRqNum)) n);
    method Action releaseEntry(Bit#(TLog#(cRqNum)) n);
endinterface

// interface for sendRqToC
interface LLCRqMshr_sendRqToC#(
    numeric type cRqNum,
    type wayT,
    type tagT,
    type dirPendT,
    type reqT // child req type
);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method LLCRqState getState(Bit#(TLog#(cRqNum)) n);
    method LLCRqSlot#(wayT, tagT, dirPendT) getSlot(Bit#(TLog#(cRqNum)) n);
    method Action setSlot(Bit#(TLog#(cRqNum)) n, LLCRqSlot#(wayT, tagT, dirPendT) s);
    // find cRq that needs to send req to child to downgrade
    // (either replacement, or incompatible children states)
    // we can pass in a suggested req idx (which will have priority)
    method Maybe#(Bit#(TLog#(cRqNum))) searchNeedRqChild(Maybe#(Bit#(TLog#(cRqNum))) suggestIdx);
endinterface

// interface for pipelineResp_xxx
interface LLCRqMshr_pipelineResp#(
    numeric type cRqNum,
    type wayT,
    type tagT,
    type dirPendT,
    type reqT // child req type
);
    method reqT getRq(Bit#(TLog#(cRqNum)) n);
    method LLCRqState getState(Bit#(TLog#(cRqNum)) n);
    method LLCRqSlot#(wayT, tagT, dirPendT) getSlot(Bit#(TLog#(cRqNum)) n);
    method Maybe#(Line) getData(Bit#(TLog#(cRqNum)) n);
    method Maybe#(Bit#(TLog#(cRqNum))) getAddrSucc(Bit#(TLog#(cRqNum)) n);
    method Maybe#(Bit#(TLog#(cRqNum))) getRepSucc(Bit#(TLog#(cRqNum)) n);
    method Action setData(Bit#(TLog#(cRqNum)) n, Maybe#(Line) d);
    method Action setStateSlot(
        Bit#(TLog#(cRqNum)) n, LLCRqState state, 
        LLCRqSlot#(wayT, tagT, dirPendT) slot
    );
    method Action setAddrSucc( // same address successor
        Bit#(TLog#(cRqNum)) n, 
        Maybe#(Bit#(TLog#(cRqNum))) succ
    );
    method Action setRepSucc( // successor due to replacement
        Bit#(TLog#(cRqNum)) n, 
        Maybe#(Bit#(TLog#(cRqNum))) succ
    );
    // find existing cRq which has gone through pipeline, but not in Done state, and has no addr successor
    // (it could have rep successor)
    // i.e. search the end of dependency chain for req to the same addr
    method Maybe#(Bit#(TLog#(cRqNum))) searchEndOfChain(Addr addr);
endinterface

interface LLCRqMshr#(
    numeric type cRqNum, 
    type wayT,
    type tagT,
    type dirPendT,
    type reqT // child req type
);
    interface LLCRqMshr_transfer#(cRqNum, wayT, tagT, dirPendT, reqT) transfer;
    interface LLCRqMshr_mRsDeq#(cRqNum) mRsDeq;
    interface LLCRqMshr_sendToM#(cRqNum, wayT, tagT, dirPendT, reqT) sendToM;
    interface LLCRqMshr_sendRsToDmaC#(cRqNum, reqT) sendRsToDmaC;
    interface LLCRqMshr_sendRqToC#(cRqNum, wayT, tagT, dirPendT, reqT) sendRqToC;
    interface LLCRqMshr_pipelineResp#(cRqNum, wayT, tagT, dirPendT, reqT) pipelineResp;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(LLCRqMshrStuck#(dirPendT, reqT)) stuck;
endinterface

function LLCRqSlot#(wayT, tagT, dirPendT) getLLCRqSlotInitVal(dirPendT dirPendInitVal);
    return LLCRqSlot {
        way: ?,
        repTag: ?,
        waitP: False,
        dirPend: dirPendInitVal
    };
endfunction

//////////////////
// safe version //
//////////////////
module mkLLCRqMshr#(
    function Addr getAddrFromReq(reqT r),
    function Bool needDownReq(dirPendT dirPend),
    dirPendT dirPendInitVal
)(
    LLCRqMshr#(cRqNum, wayT, tagT, dirPendT, reqT)
) provisos (
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(slotT, LLCRqSlot#(wayT, tagT, dirPendT)),
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(dirPendT, _dirPendSz),
    Bits#(reqT, _reqSz)
);

   Bool verbose = False;

    slotT slotInitVal = getLLCRqSlotInitVal(dirPendInitVal);

    // logical ordering: sendToM < sendRqToC < sendRsToDma/C < mRsDeq < pipelineResp < transfer
    // We put pipelineResp < transfer to cater for deq < enq of cache pipeline
    // EHR ports
    Integer sendToM_port = 0; // sendToM is read-only, so use port 0
    Integer sendRqToC_port = 0; // read req/state/slot, write slot
    Integer sendRsToDmaC_port = 0; // sendRsToDma/C read req/data, write state
    Integer mRsDeq_port = 0; // mRsDeq only writes data
    Integer pipelineResp_port = 1; // read/write lots of things
    Integer transfer_port = 2; // cRqTransfer_xx, mRsTransfer_send, read/write lots of things

    // cRq req contents
    Vector#(cRqNum, Ehr#(3, reqT)) reqVec <- replicateM(mkEhr(?));
    // MSHR entry state
    Vector#(cRqNum, Ehr#(3, LLCRqState)) stateVec <- replicateM(mkEhr(Empty));
    // summary bit of dirPend in each entry: asserted when some dirPend[i] = ToSend
    Vector#(cRqNum, Ehr#(3, Bool)) needReqChildVec <- replicateM(mkEhr(False));
    // cRq mshr slots
    Vector#(cRqNum, Ehr#(3, slotT)) slotVec <- replicateM(mkEhr(slotInitVal));
    // data valid bit
    Vector#(cRqNum, Ehr#(3, Bool)) dataValidVec <- replicateM(mkEhr(False));
    // data values
    Vector#(cRqNum, Ehr#(3, Line)) dataVec <- replicateM(mkEhr(?));
    // successor valid bit
    Vector#(cRqNum, Ehr#(3, Bool)) addrSuccValidVec <- replicateM(mkEhr(False));
    Vector#(cRqNum, Ehr#(3, Bool)) repSuccValidVec <- replicateM(mkEhr(False));
    // successor MSHR index
    RegFile#(cRqIndexT, cRqIndexT) addrSuccFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    RegFile#(cRqIndexT, cRqIndexT) repSuccFile <- mkRegFile(0, fromInteger(valueOf(cRqNum) - 1));
    // empty entry FIFO
    Fifo#(cRqNum, cRqIndexT) emptyEntryQ <- mkCFFifo;

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(cRqIndexT) initIdx <- mkReg(0);

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(cRqNum) - 1)) begin
            inited <= True;
	   if (verbose)
            $display("%t LLCRqMshrSafe %m: init empty entry done", $time);
        end
    endrule

`ifdef CHECK_DEADLOCK
    MshrDeadlockChecker#(cRqNum) checker <- mkMshrDeadlockChecker;
    FIFO#(LLCRqMshrStuck#(dirPendT, reqT)) stuckQ <- mkFIFO1;

    (* fire_when_enabled *)
    rule checkDeadlock;
        let stuckIdx <- checker.getStuckIdx;
        if(stuckIdx matches tagged Valid .n) begin
            stuckQ.enq(LLCRqMshrStuck {
                req: reqVec[n][0],
                state: stateVec[n][0],
                waitP: slotVec[n][0].waitP,
                dirPend: slotVec[n][0].dirPend
            });
        end
    endrule
`endif

    function Action writeSlot(Integer ehrPort, cRqIndexT n, slotT s);
    action
        slotVec[n][ehrPort] <= s;
        // set dirPend summary bit
        needReqChildVec[n][ehrPort] <= needDownReq(s.dirPend);
    endaction
    endfunction

    interface LLCRqMshr_transfer transfer;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][transfer_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][transfer_port];
        endmethod

        method ActionValue#(cRqIndexT) getEmptyEntryInit(reqT r, Maybe#(Line) d) if(inited);
            emptyEntryQ.deq;
            cRqIndexT n = emptyEntryQ.first;
            reqVec[n][transfer_port] <= r;
            stateVec[n][transfer_port] <= Init;
            writeSlot(transfer_port, n, slotInitVal);
            dataValidVec[n][transfer_port] <= isValid(d);
            dataVec[n][transfer_port] <= validValue(d);
            addrSuccValidVec[n][transfer_port] <= False;
            repSuccValidVec[n][transfer_port] <= False;
`ifdef CHECK_DEADLOCK
            checker.initEntry(n);
`endif
            return n;
        endmethod

        method Bool hasEmptyEntry(reqT r);
            return emptyEntryQ.notEmpty;
        endmethod
    endinterface

    interface LLCRqMshr_mRsDeq mRsDeq;
        method Action setData(cRqIndexT n, Maybe#(Line) d);
            dataValidVec[n][mRsDeq_port] <= isValid(d);
            dataVec[n][mRsDeq_port] <= fromMaybe(?, d);
        endmethod
    endinterface

    interface LLCRqMshr_sendToM sendToM;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][sendToM_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][sendToM_port];
        endmethod

        method Maybe#(Line) getData(cRqIndexT n);
            return dataValidVec[n][sendToM_port] ? Valid (dataVec[n][sendToM_port]) : Invalid;
        endmethod
    endinterface

    interface LLCRqMshr_sendRsToDmaC sendRsToDmaC;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][sendRsToDmaC_port];
        endmethod

        method Maybe#(Line) getData(cRqIndexT n);
            return dataValidVec[n][sendRsToDmaC_port] ? Valid (dataVec[n][sendRsToDmaC_port]) : Invalid;
        endmethod

        method Action releaseEntry(cRqIndexT n) if(inited);
            emptyEntryQ.enq(n);
            stateVec[n][sendRsToDmaC_port] <= Empty;
`ifdef CHECK_DEADLOCK
            checker.releaseEntry(n);
`endif
        endmethod
    endinterface

    interface LLCRqMshr_sendRqToC sendRqToC;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][sendRqToC_port];
        endmethod

        method LLCRqState getState(cRqIndexT n);
            return stateVec[n][sendRqToC_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][sendRqToC_port];
        endmethod

        method Action setSlot(cRqIndexT n, slotT s);
            writeSlot(sendRqToC_port, n, s);
        endmethod

        method Maybe#(cRqIndexT) searchNeedRqChild(Maybe#(cRqIndexT) suggestIdx);
            function Bool isNeedRqChild(cRqIndexT i);
                return (stateVec[i][sendRqToC_port] == WaitOldTag || stateVec[i][sendRqToC_port] == WaitSt)
                    && needReqChildVec[i][sendRqToC_port];
            endfunction
            if(suggestIdx matches tagged Valid .idx &&& isNeedRqChild(idx)) begin
                return suggestIdx;
            end
            else begin
                Vector#(cRqNum, cRqIndexT) idxVec = genWith(fromInteger);
                return searchIndex(isNeedRqChild, idxVec);
            end
        endmethod
    endinterface

    interface LLCRqMshr_pipelineResp pipelineResp;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][pipelineResp_port];
        endmethod

        method LLCRqState getState(cRqIndexT n);
            return stateVec[n][pipelineResp_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][pipelineResp_port];
        endmethod

        method Maybe#(Line) getData(cRqIndexT n);
            return dataValidVec[n][pipelineResp_port] ? Valid (dataVec[n][pipelineResp_port]) : Invalid;
        endmethod

        method Maybe#(cRqIndexT) getAddrSucc(cRqIndexT n);
            return addrSuccValidVec[n][pipelineResp_port] ? Valid (addrSuccFile.sub(n)) : Invalid;
        endmethod

        method Maybe#(cRqIndexT) getRepSucc(cRqIndexT n);
            return repSuccValidVec[n][pipelineResp_port] ? Valid (repSuccFile.sub(n)) : Invalid;
        endmethod

        method Action setData(cRqIndexT n, Maybe#(Line) d);
            dataValidVec[n][pipelineResp_port] <= isValid(d);
            dataVec[n][pipelineResp_port] <= fromMaybe(?, d);
        endmethod

        method Action setStateSlot(cRqIndexT n, LLCRqState state, slotT slot);
            stateVec[n][pipelineResp_port] <= state;
            writeSlot(pipelineResp_port, n, slot);
        endmethod

        method Action setAddrSucc(cRqIndexT n, Maybe#(cRqIndexT) succ);
            addrSuccValidVec[n][pipelineResp_port] <= isValid(succ);
            addrSuccFile.upd(n, fromMaybe(?, succ));
        endmethod

        method Action setRepSucc(cRqIndexT n, Maybe#(cRqIndexT) succ);
            repSuccValidVec[n][pipelineResp_port] <= isValid(succ);
            repSuccFile.upd(n, fromMaybe(?, succ));
        endmethod

        method Maybe#(cRqIndexT) searchEndOfChain(Addr addr);
            function Bool isEndOfChain(Integer i);
                // check entry i is end of chain or not
                let state = stateVec[i][pipelineResp_port];
                Bool notDone = state != Done;
                Bool processedOnce = state != Empty && state != Init;
                Bool addrMatch = getLineAddr(getAddrFromReq(reqVec[i][pipelineResp_port])) == getLineAddr(addr);
                Bool noAddrSucc = !addrSuccValidVec[i][pipelineResp_port];
                return notDone && processedOnce && addrMatch && noAddrSucc;
            endfunction
            Vector#(cRqNum, Integer) idxVec = genVector;
            return searchIndex(isEndOfChain, idxVec);
        endmethod
    endinterface

`ifdef CHECK_DEADLOCK
    interface stuck = toGet(stuckQ);
`else
    interface stuck = nullGet;
`endif
endmodule

