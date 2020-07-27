
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
import GetPut::*;
import Types::*;
import CCTypes::*;
import DefaultValue::*;
import Ehr::*;
import Fifos::*;
import MshrDeadlockChecker::*;

// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// in L1, pRq is always directly handled at the end of pipeline

// PRq MSHR entry state
typedef enum {
    Empty,
    Init,
    Done
} IPRqState deriving (Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    IPRqState state;
} IPRqMshrStuck deriving(Bits, Eq, FShow);

interface IPRqMshr_sendRsToP_pRq#(numeric type pRqNum);
    method PRqMsg#(void) getRq(Bit#(TLog#(pRqNum)) n);
    method Action releaseEntry(Bit#(TLog#(pRqNum)) n);
endinterface

interface IPRqMshr_pipelineResp#(numeric type pRqNum);
    method PRqMsg#(void) getRq(Bit#(TLog#(pRqNum)) n);
    method Action releaseEntry(Bit#(TLog#(pRqNum)) n);
    method Action setDone(Bit#(TLog#(pRqNum)) n);
`ifdef SECURITY
    method Action setFlushAddr(Bit#(TLog#(pRqNum)) n, Addr a);
`endif
endinterface

interface IPRqMshr#(numeric type pRqNum);
    // port for pRqTransfer
    method ActionValue#(Bit#(TLog#(pRqNum))) getEmptyEntryInit(PRqMsg#(void) r);

    // port for sendRsToP_pRq
    interface IPRqMshr_sendRsToP_pRq#(pRqNum) sendRsToP_pRq;

    // port for pipelineResp
    interface IPRqMshr_pipelineResp#(pRqNum) pipelineResp;

    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(IPRqMshrStuck) stuck;
endinterface


//////////////////
// safe version //
//////////////////
module mkIPRqMshrSafe(
    IPRqMshr#(pRqNum)
) provisos(
    Alias#(pRqIndexT, Bit#(TLog#(pRqNum)))
);
   Bool verbose = False;

    // EHR port
    // We put pipelineResp < transfer to cater for deq < enq of cache pipeline
    Integer pRqTransfer_port = 2;
    Integer sendRsToP_pRq_port = 1;
    Integer pipelineResp_port = 0;

    // MSHR entry state
    Vector#(pRqNum, Ehr#(3, IPRqState)) stateVec <- replicateM(mkEhr(Empty));
    Vector#(pRqNum, Ehr#(3, PRqMsg#(void))) reqVec <- replicateM(mkEhr(?));
    // empty entry FIFO
    FIFO#(pRqIndexT) emptyEntryQ <- mkSizedFIFO(valueOf(pRqNum));

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(pRqIndexT) initIdx <- mkReg(0);

    // released entry index fifos
    Fifo#(1, pRqIndexT) releaseEntryQ_sendRsToP_pRq <- mkBypassFifo;
    Fifo#(1, pRqIndexT) releaseEntryQ_pipelineResp  <- mkBypassFifo;

    rule initEmptyEntry(!inited);
        emptyEntryQ.enq(initIdx);
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(pRqNum) - 1)) begin
            inited <= True;
	   if (verbose)
            $display("%t IPRqMshrSafe %m: init empty entry done", $time);
        end
    endrule

`ifdef CHECK_DEADLOCK
    MshrDeadlockChecker#(pRqNum) checker <- mkMshrDeadlockChecker;
    FIFO#(IPRqMshrStuck) stuckQ <- mkFIFO1;

    (* fire_when_enabled *)
    rule checkDeadlock;
        let stuckIdx <- checker.getStuckIdx;
        if(stuckIdx matches tagged Valid .n) begin
            stuckQ.enq(IPRqMshrStuck {
                addr: reqVec[n][0].addr,
                toState: reqVec[n][0].toState,
                state: stateVec[n][0]
            });
        end
    endrule
`endif

    rule doReleaseEntry_sendRsToP_pRq(inited);
        let n <- toGet(releaseEntryQ_sendRsToP_pRq).get;
        emptyEntryQ.enq(n);
`ifdef CHECK_DEADLOCK
        checker.releaseEntry(n);
`endif
    endrule

    (* descending_urgency = "doReleaseEntry_sendRsToP_pRq, doReleaseEntry_pipelineResp" *)
    rule doReleaseEntry_pipelineResp(inited);
        let n <- toGet(releaseEntryQ_pipelineResp).get;
        emptyEntryQ.enq(n);
`ifdef CHECK_DEADLOCK
        checker.releaseEntry(n);
`endif
    endrule

    method ActionValue#(pRqIndexT) getEmptyEntryInit(PRqMsg#(void) r) if(inited);
        emptyEntryQ.deq;
        pRqIndexT n = emptyEntryQ.first;
        stateVec[n][pRqTransfer_port] <= Init;
        reqVec[n][pRqTransfer_port] <= r;
`ifdef CHECK_DEADLOCK
        checker.initEntry(n);
`endif
        return n;
    endmethod

    interface IPRqMshr_sendRsToP_pRq sendRsToP_pRq;
        method PRqMsg#(void) getRq(pRqIndexT n);
            return reqVec[n][sendRsToP_pRq_port];
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_sendRsToP_pRq.enq(n);
            stateVec[n][sendRsToP_pRq_port] <= Empty;
        endmethod
    endinterface

    interface IPRqMshr_pipelineResp pipelineResp;
        method PRqMsg#(void) getRq(pRqIndexT n);
            return reqVec[n][pipelineResp_port];
        endmethod

        method Action setDone(pRqIndexT n);
            stateVec[n][pipelineResp_port] <= Done;
        endmethod

        method Action releaseEntry(pRqIndexT n) if(inited);
            releaseEntryQ_pipelineResp.enq(n);
            stateVec[n][pipelineResp_port] <= Empty;
        endmethod

`ifdef SECURITY
        method Action setFlushAddr(Bit#(TLog#(pRqNum)) n, Addr a);
            reqVec[n][pipelineResp_port] <= PRqMsg {
                addr: a,
                toState: I,
                child: ?
            };
        endmethod
`endif
    endinterface

`ifdef CHECK_DEADLOCK
    interface stuck = toGet(stuckQ);
`else
    interface stuck = nullGet;
`endif
endmodule


// exportd version
module mkIPRqMshr(IPRqMshr#(pRqNum));
    let m <- mkIPRqMshrSafe;
    return m;
endmodule
