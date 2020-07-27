
// Copyright (c) 2019 Massachusetts Institute of Technology
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
import LLCRqMshr::*;

// SECURITY: this module is a quick and dirty way to model the effect of MSHR
// partition in case of a banked LLC. That is, a single core roughly sees a
// MSHR, which is not fully associative but set associated and indexed by cache
// bank ID. This is purely for **performance modeling**, not a real
// implementation.


// MSHR dependency chain invariant:
// every cRq and pRq (for same addr) which has gone through pipeline once will be linked into the chain

// in LLC, the head (h1) of a chain may be linked as the successor of the head (h2) of another chain
// when h2 is replacing the addr of h1
// h1 should be waken up and sent to pipeline when replacement is done (i.e. h2 gets to WaitSt)

interface LLCRqMshrSecureModel#(
    numeric type lgBankNum,
    numeric type cRqNum, 
    type wayT,
    type tagT,
    type dirPendT,
    type reqT // child req type
);
    interface LLCRqMshr#(cRqNum, wayT, tagT, dirPendT, reqT) mshr;
endinterface

//////////////////
// safe version //
//////////////////
module mkLLCRqMshrSecureModel#(
    function Addr getAddrFromReq(reqT r),
    function Bool needDownReq(dirPendT dirPend),
    dirPendT dirPendInitVal
)(
    LLCRqMshrSecureModel#(lgBankNum, cRqNum, wayT, tagT, dirPendT, reqT)
) provisos (
    Alias#(bankIdT, Bit#(lgBankNum)),
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(slotT, LLCRqSlot#(wayT, tagT, dirPendT)),
    Alias#(wayT, Bit#(_waySz)),
    Alias#(tagT, Bit#(_tagSz)),
    Bits#(dirPendT, _dirPendSz),
    Bits#(reqT, _reqSz),
    NumAlias#(bankNum, TExp#(lgBankNum)),
    Mul#(bankNum, cRqPerBankNum, cRqNum),
    Add#(a__, lgBankNum, AddrSz)
);
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
    Vector#(bankNum, Fifo#(cRqPerBankNum, cRqIndexT)) emptyEntryQ <- replicateM(mkCFFifo);

    // empty entry FIFO needs initialization
    Reg#(Bool) inited <- mkReg(False);
    Reg#(cRqIndexT) initIdx <- mkReg(0);
    Reg#(bankIdT) initBank <- mkReg(0);
    Reg#(Bit#(TLog#(cRqPerBankNum))) initPerBankCnt <- mkReg(0);

    rule initEmptyEntry(!inited);
        $display("%t LLCRqMshrSecureModel %m: init empty entry %d to bank %d",
                 $time, initIdx, initBank);
        emptyEntryQ[initBank].enq(initIdx);
        // update initIdx
        initIdx <= initIdx + 1;
        if(initIdx == fromInteger(valueOf(cRqNum) - 1)) begin
            inited <= True;
            $display("%t LLCRqMshrSecureModel %m: init empty entry done", $time);
        end
        // update initPerBank and initBank
        if(initPerBankCnt == fromInteger(valueof(cRqPerBankNum) - 1)) begin
            initPerBankCnt <= 0;
            initBank <= initBank + 1;
        end
        else begin
            initPerBankCnt <= initPerBankCnt + 1;
        end
    endrule

    function bankIdT getBankId(Addr a);
        return truncate(a >> valueof(LgLineSzBytes));
    endfunction

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

    interface LLCRqMshr mshr;

    interface LLCRqMshr_transfer transfer;
        method reqT getRq(cRqIndexT n);
            return reqVec[n][transfer_port];
        endmethod

        method slotT getSlot(cRqIndexT n);
            return slotVec[n][transfer_port];
        endmethod

        method ActionValue#(cRqIndexT) getEmptyEntryInit(reqT r, Maybe#(Line) d) if(inited);
            bankIdT bank = getBankId(getAddrFromReq(r));
            emptyEntryQ[bank].deq;
            cRqIndexT n = emptyEntryQ[bank].first;
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
            bankIdT bank = getBankId(getAddrFromReq(r));
            return emptyEntryQ[bank].notEmpty;
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
            bankIdT bank = getBankId(getAddrFromReq(reqVec[n][sendRsToDmaC_port]));
            emptyEntryQ[bank].enq(n);
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

    endinterface
endmodule
