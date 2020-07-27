
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
import FIFO::*;
import GetPut::*;
import Types::*;
import CCTypes::*;
import LLCRqMshr::*;
import CCPipe::*;
import LLPipe ::*;
import FShow::*;
import DefaultValue::*;
import Fifos::*;
import CacheUtils::*;
import Performance::*;
import LatencyTimer::*;
import Cntrs::*;
import ConfigReg::*;
import RandomReplace::*;

export LLCRqStuck(..);
export LLBank(..);
export mkLLBank;

// we use an infoQ to serialize all req to memory
// this also ensures that cache pipeline is never blocked
// so we do not need to buffer mem resp that needs to refill cache

// XXX we need to maintain the invariant that 
// at most 1 pRq sent to a child for an addr
// and we have to wait for resp before sending anthoer one

// In future, it would be hard to parallelize sendRqToC and pipelineResp
// because sendRqToC must see updates to dirPend made in pipelineResp
// or we simply stall sendRqToC

// XXX DMA req are put in the same MSHR as child req
// this ensures that child req always get a cache line to occupy after being sent to pipeline

// DMA req are handled in the following way
// (1) If it hits in LLC, then we can downgrade children and process it
// (2) Otherwise, we req memory, XXX and req is viewed as Done
//     The dep successor is sent to retry instead of being swapped in,
//     because we don't have a cache line to occupy...
//     When the mem resp comes, we send DMA resp and DO NOT fill cache

// XXX mem resp that refill cache are for child req, others that don't refill cache are for DMA req

// We do not need an explict bank id, because bank id is included in cRq.addr

// naming rule: child req type names < dma type names

interface LLBank#(
    numeric type lgBankNum,
    numeric type childNum,
    numeric type wayNum,
    numeric type indexSz,
    numeric type tagSz,
    numeric type cRqNum, // including DMA req
    type cRqIdT,
    type dmaRqIdT
);
    interface ParentCacheToChild#(cRqIdT, Bit#(TLog#(childNum))) to_child;
    interface DmaServer#(dmaRqIdT) dma;
    interface MemFifoClient#(LdMemRqId#(Bit#(TLog#(cRqNum))), void) to_mem;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(LLCRqStuck#(childNum, cRqIdT, dmaRqIdT)) cRqStuck;
    // performance
    method Action setPerfStatus(Bool stats);
    method Data getPerfData(LLCPerfType t);
endinterface

typedef struct {
    LLRqId#(cRqIdT, dmaRqIdT) id;
    Addr addr;
    Msi fromState;
    Msi toState;
    Bit#(TLog#(childNum)) child;
    LLCRqState state;
    Bool waitP;
    Vector#(childNum, DirPend) dirPend;
} LLCRqStuck#(numeric type childNum, type cRqIdT, type dmaRqIdT) deriving(Bits, Eq, FShow);

typedef struct {
    cRqIdT cRqId;
    Msi toState;
} LLRsInfo#(type cRqIdT) deriving(Bits, Eq, FShow);

// to mem info
typedef enum {
    Ld, // read only by child req or dma req
    DmaWr, // dma write only
    RepLd // first write replaced line then load new line (for child req)
} ToMemType deriving(Bits, Eq, FShow);

typedef struct {
    mshrIdxT mshrIdx;
    ToMemType t;
} ToMemInfo#(type mshrIdxT) deriving(Bits, Eq, FShow);

// enum for source of cRq
typedef enum {Child, Dma} LLCRqSrc deriving(Bits, Eq, FShow);

module mkLLBank#(
    module#(LLCRqMshr#(cRqNum, wayT, tagT, Vector#(childNum, DirPend), cRqT)) mkLLMshr,
    module#(LLPipe#(lgBankNum, childNum, wayNum, indexT, tagT, cRqIndexT)) mkLLPipeline,
    // Whether we resp a load request (to S) with E permission. The directory
    // is all I when this function is called. fetchFromMem indicates whether
    // the data is just fetched from DRAM or not. Any function given here will
    // not affect correctness of coherence protocol.
    function Bool respLoadWithE(Bool fetchFromMem)
)(
    LLBank#(lgBankNum, childNum, wayNum, indexSz, tagSz, cRqNum, cRqIdT, dmaRqIdT)
) provisos(
    Alias#(childT, Bit#(TLog#(childNum))),
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(dirT, Vector#(childNum, Msi)),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIndexT, Bit#(TLog#(cRqNum))),
    Alias#(cacheOwnerT, Maybe#(CRqOwner#(cRqIndexT))),
    Alias#(cacheInfoT, CacheInfo#(tagT, Msi, dirT, cacheOwnerT, void)),
    Alias#(ramDataT, RamData#(tagT, Msi, dirT, cacheOwnerT, void, Line)),
    Alias#(cRqFromCT, CRqMsg#(cRqIdT, childT)),
    Alias#(cRsFromCT, CRsMsg#(childT)),
    Alias#(pRqRsToCT, PRqRsMsg#(cRqIdT, childT)),
    Alias#(dmaRqT, DmaRq#(dmaRqIdT)),
    Alias#(dmaRsT, DmaRs#(dmaRqIdT)),
    Alias#(ldMemRqIdT, LdMemRqId#(cRqIndexT)),
    Alias#(memRsT, MemRsMsg#(ldMemRqIdT, void)),
    Alias#(toMemT, ToMemMsg#(ldMemRqIdT, void)),
    Alias#(toMemInfoT, ToMemInfo#(cRqIndexT)),
    Alias#(cRqT, LLRq#(cRqIdT, dmaRqIdT, childT)),
    Alias#(cRqSlotT, LLCRqSlot#(wayT, tagT, Vector#(childNum, DirPend))), // cRq MSHR slot
    Alias#(llCmdT, LLCmd#(childT, cRqIndexT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, dirT, cacheOwnerT, void, RandRepInfo, Line, llCmdT)),
    // requirements
    Bits#(cRqIdT, _cRqIdSz),
    Bits#(dmaRqIdT, _dmaRqIdSz),
    FShow#(cRqIdT),
    FShow#(dmaRqIdT),
    Add#(tagSz, a__, AddrSz),
    // make sure: cRqNum <= wayNum
    Add#(cRqNum, b__, wayNum)
);

   Bool verbose = False;

    LLCRqMshr#(cRqNum, wayT, tagT, Vector#(childNum, DirPend), cRqT) cRqMshr <- mkLLMshr;

    LLPipe#(lgBankNum, childNum, wayNum, indexT, tagT, cRqIndexT) pipeline <- mkLLPipeline;

    Fifo#(2, cRqFromCT) rqFromCQ <- mkCFFifo;
    Fifo#(2, cRsFromCT) rsFromCQ <- mkCFFifo;
    Fifo#(2, pRqRsToCT) toCQ <- mkCFFifo;

    Fifo#(2, dmaRqT) rqFromDmaQ <- mkCFFifo;
    Fifo#(2, dmaRsT) rsLdToDmaQ <- mkCFFifo;
    Fifo#(2, dmaRqIdT) rsStToDmaQ <- mkCFFifo;

    Fifo#(2, toMemT) toMQ <- mkCFFifo;
    Fifo#(2, memRsT) rsFromMQ <- mkCFFifo;

    // mshr index of load or write to memory, all mem accesses are ordered here
    FIFO#(toMemInfoT) toMInfoQ <- mkSizedFIFO(valueOf(cRqNum));
    // mshr index of req that is waken up when replacement is done
    Fifo#(cRqNum, cRqIndexT) cRqRetryIndexQ <- mkCFFifo;
    // mshr index of upgrade resp to child
    Fifo#(cRqNum, LLRsInfo#(cRqIndexT)) rsToCIndexQ <- mkCFFifo;
    // mshr index of resp to dma (merge 3 FIFOs for each enq places)
    FIFO#(cRqIndexT) rsLdToDmaIndexQ <- mkSizedFIFO(valueof(cRqNum));
    FIFO#(cRqIndexT) rsStToDmaIndexQ <- mkSizedFIFO(valueof(cRqNum));
    FIFO#(cRqIndexT) rsLdToDmaIndexQ_mRsDeq <- mkFIFO;
    FIFO#(cRqIndexT) rsStToDmaIndexQ_sendToM <- mkFIFO;
    FIFO#(cRqIndexT) rsLdToDmaIndexQ_pipelineResp <- mkFIFO;
    FIFO#(cRqIndexT) rsStToDmaIndexQ_pipelineResp <- mkFIFO;

`ifdef DEBUG_DMA
    // these FIFOs are enqueued when DMA req really takes effect
    // FIFOs has 0 cycle latency to match L1Bank resp latency
    Fifo#(1, dmaRqIdT) dmaWrMissQ <- mkBypassFifo;
    Fifo#(1, dmaRqIdT) dmaWrHitQ <- mkBypassFifo;
    Fifo#(1, dmaRqIdT) dmaRdMissQ <- mkBypassFifo;
    Fifo#(1, dmaRqIdT) dmaRdHitQ <- mkBypassFifo;
`endif

    // performance
`ifdef PERF_COUNT
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) dmaMemLdCnt <- mkCount(0);
    Count#(Data) dmaMemLdLat <- mkCount(0);
    Count#(Data) normalMemLdCnt <- mkCount(0);
    Count#(Data) normalMemLdLat <- mkCount(0);
    Count#(Data) mshrBlocks <- mkCount(0);
    Count#(Data) downRespCnt <- mkCount(0);
    Count#(Data) downRespDataCnt <- mkCount(0);
    Count#(Data) downReqCnt <- mkCount(0);
    Count#(Data) upRespCnt <- mkCount(0);
    Count#(Data) upRespDataCnt <- mkCount(0);
    Count#(Data) dmaLdReqCnt <- mkCount(0);
    Count#(Data) dmaStReqCnt <- mkCount(0);
    
    LatencyTimer#(cRqNum, 10) latTimer <- mkLatencyTimer; // max 1K cycle latency

    function Action incrMissCnt(cRqIndexT idx, Bool isDma);
    action
        let lat <- latTimer.done(idx);
        if(doStats) begin
            if(isDma) begin
                dmaMemLdCnt.incr(1);
                dmaMemLdLat.incr(zeroExtend(lat));
            end
            else begin
                normalMemLdCnt.incr(1);
                normalMemLdLat.incr(zeroExtend(lat));
            end
        end
    endaction
    endfunction
`endif

    function tagT getTag(Addr a) = truncateLSB(a);

    function cRqIdT getIdFromC(LLRqId#(cRqIdT, dmaRqIdT) id);
        if(id matches tagged Child .i) begin
            return i;
        end
        else begin
            return ?;
        end
    endfunction

    function dmaRqIdT getIdFromDma(LLRqId#(cRqIdT, dmaRqIdT) id);
        if(id matches tagged Dma .i) begin
            return i;
        end
        else begin
            return ?;
        end
    endfunction

    function Bool isRqFromC(LLRqId#(cRqIdT, dmaRqIdT) id);
        return id matches tagged Child .i ? True : False;
    endfunction

    function Bool isRqFromDma(LLRqId#(cRqIdT, dmaRqIdT) id);
        return id matches tagged Dma .i ? True : False;
    endfunction

    // merge ld resp index to DMA FIFO
    rule mergeRsLdToDmaIndexQ_mRsDeq;
        let idx <- toGet(rsLdToDmaIndexQ_mRsDeq).get;
        rsLdToDmaIndexQ.enq(idx);
    endrule
    (* descending_urgency = "mergeRsLdToDmaIndexQ_pipelineResp, mergeRsLdToDmaIndexQ_mRsDeq" *)
    rule mergeRsLdToDmaIndexQ_pipelineResp;
        let idx <- toGet(rsLdToDmaIndexQ_pipelineResp).get;
        rsLdToDmaIndexQ.enq(idx);
    endrule

    // merge st resp index to DMA FIFO
    rule mergeRsStToDmaIndexQ_sendToM;
        let idx <- toGet(rsStToDmaIndexQ_sendToM).get;
        rsStToDmaIndexQ.enq(idx);
    endrule
    (* descending_urgency = "mergeRsStToDmaIndexQ_pipelineResp, mergeRsStToDmaIndexQ_sendToM" *)
    rule mergeRsStToDmaIndexQ_pipelineResp;
        let idx <- toGet(rsStToDmaIndexQ_pipelineResp).get;
        rsStToDmaIndexQ.enq(idx);
    endrule

    // send retrying cRq to pipeline
    rule cRqTransfer_retry(cRqRetryIndexQ.notEmpty);
        cRqRetryIndexQ.deq;
        cRqIndexT n = cRqRetryIndexQ.first;
        // XXX don't change MSHR entry to Init
        // later cRq to same addr needs to be appended after this one
        // send to pipeline
        cRqT req = cRqMshr.transfer.getRq(n);
        pipeline.send(CRq (LLPipeCRqIn {
            addr: req.addr,
            mshrIdx: n
        }));
       if (verbose)
        $display("%t LL %m cRqTransfer_retry: ", $time, 
            fshow(n), " ; ", 
            fshow(req)
        );
    endrule

    // round-robin for accepting new cRq from child and DMA
    Reg#(LLCRqSrc) priorNewCRqSrc <- mkReg(Child); // src with priority

    function Maybe#(LLCRqSrc) newCRqSrc;
        if(priorNewCRqSrc == Child && rqFromCQ.notEmpty) begin
            return Valid (Child);
        end
        else if(priorNewCRqSrc == Dma && rqFromDmaQ.notEmpty) begin
            return Valid (Dma);
        end
        else begin
            if(rqFromCQ.notEmpty) begin
                return Valid (Child);
            end
            else if(rqFromDmaQ.notEmpty) begin
                return Valid (Dma);
            end
            else begin
                return Invalid;
            end
        end
    endfunction

    function Action flipPriorNewCRqSrc;
    action
        priorNewCRqSrc <= priorNewCRqSrc == Dma ? Child : Dma;
    endaction
    endfunction

    // insert new cRq from child to MSHR and send to pipeline
    rule cRqTransfer_new_child(!cRqRetryIndexQ.notEmpty && newCRqSrc == Valid (Child));
        rqFromCQ.deq;
        cRqFromCT r = rqFromCQ.first;
        cRqT cRq = LLRq {
            addr: r.addr,
            fromState: r.fromState,
            toState: r.toState,
            canUpToE: r.canUpToE,
            child: r.child,
            byteEn: ?,
            id: Child (r.id)
        };
        // setup new MSHR entry
        cRqIndexT n <- cRqMshr.transfer.getEmptyEntryInit(cRq, Invalid);
        // send to pipeline
        pipeline.send(CRq (LLPipeCRqIn {
            addr: cRq.addr, 
            mshrIdx: n
        }));
        // change round robin
        flipPriorNewCRqSrc;
       if (verbose)
        $display("%t LL %m cRqTransfer_new_child: ", $time, 
            fshow(n), " ; ",
            fshow(r), " ; ", 
            fshow(cRq)
        );
    endrule

`ifdef PERF_COUNT
    // perf stats: insert new cRq fails because of full MSHR
    rule cRqTransfer_new_child_block(
        !cRqRetryIndexQ.notEmpty && newCRqSrc == Valid (Child) && doStats
    );
        cRqFromCT r = rqFromCQ.first;
        cRqT cRq = LLRq {
            addr: r.addr,
            fromState: r.fromState,
            toState: r.toState,
            canUpToE: r.canUpToE,
            child: r.child,
            byteEn: ?,
            id: Child (r.id)
        };
        if(!cRqMshr.transfer.hasEmptyEntry(cRq)) begin
            mshrBlocks.incr(1);
        end
    endrule
`endif

    // insert new cRq from DMA to MSHR and send to pipeline
    rule cRqTransfer_new_dma(!cRqRetryIndexQ.notEmpty && newCRqSrc == Valid (Dma));
        rqFromDmaQ.deq;
        dmaRqT r = rqFromDmaQ.first;
        Bool write = r.byteEn != replicate(False);
        cRqT cRq = LLRq {
            addr: r.addr,
            fromState: I,
            toState: write ? M : S, // later on we use toState to distinguish DMA write vs. read
            canUpToE: False, // DMA should not go to E
            child: ?,
            byteEn: r.byteEn,
            id: Dma (r.id)
        };
        // setup new MSHR entry and data
        cRqIndexT n <- cRqMshr.transfer.getEmptyEntryInit(cRq, write ? Valid (r.data) : Invalid);
        // send to pipeline
        pipeline.send(CRq (LLPipeCRqIn {
            addr: cRq.addr, 
            mshrIdx: n
        }));
        // change round robin
        flipPriorNewCRqSrc;
       if (verbose)
        $display("%t LL %m cRqTransfer_new_dma: ", $time, 
            fshow(n), " ; ",
            fshow(r), " ; ", 
            fshow(cRq)
        );
`ifdef PERF_COUNT
        if(doStats) begin
            if(write) begin
                dmaStReqCnt.incr(1);
            end
            else begin
                dmaLdReqCnt.incr(1);
            end
        end
`endif
    endrule

`ifdef PERF_COUNT
    // perf stats: insert new cRq fails because of full MSHR
    rule cRqTransfer_new_dma_block(
        !cRqRetryIndexQ.notEmpty && newCRqSrc == Valid (Dma) && doStats
    );
        dmaRqT r = rqFromDmaQ.first;
        Bool write = r.byteEn != replicate(False);
        cRqT cRq = LLRq {
            addr: r.addr,
            fromState: I,
            toState: write ? M : S,
            canUpToE: False,
            child: ?,
            byteEn: r.byteEn,
            id: Dma (r.id)
        };
        if(!cRqMshr.transfer.hasEmptyEntry(cRq)) begin
            mshrBlocks.incr(1);
        end
    endrule
`endif

    // send downgrade resp from child to pipeline
    rule cRsTransfer;
        rsFromCQ.deq;
        cRsFromCT cRs = rsFromCQ.first;
        pipeline.send(CRs (cRs));
       if (verbose)
        $display("%t LL %m cRsTransfer: ", $time, fshow(cRs));
`ifdef PERF_COUNT
        if(doStats) begin
            downRespCnt.incr(1);
            if(isValid(cRs.data)) begin
                downRespDataCnt.incr(1);
            end
        end
`endif
    endrule
    
    // mem resp for child req, will refill cache, send it to pipeline
    (* descending_urgency = "mRsTransfer, cRsTransfer, cRqTransfer_retry, cRqTransfer_new_child, cRqTransfer_new_dma" *)
`ifdef PERF_COUNT
    // stop mshr block stats when other higher priority req is being sent to
    // pipeline
    (* preempts = "mRsTransfer, cRqTransfer_new_child_block" *)
    (* preempts = "mRsTransfer, cRqTransfer_new_dma_block" *)
    (* preempts = "cRsTransfer, cRqTransfer_new_child_block" *)
    (* preempts = "cRsTransfer, cRqTransfer_new_dma_block" *)
`endif
    rule mRsTransfer(rsFromMQ.first.id.refill);
        // get mem resp cRq index & data
        rsFromMQ.deq;
        memRsT mRs = rsFromMQ.first;
        cRqIndexT n = mRs.id.mshrIdx;
        Line respData = mRs.data;
        // get correspond cRq & slot
        cRqT cRq = cRqMshr.transfer.getRq(n);
        cRqSlotT cSlot = cRqMshr.transfer.getSlot(n);
        doAssert(isRqFromC(cRq.id), "refill mem resp must be for child req");
        // send to pipeline
        pipeline.send(MRs (LLPipeMRsIn {
            addr: cRq.addr,
            toState: cRq.toState == M ? M : E, // set upgrade state
            data: respData,
            way: cSlot.way
        }));
       if (verbose)
        $display("%t LL %m mRsTransfer: ", $time,
            fshow(mRs), " ; ",
            fshow(cRq), " ; ",
            fshow(cSlot), " ; "
        );
`ifdef PERF_COUNT
        // performance counter: normal miss lat and cnt
        incrMissCnt(n, False);
`endif
    endrule

    // this mem resp is just for a DMA req, won't go into pipeline to refill cache
    rule mRsDeq_nonRefill(!rsFromMQ.first.id.refill);
        rsFromMQ.deq;
        memRsT mRs = rsFromMQ.first;
       if (verbose)
        $display("%t LL %m mRsDeq_nonRefill: ", $time, fshow(mRs));
        // save data into cRq mshr & send to DMA resp IndexQ
        cRqMshr.mRsDeq.setData(mRs.id.mshrIdx, Valid (mRs.data));
        rsLdToDmaIndexQ_mRsDeq.enq(mRs.id.mshrIdx);
`ifdef PERF_COUNT
        // performance counter: dma miss lat and cnt
        incrMissCnt(mRs.id.mshrIdx, True);
`endif
    endrule

    // send rd/wr to mem
    // when sending to mem, we may first write back replaced line then load new line
    // this reg records when we should to the load new line for this case
    Reg#(Bool) doLdAfterReplace <- mkReg(False);
    rule sendToM;
        ToMemType t = toMInfoQ.first.t;
        cRqIndexT n = toMInfoQ.first.mshrIdx;
        cRqT cRq = cRqMshr.sendToM.getRq(n);
        cRqSlotT cSlot = cRqMshr.sendToM.getSlot(n);
        Maybe#(Line) data = cRqMshr.sendToM.getData(n);
       if (verbose)
        $display("%t LL %m sendToM: ", $time, 
            fshow(toMInfoQ.first), " ; ",
            fshow(cRq), " ; ",
            fshow(cSlot), " ; ",
            fshow(data), " ; ",
            fshow(doLdAfterReplace)
        );
        // take actions according to type
        if(t == Ld) begin
            // only load mem: can be child or dma req
            toMemT msg = Ld (LdMemRq {
                addr: cRq.addr,
                child: ?,
                id: LdMemRqId {
                    // child rq needs refill cache line, dma rq does not
                    refill: isRqFromC(cRq.id),
                    mshrIdx: n
                }
            });
            toMQ.enq(msg);
            toMInfoQ.deq; // deq info
	   if (verbose)
            $display("%t LL %m sendToM: load only: ", $time, fshow(msg));
            doAssert(!isValid(data), "cannot have data");
            doAssert(!doLdAfterReplace, "doLdAfterReplace should be false");
`ifdef PERF_COUNT
            // performance counter: start miss timer
            latTimer.start(n);
`endif
`ifdef DEBUG_DMA
            if(cRq.id matches tagged Dma .dmaId) begin
                dmaRdMissQ.enq(dmaId); // DMA read takes effect
            end
`endif
        end
        else if(t == DmaWr) begin
            // only write mem: must be dma req
            toMemT msg = Wb (WbMemRs {
                addr: cRq.addr,
                byteEn: cRq.byteEn,
                data: validValue(data)
            });
            toMQ.enq(msg);
            toMInfoQ.deq; // deq info
            // dma write can be resp (i.e. mshr entry can be released)
            rsStToDmaIndexQ_sendToM.enq(n);
	   if (verbose)
            $display("%t LL %m sendToM: dma write: ", $time, fshow(msg));
            doAssert(isRqFromDma(cRq.id), "must be dma write");
            doAssert(isValid(data), "dma write must have data");
            doAssert(!doLdAfterReplace, "doLdAfterReplace should be false");
`ifdef DEBUG_DMA
            dmaWrMissQ.enq(getIdFromDma(cRq.id)); // DMA write takes effect
`endif
        end
        else if(t == RepLd) begin
            // write back replaced line then load, must be child req
            if(doLdAfterReplace) begin // do load part
                toMemT msg = Ld (LdMemRq {
                    addr: cRq.addr,
                    child: ?,
                    id: LdMemRqId {
                        refill: True,
                        mshrIdx: n
                    }
                });
                toMQ.enq(msg);
                // whole thing is done, reset bit and deq info
                toMInfoQ.deq;
                doLdAfterReplace <= False;
	       if (verbose)
                $display("%t LL %m sendToM: rep then ld: ld: ", $time, fshow(msg));
`ifdef PERF_COUNT
                // performance counter: start miss timer
                latTimer.start(n);
`endif
            end
            else begin // do write back part
                toMemT msg = Wb (WbMemRs {
                    addr: {cSlot.repTag, truncate(cRq.addr)},
                    byteEn: replicate(True),
                    data: validValue(data)
                });
                toMQ.enq(msg);
                // don't deq info, do ld next time
                doLdAfterReplace <= True;
	       if (verbose)
                $display("%t LL %m sendToM: rep then ld: rep: ", $time, fshow(msg));
            end
            doAssert(isRqFromC(cRq.id), "must be child req");
            doAssert(isValid(data), "replace must have data");
        end
        else begin
            doAssert(False, "unknown to mem type");
        end
    endrule

    // send DMA resp
    rule sendRsLdToDma;
        rsLdToDmaIndexQ.deq;
        cRqIndexT n = rsLdToDmaIndexQ.first;
        cRqT cRq = cRqMshr.sendRsToDmaC.getRq(n);
        Maybe#(Line) data = cRqMshr.sendRsToDmaC.getData(n);
       if (verbose)
        $display("%t LL %m sendRsToDma: Ld: ", $time,
            fshow(n), " ; ",
            fshow(cRq), " ; ",
            fshow(data)
        );
        doAssert(isValid(data), "dma read req always has valid data");
        // send DMA resp
        doAssert(isRqFromDma(cRq.id), "cRq should be DMA req");
        doAssert(cRq.byteEn == replicate(False) && cRq.toState == S,
            "cRq should be DMA read"
        );
        dmaRqIdT dmaId = getIdFromDma(cRq.id);
        rsLdToDmaQ.enq(DmaRs {
            data: validValue(data),
            id: dmaId
        });
        // release MSHR entry
        cRqMshr.sendRsToDmaC.releaseEntry(n);
    endrule

    rule sendRsStToDma;
        rsStToDmaIndexQ.deq;
        cRqIndexT n = rsStToDmaIndexQ.first;
        cRqT cRq = cRqMshr.sendRsToDmaC.getRq(n);
       if (verbose)
        $display("%t LL %m sendRsToDma: St: ", $time,
            fshow(n), " ; ",
            fshow(cRq)
        );
        // send DMA resp
        doAssert(isRqFromDma(cRq.id), "cRq should be DMA req");
        doAssert(cRq.byteEn != replicate(False) && cRq.toState == M,
            "cRq should be DMA write"
        );
        dmaRqIdT dmaId = getIdFromDma(cRq.id);
        rsStToDmaQ.enq(dmaId);
        // release MSHR entry
        cRqMshr.sendRsToDmaC.releaseEntry(n);
    endrule

    // send upgrade resp to child
    rule sendRsToC(rsToCIndexQ.notEmpty);
        // send upgrade resp to child
        rsToCIndexQ.deq;
        cRqIndexT n = rsToCIndexQ.first.cRqId;
        Msi toState = rsToCIndexQ.first.toState;
        cRqT cRq = cRqMshr.sendRsToDmaC.getRq(n);
        Maybe#(Line) rsData = cRqMshr.sendRsToDmaC.getData(n);
       if (verbose)
        $display("%t LL %m sendRsToC: ", $time, 
            fshow(n), " ; ", 
            fshow(cRq), " ; ", 
            fshow(rsData), " ; ",
            fshow(toState)
        );
        // send resp to child
        doAssert(isRqFromC(cRq.id), "cRq should be child req");
        cRqIdT cRqId = getIdFromC(cRq.id);
        toCQ.enq(PRs (PRsMsg {
            addr: cRq.addr,
            toState: toState, // we may upgrade to E for req S, don't use toState in cRq
            child: cRq.child,
            data: rsData,
            id: cRqId
        }));
        // release MSHR entry
        cRqMshr.sendRsToDmaC.releaseEntry(n);
`ifdef PERF_COUNT
        if(doStats) begin
            upRespCnt.incr(1);
            if(isValid(rsData)) begin
                upRespDataCnt.incr(1);
            end
        end
`endif
    endrule

    // send downgrade req to child
    // round robin select cRq to downgrade child
    // but downgrade must wait for all upgrade resp
    Reg#(cRqIndexT) whichCRq <- mkReg(0);
    
    // we don't perform sending downgrade to child for a cRq processing by pipelineResp_cRs
    // otherwise atomicity issue may arise
    // for safety, we check cRq processed by all pipelineResp_xxx rules
    // this won't create unnecessary stalls
    // (because the cRq by pipelineResp won't match the one chosen for child downgrade)
    function Bool notPipelineResp(cRqIndexT n);
        if(pipeline.notEmpty) begin
            // use unguarded version for pipeline.first
            // (if use guarded version, compiler must lift guard correctly)
            llCmdT cmd = pipeline.unguard_first.cmd;
            cacheOwnerT owner = pipeline.unguard_first.ram.info.owner;
            Bool ownerMatch = isValid(owner) ? fromMaybe(?, owner).mshrIdx == n : False;
            return (case(cmd) matches
                tagged LLCRq .m: return m != n;
                tagged LLMRs: return !ownerMatch;
                tagged LLCRs .c: return !ownerMatch;
                default: return True;
            endcase);
        end
        else begin
            return True;
        end
    endfunction

    rule sendRqToC(!rsToCIndexQ.notEmpty);
        Maybe#(cRqIndexT) cRqNeedDown = cRqMshr.sendRqToC.searchNeedRqChild(Valid (whichCRq));
        // XXX must add this into guard
        // otherwise this rule will block pipelineResp rule from firing forever
        check(isValid(cRqNeedDown));

        cRqIndexT n = fromMaybe(?, cRqNeedDown);
        // XXX stall if cRq n is processed in pipelineResp
        check(notPipelineResp(n));

        cRqT cRq = cRqMshr.sendRqToC.getRq(n);
        cRqSlotT cSlot = cRqMshr.sendRqToC.getSlot(n);
        LLCRqState cState = cRqMshr.sendRqToC.getState(n);
        doAssert(cState == WaitSt || cState == WaitOldTag, 
            "only WaitSt and WaitOldTag needs req child"
        );
        // find a child to downgrade
        function Bool needSend(DirPend dp);
            return dp matches tagged ToSend .s ? True : False;
        endfunction
        Maybe#(childT) childToDown = searchIndex(needSend, cSlot.dirPend);
        doAssert(isValid(childToDown), ("should have a child to downgrade"));
        childT child = fromMaybe(?, childToDown);
        // get the state to downgrade to
        Msi toState = ?;
        if(cSlot.dirPend[child] matches tagged ToSend .st) begin
            toState = st;
        end
        else begin
            doAssert(False, ("dirPend should be ToSend"));
        end
        // send downgrade req: addr depends on state
        // either be cRq addr or replacing addr
        Addr rqAddr = cState == WaitSt ? cRq.addr : {cSlot.repTag, truncate(cRq.addr)};
        pRqRsToCT req = PRq (PRqMsg {
            addr: rqAddr,
            toState: toState,
            child: child
        });
        toCQ.enq(req);
        // change dirPend
        Vector#(childNum, DirPend) newDirPend = cSlot.dirPend;
        newDirPend[child] = Waiting (toState);
        cRqMshr.sendRqToC.setSlot(n, LLCRqSlot { // keep cState the same
            way: cSlot.way,
            repTag: cSlot.repTag,
            waitP: cSlot.waitP,
            dirPend: newDirPend
        });
       if (verbose)
        $display("%t LL %m sendRqToC: ", $time,
            fshow(n), " ; ",
            fshow(cRq), " ; ",
            fshow(cSlot), " ; ",
            fshow(cState), " ; ",
            fshow(req)
        );
        // change round-robin
        whichCRq <= whichCRq == fromInteger(valueOf(cRqNum) - 1) ? 0 : whichCRq + 1;
`ifdef PERF_COUNT
        if(doStats) begin
            downReqCnt.incr(1);
        end
`endif
    endrule

    // Final stage of pipeline: process all kinds of msg
    pipeOutT pipeOut = pipeline.first;
    ramDataT ram = pipeOut.ram;
    // select getCSlot, getRepSucc from cRqMshr
    cRqIndexT pipeOutCRqIdx = (case(pipeOut.cmd) matches
        tagged LLCRq .n: return n;
        default: return ram.info.owner matches tagged Valid .cOwner ? cOwner.mshrIdx : 0; // LLMRs LLCRs
    endcase);
    cRqSlotT pipeOutCSlot = cRqMshr.pipelineResp.getSlot(pipeOutCRqIdx);
    Maybe#(cRqIndexT) pipeOutRepSucc = cRqMshr.pipelineResp.getRepSucc(pipeOutCRqIdx);
    Maybe#(cRqIndexT) pipeOutAddrSucc = cRqMshr.pipelineResp.getAddrSucc(pipeOutCRqIdx);
    LLCRqState pipeOutCState = cRqMshr.pipelineResp.getState(pipeOutCRqIdx);
    cRqT pipeOutCRq = cRqMshr.pipelineResp.getRq(pipeOutCRqIdx);

    // function to process cRq hit (MSHR slot may have garbage)
    function Action cRqFromCHit(cRqIndexT n, cRqT cRq, Bool isMRs);
    action
       if (verbose)
        $display("%t LL %m pipelineResp: cRq from child Hit func: ", $time, 
            fshow(n), " ; ",
            fshow(cRq)
        );
        doAssert(n == pipeOutCRqIdx, "must match pipe out cRq idx");
        doAssert(isRqFromC(cRq.id), "should be cRq from child");
        doAssert(ram.info.tag == getTag(cRq.addr) && ram.info.cs > I,
            // this function is called by mRs, cRq, cRs
            // tag should match even for mRs, because 
            // tag has been written into cache before sending req to parent
            ("cRqHit but tag or cs incorrect")
        );
        // decide upgrade state
        Msi toState = cRq.toState;
        if(cRq.toState == S && cRq.canUpToE && ram.info.dir == replicate(I) && respLoadWithE(isMRs)) begin
            toState = E;
        end
        // update slot, data & send to indexQ
        // decide data validity using dir (which is more up to date than fromState)
        rsToCIndexQ.enq(LLRsInfo {
            cRqId: n,
            toState: toState
        });
        cRqMshr.pipelineResp.setStateSlot(n, Done, ?); // we no longer need slot info
        cRqMshr.pipelineResp.setData(n, ram.info.dir[cRq.child] == I ? Valid (ram.line) : Invalid);
        // update child dir
        dirT newDir = ram.info.dir;
        newDir[cRq.child] = toState;
        // update cs (may have E -> M)
        Msi newCs = ram.info.cs;
        if(toState == M) begin
            newCs = M;
        end
        // deq pipeline or swap in successor
        Maybe#(cRqIndexT) succ = pipeOutAddrSucc;
        pipeline.deqWrite(succ, RamData {
            info: CacheInfo {
                tag: getTag(cRq.addr), // should be the same as original tag
                cs: newCs,
                dir: newDir,
                owner: (case(succ) matches // pass owner to successor
                    tagged Valid .m: return Valid (CRqOwner {
                        mshrIdx: m,
                        replacing: False // succesor cannot replace
                    });
                    default: return Invalid;
                endcase),
                other: ?
            },
            line: ram.line // use line in ram
        }, True); // hit, so update rep info
    endaction
    endfunction

    // function to process DMA req hit (MSHR slot may have garbage)
    function Action cRqFromDmaHit(cRqIndexT n, cRqT cRq);
    action
       if (verbose)
        $display("%t LL %m pipelineResp: cRq from dma Hit func: ", $time, 
            fshow(n), " ; ",
            fshow(cRq)
        );
        doAssert(n == pipeOutCRqIdx, "must match pipe out cRq idx");
        doAssert(isRqFromDma(cRq.id), "should be cRq from dma");
        doAssert(ram.info.tag == getTag(cRq.addr) && ram.info.cs > I,
            "cRqHit but tag or cs incorrect"
        );
        doAssert((cRq.byteEn != replicate(False)) == (cRq.toState == M), "toState should match byteEn");
        // update cs (may have E -> M)
        Msi newCs = ram.info.cs;
        if(cRq.toState == M) begin
            newCs = M;
        end
        // update cache line
        Maybe#(Line) wrData = cRqMshr.pipelineResp.getData(n);
        doAssert(isValid(wrData) == (cRq.byteEn != replicate(False)),
            "dma write should carry valid data"
        );
        Line newLine = getUpdatedLine(ram.line, cRq.byteEn, validValue(wrData));
        // deq pipeline or swap in successor
        Maybe#(cRqIndexT) succ = pipeOutAddrSucc;
        pipeline.deqWrite(succ, RamData {
            info: CacheInfo {
                tag: getTag(cRq.addr), // should be the same as original tag
                cs: newCs,
                dir: ram.info.dir, // dir does not change
                owner: (case(succ) matches // pass owner to successor
                    tagged Valid .m: return Valid (CRqOwner {
                        mshrIdx: m,
                        replacing: False // succesor cannot replace
                    });
                    default: return Invalid;
                endcase),
                other: ?
            },
            line: newLine // use new line
        }, True); // hit, so update rep info
        // update slot, data & send to indexQ
        cRqMshr.pipelineResp.setStateSlot(n, Done, ?); // we no longer need  slot info
        cRqMshr.pipelineResp.setData(n, Valid (ram.line)); // save the orig cache line
        if(cRq.toState == M) begin
            rsStToDmaIndexQ_pipelineResp.enq(n);
        end
        else begin
            rsLdToDmaIndexQ_pipelineResp.enq(n);
        end
`ifdef DEBUG_DMA
        // DMA req takes effect
        dmaRqIdT dmaId = getIdFromDma(cRq.id);
        if(cRq.toState == M) begin
            dmaWrHitQ.enq(dmaId);
        end
        else begin
            dmaRdHitQ.enq(dmaId);
        end
`endif
    endaction
    endfunction

    // function to directly evict a cache line by a cRq from child (replacement)
    function Action cRqFromCEvict(cRqIndexT n, cRqT cRq, Maybe#(cRqIndexT) repSucc);
    action
        doAssert(isRqFromC(cRq.id), "only cRq from child can evict a line");
        doAssert(ram.info.dir == replicate(I) && ram.info.cs > I,
            "only evict valid line which has no children"
        );
        // write back (only when line is M, otherwise silent drop)
        // and req memory for new data (because cs is I now)
        Bool needWB = ram.info.cs == M;
        toMInfoQ.enq(ToMemInfo{
            mshrIdx: n,
            t: needWB ? RepLd : Ld
        });
        // update MSHR
        cRqMshr.pipelineResp.setStateSlot(n, WaitSt, LLCRqSlot {
            way: pipeOut.way, // use way from pipeline
            repTag: ram.info.tag, // record old tag for replacement to mem
            waitP: True, // waiting for mem resp
            dirPend: replicate(Invalid) // children are all I
        });
        cRqMshr.pipelineResp.setData(n, needWB ? Valid (ram.line) : Invalid); // replacement needs data
        // wake up successor which depends on this replacement
        if(repSucc matches tagged Valid .m) begin
            cRqRetryIndexQ.enq(m);
        end
        // deq pipe & change RAM
        pipeline.deqWrite(Invalid, RamData {
            info: CacheInfo {
                tag: getTag(cRq.addr), // set to new tag (old tag is replaced)
                cs: I,
                dir: replicate(I),
                owner: Valid (CRqOwner {
                    mshrIdx: n, // owner is current cRq
                    replacing: False // replacement is done right now
                }),
                other: ?
            },
            line: ? // data is no longer used
        }, False);
    endaction
    endfunction

    // handle cRq
    rule pipelineResp_cRq(pipeOut.cmd matches tagged LLCRq .n);
       if (verbose)
        $display("%t LL %m pipelineResp: ", $time, fshow(pipeOut));
        // cs and dir in ram have been merged with modification caused by mRs/cRs cmd

        cRqT cRq = pipeOutCRq;
       if (verbose)
        $display("%t LL %m pipelineResp: cRq: ", $time, fshow(n), " ; ", fshow(cRq));
        
        // find end of dependency chain
        Maybe#(cRqIndexT) cRqEOC = cRqMshr.pipelineResp.searchEndOfChain(cRq.addr);

        // function to check whether children needs downgrade for req from child (miss no replace)
        function Vector#(childNum, DirPend) getDirPendNonCompatForChild;
            function DirPend initPend(childT i);
                if(i == cRq.child) begin
                    return ram.info.dir[i] <= cRq.fromState ? Invalid : Waiting (cRq.fromState);
                end
                else begin
                    Msi compatState = toCompat(cRq.toState);
                    return ram.info.dir[i] <= compatState ? Invalid : ToSend (compatState);
                end
            endfunction
            Vector#(childNum, childT) idxVec = genWith(fromInteger);
            return map(initPend, idxVec);
        endfunction

        // function to check whether children needs downgrade for DMA req (miss by children state)
        function Vector#(childNum, DirPend) getDirPendNonCompatForDma;
            function DirPend initPend(childT i);
                Msi compatState = toCompat(cRq.toState);
                return ram.info.dir[i] <= compatState ? Invalid : ToSend (compatState);
            endfunction
            Vector#(childNum, childT) idxVec = genWith(fromInteger);
            return map(initPend, idxVec);
        endfunction

        // function to check whether children needs downgrade (miss with replace)
        function Vector#(childNum, DirPend) getDirPendNonI;
            function DirPend initPend(childT i);
                return ram.info.dir[i] == I ? Invalid : ToSend (I);
            endfunction
            Vector#(childNum, childT) idxVec = genWith(fromInteger);
            return map(initPend, idxVec);
        endfunction

        // function to process cRq from child miss without replacement (MSHR slot may have garbage)
        function Action cRqFromCMissNoReplacement(Vector#(childNum, DirPend) dirPend);
        action
            doAssert(isRqFromC(cRq.id), "should be cRq from child");
            // it is impossible in LLC to have slot.waitP == True in this function
            // because there is no pRq in LLC to interrupt a cRq
            cRqSlotT cSlot = pipeOutCSlot;
            doAssert(!cSlot.waitP, "waitP must be false");
            // in LLC, we req memory only when cur cs is I
            if(ram.info.cs == I) begin
                toMInfoQ.enq(ToMemInfo{
                    mshrIdx: n,
                    t: Ld
                });
                doAssert(ram.info.dir == replicate(I), "dir should be all I");
            end
            // update mshr (data field is irrelevant, should be already invalid)
            cRqMshr.pipelineResp.setStateSlot(n, WaitSt, LLCRqSlot {
                way: pipeOut.way, // use way from pipeline
                waitP: ram.info.cs == I,
                repTag: ?, // no replacement
                dirPend: dirPend
            });
            // deq pipeline & set owner, tag
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(cRq.addr), // tag may be garbage if cs == I
                    cs: ram.info.cs,
                    dir: ram.info.dir,
                    owner: Valid (CRqOwner {mshrIdx: n, replacing: False}), // owner is req itself
                    other: ?
                },
                line: ram.line
            }, False);
        endaction
        endfunction

        // function to process cRq from dma miss by children states (MSHR slot may have garbage)
        function Action cRqFromDmaMissByChildren(Vector#(childNum, DirPend) dirPend);
        action
            doAssert(isRqFromDma(cRq.id), "should be cRq from dma");
            doAssert(ram.info.tag == getTag(cRq.addr) && ram.info.cs > I,
                "tag match and cs > I"
            );
            // it is impossible in LLC to have slot.waitP == True in this function
            // because there is no pRq in LLC to interrupt a cRq
            cRqSlotT cSlot = pipeOutCSlot;
            doAssert(!cSlot.waitP, "waitP must be false");
            // update mshr
            cRqMshr.pipelineResp.setStateSlot(n, WaitSt, LLCRqSlot {
                way: pipeOut.way, // use way from pipeline
                waitP: False,
                repTag: ?, // no replacement
                dirPend: dirPend
            });
            // deq pipeline & set owner, tag
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(cRq.addr),
                    cs: ram.info.cs,
                    dir: ram.info.dir,
                    owner: Valid (CRqOwner {mshrIdx: n, replacing: False}), // owner is req itself
                    other: ?
                },
                line: ram.line
            }, False);
        endaction
        endfunction

        // function to do replacement for cRq from child
        function Action cRqFromCReplacement(Vector#(childNum, DirPend) dirPend);
        action
            doAssert(isRqFromC(cRq.id), "should be cRq from child");
            if(dirPend == replicate(Invalid)) begin
                // directly evict the line
                // this cRq cannot have repSucc, since it has not occupied the line
                Maybe#(cRqIndexT) repSucc = pipeOutRepSucc;
                doAssert(!isValid(repSucc), "cannot have rep succ");
                cRqFromCEvict(n, cRq, Invalid);
            end
            else begin
                // wait child to downgrade
                pipeline.deqWrite(Invalid, RamData {
                    info: CacheInfo {
                        tag: ram.info.tag,
                        cs: ram.info.cs,
                        dir: ram.info.dir,
                        owner: Valid (CRqOwner {
                            mshrIdx: n,
                            replacing: True // replacement is ongoing
                        }),
                        other: ?
                    },
                    line: ram.line // keep data the same
                }, False);
                cRqMshr.pipelineResp.setStateSlot(n, WaitOldTag, LLCRqSlot {
                    way: pipeOut.way,
                    repTag: ram.info.tag, // record tag for downgrading children
                    waitP: False,
                    dirPend: dirPend
                });
            end
        endaction
        endfunction
    
        // function to set cRq to Depend, and make no further change to cache
        function Action cRqSetDepNoCacheChange;
        action
            cRqMshr.pipelineResp.setStateSlot(n, Depend, getLLCRqSlotInitVal(getDirPendInitVal));
            pipeline.deqWrite(Invalid, pipeOut.ram, False);
        endaction
        endfunction

        if(ram.info.owner matches tagged Valid .cOwner) begin
            if(cOwner.mshrIdx != n) begin
                // owner is another cRq, so must just go through tag match
                LLCRqState cState = pipeOutCState;
                doAssert(cState == Init, "owner is other, must first time go through tag match");
                // tag match must be hit (because replacement algo won't give a way with owner)
                doAssert(ram.info.cs > I && ram.info.tag == getTag(cRq.addr), 
                    ("cRq should hit in tag match")
                );
                // could be two cases:
                // 1. hit on same addr req
                // 2. hit one cache line replaced by another req to different addr
                // even in 2nd case, we may have existing req to same addr
                // so first check same addr dependency
                if(cRqEOC matches tagged Valid .m) begin
                    // add to same addr dependency
                    cRqMshr.pipelineResp.setAddrSucc(m, Valid (n));
                    cRqSetDepNoCacheChange;
		   if (verbose)
                    $display("%t LL %m pipelineResp: cRq: own by other cRq, same addr dep: ", $time,
                        fshow(cOwner), " ; ", fshow(cRqEOC)
                    );
                end
                else begin
                    // must be hitting on a line being replaced
                    // add to rep dependency
                    cRqMshr.pipelineResp.setRepSucc(cOwner.mshrIdx, Valid (n));
                    cRqSetDepNoCacheChange;
		   if (verbose)
                    $display("%t LL %m pipelineResp: cRq: own by other cRq, rep dep: ", $time,
                        fshow(cOwner)
                    );
                    doAssert(cOwner.replacing, "line must be replacing");
                end
            end
            else begin
                // owner is myself, so must be swapped in
                LLCRqState cState = pipeOutCState;
                doAssert(cState == Depend, "owner is myself, must be swapped in");
                // tag should match, since always swapped in by cRq (which occupies the line and completes)
                // so cache state must be > I
                doAssert(ram.info.tag == getTag(cRq.addr) && ram.info.cs > I,
                    "cRq swapped in, tag must match, cs > I"
                );
                // since cache state must > I, there is no replacement or req to mem (LLC non-I is hit)
                // just check whether children cache are compatible
                if(cRq.id matches tagged Child ._i) begin
                    // req from child, get dir pend
                    Vector#(childNum, DirPend) dirPend = getDirPendNonCompatForChild;
                    if(dirPend == replicate(Invalid)) begin
		       if (verbose)
                        $display("%t LL %m pipelineResp: cRq from child: own by itself, hit", $time);
                        cRqFromCHit(n, cRq, False);
                    end
                    else begin
		       if (verbose)
                        $display("%t LL %m pipelineResp: cRq from child: own by itself, miss no replace: ", $time,
                            fshow(dirPend)
                        );
                        cRqFromCMissNoReplacement(dirPend);
                    end
                end
                else begin
                    // req from DMA, get dir pend
                    Vector#(childNum, DirPend) dirPend = getDirPendNonCompatForDma;
                    if(dirPend == replicate(Invalid)) begin
		       if (verbose)
                        $display("%t LL %m pipelineResp: cRq from dma: own by itself, hit", $time);
                        cRqFromDmaHit(n, cRq);
                    end
                    else begin
		       if (verbose)
                        $display("%t LL %m pipelineResp: cRq from dma: own by itself, miss by children: ", $time);
                        cRqFromDmaMissByChildren(dirPend);
                    end
                end
            end
        end
        else begin
            // cache has no owner, cRq must just go through tag match
            // here are two cases:
            // 1. cRq in Init state, first time go through tag match
            // 2. cRq in Depend state (due to rep dep, or addr dep on DMA req), waken up when replacement or DMA req is done
            LLCRqState cState = pipeOutCState;

            // only check for cRqEOC to append to dependency chain when firt time go through tag match
            if(cRqEOC matches tagged Valid .m &&& cState == Init) begin
	       if (verbose)
                $display("%t LL %m pipelineResp: cRq: no owner, depend on cRq ", $time,
                    fshow(cState), " ; ", 
                    fshow(cRqEOC)
                );
                cRqMshr.pipelineResp.setAddrSucc(m, Valid (n));
                cRqSetDepNoCacheChange;
            end
            else begin
                // normal processing
                if(cRq.id matches tagged Child ._i) begin
                    // cRq from child
                    if(ram.info.cs == I || ram.info.tag == getTag(cRq.addr)) begin
                        // No Replacement necessary, check dir
                        Vector#(childNum, DirPend) dirPend = getDirPendNonCompatForChild;
                        if(ram.info.cs > I && dirPend == replicate(Invalid)) begin
			   if (verbose)
                            $display("%t LL %m pipelineResp: cRq: no owner, hit", $time);
                            cRqFromCHit(n, cRq, False);
                        end
                        else begin
			   if (verbose)
                            $display("%t LL %m pipelineResp: cRq: no owner, miss no replace: ", $time,
                                fshow(dirPend)
                            );
                            cRqFromCMissNoReplacement(dirPend);
                        end
                    end
                    else begin
                        // need replacement, check dir
                        Vector#(childNum, DirPend) dirPend = getDirPendNonI;
		       if (verbose)
                        $display("%t LL %m pipelineResp: cRq: no owner, replace: ", $time,
                            fshow(dirPend)
                        );
                        cRqFromCReplacement(dirPend);
                    end
                end
                else begin
                    Vector#(childNum, DirPend) dirPend = getDirPendNonCompatForDma;
                    // cRq from DMA
                    if(ram.info.cs > I && ram.info.tag == getTag(cRq.addr)) begin
                        // hit in LLC, check dir
                        if(dirPend == replicate(Invalid)) begin
                            cRqFromDmaHit(n, cRq);
                        end
                        else begin
                            cRqFromDmaMissByChildren(dirPend);
                        end
                    end
                    else begin
                        // miss in LLC, so req mem and req is done!
		       if (verbose)
                        $display("%t LL %m pipelineResp: cRq from dma: no owner, miss req mem", $time);
                        toMInfoQ.enq(ToMemInfo {
                            mshrIdx: n,
                            t: cRq.toState == M ? DmaWr : Ld
                        });
                        // set req to Done & deq pipeline (no change to ram)
                        cRqMshr.pipelineResp.setStateSlot(n, Done, ?);
                        pipeline.deqWrite(Invalid, pipeOut.ram, False);
                        // retry successor (cannot swap in since we don't have a line to occupy)
                        Maybe#(cRqIndexT) addrSucc = pipeOutAddrSucc;
                        if(addrSucc matches tagged Valid .m) begin
                            cRqRetryIndexQ.enq(m);
                        end
                        // since we haven't occupied any line, we cannot have repSucc)
                        Maybe#(cRqIndexT) repSucc = pipeOutRepSucc;
                        doAssert(!isValid(repSucc), "should not have any rep succ");
                    end
                end
            end
        end
    endrule

    // handle mRs
    rule pipelineResp_mRs(pipeOut.cmd == LLMRs);
        // get cache owner
        doAssert(isValid(ram.info.owner), "mRs owner must match some cRq");
        CRqOwner#(cRqIndexT) cOwner = validValue(ram.info.owner);
        // process cRq
        cRqT cRq = pipeOutCRq;
        cRqSlotT cSlot = pipeOutCSlot;
       if (verbose)
        $display("%t LL %m pipelineResp: mRs: ", $time,
            fshow(cOwner), " ; ",
            fshow(cRq), " ; ",
            fshow(cSlot)
        );
        doAssert(isRqFromC(cRq.id), "only child req gets mem resp that refills the cache");
        doAssert(ram.info.cs >= cRq.toState && ram.info.tag == getTag(cRq.addr),
            "mRs must be tag match & have enough cs"
        );
        doAssert(ram.info.dir == replicate(I), "all children must be I");
        doAssert(!cOwner.replacing, "mRs cannot hit on replacing line");
        doAssert(cSlot.way == pipeOut.way, "mRs should hit on way in MSHR slot");
        doAssert(cSlot.waitP, "mRs should match cRq which is waiting for it");
        doAssert(cSlot.dirPend == replicate(Invalid),
            "cRq that needs mRs should not have children to wait for"
        );
        // cRq hits since all children are I
        cRqFromCHit(cOwner.mshrIdx, cRq, True);
    endrule

    // handle cRs
    rule pipelineResp_cRs(pipeOut.cmd matches tagged LLCRs .child);
        // cRs from child
        // XXX CCPipe has already updated ram.info and ram.line properly,
        // particularly for E->M case.
       if (verbose)
        $display("%t LL %m pipelineResp: cRs: ", $time, fshow(child));
        // cs should be not I
        doAssert(ram.info.cs > I, "cRs should hit on a line");
        // check owner of the line
        if(ram.info.owner matches tagged Valid .cOwner) begin
            cRqT cRq = pipeOutCRq;
            cRqSlotT cSlot = pipeOutCSlot;
            LLCRqState cState = pipeOutCState;
	   if (verbose)
            $display("%t LL %m pipelineResp: cRs: match cRq: ", $time,
                fshow(cOwner), " ; ",
                fshow(cRq), " ; ",
                fshow(cSlot), " ; ",
                fshow(cState)
            );
            doAssert(cSlot.way == pipeOut.way, "cRs way should match MSHR slot");
            // check replacing bit
            if(cOwner.replacing) begin
                // replacment: only for req from child
                doAssert(isRqFromC(cRq.id), "only child req do replace");
                doAssert(cState == WaitOldTag, "must be waiting for old tag");
                doAssert(!cSlot.waitP, "cannot wait for parent while replacing");
                doAssert(cSlot.repTag == ram.info.tag, "should match replacing tag");
                // is replacing, update dirPend
                Vector#(childNum, DirPend) newDirPend = cSlot.dirPend;
                if(ram.info.dir[child] == I) begin
                    newDirPend[child] = Invalid;
                end
                // check whether replacement is done
                if(newDirPend == replicate(Invalid)) begin
                    // replacement done, evict line
                    Maybe#(cRqIndexT) repSucc = pipeOutRepSucc;
                    cRqFromCEvict(cOwner.mshrIdx, cRq, repSucc);
		   if (verbose)
                    $display("%t LL %m pipelineResp: cRs: match cRq: replace done: ", $time,
                        fshow(repSucc)
                    );
                end
                else begin
                    // replacement is still ongoing, just deq pipe & write ram & update dirPend
                    pipeline.deqWrite(Invalid, ram, False);
                    cRqMshr.pipelineResp.setStateSlot(cOwner.mshrIdx, WaitOldTag, LLCRqSlot {
                        way: cSlot.way,
                        repTag: cSlot.repTag,
                        waitP: cSlot.waitP,
                        dirPend: newDirPend
                    });
		   if (verbose)
                    $display("%t LL %m pipelineResp: cRs: match cRq: replace not done: ", $time,
                        fshow(newDirPend)
                    );
                end
            end
            else begin
                // child downgrade, not replace: req can be from child or dma
                doAssert(cState == WaitSt, "must be waiting for child/parent state");
                doAssert(ram.info.tag == getTag(cRq.addr), "cRq tag should match cRs hit line");
                doAssert(!cSlot.waitP, "cs > I, so cannot wait for memory");
                // in WaitSt, update dirPend
                Vector#(childNum, DirPend) newDirPend = cSlot.dirPend;
                case(cSlot.dirPend[child]) matches
                    tagged ToSend .st: begin
                        if(st >= ram.info.dir[child]) begin
                            newDirPend[child] = Invalid;
                        end
                    end
                    tagged Waiting .st: begin
                        if(st >= ram.info.dir[child]) begin
                            newDirPend[child] = Invalid;
                        end
                    end
                endcase
	       if (verbose)
                $display("%t LL %m pipelineResp: cRs: match cRq: cRq in WaitSt: ", $time,
                    fshow(newDirPend)
                );
                // check hit or miss
                if(newDirPend == replicate(Invalid)) begin
                    if(cRq.id matches tagged Child ._i) begin
                        cRqFromCHit(cOwner.mshrIdx, cRq, False);
                    end
                    else begin
                        cRqFromDmaHit(cOwner.mshrIdx, cRq);
                    end
                end
                else begin
                    // still wait for children: deq pipe & write ram & update dirPend
                    pipeline.deqWrite(Invalid, ram, False);
                    cRqMshr.pipelineResp.setStateSlot(cOwner.mshrIdx, WaitSt, LLCRqSlot {
                        way: cSlot.way,
                        repTag: cSlot.repTag,
                        waitP: cSlot.waitP,
                        dirPend: newDirPend
                    });
                end
            end
        end
        else begin
            // does not match any cRq, so just deq pipe & write ram
	   if (verbose)
            $display("%t LL %m pipelineResp: cRs: no owner: ", $time);
            pipeline.deqWrite(Invalid, ram, False);
        end
    endrule

    interface ParentCacheToChild to_child;
        interface rqFromC = toFifoEnq(rqFromCQ);
        interface rsFromC = toFifoEnq(rsFromCQ);
        interface toC = toFifoDeq(toCQ);
    endinterface

    interface DmaServer dma;
        interface memReq = toFifoEnq(rqFromDmaQ);
        interface respLd = toFifoDeq(rsLdToDmaQ);
        interface respSt = toFifoDeq(rsStToDmaQ);
`ifdef DEBUG_DMA
        interface wrMissResp = toGet(dmaWrMissQ);
        interface wrHitResp = toGet(dmaWrHitQ);
        interface rdMissResp = toGet(dmaRdMissQ);
        interface rdHitResp = toGet(dmaRdHitQ);
`endif
    endinterface

    interface MemFifoClient to_mem;
        interface toM = toFifoDeq(toMQ);
        interface rsFromM = toFifoEnq(rsFromMQ);
    endinterface

    interface Get cRqStuck;
        method ActionValue#(LLCRqStuck#(childNum, cRqIdT, dmaRqIdT)) get;
            let s <- cRqMshr.stuck.get;
            return LLCRqStuck {
                id: s.req.id,
                addr: s.req.addr,
                fromState: s.req.fromState,
                toState: s.req.toState,
                child: s.req.child,
                state: s.state,
                waitP: s.waitP,
                dirPend: s.dirPend
            };
        endmethod
    endinterface

    method Action setPerfStatus(Bool stats);
`ifdef PERF_COUNT
        doStats <= stats;
`else
        noAction;
`endif
    endmethod 

    method Data getPerfData(LLCPerfType t);
        return (case(t)
`ifdef PERF_COUNT
            LLCDmaMemLdCnt: dmaMemLdCnt;
            LLCDmaMemLdLat: dmaMemLdLat;
            LLCNormalMemLdCnt: normalMemLdCnt;
            LLCNormalMemLdLat: normalMemLdLat;
            LLCMshrBlockCycles: mshrBlocks;
            LLCDownRespCnt: downRespCnt;
            LLCDownRespDataCnt: downRespDataCnt;
            LLCDownReqCnt: downReqCnt;
            LLCUpRespCnt: upRespCnt;
            LLCUpRespDataCnt: upRespDataCnt;
            LLCDmaLdReqCnt: dmaLdReqCnt;
            LLCDmaStReqCnt: dmaStReqCnt;
`endif
            default: 0;
        endcase);
    endmethod
endmodule

// Scheduling notes

// cRqTransfer_retry: read req

// cRqTransfer_new: init(write) req/state/slot, reset addrSucc/repSucc, set data

// cRsTransfer: -

// mRsDeq: write data

// mRsTransfer: read req/slot/data

// sendToM: read req/slot/data

// sendRsToDma: read req/data, release entry (write state)

// sendRsToC: read req/data, release entry (write state)

// sendRqToC:
// -- associative search: read state/needReqChildVec(slot.dirPend)
// -- for selected cRq: read req/state/slot, write slot.dirPend
// -- not affected by cRqTransfer_new
//    (state: Empty->Init, always not hit in search)
// -- not affected by sendRsToC/sendRsToDma
//    (state: Done->Empty, always not hit in search)
// -- may be affected by pipelineResp_cRs
//    so don't fire when pipelineResp operates on the same cRq

// above rules operate on completely different cRq

// pipelineResp_cRq:
// -- associative search for EOC: read state/req/addrSucc
//    -- not affected by cRqTransfer_new
//       (state: Empty->Init, always not hit in search)
//    -- not affected by sendRsToC/sendRsToDma
//       (state: Done->Empty, always not hit in search)
//    -- not affected by sendRqToC (slot is not read in search)
// -- for the processing cRq: read req/state/data/slot/addrSucc/repSucc, write state/slot/data
//    -- this cRq is different from that in cRqTransfer_new
//    -- this cRq is different from that in mRsDeq_refill
//       (if need mRs, then LLC is I, nothing other than mRs can wakeup cRq)
//    -- this cRq is different from that in mRsDeq_nonRefill
//       (when mRs for DMA req comes, it is already in Done state)
//    -- this cRq is different from that in sendRsToC/sendRsToDma
//    -- this cRq is different from that in sendRqToC (slot.dirPend not set yet)
// -- for some older cRq: write addrSucc/repSucc, differnt from cRqTransfer_new

// pipelineResp_mRs:
// -- process cRq hit: read req/slot, write state/slot/data
//    -- this cRq is different from that in cRqTransfer_new
//    -- this cRq is different from that in mRsDeq
//    -- this cRq is different from that in sendRsToC/sendRsToDma
//    -- this cRq is different from that in sendRqToC (child all I)

// pipelineResp_cRs:
// -- process cRs for a cRq: read req/state/slot/data, write state/slot/data
//    -- this cRq is different from that in cRqTransfer_new
//    -- this cRq is different from that in mRsDeq
//    -- this cRq is different from that in sendRsToC/sendRsToDma
//    -- XXX this cRq may be the same as that in sendRqToC!!!

