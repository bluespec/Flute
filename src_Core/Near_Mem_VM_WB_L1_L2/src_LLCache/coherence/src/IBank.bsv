
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

`include "ProcConfig.bsv"

import Types::*;
import MemoryTypes::*;
import Amo::*;

import Cntrs::*;
import Vector::*;
import ConfigReg::*;
import FIFO::*;
import GetPut::*;
import ClientServer::*;
import CCTypes::*;
import ICRqMshr::*;
import IPRqMshr::*;
import CCPipe::*;
import L1Pipe ::*;
import FShow::*;
import DefaultValue::*;
import Fifos::*;
import CacheUtils::*;
import Performance::*;
import LatencyTimer::*;
import RandomReplace::*;

export ICRqStuck(..);
export IPRqStuck(..);
export IBank(..);
export mkIBank;

// L1 I$

// although pRq never appears in dependency chain
// we still need pRq MSHR to limit the number of pRq
// and thus limit the size of rsToPIndexQ

typedef struct {
    Addr addr;
    ICRqState state;
    Bool waitP;
} ICRqStuck deriving(Bits, Eq, FShow);

typedef IPRqMshrStuck IPRqStuck;

interface IBank#(
    numeric type supSz, // superscalar size
    numeric type lgBankNum,
    numeric type wayNum,
    numeric type indexSz,
    numeric type tagSz,
    numeric type cRqNum,
    numeric type pRqNum
);
    interface ChildCacheToParent#(Bit#(TLog#(wayNum)), void) to_parent;
    interface InstServer#(supSz) to_proc; // to child, i.e. processor
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(ICRqStuck) cRqStuck;
    interface Get#(IPRqStuck) pRqStuck;
    // security: flush
    method Action flush;
    method Bool flush_done;
    // performance
    method Action setPerfStatus(Bool stats);
    method Data getPerfData(L1IPerfType t);
endinterface

module mkIBank#(
    Bit#(lgBankNum) bankId,
    module#(ICRqMshr#(cRqNum, wayT, tagT, procRqT, resultT)) mkICRqMshrLocal,
    module#(IPRqMshr#(pRqNum)) mkIPRqMshrLocal,
    module#(L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT)) mkL1Pipeline
)(
    IBank#(supSz, lgBankNum, wayNum, indexSz, tagSz, cRqNum, pRqNum)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(indexSz)),
    Alias#(tagT, Bit#(tagSz)),
    Alias#(cRqIdxT, Bit#(TLog#(cRqNum))),
    Alias#(pRqIdxT, Bit#(TLog#(pRqNum))),
    Alias#(cacheOwnerT, Maybe#(cRqIdxT)), // owner cannot be pRq
    Alias#(cacheInfoT, CacheInfo#(tagT, Msi, void, cacheOwnerT, void)),
    Alias#(ramDataT, RamData#(tagT, Msi, void, cacheOwnerT, void, Line)),
    Alias#(procRqT, ProcRqToI),
    Alias#(cRqToPT, CRqMsg#(wayT, void)),
    Alias#(cRsToPT, CRsMsg#(void)),
    Alias#(pRqFromPT, PRqMsg#(void)),
    Alias#(pRsFromPT, PRsMsg#(wayT, void)),
    Alias#(pRqRsFromPT, PRqRsMsg#(wayT, void)),
    Alias#(cRqSlotT, ICRqSlot#(wayT, tagT)), // cRq MSHR slot
    Alias#(l1CmdT, L1Cmd#(indexT, cRqIdxT, pRqIdxT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, Msi, void, cacheOwnerT, void, RandRepInfo, Line, l1CmdT)),
    Alias#(resultT, Vector#(supSz, Maybe#(Instruction))),
    // requirements
    FShow#(pipeOutT),
    Add#(tagSz, a__, AddrSz),
    // make sure: cRqNum <= wayNum
    Add#(cRqNum, b__, wayNum),
    Add#(TAdd#(tagSz, indexSz), TAdd#(lgBankNum, LgLineSzBytes), AddrSz)
);

       Bool verbose = False;

    ICRqMshr#(cRqNum, wayT, tagT, procRqT, resultT) cRqMshr <- mkICRqMshrLocal;

    IPRqMshr#(pRqNum) pRqMshr <- mkIPRqMshrLocal;

    L1Pipe#(lgBankNum, wayNum, indexT, tagT, cRqIdxT, pRqIdxT) pipeline <- mkL1Pipeline;

    Fifo#(1, Addr) rqFromCQ <- mkBypassFifo;

    Fifo#(2, cRsToPT) rsToPQ <- mkCFFifo;
    Fifo#(2, cRqToPT) rqToPQ <- mkCFFifo;
    Fifo#(2, pRqRsFromPT) fromPQ <- mkCFFifo;

    FIFO#(MshrIndex#(cRqIdxT, pRqIdxT)) rsToPIndexQ <- mkSizedFIFO(valueOf(TAdd#(cRqNum, pRqNum)));

    FIFO#(cRqIdxT) rqToPIndexQ <- mkSizedFIFO(valueOf(cRqNum));
    // temp fifo for pipelineResp & sendRsToP (reduce conflict)
    FIFO#(cRqIdxT) rqToPIndexQ_pipelineResp <- mkFIFO;
    FIFO#(cRqIdxT) rqToPIndexQ_sendRsToP <- mkFIFO;

    // index Q to order all in flight cRq for in-order resp
    FIFO#(cRqIdxT) cRqIndexQ <- mkSizedFIFO(valueof(cRqNum));
   
`ifdef DEBUG_ICACHE
    // id for each cRq, incremented when each new req comes
    Reg#(Bit#(64)) cRqId <- mkReg(0);
    // FIFO to signal the id of cRq that is performed
    // FIFO has 0 cycle latency to match L1 D$ resp latency
    Fifo#(1, DebugICacheResp) cRqDoneQ <- mkBypassFifo; 
`endif

    // security flush
`ifdef SECURITY
    Reg#(Bool) flushDone <- mkReg(True);
    Reg#(Bool) flushReqStart <- mkReg(False);
    Reg#(Bool) flushReqDone <- mkReg(False);
    Reg#(Bool) flushRespDone <- mkReg(False);
    Reg#(indexT) flushIndex <- mkReg(0);
    Reg#(wayT) flushWay <- mkReg(0);
`else
    Bool flushDone = True;
`endif

`ifdef PERF_COUNT
    Reg#(Bool) doStats <- mkConfigReg(False);
    Count#(Data) ldCnt <- mkCount(0);
    Count#(Data) ldMissCnt <- mkCount(0);
    Count#(Data) ldMissLat <- mkCount(0);

    LatencyTimer#(cRqNum, 10) latTimer <- mkLatencyTimer;

    function Action incrReqCnt;
    action
        if(doStats) begin
            ldCnt.incr(1);
        end
    endaction
    endfunction

    function Action incrMissCnt(cRqIdxT idx);
    action
        let lat <- latTimer.done(idx);
        if(doStats) begin
            ldMissLat.incr(zeroExtend(lat));
            ldMissCnt.incr(1);
        end
    endaction
    endfunction
`endif

    function tagT getTag(Addr a) = truncateLSB(a);

    // XXX since I$ may be requested by processor constantly
    // cRq may come at every cycle, so we must make cRq has lower priority than pRq/pRs
    // otherwise the whole system may deadlock/livelock
    // we stop accepting cRq when we need to flush for security
    rule cRqTransfer(flushDone);
        Addr addr <- toGet(rqFromCQ).get;
`ifdef DEBUG_ICACHE
        procRqT r = ProcRqToI {addr: addr, id: cRqId};
        cRqId <= cRqId + 1;
`else
        procRqT r = ProcRqToI {addr: addr};
`endif
        cRqIdxT n <- cRqMshr.getEmptyEntryInit(r);
        // send to pipeline
        pipeline.send(CRq (L1PipeRqIn {
            addr: r.addr,
            mshrIdx: n
        }));
        // enq to indexQ for in order resp
        cRqIndexQ.enq(n);
`ifdef PERF_COUNT
        // performance counter: cRq type
        incrReqCnt;
`endif
       if (verbose)
        $display("%t I %m cRqTransfer: ", $time,
            fshow(n), " ; ",
            fshow(r)
        );
    endrule

    // this descending urgency is necessary to avoid deadlock/livelock
    (* descending_urgency = "pRqTransfer, cRqTransfer" *)
    rule pRqTransfer(fromPQ.first matches tagged PRq .req);
        fromPQ.deq;
        pRqIdxT n <- pRqMshr.getEmptyEntryInit(req);
        // send to pipeline
        pipeline.send(PRq (L1PipeRqIn {
            addr: req.addr,
            mshrIdx: n
        }));
       if (verbose)
        $display("%t I %m pRqTransfer: ", $time, 
            fshow(n), " ; ", 
            fshow(req)
        );
    endrule

    // this descending urgency is necessary to avoid deadlock/livelock
    (* descending_urgency = "pRsTransfer, cRqTransfer" *)
    rule pRsTransfer(fromPQ.first matches tagged PRs .resp);
        fromPQ.deq;
        pipeline.send(PRs (L1PipePRsIn {
            addr: resp.addr,
            toState: S,
            data: resp.data,
            way: resp.id
        }));
       if (verbose)
        $display("%t I %m pRsTransfer: ", $time, fshow(resp));
        doAssert(resp.toState == S && isValid(resp.data), "I$ must upgrade to S with data");
    endrule
    
`ifdef SECURITY
    // start flush when cRq MSHR is empty
    rule startFlushReq(!flushDone && !flushReqStart && cRqMshr.emptyForFlush);
        flushReqStart <= True;
    endrule

    (* descending_urgency = "pRsTransfer, flushTransfer" *)
    (* descending_urgency = "pRqTransfer, flushTransfer" *)
    rule flushTransfer(!flushDone && flushReqStart && !flushReqDone);
        // We allocate a pRq MSHR entry for 2 reasons:
        // (1) reuse the pRq logic to send resp to parent
        // (2) control the number of downgrade resp to avoid stalling the cache
        // pipeline
        pRqIdxT n <- pRqMshr.getEmptyEntryInit(PRqMsg {
            addr: ?,
            toState: I,
            child: ?
        });
        pipeline.send(Flush (L1PipeFlushIn {
            index: flushIndex,
            way: flushWay,
            mshrIdx: n
        }));
        // increment flush index/way
        if (flushWay < fromInteger(valueof(wayNum) - 1)) begin
            flushWay <= flushWay + 1;
        end
        else begin
            flushWay <= 0;
            flushIndex <= flushIndex + 1; // index num should be power of 2
            if (flushIndex == maxBound) begin
                flushReqDone <= True;
            end
        end
       if (verbose)
        $display("%t I %m flushTransfer: ", $time, fshow(n), " ; ",
                 fshow(flushIndex), " ; ", fshow(flushWay));
    endrule
`endif

    rule sendRsToP_cRq(rsToPIndexQ.first matches tagged CRq .n);
        rsToPIndexQ.deq;
        // get cRq replacement info
        procRqT req = cRqMshr.sendRsToP_cRq.getRq(n);
        cRqSlotT slot = cRqMshr.sendRsToP_cRq.getSlot(n);
        // send resp to parent
        cRsToPT resp = CRsMsg {
            addr: {slot.repTag, truncate(req.addr)}, // get bank id & index from req
            toState: I,
            data: Invalid, // I$ never downgrade with data to writeback
            child: ?
        };
        rsToPQ.enq(resp);
        // req parent for upgrade now
        // (prevent parent resp from coming to release MSHR entry before replace resp is sent)
        rqToPIndexQ_sendRsToP.enq(n);
       if (verbose)
        $display("%t I %m sendRsToP: ", $time, 
            fshow(rsToPIndexQ.first)," ; ", 
            fshow(req), " ; ",
            fshow(slot), " ; ",
            fshow(resp)
        );
    endrule

    rule sendRsToP_pRq(rsToPIndexQ.first matches tagged PRq .n);
        rsToPIndexQ.deq;
        // get pRq info & send resp & release MSHR entry
        pRqFromPT req = pRqMshr.sendRsToP_pRq.getRq(n);
        cRsToPT resp = CRsMsg {
            addr: req.addr,
            toState: I, // I$ must downgrade to I
            data: Invalid, // I$ never downgrade with data to writeback
            child: ?
        };
        rsToPQ.enq(resp);
        pRqMshr.sendRsToP_pRq.releaseEntry(n); // mshr entry released
       if (verbose)
        $display("%t I %m sendRsToP: ", $time, 
            fshow(rsToPIndexQ.first), " ; ", 
            fshow(req), " ; ", 
            fshow(resp)
        );
        doAssert(req.toState == I, "I$ only has downgrade req to I");
    endrule

    rule sendRqToP;
        rqToPIndexQ.deq;
        cRqIdxT n = rqToPIndexQ.first;
        procRqT req = cRqMshr.sendRqToP.getRq(n);
        cRqSlotT slot = cRqMshr.sendRqToP.getSlot(n);
        cRqToPT cRqToP = CRqMsg {
            addr: req.addr,
            fromState: I, // I$ upgrade from I
            toState: S, // I$ upgrade to S
            canUpToE: False,
            id: slot.way,
            child: ?
        };
        rqToPQ.enq(cRqToP);
       if (verbose)
        $display("%t I %m sendRqToP: ", $time, 
            fshow(n), " ; ", 
            fshow(req), " ; ", 
            fshow(slot), " ; ", 
            fshow(cRqToP)
        );
`ifdef PERF_COUNT
        // performance counter: start miss timer
        latTimer.start(n);
`endif
    endrule

    // last stage of pipeline: process req

    // XXX: in L1, pRq cannot exist in dependency chain
    // because there are only two ways to include pRq into chain
    // (1) append to a cRq that could finish, but such cRq must have been directly reponded
    // (2) overtake cRq (S->M), but such downgrade can be done instaneously without the need of chaining
    //     (this cannot happen in I$)
    // Thus, dependency chain in L1 only contains cRq

    // pipeline outputs
    pipeOutT pipeOut = pipeline.first;
    ramDataT ram = pipeOut.ram;
    // get proc req to select from cRqMshr
    procRqT pipeOutCRq = cRqMshr.pipelineResp.getRq(
        case(pipeOut.cmd) matches
            tagged L1CRq .n: (n);
            default: (fromMaybe(0, ram.info.owner)); // L1PRs
        endcase
    );

    // function to get superscaler inst read result
    function resultT readInst(Line line, Addr addr);
        Vector#(LineSzInst, Instruction) instVec = unpack(pack(line));
        // the start offset for reading inst
        LineInstOffset startSel = getLineInstOffset(addr);
        // calculate the maximum inst count that could be read from line
        LineInstOffset maxCntMinusOne = maxBound - startSel;
        // read inst superscalaer
        resultT val = ?;
        for(Integer i = 0; i < valueof(supSz); i = i+1) begin
            if(fromInteger(i) <= maxCntMinusOne) begin
                LineInstOffset sel = startSel + fromInteger(i);
                val[i] = Valid (instVec[sel]);
            end
            else begin
                val[i] = Invalid;
            end
        end
        return val;
    endfunction

    // function to process cRq hit (MSHR slot may have garbage)
    function Action cRqHit(cRqIdxT n, procRqT req);
    action
       if (verbose)
        $display("%t I %m pipelineResp: Hit func: ", $time,
            fshow(n), " ; ",
            fshow(req)
        );
        // check tag & cs: even this function is called by pRs, tag should match,
        // because tag is written into cache before sending req to parent
        doAssert(ram.info.tag == getTag(req.addr) && ram.info.cs == S,
            "cRqHit but tag or cs incorrect"
        );
        // deq pipeline or swap in successor
        Maybe#(cRqIdxT) succ = cRqMshr.pipelineResp.getSucc(n);
        pipeline.deqWrite(succ, RamData {
            info: CacheInfo {
                tag: getTag(req.addr), // should be the same as original tag
                cs: ram.info.cs, // use cs in ram
                dir: ?,
                owner: succ,
                other: ?
            },
            line: ram.line
        }, True); // hit, so update rep info
        // process req to get superscalar inst read results
        // set MSHR entry as Done & save inst results
        let instResult = readInst(ram.line, req.addr);
        cRqMshr.pipelineResp.setResult(n, instResult);
        cRqMshr.pipelineResp.setStateSlot(n, Done, ?);
       if (verbose)
        $display("%t I %m pipelineResp: Hit func: update ram: ", $time,
            fshow(succ), " ; ", fshow(instResult)
        );
`ifdef DEBUG_ICACHE
        // signal that this req is performed
        cRqDoneQ.enq(DebugICacheResp {
            id: req.id,
            line: ram.line
        });
`endif
    endaction
    endfunction

    rule pipelineResp_cRq(pipeOut.cmd matches tagged L1CRq .n);
       if (verbose)
        $display("%t I %m pipelineResp: ", $time, fshow(pipeOut));

        procRqT procRq = pipeOutCRq;
       if (verbose)
        $display("%t I %m pipelineResp: cRq: ", $time, fshow(n), " ; ", fshow(procRq));
        
        // find end of dependency chain
        Maybe#(cRqIdxT) cRqEOC = cRqMshr.pipelineResp.searchEndOfChain(procRq.addr);

        // function to process cRq miss without replacement (MSHR slot may have garbage)
        function Action cRqMissNoReplacement;
        action
            cRqSlotT cSlot = cRqMshr.pipelineResp.getSlot(n);
            // it is impossible in L1 to have slot.waitP == True in this function
            // because cRq is not set to Depend when pRq invalidates it (pRq just directly resp)
            // and this func is only called when cs < S (otherwise will hit)
            // because L1 has no children to wait for
            doAssert(!cSlot.waitP && ram.info.cs == I, "waitP must be false and cs must be I");
            // Thus we must send req to parent 
            // XXX first send to a temp indexQ to avoid conflict, then merge to rqToPIndexQ later
            rqToPIndexQ_pipelineResp.enq(n);
            // update mshr
            cRqMshr.pipelineResp.setStateSlot(n, WaitSt, ICRqSlot {
                way: pipeOut.way, // use way from pipeline
                repTag: ?, // no replacement
                waitP: True // must fetch from parent
            });
            // deq pipeline & set owner, tag
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(procRq.addr), // tag may be garbage if cs == I
                    cs: ram.info.cs,
                    dir: ?,
                    owner: Valid (n), // owner is req itself
                    other: ?
                },
                line: ram.line
            }, False);
        endaction
        endfunction

        // function to do replacement for cRq
        function Action cRqReplacement;
        action
            // deq pipeline
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: getTag(procRq.addr), // set to req tag (old tag is replaced right now)
                    cs: I,
                    dir: ?,
                    owner: Valid (n), // owner is req itself
                    other: ?
                },
                line: ? // data is no longer used
            }, False);
            doAssert(ram.info.cs == S, "I$ replacement only replace S line");
            // update MSHR to save replaced tag
            // although we send req to parent later (when resp to parent is sent)
            // we set state to WaitSt now, since the req to parent is already on schedule
            cRqMshr.pipelineResp.setStateSlot(n, WaitSt, ICRqSlot {
                way: pipeOut.way, // use way from pipeline
                repTag: ram.info.tag, // tag being replaced for sending rs to parent
                waitP: True
            });
            // send replacement resp to parent
            rsToPIndexQ.enq(CRq (n));
        endaction
        endfunction
    
        // function to set cRq to Depend, and make no further change to cache
        function Action cRqSetDepNoCacheChange;
        action
            cRqMshr.pipelineResp.setStateSlot(n, Depend, defaultValue);
            pipeline.deqWrite(Invalid, pipeOut.ram, False);
        endaction
        endfunction

        if(ram.info.owner matches tagged Valid .cOwner) begin
            if(cOwner != n) begin
                // owner is another cRq, so must just go through tag match
                // tag match must be hit (because replacement algo won't give a way with owner)
                doAssert(ram.info.cs == S && ram.info.tag == getTag(procRq.addr),
                    "cRq should hit in tag match"
                );
                // should be added to a cRq in dependency chain & deq from pipeline
                doAssert(isValid(cRqEOC), "cRq hit on another cRq, cRqEOC must be true");
                cRqMshr.pipelineResp.setSucc(fromMaybe(?, cRqEOC), Valid (n));
                cRqSetDepNoCacheChange;
	       if (verbose)
                $display("%t I %m pipelineResp: cRq: own by other cRq ", $time,
                    fshow(cOwner), ", depend on cRq ", fshow(cRqEOC)
                );
            end
            else begin
                // owner is myself, so must be swapped in
                // tag should match, since always swapped in by cRq, cs = S
                doAssert(ram.info.tag == getTag(procRq.addr) && ram.info.cs == S,
                    "cRq swapped in by previous cRq, tag must match & cs = S"
                );
                // Hit
	       if (verbose)
                $display("%t I %m pipelineResp: cRq: own by itself, hit", $time);
                cRqHit(n, procRq);
            end
        end
        else begin
            // cache has no owner, cRq must just go through tag match
            // check for cRqEOC to append to dependency chain
            if(cRqEOC matches tagged Valid .k) begin
	       if (verbose)
                $display("%t I %m pipelineResp: cRq: no owner, depend on cRq ", $time, fshow(k));
                cRqMshr.pipelineResp.setSucc(k, Valid (n));
                cRqSetDepNoCacheChange;
            end
            else if(ram.info.cs == I || ram.info.tag == getTag(procRq.addr)) begin
                // No Replacement necessary
                if(ram.info.cs > I) begin
		   if (verbose)
                    $display("%t I %m pipelineResp: cRq: no owner, hit", $time);
                    cRqHit(n, procRq);
                end
                else begin
		   if (verbose)
                    $display("%t I %m pipelineResp: cRq: no owner, miss no replace", $time);
                    cRqMissNoReplacement;
                end
            end
            else begin
	       if (verbose)
                $display("%t I %m pipelineResp: cRq: no owner, replace", $time);
                cRqReplacement;
            end
        end
    endrule

    rule pipelineResp_pRs(pipeOut.cmd == L1PRs);
       if (verbose) begin
        $display("%t I %m pipelineResp: ", $time, fshow(pipeOut));
        $display("%t I %m pipelineResp: pRs: ", $time);
       end

        if(ram.info.owner matches tagged Valid .cOwner) begin
            procRqT procRq = pipeOutCRq;
            doAssert(ram.info.cs == S && ram.info.tag == getTag(procRq.addr),
                "pRs must be a hit"
            );
            cRqHit(cOwner, procRq);
`ifdef PERF_COUNT
            // performance counter: miss cRq
            incrMissCnt(cOwner);
`endif
        end
        else begin
            doAssert(False, ("pRs owner must match some cRq"));
        end
    endrule

    rule pipelineResp_pRq(pipeOut.cmd matches tagged L1PRq .n);
        pRqFromPT pRq = pRqMshr.pipelineResp.getRq(n);
       if (verbose)
        $display("%t I %m pipelineResp: pRq: ", $time, fshow(n), " ; ", fshow(pRq));

        doAssert(pRq.toState == I, "I$ pRq only downgrade to I");

        // pRq is never in dependency chain, so it is never swapped in
        // pRq must go through tag match, which either returns a tag matched way or asserts pRqMiss
        // In I$ a tag matched way should always be processed since pRq always downgrades to I
        // pRq is always directly handled: either dropped or Done

        if(pipeOut.pRqMiss) begin
	   if (verbose)
            $display("%t I %m pipelineResp: pRq: drop", $time);
            // pRq can be directly dropped, no successor (since just go through pipeline)
            pRqMshr.pipelineResp.releaseEntry(n);
            pipeline.deqWrite(Invalid, pipeOut.ram, False);
        end
        else begin
	   if (verbose)
            $display("%t I %m pipelineResp: pRq: valid process", $time);
            // should process pRq
            doAssert(ram.info.cs == S && pRq.toState == I && ram.info.tag == getTag(pRq.addr),
                "pRq should be processed"
            );
            // line cannot be owned, because
            // (1) pRq never own cache line
            // (2) if owned by cRq, cRq would have hit and released ownership
            doAssert(ram.info.owner == Invalid, "pRq cannot hit on line owned by anyone");
            // write ram: set cs to I
            pipeline.deqWrite(Invalid, RamData {
                info: CacheInfo {
                    tag: ram.info.tag,
                    cs: I, // I$ is always downgraded by pRq to I
                    dir: ?,
                    owner: Invalid, // no successor
                    other: ?
                },
                line: ? // line is not useful
            }, False);
            // pRq is done
            pRqMshr.pipelineResp.setDone(n);
            // send resp to parent
            rsToPIndexQ.enq(PRq (n));
        end
    endrule

`ifdef SECURITY
    rule pipelineResp_flush(
        !flushDone &&& !flushRespDone &&&
        pipeOut.cmd matches tagged L1Flush .flush
    );
        pRqIdxT n = flush.mshrIdx;
       if (verbose)
        $display("%t I %m pipelineResp: flush: ", $time, fshow(flush));

        // During flush, cRq MSHR is empty, so cache line cannot have owner
        doAssert(ram.info.owner == Invalid, "flushing line cannot have owner");

        // flush always goes through cache pipeline, and is directly handled
        // here: either dropped or Done
        if(ram.info.cs == I) begin
	   if (verbose)
            $display("%t I %m pipelineResp: flush: drop", $time);
            // flush can be directly dropped
            pRqMshr.pipelineResp.releaseEntry(n);
        end
        else begin
	   if (verbose)
            $display("%t I %m pipelineResp: flush: valid process", $time);
            pRqMshr.pipelineResp.setDone(n);
            rsToPIndexQ.enq(PRq (n));
            // record the flushed addr in MSHR so that sendRsToP rule knows
            // which addr is invalidated
            Bit#(LgLineSzBytes) offset = 0;
            Addr addr = {ram.info.tag, flush.index, bankId, offset};
            pRqMshr.pipelineResp.setFlushAddr(n, addr);
        end

        // always clear the cache line
        pipeline.deqWrite(Invalid, RamData {
            info: CacheInfo {
                tag: ?,
                cs: I, // downgraded to I
                dir: ?,
                owner: Invalid, // no successor
                other: ?
            },
            line: ?
        }, False);

        // check if we have finished all flush
        if (flush.index == maxBound &&
            pipeOut.way == fromInteger(valueof(wayNum) - 1)) begin
            flushRespDone <= True;
        end
    endrule

    rule completeFlush(!flushDone && flushReqStart && flushReqDone && flushRespDone);
        flushDone <= True;
        flushReqStart <= False;
        flushReqDone <= False;
        flushRespDone <= False;
    endrule
`endif

    // merge rq to parent index into indexQ
    rule rqIndexFromPipelineResp;
        let n <- toGet(rqToPIndexQ_pipelineResp).get;
        rqToPIndexQ.enq(n);
    endrule

    (* descending_urgency = "rqIndexFromPipelineResp, rqIndexFromSendRsToP" *)
    rule rqIndexFromSendRsToP;
        let n <- toGet(rqToPIndexQ_sendRsToP).get;
        rqToPIndexQ.enq(n);
    endrule

    interface ChildCacheToParent to_parent;
        interface rsToP = toFifoDeq(rsToPQ);
        interface rqToP = toFifoDeq(rqToPQ);
        interface fromP = toFifoEnq(fromPQ);
    endinterface

    interface InstServer to_proc;
        interface Put req;
            method Action put(Addr addr);
                rqFromCQ.enq(addr);
            endmethod
        endinterface
        interface Get resp;
            method ActionValue#(resultT) get if(
                cRqMshr.sendRsToC.getResult(cRqIndexQ.first) matches tagged Valid .inst
            );
                cRqIndexQ.deq;
                cRqMshr.sendRsToC.releaseEntry(cRqIndexQ.first); // release MSHR entry
	       if (verbose)
                $display("%t I %m sendRsToC: ", $time,
                    fshow(cRqIndexQ.first), " ; ",
                    fshow(inst)
                );
                return inst;
            endmethod
        endinterface
`ifdef DEBUG_ICACHE
        interface done = toGet(cRqDoneQ);
`endif
    endinterface

    interface Get cRqStuck;
        method ActionValue#(ICRqStuck) get;
            let s <- cRqMshr.stuck.get;
            return ICRqStuck {
                addr: s.req.addr,
                state: s.state,
                waitP: s.waitP
            };
        endmethod
    endinterface
                
    interface pRqStuck = pRqMshr.stuck;

`ifdef SECURITY
    method Action flush if(flushDone);
        flushDone <= False;
    endmethod
    method flush_done = flushDone._read;
`else
    method flush = noAction;
    method flush_done = True;
`endif

    method Action setPerfStatus(Bool stats);
`ifdef PERF_COUNT
        doStats <= stats;
`else
        noAction;
`endif
    endmethod

    method Data getPerfData(L1IPerfType t);
        return (case(t)
`ifdef PERF_COUNT
            L1ILdCnt: ldCnt;
            L1ILdMissCnt: ldMissCnt;
            L1ILdMissLat: ldMissLat;
`endif
            default: 0;
        endcase);
    endmethod
endmodule


// Scheduling note

// cRqTransfer (toC.req.put): write new cRq MSHR entry, cRqMshr.getEmptyEntry

// pRqTransfer: write new pRq MSHR entry, pRqMshr.getEmptyEntry

// pRsTransfer: -

// sendRsToC (toC.resp.get): read cRq MSHR result, releaseEntry

// sendRsToP_cRq: read cRq MSHR req/slot that is replacing

// sendRsToP_pRq: read pRq MSHR entry that is responding, pRqMshr.releaseEntry

// sendRqToP: read cRq MSHR req/slot that is requesting parent

// pipelineResp_cRq:
// -- read cRq MSHR req/state/slot currently processed
// -- write cRq MSHR state/slot/result currently processed
// -- write succ of some existing cRq MSHR entry (in WaitNewTag or WaitSt)
// -- read all state/req/succ in cRq MSHR entry (searchEOC)
//    -- not affected by write in cRqTransfer (state change is Empty->Init)
//    -- not affected by write in sendRsC (state change is Done->Empty)

// pipelineResp_pRs:
// -- read cRq MSHR req/succ, write cRq MSHR state/slot/result

// pipelineResp_pRq:
// -- r/w pRq MSHR entry, pRqMshr.releaseEntry

// ---- conflict analysis ----

// XXXTransfer is conflict with each other
// Impl of getEmptyEntry and releaseEntry ensures that they are not on the same entry (e.g. cRqTransfer v.s. sendRsToC)
// XXXTransfer should operate on different cRq/pRq from other rules

// sendRsToC is ordered after pipelineResp to save 1 cycle in I$ latency

// sendRqToP and sendRsToP_cRq are read only

// sendRsToP_pRq is operating on different pRq from pipelineResp_pRq (since we use CF index FIFO)

// ---- conclusion ----

// rules/methods are operating on different MSHR entries, except pipelineResp v.s. sendRsToC

// we have 5 ports from cRq MSHR
// 1. cRqTransfer
// 2. sendRsToC
// 3. sendRsToP_cRq
// 4. sendRqToP
// 5. pipelineResp

// we have 3 ports from pRq MSHR
// 1. pRqTransfer
// 2. sendRsToP_pRq
// 3. pipelineResp

// safe version: use EHR ports
// sendRsToP_cRq/sendRqToP/pipelineResp < sendRsToC < cRqTransfer
// pipelineResp < sendRsToP_pRq < pRqTransfer
// (note there is no bypass path from pipelineResp to sendRsToP_pRq since sendRsToP_pRq only reads pRq)

// unsafe version: all reads read the original reg value, except sendRsToC, which should bypass from pipelineResp
// all writes are cononicalized. NOTE: writes of sendRsToC should be after pipelineResp
// we maintain the logical ordering in safe version

