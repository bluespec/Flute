
// Copyright (c) 2018 Massachusetts Institute of Technology
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
import Assert::*;
import Ehr::*;
import Fifos::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;

// vpn -> ppn of the page containing the intermediate page table
// entry at the lowest walk level

typedef struct {
    PageWalkLevel startLevel;
    Ppn ppn; // only valid when startLevel < maxPageWalkLevel
} TranslationCacheResp deriving(Bits, Eq, FShow);

interface TranslationCache;
    method Action req(Vpn vpn, Asid asid);
    method TranslationCacheResp resp;
    method Action deqResp;
    method Action addEntry(Vpn vpn, PageWalkLevel level, Ppn ppn, Asid asid);
    method Action flush;
    method Bool flush_done;
endinterface

// ordering:
// resp < {req, addEntry}
// req C addEntry
// flush C {req, addEntry} (flush is likely to have higher urgency)

// split translation cache

// translation cache for a specific level L. This stores the PTEs at level L.
interface SingleSplitTransCache;
    method ActionValue#(Maybe#(Ppn)) req(Vpn vpn, Asid asid);
    method Action addEntry(Vpn vpn, Ppn ppn, Asid asid);
    method Action flush;
endinterface

typedef 24 SplitTransCacheSize; // size from ISCA 2010 paper
typedef Bit#(TLog#(SplitTransCacheSize)) SplitTransCacheIdx;

module mkSingleSplitTransCache#(PageWalkLevel level)(SingleSplitTransCache);
    staticAssert(level > 0 && level <= maxPageWalkLevel, "illegal level");

    Vector#(SplitTransCacheSize, Reg#(Bool)) validVec <- replicateM(mkReg(False));
    Vector#(SplitTransCacheSize, Reg#(Vpn)) vpnVec <- replicateM(mkRegU);
    Vector#(SplitTransCacheSize, Reg#(Ppn)) ppnVec <- replicateM(mkRegU);
    Vector#(SplitTransCacheSize, Reg#(Asid)) asidVec <- replicateM(mkRegU);

    // bit-LRU replacement. To reduce cycle time, we update LRU bits in the
    // next cycle after we touch an entry. However, when we have two
    // consecutive cycles inserting new entries, the latter insertion should
    // see the updates to LRU bits made by the former insertion; otherwise the
    // latter will insert to the same slot as the former one.
    Ehr#(2, Bit#(SplitTransCacheSize)) lruBit <- mkEhr(0);
    Reg#(Bit#(SplitTransCacheSize)) lruBit_upd = lruBit[0]; // write
    Reg#(Bit#(SplitTransCacheSize)) lruBit_add = lruBit[1]; // read
    // reg as 1 cycle delay for update LRU
    Ehr#(2, Maybe#(SplitTransCacheIdx)) updRepIdx <- mkEhr(Invalid);
    Reg#(Maybe#(SplitTransCacheIdx)) updRepIdx_deq = updRepIdx[0];
    Reg#(Maybe#(SplitTransCacheIdx)) updRepIdx_enq = updRepIdx[1];
    // randomly choose an LRU idx at replacement time
    Reg#(SplitTransCacheIdx) randIdx <- mkReg(0);

    // don't do anything at flush time
    PulseWire flushEn <- mkPulseWire;

    rule incRandIdx;
        randIdx <= randIdx + 1;
    endrule

    rule doUpdateRep(updRepIdx_deq matches tagged Valid .idx &&& !flushEn);
        updRepIdx_deq <= Invalid;
        // update LRU bits
        Bit#(SplitTransCacheSize) val = lruBit_upd;
        val[idx] = 1;
        if(val == maxBound) begin
            val = 0;
            val[idx] = 1;
        end
        lruBit_upd <= val;
    endrule

    method ActionValue#(Maybe#(Ppn)) req(Vpn vpn, Asid asid) if(updRepIdx_enq == Invalid && !flushEn);
        function Bool hit(SplitTransCacheIdx i);
            Bool vpn_match = getMaskedVpn(vpn, level) == getMaskedVpn(vpnVec[i], level);
            Bool asid_match = asid == asidVec[i]; // TODO global??
            return validVec[i] && vpn_match && asid_match;
        endfunction
        Vector#(SplitTransCacheSize, SplitTransCacheIdx) idxVec = genWith(fromInteger);
        if(find(hit, idxVec) matches tagged Valid .i) begin
            updRepIdx_enq <= Valid (i); // update LRU
            return Valid (ppnVec[i]);
        end
        else begin
            return Invalid;
        end
    endmethod
        
    method Action addEntry(Vpn vpn, Ppn ppn, Asid asid) if(updRepIdx_enq == Invalid && !flushEn);
        // only update LRU if the new entry already exists (TODO Maybe this is
        // not needed after we detect duplicate pagewalk mem req)
        function Bool sameEntry(SplitTransCacheIdx i);
            Bool vpn_match = getMaskedVpn(vpn, level) == getMaskedVpn(vpnVec[i], level);
            Bool asid_match = asid == asidVec[i]; // TODO global??
            return validVec[i] && vpn_match && asid_match;
        endfunction
        Vector#(SplitTransCacheSize, SplitTransCacheIdx) idxVec = genWith(fromInteger);
        if(find(sameEntry, idxVec) matches tagged Valid .idx) begin
            // entry already exists, just update LRU
            updRepIdx_enq <= Valid (idx);
        end
        else begin
            // find slot to add new entry
            SplitTransCacheIdx addIdx;
            if(findIndex( \== (False) , readVReg(validVec) ) matches tagged Valid .idx) begin
                // get empty slot
                addIdx = pack(idx);
            end
            else begin
                // find LRU slot (lruBit[i] = 0 means i is LRU slot)
                Vector#(SplitTransCacheSize, Bool) isLRU = unpack(~lruBit_add);
                if(isLRU[randIdx]) begin
                    addIdx = randIdx;
                end
                else if(findIndex(id, isLRU) matches tagged Valid .idx) begin
                    addIdx = pack(idx);
                end
                else begin
                    addIdx = 0; // this is actually impossible
                    doAssert(False, "must have at least 1 LRU slot");
                end
            end
            // update slot
            validVec[addIdx] <= True;
            vpnVec[addIdx] <= vpn; // we mask vpn at search time
            ppnVec[addIdx] <= ppn; // don't mask ppn, intermidate PTE point to a 4KB page
            asidVec[addIdx] <= asid;
            // update LRU bits
            updRepIdx_enq <= Valid (addIdx);
        end
    endmethod

    method Action flush;
        writeVReg(validVec, replicate(False));
        // also reset LRU bits
        lruBit_upd <= 0;
        updRepIdx_deq <= Invalid;
        // signal fire
        flushEn.send;
    endmethod
endmodule

(* synthesize *)
module mkSplitTransCache(TranslationCache);
    Vector#(TSub#(NumPageWalkLevels, 1), SingleSplitTransCache) caches;
    for(PageWalkLevel i = 0; i < maxPageWalkLevel; i = i+1) begin
        caches[i] <- mkSingleSplitTransCache(i + 1);
    end

    Fifo#(1, TranslationCacheResp) respQ <- mkPipelineFifo;

    method Action req(Vpn vpn, Asid asid);
        TranslationCacheResp resp;
        Vector#(TSub#(NumPageWalkLevels, 1), Maybe#(Ppn)) hits;
        for(PageWalkLevel i = 0; i < maxPageWalkLevel; i = i+1) begin
            hits[i] <- caches[i].req(vpn, asid); // cached page walk level i+1
        end
        // XXX hit in lower level has priority
        if(findIndex(isValid, hits) matches tagged Valid .idx) begin
            resp = TranslationCacheResp {
                startLevel: zeroExtend(pack(idx)),
                ppn: validValue(hits[idx])
            };
        end
        else begin
            resp = TranslationCacheResp {
                startLevel: maxPageWalkLevel,
                ppn: ?
            };
        end
        respQ.enq(resp);
    endmethod

    method TranslationCacheResp resp = respQ.first;

    method Action deqResp;
        respQ.deq;
    endmethod

    // the guard makes resp < addEntry, so pending req is ordered before the
    // newly add entry (so in case of miss, the pending req could mark itself
    // waiting on the page walk that is just about to addEntry, and get the
    // bypass)
    method Action addEntry(Vpn vpn, PageWalkLevel level, Ppn ppn, Asid asid) if(respQ.notFull);
        doAssert(level > 0, "cannot be level 0");
        doAssert(level <= maxPageWalkLevel, "level too large");
        caches[level - 1].addEntry(vpn, ppn, asid);
    endmethod
        
    method Action flush;
        for(PageWalkLevel i = 0; i < maxPageWalkLevel; i = i+1) begin
            caches[i].flush;
        end
    endmethod

    method Bool flush_done = True;
endmodule

(* synthesize *)
module mkNullTransCache(TranslationCache);
    Fifo#(1, void) reqQ <- mkPipelineFifo;

    method Action req(Vpn vpn, Asid asid);
        reqQ.enq(?);
    endmethod
    method Action deqResp;
        reqQ.deq;
    endmethod
    method TranslationCacheResp resp if(reqQ.notEmpty);
        return TranslationCacheResp {
            startLevel: maxPageWalkLevel,
            ppn: ?
        };
    endmethod
    method Action addEntry(Vpn vpn, PageWalkLevel level, Ppn ppn, Asid asid);
        noAction;
    endmethod
    method Action flush;
        noAction;
    endmethod
    method Bool flush_done = True;
endmodule
