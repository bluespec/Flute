
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
import Ehr::*;
import Types::*;
import ProcTypes::*;
import TlbTypes::*;

typedef struct {
    Bool hit;
    indexT index; // hit index, for later update LRU info
    TlbEntry entry; // hit entry
} FullAssocTlbResp#(type indexT) deriving(Bits, Eq, FShow);

// flush preempts addEntry and (translate + updateRepByHit)
// addEntry C (translate + updateRepByHit)
// This avoids bypass from addEntry to translate, and avoids races on LRU bits

// translate CF updateRepByHit.
// updateRepByHit is separted from translate to avoid making translate an
// actionvalue method which may create potential guard lifting problems.

interface FullAssocTlb#(numeric type n);
    method Action flush;
    method FullAssocTlbResp#(Bit#(TLog#(n))) translate(Vpn vpn, Asid asid);
    method Action updateRepByHit(Bit#(TLog#(n)) index); // update replacement info when hit
    method Action addEntry(TlbEntry x);
endinterface

module mkFullAssocTlb#(
    Bool randRep // introduce randomness in bit LRU replacement
)(FullAssocTlb#(tlbSz)) provisos(
    Add#(1, a__, tlbSz),
    Alias#(tlbIdxT, Bit#(TLog#(tlbSz)))
);
    Vector#(tlbSz, Reg#(Bool))     validVec <- replicateM(mkReg(False));
    Vector#(tlbSz, Reg#(TlbEntry)) entryVec <- replicateM(mkRegU);

    // bit-LRU replacement. To reduce cycle time, we update LRU bits in the
    // next cycle after we touch an entry. However, when we have two
    // consecutive cycles inserting new entries, the latter insertion should
    // see the updates to LRU bits made by the former insertion; otherwise the
    // latter will insert to the same slot as the former one.
    Ehr#(2, Bit#(tlbSz)) lruBit <- mkEhr(0);
    Reg#(Bit#(tlbSz)) lruBit_upd = lruBit[0]; // write
    Reg#(Bit#(tlbSz)) lruBit_add = lruBit[1]; // read
    // reg as 1 cycle delay for update LRU
    Ehr#(2, Maybe#(tlbIdxT)) updRepIdx <- mkEhr(Invalid);
    Reg#(Maybe#(tlbIdxT)) updRepIdx_deq = updRepIdx[0];
    Reg#(Maybe#(tlbIdxT)) updRepIdx_enq = updRepIdx[1];
    // randomly choose an LRU idx at replacement time
    Reg#(tlbIdxT) randIdx <- mkReg(0);
    if(randRep) begin
        rule incRandIdx;
            randIdx <= randIdx + 1;
        endrule
    end

    // fire signal for flush
    PulseWire flushEn <- mkPulseWire;

    rule doUpdateRep(!flushEn &&& updRepIdx_deq matches tagged Valid .idx);
        updRepIdx_deq <= Invalid;
        // update LRU bits
        Bit#(tlbSz) val = lruBit_upd;
        val[idx] = 1;
        if(val == maxBound) begin
            val = 0;
            val[idx] = 1;
        end
        lruBit_upd <= val;
    endrule

    // function to match TLB entry
    method Action flush;
        writeVReg(validVec, replicate(False));
        // also reset LRU bits
        lruBit_upd <= 0;
        updRepIdx_deq <= Invalid;
        // record fire signal
        flushEn.send;
    endmethod

    method FullAssocTlbResp#(tlbIdxT) translate(Vpn vpn, Asid asid) if(!flushEn);
        // find the matching entry
        function Bool isMatch(tlbIdxT i);
            TlbEntry entry = entryVec[i];
            Bool asidMatch = entry.asid == asid || entry.pteType.global;
            Bool vpnMatch = getMaskedVpn(vpn, entry.level) == entry.vpn;
            return validVec[i] && asidMatch && vpnMatch;
        endfunction
        Vector#(tlbSz, tlbIdxT) idxVec = genWith(fromInteger);
        if(find(isMatch, idxVec) matches tagged Valid .idx) begin
            // hit a TLB entry, get its content
            return FullAssocTlbResp {
                hit: True,
                index: idx,
                entry: entryVec[idx]
            };
        end
        else begin // miss
            return FullAssocTlbResp {
                hit: False,
                index: ?,
                entry: ?
            };
        end
    endmethod

    method Action updateRepByHit(tlbIdxT index) if(!flushEn && updRepIdx_enq == Invalid);
        updRepIdx_enq <= Valid (index);
    endmethod

    method Action addEntry(TlbEntry x) if(!flushEn && updRepIdx_enq == Invalid);
        // check ppn and vpn lower bits are 0 for super pages
        doAssert(x.ppn == getMaskedPpn(x.ppn, x.level), "ppn lower bits not 0");
        doAssert(x.vpn == getMaskedVpn(x.vpn, x.level), "vpn lower bits not 0");

        // first check if the entry already exists in TLB, this can happen
        // because we may have multiple misses on the same TLB entry. Since
        // lower bits of ppn/vpn of super pages in TLB should have been zeroed,
        // we can directly compare the full addr.
        function Bool sameEntry(tlbIdxT i);
            let en = entryVec[i];
            Bool same_page = en.vpn == x.vpn &&
                             en.level == x.level &&
                             en.asid == x.asid &&
                             en.pteType.global == x.pteType.global;
            return validVec[i] && same_page;
        endfunction
        Vector#(tlbSz, tlbIdxT) idxVec = genWith(fromInteger);
        if(any(sameEntry, idxVec)) begin
            // entry exists, update LRU
            //updRepIdx_enq <= Valid (idx);
            // FIXME: when I try to use find() to find the index, the
            // compilation just explodes...
            noAction;
        end
        else begin
            // find a slot for this translation
            // Since TLB is read-only cache, we can silently evict
            tlbIdxT addIdx;
            if(findIndex( \== (False) , readVReg(validVec) ) matches tagged Valid .idx) begin
                // get empty slot
                addIdx = pack(idx);
            end
            else begin
                // find LRU slot (lruBit[i] = 0 means i is LRU slot)
                Vector#(tlbSz, Bool) isLRU = unpack(~lruBit_add);
                if(randRep && isLRU[randIdx]) begin
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
            entryVec[addIdx] <= x;
            // update LRU bits
            updRepIdx_enq <= Valid (addIdx);
        end
    endmethod
endmodule
