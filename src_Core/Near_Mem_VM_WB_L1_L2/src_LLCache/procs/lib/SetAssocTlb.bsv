
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
import RWBramCore::*;

// set assoc tlb only for 4KB pages
// indexed by VPN

typedef struct {
    Bool hit;
    wayT way; // hit way
    TlbEntry entry; // hit entry
} SetAssocTlbResp#(type wayT) deriving(Bits, Eq, FShow);

typedef struct {
    Vpn vpn;
    Asid asid;
} SetAssocTlbTranslateReq deriving(Bits, Eq, FShow);

typedef union tagged {
    SetAssocTlbTranslateReq Translate;
    TlbEntry Refill;
} SetAssocTlbReq deriving(Bits, Eq, FShow);

interface SetAssocTlb#(
    numeric type wayNum,
    numeric type lgSetNum,
    type repInfoT // info for replacement, e.g. LRU
);
    method Action flush;
    method Bool flush_done;

    method Action req(SetAssocTlbReq r);
    // resp is only for translate req
    method SetAssocTlbResp#(Bit#(TLog#(wayNum))) resp;
    // deq resp from pipeline and may update the hit way to MRU
    method Action deqResp(Maybe#(Bit#(TLog#(wayNum))) hitWay);
endinterface

typedef enum {Flush, Ready} SetAssocTlbState deriving(Bits, Eq, FShow);

typedef struct {
    Bool valid;
    TlbEntry entry;
} SetAssocTlbEntry deriving(Bits, Eq, FShow);

module mkSetAssocTlb#(
    repInfoT repInfoInitVal,
    function wayT getRepWay(repInfoT info, Vector#(wayNum, Bool) invalid),
    function repInfoT updateRepInfo(repInfoT info, wayT way)
)(
    SetAssocTlb#(wayNum, lgSetNum, repInfoT)
) provisos(
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(lgSetNum)),
    Alias#(respT, SetAssocTlbResp#(wayT)),
    Bits#(repInfoT, a__),
    Add#(lgSetNum, b__, VpnSz)
);
    // ram for tlb entry
    Vector#(wayNum, RWBramCore#(indexT, SetAssocTlbEntry)) tlbRam <- replicateM(mkRWBramCore);
    // ram for replace info
    RWBramCore#(indexT, repInfoT) repRam <- mkRWBramCore;
    // pending
    Ehr#(2, Maybe#(SetAssocTlbReq)) pendReq <- mkEhr(Invalid);
    Reg#(Maybe#(SetAssocTlbReq)) pendReq_deq = pendReq[0];
    Reg#(Maybe#(SetAssocTlbReq)) pendReq_enq = pendReq[1];
    // init & flush index
    Reg#(indexT) flushIdx <- mkReg(0);
    // overall state
    Reg#(SetAssocTlbState) state <- mkReg(Flush);

    function indexT getIndex(Vpn vpn) = truncate(vpn);

    // we don't accept req when there is an old req to the same index, because
    // at resp end we may write RAM. This resolves read-after-write hazard.
    Wire#(Maybe#(indexT)) pendIndex <- mkBypassWire;
    (* fire_when_enabled, no_implicit_conditions *)
    rule setPendIndex;
        if(pendReq_deq matches tagged Valid .rq) begin
            case(rq) matches
                tagged Translate .r: begin
                    pendIndex <= Valid (getIndex(r.vpn));
                end
                tagged Refill .e: begin
                    pendIndex <= Valid (getIndex(e.vpn));
                end
                default: begin
                    pendIndex <= Invalid;
                end
            endcase
        end
        else begin
            pendIndex <= Invalid;
        end
    endrule

    rule doFlush(state == Flush);
        // since TLB is a read-only cache, we can discard everything
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            tlbRam[i].wrReq(flushIdx, SetAssocTlbEntry {valid: False, entry: ?});
        end
        repRam.wrReq(flushIdx, repInfoInitVal);
        // update states
        flushIdx <= flushIdx + 1;
        if(flushIdx == maxBound) begin
            // flush is done
            state <= Ready;
        end
    endrule

    rule doAddEntry(
        state == Ready &&&
        pendReq_deq matches tagged Valid (tagged Refill .newEn)
    );
        // deq pipeline reg and ram resp
        pendReq_deq <= Invalid;
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            tlbRam[i].deqRdResp;
        end
        repRam.deqRdResp;
        // get all the tlb ram resp & rep ram resp
        Vector#(wayNum, Bool) validVec;
        Vector#(wayNum, TlbEntry) entryVec;
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            let r = tlbRam[i].rdResp;
            validVec[i] = r.valid;
            entryVec[i] = r.entry;
        end
        repInfoT repInfo = repRam.rdResp;
        // check if the new entry already exists
        function Bool sameEntry(wayT w);
            let en = entryVec[w];
            Bool same_page = en.vpn == newEn.vpn &&
                             en.asid == newEn.asid &&
                             en.pteType.global == newEn.pteType.global;
            return validVec[w] && same_page;
        endfunction
        Vector#(wayNum, wayT) wayVec = genWith(fromInteger);
        if(find(sameEntry, wayVec) matches tagged Valid .way) begin
            // entry exists, update rep info
            indexT idx = getIndex(newEn.vpn);
            repRam.wrReq(idx, updateRepInfo(repInfo, way));
        end
        else begin
            // find a slot for this translation
            // Since TLB is read-only cache, we can silently evict
            function Bool flip(Bool x) = !x;
            wayT addWay = getRepWay(repInfo, map(flip, validVec));
            // update slot & rep info
            indexT idx = getIndex(newEn.vpn);
            tlbRam[addWay].wrReq(idx, SetAssocTlbEntry {
                valid: True, entry: newEn
            });
            repRam.wrReq(idx, updateRepInfo(repInfo, addWay));
        end
    endrule

    // start flush when no pending req
    method Action flush if(state == Ready && !isValid(pendReq[0]));
        state <= Flush;
        flushIdx <= 0;
    endmethod

    method Bool flush_done = state == Ready;

    // accept new req when
    // (1) in Ready
    // (2) not waiting for flush
    // (3) pipeline reg available
    method Action req(SetAssocTlbReq r) if(state == Ready && !isValid(pendReq_enq));
        // (4) new req does not match index of existing req (no read/write race
        // in BRAM)
        Vpn vpn = (case(r) matches
            tagged Translate .rq: (rq.vpn);
            tagged Refill .en: (en.vpn);
            default: ?;
        endcase);
        indexT idx = getIndex(vpn);
        when(pendIndex != Valid (idx), noAction);
        // save req
        pendReq_enq <= Valid (r);
        // read ram
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            tlbRam[i].rdReq(idx);
        end
        repRam.rdReq(idx);
    endmethod

    method respT resp if(
        state == Ready &&&
        pendReq_deq matches tagged Valid (tagged Translate .rq)
    );
        // get all the tlb ram resp & LRU
        Vector#(wayNum, SetAssocTlbEntry) entries;
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            entries[i] = tlbRam[i].rdResp;
        end
        repInfoT repInfo = repRam.rdResp;
        // do VPN match (only 4KB page) and ASID match
        function Bool entryHit(wayT i);
            SetAssocTlbEntry en = entries[i];
            Bool vpnMatch = en.entry.vpn == rq.vpn;
            Bool asidMatch = en.entry.asid == rq.asid || en.entry.pteType.global;
            return en.valid && asidMatch && vpnMatch;
        endfunction
        Vector#(wayNum, wayT) wayVec = genWith(fromInteger);
        if(find(entryHit, wayVec) matches tagged Valid .w) begin
            // hit
            return SetAssocTlbResp {
                hit: True,
                way: w,
                entry: entries[w].entry
            };
        end
        else begin
            // miss
            return SetAssocTlbResp {
                hit: False,
                way: ?,
                entry: ?
            };
        end
    endmethod

    // deq resp from pipeline and update a way and LRU
    method Action deqResp(Maybe#(wayT) hitWay) if(
        state == Ready &&&
        pendReq_deq matches tagged Valid (tagged Translate .rq)
    );
        // deq pipeline reg
        pendReq_deq <= Invalid;
        // deq ram read resp
        for(Integer i = 0; i < valueof(wayNum); i = i+1) begin
            tlbRam[i].deqRdResp;
        end
        repRam.deqRdResp;
        // update rep ram
        if(hitWay matches tagged Valid .way) begin
            let idx = getIndex(rq.vpn);
            let repInfo = repRam.rdResp;
            repRam.wrReq(idx, updateRepInfo(repInfo, way));
        end
    endmethod
endmodule
