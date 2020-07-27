
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

import Connectable::*;
import Vector::*;
import BuildVector::*;
import GetPut::*;

import CacheUtils::*;
import CCTypes::*;
import LLCache::*;
import CrossBar::*;

// import L1CoCache::*;
import LLCache_Aux :: *;

module mkL1LLConnect#(
    ParentCacheToChild#(L1Way, LLChild) llc,
    Vector#(L1Num, ChildCacheToParent#(L1Way, void)) l1
)(Empty);
    // connect cRq
    function XBarDstInfo#(Bit#(0), CRqMsg#(L1Way, LLChild)) getCRqDst(LLChild child, CRqMsg#(L1Way, void) r);
        return XBarDstInfo {
            idx: 0,
            data: CRqMsg {
                addr: r.addr,
                fromState: r.fromState,
                toState: r.toState,
                canUpToE: r.canUpToE,
                id: r.id,
                child: child
            }
        };
    endfunction
    function Get#(CRqMsg#(L1Way, void)) cRqGet(ChildCacheToParent#(L1Way, void) ifc) = toGet(ifc.rqToP);
    mkXBar(getCRqDst, map(cRqGet, l1), vec(toPut(llc.rqFromC)));

    // connect cRs
    function XBarDstInfo#(Bit#(0), CRsMsg#(LLChild)) getCRsDst(LLChild child, CRsMsg#(void) r);
        return XBarDstInfo {
            idx: 0,
            data: CRsMsg {
                addr: r.addr,
                toState: r.toState,
                data: r.data,
                child: child 
            }
        };
    endfunction
    function Get#(CRsMsg#(void)) cRsGet(ChildCacheToParent#(L1Way, void) ifc) = toGet(ifc.rsToP);
    mkXBar(getCRsDst, map(cRsGet, l1), vec(toPut(llc.rsFromC)));

    // connect pRq/pRs
    for(Integer i = 0; i < valueof(L1Num); i = i+1) begin
        rule sendPRq(llc.toC.first matches tagged PRq .rq &&& rq.child == fromInteger(i));
            llc.toC.deq;
            l1[i].fromP.enq(PRq (PRqMsg {
                addr: rq.addr,
                toState: rq.toState,
                child: ?
            }));
        endrule
        rule sendPRs(llc.toC.first matches tagged PRs .rs &&& rs.child == fromInteger(i));
            llc.toC.deq;
            l1[i].fromP.enq(PRs (PRsMsg {
                addr: rs.addr,
                toState: rs.toState,
                child: ?,
                data: rs.data,
                id: rs.id
            }));
        endrule
    end
endmodule

/*
module mkL1LLConnect#(
    ParentCacheToChild#(L1Way, LLChild) llc,
    ChildCacheToParent#(L1Way, void) dCache,
    ChildCacheToParent#(L1Way, void) iCache
)(Empty);
    LLChild dChild = 0;

    // D$
    // send cRq to P: D$ has priority
    rule doRqFromDCToP;
        let r <- toGet(dCache.rqToP).get;
        llc.rqFromC.enq(CRqMsg {
            addr: r.addr,
            fromState: r.fromState,
            toState: r.toState,
            id: r.id,
            child: dChild
        });
    endrule

    // send cRs to P: D$ has priority
    rule doRsFromDCToP;
        let r <- toGet(dCache.rsToP).get;
        llc.rsFromC.enq(CRsMsg {
            addr: r.addr,
            toState: r.toState,
            data: r.data,
            child: dChild
        });
    endrule

    // send pRs to C
    rule doRsFromPToDC(llc.toC.first matches tagged PRs .rs &&& rs.child == dChild);
        llc.toC.deq;
        dCache.fromP.enq(PRs (PRsMsg {
            addr: rs.addr,
            toState: rs.toState,
            child: ?,
            data: rs.data,
            id: rs.id
        }));
    endrule

    // send pRq to C
    rule doRqFromPToDC(llc.toC.first matches tagged PRq .rq &&& rq.child == dChild);
        llc.toC.deq;
        dCache.fromP.enq(PRq (PRqMsg {
            addr: rq.addr,
            toState: rq.toState,
            child: ?
        }));
    endrule

    // I$
    LLChild iChild = 1;

    (* descending_urgency = "doRqFromDCToP, doRqFromICToP" *)
    rule doRqFromICToP;
        let r <- toGet(iCache.rqToP).get;
        llc.rqFromC.enq(CRqMsg {
            addr: r.addr,
            fromState: r.fromState,
            toState: r.toState,
            id: r.id,
            child: iChild
        });
    endrule

    (* descending_urgency = "doRsFromDCToP, doRsFromICToP" *)
    rule doRsFromICToP;
        let r <- toGet(iCache.rsToP).get;
        llc.rsFromC.enq(CRsMsg {
            addr: r.addr,
            toState: r.toState,
            data: r.data,
            child: iChild
        });
    endrule

    rule doRsFromPToIC(llc.toC.first matches tagged PRs .rs &&& rs.child == iChild);
        llc.toC.deq;
        iCache.fromP.enq(PRs (PRsMsg {
            addr: rs.addr,
            toState: rs.toState,
            child: ?,
            data: rs.data,
            id: rs.id
        }));
    endrule

    rule doRqFromPToIC(llc.toC.first matches tagged PRq .rq &&& rq.child == iChild);
        llc.toC.deq;
        iCache.fromP.enq(PRq (PRqMsg {
            addr: rq.addr,
            toState: rq.toState,
            child: ?
        }));
    endrule
endmodule
*/
