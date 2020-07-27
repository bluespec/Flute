
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

/*
This file contains many FIFO implementations
1) Conflict Free FIFO with 2 elements
2) Pipeline FIFO with 1 element
3) Bypass FIFO with 1 element
4) Conflict Free FIFO with n elements (ptr based implementation)
5) Pipeline FIFO with n elements (ptr based implementation)
6) Searchable n-element FIFO has an extra search method added to the n-element FIFOs
   a) Searchable conflict-free n-element FIFO
   b) Searchable pipelined n-element FIFO

Clear always happens after enq and deq (but before canonicalize when applicable)
All these FIFOs have been tested with various pipelines. In particular n-element FIFO can be put in place of the respective 2-element or 1-element FIFO

*/

import Ehr::*;
import GetPut::*;
import Vector::*;
import FIFOF::*;
import Types::*; // for assertion

interface Fifo#(numeric type n, type t);
    method Bool notFull;
    method Action enq(t x);
    method Bool notEmpty;
    method Action deq;
    method t first;
    method Action clear;
endinterface

interface SupFifoEnq#(type t);
    method Bool canEnq;
    method Action enq(t x);
endinterface

interface SupFifoDeq#(type t);
    method Bool canDeq;
    method Action deq;
    method t first;
endinterface

interface SupFifo#(numeric type k, numeric type n, type t);
    interface Vector#(k, SupFifoEnq#(t)) enqS;
    interface Vector#(k, SupFifoDeq#(t)) deqS;
    method Bool internalEmpty; // for security
endinterface

function Integer getMaxIndex( FifoState#(n) s );
    return valueOf(n)-1;
endfunction

interface FifoState#(numeric type n);
    interface Reg#(Bit#(TLog#(n)))  enqP;
    interface Reg#(Bit#(TLog#(n)))  deqP;
    interface Reg#(Bool)            empty;
    interface Reg#(Bool)            full;
    method Bool isValidIndex(Integer i);
endinterface

function FifoState#(n) toFifoState( Reg#(Bit#(TLog#(n))) _enqP, Reg#(Bit#(TLog#(n))) _deqP, Reg#(Bool) _empty, Reg#(Bool) _full );
    return (interface FifoState;
                interface Reg enqP = _enqP;
                interface Reg deqP = _deqP;
                interface Reg empty = _empty;
                interface Reg full = _full;
                method Bool isValidIndex( Integer i );
                    Bool gte_deqP = fromInteger(i) >= _deqP;
                    Bool lt_enqP = fromInteger(i) < _enqP;
                    return _full || (_deqP > _enqP && (gte_deqP || lt_enqP)) || (_enqP > _deqP && gte_deqP && lt_enqP);
                endmethod
            endinterface);
endfunction

function Bool isValidIndex( FifoState#(n) s, Integer i );
    Bool gte_deqP = fromInteger(i) >= s.deqP;
    Bool lt_enqP = fromInteger(i) < s.enqP;
    return s.full || (s.deqP > s.enqP && (gte_deqP || lt_enqP)) || (s.enqP > s.deqP && gte_deqP && lt_enqP);
endfunction

instance ToGet#(Fifo#(n, t), t);
  function Get#(t) toGet(Fifo#(n, t) f);
    return (interface Get;
      method ActionValue#(t) get;
        f.deq;
        return f.first;
      endmethod
    endinterface);
  endfunction
endinstance

instance ToPut#(Fifo#(n, t), t);
  function Put#(t) toPut(Fifo#(n, t) f);
    return (interface Put;
      method Action put(t r);
        f.enq(r);
      endmethod
    endinterface);
  endfunction
endinstance

// instance ToGet#(SupFifo#(k,n, t), t);
//   function Get#(Vector#(k,Maybe#(t))) toGet(SupFifo#(k,n, t) f);
//     return (interface Get;
//       method ActionValue#(Vector#(k,Maybe#(t)) get;
// //  
//         f.deq;
//         return f.first;
//       endmethod
//     endinterface);
//   endfunction
// endinstance

// instance ToPut#(SupFifo#(k,n, t), t);
//   function Put#(Vector#(k,Maybe#(t))) toPut(SupFifo#(k,n, t) f);
//     return (interface Put;
//       method Action put(Vector#(k,Maybe#(t)) r);
//
//       f.enq(r);
//       endmethod
//     endinterface);
//   endfunction
// endinstance

module mkSupFifo(SupFifo#(k,n,t)) provisos (
    Bits#(t,tSz),
    FShow#(t),
    Add#(TExp#(TLog#(k)), 0, k) // k must be power of 2
);
    Vector#(k, FIFOF#(t)) internalFifos <- replicateM(mkUGSizedFIFOF(valueOf(n)));
    //I need a custom counter here for now will work only for power of two
    Ehr#(TAdd#(1,k), Bit#(TLog#(k))) enqueueFifo <- mkEhr(unpack(0));
    Ehr#(TAdd#(1,k), Bit#(TLog#(k))) dequeueFifo <- mkEhr(unpack(0));
    Vector#(k, Ehr#(2,Maybe#(t))) enqueueElement <- replicateM(mkEhr(tagged Invalid));
    Vector#(k, Ehr#(2, Bool)) willDequeue <- replicateM(mkEhr(False));

    function SupFifoEnq#(t) getEnqIfc(Integer i);
        Bit#(TLog#(k)) fifoIdx = enqueueFifo[0]+fromInteger(i);
        Bool can_enq = internalFifos[fifoIdx].notFull;
        return (interface SupFifoEnq;
            method Bool canEnq = can_enq;
            method Action enq(t x) if(can_enq);
                enqueueElement[i][0] <= tagged Valid x;
            endmethod
        endinterface);
    endfunction

    function SupFifoDeq#(t) getDeqIfc(Integer i);
        Bit#(TLog#(k)) fifoIdx = dequeueFifo[0]+fromInteger(i);
        Bool can_deq = internalFifos[fifoIdx].notEmpty;
        return (interface SupFifoDeq;
            method Bool canDeq = can_deq;
            method t first if(can_deq);
                return internalFifos[fifoIdx].first;
            endmethod
            method Action deq if(can_deq);
                willDequeue[i][0] <= True;
            endmethod
        endinterface);
    endfunction

    (* fire_when_enabled, no_implicit_conditions *)
    rule canonicalize;
        for (Integer i = 0; i < valueOf(k); i = i+1) begin
            if(enqueueElement[i][1] matches tagged Valid .el) begin
                enqueueFifo[i] <= enqueueFifo[0]+fromInteger(i)+1;
                internalFifos[enqueueFifo[0]+fromInteger(i)].enq(el);
                doAssert(internalFifos[enqueueFifo[0]+fromInteger(i)].notFull, "FIFO must be not full");
                if(i > 0) begin
                    doAssert(isValid(enqueueElement[i-1][1]), "FIFO enq must be consecutive");
                end
            end
            if (willDequeue[i][1]) begin
                dequeueFifo[i] <= dequeueFifo[0] + fromInteger(i) + 1;
                internalFifos[dequeueFifo[0]+fromInteger(i)].deq;
                doAssert(internalFifos[dequeueFifo[0]+fromInteger(i)].notEmpty, "FIFO must be not empty");
                if(i > 0) begin
                    doAssert(willDequeue[i-1][1], "FIFO deq must be consecutive");
                end
            end
            enqueueElement[i][1] <= tagged Invalid;
            willDequeue[i][1] <= False;
        end
    endrule

    Vector#(k,Integer) indexes = genVector;
    interface Vector enqS = map(getEnqIfc, indexes);
    interface Vector deqS = map(getDeqIfc, indexes);

    method Bool internalEmpty;
        function Bool isEmpty(Integer i) = !internalFifos[i].notEmpty;
        return all(isEmpty, indexes);
    endmethod
endmodule



// A Conflict free implementation of n element FIFO
// {notEmpty, first} < deq < clear < canonicalize
// notFull < enq < clear < canonicalize
// deq conflict free with enq
// canonicalize has no effect after clear anyway

// FIXME: This FIFO is broken because {enq, deq} can't be CF with {notEmpty, notFull}
module mkCFFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
  // n is size of fifo
  // t is data type of fifo

  // storage elements
  Vector#(n, Reg#(t))   data  <- replicateM(mkReg(unpack(0)));
  Reg#(Bit#(TLog#(n)))  enqP  <- mkReg(0);
  Reg#(Bit#(TLog#(n)))  deqP  <- mkReg(0);
  Reg#(Bool)            empty <- mkReg(True);
  Reg#(Bool)            full  <- mkReg(False);

  // requests
  Ehr#(3, Maybe#(t))    enqReq   <- mkEhr(tagged Invalid);
  Ehr#(3, Maybe#(void)) deqReq   <- mkEhr(tagged Invalid);
  Ehr#(2, Maybe#(void)) clearReq <- mkEhr(tagged Invalid);

  // useful value
  Bit#(TLog#(n))        max_index = fromInteger(valueOf(n)-1);

  // Update the state of the fifo to match any enqueue, dequeue, or clear
  // These attributes are statically checked by the compiler
  (* fire_when_enabled *)     // WILL_FIRE == CAN_FIRE
  (* no_implicit_conditions *)  // CAN_FIRE == guard (True)
  rule canonicalize;
    if( isValid(clearReq[1]) ) begin
      enqP <= 0;
      deqP <= 0;
      full <= False;
      empty <= True;
    end else begin
      let next_enqP = enqP;
      let next_deqP = deqP;
      let next_full = full;
      let next_empty = empty;

      if( isValid(enqReq[2]) ) begin
        data[enqP] <= fromMaybe(?, enqReq[2]);
        next_enqP = (enqP == max_index) ? 0 : enqP + 1;
      end
      if( isValid(deqReq[2]) ) begin
        next_deqP = (deqP == max_index) ? 0 : deqP + 1;
      end

      // compute next_full and next_empty
      // XXX need to set both next_empty and next_full for 1-element FIFO
      if( next_deqP == next_enqP ) begin
        if( isValid(enqReq[2]) ) begin
          // enqueued to full
          next_full = True;
          next_empty = False;
        end else if( isValid(deqReq[2]) ) begin
          // dequeued to empty
          next_empty = True;
          next_full = False;
        end else begin
          // no deq or enq, so no change
        end
      end else begin
        // next_deqP != next_enqP so neither full nor empty
        next_full = False;
        next_empty = False;
      end

      // update the states
      enqP <= next_enqP;
      deqP <= next_deqP;
      full <= next_full;
      empty <= next_empty;
    end

    // clear pending requests
    clearReq[1] <= tagged Invalid;
    enqReq[2] <= tagged Invalid;
    deqReq[2] <= tagged Invalid;
  endrule

  method Bool notFull = !full;

  method Action enq(t x) if( !full );
    // Tell later stages an enqueue was requested
    enqReq[0] <= tagged Valid x;
  endmethod

  method Bool notEmpty = !empty;

  method Action deq if( !empty );
    // Tell later stages a dequeue was requested
    deqReq[0] <= tagged Valid;
  endmethod

  method t first if( !empty );
    return data[deqP];
  endmethod

  method Action clear;
    // Clear any existing enq/deq requests
    // gets desired {enq, deq} < clear schedule
    enqReq[1] <= tagged Invalid;
    deqReq[1] <= tagged Invalid;
    // Tell later stages a clear was requested
    clearReq[0] <= tagged Valid;
  endmethod
endmodule

// A pipelined implementation of n element FIFO
// {notEmpty, first} < deq < notFull < enq < clear

module mkPipelineFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
  // n is size of fifo
  // t is data type of fifo
  Vector#(n, Reg#(t))   data   <- replicateM(mkReg(unpack(0)));
  Ehr#(2, Bit#(TLog#(n))) enqP   <- mkEhr(0);
  Ehr#(2, Bit#(TLog#(n))) deqP   <- mkEhr(0);
  Ehr#(3, Bool)       empty  <- mkEhr(True);
  Ehr#(3, Bool)       full   <- mkEhr(False);
  Bit#(TLog#(n))      max_index = fromInteger(valueOf(n)-1);

  method Bool notFull = !full[1];

  method Action enq(t x) if( !full[1] );
    data[enqP[0]] <= x;
    empty[1] <= False;
    let next_enqP = (enqP[0] == max_index) ? 0 : enqP[0] + 1;
    enqP[0] <= next_enqP;
    if( next_enqP == deqP[1] ) begin
      full[1] <= True;
    end
  endmethod

  method Bool notEmpty = !empty[0];

  method Action deq if( !empty[0] );
    full[0] <= False;
    let next_deqP = (deqP[0] == max_index) ? 0 : deqP[0] + 1;
    deqP[0] <= next_deqP;
    if( next_deqP == enqP[0] ) begin
      empty[0] <= True;
    end
  endmethod

  method t first if( !empty[0] );
    return data[deqP[0]];
  endmethod

  method Action clear;
    enqP[1] <= 0;
    deqP[1] <= 0;
    empty[2] <= True;
    full[2] <= False;
  endmethod
endmodule

// A Bypass implementation of n element FIFO
// notFull < enq < {notEmpty, first} < deq < clear

module mkBypassFifo( Fifo#(n, t) ) provisos (Bits#(t,tSz));
  // n is size of fifo
  // t is data type of fifo
  Vector#(n, Ehr#(2,t))   data  <- replicateM(mkEhr(?));
  Ehr#(2, Bit#(TLog#(n))) enqP  <- mkEhr(0);
  Ehr#(2, Bit#(TLog#(n))) deqP  <- mkEhr(0);
  Ehr#(3, Bool)           empty <- mkEhr(True);
  Ehr#(3, Bool)           full  <- mkEhr(False);
  Bit#(TLog#(n))          max_index = fromInteger(valueOf(n)-1);

  method Bool notFull = !full[0];

  method Action enq(t x) if( !full[0] );
    data[enqP[0]][0] <= x;
    empty[0] <= False;
    let next_enqP = (enqP[0] == max_index) ? 0 : enqP[0] + 1;
    enqP[0] <= next_enqP;
    if( next_enqP == deqP[0] ) begin
      full[0] <= True;
    end
  endmethod

  method Bool notEmpty = !empty[1];

  method Action deq if( !empty[1] );
    full[1] <= False;
    let next_deqP = (deqP[0] == max_index) ? 0 : deqP[0] + 1;
    deqP[0] <= next_deqP;
    if( next_deqP == enqP[1] ) begin
      empty[1] <= True;
    end
  endmethod

  method t first if( !empty[1] );
    return data[deqP[0]][1];
  endmethod

  method Action clear;
    enqP[1] <= 0;
    deqP[1] <= 0;
    empty[2] <= True;
    full[2] <= False;
  endmethod
endmodule


// Searchable FIFO has an extra search method
interface SFifo#(numeric type n, type t, type st);
  method Bool notFull;
  method Action enq(t x);
  method Bool notEmpty;
  method Action deq;
  method Bool search(st s);
  method t first;
  method Action clear;
endinterface

// search is conflict-free with {enq, deq, first, notFull, notEmpty}
// search <  clear < canonicalize
module mkCFSFifo#(function Bool isFound(t v, st k))(SFifo#(n, t, st)) provisos(Bits#(t, tSz), Add#(n, 1, n1), Log#(n1, sz), Add#(sz, 1, sz1));
  Integer ni = valueOf(n);
  Bit#(sz1) nb = fromInteger(ni);
  Bit#(sz1) n2 = 2*nb;
  Vector#(n, Reg#(t)) data <- replicateM(mkRegU);
  Ehr#(3, Bit#(sz1)) enqP <- mkEhr(0);
  Ehr#(3, Bit#(sz1)) deqP <- mkEhr(0);
  Ehr#(3, Bool) enqEn <- mkEhr(True);
  Ehr#(3, Bool) deqEn <- mkEhr(False);
  Ehr#(2, t)                 tempData <- mkEhr(?);
  Ehr#(2, Maybe#(Bit#(sz1))) tempEnqP <- mkEhr(Invalid);
  Ehr#(2, Maybe#(Bit#(sz1))) tempDeqP <- mkEhr(Invalid);

  Bit#(sz1) cnt0 = enqP[0] >= deqP[0]? enqP[0] - deqP[0]:
                                 (enqP[0]%nb + nb) - deqP[0]%nb;
  Bit#(sz1) cnt2 = enqP[2] >= deqP[2]? enqP[2] - deqP[2]:
                                 (enqP[2]%nb + nb) - deqP[2]%nb;
  rule canonicalize;
    if(!enqEn[2] && cnt2 != nb) enqEn[2] <= True;
    if(!deqEn[2] && cnt2 != 0) deqEn[2] <= True;

    if(isValid(tempEnqP[1]))
    begin
      data[validValue(tempEnqP[1])] <= tempData[1];
      tempEnqP[1] <= Invalid;
    end

    if(isValid(tempDeqP[1]))
    begin
      deqP[0] <= validValue(tempDeqP[1]);
      tempDeqP[1] <= Invalid;
    end
  endrule

  method Bool notFull = enqEn[0];

  method Action enq(t x) if(enqEn[0]);
    tempData[0] <= x;
    tempEnqP[0] <= Valid (enqP[0]%nb);
    enqP[0] <= (enqP[0] + 1)%n2;
    enqEn[0] <= False;
  endmethod

  method Bool notEmpty = deqEn[0];

  method Action deq if(deqEn[0]);
    tempDeqP[0] <= Valid ((deqP[0] + 1)%n2);
    deqEn[0] <= False;
  endmethod

  method t first if(deqEn[0]);
    return data[deqP[0]%nb];
  endmethod

  method Bool search(st s);
    Bool ret = False;
    for(Bit#(sz1) i = 0; i < nb; i = i + 1)
    begin
      let ptr = (deqP[0] + i)%nb;
      if(isFound(data[ptr], s) && i < cnt0)
        ret = True;
    end
    return ret;
  endmethod

  method Action clear;
    enqP[1] <= 0;
    deqP[1] <= 0;
    enqEn[1] <= True;
    deqEn[1] <= False;
  endmethod
endmodule

// {notEmpty, first} < deq < search
// search CF {enq, notFull}
// search < clear
module mkPipelineSFifo#(function Bool isFound(t v, st k))(SFifo#(n, t, st)) provisos(Bits#(t, tSz), Add#(n, 1, n1), Log#(n1, sz), Add#(sz, 1, sz1), Bits#(st, stz));
  Integer ni = valueOf(n);
  Bit#(sz1) nb = fromInteger(ni);
  Bit#(sz1) n2 = 2*nb;
  Vector#(n, Reg#(t)) data <- replicateM(mkRegU);
  Ehr#(3, Bit#(sz1)) enqP <- mkEhr(0);
  Ehr#(2, Bit#(sz1)) deqP <- mkEhr(0);

  Bit#(sz1) cnt0 = enqP[0] >= deqP[0]? enqP[0] - deqP[0]:
                                       (enqP[0]%nb + nb) - deqP[0]%nb;
  Bit#(sz1) cnt1 = enqP[0] >= deqP[1]? enqP[0] - deqP[1]:
                                       (enqP[0]%nb + nb) - deqP[1]%nb;

  method Bool notFull = cnt1 < nb;

  method Action enq(t x) if(cnt1 < nb);
    enqP[0] <= (enqP[0] + 1)%n2;
    data[enqP[0]%nb] <= x;
  endmethod

  method Bool notEmpty = cnt0 != 0;

  method Action deq if(cnt0 != 0);
    deqP[0] <= (deqP[0] + 1)%n2;
  endmethod

  method t first if(cnt0 != 0);
    return data[deqP[0]%nb];
  endmethod

  method Bool search(st s);
    Bool ret = False;
    for(Bit#(sz1) i = 0; i < nb; i = i + 1)
    begin
      let ptr = (deqP[1] + i)%nb;
      if(isFound(data[ptr], s) && i < cnt1)
        ret = True;
    end
    return ret;
  endmethod

  method Action clear;
    enqP[2] <= 0;
    deqP[1] <= 0;
  endmethod
endmodule

// Searchable Count FIFO has an extra search method which returns the count of the number of elements found
interface SCountFifo#(numeric type n, type t, type st);
  method Bool notFull;
  method Action enq(t x);
  method Bool notEmpty;
  method Action deq;
  method Bit#(TLog#(TAdd#(n, 1))) search(st s);
  method t first;
  method Action clear;
endinterface

// search is conflict-free with {enq, deq, first, notFull, notEmpty}
// search <  clear < canonicalize
module mkCFSCountFifo#(function Bool isFound(t v, st k))(SCountFifo#(n, t, st)) provisos(Bits#(t, tSz), Add#(n, 1, n1), Log#(n1, sz), Add#(sz, 1, sz1));
  Integer ni = valueOf(n);
  Bit#(sz1) nb = fromInteger(ni);
  Bit#(sz1) n2 = 2*nb;
  Vector#(n, Reg#(t)) data <- replicateM(mkRegU);
  Ehr#(3, Bit#(sz1)) enqP <- mkEhr(0);
  Ehr#(3, Bit#(sz1)) deqP <- mkEhr(0);
  Ehr#(3, Bool) enqEn <- mkEhr(True);
  Ehr#(3, Bool) deqEn <- mkEhr(False);
  Ehr#(2, t)                 tempData <- mkEhr(?);
  Ehr#(2, Maybe#(Bit#(sz1))) tempEnqP <- mkEhr(Invalid);
  Ehr#(2, Maybe#(Bit#(sz1))) tempDeqP <- mkEhr(Invalid);

  Bit#(sz1) cnt0 = enqP[0] >= deqP[0]? enqP[0] - deqP[0]:
                                 (enqP[0]%nb + nb) - deqP[0]%nb;
  Bit#(sz1) cnt2 = enqP[2] >= deqP[2]? enqP[2] - deqP[2]:
                                 (enqP[2]%nb + nb) - deqP[2]%nb;
  rule canonicalize;
    if(!enqEn[2] && cnt2 != nb) enqEn[2] <= True;
    if(!deqEn[2] && cnt2 != 0) deqEn[2] <= True;

    if(isValid(tempEnqP[1]))
    begin
      data[validValue(tempEnqP[1])] <= tempData[1];
      tempEnqP[1] <= Invalid;
    end

    if(isValid(tempDeqP[1]))
    begin
      deqP[0] <= validValue(tempDeqP[1]);
      tempDeqP[1] <= Invalid;
    end
  endrule

  method Bool notFull = enqEn[0];

  method Action enq(t x) if(enqEn[0]);
    tempData[0] <= x;
    tempEnqP[0] <= Valid (enqP[0]%nb);
    enqP[0] <= (enqP[0] + 1)%n2;
    enqEn[0] <= False;
  endmethod

  method Bool notEmpty = deqEn[0];

  method Action deq if(deqEn[0]);
    tempDeqP[0] <= Valid ((deqP[0] + 1)%n2);
    deqEn[0] <= False;
  endmethod

  method t first if(deqEn[0]);
    return data[deqP[0]%nb];
  endmethod

  method Bit#(TLog#(TAdd#(n, 1))) search(st s);
    Bit#(TLog#(TAdd#(n, 1))) ret = 0;
    for(Bit#(sz1) i = 0; i < nb; i = i + 1)
    begin
      let ptr = (deqP[0] + i)%nb;
      if(isFound(data[ptr], s) && i < cnt0)
        ret = ret + 1;
    end
    return ret;
  endmethod

  method Action clear;
    enqP[1] <= 0;
    deqP[1] <= 0;
    enqEn[1] <= True;
    deqEn[1] <= False;
  endmethod
endmodule

