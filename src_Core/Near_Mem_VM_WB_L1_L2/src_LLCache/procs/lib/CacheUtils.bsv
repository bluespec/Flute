
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
import BRAM::*;
import Types::*;
import MemoryTypes::*;
import GetPut::*;
import ClientServer::*;
import Connectable::*;
import Vector::*;
import Fifos::*;
import Ehr::*;
import FIFO::*;
import FIFOF::*;
import Performance::*;
import FShow::*;
import MsgFifo::*;

// 64B cache line
typedef 8 CLineNumData;
typedef TLog#(CLineNumData) LogCLineNumData;
typedef Bit#(LogCLineNumData) CLineDataSel;

function CLineDataSel getCLineDataSel(Addr a);
    return truncate(a >> valueOf(TLog#(NumBytes)));
endfunction

typedef TMul#(CLineNumData, DataSz) CacheLineSz;
typedef Bit#(CacheLineSz) CacheLine;

typedef TMul#(CLineNumData, NumBytes) CLineNumBytes;
typedef TLog#(CLineNumBytes) LogCLineNumBytes;
typedef Vector#(CLineNumBytes, Bool) CLineByteEn;

function Bool isCLineAlignAddr(Addr a);
    Bit#(LogCLineNumBytes) offset = truncate(a);
    return offset == 0;
endfunction

// cache line addr (drop the offset within cache line)
typedef TSub#(AddrSz, LogCLineNumBytes) CLineAddrSz;
typedef Bit#(CLineAddrSz) CLineAddr;

// cache line v.s. instruction
typedef TDiv#(CacheLineSz, InstSz) CLineNumInst;
typedef Bit#(TLog#(CLineNumInst)) CLineInstSel;

function CLineInstSel getCLineInstSel(Addr a);
    return truncate(a >> valueof(TLog#(TDiv#(InstSz, 8))));
endfunction

// FIFO enq/deq ifc
interface FifoEnq#(type t);
    method Bool notFull;
    method Action enq(t x);
endinterface

function FifoEnq#(t) toFifoEnq(Fifo#(n, t) f);
    return (interface FifoEnq;
        method notFull = f.notFull;
        method enq = f.enq;
    endinterface);
endfunction

function FifoEnq#(t) nullFifoEnq;
    return (interface FifoEnq;
        method Bool notFull = True;
        method Action enq(t x);
            noAction;
        endmethod
    endinterface);
endfunction

instance ToPut#(FifoEnq#(t), t);
    function Put#(t) toPut(FifoEnq#(t) ifc);
        return (interface Put;
            method Action put(t x);
                ifc.enq(x);
            endmethod
        endinterface);
    endfunction
endinstance

interface FifoDeq#(type t);
    method Bool notEmpty;
    method Action deq;
    method t first;
endinterface

function FifoDeq#(t) toFifoDeq(Fifo#(n, t) f);
    return (interface FifoDeq;
        method notEmpty = f.notEmpty;
        method deq = f.deq;
        method first = f.first;
    endinterface);
endfunction

function FifoDeq#(t) nullFifoDeq;
    return (interface FifoDeq;
        method Bool notEmpty = False;
        method Action deq if(False);
            noAction;
        endmethod
        method t first if(False);
            return ?;
        endmethod
    endinterface);
endfunction

instance ToGet#(FifoDeq#(t), t);
    function Get#(t) toGet(FifoDeq#(t) ifc);
        return (interface Get;
            method ActionValue#(t) get;
                ifc.deq;
                return ifc.first;
            endmethod
        endinterface);
    endfunction
endinstance

instance Connectable#(FifoEnq#(t), FifoDeq#(t));
    module mkConnection#(FifoEnq#(t) enq, FifoDeq#(t) deq)(Empty);
        mkConnection(toGet(deq), toPut(enq));
    endmodule
endinstance

instance Connectable#(FifoDeq#(t), FifoEnq#(t));
    module mkConnection#(FifoDeq#(t) deq, FifoEnq#(t) enq)(Empty);
        mkConnection(toGet(deq), toPut(enq));
    endmodule
endinstance

