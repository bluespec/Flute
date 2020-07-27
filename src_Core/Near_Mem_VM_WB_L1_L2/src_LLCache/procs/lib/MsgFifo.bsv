
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

import Fifos::*;

typedef union tagged {
    reqT Req;
    respT Resp;
} CacheMsg#(type reqT, type respT) deriving(Bits, Eq);

interface MsgFifo#(numeric type n, type reqT, type respT);
    // enq port
    method Bool reqNotFull;
    method Bool respNotFull;
    method Action enqReq(reqT r);
    method Action enqResp(respT r);
    // deq port
    method Bool notEmpty;
    method Bool reqNotEmpty;
    method Bool respNotEmpty;
    method Action deq;
    method CacheMsg#(reqT, respT) first;
endinterface

interface MsgFifoEnq#(type reqT, type respT);
    method Bool reqNotFull;
    method Bool respNotFull;
    method Action enqReq(reqT r);
    method Action enqResp(respT r);
endinterface

function MsgFifoEnq#(reqT, respT) toMsgFifoEnq(MsgFifo#(n, reqT, respT) ifc);
    return (interface MsgFifoEnq;
        method reqNotFull = ifc.reqNotFull;
        method respNotFull = ifc.respNotFull;
        method enqReq = ifc.enqReq;
        method enqResp = ifc.enqResp;
    endinterface);
endfunction

interface MsgFifoDeq#(type reqT, type respT);
    method Bool notEmpty;
    method Bool reqNotEmpty;
    method Bool respNotEmpty;
    method Action deq;
    method CacheMsg#(reqT, respT) first;
endinterface

function MsgFifoDeq#(reqT, respT) toMsgFifoDeq(MsgFifo#(n, reqT, respT) ifc);
    return (interface MsgFifoDeq;
        method notEmpty = ifc.notEmpty;
        method reqNotEmpty = ifc.reqNotEmpty;
        method respNotEmpty = ifc.respNotEmpty;
        method deq = ifc.deq;
        method first = ifc.first;
    endinterface);
endfunction

module mkMsgFifo(MsgFifo#(n, reqT, respT)) provisos(
    Bits#(reqT, reqW),
    Bits#(respT, respW),
    Add#(a__, 1, n)
);
    Fifo#(n, reqT) reqQ <- mkCFFifo;
    Fifo#(n, respT) respQ <- mkCFFifo;

    method reqNotFull = reqQ.notFull;
    method respNotFull = respQ.notFull;
    
    method Action enqReq(reqT r);
        reqQ.enq(r);
    endmethod

    method Action enqResp(respT r);
        respQ.enq(r);
    endmethod

    method Bool notEmpty = reqQ.notEmpty || respQ.notEmpty;
    method Bool reqNotEmpty = reqQ.notEmpty;
    method Bool respNotEmpty = respQ.notEmpty;

    method CacheMsg#(reqT, respT) first if(reqQ.notEmpty || respQ.notEmpty);
        // resp has higher priority than req
        if(respQ.notEmpty) begin
            return (Resp (respQ.first));
        end
        else begin
            return (Req (reqQ.first));
        end
    endmethod

    method Action deq if(reqQ.notEmpty || respQ.notEmpty);
        // resp has higher priority than req
        if(respQ.notEmpty) begin
            respQ.deq;
        end
        else begin
            reqQ.deq;
        end
    endmethod
endmodule

