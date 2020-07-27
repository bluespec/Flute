
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


// This is the connectal interface for a mem loader backed by connectal

// When the mem loader receives the request from RISC-V processor to initialize
// memory, it will send indication to host. And then host will keep writing
// data to the mem loader, and mem loader should send these writes to the LLC
// of RISC-V processor.

// The interface for host to write is similar to AXI. Write addr should be
// aligned w.r.t 8B.

interface MemLoaderRequest;
    // addr channel for host writes. A valid addr is the starting addr for a
    // series of sequentail writes. An invalid addr means all writes are done.
    method Action wrAddr(Bool valid, Bit#(64) addr);
    // data channel for host writes
    method Action wrData(Bit#(64) data, Bit#(8) byteEn, Bool last);
endinterface

interface MemLoaderIndication;
    // ask host to start write data; memStartAddr is the begin addr to
    // initialize memory (for sanity check at host side)
    method Action start(Bit#(64) memStartAddr);
    // signal that the requested series of writes is done
    method Action wrDone;
endinterface
