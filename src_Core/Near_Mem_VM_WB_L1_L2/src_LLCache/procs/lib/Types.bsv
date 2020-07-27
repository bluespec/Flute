
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
import Assert::*;

typedef 64 AddrSz;
typedef Bit#(AddrSz) Addr;

typedef 64 DataSz;
typedef Bit#(DataSz) Data;

typedef 32 InstSz;
typedef Bit#(InstSz) Instruction;

// Compressed instructions (16-bit)
typedef 16 Inst16_Sz;
typedef Bit #(Inst16_Sz) Instruction16;

typedef 0 AsidSz; // not really implement ASID
typedef Bit#(AsidSz) Asid;

typedef TDiv#(DataSz, 8) NumBytes;
typedef TLog#(NumBytes) IndxShamt;
typedef Vector#(NumBytes, Bool) ByteEn;

typedef TDiv#(DataSz, InstSz) DataSzInst;
typedef TLog#(DataSzInst) LgDataSzInst;
typedef Bit#(LgDataSzInst) DataInstOffset;

// These types show up in many places so they are defined here
typedef enum {Swap, Add, Xor, And, Or, Min, Max, Minu, Maxu, None} AmoFunc deriving(Bits, Eq, FShow, Bounded);
typedef enum { Ld, St, Lr, Sc, Amo, Fence } MemFunc deriving(Bits, Eq, FShow);
typedef struct {
  AmoFunc func;
  Bool    doubleWord;
  Bool    aq;
  Bool    rl;
} AmoInst deriving(Bits, Eq, FShow);
typedef struct {
    MemFunc mem_func; // Ld, St, Lr, Sc, Amo
    AmoFunc amo_func;
    Bool    unsignedLd;
    ByteEn  byteEn; // takes place of double word
    Bool    aq;
    Bool    rl;
} MemInst deriving(Bits, Eq, FShow);

`ifdef BSIM
function Action doAssert(Bool b, String s) = action if(!b) $fdisplay(stderr, "\n%m: ASSERT FAIL!!"); dynamicAssert(b, s); endaction;
`else
function Action doAssert(Bool b, String s) = dynamicAssert(b, s);
`endif
