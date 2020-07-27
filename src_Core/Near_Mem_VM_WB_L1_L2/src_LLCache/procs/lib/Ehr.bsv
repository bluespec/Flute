
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

/*
Comments: This EHR design generates the following scheduling constraints (forall i):
forall j >= i, r[i] < w[j]
forall j < i, r[i] > w[j]
forall j > i, w[i] < w[j]
w[i] conflicts with w[i]
forall j, r[i] is conflict free with r[j]
*/

import Vector::*;
import RWire::*;
import RevertingVirtualReg::*;

typedef  Vector#(n, Reg#(t)) Ehr#(numeric type n, type t);

function Vector#(n, t) readVEhr(i ehr_index, Vector#(n, Ehr#(n2, t)) vec_ehr) provisos (PrimIndex#(i, __a));
    function Reg#(t) get_ehr_index(Ehr#(n2, t) e) = e[ehr_index];
    return readVReg(map(get_ehr_index, vec_ehr));
endfunction

// extract vector ports from vector of EHRs
function Vector#(n, Reg#(t)) getVEhrPort(Vector#(n, Ehr#(m, t)) ehrs, Integer p);
    function Reg#(t) get(Ehr#(m, t) e) = e[p];
    return map(get, ehrs);
endfunction

module mkEhr#(t init)(Ehr#(n, t)) provisos(Bits#(t, tSz));
  Vector#(n, RWire#(t)) lat <- replicateM(mkUnsafeRWire);

  Vector#(n, Vector#(n, RWire#(Bool))) dummy <- replicateM(replicateM(mkUnsafeRWire));
  Vector#(n, Reg#(Bool)) dummy2 <- replicateM(mkRevertingVirtualReg(True)); // this must be true

  Reg#(t) rl <- mkReg(init);

  Ehr#(n, t) r = newVector;

  (* fire_when_enabled, no_implicit_conditions *)
  rule canon;
    t upd = rl;
    for(Integer i = 0; i < valueOf(n); i = i + 1)
      if(lat[i].wget matches tagged Valid .x)
        upd = x;
    rl <= upd;
  endrule

  for(Integer i = 0; i < valueOf(n); i = i + 1)
    r[i] = (interface Reg;
              method Action _write(t x);
                lat[i].wset(x);
                dummy2[i] <= True;
                for(Integer j = 0; j < i; j = j + 1)
                  dummy[i][j].wset(isValid(lat[j].wget));
              endmethod

              method t _read;
                t upd = rl;
                Bool yes = True;
                for(Integer j = i; j < valueOf(n); j = j + 1)
                  yes = yes && dummy2[j];
                for(Integer j = 0; j < i; j = j + 1)
                begin
                  if(lat[j].wget matches tagged Valid .x)
                    upd = x;
                end
                return yes? upd : unpack(0);
                // use a non-? val here! otherwise new BSV compiler will stop optimize at ? val
                // this affects judging if two rules are exclusive
              endmethod
            endinterface);

   return r;
endmodule

