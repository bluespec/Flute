// Copyright (c) 2007--2009 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$
package BusFIFO;

import BusDefines::*;
import Connectable::*;
import FIFO::*;
import FIFOF::*;
import SpecialFIFOs::*;
import FShow::*;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkBusSender#(a dflt) (BusSender#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof <- mkDFIFOF(dflt);
   let data_wire = fifof.first;

   PulseWire deq_ready <- mkPulseWire;
   PulseWire deq_deq   <- mkPulseWire;

   rule do_deq (fifof.notEmpty && (deq_ready || deq_deq));
      fifof.deq;
   endrule

   interface FIFO in;
      method Action deq;
	 deq_deq.send;
      endmethod
      method enq   = fifof.enq;
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface
   interface BusSend out;
      method a data;
	 return data_wire;
      endmethod
      method Bool valid;
	 return fifof.notEmpty;
      endmethod
      method Action ready(Bool value);
	 if (value) deq_ready.send;
      endmethod
   endinterface

endmodule

module mkBusSenderDD#(a dflt) (BusSender#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof <- mkLFIFOF;
   let data_wire = (fifof.notEmpty) ? fifof.first : dflt;

   PulseWire deq_ready <- mkPulseWire;
   PulseWire deq_deq   <- mkPulseWire;

   rule do_deq (fifof.notEmpty && (deq_ready || deq_deq));
      fifof.deq;
   endrule

   interface FIFO in;
      method Action deq;
	 deq_deq.send;
      endmethod
      method enq   = fifof.enq;
      method first = data_wire;
      method clear = fifof.clear;
   endinterface
   interface BusSend out;
      method a data;
	 return data_wire;
      endmethod
      method Bool valid;
	 return fifof.notEmpty;
      endmethod
      method Action ready(Bool value);
	 if (value) deq_ready.send;
      endmethod
   endinterface

endmodule

module mkBusReceiver (BusReceiver#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof     <- mkFIFOF;
   Wire#(a)  data_wire <- mkBypassWire;

   PulseWire enq_valid <- mkPulseWire;
   PulseWire enq_enq   <- mkPulseWire;

   rule do_enq (enq_valid || enq_enq);
      fifof.enq(data_wire);
   endrule

   interface BusRecv in;
      method Action data(a value);
	 data_wire <= value;
      endmethod
      method Action valid(Bool value);
	 if (value) enq_valid.send;
      endmethod
      method Bool ready;
	 return fifof.notFull;
      endmethod
   endinterface
   interface FIFO out;
      method Action enq(a ignore);
	 enq_enq.send;
      endmethod
      method deq   = fifof.deq;
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkSizedBusSender#(a dflt, Integer size) (BusSender#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof     <- mkSizedDFIFOF(size, dflt);
   let data_wire = fifof.first;

   PulseWire deq_ready <- mkPulseWire;
   PulseWire deq_deq   <- mkPulseWire;

   rule do_deq (fifof.notEmpty && (deq_ready || deq_deq));
      fifof.deq;
   endrule

   interface FIFO in;
      method Action deq;
	 deq_deq.send;
      endmethod
      method enq   = fifof.enq;
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface
   interface BusSend out;
      method a data;
	 return data_wire;
      endmethod
      method Bool valid;
	 return fifof.notEmpty;
      endmethod
      method Action ready(Bool value);
	 if (value) deq_ready.send;
      endmethod
   endinterface

endmodule

module mkSizedBusReceiver#(Integer size) (BusReceiver#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof     <- mkSizedFIFOF(size);
   Wire#(a)  data_wire <- mkBypassWire;

   PulseWire enq_valid <- mkPulseWire;
   PulseWire enq_enq   <- mkPulseWire;

   rule do_enq (enq_valid || enq_enq);
      fifof.enq(data_wire);
   endrule

   interface BusRecv in;
      method Action data(a value);
	 data_wire <= value;
      endmethod
      method Action valid(Bool value);
	 if (value) enq_valid.send;
      endmethod
      method Bool ready;
	 return fifof.notFull;
      endmethod
   endinterface
   interface FIFO out;
      method Action enq(a ignore);
	 enq_enq.send;
      endmethod
      method deq   = fifof.deq;
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkBypassBusSender#(a dflt) (BusSender#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof     <- mkBypassFIFOF;
   Wire#(a)  data_wire <- mkDWire(dflt);

   PulseWire deq_ready <- mkPulseWire;
   PulseWire deq_deq   <- mkPulseWire;

   rule get_data;
      data_wire <= fifof.first;
   endrule

   rule do_deq (deq_ready || deq_deq);
      fifof.deq;
   endrule

   interface FIFO in;
      method Action deq;
	 deq_deq.send;
      endmethod
      method enq   = fifof.enq;
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface
   interface BusSend out;
      method a data;
	 return data_wire;
      endmethod
      method Bool valid;
	 return fifof.notEmpty;
      endmethod
      method Action ready(Bool value);
	 if (value) deq_ready.send;
      endmethod
   endinterface

endmodule

module mkBypassBusReceiver (BusReceiver#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof     <- mkBypassFIFOF;
   Wire#(a)  data_wire <- mkBypassWire;

   PulseWire enq_valid <- mkPulseWire;
   PulseWire enq_enq   <- mkPulseWire;

   rule do_enq (enq_valid || enq_enq);
      fifof.enq(data_wire);
   endrule

   interface BusRecv in;
      method Action data(a value);
	 data_wire <= value;
      endmethod
      method Action valid(Bool value);
	 if (value) enq_valid.send;
      endmethod
      method Bool ready;
	 return fifof.notFull;
      endmethod
   endinterface
   interface FIFO out;
      method Action enq(a ignore);
	 enq_enq.send;
      endmethod
      method deq   = fifof.deq;
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkPipelineBusSender#(a dflt) (BusSender#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof     <- mkLFIFOF;
   Wire#(a)  data_wire <- mkDWire(dflt);

   PulseWire deq_ready <- mkPulseWire;
   PulseWire deq_deq   <- mkPulseWire;

   rule get_data;
      data_wire <= fifof.first;
   endrule

   rule do_deq (deq_ready || deq_deq);
      fifof.deq;
   endrule

   interface FIFO in;
      method Action deq;
	 deq_deq.send;
      endmethod
      method enq   = fifof.enq;
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface
   interface BusSend out;
      method a data;
	 return data_wire;
      endmethod
      method Bool valid;
	 return fifof.notEmpty;
      endmethod
      method Action ready(Bool value);
	 if (value) deq_ready.send;
      endmethod
   endinterface

endmodule

module mkPipelineBusReceiver (BusReceiver#(a))
   provisos(Bits#(a, sa));

   FIFOF#(a) fifof     <- mkLFIFOF;
   Wire#(a)  data_wire <- mkBypassWire;

   PulseWire enq_valid <- mkPulseWire;
   PulseWire enq_pw   <- mkPulseWire;
   PulseWire deq_pw   <- mkPulseWire;

   rule do_enq (enq_valid || enq_pw);
      fifof.enq(data_wire);
   endrule

   interface BusRecv in;
      method Action data(a value);
	 data_wire <= value;
      endmethod
      method Action valid(Bool value);
	 if (value) enq_valid.send;
      endmethod
      method Bool ready;
	 return (fifof.notFull || deq_pw);
      endmethod
   endinterface
   interface FIFO out;
      method Action enq(a ignore);
	 enq_pw.send;
      endmethod
      method Action deq;
	 deq_pw.send;
	 fifof.deq;
      endmethod
      method first = fifof.first;
      method clear = fifof.clear;
   endinterface

endmodule

endpackage
