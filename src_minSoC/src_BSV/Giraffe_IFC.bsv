
package Giraffe_IFC;

import Axi ::*;
import Axi4 ::*;
import Bus ::*;
import Connectable ::*;

`include "TLM.defines"
`include "Giraffe.defines"

(* always_ready, always_enabled *)
interface JTAG_IFC;
   (* prefix = "", result = "unused0" *)
   method Action tdi((* port = "tdi" *) Bit#(1) x);

   (* prefix = "", result = "unused1" *)
   method Action tms((* port = "tms" *) Bit#(1) x);

   (* prefix = "", result = "unused2" *)
   method Action tclk((* port = "tclk" *) Bit#(1) x);

   (* result = "tdo" *)
   method Bit#(1) tdo;

   interface Clock tclk_out;
endinterface

(* always_ready, always_enabled *)
interface DMI_Master_IFC;
   (* prefix = "", result = "unused0" *)
   method Action req_ready((* port = "req_ready" *) Bit#(1) x);

   (* result = "req_valid" *)
   method Bit#(1) req_valid;

   (* result = "req_addr" *)
   method Bit#(7) req_addr;

   (* result = "req_data" *)
   method Bit#(32) req_data;

   (* result = "req_op" *)
   method Bit#(2) req_op;

   (* result = "rsp_ready" *)
   method Bit#(1) rsp_ready;

   (* prefix = "", result = "unused1" *)
   method Action rsp_valid((* port = "rsp_valid" *) Bit#(1) x);

   (* prefix = "", result = "unused2" *)
   method Action rsp_data((* port = "rsp_data" *) Bit#(32) x);

   (* prefix = "", result = "unused3" *)
   method Action rsp_response((* port = "rsp_response" *) Bit#(2) x);
endinterface

(* always_ready, always_enabled *)
interface DMI_IFC;
   (* result = "req_ready" *)
   method Bit#(1) req_ready;

   (* prefix = "", result = "unused0" *)
   method Action req_valid((* port = "req_valid" *) Bit#(1) x);

   (* prefix = "", result = "unused1" *)
   method Action req_addr((* port = "req_addr" *) Bit#(7) x);

   (* prefix = "", result = "unused2" *)
   method Action req_data((* port = "req_data" *) Bit#(32) x);

   (* prefix = "", result = "unused3" *)
   method Action req_op((* port = "req_op" *) Bit#(2) x);

   (* prefix = "", result = "unused4" *)
   method Action rsp_ready((* port = "rsp_ready" *) Bit#(1) x);

   (* result = "rsp_valid" *)
   method Bit#(1) rsp_valid;

   (* result = "rsp_data" *)
   method Bit#(32) rsp_data;

   (* result = "rsp_response" *)
   method Bit#(2) rsp_response;
endinterface

instance Connectable#(DMI_Master_IFC, DMI_IFC);
   module mkConnection#(DMI_Master_IFC m, DMI_IFC s)(Empty);
      mkConnection(m.req_ready, s.req_ready);
      mkConnection(m.req_valid, s.req_valid);
      mkConnection(m.req_addr, s.req_addr);
      mkConnection(m.req_data, s.req_data);
      mkConnection(m.req_op, s.req_op);
      mkConnection(m.rsp_ready, s.rsp_ready);
      mkConnection(m.rsp_valid, s.rsp_valid);
      mkConnection(m.rsp_data, s.rsp_data);
      mkConnection(m.rsp_response, s.rsp_response);
   endmodule
endinstance

instance Connectable#(DMI_IFC, DMI_Master_IFC);
   module mkConnection#(DMI_IFC s, DMI_Master_IFC m)(Empty);
      mkConnection(m, s);
   endmodule
endinstance

interface Core_IFC;
   interface Axi4LRdWrMaster#(`TLM_PRM_Giraffe) master0;
   interface Axi4LRdWrMaster#(`TLM_PRM_Giraffe) master1;
   interface Axi4LRdWrMaster#(`TLM_PRM_Giraffe) master2;
   interface Axi4LRdWrMaster#(`TLM_PRM_Giraffe) master3;

   (* always_ready, always_enabled *)
   (* prefix = "", result = "unused0" *)
   method Action interrupt0((* port = "interrupt0" *) Bit#(1) x);

   (* always_ready, always_enabled *)
   (* prefix = "", result = "unused1" *)
   method Action interrupt1((* port = "interrupt1" *) Bit#(1) x);

   (* always_ready, always_enabled *)
   (* prefix = "", result = "unused2" *)
   method Action interrupt2((* port = "interrupt2" *) Bit#(1) x);

`ifdef JTAG_TAP
   (* prefix = "" *)
   interface JTAG_IFC jtag;
`else
   (* prefix = "dmi" *)
   interface DMI_IFC dmi;
`endif
endinterface

interface Platform_IFC;
   interface Axi4LRdWrSlave#(`TLM_PRM_Giraffe) slave0;
   interface Axi4LRdWrSlave#(`TLM_PRM_Giraffe) slave1;
   interface Axi4LRdWrSlave#(`TLM_PRM_Giraffe) slave2;
   interface Axi4LRdWrSlave#(`TLM_PRM_Giraffe) slave3;

   (* always_ready, always_enabled *)
   method Bit#(1) interrupt0;

   (* always_ready, always_enabled *)
   method Bit#(1) interrupt1;

   (* always_ready, always_enabled *)
   method Bit#(1) interrupt2;

   interface BusSend#(Bit#(8)) uart_out;
   interface BusRecv#(Bit#(8)) uart_in;

endinterface

endpackage
