
package Axi4LRegFile;

import Assert ::*;
import Connectable ::*;
import FIFOF ::*;
import RegFile ::*;

import AXI4_Lite_Types ::*;
import Fabric_Defs ::*;
import Semi_FIFOF ::*;

function Bit#(m) update_data(Bit#(mb) strb, Bit#(m) d_old, Bit#(m) d_new)
   provisos(
      Div#(m,8,mb),
      Add#(a__, 8, m)
      );

   Bit#(m) mask = ?;
   for (Integer i = 0; i < valueOf(mb); i = i + 1)
      mask[(i+1)*8-1:i*8] = strb[i] == 1 ? 8'hff : 8'h00;

   return (d_old & ~mask) | (d_new & mask);
endfunction

module mkAxi4LRegFile#(Integer size)(AXI4_Lite_Slave_IFC#(Wd_Addr, Wd_Data, Wd_User));

   staticAssert(size == 128, "mkAxi4LRegFile supports any value of 'size' that is equal to 128");

   RegFile#(Bit#(TDiv#(128,Wd_Data)), Bit#(Wd_Data)) rf <- mkRegFileFull;
   AXI4_Lite_Slave_Xactor_IFC#(Wd_Addr, Wd_Data, Wd_User) xactor <- mkAXI4_Lite_Slave_Xactor;

   Bit#(Wd_Addr) addr_shift = fromInteger(valueOf(TLog#(Wd_Data)));

   FIFOF#(AXI4_Lite_Wr_Addr#(Wd_Addr, Wd_User)) f_aw <- mkFIFOF;
   FIFOF#(AXI4_Lite_Wr_Data#(Wd_Data)) f_w <- mkFIFOF;
   FIFOF#(AXI4_Lite_Wr_Resp#(Wd_User)) f_b <- mkFIFOF;

   FIFOF#(AXI4_Lite_Rd_Addr#(Wd_Addr, Wd_User)) f_ar <- mkFIFOF;
   FIFOF#(AXI4_Lite_Rd_Data#(Wd_Data, Wd_User)) f_r <- mkFIFOF;

   mkConnection(xactor.o_wr_addr, f_aw);
   mkConnection(xactor.o_wr_data, f_w);
   mkConnection(f_b, xactor.i_wr_resp);

   mkConnection(xactor.o_rd_addr, f_ar);
   mkConnection(f_r, xactor.i_rd_data);

   rule write;
      let aw = f_aw.first;
      let w = f_w.first;
      f_aw.deq;
      f_w.deq;
      rf.upd(truncate(aw.awaddr >> addr_shift),
	     update_data(w.wstrb, rf.sub(truncate(aw.awaddr >> addr_shift)), w.wdata));
      f_b.enq(AXI4_Lite_Wr_Resp {
	 bresp: AXI4_LITE_OKAY,
	 buser: aw.awuser
	 });
   endrule

   rule read;
      let ar = f_ar.first;
      f_ar.deq;
      f_r.enq(AXI4_Lite_Rd_Data {
	 rresp: AXI4_LITE_OKAY,
	 rdata: rf.sub(truncate(ar.araddr >> addr_shift)),
	 ruser: ar.aruser
	 });
   endrule

   return xactor.axi_side;
endmodule

endpackage
