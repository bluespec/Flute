
package TV_Extra;

import Assert ::*;
import BUtils ::*;
import ClientServer ::*;
import FIFOF ::*;
import GetPut ::*;
import Memory ::*;

import AXI4_Lite_Fabric ::*;
import AXI4_Lite_Types ::*;
import Fabric_Defs ::*;
import ISA_Decls ::*;
import Semi_FIFOF ::*;
import TV_Info ::*;

interface Memory_TV_IFC;
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) slave;
   interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) master;
   interface Get#(Info_CPU_to_Verifier) to_verifier;
endinterface

module mkMemoryTV(Memory_TV_IFC);

   AXI4_Lite_Master_Xactor_IFC#(Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Lite_Master_Xactor;
   AXI4_Lite_Slave_Xactor_IFC#(Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Lite_Slave_Xactor;
   FIFOF #(Info_CPU_to_Verifier)  f_to_verifier <- mkFIFOF;

`ifdef FABRIC64
   staticAssert(valueOf(Wd_User) == 64, "mkMemoryTV only supports Wd_User of 64");
`else
   staticAssert(valueOf(Wd_User) == 32, "mkMemoryTV only supports Wd_User of 32");
`endif

   rule request;
      rule write;
	 slave_xactor.o_wr_addr.deq;
	 slave_xactor.o_wr_data.deq;

	 master_xactor.i_wr_addr.enq(slave_xactor.o_wr_addr.first);
	 master_xactor.i_wr_data.enq(slave_xactor.o_wr_data.first);

	 Info_CPU_to_Verifier to_verifier = ?;
	 to_verifier.pc = fromInteger(pc_tv_cmd);
	 to_verifier.instr = fromInteger(tv_cmd_mem_write32);

`ifdef FABRIC64
	 if (slave_xactor.o_wr_data.first.wstrb == 'h0f) begin
	    to_verifier.addr = cExtend(slave_xactor.o_wr_addr.first.awaddr);
	    to_verifier.data1 = truncate(slave_xactor.o_wr_data.first.wdata);
	 end
	 else if (slave_xactor.o_wr_data.first.wstrb == 'hf0) begin
	    to_verifier.addr = cExtend(slave_xactor.o_wr_addr.first.awaddr + 4);
	    to_verifier.data1 = truncateLSB(slave_xactor.o_wr_data.first.wdata);
	 end
	 else
	    dynamicAssert(False, "mkMemoryTV: unsupported byte enables");
`else
	 to_verifier.addr = slave_xactor.o_wr_addr.first.awaddr;
	 to_verifier.data1 = slave_xactor.o_wr_data.first.wdata;
`endif

	 f_to_verifier.enq(to_verifier);
      endrule

      rule read;
	 slave_xactor.o_rd_addr.deq;
	 master_xactor.i_rd_addr.enq(slave_xactor.o_rd_addr.first);
      endrule
   endrule

   rule response;
      rule write;
	 master_xactor.o_wr_resp.deq;
	 slave_xactor.i_wr_resp.enq(master_xactor.o_wr_resp.first);
      endrule

      rule read;
	 master_xactor.o_rd_data.deq;
	 slave_xactor.i_rd_data.enq(master_xactor.o_rd_data.first);
      endrule
   endrule

   interface slave = slave_xactor.axi_side;
   interface master = master_xactor.axi_side;
   interface Get to_verifier = toGet(f_to_verifier);
endmodule

interface Register_TV_IFC#(numeric type a);
   interface MemoryClient#(a, XLEN) client;
   interface MemoryServer#(a, XLEN) server;
   interface Get#(Info_CPU_to_Verifier) to_verifier;
endinterface

module mkRegisterTV#(Integer offset)(Register_TV_IFC#(a));
   FIFOF#(MemoryRequest#(a, XLEN)) f_req_in <- mkFIFOF;
   FIFOF#(MemoryRequest#(a, XLEN)) f_req_out <- mkFIFOF;
   FIFOF#(MemoryResponse#(XLEN)) f_rsp <- mkFIFOF;
   FIFOF #(Info_CPU_to_Verifier)  f_to_verifier <- mkFIFOF;

   rule request;
      let req = f_req_in.first;
      f_req_in.deq;
      f_req_out.enq(req);

      if (req.write) begin
	 Info_CPU_to_Verifier to_verifier = ?;
	 to_verifier.pc = fromInteger(pc_tv_cmd);
	 to_verifier.instr = fromInteger(tv_cmd_reg_write);
	 to_verifier.addr = cExtend(req.address) + fromInteger(offset);
	 to_verifier.data1 = cExtend(req.data);
	 f_to_verifier.enq(to_verifier);
      end
   endrule

   interface MemoryClient client = toGPClient(f_req_out, f_rsp);
   interface MemoryServer server = toGPServer(f_req_in, f_rsp);
   interface Get to_verifier = toGet(f_to_verifier);
endmodule

endpackage
