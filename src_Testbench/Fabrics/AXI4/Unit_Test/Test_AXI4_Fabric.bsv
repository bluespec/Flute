/*
  Unit test for AXI4_Fabric
 
  This test attempts to recreate traffic patterns that were observed
  to cause data corruption in AWSteria.
 
  The fabric is instantiated with 3 master and 4 slave ports, as in
  AWSteria.  One slave is connected to a DDR model, the rest are
  unconnected and unused.  Two master ports connect to master A and
  master B modules, defined here.  The third master port is
  unconnected and unused.
 
  Master A behaves like the host in AWSteria.  For the tests here, it
  will read 32-bits from a single memory location (not overlapping
  with master B), bitwise invert the value, and write it back to the
  same location.
 
  Master B behaves like uncached access from the processor in
  AWSteria.  Across a 4kiB region of memory, it will perform the
  following, all as 32-bit accesses: (1) write zeros, (2) write
  sequential integers, and (3) read back check the integers.
 
  Various aspects of the behavior of the masters and slave modules may
  be adjusted.  See the struct TestParams.

  There are 5 test cases defined.  Supply '-D TESTCASE=<n>' to bsc
  to select which to compile.

*/

package Test_AXI4_Fabric;

import Assert ::*;
import BUtils ::*;
import Connectable ::*;
import FIFOF ::*;
import LFSR ::*;
import StmtFSM ::*;

import AXI4_Deburster ::*;
import AXI4_Fabric ::*;
import AXI4_Types ::*;
import AXI4_Widener ::*;
import DDR_Model ::*;
import Semi_FIFOF ::*;

function Action write_word_addr(AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor, Bit#(naddr) addr)
   provisos(
      Mul#(nbytes,8,ndata),
      // per bsc
      Mul#(32, a__, ndata)
      );
   action
      AXI4_Wr_Addr#(nid, naddr, nuser) wrreq = AXI4_Wr_Addr {
	 awid: 0,
	 awaddr: addr,
	 awlen: 0,
	 awsize: axsize_8,
	 awburst: 0,
	 awlock: 0,
	 awcache: 0,
	 awprot: 0,
	 awqos: 0,
	 awregion: 0,
	 awuser: 0
	 };

      xactor.i_wr_addr.enq(wrreq);
   endaction
endfunction

function Action write_word_data(AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor, Bit#(naddr) addr, Bit#(32) data)
   provisos(
      Mul#(nbytes,8,ndata),
      Add#(strbmask,1,nbytes),
      // per bsc
      Mul#(32, a__, ndata)
      );
   action
      AXI4_Wr_Data#(ndata, nuser) wrdata = AXI4_Wr_Data {
	 wdata: duplicate(data),
	 wstrb: 'hf << (addr & fromInteger(valueof(strbmask))),
	 wlast: True,
	 wuser: 0
	 };

      xactor.i_wr_data.enq(wrdata);
   endaction
endfunction

function Action write_word(AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor, Bit#(naddr) addr, Bit#(32) data)
   provisos(
      Mul#(nbytes,8,ndata),
      // per bsc
      Mul#(32, a__, ndata),
      Add#(b__, 1, nbytes)
      );
   action
      write_word_addr(xactor, addr);
      write_word_data(xactor, addr, data);
   endaction
endfunction

function Action write_word_resp(AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor);
   action
      xactor.o_wr_resp.deq;
   endaction
endfunction

function Action read_word_addr(AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor, Bit#(naddr) addr)
   provisos(
      Mul#(nbytes,8,ndata),
      // per bsc
      Mul#(32, a__, ndata)
      );
   action
      AXI4_Rd_Addr#(nid, naddr, nuser) rdreq = AXI4_Rd_Addr {
	 arid: 0,
	 araddr: addr,
	 arlen: 0,
	 arsize: axsize_8,
	 arburst: 0,
	 arlock: 0,
	 arcache: 0,
	 arprot: 0,
	 arqos: 0,
	 arregion: 0,
	 aruser: 0
	 };

      xactor.i_rd_addr.enq(rdreq);
   endaction
endfunction

function ActionValue#(Bit#(32)) read_word_data(AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor, Bit#(naddr) addr)
   provisos(
      Mul#(nbytes,8,ndata),
      // per bsc
      Mul#(32, a__, ndata)
      );
   actionvalue
      let rddata = xactor.o_rd_data.first;
      xactor.o_rd_data.deq;
      Bit#(naddr) addrmask = fromInteger(valueOf(nbytes)) - 1;
      return cExtend(rddata.rdata >> ((addr & addrmask) * 8));
   endactionvalue
endfunction

module mkLatencyFIFO#(Integer cycles)(FIFOF#(t))
   provisos(Bits#(t, a__));

   Bit#(32) bcycles = fromInteger(cycles);

   staticAssert((bcycles >= 0) && (bcycles <= 256), "cycles must be between 0 and 256, inclusive");
   staticAssert((bcycles == 0) || ((bcycles & (bcycles - 1)) == 0), "non-zero cycles must be a power of two");

   Bit#(32) mask = bcycles - 1;
   Reg#(Maybe#(Bit#(32))) count <- mkReg(tagged Invalid);
   let lfsr <- mkLFSR_8;

   FIFOF#(t) f_in <- mkLFIFOF;
   FIFOF#(t) f_out <- mkLFIFOF;

   if (bcycles == 0)
      mkConnection(to_FIFOF_O(f_in), to_FIFOF_I(f_out));
   else begin
      rule rl_start(count matches tagged Invalid &&& f_in.notEmpty);
	 count <= tagged Valid (cExtend(lfsr.value) & mask);
	 lfsr.next;
      endrule

      rule rl_count(count matches tagged Valid .x &&& x != 0);
	 count <= tagged Valid (x - 1);
      endrule

      rule rl_forward(count matches tagged Valid .x &&& x == 0);
	 count <= tagged Invalid;
	 f_out.enq(f_in.first);
	 f_in.deq;
      endrule
   end

   method enq = f_in.enq;
   method deq = f_out.deq;
   method first = f_out.first;
   method notFull = f_in.notFull;
   method notEmpty = f_out.notEmpty;
   method Action clear();
      action
	 f_in.clear;
	 f_out.clear;
	 count <= tagged Invalid;
      endaction
   endmethod
endmodule

module mkAXI4LatencyInjection#(Integer req_cycles, Integer resp_cycles)
   (Tuple2#(
      AXI4_Master_IFC #(nid, naddr, ndata, nuser),
      AXI4_Slave_IFC #(nid, naddr, ndata, nuser)));

   AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) master <- mkAXI4_Master_Xactor;
   AXI4_Slave_Xactor_IFC#(nid, naddr, ndata, nuser) slave <- mkAXI4_Slave_Xactor;

   FIFOF#(AXI4_Wr_Addr#(nid, naddr, nuser)) f_wr_addr <- mkLatencyFIFO(req_cycles);
   FIFOF#(AXI4_Wr_Data#(ndata, nuser)) f_wr_data <- mkLatencyFIFO(req_cycles);
   FIFOF#(AXI4_Wr_Resp#(nid, nuser)) f_wr_resp <- mkLatencyFIFO(resp_cycles);

   FIFOF#(AXI4_Rd_Addr#(nid, naddr, nuser)) f_rd_addr <- mkLatencyFIFO(req_cycles);
   FIFOF#(AXI4_Rd_Data#(nid, ndata, nuser)) f_rd_data <- mkLatencyFIFO(resp_cycles);

   mkConnection(master.i_wr_addr, to_FIFOF_O(f_wr_addr));
   mkConnection(to_FIFOF_I(f_wr_addr), slave.o_wr_addr);

   mkConnection(master.i_wr_data, to_FIFOF_O(f_wr_data));
   mkConnection(to_FIFOF_I(f_wr_data), slave.o_wr_data);

   mkConnection(master.o_wr_resp, to_FIFOF_I(f_wr_resp));
   mkConnection(to_FIFOF_O(f_wr_resp), slave.i_wr_resp);

   mkConnection(master.i_rd_addr, to_FIFOF_O(f_rd_addr));
   mkConnection(to_FIFOF_I(f_rd_addr), slave.o_rd_addr);

   mkConnection(master.o_rd_data, to_FIFOF_I(f_rd_data));
   mkConnection(to_FIFOF_O(f_rd_data), slave.i_rd_data);

   return tuple2(master.axi_side, slave.axi_side);
endmodule

// continually read and write address 0x1000
module mkMasterA(AXI4_Master_IFC#(nid, naddr, ndata, nuser))
   provisos(
      // per bsc
      Add#(a__, 1, b__),
      Mul#(b__, 8, ndata),
      Mul#(32, c__, ndata)
   );
   AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor <- mkAXI4_Master_Xactor;

   Reg#(Bit#(32)) iter <- mkReg(1);
   Reg#(Bit#(32)) data <- mkRegU;
   Reg#(Bit#(4)) r <- mkRegU;
   let lfsr <- mkLFSR_4;

   Stmt s =
   seq
      write_word(xactor, 'h1000, 'haaaaaaaa);
      write_word_resp(xactor);

      while (True) seq
	 if (iter % 1000 == 0)
	    $display("A %d", iter);
	 iter <= iter + 1;

	 read_word_addr(xactor, 'h0004);
	 action
	    let d <- read_word_data(xactor, 'h0004);
	 endaction

	 read_word_addr(xactor, 'h1000);
	 action
	    let d <- read_word_data(xactor, 'h1000);
	    data <= d;
	 endaction

	 write_word(xactor, 'h1000, ~data);
	 write_word_resp(xactor);

	 action
	    lfsr.next;
	    r <= cExtend(lfsr.value);
	 endaction
	 while (r > 0)
	    r <= r - 1;
      endseq
   endseq;

   let fsm <- mkAutoFSM(s);

   return xactor.axi_side;
endmodule

module mkMasterA_Parallel(AXI4_Master_IFC#(nid, naddr, ndata, nuser))
   provisos(
      // per bsc
      Add#(a__, 1, b__),
      Mul#(b__, 8, ndata),
      Mul#(32, c__, ndata)
   );
   AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor <- mkAXI4_Master_Xactor;

   Reg#(Bit#(32)) iter <- mkReg(1);
   Reg#(Bit#(32)) data <- mkRegU;
   Reg#(Bit#(4)) r <- mkRegU;
   let lfsr <- mkLFSR_4;

   Stmt s =
   seq
      write_word(xactor, 'h1000, 'haaaaaaaa);
      write_word_resp(xactor);

      par
	 while (True) seq
	    if (iter % 1000 == 0)
	       $display("A %d", iter);
	    iter <= iter + 1;

	    read_word_addr(xactor, 'h1000);
	    action
	       let d <- read_word_data(xactor, 'h1000);
	       data <= d;
	    endaction

	    action
	       lfsr.next;
	       r <= cExtend(lfsr.value);
	    endaction
	    while (r > 0)
	       r <= r - 1;
	 endseq

	 while (True) seq
	    write_word(xactor, 'h1000, ~data);
	 endseq

	 while (True) seq
	    write_word_resp(xactor);
	 endseq
      endpar
   endseq;

   let fsm <- mkAutoFSM(s);

   return xactor.axi_side;
endmodule

module mkMasterA_SplitWrite(AXI4_Master_IFC#(nid, naddr, ndata, nuser))
   provisos(
      // per bsc
      Add#(a__, 1, b__),
      Mul#(b__, 8, ndata),
      Mul#(32, c__, ndata)
   );
   AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor <- mkAXI4_Master_Xactor;

   Reg#(Bit#(32)) iter <- mkReg(1);
   Reg#(Bit#(32)) data <- mkRegU;
   Reg#(Bit#(4)) r <- mkRegU;
   let lfsr <- mkLFSR_4;

   Stmt s =
   seq
      write_word_addr(xactor, 'h1000);
      write_word_data(xactor, 'h1000, 'haaaaaaaa);
      write_word_resp(xactor);

      while (True) seq
	 if (iter % 1000 == 0)
	    $display("A %d", iter);
	 iter <= iter + 1;

	 read_word_addr(xactor, 'h0004);
	 action
	    let d <- read_word_data(xactor, 'h0004);
	 endaction

	 read_word_addr(xactor, 'h1000);
	 action
	    let d <- read_word_data(xactor, 'h1000);
	    data <= d;
	 endaction

	 write_word_data(xactor, 'h1000, ~data);
	 action
	    lfsr.next;
	    r <= cExtend(lfsr.value);
	 endaction
	 while (r > 0)
	    r <= r - 1;
	 write_word_addr(xactor, 'h1000);
	 write_word_resp(xactor);

	 action
	    lfsr.next;
	    r <= cExtend(lfsr.value);
	 endaction
	 while (r > 0)
	    r <= r - 1;
      endseq
   endseq;

   let fsm <- mkAutoFSM(s);

   return xactor.axi_side;
endmodule

// sweep addresses 0 through 0xfff
//   write zero
//   write sequential integers
//   read and check integers
module mkMasterB(AXI4_Master_IFC#(nid, naddr, ndata, nuser))
   provisos(
      // per bsc
      Add#(a__, 1, b__),
      Mul#(b__, 8, ndata),
      Mul#(32, c__, ndata)
   );
   AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor <- mkAXI4_Master_Xactor;

   Reg#(Bit#(64)) data <- mkRegU;
   Reg#(Bit#(32)) i <- mkRegU;
   Reg#(Bit#(32)) j <- mkRegU;
   Reg#(Bit#(32)) iter <- mkReg(1);

   Stmt s =
   seq
      while (True) seq
	 if (iter % 10 == 0)
	    $display("B %d", iter);
	 iter <= iter + 1;

	 par
	    for (i <= 0; i < 'h400; i <= i + 1) seq
	       write_word(xactor, cExtend(i * 4), cExtend(0));
	    endseq

	    for (j <= 0; j < 'h400; j <= j + 1) seq
	       write_word_resp(xactor);
	    endseq
	 endpar

	 par
	    for (i <= 0; i < 'h400; i <= i + 1) seq
	       write_word(xactor, cExtend(i * 4), cExtend(i));
	    endseq

	    for (j <= 0; j < 'h400; j <= j + 1) seq
	       write_word_resp(xactor);
	    endseq
	 endpar

	 seq
	    for (i <= 0; i < 'h400; i <= i + 1) seq
	       read_word_addr(xactor, cExtend(i * 4));
	       action
		  let d <- read_word_data(xactor, cExtend(i * 4));
		  if (cExtend(d) != i) begin
		     $display("FAIL addr %x (!= %x)", i, d);
		     $finish;
		  end
	       endaction
	    endseq
	 endseq
      endseq
   endseq;

   let fsm <- mkAutoFSM(s);

   return xactor.axi_side;
endmodule

module mkMasterB_SplitWrite(AXI4_Master_IFC#(nid, naddr, ndata, nuser))
   provisos(
      // per bsc
      Add#(a__, 1, b__),
      Mul#(b__, 8, ndata),
      Mul#(32, c__, ndata)
   );
   AXI4_Master_Xactor_IFC#(nid, naddr, ndata, nuser) xactor <- mkAXI4_Master_Xactor;

   Reg#(Bit#(32)) iter <- mkReg(1);
   Reg#(Bit#(64)) data <- mkRegU;
   Reg#(Bit#(32)) i <- mkRegU;
   Reg#(Bit#(32)) j <- mkRegU;
   Reg#(Bit#(4)) r <- mkRegU;
   let lfsr <- mkLFSR_4;

   Stmt s =
   seq
      while (True) seq
	 if (iter % 10 == 0)
	    $display("B %d", iter);
	 iter <= iter + 1;

	 par
	    for (i <= 0; i < 'h400; i <= i + 1) seq
	       write_word_data(xactor, cExtend(i * 4), cExtend(0));
	       write_word_addr(xactor, cExtend(i * 4));
	    endseq

	    for (j <= 0; j < 'h400; j <= j + 1) seq
	       write_word_resp(xactor);
	    endseq
	 endpar

	 par
	    for (i <= 0; i < 'h400; i <= i + 1) seq
	       write_word_data(xactor, cExtend(i * 4), cExtend(i));
	       action
		  lfsr.next;
		  r <= cExtend(lfsr.value);
	       endaction
	       while (r > 0)
		  r <= r - 1;
	       write_word_addr(xactor, cExtend(i * 4));
	    endseq

	    for (j <= 0; j < 'h400; j <= j + 1) seq
	       write_word_resp(xactor);
	    endseq
	 endpar

	 seq
	    for (i <= 0; i < 'h400; i <= i + 1) seq
	       read_word_addr(xactor, cExtend(i * 4));
	       action
		  let d <- read_word_data(xactor, cExtend(i * 4));
		  if (cExtend(d) != i) begin
		     $display("FAIL addr %x (!= %x)", i, d);
		     $finish;
		  end
	       endaction
	    endseq
	 endseq
      endseq
   endseq;

   let fsm <- mkAutoFSM(s);

   return xactor.axi_side;
endmodule

typedef enum {
   Normal,
   DDR
   } MemType deriving (Bits, Eq);

typedef enum {
   Normal,
   Parallel,
   SplitWrite
   } MasterAType deriving (Bits, Eq);

typedef enum {
   Normal,
   Narrow,
   Narrow_SplitWrite
   } MasterBType deriving (Bits, Eq);

typedef struct {
   MemType mem_type;
   MasterAType master_a_type;
   MasterBType master_b_type;
   Integer slave_request_latency;
   Integer slave_response_latency;
   Integer master_request_latency;
   Integer master_response_latency;
   } TestParams deriving (Bits, Eq);

instance DefaultValue#(TestParams);
   defaultValue = TestParams {
      mem_type: DDR,
      master_a_type: Normal,
      master_b_type: Normal,
      slave_request_latency: 0,
      slave_response_latency: 0,
      master_request_latency: 0,
      master_response_latency: 0
      };
endinstance

module mkTestGenerator#(TestParams params)(Empty)
   provisos(
      NumAlias#(nid, 16),
      NumAlias#(naddr, 64),
      NumAlias#(ndata, 512),
      NumAlias#(nuser, 0),
      NumAlias#(nmaster, 3),
      NumAlias#(nslave, 4)
      );
   function fn_addr(addr);
      return tuple2(True, 0);
   endfunction

   AXI4_Fabric_IFC#(nmaster, nslave, nid, naddr, ndata, nuser) fabric <- mkAXI4_Fabric(fn_addr);

   if (params.mem_type == Normal) begin
      staticAssert(False, "normal memory not supported yet");

      //let mem <- mkMem_Model(0, 0, False, "", 0, 'h80000000, 'h80000000);
      AXI4_Deburster_IFC#(nid, naddr, ndata, nuser) deburster <- mkAXI4_Deburster;

      //mkConnection(deburster.to_slave, mem);
      mkConnection(fabric.v_to_slaves[0], deburster.from_master);
   end
   else if (params.mem_type == DDR) begin
      let mem <- mkDDR_A_Model;
      Tuple2#(
	 AXI4_Master_IFC #(nid, naddr, ndata, nuser),
	 AXI4_Slave_IFC #(nid, naddr, ndata, nuser)) mem_latency <-
            mkAXI4LatencyInjection(params.slave_request_latency, params.slave_response_latency);

      mkConnection(tpl_1(mem_latency), mem);
      mkConnection(fabric.v_to_slaves[0], tpl_2(mem_latency));
   end

   AXI4_Master_IFC#(nid, naddr, ndata, nuser) master_a = ?;
   if (params.master_a_type == Normal)
      master_a <- mkMasterA;
   else if (params.master_a_type == Parallel)
      master_a <- mkMasterA_Parallel;
   else if (params.master_a_type == SplitWrite)
      master_a <- mkMasterA_SplitWrite;
   else
      staticAssert(False, "master A type not supported");

   AXI4_Master_IFC#(nid, naddr, ndata, nuser) master_b = ?;
   AXI4_Master_IFC#(nid, naddr, 64, nuser) master_b_orig = ?;
   if (params.master_b_type == Normal)
      master_b <- mkMasterB;
   else if (params.master_b_type == Narrow) begin
      master_b_orig <- mkMasterB;
   end
   else if (params.master_b_type == Narrow_SplitWrite)
      master_b_orig <- mkMasterB_SplitWrite;
   else
      staticAssert(False, "master B type not supported");

   if (params.master_b_type != Normal) begin
      AXI4_Widener_IFC#(nid, naddr, 64, ndata, nuser) widener <- mkAXI4_Widener;
      mkConnection(master_b_orig, widener.from_master);
      master_b = widener.to_slave;
   end

   Tuple2#(
      AXI4_Master_IFC #(nid, naddr, ndata, nuser),
      AXI4_Slave_IFC #(nid, naddr, ndata, nuser)) mastera_latency <-
         mkAXI4LatencyInjection(params.master_request_latency, params.master_response_latency);

   Tuple2#(
      AXI4_Master_IFC #(nid, naddr, ndata, nuser),
      AXI4_Slave_IFC #(nid, naddr, ndata, nuser)) masterb_latency <-
         mkAXI4LatencyInjection(params.master_request_latency, params.master_response_latency);

   mkConnection(master_a, tpl_2(mastera_latency));
   mkConnection(tpl_1(mastera_latency), fabric.v_from_masters[0]);

   mkConnection(master_b, tpl_2(masterb_latency));
   mkConnection(tpl_1(masterb_latency), fabric.v_from_masters[1]);
endmodule

module mkTestCase#(Integer n)(Empty);
   TestParams params = defaultValue;

   if (n == 1) begin
      // default
   end
   else if (n == 2) begin
      params.master_b_type = Narrow;
   end
   else if (n == 3) begin
      params.master_a_type = SplitWrite;
      params.master_b_type = Narrow;
   end
   else if (n == 4) begin
      params.master_a_type = SplitWrite;
      params.master_b_type = Narrow_SplitWrite;
      params.slave_response_latency = 16;
   end
   else if (n == 5) begin
      params.master_b_type = Narrow;
      params.master_request_latency = 16;
      params.master_response_latency = 16;
      params.slave_request_latency = 16;
      params.slave_response_latency = 16;
   end
   else begin
      staticAssert(False, "invalid test case");
   end

   let _ifc <- mkTestGenerator(params);
endmodule

module mkTest(Empty);

   Integer n = 1;

`ifdef TESTCASE
   n = `TESTCASE;
`endif

   let test <- mkTestCase(n);

   Stmt s =
   seq
      $display("test %d", n);
      while (True) seq
	 action
	    let t <- $time;
	    if (t > 100000000) begin
	       $display("PASS");
	       $finish;
	    end
	 endaction
      endseq
   endseq;

   let fsm <- mkAutoFSM(s);
endmodule

endpackage
