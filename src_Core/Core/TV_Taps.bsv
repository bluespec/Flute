// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package TV_Taps;

// ================================================================
// This package defines 'taps' on connections between
// - DM and CPU, on which DM accesses CPU GPRs, FPRs and CSRs
// - DM and memory bus, on which DM accesses memory
// Each tap snoops 'writes', and produces a corresponsing Trace_Data
// write-memory command for the Tandem Verifier, so that it keeps its
// GPRs, FPRs, CSRs and memories in sync.

// ================================================================
// BSV library imports

import Assert        :: *;
import BUtils        :: *;
import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF  :: *;
import GetPut_Aux  :: *;

// ================================================================
// Project imports

import ISA_Decls      :: *;
import DM_CPU_Req_Rsp :: *;
import TV_Info        :: *;

import AXI4_Types   :: *;
import Fabric_Defs  :: *;

// ================================================================
// DM-to-memory tap

interface DM_Mem_Tap_IFC;
   interface AXI4_Slave_IFC  #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  slave;
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  master;
   interface Get #(Trace_Data)                                    trace_data_out;
endinterface

(* synthesize *)
module mkDM_Mem_Tap (DM_Mem_Tap_IFC);

   // Transactor facing DM
   AXI4_Slave_Xactor_IFC  #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor  <- mkAXI4_Slave_Xactor;

   // Transactor facing memory bus
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;

   // Tap output
   FIFOF #(Trace_Data)  f_trace_data <- mkFIFOF;

   // ----------------
   // AXI requests

   // Snoop write requests
   rule write_reqs;
      let wr_addr = slave_xactor.o_wr_addr.first;
      slave_xactor.o_wr_addr.deq;

      let wr_data = slave_xactor.o_wr_data.first;
      slave_xactor.o_wr_data.deq;

      // Pass-through
      master_xactor.i_wr_addr.enq (wr_addr);
      master_xactor.i_wr_data.enq (wr_data);

      // Tap
      Bit #(64) paddr = ?;
      Bit #(64) stval = ?;
`ifdef FABRIC64
      if (wr_data.wstrb == 'h0f) begin
	 paddr = zeroExtend (wr_addr.awaddr);
 	 stval = (wr_data.wdata & 'h_FFFF_FFFF);
      end
      else if (wr_data.wstrb == 'hf0) begin
	 paddr = zeroExtend (wr_addr.awaddr);
	 stval = ((wr_data.wdata >> 32) & 'h_FFFF_FFFF);
      end
      else
	 dynamicAssert(False, "mkDM_Mem_Tap: unsupported byte enables");
`else
      paddr = zeroExtend (wr_addr.awaddr);
      stval = zeroExtend (wr_data.wdata);
`endif      
      Trace_Data td = mkTrace_MEM_WRITE (f3_SIZE_W, truncate (stval), paddr);
      f_trace_data.enq (td);
   endrule

   // Read requests, write responses and read responses are not snooped
   mkConnection (slave_xactor.o_rd_addr, master_xactor.i_rd_addr);
   mkConnection (slave_xactor.i_wr_resp, master_xactor.o_wr_resp);
   mkConnection (slave_xactor.i_rd_data, master_xactor.o_rd_data);

   // ================================================================
   // INTERFACE

   // Facing DM
   interface slave  = slave_xactor.axi_side;
   // Facing bus
   interface master = master_xactor.axi_side;
   // Tap towards verifier
   interface Get trace_data_out = toGet (f_trace_data);

endmodule: mkDM_Mem_Tap

// ================================================================
// DM-to-CPU GPR tap (for writes to GPRs)

interface DM_GPR_Tap_IFC;
   interface Client #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN))  client;
   interface Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN))  server;
   interface Get #(Trace_Data)        trace_data_out;
endinterface

(* synthesize *)
module mkDM_GPR_Tap (DM_GPR_Tap_IFC);
   // req from DM
   FIFOF #(DM_CPU_Req #(5,  XLEN)) f_req_in     <- mkFIFOF;
   // req to CPU
   FIFOF #(DM_CPU_Req #(5,  XLEN)) f_req_out    <- mkFIFOF;
   // resp CPU->DM
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_rsp        <- mkFIFOF;
   // Tap to TV
   FIFOF #(Trace_Data)             f_trace_data <- mkFIFOF;

   rule request;
      let req <- pop (f_req_in);

      // Pass-through to CPU
      f_req_out.enq(req);

      // Snoop writes and send trace data to TV
      if (req.write) begin
	 Trace_Data td;
	 td = mkTrace_GPR_WRITE (req.address, req.data);
	 f_trace_data.enq (td);
      end
   endrule

   interface Client client = toGPClient (f_req_out, f_rsp);
   interface Server server = toGPServer (f_req_in,  f_rsp);

   interface Get trace_data_out = toGet (f_trace_data);
endmodule: mkDM_GPR_Tap

// ================================================================
// DM-to-CPU FPR tap (for writes to FPRs)

`ifdef ISA_F_OR_D

interface DM_FPR_Tap_IFC;
   interface Client #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) client;
   interface Server #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN)) server;
   interface Get #(Trace_Data) trace_data_out;
endinterface

(* synthesize *)
module mkDM_FPR_Tap (DM_FPR_Tap_IFC);
   // req from DM
   FIFOF #(DM_CPU_Req #(5,  XLEN)) f_req_in     <- mkFIFOF;
   // req to CPU
   FIFOF #(DM_CPU_Req #(5,  XLEN)) f_req_out    <- mkFIFOF;
   // resp CPU->DM
   FIFOF #(DM_CPU_Rsp #(XLEN))     f_rsp        <- mkFIFOF;
   // Tap to TV
   FIFOF #(Trace_Data)             f_trace_data <- mkFIFOF;

   rule request;
      let req <- pop (f_req_in);

      // Pass-through to CPU
      f_req_out.enq(req);

      // Snoop writes and send trace data to TV
      if (req.write) begin
	 Trace_Data td;
	 td = mkTrace_FPR_WRITE (req.address, req.data);
	 f_trace_data.enq (td);
      end
   endrule

   interface Client client = toGPClient (f_req_out, f_rsp);
   interface Server server = toGPServer (f_req_in,  f_rsp);

   interface Get trace_data_out = toGet (f_trace_data);
endmodule: mkDM_FPR_Tap

`endif

// ================================================================
// DM-to-CPU CSR tap (for writes to CSRs)

interface DM_CSR_Tap_IFC;
   interface Client #(DM_CPU_Req #(12,  XLEN), DM_CPU_Rsp #(XLEN)) client;
   interface Server #(DM_CPU_Req #(12,  XLEN), DM_CPU_Rsp #(XLEN)) server;
   interface Get #(Trace_Data)  trace_data_out;
endinterface

(* synthesize *)
module mkDM_CSR_Tap (DM_CSR_Tap_IFC);
   // req from DM
   FIFOF #(DM_CPU_Req #(12,  XLEN)) f_req_in     <- mkFIFOF;
   // req to CPU
   FIFOF #(DM_CPU_Req #(12,  XLEN)) f_req_out    <- mkFIFOF;
   // resp CPU->DM
   FIFOF #(DM_CPU_Rsp #(XLEN))      f_rsp        <- mkFIFOF;
   // Tap to TV
   FIFOF #(Trace_Data)              f_trace_data <- mkFIFOF;

   rule request;
      let req <- pop (f_req_in);

      // Pass-through to CPU
      f_req_out.enq(req);

      // Snoop writes and send trace data to TV
      if (req.write) begin
	 Trace_Data td = mkTrace_CSR_WRITE (req.address, req.data);
	 f_trace_data.enq (td);
      end
   endrule

   interface Client client = toGPClient (f_req_out, f_rsp);
   interface Server server = toGPServer (f_req_in,  f_rsp);

   interface Get trace_data_out = toGet (f_trace_data);
endmodule: mkDM_CSR_Tap

// ================================================================

endpackage
