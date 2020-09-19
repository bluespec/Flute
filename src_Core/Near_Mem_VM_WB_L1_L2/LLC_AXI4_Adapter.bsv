// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package LLC_AXI4_Adapter;

// ================================================================
// This adapter converts the mem-side interface of the LLC to an AXI4
// interface.

// ================================================================
// BSV lib imports

import ConfigReg :: *;
import Assert    :: *;
import FIFOF     :: *;
import Vector    :: *;

// ----------------
// BSV additional libs

import GetPut_Aux     :: *;
import Cur_Cycle      :: *;
import Semi_FIFOF     :: *;
import CreditCounter  :: *;

// ================================================================
// Project imports

// ----------------
// From MIT RISCY-OOO

import Types       :: *;
import CacheUtils  :: *;
import CCTypes     :: *;

// ----------------
// From Bluespec RISC-V CPUs

import AXI4_Types   :: *;
import Fabric_Defs  :: *;
import Near_Mem_IFC :: *;

// ================================================================

interface LLC_AXI4_Adapter_IFC;
   method Action reset;

   // Fabric master interface for memory
   interface Near_Mem_Fabric_IFC  mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;

endinterface

// ================================================================

module mkLLC_AXi4_Adapter #(MemFifoClient #(idT, childT) llc)
                          (LLC_AXI4_Adapter_IFC)
   provisos(Bits#(idT, a__),
	    Bits#(childT, b__),
	    FShow#(ToMemMsg#(idT, childT)),
	    FShow#(MemRsMsg#(idT, childT)),
	    Add#(SizeOf#(Line), 0, 512)); // assert Line sz = 512

   // Verbosity: 0: quiet; 2: LLC transactions; 3: loop detail
   Integer verbosity = 0;

   // ================================================================
   // Fabric request/response

   AXI4_Master_Xactor_IFC #(Wd_Id_Mem, Wd_Addr_Mem, Wd_Data_Mem, Wd_User_Mem)
   master_xactor <- mkAXI4_Master_Xactor_2;

   // For discarding write-responses
   CreditCounter_IFC #(4) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

   Reg #(Bool) rg_ddr4_ready <- mkReg (False);
   Reg #(Bool) rg_AXI4_error <- mkReg (False);

   // ================================================================
   // Functions to interact with the fabric

   // Send a read-request into the fabric
   function Action fa_fabric_send_read_req (Fabric_Addr  addr);
      action
	 AXI4_Size  size = axsize_8;
	 let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					     araddr:   addr,
					     arlen:    0,           // burst len = arlen+1
					     arsize:   size,
					     arburst:  fabric_default_burst,
					     arlock:   fabric_default_lock,
					     arcache:  fabric_default_arcache,
					     arprot:   fabric_default_prot,
					     arqos:    fabric_default_qos,
					     arregion: fabric_default_region,
					     aruser:   fabric_default_user};

	 master_xactor.i_rd_addr.enq (mem_req_rd_addr);

	 // Debugging
	 if (verbosity >= 3) begin
	    $display ("    ", fshow (mem_req_rd_addr));
	 end
      endaction
   endfunction

   // Send a write-request into the fabric
   function Action fa_fabric_send_write_req (Fabric_Addr  addr, Fabric_Strb  strb, Bit #(64)  st_val);
      action
	 AXI4_Size  size = axsize_8;
	 let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
					     awaddr:   addr,
					     awlen:    0,           // burst len = awlen+1
					     awsize:   size,
					     awburst:  fabric_default_burst,
					     awlock:   fabric_default_lock,
					     awcache:  fabric_default_awcache,
					     awprot:   fabric_default_prot,
					     awqos:    fabric_default_qos,
					     awregion: fabric_default_region,
					     awuser:   fabric_default_user};

	 let mem_req_wr_data = AXI4_Wr_Data {wdata:  st_val,
					     wstrb:  strb,
					     wlast:  True,
					     wuser:  fabric_default_user};

	 master_xactor.i_wr_addr.enq (mem_req_wr_addr);
	 master_xactor.i_wr_data.enq (mem_req_wr_data);

	 // Expect a fabric response
	 ctr_wr_rsps_pending.incr;

	 // Debugging
	 if (verbosity >= 3) begin
	    $display ("            To fabric: ", fshow (mem_req_wr_addr));
	    $display ("                       ", fshow (mem_req_wr_data));
	 end
      endaction
   endfunction

   // ================================================================
   // Handle read requests and responses
   // Don't do reads while writes are outstanding.

   // Each 512b cache line takes 8 beats, each handling 64 bits
   Reg #(Bit #(3)) rg_rd_req_beat <- mkReg (0);
   Reg #(Bit #(3)) rg_rd_rsp_beat <- mkReg (0);

   FIFOF #(LdMemRq #(idT, childT)) f_pending_reads <- mkFIFOF;
   Reg #(Bit #(512)) rg_cline <- mkRegU;

   rule rl_handle_read_req (llc.toM.first matches tagged Ld .ld
			    &&& (ctr_wr_rsps_pending.value == 0)
			    &&& rg_ddr4_ready);
      if ((verbosity >= 2) && (rg_rd_req_beat == 0)) begin
	 $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_req: Ld request from LLC to memory: beat %0d",
		   cur_cycle, rg_rd_req_beat);
	 $display ("    ", fshow (ld));
      end

      Addr  line_addr = { ld.addr [63:6], 6'h0 };                      // Addr of containing cache line
      Addr  offset    = zeroExtend ( { rg_rd_req_beat, 3'b_000 } );    // Addr offset of 64b word for this beat
      fa_fabric_send_read_req (line_addr | offset);

      if (rg_rd_req_beat == 0)
	 f_pending_reads.enq (ld);

      if (rg_rd_req_beat == 7)
	 llc.toM.deq;

      rg_rd_req_beat <= rg_rd_req_beat + 1;
   endrule

   rule rl_handle_read_rsps;
      let  mem_rsp <- pop_o (master_xactor.o_rd_data);

      if (verbosity >= 2) begin
	 $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_rsps: beat %0d ", cur_cycle, rg_rd_rsp_beat);
	 $display ("    ", fshow (mem_rsp));
      end

      if (mem_rsp.rresp != axi4_resp_okay) begin
	 // TODO: need to raise a non-maskable interrupt (NMI) here
	 $display ("%0d: LLC_AXI4_Adapter.rl_handle_read_rsp: fabric response error; exit", cur_cycle);
	 $display ("    ", fshow (mem_rsp));
	 $finish (1);
      end

      // Shift next 64 bits from fabric into the cache line being assembled
      let new_cline = { mem_rsp.rdata, rg_cline [511:64] };

      if (rg_rd_rsp_beat == 7) begin
	 let ldreq <- pop (f_pending_reads);
	 MemRsMsg #(idT, childT) resp = MemRsMsg {data:  unpack (new_cline),
						  child: ldreq.child,
						  id:    ldreq.id};

	 llc.rsFromM.enq (resp);

	 if (verbosity >= 2)
	    $display ("    Response to LLC: ", fshow (resp));
      end

      rg_cline <= new_cline;
      rg_rd_rsp_beat <= rg_rd_rsp_beat + 1;
   endrule

   // ================================================================
   // Handle write requests and responses

   // Each 512b cache line takes 8 beats, each handling 64 bits
   Reg #(Bit #(3)) rg_wr_req_beat <- mkReg (0);
   Reg #(Bit #(3)) rg_wr_rsp_beat <- mkReg (0);

   FIFOF #(WbMemRs) f_pending_writes <- mkFIFOF;

   rule rl_handle_write_req (llc.toM.first matches tagged Wb .wb
			     &&& rg_ddr4_ready);
      if ((verbosity >= 2) && (rg_wr_req_beat == 0)) begin
	 $display ("%d: LLC_AXI4_Adapter.rl_handle_write_req: Wb request from LLC to memory:", cur_cycle);
	 $display ("    ", fshow (wb));
      end

      Addr       line_addr = { wb.addr [63:6], 6'h0 };    // Addr of containing cache line
      Line       line_data = wb.data;
      Vector #(8, Bit #(8)) line_bes = unpack (pack (wb.byteEn));

      Addr  offset = zeroExtend ( { rg_wr_req_beat, 3'b_000 } );    // Addr offset of 64b word for this beat
      Bit #(64)  data64 = line_data [rg_wr_req_beat];
      Bit #(8)   strb8  = line_bes  [rg_wr_req_beat];
      fa_fabric_send_write_req (line_addr | offset, strb8, data64);

      if (rg_wr_req_beat == 0)
	 f_pending_writes.enq (wb);

      if (rg_wr_req_beat == 7)
	 llc.toM.deq;

      rg_wr_req_beat <= rg_wr_req_beat + 1;
   endrule

   // ----------------
   // Discard write-responses from the fabric (LLC does not expect write-responses)

   rule rl_discard_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      if (verbosity >= 2) begin
	 $display ("%0d: LLC_AXI4_Adapter.rl_discard_write_rsp: beat %0d ", cur_cycle, rg_wr_rsp_beat);
	 $display ("    ", fshow (wr_resp));
      end

      if (ctr_wr_rsps_pending.value == 0) begin
	 rg_AXI4_error <= True;
	 $display ("%0d: %m.LLC_AXI4_Adapter.rl_discard_write_rsp", cur_cycle);
	 $display ("    INTERNAL ERROR: unexpected Wr response (ctr_wr_rsps_pending.value == 0)");
	 $display ("    ", fshow (wr_resp));
	 $finish (1);
	 // TODO: need to raise a non-maskable interrupt (NMI) here?
      end

      ctr_wr_rsps_pending.decr;

      if (wr_resp.bresp != axi4_resp_okay) begin
	 rg_AXI4_error <= True;
	 $display ("%0d: %m.LLC_AXI4_Adapter.rl_discard_write_rsp", cur_cycle);
	 $display ("    ERROR: fabric response error: exit");
	 $display ("    ", fshow (wr_resp));
	 $finish (1);
	 // TODO: need to raise a non-maskable interrupt (NMI) here?
      end

      if (rg_wr_rsp_beat == 7) begin
	 let wrreq <- pop (f_pending_writes);
	 // LLC does not expect any response for writes
      end

      rg_wr_rsp_beat <= rg_wr_rsp_beat + 1;
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      ctr_wr_rsps_pending.clear;
   endmethod

   // Fabric interface for memory
   interface mem_master = master_xactor.axi_side;

   // ----------------------------------------------------------------
   // Misc. control and status

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;
      rg_ddr4_ready <= True;
      $display ("%0d: %m.LLC_AXI4_Adapter.ma_ddr4_ready: enabling all rules", cur_cycle);
   endmethod

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status;
      return (rg_AXI4_error ? 1 : 0);
   endmethod

endmodule

// ================================================================

endpackage
