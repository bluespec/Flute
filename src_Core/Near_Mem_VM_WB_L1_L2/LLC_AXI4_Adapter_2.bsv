// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package LLC_AXI4_Adapter_2;

// ================================================================
// This adapter converts the mem-side interface of the LLC to an AXI4
// interface, with data transferred at full line width (so, 1 beat, no bursts).

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

   // Inform core that DDR4 has been initialized and is ready to accept requests
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

   // Verbosity: 0: quiet; 1: rules
   Integer verbosity = 0;

   // ================================================================
   // Fabric request/response

   AXI4_Master_Xactor_IFC #(Wd_Id_Mem,
			    Wd_Addr_Mem,
			    Wd_Data_Mem,
			    Wd_User_Mem) master_xactor <- mkAXI4_Master_Xactor_2;

   // For discarding write-responses
   CreditCounter_IFC #(4) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

   Reg #(Bool) rg_ddr4_ready <- mkReg (False);
   Reg #(Bool) rg_AXI4_error <- mkReg (False);

   // ================================================================
   // Handle read requests and responses
   // Don't do reads while writes are outstanding.

   FIFOF #(LdMemRq #(idT, childT)) f_pending_reads <- mkFIFOF;

   rule rl_handle_read_req (llc.toM.first matches tagged Ld .ld
			    &&& (ctr_wr_rsps_pending.value == 0)
			    &&& rg_ddr4_ready);
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_handle_read_req", cur_cycle);
	 $display ("    ", fshow (ld));
      end

      llc.toM.deq;
      AXI4_Rd_Addr #(Wd_Id_Mem, Wd_Addr_Mem, Wd_User_Mem)
      mem_req_rd_addr = AXI4_Rd_Addr {arid:     0,
				      araddr:   ld.addr,
				      arlen:    0,           // burst len = arlen+1
				      arsize:   axsize_8,
				      arburst:  fabric_default_burst,
				      arlock:   fabric_default_lock,
				      arcache:  fabric_default_arcache,
				      arprot:   fabric_default_prot,
				      arqos:    fabric_default_qos,
				      arregion: fabric_default_region,
				      aruser:   fabric_default_user};
      master_xactor.i_rd_addr.enq (mem_req_rd_addr);
      f_pending_reads.enq (ld);
   endrule

   rule rl_handle_read_rsps;
      let ldreq    <- pop (f_pending_reads);
      let mem_rsp <- pop_o (master_xactor.o_rd_data);

      if (mem_rsp.rresp != axi4_resp_okay) begin
	 // TODO: need to raise a non-maskable interrupt (NMI) here
	 $display ("%0d: %m.rl_handle_read_rsp", cur_cycle);
	 $display ("    ERROR: fabric response error; exit");
	 $display ("    ", fshow (mem_rsp));
	 $finish (1);
      end

      MemRsMsg #(idT, childT) resp = MemRsMsg {data:  unpack (mem_rsp.rdata),
					       child: ldreq.child,
					       id:    ldreq.id};
      llc.rsFromM.enq (resp);

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_handle_read_rsps", cur_cycle);
	 $display ("    ", fshow (mem_rsp));
	 $display ("    -> LLC: ", fshow (resp));
      end
   endrule

   // ================================================================
   // Handle write requests and responses

   rule rl_handle_write_req (llc.toM.first matches tagged Wb .wb
			     &&& rg_ddr4_ready);
      llc.toM.deq;
      AXI4_Wr_Addr #(Wd_Id_Mem, Wd_Addr_Mem, Wd_User_Mem)
      mem_req_wr_addr = AXI4_Wr_Addr {awid:     0,
				      awaddr:   wb.addr,
				      awlen:    0,           // burst len = awlen+1
				      awsize:   axsize_8,
				      awburst:  fabric_default_burst,
				      awlock:   fabric_default_lock,
				      awcache:  fabric_default_awcache,
				      awprot:   fabric_default_prot,
				      awqos:    fabric_default_qos,
				      awregion: fabric_default_region,
				      awuser:   fabric_default_user};
      AXI4_Wr_Data #(Wd_Data_Mem, Wd_User_Mem)
      mem_req_wr_data = AXI4_Wr_Data {wdata:  pack (wb.data),
				      wstrb:  pack (wb.byteEn),
				      wlast:  True,
				      wuser:  fabric_default_user};
      master_xactor.i_wr_addr.enq (mem_req_wr_addr);
      master_xactor.i_wr_data.enq (mem_req_wr_data);

      // Expect a fabric response
      ctr_wr_rsps_pending.incr;

      if (verbosity >= 1) begin
	 $display ("%d: %m.rl_handle_write_req: Wb request from LLC to memory:", cur_cycle);
	 $display ("%d: LLC_AXI4_Adapter.rl_handle_write_req: Wb request from LLC to memory:", cur_cycle);
	 $display ("    ", fshow (wb));
      end
   endrule

   // ----------------
   // Discard write-responses from the fabric (LLC does not expect write-responses)

   rule rl_discard_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_discard_write_rsp", cur_cycle);
	 $display ("    ", fshow (wr_resp));
      end

      if (ctr_wr_rsps_pending.value == 0) begin
	 rg_AXI4_error <= True;
	 $display ("%0d: %m.rl_discard_write_rsp", cur_cycle);
	 $display ("    INTERNAL ERROR: unexpected Wr response (ctr_wr_rsps_pending.value == 0)");
	 $display ("    ", fshow (wr_resp));
	 $finish (1);
	 // TODO: need to raise a non-maskable interrupt (NMI) here?
      end

      ctr_wr_rsps_pending.decr;

      if (wr_resp.bresp != axi4_resp_okay) begin
	 rg_AXI4_error <= True;
	 $display ("%0d: %m.rl_discard_write_rsp", cur_cycle);
	 $display ("    ERROR: fabric response error: exit");
	 $display ("    ", fshow (wr_resp));
	 $finish (1);
	 // TODO: need to raise a non-maskable interrupt (NMI) here?
      end
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

   // Inform core that DDR4 has been initialized and is ready to accept requests
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
