// Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved

package Test_PLIC;

// ================================================================
// Standalone unit-test testbench for PLIC.
//
// ================================================================
// Bluespec lib imports

import  ConfigReg    :: *;
import  Vector       :: *;
import  FIFOF        :: *;
import  GetPut       :: *;
import  ClientServer :: *;
import  Connectable  :: *;
import  Assert       :: *;
import  StmtFSM      :: *;

// ----------------
// BSV additional libs

import  Cur_Cycle  :: *;
import  GetPut_Aux :: *;
import  Semi_FIFOF :: *;

// ================================================================
// Project imports

import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;    // for Wd_Id, Wd_Addr, Wd_Data, Wd_User
import SoC_Map      :: *;
import PLIC         :: *;
import PLIC_16_2_7  :: *;

// ================================================================

Integer n_external_interrupt_sources = valueOf (N_External_Interrupt_Sources);
typedef TAdd #(1, N_External_Interrupt_Sources) N_Interrupt_Sources;
typedef Bit #(TLog #(N_Interrupt_Sources))  Source_Id;

Integer plic_n_targets               = valueOf (PLIC_N_Targets);
Integer target_0 = 0;
Integer target_1 = 1;

Integer plic_max_priority            = valueOf (PLIC_Max_Priority);
typedef Bit #(TLog #(TAdd #(1, PLIC_Max_Priority)))  Priority;

// ================================================================
// The Core module

(* synthesize *)
module mkTest_PLIC (Empty);

   // System address map
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // PLIC (Platform-Level Interrupt Controller)
   PLIC_IFC_16_2_7  plic <- mkPLIC_16_2_7;

   // Master transactor through which to read/write PLIC regs
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;

   Vector #(N_Interrupt_Sources, Reg #(Bool)) vrg_irqs <- replicateM (mkReg (False));

   // ================================================================
   // AXI4 interactions

   FIFOF #(Fabric_Addr) f_read_addr <- mkFIFOF;
   FIFOF #(Fabric_Data) f_read_data <- mkFIFOF;

   function Action fa_read_req (Integer addr);
      action
	 let fabric_addr = soc_map.m_plic_addr_base + fromInteger (addr);

	 let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					     araddr:   fabric_addr,
					     arlen:    0,           // burst len = arlen+1
					     arsize:   zeroExtend (axsize_4),
					     arburst:  fabric_default_burst,
					     arlock:   fabric_default_lock,
					     arcache:  fabric_default_arcache,
					     arprot:   fabric_default_prot,
					     arqos:    fabric_default_qos,
					     arregion: fabric_default_region,
					     aruser:   fabric_default_user};
	 master_xactor.i_rd_addr.enq (mem_req_rd_addr);
	 f_read_addr.enq (fabric_addr);
      endaction
   endfunction

   function Action fa_read_rsp;
      action
	 let fabric_addr <- pop (f_read_addr);
	 let rd_data <- pop_o (master_xactor.o_rd_data);
	 if (rd_data.rresp != axi4_resp_okay) begin
	    $display ("ERROR: fa_read_rsp: fabric response error");
	    $display ("    ", fshow (rd_data));
	 end
	 let x = (((valueOf (Wd_Data) == 64) && ((fabric_addr & 'h7) == 'h4))
		  ? (rd_data.rdata >> 32)
		  : rd_data.rdata);
	 f_read_data.enq (x);
      endaction
   endfunction

   function Action fa_write_req (Integer addr, Fabric_Data data);
      action
	 let fabric_addr = soc_map.m_plic_addr_base + fromInteger (addr);

	 let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
					     awaddr:   fabric_addr,
					     awlen:    0,           // burst len = awlen+1
					     awsize:   zeroExtend (axsize_4),
					     awburst:  fabric_default_burst,
					     awlock:   fabric_default_lock,
					     awcache:  fabric_default_awcache,
					     awprot:   fabric_default_prot,
					     awqos:    fabric_default_qos,
					     awregion: fabric_default_region,
					     awuser:   fabric_default_user};

	 let x = (((valueOf (Wd_Data) == 64) && ((fabric_addr & 'h7) == 'h4))
		  ? (data << 32)
		  : data);
	 let mem_req_wr_data = AXI4_Wr_Data {wid:    fabric_default_id,
					     wdata:  x,
					     wstrb:  '1,
					     wlast:  True,
					     wuser:  fabric_default_user};
	 master_xactor.i_wr_addr.enq (mem_req_wr_addr);
	 master_xactor.i_wr_data.enq (mem_req_wr_data);
      endaction
   endfunction

   function Action fa_write_rsp;
      action
	 let wr_resp <- pop_o (master_xactor.o_wr_resp);

	 if (wr_resp.bresp != axi4_resp_okay) begin
	    $display ("ERROR: rl_discard_write_rsp: fabric response error");
	    $display ("    ", fshow (wr_resp));
	 end
      endaction
   endfunction

   // ================================================================
   // Help functions to interact with PLIC at an "API" level

   function Action fa_print_plic_eips ();
      action
	 $write ("PLIC.v_target  eip =");
	 $write (" ", fshow (plic.v_targets [0].m_eip));
	 $write (" ", fshow (plic.v_targets [1].m_eip));
	 $display ("");
      endaction
   endfunction

   function Stmt fstmt_set_source_priority (Integer src, Priority prio);
      return seq
		fa_write_req (src * 4, zeroExtend (prio));
		fa_write_rsp;
	     endseq;
   endfunction

   function Stmt fstmt_set_target_ies (Integer target, Bit #(32) ies);
      return seq
		fa_write_req ('h2000 + (target * 'h80), zeroExtend (ies));
		fa_write_rsp;
	     endseq;
   endfunction

   function Stmt fstmt_set_target_threshold (Integer target, Priority threshold);
      return seq
		fa_write_req ('h20_0000 + (target * 'h1000), zeroExtend (threshold));
		fa_write_rsp;
	     endseq;
   endfunction

   function Stmt fstmt_claim (Integer target);
      return seq
		fa_read_req ('h20_0004 + (target * 'h1000));
		fa_read_rsp;
		action
		   let x <- pop (f_read_data);
		   $display ("fstmt_claim: PLIC returned %0d", x);
		endaction
	     endseq;
   endfunction

   function Stmt fstmt_complete (Integer target, Source_Id source_id);
      return seq
		fa_write_req ('h20_0004 + (target * 'h1000), zeroExtend (source_id));
		fa_write_rsp;
	     endseq;
   endfunction

   // ================================================================
   // BEHAVIOR

   mkConnection (master_xactor.axi_side,  plic.axi4_slave);

   // Drive all interrupt requests from local regs
   for (Integer j = 0; j < n_external_interrupt_sources; j = j + 1)
      rule rl_drive_irq;
	 plic.v_sources [j].m_interrupt_req (vrg_irqs [j]);
      endrule

   // ================================================================

   Stmt init = seq
		  action
		     $display ("Initializing PLIC");
		     plic.server_reset.request.put (?);
		  endaction
		  action
		     let rsp <- plic.server_reset.response.get;
		     plic.set_addr_map (zeroExtend (soc_map.m_plic_addr_base),
				       zeroExtend (soc_map.m_plic_addr_lim));
		  endaction
		  fstmt_set_source_priority (1,  0);
		  fstmt_set_source_priority (2,  0);
		  fstmt_set_source_priority (3,  0);
		  fstmt_set_source_priority (4,  0);

		  fstmt_set_source_priority (5,  0);
		  fstmt_set_source_priority (6,  0);
		  fstmt_set_source_priority (7,  0);
		  fstmt_set_source_priority (8,  0);

		  fstmt_set_source_priority (9,  0);
		  fstmt_set_source_priority (10, 0);
		  fstmt_set_source_priority (11, 0);
		  fstmt_set_source_priority (12, 0);

		  fstmt_set_source_priority (13, 0);
		  fstmt_set_source_priority (14, 0);
		  fstmt_set_source_priority (15, 0);
		  fstmt_set_source_priority (16, 0);

		  fstmt_set_target_ies (target_0, 0);
		  fstmt_set_target_ies (target_1, 0);

		  fstmt_set_target_threshold (target_0, 7);
		  fstmt_set_target_threshold (target_1, 7);
		  delay (5);
		  $display ("Finished Initializing PLIC");
	       endseq;

   Stmt test1 = seq
		   $display (">---------------- TEST 1");
		   plic.set_verbosity (1);
		   fstmt_set_source_priority (5, 4);

		   fstmt_set_target_ies (target_0, 'b10_0000);    // bit 5
		   fstmt_set_target_threshold (target_0, 4);

		   fstmt_set_target_ies (target_1, 'b10_0000);    // bit 5
		   fstmt_set_target_threshold (target_1, 2);

		   plic.show_PLIC_state;
		   fa_print_plic_eips;

		   vrg_irqs [5] <= True;
		   delay (2);
		   plic.show_PLIC_state;
		   fa_print_plic_eips;

		   fstmt_set_target_threshold (target_0, 3);
		   plic.show_PLIC_state;
		   fa_print_plic_eips;

		   fstmt_claim (target_1);
		   plic.show_PLIC_state;
		   fa_print_plic_eips;

		   fstmt_complete (target_1, 5);
		   plic.show_PLIC_state;
		   fa_print_plic_eips;
		endseq;

   // ================================================================

   mkAutoFSM (seq
		 init;
		 test1;
		 $finish (0);
	      endseq);


endmodule

// ================================================================

endpackage
