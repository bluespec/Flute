// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

// Near_Mem_IFC is an abstraction of the 'near' memory subsystem (TCMs
// (Tightly Coupled Memories), MMUs, L1 Caches, L2 caches, etc.

// On the CPU side it directly services instruction fetches and DMem
// reads and writes.

// On the Fabric side it has one or two Client sub-interfaces and a
// Server sub-interface.  The Client sub-interfaces are used to
// pass-through, to the fabric, I/O requests, cache-fill/ writeback
// requests, and any other memory requests outside the designated
// address range of Near_Mem.  There are two Client interfaces to
// accommodate IMem and DMem requests concurrently.

// This implementation of Near_Mem contains an IMem (MMU+L1-Cache) and
// a DMem (MMU+L1-Cache) and a unified L2 cache, with coherent L1s and
// L2.

package Near_Mem_Caches;

// ================================================================
// BSV lib imports

import ConfigReg    :: *;
import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import Near_Mem_IFC     :: *;
import MMU_Cache_Common :: *;
import Cache_Decls      :: *;
import CPU_IFC          :: *;    // For Wd_Id/Addr/User/Data_Dma

import AXI4_Types   :: *;
import Fabric_Defs  :: *;

// System address map and pc_reset value
import SoC_Map :: *;

`ifdef ISA_PRIV_S
import PTW :: *;
`endif

import D_MMU_Cache :: *;
import I_MMU_Cache :: *;

import LLCache_Aux   :: *;
import CacheUtils    :: *;
import CCTypes       :: *;
import LLCache       :: *;
import L1LLConnect   :: *;

`ifdef MEM_512b
import LLC_AXI4_Adapter_2   :: *;
`else
import LLC_AXI4_Adapter     :: *;
`endif

import MMIO_AXI4_Adapter    :: *;
import LLC_DMA_AXI4_Adapter :: *;

// ================================================================
// Exports

export mkNear_Mem;

// ================================================================
// Debug verbosities

// 0-1: quiet; 2 show L1-to-L2 and L2-to-L1 messages
Integer verbosity_I_L1_L2 = 0;    // for I-Cache
Integer verbosity_D_L1_L2 = 0;    // for D-Cache

// 0=quiet, 1 = rule firings
Integer verbosity_mmio_axi4_adapter = 0;

// ================================================================
// Interface type-conversion:
// from  L1 Cache's L2_to_L2_Client and L2_to_L1_Server interfaces
// to    L1LLConnect's ChildCacheToParent interface

function CCTypes::Msi fv_Meta_State_to_Msi (Meta_State s);
   return (case (s)
	      META_INVALID:   I;
	      META_SHARED:    S;
	      META_EXCLUSIVE: E;
	      META_MODIFIED:  M;
	   endcase);
endfunction

function Meta_State fv_Msi_to_Meta_State (CCTypes::Msi s);
   return (case (s)
	      I: META_INVALID;
	      S: META_SHARED;
	      E: META_EXCLUSIVE;
	      M: META_MODIFIED;
	   endcase);
endfunction

function Maybe #(CCTypes::Line) fv_M_CLine_to_M_Line (Maybe #(CLine) m_cline);
   return (case (m_cline) matches
	      tagged Invalid: tagged Invalid;
	      tagged Valid .x: tagged Valid (unpack (x));
	   endcase);
endfunction

function Maybe #(CLine) fv_M_Line_to_M_CLine (Maybe #(CCTypes::Line) m_line);
   return (case (m_line) matches
	      tagged Invalid:  tagged Invalid;
	      tagged Valid .x: tagged Valid (pack (x));
	   endcase);
endfunction

// ----------------

module mkL1_IFC_Adapter #(Integer                                          verbosity_L1_L2,
			  Integer                                          id,
			  Client_Semi_FIFOF #(L1_to_L2_Req, L2_to_L1_Rsp)  l1_to_l2_client,
			  Server_Semi_FIFOF #(L2_to_L1_Req, L1_to_L2_Rsp)  l2_to_l1_server)
       (ChildCacheToParent #(L1Way, void));

   interface FifoDeq rsToP;    // #(CRsMsg #(void))
      method Bool notEmpty;
	 return l2_to_l1_server.response.notEmpty;
      endmethod
      method Action deq;
	 l2_to_l1_server.response.deq;

	 if (verbosity_L1_L2 >= 2) begin
	    L1_to_L2_Rsp rsp = l2_to_l1_server.response.first;
	    $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id, fshow_L1_to_L2_Rsp (rsp));
	 end
      endmethod
      method CRsMsg #(void) first;
	 L1_to_L2_Rsp rsp = l2_to_l1_server.response.first;
	 let cRsMsg = CRsMsg {addr:    rsp.addr,
			      toState: fv_Meta_State_to_Msi (rsp.to_state),
			      data:    fv_M_CLine_to_M_Line (rsp.m_cline),
			      child:   ? };
	 return cRsMsg;
      endmethod
   endinterface

   interface FifoDeq rqToP;    // #(CRqMsg #(L1Way, void))
      method Bool notEmpty;
	 return l1_to_l2_client.request.notEmpty;
      endmethod
      method Action deq;
	 l1_to_l2_client.request.deq;

	 if (verbosity_L1_L2 >= 2) begin
	    L1_to_L2_Req req = l1_to_l2_client.request.first;
	    $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id, fshow_L1_to_L2_Req (req));
	 end
      endmethod
      method CRqMsg #(L1Way, void) first;
	 L1_to_L2_Req req = l1_to_l2_client.request.first;
	 let cRqMsg = CRqMsg {addr:      req.addr,
			      fromState: fv_Meta_State_to_Msi (req.from_state),
			      toState:   fv_Meta_State_to_Msi (req.to_state),
			      canUpToE:  req.can_up_to_E,
			      id:        ?,
			      child:     ? };
	 return cRqMsg;
      endmethod
   endinterface

   interface FifoEnq fromP;    // #(PRqRsMsg #(L1Way, void)
      method Bool notFull;
	 return (l1_to_l2_client.response.notFull
		 && l2_to_l1_server.request.notFull);
      endmethod
      method Action enq (PRqRsMsg #(L1Way, void) x);
	 case (x) matches
	    tagged PRq .prq:
	       begin
		  let req = L2_to_L1_Req {addr:     prq.addr,
					  to_state: fv_Msi_to_Meta_State (prq.toState) };
		  // Note: prq.child is discarded
		  l2_to_l1_server.request.enq (req);

		  if (verbosity_L1_L2 >= 2)
		     $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id,
			       fshow_L2_to_L1_Req (req));
	       end

	    tagged PRs .prs:
	       begin
		  let rsp = L2_to_L1_Rsp {addr:     prs.addr,
					  to_state: fv_Msi_to_Meta_State (prs.toState),
					  m_cline:  fv_M_Line_to_M_CLine (prs.data) };
		  l1_to_l2_client.response.enq (rsp);

		  if (verbosity_L1_L2 >= 2)
		     $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id,
			       fshow_L2_to_L1_Rsp (rsp));
	       end
	 endcase
      endmethod
   endinterface
endmodule

// ================================================================
// The module

// Module state
typedef enum {STATE_RESET, STATE_RESETTING, STATE_READY } State
deriving (Bits, Eq, FShow);

(* synthesize *)
module mkNear_Mem (Near_Mem_IFC);

   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (0);
   Reg #(State)    rg_state      <- mkReg (STATE_READY);

   // ----------------
   // System address map and pc reset value
   SoC_Map_IFC  soc_map  <- mkSoC_Map;

   // Reset response queue
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   I_MMU_Cache_IFC  i_mmu_cache <- mkI_MMU_Cache;
   D_MMU_Cache_IFC  d_mmu_cache <- mkD_MMU_Cache;

   // last level cache
   LLCache llc <- mkLLCache;

   // Adapter for llc's 'coherent DMA interface'
   AXI4_Slave_IFC #(Wd_Id_Dma,
		    Wd_Addr_Dma,
		    Wd_Data_Dma,
		    Wd_User_Dma)  llc_dma_axi4_adapter <- mkLLC_DMA_AXI4_Adapter (llc.dma);

   // Adapter for back-side of LLC to AXI4
   LLC_AXI4_Adapter_IFC  llc_axi4_adapter <- mkLLC_AXi4_Adapter (llc.to_mem);

   // Adapter for MMIO interfaces of i_mmu_cache and d_mmu_cache to AXI4
   MMIO_AXI4_Adapter_IFC #(2)
       mmio_axi4_adapter <- mkMMIO_AXI4_Adapter_2 (fromInteger (verbosity_mmio_axi4_adapter));

   // ----------------
   // Connections from IMem to DMem (servicing PTW requests and PTE-writebacks)

`ifdef ISA_PRIV_S
   mkConnection (i_mmu_cache.ptw_client,      d_mmu_cache.imem_ptw_server);
   mkConnection (i_mmu_cache.pte_writeback_g, d_mmu_cache.imem_pte_writeback_p);
`endif

   // ----------------
   // connect LLC to L1 caches, creating a crossbar

   Vector#(L1Num, ChildCacheToParent#(L1Way, void)) l1 = ?;
   let ifc_I_L1 <- mkL1_IFC_Adapter (verbosity_I_L1_L2,
				     0,
				     i_mmu_cache.l1_to_l2_client,
				     i_mmu_cache.l2_to_l1_server);
   l1 [0] = ifc_I_L1;
   let ifc_D_L1 <- mkL1_IFC_Adapter (verbosity_D_L1_L2,
				     1,
				     d_mmu_cache.l1_to_l2_client,
				     d_mmu_cache.l2_to_l1_server);
   l1 [1] = ifc_D_L1;
   mkL1LLConnect (llc.to_child, l1);

   // ----------------
   //Connect  MMIO interfaces of i_mmu_cache and d_mmu_cache to mmio_axi4_adapter

   mkConnection (i_mmu_cache.mmio_client, mmio_axi4_adapter.v_mmio_server [0]);
   mkConnection (d_mmu_cache.mmio_client, mmio_axi4_adapter.v_mmio_server [1]);

   // ----------------------------------------------------------------
   // BEHAVIOR

   // ----------------
   // Reset
   // This reset state machine operates on external soft-reset request.

   rule rl_reset (rg_state == STATE_RESET);
      rg_state <= STATE_RESETTING;

      if (cfg_verbosity > 1)
	 $display ("%0d: Near_Mem.rl_reset", cur_cycle);
   endrule

   rule rl_reset_complete (rg_state == STATE_RESETTING);
      f_reset_rsps.enq (?);
      rg_state <= STATE_READY;

      if (cfg_verbosity > 1)
	 $display ("%0d: Near_Mem.rl_reset_complete", cur_cycle);
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   // Reset
   interface Server server_reset;
      interface Put request;
	 method Action put (Token t) if (rg_state == STATE_READY);
	    rg_state <= STATE_RESET;
	 endmethod
      endinterface

      interface Get response;
	 method ActionValue #(Token) get ();
	    let rsp <- pop (f_reset_rsps);
	    return rsp;
	 endmethod
      endinterface
   endinterface

   // ----------------------------------------------------------------
   // CPU side interfaces

   // IMem
   interface IMem_IFC imem;
      // CPU side: IMem request
      method Action  req (Bit #(3) f3,
			  WordXL addr,
			  // The following  args for VM
			  Priv_Mode  priv,
			  Bit #(1)   sstatus_SUM,
			  Bit #(1)   mstatus_MXR,
			  WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }
	 i_mmu_cache.ma_req (addr, priv, sstatus_SUM, mstatus_MXR, satp);
      endmethod

      // CPU side: IMem response
      method Bool     valid          = i_mmu_cache.valid;
      method Bool     is_i32_not_i16 = True;
      method WordXL   pc             = i_mmu_cache.addr;
      method Instr    instr          = truncate (i_mmu_cache.word64);
      method Bool     exc            = i_mmu_cache.exc;
      method Exc_Code exc_code       = i_mmu_cache.exc_code;
      method WordXL   tval           = i_mmu_cache.addr;
   endinterface

   // DMem
   interface DMem_IFC dmem;
      // CPU side: DMem request
      method Action  req (CacheOp op,
			  Bit #(3) f3,
`ifdef ISA_A
			  Bit #(7) amo_funct7,
`endif
			  WordXL addr,
			  Bit #(64) store_value,
			  // The following  args for VM
			  Priv_Mode  priv,
			  Bit #(1)   sstatus_SUM,
			  Bit #(1)   mstatus_MXR,
			  WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }
	 d_mmu_cache.ma_req (op, f3,
`ifdef ISA_A
			     amo_funct7,
`endif
			     addr, store_value, priv, sstatus_SUM, mstatus_MXR, satp);
      endmethod

      // CPU side: DMem response
      method Bool       valid      = d_mmu_cache.valid;
      method Bit #(64)  word64     = d_mmu_cache.word64;
      method Bit #(64)  st_amo_val = d_mmu_cache.st_amo_val;
      method Bool       exc        = d_mmu_cache.exc;
      method Exc_Code   exc_code   = d_mmu_cache.exc_code;
   endinterface

   // FENCE.I (potentially flush both IMem and DMem)
   interface Server server_fence_i;
      interface Put request;
	 method Action put (Token t);
	    // With coherent caches, no need for any action here
	    // i_mmu_cache.flush_server.request.put (flush_to_invalid);
	    // d_mmu_cache.flush_server.request.put (flush_to_invalid);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue #(Token) get;
	    // With coherent caches, no need for any action here
	    // let ti <- i_mmu_cache.flush_server.response.get;
	    // let td <- d_mmu_cache.flush_server.response.get;
	    return ?;
	 endmethod
      endinterface
   endinterface

   // FENCE (potentially flush DMem)
   interface Server server_fence;
      interface Put request;
	 method Action put (Fence_Ordering t);
	    // With coherent caches, no need for any action here
	    // d_mmu_cache.flush_server.request.put (flush_to_invalid);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue #(Token) get;
	    // With coherent caches, no need for any action here
	    // let td <- d_mmu_cache.flush_server.response.get;
	    return ?;
	 endmethod
      endinterface
   endinterface

`ifdef ISA_PRIV_S
   // SFENCE_VMA (potentially flush TLBs and DMem)
   interface Server sfence_vma_server;
      interface Put request;
	 method Action put (Token t);
	    i_mmu_cache.tlb_flush;
	    d_mmu_cache.tlb_flush;
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue #(Token) get;
	    return ?;
	 endmethod
      endinterface
   endinterface
`endif

   // ----------------------------------------------------------------
   // Fabric side

   interface imem_master = mmio_axi4_adapter.mem_master;
   interface mem_master  = llc_axi4_adapter.mem_master;

   // ----------------------------------------------------------------
   // Interface to 'coherent DMA' port of optional L2 cache

   interface dma_server = llc_dma_axi4_adapter;

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
      d_mmu_cache.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Bit #(64) mv_tohost_value = d_mmu_cache.mv_tohost_value;
`endif

   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;
      llc_axi4_adapter.ma_ddr4_ready;
   endmethod

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status;
      return llc_axi4_adapter.mv_status;
   endmethod

endmodule

// ================================================================

endpackage: Near_Mem_Caches
