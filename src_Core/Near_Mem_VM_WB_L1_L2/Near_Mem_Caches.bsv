// Copyright (c) 2016-2021 Bluespec, Inc. All Rights Reserved.

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
// Options for cache-coherent access for devices
// Default is OPTION_DMA_CACHE, override with OPTION_L2_COHERENT_DMA_PORT

// OPTION_DMA_CACHE    OPTION_L2_COHERENT_DMA_PORT
//     defined              defined                => OPTION_L2_COHERENT_DMA_PORT
//     defined            undefined                => OPTION_DMA_CACHE
//   undefined              defined                => OPTION_L2_COHERENT_DMA_PORT
//   undefined            undefined                => OPTION_DMA_CACHE

// Default is OPTION_DMA_CACHE, unless overridden by explicit OPTION_L2_COHERENT_DMA_PORT
`ifdef OPTION_DMA_CACHE

`ifdef OPTION_L2_COHERENT_DMA_PORT
`undef OPTION_DMA_CACHE
`endif

`else

`ifndef OPTION_L2_COHERENT_DMA_PORT
`define OPTION_DMA_CACHE
`endif

`endif

// ================================================================
// Project imports

import ISA_Decls        :: *;
import Near_Mem_IFC     :: *;
import MMU_Cache_Common :: *;
import Cache_Decls      :: *;
import CPU_IFC          :: *;    // For Wd_Id/Addr/User/Data_Dma

import AXI4_Types   :: *;
import Fabric_Defs  :: *;

`ifdef ISA_PRIV_S
import PTW :: *;
`endif

import D_MMU_Cache :: *;
import I_MMU_Cache :: *;

import L1_IFC_Adapter :: *;

// This option is for "L1-like" cache connecting to L2 on an L1-like port
`ifdef OPTION_DMA_CACHE
import DMA_Cache   :: *;
`endif

// This option is for a direct connection to L2's "coherent DMA" port
`ifdef OPTION_L2_COHERENT_DMA_PORT
import LLC_DMA_AXI4_Adapter :: *;
`endif

// ----------------
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

// ================================================================
// Exports

export mkNear_Mem;

// ================================================================
// Debug verbosities

// 0-1: quiet; 2 show L1-to-L2 and L2-to-L1 messages
Integer verbosity_I_L1_L2   = 0;    // for I-Cache
Integer verbosity_D_L1_L2   = 0;    // for D-Cache
Integer verbosity_DMA_L1_L2 = 0;    // for DMA-Cache

// 0=quiet, 1 = rule firings
Integer verbosity_mmio_axi4_adapter = 0;

// ================================================================
// The module

// Module state
typedef enum {STATE_RESET, STATE_RESETTING, STATE_READY } State
deriving (Bits, Eq, FShow);

(* synthesize *)
module mkNear_Mem (Near_Mem_IFC);

   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (0);
   Reg #(State)    rg_state      <- mkReg (STATE_READY);

   // Reset response queue
   FIFOF #(Token) f_reset_rsps <- mkFIFOF;

   // L1 caches
   I_MMU_Cache_IFC  i_mmu_cache <- mkI_MMU_Cache;    // Instruction fetch
   D_MMU_Cache_IFC  d_mmu_cache <- mkD_MMU_Cache;    // Data, PTWs
`ifdef OPTION_DMA_CACHE
   DMA_Cache_IFC    dma_cache   <- mkDMA_Cache;      // External 'devices'
`endif

   // last level cache
   LLCache llc <- mkLLCache;

`ifdef OPTION_DMA_CACHE
   messageM ("INFO: Near_Mem_Caches: coherent device access with DMA_Cache");

   // Tie-offs for llc.dma, which is not used here
   FifoDeq #(DmaRq #(LLCDmaReqId)) nullFifoDeq_memReq = nullFifoDeq;
   FifoEnq #(DmaRs #(LLCDmaReqId)) nullFifoEnq_RespLd = nullFifoEnq;
   FifoEnq #(LLCDmaReqId)          nullFifoEnq_RespSt = nullFifoEnq;

   mkConnection (llc.dma.memReq, nullFifoDeq_memReq);
   mkConnection (llc.dma.respLd, nullFifoEnq_RespLd);
   mkConnection (llc.dma.respSt, nullFifoEnq_RespSt);
`endif

`ifdef OPTION_L2_COHERENT_DMA_PORT
   messageM ("INFO: Near_Mem_Caches: coherent device access with direct coherent L2 port");

   // Adapter for llc's 'coherent DMA interface'
   AXI4_Slave_IFC #(Wd_Id_Dma,
		    Wd_Addr_Dma,
		    Wd_Data_Dma,
		    Wd_User_Dma)  llc_dma_axi4_adapter <- mkLLC_DMA_AXI4_Adapter (llc.dma);
`endif

   // Adapter for back-side of LLC to AXI4
   LLC_AXI4_Adapter_IFC  llc_axi4_adapter <- mkLLC_AXi4_Adapter (llc.to_mem);

   // Adapter for MMIO interfaces of i_mmu_cache and d_mmu_cache to AXI4
   MMIO_AXI4_Adapter_IFC #(Num_MMIO_L1_Clients)
       mmio_axi4_adapter <- mkMMIO_AXI4_Adapter_2 (fromInteger (verbosity_mmio_axi4_adapter));

   // ----------------
   // Connections from IMem to DMem (servicing PTW requests and PTE-writebacks)

`ifdef ISA_PRIV_S
   mkConnection (i_mmu_cache.ptw_client,      d_mmu_cache.imem_ptw_server);
   mkConnection (i_mmu_cache.pte_writeback_g, d_mmu_cache.imem_pte_writeback_p);
`endif

   // ----------------
   // connect L1 caches to LLC (L2)

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

`ifdef OPTION_DMA_CACHE
   let ifc_DMA_L1 <- mkL1_IFC_Adapter (verbosity_DMA_L1_L2,
				       2,
				       dma_cache.l1_to_l2_client,
				       dma_cache.l2_to_l1_server);
   l1 [2] = ifc_DMA_L1;
`endif

   mkL1LLConnect (llc.to_child, l1);

   // ----------------
   //Connect  MMIO interfaces of i_mmu_cache, d_mmu_cache and dma_cache to mmio_axi4_adapter

   mkConnection (i_mmu_cache.mmio_client, mmio_axi4_adapter.v_mmio_server [0]);
   mkConnection (d_mmu_cache.mmio_client, mmio_axi4_adapter.v_mmio_server [1]);
`ifdef OPTION_DMA_CACHE
   mkConnection (dma_cache.mmio_client,   mmio_axi4_adapter.v_mmio_server [2]);
`endif
`ifdef OPTION_L2_COHERENT_DMA_PORT
   // TODO: llc_dma_axi4_adapter should triage MMIO and connect here
   Client #(Single_Req, Single_Rsp) cstub = client_stub;
   mkConnection (cstub, mmio_axi4_adapter.v_mmio_server [2]);
`endif

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
   // Interface for coherent access by devices

`ifdef OPTION_DMA_CACHE
   interface dma_server = dma_cache.axi4_s;
`endif

`ifdef OPTION_L2_COHERENT_DMA_PORT
   interface dma_server = llc_dma_axi4_adapter;
`endif

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
