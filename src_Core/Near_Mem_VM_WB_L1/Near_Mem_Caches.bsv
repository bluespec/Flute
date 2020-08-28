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

// This implementation of Near_Mem contains an IMem (MMU+Cache) and a
// DMem (MMU+Cache) Fabric-side Server interface is not used (no back
// door to caches).

package Near_Mem_Caches;

// ================================================================
// BSV lib imports

import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import Near_Mem_IFC     :: *;
import MMU_Cache_Common :: *;

`ifdef ISA_PRIV_S
import PTW              :: *;
`endif

import D_MMU_Cache      :: *;
import I_MMU_Cache      :: *;

import AXI4_Types   :: *;
import Fabric_Defs  :: *;

// System address map and pc_reset value
import SoC_Map :: *;

// ================================================================
// Exports

export mkNear_Mem;

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

   // ----------------------------------------------------------------
   // Connections from IMem to DMem (servicing PTW requests and PTE-writebacks)

`ifdef ISA_PRIV_S
   mkConnection (i_mmu_cache.ptw_client,      d_mmu_cache.imem_ptw_server);
   mkConnection (i_mmu_cache.pte_writeback_g, d_mmu_cache.imem_pte_writeback_p);
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

   // ----------------
   // IMem

   // CPU side
   interface IMem_IFC imem;
      // CPU side: IMem request
      method Action  req (Bit #(3) f3,
			  WordXL addr,
			  // The following  args for VM
			  Priv_Mode  priv,
			  Bit #(1)   sstatus_SUM,
			  Bit #(1)   mstatus_MXR,
			  WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }
	 i_mmu_cache.req (addr, priv, sstatus_SUM, mstatus_MXR, satp);
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

   // Fabric side
   interface imem_master = i_mmu_cache.mem_master;

   // ----------------
   // DMem

   // CPU side
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
	 d_mmu_cache.req (op, f3,
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

   // Fabric side
   interface Near_Mem_Fabric_IFC  mem_master = d_mmu_cache.mem_master;

   // ----------------
   // FENCE.I: flush both IMem and DMem

   interface Server server_fence_i;
      interface Put request;
	 method Action put (Token t);
	    i_mmu_cache.flush_server.request.put (flush_to_invalid);
	    d_mmu_cache.flush_server.request.put (flush_to_invalid);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue #(Token) get;
	    let ti <- i_mmu_cache.flush_server.response.get;
	    let td <- d_mmu_cache.flush_server.response.get;
	    return ?;
	 endmethod
      endinterface
   endinterface

   // ----------------
   // FENCE: flush DMem

   interface Server server_fence;
      interface Put request;
	 method Action put (Fence_Ordering t);
	    d_mmu_cache.flush_server.request.put (flush_to_invalid);
	 endmethod
      endinterface
      interface Get response;
	 method ActionValue #(Token) get;
	    let td <- d_mmu_cache.flush_server.response.get;
	    return ?;
	 endmethod
      endinterface
   endinterface

   // ----------------
   // SFENCE_VMA: flush TLBs and DMem

`ifdef ISA_PRIV_S
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
   // Interface to 'coherent DMA' port of optional L2 cache
   // This version (WB_L1) has no L2, so we stub this out.

   interface AXI4_Slave_IFC dma_server = dummy_AXI4_Slave_ifc;

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

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;
      i_mmu_cache.ma_ddr4_ready;
      d_mmu_cache.ma_ddr4_ready;
   endmethod

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status;
      return d_mmu_cache.mv_status;
   endmethod

endmodule

// ================================================================

endpackage: Near_Mem_Caches
