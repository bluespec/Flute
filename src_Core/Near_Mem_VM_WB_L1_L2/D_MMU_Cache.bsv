// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package D_MMU_Cache;

// ================================================================
// A combined MMU and L1 Cache for the RISC-V data stream.
// Handles LD, ST, AMO_LR, AMO_SC, and remaining AMO_ops.
//
// The MMU does VA-to-PA addr translation.
// (VA=virtual addr; PA=physical addr.)
//
// The cache is probed speculatively using VA (in parallel with MMU).
//     Uses bits that are the same in VA and PA, i.e., byte-in-page
//     address bits.
//
// After MMU translation, there is a 2-way triage based on PA:
//  - Cacheable:   request goes to the cache module
//  - Uncacheable: request goes to the MMIO  module
//
// For cacheable addrs:
//    Provide the MMU-translated PA to the cache for tag-matching, and
//    wait if miss (wait until hit/err).
//
// For non-cacheable addrs:
//     Request goes to MMIO module, and
//     wait for MMIO response.
//
// Front-side interface (CPU-facing): this MMU_Cache is parameterized for
// data-width It can be used for both RV32 and RV64 CPUs.  RV32
// vs. RV64 only affects width of some CPU-side interface ports:
//    - inputs req 'addr' and 'satp'    (type WordXL)
//    - output response 'addr' (copy of requesting addr)    (type WordXL)
//    - data input (store-value) and data output (load-value) are
//        always 64b, to support double-precision floating point mem
//        LD/ST in RV32.
//
// Back-side interfaces
//    - Client and Server connecting to next-level cache:
//        cache-line-width, with MESI coherence protocol.
//    - Client for MMIO single-word requests/responses

// ----------------
// NOTE regarding "watch tohost"
// Special (fragile) ad hoc support for standard ISA tests during
// simulation: observe writes to physical addr <tohost> and stop on
// non-zero write.  This activity is done here rather than at memory
// because, in the standard ISA tests, the <tohost> addr can be within
// the cacheable memory region, and therefore may never be written
// back to memory.  The actual address is supplied via the
// 'set_watch_tohost' method.  Standard ISA tests terminate by writing
// a non-zero value to the <tohost> addr. Bit [0] is always 1. Bits
// [n:1] specify which specific sub-test within the test failed.

// ================================================================
// BSV lib imports

import Vector       :: *;
import BRAMCore     :: *;
import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import Assert       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import ISA_Decls    :: *;
import Near_Mem_IFC :: *;

import SoC_Map :: *;

import MMU_Cache_Common :: *;

`ifdef ISA_PRIV_S
import TLB :: *;
import PTW :: *;
`endif

import Cache :: *;
import MMIO  :: *;

// ================================================================

export  D_MMU_Cache_IFC (..),  mkD_MMU_Cache;

// ================================================================
// MODULE INTERFACE

interface D_MMU_Cache_IFC;
   // CPU interface: request
   (* always_ready *)
   method Action  ma_req (CacheOp    op,
			  Bit #(3)   f3,
`ifdef ISA_A
			  Bit #(7)   amo_funct7,
`endif
			  WordXL     va,
			  Bit #(64)  st_value,
			  // The following args for VM
			  Priv_Mode  priv,
			  Bit #(1)   sstatus_SUM,
			  Bit #(1)   mstatus_MXR,
			  WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }

   // CPU interface: response
   (* always_ready *)  method Bool       valid;
   (* always_ready *)  method WordXL     addr;        // req addr for which this is a response
   (* always_ready *)  method Bit #(64)  word64;      // rd_val (data for LD, LR, AMO, SC success/fail result)
   (* always_ready *)  method Bit #(64)  st_amo_val;  // Final stored value for ST, SC, AMO
   (* always_ready *)  method Bool       exc;
   (* always_ready *)  method Exc_Code   exc_code;

   // Cache flush request/response
   interface Server #(Bit #(1), Token) flush_server;

`ifdef ISA_PRIV_S
   // TLB flush
   method Action tlb_flush;

   // PTW and PTE-writeback requests from I_MMU_Cache are serviced by D_MMU_Cache
   interface Server #(PTW_Req, PTW_Rsp)  imem_ptw_server;
   interface Put #(Tuple2 #(PA, WordXL)) imem_pte_writeback_p;
`endif

   // ----------------
   // Cache-line interface facing next level cache or memory
   // (for refills, writebacks, downgrades, ...)

   interface Client_Semi_FIFOF #(L1_to_L2_Req, L2_to_L1_Rsp)  l1_to_l2_client;
   interface Server_Semi_FIFOF #(L2_to_L1_Req, L1_to_L2_Rsp)  l2_to_l1_server;

   // ----------------
   // MMIO interface facing memory

   interface Client #(Single_Req, Single_Rsp) mmio_client;

`ifdef WATCH_TOHOST
   // ----------------------------------------------------------------
   // For ISA tests: watch memory writes to <tohost> addr (see NOTE: "tohost" above)

   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
   method Bit #(64) mv_tohost_value;
`endif

endinterface

// ****************************************************************
// ****************************************************************
// ****************************************************************
// Internal types and constants.

// The interaction with 'cache' (from mkCache) is done in 2-to-3 steps:
//   Step A: provide an address (virtual or physical) to probe cache RAMs
//   Step B: provide an address (physical) to tag-match for HIT/MISS
//   Step C: in case of MISS in Step B, the cache will have started
//           the refill; this step WAITs until it is ready.

// The cache is occupied during several operations:
//   1. CPU requests
//   2. PTW-reads  (reading Page Table Entries during Page Table Walks)
//        for both D-Mem and I-Mem
//        For D-Mem, requests occur during (1)
//        For I-Mem, requests can arrive asynchronously w.r.t. (1).
//   3. PTE-writes (writing modified Page Table Entries during MMU access)
//        for both D-Mem and I-Mem
//        For D-Mem, requests occur during (1)
//        For I-Mem, requests can arrive asynchronously w.r.t. (1).
//   4. 'Flushes' (resulting in all entries INVALID)
//        These are mutually exclusive w.r.t. (1)
//   5. Coherence requests from L2 to downgrade a cache line.
//        Can arrive asynchronously w.r.t. (1)

// ----------------
// State of rg_mmu_cache_req (request from CPU)
typedef enum {REQ_STATE_EMPTY,        // No current request from CPU
	      REQ_STATE_FULL_A,       // Ready for cache step A
	      REQ_STATE_FULL_B }      // Ready for cache step B
        Req_State
deriving (Bits, Eq, FShow);

// ----------------
// State of module

typedef enum {STATE_MAIN,               // Ready to service CPU requests
	      STATE_MAIN_ST_WAIT,       // 1-cycle wait on stores, for SRAM propagation
	      STATE_MAIN_CACHE_WAIT,    // On cache miss wait for cache to refill
	      STATE_MAIN_MMIO_WAIT,     // Wait for MMIO response
	      STATE_FLUSH_WAIT

`ifdef ISA_PRIV_S
	    , STATE_PTW_WAIT,

	      STATE_PTE_RD_B,           // Handle PTW read-PTE req
	      STATE_PTE_RD_CACHE_WAIT,  // Wait for cache during PTW read-PTE req

	      STATE_PTE_WR_B,           // Handle modified-PTE writeback
	      STATE_PTE_WR_CACHE_WAIT   // Wait for cache during modified-PTE writeback
`endif
   } State
deriving (Bits, Eq, FShow);

// ----------------
// Exception codes depending on the kind of request

function Exc_Code fv_exc_code_misaligned (MMU_Cache_Req req);
   return (  ((req.op == CACHE_LD) || fv_is_AMO_LR (req))
	   ? exc_code_LOAD_ADDR_MISALIGNED
	   : exc_code_STORE_AMO_ADDR_MISALIGNED);
endfunction

function Exc_Code fv_exc_code_access_fault (MMU_Cache_Req req);
   return (  ((req.op == CACHE_LD) || fv_is_AMO_LR (req))
	   ? exc_code_LOAD_ACCESS_FAULT
	   : exc_code_STORE_AMO_ACCESS_FAULT);
endfunction

function Exc_Code fv_exc_code_page_fault (MMU_Cache_Req req);
   return (  ((req.op == CACHE_LD) || fv_is_AMO_LR (req))
	   ? exc_code_LOAD_PAGE_FAULT
	   : exc_code_STORE_AMO_PAGE_FAULT);
endfunction

// ================================================================
// MODULE IMPLEMENTATION
                
(* synthesize *)
module mkD_MMU_Cache (D_MMU_Cache_IFC);

   // For debugging
   Integer verbosity       = 0;    // 1: Requests and responses; 2: rules; 3: detail
   Integer verbosity_ptw   = 0;    // 1: rule firings
   Integer verbosity_cache = 0;    // 1: rules; 2: detail
   Integer verbosity_mmio  = 0;    // 1: rules

   // ----------------------------------------------------------------
   // Major sub-modules

   // SoC_Map is needed for method 'm_is_mem_addr' to distinguish mem
   // (cached) and other (non-cached) addrs
   SoC_Map_IFC soc_map <- mkSoC_Map;

   Bool dmem_not_imem = True;
   Cache_IFC  cache <- mkCache (dmem_not_imem,
				fromInteger (verbosity_cache));

   MMIO_IFC   mmio  <- mkMMIO (fromInteger (verbosity_mmio));

`ifdef ISA_PRIV_S
   TLB_IFC    tlb   <- mkTLB (dmem_not_imem,
			      fromInteger (verbosity_cache));
   PTW_IFC    ptw   <- mkPTW (fromInteger (verbosity_ptw));

   // Writebacks to mem of PTEs whose PTE.A and/or PTE.D have been modified
   FIFOF #(Tuple2 #(PA, WordXL)) f_dmem_pte_writebacks <- mkFIFOF;
`endif

   // ----------------------------------------------------------------
   // Overall state of this module

   Reg #(State) crg_state [2] <- mkCReg (2, STATE_MAIN);

   // Current request from the CPU
   Reg #(Req_State)      crg_mmu_cache_req_state [2] <- mkCReg  (2, REQ_STATE_EMPTY);
   Reg #(MMU_Cache_Req)  crg_mmu_cache_req       [2] <- mkCRegU (2);

   // ----------------------------------------------------------------
   // Outputs from this module
   // 'final_st_val' is the final stored value for ST, SC, AMO (for verification only)

   Reg #(Bool)      crg_valid [2]        <- mkCReg (2, False);
   Reg #(Bool)      crg_exc [2]          <- mkCRegU (2);
   Reg #(Exc_Code)  crg_exc_code [2]     <- mkCRegU (2);
   Reg #(Bit #(64)) crg_ld_val [2]       <- mkCRegU (2);  // Load-val for LOAD/LR/AMO, success/fail for SC
   Reg #(Bit #(64)) crg_final_st_val [2] <- mkCRegU (2);

`ifdef WATCH_TOHOST
   // See NOTE: "tohost" above.
   // "tohost" addr on which to monitor writes, for standard ISA tests.
   // These are set by the 'set_watch_tohost' method but are otherwise read-only.
   Reg #(Bool)      rg_watch_tohost <- mkReg (False);
   Reg #(Bit #(64)) rg_tohost_addr  <- mkReg ('h_8000_1000);
   Reg #(Bit #(64)) rg_tohost_value <- mkReg (0);
`endif

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR

   // ----------------------------------------------------------------
   // If WATCH_TOHOST is configured, this function monitors STOREs for
   // writes to the "tohost" address.  This is used only in certain
   // ISA and other tests, in simulation.

`ifdef WATCH_TOHOST
   Reg #(Bool) rg_pass_fail_msg_printed <- mkReg (False);
`endif

   function Action fa_watch_tohost (Bit #(64) addr, Bit #(64) final_st_val);
      action
`ifdef WATCH_TOHOST
	 if (rg_watch_tohost
	     && (addr == rg_tohost_addr)
	     && (final_st_val != 0))
	    begin
	       rg_tohost_value <= final_st_val;

	       if (! rg_pass_fail_msg_printed) begin
		  let test_num = (final_st_val >> 1);
		  $display ("%0d: %m.fa_watch_tohost", cur_cycle);
		  if (test_num == 0) $write ("    PASS");
		  else               $write ("    FAIL <test_%0d>", test_num);
		  $display ("  (<tohost>  addr %0h  data %0h)", addr, final_st_val);
		  rg_pass_fail_msg_printed <= True;
	       end
	    end
`endif
      endaction
   endfunction

   // ================================================================
   // This rule is basically the body of method ma_req; decoupling
   // through a wire affords scheduling flexibility.

   // Note: This rule can fire whenever a request is made.
   // The CPU pipeline ensures that this request is made only
   // when the downstream stage is not blocked.

   Wire #(MMU_Cache_Req) wire_mmu_cache_req <- mkWire;

   (* fire_when_enabled *)
   rule rl_CPU_req;
      let mmu_cache_req = wire_mmu_cache_req;

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_CPU_req", cur_cycle);
	 $display ("    ", fshow_MMU_Cache_Req (mmu_cache_req));
      end

      // Assertion check: CPU pipe should never submit a request while
      // the previous request is still being serviced
      if (crg_mmu_cache_req_state [1] != REQ_STATE_EMPTY) begin
	 $display ("%0d: %m.rl_CPU_req", cur_cycle);
	 $display ("    INTERNAL ERROR: crg_mmu_cache_req_state: ",
		   fshow (crg_mmu_cache_req_state [1]), "; expected EMPTY");
	 $display ("    ", fshow_MMU_Cache_Req (mmu_cache_req));
	 $finish (1);
      end

      // Register it here and in MMIO module
      crg_mmu_cache_req [1] <= mmu_cache_req;
      mmio.req (mmu_cache_req);

      crg_valid [1] <= False;

      if ((crg_state [1] != STATE_MAIN) || (! cache.mv_is_idle)) begin
	 if (verbosity >= 1)
	    $display ("    Cache busy; probe later");

	 crg_mmu_cache_req_state [1] <= REQ_STATE_FULL_A;
      end
      else begin
	 if (verbosity >= 1)
	    $display ("    Probe cache (cache.ma_request_va)");

	 // Start cache probe with VA
	 cache.ma_request_va (mmu_cache_req.va);
	 crg_mmu_cache_req_state [1] <= REQ_STATE_FULL_B;
      end
   endrule

   // ================================================================
   // CPU request-handling: Perform Step_A for CPU request
   // This situation only arises when rl_CPU_req could not perform
   // Step_A because the CPU request arrived while the cache was
   // otherwise occupied.

   (* descending_urgency = "rl_CPU_req, rl_CPU_req_A" *)
   rule rl_CPU_req_A (   (crg_state [0] == STATE_MAIN)
		      && (crg_mmu_cache_req_state [0] == REQ_STATE_FULL_A)
		      && cache.mv_is_idle);

      let mmu_cache_req = crg_mmu_cache_req [0];

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_CPU_req_A", cur_cycle);
	 $display ("    ", fshow_MMU_Cache_Req (mmu_cache_req));
      end

      cache.ma_request_va (mmu_cache_req.va);
      crg_mmu_cache_req_state [0] <= REQ_STATE_FULL_B;
   endrule

   // ================================================================
   // CPU request-handling: Perform cache step B for CPU request

`ifdef ISA_PRIV_S
   // VM translation (VA to PA)
   VM_Xlate_Result vm_xlate_result = tlb.mv_vm_xlate (crg_mmu_cache_req [0].va,
						      crg_mmu_cache_req [0].satp,
						      ((crg_mmu_cache_req [0].op == CACHE_LD)
						       || fv_is_AMO_LR (crg_mmu_cache_req [0])),
						      crg_mmu_cache_req [0].priv,
						      crg_mmu_cache_req [0].sstatus_SUM,
						      crg_mmu_cache_req [0].mstatus_MXR);
`else
   // In non-VM, translation result (PA) is same as VA
   VM_Xlate_Result vm_xlate_result = VM_Xlate_Result {outcome: VM_XLATE_OK,
						      pa:      crg_mmu_cache_req [0].va};
`endif

   rule rl_CPU_req_B ((crg_state [0] == STATE_MAIN)
		      && (crg_mmu_cache_req_state [0] == REQ_STATE_FULL_B));

      let mmu_cache_req       = crg_mmu_cache_req [0];
      let mmu_cache_req_state = crg_mmu_cache_req_state [0];

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_CPU_req_B", cur_cycle);
	 $display ("    ", fshow_MMU_Cache_Req (mmu_cache_req));
      end

      if (verbosity >= 3)
	 $display ("    ", fshow_VM_Xlate_Result (vm_xlate_result));

      if (! fn_is_aligned (mmu_cache_req.f3 [1:0], mmu_cache_req.va)) begin
	 // Misaligned accesses not supported
	 crg_valid [0]               <= True;
	 crg_exc [0]                 <= True;
	 crg_exc_code [0]            <= fv_exc_code_misaligned (mmu_cache_req);
	 crg_mmu_cache_req_state [0] <= REQ_STATE_EMPTY;

	 if (verbosity >= 3)
	    $display ("    MISALIGNED exception");
      end

`ifdef ISA_PRIV_S
      // ---- TLB miss
      else if (vm_xlate_result.outcome == VM_XLATE_TLB_MISS) begin
	 // Start a Page Table Walk
	 if (verbosity >= 3)
	    $display ("    Start PTW; -> STATE_PTW_WAIT");

	 let ptw_req = PTW_Req {va: mmu_cache_req.va, satp: mmu_cache_req.satp};
	 ptw.dmem_server.request.put (ptw_req);

	 crg_valid [0] <= False;
	 crg_state [0] <= STATE_PTW_WAIT;
      end

      // ---- TLB translation exception
      else if (vm_xlate_result.outcome == VM_XLATE_EXCEPTION) begin
	 if (verbosity >= 3)
	    $display ("    VM_XLATE_EXCEPTION");

	 crg_valid [0]               <= True;
	 crg_exc [0]                 <= True;
	 crg_exc_code [0]            <= vm_xlate_result.exc_code;
	 crg_mmu_cache_req_state [0] <= REQ_STATE_EMPTY;
      end
`endif

      // ---- TLB success
      else begin
	 dynamicAssert ((vm_xlate_result.outcome == VM_XLATE_OK), "FAIL: unknown vm_xlate result");

`ifdef ISA_PRIV_S
	 // If PTE A, D bits modified ...
	 if (vm_xlate_result.pte_modified) begin
	    // Update the TLB
	    ASID asid = fn_satp_to_ASID (mmu_cache_req.satp);
	    VPN  vpn  = fn_Addr_to_VPN  (mmu_cache_req.va);
	    tlb.ma_insert (asid,
			   vpn,
			   vm_xlate_result.pte,
			   vm_xlate_result.pte_level,
			   vm_xlate_result.pte_pa);
	    // Writeback the modified PTE to memory
	    // Enqueue it to be written back to memory
	    f_dmem_pte_writebacks.enq (tuple2 (vm_xlate_result.pte_pa, vm_xlate_result.pte));
	    if (verbosity >= 3)
	       $display ("    Writeback updated PTE: pa %0h pte %0h",
			 vm_xlate_result.pte_pa,
			 vm_xlate_result.pte);
	 end
`endif
	 // Triage cached (memory) vs. uncached (IO, other non-mem) addresses
	 let is_mem_addr = soc_map.m_is_mem_addr (fv_PA_to_Fabric_Addr (vm_xlate_result.pa));

	 // Address is for memory (cacheable)
	 if (is_mem_addr) begin
	    // Cache operation (lookup, write, amo, ...)
	    let cache_result <- cache.mav_request_pa (mmu_cache_req, vm_xlate_result.pa);

	    if (cache_result.outcome == CACHE_MISS) begin
	       crg_valid [0] <= False;
	       crg_state [0] <= STATE_MAIN_CACHE_WAIT;
	       if (verbosity >= 3)
		  $display ("    Cache Miss: waiting for refill -> STATE_MAIN_CACHE_WAIT");
	    end
	    else begin    // Cache hit
	       crg_exc [0]          <= False;
	       crg_ld_val [0]       <= cache_result.final_ld_val;
	       crg_final_st_val [0] <= cache_result.final_st_val;

	       if (cache_result.outcome == CACHE_READ_HIT) begin
		  // Consume request and drive response immediately
		  crg_valid [0]               <= True;
		  crg_mmu_cache_req_state [0] <= REQ_STATE_EMPTY;
		  if (verbosity >= 3)
		     $display ("    Cache Read-hit: final_ld_val %0h; remain in STATE_MAIN",
			       cache_result.final_ld_val);
	       end
	       else if (cache_result.outcome == CACHE_WRITE_HIT) begin
		  // Provide response only after a cycle delay to
		  // avoid SRAM conflicts, in case the next request
		  // is a read for the same SRAM address we just wrote.
		  crg_valid [0] <= False;
		  crg_state [0] <= STATE_MAIN_ST_WAIT;
		  if (verbosity >= 3)
		     $display ("    Cache Write-hit: final_ld_val %0h final_st_val %0h -> STATE_MAIN_ST_WAIT",
			       cache_result.final_ld_val, cache_result.final_st_val);
	       end
	    end
	 end

	 // Address is for non-memory (I/O, non-cacheable)
	 else begin
	    crg_valid [0] <= False;
	    mmio.start (vm_xlate_result.pa);
	    crg_state [0] <= STATE_MAIN_MMIO_WAIT;
	    if (verbosity >= 3)
	       $display ("    MMIO started; -> STATE_MAIN_MMIO_WAIT");
	 end

	 // ISA tests: monitor 'tohost' address for test completion
	 fa_watch_tohost (zeroExtend (vm_xlate_result.pa), mmu_cache_req.st_value);
      end
   endrule: rl_CPU_req_B

   // ================================================================
   // 1-cycle ST wait

   rule rl_CPU_ST_wait (crg_state [0] == STATE_MAIN_ST_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_CPU_ST_wait -> STATE_MAIN", cur_cycle);
      crg_valid [0]               <= True;
      crg_mmu_cache_req_state [0] <= REQ_STATE_EMPTY;
      crg_state [0]               <= STATE_MAIN;
   endrule

   // ================================================================
   // Wait for cache to finish refill, then try again or drive exception

   rule rl_CPU_cache_wait (crg_state [0] == STATE_MAIN_CACHE_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_CPU_cache_wait: done -> STATE_MAIN", cur_cycle);

      if (! cache.mv_refill_ok) begin
	 crg_valid [0]               <= True;
	 crg_exc [0]                 <= True;
	 crg_exc_code [0]            <= fv_exc_code_access_fault (crg_mmu_cache_req [0]);
	 crg_mmu_cache_req_state [0] <= REQ_STATE_EMPTY;
	 if (verbosity >= 2)
	    $display ("    (error on refill)", cur_cycle);
      end
      crg_state [0] <= STATE_MAIN;
   endrule

   // ================================================================
   // Wait until mmio.result is available.
   // If no error, drive response.
   // If error, go to drive exception rsponse.

   rule rl_CPU_req_mmio_WAIT (crg_state [0] == STATE_MAIN_MMIO_WAIT);
      match { .err, .ld_val, .final_st_val } = mmio.result;

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_CPU_req_mmio_WAIT", cur_cycle);
	 $display ("    mmio.result = (err %0d, ld_val %0h, final_st_val %0h)",
		   err, ld_val, final_st_val);
      end

      crg_valid [0]               <= True;
      crg_ld_val [0]              <= ld_val;
      crg_final_st_val [0]        <= final_st_val;
      crg_exc [0]                 <= err;
      crg_exc_code [0]            <= fv_exc_code_access_fault (crg_mmu_cache_req [0]);
      crg_mmu_cache_req_state [0] <= REQ_STATE_EMPTY;
      crg_state [0]               <= STATE_MAIN;
   endrule

   // ================================================================
   // On TLB miss, do a PTW, then try again or go to exception.

`ifdef ISA_PRIV_S
   rule rl_PTW_wait (crg_state [0] == STATE_PTW_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_PTW_wait", cur_cycle);

      let ptw_rsp <- ptw.dmem_server.response.get;

      if (ptw_rsp.result == PTW_OK) begin
	 // Insert into TLB
	 ASID asid = fn_satp_to_ASID (crg_mmu_cache_req [0].satp);
	 VPN  vpn  = fn_Addr_to_VPN  (crg_mmu_cache_req [0].va);
	 tlb.ma_insert (asid, vpn, ptw_rsp.pte, ptw_rsp.level, ptw_rsp.pte_pa);

	 crg_mmu_cache_req_state [0] <= REQ_STATE_FULL_A;
	 crg_state [0] <= STATE_MAIN;
	 if (verbosity >= 3)
	    $display ("    ok; retry -> STATE_MAIN");
      end
      else begin
	 crg_valid [0] <= True;
	 crg_exc   [0] <= True;
	 if (ptw_rsp.result == PTW_ACCESS_FAULT) begin
	    if (verbosity >= 3)
	       $display ("    ACCESS FAULT -> STATE_MAIN");
	    crg_exc_code [0] <= fv_exc_code_access_fault (crg_mmu_cache_req [0]);
	 end
	 else begin // PTW_PAGE_FAULT
	    if (verbosity >= 3)
	       $display ("    PAGE FAULT -> STATE_MAIN");
	    crg_exc_code [0] <= fv_exc_code_page_fault (crg_mmu_cache_req [0]);
	 end
	 crg_mmu_cache_req_state [0] <= REQ_STATE_EMPTY;
	 crg_state [0]              <= STATE_MAIN;
      end
   endrule
`endif

   // ****************************************************************
   // ****************************************************************
   // CACHE FLUSH

   FIFOF #(Bit #(1))  f_cache_flush_reqs <- mkFIFOF;
   FIFOF #(Bit #(0))  f_cache_flush_rsps <- mkFIFOF;

   rule rl_cache_flush_start ((crg_state [0] == STATE_MAIN)
			      && (crg_mmu_cache_req_state [0] == REQ_STATE_EMPTY));
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_cache_flush_start", cur_cycle);

      let to_state_code = f_cache_flush_reqs.first;
      cache.flush_server.request.put (to_state_code);
      crg_state [0] <= STATE_FLUSH_WAIT;
   endrule

   rule rl_cache_flush_finish (crg_state [0] == STATE_FLUSH_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_cache_flush_finish", cur_cycle);

      f_cache_flush_reqs.deq;
      let x <- cache.flush_server.response.get;
      f_cache_flush_rsps.enq (?);
      crg_state [0] <= STATE_MAIN;
   endrule

   // ****************************************************************
   // ****************************************************************
   // PTW (Page Table Walks) service PTW memory requests (which are reads, only)
   // from the cache

`ifdef ISA_PRIV_S
   // Holds request between _A and _B rules
   // TODO: if PTW had a SemiFIFOF interface instead of Get, we wouldn't need this reg.
   Reg #(PTW_Mem_Req) rg_ptw_mem_req <- mkRegU;

   // 1-element stack to remember crg_state [0], so that we can
   // restore it after servicing the PTW-RD cache request.
   Reg #(State) rg_state_stack_during_ptw_rd <- mkRegU;

   Bool ok_to_do_DMem_PTW = (crg_state [0] == STATE_PTW_WAIT);
   Bool ok_to_do_IMem_PTW = (   (crg_state [0] == STATE_MAIN)
			     && (crg_mmu_cache_req_state [0] == REQ_STATE_EMPTY));

   // Step A
   rule rl_ptw_rd_A (ok_to_do_DMem_PTW || ok_to_do_IMem_PTW);
      let ptw_mem_req <- ptw.mem_client.request.get;

      if (verbosity >= 3) begin
	 $display ("%0d: %m.rl_ptw_rd_A: ", cur_cycle);
	 $display ("    cache.ma_request_va %0h -> STATE_PTE_RD_B", ptw_mem_req.pte_pa);
      end

      // Start the cache RAM probe with "va" (= pte_pa)
      cache.ma_request_va (truncate (ptw_mem_req.pte_pa));
      rg_ptw_mem_req               <= ptw_mem_req;
      rg_state_stack_during_ptw_rd <= crg_state [0];
      crg_state [0]                <= STATE_PTE_RD_B;
   endrule
   
   // Step B
   rule rl_ptw_rd_B (crg_state [0] == STATE_PTE_RD_B);
      let req = MMU_Cache_Req {op:          CACHE_LD,
			       f3:          ((xlen == 32) ? 3'b010 : 3'b011),
			       va:          truncate (rg_ptw_mem_req.pte_pa),
			       st_value:    ?,
			       amo_funct7:  0,
			       priv:        m_Priv_Mode,
			       sstatus_SUM: 0,
			       mstatus_MXR: 0,
			       satp:        0};
      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_ptw_rd_B: cache request_B", cur_cycle);
	 $display ("    ", fshow_MMU_Cache_Req (req));
      end
      
      let cache_result <- cache.mav_request_pa (req, rg_ptw_mem_req.pte_pa);
      if (verbosity >= 3)
	 $display ("    rl_ptw_rd_B: ", fshow_Cache_Result (cache_result));
      
      // Assertion check: cannot be a WRITE_HIT
      if (cache_result.outcome == CACHE_WRITE_HIT) begin
	 $display ("%0d: %m.rl_ptw_rd_B", cur_cycle);
	 $display ("    INTERNAL ERROR: cannot have CACHE_WRITE_HIT for PTW read-request to cache");
	 $display ("    ", fshow_MMU_Cache_Req (req));
	 $finish (1);
      end

      if (cache_result.outcome == CACHE_READ_HIT) begin
	 let ptw_mem_rsp = PTW_Mem_Rsp {ok: True, pte: truncate (cache_result.final_ld_val)};
	 ptw.mem_client.response.put (ptw_mem_rsp);
	 crg_state [0] <= rg_state_stack_during_ptw_rd;
	 if (verbosity >= 3)
	    $display ("    rl_ptw_rd_B: -> ", fshow (rg_state_stack_during_ptw_rd));
      end
      else begin // Miss (CACHE_MISS only; CACHE_WRITE_HIT not possible in PTW)
	 crg_state [0] <= STATE_PTE_RD_CACHE_WAIT;
	 if (verbosity >= 3)
	    $display ("    rl_ptw_rd_B: -> STATE_PTE_RD_CACHE_WAIT");
      end
   endrule
   
   // Wait for cache miss to be serviced
   rule rl_ptw_rd_wait (crg_state [0] == STATE_PTE_RD_CACHE_WAIT);
      if (verbosity >= 3)
	 $display ("%0d: %m.rl_ptw_rd_wait", cur_cycle);

      if (cache.mv_refill_ok)
	 crg_state [0] <= STATE_PTE_RD_B;
      else begin
	 let ptw_mem_rsp = PTW_Mem_Rsp {ok: False, pte: ?};
	 ptw.mem_client.response.put (ptw_mem_rsp);
	 crg_state [0] <= STATE_MAIN;
      end
   endrule
`endif

   // ****************************************************************
   // ****************************************************************
   // Modified-PTE write-backs: Service PTE writebacks to the cache
   // (PTE 'A' (accessed) and 'D' (dirty) bits can be modified)

`ifdef ISA_PRIV_S
   // ----------------
   // Merge PTE writeback requests from IMem and DMem
   // From I_MMU_Cache
   FIFOF #(Tuple2 #(PA, WordXL)) f_imem_pte_writebacks <- mkFIFOF;
   // Merged from I_MMU_Cache and D_MMU_Cache
   FIFOF #(Tuple2 #(PA, WordXL)) f_pte_writebacks <- mkFIFOF;

   mkConnection (toGet (f_imem_pte_writebacks), toPut (f_pte_writebacks));
   mkConnection (toGet (f_dmem_pte_writebacks), toPut (f_pte_writebacks));

   // ----------------

   match { .pte_writeback_pa, .pte_writeback_pte } = f_pte_writebacks.first;

   // Service PTE writeback requests
   // The rules _VA and _PA are for the two-phase requests to the cache,
   // normally with the VA followed by the PA, but in this case
   // they're the same.

   // Phase A
   rule rl_pte_wb_req_A ((crg_state [0] == STATE_MAIN)
			 && (crg_mmu_cache_req_state [0] == REQ_STATE_EMPTY));
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_pte_wb_req_A: cache request pte_pa %0h pte %0h",
		   cur_cycle, pte_writeback_pa, pte_writeback_pte);

      // Start the cache probe
      cache.ma_request_va (truncate (pte_writeback_pa));
      crg_state [0] <= STATE_PTE_WR_B;
   endrule
   
   // Phase B
   rule rl_pte_wb_req_B (crg_state [0] == STATE_PTE_WR_B);
      let req = MMU_Cache_Req {op:          CACHE_ST,
			       f3:          ((xlen == 32) ? 3'b010 : 3'b011),
			       va:          truncate (pte_writeback_pa),
			       st_value:    zeroExtend (pte_writeback_pte),
			       amo_funct7:  0,
			       priv:        m_Priv_Mode,
			       sstatus_SUM: 0,
			       mstatus_MXR: 0,
			       satp:        0};
      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_pte_wb_req_B: cache request", cur_cycle);
	 $display ("    ", fshow_MMU_Cache_Req (req));
      end
      
      let cache_result <- cache.mav_request_pa (req, pte_writeback_pa);
      if (verbosity >= 3)
	 $display ("    ", fshow_Cache_Result (cache_result));
      
      if (cache_result.outcome == CACHE_WRITE_HIT) begin
	 crg_state [0] <= STATE_MAIN;    // No response expected for writes
	 f_pte_writebacks.deq;
      end
      else // Miss
	 crg_state [0] <= STATE_PTE_WR_CACHE_WAIT;
   endrule
   
   // Wait for cache miss to be serviced
   rule rl_pte_wb_cache_WAIT (crg_state [0] == STATE_PTE_WR_CACHE_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_pte_wb_cache_WAIT", cur_cycle);

      if (! cache.mv_refill_ok) begin
	 // Assertion failure: we should never see a cache error response
	 // (this PTE was read earlier successfully from the cache)
	 $display ("%0d: %m.rl_pte_wb_req_cache_WAIT: ERROR: unexpected cache error response",
		   cur_cycle);
	 $display ("    pte_pa %0d  pa %0h", pte_writeback_pa, pte_writeback_pte);
	 $finish (1);
      end
      crg_state [0] <= STATE_PTE_WR_B;
   endrule
`endif

   // ****************************************************************
   // ****************************************************************
   // INTERFACE
   // ****************************************************************
   // ****************************************************************

   // CPU interface: request
   // NOTE: this has no flow control: CPU should only invoke it when consuming prev output.
   // As soon as this method is called, the module starts working on this new request.
   method Action ma_req (CacheOp    op,
			 Bit #(3)   f3,
`ifdef ISA_A
			 Bit #(7)   amo_funct7,
`endif
			 WordXL     va,
			 Bit #(64)  st_value,
			 // The following  args for VM
			 Priv_Mode  priv,
			 Bit #(1)   sstatus_SUM,
			 Bit #(1)   mstatus_MXR,
			 WordXL     satp);         // = { VM_Mode, ASID, PPN_for_page_table }

      let mmu_cache_req = MMU_Cache_Req {op:          op,
					 f3:          f3,
					 va:          va,
					 st_value:    st_value
`ifdef ISA_A
				       , amo_funct7:  amo_funct7
`endif
`ifdef ISA_PRIV_S
				       , priv:        priv,
					 sstatus_SUM: sstatus_SUM,
					 mstatus_MXR: mstatus_MXR,
					 satp:        satp
`endif
					 };
      wire_mmu_cache_req <= mmu_cache_req;
   endmethod

   method Bool  valid;
      return crg_valid [0];
   endmethod

   method WordXL  addr;    // req addr for which this is a response
      return crg_mmu_cache_req [0].va;
   endmethod

   method Bit #(64)  word64;
      return crg_ld_val [0];
   endmethod

   method Bit #(64)  st_amo_val;
      return crg_final_st_val [0];
   endmethod

   method Bool  exc;
      return crg_exc [0];
   endmethod

   method Exc_Code  exc_code;
      return crg_exc_code [0];
   endmethod

   // Flush request/response
   interface Server flush_server = toGPServer (f_cache_flush_reqs, f_cache_flush_rsps);

`ifdef ISA_PRIV_S
   // TLB flush
   method Action tlb_flush () = tlb.ma_flush;

   // Service PTW requests from I_MMU_Cache
   interface Server imem_ptw_server = ptw.imem_server;

   // Service PTE-writeback requests from I_MMU_Cache
   interface Put    imem_pte_writeback_p = toPut (f_imem_pte_writebacks);
`endif

   // ----------------
   // Cache-line interface facing next level cache or memory
   // (for refills, writebacks, downgrades, ...)

   interface l1_to_l2_client = cache.l1_to_l2_client;
   interface l2_to_l1_server = cache.l2_to_l1_server;

   // ----------------
   // MMIO interface facing memory

   interface mmio_client = mmio.mmio_client;

   // ----------------------------------------------------------------
   // For ISA tests: watch memory writes to <tohost> addr (see NOTE: "tohost" above)

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
      rg_watch_tohost <= watch_tohost;
      rg_tohost_addr  <= tohost_addr;
      $display ("%0d: %m.set_watch_tohost: watch %0d, addr %0h",
		cur_cycle, watch_tohost, tohost_addr);
   endmethod

   method Bit #(64) mv_tohost_value;
      return rg_tohost_value;
   endmethod
`endif

endmodule: mkD_MMU_Cache

// ================================================================

endpackage: D_MMU_Cache
