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
//  - Cacheable:   request goes to the cache logic
//                   (back end of cache logic talks to fabric interface)
//  - Uncacheable: request does directly to fabric interface
//
// For cacheable addrs:
//    Provide the MMU-translated PA to the cache for tag-matching, and
//    wait if miss (wait until hit/err).
//
// For non-cacheable addrs:
//     Loads and Stores go directly to the fabric.
//     LR/SC are not supported (SC always fails).
//     AMO ops do a read-modify-write across the fabric (CAVEAT: there
//     is no 'locking' of the location at memory during the operation,
//     so it may not really be atomic.)
//
// Interfaces:
// This MMU_Cache is parameterized for data-width on both the front
// side interface (facing CPU) and the back side interface (facing
// fabric).
// CPU-facing interface: can be used for both RV32 and RV64 CPUs.
// RV32 vs. RV64 only affects width of some CPU-side interface
// ports:
//    - inputs req 'addr' and 'satp'    (type WordXL)
//    - output response 'addr' (copy of requesting addr)    (type WordXL)
//    - output response load-value and input request store-value are
//        always 64b because of double-precision floating point LD/ST
//        in RV32
// Fabric-facing interface: AXI4, with data width 32b or 64b (type Wd_Data).

// ----------------
// NOTE: "tohost"
// Special (fragile) ad hoc support for standard ISA tests during
// simulation: watch writes to physical addr <tohost> and stop on
// non-zero write.  This activity is done here rather than at memory
// because, in the standard ISA tests, the <tohost> addr is within the
// cacheable memory region, and therefore may never be written back to
// memory.  The actual address is supplied via the 'set_watch_tohost'
// method.  Standard ISA tests terminate by writing a non-zero value
// to the <tohost> addr. Bit [0] is always 1. Bits [n:1] specify which
// specific sub-test within the test failed.

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

import Cur_Cycle     :: *;
import GetPut_Aux    :: *;

// ================================================================
// Project imports

import ISA_Decls    :: *;
import Near_Mem_IFC :: *;

import SoC_Map      :: *;

import MMU_Cache_Common :: *;

`ifdef ISA_PRIV_S
import TLB :: *;
import PTW :: *;
`endif

import Cache                  :: *;
import MMIO                   :: *;
import MMU_Cache_AXI4_Adapter :: *;

import AXI4_Types  :: *;
import Fabric_Defs :: *;

// ================================================================

export  D_MMU_Cache_IFC (..),  mkD_MMU_Cache;

// ================================================================
// MODULE INTERFACE

interface D_MMU_Cache_IFC;
   // CPU interface: request
   (* always_ready *)
   method Action  req (CacheOp    op,
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
		       WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }


   // CPU interface: response
   (* always_ready *)  method Bool       valid;
   (* always_ready *)  method WordXL     addr;        // req addr for which this is a response
   (* always_ready *)  method Bit #(64)  word64;      // rd_val data for LD, LR, AMO, SC success/fail result)
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

   // Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

`ifdef WATCH_TOHOST
   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr (see NOTE: "tohost" above)

   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
   method Bit #(64) mv_tohost_value;
`endif

   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;

endinterface

// ****************************************************************
// ****************************************************************
// ****************************************************************
// Internal types and constants

// The FSMs below compete for the cache, each with a VA-PA-WAIT sequence.
//     VA: Register the VA (virt addr) to probe the cache RAMs (virtual addr)
//     PA: Cache SRAM output tag-match with PA (phys addr), >=1 cycle after VA step
//     WAIT: Wait for cache refill, if PA step resulted in a miss, and retry PA

// ----------------
// CPU requests

typedef enum {FSM_MAIN_IDLE,          // No active request
              FSM_MAIN_PA,            // If TLB hit, probe cache with PA
	      FSM_MAIN_CACHE_WAIT,    // On cache miss wait for cache to refill
	      FSM_MAIN_MMIO_WAIT      // Wait for MMIO response

`ifdef ISA_PRIV_S
	    , FSM_MAIN_PTW_START,     // Send PTW request
	      FSM_MAIN_PTW_FINISHED   // Resume after PTW response
`endif
   } FSM_MAIN_State
deriving (Bits, Eq, FShow);

function Bool fv_fsm_main_interruptable (FSM_MAIN_State  state);
   return ((state == FSM_MAIN_IDLE) || (state == FSM_MAIN_PA));
endfunction

function Bool fv_fsm_main_interruptable_2 (FSM_MAIN_State  state);
   return (state != FSM_MAIN_CACHE_WAIT);
endfunction

// ----------------
// Cache-flush server state

typedef enum {FSM_FLUSH_IDLE, FSM_FLUSHING} FSM_Flush_State
deriving (Bits, Eq, FShow);

// ----------------
// Memory requests to the cache during Page-Table-Walks (PTWs)

`ifdef ISA_PRIV_S
typedef enum {FSM_PTW_IDLE,           // No active PTW mem request
	      FSM_PTW_MEM_REQ_PA,     // Finish cache access. Hit: respond to PTW -> A; Miss -> C
	      FSM_PTW_MEM_REQ_WAIT    // Cache refill finished; ok: -> B, err: respond to PTW -> B
   } FSM_PTW_State
deriving (Bits, Eq, FShow);

// ----------------
// Memory requests to the cache to writeback modified Page-Table-Entries (PTEs)

typedef enum {FSM_PTE_WB_IDLE,    // No active PTE mem request
	      FSM_PTE_WB_PA,      // Finish cache access. Hit: done -> A; Miss -> C
	      FSM_PTE_WB_WAIT     // Cache refill finished; ok: -> B, err: assertion failure
   } FSM_PTE_WB_State
deriving (Bits, Eq, FShow);
`endif

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

   // Verbosity: 0: quiet
   //            1: Requests and responses
   //            2: rule firings
   //            3: + detail
   Reg #(Bit #(3)) verbosity <- mkReg (0);
   Integer verbosity_cache        = 0;
   Integer verbosity_axi4_adapter = 0;

   // Major sub-modules
`ifdef ISA_PRIV_S
   TLB_IFC                     tlb          <- mkTLB;
   PTW_IFC                     ptw          <- mkPTW;
`endif
   Cache_IFC                   cache        <- mkCache (fromInteger (verbosity_cache));
   MMIO_IFC                    mmio         <- mkMMIO;
   MMU_Cache_AXI4_Adapter_IFC  axi4_adapter <- mkMMU_Cache_AXI4_Adapter (fromInteger (verbosity_axi4_adapter));

   // ================================================================
   // Major connections between modules

   // Cache's memory interface to AXI4 fabric adapter
   mkConnection (cache.g_mem_req,       axi4_adapter.p_mem_line_req);
   mkConnection (cache.g_write_data,    axi4_adapter.p_mem_line_write_data);
   mkConnection (cache.p_mem_read_data, axi4_adapter.g_mem_line_read_data);

   // MMIO's memory interface to AXI4 fabric adapter
   mkConnection (mmio.g_mem_req,       axi4_adapter.p_mem_single_req);
   mkConnection (mmio.g_write_data,    axi4_adapter.p_mem_single_write_data);
   mkConnection (mmio.p_mem_read_data, axi4_adapter.g_mem_single_read_data);

   // ================================================================
   // Overall state of this module

   Reg #(FSM_MAIN_State)   rg_fsm_main_state   <- mkReg (FSM_MAIN_IDLE);
   Reg #(FSM_Flush_State)  rg_fsm_flush_state  <- mkReg (FSM_FLUSH_IDLE);

`ifdef ISA_PRIV_S
   Reg #(FSM_PTW_State)    rg_fsm_ptw_state    <- mkReg (FSM_PTW_IDLE);
   Reg #(FSM_PTE_WB_State) rg_fsm_pte_wb_state <- mkReg (FSM_PTE_WB_IDLE);
`endif

   // SoC_Map is needed for method 'm_is_mem_addr' to distinguish mem
   // (cached) and other (non-cached) addrs
   SoC_Map_IFC soc_map <- mkSoC_Map;

   // Current request from the CPU
   Reg #(MMU_Cache_Req) rg_req <- mkRegU;

   // Phys addr (initially taken from rg_req.va; VM xlation may replace it)
   Reg #(PA)  rg_pa <- mkRegU;

   // Writebacks to mem of PTEs whose PTE.A and/or PTE.D have been modified
   FIFOF #(Tuple2 #(PA, WordXL)) f_dmem_pte_writebacks <- mkFIFOF;

   // ----------------------------------------------------------------
   // Outputs from this module
   // 'final_st_val' is the final stored value for ST, SC, AMO (for verification only)

   Reg #(Bool)      rg_valid        <- mkReg (False);
   Reg #(Bool)      rg_exc          <- mkRegU;
   Reg #(Exc_Code)  rg_exc_code     <- mkRegU;
   Reg #(Bit #(64)) rg_ld_val       <- mkRegU;         // Load-value for LOAD/LR/AMO, success/fail for SC
   Reg #(Bit #(64)) rg_final_st_val <- mkRegU;

   Reg #(Bool)      dw_valid        <- mkDWire (False);
   Reg #(Bool)      dw_exc          <- mkDWire (False);
   Reg #(Exc_Code)  dw_exc_code     <- mkDWire (?);
   Reg #(Bit #(64)) dw_ld_val       <- mkDWire (?);
   Reg #(Bit #(64)) dw_final_st_val <- mkDWire (?);

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
   // FSM_MAIN: Service requests from CPU

   // ================================================================
   // Responses to CPU

   // This Action function drives responses to CPU.

   function Action fa_cpu_response (Bool valid, Bool exc, Exc_Code exc_code,
				    Bit #(64) ld_val, Bit #(64) final_st_val);
      action
	 dw_valid        <= valid;
	 dw_exc          <= exc;
	 dw_exc_code     <= exc_code;
	 dw_ld_val       <= ld_val;
	 dw_final_st_val <= final_st_val;

	 if (valid && (verbosity >= 2)) begin
	    $write ("%0d: %m.fa_cpu_response:", cur_cycle);
	    if (exc)
	       $display (" exc_code: %0d", exc_code);
	    else
	       $display (" ld_val %0h, final_st_val %0h", ld_val, final_st_val);
	 end

`ifdef WATCH_TOHOST
	 // ----------------
	 // "tohost" addr on which to monitor writes, for standard ISA tests.
	 // See NOTE: "tohost" above.
	 if (rg_watch_tohost
	     && valid
	     && (rg_req.op == CACHE_ST)
	     && (zeroExtend (rg_pa) == rg_tohost_addr)
	     && (final_st_val != 0))
	    begin					      
	       rg_tohost_value <= final_st_val;

	       if (verbosity >= 1) begin
		  let test_num = (final_st_val >> 1);
		  $display ("%0d: %m.fa_watch_tohost", cur_cycle);
		  if (test_num == 0) $write ("    PASS");
		  else               $write ("    FAIL <test_%0d>", test_num);
		  $display ("  (<tohost>  addr %0h  data %0h)", rg_pa, final_st_val);
	       end
	    end
`endif
      endaction
   endfunction

   // ================================================================
   // Drive response from registers while Idle

   rule rl_fsm_main_idle (rg_fsm_main_state == FSM_MAIN_IDLE);
      fa_cpu_response (rg_valid, rg_exc, rg_exc_code, rg_ld_val, rg_final_st_val);
   endrule

   // ================================================================
   // This rule is basically the body of method ma_req; decoupling
   // through a wire affords scheduling flexibility.
   // Registers an incoming request and starts the cache probe with
   // the VA.

   // WARNING: the 'ma_req' method (and by implication this rule)
   // should only be invoked by the environment when it has collected
   // the response from the last request, i.e., never override a
   // request with a new one while the current request is still being
   // serviced.

   Wire #(MMU_Cache_Req) wire_mmu_cache_req <- mkWire;

   (* fire_when_enabled *)
   rule rl_req;
      let mmu_cache_req = wire_mmu_cache_req;
      if (verbosity >= 1) begin
	 $display ("%0d: %m.ma_req", cur_cycle);
	 $display ("    Req: ", fshow_MMU_Cache_Req (mmu_cache_req));
      end

      // Register it here and in MMIO module
      rg_req <= mmu_cache_req;
      mmio.req (mmu_cache_req);

      // Initial default PA assumes no VM translation
      rg_pa <= fn_WordXL_to_PA (mmu_cache_req.va);

      if (! fn_is_aligned (mmu_cache_req.f3 [1:0], mmu_cache_req.va)) begin
	 // Misaligned accesses not supported
	 rg_valid      <= True;
	 rg_exc        <= True;
	 rg_exc_code   <= fv_exc_code_misaligned (mmu_cache_req);
	 rg_fsm_main_state <= FSM_MAIN_IDLE;
      end
      else begin
	 // Start cache probe with VA
	 cache.ma_request_va (mmu_cache_req.va);
	 // Follow up with PA
	 rg_fsm_main_state <= FSM_MAIN_PA;
      end
   endrule

`ifdef ISA_PRIV_S
   // VM translation (VA to PA)
   VM_Xlate_Result vm_xlate_result = tlb.mv_vm_xlate (rg_req.va,
						      rg_req.satp,
						      True,    // dmem_not_imem,
						      ((rg_req.op == CACHE_LD)
						       || fv_is_AMO_LR (rg_req)),
						      rg_req.priv,
						      rg_req.sstatus_SUM,
						      rg_req.mstatus_MXR);
`else
   // In non-VM, translation result (PA) is same as VA
   VM_Xlate_Result vm_xlate_result = VM_Xlate_Result {outcome: VM_XLATE_OK,
						      pa:      rg_req.va};
`endif

   // Note: PTW and PTE Writebacks from I_MMU_Cache arrive
   // asynchronously w.r.t. data stream, and may be occupying the
   // cache.  The latter two conditions below stall this rule if so.

   Reg #(Bit #(4)) rg_ctr <- mkRegU;
   rule rl_count;
      rg_ctr <= rg_ctr + 1;
   endrule

`ifdef ISA_PRIV_S
   Bool pt_idle = ((rg_fsm_ptw_state == FSM_PTW_IDLE)
		   && (rg_fsm_pte_wb_state == FSM_PTE_WB_IDLE));
`else
   Bool pt_idle = True;
`endif

   rule rl_fsm_main_PA ((rg_fsm_main_state == FSM_MAIN_PA)
			&& (rg_ctr != 0)
			&& (rg_fsm_flush_state == FSM_FLUSH_IDLE)
			&& pt_idle);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_fsm_main_PA:\n    ", cur_cycle, fshow_MMU_Cache_Req (rg_req));

      if (verbosity >= 3)
	 $display ("    ", fshow_VM_Xlate_Result (vm_xlate_result));
      rg_pa <= vm_xlate_result.pa;

`ifdef ISA_PRIV_S
      // ---- TLB miss
      if (vm_xlate_result.outcome == VM_XLATE_TLB_MISS) begin
	 rg_fsm_main_state <= FSM_MAIN_PTW_START;
      end

      // ---- TLB translation exception
      else if (vm_xlate_result.outcome == VM_XLATE_EXCEPTION) begin
	 rg_valid          <= True;
	 rg_exc            <= True;
	 rg_exc_code       <= vm_xlate_result.exc_code;
	 rg_fsm_main_state <= FSM_MAIN_IDLE;
      end

      // ---- TLB success
      else
`endif
	 begin
	    dynamicAssert ((vm_xlate_result.outcome == VM_XLATE_OK), "FAIL: unknown vm_xlate result");

`ifdef ISA_PRIV_S
	    // If PTE A, D bits modified ...
	    if (vm_xlate_result.pte_modified) begin
	       // Update the TLB
	       ASID asid = fn_satp_to_ASID (rg_req.satp);
	       VPN  vpn  = fn_Addr_to_VPN  (rg_req.va);
	       tlb.ma_insert (asid,
			      vpn,
			      vm_xlate_result.pte,
			      vm_xlate_result.pte_level,
			      vm_xlate_result.pte_pa);
	       // Writeback the modified PTE to memory	
	       // Enqueue it to be written back to memory
	       f_dmem_pte_writebacks.enq (tuple2 (vm_xlate_result.pte_pa, vm_xlate_result.pte));
	       if (verbosity >= 1)
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
	       let cache_result <- cache.mav_request_pa (rg_req, vm_xlate_result.pa);
	       if (cache_result.outcome == CACHE_MISS) begin
		  rg_fsm_main_state <= FSM_MAIN_CACHE_WAIT;
		  if (verbosity >= 3)
		     $display ("    Cache Miss: waiting for refill ...");
	       end
	       else begin    // Cache hit
		  if (cache_result.outcome == CACHE_READ_HIT) begin
		     // Drive response immediately
		     // (and until the next request changes something
		     // we'll remain in this state indefinitely)
		     fa_cpu_response (True,                         // valid
				      False,                        // exc
				      ?,                            // exc_code
				      cache_result.final_ld_val,
				      cache_result.final_st_val);
		  end
		  else if (cache_result.outcome == CACHE_WRITE_HIT) begin
		     // Provide response only after a cycle to avoid
		     // SRAM conflicts, if the request is a read for
		     // the same SRAM address.
		     rg_valid          <= True;
		     rg_exc            <= False;
		     rg_ld_val         <= cache_result.final_ld_val;
		     rg_final_st_val   <= cache_result.final_st_val;
		     rg_fsm_main_state <= FSM_MAIN_IDLE;
		  end
	       end
	    end

	    // Address is for non-memory (I/O, non-cacheable)
	    else begin
	       mmio.start (vm_xlate_result.pa);
	       rg_fsm_main_state <= FSM_MAIN_MMIO_WAIT;
	       if (verbosity >= 3)
		  $display ("    MMIO started; goto FSM_MAIN_MMIO_WAIT");
	    end
	 end
   endrule: rl_fsm_main_PA

   // ================================================================
   // Wait for cache to finish refill, then try again or drive exception

   rule rl_fsm_main_cache_WAIT (rg_fsm_main_state == FSM_MAIN_CACHE_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_fsm_main_cache_WAIT: awaiting cache refill", cur_cycle);

      if (cache.mv_refill_ok)
	 rg_fsm_main_state <= FSM_MAIN_PA;
      else begin
	 rg_valid          <= True;
	 rg_exc            <= True;
	 rg_exc_code       <= fv_exc_code_access_fault (rg_req);
	 rg_fsm_main_state <= FSM_MAIN_IDLE;
      end
   endrule

   // ================================================================
   // Wait until mmio.result is available.
   // If no error, drive response.
   // If error, go to drive exception rsponse.

   rule rl_fsm_main_mmio_WAIT (rg_fsm_main_state == FSM_MAIN_MMIO_WAIT);
      if (verbosity >= 2)
	 $display ("%d: %m.rl_fsm_main_mmio_WAIT", cur_cycle);

      match { .err, .ld_val, .final_st_val } = mmio.result;

      rg_valid          <= True;
      rg_ld_val         <= ld_val;
      rg_final_st_val   <= final_st_val;
      rg_exc            <= err;
      rg_exc_code       <= fv_exc_code_access_fault (rg_req);
      rg_fsm_main_state <= FSM_MAIN_IDLE;
   endrule

   // ================================================================
   // On TLB miss, do a PTW, then try again or go to exception.

`ifdef ISA_PRIV_S
   rule rl_fsm_main_PTW_start (rg_fsm_main_state == FSM_MAIN_PTW_START);
      let ptw_req = PTW_Req {va: rg_req.va, satp: rg_req.satp};
      ptw.dmem_server.request.put (ptw_req);
      rg_fsm_main_state <= FSM_MAIN_PTW_FINISHED;
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_fsm_main_PTW_start:\n    ", cur_cycle, fshow (ptw_req));
   endrule

   rule rl_fsm_main_PTW_finished (rg_fsm_main_state == FSM_MAIN_PTW_FINISHED);
      let ptw_rsp <- ptw.dmem_server.response.get;

      if (ptw_rsp.result == PTW_OK) begin
	 // Insert into TLB
	 ASID asid = fn_satp_to_ASID (rg_req.satp);
	 VPN  vpn  = fn_Addr_to_VPN  (rg_req.va);
	 tlb.ma_insert (asid, vpn, ptw_rsp.pte, ptw_rsp.level, ptw_rsp.pte_pa);

	 // Note: since PTW uses the cache, will klobber cache's
	 // request variables, so re-request with va again here.
	 cache.ma_request_va (rg_req.va);
	 rg_fsm_main_state <= FSM_MAIN_PA;
	 if (verbosity >= 2)
	    $display ("%0d: %m.rl_fsm_main_PTW_finished: ok; retry", cur_cycle);
      end
      else begin
	 rg_valid <= True;
	 rg_exc   <= True;
	 if (ptw_rsp.result == PTW_ACCESS_FAULT) begin
	    if (verbosity >= 2)
	       $display ("%0d: %m.rl_fsm_main_PTW_finished: ACCESS FAULT", cur_cycle);
	    rg_exc_code <= fv_exc_code_access_fault (rg_req);
	 end
	 else begin // PTW_PAGE_FAULT
	    if (verbosity >= 2)
	       $display ("%0d: %m.rl_fsm_main_PTW_finished: PAGE FAULT", cur_cycle);
	    rg_exc_code <= fv_exc_code_page_fault (rg_req);
	 end
	 rg_fsm_main_state <= FSM_MAIN_IDLE;
      end
   endrule
`endif

   // ****************************************************************
   // ****************************************************************
   // CACHE FLUSH

   FIFOF #(Bit #(1))  f_cache_flush_reqs <- mkFIFOF;
   FIFOF #(Bit #(0))  f_cache_flush_rsps <- mkFIFOF;

   rule rl_cache_flush_start ((rg_fsm_flush_state == FSM_FLUSH_IDLE)
			      && fv_fsm_main_interruptable (rg_fsm_main_state));
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_cache_flush_start", cur_cycle);

      let to_state_code = f_cache_flush_reqs.first;
      cache.flush_server.request.put (to_state_code);
      rg_fsm_flush_state <= FSM_FLUSHING;

      // Set FSM_MAIN state to IDLE so it won't re-do last request.
      rg_fsm_main_state <= FSM_MAIN_IDLE;
   endrule

   rule rl_cache_flush_finish (rg_fsm_flush_state == FSM_FLUSHING);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_cache_flush_finish", cur_cycle);

      f_cache_flush_reqs.deq;
      let x <- cache.flush_server.response.get;
      f_cache_flush_rsps.enq (?);
      rg_fsm_flush_state <= FSM_FLUSH_IDLE;
   endrule

   // ****************************************************************
   // ****************************************************************
   // FSM_PTW: service PTW memory requests (which are reads, only)
   // from the cache

`ifdef ISA_PRIV_S
   // Holds request between _A and _B rules
   // TODO: if PTW had a SemiFIFOF interface instead of Get, we wouldn't need this reg.
   Reg #(PTW_Mem_Req) rg_ptw_mem_req <- mkRegU;

   // Phase 1 (VA)
   (* descending_urgency = "rl_fsm_main_PA, rl_fsm_ptw_mem_req_VA" *)
   rule rl_fsm_ptw_mem_req_VA ((rg_fsm_ptw_state == FSM_PTW_IDLE)
			       && fv_fsm_main_interruptable_2 (rg_fsm_main_state));
      let ptw_mem_req <- ptw.mem_client.request.get;
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_fsm_ptw_mem_req_VA: cache request va %0h",
		   cur_cycle, ptw_mem_req.pte_pa);

      // Start the cache RAM probe with "va" (= pte_pa)
      cache.ma_request_va (truncate (ptw_mem_req.pte_pa));
      rg_ptw_mem_req   <= ptw_mem_req;
      rg_fsm_ptw_state <= FSM_PTW_MEM_REQ_PA;
   endrule
   
   // Phase 2 (PA)
   rule rl_fsm_ptw_mem_req_PA (rg_fsm_ptw_state == FSM_PTW_MEM_REQ_PA);
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
	 $display ("%0d: %m.rl_fsm_ptw_req_PA: cache request", cur_cycle);
	 $display ("    ", fshow_MMU_Cache_Req (req));
      end
      
      let cache_result <- cache.mav_request_pa (req, rg_ptw_mem_req.pte_pa);
      if (verbosity >= 2)
	 $display ("    ", fshow_Cache_Result (cache_result));
      
      if (cache_result.outcome == CACHE_READ_HIT) begin
	 let ptw_mem_rsp = PTW_Mem_Rsp {ok: True, pte: truncate (cache_result.final_ld_val)};
	 ptw.mem_client.response.put (ptw_mem_rsp);
	 rg_valid         <= False;
	 rg_fsm_ptw_state <= FSM_PTW_IDLE;
      end
      else // Miss (CACHE_MISS only; CACHE_WRITE_HIT not possible in PTW)
	 rg_fsm_ptw_state <= FSM_PTW_MEM_REQ_WAIT;
   endrule
   
   // Wait for cache miss to be serviced
   rule rl_fsm_ptw_mem_req_wait (rg_fsm_ptw_state == FSM_PTW_MEM_REQ_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_fsm_ptw_mem_req_wait", cur_cycle);

      if (cache.mv_refill_ok)
	 rg_fsm_ptw_state <= FSM_PTW_MEM_REQ_PA;
      else begin
	 let ptw_mem_rsp = PTW_Mem_Rsp {ok: False, pte: ?};
	 ptw.mem_client.response.put (ptw_mem_rsp);
	 rg_valid         <= False;
	 rg_fsm_ptw_state <= FSM_PTW_IDLE;
      end
   endrule
`endif

   // ****************************************************************
   // ****************************************************************
   // FSM_PTE_WB: Service PTE writebacks to the cache

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

   // Phase 1 (VA)
   (* descending_urgency = "rl_fsm_main_PA, rl_fsm_pte_wb_req_VA" *)
   rule rl_fsm_pte_wb_req_VA ((rg_fsm_pte_wb_state == FSM_PTE_WB_IDLE)
			      && fv_fsm_main_interruptable (rg_fsm_main_state));
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_fsm_pte_wb_req_VA: cache request pte_pa %0h pte %0h",
		   cur_cycle, pte_writeback_pa, pte_writeback_pte);

      // Start the cache probe
      cache.ma_request_va (truncate (pte_writeback_pa));
      rg_fsm_pte_wb_state <= FSM_PTE_WB_PA;
   endrule
   
   // Phase 2 (PA)
   rule rl_fsm_pte_wb_req_PA (rg_fsm_pte_wb_state == FSM_PTE_WB_PA);
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
	 $display ("%0d: %m.rl_fsm_pte_wb_req_PA: cache request", cur_cycle);
	 $display ("    ", fshow_MMU_Cache_Req (req));
      end
      
      let cache_result <- cache.mav_request_pa (req, pte_writeback_pa);
      if (verbosity >= 2)
	 $display ("    ", fshow_Cache_Result (cache_result));
      
      if (cache_result.outcome == CACHE_WRITE_HIT) begin
	 rg_valid      <= False;
	 rg_fsm_pte_wb_state <= FSM_PTE_WB_IDLE;    // No response expected for writes
	 f_pte_writebacks.deq;
      end
      else // Miss
	 rg_fsm_pte_wb_state <= FSM_PTE_WB_WAIT;
   endrule
   
   // Wait for cache miss to be serviced
   rule rl_fsm_pte_wb_req_cache_WAIT (rg_fsm_pte_wb_state == FSM_PTE_WB_WAIT);
      if (verbosity >= 2)
	 $display ("%0d: %m.rl_fsm_pte_wb_req_cache_WAIT", cur_cycle);

      if (! cache.mv_refill_ok) begin
	 // Assertion failure: we should never see a cache error response
	 // (this PTE was read earlier successfully from the cache)
	 $display ("%0d: %m.rl_fsm_pte_wb_req_cache_WAIT: ERROR: unexpected cache error response",
		   cur_cycle);
	 $display ("    pte_pa %0d  pa %0h", pte_writeback_pa, pte_writeback_pte);
	 $finish (1);
      end
      rg_fsm_pte_wb_state <= FSM_PTE_WB_PA;
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
   method Action  req (CacheOp    op,
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

      let cache_req = MMU_Cache_Req {op:          op,
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
      wire_mmu_cache_req <= cache_req;
   endmethod

   method Bool  valid;
      return dw_valid;
   endmethod

   method WordXL  addr;    // req addr for which this is a response
      return rg_req.va;
   endmethod

   method Bit #(64)  word64;
      return dw_ld_val;
   endmethod

   method Bit #(64)  st_amo_val;
      return dw_final_st_val;
   endmethod

   method Bool  exc;
      return dw_exc;
   endmethod

   method Exc_Code  exc_code;
      return dw_exc_code;
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

   // Fabric master interface
   interface AXI4_Master_IFC mem_master = axi4_adapter.mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
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

   // Signal that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;
      axi4_adapter.ma_ddr4_ready;
   endmethod

   // Misc. status; 0 = running, no error
   method Bit #(8) mv_status;
      // Note: currently on looking at write-errors, which only happens in DMem
      return axi4_adapter.mv_status;
   endmethod

endmodule: mkD_MMU_Cache

// ================================================================

endpackage: D_MMU_Cache
