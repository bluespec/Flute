// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package Cache;

// ================================================================
// Organization: in-order, blocking, "write-back" policy:
//    On a miss,
//      - A victim way is identified in the addressed cache set
//      - If the cache line is MODIFIED, it is written back ("writeback")
//      - The required cache line is loaded ("refill")

// Storage is in two separate SRAMs, the tag-ram and the data ram.
// Each tag ram address holds a set of tags (set-associativity)
// Each data RAM address holds 64b (exploiting RAM-internal muxes) for 64-bit read/write.

// TODO:
//    LR/SC reservation management

// ================================================================
// BSV lib imports

import Vector       :: *;
import BRAMCore     :: *;
import FIFOF        :: *;
import ClientServer :: *;
import GetPut       :: *;
import Assert       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import Near_Mem_IFC     :: *;
import Cache_Decls      :: *;
import MMU_Cache_Common :: *;

// ================================================================

export  Cache_Result_Type (..), Cache_Result (..), fshow_Cache_Result;
export  Cache_IFC (..);
export  mkCache;

// ================================================================
// MODULE INTERFACE

typedef enum { CACHE_MISS, CACHE_READ_HIT, CACHE_WRITE_HIT } Cache_Result_Type
deriving (Bits, Eq, FShow);

typedef struct {
   Cache_Result_Type  outcome;
   Bit #(64)          final_ld_val;
   Bit #(64)          final_st_val;
   } Cache_Result
deriving (Bits, FShow);

function Fmt fshow_Cache_Result (Cache_Result result);
   return case (result.outcome)
	     CACHE_MISS:      $format ("CACHE_MISS");
	     CACHE_READ_HIT:  $format ("CACHE_READ_HIT (ld_val %0h)",  result.final_ld_val);
	     CACHE_WRITE_HIT: $format ("CACHE_WRITE_HIT (st_val %0h)", result.final_st_val);
	  endcase;
endfunction

// ----------------

interface Cache_IFC;
   // This starts a new request (with virt addr)
   // while the virt addr is being translated to a phys addr
   (* always_ready *)
   method Action ma_request_va (WordXL va);

   // This completes a new request with the phys addr
   method ActionValue #(Cache_Result) mav_request_pa (MMU_Cache_Req req, PA pa);

   // ----------------
   // Indicates cache is IDLE, ready for new request

   (* always_ready *)
   method Bool mv_is_idle;

   // ----------------
   // Stalls until refill done and then returns ok (True) or error (False)
   method Bool mv_refill_ok ();

   // ----------------
   // Cache flush request/response
   // Bit #(1) request specifies new meta-state: 0=INVALID, 1=CLEAN

   interface Server #(Bit #(1), Token) flush_server;

   // ----------------
   // Interface to next level cache or memory (for refills, writebacks, downgrades, ...)

   interface Client_Semi_FIFOF #(L1_to_L2_Req, L2_to_L1_Rsp)  l1_to_l2_client;
   interface Server_Semi_FIFOF #(L2_to_L1_Req, L1_to_L2_Rsp)  l2_to_l1_server;
endinterface

// ================================================================
// Constants used in RAM instantiation and access API

Bool bram_with_output_reg = False;
Bool bram_cmd_read  = False;
Bool bram_cmd_write = True;

// ================================================================
// Overall state of the cache module FSM

typedef enum {FSM_INITIALIZE,
	      FSM_IDLE,

	      FSM_REPLACE_START,
	      FSM_WRITEBACK_LOOP,

              FSM_REFILL_START,           // On cache miss, initiate refill of cache line in cache
              FSM_UPGRADE_REFILL,
              FSM_REFILL_FINAL_DELAY,     // 1-cycle delay after refill due to SRAM-write requirement

	      FSM_DOWNGRADE_B,            // Kick off writeback if necessary
	      FSM_DOWNGRADE_C,            // Final step after downgrade-writeback

	      FSM_FLUSH_LOOP,
	      FSM_FLUSH_LOOP_WRITEBACK_SEQUEL
   } FSM_State
deriving (Bits, Eq, FShow);

// ================================================================
// Cache entries and sets
// A cache entry is: meta information (state, ctag) and data

typedef struct {
   Meta_State  state;
   CTag        ctag;
   } Meta
deriving (Bits, FShow);

// A CSet of Meta information
typedef Vector #(Ways_per_CSet, Meta)  CSet_Meta;

// A CSet of CWords
typedef Vector #(Ways_per_CSet, Bit #(64)) CSet_CWord;

function PA fn_cline_pa_from_tag_and_cset_in_cache (CTag  ctag, CSet_in_Cache  cset_in_cache);
   Byte_in_CLine byte_in_cline = 0;
   PA            cline_pa      = { ctag, cset_in_cache, byte_in_cline };
   return cline_pa;
endfunction

function Fmt fshow_cset_meta (CSet_in_Cache  cset_in_cache,
			      CSet_Meta      cset_meta);
   Fmt fmt = $format ("CSet [%0h] (state, tag)\n", cset_in_cache);
   for (Integer j = 0; j < ways_per_cset; j = j + 1) begin
      fmt = fmt + $format ("        way %0d: ", j, fshow (cset_meta [j].state));
      if (cset_meta [j].state != META_INVALID) begin
	 let pa = fn_cline_pa_from_tag_and_cset_in_cache (cset_meta [j].ctag, cset_in_cache);
	 fmt = fmt + $format (", ctag %0h (= pa %0h)", cset_meta [j].ctag, pa);
      end
      fmt = fmt + $format ("\n");
   end
   return fmt;
endfunction

function Fmt fshow_cset_cword (CSet_CWord cset_cword);
   Fmt fmt = $format ("CSet_Cword {");
   for (Integer j = 0; j < ways_per_cset; j = j + 1) begin
      if (j != 0)
	 fmt = fmt + $format (", ");
      fmt = fmt + $format ("%0h", cset_cword [j]);
   end
   fmt = fmt + $format ("}");
   return fmt;
endfunction

// ================================================================
// Choose a victim for eviction
// TODO: improve this, to a per-cset round-robin, or LRU, ...

function Way_in_CSet fn_incr_way (Way_in_CSet w);
   // The extend/truncate trickery below is because Way_in_CSet could
   // be Bit #(0) (in direct-mapped case).  for which the '1' in the
   // '+ 1' expr below is not a valid literal.  Extend/truncate here
   // allows +1 to occur at a minimum of Bit #(1).
   Bit #(TAdd #(1, Bits_per_Way_in_CSet)) tmp = extend (w);
   tmp = tmp + 1;
   Way_in_CSet new_way = truncate (tmp);
   return new_way;
endfunction

function Way_in_CSet fv_choose_victim_way (CSet_Meta    cset_meta,
					   Way_in_CSet  old_way);
   // ----------------
   // Pick a victim 'way'
   // Start by looking for an EMPTY way
   Bool        victim_found = False;
   Way_in_CSet victim_way   = 0;
   for (Integer way = 0; way < ways_per_cset; way = way + 1) begin
      Bool is_empty = (cset_meta [way].state == META_INVALID);
      if ((! victim_found) && is_empty) begin
	 victim_found = True;
	 victim_way   = fromInteger (way);
      end
   end

   // If no EMPTY way found, increment old_way.
   // Note: this victim may be CLEAN or MODIFIED.
   if (! victim_found)
      victim_way = fn_incr_way (old_way);

   return victim_way;
endfunction

// ================================================================
// Help functions for RAM access

// ----------------------------------------------------------------
// RAM read-output hit/miss info

typedef struct {
   Bit #(2)     num_valids;     // # of hits in set (should be 0 or 1; error if > 1)
   Meta_State   valid_state;    // M, E, S, I
   Bit #(64)    data;           // if valid
   Way_in_CSet  way;            // if valid (for subsequent updates)
   } Valid_Info
deriving (Bits, Eq, FShow);

// ----------------------------------------------------------------
// Update a byte, halfword, word or doubleword in a CWord at Way in a CSet_CWord

function CSet_CWord fn_update_cset_cword (CSet_CWord   old_cset_cword,
					  Way_in_CSet   way,
					  Bit #(n)      addr,
					  Bit #(3)      f3,
					  Bit #(64)     cword);
   let old_cword = old_cset_cword [way];
   let old_B0    = old_cword [7:0];
   let old_B1    = old_cword [15:8];
   let old_B2    = old_cword [23:16];
   let old_B3    = old_cword [31:24];
   let old_B4    = old_cword [39:32];
   let old_B5    = old_cword [47:40];
   let old_B6    = old_cword [55:48];
   let old_B7    = old_cword [63:56];

   let new_cset_cword = old_cset_cword;
   let new_cword      = old_cword;
   Bit #(3) addr_lsbs = addr [2:0];

   // Replace relevant bytes in new_cword
   case (f3)
      f3_SB:  case (addr_lsbs)
		 'h0 : new_cword [ 7:0 ] = cword [7:0];
		 'h1 : new_cword [15:8 ] = cword [7:0];
		 'h2 : new_cword [23:16] = cword [7:0];
		 'h3 : new_cword [31:24] = cword [7:0];
		 'h4 : new_cword [39:32] = cword [7:0];
		 'h5 : new_cword [47:40] = cword [7:0];
		 'h6 : new_cword [55:48] = cword [7:0];
		 'h7 : new_cword [63:56] = cword [7:0];
	      endcase
      f3_SH:  case (addr_lsbs)
		 'h0 : new_cword [15:0 ] = cword [15:0];
		 'h2 : new_cword [31:16] = cword [15:0];
		 'h4 : new_cword [47:32] = cword [15:0];
		 'h6 : new_cword [63:48] = cword [15:0];
	      endcase
      f3_SW:  case (addr_lsbs)
		 'h0 : new_cword [31:0]  = cword [31:0];
		 'h4 : new_cword [63:32] = cword [31:0];
	      endcase
      f3_SD:  new_cword = cword;
   endcase
   new_cset_cword [way] = new_cword;
   return new_cset_cword;
endfunction: fn_update_cset_cword

// ================================================================
// MODULE IMPLEMENTATION

(* synthesize *)
module mkCache #(parameter Bool      dcache_not_icache,
		 parameter Bit #(3)  verbosity)
               (Cache_IFC);

   // Verbosity: 0: quiet; 1: rules; 2: loop iterations

   Reg #(FSM_State) rg_fsm_state <- mkReg (FSM_INITIALIZE);

   // 'Writeback' is a subroutine that is invoked on evictions
   // (capacity misses), downgrade requests from L2, and flushes.
   Reg #(Meta_State) rg_post_wb_meta_state <- mkRegU;    // post-writeback cline state
   Reg #(FSM_State)  rg_post_wb_fsm_state  <- mkRegU;    // post-writeback FSM state (continuation)

   // Cache-relevant fields of the current MMU_Cache_Req request
   Reg #(WordXL)   rg_va         <- mkRegU;

   Reg #(CacheOp)  rg_cache_op   <- mkRegU;
   Reg #(Bit #(3)) rg_f3         <- mkRegU;
`ifdef ISA_A
   Reg #(Bit #(7)) rg_amo_funct7 <- mkRegU;
`endif

   // Phys addr
   Reg #(PA)       rg_pa <- mkRegU;             // Physical addr, 1 cycle later and sustained

   // Cache RAMs
   //     Port A used for the main hit/miss path (for MMU_Cache client)
   //     Port B is used for writebacks and refills (to/from main memory)
   // Meta-data RAM
   BRAM_DUAL_PORT #(CSet_in_Cache,
		    CSet_Meta)     ram_cset_meta   <- mkBRAMCore2 (csets_per_cache,
								   bram_with_output_reg);
   // Data RAM
   // Note: a cset_cword is addressed by { cset_in_cache, cword_in_cline },
   BRAM_DUAL_PORT #(CSet_CWord_in_Cache,
		    CSet_CWord)         ram_cset_cword <- mkBRAMCore2 (cset_cwords_per_cache,
								       bram_with_output_reg);

   // ----------------
   // Reservation regs for AMO LR/SC (Load-Reserved/Store-Conditional)

`ifdef ISA_A
   Reg #(Bool)       rg_lrsc_valid <- mkReg (False);
   Reg #(PA)         rg_lrsc_pa    <- mkRegU;    // Phys. address for an active LR
   Reg #(MemReqSize) rg_lrsc_size  <- mkRegU;
`endif

   // ----------------
   // State for choosing next eviction victim
   // TODO: this cache-global state be replaced by per-set state (e.g., LRU, random, ...)

   Reg #(Way_in_CSet) rg_victim_way <- mkReg (0);

   // ----------------
   // Loop-control index registers
   // These are used to loop over csets, ways, and cword-in-lines.
   Reg #(CSet_in_Cache)  rg_cset_in_cache  <- mkReg (0);    // ready for initialization loop
   Reg #(CWord_in_CLine) rg_cword_in_cline <- mkRegU;
   Reg #(Way_in_CSet)    rg_way_in_cset    <- mkRegU;

   // Record if there was a fabric error during any beats of a refill
   Reg #(Bool) rg_error_during_refill <- mkRegU;

   // ----------------
   // Requests/responses to next-level cache or Memory (for upgrades and refills)

   FIFOF #(L1_to_L2_Req)  f_L1_to_L2_reqs <- mkFIFOF;
   FIFOF #(L2_to_L1_Rsp)  f_L2_to_L1_rsps <- mkFIFOF;

   // Buffer to hold a cache line from next-level during refill
   Reg #(Vector #(CWords_per_CLine, Bit #(64))) rg_read_cline_buf <- mkRegU;

   // ----------------
   // Requests/responses from next-level cache or Memory (for downgrades and writebacks)

   FIFOF #(L2_to_L1_Req)  f_L2_to_L1_reqs <- mkFIFOF;
   FIFOF #(L1_to_L2_Rsp)  f_L1_to_L2_rsps <- mkFIFOF;

   // Buffer to hold a cache line to next-level during writeback
   Reg #(Vector #(CWords_per_CLine, Bit #(64))) rg_write_cline_buf <- mkRegU;

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR: RAM Port A outputs continuously indicate hit/miss
   // based on address request on port A and tag-match with physical
   // address dw_pa.  Request is at cset_in_cache address derived from
   // virtual address.

   // ----------------
   // Continuous values derived from rg_va

   let va_cset_in_cache       = fn_Addr_to_CSet_in_Cache (rg_va);
   let va_cword_in_cline      = fn_Addr_to_CWord_in_CLine (rg_va);
   let va_cset_cword_in_cache = fn_Addr_to_CSet_CWord_in_Cache (rg_va);

   // ----------------
   // Continuous output values from RAM ports A
   let ram_A_cset_meta  = ram_cset_meta.a.read;
   let ram_A_cset_cword = ram_cset_cword.a.read;

   // ----------------
   // Valid_Info is a pure combinational function of the A-outputs of
   // the RAMs (current cache set) and the phys addr (tag-match)

   function Valid_Info fv_ram_A_valid_info (PA pa);

      // # of valids in set (should be 0 or 1; error if > 1)
      Bit #(2)     num_valids  = 0;
      Meta_State   valid_state = META_INVALID;    // M, E, S, I
      Way_in_CSet  way_hit     = 0;
      Bit #(64)    cword       = 0;

      CTag  pa_ctag = fn_PA_to_CTag (pa);

      for (Integer way = 0; way < ways_per_cset; way = way + 1) begin
	 let hit_at_way  = (   (ram_A_cset_meta [way].state != META_INVALID)
			    && (ram_A_cset_meta [way].ctag  == pa_ctag));
	 if (hit_at_way && (num_valids < 2)) begin
	    num_valids = num_valids + 1;
	    valid_state = ram_A_cset_meta [way].state;
	    way_hit     = fromInteger (way);
	 end

	 let cword_at_way = ram_A_cset_cword [way];
	 cword  = (cword | (cword_at_way & pack (replicate (hit_at_way))));
      end

      return Valid_Info {num_valids:  num_valids,
			 valid_state: valid_state,
			 data:        cword,
			 way:         way_hit};    // For possible subsequent update
   endfunction

   // ****************************************************************
   // ****************************************************************
   // Request RAMs (on request methods, and when returning to IDLE
   // after refills.

   function Action fa_req_rams_A (WordXL va);
      action
	 // Request meta RAM
	 let cset_in_cache = fn_Addr_to_CSet_in_Cache (va);
	 ram_cset_meta.a.put (bram_cmd_read, cset_in_cache, ?);

	 // Request data RAM
	 let cset_cword_in_cache = fn_Addr_to_CSet_CWord_in_Cache (va);
	 ram_cset_cword.a.put (bram_cmd_read, cset_cword_in_cache, ?);

	 if (verbosity >= 2)
	    $display ("    fa_req_rams_A %0h cset_in_cache %0h, cset_cword_in_cache %0h",
		      va, cset_in_cache, cset_cword_in_cache);
      endaction
   endfunction

   // ****************************************************************
   // ****************************************************************
   // Write actions on a cache hit
   function Action fa_write (PA pa, Bit #(3) f3, Bit #(64) st_value);
      action
	 let valid_info = fv_ram_A_valid_info (pa);
	 let way        = valid_info.way;

	 // Assert: current mv_response is EXCLUSIVE/MODIFIED
	 // Writes data into that currently probed cache line
	 if ((valid_info.num_valids != 1)
	     || (valid_info.valid_state < META_SHARED))
	    begin
	       $display ("%0d: %m.fa_write: INTERNAL ERROR", cur_cycle);
	       $display ("    va_cset_cword_in_cache %0h way %0d pa %0h f3 %0d st_value %0h",
			 va_cset_cword_in_cache, way, pa, f3, st_value);
	       $display ("    Cache write on a miss (need EXCLUSIVE)");
	       $finish (1);
	    end

	 // Update cache line data
	 let new_cset_cword = fn_update_cset_cword (ram_A_cset_cword,
						    way,
						    pa,
						    f3,
						    st_value);
	 ram_cset_cword.b.put (bram_cmd_write, va_cset_cword_in_cache, new_cset_cword);
	 if (verbosity >= 1) begin
	    $display ("      cache.fa_write: va_cset_cword_in_cache %0h way %0d pa %0h f3 %0d st_value %0h",
		      va_cset_cword_in_cache, way, pa, f3, st_value);
	    $display ("      from: ", fshow_cset_cword (ram_A_cset_cword));
	    $display ("      to:   ", fshow_cset_cword (new_cset_cword));
	 end

	 // Update cache meta info to MODIFIED
	 let new_cset_meta = ram_A_cset_meta;
	 new_cset_meta [way] = Meta {state: META_MODIFIED, ctag:  fn_PA_to_CTag (pa)};
	 ram_cset_meta.b.put (bram_cmd_write, va_cset_in_cache, new_cset_meta);
      endaction
   endfunction

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR: CACHE-LINE WRITEBACK (due to evictions, downgrades and flushes)
   // These rules are a "subroutine" to writeback a cache line
   //   - in normal cache operation (writeback before refill)
   //   - downgrade request from L2/Mem
   //   - in flush operations (writeback and stay empty)
   // rg_post_wb_fsm_state specifies fsm state after writeback.
   //
   // function fa_cache_writeback_loop_prequel is called before rl_writeback_loop
   // Preconditions:
   //   - ram_cset_meta.b    has been requested for target cset_in_cache

   function Action fa_cache_writeback_loop_prequel (CTag           tag,
						    CSet_in_Cache  cset_in_cache,
						    Way_in_CSet    way_in_cset,
						    Meta_State     post_wb_meta_state,
						    FSM_State      post_wb_fsm_state);
      action
	 if (verbosity >= 1) begin
	    PA cline_pa = fn_CTag_and_CSet_to_CLine_PA (tag, cset_in_cache);
	    $display ("    fa_cache_writeback_loop_prequel @ %0h (cset %0h, way %0h)",
		      cline_pa, cset_in_cache, way_in_cset);
	 end

	 // What to do after the loop
	 rg_post_wb_meta_state <= post_wb_meta_state;
	 rg_post_wb_fsm_state  <= post_wb_fsm_state;

	 // Request data RAM A for first CSet_CWord for this line
	 CWord_in_CLine       cword_in_cline      = 0;
	 CSet_CWord_in_Cache  cset_cword_in_cache = { cset_in_cache, cword_in_cline };
	 ram_cset_cword.a.put (bram_cmd_read, cset_cword_in_cache, ?);

	 rg_cword_in_cline <= cword_in_cline;
      endaction
   endfunction

   // ----------------
   // rl_writeback_loop:
   // Assume proper setup of loop index regs, rg_post_wb_meta_state, rg_post_wb_fsm_state
   // and that a cword_cset has been requested from data RAM B

   rule rl_writeback_loop (rg_fsm_state == FSM_WRITEBACK_LOOP);
      // Accumulate a cword into rg_write_cline_buf
      CSet_CWord  cset_cword = ram_cset_cword.a.read;
      Bit #(64)   cword      = cset_cword [rg_way_in_cset];
      Vector #(CWords_per_CLine, Bit #(64)) v_cword = shiftInAtN (rg_write_cline_buf, cword);
      rg_write_cline_buf <= v_cword;

      if (   ((verbosity >= 1) && (rg_cword_in_cline == 0))
	  || (verbosity >= 2))
	 begin
	    $display ("%0d: %m.rl_writeback_loop", cur_cycle);
	    $display ("    cset %0h way %0h cword %0h data %0h",
		      rg_cset_in_cache, rg_way_in_cset, rg_cword_in_cline, cword);
	    $display ("    cset_cword: ", fshow (cset_cword));
	 end

      // If last cset_cword in cline, return to continuation
      Bool last = (rg_cword_in_cline == fromInteger (cwords_per_cline - 1));
      if (last) begin
	 // Send write-request to L2/mem
	 PA wb_cline_pa = fn_cline_pa_from_tag_and_cset_in_cache
	                      (ram_cset_meta.a.read [rg_way_in_cset].ctag,
			       rg_cset_in_cache);
	 f_L1_to_L2_rsps.enq (L1_to_L2_Rsp {addr:     zeroExtend (wb_cline_pa),
					    to_state: rg_post_wb_meta_state,
					    m_cline:  tagged Valid (pack (v_cword)) });
	 rg_fsm_state <= rg_post_wb_fsm_state;
	 if (verbosity >= 1) begin
	    $display ("%0d: %m.rl_writeback_loop", cur_cycle);
	    $display ("    Done; writeback cline @ %0h", wb_cline_pa, " -> ",
		      fshow (rg_post_wb_fsm_state));
	    for (Integer j = 0; j < cwords_per_cline; j = j + 1)
	       $display ("        [%0d]  %016h", j, v_cword [j]);
	 end
      end
      else begin
	 // Request next cset_cword from data RAM B to be accumulated, and increment index
	 CWord_in_CLine       cword_in_cline      = rg_cword_in_cline + 1;
	 CSet_CWord_in_Cache  cset_cword_in_cache = { rg_cset_in_cache, cword_in_cline };
	 ram_cset_cword.a.put (bram_cmd_read, cset_cword_in_cache, ?);

	 rg_cword_in_cline <= cword_in_cline;
	 if (verbosity >= 2)
	    $display ("    Requested cword_in_cline %0d", cword_in_cline);
      end
   endrule

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR: REPLACEMENT

   // On a miss, do a replace (cache-line writeback followed by refill)
   rule rl_replace (rg_fsm_state == FSM_REPLACE_START);
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_replace", cur_cycle);

      let victim_way = fv_choose_victim_way (ram_A_cset_meta, rg_victim_way);
      // Record state, for future victim selection
      rg_victim_way <= victim_way;
      PA victim_cline_pa = fn_CTag_and_CSet_to_CLine_PA (ram_A_cset_meta [victim_way].ctag, va_cset_in_cache);

      // Initialize loop-control index regs
      rg_cset_in_cache <= va_cset_in_cache;
      rg_way_in_cset   <= victim_way;

      if (verbosity >= 1)
	 $display ("    cset %0h way %0h", va_cset_in_cache, victim_way);

      if (ram_A_cset_meta [victim_way].state == META_MODIFIED) begin
	 if (verbosity >= 1) begin
	    $display ("    Victim cline pa %0h: MODIFIED: evict->INVALID (writeback)", victim_cline_pa);
	    $display ("    writeback loop prequel, then -> WRITEBACK_LOOP");
	 end
	 fa_cache_writeback_loop_prequel (ram_A_cset_meta [victim_way].ctag,
					  va_cset_in_cache,
					  victim_way,
					  META_INVALID,
					  FSM_REFILL_START);
	 rg_fsm_state <= FSM_WRITEBACK_LOOP;
      end
      else if (ram_A_cset_meta [victim_way].state != META_INVALID) begin
	 if (verbosity >= 1) begin
	    $write   ("    Victim cline pa %0h: ", victim_cline_pa);
	    $write   (fshow (ram_A_cset_meta [victim_way].state));
	    $display (": evict->INVALID (no writeback, notify downgrade to L2)");
	    $display ("    -> REFILL_START");
	 end
	 let msg = L1_to_L2_Rsp {addr:     zeroExtend (victim_cline_pa),
				 to_state: META_INVALID,
				 m_cline:  tagged Invalid};
	 f_L1_to_L2_rsps.enq (msg);
	 rg_fsm_state <= FSM_REFILL_START;
      end
      else begin
	 if (verbosity >= 1)
	    $display ("    Victim cline: INVALID; no eviction/notification; -> REFILL_START");
	 rg_fsm_state <= FSM_REFILL_START;
      end
   endrule: rl_replace

   // ================================================================
   // CACHE-LINE REFILLS
   // Start cache-line refill loop only when no write-responses are
   // outstanding (to avoid dealing with out-of-order read/write
   // paths through the fabric).
   // Send burst request into fabric for cache line.
   // Update meta-data.
   // Assume rg_cset_in_cache has been initialized.

   rule rl_refill_start (rg_fsm_state == FSM_REFILL_START);
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_refill_start @ %0h (cset %0h way %0h)", cur_cycle,
		   rg_pa, rg_cset_in_cache, rg_way_in_cset);

      // Update meta-data to SHARED, optimistically.
      // If any bus response during the refill is an error-response,
      // we'll change the meta-data to INVALID at the end of the refill.
      let new_ram_A_cset_meta = ram_A_cset_meta;
      new_ram_A_cset_meta [rg_way_in_cset] = Meta {state: META_SHARED,
						   ctag:  fn_PA_to_CTag (rg_pa)};
      ram_cset_meta.b.put (bram_cmd_write, va_cset_in_cache, new_ram_A_cset_meta);

      // Send read request for full cache line
      PA    cline_pa      = fn_align_Addr_to_CLine (rg_pa);
      Bool  for_write     = (   (rg_cache_op == CACHE_ST)
`ifdef ISA_A
			     || ((rg_cache_op == CACHE_AMO) && (rg_amo_funct7 [6:2] != f5_AMO_LR))
`endif
			   );
      Meta_State to_state = (for_write ? META_EXCLUSIVE : META_SHARED);

      let l1_to_l2_req = L1_to_L2_Req {addr:        zeroExtend (cline_pa),
				       from_state:  META_INVALID,
				       to_state:    to_state,
				       can_up_to_E: dcache_not_icache};
      f_L1_to_L2_reqs.enq (l1_to_l2_req);

      // Request read of first CSet_CWord in CLine
      // for cset_cword read-modify-write
      let                 cword_in_cline       = 0;
      CSet_CWord_in_Cache cset_cword_in_cache  = { rg_cset_in_cache, cword_in_cline };
      ram_cset_cword.a.put (bram_cmd_read, cset_cword_in_cache, ?);

      // Enter cache refill loop, awaiting refill responses from mem
      // Note: loop-control index regs rg_cset_in_cache and rg_way_in_cset
      // were initialized in rl_writeback_start, whether or not
      // a writeback was needed.

      rg_cword_in_cline      <= 0;
      rg_fsm_state           <= FSM_UPGRADE_REFILL;
      rg_error_during_refill <= False;

      if (verbosity >= 2) begin
	 $display ("    Requesting cline: ram_cset_cword.a: cword-in-cache: 0x%0h", cset_cword_in_cache);
	 $display ("    ", fshow (l1_to_l2_req));
	 $display ("    -> FSM_UPGRADE_REFILL");
      end
   endrule: rl_refill_start

   // ----------------------------------------------------------------
   // Loop that processes an upgrade response from the next-level cache/mem.
   // The response is collected on iteration 0.
   // and the cset meta-state is updated to the upgraded state.
   // If S->E upgrade, response has no cache line, and the loop exits immediately.

   // Otherwise, response has a cache line, which we buffer, and loop
   // to write cwords into the data RAM.
   // - update cword in cset_cword ram, and
   // - initiate read of next cset_cword from ram for read-modify-write of cset

   rule rl_refill_loop (rg_fsm_state == FSM_UPGRADE_REFILL);
      if (   (verbosity >= 2)
	  || ((rg_cword_in_cline == 0) && (verbosity >= 1)))
	 begin
	    $display ("%0d: %m.rl_refill_loop", cur_cycle);
	    $display ("    addr %0h  (cset %0h way %0d word %0d)",
		      rg_pa, rg_cset_in_cache, rg_way_in_cset, rg_cword_in_cline);
	 end

      Bool  update_data = True;

      // On iteration 0, cline comes from f_L2_to_L1_rsps and is registered in rg_read_cline_buf
      // On subsequent iterations, cline comes from rg_read_cline_buf
      Vector #(CWords_per_CLine, Bit #(64)) v_cword = ?;
      if (rg_cword_in_cline == 0) begin
	 let cline_rsp <- pop (f_L2_to_L1_rsps);

	 // Update cline meta-data
	 let new_ram_A_cset_meta = ram_A_cset_meta;
	 new_ram_A_cset_meta [rg_way_in_cset] = Meta {state: cline_rsp.to_state,
						      ctag:  fn_PA_to_CTag (rg_pa)};
	 ram_cset_meta.b.put (bram_cmd_write, va_cset_in_cache, new_ram_A_cset_meta);
	 if (verbosity >= 1)
	    $display ("    -> ", fshow (cline_rsp.to_state));
	 if (cline_rsp.m_cline matches tagged Valid .cline) begin
	    v_cword = unpack (cline);
	    if (verbosity >= 1) begin
	       for (Integer j = 0; j < cwords_per_cline; j = j + 1)
		  $display ("        [%0d]  %016h", j, v_cword [j]);
	    end
	 end
	 else
	    update_data = False;
      end
      else
	 v_cword = rg_read_cline_buf;

      rg_read_cline_buf <= shiftInAtN (v_cword, ?);

      if (update_data) begin
	 // Next cword is at index 0 of cline viewed as vector of cwords
	 let cword = v_cword [0];

	 // Update the CSet_CWord (BRAM port B)
	 let new_cset_cword              = ram_cset_cword.a.read; 
	 new_cset_cword [rg_way_in_cset] = cword;
	 let cset_cword_in_cache         = { va_cset_in_cache, rg_cword_in_cline };
	 ram_cset_cword.b.put (bram_cmd_write, cset_cword_in_cache, new_cset_cword);
      end

      Bool final_iter = (   (! update_data)
			 || (rg_cword_in_cline == fromInteger (cwords_per_cline - 1)));
      if (final_iter) begin
	 if (verbosity >= 1)
	    $display ("%0d: %m.rl_refill_loop: done", cur_cycle);

	 // Re-request the cset from the RAMs.
	 // cept: if the memory request is for the last
	 // cword-in-cline (which is written in this rule) re-request
	 // after a 1-cycle delay to allow this write to propagate in
	 // the SRAM.
	 if ((va_cword_in_cline == fromInteger (cwords_per_cline - 1))
	     || (! update_data))
	    rg_fsm_state <= FSM_REFILL_FINAL_DELAY;
	 else begin
	    fa_req_rams_A (rg_va);
	    rg_fsm_state <= FSM_IDLE;
	 end
      end
      else begin
	 // Not last cword in line; initiate RAM read for next cword_set
	 // for read-modify-write of cset
	 let next_cword_in_cline      = rg_cword_in_cline + 1;
	 let next_cset_cword_in_cache = { va_cset_in_cache, next_cword_in_cline };
	 ram_cset_cword.a.put (bram_cmd_read, next_cset_cword_in_cache, ?);

	 rg_cword_in_cline <= next_cword_in_cline;
	 if (verbosity >= 2)
	    $display ("    Requesting ram_cset_cword.a cword-in-cache: 0x%0h",
		      next_cset_cword_in_cache);
      end
   endrule: rl_refill_loop

   rule rl_refill_loop_final (rg_fsm_state == FSM_REFILL_FINAL_DELAY);
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_refill_loop_final; re-request RAM", cur_cycle);
      fa_req_rams_A (rg_va);
      rg_fsm_state <= FSM_IDLE;
   endrule

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR: FLUSH
   // Visits all lines, writing back those that are MODIFIED.
   // If flush_req is 0, new meta-state is INVALID
   // If flush_req is 1, new meta-state is CLEAN

   FIFOF #(Bit #(1))  f_flush_reqs <- mkFIFOF;
   FIFOF #(Token)     f_flush_rsps <- mkFIFOF;

   Bool last_cset_and_way = (   (rg_cset_in_cache == fromInteger (csets_per_cache - 1))
			     && (rg_way_in_cset   == fromInteger (ways_per_cset - 1)));

   // New meta state is CLEAN or INVALID, depending on type of flush required
   Reg #(Meta_State) rg_new_meta_state <- mkRegU;
   // This reg holds the current cset_meta as we iterate across its associative ways
   Reg #(CSet_Meta)  rg_new_cset_meta  <- mkRegU;

   rule rl_flush_start (rg_fsm_state == FSM_IDLE);
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_flush_start", cur_cycle);

      let new_state_code <- pop (f_flush_reqs);
      rg_cset_in_cache   <= 0;
      rg_way_in_cset     <= 0;
      rg_new_meta_state  <= ((new_state_code == flush_to_invalid) ? META_INVALID : META_SHARED);
      rg_fsm_state       <= FSM_FLUSH_LOOP;

      // Initiate RAM read of first CSet
      ram_cset_meta.a.put (bram_cmd_read, 0, ?);
   endrule

   function Action fa_incr_flush_loop_indexes;
      action
	 if (rg_way_in_cset == fromInteger (ways_per_cset - 1)) begin
	    // This cset done; move to next cset, way 0
	    let next_cset_in_cache = rg_cset_in_cache + 1;
	    rg_way_in_cset <= 0;
	    rg_cset_in_cache <= next_cset_in_cache;
	    // Initiate read of next CSet
	    ram_cset_meta.a.put (bram_cmd_read, next_cset_in_cache, ?);
	 end
	 else
	    // This way done, move to next way in this cset
	    rg_way_in_cset <= rg_way_in_cset + 1;
      endaction
   endfunction

   // This rule loops over all csets and ways, writing back any modified
   // lines and marking all lines EMPTY.
   // Uses rl_writeback_loop as a subroutine to writeback modified lines.

   rule rl_flush_loop (   (rg_fsm_state == FSM_FLUSH_LOOP)
		       && (rg_cset_in_cache <= fromInteger (csets_per_cache - 1))
		       && (rg_way_in_cset   <= fromInteger (ways_per_cset - 1)));

      let old_cset_meta = ((rg_way_in_cset == 0) ? ram_cset_meta.a.read : rg_new_cset_meta);
      let old_meta      = old_cset_meta [rg_way_in_cset];
      let cline_pa      = fn_CTag_and_CSet_to_CLine_PA (old_meta.ctag, rg_cset_in_cache);
      let line_state = old_meta.state;

      if (verbosity >= 2) begin
	 $display ("%0d: %m.rl_flush_loop", cur_cycle);
	 $display ("    @ %0h (cset %0h, way %0d) cline state ",
		   cline_pa, rg_cset_in_cache, rg_way_in_cset, fshow (line_state));
      end

      // Update line state in RAM
      let new_meta      = Meta {state: rg_new_meta_state, ctag:  old_meta.ctag};
      let new_cset_meta = old_cset_meta;
      new_cset_meta [rg_way_in_cset] = new_meta;
      rg_new_cset_meta <= new_cset_meta;
      if (verbosity >= 2) begin
	 $display ("    Updating cset_meta:");
	 $write   ("    Old: ", fshow_cset_meta (rg_cset_in_cache, old_cset_meta));
	 $write   ("    New: ", fshow_cset_meta (rg_cset_in_cache, new_cset_meta));
      end

      if (line_state == META_MODIFIED) begin
	 if (verbosity >= 2)
	    $display ("    MODIFIED; writeback cline");
	 // Prepare for 'writeback-loop' (start burst-write request, etc.)
	 fa_cache_writeback_loop_prequel (old_meta.ctag,
					  rg_cset_in_cache,
					  rg_way_in_cset,
					  rg_new_meta_state,
					  FSM_FLUSH_LOOP_WRITEBACK_SEQUEL);
	 rg_fsm_state <= FSM_WRITEBACK_LOOP;
      end
      else begin // no writeback
	 if (line_state > rg_new_meta_state) begin
	    // downgrade-notify L2
	    let downgrade_msg = L1_to_L2_Rsp {addr:     zeroExtend (cline_pa),
					      to_state: rg_new_meta_state,
					      m_cline:  tagged Invalid };
	    f_L1_to_L2_rsps.enq (downgrade_msg);
	 end
	 // Write new cset_meta back if it's the last way in this set
	 if (rg_way_in_cset == fromInteger (ways_per_cset - 1))
	    ram_cset_meta.b.put (bram_cmd_write, rg_cset_in_cache, new_cset_meta);

	 if (last_cset_and_way) begin
	    // Respond ack to requestor and goto IDLE
	    f_flush_rsps.enq (?);
	    fa_req_rams_A (rg_va);    // TODO: not necessary? should not be any rg_va in progress?
	    rg_fsm_state <= FSM_IDLE;
	    if (verbosity >= 2)
	       $display ("%0d: %m.rl_flush_loop: done; -> FSM_IDLE", cur_cycle);
	 end
	 else begin
	    fa_incr_flush_loop_indexes;
	 end
      end
   endrule

   rule rl_flush_loop_writeback_sequel (rg_fsm_state == FSM_FLUSH_LOOP_WRITEBACK_SEQUEL);
      // Write new cset_meta back if it's the last way in this set
      if (rg_way_in_cset == fromInteger (ways_per_cset - 1))
	 ram_cset_meta.b.put (bram_cmd_write, rg_cset_in_cache, rg_new_cset_meta);

      if (last_cset_and_way) begin
	 // Respond ack to requestor and goto IDLE
	 f_flush_rsps.enq (?);
	 fa_req_rams_A (rg_va);    // TODO: not necessary? should not be any rg_va in progress?
	 rg_fsm_state <= FSM_IDLE;
	 if (verbosity >= 1)
	    $display ("%0d: %m.rl_flush_writeback_sequel; flush loop done; -> FSM_IDLE", cur_cycle);
      end
      else begin
	 fa_incr_flush_loop_indexes;
	 rg_fsm_state <= FSM_FLUSH_LOOP;
	 if (verbosity >= 1)
	    $display ("%0d: %m.rl_flush_writeback_sequel; continue", cur_cycle);
      end
   endrule

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR: DOWNGRADE REQUEST FROM L2
   // ****************************************************************
   // ****************************************************************
   // Note: An L2-to-L1 downgrade request for line Y can come at any
   // time, including in the middle of an L1-to-L2 refill request for
   // line X.

   // The L2-to-L1 downgrade(Y) request may even be triggered by the
   // L1-to-L2 refill(X) request (if X misses in the L2, L2 may choose
   // to evict Y, which causes L2 to request L1 to downgrade Y, to
   // maintain the inclusion property). This is like a nested callback.

   // The L2-to-L1 downgrade(Y) request here may be triggered by any
   // client (e.g., another L1) making a refill request to L2 which
   // misses, causing L2 to evict(Y) for which it asks this L1 to
   // downgrade(Y).  This is completely 'asynchronous'.

   // Thus, we keep separate state for the downgrade process, so as
   // not to disturb existing state. Further, some state has to be
   // disturbed, so we need to save it before the downgrade and
   // restore it after.

   // Saved state during downgrade
   Reg #(FSM_State)      rg_save_fsm_state      <- mkRegU;    // post-downgrade FSM state (save/restore)
   Reg #(WordXL)         rg_save_va             <- mkRegU;
   Reg #(PA)             rg_save_pa             <- mkRegU;
   Reg #(CSet_in_Cache)  rg_save_cset_in_cache  <- mkRegU;
   Reg #(CWord_in_CLine) rg_save_cword_in_cline <- mkRegU;
   Reg #(Way_in_CSet)    rg_save_way_in_cset    <- mkRegU;

   // ----------------
   // Start downgrade by probing cache for downgrade addr

   rule rl_downgrade_req_from_L2_A (   (rg_fsm_state == FSM_IDLE)
				    || (   (rg_fsm_state == FSM_UPGRADE_REFILL)
					&& (rg_cword_in_cline == 0)));
      let l2_to_l1_req = f_L2_to_L1_reqs.first;
      let addr         = l2_to_l1_req.addr;
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_downgrade_req_from_L2_A", cur_cycle);
	 $display ("    Save rg_va             = %0h", rg_va);
	 $display ("    Save rg_pa             = %0h", rg_pa);
	 $display ("    Save rg_cset_in_cache  = %0h", rg_cset_in_cache);
	 $display ("    Save rg_cword_in_cline = %0h", rg_cword_in_cline);
	 $display ("    Save rg_way_in_cset    = %0h", rg_way_in_cset);
	 $display ("    Probe RAMs for: ", fshow (l2_to_l1_req));
	 $display ("    -> FSM_DOWNGRADE_B");
      end

      // 'push' state for after downgrade:
      rg_save_fsm_state      <= rg_fsm_state;
      rg_save_va             <= rg_va;
      rg_save_pa             <= rg_pa;
      rg_save_cset_in_cache  <= rg_cset_in_cache;
      rg_save_cword_in_cline <= rg_cword_in_cline;
      rg_save_way_in_cset    <= rg_way_in_cset;

      // Set state for downgrade:
      rg_fsm_state     <= FSM_DOWNGRADE_B;
      rg_va            <= addr;
      rg_pa            <= addr;
      rg_cset_in_cache <= fn_Addr_to_CSet_in_Cache (addr);
      fa_req_rams_A (truncate (addr));
   endrule: rl_downgrade_req_from_L2_A

   // ----------------
   // Check cline state, and set to the downgraded state immediately or as part of writeback

   rule rl_downgrade_req_from_L2_B (rg_fsm_state == FSM_DOWNGRADE_B);
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_downgrade_req_from_L2_B", cur_cycle);

      let l2_to_l1_req  = f_L2_to_L1_reqs.first;

      if (verbosity >= 1)
	 $write ("    ", fshow_cset_meta (rg_cset_in_cache, ram_A_cset_meta));

      let valid_info = fv_ram_A_valid_info (rg_pa);
      if (verbosity >= 1)
	 $display ("    valid_info = ", fshow (valid_info));

      // Assertion: cannot match > 1 item in CSet
      if (valid_info.num_valids > 1) begin
	 $display ("    INTERNAL ERROR pa %0h", rg_pa);
	 $display ("    # of valids in set: %0d    (should be 0 or 1)", valid_info.num_valids);
	 $write   (fshow_cset_meta (rg_cset_in_cache, ram_A_cset_meta));
	 $finish (1);
      end

      if (valid_info.num_valids == 0) begin
	 // MISS: equivalent to I, so 'downgrade' is already satisfied.
	 // No response needed: L2 thought that this L1 had this line.
	 // MISS means it must have been recently evicted (voluntary
	 // downgrade) and this L1 must have informed L2 already with
	 // a response (the messages crossed).
	 // let rsp = L1_to_L2_Rsp {addr: rg_pa, to_state: META_INVALID, m_cline: tagged Invalid};
	 // f_L1_to_L2_rsps.enq (rsp);
	 // if (verbosity >= 1)
	 //    $display ("    Send ", fshow (rsp), " -> FSM_DOWNGRADE_C");
	 rg_fsm_state <= FSM_DOWNGRADE_C;
	 if (verbosity >= 1)
	    $display ("    MISS (= INVALID already); ignoring; -> FSM_DOWNGRADE_C");
      end
      else begin
	 // HIT (valid_info.num_valids == 1)
	 rg_way_in_cset <= valid_info.way;

	 // Update meta to <to_state>
	 let new_ram_A_cset_meta = ram_A_cset_meta;
	 new_ram_A_cset_meta [valid_info.way] = Meta {state: l2_to_l1_req.to_state,
						      ctag:  fn_PA_to_CTag (rg_pa)};
	 ram_cset_meta.b.put (bram_cmd_write, rg_cset_in_cache, new_ram_A_cset_meta);
	 if (verbosity >= 1)
	    $display ("    Update meta state to ", fshow (l2_to_l1_req.to_state));

	 // Check if victim needs to be written back (if MODIFIED) or not (otherwise)
	 if (valid_info.valid_state < META_MODIFIED) begin
	    // Victim was SHARED/EXCLUSIVE (so, clean): respond and done
	    let rsp = L1_to_L2_Rsp {addr:     rg_pa,
				    to_state: l2_to_l1_req.to_state,
				    m_cline:  tagged Invalid};
	    f_L1_to_L2_rsps.enq (rsp);
	    rg_fsm_state <= FSM_DOWNGRADE_C;
	    if (verbosity >= 1)
	       $display ("    Send ", fshow (rsp), " -> FSM_DOWNGRADE_C");
	 end
	 else begin
	    // Victim was MODIFIED: writeback first, then done
	    //    (writeback will send L1_to_L2_Rsp with the modified cache line)
	    fa_cache_writeback_loop_prequel (fn_PA_to_CTag (rg_pa),
					     rg_cset_in_cache,
					     valid_info.way,
					     l2_to_l1_req.to_state,
					     FSM_DOWNGRADE_C);
	    rg_fsm_state <= FSM_WRITEBACK_LOOP;
	    if (verbosity >= 1)
	       $display ("    -> FSM_WRITEBACK_LOOP");
	 end
      end
   endrule: rl_downgrade_req_from_L2_B

   // ----------------
   // If did a writeback for the downgrade, restore state (incl. re-probe the rams).

   rule rl_downgrade_req_from_L2_C (rg_fsm_state == FSM_DOWNGRADE_C);
      let l2_to_l1_req <- pop (f_L2_to_L1_reqs);    // consume the downgrade request

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_downgrade_req_from_L2_C", cur_cycle);
	 $display ("    writeback done; restore and reprobe RAMs");
	 $display ("    rg_fsm_state      <= ", fshow (rg_save_fsm_state));
	 $display ("    rg_va             <= %0h", rg_save_va);
	 $display ("    rg_pa             <= %0h", rg_save_pa);
	 $display ("    rg_cset_in_cache  <= %0h", rg_save_cset_in_cache);
	 $display ("    rg_cword_in_cline <= %0h", rg_save_cword_in_cline);
	 $display ("    rg_way_in_cset    <= %0h", rg_save_way_in_cset);
      end
      // 'Pop' state from before downgrade:
      rg_fsm_state      <= rg_save_fsm_state;
      rg_va             <= rg_save_va;
      rg_pa             <= rg_save_pa;
      rg_cset_in_cache  <= rg_save_cset_in_cache;
      rg_cword_in_cline <= rg_save_cword_in_cline;
      rg_way_in_cset    <= rg_save_way_in_cset;

      // Re-Request meta RAM
      ram_cset_meta.a.put (bram_cmd_read, rg_save_cset_in_cache, ?);

      // Re-Request data RAM
      CSet_CWord_in_Cache  cset_cword_in_cache = fn_Addr_to_CSet_CWord_in_Cache (rg_save_va);
      if (rg_save_fsm_state == FSM_UPGRADE_REFILL)
	 cset_cword_in_cache  = { rg_save_cset_in_cache, rg_save_cword_in_cline };

      ram_cset_cword.a.put (bram_cmd_read, cset_cword_in_cache, ?);
   endrule: rl_downgrade_req_from_L2_C

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR: INITIALIZING AFTER RESET
   // ****************************************************************
   // ****************************************************************
   // This rule loops over csets, setting state of each cline in the set to INVALID
   // Assumes rg_cset_in_cache resets to 0

   rule rl_initialize (rg_fsm_state == FSM_INITIALIZE);
      let meta = Meta { state: META_INVALID, ctag: ? };
      ram_cset_meta.a.put (bram_cmd_write, rg_cset_in_cache, replicate (meta));

      if (rg_cset_in_cache == fromInteger (csets_per_cache - 1)) begin
	 rg_fsm_state <= FSM_IDLE;

	 $display ("%0d: INFO: %m.rl_initialize", cur_cycle);
	 $display ("    Size %0d KB, Associativity %0d, CLine size %0d bytes (= %0d XLEN words)",
		   kb_per_cache, ways_per_cset, (cwords_per_cline * 8),
`ifdef RV32
		   (cwords_per_cline * 2)
`else
		   (cwords_per_cline * 1)
`endif
		   );
	 if (verbosity >= 1)
	    $display ("    All clines (%0d sets %0d ways) initialized to INVALID",
		      cur_cycle, csets_per_cache, ways_per_cset);
      end
      rg_cset_in_cache <= rg_cset_in_cache + 1;
   endrule

   // ****************************************************************
   // ****************************************************************
   // INTERFACE

   // This starts a new request (with virt addr)
   // while the virt addr is being translated to a phys addr
   method Action ma_request_va (WordXL va);    // if (rg_fsm_state == FSM_IDLE);
      fa_req_rams_A (va);
      rg_va                  <= va;
      rg_cset_in_cache       <= fn_Addr_to_CSet_in_Cache (va);
      if (verbosity >= 1)
	 $display ("%0d: %m.ma_request_va: %0h", cur_cycle, va);
   endmethod

   // This completes a new request with the phys addr
   method ActionValue #(Cache_Result)
          mav_request_pa (MMU_Cache_Req req, PA pa)
          if  ((rg_fsm_state == FSM_IDLE)
	       && (! f_L2_to_L1_reqs.notEmpty)
	       && (! f_flush_reqs.notEmpty));
      actionvalue
	 if (verbosity >= 1) begin
	    $display ("%0d: %m.mav_request_pa: pa %0h", cur_cycle, pa);
	    $display ("    ", fshow_MMU_Cache_Req (req));
	 end

	 Cache_Result result = ?;
	 rg_cache_op <= req.op;
	 rg_f3       <= req.f3;
`ifdef ISA_A
	 rg_amo_funct7 <= req.amo_funct7;
`endif
	 rg_pa <= pa;

	 if (verbosity >= 1)
	    $write ("    ", fshow_cset_meta (va_cset_in_cache, ram_A_cset_meta));

	 let valid_info = fv_ram_A_valid_info (pa);
	 if (verbosity >= 1)
	    $display ("    valid_info = ", fshow (valid_info));
	 let data       = fv_from_byte_lanes (zeroExtend (req.va), req.f3 [1:0], valid_info.data);
	 data = fv_extend (req.f3, data);

	 if (valid_info.num_valids > 1) begin
	    // Assertion failure: # cannot match more than 1 item in a set
	    $display ("    INTERNAL ERROR ", fshow (req.op), " va %0h pa %0h", req.va, pa);
	    $display ("    # of valids in set: %0d    (should be 0 or 1)", valid_info.num_valids);
	    $write   ("    ", fshow_cset_meta (va_cset_in_cache, ram_A_cset_meta));
	    $finish (1);
	 end

	 Bool valid = (valid_info.num_valids == 1);    // Could be M, E or S

	 // Load-hit
	 if (valid && (req.op == CACHE_LD)) begin
	    if (verbosity >= 1)
	       $display ("    LOAD-HIT: va %0h pa %0h data %0h", req.va, pa, data);
	    result = Cache_Result {outcome:      CACHE_READ_HIT,
				   final_ld_val: data,
				   final_st_val: ?};
	 end

	 // Store-hit
	 else if (valid && (req.op == CACHE_ST) && (valid_info.valid_state > META_SHARED)) begin
	    if (verbosity >= 1)
	       $display ("    STORE-HIT: va %0h pa %0h data %0h", req.va, pa, req.st_value);
	    fa_write (pa, req.f3, req.st_value);
	    result = Cache_Result {outcome:      CACHE_WRITE_HIT,
				   final_ld_val: 0,
				   final_st_val: req.st_value};

	    // Cancel LR/SC reservation if this store is for this addr
	    // TODO: should we cancel it on ANY store?
	    if (rg_lrsc_pa == pa)
	       rg_lrsc_valid <= False;
	 end

`ifdef ISA_A
	 // AMO LR-hit
	 else if (valid && fv_is_AMO_LR (req)) begin
	    if (verbosity >= 1)
	       $display ("    LR-HIT: va %0h pa %0h data %0h", req.va, pa, data);
	    rg_lrsc_valid <= True;
	    rg_lrsc_pa    <= pa;
	    rg_lrsc_size  <= req.f3 [1:0];
	    result = Cache_Result {outcome:      CACHE_READ_HIT,
				   final_ld_val: data,
				   final_st_val: ?};
	 end

	 // AMO SC-hit
	 else if (valid && fv_is_AMO_SC (req) && (valid_info.valid_state > META_SHARED)) begin
	    if (rg_lrsc_valid && (rg_lrsc_pa == pa)) begin
	       if (verbosity >= 1)
		  $display ("    SC-HIT and success: va %0h pa %0h data %0h",
			    req.va, pa, req.st_value);
	       rg_lrsc_valid <= False;
	       fa_write (pa, req.f3, req.st_value);
	       result = Cache_Result {outcome:      CACHE_WRITE_HIT,
				      final_ld_val: 0,    // SC success
				      final_st_val: req.st_value};
	    end
	    else begin
	       if (verbosity >= 1)
		  $display ("    SC-HIT and fail: va %0h pa %0h data %0h",
			    req.va, pa, req.st_value);
	       result = Cache_Result {outcome:      CACHE_READ_HIT,
				      final_ld_val: 1,    // SC fail
				      final_st_val: 0};
	    end
	 end

	 // Hit for all AMO read-modify-writes (i.e., AMO other than LR and SC)
	 else if (valid && (req.op == CACHE_AMO) && (valid_info.valid_state > META_SHARED)) begin
	    Fmt fmt_op = fshow_f5_AMO_op (req.amo_funct7 [6:2]);
	    if (verbosity >= 1) begin
	       $display ("    AMO-HIT: va %0h pa %0h data %0h", req.va, pa, req.st_value);
	       $display ("    f3 %3b AMO ", req.f3, fmt_op);
	       $display ("    va %0h  pa %0h  st_value %0h", req.va, pa, req.st_value);
	       $display ("    Cache cword %0h, load-result %0h",
			 valid_info.data, valid_info.data);
	    end

	    let size_code  = req.f3 [1:0];
	    let cache_data = fv_from_byte_lanes (zeroExtend (req.va), size_code, valid_info.data);
	    // Do the AMO op on the loaded value and the store value
	    match {.new_ld_val,
		   .new_st_val} = fv_amo_op (size_code,
					     req.amo_funct7 [6:2],
					     cache_data,
					     req.st_value);
	    if (verbosity >= 1) begin
	       $display ("    ", fmt_op, " (%0h, %0h) -> %0h",
			 cache_data, req.st_value, new_st_val);
	    end
	    fa_write (pa, req.f3, new_st_val);
	    result = Cache_Result {outcome:      CACHE_WRITE_HIT,
				   final_ld_val: new_ld_val,
				   final_st_val: new_st_val};

	    // Cancel LR/SC reservation if this store is for this addr
	    if (rg_lrsc_pa == pa)
	       rg_lrsc_valid <= False;
	 end
`endif

	 // MISS, or Store-upgrade
	 else begin
	    Bool upgrade = (   valid
			    && (valid_info.valid_state == META_SHARED)
`ifdef ISA_A
			    && (   (req.op == CACHE_ST)
				|| (req.op == CACHE_AMO))  // (valid && LR) handled as hit, earlier
`endif
			       );
	    if (upgrade) begin
	       let cline_addr = zeroExtend (fn_align_Addr_to_CLine (pa));
	       f_L1_to_L2_reqs.enq (L1_to_L2_Req {addr:        cline_addr,
						  from_state:  META_SHARED,
						  to_state:    META_EXCLUSIVE,
						  can_up_to_E: dcache_not_icache});
	       rg_cset_in_cache       <= va_cset_in_cache;
	       rg_way_in_cset         <= valid_info.way;
	       rg_cword_in_cline      <= 0;
	       rg_error_during_refill <= False;
	       rg_fsm_state           <= FSM_UPGRADE_REFILL;

	       if (verbosity >= 1) begin
		  $display ("    Store-hit, but need S->E upgrade");
		  $display ("    va %0h pa %0h data %0h -> FSM_UPGRADE_REFILL",
			    req.va, pa, req.st_value);
	       end
	    end
	    else begin
	       if (verbosity >= 1)
		  $display ("    MISS: va %0h pa %0h; -> REPLACE_START", req.va, pa);
	       rg_fsm_state <= FSM_REPLACE_START;
	    end
	    result = Cache_Result {outcome: CACHE_MISS,
				   final_ld_val: ?,
				   final_st_val: ?};
`ifdef ISA_A
	    // If the line being replaced/upgraded contains the LRSC reserved addr,
	    // cancel the reservation.
	    Bool cancel = (ram_A_cset_meta [valid_info.way].ctag == fn_PA_to_CTag (rg_lrsc_pa));
	    if (cancel)
	       rg_lrsc_valid <= False;
`endif
	 end

	 return result;
      endactionvalue
   endmethod: mav_request_pa

   // ----------------
   // Indicates cache is IDLE, ready for new request

   method Bool mv_is_idle;
      return (rg_fsm_state == FSM_IDLE);
   endmethod

   // ----------------
   // Stalls until refill done and then returns ok (True) or error (False)

   method Bool mv_refill_ok () if (rg_fsm_state == FSM_IDLE);
      return (! rg_error_during_refill);
   endmethod

   // ----------------
   // Flushes

   interface Server flush_server = toGPServer (f_flush_reqs, f_flush_rsps);

   // ----------------
   // Interface to next level cache or memory (for refills, writebacks, downgrades, ...)

   interface Client_Semi_FIFOF l1_to_l2_client = fifofs_to_Client_Semi_FIFOF (f_L1_to_L2_reqs,
									      f_L2_to_L1_rsps);

   interface Server_Semi_FIFOF l2_to_l1_server = fifofs_to_Server_Semi_FIFOF (f_L2_to_L1_reqs,
									      f_L1_to_L2_rsps);
endmodule

// ================================================================

endpackage
