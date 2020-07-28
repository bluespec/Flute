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
import GetPut       :: *;
import ClientServer :: *;
import Assert       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle     :: *;
import GetPut_Aux    :: *;

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
   // Stalls until refill done and then returns ok (True) or error (False)
   method Bool mv_refill_ok ();

   // ----------------
   // Cache flush request/response
   // Bit #(1) request specifies new meta-state: 0=INVALID, 1=SHARED

   interface Server #(Bit #(1), Token) flush_server;

   // ----------------
   // Interface to next level (for refills, writebacks)

   interface Get #(Line_Req)  g_mem_req;
   interface Get #(Bit #(64)) g_write_data;
   interface Put #(Read_Data) p_mem_read_data;
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
              FSM_REFILL_LOOP,            // Refill
              FSM_REFILL_FINAL_DELAY,     // 1-cycle delay after refill due to SRAM-write requirement

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

function Fmt fshow_cset_meta (CSet_in_Cache  cset_in_cache,
			      CSet_Meta      cset_meta);
   Fmt fmt = $format ("CSet[%0h] (state, tag){", cset_in_cache);
   for (Integer j = 0; j < ways_per_cset; j = j + 1) begin
      fmt = fmt + $format (" way%0d (", j, fshow (cset_meta [j].state));
      if (cset_meta [j].state != META_INVALID)
	 fmt = fmt + $format (", ctag %0h", cset_meta [j].ctag);
      fmt = fmt + $format (")");
   end
   fmt = fmt + $format ("}");
   return fmt;
endfunction

function Fmt fshow_cset_cword (CSet_CWord cset_cword);
   Fmt fmt = $format ("CSet_CWord {");
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
   // Note: this victim may be SHARED or MODIFIED.
   if (! victim_found)
      victim_way = fn_incr_way (old_way);

   return victim_way;
endfunction

// ================================================================
// Help functions for RAM access

// ----------------------------------------------------------------
// RAM read-output hit/miss info

typedef struct {
   Bool                 hit;
   Bit #(64)            data;         // valid if hit
   Way_in_CSet          way;          // valid if hit, for subsequent updates

   // Assertion error if multi-way hit (at most one way should hit)
   Bool                 multi_way_hit;
   } Hit_Miss_Info
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
module mkCache #(parameter Bit #(3) verbosity)
               (Cache_IFC);

   // 0: quiet; 1: rules; 2: loop iterations
   // Integer verbosity = 2;

   Reg #(FSM_State) rg_fsm_state <- mkReg (FSM_INITIALIZE);

   // After a writeback, we may refill (for mem ops) or leave cache empty (flushes)
   // FSM_REFILL_START or FSM_IDLE
   Reg #(FSM_State) rg_writeback_sequel <- mkRegU;

   // The address of the current request.
   Reg #(WordXL) rg_va <- mkRegU;             // Virtual addr, used to probe the cache
   Reg #(PA)     rg_pa <- mkRegU;             // Physical addr, 1 cycle later and sustained
   Wire #(PA)    dw_pa <- mkDWire (rg_pa);    // Physical addr on 'mav_request_pa', else rg_pa

   // Cache RAMs
   //     Port A used for the main hit/miss path (for MMU_Cache client)
   //     Port B is used for writebacks and refills (to/from main memory)
   // Meta-data RAM
   BRAM_DUAL_PORT #(CSet_in_Cache,
		    CSet_Meta)             ram_cset_meta   <- mkBRAMCore2 (csets_per_cache,
									   bram_with_output_reg);
   // Data RAM
   // Note: a cset_cword is addressed by { cset_in_cache, cword_in_cline },
   BRAM_DUAL_PORT #(CSet_CWord_in_Cache,
		    CSet_CWord)          ram_cset_cword <- mkBRAMCore2 (cset_cwords_per_cache,
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
   // Memory interface (for refills, writebacks)

   FIFOF #(Line_Req)   f_line_reqs  <- mkFIFOF;
   FIFOF #(Bit #(64))  f_write_data <- mkFIFOF;
   FIFOF #(Read_Data)  f_read_data  <- mkFIFOF;

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
   // Hit/Miss is a pure combinational function of the B-outputs of
   // the RAMs (current cache set) and the phys addr (tag-match)

   function Hit_Miss_Info fv_ram_A_hit_miss (PA pa);

      Bool         hit           = False;
      Bool         multi_way_hit = False;
      Way_in_CSet  way_hit       = 0;
      Bit #(64)    cword         = 0;

      CTag  pa_ctag = fn_PA_to_CTag (pa);

      for (Integer way = 0; way < ways_per_cset; way = way + 1) begin
	 let hit_at_way  = (   (ram_A_cset_meta [way].state != META_INVALID)
			    && (ram_A_cset_meta [way].ctag  == pa_ctag));

	 if (hit && hit_at_way) multi_way_hit = True;

	 let cword_at_way = ram_A_cset_cword [way];

	 hit = hit || hit_at_way;
	 if (hit_at_way) way_hit = fromInteger (way);
	 cword  = (cword | (cword_at_way & pack (replicate (hit_at_way))));
      end

      return Hit_Miss_Info {hit:           hit,
			    data:          cword,
			    way:           way_hit,           // For possible subsequent update
			    multi_way_hit: multi_way_hit};    // Assertion error if true
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
	    $display ("    fa_request_va %0h cset_in_cache %0h, cset_cword_in_cache %0h",
		      va, cset_in_cache, cset_cword_in_cache);
      endaction
   endfunction

   // ****************************************************************
   // ****************************************************************
   // Write actions on a cache hit
   function Action fa_write (PA pa, Bit #(3) f3, Bit #(64) st_value);
      action
	 let hit_miss_info = fv_ram_A_hit_miss (pa);
	 let way           = hit_miss_info.way;

	 // Assert: current mv_response is SHARED/MODIFIED
	 // Writes data into that currently probed cache line
	 if (! hit_miss_info.hit)
	    begin
	       $display ("%0d: %m.fa_write: INTERNAL_ERROR", cur_cycle);
	       $display ("    va_cset_cword_in_cache %0h way %0d pa %0h f3 %0d st_value %0h",
			 va_cset_cword_in_cache, way, pa, f3, st_value);
	       $display ("    Cache write on a miss (need SHARED/MODIFIED)");
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
   // BEHAVIOR: CACHE-LINE WRITEBACK (preceding refills, and for flushes)
   // These rules are a "subroutine" to writeback a cache line
   //   - in normal cache operation (writeback before refill)
   //   - in flush operations (writeback and stay empty)
   // rg_writeback_sequel specifies fsm state after writeback.
   //
   // function fa_cache_writeback_loop_prequel is called before rl_writeback_loop
   // Preconditions:
   //   - ram_cset_meta.b    has been requested for target cset_in_cache

   function Action fa_cache_writeback_loop_prequel (CSet_in_Cache  cset_in_cache,
						    Way_in_CSet    way_in_cset);
      action
	 if (verbosity >= 1)
	    $display ("    fa_cache_writeback_loop_prequel: cset %0h, way %0h",
		      cset_in_cache, way_in_cset);

	 // Send write-burst request to mem
	 Byte_in_CLine  byte_in_cline = 0;
	 PA  wb_cline_pa = {ram_cset_meta.a.read [way_in_cset].ctag,
			    cset_in_cache,
			    byte_in_cline };
	 f_line_reqs.enq (Line_Req {is_read: False, addr: zeroExtend (wb_cline_pa)});
	 if (verbosity >= 1)
	    $display ("    line addr: %0h", wb_cline_pa);

	 // Request data RAM A for first CSet_CWord for this line
	 CWord_in_CLine       cword_in_cline      = 0;
	 CSet_CWord_in_Cache  cset_cword_in_cache = { cset_in_cache, cword_in_cline };
	 ram_cset_cword.a.put (bram_cmd_read, cset_cword_in_cache, ?);

	 rg_cword_in_cline <= 0;
      endaction
   endfunction

   // ----------------
   // rl_writeback_loop:
   // Assume proper setup of loop index regs and rg_writeback_sequel
   // and that a cword_cset has been requested from data RAM B

   rule rl_writeback_loop (rg_fsm_state == FSM_WRITEBACK_LOOP);
      // Writeback a cword
      CSet_CWord cset_cword = ram_cset_cword.a.read;
      Bit #(64)  cword      = cset_cword [rg_way_in_cset];
      f_write_data.enq (cword);

      if (   ((verbosity >= 1) && (rg_cword_in_cline == 0))
	  || (verbosity >= 2))
	 $display ("%0d: %m.rl_writeback_loop [cset %0h way %0h cword %0h] data %0h",
		   cur_cycle, rg_cset_in_cache, rg_way_in_cset, rg_cword_in_cline, cword);

      // If last cset_cword in cline, return to continuation
      Bool last = (rg_cword_in_cline == fromInteger (cwords_per_cline - 1));
      if (last) begin
	 rg_fsm_state <= rg_writeback_sequel;
	 if (verbosity >= 1)
	    $display ("%0d: %m.rl_writeback_loop: done; goto ", cur_cycle, fshow (rg_writeback_sequel));
      end
      else begin
	 // Request next cset_cword from data RAM B to be written back and increment index
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

      // Initialize loop-control index regs
      rg_cset_in_cache   <= va_cset_in_cache;
      rg_way_in_cset     <= victim_way;

      if (ram_A_cset_meta [victim_way].state == META_MODIFIED) begin
	 if (verbosity >= 1)
	    $display ("    Writeback needed: loop prequel: cset %0h way %0h",
		      va_cset_in_cache, victim_way);
	 fa_cache_writeback_loop_prequel (va_cset_in_cache, victim_way);
	 rg_fsm_state        <= FSM_WRITEBACK_LOOP;
	 rg_writeback_sequel <= FSM_REFILL_START;
      end
      else begin
	 if (verbosity >= 1)
	    $display ("    Writeback not needed: start refill: cset %0h way %0h",
		      va_cset_in_cache, victim_way);
	 rg_fsm_state <= FSM_REFILL_START;
      end
   endrule

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
	 $display ("%0d: %m.rl_refill_start: cset %0h way %0h", cur_cycle,
		   rg_cset_in_cache, rg_way_in_cset);

      // Update meta-data to SHARED, optimistically.
      // If any bus response during the refill is an error-response,
      // we'll change the meta-data to INVALID at the end of the refill.
      let new_ram_A_cset_meta = ram_A_cset_meta;
      new_ram_A_cset_meta [rg_way_in_cset] = Meta {state: META_SHARED,
						   ctag:  fn_PA_to_CTag (rg_pa)};
      ram_cset_meta.b.put (bram_cmd_write, va_cset_in_cache, new_ram_A_cset_meta);

      // Send read-burst request into fabric for full cache line
      PA cline_pa = fn_align_Addr_to_CLine (rg_pa);
      f_line_reqs.enq (Line_Req {is_read: True, addr: zeroExtend (cline_pa)});

      // Request read of first CSet_CWord in CLine (BRAM port B)
      // for cset_cword read-modify-write
      let                 cword_in_cline       = 0;
      CSet_CWord_in_Cache cset_cword_in_cache  = { rg_cset_in_cache, cword_in_cline };
      ram_cset_cword.a.put (bram_cmd_read, cset_cword_in_cache, ?);

      // Enter cache refill loop, awaiting refill responses from mem
      // Note: loop-control index regs rg_cset_in_cache and rg_way_in_cset
      // were initialized in rl_writeback_start, whether or not
      // a writeback was needed.

      rg_cword_in_cline      <= 0;
      rg_error_during_refill <= False;
      rg_fsm_state           <= FSM_REFILL_LOOP;

      if (verbosity >= 2) begin
	 $display ("    Requesting line at mem addr %0h", cline_pa);
	 $display ("    Requesting ram_cset_cword.a: cword-in-cache: 0x%0h", cset_cword_in_cache);
	 $display ("    goto FSM_REFILL_LOOP");
      end
   endrule: rl_refill_start

   // ----------------------------------------------------------------
   // Loop that receives responses from the fabric with fabric-words of the cline (from mem).
   // Update cword in cset_cword ram, and
   // initiate read of next cset_cword from ram.
   // On last cword, update meta state to SHARED (normal case)
   //     or INVALID (if there was a fabric error on refill).

   rule rl_refill_loop (rg_fsm_state == FSM_REFILL_LOOP);
      if (    (verbosity >= 2)
	  || ((verbosity >= 1) && (rg_cword_in_cline == 0)))
	 begin
	    $display ("%0d: %m.rl_refill_loop", cur_cycle);
	    $display ("    set 0x%0h way %0d word %0d",
		      rg_cset_in_cache, rg_way_in_cset, rg_cword_in_cline);
	 end

      let read_data <- pop (f_read_data);
      if (verbosity >= 2)
	 $display ("    mem rsp: ok %0d data %0h", pack (read_data.ok), read_data.data);

      // Bus errors; remember it, and handle after all the refill responses
      if ((! read_data.ok) && (verbosity >= 2))
	 $display ("    Fabric ERROR in load-response");
      Bool err = (! read_data.ok) || rg_error_during_refill;
      rg_error_during_refill <= err;

      // Update the CSet_CWord (BRAM port B) (if this response was not an error)
      if (! err) begin
	 let new_cset_cword              = ram_cset_cword.a.read; 
	 new_cset_cword [rg_way_in_cset] = read_data.data;
	 let cset_cword_in_cache = { va_cset_in_cache, rg_cword_in_cline };
	 ram_cset_cword.b.put (bram_cmd_write, cset_cword_in_cache, new_cset_cword);
      end

      Bool last_cword_in_cline = (rg_cword_in_cline == fromInteger (cwords_per_cline - 1));
      if (last_cword_in_cline) begin
	 if (verbosity >= 1)
	    $display ("%0d: %m.rl_refill_loop: done", cur_cycle);

	 // If error during refill, update cset meta-data to INVALID
	 if (err) begin
	    let new_ram_A_cset_meta = ram_A_cset_meta;
	    new_ram_A_cset_meta [rg_way_in_cset] = Meta {state: META_INVALID,
							 ctag:  fn_PA_to_CTag (rg_pa)};
	    ram_cset_meta.b.put (bram_cmd_write, va_cset_in_cache, new_ram_A_cset_meta);
	    if (verbosity >= 1)
	       $display ("    Setting meta-data to INVALID (err during refill)");
	 end

	 // Re-request the cset from the RAMs.
	 // Except: if the memory request is for the last
	 // cword-in-cline (which is written in this rule) re-request
	 // after a 1-cycle delay to allow this write to propagate in
	 // the SRAM.
	 if (va_cword_in_cline == fromInteger (cwords_per_cline - 1))
	    rg_fsm_state <= FSM_REFILL_FINAL_DELAY;
	 else begin
	    fa_req_rams_A (rg_va);
	    rg_fsm_state <= FSM_IDLE;
	 end
      end
      else begin
	 // Not last cword in line; initiate RAM read for next cword_set
	 let next_cword_in_cline      = rg_cword_in_cline + 1;
	 let next_cset_cword_in_cache = { va_cset_in_cache, next_cword_in_cline };
	 ram_cset_cword.a.put (bram_cmd_read, next_cset_cword_in_cache, ?);
	 rg_cword_in_cline <= next_cword_in_cline;
	 if (verbosity >= 2)
	    $display ("    Requesting ram_cset_cword.a cword-in-cache: 0x%0h", next_cset_cword_in_cache);
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
   // If flush_req is 1, new meta-state is SHARED

   FIFOF #(Bit #(1))  f_flush_reqs <- mkFIFOF;
   FIFOF #(Token)     f_flush_rsps <- mkFIFOF;

   Bool last_cset_and_way = (   (rg_cset_in_cache == fromInteger (csets_per_cache - 1))
			     && (rg_way_in_cset   == fromInteger (ways_per_cset - 1)));

   // New meta state is SHARED or INVALID, depending on type of flush required
   Reg #(Meta_State) rg_new_meta_state <- mkRegU;
   // This reg holds the current cset_meta as we iterate across its associative ways
   Reg #(CSet_Meta)  rg_new_cset_meta  <- mkRegU;

   rule rl_flush_start (rg_fsm_state == FSM_IDLE);
      if (verbosity >= 1)
	 $display ("%0d: %m.rl_flush_start", cur_cycle);

      let new_state_code <- pop (f_flush_reqs);
      rg_cset_in_cache   <= 0;
      rg_way_in_cset     <= 0;
      rg_new_meta_state  <= ((new_state_code == 0) ? META_INVALID : META_SHARED);
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

      if (verbosity >= 1)
	 $display ("%0d: %m.rl_flush_loop: line [cset %0x, way %0d]",
		   cur_cycle, rg_cset_in_cache, rg_way_in_cset);

      // Update line state
      let old_cset_meta = ((rg_way_in_cset == 0) ? ram_cset_meta.a.read : rg_new_cset_meta);
      let old_meta      = old_cset_meta [rg_way_in_cset];
      let new_meta      = Meta {state: rg_new_meta_state, ctag:  old_meta.ctag};
      let new_cset_meta = old_cset_meta;
      new_cset_meta [rg_way_in_cset] = new_meta;
      rg_new_cset_meta <= new_cset_meta;
      if (verbosity >= 1) begin
	 $display ("    Updating cset_meta:");
	 $display ("    Old: ", fshow_cset_meta (rg_cset_in_cache, old_cset_meta));
	 $display ("    New: ", fshow_cset_meta (rg_cset_in_cache, new_cset_meta));
      end

      let line_state = old_meta.state;
      if (line_state == META_MODIFIED) begin
	 if (verbosity >= 2) begin
	    $display ("    MODIFIED; writeback");
	    $display ("%0d: %m.rl_flush_loop: writeback line [cset %0x, way %0d]",
		      cur_cycle, rg_cset_in_cache, rg_way_in_cset);
	 end

	 // Prepare for 'writeback-loop' (start burst-write request, etc.)
	 fa_cache_writeback_loop_prequel (rg_cset_in_cache, rg_way_in_cset);
	 // Invoke 'writeback-loop' subroutine for this [cset][way],
	 rg_writeback_sequel <= FSM_FLUSH_LOOP_WRITEBACK_SEQUEL;
	 rg_fsm_state        <= FSM_WRITEBACK_LOOP;
      end
      else begin // EMPTY or SHARED
	 if (verbosity >= 2)
	    $display ("    Line cstate: ", fshow (line_state));

	 // Write new cset_meta back if it's the last way in this set
	 if (rg_way_in_cset == fromInteger (ways_per_cset - 1))
	    ram_cset_meta.b.put (bram_cmd_write, rg_cset_in_cache, new_cset_meta);

	 if (last_cset_and_way) begin
	    // Respond ack to requestor and goto IDLE
	    f_flush_rsps.enq (?);
	    rg_fsm_state <= FSM_IDLE;
	    if (verbosity >= 2)
	       $display ("%0d: %m.rl_flush_loop: done; goto IDLE", cur_cycle);
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
	 fa_req_rams_A (rg_va);
	 rg_fsm_state <= FSM_IDLE;
	 if (verbosity >= 1)
	    $display ("%0d: %m.rl_flush_writeback_sequel; flush loop done; goto IDLE", cur_cycle);
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
	 $display ("    Size %0d KB, Associativity %0d, Line size %0d bytes (= %0d XLEN words)",
		   kb_per_cache, ways_per_cset, (cwords_per_cline * 8),
`ifdef RV32
		   (cwords_per_cline * 2)
`else
		   (cwords_per_cline * 1)
`endif
		   );
	 if (verbosity >= 1)
	    $display ("    All lines (%0d sets %0d ways) initialized to INVALID",
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
      rg_va  <= va;
      rg_error_during_refill <= False;
      if (verbosity >= 1)
	 $display ("%0d: %m.ma_request_va: %0h", cur_cycle, va);
   endmethod

   // This completes a new request with the phys addr
   method ActionValue #(Cache_Result)
          mav_request_pa (MMU_Cache_Req req, PA pa)
          if  ((rg_fsm_state == FSM_IDLE) && (! f_flush_reqs.notEmpty));
      actionvalue
	 Cache_Result result = ?;
	 rg_pa <= pa;

	 let hit_miss_info = fv_ram_A_hit_miss (pa);
	 let data = fv_from_byte_lanes (zeroExtend (req.va), req.f3 [1:0], hit_miss_info.data);
	 data = fv_extend (req.f3, data);

	 if (hit_miss_info.multi_way_hit) begin
	    // Assertion failure: # cannot match more than 1 item in a set
	    $display ("%0d: %m.mav_request_pa: INTERNAL ERROR", cur_cycle);
	    $display ("    ", fshow (req.op), " va %0h pa %0h", req.va, pa);
	    $display ("    # of hits in set > 1 (should be 0 or 1)");
	    $display (fshow_cset_meta (fn_Addr_to_CSet_in_Cache (rg_va), ram_A_cset_meta));
	    $finish (1);
	 end

	 if (! hit_miss_info.hit) begin
	    if (verbosity >= 1) begin
	       $display ("%0d: %m.mav_request_pa: MISS: va %0h pa %0h", cur_cycle, req.va, pa);
	       $display ("    Starting replacement");
	    end
	    rg_fsm_state <= FSM_REPLACE_START;
	    result = Cache_Result {outcome: CACHE_MISS,
				   final_ld_val: ?,
				   final_st_val: ?};

`ifdef ISA_A
	    // If the line being replaced contains the LRSC reserved addr,
	    // cancel the reservation.
	    Bool cancel = (hit_miss_info.hit
			   && (ram_A_cset_meta [hit_miss_info.way].ctag == fn_PA_to_CTag (rg_lrsc_pa)));
	    if (cancel)
	       rg_lrsc_valid <= False;
`endif
	 end
	 else begin
	    // Hit

	    // Load
	    if (req.op == CACHE_LD) begin
	       if (verbosity >= 1)
		  $display ("%0d: %m.mav_request_pa: Load-hit: va %0h pa %0h data %0h",
			    cur_cycle, req.va, pa, data);
	       result = Cache_Result {outcome: CACHE_READ_HIT, final_ld_val: data, final_st_val:?};
	    end

	    // Store
	    else if (req.op == CACHE_ST) begin
	       if (verbosity >= 1)
		  $display ("%0d: %m.mav_request_pa: Store-hit: va %0h pa %0h data %0h",
			    cur_cycle, req.va, pa, req.st_value);
	       fa_write (pa, req.f3, req.st_value);
	       result = Cache_Result {outcome: CACHE_WRITE_HIT, final_ld_val: 0, final_st_val: req.st_value};

`ifdef ISA_A
	       // Cancel LR/SC reservation if this store is for this addr
	       // TODO: should we cancel it on ANY store?
	       if (rg_lrsc_pa == pa)
		  rg_lrsc_valid <= False;
`endif
	    end

`ifdef ISA_A
	    // AMO LR
	    else if (fv_is_AMO_LR (req)) begin
	       if (verbosity >= 1)
		  $display ("%0d: %m.mav_request_pa: LR-hit: va %0h pa %0h data %0h",
			    cur_cycle, req.va, pa, data);
	       rg_lrsc_valid <= True;
	       rg_lrsc_pa    <= pa;
	       rg_lrsc_size  <= req.f3 [1:0];
	       result = Cache_Result {outcome: CACHE_READ_HIT, final_ld_val: data, final_st_val: ?};
	    end

	    // AMO SC
	    else if (fv_is_AMO_SC (req)) begin
	       if (rg_lrsc_valid && (rg_lrsc_pa == pa)) begin
		  if (verbosity >= 1)
		     $display ("%0d: %m.mav_request_pa: SC-hit and success: va %0h pa %0h data %0h",
			       cur_cycle, req.va, pa, req.st_value);
		  rg_lrsc_valid <= False;
		  fa_write (pa, req.f3, req.st_value);
		  result = Cache_Result {outcome:      CACHE_WRITE_HIT,
					 final_ld_val: 0,    // SC success
					 final_st_val: req.st_value};
	       end
	       else begin
		  if (verbosity >= 1)
		     $display ("%0d: %m.mav_request_pa: SC-hit and fail: va %0h pa %0h data %0h",
			       cur_cycle, req.va, pa, req.st_value);
		  result = Cache_Result {outcome:      CACHE_READ_HIT,
					 final_ld_val: 1,    // SC fail
					 final_st_val: 0};
	       end
	    end

	    // All AMO read-modify-writes (i.e., AMO other than LR and SC)
	    else begin
	       dynamicAssert ((req.op == CACHE_AMO), "Cache: expecting AMO op here");
	       Fmt fmt_op = fshow_f5_AMO_op (req.amo_funct7 [6:2]);
	       if (verbosity >= 1) begin
		  $display ("%0d: %m.mav_request_pa: f3 %3b AMO ", cur_cycle, req.f3, fmt_op);
		  $display ("      va %0h  pa %0h  rs2_val %0h", req.va, pa, req.st_value);
		  $display ("      Cache cword %0h, load-result %0h", hit_miss_info.data, hit_miss_info.data);
	       end

	       let size_code  = req.f3 [1:0];
	       let cache_data = fv_from_byte_lanes (zeroExtend (req.va), size_code, hit_miss_info.data);
	       // Do the AMO op on the loaded value and the store value
	       match {.new_ld_val,
		      .new_st_val} = fv_amo_op (size_code, req.amo_funct7 [6:2], cache_data, req.st_value);

	       if (verbosity >= 1)
		  $display ("      ", fmt_op, " (%0h, %0h) -> %0h", cache_data, req.st_value, new_st_val);

	       fa_write (pa, req.f3, new_st_val);
	       result = Cache_Result {outcome:      CACHE_WRITE_HIT,
				      final_ld_val: new_ld_val,
				      final_st_val: new_st_val};

	       // Cancel LR/SC reservation if this store is for this addr
	       if (rg_lrsc_pa == pa)
		  rg_lrsc_valid <= False;
	    end
`endif
	 end
	 return result;
      endactionvalue
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
   // Memory interface (for refills, writebacks)

   interface Get g_mem_req       = toGet (f_line_reqs);
   interface Get g_write_data    = toGet (f_write_data);
   interface Put p_mem_read_data = toPut (f_read_data);
endmodule

// ================================================================

endpackage
