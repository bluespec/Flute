// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package Mem_Controller;

// ================================================================
// This module is a slave on the interconnect Fabric.
//
// On the back side of the Mem_Controller is a ``raw'' memory
// interface, a simple, wide, R/W interface,
// which is connected to real memory in hardware (BRAM, DRAM, ...)
// and to a model thereof in simulation.
//
// The raw mem interface data width is typically one or two cache lines.
// Note: raw mem write requests are 'fire and forget'; there is no ack

// ----------------
// This slave IP can be attached to fabrics with 32b- or 64b-wide data channels.
//    (NOTE: this is the width of the fabric, which can be chosen
//      independently of the native width of a CPU master on the
//      fabric (such as RV32/RV64 for a RISC-V CPU).
// When attached to 32b-wide fabric, 64-bit locations must be
// read/written in two 32b transaction, once for the lower 32b and
// once for the upper 32b.

// When fabric data is 64b wide, fabric addresses must be 8B-aligned
//    - Reads always return 64b data
//    - Write data should be 64b wide with an 8b byte-strobe indicating
//        which bytes are to be written. Strobes should be for aligned
//        1B, 2B, 4B or 8B chunks.

// When fabric data is 32b wide, fabric addresses must be 4B-aligned
//    - Reads always return 32b data
//    - Write data should be 32b wide with a 4b byte-strobe indicating
//        which bytes are to be written. Strobes should for aligned
//        1B, 2B, or 4B chunks.

// Some of the 'truncate()'s and 'zeroExtend()'s below are no-ops but
// necessary to satisfy type-checking.
// ================================================================

export

Bits_per_Raw_Mem_Addr,
Raw_Mem_Addr,

Bits_per_Raw_Mem_Word,
Raw_Mem_Word,

Mem_Controller_IFC (..),
mkMem_Controller,

status_mem_controller_terminated;

// ================================================================
// BSV library imports

import  Vector       :: *;
import  FIFOF        :: *;
import  SpecialFIFOs :: *;
import  GetPut       :: *;
import  ClientServer :: *;
import  Memory       :: *;
import  ConfigReg    :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;
import ByteLane   :: *;

// ================================================================
// Project imports

import Fabric_Defs :: *;
import SoC_Map     :: *;
import AXI4_Types  :: *;

// ================================================================
// Raw mem data width:    256 (bits/ 32 x Byte/ 8 x Word32/ 4 x Word64)
// Raw mem address width: 64  (arbitrarily chosen generously large)

typedef 256  Bits_per_Raw_Mem_Word;
typedef Bit #(Bits_per_Raw_Mem_Word)  Raw_Mem_Word;

typedef 64  Bits_per_Raw_Mem_Addr;
typedef Bit #(Bits_per_Raw_Mem_Addr)  Raw_Mem_Addr;

// ----------------
// Views of raw mem word as bytes, word32s and word64s
// Example values on the right based on 256b raw mem data width
typedef TDiv #(Bits_per_Raw_Mem_Word,  8)              Bytes_per_Raw_Mem_Word;             // 32 bytes
Integer bytes_per_raw_mem_word = valueOf (Bytes_per_Raw_Mem_Word);

// # of addr lsbs to index a byte in a Raw_Mem_Word
typedef TLog #(Bytes_per_Raw_Mem_Word)                 Bits_per_Byte_in_Raw_Mem_Word;      //  5
Integer bits_per_byte_in_raw_mem_word = valueOf (Bits_per_Byte_in_Raw_Mem_Word);
Integer hi_byte_in_raw_mem_word = bits_per_byte_in_raw_mem_word - 1;                       //  4

typedef TDiv #(Bits_per_Raw_Mem_Word, 32)              Word32s_per_Raw_Mem_Word;           //  8 x 32b words
Integer word32s_per_raw_mem_word = valueOf (Word32s_per_Raw_Mem_Word);
// # of addr lsbs to index a Word32 in a Raw_Mem_Word seen as a vector of Word32s
typedef TLog #(Word32s_per_Raw_Mem_Word)               Bits_per_Word32_in_Raw_Mem_Word;    //  3
// Type of index of a Word32 in a Raw_Mem_Word seen as a vector of Word32s
typedef Bit #(Bits_per_Word32_in_Raw_Mem_Word)         Word32_in_Raw_Mem_Word;

typedef TDiv #(Bits_per_Raw_Mem_Word, 64)              Word64s_per_Raw_Mem_Word;           //  4 x 64b words
Integer word64s_per_raw_mem_word = valueOf (Word64s_per_Raw_Mem_Word);
// # of addr lsbs to index a Word64 in a Raw_Mem_Word seen as a vector of Word64s
typedef TLog #(Word64s_per_Raw_Mem_Word)               Bits_per_Word64_in_Raw_Mem_Word;    //  2
// Type of index of a Word64 in a Raw_Mem_Word seen as a vector of Word64s
typedef Bit #(Bits_per_Word64_in_Raw_Mem_Word)         Word64_in_Raw_Mem_Word;

typedef TDiv #(Bytes_per_Raw_Mem_Word, Bytes_per_Fabric_Data)  Fabric_Data_per_Raw_Mem_Word;

// Index of bit that selects a fabric data word in an address
`ifdef FABRIC32
Integer  lo_fabric_data = 2;
`endif

`ifdef FABRIC64
Integer  lo_fabric_data = 3;
`endif

// ================================================================

function Bool fn_addr_is_aligned (Fabric_Addr  addr, AXI4_Size  size);
   Bool is_aligned = (   (size == axsize_1)
		      || ((size == axsize_2)    && (addr [0]   == 1'h0))
		      || ((size == axsize_4)    && (addr [1:0] == 2'h0))
		      || ((size == axsize_8)    && (addr [2:0] == 3'h0))
		      || ((size == axsize_16)   && (addr [3:0] == 4'h0))
		      || ((size == axsize_32)   && (addr [4:0] == 5'h0))
		      || ((size == axsize_64)   && (addr [5:0] == 6'h0))
		      || ((size == axsize_128)  && (addr [6:0] == 7'h0)));
   return is_aligned;
endfunction

function Bool fn_addr_is_in_range (Fabric_Addr addr_base, Fabric_Addr addr, Fabric_Addr addr_lim);
   // Note: 'in_range' is redundant if the fabric only delivers
   // relevant addresses to this module so this is just a bit of
   // defensive programming.

   return ((addr_base <= addr) && (addr < addr_lim));
endfunction

function Bool fn_addr_is_ok (Fabric_Addr addr_base, Fabric_Addr addr, Fabric_Addr addr_lim, AXI4_Size  size);
   return (   fn_addr_is_aligned (addr, size)
	   && fn_addr_is_in_range (addr_base, addr, addr_lim));
endfunction

// Compute raw mem addr that holds a given fabric addr
function Raw_Mem_Addr fn_addr_to_raw_mem_addr (Fabric_Addr addr);
   Fabric_Addr a1 = addr >> log2 (bytes_per_raw_mem_word);
   return extend (a1);
endfunction

// Compute # of raw mem words from base to lim fabric addrs
function Raw_Mem_Addr fn_raw_mem_words_per_mem (Fabric_Addr base, Fabric_Addr lim);
   return fn_addr_to_raw_mem_addr (lim - base);
endfunction

// ================================================================
// Local constants and types

// Module state
typedef enum {STATE_POWER_ON_RESET,
`ifdef INCLUDE_INITIAL_MEMZERO
	      STATE_ZEROING_MEM,           // while zero-ing out memory
`endif
	      STATE_RESET_RELOAD_CACHE,    // on reset, start reload on reset
	      STATE_RELOADING,             // while reloading the raw-mem word cache
	      STATE_READY                  // while handling requests
   } State
deriving (Bits, Eq, FShow);

// ================================================================
// Catch-all status

Integer status_mem_controller_terminated = 1;

// ================================================================
// Interface

interface Mem_Controller_IFC;
   // Reset
   interface Server #(Bit #(0), Bit #(0)) server_reset;

   // set_addr_map should be called after this module's reset
   method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);

   // Main Fabric Reqs/Rsps
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave;

   // To raw memory (outside the SoC)
   interface MemoryClient #(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word)  to_raw_mem;

   // Catch-all status; return-value can identify the origin (0 = none)
   (* always_ready *)
   method Bit #(8) status;

   // For ISA tests: watch memory writes to <tohost> addr
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
endinterface

// ================================================================
// AXI4 has independent read and write channels and does not specify
// which one should be prioritized if requests are available on both
// channels.  We merge them into a single queue.

typedef enum { REQ_OP_RD, REQ_OP_WR } Req_Op
deriving (Bits, Eq, FShow);

typedef struct {Req_Op                     req_op;

		// AW and AR channel info
		Fabric_Id                  id;
		Fabric_Addr                addr;
		AXI4_Len                   len;
		AXI4_Size                  size;
		AXI4_Burst                 burst;
		AXI4_Lock                  lock;
		AXI4_Cache                 cache;
		AXI4_Prot                  prot;
		AXI4_QoS                   qos;
		AXI4_Region                region;
		Bit #(Wd_User)             user;

		// Write data info
		Bit #(TDiv #(Wd_Data, 8))  wstrb;
		Fabric_Data                data;
   } Req
deriving (Bits, FShow);

// ================================================================

(* synthesize *)
module mkMem_Controller (Mem_Controller_IFC);

   // verbosity 0: quiet
   // verbosity 1: reset, initialized
   // verbosity 2: reads, writes
   // verbosity 3: more detail of local raw_mem interactions
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (0);

   Reg #(State)       rg_state     <- mkReg (STATE_POWER_ON_RESET);
   Reg #(Fabric_Addr) rg_addr_base <- mkRegU;
   Reg #(Fabric_Addr) rg_addr_lim  <- mkRegU;

   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

   // Communication with fabric
   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Slave_Xactor;

   // Requests merged from the (WrA, WrD) and RdA channels
   FIFOF #(Req) f_reqs <- mkPipelineFIFOF;

   // FIFOFs for requests/responses to raw memory
   FIFOF #(MemoryRequest  #(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word))
       f_raw_mem_reqs <- mkPipelineFIFOF;
   FIFOF #(MemoryResponse #(Bits_per_Raw_Mem_Word))
       f_raw_mem_rsps <- mkPipelineFIFOF;

   // We maintain a 1-raw_mem_word cache
   Reg #(Bool)          rg_cached_clean        <- mkRegU;
   Reg #(Raw_Mem_Addr)  rg_cached_raw_mem_addr <- mkRegU;
   Reg #(Raw_Mem_Word)  rg_cached_raw_mem_word <- mkRegU;

   // Ad hoc ISA-test simulation support: watch <tohost> and stop on non-zero write.
   // The default tohost_addr here is fragile (may change on recompilation of tests).
   // Proper value can be provided with 'set_watch_tohost' method from symbol table
   Reg #(Bool)        rg_watch_tohost <- mkReg (False);
   Reg #(Fabric_Addr) rg_tohost_addr  <- mkReg ('h_8000_1000);

   // Catch-all status
   Reg #(Bit #(8)) rg_status <- mkReg (0);

   // ================================================================
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Reset

   function Action fa_reset_actions;
      action
	 slave_xactor.reset;
	 f_raw_mem_reqs.clear;
	 f_raw_mem_rsps.clear;
	 rg_status <= 0;
      endaction
   endfunction

   rule rl_power_on_reset (rg_state == STATE_POWER_ON_RESET);
      if (cfg_verbosity > 1)
	 $display ("%0d: Mem_Controller.rl_power_on_reset", cur_cycle);
      fa_reset_actions ();
      rg_state <= STATE_RESET_RELOAD_CACHE;
   endrule

   rule rl_external_reset (rg_state == STATE_READY);
      if (cfg_verbosity > 1)
	 $display ("%0d: Mem_Controller.rl_external_reset => STATE_RESET_RELOAD_CACHE", cur_cycle);

      f_reset_reqs.deq;
      fa_reset_actions ();
      rg_state <= STATE_RESET_RELOAD_CACHE;
      f_reset_rsps.enq (?);
   endrule

   // On reset, we initialize the local cache with contents of raw_mem_addr 0
   rule rl_reset_reload_cache (rg_state == STATE_RESET_RELOAD_CACHE);
      let raw_mem_req = MemoryRequest {write:   False,
				       byteen:  '1,
				       address: 0,
				       data:    ?};
      f_raw_mem_reqs.enq (raw_mem_req);
      rg_cached_raw_mem_addr <= 0;
      rg_state <= STATE_RELOADING;
      if (cfg_verbosity > 1)
	 $display ("%0d: Mem_Controller.rl_reset_reload_cache => STATE_RELOADING", cur_cycle);
   endrule

   // ----------------------------------------------------------------
   // Merge requests into a single queue, prioritizing reads over writes

   rule rl_merge_rd_req;
      let rda <- pop_o (slave_xactor.o_rd_addr);
      let req = Req {req_op:     REQ_OP_RD,
		     id:         rda.arid,
		     addr:       rda.araddr,
		     len:        rda.arlen,
		     size:       rda.arsize,
		     burst:      rda.arburst,
		     lock:       rda.arlock,
		     cache:      rda.arcache,
		     prot:       rda.arprot,
		     qos:        rda.arqos,
		     region:     rda.arregion,
		     user:       rda.aruser,
		     wstrb:      ?,
		     data:       ?};
      f_reqs.enq (req);

      if (cfg_verbosity > 2) begin
	 $display ("%0d: Mem_Controller.rl_merge_rd_req", cur_cycle);
	 $display ("        ", fshow (rda));
      end
   endrule

   (* descending_urgency = "rl_merge_rd_req, rl_merge_wr_req" *)
   rule rl_merge_wr_req;
      let wra <- pop_o (slave_xactor.o_wr_addr);
      let wrd <- pop_o (slave_xactor.o_wr_data);
      let req = Req {req_op:     REQ_OP_WR,
		     id:         wra.awid,
		     addr:       wra.awaddr,
		     len:        wra.awlen,
		     size:       wra.awsize,
		     burst:      wra.awburst,
		     lock:       wra.awlock,
		     cache:      wra.awcache,
		     prot:       wra.awprot,
		     qos:        wra.awqos,
		     region:     wra.awregion,
		     user:       wra.awuser,
		     wstrb:      wrd.wstrb,
		     data:       wrd.wdata};
      f_reqs.enq (req);

      if (cfg_verbosity > 2) begin
	 $display ("%0d: Mem_Controller.rl_merge_wr_req", cur_cycle);
	 $display ("        ", fshow (wra));
	 $display ("        ", fshow (wrd));
      end
   endrule

   // ----------------------------------------------------------------
   // Handle request from fabric

   let req_byte_offset  = f_reqs.first.addr - rg_addr_base;    // within this memory unit
   let req_raw_mem_addr = fn_addr_to_raw_mem_addr (req_byte_offset);

   // ----------------
   // This rule fires when there's no fabric req and the cached raw_mem_word is dirty;
   // it writes back the dirty raw_mem_word; the cached raw_mem_word becomes clean

   rule rl_writeback_dirty_idle (   (rg_state == STATE_READY)
				 && (! f_reqs.notEmpty)           // Idle
				 && (! rg_cached_clean));
      let raw_mem_req = MemoryRequest {write:   True,
				       byteen:  '1,
				       address: rg_cached_raw_mem_addr,
				       data:    rg_cached_raw_mem_word};
      f_raw_mem_reqs.enq (raw_mem_req);
      rg_cached_clean <= True;
      if (cfg_verbosity > 2)
	 $display ("%0d: Mem_Controller.rl_writeback_dirty_idle to raw addr 0x%0h",
		   cur_cycle, rg_cached_raw_mem_addr);
   endrule

   // ----------------
   // This rule fires on a fabric req when the cached raw_mem_word has a
   // different raw_mem_word-addr and is dirty;
   // it writes back the dirty raw_mem_word; the cached raw_mem_word becomes clean

   rule rl_writeback_dirty (   (rg_state == STATE_READY)
			    && fn_addr_is_ok (rg_addr_base, f_reqs.first.addr, rg_addr_lim, f_reqs.first.size)
			    && (rg_cached_raw_mem_addr != req_raw_mem_addr)
			    && (! rg_cached_clean));
      let raw_mem_req = MemoryRequest {write:   True,
				       byteen:  '1,
				       address: rg_cached_raw_mem_addr,
				       data:    rg_cached_raw_mem_word};
      f_raw_mem_reqs.enq (raw_mem_req);
      rg_cached_clean <= True;
      if (cfg_verbosity > 2)
	 $display ("%0d: Mem_Controller.rl_writeback_dirty to raw addr 0x%0h",
		   cur_cycle, rg_cached_raw_mem_addr);
   endrule

   // ----------------
   // This rule fires on a fabric req when the cached raw_mem_word has a
   // different addr and is clean; we overwrite with the correct raw_mem_word
   // by reloading from memory; the new cached raw_mem_word is clean.

   rule rl_miss_clean_req (   (rg_state == STATE_READY)
			   && fn_addr_is_ok (rg_addr_base, f_reqs.first.addr, rg_addr_lim, f_reqs.first.size)
			   && (rg_cached_raw_mem_addr != req_raw_mem_addr)
			   && rg_cached_clean);
      let raw_mem_req = MemoryRequest {write:   False,
				       byteen:  '1,
				       address: req_raw_mem_addr,
				       data:    ?};
      f_raw_mem_reqs.enq (raw_mem_req);
      rg_cached_raw_mem_addr <= req_raw_mem_addr;
      rg_state <= STATE_RELOADING;

      if (cfg_verbosity > 2)
	 $display ("%0d: Mem_Controller.rl_miss_clean_req: read raw addr 0x%0h",
		   cur_cycle, req_raw_mem_addr);
   endrule

   rule rl_reload (rg_state == STATE_RELOADING);
      let raw_mem_rsp <- pop (f_raw_mem_rsps);
      Raw_Mem_Word raw_mem_word = unpack (raw_mem_rsp.data);
      rg_cached_raw_mem_word <= raw_mem_word;
      rg_state <= STATE_READY;
      rg_cached_clean <= True;

      if (cfg_verbosity > 2) begin
	 $display ("%0d: Mem_Controller.rl_reload: raw addr 0x%0h", cur_cycle, rg_cached_raw_mem_addr);
	 $display ("        ", fshow (raw_mem_word));
      end
   endrule

   // ----------------
   // This rule fires on a fabric read request when the cached raw_mem_word has the
   // same addr ('hit'), whether clean or dirty.
   // Returns the full Wd_Data-wide word containing the byte specified by the address.
   // i.e., we do not extract relevant bytes here, leaving that to the requestor.

   rule rl_process_rd_req  (   (rg_state == STATE_READY)
			    && fn_addr_is_ok (rg_addr_base, f_reqs.first.addr, rg_addr_lim, f_reqs.first.size)
			    && (rg_cached_raw_mem_addr == req_raw_mem_addr)
			    && (f_reqs.first.req_op == REQ_OP_RD));

      // ----------------
      // We need to select the fabric data word from the raw mem word that contains the target address.

      // View the raw mem word as a vector of fabric data words (Wd_Data width words)
      Vector #(Fabric_Data_per_Raw_Mem_Word, Bit #(Wd_Data)) raw_mem_word_V_fabric_data = unpack (rg_cached_raw_mem_word);

      // Get the index into this vector of the fabric word containing the target address.
      // For this index, use a generous size (here Bit #(16)), and let zeroExtend pad it automaticallly.
      Fabric_Addr addr = f_reqs.first.addr;
      Bit #(Bits_per_Byte_in_Raw_Mem_Word) n = addr [hi_byte_in_raw_mem_word : 0];
      n = (n >> lo_fabric_data);

      // Select the fabric data word of interest
      Bit #(Wd_Data) rdata = raw_mem_word_V_fabric_data [n];

      let rdr = AXI4_Rd_Data {rid:   f_reqs.first.id,
			      rdata: rdata,
			      rresp: axi4_resp_okay,
			      rlast: True,
			      ruser: f_reqs.first.user};
      slave_xactor.i_rd_data.enq (rdr);
      f_reqs.deq;

      if (cfg_verbosity > 1) begin
	 $display ("%0d: Mem_Controller.rl_process_rd_req: ", cur_cycle);
	 $display ("        ", fshow (f_reqs.first));
	 $display ("     => ", fshow (rdr));
      end
   endrule

   // ----------------
   // This rule fires on a fabric write request when the cached raw_mem_word has the
   // same addr ('hit'), whether clean or dirty.

   rule rl_process_wr_req  (   (rg_state == STATE_READY)
			    && fn_addr_is_ok (rg_addr_base, f_reqs.first.addr, rg_addr_lim, f_reqs.first.size)
			    && (rg_cached_raw_mem_addr == req_raw_mem_addr)
			    && (f_reqs.first.req_op == REQ_OP_WR));
      // Get the old (cached) value of the word64
      Word64_in_Raw_Mem_Word word64_in_raw_mem_word = f_reqs.first.addr [hi_byte_in_raw_mem_word : 3];
      Vector #(Word64s_per_Raw_Mem_Word, Bit #(64)) raw_mem_word_V_Word64 = unpack (rg_cached_raw_mem_word);
      Bit #(64) word64_old = raw_mem_word_V_Word64 [word64_in_raw_mem_word];

      // Lane-adjust the new word64
      Bit #(64) word64_new = zeroExtend (f_reqs.first.data);
      Bit #(8)  strobe     = zeroExtend (f_reqs.first.wstrb);
      if ((valueOf (Wd_Data) == 32) && (f_reqs.first.addr [2] == 1'b1)) begin
	 // Upper 32b only
	 word64_new = { word64_new [31:0], 0 };
	 strobe     = { strobe     [3:0],  0 };
      end
      Bit #(64) mask     = fn_strobe_to_mask (strobe);
      let updated_word64 = ((word64_old & (~ mask)) | (word64_new & mask));

      // Write it back into the cached raw_mem_word
      raw_mem_word_V_Word64 [word64_in_raw_mem_word] = updated_word64;
      rg_cached_raw_mem_word <= pack (raw_mem_word_V_Word64);
      rg_cached_clean        <= False;

      let wrr = AXI4_Wr_Resp {bid:   f_reqs.first.id,
			      bresp: axi4_resp_okay,
			      buser: f_reqs.first.user};
      slave_xactor.i_wr_resp.enq (wrr);
      f_reqs.deq;

      if (cfg_verbosity > 1) begin
	 $display ("%0d: Mem_Controller.rl_process_wr_req: ", cur_cycle);
	 $display ("        ", fshow (f_reqs.first));
	 $display ("     => ", fshow (wrr));
      end

      // For simulation testing of riscv-tests/isa only:
      if ((rg_watch_tohost)
	  && (f_reqs.first.addr == rg_tohost_addr)
	  && (word64_new != 0))
	 begin

	    $display ("%0d: Mem_Controller.rl_process_wr_req: addr 0x%0h (<tohost>) data 0x%0h",
		      cur_cycle, f_reqs.first.addr, word64_new);
	    let exit_value = (word64_new >> 1);
	    if (exit_value == 0)
	       $display ("PASS");
	    else
	       $display ("FAIL %0d", exit_value);
	    rg_status <= fromInteger (status_mem_controller_terminated);
	 end
   endrule

   // ================================================================
   // Zero-memory FSM: zero out memory.
   // If needed, we must provide a way to enable it from the debug_module
   // This rule should be enabled with:
   //     rg_cached_raw_mem_addr <= 0;
   //     rg_state               <= STATE_ZEROING_MEM;

`ifdef INCLUDE_INITIAL_MEMZERO
   rule rl_zero_mem (rg_state == STATE_ZEROING_MEM);
      let raw_mem_req = MemoryRequest {write:   True,
				       byteen:  '1,
				       address: rg_cached_raw_mem_addr,
				       data:    0};
      f_raw_mem_reqs.enq (raw_mem_req);

      // Last write
      let raw_mem_words_per_mem = fn_raw_mem_words_per_mem (rg_addr_base, rg_addr_lim);
      if (rg_cached_raw_mem_addr == (raw_mem_words_per_mem - 1)) begin
	 rg_cached_raw_mem_addr <= rg_cached_raw_mem_addr;
	 rg_cached_raw_mem_word <= unpack (0);
	 rg_cached_clean        <= True;
	 rg_state <= STATE_READY;

	 // if (cfg_verbosity != 0)
	    $display ("%0d: Mem_Controller: zeroed %0d raw-memory locations (%0d-bit words)",
		      cur_cycle, raw_mem_words_per_mem, valueOf (Bits_per_Raw_Mem_Word));
      end
      else
	 rg_cached_raw_mem_addr <= rg_cached_raw_mem_addr + 1;
   endrule
`endif

   // ================================================================
   // Invalid address

   rule rl_invalid_rd_address (   (rg_state == STATE_READY)
			       && (! fn_addr_is_ok (rg_addr_base, f_reqs.first.addr, rg_addr_lim, f_reqs.first.size))
			       && (f_reqs.first.req_op == REQ_OP_RD));
      Fabric_Data rdata = zeroExtend (f_reqs.first.addr);
      let rdr = AXI4_Rd_Data {rid:   f_reqs.first.id,
			      rdata: rdata,                 // for debugging only
			      rresp: axi4_resp_slverr,
			      rlast: True,
			      ruser: f_reqs.first.user};
      slave_xactor.i_rd_data.enq (rdr);
      f_reqs.deq;

      $write ("%0d: ERROR: Mem_Controller:", cur_cycle);
      if (! fn_addr_is_aligned (f_reqs.first.addr, f_reqs.first.size))
	 $display (" read-addr is misaligned");
      else
	 $display (" read-addr is out of bounds");
      $display ("        rg_addr_base 0x%0h  rg_addr_lim 0x%0h", rg_addr_base, rg_addr_lim);
      $display ("        ", fshow (f_reqs.first));
      $display ("     => ", fshow (rdr));
   endrule

   rule rl_invalid_wr_address (   (rg_state == STATE_READY)
			       && (! fn_addr_is_ok (rg_addr_base, f_reqs.first.addr, rg_addr_lim, f_reqs.first.size))
			       && (f_reqs.first.req_op == REQ_OP_WR));
      let wrr = AXI4_Wr_Resp {bid:   f_reqs.first.id,
			      bresp: axi4_resp_slverr,
			      buser: f_reqs.first.user};
      slave_xactor.i_wr_resp.enq (wrr);
      f_reqs.deq;

      $write ("%0d: ERROR: Mem_Controller:", cur_cycle);
      if (! fn_addr_is_aligned (f_reqs.first.addr, f_reqs.first.size))
	 $display (" write-addr is misaligned");
      else
	 $display (" write-addr is out of bounds");
      $display ("        rg_addr_base 0x%0h  rg_addr_lim 0x%0h", rg_addr_base, rg_addr_lim);
      $display ("        ", fshow (f_reqs.first));
      $display ("     => ", fshow (wrr));
   endrule

   // ================================================================
   // INTERFACE

   // Reset
   interface  server_reset = toGPServer (f_reset_reqs, f_reset_rsps);

   // set_addr_map should be called after this module's reset
   method Action  set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim) if (rg_state == STATE_READY);
      rg_addr_base <= addr_base;
      rg_addr_lim  <= addr_lim;
      $display ("%0d: Mem_Controller.set_addr_map: addr_base 0x%0h addr_lim 0x%0h",
		cur_cycle, addr_base, addr_lim);

`ifdef INCLUDE_INITIAL_MEMZERO
      rg_cached_raw_mem_addr <= 0;
      rg_state               <= STATE_ZEROING_MEM;
      $display ("%0d: Mem_Controller.set_addr_map: zeroing memory from 0x%0h to 0x%0h",
		cur_cycle, addr_base, addr_lim);
`endif
   endmethod

   // Main Fabric Reqs/Rsps
   interface  slave = slave_xactor.axi_side;

   // To raw memory (outside the SoC)
   interface  to_raw_mem = toGPClient (f_raw_mem_reqs, f_raw_mem_rsps);

   // Catch-all status; return-value can identify the origin (0 = none)
   method Bit #(8) status;
      return rg_status;
   endmethod

   // For ISA tests: watch memory writes to <tohost> addr
   method Action set_watch_tohost (Bool watch_tohost, Fabric_Addr tohost_addr);
      rg_watch_tohost <= watch_tohost;
      rg_tohost_addr  <= tohost_addr;
   endmethod
endmodule

// ================================================================

endpackage
