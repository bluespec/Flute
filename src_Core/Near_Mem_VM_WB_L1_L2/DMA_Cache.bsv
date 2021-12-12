// Copyright (c) 2016-2021 Bluespec, Inc. All Rights Reserved.

package DMA_Cache;

// ================================================================
// This is an L1 Cache, coherent with other I- and D- caches.
// Can be used for coherent access to mem from devices.
// All addresses are physical addresses.
// All transactions are full AXI4 width (512b in AWSteria)

// The 'front' side faces the external device: an AXI4 'S' interface.
// The 'back' side has two interfaces:
// - For cacheable addresses: to the next-level cache (L2, usually),
//                            with MESI coherence protocol
// - For all other addresses: to MMIO

// Note on terminology:
// - M,S = 'manager','subordinate'
// - Used to be 'master', 'slave'.
// - ARM officially adopted the new terminology circa 2021.
// - We'll just use M and S (we also use Client and Server, respectively).

// ================================================================
// BSV lib imports

import Vector       :: *;
import RegFile      :: *;
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

// ----------------
// AXI

import AXI4_Types :: *;
import AXI4_to_LD :: *;
import AXI4_to_ST :: *;

// ================================================================
// Project imports

import ISA_Decls        :: *;
import Near_Mem_IFC     :: *;    // For Wd_{Id/Addr/User/Data}_Mem
import AXI_Widths       :: *;    // For Wd_{Id/Addr/User/Data}_Dma
import SoC_Map          :: *;    // For predicate on addresses (cacheble or not)
import MMU_Cache_Common :: *;    // For L1<->L2 reqs and rsps, Single_Req/Rsp for MMIO

// ================================================================

export  DMA_Cache_IFC (..),  mkDMA_Cache;

// ================================================================
// MODULE INTERFACE

interface DMA_Cache_IFC;
   // Facing external client
   interface AXI4_Slave_IFC #(Wd_Id_Dma,
			      Wd_Addr_Dma,
			      Wd_Data_Dma,
			      Wd_User_Dma)    axi4_s;

   // ----------------
   // Cache-line interface facing next level cache or memory
   // (for refills, writebacks, downgrades, ...)

   interface Client_Semi_FIFOF #(L1_to_L2_Req, L2_to_L1_Rsp)  l1_to_l2_client;
   interface Server_Semi_FIFOF #(L2_to_L1_Req, L1_to_L2_Rsp)  l2_to_l1_server;

   // ----------------
   // MMIO interface facing memory

   interface Client #(Single_Req, Single_Rsp) mmio_client;

endinterface

// ****************************************************************
// ****************************************************************
// ****************************************************************
// Internal types and constants.

typedef Bit #(Wd_Data_Dma)      Data;                   // Data on AXI4
typedef TDiv #(Wd_Data_Dma, 8)  Bytes_per_Data;         // 512/8 = 64
typedef TLog #(Bytes_per_Data)  Bits_per_Byte_in_Data;  // log (64) = 6

Integer bits_per_byte_in_data  = valueOf (Bits_per_Byte_in_Data);

typedef 1   DMA_Cache_Num_Ways;    // Direct-mapped, for now
typedef 64  DMA_Cache_Num_Sets;    // Cache holds 64x64 bytes = 4 KB = 1 page

typedef TLog #(DMA_Cache_Num_Sets)    DMA_Cache_Index_Width;
typedef Bit #(DMA_Cache_Index_Width)  DMA_Cache_Index;

function DMA_Cache_Index fn_addr_to_index (Bit #(64) addr);
   return truncate (addr >> bits_per_byte_in_data);
endfunction

function Bit #(64) fn_addr_to_line_addr (Bit #(64) addr);
   return ((addr >> bits_per_byte_in_data) << bits_per_byte_in_data);
endfunction

typedef struct {
   Meta_State          tag_state;
   Bit #(Wd_Addr_Dma)  tag_addr;
   } DMA_Cache_Tag
deriving (Bits, FShow);

typedef Vector #(DMA_Cache_Num_Ways, DMA_Cache_Tag) Tag_Set;
typedef Vector #(DMA_Cache_Num_Ways, Data)          Data_Set;

// ----------------
// Fix these functions if we move to more set-associative cache

function Tag_Set fn_mk_Tag_Set (DMA_Cache_Tag tag0);
   Tag_Set tag_set = ?;
   tag_set [0] = tag0;
   return tag_set;
endfunction

function Data_Set fn_mk_Data_Set (Data data0);
   Data_Set data_set = ?;
   data_set [0] = data0;
   return data_set;
endfunction

// ----------------
// State machine

typedef enum {STATE_INITIALIZING,    // Setting all cache tags to Invalid
              STATE_IDLE,            // Ready to service external requests
	      STATE_EVICTED,         // Data evicted, ready to request new data
	      STATE_UPGRADE          // refill/upgrade request sent, awaiting response
   } State
deriving (Bits, Eq, FShow);

// ================================================================
// AXI4 has independent read-req, write-req and write-data channels.
// Here, we merge them into a single queue using this merged struct.

typedef enum { AXI4_OP_RD, AXI4_OP_WR } AXI4_Op
deriving (Bits, Eq, FShow);

typedef struct {AXI4_Op                    req_op;

		// AW and AR channel info
		Bit #(Wd_Id_Dma)           id;
		Bit #(Wd_Addr_Dma)         addr;
		AXI4_Len                   len;
		AXI4_Size                  size;
		AXI4_Burst                 burst;
		AXI4_Lock                  lock;
		AXI4_Cache                 cache;
		AXI4_Prot                  prot;
		AXI4_QoS                   qos;
		AXI4_Region                region;
		Bit #(Wd_User_Dma)         user;

		// Write data info
		Bit #(TDiv #(Wd_Data_Dma, 8))  wstrb;
		Bit #(Wd_Data_Dma)             wdata;
   } AXI4_Req
deriving (Bits, FShow);

// ================================================================
// MODULE IMPLEMENTATION
                
(* synthesize *)
module mkDMA_Cache (DMA_Cache_IFC);

   // For debugging: 0: quiet 1: rules
   Integer verbosity         = 0;
   Integer verbosity_merge   = 0;
   Integer verbosity_MMIO_rd = 0;
   Integer verbosity_MMIO_wr = 0;

   // ----------------
   // SoC_Map needed for 'is_cached_addr' (vs. MMIO) check
   SoC_Map_IFC soc_map <- mkSoC_Map;

   function Bool fn_is_cached_addr (Bit #(Wd_Addr_Dma) addr);
      return soc_map.m_is_mem_addr (addr);
   endfunction

   // ----------------
   // FSM state

   Reg #(State) rg_state <- mkReg (STATE_INITIALIZING);

   // Transactor for external client
   AXI4_Slave_Xactor_IFC #(Wd_Id_Dma,
			   Wd_Addr_Dma,
			   Wd_Data_Dma,
			   Wd_User_Dma) axi4_s_xactor <- mkAXI4_Slave_Xactor;

   // ----------------
   // Cache structures
   RegFile #(DMA_Cache_Index, Tag_Set)  rf_tag_sets <- mkRegFileFull;
   RegFile #(DMA_Cache_Index, Data_Set) rf_data_sets <- mkRegFileFull;


   // DMA Cache to L2 requests and responses
   FIFOF #(L1_to_L2_Req) f_L1_to_L2_Reqs <- mkFIFOF;
   FIFOF #(L2_to_L1_Rsp) f_L2_to_L1_Rsps <- mkFIFOF;

   // L2 to DMA Cache requests and responses
   FIFOF #(L2_to_L1_Req) f_L2_to_L1_Reqs <- mkFIFOF;
   FIFOF #(L1_to_L2_Rsp) f_L1_to_L2_Rsps <- mkFIFOF;

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR

   // ================================================================
   // Initialization: loop through all cache sets, setting all tags to INVALID

   Reg #(Bit #(DMA_Cache_Index_Width)) rg_init_index <- mkReg (0);

   rule rl_init (rg_state == STATE_INITIALIZING);
      if ((verbosity >= 1) && (rg_init_index == 0)) begin
	 $display ("%0d: %m.rl_init: Initializing cache ...", cur_cycle);
      end

      let tag     = DMA_Cache_Tag { tag_state: META_INVALID, tag_addr: 0 };
      let tag_set = fn_mk_Tag_Set (tag);
      rf_tag_sets.upd (rg_init_index, tag_set);
      if (rg_init_index == '1) begin
	 rg_state <= STATE_IDLE;
	 if (verbosity >= 1)
	    $display ("    ... done");
      end
      else
	 rg_init_index <= rg_init_index + 1;
   endrule

   // ================================================================
   // Merge cacheable requests into a single queue, prioritizing reads over writes

   FIFOF #(AXI4_Req) f_reqs <- mkFIFOF;

   rule rl_merge_rd_req (fn_is_cached_addr (axi4_s_xactor.o_rd_addr.first.araddr));
      let rda <- pop_o (axi4_s_xactor.o_rd_addr);

      // Assert arlen = 0 (single beat)
      if (rda.arlen != 0) begin
	 $display ("%0d: ERROR: %m.rl_merge_rd_req: burst requests not supported",
		   cur_cycle);
	 $display ("    ", fshow (rda));
	 $finish (1);
      end

      let req = AXI4_Req {req_op:     AXI4_OP_RD,
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
			  wdata:      ?};
      f_reqs.enq (req);

      if (verbosity_merge > 0) begin
	 $display ("%0d: %m.rl_merge_rd_req", cur_cycle);
	 $display ("        ", fshow (rda));
      end
   endrule

   (* descending_urgency = "rl_merge_rd_req, rl_merge_wr_req" *)
   rule rl_merge_wr_req (fn_is_cached_addr (axi4_s_xactor.o_wr_addr.first.awaddr));
      let wra <- pop_o (axi4_s_xactor.o_wr_addr);
      let wrd <- pop_o (axi4_s_xactor.o_wr_data);

      // Assert awlen = 0 (single beat)
      if (wra.awlen != 0) begin
	 $display ("%0d: ERROR: %m.rl_merge_wr_req: burst requests not supported",
		   cur_cycle);
	 $display ("    ", fshow (wra));
	 $finish (1);
      end

      let req = AXI4_Req {req_op:     AXI4_OP_WR,
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
			  wdata:      wrd.wdata};
      f_reqs.enq (req);

      if (verbosity_merge > 0) begin
	 $display ("%0d: %m.rl_merge_wr_req", cur_cycle);
	 $display ("    ", fshow (wra));
	 $display ("    ", fshow (wrd));
      end
   endrule

   // ================================================================
   // Handle downgrade-request from L2 (highest priority)

   rule rl_downgrade (rg_state != STATE_INITIALIZING);
      let l2_to_l1_req <- pop (f_L2_to_L1_Reqs);
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_downgrade", cur_cycle);
	 $display ("    ", fshow (l2_to_l1_req));
      end

      // Probe tag mem for this address and data
      let             line_addr = fn_addr_to_line_addr (l2_to_l1_req.addr);
      DMA_Cache_Index index     = fn_addr_to_index (line_addr);
      let             tag_set   = rf_tag_sets.sub  (index);
      let             data_set  = rf_data_sets.sub (index);
      let             tag       = tag_set  [0];    // For now, direct mapped only
      let             data      = data_set [0];    // For now, direct mapped only
      if (verbosity >= 1) begin
	 $display ("    Index %0h  ", index, fshow (tag));
      end

      // Ignore if already INVALID, or already in requested to-state,
      // or already has different address
      Bool ignore = (   (tag.tag_state == META_INVALID)
		     || ((tag.tag_state <= l2_to_l1_req.to_state) && (tag.tag_addr == line_addr))
		     || (tag.tag_addr != line_addr));

      if (ignore) begin
	 if (verbosity >= 1) begin
	    $display ("    Miss (already evicted/downgraded); no action");
	 end
      end
      else begin
	 // Compose and return response
	 let m_cline = ((tag.tag_state == META_MODIFIED)
			? tagged Valid (data)
			: tagged Invalid);
	 let rsp = L1_to_L2_Rsp {addr:     line_addr,
				 to_state: l2_to_l1_req.to_state,
				 m_cline:  m_cline};
	 f_L1_to_L2_Rsps.enq (rsp);

	 if (verbosity >= 1)
	    $display ("    Send ", fshow (rsp));

	 // Update cache tag to state requested by L2
	 let new_tag = DMA_Cache_Tag {tag_state: l2_to_l1_req.to_state,
				      tag_addr:  line_addr };
	 tag_set [0] = new_tag;        // Direct mapped only, for now
	 rf_tag_sets.upd(index, tag_set);
      end
   endrule

   // ================================================================
   // Some convenience values for head of merged queue (current request)

   let req       = f_reqs.first;
   let req_op    = req.req_op;
   let addr      = req.addr;
   let line_addr = fn_addr_to_line_addr (addr);
   let index     = fn_addr_to_index (addr);
   let tag_set   = rf_tag_sets.sub (index);
   let tag       = tag_set [0];    // Direct mapped only, for now

   // ================================================================
   // Evictions on conflict-miss
   // (Cache frame for AXI4 request's address is currently occupied by
   //  data with a different address; needs to be handed back to L2).

   Bool evict = (   (tag.tag_state != META_INVALID)
		 && (tag.tag_addr  != line_addr));

   rule rl_evict ((rg_state == STATE_IDLE)
		  && (! f_L2_to_L1_Reqs.notEmpty)    // L2-to-L1 requests have priority
		  && evict);
      // Evict current occupant (=> INVALID), and evict data if modified
      let data_set = rf_data_sets.sub (index);
      let data     = data_set [0];    // Direct mapped only, for now
      let m_cline  = ((tag.tag_state != META_MODIFIED)
		      ? tagged Invalid
		      : tagged Valid data);
      let l1_to_l2_rsp = L1_to_L2_Rsp {addr:     tag.tag_addr,
				       to_state: META_INVALID,
				       m_cline:  m_cline };
      f_L1_to_L2_Rsps.enq (l1_to_l2_rsp);

      // Set cache frame tag to INVALID
      let new_tag     = DMA_Cache_Tag {tag_state: META_INVALID, tag_addr:  ? };
      let new_tag_set = fn_mk_Tag_Set (new_tag);
      rf_tag_sets.upd (index, new_tag_set);

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_evict", cur_cycle);
	 $display ("    ", fshow (l1_to_l2_rsp));
      end
   endrule

   // ================================================================
   // For write reqs, update data into the cache line and send AXI4 response
   // For read  reqs, read data from the cache line and send AXI4 response

   function Action fa_respond_to_client (Bool err, Data rdata);
      action
	 let axi4_resp = (err ? axi4_resp_slverr : axi4_resp_okay);
	 if (req_op == AXI4_OP_RD) begin
	    AXI4_Rd_Data #(Wd_Id_Dma, Wd_Data_Dma, Wd_User_Dma)
	    rd_data = AXI4_Rd_Data {rid:   req.id,
				    rdata: rdata,
				    rresp: axi4_resp,
				    rlast: True,
				    ruser: req.user};
	    axi4_s_xactor.i_rd_data.enq (rd_data);

	    if(verbosity >= 1)
               $display("    ", fshow(rd_data));
	 end
	 else begin
	    AXI4_Wr_Resp #(Wd_Id_Dma, Wd_User_Dma)
	    wr_resp = AXI4_Wr_Resp {bid:   req.id,
				    bresp: axi4_resp,
				    buser: req.user};
	    axi4_s_xactor.i_wr_resp.enq (wr_resp);

	    if(verbosity >= 1)
               $display("    ", fshow(wr_resp));
	 end
      endaction
   endfunction

   function Action fa_upd_cache_and_respond (Meta_State new_tag_state, Data rdata);
      action
	 // Update cache tag
	 let new_tag = DMA_Cache_Tag {tag_state: ((req_op == AXI4_OP_WR)
						  ? META_MODIFIED
						  : new_tag_state),
				      tag_addr: line_addr};
	 let new_tag_set = fn_mk_Tag_Set (new_tag);
	 rf_tag_sets.upd (index, new_tag_set);
	 if(verbosity >= 1)
            $display("    new_tag_set = ", fshow (new_tag_set));

	 // For writes, modify cache using rdata, write-data, wstrb
	 Data new_data = rdata;
	 if (req_op == AXI4_OP_WR) begin
	    Vector #(Bytes_per_Data, Bit #(8)) v_bytes_old = unpack (rdata);
	    Vector #(Bytes_per_Data, Bit #(8)) v_bytes_wr  = unpack (req.wdata);
	    Vector #(Bytes_per_Data, Bit #(8)) v_bytes_new = ?;
	    for (Integer j = 0; j < valueOf (Bytes_per_Data); j = j + 1)
	       v_bytes_new [j] = ((req.wstrb [j] == 1'b0) ? v_bytes_old [j] : v_bytes_wr [j]);
	    new_data = pack (v_bytes_new);
	 end

	 // Update data in cache
	 let new_data_set = fn_mk_Data_Set (new_data);
	 rf_data_sets.upd (index, new_data_set);
	 if(verbosity >= 1)
            $display("    new_data_set = ", fshow (new_data_set));

	 // Send response to client
	 Bool err = False;
	 fa_respond_to_client (err, rdata);
      endaction
   endfunction

   // ================================================================
   // Hit; no eviction, no refill;
   //      just respond and update meta state to MODIFIED on writes

   Bool is_hit = ((   ((req_op == AXI4_OP_RD) && (tag.tag_state >= META_SHARED))
		   || ((req_op == AXI4_OP_WR) && (tag.tag_state >= META_EXCLUSIVE)))
		  && (tag.tag_addr  == line_addr));

   rule rl_hit ((rg_state == STATE_IDLE)
		&& (! f_L2_to_L1_Reqs.notEmpty)    // L2-to-L1 requests have priority
		&& is_hit);

      let new_tag_state = ((req_op == AXI4_OP_RD) ? tag.tag_state : META_MODIFIED);
      let data_set = rf_data_sets.sub (index);
      let old_data = data_set [0];    // Direct-mapped only, for now

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_hit", cur_cycle);
      end
      fa_upd_cache_and_respond (new_tag_state, old_data);

      // Done with the current request; discard it
      f_reqs.deq;
   endrule

   // ================================================================
   // Hit; but first needs upgrade from Invalid/Shared to Shared/Exclusive
   // Do upgrade if tag state is
   // Req is RD:  tag state is I
   // Reg is WR:  tag state is I or
   //             tag state is S, E and addr matches tag

   function Bool fn_do_upgrade ();
      Bool result;
      if (tag.tag_state == META_INVALID)
	 // Refill needed for read or write
	 result = True;
      else if (tag.tag_addr != line_addr)
	 // Tag does not match; need to evict before upgrade
	 result = False;
      else if (req_op == AXI4_OP_RD)
	 // RD and tag match; no need to upgrade
	 result = False;
      else    
	 // WR and tag match; upgrade if S (not E, M)
	 result = (tag.tag_state == META_SHARED);
      return result;
   endfunction

   rule rl_upgrade_req ((rg_state == STATE_IDLE)
			&& (! f_L2_to_L1_Reqs.notEmpty)  // L2-to-L1 reqs have priority
			&& fn_do_upgrade ());
      // Request refill/upgrade from L2
      let l1_to_l2_req = L1_to_L2_Req {addr:        line_addr,
				       from_state:  tag.tag_state,
				       to_state:    ((req_op == AXI4_OP_RD)
						     ? META_SHARED
						     : META_EXCLUSIVE),
				       can_up_to_E: (req_op == AXI4_OP_WR)};
      f_L1_to_L2_Reqs.enq (l1_to_l2_req);
      rg_state <= STATE_UPGRADE;

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_upgrade_req", cur_cycle);
	 $display ("    ", fshow (l1_to_l2_req));
      end
   endrule

   // When upgrade response arrives from L2, update cache, and respond to client

   rule rl_upgrade_rsp ((rg_state == STATE_UPGRADE)
			&& (! f_L2_to_L1_Reqs.notEmpty));  // L2-to-L1 reqs have priority
      let rsp <- pop (f_L2_to_L1_Rsps);

      // Assert response is for addr requested
      if (rsp.addr != line_addr) begin
	 $display ("%0d: %m.rl_upgrade_rsp: ERROR: rsp is not for addr %0h",
		   cur_cycle, line_addr);
	 $display ("    ", fshow (rsp));
	 $finish (1);
      end

      // Old data can be from L2 or from this cache
      Data old_data = ?;
      if (rsp.m_cline matches tagged Valid .x) begin
	 if (tag.tag_state != META_INVALID) begin
	    $display ("%0d: %m.rl_upgrade_rsp: ERROR: rsp has refill data for non-INVALID frame",
		      cur_cycle);
	    $display ("    ", fshow (rsp));
	    $finish (1);
	 end
	 old_data = x;
      end
      else begin
	 // Assert L2 response contains data if meta state was INVALID
	 if (tag.tag_state == META_INVALID) begin
	    $display ("%0d: %m.rl_upgrade_rsp: ERROR: rsp has no data for INVALID frame",
		      cur_cycle);
	    $display ("    ", fshow (rsp));
	    $finish (1);
	 end
	 old_data = rf_data_sets.sub (index) [0];    // Direct-mapped only, for now
      end

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_upgrade_rsp", cur_cycle);
      end
      fa_upd_cache_and_respond (rsp.to_state, old_data);

      // Done with the current request; discard it
      f_reqs.deq;
      rg_state <= STATE_IDLE;
   endrule

   // ****************************************************************
   // MMIO (request addr is not in cached mem)

   // LD/ST requests out of the AXI4_to_LD/ST converters
   FIFOF #(Single_Req)  f_single_reqs  <- mkFIFOF;
   FIFOF #(Single_Rsp)  f_single_rsps  <- mkFIFOF;

   // Remembers order of LD/ST requests to system
   FIFOF #(Bool) f_mmio_rsp_is_load <- mkSizedFIFOF (16);

   // ================================================================
   // MMIO reads

   // AXI4 to LD converter
   FIFOF #(AXI4_Rd_Addr #(Wd_Id_Dma,
			  Wd_Addr_Dma,
			  Wd_User_Dma)) f_rd_addr <- mkFIFOF;
   FIFOF #(AXI4_Rd_Data #(Wd_Id_Dma,
			  Wd_Data_Dma,
			  Wd_User_Dma)) f_rd_data <- mkFIFOF;

   AXI4_to_LD_IFC #(Wd_Addr_Dma, 64)
   axi4_to_ld <- mkAXI4_to_LD (to_FIFOF_O (f_rd_addr),
			       to_FIFOF_I (f_rd_data));

   // Pass MMIO read requests into axi4_to_ld module
   rule rl_mmio_AXI_rd_req (! fn_is_cached_addr (axi4_s_xactor.o_rd_addr.first.araddr));
      let rda <- pop_o (axi4_s_xactor.o_rd_addr);
      f_rd_addr.enq (rda);

      if (verbosity_MMIO_rd != 0) begin
	 $display ("%0d: %m.rl_mmio_AXI_rd_req", cur_cycle);
	 $display ("    awid %0h awaddr %0h awlen %0h awsize %0h awuser %0h",
		   rda.arid, rda.araddr, rda.arlen, rda.arsize, rda.aruser);
      end
   endrule

   // Pass MMIO LD requests from axi4_to_ld module out to system
   rule rl_mmio_LD_req;
      match { .size_code, .addr } <- pop_o (axi4_to_ld.reqs);
      let req = Single_Req {is_read:   True,
			    addr:      addr,
			    size_code: size_code,
			    data:      ?};
      f_single_reqs.enq (req);
      f_mmio_rsp_is_load.enq (True);

      if (verbosity_MMIO_rd != 0) begin
	 $display ("%0d: %m.rl_mmio_LD_req: ", cur_cycle);
	 $display ("    ", fshow (req));
      end
   endrule

   // Pass MMIO LD response from system back into axi4_to_ld_module
   rule rl_mmio_LD_rsp (f_mmio_rsp_is_load.first);
      f_mmio_rsp_is_load.deq;
      let rsp <- pop (f_single_rsps);
      Bool err = (! rsp.ok);
      axi4_to_ld.rsps.enq (tuple2 (err, rsp.data));

      if (verbosity_MMIO_rd != 0) begin
	 $display ("%0d: %m.rl_mmio_LD_rsp: ", cur_cycle);
	 $display ("    ", fshow (rsp));
      end
   endrule

   // Pass AXI4 response to client
   rule rl_mmio_AXI_rd_rsp;
      let rdd <- pop (f_rd_data);
      axi4_s_xactor.i_rd_data.enq (rdd);

      if (verbosity_MMIO_rd != 0) begin
	 $display ("%0d: %m.rl_mmio_AXI_rd_rsp: ", cur_cycle);
	 $display ("    ", fshow (rdd));
      end
   endrule

   // ================================================================
   // MMIO writes

   // AXI4 to ST converter
   FIFOF #(AXI4_Wr_Addr #(Wd_Id_Dma,
			  Wd_Addr_Dma,
			  Wd_User_Dma))  f_wr_addr <- mkFIFOF;
   FIFOF #(AXI4_Wr_Data #(Wd_Data_Dma,
			  Wd_User_Dma))  f_wr_data <- mkFIFOF;
   FIFOF #(AXI4_Wr_Resp #(Wd_Id_Dma,
			  Wd_User_Dma))  f_wr_resp <- mkFIFOF;

   AXI4_to_ST_IFC #(Wd_Addr_Dma, 64)
   axi4_to_st <- mkAXI4_to_ST (to_FIFOF_O (f_wr_addr),
			       to_FIFOF_O (f_wr_data),
			       to_FIFOF_I (f_wr_resp));

   // Pass MMIO write requests into axi4_to_st module
   rule rl_mmio_axi_wr_req (! fn_is_cached_addr (axi4_s_xactor.o_wr_addr.first.awaddr));
      let wra <- pop_o (axi4_s_xactor.o_wr_addr);
      let wrd <- pop_o (axi4_s_xactor.o_wr_data);
      f_wr_addr.enq (wra);
      f_wr_data.enq (wrd);

      if (verbosity_MMIO_wr != 0) begin
	 $display ("%0d: %m.rl_mmio_AXI_wr_req", cur_cycle);
	 $display ("    awid %0h awaddr %0h awlen %0h awsize %0h awuser %0h",
		   wra.awid, wra.awaddr, wra.awlen, wra.awsize, wra.awuser);
	 $display ("    (<wdata>, <wstrb> below) wlast %0d wuser %0h",
		   wrd.wlast, wrd.wuser);
	 $display ("    [127:0]    %032h %04h", wrd.wdata [127:0],   wrd.wstrb [15:0]);
	 $display ("    [255:128]  %032h %04h", wrd.wdata [255:128], wrd.wstrb [31:16]);
	 $display ("    [383:256]  %032h %04h", wrd.wdata [383:256], wrd.wstrb [47:32]);
	 $display ("    [511:384]  %032h %04h", wrd.wdata [511:384], wrd.wstrb [63:48]);
      end
   endrule

   // Pass MMIO ST requests out from axi4_to_st module out to system
   rule rl_mmio_ST_req;
      match { .size_code, .addr, .wdata } <- pop_o (axi4_to_st.reqs);
      let req = Single_Req {is_read:   False,
			    addr:      addr,
			    size_code: size_code,
			    data:      wdata};
      f_single_reqs.enq (req);
      f_mmio_rsp_is_load.enq (False);

      if (verbosity_MMIO_wr != 0) begin
	 $display ("%0d: %m.rl_mmio_ST_req: ", cur_cycle);
	 $display ("    ", fshow (req));
      end
   endrule

   // For MMIO ST requests, no response expected from system
   rule rl_mmio_st_rsp (! f_mmio_rsp_is_load.first);
      f_mmio_rsp_is_load.deq;
      Bool err = False;
      axi4_to_st.rsps.enq (err);

      if (verbosity_MMIO_wr != 0) begin
	 $display ("%0d: %m.rl_mmio_ST_rsp: ", cur_cycle);
	 $display ("    err = ", fshow (err));
      end
   endrule

   // Pass AXI4 response to client
   rule rl_mmio_axi_wr_rsp;
      let wrr <- pop (f_wr_resp);
      axi4_s_xactor.i_wr_resp.enq (wrr);

      if (verbosity_MMIO_wr != 0) begin
	 $display ("%0d: %m.rl_mmio_AXI_wr_rsp: ", cur_cycle);
	 $display ("    ", fshow (wrr));
      end
   endrule

   // ****************************************************************
   // ****************************************************************
   // INTERFACE
   // ****************************************************************
   // ****************************************************************

   // Facing external client
   interface AXI4_Slave_IFC axi4_s = axi4_s_xactor.axi_side;

   // ----------------
   // Facing L2 cache (for refills, writebacks, downgrades, ...)

   interface l1_to_l2_client = fifofs_to_Client_Semi_FIFOF (f_L1_to_L2_Reqs, f_L2_to_L1_Rsps);
   interface l2_to_l1_server = fifofs_to_Server_Semi_FIFOF (f_L2_to_L1_Reqs, f_L1_to_L2_Rsps);

   // ----------------
   // MMIO interface facing memory

   interface mmio_client = toGPClient (f_single_reqs, f_single_rsps);

endmodule: mkDMA_Cache

// ================================================================

endpackage: DMA_Cache
