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

// ================================================================
// Project imports

import ISA_Decls    :: *;
import Near_Mem_IFC :: *;

import SoC_Map :: *;    // For predicate on addresses (cacheble or not)

import MMU_Cache_Common :: *;    // For L1<->L2 reqs and rsps

import MMIO  :: *;

import AXI4_Types   :: *;
import AXI_Widths   :: *;
import Near_Mem_IFC :: *;    // For Wd_{Id/Addr/User/Data}_Dma

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

typedef Bit #(64)               Slice;                  // MMIO width
typedef TDiv #(Wd_Data_Dma, 64) Slices_per_Data;        // 512/64 = 8

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
	      STATE_UPGRADE,         // refill/upgrade request sent, awaiting response
	      STATE_MMIO_RD_WAIT,    // Waiting for MMIO slice-read response
	      STATE_MMIO_WR_WAIT     // Waiting for MMIO slice-write response
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

   // For debugging
   Integer verbosity       = 0;    // 1: rules
   Integer verbosity_mmio  = 0;    // 1: rules

   // ----------------
   // SoC_Map needed for method 'm_is_mem_addr' distinguishing cached vs. other addrs
   SoC_Map_IFC soc_map <- mkSoC_Map;

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

   // ----------------
   // MMIO sub-module

   MMIO_IFC   mmio  <- mkMMIO (fromInteger (verbosity_mmio));

   // ****************************************************************
   // ****************************************************************
   // BEHAVIOR

   // ================================================================
   // Initialization: loop through all cache sets, setting all tags to INVALID

   Reg #(Bit #(DMA_Cache_Index_Width)) rg_init_index <- mkReg (0);

   rule rl_init (rg_state == STATE_INITIALIZING);
      if ((verbosity >= 1) && (rg_init_index == 0)) begin
	 $display ("%0d: DMA_Cache.rl_init: Initializing cache ...", cur_cycle);
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
   // Merge requests into a single queue, prioritizing reads over writes

   FIFOF #(AXI4_Req) f_reqs <- mkFIFOF;

   rule rl_merge_rd_req;
      let rda <- pop_o (axi4_s_xactor.o_rd_addr);

      // Assert arlen = 0 (single beat)
      if (rda.arlen != 0) begin
	 $display ("%0d: DMA_Merge.rl_merge_rd_req: ERROR: burst request; not supported", cur_cycle);
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

      if (verbosity >= 2) begin
	 $display ("%0d: DMA_Cache.rl_merge_rd_req", cur_cycle);
	 $display ("        ", fshow (rda));
      end
   endrule

   (* descending_urgency = "rl_merge_rd_req, rl_merge_wr_req" *)
   rule rl_merge_wr_req;
      let wra <- pop_o (axi4_s_xactor.o_wr_addr);
      let wrd <- pop_o (axi4_s_xactor.o_wr_data);

      // Assert awlen = 0 (single beat)
      if (wra.awlen != 0) begin
	 $display ("%0d: DMA_Merge.rl_merge_wr_req: ERROR: burst request; not supported", cur_cycle);
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

      if (verbosity >= 2) begin
	 $display ("%0d: DMA_Cache.rl_merge_wr_req", cur_cycle);
	 $display ("    ", fshow (wra));
	 $display ("    ", fshow (wrd));
      end
   endrule

   // ================================================================
   // Handle downgrade-request from L2 (highest priority)

   rule rl_downgrade (rg_state != STATE_INITIALIZING);
      let l2_to_l1_req <- pop (f_L2_to_L1_Reqs);
      if (verbosity >= 1) begin
	 $display ("%0d: DMA_Cache.rl_downgrade", cur_cycle);
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
		  && (! f_L2_to_L1_Reqs.notEmpty)        // L2-to-L1 requests have priority
		  && soc_map.m_is_mem_addr (addr)        // cacheable addrs only
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
	 $display ("%0d: DMA_Cache.rl_evict", cur_cycle);
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
		&& (! f_L2_to_L1_Reqs.notEmpty)        // L2-to-L1 requests have priority
		&& soc_map.m_is_mem_addr (addr)        // cacheable addrs only
		&& is_hit);

      let new_tag_state = ((req_op == AXI4_OP_RD) ? tag.tag_state : META_MODIFIED);
      let data_set = rf_data_sets.sub (index);
      let old_data = data_set [0];    // Direct-mapped only, for now

      if (verbosity >= 1) begin
	 $display ("%0d: DMA_Cache.rl_hit", cur_cycle);
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
			&& (! f_L2_to_L1_Reqs.notEmpty)        // L2-to-L1 requests have priority
			&& soc_map.m_is_mem_addr (addr)        // cacheable addrs only
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
	 $display ("%0d: DMA_Cache.rl_upgrade_req", cur_cycle);
	 $display ("    ", fshow (l1_to_l2_req));
      end
   endrule

   // When upgrade response arrives from L2, update cache, and respond to client

   rule rl_upgrade_rsp ((rg_state == STATE_UPGRADE)
			&& (! f_L2_to_L1_Reqs.notEmpty));        // L2-to-L1 requests have priority
      let rsp <- pop (f_L2_to_L1_Rsps);

      // Assert response is for addr requested
      if (rsp.addr != line_addr) begin
	 $display ("%0d: DMA_Cache.rl_upgrade_rsp: ERROR: rsp is not for addr %0h",
		   cur_cycle, line_addr);
	 $display ("    ", fshow (rsp));
	 $finish (1);
      end

      // Old data can be from L2 or from this cache
      Data old_data = ?;
      if (rsp.m_cline matches tagged Valid .x) begin
	 if (tag.tag_state != META_INVALID) begin
	    $display ("%0d: DMA_Cache.rl_upgrade_rsp: ERROR: rsp has refill data for non-INVALID frame",
		      cur_cycle);
	    $display ("    ", fshow (rsp));
	    $finish (1);
	 end
	 old_data = x;
      end
      else begin
	 // Assert L2 response contains data if meta state was INVALID
	 if (tag.tag_state == META_INVALID) begin
	    $display ("%0d: DMA_Cache.rl_upgrade_rsp: ERROR: rsp has no data for INVALID frame",
		      cur_cycle);
	    $display ("    ", fshow (rsp));
	    $finish (1);
	 end
	 old_data = rf_data_sets.sub (index) [0];    // Direct-mapped only, for now
      end

      if (verbosity >= 1) begin
	 $display ("%0d: DMA_Cache.rl_upgrade_rsp", cur_cycle);
      end
      fa_upd_cache_and_respond (rsp.to_state, old_data);

      // Done with the current request; discard it
      f_reqs.deq;
      rg_state <= STATE_IDLE;
   endrule

   // ================================================================
   // MMIO (request addr is not in cached mem: pass thru to MMIO box)

   // These regs are used to slice 512-bit data into 64-bit slices, one per MMIO transaction
   Reg #(Bit #(Wd_Addr_Dma))                        rg_mmio_addr       <- mkRegU;
   Reg #(Bit #(8))                                  rg_mmio_num_bytes  <- mkRegU;
   Reg #(Vector #(Slices_per_Data, Slice))          rg_mmio_v_slices   <- mkRegU;
   Reg #(Bool)                                      rg_mmio_err        <- mkRegU;
   Reg #(Bit #(TAdd #(1, TLog #(Slices_per_Data)))) rg_mmio_num_slices <- mkRegU;

   // ----------------
   // Send an mmio request for one slice (up to 64bits)
   function Action fa_mmio_slice_req (CacheOp    cache_op,
				      Bit #(64)  addr1,
				      Slice      slice,
				      Bit #(8)   num_bytes);
      action
	 Bit #(3)  size_code = ((num_bytes >= 8) ? axsize_8 : fv_num_bytes_to_AXI4_Size (num_bytes));
	 let mmu_cache_req   = MMU_Cache_Req {op:          cache_op,
					      f3:          size_code,
					      va:          addr1,
					      st_value:    slice
`ifdef ISA_A
					    , amo_funct7:  0     // Bogus (AMO not supported here)
`endif
`ifdef ISA_PRIV_S
					    , priv:        m_Priv_Mode,    // Machine mode (TODO: ok?)
					      sstatus_SUM: 0,    // Bogus (we're in machine mode)
					      mstatus_MXR: 0,    // Bogus (we're in machine mode)
					      satp:        0     // Bogus (we're not doing virt mem)
`endif
					      };
	 mmio.req   (mmu_cache_req);
	 mmio.start (addr1);
	 if (verbosity >= 2) begin
	    $display ("    MMIO slice req: op %0d  size_code %0d  addr %0h  slice data %0h",
		      cache_op, size_code, addr1, slice);
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // MMIO Writes
  
   // MMIO Write-requests (first slice)
   rule rl_mmio_wr_req (   (rg_state == STATE_IDLE)
			&& (! f_L2_to_L1_Reqs.notEmpty)        // L2-to-L1 requests have priority
			&& (! soc_map.m_is_mem_addr (addr))    // all non-cacheable addrs
			&& (req_op == AXI4_OP_WR));
      if (verbosity >= 1) begin
	 $display ("%0d: DMA_Cache.rl_mmio_wr_req", cur_cycle);
	 $display ("    ", fshow (req));
      end

      // Shift wdata to LSBs.
      // (We do this on a byte-vector to clarify the shift is in byte-units, not bit-units,
      //  for a more hardware-efficient shifter)
      Vector #(Bytes_per_Data, Bit #(8)) v_bytes = unpack (req.wdata);
      let v_shifted_bytes = rotateBy (v_bytes, unpack (req.addr [5:0]));
      let shifted_wdata   = pack (v_shifted_bytes);

      Vector #(Slices_per_Data, Slice) v_slices = unpack (shifted_wdata);
      Bit #(8) num_bytes  = fv_AXI4_Size_to_num_bytes (req.size);
      fa_mmio_slice_req (CACHE_ST, req.addr, v_slices [0], num_bytes);

      rg_mmio_addr       <= req.addr;
      rg_mmio_num_bytes  <= num_bytes;
      rg_mmio_v_slices   <= v_slices;
      rg_mmio_err        <= False;

      rg_state          <= STATE_MMIO_WR_WAIT;
   endrule

   // Collect MMIO slice-write response; request next slice or finish by responding to client
   rule rl_mmio_wr_slice_rsp (rg_state == STATE_MMIO_WR_WAIT);
      match { .slice_err, .*, .* } = mmio.result;
      // Accumulate errors
      let err = (slice_err || rg_mmio_err);

      if (verbosity >= 1) begin
	 $display ("%0d: DMA_Cache.rl_mmio_wr_slice_rsp: slice_err %0d  err %0d, num_bytes remaining %0h",
		   cur_cycle, slice_err, err, rg_mmio_num_bytes);
      end

      if (rg_mmio_num_bytes <= 8) begin
	 // Last slice done; discard client's request, respond to client
	 fa_respond_to_client (err, ?);
	 f_reqs.deq;
	 rg_state <= STATE_IDLE;
      end
      else begin
	 // Write next slice
	 let addr      = rg_mmio_addr + 8;
	 let num_bytes = rg_mmio_num_bytes - 8;
	 let v_slices  = shiftInAtN (rg_mmio_v_slices, 0);
	 fa_mmio_slice_req (CACHE_ST, addr, v_slices [0], num_bytes);
	 rg_mmio_addr       <= addr;
	 rg_mmio_num_bytes  <= num_bytes;
	 rg_mmio_v_slices   <= v_slices;
	 rg_mmio_err        <= err;
      end
   endrule

   // ----------------------------------------------------------------
   // MMIO Reads
  
   // MMIO Read-requests (first slice)
   rule rl_mmio_rd_req (   (rg_state == STATE_IDLE)
			&& (! f_L2_to_L1_Reqs.notEmpty)        // L2-to-L1 requests have priority
			&& (! soc_map.m_is_mem_addr (addr))    // all non-cacheable addrs
			&& (req_op == AXI4_OP_RD));
      if (verbosity >= 1) begin
	 $display ("%0d: DMA_Cache.rl_mmio_rd_req", cur_cycle);
	 $display ("    ", fshow (req));
      end

      Bit #(8) num_bytes  = fv_AXI4_Size_to_num_bytes (req.size);
      fa_mmio_slice_req (CACHE_LD, req.addr, ?, num_bytes);

      // Prepare the slice-accumulation registers
      rg_mmio_addr       <= req.addr;
      rg_mmio_num_bytes  <= num_bytes;
      rg_mmio_v_slices   <= unpack (0);
      rg_mmio_err        <= False;
      rg_mmio_num_slices <= 1;    // Current empty-count in v_slices

      rg_state          <= STATE_MMIO_RD_WAIT;
   endrule

   // Collect MMIO slice-read response; request next slice or finish by responding to client
   rule rl_mmio_rd_slice_rsp (rg_state == STATE_MMIO_RD_WAIT);
      match { .slice_err, .slice_data, .* } = mmio.result;
      if (verbosity >= 1) begin
	 $display ("%0d: DMA_Cache.rl_mmio_rd_slice_rsp: addr %0h  slice_err %0d  slice_data %0h",
		   cur_cycle, rg_mmio_addr, slice_err, slice_data);
      end

      // Accumulate this slice's err and data
      let err      = (slice_err || rg_mmio_err);
      let v_slices = shiftInAtN (rg_mmio_v_slices, slice_data);

      if (rg_mmio_num_bytes <= 8) begin
	 // Last slice done

	 // v_slices has data aligned to most-significant slice
	 // Shift it into LSBs

	 Bit #(TAdd #(1, TLog #(Slices_per_Data))) rot_amt_slices = { 1'b1, 0 } - rg_mmio_num_slices;

	 let v_slices_in_lsbs = rotateBy (v_slices, unpack (truncate (rot_amt_slices)));
	 let rdata_in_lsbs    = pack (v_slices_in_lsbs);

	 // Shift rdata into proper lanes for AXI rdata.
	 // (We do it on a byte-vector to clarify the shift is in byte-units, not bit-units,
	 //  for a more hardware-efficient shifter)
	 Vector #(Bytes_per_Data, Bit #(8)) v_bytes = unpack (rdata_in_lsbs);
	 UInt #(7) rot_amt_bytes = unpack ('h40 - { 0, req.addr [5:0] });
	 Bit #(Wd_Data_Dma) rdata = pack (rotateBy (v_bytes, truncate (rot_amt_bytes)));
	 if (verbosity >= 1) begin
	    $display ("    v_slices         ", fshow (v_slices));
	    $display ("    num_slices %0d  rot_amt_slices %0d", rg_mmio_num_slices, rot_amt_slices);
	    $display ("    v_slices_in_lsbs ", fshow (v_slices_in_lsbs));
	    $display ("    rdata_in_lsbs %0h", rdata_in_lsbs);
	    $display ("    v_bytes ", fshow (v_bytes));
	    $display ("    rot_amt_bytes 0x%0h (ignore MSB)", rot_amt_bytes);
	    $display ("    rdata %0h", rdata);
	 end

	 // Respond to client
	 fa_respond_to_client (err, rdata);
	 // Discard client's request
	 f_reqs.deq;
	 rg_state <= STATE_IDLE;
      end
      else begin
	 // Issue next request and accumulate current slice
	 let addr            = rg_mmio_addr + 8;
	 let num_bytes       = rg_mmio_num_bytes - 8;
	 fa_mmio_slice_req (CACHE_LD, addr, ?, num_bytes);

	 rg_mmio_addr       <= addr;
	 rg_mmio_num_bytes  <= num_bytes;
	 rg_mmio_v_slices   <= v_slices;
	 rg_mmio_err        <= err;
	 rg_mmio_num_slices <= rg_mmio_num_slices + 1;
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

   interface mmio_client = mmio.mmio_client;

endmodule: mkDMA_Cache

// ================================================================

endpackage: DMA_Cache
