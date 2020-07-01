package AXI4_Lite_MMU_Cache_Adapter;

// ================================================================
// An adapter for an AXI4-Lite master to drive an MMU_Cache_IFC.
// Supports arbitrarily-strobed writes, but anything that's not naturally
// sized and aligned will be turned into a series of individual byte
// reads (with no atomicity guarantees). Being AXI4-Lite, reads are
// always full width. Thus, this should not be used to access addresses
// with side-effects.

export AXI4_Lite_MMU_Cache_Adapter_IFC (..),
       mkAXI4_Lite_MMU_Cache_Adapter;

// ================================================================
// BSV lib imports

import Assert       :: *;
import FIFO         :: *;
import SpecialFIFOs :: *;

// ----------------
// BSV additional libs

import Semi_FIFOF :: *;

// ================================================================
// Project imports

import ISA_Decls       :: *;
import Near_Mem_IFC    :: *;
import MMU_Cache       :: *;
import Fabric_Defs     :: *;
import AXI4_Lite_Types :: *;

// ================================================================
// Local constants and types

typedef union tagged {
   void CacheRead;
   void CacheWrite;
   void SkippedWrite;
   Tuple3 #(Fabric_Addr, Fabric_Data, Fabric_Strb) UnnaturalWrite;
} PendingReq deriving (Bits, Eq, FShow);

// ================================================================
// Interface

interface AXI4_Lite_MMU_Cache_Adapter_IFC;
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) from_master;
endinterface

// ================================================================
// Implementation module

module mkAXI4_Lite_MMU_Cache_Adapter #(MMU_Cache_IFC cache)
				     (AXI4_Lite_MMU_Cache_Adapter_IFC);

   AXI4_Lite_Slave_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) xactor_from_master <- mkAXI4_Lite_Slave_Xactor;

   // Stores information associated with the outstanding request. We
   // want all the properties of a pipeline FIFO, namely that there is
   // only one outstanding request and that we can simultaneously
   // consume a response and issue a new request.
   FIFO #(PendingReq) f_req <- mkPipelineFIFO;

   // For unnatural writes, we track the last byte processed as we
   // iterate through byte-by-byte, as well as make errors sticky,
   Reg #(Bit #(ZLSBs_Aligned_Fabric_Addr)) rg_unnatural_last_offset <- mkReg (0);
   Reg #(AXI4_Lite_Resp) rg_unnatural_bresp <- mkReg (AXI4_LITE_OKAY);

   (* descending_urgency = "rl_rd_xaction, rl_wr_xaction" *)
   rule rl_wr_xaction;
      AXI4_Lite_Wr_Addr #(Wd_Addr, Wd_User) wra <- pop_o (xactor_from_master.o_wr_addr);
      AXI4_Lite_Wr_Data #(Wd_Data)          wrd <- pop_o (xactor_from_master.o_wr_data);

      Bit #(ZLSBs_Aligned_Fabric_Addr) offset = 0;
      Fabric_Data mask    = 'hFF;
      Bit #(3)    f3      = f3_SB;
      Bool        natural = False;
      case (wrd.wstrb)
`ifdef FABRIC64
	 'hFF: begin offset=0; mask = 'hFFFF_FFFF_FFFF_FFFF; f3=f3_SD; natural=True; end
	 'hF0: begin offset=4; mask =           'hFFFF_FFFF; f3=f3_SW; natural=True; end
	 'hC0: begin offset=6; mask =                'hFFFF; f3=f3_SH; natural=True; end
	 'h30: begin offset=4; mask =                'hFFFF; f3=f3_SH; natural=True; end
	 'h80: begin offset=7; mask =                  'hFF; f3=f3_SB; natural=True; end
	 'h40: begin offset=6; mask =                  'hFF; f3=f3_SB; natural=True; end
	 'h20: begin offset=5; mask =                  'hFF; f3=f3_SB; natural=True; end
	 'h10: begin offset=4; mask =                  'hFF; f3=f3_SB; natural=True; end
`endif
	 'hF:  begin offset=0; mask =           'hFFFF_FFFF; f3=f3_SW; natural=True; end
	 'hC:  begin offset=2; mask =                'hFFFF; f3=f3_SH; natural=True; end
	 'h3:  begin offset=0; mask =                'hFFFF; f3=f3_SH; natural=True; end
	 'h8:  begin offset=3; mask =                  'hFF; f3=f3_SB; natural=True; end
	 'h4:  begin offset=2; mask =                  'hFF; f3=f3_SB; natural=True; end
	 'h2:  begin offset=1; mask =                  'hFF; f3=f3_SB; natural=True; end
	 'h1:  begin offset=0; mask =                  'hFF; f3=f3_SB; natural=True; end
      endcase
      Bit #(64) addr64 = zeroExtend (wra.awaddr);
      addr64 [zlsbs_aligned_fabric_addr-1:0] = offset;
      Bit #(64) st_value = ((zeroExtend (wrd.wdata) >> {offset, 3'b0}) & mask);
      if (natural || (wrd.wstrb [0] == 1'b1)) begin
	 cache.req (CACHE_ST,
		    f3,
`ifdef ISA_A
		    ?,
`endif
		    truncate (addr64),
		    st_value,
		    m_Priv_Mode,
		    ?,
		    ?,
		    ?);
      end
      PendingReq req;
      if (natural)
	 req = tagged CacheWrite;
      else if (wrd.wstrb == 0)
	 req = tagged SkippedWrite;
      else
	 req = tagged UnnaturalWrite (tuple3 (wra.awaddr, wrd.wdata, wrd.wstrb));
      f_req.enq (req);
   endrule

   rule rl_wr_resp_cache (f_req.first matches tagged CacheWrite &&& cache.valid);
      let wrr = AXI4_Lite_Wr_Resp {
	    bresp: cache.exc ? AXI4_LITE_SLVERR : AXI4_LITE_OKAY,
	    buser: 0
      };
      xactor_from_master.i_wr_resp.enq (wrr);
      f_req.deq;
   endrule

   rule rl_wr_resp_skipped (f_req.first matches tagged SkippedWrite);
      let wrr = AXI4_Lite_Wr_Resp {
	    bresp: AXI4_LITE_OKAY,
	    buser: 0
      };
      xactor_from_master.i_wr_resp.enq (wrr);
      f_req.deq;
   endrule

   // rl_wr_xaction will have issued the first byte cache write request
   // if that lane was strobed. Keep issuing byte requests or skipping
   // to the next byte until we've processed the whole width.
   rule rl_wr_unnatural (    f_req.first matches tagged UnnaturalWrite ({ .awaddr, .wdata, .wstrb })
			 &&& (cache.valid || (wstrb [rg_unnatural_last_offset] == 1'b0)));
      let bresp = rg_unnatural_bresp;
      if ((wstrb [rg_unnatural_last_offset] == 1'b1) && cache.valid && cache.exc)
	 bresp = AXI4_LITE_SLVERR;

      if (rg_unnatural_last_offset == maxBound) begin
	 let wrr = AXI4_Lite_Wr_Resp {
	       bresp: bresp,
	       buser: 0
	 };
	 xactor_from_master.i_wr_resp.enq (wrr);
	 rg_unnatural_last_offset <= 0;
	 rg_unnatural_bresp <= AXI4_LITE_OKAY;
	 f_req.deq;
      end
      else begin
	 Bit #(64) addr64 = zeroExtend (awaddr);
	 Bit #(ZLSBs_Aligned_Fabric_Addr) offset = rg_unnatural_last_offset + 1;
	 addr64 [zlsbs_aligned_fabric_addr-1:0] = offset;
	 Bit #(64) st_value = ((zeroExtend (wdata) >> {offset, 3'b0}) & 'hFF);
	 if (wstrb [offset] == 1'b1) begin
	    cache.req (CACHE_ST,
		       f3_SB,
`ifdef ISA_A
		       ?,
`endif
		       truncate (addr64),
		       st_value,
		       m_Priv_Mode,
		       ?,
		       ?,
		       ?);
	 end
	 rg_unnatural_last_offset <= offset;
	 rg_unnatural_bresp <= bresp;
      end
   endrule

   rule rl_rd_xaction;
      AXI4_Lite_Rd_Addr #(Wd_Addr, Wd_User) rda <- pop_o (xactor_from_master.o_rd_addr);

`ifdef FABRIC32
      Bit #(3) f3 = f3_LW;
`endif
`ifdef FABRIC64
      Bit #(3) f3 = f3_LD;
`endif
      Bit #(64) addr64 = zeroExtend (rda.araddr);
      Bit #(ZLSBs_Aligned_Fabric_Addr) offset = 0;
      addr64 [zlsbs_aligned_fabric_addr-1:0] = offset;
      cache.req (CACHE_LD,
		 f3,
`ifdef ISA_A
		 ?,
`endif
		 truncate (addr64),
		 ?,
		 m_Priv_Mode,
		 ?,
		 ?,
		 ?);
      f_req.enq (tagged CacheRead);
   endrule

   rule rl_rd_resp (f_req.first matches tagged CacheRead &&& cache.valid);
      let rdr = AXI4_Lite_Rd_Data {
	    rresp: cache.exc ? AXI4_LITE_SLVERR : AXI4_LITE_OKAY,
	    rdata: truncate (cache.word64),
	    ruser: 0
      };
      xactor_from_master.i_rd_data.enq (rdr);
      f_req.deq;
   endrule

   interface from_master = xactor_from_master.axi_side;
endmodule

endpackage : AXI4_Lite_MMU_Cache_Adapter
