// Copyright (c) 2021 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4L_S_to_AXI4_M_Adapter;

// ================================================================
// This package defines an adapter module that does two things:
// - AXI4-Lite S to AXI4 M adaptation
// - widening (AXI4L_S fields are narrower than AXI4_M)

// ================================================================
// Bluespec library imports

import Vector       :: *;
import FIFOF        :: *;
import SpecialFIFOs :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import Semi_FIFOF      :: *;
import AXI4_Lite_Types :: *;
import AXI4_Types      :: *;

// ================================================================
// The interface for the adapter module

interface AXI4L_S_to_AXI4_M_Adapter_IFC #(numeric type wd_addr_AXI4L_S,
					  numeric type wd_data_AXI4L_S,
					  numeric type wd_user_AXI4L_S,

					  numeric type wd_id_AXI4_M,
					  numeric type wd_addr_AXI4_M,
					  numeric type wd_data_AXI4_M,
					  numeric type wd_user_AXI4_M);
   // AXI4L S side
   interface AXI4_Lite_Slave_IFC #(wd_addr_AXI4L_S,
				   wd_data_AXI4L_S,
				   wd_user_AXI4L_S) ifc_AXI4L_S;

   // AXI4 M side
   interface AXI4_Master_IFC #(wd_id_AXI4_M,
			       wd_addr_AXI4_M,
			       wd_data_AXI4_M,
			       wd_user_AXI4_M) ifc_AXI4_M;
endinterface

// ================================================================
// The adapter module

module mkAXI4L_S_to_AXI4_M_Adapter (AXI4L_S_to_AXI4_M_Adapter_IFC #(wd_addr_AXI4L_S,
								    wd_data_AXI4L_S,
								    wd_user_AXI4L_S,
								    wd_id_AXI4_M,
								    wd_addr_AXI4_M,
								    wd_data_AXI4_M,
								    wd_user_AXI4_M))
   // these provisos specify widening
   provisos (Add #(wd_addr_AXI4L_S,         __a, wd_addr_AXI4_M),      // addr widening
	     Mul #(wd_data_AXI4L_S, expansion_t, wd_data_AXI4_M),      // data widening
	     Add #(wd_user_AXI4L_S,         __u, wd_user_AXI4_M),      // user widening
	     Div #(wd_data_AXI4L_S,           8, wd_bytes_AXI4L_S),
	     Div #(wd_data_AXI4_M,            8, wd_bytes_AXI4_M),

	     Bits #(Vector #(expansion_t, Bit #(wd_data_AXI4L_S)),  wd_data_AXI4_M),
	     Bits #(Vector #(expansion_t, Bit #(wd_bytes_AXI4L_S)), wd_bytes_AXI4_M),

	     NumAlias #(TLog #(wd_bytes_AXI4L_S), addr_index_S_t),
	     NumAlias #(TLog #(wd_bytes_AXI4_M),  addr_index_M_t),
	     Add #(addr_index_S_t, index_width_t, addr_index_M_t)
	     );

   // 0 quiet; 1: rules
   Integer verbosity = 0;

   // AXI4L S transactor
   AXI4_Lite_Slave_Xactor_IFC #(wd_addr_AXI4L_S,
				wd_data_AXI4L_S,
				wd_user_AXI4L_S) xactor_AXI4L_S <- mkAXI4_Lite_Slave_Xactor;

   // AXI4 M transactor
   AXI4_Master_Xactor_IFC #(wd_id_AXI4_M,
			    wd_addr_AXI4_M,
			    wd_data_AXI4_M,
			    wd_user_AXI4_M)      xactor_AXI4_M <- mkAXI4_Master_Xactor;

   // ================================================================
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Write requests (Write Address and Write Data channels)

   rule rl_wr_addr_data;
      let awaddr_AXI4L <- pop_o (xactor_AXI4L_S.o_wr_addr);
      let wdata_AXI4L  <- pop_o (xactor_AXI4L_S.o_wr_data);

      Bit #(wd_id_AXI4_M)   id_AXI4   = 1;
      Bit #(wd_addr_AXI4_M) addr_AXI4 = zeroExtend (awaddr_AXI4L.awaddr);
      Bit #(wd_user_AXI4_M) user_AXI4 = zeroExtend (awaddr_AXI4L.awuser);

      AXI4_Wr_Addr #(wd_id_AXI4_M, wd_addr_AXI4_M, wd_user_AXI4_M)
          awaddr_AXI4 = AXI4_Wr_Addr {awid:     id_AXI4,
				      awaddr:   addr_AXI4,
				      awlen:    0,    // AXI4 encoding for "1 beat"
				      awsize:   axsize_4,
				      awburst:  axburst_fixed,
				      awlock:   axlock_normal,
				      awcache:  awcache_dev_nonbuf,
				      awprot:   awaddr_AXI4L.awprot,
				      awqos:    0,
				      awregion: 0,
				      awuser:   user_AXI4};

      // Lane-align wdata for the wider AXI4

      Vector #(expansion_t, Bit #(wd_data_AXI4L_S))  v_data = unpack (0);
      Vector #(expansion_t, Bit #(wd_bytes_AXI4L_S)) v_strb = unpack (0);

      Integer hi = valueOf (addr_index_M_t) - 1;
      Integer lo = valueOf (addr_index_S_t);
      Bit #(index_width_t) index = awaddr_AXI4L.awaddr [hi:lo];
      v_data [index] = wdata_AXI4L.wdata;
      v_strb [index] = wdata_AXI4L.wstrb;

      AXI4_Wr_Data #(wd_data_AXI4_M, wd_user_AXI4_M)
          wdata_AXI4 = AXI4_Wr_Data {wdata: pack (v_data),
				     wstrb: pack (v_strb),
				     wlast: True,
				     wuser: user_AXI4};

      xactor_AXI4_M.i_wr_addr.enq (awaddr_AXI4);
      xactor_AXI4_M.i_wr_data.enq (wdata_AXI4);

      // Debugging
      if (verbosity > 0) begin
	 $display ("%0d: %m.rl_wr_addr_data:", cur_cycle);
	 $display ("        ", fshow (awaddr_AXI4L));
	 $display ("    ==>   ", fshow (awaddr_AXI4));
	 $display ("        ", fshow (wdata_AXI4));
	 $display ("    ==>   ", fshow (wdata_AXI4));
      end
   endrule

   // ----------------------------------------------------------------
   // Write responses (Write Data channel)

   rule rl_wr_resp;
      let wr_resp_AXI4 <- pop_o (xactor_AXI4_M.o_wr_resp);

      Bit #(wd_user_AXI4L_S) user = truncate (wr_resp_AXI4.buser);

      AXI4_Lite_Wr_Resp #(wd_user_AXI4L_S)
          wr_resp_AXI4L = AXI4_Lite_Wr_Resp {bresp: unpack (wr_resp_AXI4.bresp),
					     buser: user};
      xactor_AXI4L_S.i_wr_resp.enq (wr_resp_AXI4L);

      // Debugging
      if (verbosity > 0) begin
	 $display ("%0d: %m.rl_wr_resp:", cur_cycle);
	 $display ("        ", fshow (wr_resp_AXI4));
	 $display ("    ==>   ", fshow (wr_resp_AXI4L));
      end
   endrule

   // ----------------------------------------------------------------
   // Read requests (Read Addr channel)

   // This FIFOF remembers addrs so returned data can properly lane-aligned.
   // For full pipelining, needs to be deep enough to cover latency to target and back.
   FIFOF #(Bit #(wd_addr_AXI4L_S)) f_rd_addrs <- mkSizedFIFOF (32);

   // ----------------

   rule rl_rd_addr;
      let araddr_AXI4L <- pop_o (xactor_AXI4L_S.o_rd_addr);

      Bit #(wd_id_AXI4_M)   id_AXI4   = 1;
      Bit #(wd_addr_AXI4_M) addr_AXI4 = zeroExtend (araddr_AXI4L.araddr);
      Bit #(wd_user_AXI4_M) user_AXI4 = zeroExtend (araddr_AXI4L.aruser);

      AXI4_Rd_Addr #(wd_id_AXI4_M, wd_addr_AXI4_M, wd_user_AXI4_M)
          araddr_AXI4 = AXI4_Rd_Addr {arid:     id_AXI4,
				      araddr:   addr_AXI4,
				      arlen:    0,    // AXI4 encoding for "1 beat"
				      arsize:   axsize_4,
				      arburst:  axburst_fixed,
				      arlock:   axlock_normal,
				      arcache:  arcache_dev_nonbuf,
				      arprot:   araddr_AXI4L.arprot,
				      arqos:    0,
				      arregion: 0,
				      aruser:   user_AXI4};

      xactor_AXI4_M.i_rd_addr.enq (araddr_AXI4);

      // Remember addrs so returned data can properly lane-aligned
      f_rd_addrs.enq (araddr_AXI4L.araddr);

      // Debugging
      if (verbosity > 0) begin
	 $display ("%0d: %m.rl_rd_addr:", cur_cycle);
	 $display ("        ", fshow (araddr_AXI4L));
	 $display ("    ==>   ", fshow (araddr_AXI4));
      end
   endrule

   // ----------------------------------------------------------------
   // Read responses (Read Data channel)

   rule rl_rd_data;
      let rd_data_AXI4 <- pop_o (xactor_AXI4_M.o_rd_data);
      let addr_AXI4L   <- pop (f_rd_addrs);

      // Lane-align rdata for the narrower AXI4L

      Vector #(expansion_t, Bit #(wd_data_AXI4L_S))  v_data = unpack (rd_data_AXI4.rdata);

      Integer hi = valueOf (addr_index_M_t) - 1;
      Integer lo = valueOf (addr_index_S_t);
      Bit #(index_width_t) index = addr_AXI4L [hi:lo];
      Bit #(wd_data_AXI4L_S) rdata_AXI4L = v_data [index];

      Bit #(wd_user_AXI4L_S) ruser_AXI4L = truncate (rd_data_AXI4.ruser);

      AXI4_Lite_Rd_Data #(wd_data_AXI4L_S, wd_user_AXI4L_S)
          rd_data_AXI4L = AXI4_Lite_Rd_Data {rresp: unpack (rd_data_AXI4.rresp),
					     rdata: rdata_AXI4L,
					     ruser: ruser_AXI4L };

      xactor_AXI4L_S.i_rd_data.enq (rd_data_AXI4L);

      // Debugging
      if (verbosity > 0) begin
	 $display ("%0d: %m.rl_rd_data:", cur_cycle);
	 $display ("        ", fshow (rd_data_AXI4));
	 $display ("    ==>   ", fshow (rd_data_AXI4L));
      end
   endrule

   // ----------------------------------------------------------------
   // INTERFACE

   interface ifc_AXI4L_S = xactor_AXI4L_S.axi_side;
   interface ifc_AXI4_M  = xactor_AXI4_M .axi_side;
endmodule

// ================================================================

endpackage
