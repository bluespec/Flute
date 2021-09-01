// Copyright (c) 2016-2021 Bluespec, Inc. All Rights Reserved.

// This module transforms an L1's L2-facing interface
//    (i.e., l1_to_l2_client and l2_to_l1_server)
// to the ChildCacheToParent interface required by the L2.

package L1_IFC_Adapter;

// ================================================================

export mkL1_IFC_Adapter;

// ================================================================
// BSV lib imports

import ConfigReg    :: *;
import Vector       :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ----------------
// Project imports

import MMU_Cache_Common :: *;
import Cache_Decls      :: *;

import LLCache_Aux   :: *;
import CacheUtils    :: *;
import CCTypes       :: *;

// ================================================================
// Interface type-conversion:
// from  L1 Cache's L2_to_L2_Client and L2_to_L1_Server interfaces
// to    L1LLConnect's ChildCacheToParent interface

function CCTypes::Msi fv_Meta_State_to_Msi (Meta_State s);
   return (case (s)
	      META_INVALID:   I;
	      META_SHARED:    S;
	      META_EXCLUSIVE: E;
	      META_MODIFIED:  M;
	   endcase);
endfunction

function Meta_State fv_Msi_to_Meta_State (CCTypes::Msi s);
   return (case (s)
	      I: META_INVALID;
	      S: META_SHARED;
	      E: META_EXCLUSIVE;
	      M: META_MODIFIED;
	   endcase);
endfunction

function Maybe #(CCTypes::Line) fv_M_CLine_to_M_Line (Maybe #(CLine) m_cline);
   return (case (m_cline) matches
	      tagged Invalid: tagged Invalid;
	      tagged Valid .x: tagged Valid (unpack (x));
	   endcase);
endfunction

function Maybe #(CLine) fv_M_Line_to_M_CLine (Maybe #(CCTypes::Line) m_line);
   return (case (m_line) matches
	      tagged Invalid:  tagged Invalid;
	      tagged Valid .x: tagged Valid (pack (x));
	   endcase);
endfunction

// ----------------

module mkL1_IFC_Adapter #(Integer                                          verbosity_L1_L2,
			  Integer                                          id,
			  Client_Semi_FIFOF #(L1_to_L2_Req, L2_to_L1_Rsp)  l1_to_l2_client,
			  Server_Semi_FIFOF #(L2_to_L1_Req, L1_to_L2_Rsp)  l2_to_l1_server)
       (ChildCacheToParent #(L1Way, void));

   interface FifoDeq rsToP;    // #(CRsMsg #(void))
      method Bool notEmpty;
	 return l2_to_l1_server.response.notEmpty;
      endmethod
      method Action deq;
	 l2_to_l1_server.response.deq;

	 if (verbosity_L1_L2 >= 2) begin
	    L1_to_L2_Rsp rsp = l2_to_l1_server.response.first;
	    $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id, fshow_L1_to_L2_Rsp (rsp));
	 end
      endmethod
      method CRsMsg #(void) first;
	 L1_to_L2_Rsp rsp = l2_to_l1_server.response.first;
	 let cRsMsg = CRsMsg {addr:    rsp.addr,
			      toState: fv_Meta_State_to_Msi (rsp.to_state),
			      data:    fv_M_CLine_to_M_Line (rsp.m_cline),
			      child:   ? };
	 return cRsMsg;
      endmethod
   endinterface

   interface FifoDeq rqToP;    // #(CRqMsg #(L1Way, void))
      method Bool notEmpty;
	 return l1_to_l2_client.request.notEmpty;
      endmethod
      method Action deq;
	 l1_to_l2_client.request.deq;

	 if (verbosity_L1_L2 >= 2) begin
	    L1_to_L2_Req req = l1_to_l2_client.request.first;
	    $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id, fshow_L1_to_L2_Req (req));
	 end
      endmethod
      method CRqMsg #(L1Way, void) first;
	 L1_to_L2_Req req = l1_to_l2_client.request.first;
	 let cRqMsg = CRqMsg {addr:      req.addr,
			      fromState: fv_Meta_State_to_Msi (req.from_state),
			      toState:   fv_Meta_State_to_Msi (req.to_state),
			      canUpToE:  req.can_up_to_E,
			      id:        ?,
			      child:     ? };
	 return cRqMsg;
      endmethod
   endinterface

   interface FifoEnq fromP;    // #(PRqRsMsg #(L1Way, void)
      method Bool notFull;
	 return (l1_to_l2_client.response.notFull
		 && l2_to_l1_server.request.notFull);
      endmethod
      method Action enq (PRqRsMsg #(L1Way, void) x);
	 case (x) matches
	    tagged PRq .prq:
	       begin
		  let req = L2_to_L1_Req {addr:     prq.addr,
					  to_state: fv_Msi_to_Meta_State (prq.toState) };
		  // Note: prq.child is discarded
		  l2_to_l1_server.request.enq (req);

		  if (verbosity_L1_L2 >= 2)
		     $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id,
			       fshow_L2_to_L1_Req (req));
	       end

	    tagged PRs .prs:
	       begin
		  let rsp = L2_to_L1_Rsp {addr:     prs.addr,
					  to_state: fv_Msi_to_Meta_State (prs.toState),
					  m_cline:  fv_M_Line_to_M_CLine (prs.data) };
		  l1_to_l2_client.response.enq (rsp);

		  if (verbosity_L1_L2 >= 2)
		     $display ("%0d: L1_IFC_Adapter [%0d]: ", cur_cycle, id,
			       fshow_L2_to_L1_Rsp (rsp));
	       end
	 endcase
      endmethod
   endinterface
endmodule

// ================================================================

endpackage
