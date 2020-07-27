// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package PTW;

// ================================================================
// The PTWalker performs a page-table walk, given SATP (which contains
// the root-pointer of page-table) and a virtual address.

// The final result is either an exception (access or page fault) or
// success (with the target PTE).

// Handles (kilo)pages, megapages and gigapages.
// Handles Sv32, Sv39, Sv48
//
// Terminology: VA=virtual addr; PA=physical addr
//
// ================================================================
// BSV lib imports

import Vector       :: *;
import BRAMCore     :: *;
import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle     :: *;
import GetPut_Aux    :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;

// ================================================================

export  PTW_Result (..), PTW_Req (..), PTW_Rsp (..);
export  PTW_Mem_Req (..), PTW_Mem_Rsp (..);
export  PTW_IFC (..);
export  mkPTW;

// ================================================================
// MODULE INTERFACE

// ----------------
// Requests/responses to PTW from IMem and DMem

typedef struct {
   WordXL va;
   WordXL satp;
   } PTW_Req
deriving (Bits, FShow);

typedef enum {
   PTW_OK,
   PTW_ACCESS_FAULT,
   PTW_PAGE_FAULT
   } PTW_Result
deriving (Bits, Eq, FShow);

typedef struct {
   PTW_Result  result;
   PTE         pte;        // relevant if result == PTW_OK or PTW_PAGE_FAULT

   // Info for PTE insertion into TLB
   Bit #(2)     level;
   PA           pte_pa;
   } PTW_Rsp
deriving (Bits, FShow);

// ----------------
// Requests/responses to memory from PTW
// All requests are reads

Bool mem_op_read  = True;
Bool mem_op_write = False;

typedef struct {
   PA  pte_pa;
   } PTW_Mem_Req
deriving (Bits, FShow);

// Responses are only expected for reads; writes are "fire and forget"

typedef struct {
   Bool    ok;
   WordXL  pte;
   } PTW_Mem_Rsp
deriving (Bits, FShow);

// ----------------

interface PTW_IFC;
   // ----------------
   // PTW requests from IMem and DMem
   interface Server #(PTW_Req, PTW_Rsp)  imem_server;
   interface Server #(PTW_Req, PTW_Rsp)  dmem_server;

   // ----------------
   // PTW's requests to memory and responses.
   // Responses expected only for reads
   interface Client #(PTW_Mem_Req, PTW_Mem_Rsp) mem_client;
endinterface

// ================================================================
// FSM state

typedef enum {FSM_IDLE,       // No PTW in progress
`ifdef RV64
	      FSM_LEVEL_2,    // Page Table Walk, Request Level 2
`endif
	      FSM_LEVEL_1,    // Page Table Walk, Request Level 1
	      FSM_LEVEL_0     // Page Table Walk, Request Level 0
   } PTW_State
deriving (Bits, Eq, FShow);

// ================================================================

(* synthesize *)
module mkPTW #(parameter Bit #(3) verbosity) (PTW_IFC);

   // verbosity: 0: quiet; 1: rule firings

   // Overall state of this module
   Reg #(PTW_State)   rg_state      <- mkReg (FSM_IDLE);

   // Requests from IMem/DMem, and responses
   FIFOF #(PTW_Req) f_imem_reqs <- mkFIFOF;
   FIFOF #(PTW_Rsp) f_imem_rsps <- mkFIFOF;
   // Merged: True: from DMem, False: from IMem
   FIFOF #(Tuple2 #(Bool, PTW_Req)) f_dmem_imem_reqs <- mkFIFOF;

   FIFOF #(PTW_Req) f_dmem_reqs <- mkFIFOF;
   FIFOF #(PTW_Rsp) f_dmem_rsps <- mkFIFOF;

   // Requests to memory, and responses, for PTE refills/writebacks
   FIFOF #(PTW_Mem_Req) f_mem_reqs <- mkFIFOF;
   FIFOF #(PTW_Mem_Rsp) f_mem_rsps <- mkFIFOF;

   // ****************************************************************
   // BEHAVIOR

   // ================================================================
   // Arbitrate and merge requests
   // TODO: this merge could be done in the server 'put' methods, and
   // then we don't need separate FIFOs for each client's requests.

   // This urgency annotation is arbitrary

   rule rl_merge_dmem_reqs;
      let req <- pop (f_dmem_reqs);
      f_dmem_imem_reqs.enq (tuple2 (True, req));

      if (verbosity >= 1)
	 $display ("%0d: %m.rl_merge_dmem_reqs:\n    ", cur_cycle, fshow (req));
   endrule

   (* descending_urgency = "rl_merge_imem_reqs, rl_merge_dmem_reqs" *)
   rule rl_merge_imem_reqs;
      let req <- pop (f_imem_reqs);
      f_dmem_imem_reqs.enq (tuple2 (False, req));

      if (verbosity >= 1)
	 $display ("%0d: %m.rl_merge_imem_reqs:\n    ", cur_cycle, fshow (req));
   endrule

   // ================================================================
   // Derivations from head of request queue

   match { .dmem_not_imem, .ptw_req } = f_dmem_imem_reqs.first;

   // Derivations from ptw_req.va
   VA      va     = fn_WordXL_to_VA (ptw_req.va);
   VPN     vpn    = fn_Addr_to_VPN (va);
`ifdef RV64
   VPN_J   vpn_2  = fn_Addr_to_VPN_2 (va);
`endif
   VPN_J   vpn_1  = fn_Addr_to_VPN_1 (va);
   VPN_J   vpn_0  = fn_Addr_to_VPN_0 (va);
   Offset  offset = fn_Addr_to_Offset (ptw_req.va);

   // Derivations from ptw_req.satp
   VM_Mode  vm_mode  = fn_satp_to_VM_Mode (ptw_req.satp);
   ASID     asid     = fn_satp_to_ASID    (ptw_req.satp);
   PPN      satp_ppn = fn_satp_to_PPN     (ptw_req.satp);
   PA       satp_pa  = fn_PPN_and_Offset_to_PA (satp_ppn, 12'b0);

   // This reg is used during a PTW: the PA of a PTE
   Reg #(PA) rg_pte_pa <- mkRegU;

   function Action ptw_rsp_enq (PTW_Rsp rsp);
      action
	 // Consume the request
	 f_dmem_imem_reqs.deq;
	 // Enq the response
	 if (dmem_not_imem) f_dmem_rsps.enq (rsp);
	 else               f_imem_rsps.enq (rsp);
      endaction
   endfunction

   // ================================================================
   // Start a PTW

   rule rl_ptw_start (rg_state == FSM_IDLE);
      // RV32.Sv32: Page Table top is at Level 1
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_ptw_start:", cur_cycle);
	 if (dmem_not_imem) $write ("    for D_Mem:");
	 else               $write ("    for I_Mem:");
	 $display (" satp_pa %0h  va %0h", satp_pa, va);
      end
`ifdef RV32
      PA           vpn_1_pa            = (zeroExtend (vpn_1) << bits_per_byte_in_wordxl);
      PA           lev_1_pte_pa        = satp_pa + vpn_1_pa;
      let mem_req = PTW_Mem_Req {pte_pa: lev_1_pte_pa};
      f_mem_reqs.enq (mem_req);
      rg_pte_pa <= lev_1_pte_pa;
      rg_state  <= FSM_LEVEL_1;

      if (verbosity >= 1)
	 $display ("    Sv32: mem_req level 1 PTE for PA %0h", lev_1_pte_pa);

`elsif SV39

      PA           vpn_2_pa            = (zeroExtend (vpn_2) << bits_per_byte_in_wordxl);
      PA           lev_2_pte_pa        = satp_pa + vpn_2_pa;
      let mem_req = PTW_Mem_Req {pte_pa: lev_2_pte_pa};
      f_mem_reqs.enq (mem_req);
      rg_pte_pa <= lev_2_pte_pa;
      rg_state  <= FSM_LEVEL_2;

      if (verbosity >= 1)
	 $display ("    Sv39/Sv48: mem_req level 2 PTE for PA %0h", lev_2_pte_pa);
`endif
   endrule

   // ----------------
   // Receive Level 2 PTE and process it (Sv39 or Sv48 only)

`ifdef SV39
   rule rl_ptw_level_2 (rg_state == FSM_LEVEL_2);
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_ptw_level_2:", cur_cycle);
	 $display ("    satp_pa %0h  va %0h  pte_pa %0h", satp_pa, va, rg_pte_pa);
      end

      // Memory read-response is a level 1 PTE
      let mem_rsp <- pop (f_mem_rsps);
      let ok  = mem_rsp.ok;
      let pte = mem_rsp.pte;

      if (! ok) begin    // Bus error
	 let ptw_rsp = PTW_Rsp {result: PTW_ACCESS_FAULT, pte: pte, level: 2, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state      <= FSM_IDLE;
	 if (verbosity >= 1)
	    $display ("    ACCESS FAULT: fabric response error");
      end

      else if (is_invalid_pte (pte)) begin
	 let ptw_rsp = PTW_Rsp {result: PTW_PAGE_FAULT, pte: pte, level: 2, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state      <= FSM_IDLE;
	 if (verbosity >= 1)
	    $display ("    pte %0h: PAGE FAULT: invalid PTE", pte);
      end

      // Pointer to next-level PTE
      else if ((fn_PTE_to_X (pte) == 0) && (fn_PTE_to_R (pte) == 0)) begin
	 PPN          ppn                 = fn_PTE_to_PPN (pte);
	 PA           lev_1_PTN_pa        = fn_PPN_and_Offset_to_PA (ppn, 12'b0);
	 PA           vpn_1_pa            = (zeroExtend (vpn_1) << bits_per_byte_in_wordxl);
	 PA           lev_1_pte_pa        = lev_1_PTN_pa + vpn_1_pa;
	 let mem_req = PTW_Mem_Req {pte_pa: lev_1_pte_pa};
	 f_mem_reqs.enq (mem_req);
	 rg_pte_pa <= lev_1_pte_pa;
	 rg_state  <= FSM_LEVEL_1;

	 if (verbosity >= 1)
	    $display ("    pte %0h: continue to level 1: req addr %0h", pte, lev_1_pte_pa);
      end

      // Leaf PTE pointing at address-space gigapage
      else begin
	 // Fault if PPN [1] or PPN [0] are not 0
	 PPN_1 ppn_1 = fn_PTE_to_PPN_1 (pte);
	 PPN_0 ppn_0 = fn_PTE_to_PPN_0 (pte);
	 if ((ppn_1 != 0) || (ppn_0 != 0)) begin
	    let ptw_rsp = PTW_Rsp {result: PTW_PAGE_FAULT, pte: pte, level: 2, pte_pa: rg_pte_pa};
	    ptw_rsp_enq (ptw_rsp);
	    rg_state <= FSM_IDLE;
	    if (verbosity >= 1)
	       $display ("    pte %0h (leaf->gigapage): PAGE FAULT: PPN[1]/PPN[0] not 0", pte);
	 end
	 else begin
	    // Success: gigapage PTE
	    let ptw_rsp = PTW_Rsp {result: PTW_OK, pte: pte, level: 2, pte_pa: rg_pte_pa};
	    ptw_rsp_enq (ptw_rsp);
	    rg_state <= FSM_IDLE;

	    if (verbosity >= 1) begin
	       PPN  ppn                = fn_PTE_to_PPN (pte);
	       PA   addr_space_page_pa = fn_PPN_and_Offset_to_PA (ppn, 12'b0);
	       $display ("    pte %0h (leaf->gigapage). pa %0h", pte, addr_space_page_pa);
	    end
	 end
      end
   endrule: rl_ptw_level_2
`endif      // ifdef SV39

   // ----------------
   // Receive Level 1 PTE and process it (Sv32, Sv39 or Sv48)

   rule rl_ptw_level_1 (rg_state == FSM_LEVEL_1);
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_ptw_level_1:", cur_cycle);
	 $display ("    satp_pa %0h  va %0h  pte_pa %0h", satp_pa, va, rg_pte_pa);
      end

      // Memory read-response is a level 1 PTE
      let mem_rsp <- pop (f_mem_rsps);
      let ok  = mem_rsp.ok;
      let pte = mem_rsp.pte;

      // Bus error
      if (! ok) begin
	 let ptw_rsp = PTW_Rsp {result: PTW_ACCESS_FAULT, pte: pte, level: 1, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state <= FSM_IDLE;
	 if (verbosity >= 1)
	    $display ("    ACCESS FAULT: fabric response error");
      end

      // Invalid PTE
      else if (is_invalid_pte (pte)) begin
	 let ptw_rsp = PTW_Rsp {result: PTW_PAGE_FAULT, pte: pte, level: 1, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state <= FSM_IDLE;

	 if (verbosity >= 1)
	    $display ("    pte %0h: PAGE FAULT: invalid PTE", pte);
      end

      // Pointer to next-level PTE
      else if ((fn_PTE_to_X (pte) == 0) && (fn_PTE_to_R (pte) == 0)) begin
	 PPN          ppn                 = fn_PTE_to_PPN (pte);
	 PA           lev_0_PTN_pa        = fn_PPN_and_Offset_to_PA (ppn, 12'b0);
	 PA           vpn_0_pa            = (zeroExtend (vpn_0) << bits_per_byte_in_wordxl);
	 PA           lev_0_pte_pa        = lev_0_PTN_pa + vpn_0_pa;
	 let mem_req = PTW_Mem_Req {pte_pa: lev_0_pte_pa};
	 f_mem_reqs.enq (mem_req);
	 rg_pte_pa <= lev_0_pte_pa;
	 rg_state  <= FSM_LEVEL_0;

	 if (verbosity >= 1)
	    $display ("    pte %0h: continue to level 0: req addr %0h", pte, lev_0_pte_pa);
      end

      // Leaf PTE pointing at address-space megapage
      // (permissions will be checked on subsequent TLB hit)
      else begin
	 // Fault if PPN [0] is not 0
	 PPN_0 ppn_0 = fn_PTE_to_PPN_0 (pte);
	 if (ppn_0 != 0) begin
	    let ptw_rsp = PTW_Rsp {result: PTW_PAGE_FAULT, pte: pte, level: 1, pte_pa: rg_pte_pa};
	    ptw_rsp_enq (ptw_rsp);
	    rg_state      <= FSM_IDLE;

	    if (verbosity >= 1)
	       $display ("    pte %0h (leaf->megapage): PAGE FAULT: PPN [0] is not zero", pte);
	 end

	 else begin
	    // Success: megapage PTE
	    let ptw_rsp = PTW_Rsp {result: PTW_OK, pte: pte, level: 1, pte_pa: rg_pte_pa};
	    ptw_rsp_enq (ptw_rsp);
	    rg_state <= FSM_IDLE;

	    if (verbosity >= 1) begin
	       PPN ppn                = fn_PTE_to_PPN (pte);
	       PA  addr_space_page_pa = fn_PPN_and_Offset_to_PA (ppn, 12'b0);
	       $display ("    pte %0h (leaf->megapage); pa %0h", pte, addr_space_page_pa);
	    end
	 end
      end
   endrule: rl_ptw_level_1

   // ----------------
   // Receive Level 0 PTE and process it

   rule rl_ptw_level_0 (rg_state == FSM_LEVEL_0);
      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_ptw_level_0:", cur_cycle);
	 $display ("    satp_pa %0h  va %0h  pte_pa %0h", satp_pa, va, rg_pte_pa);
      end

      // Memory read-response is a level 0 PTE
      let mem_rsp <- pop (f_mem_rsps);
      let ok  = mem_rsp.ok;
      let pte = mem_rsp.pte;

      // Bus error
      if (! ok) begin
	 let ptw_rsp = PTW_Rsp {result: PTW_ACCESS_FAULT, pte: pte, level: 0, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state      <= FSM_IDLE;
	 if (verbosity >= 1)
	    $display ("    ACCESS FAULT: fabric response error");
      end

      // Invalid PTE
      else if (is_invalid_pte (pte)) begin
	 let ptw_rsp = PTW_Rsp {result: PTW_PAGE_FAULT, pte: pte, level: 0, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state      <= FSM_IDLE;

	 if (verbosity >= 1)
	    $display ("    pte %0h: PAGE FAULT: invalid PTE", pte);
      end

      // Pointer to next-level PTE: invalid at level 0
      else if ((fn_PTE_to_X (pte) == 0) && (fn_PTE_to_R (pte) == 0)) begin
	 let ptw_rsp = PTW_Rsp {result: PTW_PAGE_FAULT, pte: pte, level: 0, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state      <= FSM_IDLE;

	 if (verbosity >= 1)
	    $display ("    pte %0h: PAGE FAULT: not a leaf PTE", pte);
      end

      else begin
	 // Success: kilopage pointer
	 let ptw_rsp = PTW_Rsp {result: PTW_OK, pte: pte, level: 0, pte_pa: rg_pte_pa};
	 ptw_rsp_enq (ptw_rsp);
	 rg_state <= FSM_IDLE;

	 if (verbosity >= 1) begin
	    PPN ppn                = fn_PTE_to_PPN (pte);
	    PA  addr_space_page_pa = fn_PPN_and_Offset_to_PA (ppn, 12'b0);
	    $display ("    pte %0h: leaf PTE; pa %0h", pte, addr_space_page_pa);
	 end
      end
   endrule

   // ================================================================
   // INTERFACE

   // ----------------
   // PTW requests from IMem and DMem
   interface Server imem_server = toGPServer (f_imem_reqs, f_imem_rsps);
   interface Server dmem_server = toGPServer (f_dmem_reqs, f_dmem_rsps);

   // ----------------
   // PTW's requests to memory and responses.
   // Responses expected only for reads
   interface Client mem_client = toGPClient (f_mem_reqs, f_mem_rsps);

endmodule

// ================================================================

endpackage
