// Copyright (c) 2017-2019 Bluespec, Inc. All Rights Reserved.

package DM_System_Bus;

// ================================================================
// This package implements the 'System Bus Access' part of the RISC-V
// Debug Module, i.e., read/write access to RISC-V system memory.

// ================================================================
// BSV library imports

import FIFOF :: *;

// ----------------
// Other library imports

import Semi_FIFOF :: *;

// ================================================================
// Project Imports

import ISA_Decls :: *;
import DM_Common :: *;

import Fabric_Defs     :: *;
import SoC_Map         :: *;
import AXI4_Lite_Types :: *;

// ================================================================
// Interface

interface DM_System_Bus_IFC;
   method Action reset;

   // ----------------
   // DMI facing GDB/host
   method ActionValue #(DM_Word) av_read  (DM_Addr dm_addr);
   method Action  write (DM_Addr dm_addr, DM_Word dm_word);

   // ----------------
   // Facing System
   interface AXI4_Lite_Master_IFC #(Wd_Addr, Wd_Data, Wd_User) master;
endinterface

// ================================================================
// AXI4-Lite 'user' field

Bit #(Wd_User) dummy_user = ?;

// ================================================================

function Bit #(n) fn_align_addr_to_32_bit (Bit #(n) addr);
   return addr & (~ 'h3);
endfunction

function Bit #(n) fn_align_addr_to_64_bit (Bit #(n) addr);
   return addr & (~ 'h7);
endfunction

// ================================================================
// System Bus access states

typedef enum {SB_IDLE,
	      SB_READ_START,
	      SB_READ_FINISH_L32,
	      SB_READ_FINISH_U32,
	      SB_WRITE_START,
	      SB_WRITE_FINISH
   } SB_State
deriving (Bits, Eq, FShow);

(* synthesize *)
module mkDM_System_Bus (DM_System_Bus_IFC);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   // ----------------------------------------------------------------

   // TRX interface to memory
   AXI4_Lite_Master_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Lite_Master_Xactor_2;

   // ----------------------------------------------------------------
   // System Bus Read Engine state

   Reg #(SB_State) rg_sb_state <- mkRegU;
   Bool sbbusy = (rg_sb_state != SB_IDLE);

   // ----------------------------------------------------------------
   // rg_sbaddress0,1 (2 not implemented)
   // rg_sbdat0,1     (2, 3 not implemented)
   // Support for RV64. Instead of defining in terms of XLEN, defining using
   // DM_Word which is always Bit#(32). 64-bit addressing supported for RV64 but
   // only 32-bit data accesses are supported from debugger

   Reg #(DM_Word) rg_sbaddress0 <- mkRegU;
   Reg #(DM_Word) rg_sbdata0    <- mkRegU;

`ifdef RV64
   Reg #(DM_Word) rg_sbaddress1 <- mkRegU;
   DM_Word sbaddress1 = rg_sbaddress1;

   Reg #(DM_Word) rg_sbdata1    <- mkRegU;
`else
   DM_Word sbaddress1 = 0;
`endif


   // ----------------------------------------------------------------
   // rg_sbcs

   Reg #(Bool)        rg_sbcs_sbbusyerror     <- mkRegU;
   Reg #(Bool)        rg_sbcs_sbreadonaddr    <- mkRegU;
   Reg #(DM_sbaccess) rg_sbcs_sbaccess        <- mkRegU;
   Reg #(Bool)        rg_sbcs_sbautoincrement <- mkRegU;
   Reg #(Bool)        rg_sbcs_sbreadondata    <- mkRegU;
   Reg #(DM_sberror)  rg_sbcs_sberror         <- mkRegU;

   UInt #(3)          sbversion = 1;

   DM_Word virt_rg_sbcs = {
        pack (sbversion)
      , 6'b0
      , pack (rg_sbcs_sbbusyerror)
      , pack (sbbusy)
      , pack (rg_sbcs_sbreadonaddr)
      , pack (rg_sbcs_sbaccess)
      , pack (rg_sbcs_sbautoincrement)
      , pack (rg_sbcs_sbreadondata)
      , pack (rg_sbcs_sberror)
`ifdef RV64
      , 7'd64                          // sbasize -- address size
`endif
`ifdef RV32
      , 7'd32                          // sbasize -- address size
`endif
      , 1'b0                           // sbaccess128
      , 1'b0                           // sbaccess64
      , 1'b1                           // sbaccess32
      , 1'b1                           // sbaccess16
      , 1'b1};                         // sbaccess8

   function Action fa_rg_sbcs_write (DM_Word dm_word);
      action
	 Bool        sbbusyerror     = unpack (dm_word [22]);
	 Bool        sbreadonaddr    = unpack (dm_word [20]);
	 DM_sbaccess sbaccess        = unpack (dm_word [19:17]);
	 Bool        sbautoincrement = unpack (dm_word [16]);
	 Bool        sbreadondata    = unpack (dm_word [15]);
	 DM_sberror  sberror         = unpack (dm_word [14:12]);

	 // No-op if not clearing existing error
	 if ((rg_sbcs_sberror != DM_SBERROR_NONE) && (sberror == DM_SBERROR_NONE)) begin
	    // Existing error is not being cleared
	    noAction;
	    $display ("DM_System_Bus.sbcs_write <= 0x%08h: ERROR", dm_word);
	    $display ("    ERROR: existing error (0x%0h) not being cleared", rg_sbcs_sberror);
	 end

	 else if (rg_sbcs_sbbusyerror && sbbusyerror) begin
	    $display ("DM_System_Bus.sbcs_write <= 0x%08h: clear sbbusyerror", dm_word);
	    rg_sbcs_sbbusyerror <= False;
	 end

	 // Check that requested access size is supported
	 else if (   (sbaccess == DM_SBACCESS_128_BIT)
		  || (sbaccess == DM_SBACCESS_64_BIT))
	    begin
	       rg_sbcs_sberror <= DM_SBERROR_OTHER;
	       $display ("DM_System_Bus.sbcs_write <= 0x%08h: ERROR", dm_word);
	       $display ("    ERROR: sbaccess ", fshow (sbaccess), " not supported");
	    end

	 // Ok
	 else begin
	    if (verbosity != 0)
	       $display ("DM_System_Bus.sbcs_write: 0x%08h", dm_word);

	    rg_sbcs_sbreadonaddr    <= sbreadonaddr;
	    rg_sbcs_sbaccess        <= sbaccess;
	    rg_sbcs_sbautoincrement <= sbautoincrement;
	    rg_sbcs_sbreadondata    <= sbreadondata;
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // rg_sbdata0, rg_sbdata1 reads

   function ActionValue #(DM_Word) fav_rg_sbdata_read (DM_Addr dm_addr);
      actionvalue
	 DM_Word result = 0;
	 if (sbbusy) begin
	    noAction;
	    $display ("DM_System_Bus.sbdata0_read: busy, setting sbbusyerror");
	    rg_sbcs_sbbusyerror <= True;
	 end

	 else if (rg_sbcs_sbbusyerror || rg_sbcs_sberror != DM_SBERROR_NONE) begin
	    $display ("DM_System_Bus.sbdata0.read: ignoring due to error");
	 end

	 else begin
	    if (dm_addr == dm_addr_sbdata0) result = rg_sbdata0;
`ifdef RV64
	    if (dm_addr == dm_addr_sbdata1) result = rg_sbdata1;
`endif
	    if (rg_sbcs_sbreadondata) begin
`ifdef FABRIC64
	       Bit #(64) addr64 = fn_align_addr_to_64_bit ({sbaddress1, rg_sbaddress0});
	       rg_sb_state <= ((rg_sbaddress0 [2] == 1'b1) ? SB_READ_FINISH_U32 : SB_READ_FINISH_L32);
`else
	       Bit #(64) addr64 = fn_align_addr_to_32_bit ({sbaddress1, rg_sbaddress0});
	       rg_sb_state <= SB_READ_FINISH_L32;
`endif
	       Fabric_Addr fabric_addr = truncate (addr64);
	       let rda = AXI4_Lite_Rd_Addr {araddr: fabric_addr,
					    arprot: 0,
					    aruser: dummy_user};
	       master_xactor.i_rd_addr.enq (rda);
	       if (verbosity != 0) begin
		  $display ("DM_System_Bus.sbdata_read: autoread: Initiating read");
		  $display ("     ", fshow (rda));
	       end
	    end
	 end
	 return result;
      endactionvalue
   endfunction

   // ----------------------------------------------------------------
   // rg_sbdata0, rg_sbdata1 writes

   function Action fa_rg_sbdata_write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 if (sbbusy) begin
	    noAction;
	    $display ("DM_System_Bus.sbdata0_write: busy, setting sbbusyerror");
	    rg_sbcs_sbbusyerror <= True;
	 end

	 else if (rg_sbcs_sbbusyerror || rg_sbcs_sberror != DM_SBERROR_NONE) begin
	    $display ("DM_System_Bus.sbdata0.write: ignoring due to error");
	 end

	 else begin
	    // Initiate system bus write
	    let incr = fn_sbaccess_to_addr_incr (rg_sbcs_sbaccess);

	    if (dm_addr == dm_addr_sbdata0) rg_sbdata0 <= dm_word;
`ifdef RV64
	    if (dm_addr == dm_addr_sbdata1) rg_sbdata1 <= dm_word;
`endif

	    rg_sb_state     <= SB_WRITE_FINISH;

`ifdef FABRIC64
	    Bit #(64) addr64 = fn_align_addr_to_64_bit ({ sbaddress1, rg_sbaddress0 });
`else
	    Bit #(64) addr64 = fn_align_addr_to_32_bit ({ sbaddress1, rg_sbaddress0 });
`endif
	    Fabric_Addr fabric_addr = truncate (addr64);
	    let wra = AXI4_Lite_Wr_Addr {awaddr: fabric_addr,
					 awprot: 0,
					 awuser: dummy_user};
	    master_xactor.i_wr_addr.enq (wra);

`ifdef FABRIC64
	    // Using only half the lanes of a 64-bit system bus. Duplicate the
	    // word across lanes and select between upper and lowere lanes based
	    // on fabric_addr [2]
	    let wrd = AXI4_Lite_Wr_Data {wdata: {dm_word, dm_word},
					 wstrb: (rg_sbaddress0 [2] == 1'b1) ? 'hf0 : 'h0f};   
`else
	    let wrd = AXI4_Lite_Wr_Data {wdata: dm_word,
					 wstrb: '1};
`endif
	    master_xactor.i_wr_data.enq (wrd);

	    if (verbosity != 0) begin
	       $display ("DM_System_Bus.write: (%0d bytes)", incr);
	       $display ("    ", fshow (wra));
	       $display ("    ", fshow (wrd));
	    end
	 end
      endaction
   endfunction

   // ================================================================
   // System bus access engine

   (* descending_urgency = "rl_sb_read_finish, reset" *)
   (* descending_urgency = "rl_sb_read_finish, write" *)
   rule rl_sb_read_finish (   (   (rg_sb_state == SB_READ_FINISH_U32)
                               || (rg_sb_state == SB_READ_FINISH_L32))
			   && (rg_sbcs_sberror == DM_SBERROR_NONE));
      Integer incr = fn_sbaccess_to_addr_incr (rg_sbcs_sbaccess);
      let rdr <- pop_o (master_xactor.o_rd_data);

      // Select the correct lanes to read from
      Bit #(64) data64 = zeroExtend (rdr.rdata);
      DM_Word rdata = ((rg_sb_state == SB_READ_FINISH_L32) ? data64 [31:0] : data64 [63:32]);

      if (rdr.rresp != AXI4_LITE_OKAY)
	 rg_sbcs_sberror <= DM_SBERROR_OTHER;

      rg_sbdata0      <= rdata;
      if (verbosity != 0) begin
	 $display ("DM_System_Bus.rule_sb_read_finish: [0x%0h], (%0d bytes)",
		   { sbaddress1, rg_sbaddress0 }, incr);
	 $display ("    => data 0x%0h", rdata);
      end

      rg_sb_state <= SB_IDLE;

      if (rg_sbcs_sbautoincrement) begin
         // Increment the sbaddress; use 64b to avoid overflows
	 Bit #(64) addr64 = {sbaddress1, rg_sbaddress0};
	 addr64 = addr64 + fromInteger (incr);

	 rg_sbaddress0 <= addr64 [31:0];
`ifdef RV64
	 rg_sbaddress1 <= addr64 [63:32];
`endif
	 if (verbosity != 0)
	    $display ("    autoincrement");
      end
   endrule

   (* descending_urgency = "rl_sb_write_finish, reset" *)
   (* descending_urgency = "rl_sb_write_finish, write" *)
   rule rl_sb_write_finish (   (rg_sb_state == SB_WRITE_FINISH)
			    && (rg_sbcs_sberror == DM_SBERROR_NONE));
      Integer incr = fn_sbaccess_to_addr_incr (rg_sbcs_sbaccess);

      rg_sb_state     <= SB_IDLE;

      Bit #(64) addr64 = {sbaddress1, rg_sbaddress0};
      if (verbosity != 0)
	 $display ("DM_System_Bus.rule_sb_write_finish: [0x%016h] %0d bytes <= 0x%08h",
	           addr64,
		   incr,
		   rg_sbdata0);

      if (rg_sbcs_sbautoincrement) begin
         // Increment the sbaddress; use 64b to avoid overflows
	 addr64 = addr64 + fromInteger (incr);

	 rg_sbaddress0 <= addr64 [31:0];
`ifdef RV64
	 rg_sbaddress1 <= addr64 [63:32];
`endif
	 if (verbosity != 0)
	    $display ("    autoincrement");
      end
   endrule

   rule rl_sb_write_response;
      let wrr <- pop_o (master_xactor.o_wr_resp);
      if (wrr.bresp != AXI4_LITE_OKAY)
	 rg_sbcs_sberror <= DM_SBERROR_OTHER;
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      // master_xactor.reset;    // TODO: introduces a scheduling cycle: fix this

      rg_sb_state <= SB_IDLE;

      rg_sbcs_sbbusyerror     <= False;
      rg_sbcs_sbreadonaddr    <= False;
      rg_sbcs_sbaccess        <= DM_SBACCESS_32_BIT;
      rg_sbcs_sbautoincrement <= False;
      rg_sbcs_sbreadondata    <= False;
      rg_sbcs_sberror         <= DM_SBERROR_NONE;

      rg_sbaddress0           <= 0;
      rg_sbdata0              <= 0;
`ifdef RV64
      rg_sbaddress1           <= 0;
`endif

      if (verbosity != 0)
	 $display ("DM_System_Bus: reset");
   endmethod

   // ----------------
   // DMI facing GDB/host

   // The predicate on read allows communication flow control to
   // throttle requests.  This achieves better performance, but is not
   // workable for a true JTAG transport.
   method ActionValue #(DM_Word) av_read (DM_Addr dm_addr) if (!sbbusy);
      actionvalue
	 DM_Word dm_word = 0;
	 if (dm_addr == dm_addr_sbcs) begin
	    dm_word = virt_rg_sbcs;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.read: [sbcs] => ", fshow_sbcs (dm_word));
	 end
	 else if (dm_addr == dm_addr_sbaddress0) begin
	    dm_word = rg_sbaddress0;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.read: [sbaddress0] => 0x%08h", dm_word);
	 end
	 else if (dm_addr == dm_addr_sbdata0) begin
	    dm_word <- fav_rg_sbdata_read (dm_addr_sbdata0);
	 end
`ifdef RV64
	 else if (dm_addr == dm_addr_sbaddress1) begin
	    dm_word = rg_sbaddress1;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.read: [sbaddress1] => 0x%08h", dm_word);
	 end
	 else if (dm_addr == dm_addr_sbdata1) begin
	    dm_word <- fav_rg_sbdata_read (dm_addr_sbdata1);
	 end
`endif
	 else begin
	    // dm_addr_sbaddress2..
	    // dm_addr_sbdata1...
	    dm_word = 0;
	    $display ("DM_System_Bus.read: [", fshow_dm_addr (dm_addr), "] not supported");
	 end
	 return dm_word;
      endactionvalue
   endmethod

   method Action write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 if (dm_addr == dm_addr_sbcs) begin
	    if (verbosity != 0)
	       $display ("DM_System_Bus.write: [sbcs] <= ", fshow_sbcs (dm_word));
            fa_rg_sbcs_write (dm_word);
	 end
	 else if (dm_addr == dm_addr_sbaddress0) begin
	    rg_sbaddress0 <= dm_word;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.write: [sbaddress0] <= 0x%08h", dm_word);

	    if (rg_sbcs_sbreadonaddr) begin
`ifdef FABRIC64
	       Bit #(64) addr64 = fn_align_addr_to_64_bit ({ sbaddress1, dm_word });
	       rg_sb_state <= ((dm_word [2] == 1'b1) ? SB_READ_FINISH_U32 : SB_READ_FINISH_L32);
`else
	       Bit #(64) addr64 = fn_align_addr_to_32_bit ({ sbaddress1, dm_word });
	       rg_sb_state <= SB_READ_FINISH_L32;
`endif
	       Fabric_Addr fabric_addr = truncate (addr64);
	       let rda = AXI4_Lite_Rd_Addr {araddr: fabric_addr,
					    arprot: 0,
					    aruser: dummy_user};
	       master_xactor.i_rd_addr.enq (rda);
	       if (verbosity != 0) begin
		  $display ("DM_System_Bus.sbcs_write: Initiating singleread");
		  $display ("    ", fshow (rda));
	       end
	    end
	 end
	 else if (dm_addr == dm_addr_sbdata0) begin
	    fa_rg_sbdata_write (dm_addr_sbdata0, dm_word);
	 end
`ifdef RV64
	 else if (dm_addr == dm_addr_sbaddress1) begin
	    rg_sbaddress1 <= dm_word;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.write: [sbaddress1] <= 0x%08h", dm_word);
	 end
	 else if (dm_addr == dm_addr_sbdata1) begin
	    fa_rg_sbdata_write (dm_addr_sbdata1, dm_word);
	 end
`endif
	 else begin
	    // dm_addr_sbaddress2..
	    // dm_addr_sbdata1..
	    let addr_name = fshow_dm_addr (dm_addr);
	    $display ("DM_System_Bus.write: [", addr_name, "] <= 0x%08h; addr not supported", dm_word);
	 end
      endaction
   endmethod

   // ----------------
   // Facing System
   interface AXI4_Lite_Master_IFC master = master_xactor.axi_side;
endmodule

// ================================================================

endpackage
