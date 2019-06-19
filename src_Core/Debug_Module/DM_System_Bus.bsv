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

import AXI4_Types  :: *;
import Fabric_Defs :: *;

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
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master;
endinterface

// ================================================================
// Local definitions

// ----------------
// Convert DM code for access size to AXI4 code for access size

function AXI4_Size  fn_DM_sbaccess_to_AXI4_Size (DM_sbaccess sbaccess);
   AXI4_Size  axi4_size = case (sbaccess)
			     DM_SBACCESS_8_BIT:  axsize_1;
			     DM_SBACCESS_16_BIT: axsize_2;
			     DM_SBACCESS_32_BIT: axsize_4;
			     DM_SBACCESS_64_BIT: axsize_8;
			  endcase;
   return axi4_size;
endfunction

// ----------------
// Extract bytes from raw word read from fabric.
// The bytes of interest are offset according to LSBs of addr.
// Arguments:
//  - a DM_sbaccess    (indicating size of access)
//  - a byte-address
//  - a load-word from fabric
// result:
//  - word with correct byte(s) shifted into LSBs and zero extended

function Bit #(64)  fn_extract_and_extend_bytes (DM_sbaccess  sbaccess,
						 Bit #(64)    read_addr,
						 Bit #(64)    word64);
   Bit #(3) addr_lsbs = read_addr [2:0];
   if (valueOf (Wd_Data) == 32)
      addr_lsbs = (addr_lsbs & 'h3);

   Bit #(64) result    = 0;
   case (sbaccess)
      DM_SBACCESS_8_BIT:  case (addr_lsbs)
			     'h0: result = zeroExtend (word64 [ 7: 0]);
			     'h1: result = zeroExtend (word64 [15: 8]);
			     'h2: result = zeroExtend (word64 [23:16]);
			     'h3: result = zeroExtend (word64 [31:24]);
			     'h4: result = zeroExtend (word64 [39:32]);
			     'h5: result = zeroExtend (word64 [47:40]);
			     'h6: result = zeroExtend (word64 [55:48]);
			     'h7: result = zeroExtend (word64 [63:56]);
			  endcase

      DM_SBACCESS_16_BIT: case (addr_lsbs)
			     'h0: result = zeroExtend (word64 [15: 0]);
			     'h2: result = zeroExtend (word64 [31:16]);
			     'h4: result = zeroExtend (word64 [47:32]);
			     'h6: result = zeroExtend (word64 [63:48]);
			  endcase

      DM_SBACCESS_32_BIT: case (addr_lsbs)
			     'h0: result = zeroExtend (word64 [31: 0]);
			     'h4: result = zeroExtend (word64 [63:32]);
			  endcase

      DM_SBACCESS_64_BIT: case (addr_lsbs)
			     'h0: result = word64;
			  endcase
   endcase
   return result;
endfunction

// ----------------
// Compute address, data and strobe (byte-enables) for writes to fabric

function Tuple4 #(Fabric_Addr,    // addr is 32b- or 64b-aligned
		  Fabric_Data,    // data is lane-aligned
		  Fabric_Strb,    // strobe
		  AXI4_Size)      // 8 for 8-byte writes, else 4
   fn_to_fabric_write_fields (DM_sbaccess  sbaccess,    // size of access
			      Bit #(64)    addr,
			      Bit #(64)    word64);     // data is in lsbs

   // First compute addr, data and strobe for a 64b-wide fabric
   Bit #(8)   strobe64    = 0;
   Bit #(3)   shift_bytes = addr [2:0];
   Bit #(6)   shift_bits  = { shift_bytes, 3'b0 };
   AXI4_Size  axsize      = axsize_128;    // Will be updated in 'case' below

   case (sbaccess)
      DM_SBACCESS_8_BIT:  begin
			     word64   = (word64 << shift_bits);
			     strobe64 = ('b_1   << shift_bytes);
			     axsize   = axsize_1;
			  end
      DM_SBACCESS_16_BIT: begin
			     word64   = (word64 << shift_bits);
			     strobe64 = ('b_11  << shift_bytes);
			     axsize   = axsize_2;
			  end
      DM_SBACCESS_32_BIT: begin
			     word64   = (word64  << shift_bits);
			     strobe64 = ('b_1111 << shift_bytes);
			     axsize   = axsize_4;
			  end
      DM_SBACCESS_64_BIT: begin
			     strobe64 = 'b_1111_1111;
			     axsize   = axsize_8;
			  end
   endcase

   // Adjust for 32b fabrics
   if ((valueOf (Wd_Data) == 32) && (addr [2] == 1'b1)) begin
      word64   = { 32'h0, word64 [63:32] };
      strobe64 = { 4'h0, strobe64 [7:4] };
   end

   // Finally, create fabric addr/data/strobe
   Fabric_Addr  fabric_addr   = truncate (addr);
   Fabric_Data  fabric_data   = truncate (word64);
   Fabric_Strb  fabric_strobe = truncate (strobe64);

   return tuple4 (fabric_addr, fabric_data, fabric_strobe, axsize);
endfunction: fn_to_fabric_write_fields

// ----------------
// System Bus access states

typedef enum {SB_NOTBUSY,
	      SB_READ_FINISH,
	      SB_WRITE_FINISH
   } SB_State
deriving (Bits, Eq, FShow);

// ================================================================
// Module implementation

(* synthesize *)
module mkDM_System_Bus (DM_System_Bus_IFC);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   // ----------------------------------------------------------------

   // Interface to memory fabric
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;

   // ----------------------------------------------------------------
   // System Bus state

   Reg #(SB_State) rg_sb_state <- mkRegU;
   Bool sbbusy = (rg_sb_state != SB_NOTBUSY);

   // ----------------------------------------------------------------
   // rg_sbaddress0,1  (2 not implemented)
   // rg_sbdata0       (1, 2, 3 not implemented)
   // Support for RV64. Instead of defining in terms of XLEN, defining using
   // DM_Word which is always Bit#(32). 64-bit addressing supported for RV64 but
   // only 32-bit data accesses are supported from debugger

   Reg #(DM_Word)  rg_sbaddress0 <- mkReg (0);
   Reg #(DM_Word)  rg_sbaddress1 <- mkReg (0);     // Will always be zero for RV32

   // Saved address during a read rg_sbaddress0/1 may be autoincremented,
   // but we need original addr byte-lane extraction from response
   Reg #(Bit #(64)) rg_sbaddress_reading <- mkRegU;

   Bit #(64) sbaddress = { rg_sbaddress1, rg_sbaddress0 };

   Reg #(DM_Word) rg_sbdata0    <- mkRegU;

   // ----------------------------------------------------------------
   // rg_sbcs

   Reg #(Bool)        rg_sbcs_sbbusyerror     <- mkRegU;
   Reg #(Bool)        rg_sbcs_sbreadonaddr    <- mkRegU;
   Reg #(DM_sbaccess) rg_sbcs_sbaccess        <- mkRegU;
   Reg #(Bool)        rg_sbcs_sbautoincrement <- mkRegU;
   Reg #(Bool)        rg_sbcs_sbreadondata    <- mkRegU;
   Reg #(DM_sberror)  rg_sbcs_sberror         <- mkRegU;

   UInt #(3)          sbversion = 1;

   DM_Word virt_rg_sbcs = {pack (sbversion),
			   6'b0,
			   pack (rg_sbcs_sbbusyerror),
			   pack (sbbusy),
			   pack (rg_sbcs_sbreadonaddr),
			   pack (rg_sbcs_sbaccess),
			   pack (rg_sbcs_sbautoincrement),
			   pack (rg_sbcs_sbreadondata),
			   pack (rg_sbcs_sberror),
`ifdef RV64
			   7'd64,    // sbasize -- address size
`endif
`ifdef RV32
			   7'd32,    // sbasize -- address size
`endif
			   1'b0,     // sbaccess128
			   1'b0,     // sbaccess64
			   1'b1,     // sbaccess32
			   1'b1,     // sbaccess16
			   1'b1};    // sbaccess8

   // ----------------
   // Local defs and help functions

   Integer addr_incr = fn_sbaccess_to_addr_incr (rg_sbcs_sbaccess);

   function Action fa_sbaddress_incr (Bit #(64) addr64);
      action
	 Bit #(64) next_sbaddress = addr64 + fromInteger (addr_incr);
`ifdef RV64
	 rg_sbaddress1 <= next_sbaddress [63:32];
`else
	 rg_sbaddress1 <= 0;
`endif
	 rg_sbaddress0 <= next_sbaddress [31:0];

	 if (verbosity != 0)
	    $display ("    Increment sbaddr 0x%08h -> 0x%08h", addr64, next_sbaddress);
      endaction
   endfunction

   // ----------------
   // Construction and sending of fabric read-requests

   function Action fa_fabric_send_read_req (Bit #(64)  addr64);
      action
	 Fabric_Addr fabric_addr = truncate (addr64);
	 let rda = AXI4_Rd_Addr {arid:     fabric_default_id,
				 araddr:   fabric_addr,
				 arlen:    0,                      // burst len = arlen+1
				 arsize:   fn_DM_sbaccess_to_AXI4_Size (rg_sbcs_sbaccess),
				 arburst:  fabric_default_burst,
				 arlock:   fabric_default_lock,
				 arcache:  fabric_default_arcache,
				 arprot:   fabric_default_prot,
				 arqos:    fabric_default_qos,
				 arregion: fabric_default_region,
				 aruser:   fabric_default_user};
	 master_xactor.i_rd_addr.enq (rda);

	 // Save read-address for byte-lane extraction from later response
	 // (since rg_sbaddress may be incremented by then).
	 rg_sbaddress_reading <= addr64;

	 rg_sb_state <= SB_READ_FINISH;

	 if (verbosity != 0) begin
	    $display ("    DM_System_Bus.fa_fabric_send_read_req, and => SB_READ_FINISH ");
	    $display ("    ", fshow (rda));
	 end
      endaction
   endfunction

   // ----------------
   // Construction and sending of fabric write-requests

   function Action fa_fabric_send_write_req (Bit #(64) data64);
      action
	 match {.fabric_addr,
		.fabric_data,
		.fabric_strb,
		.fabric_size} = fn_to_fabric_write_fields (rg_sbcs_sbaccess, sbaddress, data64);
	 
	 // fabric_addr is always fabric-data-width aligned
	 // fabric_data is properly lane-adjusted
	 // fabric_strb identifies the lanes to be written
	 // awsize is always the fabric width

	 let wra = AXI4_Wr_Addr {awid:     fabric_default_id,
				 awaddr:   fabric_addr,
				 awlen:    0,                      // burst len = awlen+1
				 awsize:   fabric_size,
				 awburst:  fabric_default_burst,
				 awlock:   fabric_default_lock,
				 awcache:  fabric_default_awcache,
				 awprot:   fabric_default_prot,
				 awqos:    fabric_default_qos,
				 awregion: fabric_default_region,
				 awuser:   fabric_default_user};
	 master_xactor.i_wr_addr.enq (wra);

	 let wrd = AXI4_Wr_Data {wdata: fabric_data,
				 wstrb: fabric_strb,
				 wlast: True,
				 wuser: fabric_default_user};
	 master_xactor.i_wr_data.enq (wrd);

	 if (verbosity != 0) begin
	    $display ("    DM_System_Bus.fa_fabric_send_write_req:");
	    $display ("    ", fshow (wra));
	    $display ("    ", fshow (wrd));
	 end
      endaction
   endfunction

   // ================================================================
   // Writes to sbcs

   function Action fa_rg_sbcs_write (DM_Word  dm_word);
      action
	 Bool        sbbusyerror     = unpack (dm_word [22]);
	 Bool        sbreadonaddr    = unpack (dm_word [20]);
	 DM_sbaccess sbaccess        = unpack (dm_word [19:17]);
	 Bool        sbautoincrement = unpack (dm_word [16]);
	 Bool        sbreadondata    = unpack (dm_word [15]);
	 DM_sberror  sberror         = unpack (dm_word [14:12]);

	 // No-op if not clearing existing sberror
	 if ((rg_sbcs_sberror != DM_SBERROR_NONE) && (sberror == DM_SBERROR_NONE)) begin
	    // Existing error is not being cleared
	    $display ("DM_System_Bus.sbcs_write <= 0x%08h: ERROR", dm_word);
	    $display ("    ERROR: existing sberror (0x%0h) is not being cleared.", rg_sbcs_sberror);
	    $display ("    Must be cleared to re-enable system bus access.");
	 end

	 // No-op if not clearing existing sbbusyerror
	 else if (rg_sbcs_sbbusyerror && (! sbbusyerror)) begin
	    $display ("DM_System_Bus.sbcs_write <= 0x%08h: ERROR", dm_word);
	    $display ("    ERROR: existing sbbusyerror (%0d) is not being cleared.", rg_sbcs_sbbusyerror);
	    $display ("    Must be cleared to re-enable system bus access.");
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
	    if (verbosity != 0) begin
	       $display ("    DM_System_Bus.sbcs_write: ", fshow_sbcs (dm_word));
	       if (rg_sbcs_sberror != DM_SBERROR_NONE)
		  $display ("        Clearing sbcs.sberror");
	       if (rg_sbcs_sbbusyerror)
		  $display ("        Clearing sbcs.sbbusyerror");
	    end

	    rg_sbcs_sbbusyerror     <= False;
	    rg_sbcs_sbreadonaddr    <= sbreadonaddr;
	    rg_sbcs_sbaccess        <= sbaccess;
	    rg_sbcs_sbautoincrement <= sbautoincrement;
	    rg_sbcs_sbreadondata    <= sbreadondata;
	    rg_sbcs_sberror         <= DM_SBERROR_NONE;
	 end
      endaction
   endfunction: fa_rg_sbcs_write

   // ================================================================
   // rg_sbaddress0, rg_sbaddress1 writes

   function Action fa_rg_sbaddress_write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 // Debug announce
	 if (verbosity != 0) begin
	    $write ("DM_System_Bus.sbaddress.write: [0x%08h] <= 0x%08h", dm_addr, dm_word);
	    if (rg_sbcs_sbreadonaddr) begin
	       $write ("; readonaddr");
	       if (rg_sbcs_sbautoincrement)
		  $write ("; autoincrement");
	    end
	    $display ("");
	 end

	 if (sbbusy) begin
	    $display ("DM_System_Bus.sbaddress.write: busy, setting sbbusyerror");
	    rg_sbcs_sbbusyerror <= True;
	 end

	 else if (rg_sbcs_sbbusyerror)
	    $display ("DM_System_Bus.sbaddress.write: ignoring due to sbbusyerror");

	 else if (rg_sbcs_sberror != DM_SBERROR_NONE)
	    $display ("DM_System_Bus.sbaddress.write: ignoring due to sberror = 0x%0h",
		      rg_sbcs_sberror);

	 else if (dm_addr == dm_addr_sbaddress0) begin
	    Bit #(64) addr64 = { rg_sbaddress1, dm_word };
	    if (rg_sbcs_sbreadonaddr) begin
	       fa_fabric_send_read_req  (addr64);
	       if (rg_sbcs_sbautoincrement)
		  fa_sbaddress_incr (addr64);
	       else
		  rg_sbaddress0 <= dm_word;
	    end
	    else
	       rg_sbaddress0 <= dm_word;
	 end

	 else begin // (dm_addr == dm_addr_sbaddress1)
`ifdef RV32
	    rg_sbaddress1 <= 0;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.write: [sbaddress1] <= 0 (RV32: ignoring arg value 0x%08h)",
			 dm_word);
`else
	    rg_sbaddress1 <= dm_word;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.write: [sbaddress1] <= 0x%08h", dm_word);
`endif
	 end
      endaction
   endfunction

   // ================================================================
   // rg_sbdata0, rg_sbdata1 reads

   function ActionValue #(DM_Word) fav_rg_sbdata_read (DM_Addr dm_addr);
      actionvalue
	 DM_Word result = 0;
	 if (sbbusy) begin
	    $display ("DM_System_Bus.sbdata.read: busy, setting sbbusyerror");
	    rg_sbcs_sbbusyerror <= True;
	 end

	 else if (rg_sbcs_sbbusyerror)
	    $display ("DM_System_Bus.sbdata.read: ignoring due to sbbusyerror");

	 else if (rg_sbcs_sberror != DM_SBERROR_NONE)
	    $display ("DM_System_Bus.sbdata.read: ignoring due to sberror = 0x%0h", rg_sbcs_sberror);

	 else begin
	    if (dm_addr == dm_addr_sbdata0)
	       result = rg_sbdata0;
	    /* FUTURE: when supporting DM_SBACCESS_64_BIT
	    else if (dm_addr == dm_addr_sbdata1)
	       result = rg_sbdata1;
	    */

	    // Increment sbaddress if needed
	    if (rg_sbcs_sbautoincrement)
	       fa_sbaddress_incr (sbaddress);

	    // Auto-read next data if needed
	    if (rg_sbcs_sbreadondata && (dm_addr == dm_addr_sbdata0))
	       fa_fabric_send_read_req (sbaddress);
	 end
	 return result;
      endactionvalue
   endfunction

   // ----------------
   // Finish read request (handle fabric response)

   (* descending_urgency = "rl_sb_read_finish, reset" *)
   (* descending_urgency = "rl_sb_read_finish, write" *)
   rule rl_sb_read_finish (   (rg_sb_state == SB_READ_FINISH)
			   && (rg_sbcs_sberror == DM_SBERROR_NONE));
      let rdr <- pop_o (master_xactor.o_rd_data);
      if (verbosity != 0)
	 $display ("DM_System_Bus.rule_sb_read_finish: rdr = ", fshow (rdr));

      // Extract relevant bytes from fabric data
      Bit #(64) rdata64 = zeroExtend (rdr.rdata);
      Bit #(64) data    = fn_extract_and_extend_bytes (rg_sbcs_sbaccess, rg_sbaddress_reading, rdata64);

      if (rdr.rresp != axi4_resp_okay) begin
	 $display ("DM_System_Bus.rule_sb_read_finish: setting rg_sbcs_sberror to DM_SBERROR_OTHER\n");
	 $display ("    rdr = ", fshow (rdr));
	 rg_sbcs_sberror <= DM_SBERROR_OTHER;
      end

      rg_sbdata0 <= data [31:0];
      /* FUTURE: when supporting DM_SBACCESS_64_BIT
      rg_sbdata1 <= data [63:32];
      */

      if (verbosity != 0) begin
	 $display ("DM_System_Bus.rule_sb_read_finish: addr 0x%0h,  sbaccess %0d (%0d bytes)",
		   rg_sbaddress_reading, rg_sbcs_sbaccess, addr_incr);
	 $display ("    rg_sbdata0 <= 0x%0h", data);
	 $display ("    module state => SB_NOTBUSY");
      end

      rg_sb_state <= SB_NOTBUSY;
   endrule

   // ================================================================
   // rg_sbdata0, rg_sbdata1 writes

   function Action fa_rg_sbdata_write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 if (sbbusy) begin
	    $display ("DM_System_Bus.sbdata.write: busy, setting sbbusyerror");
	    rg_sbcs_sbbusyerror <= True;
	 end

	 else if (rg_sbcs_sbbusyerror) begin
	    $display ("DM_System_Bus.sbdata.write: ignoring due to sbbusyerror");
	 end

	 else if (rg_sbcs_sberror != DM_SBERROR_NONE) begin
	    $display ("DM_System_Bus.sbdata.write: ignoring due to sberror = 0x%0h",
		      rg_sbcs_sberror);
	 end

	 else begin
	    if (verbosity != 0)
	       $display ("    DM_System_Bus.fa_rg_sbdata_write: dm_addr 0x%08h  dm_word 0x%08h",
			 dm_addr, dm_word);

	    if (dm_addr == dm_addr_sbdata0)
	       rg_sbdata0 <= dm_word;
	    /* FUTURE: when supporting DM_SBACCESS_64_BIT
	    else if (dm_addr == dm_addr_sbdata1)
	       rg_sbdata1 <= dm_word;
	    */

	    // Initiate system bus write if writing to sbdata0
	    if (dm_addr == dm_addr_sbdata0) begin
	       fa_fabric_send_write_req (zeroExtend (dm_word));

	       // Increment sbaddr ifneeded
	       if (rg_sbcs_sbautoincrement)
		  fa_sbaddress_incr (sbaddress);
	    end
	 end
      endaction
   endfunction

   // ----------------
   // Consume write-responses

   rule rl_sb_write_response;
      let wrr <- pop_o (master_xactor.o_wr_resp);
      if (wrr.bresp != axi4_resp_okay)
	 rg_sbcs_sberror <= DM_SBERROR_OTHER;
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      // master_xactor.reset;    // TODO: introduces a scheduling cycle: fix this

      rg_sb_state <= SB_NOTBUSY;

      rg_sbcs_sbbusyerror     <= False;
      rg_sbcs_sbreadonaddr    <= False;
      rg_sbcs_sbaccess        <= DM_SBACCESS_32_BIT;
      rg_sbcs_sbautoincrement <= False;
      rg_sbcs_sbreadondata    <= False;
      rg_sbcs_sberror         <= DM_SBERROR_NONE;

      rg_sbaddress0           <= 0;
      rg_sbaddress1           <= 0;
      rg_sbdata0              <= 0;

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

	 else if (dm_addr == dm_addr_sbaddress1) begin
	    dm_word = rg_sbaddress1;
	    if (verbosity != 0)
	       $display ("DM_System_Bus.read: [sbaddress1] => 0x%08h", dm_word);
	 end

	 else if (dm_addr == dm_addr_sbdata0) begin
	    dm_word <- fav_rg_sbdata_read (dm_addr_sbdata0);
	 end

	 else begin
	    // Unsupported dm address
	    dm_word = 0;
	    $display ("DM_System_Bus.read: [", fshow_dm_addr (dm_addr), "] not supported");
	 end
	 return dm_word;
      endactionvalue
   endmethod

   method Action write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 if (dm_addr == dm_addr_sbcs)
            fa_rg_sbcs_write (dm_word);

	 else if ((dm_addr == dm_addr_sbaddress0) || (dm_addr == dm_addr_sbaddress1))
	    fa_rg_sbaddress_write (dm_addr, dm_word);

	 else if (dm_addr == dm_addr_sbdata0) // FUTURE: || (dm_addr == dm_addr_sbdata1)
	    fa_rg_sbdata_write (dm_addr, dm_word);

	 else begin
	    // Unsupported dm_addr
	    let addr_name = fshow_dm_addr (dm_addr);
	    $display ("DM_System_Bus.write: [", addr_name, "] <= 0x%08h; addr not supported", dm_word);
	 end
      endaction
   endmethod

   // ----------------
   // Facing System
   interface AXI4_Master_IFC master = master_xactor.axi_side;
endmodule

// ================================================================

endpackage
