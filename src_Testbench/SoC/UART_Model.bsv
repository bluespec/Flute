// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved

package UART_Model;

// ================================================================
// This package implements a slave IP, a UART model.
//
// This is a basic (and somewhat incomplete) model of a classic 16550
// UART, enough to do basic character reads and writes, interrupts,
// etc.  Just sends/receives the chars into Get/Put interfaces,
// leaving it to external logic to manage actual physical
// tranmit/receive.  In particular, this module does nothing about
// clock speed, baud rates, etc.
//
// Bus interface width: This slave IP can be attached to fabrics with
// 32b- or 64b-wide data channels.  The type parameter 'Wd_Data' in
// Fabric_Defs.bsv specifies this.
//
// Address stride: the 16550 UART's registers are just 1-byte wide.
// As a slave IP in a system, this IP places them at aligned addresses
// with a gap of 4 or 8 bytes.  This is controlled by the Integer
// parameter 'address_stride' in this file.

// Some of the 'truncate()'s and 'zeroExtend()'s below are no-ops but
// necessary to satisfy type-checking, to manage these width
// variations.
// ================================================================

export UART_IFC (..), mkUART;

// ================================================================
// BSV library imports

import  Vector        :: *;
import  FIFOF         :: *;
import  GetPut        :: *;
import  ClientServer  :: *;
import  ConfigReg     :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import AXI4_Types  :: *;
import Fabric_Defs :: *;

// ================================================================
// UART registers and their address offsets

Bit #(3)  addr_UART_rbr  = 3'h_0;    // receiver buffer register (read only)
Bit #(3)  addr_UART_thr  = 3'h_0;    // transmitter holding register (write only)
Bit #(3)  addr_UART_ier  = 3'h_1;    // interrupt enable register
Bit #(3)  addr_UART_iir  = 3'h_2;    // interrupt id register    (read-only)
Bit #(3)  addr_UART_lcr  = 3'h_3;    // line control reg
Bit #(3)  addr_UART_mcr  = 3'h_4;    // modem control reg
Bit #(3)  addr_UART_lsr  = 3'h_5;    // line status reg     (read-only)
Bit #(3)  addr_UART_msr  = 3'h_6;    // modem status reg    (read-only)
Bit #(3)  addr_UART_scr  = 3'h_7;    // scratch pad reg

// Aliased registers, depending on control bits
Bit #(3)  addr_UART_dll  = 3'h_0;    // divisor latch low
Bit #(3)  addr_UART_dlm  = 3'h_1;    // divisor latch high
Bit #(3)  addr_UART_fcr  = 3'h_2;    // fifo control reg    (write-only)

// Bit fields of ier (Interrupt Enable Register)
Bit #(8)  uart_ier_erbfi = 8'h_01;     // Enable Received Data Available Interrupt
Bit #(8)  uart_ier_etbei = 8'h_02;     // Enable Transmitter Holding Register Empty Interrupt
Bit #(8)  uart_ier_elsi  = 8'h_04;     // Enable Receiver Line Status Interrupt
Bit #(8)  uart_ier_edssi = 8'h_08;     // Enable Modem Status Interrupt

// iir values (Interrupt Identification Register) in decreasing priority of interrupts
Bit #(8)  uart_iir_none  = 8'h_01;     // None (no interrupts pending)
Bit #(8)  uart_iir_rls   = 8'h_06;     // Receiver Line Status
Bit #(8)  uart_iir_rda   = 8'h_04;     // Received Data Available
Bit #(8)  uart_iir_cti   = 8'h_0C;     // Character Timeout Indication
Bit #(8)  uart_iir_thre  = 8'h_02;     // Transmitter Holding Register Empty
Bit #(8)  uart_iir_ms    = 8'h_00;     // Modem Status

// Bit fields of LCR
Bit #(8)  uart_lcr_dlab  = 8'h_80;     // Divisor latch access bit
Bit #(8)  uart_lcr_bc    = 8'h_40;     // Break control
Bit #(8)  uart_lcr_sp    = 8'h_20;     // Stick parity
Bit #(8)  uart_lcr_eps   = 8'h_10;     // Even parity
Bit #(8)  uart_lcr_pen   = 8'h_08;     // Parity enable
Bit #(8)  uart_lcr_stb   = 8'h_04;     // # of stop bits (0=1b,1=2b)
Bit #(8)  uart_lcr_wls   = 8'h_03;     // word len (0:5b,1:6b,2:7b,3:8b)

// Bit fields of LSR
Bit #(8)  uart_lsr_rxfe  = 8'h_80;    // Receiver FIFO error
Bit #(8)  uart_lsr_temt  = 8'h_40;    // Transmitter empty
Bit #(8)  uart_lsr_thre  = 8'h_20;    // THR empty
Bit #(8)  uart_lsr_bi    = 8'h_10;    // Break interrupt
Bit #(8)  uart_lsr_fe    = 8'h_08;    // Framing Error
Bit #(8)  uart_lsr_pe    = 8'h_04;    // Parity Error
Bit #(8)  uart_lsr_oe    = 8'h_02;    // Overrun Error
Bit #(8)  uart_lsr_dr    = 8'h_01;    // Data Ready

Bit #(8)  uart_lsr_reset_value = (uart_lsr_temt | uart_lsr_thre);

// ================================================================
// THIS MODULE's INTERFACE

interface UART_IFC;
   // Reset
   interface Server #(Bit #(0), Bit #(0))  server_reset;

   // set_addr_map should be called after this module's reset
   method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);

   // Main Fabric Reqs/Rsps
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave;

   // To external console
   interface Get #(Bit #(8))  get_to_console;
   interface Put #(Bit #(8))  put_from_console;

   // Interrupt pending
   (* always_ready *)
   method Bool  intr;
endinterface

// ================================================================
// Local types and constants

// Module state
typedef enum {STATE_START,
	      STATE_READY
   } Module_State
deriving (Bits, Eq, FShow);

// ================================================================
// Addressing of UART registers

// ----------------------------------------------------------------
// UART reg addresses should be at stride 4 or 8.

Integer address_stride = 4;
// Integer address_stride = 8;

// ----------------------------------------------------------------
// Split a bus address into (offset, lsbs), based on the address
// stride.

function Tuple2 #(Bit #(64), Bit #(3)) split_addr (Bit #(64) addr);
   Bit #(64) offset = ((address_stride == 4) ? (addr >> 2)           : (addr >> 3));
   Bit #(3)  lsbs   = ((address_stride == 4) ? { 1'b0, addr [1:0] }  : addr [2:0]);

   return tuple2 (offset, lsbs);
endfunction

// ----------------------------------------------------------------
// Extract data from AXI4 byte lanes, based on the AXI4 'strobe'
// (byte-enable) bits.

function Bit #(64) fn_extract_AXI4_data (Bit #(64) data, Bit #(8) strb);
   Bit #(64) result = 0;
   case (strb)
      8'b_0000_0001: result = zeroExtend (data [ 7:0]);
      8'b_0000_0010: result = zeroExtend (data [15:8]);
      8'b_0000_0100: result = zeroExtend (data [23:16]);
      8'b_0000_1000: result = zeroExtend (data [31:24]);
      8'b_0001_0000: result = zeroExtend (data [39:32]);
      8'b_0010_0000: result = zeroExtend (data [47:40]);
      8'b_0100_0000: result = zeroExtend (data [55:48]);
      8'b_1000_0000: result = zeroExtend (data [63:56]);

      8'b_0000_0011: result = zeroExtend (data [15:0]);
      8'b_0000_1100: result = zeroExtend (data [31:16]);
      8'b_0011_0000: result = zeroExtend (data [47:32]);
      8'b_1100_0000: result = zeroExtend (data [63:48]);

      8'b_0000_1111: result = zeroExtend (data [31:0]);
      8'b_1111_0000: result = zeroExtend (data [63:32]);

      8'b_1111_1111: result = zeroExtend (data [63:0]);
   endcase
   return result;
endfunction

// ================================================================
// THIS MODULE's IMPLEMENTATION

(* synthesize *)
module mkUART (UART_IFC);

   // 0: quiet; 1: reset and char input/output; 2: + detail
   Integer verbosity = 0;

   Reg #(Module_State) rg_state     <- mkReg (STATE_START);

   // These regs represent where this UART is placed in the address space.
   Reg #(Fabric_Addr)  rg_addr_base <- mkRegU;
   Reg #(Fabric_Addr)  rg_addr_lim  <- mkRegU;

   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

   // ----------------
   // Connector to AXI4 fabric

   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Slave_Xactor;

   // ----------------
   // character queues to and from external circuitry for the console

   FIFOF #(Bit #(8)) f_from_console <- mkFIFOF;
   FIFOF #(Bit #(8)) f_to_console   <- mkFIFOF;

   // ----------------
   // These are the 16550 UART registers
   // See fn_addr_offset() above for meaning of 'addr offset'

   Reg #(Bit #(8))  rg_rbr <- mkRegU;       // addr offset 0
   Reg #(Bit #(8))  rg_thr <- mkRegU;       // addr offset 0
   Reg #(Bit #(8))  rg_dll <- mkReg (0);    // addr offset 0

   Reg #(Bit #(8))  rg_ier <- mkReg (0);    // addr offset 1
   Reg #(Bit #(8))  rg_dlm <- mkReg (0);    // addr offset 1

   // IIR is a virtual read-only register computed from other regs
   Reg #(Bit #(8))  rg_fcr <- mkReg (0);    // addr offset 2

   Reg #(Bit #(8))  rg_lcr <- mkReg (0);    // addr offset 3
   Reg #(Bit #(8))  rg_mcr <- mkReg (0);    // addr offset 4
   Reg #(Bit #(8))  rg_lsr <- mkReg (uart_lsr_reset_value);    // addr offset 5
   Reg #(Bit #(8))  rg_msr <- mkReg (0);    // addr offset 6
   Reg #(Bit #(8))  rg_scr <- mkReg (0);    // addr offset 7

   // ----------------
   // Virtual read-only register IIR

   function Bit #(8) fn_iir ();
      Bit #(8) iir = uart_iir_none;

      if (   ((rg_ier & uart_ier_erbfi) != 0)    // Rx interrupt enabled
	  && ((rg_lsr & uart_lsr_dr)    != 0))   // data ready
	 iir = uart_iir_rda;

      else if ((rg_ier & uart_ier_etbei) != 0)   // Tx Holding Reg Empty intr enabled
	 iir = uart_iir_thre;

      return iir;
   endfunction

   // ----------------
   // Test if an interrupt is pending

   function Bool fn_intr ();
      let iir = fn_iir ();
      Bool eip = ((iir & uart_iir_none) == 0);
      return eip;
   endfunction

   // ================================================================
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Soft reset (on token in f_reset_reqs)

   rule rl_reset;
      f_reset_reqs.deq;

      rg_dll <= 0;
      rg_ier <= 0;
      rg_dlm <= 0;
      rg_fcr <= 0;
      rg_lcr <= 0;
      rg_mcr <= 0;
      rg_lsr <= uart_lsr_reset_value;
      rg_msr <= 0;
      rg_scr <= 0;

      slave_xactor.reset;
      rg_state <= STATE_READY;

      f_reset_rsps.enq (?);

      if (verbosity != 0)
	 $display ("%0d: UART.rl_reset", cur_cycle);
   endrule

   // ----------------------------------------------------------------
   // Handle fabric read requests

   rule rl_process_rd_req (rg_state == STATE_READY);
      let rda <- pop_o (slave_xactor.o_rd_addr);

      let byte_addr = rda.araddr - rg_addr_base;
      match { .offset, .lsbs } = split_addr (zeroExtend (byte_addr));

      Bit #(8)  rdata_byte = 0;
      AXI4_Resp rresp      = axi4_resp_okay;

      if ((rda.araddr < rg_addr_base) || (rda.araddr >= rg_addr_lim)) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: UART addr out of bounds", cur_cycle);
	 $display ("    UART base addr 0x%0h  limit addr 0x%0h", rg_addr_base, rg_addr_lim);
	 $display ("    AXI4 request: ", fshow (rda));
	 rresp = axi4_resp_decerr;
      end
      else if (lsbs != 0) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: UART misaligned addr", cur_cycle);
	 $display ("    ", fshow (rda));
	 rresp = axi4_resp_slverr;
      end
      else if (offset [63:3] != 0) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: UART unsupported addr", cur_cycle);
	 $display ("    Register offset 0x%0h", offset);
	 $display ("    ", fshow (rda));
	 rresp = axi4_resp_decerr;
      end

      // offset 0: RBR
      else if ((offset [2:0] == addr_UART_rbr) && ((rg_lcr & uart_lcr_dlab) == 0)) begin
	 // Read an input char
	 rg_lsr <= (rg_lsr & (~ uart_lsr_dr));    // Reset data-ready
	 rdata_byte = rg_rbr;
      end
      // offset 0: DLL
      else if ((offset [2:0] == addr_UART_dll) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rdata_byte = rg_dll;

      // offset 1: IER
      else if ((offset [2:0] == addr_UART_ier) && ((rg_lcr & uart_lcr_dlab) == 0))
	 rdata_byte = rg_ier;
      // offset 1: DLM
      else if ((offset [2:0] == addr_UART_dlm) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rdata_byte = rg_dlm;

      // offset 2: IIR (read-only)
      else if (offset [2:0] == addr_UART_iir) rdata_byte  = fn_iir();

      // offset 3: LCR
      else if (offset [2:0] == addr_UART_lcr) rdata_byte  = { 0, rg_lcr };
      // offset 4: MCR
      else if (offset [2:0] == addr_UART_mcr) rdata_byte  = { 0, rg_mcr };
      // offset 5: LSR
      else if (offset [2:0] == addr_UART_lsr) rdata_byte  = { 0, rg_lsr };
      // offset 6: MSR
      else if (offset [2:0] == addr_UART_msr) rdata_byte  = { 0, rg_msr };
      // offset 7: SCR
      else if (offset [2:0] == addr_UART_scr) rdata_byte  = { 0, rg_scr };

      else begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: UART unsupported addr", cur_cycle);
	 $display ("    ", fshow (rda));
	 rresp = axi4_resp_decerr;
      end

      // Align data byte for AXI4 data bus based on fabric-width
      Fabric_Data rdata = zeroExtend (rdata_byte);
      if ((valueOf (Wd_Data) == 64) && (byte_addr [2:0] == 3'b100))
	 rdata = rdata << 32;

      // Send read-response to bus
      let rdr = AXI4_Rd_Data {rid:   rda.arid,
			      rdata: rdata,
			      rresp: rresp,
			      rlast: True,
			      ruser: rda.aruser};
      slave_xactor.i_rd_data.enq (rdr);

      if (verbosity > 1) begin
	 $display ("%0d: %m.rl_process_rd_req", cur_cycle);
	 $display ("    ", fshow (rda));
	 $display ("    ", fshow (rdr));
	 $display ("    UART reg [%0h] => 0x%0h", offset [2:0], rdata_byte);
      end
   endrule

   // ----------------------------------------------------------------
   // Handle fabric write requests

   rule rl_process_wr_req (rg_state == STATE_READY);
      let wra <- pop_o (slave_xactor.o_wr_addr);
      let wrd <- pop_o (slave_xactor.o_wr_data);

      Bit #(64) wdata     = zeroExtend (wrd.wdata);
      Bit #(8)  wstrb     = zeroExtend (wrd.wstrb);
      Bit #(8)  data_byte = truncate (fn_extract_AXI4_data (wdata, wstrb));

      let byte_addr = wra.awaddr - rg_addr_base;
      match { .offset, .lsbs } = split_addr (zeroExtend (byte_addr));

      AXI4_Resp bresp = axi4_resp_okay;

      if ((wra.awaddr < rg_addr_base) || (wra.awaddr >= rg_addr_lim)) begin
	 $display ("%0d: %m.rl_process_rd_req: ERROR: UART addr out of bounds", cur_cycle);
	 $display ("    UART base addr 0x%0h  limit addr 0x%0h", rg_addr_base, rg_addr_lim);
	 $display ("    AXI4 request: ", fshow (wra));
	 bresp = axi4_resp_decerr;
      end
      else if (lsbs != 0) begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: UART misaligned addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 bresp = axi4_resp_slverr;
      end
      else if (offset [63:3] != 0) begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: UART unsupported addr", cur_cycle);
	 $display ("    Register offset 0x%0h", offset);
	 $display ("    ", fshow (wra));
	 $display ("    ", fshow (wrd));
	 bresp = axi4_resp_decerr;
      end

      // offset 0: THR
      else if ((offset [2:0] == addr_UART_thr) && ((rg_lcr & uart_lcr_dlab) == 0)) begin
	 // Write a char to the serial line
	 rg_thr <= data_byte;
	 f_to_console.enq (data_byte);
	 if (verbosity != 0)
	    $display ("%0d: %m.rl_process_wr_req: ASCII %0d (0x%0h)",
		      cur_cycle, data_byte, data_byte);
      end
      // offset 0: DLL
      else if ((offset [2:0] == addr_UART_dll) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rg_dll <= data_byte;

      // offset 1: IER
      else if ((offset [2:0] == addr_UART_ier) && ((rg_lcr & uart_lcr_dlab) == 0))
	 rg_ier <= data_byte;
      // offset 1: DLM
      else if ((offset [2:0] == addr_UART_dlm) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rg_dlm <= data_byte;

      // offset 2: FCR (write-only)
      else if (offset [2:0] == addr_UART_fcr) rg_fcr <= data_byte;

      // offset 3: LCR
      else if (offset [2:0] == addr_UART_lcr) rg_lcr <= data_byte;
      // offset 4: MCR
      else if (offset [2:0] == addr_UART_mcr) rg_mcr <= data_byte;
      // offset 5: LSR
      else if (offset [2:0] == addr_UART_lsr) noAction;    // LSR is read-only
      // offset 6: MSR
      else if (offset [2:0] == addr_UART_msr) noAction;    // MSR is read-only
      // offset 7: SCR
      else if (offset [2:0] == addr_UART_scr) rg_scr <= data_byte;

      else begin
	 $display ("%0d: %m.rl_process_wr_req: ERROR: UART unsupported addr", cur_cycle);
	 $display ("    ", fshow (wra));
	 $display ("    ", fshow (wrd));
	 bresp = axi4_resp_decerr;
      end

      // Send write-response to bus
      let wrr = AXI4_Wr_Resp {bid:   wra.awid,
			      bresp: bresp,
			      buser: wra.awuser};
      slave_xactor.i_wr_resp.enq (wrr);

      if (verbosity > 1) begin
	 $display ("%0d: %m.rl_process_wr_req", cur_cycle);
	 $display ("    ", fshow (wra));
	 $display ("    ", fshow (wrd));
	 $display ("    ", fshow (wrr));
	 $display ("    UART reg [%0h] <= data %0h", offset [2:0], data_byte);
      end
   endrule

   // ----------------------------------------------------------------
   // Receive a char from the serial line when RBR is empty (i.e., LSR.DR is 0),
   // and deposit it into RBR
   // and set it full (LSR.DR = 1)

   (* descending_urgency = "rl_receive, rl_process_rd_req" *)

   rule rl_receive ((rg_lsr & uart_lsr_dr) == 0);
      let ch <- pop (f_from_console);
      rg_rbr <= ch;

      let new_lsr = (rg_lsr | uart_lsr_dr);    // Set data-ready
      rg_lsr <= new_lsr;

      if (verbosity > 1)
	 $display ("UART_Model.rl_receive: received char 0x%0h; new_lsr = 0x%0h",
		   ch, new_lsr);
   endrule

   // ================================================================
   // INTERFACE

   // Reset
   interface server_reset   = toGPServer (f_reset_reqs, f_reset_rsps);

   // set_addr_map should be called after this module's reset
   method Action  set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);
      if (addr_base [2:0] != 0)
	 $display ("%0d: WARNING: UART.set_addr_map: addr_base 0x%0h is not 8-Byte-aligned",
		   cur_cycle, addr_base);

      if (addr_lim [2:0] != 0)
	 $display ("%0d: WARNING: UART.set_addr_map: addr_lim 0x%0h is not 8-Byte-aligned",
		   cur_cycle, addr_lim);

      rg_addr_base <= addr_base;
      rg_addr_lim  <= addr_lim;
   endmethod

   // Main Fabric Reqs/Rsps
   interface  slave = slave_xactor.axi_side;

   // To external console
   interface  put_from_console = toPut (f_from_console);
   interface  get_to_console   = toGet (f_to_console);

   // Interrupt pending
   method Bool  intr;
      return fn_intr ();
   endmethod
endmodule

// ================================================================

endpackage
