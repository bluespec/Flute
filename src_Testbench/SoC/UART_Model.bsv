// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package UART_Model;

// ================================================================
// This package implements a slave IP, a UART model.
//
// This is a very basic (and very incomplete!!) model of a classic
// 16550 UART, just enough to do basic character reads and writes.
//
// ----------------
// This slave IP can be attached to fabrics with 32b- or 64b-wide data channels.
//    (NOTE: this is the width of the fabric, which can be chosen
//      independently of the native width of a CPU master on the
//      fabric (such as RV32/RV64 for a RISC-V CPU).
// When attached to 32b-wide fabric, 64-bit locations must be
// read/written in two 32b transaction, once for the lower 32b and
// once for the upper 32b.
//
// Some of the 'truncate()'s and 'zeroExtend()'s below are no-ops but
// necessary to satisfy type-checking.
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
// Interface

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

// ----------------
// Split a bus address into (offset in UART, lsbs)

function Tuple3 #(Bit #(2), Bit #(3), Bit #(3)) split_addr (Bit #(64) addr);
   // 8-byte stride
   Bit #(2)  msbs   = addr [7:6];
   Bit #(3)  offset = addr [5:3];
   Bit #(3)  lsbs   = addr [2:0];

   return tuple3 (msbs, offset, lsbs);
endfunction

// ================================================================

(* synthesize *)
module mkUART (UART_IFC);

   Reg #(Bit #(8)) cfg_verbosity <- mkConfigReg (0);

   Reg #(Module_State) rg_state     <- mkReg (STATE_START);
   Reg #(Fabric_Addr)  rg_addr_base <- mkRegU;
   Reg #(Fabric_Addr)  rg_addr_lim  <- mkRegU;

   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

   // ----------------
   // Connector to fabric

   AXI4_Slave_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Slave_Xactor;

   // ----------------
   // character queues to and from the console

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
      Bit #(8) iir = 0;

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

      if (cfg_verbosity != 0)
	 $display ("%0d: UART.rl_reset", cur_cycle);
   endrule

   // ----------------------------------------------------------------
   // Handle fabric read requests

   rule rl_process_rd_req (rg_state == STATE_READY);
      let rda <- pop_o (slave_xactor.o_rd_addr);

      let byte_addr = rda.araddr - rg_addr_base;
      let { msbs, offset, lsbs } = split_addr (zeroExtend (byte_addr));

      Bit #(8)  rdata_byte = 0;
      AXI4_Resp rresp      = axi4_resp_okay;

      if (lsbs != 0) begin
	 $display ("%0d: ERROR: UART.rl_process_rd_req: misaligned addr", cur_cycle);
	 $display ("            ", fshow (rda));
	 rresp = axi4_resp_slverr;
      end
      else if (msbs != 0) begin
	 $display ("%0d: ERROR: UART.rl_process_rd_req: unrecognized addr", cur_cycle);
	 $display ("            ", fshow (rda));
	 rresp = axi4_resp_slverr;
      end

      // offset 0: RBR
      else if ((offset == addr_UART_rbr) && ((rg_lcr & uart_lcr_dlab) == 0)) begin
	 // Read an input char
	 rg_lsr <= (rg_lsr & (~ uart_lsr_dr));    // Reset data-ready
	 rdata_byte = rg_rbr;
      end
      // offset 0: DLL
      else if ((offset == addr_UART_dll) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rdata_byte = rg_dll;

      // offset 1: IER
      else if ((offset == addr_UART_ier) && ((rg_lcr & uart_lcr_dlab) == 0))
	 rdata_byte = rg_ier;
      // offset 1: DLM
      else if ((offset == addr_UART_dlm) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rdata_byte = rg_dlm;

      // offset 2: IIR (read-only)
      else if (offset == addr_UART_iir) rdata_byte  = fn_iir();

      // offset 3: LCR
      else if (offset == addr_UART_lcr) rdata_byte  = { 0, rg_lcr };
      // offset 4: MCR
      else if (offset == addr_UART_mcr) rdata_byte  = { 0, rg_mcr };
      // offset 5: LSR
      else if (offset == addr_UART_lsr) rdata_byte  = { 0, rg_lsr };
      // offset 6: MSR
      else if (offset == addr_UART_msr) rdata_byte  = { 0, rg_msr };
      // offset 7: SCR
      else if (offset == addr_UART_scr) rdata_byte  = { 0, rg_scr };

      else begin
	 $display ("%0d: ERROR: UART.rl_process_rd_req: unrecognized addr", cur_cycle);
	 $display ("            ", fshow (rda));
	 rresp = axi4_resp_slverr;
      end

      // Send read-response to bus
      Fabric_Data rdata = zeroExtend (rdata_byte);
      let rdr = AXI4_Rd_Data {rid:   rda.arid,
			      rdata: rdata,
			      rresp: rresp,
			      rlast: True,
			      ruser: rda.aruser};
      slave_xactor.i_rd_data.enq (rdr);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: UART.rl_process_rd_req", cur_cycle);
	 $display ("            ", fshow (rda));
	 $display ("            ", fshow (rdr));
      end
   endrule

   // ----------------------------------------------------------------
   // Handle fabric write requests

   rule rl_process_wr_req (rg_state == STATE_READY);
      let wra <- pop_o (slave_xactor.o_wr_addr);
      let wrd <- pop_o (slave_xactor.o_wr_data);

      Bit #(64) wdata     = zeroExtend (wrd.wdata);
      Bit #(8)  wstrb     = zeroExtend (wrd.wstrb);
      Bit #(8)  data_byte = wdata [7:0];

      let byte_addr = wra.awaddr - rg_addr_base;
      let { msbs, offset, lsbs } = split_addr (zeroExtend (byte_addr));

      AXI4_Resp bresp = axi4_resp_okay;

      if ((lsbs != 0) || (wstrb [0] == 1'b0))  begin
	 $display ("%0d: ERROR: UART.rl_process_wr_req: misaligned addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 bresp = axi4_resp_slverr;
      end
      else if (msbs != 0) begin
	 $display ("%0d: ERROR: UART.rl_process_wr_req: unrecognized addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 bresp = axi4_resp_slverr;
      end

      // offset 0: THR
      else if ((offset == addr_UART_thr) && ((rg_lcr & uart_lcr_dlab) == 0)) begin
	 // Write a char to the serial line
	 rg_thr <= data_byte;
	 f_to_console.enq (data_byte);
      end
      // offset 0: DLL
      else if ((offset == addr_UART_dll) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rg_dll <= data_byte;

      // offset 1: IER
      else if ((offset == addr_UART_ier) && ((rg_lcr & uart_lcr_dlab) == 0))
	 rg_ier <= data_byte;
      // offset 1: DLM
      else if ((offset == addr_UART_dlm) && ((rg_lcr & uart_lcr_dlab) != 0))
	 rg_dlm <= data_byte;

      // offset 2: FCR (write-only)
      else if (offset == addr_UART_fcr) rg_fcr <= data_byte;

      // offset 3: LCR
      else if (offset == addr_UART_lcr) rg_lcr <= data_byte;
      // offset 4: MCR
      else if (offset == addr_UART_mcr) rg_mcr <= data_byte;
      // offset 5: LSR
      else if (offset == addr_UART_lsr) noAction;    // LSR is read-only
      // offset 6: MSR
      else if (offset == addr_UART_msr) noAction;    // MSR is read-only
      // offset 7: SCR
      else if (offset == addr_UART_scr) rg_scr <= data_byte;

      else begin
	 $display ("%0d: ERROR: UART.rl_process_wr_req: unrecognized addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 bresp = axi4_resp_slverr;
      end

      // Send write-response to bus
      let wrr = AXI4_Wr_Resp {bid:   wra.awid,
			      bresp: bresp,
			      buser: wra.awuser};
      slave_xactor.i_wr_resp.enq (wrr);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: UART.rl_process_wr_req", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 $display ("            ", fshow (wrr));
      end
   endrule

   // ----------------------------------------------------------------
   // Receive a char from the serial line when RBR is empty (i.e., LSR.DR is 0),
   // and deposit it into RBR
   // and set it full (LSR.DR = 1)

   rule rl_receive ((rg_lsr & uart_lsr_dr) == 0);
      let ch <- pop (f_from_console);
      rg_rbr <= ch;

      let new_lsr = (rg_lsr | uart_lsr_dr);    // Set data-ready
      rg_lsr <= new_lsr;

      if (cfg_verbosity > 1)
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
