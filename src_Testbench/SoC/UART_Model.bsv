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

import Fabric_Defs     :: *;
import AXI4_Lite_Types :: *;

// ================================================================
// Local constants and types

// Module state
typedef enum {STATE_START,
	      STATE_READY
   } State
deriving (Bits, Eq, FShow);

// ================================================================
// Interface

interface UART_IFC;
   // Reset
   interface Server #(Bit #(0), Bit #(0))  server_reset;

   // set_addr_map should be called after this module's reset
   method Action set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);

   // Main Fabric Reqs/Rsps
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) slave;

   // To external console
   interface Get #(Bit #(8))  get_to_console;
   interface Put #(Bit #(8))  put_from_console;
endinterface

// ================================================================
// Definitions of various bits in UART regs
// This is not a complete def for all the regs, just those actually used in this model

typedef enum {LCR_WLS5 = 8'h00,
	      LCR_WLS6 = 8'h01,
	      LCR_WLS7 = 8'h02,
	      LCR_WLS8 = 8'h03,
	      LCR_STB  = 8'h04,
	      LCR_PEN  = 8'h08,
	      LCR_EPS  = 8'h10,
	      LCR_SP   = 8'h20,
	      LCR_BC   = 8'h40,
	      LCR_DLAB = 8'h80
   }  LCR_T
deriving (Eq, Bits, FShow);

typedef enum {LSR_DR   = 8'h01,
	      LSR_OE   = 8'h02,
	      LSR_PE   = 8'h04,
	      LSR_FE   = 8'h08,
	      LSR_BI   = 8'h10,
	      LSR_THRE = 8'h20,
	      LSR_TEMT = 8'h40,
	      LSR_RXFE = 8'h80
   } LSR_T
deriving (Eq, Bits, FShow);

// ================================================================

(* synthesize *)
module mkUART (UART_IFC);

   Reg #(Bit #(8)) cfg_verbosity <- mkConfigReg (0);

   Reg #(State)       rg_state     <- mkReg (STATE_START);
   Reg #(Fabric_Addr) rg_addr_base <- mkRegU;
   Reg #(Fabric_Addr) rg_addr_lim  <- mkRegU;

   FIFOF #(Bit #(0)) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_reset_rsps <- mkFIFOF;

   // ----------------
   // Connector to fabric

   AXI4_Lite_Slave_Xactor_IFC #(Wd_Addr, Wd_Data, Wd_User) slave_xactor <- mkAXI4_Lite_Slave_Xactor;

   // ----------------

   FIFOF #(Bit #(8)) f_from_console <- mkFIFOF;
   FIFOF #(Bit #(8)) f_to_console   <- mkFIFOF;

   // These are the 16550 UART registers
   // A separate rg_rbr is not necessary as we are driving RX Data directly from
   // f_from_console.first
   Reg #(Bit #(8))  rg_thr <- mkRegU;    // 0x00    a.k.a. txd
   Reg #(Bit #(8))  rg_dll <- mkRegU;    // 0x00

   Reg #(Bit #(8))  rg_ier <- mkRegU;    // 0x04    a.k.a. dlm
   Reg #(Bit #(8))  rg_dlh <- mkRegU;    // 0x04

   Reg #(Bit #(8))  rg_iir <- mkRegU;    // 0x08
   Reg #(Bit #(8))  rg_fcr <- mkRegU;    // 0x08

   Reg #(Bit #(8))  rg_lcr <- mkRegU;    // 0x0c
   Reg #(Bit #(8))  rg_mcr <- mkRegU;    // 0x10
   // Transmit is always ready. DR field is set when there is data in the
   // f_from_console. All other fields are unused. As no status bits are being
   // set (except DR), do not need a complete register here. Not removing in
   // order to leave scope for extending functionality. 
   Reg #(Bit #(8))  rg_lsr <- mkReg (8'b01100000);    // 0x14
   Reg #(Bit #(8))  rg_msr <- mkRegU;    // 0x18
   Reg #(Bit #(8))  rg_scr <- mkRegU;    // 0x1c

   // ================================================================
   // BEHAVIOR

   // ----------------------------------------------------------------
   // Reset

   rule rl_reset;
      f_reset_reqs.deq;
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

      let byte_addr  = rda.araddr - rg_addr_base;

      Fabric_Data    rdata = 0;
      AXI4_Lite_Resp rresp = AXI4_LITE_OKAY;

      if (byte_addr [2:0] != 0) begin
	 $display ("%0d: ERROR: UART.rl_process_rd_req: misaligned addr", cur_cycle);
	 $display ("            ", fshow (rda));
	 rresp = AXI4_LITE_SLVERR;
      end
      else
      case (byte_addr [7:3])
         5'h00: begin
		   // Drive RX_DATA from the first entry of f_from_console
		   let ch = f_from_console.first;
		   f_from_console.deq;
		   rdata = {0, ch};
		end
	 5'h01: rdata = {0, rg_ier};
	 5'h02: rdata = {0, rg_iir};
	 5'h03: rdata = {0, rg_lcr};
	 5'h04: rdata = {0, rg_mcr};
	 5'h05: begin
		   // Transmit is always ready (from reset).
		   //  Receive is ready if f_from_console is not empty.
		   let lsr = (f_from_console.notEmpty
			      ? (rg_lsr | pack (LSR_DR))
			      : rg_lsr);
		   rdata = {0, lsr};
		end
	 5'h06: rdata = {0, rg_msr};
	 5'h07: rdata = {0, rg_scr};
	 default: begin
		     $display ("%0d: ERROR: UART.rl_process_rd_req: unrecognized addr", cur_cycle);
		     $display ("            ", fshow (rda));
		     rresp = AXI4_LITE_SLVERR;
		  end
      endcase

      let rdr = AXI4_Lite_Rd_Data {rresp: rresp, rdata: rdata, ruser: rda.aruser};
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

      Bit #(64) wdata = zeroExtend (wrd.wdata);
      Bit #(8)  wstrb = zeroExtend (wrd.wstrb);

      let byte_addr  = wra.awaddr - rg_addr_base;

      AXI4_Lite_Resp bresp = AXI4_LITE_OKAY;

      if ((byte_addr [2:0] != 0) || (wstrb[0] != 1'b1))  begin
	 $display ("%0d: ERROR: UART.rl_process_wr_req: misaligned addr", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 bresp = AXI4_LITE_SLVERR;
      end
      else
      case (byte_addr [7:3])
         5'h00: if ((rg_lcr & pack (LCR_DLAB)) == 0) begin
		   Bit #(8) ch = wdata [7:0];
		   rg_thr <= ch;
		   f_to_console.enq (ch);
		end
	 5'h01: rg_ier <= wdata [7:0];
	 5'h02: rg_iir <= wdata [7:0];
	 5'h03: rg_lcr <= wdata [7:0];
	 5'h04: rg_mcr <= wdata [7:0];
	 5'h05: rg_lsr <= wdata [7:0];
	 5'h06: rg_msr <= wdata [7:0];
	 5'h07: rg_scr <= wdata [7:0];
	 default: begin
		     $display ("%0d: ERROR: UART.rl_process_wr_req: unrecognized addr", cur_cycle);
		     $display ("            ", fshow (wra));
		     $display ("            ", fshow (wrd));
		     bresp = AXI4_LITE_SLVERR;
		  end
      endcase

      let wrr = AXI4_Lite_Wr_Resp {bresp: bresp, buser: wra.awuser};
      slave_xactor.i_wr_resp.enq (wrr);

      if (cfg_verbosity > 1) begin
	 $display ("%0d: UART.rl_process_wr_req", cur_cycle);
	 $display ("            ", fshow (wra));
	 $display ("            ", fshow (wrd));
	 $display ("            ", fshow (wrr));
      end
   endrule

   // ================================================================
   // INTERFACE

   // Reset
   interface server_reset   = toGPServer (f_reset_reqs, f_reset_rsps);

   // set_addr_map should be called after this module's reset
   method Action  set_addr_map (Fabric_Addr addr_base, Fabric_Addr addr_lim);
      if (addr_base [1:0] != 0)
	 $display ("%0d: WARNING: UART.set_addr_map: addr_base 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_base);

      if (addr_lim [1:0] != 0)
	 $display ("%0d: WARNING: UART.set_addr_map: addr_lim 0x%0h is not 4-Byte-aligned",
		   cur_cycle, addr_lim);

      rg_addr_base <= addr_base;
      rg_addr_lim  <= addr_lim;
   endmethod

   // Main Fabric Reqs/Rsps
   interface  slave = slave_xactor.axi_side;

   // To external console
   interface  put_from_console = toPut (f_from_console);
   interface  get_to_console   = toGet (f_to_console);
endmodule

// ================================================================

endpackage
