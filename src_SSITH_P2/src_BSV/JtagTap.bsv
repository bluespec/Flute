
package JtagTap;

import BUtils ::*;
import Clocks ::*;
import DefaultValue ::*;
import FIFOF ::*;
import FIFOLevel ::*;
import Reserved ::*;

import Giraffe_IFC ::*;

typedef 6 ABITS;
`ifdef XILINX_BSCAN
`ifdef XILINX_XCVU095
typedef 6 IR_LENGTH;
`elsif XILINX_XCVU9P
typedef 18 IR_LENGTH;
`endif
`else
typedef 5 IR_LENGTH;
`endif
typedef TAdd#(ABITS,34) DR_LENGTH;
Bit#(IR_LENGTH) irmask = zExtendLSB(1'b1);
Bit#(IR_LENGTH) ir_bypass0 = 0;
Bit#(IR_LENGTH) ir_idcode = 1;
`ifdef XILINX_BSCAN
`ifdef XILINX_XCVU095
Bit#(IR_LENGTH) ir_dtmcs = 'h02;
Bit#(IR_LENGTH) ir_dmi = 'h03;
`elsif XILINX_XCVU9P

Bit#(IR_LENGTH) ir_dtmcs = 'b100010100100100100;    // USER3
                        // 'b000010100100100100;    USER1

Bit#(IR_LENGTH) ir_dmi = 'b000011100100100100;
`endif
`else
Bit#(IR_LENGTH) ir_dtmcs = 'h10;
Bit#(IR_LENGTH) ir_dmi = 'h11;
`endif
Bit#(IR_LENGTH) ir_reserved12 = 'h12;
Bit#(IR_LENGTH) ir_reserved13 = 'h13;
Bit#(IR_LENGTH) ir_reserved14 = 'h14;
Bit#(IR_LENGTH) ir_reserved15 = 'h15;
Bit#(IR_LENGTH) ir_reserved16 = 'h16;
Bit#(IR_LENGTH) ir_reserved17 = 'h17;
Bit#(IR_LENGTH) ir_bypass1f = '1;
Bit#(32) idcode = 'h00000ffd;
Bit#(DR_LENGTH) drmask_bypass = 1;
Bit#(DR_LENGTH) drmask_idcode = 1<<31;
Bit#(DR_LENGTH) drmask_dtmcs = 1<<31;
Bit#(DR_LENGTH) drmask_dmi = zExtendLSB(1'b1);

typedef enum {
   TEST_LOGIC_RESET,
   RUN_TEST_IDLE,
   SELECT_DR_SCAN,
   CAPTURE_DR,
   SHIFT_DR,
   EXIT1_DR,
   PAUSE_DR,
   EXIT2_DR,
   UPDATE_DR,
   SELECT_IR_SCAN,
   CAPTURE_IR,
   SHIFT_IR,
   EXIT1_IR,
   PAUSE_IR,
   EXIT2_IR,
   UPDATE_IR
   } JtagState deriving (Bits, Eq, FShow);

typedef struct {
   ReservedZero#(14) reserved;
   Bit#(1) dmihardreset;
   Bit#(1) dmireset;
   ReservedZero#(1) reserved2;
   Bit#(3) idle;
   Bit#(2) dmistat;
   Bit#(6) abits;
   Bit#(4) version;
   } DTMCS deriving (Bits, Eq, FShow);

instance DefaultValue#(DTMCS);
   defaultValue = DTMCS {
      reserved: ?,
      dmihardreset: 0,
      dmireset: 0,
      reserved2: ?,
      idle: 0,
      dmistat: 0,
      abits: fromInteger(valueOf(ABITS)),
      version: 1
      };
endinstance

// This would be an enum, but bsv doens't allow more than one tag for
// a particular value.
typedef Bit#(2) DMI_OP;
DMI_OP opNOP = 0;
DMI_OP opREAD = 1;
DMI_OP opWRITE = 2;
DMI_OP opRESERVED = 3;
DMI_OP opSUCCESS = 0;
DMI_OP opFAILED = 2;
DMI_OP opBUSY = 3;

typedef struct {
   Bit#(6) address;
   Bit#(32) data;
   DMI_OP op;
   } DMI deriving (Bits, Eq, FShow);

instance DefaultValue#(DMI);
   defaultValue = unpack(0);
endinstance

function Bool f_dmi_stuck(DMI dmi);
   case (dmi.op)
      opFAILED, opBUSY: return True;
      default: return False;
   endcase
endfunction

function JtagState f_next_state(JtagState state, Bit#(1) tms);
   if (tms == 0)
      case (state)
	 TEST_LOGIC_RESET: return RUN_TEST_IDLE;
	 RUN_TEST_IDLE: return RUN_TEST_IDLE;
	 SELECT_DR_SCAN: return CAPTURE_DR;
	 CAPTURE_DR: return SHIFT_DR;
	 SHIFT_DR: return SHIFT_DR;
	 EXIT1_DR: return PAUSE_DR;
	 PAUSE_DR: return PAUSE_DR;
	 EXIT2_DR: return SHIFT_DR;
	 UPDATE_DR: return RUN_TEST_IDLE;
	 SELECT_IR_SCAN: return CAPTURE_IR;
	 CAPTURE_IR: return SHIFT_IR;
	 SHIFT_IR: return SHIFT_IR;
	 EXIT1_IR: return PAUSE_IR;
	 PAUSE_IR: return PAUSE_IR;
	 EXIT2_IR: return SHIFT_IR;
	 UPDATE_IR: return RUN_TEST_IDLE;
      endcase
   else
      case (state)
	 TEST_LOGIC_RESET: return TEST_LOGIC_RESET;
	 RUN_TEST_IDLE: return SELECT_DR_SCAN;
	 SELECT_DR_SCAN: return SELECT_IR_SCAN;
	 CAPTURE_DR: return EXIT1_DR;
	 SHIFT_DR: return EXIT1_DR;
	 EXIT1_DR: return UPDATE_DR;
	 PAUSE_DR: return EXIT2_DR;
	 EXIT2_DR: return UPDATE_DR;
	 UPDATE_DR: return SELECT_DR_SCAN;
	 SELECT_IR_SCAN: return TEST_LOGIC_RESET;
	 CAPTURE_IR: return EXIT1_IR;
	 SHIFT_IR: return EXIT1_IR;
	 EXIT1_IR: return UPDATE_IR;
	 PAUSE_IR: return EXIT2_IR;
	 EXIT2_IR: return UPDATE_IR;
	 UPDATE_IR: return SELECT_DR_SCAN;
      endcase
endfunction

interface JtagTap_IFC;
   interface JTAG_IFC jtag;
   interface DMI_Master_IFC dmi;
endinterface

(* synthesize *)
module mkJtagTap(JtagTap_IFC);
   let clk <- exposeCurrentClock;
   let rst <- exposeCurrentReset;

   Wire#(Bit#(1)) w_tck <- mkDWire(?);

   let tck_clock <- mkUngatedClock(?);
   let tck = tck_clock.new_clk;

   (* no_implicit_conditions, fire_when_enabled *)
   rule rl_tck;
      tck_clock.setClockValue(w_tck);
   endrule

   let rst_tck <- mkAsyncResetFromCR(4, tck);

   Wire#(Bit#(1)) w_tms <- mkDWire(?, clocked_by tck, reset_by rst_tck);
   Wire#(Bit#(1)) w_tdi <- mkDWire(?, clocked_by tck, reset_by rst_tck);

   Wire#(Bit#(1)) w_req_ready <- mkWire;
   Wire#(Bit#(1)) w_req_valid <- mkDWire(0);
   Wire#(Bit#(7)) w_req_addr <- mkDWire(?);
   Wire#(Bit#(32)) w_req_data <- mkDWire(?);
   Wire#(Bit#(2)) w_req_op <- mkDWire(?);
   Wire#(Bit#(1)) w_rsp_ready <- mkDWire(0);
   Wire#(Bit#(1)) w_rsp_valid <- mkWire;
   Wire#(Bit#(32)) w_rsp_data <- mkDWire(?);
   Wire#(Bit#(2)) w_rsp_response <- mkDWire(?);

   Array#(Reg#(Bit#(1))) r_tdo <- mkCRegU(2, clocked_by tck, reset_by rst_tck);

   Reg#(JtagState) r_state <- mkReg(TEST_LOGIC_RESET, clocked_by tck, reset_by rst_tck);

   Reg#(Bit#(IR_LENGTH)) r_ir <- mkRegU(clocked_by tck, reset_by rst_tck);
   Reg#(Bit#(DR_LENGTH)) r_dr <- mkRegU(clocked_by tck, reset_by rst_tck);
   Reg#(Bit#(DR_LENGTH)) r_drmask <- mkRegU(clocked_by tck, reset_by rst_tck);

   Reg#(DMI) r_dmi <- mkReg(defaultValue, clocked_by tck, reset_by rst_tck);

   Wire#(Tuple3#(Bit#(7),Bit#(32),Bit#(2))) w_dmi_req <- mkWire(clocked_by tck, reset_by rst_tck);
   SyncFIFOLevelIfc#(Tuple3#(Bit#(7),Bit#(32),Bit#(2)), 2) f_dmi_req <- mkSyncFIFOLevel(tck, rst_tck, clk);
   FIFOF#(void) f_dmi_busy <- mkFIFOF(clocked_by tck, reset_by rst_tck);
   Wire#(Bool) w_dmi_reset <- mkWire(clocked_by tck, reset_by rst_tck);
   Array#(Reg#(Bool)) r_dmistat_busy <- mkCReg(2, False, clocked_by tck, reset_by rst_tck);
   SyncFIFOLevelIfc#(DMI, 2) f_dmi_rsp <- mkSyncFIFOLevel(clk, rst, tck);

   (* no_implicit_conditions, fire_when_enabled *)
   rule tick;
      let newir = r_ir;
      let newdr = r_dr;
      let newdrmask = r_drmask;

      if (r_state == TEST_LOGIC_RESET) begin
	 newir = ir_idcode;
      end
      else if (r_state == CAPTURE_DR) begin
	 if (newir == ir_bypass0 ||
	     newir == ir_reserved12 ||
	     newir == ir_reserved13 ||
	     newir == ir_reserved14 ||
	     newir == ir_reserved15 ||
	     newir == ir_reserved16 ||
	     newir == ir_reserved17 ||
	     newir == ir_bypass1f) begin
            newdr = 0;
	 end
	 else if (newir == ir_idcode) begin
	    newdr = cExtend(idcode);
	    //newdrmask = drmask_idcode;
	 end
	 else if (newir == ir_dtmcs) begin
	    DTMCS dtmcs = defaultValue;
	    dtmcs.dmistat = cExtend(r_dmi.op);
	    newdr = cExtend(dtmcs);
	    //newdrmask = drmask_dtmcs;
	 end
	 else if (newir == ir_dmi) begin
	    if (r_dmistat_busy[0] || (!f_dmi_stuck(r_dmi) && f_dmi_busy.notEmpty))
	       newdr = cExtend(DMI { address: ?, data: ?, op: opBUSY });
	    else
	       newdr = cExtend(r_dmi);

	    //newdrmask = drmask_dmi;
	 end
	 else begin
	    $display("WARNING: unsupported IR: 'h%x\n", newir);
	    newdr = cExtend(newir) | 'hf << 24;
	 end
      end
      else if (r_state == SHIFT_DR) begin
	 r_tdo[0] <= truncate(newdr);
	 newdr = (newdr >> 1) | (w_tdi == 1 ? r_drmask : 0);
      end
      else if (r_state == UPDATE_DR) begin
	 if (newir == ir_dtmcs) begin
	    /// \todo is this what the debug spec means by "write dtmcs"?
	    DTMCS dtmcs = cExtend(newdr);
	    if (dtmcs.dmihardreset == 1)
	       w_dmi_reset <= True;
	    else if (dtmcs.dmireset == 1)
	       w_dmi_reset <= False;
	 end
	 else if (newir == ir_dmi) begin
	    if (f_dmi_stuck(r_dmi)) begin
	       // drop request
	       noAction;
	    end
	    else if (f_dmi_busy.notEmpty) begin
	       // busy
	       r_dmistat_busy[0] <= True;
	    end
	    else begin
	       // issue request
	       w_dmi_req <= cExtend(newdr);
	    end
	 end
      end
      else if (r_state == SHIFT_IR) begin
	 r_tdo[0] <= truncate(newir);
	 newir = (newir >> 1) | (w_tdi == 1 ? irmask : 0);
      end

      // bsc complains if this is written above in the more logical way
      if (r_state == CAPTURE_DR) begin
	 if (newir == ir_bypass0 ||
	     newir == ir_reserved12 ||
	     newir == ir_reserved13 ||
	     newir == ir_reserved14 ||
	     newir == ir_reserved15 ||
	     newir == ir_reserved16 ||
	     newir == ir_reserved17 ||
	     newir == ir_bypass1f)
	    newdrmask = drmask_bypass;
	 else if (newir == ir_idcode)
	    newdrmask = drmask_idcode;
	 else if (newir == ir_dtmcs)
	    newdrmask = drmask_dtmcs;
	 else if (newir == ir_dmi)
	    newdrmask = drmask_dmi;
	 else
	    newdrmask = 1<<32;
      end

      r_state <= f_next_state(r_state, w_tms);
      r_ir <= newir;
      r_dr <= newdr;
      r_drmask <= newdrmask;
   endrule

   (* fire_when_enabled *)
   rule dmi_reset;
      let tmp = w_dmi_reset;
      if (tmp) begin
	 f_dmi_req.sClear;
	 f_dmi_rsp.dClear;
	 f_dmi_busy.clear;
      end
      r_dmistat_busy[1] <= False;
   endrule

   (* fire_when_enabled *)
   rule dmi_start(tpl_3(w_dmi_req) != opNOP);
      f_dmi_req.enq(w_dmi_req);
      f_dmi_busy.enq(?);
   endrule

   rule dmi_request;
      match {.addr, .data, .op} = f_dmi_req.first;
      w_req_addr <= addr;
      w_req_data <= data;
      w_req_op <= op;
   endrule

   rule dmi_request_valid;
      w_req_valid <= f_dmi_req.dNotEmpty ? 1 : 0;
   endrule

   rule dmi_request_deq(w_req_ready == 1 && w_req_valid == 1);
      f_dmi_req.deq;
   endrule

   rule dmi_response(w_rsp_valid == 1 && w_rsp_ready == 1);
      let newdmi = DMI {
	 address: ?,
	 data: w_rsp_data,
	 op: w_rsp_response
	 };

      f_dmi_rsp.enq(newdmi);
   endrule

   rule dmi_response_tck;
      let newdmi = f_dmi_rsp.first;
      f_dmi_rsp.deq;

      if (!f_dmi_stuck(r_dmi))
	 r_dmi <= newdmi;

      f_dmi_busy.deq;
   endrule

   rule dmi_response_ready;
      w_rsp_ready <= f_dmi_rsp.sNotFull ? 1 : 0;
   endrule

   interface JTAG_IFC jtag;
      method tclk = w_tck._write;
      method tms = w_tms._write;
      method tdi = w_tdi._write;
      method tdo = r_tdo[1];
      interface tclk_out = tck;
   endinterface

   interface DMI_Master_IFC dmi;
      method req_ready = w_req_ready._write;
      method req_valid = w_req_valid;
      method req_addr = w_req_addr;
      method req_data = w_req_data;
      method req_op = w_req_op;
      method rsp_ready = w_rsp_ready;
      method rsp_valid = w_rsp_valid._write;
      method rsp_data = w_rsp_data._write;
      method rsp_response = w_rsp_response._write;
   endinterface

endmodule

endpackage
