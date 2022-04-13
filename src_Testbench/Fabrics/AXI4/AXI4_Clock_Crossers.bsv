// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil
// (original code for mkAXI4_Clock_Crossing from Joe Stoy)

package AXI4_Clock_Crossers;

// ================================================================
// This package defines clock-domain-crossing modules for AXI4 M and S
// interfaces:

//   Clock -> Clock -> AXI4_M_IFC -> Module #(AXI4_M_IFC)
export mkAXI4_M_Clock_Crosser;

//   Clock -> Clock -> AXI4_S_IFC -> Module #(AXI4_S_IFC)
export mkAXI4_S_Clock_Crosser;

export AXI4_ClockCrossing_IFC (..);

//   Clock1 -> Clock2 -> Module #(AXI4_Clock_Crossing_IFC)
export mkAXI4_ClockCrossing;

//   Clock1 ->           Module #(AXI4_Clock_Crossing_IFC)
export mkAXI4_ClockCrossingToCC;

// ================================================================
// Bluespec library imports

import Clocks      :: *;
import Connectable :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import AXI4_Types     :: *;
import AXI_SyncBuffer :: *;
import AXI4_Xactors   :: *;

// ================================================================
// Clock1 -> Clock2 -> AXI4_S_IFC_1 -> Module #(AXI4_S_IFC_2)
// Clock1 should be same clock as axi4_S_1
// Clock2 is clock of result axi4_S_2

module mkAXI4_S_Clock_Crosser
   #(Integer depth,
     Clock clk1, Reset rst1,
     Clock clk2, Reset rst2,
     AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user) axi4_S_1)
   (AXI4_Slave_IFC #(wd_id, wd_addr, wd_data, wd_user));

   // Transactor towards ifc1
   AXI4_Master_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user)
   axi4_M_xactor <- mkAXI4_Master_Xactor (clocked_by clk1, reset_by rst1);

   // Syncbuffer between transactors
   AXI_SyncBuffer_IFC #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user),
			AXI4_Wr_Data #(wd_data, wd_user),
			AXI4_Wr_Resp #(wd_id, wd_user),
			AXI4_Rd_Addr #(wd_id, wd_addr, wd_user),
			AXI4_Rd_Data #(wd_id, wd_data, wd_user))
   axi4_syncbuf <- mkAXI_SyncBuffer (depth, clk2, rst2, clk1, rst1);

   // Transactor with ifc2
   AXI4_Slave_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user)
   axi4_S_xactor <- mkAXI4_Slave_Xactor (clocked_by clk2, reset_by rst2);

   // ----------------

   mkConnection (axi4_M_xactor.axi_side, axi4_S_1);

   mkConnection (axi4_M_xactor.i_wr_addr, axi4_syncbuf.to_S.o_aw);
   mkConnection (axi4_M_xactor.i_wr_data, axi4_syncbuf.to_S.o_w);
   mkConnection (axi4_M_xactor.o_wr_resp, axi4_syncbuf.to_S.i_b);
   mkConnection (axi4_M_xactor.i_rd_addr, axi4_syncbuf.to_S.o_ar);
   mkConnection (axi4_M_xactor.o_rd_data, axi4_syncbuf.to_S.i_r);

   mkConnection (axi4_syncbuf.from_M.i_aw, axi4_S_xactor.o_wr_addr);
   mkConnection (axi4_syncbuf.from_M.i_w,  axi4_S_xactor.o_wr_data);
   mkConnection (axi4_syncbuf.from_M.o_b,  axi4_S_xactor.i_wr_resp);
   mkConnection (axi4_syncbuf.from_M.i_ar, axi4_S_xactor.o_rd_addr);
   mkConnection (axi4_syncbuf.from_M.o_r,  axi4_S_xactor.i_rd_data);

   // ----------------
   // INTERFACE

   let axi4_S_2 = axi4_S_xactor.axi_side;
   return axi4_S_2;
endmodule

// ================================================================
// Clock1 -> Clock2 -> AXI4_M_IFC_1 -> Module #(AXI4_M_IFC_2)
// Clock1 should be same clock as axi4_M_1
// Clock2 is clock of result axi4_M_2

module mkAXI4_M_Clock_Crosser
   #(Integer depth,
     Clock clk1, Reset rst1,
     Clock clk2, Reset rst2,
     AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user) axi4_M_1)
   (AXI4_Master_IFC #(wd_id, wd_addr, wd_data, wd_user));

   // Transactor towards ifc1
   AXI4_Slave_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user)
   axi4_S_xactor <- mkAXI4_Slave_Xactor (clocked_by clk1, reset_by rst1);

   // Syncbuffer between transactors
   AXI_SyncBuffer_IFC #(AXI4_Wr_Addr #(wd_id, wd_addr, wd_user),
			AXI4_Wr_Data #(wd_data, wd_user),
			AXI4_Wr_Resp #(wd_id, wd_user),
			AXI4_Rd_Addr #(wd_id, wd_addr, wd_user),
			AXI4_Rd_Data #(wd_id, wd_data, wd_user))
   axi4_syncbuf <- mkAXI_SyncBuffer (depth, clk1, rst1, clk2, rst2);

   // Transactor with ifc2
   AXI4_Master_Xactor_IFC #(wd_id, wd_addr, wd_data, wd_user)
   axi4_M_xactor <- mkAXI4_Master_Xactor (clocked_by clk2, reset_by rst2);

   // ----------------

   mkConnection (axi4_M_1, axi4_S_xactor.axi_side);

   mkConnection (axi4_S_xactor.o_wr_addr, axi4_syncbuf.from_M.i_aw);
   mkConnection (axi4_S_xactor.o_wr_data, axi4_syncbuf.from_M.i_w);
   mkConnection (axi4_S_xactor.i_wr_resp, axi4_syncbuf.from_M.o_b);
   mkConnection (axi4_S_xactor.o_rd_addr, axi4_syncbuf.from_M.i_ar);
   mkConnection (axi4_S_xactor.i_rd_data, axi4_syncbuf.from_M.o_r);

   mkConnection (axi4_syncbuf.to_S.o_aw, axi4_M_xactor.i_wr_addr);
   mkConnection (axi4_syncbuf.to_S.o_w,  axi4_M_xactor.i_wr_data);
   mkConnection (axi4_syncbuf.to_S.i_b,  axi4_M_xactor.o_wr_resp);
   mkConnection (axi4_syncbuf.to_S.o_ar, axi4_M_xactor.i_rd_addr);
   mkConnection (axi4_syncbuf.to_S.i_r,  axi4_M_xactor.o_rd_data);

   // ----------------
   // INTERFACE

   let axi4_M_2 = axi4_M_xactor.axi_side;
   return axi4_M_2;
endmodule

// ================================================================
// Clock1 -> Clock2 -> Module #(AXI4_Clock_Crossing_IFC)

// ----------------

interface AXI4_ClockCrossing_IFC #(numeric type id_,
				   numeric type addr_,
				   numeric type data_,
				   numeric type user_);
   interface AXI4_Slave_IFC  #(id_, addr_, data_, user_) from_M;
   interface AXI4_Master_IFC #(id_, addr_, data_, user_) to_S;
endinterface

// ----------------

module mkAXI4_ClockCrossing
   #(Clock clock_M, Reset reset_M,
     Clock clock_S, Reset reset_S)
   (AXI4_ClockCrossing_IFC #(id_, addr_, data_, user_));

   // SyncFIFOs for the 5 channels

   SyncFIFOIfc #(AXI4_Wr_Addr #(id_, addr_, user_))
   f_aw <- mkSyncFIFO (4, clock_M, reset_M,  clock_S);

   SyncFIFOIfc #(AXI4_Wr_Data #(data_, user_))
   f_w  <- mkSyncFIFO (4, clock_M, reset_M,  clock_S);

   SyncFIFOIfc #(AXI4_Wr_Resp #(id_, user_))
   f_b  <- mkSyncFIFO (4,  clock_S,  reset_S, clock_M);

   SyncFIFOIfc #(AXI4_Rd_Addr #(id_, addr_, user_))
   f_ar <- mkSyncFIFO (4, clock_M, reset_M,  clock_S);

   SyncFIFOIfc #(AXI4_Rd_Data #(id_, data_, user_))
   f_r  <- mkSyncFIFO (4,  clock_S,  reset_S, clock_M);

   // Transactors for each end

   AXI4_Slave_IFC  #(id_, addr_, data_, user_)
   xactor_S <- mkAXI4_Xactor_S_3 (f_aw, f_w, f_b, f_ar, f_r,
				  clocked_by clock_M,
				  reset_by   reset_M);
   AXI4_Master_IFC #(id_, addr_, data_, user_)
   xactor_M <- mkAXI4_Xactor_M_3 (f_aw, f_w, f_b, f_ar, f_r,
				  clocked_by clock_S,
				  reset_by reset_S);

   // ----------------
   interface AXI4_Slave_IFC  from_M =  xactor_S;
   interface AXI4_Master_IFC to_S   = xactor_M;
endmodule

// ----------------------------------------------------------------
// Same as above, with slave-side using current-clock

module mkAXI4_ClockCrossingToCC
   #(Clock clock_M, Reset reset_M)
   (AXI4_ClockCrossing_IFC #(id_, addr_, data_, user_));

   let clock_S <- exposeCurrentClock;
   let reset_S <- exposeCurrentReset;

   let crossing <- mkAXI4_ClockCrossing (clock_M, reset_M,
					 clock_S, reset_S);

   return crossing;
endmodule

// ================================================================

endpackage
