// Copyright (c) 2022 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package AXI4L_Clock_Crossers;

// ================================================================
// This package defines clock-domain-crossing modules for AXI4L M and S
// interfaces:

//   Clock -> Clock -> AXI4L_M_IFC -> AXI4L_M_IFC
export mkAXI4L_M_Clock_Crosser;

//   Clock -> Clock -> AXI4L_S_IFC -> AXI4L_S_IFC
export mkAXI4L_S_Clock_Crosser;

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

import AXI4_Lite_Types :: *;
import AXI_SyncBuffer  :: *;

// ================================================================
// Clock1 -> Clock2 -> AXI4L_S_IFC_1 -> AXI4L_S_IFC_2
// Clock1 should be same clock as axi4_S_1
// Clock2 is clock of result axi4_S_2

module mkAXI4L_S_Clock_Crosser
   #(Integer depth,
     Clock clk1, Reset rst1,
     Clock clk2, Reset rst2,
     AXI4_Lite_Slave_IFC #(wd_addr, wd_data, wd_user) axi4L_S_1)
   (AXI4_Lite_Slave_IFC #(wd_addr, wd_data, wd_user));

   // Transactor towards ifc1
   AXI4_Lite_Master_Xactor_IFC #(wd_addr, wd_data, wd_user)
   axi4L_M_xactor <- mkAXI4_Lite_Master_Xactor (clocked_by clk1, reset_by rst1);

   // Syncbuffer between transactors
   AXI_SyncBuffer_IFC #(AXI4_Lite_Wr_Addr #(wd_addr, wd_user),
			AXI4_Lite_Wr_Data #(wd_data),
			AXI4_Lite_Wr_Resp #(wd_user),
			AXI4_Lite_Rd_Addr #(wd_addr, wd_user),
			AXI4_Lite_Rd_Data #(wd_data, wd_user))
   axi4L_syncbuf <- mkAXI_SyncBuffer (depth, clk2, rst2, clk1, rst1);

   // Transactor with ifc2
   AXI4_Lite_Slave_Xactor_IFC #(wd_addr, wd_data, wd_user)
   axi4L_S_xactor <- mkAXI4_Lite_Slave_Xactor (clocked_by clk2, reset_by rst2);

   // ----------------

   mkConnection (axi4L_M_xactor.axi_side, axi4L_S_1);

   mkConnection (axi4L_M_xactor.i_wr_addr, axi4L_syncbuf.to_S.o_aw);
   mkConnection (axi4L_M_xactor.i_wr_data, axi4L_syncbuf.to_S.o_w);
   mkConnection (axi4L_M_xactor.o_wr_resp, axi4L_syncbuf.to_S.i_b);
   mkConnection (axi4L_M_xactor.i_rd_addr, axi4L_syncbuf.to_S.o_ar);
   mkConnection (axi4L_M_xactor.o_rd_data, axi4L_syncbuf.to_S.i_r);

   mkConnection (axi4L_syncbuf.from_M.i_aw, axi4L_S_xactor.o_wr_addr);
   mkConnection (axi4L_syncbuf.from_M.i_w,  axi4L_S_xactor.o_wr_data);
   mkConnection (axi4L_syncbuf.from_M.o_b,  axi4L_S_xactor.i_wr_resp);
   mkConnection (axi4L_syncbuf.from_M.i_ar, axi4L_S_xactor.o_rd_addr);
   mkConnection (axi4L_syncbuf.from_M.o_r,  axi4L_S_xactor.i_rd_data);

   // ----------------
   // INTERFACE

   let axi4_S_2 = axi4L_S_xactor.axi_side;
   return axi4_S_2;
endmodule

// ================================================================
// Clock1 -> Clock2 -> AXI4L_M_IFC_1 -> AXI4L_M_IFC_2
// Clock1 should be same clock as axi4L_M_1
// Clock2 is clock of result axi4L_M_2

module mkAXI4L_M_Clock_Crosser
   #(Integer depth,
     Clock clk1, Reset rst1,
     Clock clk2, Reset rst2,
     AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user) axi4L_M_1)
   (AXI4_Lite_Master_IFC #(wd_addr, wd_data, wd_user));

   // Transactor towards ifc1
   AXI4_Lite_Slave_Xactor_IFC #(wd_addr, wd_data, wd_user)
   axi4L_S_xactor <- mkAXI4_Lite_Slave_Xactor (clocked_by clk1, reset_by rst1);

   // Syncbuffer between transactors
   AXI_SyncBuffer_IFC #(AXI4_Lite_Wr_Addr #(wd_addr, wd_user),
			AXI4_Lite_Wr_Data #(wd_data),
			AXI4_Lite_Wr_Resp #(wd_user),
			AXI4_Lite_Rd_Addr #(wd_addr, wd_user),
			AXI4_Lite_Rd_Data #(wd_data, wd_user))
   axi4L_syncbuf <- mkAXI_SyncBuffer (depth, clk1, rst1, clk2, rst2);

   // Transactor with ifc2
   AXI4_Lite_Master_Xactor_IFC #(wd_addr, wd_data, wd_user)
   axi4L_M_xactor <- mkAXI4_Lite_Master_Xactor (clocked_by clk2, reset_by rst2);

   // ----------------

   mkConnection (axi4L_M_1, axi4L_S_xactor.axi_side);

   mkConnection (axi4L_S_xactor.o_wr_addr, axi4L_syncbuf.from_M.i_aw);
   mkConnection (axi4L_S_xactor.o_wr_data, axi4L_syncbuf.from_M.i_w);
   mkConnection (axi4L_S_xactor.i_wr_resp, axi4L_syncbuf.from_M.o_b);
   mkConnection (axi4L_S_xactor.o_rd_addr, axi4L_syncbuf.from_M.i_ar);
   mkConnection (axi4L_S_xactor.i_rd_data, axi4L_syncbuf.from_M.o_r);

   mkConnection (axi4L_syncbuf.to_S.o_aw, axi4L_M_xactor.i_wr_addr);
   mkConnection (axi4L_syncbuf.to_S.o_w,  axi4L_M_xactor.i_wr_data);
   mkConnection (axi4L_syncbuf.to_S.i_b,  axi4L_M_xactor.o_wr_resp);
   mkConnection (axi4L_syncbuf.to_S.o_ar, axi4L_M_xactor.i_rd_addr);
   mkConnection (axi4L_syncbuf.to_S.i_r,  axi4L_M_xactor.o_rd_data);

   // ----------------
   // INTERFACE

   let axi4L_M_2 = axi4L_M_xactor.axi_side;
   return axi4L_M_2;
endmodule

// ================================================================

endpackage
