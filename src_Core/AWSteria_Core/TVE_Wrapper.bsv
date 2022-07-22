// Copyright (c) 2021-2022 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package TVE_Wrapper;

// ================================================================
// This package defines three alternate module implementations for the
// mkTV_Wrapper module
//     mkTVE_Wrapper1    when Debug Module present, TV Encoder present
//     mkTVE_Wrapper2    when Debug Module present, TV Encoder absent
//     mkTVE_Wrapper3    when Debug Module absent,  TV Encoder absent

// ================================================================
// BSV library imports

import Vector        :: *;
import FIFOF         :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ----------------
// AXI

import AXI4_Types   :: *;
import AXI4_Fabric  :: *;

// ================================================================
// Project imports

// Main fabric
import Fabric_Defs :: *;    // for Wd_{Id,Addr,Data,User}
import AXI_Widths  :: *;    // for Wd_{Id,Addr,Data,User}_Dma
import SoC_Map     :: *;

import ISA_Decls :: *;
import CPU_IFC   :: *;

import DM_Common      :: *;    // For Server_DMI
import Debug_Module   :: *;
import DM_CPU_Req_Rsp :: *;

import Dma_Server_Mux :: *;

import TV_Trace_Data :: *;
import TV_Info       :: *;
import TV_Encode     :: *;

// TV_Taps needed when both GDB_CONTROL and TANDEM_VERIF are present
import TV_Taps :: *;

// ================================================================
// Common parameter struct for all module variants

typedef struct {
   // CPU's TV out
   Get #(Trace_Data)                cpu_trace_data_out;

   // CPU's register debug interfaces
   Server #(DM_CPU_Req #(5,  XLEN),
	    DM_CPU_Rsp #(XLEN))     cpu_hart0_gpr_mem_server;
`ifdef ISA_F
   Server #(DM_CPU_Req #(5,  FLEN),
	    DM_CPU_Rsp #(FLEN))     cpu_hart0_fpr_mem_server;
`endif
   Server #(DM_CPU_Req #(12, XLEN),
	    DM_CPU_Rsp #(XLEN))     cpu_hart0_csr_mem_server;

   // CPU's DMA interface
   AXI4_Slave_IFC #(Wd_Id_Dma,
		    Wd_Addr_Dma,
		    Wd_Data_Dma,
		    Wd_User_Dma)    cpu_dma_server;
} TVE_Wrapper_Param;

// ================================================================
// Interface

interface TVE_Wrapper_IFC;
   // GPR mem server to Debug Module
   interface Server #(DM_CPU_Req #(5,  XLEN),
		      DM_CPU_Rsp #(XLEN))     hart0_gpr_mem_server;
`ifdef ISA_F
   // FPR mem server to Debug Module
   interface Server #(DM_CPU_Req #(5,  FLEN),
		      DM_CPU_Rsp #(FLEN))     hart0_fpr_mem_server;
`endif
   // CSR mem server to Debug Module
   interface Server #(DM_CPU_Req #(12, XLEN),
		      DM_CPU_Rsp #(XLEN))     hart0_csr_mem_server;

   // System Bus Access interface to Debug Module
   interface AXI4_Slave_IFC #(Wd_Id,
			      Wd_Addr,
			      Wd_Data,
			      Wd_User)        sba_S;

   // TV output
   interface FIFOF_O #(TV_Info)               fo_tv_info;

   // DMA interface
   interface AXI4_Slave_IFC #(Wd_Id_Dma,
			      Wd_Addr_Dma,
			      Wd_Data_Dma,
			      Wd_User_Dma)    dma_S;
endinterface

// ****************************************************************
// Top-level selection of one of the four modules that follow

module mkTVE_Wrapper #(TVE_Wrapper_Param param) (TVE_Wrapper_IFC);

   let tve_wrapper_module =
`ifdef INCLUDE_TANDEM_VERIF

  `ifdef INCLUDE_GDB_CONTROL
       mkTVE_Wrapper1;    // TV, DM
  `else
       mkTVE_Wrapper2;    // TV, no DM
  `endif

`else

  `ifdef INCLUDE_GDB_CONTROL
       mkTVE_Wrapper3;    // no TV, DM
  `else
       mkTVE_Wrapper4;    // no TV, no DM
  `endif

`endif

   TVE_Wrapper_IFC ifc <- tve_wrapper_module (param);
   return ifc;
endmodule

// ****************************************************************
// Variant: Debug Module present, Tandem Verification absent

module mkTVE_Wrapper1 #(TVE_Wrapper_Param param)
                       (TVE_Wrapper_IFC);

   // ----------------
   // Instantiate taps. Each 'tap' siphons off a record for the Tandem Verifier,
   // for each Debug-Module write to a CPU CSR, FPR, GPR, or memory.
   DM_GPR_Tap_IFC dm_gpr_tap <- mkDM_GPR_Tap;
`ifdef ISA_F
   DM_FPR_Tap_IFC dm_fpr_tap <- mkDM_FPR_Tap;
`endif
   DM_CSR_Tap_IFC dm_csr_tap <- mkDM_CSR_Tap;
   DM_Mem_Tap_IFC dm_mem_tap <- mkDM_Mem_Tap;

   // ----------------
   // Connect register taps to CPU GPR/FPR/CSR servers
   mkConnection (dm_gpr_tap.client, param.cpu_hart0_gpr_mem_server);
`ifdef ISA_F
   mkConnection (dm_fpr_tap.client, param.cpu_hart0_fpr_mem_server);
`endif
   mkConnection (dm_csr_tap.client, param.cpu_hart0_csr_mem_server);

   // ----------------
   // Instantiate mem dma mux
   Dma_Server_Mux_IFC  dma_server_mux <- mkDma_Server_Mux;

   // Connect mem dma mux to CPU
   mkConnection (dma_server_mux.target_client, param.cpu_dma_server);

   // Connect mem tap to mem dma mux
   mkConnection (dm_mem_tap.master, dma_server_mux.initiator_B_server);

   // ================================================================
   // TV Encoder and connections
   // TV Encoder transforms Trace_Data structures produced by the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier

   TV_Encode_IFC tv_encode <- mkTV_Encode;

   // FIFOF to merge TV data from CPU and from Debug Module
   FIFOF #(Trace_Data) f_trace_data_merged <- mkFIFOF;

   // Connect FIFOF to TV Encoder
   mkConnection (toGet (f_trace_data_merged), tv_encode.trace_data_in);

   // The following merges into f_trace_data_merged are equivalent to:
   //     mkConnection (..., toPut (f_trace_data_merged))
   // but using rules allows us to name them in scheduling attributes.

   // Merge-in CPU's TV data.
   rule merge_cpu_trace_data;
      let tmp <- param.cpu_trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

   rule merge_dm_gpr_trace_data;
      let tmp <- dm_gpr_tap.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

`ifdef ISA_F
   rule merge_dm_fpr_trace_data;
      let tmp <- dm_fpr_tap.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule
`endif

   rule merge_dm_csr_trace_data;
      let tmp <- dm_csr_tap.trace_data_out.get;
      f_trace_data_merged.enq(tmp);
   endrule

   // Merge-in memory-write TV data
   rule merge_dm_mem_trace_data;
      let tmp <- dm_mem_tap.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

   (* descending_urgency = "merge_dm_csr_trace_data, merge_dm_mem_trace_data" *)
`ifdef ISA_F
   (* descending_urgency = "merge_dm_fpr_trace_data, merge_dm_gpr_trace_data" *)
`endif
   (* descending_urgency = "merge_dm_gpr_trace_data, merge_dm_csr_trace_data" *)
   (* descending_urgency = "merge_dm_mem_trace_data, merge_cpu_trace_data"    *)
   rule rl_bogus_for_sched_attributes_only;
   endrule

   // Adapter to convert 2-tuple tv_encode output to TV_Info struct
   // Note: would not be necessary if TV Encoder directly produced this struct
   // TODO: change TV Encoder for this?
   FIFOF #(TV_Info) f_tv_info <- mkFIFOF;

   rule rl_tv_adapter;
      match { .n, .v } <- tv_encode.tv_vb_out.get;
      f_tv_info.enq (TV_Info { num_bytes: n, vec_bytes: v });
   endrule

   // ================================================================
   // INTERFACE

   // GPR mem server to Debug Module
   interface Server  hart0_gpr_mem_server = dm_gpr_tap.server;
`ifdef ISA_F
   // FPR mem server to Debug Module
   interface Server  hart0_fpr_mem_server = dm_fpr_tap.server;
`endif
   // CSR mem server to Debug Module
   interface Server  hart0_csr_mem_server = dm_csr_tap.server;

   // System Bus Access interface to Debug Module
   interface AXI4_Slave_IFC  sba_S        = dm_mem_tap.slave;

   // TV output
   interface FIFOF_O         fo_tv_info   = to_FIFOF_O (f_tv_info);
   // DMA server
   interface AXI4_Slave_IFC  dma_S        = dma_server_mux.initiator_A_server;
endmodule

// ****************************************************************
// Variant: Debug Module absent, Tandem Verification present

module mkTVE_Wrapper2 #(TVE_Wrapper_Param param)
                       (TVE_Wrapper_IFC);

   // ================================================================
   // TV Encoder and connections
   // TV Encoder transforms Trace_Data structures produced by the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier

   TV_Encode_IFC tv_encode <- mkTV_Encode;

   // Connect CPU's TV output data to TV Encoder
   mkConnection (tv_encode.trace_data_in, param.cpu_trace_data_out);

   // Adapter to convert 2-tuple tv_encode output to TV_Info struct
   // Note: would not be necessary if TV Encoder directly produced this struct
   // TODO: change TV Encoder for this?
   FIFOF #(TV_Info) f_tv_info <- mkFIFOF;

   rule rl_tv_adapter;
      match { .n, .v } <- tv_encode.tv_vb_out.get;
      f_tv_info.enq (TV_Info { num_bytes: n, vec_bytes: v });
   endrule

   // ================================================================
   // INTERFACE

   // GPR mem server to Debug Module
   interface Server  hart0_gpr_mem_server = param.cpu_hart0_gpr_mem_server;
`ifdef ISA_F
   // FPR mem server to Debug Module
   interface Server  hart0_fpr_mem_server = param.cpu_hart0_fpr_mem_server;
`endif
   // CSR mem server to Debug Module
   interface Server  hart0_csr_mem_server = param.cpu_hart0_csr_mem_server;

   // System Bus Access interface to Debug Module
   interface AXI4_Slave_IFC  sba_S        = dummy_AXI4_Slave_ifc;

   // TV output
   interface FIFOF_O         fo_tv_info   = to_FIFOF_O (f_tv_info);
   // DMA server
   interface AXI4_Slave_IFC  dma_S        = param.cpu_dma_server;
endmodule

// ****************************************************************
// Variant: Debug Module present, Tandem Verification absent

module mkTVE_Wrapper3 #(TVE_Wrapper_Param param)
                       (TVE_Wrapper_IFC);

   // ----------------
   // Instantiate mem dma mux
   Dma_Server_Mux_IFC  dma_server_mux <- mkDma_Server_Mux;

   // Connect mem dma mux to CPU
   mkConnection (dma_server_mux.target_client, param.cpu_dma_server);

   // ================================================================
   // INTERFACE

   // GPR mem server to Debug Module
   interface Server  hart0_gpr_mem_server = param.cpu_hart0_gpr_mem_server;
`ifdef ISA_F
   // FPR mem server to Debug Module
   interface Server  hart0_fpr_mem_server = param.cpu_hart0_fpr_mem_server;
`endif
   // CSR mem server to Debug Module
   interface Server  hart0_csr_mem_server = param.cpu_hart0_csr_mem_server;

   // System Bus Access interface to Debug Module
   interface AXI4_Slave_IFC  sba_S        = dma_server_mux.initiator_B_server;

   // TV output
   interface FIFOF_O         fo_tv_info   = dummy_FIFOF_O;
   // DMA server
   interface AXI4_Slave_IFC  dma_S        = dma_server_mux.initiator_A_server;
endmodule

// ****************************************************************
// Variant: Debug Module absent, Tandem Verification absent

module mkTVE_Wrapper4 #(TVE_Wrapper_Param param)
                       (TVE_Wrapper_IFC);

   // ================================================================
   // INTERFACE

   // GPR mem server to Debug Module
   interface Server  hart0_gpr_mem_server = param.cpu_hart0_gpr_mem_server;
`ifdef ISA_F
   // FPR mem server to Debug Module
   interface Server  hart0_fpr_mem_server = param.cpu_hart0_fpr_mem_server;
`endif
   // CSR mem server to Debug Module
   interface Server  hart0_csr_mem_server = param.cpu_hart0_csr_mem_server;

   // System Bus Access interface to Debug Module
   interface AXI4_Slave_IFC  sba_S        = dummy_AXI4_Slave_ifc;

   // TV output
   interface FIFOF_O         fo_tv_info   = dummy_FIFOF_O;
   // DMA server
   interface AXI4_Slave_IFC  dma_S        = param.cpu_dma_server;
endmodule

// ****************************************************************

endpackage
