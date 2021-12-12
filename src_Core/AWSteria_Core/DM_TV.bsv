// Copyright (c) 2021 Bluespec, Inc. All Rights Reserved.
// Author: Rishiyur S. Nikhil

package DM_TV;

// ================================================================
// This package defines four substitutable modules:
//     mknoDM_noTV
//     mknoDM_TV
//     mkDM_noTV
//     mkDM_TV
// that fit into AWSteria_Core, covering the four choices of optional
// Debug Module and optional Tandem Verification.
//

// In Core_v2's mkCore this code is in-line, with 'Ifdefs' governing
// the various choices, making it difficult to read; hence this
// 4-variant organization.

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
// import Fabric_Defs :: *;    // for Wd_{Id,Addr,Data,User}
import AXI_Widths  :: *;    // for Wd_{Id,Addr,Data,User}_Dma
import SoC_Map     :: *;

import ISA_Decls :: *;
import CPU_IFC   :: *;

import DMI            :: *;
import Debug_Module   :: *;
import DM_CPU_Req_Rsp :: *;

import Dma_Server_Mux :: *;

import TV_Trace_Data :: *;
import TV_Info       :: *;
import TV_Encode     :: *;

// TV_Taps needed when both GDB_CONTROL and TANDEM_VERIF are present
import TV_Taps :: *;

// ================================================================
// Common interface for all 4 variants of the module

// All the modules take a parameter of the following type

typedef struct {
   // CPU's Coherent DMA interface
   AXI4_Slave_IFC #(Wd_Id_Dma,
		    Wd_Addr_Dma,
		    Wd_Data_Dma,
		    Wd_User_Dma)    cpu_dma_server;
   // CPU's TV out
   Get #(Trace_Data)                cpu_trace_data_out;

   // CPU's debug interfaces
   Put #(Bit #(4))                  cpu_hart0_put_other_req;
   Server #(Bool, Bool)             cpu_hart0_server_run_halt;
   Server #(DM_CPU_Req #(12, XLEN),
	    DM_CPU_Rsp #(XLEN))     cpu_hart0_csr_mem_server;
`ifdef ISA_F
   Server #(DM_CPU_Req #(5,  FLEN),
	    DM_CPU_Rsp #(FLEN))     cpu_hart0_fpr_mem_server;
`endif
   Server #(DM_CPU_Req #(5,  XLEN),
	    DM_CPU_Rsp #(XLEN))     cpu_hart0_gpr_mem_server;

   // From Control/Status
   Client #(Bool, Bool)             cs_cl_hart0_reset;
   Client #(Bool, Bool)             cs_cl_run_halt;
   Client #(DM_CPU_Req #(12, XLEN),
	    DM_CPU_Rsp #(XLEN))     cs_cl_csr_rw;
   } DM_TV_Param;

// All the modules return an interface of the following type

interface DM_TV_IFC;
   interface AXI4_Slave_IFC  #(Wd_Id_Dma,
			       Wd_Addr_Dma,
			       Wd_Data_Dma,
			       Wd_User_Dma)  dma_S;
   interface FIFOF_O #(TV_Info)              fo_tv_info;
   interface Server_Semi_FIFOF #(DMI_Req,
				 DMI_Rsp)    se_dmi;
   interface Client #(Bool, Bool)            cl_reset;
   interface Client_Semi_FIFOF #(Bit #(0),
				 Bit #(0))   cl_ndm_reset;
endinterface

// ****************************************************************
// Top-level selection of one of the four modules that follow

module mkDM_TV #(DM_TV_Param param) (DM_TV_IFC);

   // Choose dm_tv module depending on GDB and TV options
   let dm_tv_module =
`ifdef INCLUDE_GDB_CONTROL
  `ifdef INCLUDE_TANDEM_VERIF
       mkDM_and_TV
  `else
       mkDM_noTV
  `endif
`else
  `ifdef INCLUDE_TANDEM_VERIF
       mknoDM_TV
  `else
       mknoDM_noTV
  `endif
`endif
   ;

   DM_TV_IFC ifc <- dm_tv_module (param);
   return ifc;
endmodule

// ****************************************************************
// Variant: Debug Module present, Tandem Verification present

module mkDM_and_TV #(DM_TV_Param param) (DM_TV_IFC);

   // ================================================================
   // Debug Module and connections

   Debug_Module_IFC  debug_module <- mkDebug_Module;

   // Connect "other" requests from debug module to CPU.
   // TODO: this functionality is non-spec and will be removed.
   mkConnection (debug_module.hart0_get_other_req, param.cpu_hart0_put_other_req);

   // Each 'tap' siphons off a record for the Tandem Verifier,
   // for each Debug-Module write to a CPU CSR, FPR, GPR, or memory.
   DM_CSR_Tap_IFC dm_csr_tap <- mkDM_CSR_Tap;
`ifdef ISA_F
   DM_FPR_Tap_IFC dm_fpr_tap <- mkDM_FPR_Tap;
`endif
   DM_GPR_Tap_IFC dm_gpr_tap <- mkDM_GPR_Tap;
   DM_Mem_Tap_IFC dm_mem_tap <- mkDM_Mem_Tap;

   // Connect Debug module to taps
   mkConnection (debug_module.hart0_csr_mem_client, dm_csr_tap.server);
`ifdef ISA_F
   mkConnection (debug_module.hart0_fpr_mem_client, dm_fpr_tap.server);
`endif
   mkConnection (debug_module.hart0_gpr_mem_client, dm_gpr_tap.server);
   mkConnection (debug_module.master,               dm_mem_tap.slave);

   // ----------------
   // 2x1 muxes merging controls from
   //     mkHost_Control_Status and
   //     Debug Module
   // to CPU

   // Controls degree of pipelining of these muxes
   Integer in_flight = 1;

   // FIFOFs for merging resets
   FIFOF #(Bool) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bool) f_reset_rsps <- mkFIFOF;

   // Merge reset
   let empty0 <- mkMux_Clients_Server (in_flight,
				       cons (param.cs_cl_hart0_reset,
					     cons (debug_module.hart0_reset_client,
						   nil)),
				       toGPServer (f_reset_reqs, f_reset_rsps));
   // Merge run/halt
   let empty1 <- mkMux_Clients_Server (in_flight,
				       cons (param.cs_cl_run_halt,
					     cons (debug_module.hart0_client_run_halt,
						   nil)),
				       param.cpu_hart0_server_run_halt);
   // Merge CSR r/w
   let empty2 <- mkMux_Clients_Server (in_flight,
				       cons (param.cs_cl_csr_rw,
					     cons (dm_csr_tap.client,
						   nil)),
				       param.cpu_hart0_csr_mem_server);

`ifdef ISA_F
   // Connect FPR r/w from Debug Module Tap to CPU
   mkConnection (dm_fpr_tap.client, param.cpu_hart0_fpr_mem_server);
`endif

   // Connect GPR r/w from Debug Module Tap to CPU
   mkConnection (dm_gpr_tap.client, param.cpu_hart0_gpr_mem_server);

   // Connect Debug Module mem tap to CPU dma_server via DMA_Server_Mux
   Dma_Server_Mux_IFC  dma_server_mux <- mkDma_Server_Mux;
   mkConnection (dm_mem_tap.master,            dma_server_mux.initiator_B_server);
   mkConnection (dma_server_mux.target_client, param.cpu_dma_server);

   // ----------------
   // GDB requests and responses into DMI input of Debug module

   FIFOF #(DMI_Req) f_dmi_reqs <- mkFIFOF;
   FIFOF #(DMI_Rsp) f_dmi_rsps <- mkFIFOF;

   rule rl_dmi_req;
      let req <- pop (f_dmi_reqs);
      if (req.is_read)
	 debug_module.dmi.read_addr (req.addr);
      else
	 debug_module.dmi.write (req.addr, req.wdata);
   endrule

   rule rl_dmi_rsp;
      let x <- debug_module.dmi.read_data;
      let rsp = DMI_Rsp { rdata: x };
      f_dmi_rsps.enq (rsp);
   endrule

   // ----------------
   // Non-Debug-Module reset requests and responses from Debug-Module
   // Note: if the Debug Module's ndm_reset_client ifc was a
   // Client_FIFOF #(Bit #(0), Bit #(0)), we woudn't need this.
   // TODO: change Debug Module for that?

   FIFOF #(Bit #(0)) f_ndm_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_ndm_rsps <- mkFIFOF;

   rule rl_ndm_req;
      // Note: ignoring bool 'running/halted' indication x
      Bool x <- debug_module.ndm_reset_client.request.get;
      f_ndm_reqs.enq (?);
   endrule

   rule rl_ndm_rsp;
      // Note: ignoring Bit #(0) 'running/halted' response y, feeding True
      Bit #(0) y <- pop (f_ndm_rsps);
      Bool y1 = True;
      debug_module.ndm_reset_client.response.put (y1);
   endrule

   // ================================================================
   // TV Encoder and connections
   // TV Encoder transforms Trace_Data structures produced by the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier

   TV_Encode_IFC tv_encode <- mkTV_Encode;

   // FIFOF to merge TV data from CPU and from Debug Module
   FIFOF #(Trace_Data) f_trace_data_merged <- mkFIFOF;
   mkConnection (toGet (f_trace_data_merged), tv_encode.trace_data_in);

   // The following merges into f_trace_data_merged are equivalent to:
   //     mkConnection (..., toPut (f_trace_data_merged))
   // but using rules allows us to name them in scheduling attributes.

   // Merge-in CPU's TV data.
  rule merge_cpu_trace_data;
      let tmp <- param.cpu_trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule

   rule merge_dm_csr_trace_data;
      let tmp <- dm_csr_tap.trace_data_out.get;
      f_trace_data_merged.enq(tmp);
   endrule

`ifdef ISA_F
   rule merge_dm_fpr_trace_data;
      let tmp <- dm_fpr_tap.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
   endrule
`endif

   rule merge_dm_gpr_trace_data;
      let tmp <- dm_gpr_tap.trace_data_out.get;
      f_trace_data_merged.enq (tmp);
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

   interface AXI4_Slave_IFC    dma_S        = dma_server_mux.initiator_A_server;
   interface FIFOF_O           fo_tv_info   = to_FIFOF_O (f_tv_info);
   interface Server_Semi_FIFOF se_dmi       = fifofs_to_Server_Semi_FIFOF (f_dmi_reqs,
									   f_dmi_rsps);
   interface Client            cl_reset     = toGPClient (f_reset_reqs, f_reset_rsps);
   interface Client_Semi_FIFOF cl_ndm_reset = fifofs_to_Client_Semi_FIFOF (f_ndm_reqs,
									   f_ndm_rsps);
endmodule

// ****************************************************************
// Variant: Debug Module present, Tandem Verification absent

module mkDM_noTV #(DM_TV_Param param) (DM_TV_IFC);

   // ================================================================
   // Debug Module and connections

   Debug_Module_IFC  debug_module <- mkDebug_Module;

   // Connect "other" requests from debug module to CPU.
   // TODO: this functionality is non-spec and will be removed.
   mkConnection (debug_module.hart0_get_other_req, param.cpu_hart0_put_other_req);

   // ----------------
   // 2x1 muxes merging controls from
   //     mkHost_Control_Status and
   //     Debug Module
   // to CPU

   // Controls degree of pipelining of these muxes
   Integer in_flight = 1;

   // FIFOFs for merging resets
   FIFOF #(Bool) f_reset_reqs <- mkFIFOF;
   FIFOF #(Bool) f_reset_rsps <- mkFIFOF;

   // Merge reset
   let empty0 <- mkMux_Clients_Server (in_flight,
				       cons (param.cs_cl_hart0_reset,
					     cons (debug_module.hart0_reset_client,
						   nil)),
				       toGPServer (f_reset_reqs, f_reset_rsps));
   // Merge run/halt
   let empty1 <- mkMux_Clients_Server (in_flight,
				       cons (param.cs_cl_run_halt,
					     cons (debug_module.hart0_client_run_halt,
						   nil)),
				       param.cpu_hart0_server_run_halt);
   // Merge CSR r/w
   let empty2 <- mkMux_Clients_Server (in_flight,
				       cons (param.cs_cl_csr_rw,
					     cons (debug_module.hart0_csr_mem_client,
						   nil)),
				       param.cpu_hart0_csr_mem_server);

`ifdef ISA_F
   // Connect FPR r/w from Debug Module to CPU
   mkConnection (debug_module.hart0_fpr_mem_client, param.cpu_hart0_fpr_mem_server);
`endif

   // Connect GPR r/w from Debug Module to CPU
   mkConnection (debug_module.hart0_gpr_mem_client, param.cpu_hart0_gpr_mem_server);

   // Connect Debug Module to CPU dma_server via DMA_Server_Mux
   Dma_Server_Mux_IFC  dma_server_mux <- mkDma_Server_Mux;
   mkConnection (debug_module.master,          dma_server_mux.initiator_B_server);
   mkConnection (dma_server_mux.target_client, param.cpu_dma_server);

   // ----------------
   // GDB requests and responses into DMI input of Debug module

   FIFOF #(DMI_Req) f_dmi_reqs <- mkFIFOF;
   FIFOF #(DMI_Rsp) f_dmi_rsps <- mkFIFOF;

   rule rl_dmi_req;
      let req <- pop (f_dmi_reqs);
      if (req.is_read)
	 debug_module.dmi.read_addr (req.addr);
      else
	 debug_module.dmi.write (req.addr, req.wdata);
   endrule

   rule rl_dmi_rsp;
      let x <- debug_module.dmi.read_data;
      let rsp = DMI_Rsp { rdata: x };
      f_dmi_rsps.enq (rsp);
   endrule

   // ----------------
   // Non-Debug-Module reset requests and responses from Debug-Module
   // Note: if the Debug Module's ndm_reset_client ifc was a
   // Client_FIFOF #(Bit #(0), Bit #(0)), we woudn't need this.
   // TODO: change Debug Module for that?

   FIFOF #(Bit #(0)) f_ndm_reqs <- mkFIFOF;
   FIFOF #(Bit #(0)) f_ndm_rsps <- mkFIFOF;

   rule rl_ndm_req;
      // Note: ignoring bool 'running/halted' indication x
      Bool x <- debug_module.ndm_reset_client.request.get;
      f_ndm_reqs.enq (?);
   endrule

   rule rl_ndm_rsp;
      // Note: ignoring Bit #(0) 'running/halted' response y, feeding True
      Bit #(0) y <- pop (f_ndm_rsps);
      Bool y1 = True;
      debug_module.ndm_reset_client.response.put (y1);
   endrule

   // ================================================================
   // INTERFACE

   interface AXI4_Slave_IFC    dma_S        = dma_server_mux.initiator_A_server;
   interface FIFOF_O           fo_tv_info   = dummy_FIFOF_O;
   interface Server_Semi_FIFOF se_dmi       = fifofs_to_Server_Semi_FIFOF (f_dmi_reqs,
									   f_dmi_rsps);
   interface Client            cl_reset     = toGPClient (f_reset_reqs,
							  f_reset_rsps);
   interface Client_Semi_FIFOF cl_ndm_reset = fifofs_to_Client_Semi_FIFOF (f_ndm_reqs,
									   f_ndm_rsps);
endmodule

// ****************************************************************
// Variant: Debug Module absent, Tandem Verification present

module mk_noDM_TV #(DM_TV_Param param) (DM_TV_IFC);

   // ================================================================
   // Connections withtout Debug Module

   Get #(Bit #(4)) getstub1 = getstub;
   mkConnection (getstub1, param.cpu_hart0_put_other_req);

   mkConnection (param.cs_cl_run_halt,  param.cpu_hart0_server_run_halt);
   mkConnection (param.cs_cl_csr_rw,    param.cpu_hart0_csr_mem_server);
`ifdef ISA_F
   Client #(DM_CPU_Req #(5,  FLEN),
	    DM_CPU_Rsp #(FLEN))     client_stub1 = client_stub;
   mkConnection (client_stub1,  param.cpu_hart0_fpr_mem_server);
`endif
   Client #(DM_CPU_Req #(5,  XLEN),
	    DM_CPU_Rsp #(XLEN))     client_stub2 = client_stub;
   mkConnection (client_stub2,  param.cpu_hart0_gpr_mem_server);

   // ================================================================
   // TV Encoder and connections
   // TV encoder transforms Trace_Data structures produced by the CPU and DM
   // into encoded byte vectors for transmission to the Tandem Verifier

   TV_Encode_IFC tv_encode <- mkTV_Encode;

   // Connect CPU TV output to TV Encoder
   mkConnection (param.cpu_trace_data_out, tv_encode.trace_data_in);

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

   interface AXI4_Slave_IFC    dma_S         = param.cpu_dma_server;
   interface FIFOF_O           fo_tv_info    = to_FIFOF_O (f_tv_info);
   interface Server_Semi_FIFOF se_dmi        = dummy_Server_Semi_FIFOF;
   interface Client            cl_reset      = param.cs_cl_hart0_reset;
   interface Client_Semi_FIFOF cl_ndm_reset  = dummy_Client_Semi_FIFOF;
endmodule

// ****************************************************************
// Variant: Debug Module absent, Tandem Verification absent

module mk_noDM_noTV #(DM_TV_Param param) (DM_TV_IFC);

   // ================================================================
   // Connections withtout Debug Module

   Get #(Bit #(4)) getstub1 = getstub;
   mkConnection (getstub1, param.cpu_hart0_put_other_req);

   mkConnection (param.cs_cl_run_halt,  param.cpu_hart0_server_run_halt);
   mkConnection (param.cs_cl_csr_rw,    param.cpu_hart0_csr_mem_server);
`ifdef ISA_F
   Client #(DM_CPU_Req #(5,  FLEN),
	    DM_CPU_Rsp #(FLEN))     client_stub1 = client_stub;
   mkConnection (client_stub1,  param.cpu_hart0_fpr_mem_server);
`endif
   Client #(DM_CPU_Req #(5,  XLEN),
	    DM_CPU_Rsp #(XLEN))     client_stub2 = client_stub;
   mkConnection (client_stub2,  param.cpu_hart0_gpr_mem_server);

   // ================================================================
   // INTERFACE

   interface AXI4_Slave_IFC    dma_S         = param.cpu_dma_server;
   interface FIFOF_O           fo_tv_info    = dummy_FIFOF_O;
   interface Server_Semi_FIFOF se_dmi        = dummy_Server_Semi_FIFOF;
   interface Client            cl_reset      = param.cs_cl_hart0_reset;
   interface Client_Semi_FIFOF cl_ndm_reset  = dummy_Client_Semi_FIFOF;
endmodule

// ****************************************************************

endpackage
