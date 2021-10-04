// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package BSCore;

// ================================================================
// This package defines the interface and implementation of the 'P1 Core'
// for the DARPA SSITH project.
// This P1 core contains:
//    - Piccolo CPU, including
//        - Near_Mem (ICache and DCache)
//        - Near_Mem_IO (Timer, Software-interrupt, and other mem-mapped-locations)
//        - External interrupt request lines
//        - 2 x AXI4 Master interfaces (from DM and ICache, and from DCache)
//    - RISC-V Debug Module (DM)
//    - JTAG TAP interface for DM
//    - Optional Tandem Verification trace stream output interface

// ================================================================
// BSV library imports

import Vector        :: *;
import FIFO          :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;
import Bus           :: *;
import Clocks        :: *;

// ----------------
// BSV additional libs

import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import SoC_Map  :: *;
import PLIC_16_1_7 :: *;

// The basic core
import Core_IFC :: *;
import Core     :: *;

// External interrupt request interface
import PLIC :: *;    // for PLIC_Source_IFC type which is exposed at P2_Core interface

// Main Fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;

`ifdef FABRIC_AHBL
import AHBL_Types   :: *;
import AHBL_Defs    :: *;
`endif

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info :: *;
import AXI4_Stream ::*;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module       :: *;
import Debug_Interfaces   :: *;
import Jtag               :: *;
import JtagTap            :: *;
import Giraffe_IFC        :: *;
`endif

// ================================================================
// Constant: cycles to hold SoC in reset for ndm reset:

UInt#(6) ndm_interval = 20;
UInt#(6) por_interval = 20;

// ================================================================
// The BSCore interface

interface BSCore_IFC;

   // ----------------------------------------------------------------
   // Core CPU interfaces
`ifndef Near_Mem_TCM
   // CPU IMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master0;
`endif

`ifdef FABRIC_AHBL
   // CPU DMem (incl. I/O) to Fabric master interface
   interface AHBL_Master_IFC#(AHBL_Defs::AHB_Wd_Data) master1;
`else
   // CPU DMem (incl. I/O) to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master1;
`endif

   // External interrupt sources
   (* always_ready, always_enabled, prefix="" *)
   method  Action interrupt_reqs ((* port="cpu_external_interrupt_req" *) Bit #(N_External_Interrupt_Sources)  reqs);

`ifdef INCLUDE_DMEM_SLAVE
   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) slave0;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface.  The data signal is
   // packed output tuples (n,vb),/ where 'vb' is a vector of
   // bytes with relevant bytes in locations [0]..[n-1]

   interface AXI4_Stream_Master_IFC #(Wd_SId, Wd_SDest, Wd_SData, Wd_SUser)  tv_verifier_info_tx;
`endif

`ifdef INCLUDE_GDB_CONTROL
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) debug;
`endif

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr
`ifdef Near_Mem_TCM
`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool  watch_tohost, Bit #(64)  tohost_addr);
   method Bit #(64) mv_tohost_value;
`endif

`ifdef TCM_LOADER
   // connections to loader
   interface AXI4_Slave_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) loader_slave;
   (* always_ready *)
   method Bool reset_done;
   (* always_ready, always_enabled *)
   method Action cpu_halt(Bool x);
`endif
`endif

endinterface

// ================================================================

(* synthesize *)
module mkBSCore (BSCore_IFC);
   let clk <- exposeCurrentClock;
   // Reset this by default reset, so core is reset by both default and ndm
   let ndmIfc <- mkReset(2, True, clk);
   let coreRSTN = ndmIfc.new_rst;

   // Core: CPU + Near_Mem_IO (CLINT) + PLIC + Debug module (optional) + TV (optional)
   Core_IFC::Core_IFC #(N_External_Interrupt_Sources)  core <- mkCore(reset_by coreRSTN);

   // ================================================================
   // Tie-offs (not used in SSITH GFE)

   // Set core's verbosity
   rule rl_never (False);
      core.set_verbosity (?, ?);
   endrule

   // Tie-offs
   rule rl_always (True);
      // Non-maskable interrupt request.
      core.nmi_req (False);
   endrule

   // ================================================================
   // Reset on startup, and also on NDM reset from Debug Module
   // (NDM reset from Debug Module = "non-debug-module-reset" = reset all except Debug Module)

   Reg #(Bool)          rg_once       <- mkReg (False); // also set False by ndmreset
   Reg #(Bool)          rg_reset_done <- mkReg (False);
   Reg #(Bool)          rg_last_cpuh  <- mkReg (False);
   Reg #(Maybe #(Bool)) rg_ldr_reset  <- mkReg (tagged Invalid);
`ifdef INCLUDE_GDB_CONTROL
   Reg #(UInt #(6))     rg_ndm_count <- mkReg (0);

   rule decNdmCountRl (rg_ndm_count != 0);
      rg_ndm_count <= rg_ndm_count -1;
      ndmIfc.assertReset();
   endrule
`endif

   let coreInReset <- isResetAsserted(reset_by coreRSTN);

   rule rl_once (! rg_once && ! coreInReset);
      Bool running = True;
      if (rg_ldr_reset matches tagged Valid False)
	 running = False;
      rg_ldr_reset <= Invalid;
      core.cpu_reset_server.request.put (running);
      // TODO: maybe set rg_ndm_count if debug_module present?
      rg_once <= True;
   endrule

`ifdef TCM_LOADER
   (*descending_urgency="rl_once, cpu_halt"*)
`endif
   rule rl_reset_response;
      let running <- core.cpu_reset_server.response.get;
`ifndef TCM_LOADER
      if (running) $display("Trace starting");
`endif
`ifdef INCLUDE_GDB_CONTROL
      // wait for end of ndm_interval:
      when (rg_ndm_count == 0, noAction);
`endif
      rg_reset_done <= True;
   endrule

   // ----------------

`ifdef INCLUDE_TANDEM_VERIF
   let tv_xactor <- mkTV_Xactor;
   mkConnection (core.tv_verifier_info_get, tv_xactor.tv_in);
`endif

   // ================================================================
   // INTERFACE
`ifndef Near_Mem_TCM
   // CPU IMem to Fabric master interface
   interface AXI4_Master_IFC master0 = core.cpu_imem_master;
`endif

`ifdef FABRIC_AHBL
   // CPU DMem to Fabric master interface
   interface AHBL_Master_IFC master1 = core.cpu_dmem_master;
`else
   // CPU DMem to Fabric master interface
   interface AXI4_Master_IFC master1 = core.cpu_dmem_master;
`endif

   // External interrupts
   method  Action interrupt_reqs (Bit #(N_External_Interrupt_Sources) reqs);
      for (Integer j = 0; j < valueOf (N_External_Interrupt_Sources); j = j + 1) begin
	 Bool req_j = unpack (reqs [j]);
	 core.core_external_interrupt_sources [j].m_interrupt_req (req_j);
      end
   endmethod

`ifdef INCLUDE_DMEM_SLAVE
   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

   interface AXI4_Lite_Slave_IFC slave0 = core.cpu_dmem_slave;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface.  The data signal is
   // packed output tuples (n,vb),/ where 'vb' is a vector of
   // bytes with relevant bytes in locations [0]..[n-1]

   interface tv_verifier_info_tx = tv_xactor.axi_out;
`endif

`ifdef INCLUDE_GDB_CONTROL
   interface debug = core.debug;
`endif

`ifdef Near_Mem_TCM
`ifdef WATCH_TOHOST
   // For ISA tests: watch memory writes to <tohost> addr
   method Action set_watch_tohost (Bool  watch_tohost, Bit #(64)  tohost_addr);
      core.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Bit #(64) mv_tohost_value = core.mv_tohost_value;
`endif
`ifdef TCM_LOADER
   interface loader_slave = core.loader_slave;
   method Action cpu_halt (x);
      if (x != rg_last_cpuh && !isValid(rg_ldr_reset)) begin
	 rg_ldr_reset <= tagged Valid (!x); // value is "running"
	 rg_once <= False;
	 rg_last_cpuh <= x;
	 rg_reset_done <= False;
	 if (!x) $display("Trace starting");
      end
   endmethod
   method reset_done = rg_reset_done;
`endif
`endif

endmodule

// ================================================================

`ifdef INCLUDE_TANDEM_VERIF

// ================================================================
// TV AXI4 Stream Parameters

typedef SizeOf #(Info_CPU_to_Verifier)Wd_SData;
typedef 0 Wd_SDest;
typedef 0 Wd_SUser;
typedef 0 Wd_SId;

// ================================================================

interface TV_Xactor;
   interface Put #(Info_CPU_to_Verifier) tv_in;
   interface AXI4_Stream_Master_IFC #(Wd_SId, Wd_SDest, Wd_SData, Wd_SUser)  axi_out;
endinterface

function AXI4_Stream #(Wd_SId, Wd_SDest, Wd_SData, Wd_SUser) fn_TVToAxiS (Info_CPU_to_Verifier x);
   return AXI4_Stream {tid: ?,
		       tdata: pack(x),
		       tstrb: '1,
		       tkeep: '1,
		       tlast: True,
		       tdest: ?,
		       tuser: ? };
endfunction

(*synthesize*)
module mkTV_Xactor (TV_Xactor);
   AXI4_Stream_Master_Xactor_IFC #(Wd_SId, Wd_SDest, Wd_SData, Wd_SUser)
                               tv_xactor <- mkAXI4_Stream_Master_Xactor;

   interface Put tv_in;
      method Action put(x);
	 toPut(tv_xactor.i_stream).put(fn_TVToAxiS(x));
      endmethod
   endinterface

   interface axi_out = tv_xactor.axi_side;
endmodule
`endif

// ================================================================

endpackage
