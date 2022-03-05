//
// Generated by Bluespec Compiler, version 2021.07-22-g61dc0ebb (build 61dc0ebb)
//
//
// Ports:
// Name                         I/O  size props
// master1_HADDR                  O    32 reg
// master1_HBURST                 O     3 const
// master1_HMASTLOCK              O     1 const
// master1_HPROT                  O     4 const
// master1_HSIZE                  O     3 reg
// master1_HTRANS                 O     2 reg
// master1_HWDATA                 O    32 reg
// master1_HWRITE                 O     1 reg
// tv_verifier_info_tx_tvalid     O     1 reg
// tv_verifier_info_tx_tdata      O   608 reg
// tv_verifier_info_tx_tstrb      O    76 reg
// tv_verifier_info_tx_tkeep      O    76 reg
// tv_verifier_info_tx_tlast      O     1 reg
// debug_awready                  O     1 reg
// debug_wready                   O     1 reg
// debug_bvalid                   O     1 reg
// debug_bid                      O     4 reg
// debug_bresp                    O     2 reg
// debug_arready                  O     1 reg
// debug_rvalid                   O     1 reg
// debug_rid                      O     4 reg
// debug_rdata                    O    32 reg
// debug_rresp                    O     2 reg
// debug_rlast                    O     1 reg
// RDY_set_watch_tohost           O     1 const
// mv_tohost_value                O    64
// RDY_mv_tohost_value            O     1 const
// CLK                            I     1 clock
// RST_N                          I     1 reset
// master1_HRDATA                 I    32
// master1_HREADY                 I     1
// master1_HRESP                  I     1
// cpu_external_interrupt_req     I    16
// tv_verifier_info_tx_tready     I     1
// debug_awvalid                  I     1
// debug_awid                     I     4 reg
// debug_awaddr                   I    32 reg
// debug_awlen                    I     8 reg
// debug_awsize                   I     3 reg
// debug_awburst                  I     2 reg
// debug_awlock                   I     1 reg
// debug_awcache                  I     4 reg
// debug_awprot                   I     3 reg
// debug_awqos                    I     4 reg
// debug_awregion                 I     4 reg
// debug_wvalid                   I     1
// debug_wdata                    I    32 reg
// debug_wstrb                    I     4 reg
// debug_wlast                    I     1 reg
// debug_bready                   I     1
// debug_arvalid                  I     1
// debug_arid                     I     4 reg
// debug_araddr                   I    32 reg
// debug_arlen                    I     8 reg
// debug_arsize                   I     3 reg
// debug_arburst                  I     2 reg
// debug_arlock                   I     1 reg
// debug_arcache                  I     4 reg
// debug_arprot                   I     3 reg
// debug_arqos                    I     4 reg
// debug_arregion                 I     4 reg
// debug_rready                   I     1
// set_watch_tohost_watch_tohost  I     1
// set_watch_tohost_tohost_addr   I    64
// EN_set_watch_tohost            I     1
//
// No combinational paths from inputs to outputs
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkBSCore(CLK,
		RST_N,

		master1_HADDR,

		master1_HBURST,

		master1_HMASTLOCK,

		master1_HPROT,

		master1_HSIZE,

		master1_HTRANS,

		master1_HWDATA,

		master1_HWRITE,

		master1_HRDATA,

		master1_HREADY,

		master1_HRESP,

		cpu_external_interrupt_req,

		tv_verifier_info_tx_tvalid,

		tv_verifier_info_tx_tdata,

		tv_verifier_info_tx_tstrb,

		tv_verifier_info_tx_tkeep,

		tv_verifier_info_tx_tlast,

		tv_verifier_info_tx_tready,

		debug_awvalid,
		debug_awid,
		debug_awaddr,
		debug_awlen,
		debug_awsize,
		debug_awburst,
		debug_awlock,
		debug_awcache,
		debug_awprot,
		debug_awqos,
		debug_awregion,

		debug_awready,

		debug_wvalid,
		debug_wdata,
		debug_wstrb,
		debug_wlast,

		debug_wready,

		debug_bvalid,

		debug_bid,

		debug_bresp,

		debug_bready,

		debug_arvalid,
		debug_arid,
		debug_araddr,
		debug_arlen,
		debug_arsize,
		debug_arburst,
		debug_arlock,
		debug_arcache,
		debug_arprot,
		debug_arqos,
		debug_arregion,

		debug_arready,

		debug_rvalid,

		debug_rid,

		debug_rdata,

		debug_rresp,

		debug_rlast,

		debug_rready,

		set_watch_tohost_watch_tohost,
		set_watch_tohost_tohost_addr,
		EN_set_watch_tohost,
		RDY_set_watch_tohost,

		mv_tohost_value,
		RDY_mv_tohost_value);
  input  CLK;
  input  RST_N;

  // value method master1_haddr
  output [31 : 0] master1_HADDR;

  // value method master1_hburst
  output [2 : 0] master1_HBURST;

  // value method master1_hmastlock
  output master1_HMASTLOCK;

  // value method master1_hprot
  output [3 : 0] master1_HPROT;

  // value method master1_hsize
  output [2 : 0] master1_HSIZE;

  // value method master1_htrans
  output [1 : 0] master1_HTRANS;

  // value method master1_hwdata
  output [31 : 0] master1_HWDATA;

  // value method master1_hwrite
  output master1_HWRITE;

  // action method master1_hrdata
  input  [31 : 0] master1_HRDATA;

  // action method master1_hready
  input  master1_HREADY;

  // action method master1_hresp
  input  master1_HRESP;

  // action method interrupt_reqs
  input  [15 : 0] cpu_external_interrupt_req;

  // value method tv_verifier_info_tx_m_tvalid
  output tv_verifier_info_tx_tvalid;

  // value method tv_verifier_info_tx_m_tid

  // value method tv_verifier_info_tx_m_tdata
  output [607 : 0] tv_verifier_info_tx_tdata;

  // value method tv_verifier_info_tx_m_tstrb
  output [75 : 0] tv_verifier_info_tx_tstrb;

  // value method tv_verifier_info_tx_m_tkeep
  output [75 : 0] tv_verifier_info_tx_tkeep;

  // value method tv_verifier_info_tx_m_tlast
  output tv_verifier_info_tx_tlast;

  // value method tv_verifier_info_tx_m_tdest

  // value method tv_verifier_info_tx_m_tuser

  // action method tv_verifier_info_tx_m_tready
  input  tv_verifier_info_tx_tready;

  // action method debug_m_awvalid
  input  debug_awvalid;
  input  [3 : 0] debug_awid;
  input  [31 : 0] debug_awaddr;
  input  [7 : 0] debug_awlen;
  input  [2 : 0] debug_awsize;
  input  [1 : 0] debug_awburst;
  input  debug_awlock;
  input  [3 : 0] debug_awcache;
  input  [2 : 0] debug_awprot;
  input  [3 : 0] debug_awqos;
  input  [3 : 0] debug_awregion;

  // value method debug_m_awready
  output debug_awready;

  // action method debug_m_wvalid
  input  debug_wvalid;
  input  [31 : 0] debug_wdata;
  input  [3 : 0] debug_wstrb;
  input  debug_wlast;

  // value method debug_m_wready
  output debug_wready;

  // value method debug_m_bvalid
  output debug_bvalid;

  // value method debug_m_bid
  output [3 : 0] debug_bid;

  // value method debug_m_bresp
  output [1 : 0] debug_bresp;

  // value method debug_m_buser

  // action method debug_m_bready
  input  debug_bready;

  // action method debug_m_arvalid
  input  debug_arvalid;
  input  [3 : 0] debug_arid;
  input  [31 : 0] debug_araddr;
  input  [7 : 0] debug_arlen;
  input  [2 : 0] debug_arsize;
  input  [1 : 0] debug_arburst;
  input  debug_arlock;
  input  [3 : 0] debug_arcache;
  input  [2 : 0] debug_arprot;
  input  [3 : 0] debug_arqos;
  input  [3 : 0] debug_arregion;

  // value method debug_m_arready
  output debug_arready;

  // value method debug_m_rvalid
  output debug_rvalid;

  // value method debug_m_rid
  output [3 : 0] debug_rid;

  // value method debug_m_rdata
  output [31 : 0] debug_rdata;

  // value method debug_m_rresp
  output [1 : 0] debug_rresp;

  // value method debug_m_rlast
  output debug_rlast;

  // value method debug_m_ruser

  // action method debug_m_rready
  input  debug_rready;

  // action method set_watch_tohost
  input  set_watch_tohost_watch_tohost;
  input  [63 : 0] set_watch_tohost_tohost_addr;
  input  EN_set_watch_tohost;
  output RDY_set_watch_tohost;

  // value method mv_tohost_value
  output [63 : 0] mv_tohost_value;
  output RDY_mv_tohost_value;

  // signals for module outputs
  wire [607 : 0] tv_verifier_info_tx_tdata;
  wire [75 : 0] tv_verifier_info_tx_tkeep, tv_verifier_info_tx_tstrb;
  wire [63 : 0] mv_tohost_value;
  wire [31 : 0] debug_rdata, master1_HADDR, master1_HWDATA;
  wire [3 : 0] debug_bid, debug_rid, master1_HPROT;
  wire [2 : 0] master1_HBURST, master1_HSIZE;
  wire [1 : 0] debug_bresp, debug_rresp, master1_HTRANS;
  wire RDY_mv_tohost_value,
       RDY_set_watch_tohost,
       debug_arready,
       debug_awready,
       debug_bvalid,
       debug_rlast,
       debug_rvalid,
       debug_wready,
       master1_HMASTLOCK,
       master1_HWRITE,
       tv_verifier_info_tx_tlast,
       tv_verifier_info_tx_tvalid;

  // register coreInReset_isInReset
  reg coreInReset_isInReset;
  wire coreInReset_isInReset$D_IN, coreInReset_isInReset$EN;

  // register rg_last_cpuh
  reg rg_last_cpuh;
  wire rg_last_cpuh$D_IN, rg_last_cpuh$EN;

  // register rg_ldr_reset
  reg [1 : 0] rg_ldr_reset;
  wire [1 : 0] rg_ldr_reset$D_IN;
  wire rg_ldr_reset$EN;

  // register rg_ndm_count
  reg [5 : 0] rg_ndm_count;
  wire [5 : 0] rg_ndm_count$D_IN;
  wire rg_ndm_count$EN;

  // register rg_once
  reg rg_once;
  wire rg_once$D_IN, rg_once$EN;

  // register rg_reset_done
  reg rg_reset_done;
  wire rg_reset_done$D_IN, rg_reset_done$EN;

  // ports of submodule core
  wire [607 : 0] core$tv_verifier_info_get_get;
  wire [63 : 0] core$mv_tohost_value,
		core$set_verbosity_logdelay,
		core$set_watch_tohost_tohost_addr;
  wire [31 : 0] core$cpu_dmem_master_HADDR,
		core$cpu_dmem_master_HRDATA,
		core$cpu_dmem_master_HWDATA,
		core$debug_araddr,
		core$debug_awaddr,
		core$debug_rdata,
		core$debug_wdata;
  wire [7 : 0] core$debug_arlen, core$debug_awlen;
  wire [3 : 0] core$cpu_dmem_master_HPROT,
	       core$debug_arcache,
	       core$debug_arid,
	       core$debug_arqos,
	       core$debug_arregion,
	       core$debug_awcache,
	       core$debug_awid,
	       core$debug_awqos,
	       core$debug_awregion,
	       core$debug_bid,
	       core$debug_rid,
	       core$debug_wstrb,
	       core$set_verbosity_verbosity;
  wire [2 : 0] core$cpu_dmem_master_HBURST,
	       core$cpu_dmem_master_HSIZE,
	       core$debug_arprot,
	       core$debug_arsize,
	       core$debug_awprot,
	       core$debug_awsize;
  wire [1 : 0] core$cpu_dmem_master_HTRANS,
	       core$debug_arburst,
	       core$debug_awburst,
	       core$debug_bresp,
	       core$debug_rresp;
  wire core$EN_cpu_reset_server_request_put,
       core$EN_cpu_reset_server_response_get,
       core$EN_set_verbosity,
       core$EN_set_watch_tohost,
       core$EN_tv_verifier_info_get_get,
       core$RDY_cpu_reset_server_request_put,
       core$RDY_cpu_reset_server_response_get,
       core$RDY_tv_verifier_info_get_get,
       core$core_external_interrupt_sources_0_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_10_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_11_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_12_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_13_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_14_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_15_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_1_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_2_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_3_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_4_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_5_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_6_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_7_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_8_m_interrupt_req_set_not_clear,
       core$core_external_interrupt_sources_9_m_interrupt_req_set_not_clear,
       core$cpu_dmem_master_HMASTLOCK,
       core$cpu_dmem_master_HREADY,
       core$cpu_dmem_master_HRESP,
       core$cpu_dmem_master_HWRITE,
       core$cpu_reset_server_request_put,
       core$cpu_reset_server_response_get,
       core$debug_arlock,
       core$debug_arready,
       core$debug_arvalid,
       core$debug_awlock,
       core$debug_awready,
       core$debug_awvalid,
       core$debug_bready,
       core$debug_bvalid,
       core$debug_rlast,
       core$debug_rready,
       core$debug_rvalid,
       core$debug_wlast,
       core$debug_wready,
       core$debug_wvalid,
       core$nmi_req_set_not_clear,
       core$set_watch_tohost_watch_tohost;

  // ports of submodule ndmIfc
  wire ndmIfc$ASSERT_IN, ndmIfc$OUT_RST;

  // ports of submodule tv_xactor
  wire [607 : 0] tv_xactor$axi_out_tdata, tv_xactor$tv_in_put;
  wire [75 : 0] tv_xactor$axi_out_tkeep, tv_xactor$axi_out_tstrb;
  wire tv_xactor$EN_tv_in_put,
       tv_xactor$RDY_tv_in_put,
       tv_xactor$axi_out_tlast,
       tv_xactor$axi_out_tready,
       tv_xactor$axi_out_tvalid;

  // rule scheduling signals
  wire CAN_FIRE_RL_coreInReset_isResetAssertedUpdate,
       CAN_FIRE_RL_decNdmCountRl,
       CAN_FIRE_RL_mkConnectionGetPut,
       CAN_FIRE_RL_rl_always,
       CAN_FIRE_RL_rl_once,
       CAN_FIRE_RL_rl_reset_response,
       CAN_FIRE_debug_m_arvalid,
       CAN_FIRE_debug_m_awvalid,
       CAN_FIRE_debug_m_bready,
       CAN_FIRE_debug_m_rready,
       CAN_FIRE_debug_m_wvalid,
       CAN_FIRE_interrupt_reqs,
       CAN_FIRE_master1_hrdata,
       CAN_FIRE_master1_hready,
       CAN_FIRE_master1_hresp,
       CAN_FIRE_set_watch_tohost,
       CAN_FIRE_tv_verifier_info_tx_m_tready,
       WILL_FIRE_RL_coreInReset_isResetAssertedUpdate,
       WILL_FIRE_RL_decNdmCountRl,
       WILL_FIRE_RL_mkConnectionGetPut,
       WILL_FIRE_RL_rl_always,
       WILL_FIRE_RL_rl_once,
       WILL_FIRE_RL_rl_reset_response,
       WILL_FIRE_debug_m_arvalid,
       WILL_FIRE_debug_m_awvalid,
       WILL_FIRE_debug_m_bready,
       WILL_FIRE_debug_m_rready,
       WILL_FIRE_debug_m_wvalid,
       WILL_FIRE_interrupt_reqs,
       WILL_FIRE_master1_hrdata,
       WILL_FIRE_master1_hready,
       WILL_FIRE_master1_hresp,
       WILL_FIRE_set_watch_tohost,
       WILL_FIRE_tv_verifier_info_tx_m_tready;

  // value method master1_haddr
  assign master1_HADDR = core$cpu_dmem_master_HADDR ;

  // value method master1_hburst
  assign master1_HBURST = core$cpu_dmem_master_HBURST ;

  // value method master1_hmastlock
  assign master1_HMASTLOCK = core$cpu_dmem_master_HMASTLOCK ;

  // value method master1_hprot
  assign master1_HPROT = core$cpu_dmem_master_HPROT ;

  // value method master1_hsize
  assign master1_HSIZE = core$cpu_dmem_master_HSIZE ;

  // value method master1_htrans
  assign master1_HTRANS = core$cpu_dmem_master_HTRANS ;

  // value method master1_hwdata
  assign master1_HWDATA = core$cpu_dmem_master_HWDATA ;

  // value method master1_hwrite
  assign master1_HWRITE = core$cpu_dmem_master_HWRITE ;

  // action method master1_hrdata
  assign CAN_FIRE_master1_hrdata = 1'd1 ;
  assign WILL_FIRE_master1_hrdata = 1'd1 ;

  // action method master1_hready
  assign CAN_FIRE_master1_hready = 1'd1 ;
  assign WILL_FIRE_master1_hready = 1'd1 ;

  // action method master1_hresp
  assign CAN_FIRE_master1_hresp = 1'd1 ;
  assign WILL_FIRE_master1_hresp = 1'd1 ;

  // action method interrupt_reqs
  assign CAN_FIRE_interrupt_reqs = 1'd1 ;
  assign WILL_FIRE_interrupt_reqs = 1'd1 ;

  // value method tv_verifier_info_tx_m_tvalid
  assign tv_verifier_info_tx_tvalid = tv_xactor$axi_out_tvalid ;

  // value method tv_verifier_info_tx_m_tdata
  assign tv_verifier_info_tx_tdata = tv_xactor$axi_out_tdata ;

  // value method tv_verifier_info_tx_m_tstrb
  assign tv_verifier_info_tx_tstrb = tv_xactor$axi_out_tstrb ;

  // value method tv_verifier_info_tx_m_tkeep
  assign tv_verifier_info_tx_tkeep = tv_xactor$axi_out_tkeep ;

  // value method tv_verifier_info_tx_m_tlast
  assign tv_verifier_info_tx_tlast = tv_xactor$axi_out_tlast ;

  // action method tv_verifier_info_tx_m_tready
  assign CAN_FIRE_tv_verifier_info_tx_m_tready = 1'd1 ;
  assign WILL_FIRE_tv_verifier_info_tx_m_tready = 1'd1 ;

  // action method debug_m_awvalid
  assign CAN_FIRE_debug_m_awvalid = 1'd1 ;
  assign WILL_FIRE_debug_m_awvalid = 1'd1 ;

  // value method debug_m_awready
  assign debug_awready = core$debug_awready ;

  // action method debug_m_wvalid
  assign CAN_FIRE_debug_m_wvalid = 1'd1 ;
  assign WILL_FIRE_debug_m_wvalid = 1'd1 ;

  // value method debug_m_wready
  assign debug_wready = core$debug_wready ;

  // value method debug_m_bvalid
  assign debug_bvalid = core$debug_bvalid ;

  // value method debug_m_bid
  assign debug_bid = core$debug_bid ;

  // value method debug_m_bresp
  assign debug_bresp = core$debug_bresp ;

  // action method debug_m_bready
  assign CAN_FIRE_debug_m_bready = 1'd1 ;
  assign WILL_FIRE_debug_m_bready = 1'd1 ;

  // action method debug_m_arvalid
  assign CAN_FIRE_debug_m_arvalid = 1'd1 ;
  assign WILL_FIRE_debug_m_arvalid = 1'd1 ;

  // value method debug_m_arready
  assign debug_arready = core$debug_arready ;

  // value method debug_m_rvalid
  assign debug_rvalid = core$debug_rvalid ;

  // value method debug_m_rid
  assign debug_rid = core$debug_rid ;

  // value method debug_m_rdata
  assign debug_rdata = core$debug_rdata ;

  // value method debug_m_rresp
  assign debug_rresp = core$debug_rresp ;

  // value method debug_m_rlast
  assign debug_rlast = core$debug_rlast ;

  // action method debug_m_rready
  assign CAN_FIRE_debug_m_rready = 1'd1 ;
  assign WILL_FIRE_debug_m_rready = 1'd1 ;

  // action method set_watch_tohost
  assign RDY_set_watch_tohost = 1'd1 ;
  assign CAN_FIRE_set_watch_tohost = 1'd1 ;
  assign WILL_FIRE_set_watch_tohost = EN_set_watch_tohost ;

  // value method mv_tohost_value
  assign mv_tohost_value = core$mv_tohost_value ;
  assign RDY_mv_tohost_value = 1'd1 ;

  // submodule core
  mkCore core(.CLK(CLK),
	      .RST_N(ndmIfc$OUT_RST),
	      .core_external_interrupt_sources_0_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_0_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_10_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_10_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_11_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_11_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_12_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_12_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_13_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_13_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_14_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_14_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_15_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_15_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_1_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_1_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_2_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_2_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_3_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_3_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_4_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_4_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_5_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_5_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_6_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_6_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_7_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_7_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_8_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_8_m_interrupt_req_set_not_clear),
	      .core_external_interrupt_sources_9_m_interrupt_req_set_not_clear(core$core_external_interrupt_sources_9_m_interrupt_req_set_not_clear),
	      .cpu_dmem_master_HRDATA(core$cpu_dmem_master_HRDATA),
	      .cpu_dmem_master_HREADY(core$cpu_dmem_master_HREADY),
	      .cpu_dmem_master_HRESP(core$cpu_dmem_master_HRESP),
	      .cpu_reset_server_request_put(core$cpu_reset_server_request_put),
	      .debug_araddr(core$debug_araddr),
	      .debug_arburst(core$debug_arburst),
	      .debug_arcache(core$debug_arcache),
	      .debug_arid(core$debug_arid),
	      .debug_arlen(core$debug_arlen),
	      .debug_arlock(core$debug_arlock),
	      .debug_arprot(core$debug_arprot),
	      .debug_arqos(core$debug_arqos),
	      .debug_arregion(core$debug_arregion),
	      .debug_arsize(core$debug_arsize),
	      .debug_arvalid(core$debug_arvalid),
	      .debug_awaddr(core$debug_awaddr),
	      .debug_awburst(core$debug_awburst),
	      .debug_awcache(core$debug_awcache),
	      .debug_awid(core$debug_awid),
	      .debug_awlen(core$debug_awlen),
	      .debug_awlock(core$debug_awlock),
	      .debug_awprot(core$debug_awprot),
	      .debug_awqos(core$debug_awqos),
	      .debug_awregion(core$debug_awregion),
	      .debug_awsize(core$debug_awsize),
	      .debug_awvalid(core$debug_awvalid),
	      .debug_bready(core$debug_bready),
	      .debug_rready(core$debug_rready),
	      .debug_wdata(core$debug_wdata),
	      .debug_wlast(core$debug_wlast),
	      .debug_wstrb(core$debug_wstrb),
	      .debug_wvalid(core$debug_wvalid),
	      .nmi_req_set_not_clear(core$nmi_req_set_not_clear),
	      .set_verbosity_logdelay(core$set_verbosity_logdelay),
	      .set_verbosity_verbosity(core$set_verbosity_verbosity),
	      .set_watch_tohost_tohost_addr(core$set_watch_tohost_tohost_addr),
	      .set_watch_tohost_watch_tohost(core$set_watch_tohost_watch_tohost),
	      .EN_cpu_reset_server_request_put(core$EN_cpu_reset_server_request_put),
	      .EN_cpu_reset_server_response_get(core$EN_cpu_reset_server_response_get),
	      .EN_tv_verifier_info_get_get(core$EN_tv_verifier_info_get_get),
	      .EN_set_verbosity(core$EN_set_verbosity),
	      .EN_set_watch_tohost(core$EN_set_watch_tohost),
	      .RDY_cpu_reset_server_request_put(core$RDY_cpu_reset_server_request_put),
	      .cpu_reset_server_response_get(core$cpu_reset_server_response_get),
	      .RDY_cpu_reset_server_response_get(core$RDY_cpu_reset_server_response_get),
	      .cpu_dmem_master_HADDR(core$cpu_dmem_master_HADDR),
	      .cpu_dmem_master_HBURST(core$cpu_dmem_master_HBURST),
	      .cpu_dmem_master_HMASTLOCK(core$cpu_dmem_master_HMASTLOCK),
	      .cpu_dmem_master_HPROT(core$cpu_dmem_master_HPROT),
	      .cpu_dmem_master_HSIZE(core$cpu_dmem_master_HSIZE),
	      .cpu_dmem_master_HTRANS(core$cpu_dmem_master_HTRANS),
	      .cpu_dmem_master_HWDATA(core$cpu_dmem_master_HWDATA),
	      .cpu_dmem_master_HWRITE(core$cpu_dmem_master_HWRITE),
	      .tv_verifier_info_get_get(core$tv_verifier_info_get_get),
	      .RDY_tv_verifier_info_get_get(core$RDY_tv_verifier_info_get_get),
	      .debug_awready(core$debug_awready),
	      .debug_wready(core$debug_wready),
	      .debug_bvalid(core$debug_bvalid),
	      .debug_bid(core$debug_bid),
	      .debug_bresp(core$debug_bresp),
	      .debug_arready(core$debug_arready),
	      .debug_rvalid(core$debug_rvalid),
	      .debug_rid(core$debug_rid),
	      .debug_rdata(core$debug_rdata),
	      .debug_rresp(core$debug_rresp),
	      .debug_rlast(core$debug_rlast),
	      .RDY_set_verbosity(),
	      .RDY_set_watch_tohost(),
	      .mv_tohost_value(core$mv_tohost_value),
	      .RDY_mv_tohost_value());

  // submodule ndmIfc
  MakeResetA #(.RSTDELAY(32'd2), .init(1'd0)) ndmIfc(.CLK(CLK),
						     .RST(RST_N),
						     .DST_CLK(CLK),
						     .ASSERT_IN(ndmIfc$ASSERT_IN),
						     .ASSERT_OUT(),
						     .OUT_RST(ndmIfc$OUT_RST));

  // submodule tv_xactor
  mkTV_Xactor tv_xactor(.CLK(CLK),
			.RST_N(RST_N),
			.axi_out_tready(tv_xactor$axi_out_tready),
			.tv_in_put(tv_xactor$tv_in_put),
			.EN_tv_in_put(tv_xactor$EN_tv_in_put),
			.RDY_tv_in_put(tv_xactor$RDY_tv_in_put),
			.axi_out_tvalid(tv_xactor$axi_out_tvalid),
			.axi_out_tdata(tv_xactor$axi_out_tdata),
			.axi_out_tstrb(tv_xactor$axi_out_tstrb),
			.axi_out_tkeep(tv_xactor$axi_out_tkeep),
			.axi_out_tlast(tv_xactor$axi_out_tlast));

  // rule RL_rl_always
  assign CAN_FIRE_RL_rl_always = 1'd1 ;
  assign WILL_FIRE_RL_rl_always = 1'd1 ;

  // rule RL_decNdmCountRl
  assign CAN_FIRE_RL_decNdmCountRl = rg_ndm_count != 6'd0 ;
  assign WILL_FIRE_RL_decNdmCountRl = CAN_FIRE_RL_decNdmCountRl ;

  // rule RL_rl_once
  assign CAN_FIRE_RL_rl_once =
	     core$RDY_cpu_reset_server_request_put && !rg_once &&
	     !coreInReset_isInReset ;
  assign WILL_FIRE_RL_rl_once = CAN_FIRE_RL_rl_once ;

  // rule RL_rl_reset_response
  assign CAN_FIRE_RL_rl_reset_response =
	     rg_ndm_count == 6'd0 && core$RDY_cpu_reset_server_response_get ;
  assign WILL_FIRE_RL_rl_reset_response = CAN_FIRE_RL_rl_reset_response ;

  // rule RL_mkConnectionGetPut
  assign CAN_FIRE_RL_mkConnectionGetPut =
	     core$RDY_tv_verifier_info_get_get && tv_xactor$RDY_tv_in_put ;
  assign WILL_FIRE_RL_mkConnectionGetPut = CAN_FIRE_RL_mkConnectionGetPut ;

  // rule RL_coreInReset_isResetAssertedUpdate
  assign CAN_FIRE_RL_coreInReset_isResetAssertedUpdate =
	     coreInReset_isInReset ;
  assign WILL_FIRE_RL_coreInReset_isResetAssertedUpdate =
	     coreInReset_isInReset ;

  // register coreInReset_isInReset
  assign coreInReset_isInReset$D_IN = 1'd0 ;
  assign coreInReset_isInReset$EN = coreInReset_isInReset ;

  // register rg_last_cpuh
  assign rg_last_cpuh$D_IN = 1'b0 ;
  assign rg_last_cpuh$EN = 1'b0 ;

  // register rg_ldr_reset
  assign rg_ldr_reset$D_IN = 2'd0 ;
  assign rg_ldr_reset$EN = CAN_FIRE_RL_rl_once ;

  // register rg_ndm_count
  assign rg_ndm_count$D_IN = rg_ndm_count - 6'd1 ;
  assign rg_ndm_count$EN = CAN_FIRE_RL_decNdmCountRl ;

  // register rg_once
  assign rg_once$D_IN = 1'd1 ;
  assign rg_once$EN = CAN_FIRE_RL_rl_once ;

  // register rg_reset_done
  assign rg_reset_done$D_IN = 1'd1 ;
  assign rg_reset_done$EN =
	     rg_ndm_count == 6'd0 && core$RDY_cpu_reset_server_response_get ;

  // submodule core
  assign core$core_external_interrupt_sources_0_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[0] ;
  assign core$core_external_interrupt_sources_10_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[10] ;
  assign core$core_external_interrupt_sources_11_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[11] ;
  assign core$core_external_interrupt_sources_12_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[12] ;
  assign core$core_external_interrupt_sources_13_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[13] ;
  assign core$core_external_interrupt_sources_14_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[14] ;
  assign core$core_external_interrupt_sources_15_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[15] ;
  assign core$core_external_interrupt_sources_1_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[1] ;
  assign core$core_external_interrupt_sources_2_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[2] ;
  assign core$core_external_interrupt_sources_3_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[3] ;
  assign core$core_external_interrupt_sources_4_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[4] ;
  assign core$core_external_interrupt_sources_5_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[5] ;
  assign core$core_external_interrupt_sources_6_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[6] ;
  assign core$core_external_interrupt_sources_7_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[7] ;
  assign core$core_external_interrupt_sources_8_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[8] ;
  assign core$core_external_interrupt_sources_9_m_interrupt_req_set_not_clear =
	     cpu_external_interrupt_req[9] ;
  assign core$cpu_dmem_master_HRDATA = master1_HRDATA ;
  assign core$cpu_dmem_master_HREADY = master1_HREADY ;
  assign core$cpu_dmem_master_HRESP = master1_HRESP ;
  assign core$cpu_reset_server_request_put =
	     !rg_ldr_reset[1] || rg_ldr_reset[0] ;
  assign core$debug_araddr = debug_araddr ;
  assign core$debug_arburst = debug_arburst ;
  assign core$debug_arcache = debug_arcache ;
  assign core$debug_arid = debug_arid ;
  assign core$debug_arlen = debug_arlen ;
  assign core$debug_arlock = debug_arlock ;
  assign core$debug_arprot = debug_arprot ;
  assign core$debug_arqos = debug_arqos ;
  assign core$debug_arregion = debug_arregion ;
  assign core$debug_arsize = debug_arsize ;
  assign core$debug_arvalid = debug_arvalid ;
  assign core$debug_awaddr = debug_awaddr ;
  assign core$debug_awburst = debug_awburst ;
  assign core$debug_awcache = debug_awcache ;
  assign core$debug_awid = debug_awid ;
  assign core$debug_awlen = debug_awlen ;
  assign core$debug_awlock = debug_awlock ;
  assign core$debug_awprot = debug_awprot ;
  assign core$debug_awqos = debug_awqos ;
  assign core$debug_awregion = debug_awregion ;
  assign core$debug_awsize = debug_awsize ;
  assign core$debug_awvalid = debug_awvalid ;
  assign core$debug_bready = debug_bready ;
  assign core$debug_rready = debug_rready ;
  assign core$debug_wdata = debug_wdata ;
  assign core$debug_wlast = debug_wlast ;
  assign core$debug_wstrb = debug_wstrb ;
  assign core$debug_wvalid = debug_wvalid ;
  assign core$nmi_req_set_not_clear = 1'd0 ;
  assign core$set_verbosity_logdelay = 64'h0 ;
  assign core$set_verbosity_verbosity = 4'h0 ;
  assign core$set_watch_tohost_tohost_addr = set_watch_tohost_tohost_addr ;
  assign core$set_watch_tohost_watch_tohost = set_watch_tohost_watch_tohost ;
  assign core$EN_cpu_reset_server_request_put = CAN_FIRE_RL_rl_once ;
  assign core$EN_cpu_reset_server_response_get =
	     CAN_FIRE_RL_rl_reset_response ;
  assign core$EN_tv_verifier_info_get_get = CAN_FIRE_RL_mkConnectionGetPut ;
  assign core$EN_set_verbosity = 1'b0 ;
  assign core$EN_set_watch_tohost = EN_set_watch_tohost ;

  // submodule ndmIfc
  assign ndmIfc$ASSERT_IN = CAN_FIRE_RL_decNdmCountRl ;

  // submodule tv_xactor
  assign tv_xactor$axi_out_tready = tv_verifier_info_tx_tready ;
  assign tv_xactor$tv_in_put = core$tv_verifier_info_get_get ;
  assign tv_xactor$EN_tv_in_put = CAN_FIRE_RL_mkConnectionGetPut ;

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (RST_N == `BSV_RESET_VALUE)
      begin
        rg_last_cpuh <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_ldr_reset <= `BSV_ASSIGNMENT_DELAY 2'd0;
	rg_ndm_count <= `BSV_ASSIGNMENT_DELAY 6'd0;
	rg_once <= `BSV_ASSIGNMENT_DELAY 1'd0;
	rg_reset_done <= `BSV_ASSIGNMENT_DELAY 1'd0;
      end
    else
      begin
        if (rg_last_cpuh$EN)
	  rg_last_cpuh <= `BSV_ASSIGNMENT_DELAY rg_last_cpuh$D_IN;
	if (rg_ldr_reset$EN)
	  rg_ldr_reset <= `BSV_ASSIGNMENT_DELAY rg_ldr_reset$D_IN;
	if (rg_ndm_count$EN)
	  rg_ndm_count <= `BSV_ASSIGNMENT_DELAY rg_ndm_count$D_IN;
	if (rg_once$EN) rg_once <= `BSV_ASSIGNMENT_DELAY rg_once$D_IN;
	if (rg_reset_done$EN)
	  rg_reset_done <= `BSV_ASSIGNMENT_DELAY rg_reset_done$D_IN;
      end
  end

  always@(posedge CLK or `BSV_RESET_EDGE ndmIfc$OUT_RST)
  if (ndmIfc$OUT_RST == `BSV_RESET_VALUE)
    begin
      coreInReset_isInReset <= `BSV_ASSIGNMENT_DELAY 1'd1;
    end
  else
    begin
      if (coreInReset_isInReset$EN)
	coreInReset_isInReset <= `BSV_ASSIGNMENT_DELAY
	    coreInReset_isInReset$D_IN;
    end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    coreInReset_isInReset = 1'h0;
    rg_last_cpuh = 1'h0;
    rg_ldr_reset = 2'h2;
    rg_ndm_count = 6'h2A;
    rg_once = 1'h0;
    rg_reset_done = 1'h0;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on

  // handling of system tasks

  // synopsys translate_off
  always@(negedge CLK)
  begin
    #0;
    if (RST_N != `BSV_RESET_VALUE)
      if (ndmIfc$OUT_RST != `BSV_RESET_VALUE)
	if (WILL_FIRE_RL_rl_reset_response &&
	    core$cpu_reset_server_response_get)
	  $display("Trace starting");
  end
  // synopsys translate_on
endmodule  // mkBSCore

