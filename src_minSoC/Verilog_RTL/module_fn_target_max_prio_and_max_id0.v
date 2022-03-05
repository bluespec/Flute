//
// Generated by Bluespec Compiler, version 2021.07-22-g61dc0ebb (build 61dc0ebb)
//
//
// Ports:
// Name                         I/O  size props
// fn_target_max_prio_and_max_id0  O     8
// fn_target_max_prio_and_max_id0_vrg_source_ip  I    17
// fn_target_max_prio_and_max_id0_vvrg_ie  I    17
// fn_target_max_prio_and_max_id0_vrg_source_prio  I    51
// fn_target_max_prio_and_max_id0_target_id  I     5 unused
//
// Combinational paths from inputs to outputs:
//   (fn_target_max_prio_and_max_id0_vrg_source_ip,
//    fn_target_max_prio_and_max_id0_vvrg_ie,
//    fn_target_max_prio_and_max_id0_vrg_source_prio,
//    fn_target_max_prio_and_max_id0_target_id) -> fn_target_max_prio_and_max_id0
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

module module_fn_target_max_prio_and_max_id0(fn_target_max_prio_and_max_id0_vrg_source_ip,
					     fn_target_max_prio_and_max_id0_vvrg_ie,
					     fn_target_max_prio_and_max_id0_vrg_source_prio,
					     fn_target_max_prio_and_max_id0_target_id,
					     fn_target_max_prio_and_max_id0);
  // value method fn_target_max_prio_and_max_id0
  input  [16 : 0] fn_target_max_prio_and_max_id0_vrg_source_ip;
  input  [16 : 0] fn_target_max_prio_and_max_id0_vvrg_ie;
  input  [50 : 0] fn_target_max_prio_and_max_id0_vrg_source_prio;
  input  [4 : 0] fn_target_max_prio_and_max_id0_target_id;
  output [7 : 0] fn_target_max_prio_and_max_id0;

  // signals for module outputs
  wire [7 : 0] fn_target_max_prio_and_max_id0;

  // remaining internal signals
  wire [7 : 0] IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d163,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d165,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d167,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d169,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d171,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d173,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d175;
  wire [2 : 0] IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d103,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d110,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d117,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d124,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d131,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d40,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d47,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d54,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d61,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d68,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d75,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d82,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d89,
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d96;
  wire fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d102,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d109,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d116,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d123,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d130,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d137,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d46,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d53,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d60,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d67,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d74,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d81,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d88,
       fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d95;

  // value method fn_target_max_prio_and_max_id0
  assign fn_target_max_prio_and_max_id0 =
	     (fn_target_max_prio_and_max_id0_vrg_source_ip[16] &&
	      fn_target_max_prio_and_max_id0_vrg_source_prio[50:48] >
	      (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d137 ?
		 fn_target_max_prio_and_max_id0_vrg_source_prio[47:45] :
		 IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d131) &&
	      fn_target_max_prio_and_max_id0_vvrg_ie[16]) ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[50:48],
		 5'd16 } :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d175 ;

  // remaining internal signals
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d103 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d102 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[32:30] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d96 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d110 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d109 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[35:33] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d103 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d117 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d116 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[38:36] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d110 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d124 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d123 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[41:39] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d117 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d131 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d130 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[44:42] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d124 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d163 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d53 ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[11:9],
		 5'd3 } :
	       (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d46 ?
		  { fn_target_max_prio_and_max_id0_vrg_source_prio[8:6],
		    5'd2 } :
		  ((fn_target_max_prio_and_max_id0_vrg_source_ip[1] &&
		    fn_target_max_prio_and_max_id0_vrg_source_prio[5:3] !=
		    3'd0 &&
		    fn_target_max_prio_and_max_id0_vvrg_ie[1]) ?
		     { fn_target_max_prio_and_max_id0_vrg_source_prio[5:3],
		       5'd1 } :
		     8'd0)) ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d165 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d67 ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[17:15],
		 5'd5 } :
	       (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d60 ?
		  { fn_target_max_prio_and_max_id0_vrg_source_prio[14:12],
		    5'd4 } :
		  IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d163) ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d167 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d81 ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[23:21],
		 5'd7 } :
	       (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d74 ?
		  { fn_target_max_prio_and_max_id0_vrg_source_prio[20:18],
		    5'd6 } :
		  IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d165) ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d169 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d95 ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[29:27],
		 5'd9 } :
	       (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d88 ?
		  { fn_target_max_prio_and_max_id0_vrg_source_prio[26:24],
		    5'd8 } :
		  IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d167) ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d171 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d109 ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[35:33],
		 5'd11 } :
	       (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d102 ?
		  { fn_target_max_prio_and_max_id0_vrg_source_prio[32:30],
		    5'd10 } :
		  IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d169) ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d173 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d123 ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[41:39],
		 5'd13 } :
	       (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d116 ?
		  { fn_target_max_prio_and_max_id0_vrg_source_prio[38:36],
		    5'd12 } :
		  IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d171) ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d175 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d137 ?
	       { fn_target_max_prio_and_max_id0_vrg_source_prio[47:45],
		 5'd15 } :
	       (fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d130 ?
		  { fn_target_max_prio_and_max_id0_vrg_source_prio[44:42],
		    5'd14 } :
		  IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d173) ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d40 =
	     (fn_target_max_prio_and_max_id0_vrg_source_ip[1] &&
	      fn_target_max_prio_and_max_id0_vrg_source_prio[5:3] != 3'd0 &&
	      fn_target_max_prio_and_max_id0_vvrg_ie[1]) ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[5:3] :
	       3'd0 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d47 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d46 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[8:6] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d40 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d54 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d53 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[11:9] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d47 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d61 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d60 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[14:12] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d54 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d68 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d67 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[17:15] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d61 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d75 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d74 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[20:18] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d68 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d82 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d81 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[23:21] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d75 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d89 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d88 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[26:24] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d82 ;
  assign IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d96 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d95 ?
	       fn_target_max_prio_and_max_id0_vrg_source_prio[29:27] :
	       IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d89 ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d102 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[10] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[32:30] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d96 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[10] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d109 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[11] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[35:33] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d103 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[11] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d116 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[12] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[38:36] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d110 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[12] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d123 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[13] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[41:39] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d117 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[13] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d130 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[14] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[44:42] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d124 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[14] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d137 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[15] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[47:45] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d131 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[15] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d46 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[2] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[8:6] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d40 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[2] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d53 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[3] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[11:9] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d47 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[3] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d60 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[4] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[14:12] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d54 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[4] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d67 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[5] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[17:15] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d61 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[5] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d74 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[6] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[20:18] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d68 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[6] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d81 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[7] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[23:21] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d75 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[7] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d88 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[8] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[26:24] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d82 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[8] ;
  assign fn_target_max_prio_and_max_id0_vrg_source_ip_B_ETC___d95 =
	     fn_target_max_prio_and_max_id0_vrg_source_ip[9] &&
	     fn_target_max_prio_and_max_id0_vrg_source_prio[29:27] >
	     IF_fn_target_max_prio_and_max_id0_vrg_source_i_ETC___d89 &&
	     fn_target_max_prio_and_max_id0_vvrg_ie[9] ;
endmodule  // module_fn_target_max_prio_and_max_id0

