// Copyright (c) 2007--2011 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package AxiMonitor;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import AxiDefines::*;
import AxiPC::*;
import TLM3::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface AxiMonitor#(`TLM_PRM_DCL);
endinterface

module mkAxiMonitor#(Bool include_pc,
		     AxiWrMaster#(`TLM_PRM) master_wr,
		     AxiWrSlave#(`TLM_PRM)  slave_wr,
		     AxiRdMaster#(`TLM_PRM) master_rd,
		     AxiRdSlave#(`TLM_PRM)  slave_rd) (AxiMonitor#(`TLM_PRM));

   if (include_pc && genVerilog)
      begin

	 AxiPC_Ifc#(`TLM_PRM) checker <- mkAxiPC;

	 ////////////////////////////////////////////////////////////////////////////////
	 /// Protocol Checker connections;
	 ////////////////////////////////////////////////////////////////////////////////

	 rule connect_checker;

	    checker.aw_id(master_wr.awID);
	    checker.aw_len(master_wr.awLEN);
	    checker.aw_size(master_wr.awSIZE);
	    checker.aw_burst(master_wr.awBURST);
	    checker.aw_lock(master_wr.awLOCK);
	    checker.aw_cache(master_wr.awCACHE);
	    checker.aw_prot(master_wr.awPROT);
	    checker.aw_addr(master_wr.awADDR);
	    checker.aw_valid(master_wr.awVALID);
	    checker.aw_ready(slave_wr.awREADY);

	    checker.w_id(master_wr.wID);
	    checker.w_data(master_wr.wDATA);
	    checker.w_strb(master_wr.wSTRB);
	    checker.w_last(master_wr.wLAST);
	    checker.w_valid(master_wr.wVALID);
	    checker.w_ready(slave_wr.wREADY);

	    checker.b_id(slave_wr.bID);
	    checker.b_resp(slave_wr.bRESP);
	    checker.b_valid(slave_wr.bVALID);
	    checker.b_ready(master_wr.bREADY);

	    checker.ar_id(master_rd.arID);
	    checker.ar_len(master_rd.arLEN);
	    checker.ar_size(master_rd.arSIZE);
	    checker.ar_burst(master_rd.arBURST);
	    checker.ar_lock(master_rd.arLOCK);
	    checker.ar_cache(master_rd.arCACHE);
	    checker.ar_prot(master_rd.arPROT);
	    checker.ar_addr(master_rd.arADDR);
	    checker.ar_valid(master_rd.arVALID);
        checker.ar_ready(slave_rd.arREADY);

	    checker.r_id(slave_rd.rID);
	    checker.r_data(slave_rd.rDATA);
	    checker.r_resp(slave_rd.rRESP);
	    checker.r_last(slave_rd.rLAST);
	    checker.r_valid(slave_rd.rVALID);
	    checker.r_ready(master_rd.rREADY);

	 endrule
      end

endmodule

endpackage
