// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package Mem_Model;

// ================================================================
// A simulation model of external DRAM memory.
// Uses a register file to model memory.

// ================================================================
// BSV library imports

import  RegFile      :: *;
import  Vector       :: *;
import  FIFOF        :: *;
import  GetPut       :: *;
import  ClientServer :: *;
import  Memory       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import Mem_Controller :: *;

// ================================================================
// Mem Model interface

interface Mem_Model_IFC;
   // The read/write interface
   interface  MemoryServer #(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word)  mem_server;
endinterface

// ================================================================
// Mem Model implementation

(* synthesize *)
module mkMem_Model (Mem_Model_IFC);

   Integer verbosity = 0;    // 0 = quiet; 1 = verbose

   Raw_Mem_Addr alloc_size = 'h_80_0000;    // 8M raw mem words, or 256MB
                   
   RegFile #(Raw_Mem_Addr, Bit #(Bits_per_Raw_Mem_Word)) rf <- mkRegFileLoad ("Mem.hex", 0, alloc_size - 1);

   FIFOF #(MemoryResponse #(Bits_per_Raw_Mem_Word))  f_raw_mem_rsps <- mkFIFOF;

   // ----------------------------------------------------------------
   // INTERFACE

   interface MemoryServer mem_server;
      interface Put request;
	 method Action put (MemoryRequest  #(Bits_per_Raw_Mem_Addr, Bits_per_Raw_Mem_Word) req);
	    if (req.address >= alloc_size) begin
	       $display ("%0d: ERROR: Mem_Model.request.put: addr 0x%0h >= size 0x%0h (num raw-mem words)",
			 cur_cycle, req.address, alloc_size);
	       $finish (1);    // Assertion failure: address out of bounds
	    end
	    else if (req.write) begin
	       rf.upd (req.address, req.data);
	       if (verbosity != 0)
		  $display ("%0d: Mem_Model write [0x%0h] <= 0x%0h", cur_cycle, req.address, req.data);
	    end
	    else begin
	       let x = rf.sub (req.address);
	       let rsp = MemoryResponse {data: x};
	       f_raw_mem_rsps.enq (rsp);
	       if (verbosity != 0)
		  $display ("%0d: Mem_Model read  [0x%0h] => 0x%0h", cur_cycle, req.address, x);
	    end
	 endmethod
      endinterface

      interface Get response = toGet (f_raw_mem_rsps);
   endinterface
endmodule

// ================================================================

endpackage
