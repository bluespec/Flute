// Copyright (c) 2013-2019 Bluespec, Inc. All Rights Reserved.

package GetPut_Aux;

// ================================================================
// Misc. additional useful definitions on FIFOs, Gets and Puts

// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// A convenience function to 'pop' a value from a FIFO, FIFOF, ...

function ActionValue #(t) pop (ifc f)
   provisos (ToGet #(ifc, t));
   return toGet (f).get;
endfunction

// ================================================================
// No-op stubs for Get, Put, Client and Server interfaces.
// 'get' is never enabled.
// 'put' is always enabled and just discards its argument.

Get #(t) getstub = interface Get;
		      method ActionValue #(t) get () if (False);
			 return ?;
		      endmethod
		   endinterface;

Put #(t) putstub = interface Put;
		      method Action put (t x) if (True);
			 noAction;
		      endmethod
		   endinterface;

Client #(t1,t2) client_stub = interface Client;
				 interface request  = getstub;
				 interface response = putstub;
			      endinterface;

Server #(t1,t2) server_stub = interface Server;
				 interface request  = putstub;
				 interface response = getstub;
			      endinterface;

// ================================================================
// For debugging, a convenience function to display full/empty status of a FIFO

function Action fa_show_FIFOF_state (String s, FIFOF #(t) f);
   action
      $write ("%s:", s);
      if (! f.notEmpty) $display (" Empty");
      else if (! f.notFull) $display (" Full");
      else $display (" neither empty nor full");
   endaction
endfunction

// ================================================================
// DiscardFIFOF
//     enqueue side: always ready, and discards everything
//     dequeue side: always empty, never returns anything

module mkDiscardFIFOF (FIFOF #(t));
   method Action  enq (t x);
      noAction;
   endmethod

   method Bool notFull;
      return True;
   endmethod

   method t first () if (False);
      return ?;
   endmethod

   method Action deq () if (False);
      noAction;
   endmethod

   method Bool notEmpty;
      return False;
   endmethod

   method Action clear;
      noAction;
   endmethod
endmodule

// ================================================================
// dummy_FIFO interface
//     enqueue side: never ready;
//     dequeue side: never ready

FIFOF #(t) dummy_FIFOF = interface FIFOF;
			    method Action enq (x) if (False);
			       noAction;
			    endmethod

			    method notFull;
			       return False;
			    endmethod

			    method first () if (False);
			       return ?;
			    endmethod

			    method Action deq () if (False);
			       noAction;
			    endmethod

			    method notEmpty;
			       return False;
			    endmethod

			    method Action clear;
			       noAction;
			    endmethod
			 endinterface;

// ================================================================

endpackage
