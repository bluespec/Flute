// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package MMU_Cache_Common;

// ================================================================
// Types etc. shared by multiple modules in MMU_Cache complex.

// ================================================================
// Project imports

import ISA_Decls   :: *;
import Fabric_Defs :: *;

// ================================================================
// Near_Mem opcodes

typedef enum {  CACHE_LD
	      , CACHE_ST
`ifdef ISA_A
	      , CACHE_AMO
`endif
   } CacheOp
deriving (Bits, Eq, FShow);

// ================================================================
// Check if addr is aligned

function Bool fn_is_aligned (Bit #(3) f3, Bit #(n) addr);
   return (    (f3 [1:0] == 2'b00)                                // B, BU
	   || ((f3 [1:0] == 2'b01) && (addr [0] == 1'b0))         // H, HU
	   || ((f3 [1:0] == 2'b10) && (addr [1:0] == 2'b00))      // W, WU
	   || ((f3 [1:0] == 2'b11) && (addr [2:0] == 3'b000))     // D
	   );
endfunction

// ================================================================
// Convert width of an address from PA to Fabric_Addr

function Fabric_Addr fn_PA_to_Fabric_Addr (PA pa);
   Bit #(TAdd #(Wd_Addr, PA_sz)) fa = zeroExtend (pa);
   Integer hi = valueOf (Wd_Addr) - 1;
   return fa [hi:0];
endfunction

// ================================================================

endpackage
