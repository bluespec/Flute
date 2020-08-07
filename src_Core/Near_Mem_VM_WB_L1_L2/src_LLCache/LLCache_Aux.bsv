// These are some minimal excerpts of code from certain files (noted
// below) of Toooba/RISCY-OOO needed for LLCache.bsv.

// The files named here themselves import other files which, in turn,
// import more files, with a large transitive closure.  These excerpts
// are the minimum defs actually needed by LLCache, and avoid
// importing all those otherwise unnecessary files.

package LLCache_Aux;

`include "ProcConfig.bsv"

// ----------------
// From RISCY-OOO

import Types::*;
import ProcTypes::*;
import CCTypes::*;

// ----------------
// From Piccolo/Flute

import Near_Mem_IFC :: *;    // For Wd_Id_Dma

// ================================================================
// From L1CoCache.bsv

typedef TMul#(CoreNum, 2) L1Num;
typedef `LOG_L1_WAYS LgL1WayNum;
typedef Bit#(LgL1WayNum) L1Way;

// ================================================================
// Id for LLC's coherent cache port
// This is simply the AXI4 awid/arid

typedef Bit #(Wd_Id_Dma) LLCDmaReqId;

// ================================================================

endpackage
