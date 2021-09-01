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
// From Bluespec RISC-V CPUs

import AXI_Widths   :: *;

// ================================================================
// Options for cache-coherent access for devices
// Default is OPTION_DMA_CACHE, override with OPTION_L2_COHERENT_DMA_PORT

// OPTION_DMA_CACHE    OPTION_L2_COHERENT_DMA_PORT
//     defined              defined                => OPTION_L2_COHERENT_DMA_PORT
//     defined            undefined                => OPTION_DMA_CACHE
//   undefined              defined                => OPTION_L2_COHERENT_DMA_PORT
//   undefined            undefined                => OPTION_DMA_CACHE

// Default is OPTION_DMA_CACHE, unless overridden by explicit OPTION_L2_COHERENT_DMA_PORT
`ifdef OPTION_DMA_CACHE

`ifdef OPTION_L2_COHERENT_DMA_PORT
`undef OPTION_DMA_CACHE
`endif

`else

`ifndef OPTION_L2_COHERENT_DMA_PORT
`define OPTION_DMA_CACHE
`endif

`endif

// ================================================================
// From L1CoCache.bsv

`ifdef OPTION_DMA_CACHE
// When device's coherent port comes from DMA_Cache
typedef TAdd #(TMul#(CoreNum, 2), 1) L1Num;    // per core (I-Cache, D-Cache) and DMA-Cache
`endif

`ifdef OPTION_L2_COHERENT_DMA_PORT
// When device's coherent port connects directly to LLC.dma
typedef TMul#(CoreNum, 2) L1Num;    // per core (I-Cache, D-Cache)
`endif

typedef `LOG_L1_WAYS LgL1WayNum;
typedef Bit#(LgL1WayNum) L1Way;

// ================================================================
// Id for LLC's coherent cache port
// This is simply the AXI4 awid/arid

typedef Bit #(Wd_Id_Dma) LLCDmaReqId;

// ================================================================

endpackage
