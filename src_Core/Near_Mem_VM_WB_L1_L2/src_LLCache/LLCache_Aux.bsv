// These are some minimal excerpts of code from certain files (noted
// below) of Toooba/RISCY-OOO needed for LLCache.bsv.

// The files named here themselves import other files which, in turn,
// import more files, with a large transitive closure.  These excerpts
// are the minimum defs actually needed by LLCache, and avoid
// importing all those otherwise unnecessary files.

package LLCache_Aux;

`include "ProcConfig.bsv"

import Types::*;
import ProcTypes::*;
import CCTypes::*;

// ================================================================
// From TlbTypes.bsv

typedef `L2TLB_REQ_NUM L2TlbReqNum;
typedef Bit#(TLog#(L2TlbReqNum)) L2TlbReqIdx;

// ================================================================
// From L2Tlb.bsv

typedef L2TlbReqIdx TlbMemReqId;

// ================================================================
// From MemLoader.bsv

typedef void MemLoaderMemReqId;

// ================================================================
// From LLCDmaConnect.bsv

typedef struct {
    CoreId core;
    TlbMemReqId id;
    LineDataOffset dataSel;
} TlbDmaReqId deriving(Bits, Eq, FShow);

typedef union tagged {
    MemLoaderMemReqId MemLoader;
    TlbDmaReqId Tlb;
} LLCDmaReqId deriving(Bits, Eq, FShow);

// ================================================================
// From L1CoCache.bsv

typedef TMul#(CoreNum, 2) L1Num;
typedef `LOG_L1_WAYS LgL1WayNum;
typedef Bit#(LgL1WayNum) L1Way;

// ================================================================

endpackage
