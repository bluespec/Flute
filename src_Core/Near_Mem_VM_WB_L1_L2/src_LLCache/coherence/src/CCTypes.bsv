
// Copyright (c) 2017 Massachusetts Institute of Technology
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Types::*; // import from RISCY repo
import MemoryTypes::*; // import from RISCY repo
import Vector::*;
import FShow::*;
import CacheUtils::*;
import Assert::*;
import Connectable::*;
import GetPut::*;
import ClientServer::*;

typedef enum {
    I = 2'd0, 
    S = 2'd1, 
    E = 2'd2, 
    M = 2'd3
} MESI deriving(Bits, Eq, FShow);
typedef MESI Msi;

instance Ord#(MESI);
    function Bool \< ( MESI x, MESI y );
        return pack(x) < pack(y);
    endfunction
    function Bool \<= ( MESI x, MESI y );
        return pack(x) <= pack(y);
    endfunction
    function Bool \> ( MESI x, MESI y );
        return pack(x) > pack(y);
    endfunction
    function Bool \>= ( MESI x, MESI y );
        return pack(x) >= pack(y);
    endfunction
    function Ordering compare( MESI x, MESI y );
        return compare(pack(x), pack(y));
    endfunction
    function MESI min( MESI x, MESI y );
        return x < y ? x : y;
    endfunction
    function MESI max( MESI x, MESI y );
        return x > y ? x : y;
    endfunction
endinstance

// whether cache state is enough to serice upgrade req, i.e., no
// need to req parent
function Bool enoughCacheState(Msi cs, Msi to);
    return cs >= to || cs >= E;
endfunction

// the maximum dir state that a peer can have so that it will not
// be downgraded to service upgrade req to state x
function Msi toCompat(Msi x);
    return x == S ? S : I;
endfunction

typedef TDiv#(DataSz, 8) DataSzBytes;
typedef TLog#(DataSzBytes) LgDataSzBytes;
typedef Bit#(LgDataSzBytes) DataBytesOffset;

typedef TDiv#(InstSz, 8) InstSzBytes;
typedef TLog#(InstSzBytes) LgInstSzBytes;

// 64B cache line -- XXX same with parameters in CacheUtils.bsv
typedef CacheUtils::LogCLineNumData LgLineSzData;
typedef CacheUtils::LogCLineNumBytes LgLineSzBytes;
typedef CacheUtils::CLineAddrSz LineAddrSz;
typedef CacheUtils::CLineAddr LineAddr;

typedef CacheUtils::CLineNumData LineSzData;
typedef CacheUtils::CLineDataSel LineDataOffset;

typedef CacheUtils::CLineNumBytes LineSzBytes;
typedef CacheUtils::CLineByteEn LineByteEn;

typedef TDiv#(LineSzBytes, InstSzBytes) LineSzInst;
typedef Bit#(TLog#(LineSzInst)) LineInstOffset;

typedef Vector#(LineSzData, Data) Line;

function LineAddr getLineAddr(Addr addr) = truncateLSB(addr);

function LineDataOffset getLineDataOffset(Addr a);
    return truncate(a >> valueOf(LgDataSzBytes));
endfunction

function LineInstOffset getLineInstOffset(Addr a);
    return truncate(a >> valueof(LgInstSzBytes));
endfunction

function Line getUpdatedLine(Line curLine, LineByteEn wrBE, Line wrLine);
    Vector#(LineSzBytes, Bit#(8)) curVec = unpack(pack(curLine));
    Vector#(LineSzBytes, Bit#(8)) wrVec = unpack(pack(wrLine));
    function Bit#(8) getNewByte(Integer i);
        return wrBE[i] ? wrVec[i] : curVec[i];
    endfunction
    Vector#(LineSzBytes, Bit#(8)) newVec = map(getNewByte, genVector);
    return unpack(pack(newVec));
endfunction
 
function Data getUpdatedData(Data curData, ByteEn wrBE, Data wrData);
    Vector#(DataSzBytes, Bit#(8)) curVec = unpack(pack(curData));
    Vector#(DataSzBytes, Bit#(8)) wrVec = unpack(pack(wrData));
    function Bit#(8) getNewByte(Integer i);
        return wrBE[i] ? wrVec[i] : curVec[i];
    endfunction
    Vector#(DataSzBytes, Bit#(8)) newVec = map(getNewByte, genVector);
    return pack(newVec);
endfunction

// calculate tag
typedef TSub#(AddrSz, TAdd#(LgLineSzBytes, TAdd#(lgBankNum, lgSetNum)))
    GetTagSz#(numeric type lgBankNum, numeric type lgSetNum);

// dependency tracking in MSHR
typedef union tagged {
    cRqIdxT CRq;
    pRqIdxT PRq;
} MshrIndex#(type cRqIdxT, type pRqIdxT) deriving(Bits, Eq, FShow);

// cache owner
typedef struct {
    cRqIdxT mshrIdx;
    Bool replacing;
} CRqOwner#(type cRqIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    pRqIdxT mshrIdx;
    Bool hasSucc; // has successor in dependency chain
} PRqOwner#(type pRqIdxT) deriving(Bits, Eq, FShow);

typedef union tagged {
    void Invalid;
    CRqOwner#(cRqIdxT) CRq;
    PRqOwner#(pRqIdxT) PRq;
} CacheOwner#(type cRqIdxT, type pRqIdxT) deriving(Bits, Eq, FShow);

// cache info: tag, cs, dir, owner, and other
typedef struct {
    tagT tag;
    msiT cs;
    dirT dir;
    ownerT owner;
    otherT other;
} CacheInfo#(
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type otherT
) deriving(Bits, Eq, FShow);

// ram output
typedef struct {
    CacheInfo#(tagT, msiT, dirT, ownerT, otherT) info;
    lineT line;
} RamData#(
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type otherT,
    type lineT
) deriving(Bits, Eq, FShow);

// processor req/resp
typedef struct {
    idT id;
    Addr addr;
    Msi toState;
    // below are detailed mem op
    MemOp op; // Ld, St, Lr, Sc, Amo
    ByteEn byteEn; // valid when op == Sc
    Data data; // valid when op == Sc/Amo
    AmoInst amoInst; // valid when op == Amo
} ProcRq#(type idT) deriving(Bits, Eq, FShow);

interface L1ProcReq#(type idT);
    method Action req(ProcRq#(idT) r);
endinterface

interface L1ProcResp#(type idT);
    method Action respLd(idT id, Data resp);
    method Action respLrScAmo(idT id, Data resp);
    method ActionValue#(Tuple2#(LineByteEn, Line)) respSt(idT id);
    method Action evict(LineAddr a); // called when cache line is evicted
endinterface

// RISCV-specific store-cond return values
typedef 0 ScSuccVal;
typedef 1 ScFailVal;

`ifdef DEBUG_ICACHE
typedef struct {
    Bit#(64) id;
    Line line;
} DebugICacheResp deriving(Bits, Eq, FShow);
`endif

// I$ req/resp
interface InstServer#(numeric type supSz);
    interface Put#(Addr) req;
    interface Get#(Vector#(supSz, Maybe#(Instruction))) resp;
`ifdef DEBUG_ICACHE
    interface Get#(DebugICacheResp) done; // the id and cache line of the I$ req that truly performs
`endif
endinterface

typedef struct {
    Addr addr;
`ifdef DEBUG_ICACHE
    Bit#(64) id; // incremening id for each incoming I$ req (0,1,...)
`endif
} ProcRqToI deriving(Bits, Eq, FShow);

// child/parent req/resp
typedef struct {
    Addr addr;
    Msi fromState;
    Msi toState;
    Bool canUpToE; // meaningful to upgrade to E if toState is S
    idT id; // slot id in child cache
    childT child; // from which child
} CRqMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    Maybe#(Line) data;
    childT child; // from which child
} CRsMsg#(type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    childT child; // to which child
} PRqMsg#(type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    Msi toState;
    childT child; // to which child
    Maybe#(Line) data;
    idT id; // slot id in cache
} PRsMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef union tagged {
    PRqMsg#(childT) PRq;
    PRsMsg#(idT, childT) PRs;
} PRqRsMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

interface ChildCacheToParent#(type cRqIdT, type childT);
    interface FifoDeq#(CRsMsg#(childT)) rsToP;
    interface FifoDeq#(CRqMsg#(cRqIdT, childT)) rqToP;
    interface FifoEnq#(PRqRsMsg#(cRqIdT, childT)) fromP;
endinterface

interface ParentCacheToChild#(type cRqIdT, type childT);
    interface FifoEnq#(CRsMsg#(childT)) rsFromC;
    interface FifoEnq#(CRqMsg#(cRqIdT, childT)) rqFromC;
    interface FifoDeq#(PRqRsMsg#(cRqIdT, childT)) toC;
endinterface

// unified child req & dma req
typedef union tagged {
    cRqIdT Child;
    dmaRqIdT Dma;
} LLRqId#(type cRqIdT, type dmaRqIdT) deriving(Bits, Eq, FShow);

typedef struct {
    // common addr
    Addr addr;
    // child req stuff
    Msi fromState;
    Msi toState;
    Bool canUpToE;
    childT child;
    // dma req stuff
    LineByteEn byteEn;
    // req id: distinguish between child and dma
    LLRqId#(cRqIdT, dmaRqIdT) id;
} LLRq#(type cRqIdT, type dmaRqIdT, type childT) deriving(Bits, Eq, FShow);

// memory msg
typedef struct {
    Addr addr;
    childT child; // from which LLC/Dir
    idT id; // ld req id and other info need encoding
} LdMemRq#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef struct { // LdMemRq id with more info encoded to handle DMA req in LLC
    Bool refill; // the future mem resp will refill LLC cache line
    // this is False for DMA read req that miss in LLC (i.e. resp won't refill LLC)
    mshrIdxT mshrIdx; // mshr id
} LdMemRqId#(type mshrIdxT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    LineByteEn byteEn;
    Line data;
} WbMemRs deriving(Bits, Eq, FShow);

typedef union tagged {
    LdMemRq#(idT, childT) Ld;
    WbMemRs Wb;
} ToMemMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Line data;
    childT child; // send to which LLC/Dir
    idT id; // original Ld req id
} MemRsMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

// Dma req/resp
typedef struct {
    Addr addr;
    LineByteEn byteEn; // all False means read
    Line data;
    idT id; // req id (resp may come out of order, may contain routing info)
} DmaRq#(type idT) deriving(Bits, Eq, FShow);

typedef struct {
    Line data; // meaningless for write
    idT id;
} DmaRs#(type idT) deriving(Bits, Eq, FShow);

interface DmaServer#(type dmaRqIdT);
    interface FifoEnq#(DmaRq#(dmaRqIdT)) memReq;
    interface FifoDeq#(DmaRs#(dmaRqIdT)) respLd;
    interface FifoDeq#(dmaRqIdT) respSt;
`ifdef DEBUG_DMA
    // signal when DMA req really takes effect
    interface Get#(dmaRqIdT) wrMissResp;
    interface Get#(dmaRqIdT) wrHitResp;
    interface Get#(dmaRqIdT) rdMissResp;
    interface Get#(dmaRqIdT) rdHitResp;
`endif
endinterface

// memory interface
interface MemFifoServer#(type idT, type childT);
    interface FifoEnq#(ToMemMsg#(idT, childT)) fromC;
    interface FifoDeq#(MemRsMsg#(idT, childT)) rsToC;
endinterface

interface MemFifoClient#(type idT, type childT);
    interface FifoDeq#(ToMemMsg#(idT, childT)) toM;
    interface FifoEnq#(MemRsMsg#(idT, childT)) rsFromM;
endinterface

instance Connectable#(MemFifoServer#(idT, childT), MemFifoClient#(idT, childT));
    module mkConnection#(
        MemFifoServer#(idT, childT) server, 
        MemFifoClient#(idT, childT) client
    )(Empty);
        rule doCToM;
            client.toM.deq;
            server.fromC.enq(client.toM.first);
        endrule
        rule doMToC;
            server.rsToC.deq;
            client.rsFromM.enq(server.rsToC.first);
        endrule
    endmodule
endinstance

instance Connectable#(MemFifoClient#(idT, childT), MemFifoServer#(idT, childT));
    module mkConnection#(
        MemFifoClient#(idT, childT) client,
        MemFifoServer#(idT, childT) server
    )(Empty);
        mkConnection(server, client);
    endmodule
endinstance

// MSHR dir pending bits
typedef union tagged {
    void Invalid;
    Msi ToSend; // need to send down req to downgrade to some state
    Msi Waiting; // waiting for down resp for some state
} DirPend deriving(Bits, Eq, FShow);

function Vector#(childNum, DirPend) getDirPendInitVal;
    return replicate(Invalid);
endfunction

function Bool getNeedReqChild(Vector#(childNum, DirPend) dirPend);
    // function to determine whether we need to send req to some children
    function Bool isToSend(DirPend dp);
        if(dp matches tagged ToSend .s) begin
            return True;
        end
        else begin
            return False;
        end
    endfunction
    return any(isToSend, dirPend);
endfunction

// Self-inv cache dir pending bits
typedef union tagged {
    void Invalid;
    childT ToSend;
    childT Waiting;
} SelfInvDirPend#(type childT) deriving(Bits, Eq, FShow);

function SelfInvDirPend#(childT) getSelfInvDirPendInitVal;
    return Invalid;
endfunction

function Bool getSelfInvNeedReqChild(SelfInvDirPend#(childT) dirPend);
    return dirPend matches tagged ToSend .c ? True : False;
endfunction

// useful functions
function Action check(Bool v);
action
    when(v, noAction);
endaction
endfunction

function Maybe#(Bit#(logv)) searchIndex
    (function Bool pred(element_type x1), Vector#(vsize, element_type) vect)
        provisos (Log#(vsize, logv));
    return case (findIndex(pred, vect)) matches
        tagged Valid .idx: return tagged Valid pack(idx);
        Invalid: return tagged Invalid;
    endcase;
endfunction

function a readReg(Reg#(a) r) provisos(Bits#(a, aSz)) = r;
function Vector#(n, a) readVector(Vector#(n, Reg#(a)) vr) provisos(Bits#(a, aSz)) = map(readReg, vr);

// doAssert now defined in Types.bsv
//function Action doAssert(Bool b, String str) = dynamicAssert(b, "%m: " + str);

function Get#(t) nullGet;
    return (interface Get;
        method ActionValue#(t) get if(False);
            return ?;
        endmethod
    endinterface);
endfunction
