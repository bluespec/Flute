This Near_Mem_VM_WB_L1_L2 is an optional memory system for
Piccolo/Flute, containing an L1 cache with writeback policy, attached
to an L2 cache writeback-policy taken from Toooba/RISCY-OOO.

The L2 cache supports a MESI cache-coherence protocol with the L1
caches (IMem and DMem), and also supports a coherent DMA port.

The files immediately in 'src_LLCache/'
    LLCache.bsv
    L1LLConnect.bsv
are taken from a Toooba repo with small local modifications.
The file
    LLCache_Aux.bsv
is locally created, copying a few small code excerpts from
Toooba/RISCY-OOO and greatly prunes the 'import' graph.

The files in 'src_LLCache/procs/' and 'src_LLCache/coherence/' are
taken from a Toooba repo, using the Copy_Toooba_Files.mk makefile.
They are taken as-is and not modified here.

>================================================================
Below are miscellaneous notes taken while studying LLCache.bsv (from
Toooba/RISCY-OOO) and its dependent files, in preparation for
integrating it into Flute's WB_L1_L2 memory system.

>----------------------------------------------------------------
These are configs:

    typedef `LOG_LLC_LINES LgLLLineNum;
    typedef `LOG_LLC_WAYS LgLLWayNum;

The following are about partitioning the MSHR for security purposes,
not relevant for us:

    `ifdef SECURITY
    `ifndef DISABLE_SECURE_LLC_MSHR
    `ifndef DISABLE_SECURE_LLC_ARBITER
    `ifndef DISABLE_SECURE_BW
    `ifdef USE_LLC_MSHR_SECURE_MODEL

    `define USE_LLC_MSHR_SECURE_MODEL
    `define USE_LLC_ARBITER_SECURE_MODEL


Self-invalidation (SI) in cache:

    Sizhuo thesis: For WMM, two flavors of implementations: one uses
      conventional MESI coherence protocol (same as TSO
      implementation); the other uses self-invalidation coherence
      protocol, which cannot be used in any TSO implementations.

      Main idea SI: the directory tracks only the child cache that
      owns data in exclusive state, and does not track any shared
      copies. Child cache will self-invalidate all its shared copies
      if core executes a fence.

>----------------------------------------------------------------
Macros in `if and `elsif in original set of files:
(not including macros in `defines, of which there are many)

    BOARD_xsim
    BSIM
    BSV_POSITIVE_RESET
    CACHE_SMALL
    CACHE_LARGE
    CACHE_MC_1MB
    CACHE_MC_2MB
    CHECK_DEADLOCK
    CORE_TINY
    CORE_SMALL
    CORE_MEDIUM
    CORE_SMALL_WIDE
    CORE_BOOM
    CORE_LARGE
    CORE_LARGE_WIDE
    DEBUG_DMA
    DEBUG_ICACHE
    DISABLE_SECURE_LLC_ARBITER
    DISABLE_SECURE_BW
    DISABLE_SECURE_BW
    DISABLE_SECURE_LLC
    DISABLE_SECURE_LLC_MSHR
    INCLUDE_GDB_CONTROL
    Near_Mem_TCM
    NO_LOAD_RESP_E
    PcieClockPeriod
    PERF_COUNT
    SECURITY
    SELF_INV_CACHE
    SIM_LLC_ARBITER_NUM
    SIM_LOG_LLC_PARTITION_NUM
    SIMULATION
    USE_BSV_BRAM_SYNC_FIFO
    USE_CONNECTAL_BRAM_SYNC_FIFO
    USE_LLC_ARBITER_SECURE_MODEL
    USE_LLC_MSHR_SECURE_MODEL
    USE_XILINX_MACRO
    USE_XILINX_SYNC_FIFO
    XilinxUltrascale
    XILINX

>================================================================
Interface of LLCache

// ----------------------------------------------------------------
// procs/lib/LLCache.bsv

typedef L1Num LLChildNum;                           // 2
typedef TMul#(CoreNum, 2) L1Num;                    // 2
typedef Bit#(TLog#(LLChildNum)) LLChild;            // Bit #(1)

typedef `LOG_L1_WAYS LgL1WayNum;                    // 3
typedef Bit#(LgL1WayNum) L1Way;                     // Bit #(3)
typedef L1Way LLCRqId;                              // Bit #(3)

typedef Bit#(TLog#(L2TlbReqNum)) L2TlbReqIdx;       // Bit #(1)
typedef L2TlbReqIdx TlbMemReqId;                    // Bit #(1)

typedef 8 CLineNumData;                             // 8
typedef Bit#(LogCLineNumData) CLineDataSel;         // Bit #(3)
typedef CacheUtils::CLineDataSel LineDataOffset;    // Bit #(3)

typedef struct {
    CoreId core;               // Bit #(0)
    TlbMemReqId id;            // Bit #(1)
    LineDataOffset dataSel;    // Bit #(3)
} TlbDmaReqId
deriving(Bits, Eq, FShow);

typedef void MemLoaderMemReqId;                     // void

typedef union tagged {
    MemLoaderMemReqId MemLoader;                    // void
    TlbDmaReqId Tlb;                                // Bit #(5)
} LLCDmaReqId deriving(Bits, Eq, FShow);

typedef struct { // LdMemRq id with more info encoded to handle DMA req in LLC
    Bool refill; // the future mem resp will refill LLC cache line
    // this is False for DMA read req that miss in LLC (i.e. resp won't refill LLC)
    mshrIdxT mshrIdx; // mshr id
} LdMemRqId#(type mshrIdxT) deriving(Bits, Eq, FShow);

typedef Bit#(TLog#(LLCRqNum)) LLCRqMshrIdx;         // Bit #(4)

interface LLCache;
    interface ParentCacheToChild#(LLCRqId, LLChild) to_child;
    interface DmaServer#(LLCDmaReqId) dma;
    interface MemFifoClient#(LdMemRqId#(LLCRqMshrIdx), void) to_mem;
    // detect deadlock: only in use when macro CHECK_DEADLOCK is defined
    interface Get#(LLCStuck) cRqStuck;
    // performance
    interface Perf#(LLCPerfType) perf;
endinterface

// ----------------------------------------------------------------
// procs/lib/ProcTypes.bsv

typedef `NUM_CORES CoreNum;                         // 1
typedef Bit#(TLog#(CoreNum)) CoreId;                // Bit #(0)

// ----------------------------------------------------------------
// coherence/src/CCTypes.bsv

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

typedef enum {
    I = 2'd0, 
    S = 2'd1, 
    E = 2'd2, 
    M = 2'd3
} MESI deriving(Bits, Eq, FShow);

typedef MESI Msi;

typedef struct {
    Addr addr;
    Msi fromState;
    Msi toState;
    Bool canUpToE; // meaningful to upgrade to E if toState is S
    idT id; // slot id in child cache
    childT child; // from which child
} CRqMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef CacheUtils::CLineNumData LineSzData;    // 8
typedef Vector#(LineSzData, Data) Line;         // Bit #(512)

typedef struct {
    Addr addr;
    Msi toState;
    Maybe#(Line) data;
    childT child; // from which child
} CRsMsg#(type childT) deriving(Bits, Eq, FShow);

typedef union tagged {
    PRqMsg#(childT) PRq;
    PRsMsg#(idT, childT) PRs;
} PRqRsMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

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

// ----------------

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

typedef CacheUtils::CLineByteEn LineByteEn;        // Bit #(64)

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

// ----------------

interface MemFifoClient#(type idT, type childT);
    interface FifoDeq#(ToMemMsg#(idT, childT)) toM;
    interface FifoEnq#(MemRsMsg#(idT, childT)) rsFromM;
endinterface

typedef union tagged {
    LdMemRq#(idT, childT) Ld;
    WbMemRs Wb;
} ToMemMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    childT child; // from which LLC/Dir
    idT id; // ld req id and other info need encoding
} LdMemRq#(type idT, type childT) deriving(Bits, Eq, FShow);

typedef struct {
    Addr addr;
    LineByteEn byteEn;
    Line data;
} WbMemRs deriving(Bits, Eq, FShow);

typedef struct {
    Line data;
    childT child; // send to which LLC/Dir
    idT id; // original Ld req id
} MemRsMsg#(type idT, type childT) deriving(Bits, Eq, FShow);

// ----------------------------------------------------------------
// procs/lib/Types.bsv

typedef 64 AddrSz;                // 64
typedef Bit#(AddrSz) Addr;        // Bit #(64)

// ----------------------------------------------------------------
// procs/lib/CacheUtils.bsv

typedef 64 DataSz;                // 64
typedef Bit#(DataSz) Data;        // Bit #(64)

typedef TDiv#(DataSz, 8) NumBytes;
typedef 8 CLineNumData;
typedef TMul#(CLineNumData, NumBytes) CLineNumBytes;
typedef Vector#(CLineNumBytes, Bool) CLineByteEn;

// ----------------------------------------------------------------
