
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

import FShow::*;

// performance counter typedefs

// query performance counters in each stage/module
typedef enum {
    L1ILdCnt,
    L1ILdMissCnt,
    L1ILdMissLat,
    L1IReconcileCnt
} L1IPerfType deriving(Bits, Eq, FShow);

typedef enum {
    L1DLdCnt,
    L1DLdMissCnt,
    L1DLdMissLat,
    L1DStCnt,
    L1DStMissCnt,
    L1DStMissLat,
    L1DAmoCnt,
    L1DAmoMissCnt,
    L1DAmoMissLat,
    L1DSelfInvCnt, // self inv due to max hits
    L1DReconcileCnt
} L1DPerfType deriving(Bits, Eq, FShow);

typedef enum {
    LLCDmaMemLdCnt,
    LLCDmaMemLdLat,
    LLCNormalMemLdCnt,
    LLCNormalMemLdLat,
    LLCMshrBlockCycles, // full MSHR blocks new cRq
    LLCDownRespCnt,
    LLCDownRespDataCnt,
    LLCDownReqCnt,
    LLCUpRespCnt,
    LLCUpRespDataCnt,
    LLCDmaLdReqCnt,
    LLCDmaStReqCnt
} LLCPerfType deriving(Bits, Eq, FShow);

typedef enum {
    L1TlbAccessCnt,
    L1TlbMissParentCnt, // miss and send req to parent TLB
    L1TlbMissParentLat,
    L1TlbMissPeerCnt, // miss and wait for resp for peer entry
    L1TlbMissPeerLat,
    L1TlbHitUnderMissCnt,
    L1TlbAllMissCycles // all TLB req entries are miss, so TLB is blocked
} L1TlbPerfType deriving(Bits, Eq, FShow);

typedef enum {
    L2TlbInstMissCnt,
    L2TlbInstMissLat,
    L2TlbInstPageWalks,
    L2TlbInstSavedPageWalks,
    L2TlbInstHugePageHits, // hits on huge page (2MB, 1GB)
    L2TlbInstHugePageMisses, // miss (i.e., page walk) to get huge page (2MB, 1GB)
    L2TlbDataMissCnt,
    L2TlbDataMissLat,
    L2TlbDataPageWalks,
    L2TlbDataSavedPageWalks,
    L2TlbDataHugePageHits,
    L2TlbDataHugePageMisses,
    L2TlbHitUnderMissCnt,
    L2TlbAllMissCycles, // all TLB req entries are doing page walk, so TLB is blocked
    L2TlbPeerSavedMemReqs // mem req saved by re-using peer req's  page walk
} L2TlbPerfType deriving(Bits, Eq, FShow);

typedef enum {
    DecRedirectBr,
    DecRedirectJmp,
    DecRedirectJr,
    DecRedirectOther
} DecStagePerfType deriving(Bits, Eq, FShow);

typedef enum {
    SupRenameCnt, // number of cycles that rename correct path inst cnt > 1
    SpecNoneCycles,
    SpecNonMemCycles,
    ExeRedirectBr,
    ExeRedirectJr,
    ExeRedirectOther,
    ExeLdStallByLd,
    ExeLdStallBySt,
    ExeLdStallBySB,
    ExeLdForward,
    ExeLdMemLat,
    ExeStMemLat,
    ExeLdToUseLat,
    ExeLdToUseCnt,
    ExeTlbExcep,
    ExeScSuccessCnt,
    ExeLrScAmoAcqCnt,
    ExeLrScAmoRelCnt,
    ExeFenceAcqCnt,
    ExeFenceRelCnt,
    ExeFenceCnt,
    ExeIntMulCnt,
    ExeIntDivCnt,
    ExeFpFmaCnt,
    ExeFpDivCnt,
    ExeFpSqrtCnt
} ExeStagePerfType deriving(Bits, Eq, FShow);

typedef enum {
    LdQFullCycles,
    StQFullCycles,
    ROBFullCycles,
    AluRS0FullCycles,
    AluRS1FullCycles,
    FpuMulDivRSFullCycles,
    MemRSFullCycles,
    EpochFullCycles,
    SpecTagFullCycles
} CoreSizePerfType deriving(Bits, Eq, FShow);

typedef enum {
    CycleCnt,
    InstCnt,
    UserInstCnt,
    SupComUserCnt, // number of cycles that commit user inst cnt > 1
    ComBrCnt,
    ComJmpCnt,
    ComJrCnt,
    ComLdCnt,
    ComStCnt,
    ComLrCnt,
    ComScCnt,
    ComAmoCnt,
    ComLdKillByLd,
    ComLdKillBySt,
    ComLdKillByCache,
    ComSysCnt, // system inst count
    ExcepCnt,
    InterruptCnt,
    FlushTlbCnt,
    FlushSecurityCnt,
    FlushBPCnt,
    FlushCacheCnt
} ComStagePerfType deriving(Bits, Eq, FShow);

// PerfReq = XXPerfType

typedef struct {
    perfType pType;
    Bit#(64) data;
} PerfResp#(type perfType) deriving(Bits, Eq);

interface Perf#(type perfType);
    method Action setStatus(Bool doStats); // change whether we collect data
    method Action req(perfType r);
    method ActionValue#(PerfResp#(perfType)) resp;
    method Bool respValid;
endinterface

// query performance counters in the whole processor
typedef Bit#(5) PerfType; // for all XXPerfType

// which stage/module to query
typedef enum {
    ICache,
    DCache,
    ITlb,
    DTlb,
    L2Tlb,
    DecStage,
    ExeStage,
    ComStage,
    CoreSize,
    LLC
} PerfLocation deriving(Bits, Eq, FShow);

typedef struct {
    PerfLocation loc;
    PerfType pType;
} ProcPerfReq deriving(Bits, Eq);

typedef struct {
    PerfLocation loc;
    PerfType pType;
    Bit#(64) data;
} ProcPerfResp deriving(Bits, Eq);
