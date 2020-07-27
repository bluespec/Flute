
// Copyright (c) 2017 Massachusetts Institute of Technology
// Portions (c) 2020 Bluespec, Inc.
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

`include "ProcConfig.bsv"
import Vector::*;
import Types::*;
import FShow::*;
import DefaultValue::*;
import MemoryTypes::*;

typedef `NUM_CORES CoreNum;
typedef Bit#(TLog#(CoreNum)) CoreId;

typedef `sizeSup SupSize;
typedef Bit#(TLog#(SupSize)) SupWaySel;
typedef Bit#(TLog#(TAdd#(SupSize, 1))) SupCnt;

typedef `NUM_EPOCHS NumEpochs;
typedef Bit#(TLog#(NumEpochs)) Epoch;

typedef `NUM_SPEC_TAGS NumSpecTags;
typedef Bit#(TLog#(NumSpecTags)) SpecTag;
typedef Bit#(NumSpecTags) SpecBits;

typedef `ROB_SIZE NumInstTags;
typedef TDiv#(NumInstTags, SupSize) SingleScalarSize;
typedef Bit#(TLog#(SingleScalarSize)) SingleScalarPtr;
typedef Bit#(TAdd#(1, TLog#(SingleScalarSize))) SingleScalarLen;

// consider ROB as a FIFO of size 2^log(NumInstTags)
// inst time is the index of the inst in the FIFO
// This indicates older/younger inst
typedef Bit#(TLog#(NumInstTags)) InstTime;

typedef struct {
    SupWaySel way; // which way in superscalar
    SingleScalarPtr ptr; // pointer within a way
    InstTime t; // inst time in ROB (for dispatch in reservation station)
} InstTag deriving(Bits, Eq, FShow);

typedef `SB_SIZE SBSize;
typedef Bit#(TLog#(SBSize)) SBIndex;

//typedef `LDSTQ_SIZE LdStQSize;
//typedef Bit#(TLog#(LdStQSize)) LdStQTag;

typedef `LDQ_SIZE LdQSize;
typedef Bit#(TLog#(LdQSize)) LdQTag;

typedef `STQ_SIZE StQSize;
typedef Bit#(TLog#(StQSize)) StQTag;

typedef union tagged {
    LdQTag Ld;
    StQTag St;
} LdStQTag deriving(Bits, Eq, FShow);

typedef enum {Ld, St, Cache} LdKilledBy deriving(Bits, Eq, FShow);

typedef `DRAM_MAX_REQS DramMaxReqs;
typedef `DRAM_MAX_READS DramMaxReads;
typedef `DRAM_MAX_WRITES DramMaxWrites;
typedef `DRAM_LATENCY DramLatency;

typedef Bit#(`LOG_DEADLOCK_CYCLES) DeadlockTimer;

typedef struct {
    // ISA modes
    Bool s;
    Bool u;
    // standard ISA extensions
    Bool m;
    Bool a;
    Bool f;
    Bool d;
    Bool c;
} RiscVISASubset deriving (Bits, Eq, FShow);

instance DefaultValue#(RiscVISASubset);
    function RiscVISASubset defaultValue = RiscVISASubset {
        s: True, u: True,
        m: `m , a: `a , f: `f , d: `d, c: `c
    };
endinstance

function Bit#(2) getXLBits = 2'b10; // MXL/SXL/UXL fix to RV64

function Bit#(26) getExtensionBits(RiscVISASubset isa);
    // include S and I by default
    Bit#(26)   ext =       26'b00000001000000000100000000;
    if (isa.u) ext = ext | 26'b00000100000000000000000000;
    if (isa.m) ext = ext | 26'b00000000000001000000000000;
    if (isa.a) ext = ext | 26'b00000000000000000000000001;
    if (isa.f) ext = ext | 26'b00000000000000000000100000;
    if (isa.d) ext = ext | 26'b00000000000000000000001000;
    if (isa.c) ext = ext | 26'b00000000000000000000000100;
    return ext;
endfunction

typedef Bit#(5) GprRIndx;
typedef Bit#(5) FpuRIndx;
typedef union tagged {
    GprRIndx Gpr;
    FpuRIndx Fpu;
} ArchRIndx deriving (Bits, Eq, FShow, Bounded);

typedef TExp#(SizeOf#(ArchRIndx)) NumArchReg;

typedef TAdd#(NumArchReg, `ROB_SIZE) NumPhyReg;
typedef Bit#(TLog#(NumPhyReg)) PhyRIndx;

typedef struct {
    PhyRIndx indx;
    Bool isFpuReg; // need to keep track of this for fs
} PhyDst deriving (Bits, Eq, FShow);

typedef struct {
    Maybe#(ArchRIndx) src1;
    Maybe#(ArchRIndx) src2;
    Maybe#(FpuRIndx) src3;
    Maybe#(ArchRIndx) dst;
} ArchRegs deriving (Bits, Eq, FShow);

typedef struct {
    Maybe#(PhyRIndx) src1;
    Maybe#(PhyRIndx) src2;
    Maybe#(PhyRIndx) src3;
    Maybe#(PhyDst) dst;
} PhyRegs deriving (Bits, Eq, FShow);

typedef struct {
    Bool src1;
    Bool src2;
    Bool src3;
    Bool dst;
} RegsReady deriving(Bits, Eq, FShow);

function Bool allRegsReady(RegsReady x);
    return x.src1 && x.src2 && x.src3 && x.dst;
endfunction

typedef enum {
    Invalid = 7'b0,
    Load    = 7'b0000011,
    LoadFp  = 7'b0000111,
    MiscMem = 7'b0001111,
    OpImm   = 7'b0010011,
    Auipc   = 7'b0010111,
    OpImm32 = 7'b0011011,
    Store   = 7'b0100011,
    StoreFp = 7'b0100111,
    Amo     = 7'b0101111,
    Op      = 7'b0110011,
    Lui     = 7'b0110111,
    Op32    = 7'b0111011,
    Fmadd   = 7'b1000011,
    Fmsub   = 7'b1000111,
    Fnmsub  = 7'b1001011,
    Fnmadd  = 7'b1001111,
    OpFp    = 7'b1010011,
    Branch  = 7'b1100011,
    Jalr    = 7'b1100111,
    Jal     = 7'b1101111,
    System  = 7'b1110011
} Opcode deriving(Bits, Eq, FShow);

function Opcode unpackOpcode(Bit#(7) x);
    return (case(x)
        pack(Opcode'(Load   )): (Load   );
        pack(Opcode'(LoadFp )): (LoadFp );
        pack(Opcode'(MiscMem)): (MiscMem); 
        pack(Opcode'(OpImm  )): (OpImm  );
        pack(Opcode'(Auipc  )): (Auipc  );
        pack(Opcode'(OpImm32)): (OpImm32);
        pack(Opcode'(Store  )): (Store  );
        pack(Opcode'(StoreFp)): (StoreFp);
        pack(Opcode'(Amo    )): (Amo    );
        pack(Opcode'(Op     )): (Op     );
        pack(Opcode'(Lui    )): (Lui    );
        pack(Opcode'(Op32   )): (Op32   );
        pack(Opcode'(Fmadd  )): (Fmadd  );
        pack(Opcode'(Fmsub  )): (Fmsub  );
        pack(Opcode'(Fnmsub )): (Fnmsub );
        pack(Opcode'(Fnmadd )): (Fnmadd );
        pack(Opcode'(OpFp   )): (OpFp   );
        pack(Opcode'(Branch )): (Branch );
        pack(Opcode'(Jalr   )): (Jalr   );
        pack(Opcode'(Jal    )): (Jal    );
        pack(Opcode'(System )): (System );
        default               : (Invalid);
    endcase);
endfunction

typedef enum {
    // user standard CSRs
    CSRfflags     = 12'h001,
    CSRfrm        = 12'h002,
    CSRfcsr       = 12'h003,
    CSRcycle      = 12'hc00,
    CSRtime       = 12'hc01,
    CSRinstret    = 12'hc02,
    // user non-standard CSRs (TODO)
    CSRterminate  = 12'h800, // terminate (used to exit Linux)
    CSRstats      = 12'h801, // turn on/off perf counters
    // supervisor standard CSRs
    CSRsstatus    = 12'h100,
    // no user trap handler, so no se/ideleg
    CSRsie        = 12'h104,
    CSRstvec      = 12'h105,
    CSRscounteren = 12'h106,
    CSRsscratch   = 12'h140,
    CSRsepc       = 12'h141,
    CSRscause     = 12'h142,
    CSRstval      = 12'h143, // it's still called sbadaddr in spike
    CSRsip        = 12'h144,
    CSRsatp       = 12'h180, // it's still called sptbr in spike
    // machine standard CSRs
    CSRmstatus    = 12'h300,
    CSRmisa       = 12'h301,
    CSRmedeleg    = 12'h302,
    CSRmideleg    = 12'h303,
    CSRmie        = 12'h304,
    CSRmtvec      = 12'h305,
    CSRmcounteren = 12'h306,
    CSRmscratch   = 12'h340,
    CSRmepc       = 12'h341,
    CSRmcause     = 12'h342,
    CSRmtval      = 12'h343, // it's still called mbadaddr in spike
    CSRmip        = 12'h344,
    CSRmcycle     = 12'hb00,
    CSRminstret   = 12'hb02,
    CSRmvendorid  = 12'hf11,
    CSRmarchid    = 12'hf12,
    CSRmimpid     = 12'hf13,
    CSRmhartid    = 12'hf14,
`ifdef SECURITY
    // sanctum machine CSR
    CSRmevbase    = 12'h7c0,
    CSRmevmask    = 12'h7c1,
    CSRmeatp      = 12'h7c2,
    CSRmmrbm      = 12'h7c3,
    CSRmemrbm     = 12'h7c4,
    CSRmparbase   = 12'h7c5,
    CSRmparmask   = 12'h7c6,
    CSRmeparbase  = 12'h7c7,
    CSRmeparmask  = 12'h7c8,
    CSRmflush     = 12'h7c9, // flush pipeline + cache
    CSRmspec      = 12'h7ca, // control speculation
    // sanctum user CSR
    CSRtrng       = 12'hcc0, // random number for secure boot
`endif

   CSRtselect     = 12'h7A0,    // Debug/trace tselect
   CSRtdata1      = 12'h7A1,    // Debug/trace tdata1
   CSRtdata2      = 12'h7A2,    // Debug/trace tdata2
   CSRtdata3      = 12'h7A3,    // Debug/trace tdata3

`ifdef INCLUDE_GDB_CONTROL
   CSRdcsr        = 12'h7B0,    // Debug control and status
   CSRdpc         = 12'h7B1,    // Debug PC
   CSRdscratch0   = 12'h7B2,    // Debug scratch0
   CSRdscratch1   = 12'h7B3,    // Debug scratch1
`endif

    // CSR that catches all the unimplemented CSRs. To avoid exception on this,
    // make it a user non-standard read/write CSR.
    // Bluespec: in RenameStage.getTrap(), we force this to be a csr_access_trap
    CSRnone       = 12'h8ff
} CSR deriving(Bits, Eq, FShow);

function CSR unpackCSR(Bit#(12) x);
    return (case(x)
        pack(CSR'(CSRfflags    )): (CSRfflags    );
        pack(CSR'(CSRfrm       )): (CSRfrm       );
        pack(CSR'(CSRfcsr      )): (CSRfcsr      );
        pack(CSR'(CSRcycle     )): (CSRcycle     );
        pack(CSR'(CSRtime      )): (CSRtime      );
        pack(CSR'(CSRinstret   )): (CSRinstret   );
        pack(CSR'(CSRterminate )): (CSRterminate );
        pack(CSR'(CSRstats     )): (CSRstats     );
        pack(CSR'(CSRsstatus   )): (CSRsstatus   );
        pack(CSR'(CSRsie       )): (CSRsie       );
        pack(CSR'(CSRstvec     )): (CSRstvec     );
        pack(CSR'(CSRscounteren)): (CSRscounteren);
        pack(CSR'(CSRsscratch  )): (CSRsscratch  );
        pack(CSR'(CSRsepc      )): (CSRsepc      );
        pack(CSR'(CSRscause    )): (CSRscause    );
        pack(CSR'(CSRstval     )): (CSRstval     );
        pack(CSR'(CSRsip       )): (CSRsip       );
        pack(CSR'(CSRsatp      )): (CSRsatp      );
        pack(CSR'(CSRmstatus   )): (CSRmstatus   );
        pack(CSR'(CSRmisa      )): (CSRmisa      );
        pack(CSR'(CSRmedeleg   )): (CSRmedeleg   );
        pack(CSR'(CSRmideleg   )): (CSRmideleg   );
        pack(CSR'(CSRmie       )): (CSRmie       );
        pack(CSR'(CSRmtvec     )): (CSRmtvec     );
        pack(CSR'(CSRmcounteren)): (CSRmcounteren);
        pack(CSR'(CSRmscratch  )): (CSRmscratch  );
        pack(CSR'(CSRmepc      )): (CSRmepc      );
        pack(CSR'(CSRmcause    )): (CSRmcause    );
        pack(CSR'(CSRmtval     )): (CSRmtval     );
        pack(CSR'(CSRmip       )): (CSRmip       );
        pack(CSR'(CSRmcycle    )): (CSRmcycle    );
        pack(CSR'(CSRminstret  )): (CSRminstret  );
        pack(CSR'(CSRmvendorid )): (CSRmvendorid );
        pack(CSR'(CSRmarchid   )): (CSRmarchid   );
        pack(CSR'(CSRmimpid    )): (CSRmimpid    );
        pack(CSR'(CSRmhartid   )): (CSRmhartid   );
`ifdef SECURITY
        pack(CSR'(CSRmevbase   )): (CSRmevbase   );
        pack(CSR'(CSRmevmask   )): (CSRmevmask   );
        pack(CSR'(CSRmeatp     )): (CSRmeatp     );
        pack(CSR'(CSRmmrbm     )): (CSRmmrbm     );
        pack(CSR'(CSRmemrbm    )): (CSRmemrbm    );
        pack(CSR'(CSRmparbase  )): (CSRmparbase  );
        pack(CSR'(CSRmparmask  )): (CSRmparmask  );
        pack(CSR'(CSRmeparbase )): (CSRmeparbase );
        pack(CSR'(CSRmeparmask )): (CSRmeparmask );
        pack(CSR'(CSRmflush    )): (CSRmflush    );
        pack(CSR'(CSRmspec     )): (CSRmspec     );
        pack(CSR'(CSRtrng      )): (CSRtrng      );
`endif

        pack(CSR'(CSRtselect   )): (CSRtselect   );
        pack(CSR'(CSRtdata1    )): (CSRtdata1    );
        pack(CSR'(CSRtdata2    )): (CSRtdata2    );
        pack(CSR'(CSRtdata3    )): (CSRtdata3    );

`ifdef INCLUDE_GDB_CONTROL
        pack(CSR'(CSRdcsr      )): (CSRdcsr      );
        pack(CSR'(CSRdpc       )): (CSRdpc       );
        pack(CSR'(CSRdscratch0 )): (CSRdscratch0 );
        pack(CSR'(CSRdscratch1 )): (CSRdscratch1 );
`endif

        default                  : (CSRnone      );
    endcase);
endfunction

// values for CSRmspec
Bit#(2) mSpecAll    = 0; // every inst can speculate
Bit#(2) mSpecNonMem = 1; // only non-memory inst can speculate
Bit#(2) mSpecNone   = 2; // no inst can speculate

typedef enum {
    Unsupported,
    Nop,
    Amo,
    Alu,
    Ld, St, Lr, Sc,
    J, Jr, Br,
    Auipc,
    Fpu,
    Csr,
    Fence,
    FenceI, SFence,
    Ecall, Ebreak,
    Sret, Mret, // do not support URET
    Interrupt // we may turn an inst to an interrupt in implementation
} IType deriving(Bits, Eq, FShow);

typedef enum {
    Eq, Neq,
    Lt, Ltu, Ge, Geu,
    AT, NT
} BrFunc deriving(Bits, Eq, FShow);

typedef enum {
    Add, Addw, Sub, Subw,
    And, Or, Xor,
    Slt, Sltu, Sll, Sllw, Sra, Sraw, Srl, Srlw,
    Csrw, Csrs, Csrc
} AluFunc deriving(Bits, Eq, FShow);

typedef enum {Mul, Mulh, Div, Rem} MulDivFunc deriving(Bits, Eq, FShow);

typedef enum {
    Signed, Unsigned, SignedUnsigned
} MulDivSign deriving(Bits, Eq, FShow);

typedef struct {
    MulDivFunc  func;
    Bool        w; // use word, i.e. 32-bit
    MulDivSign  sign;
} MulDivInst deriving(Bits, Eq, FShow);

typedef enum {
    FAdd, FSub, FMul, FDiv, FSqrt,
    FSgnj, FSgnjn, FSgnjx,
    FMin, FMax,
    FCvt_FF,
    FCvt_WF, FCvt_WUF, FCvt_LF, FCvt_LUF,
    FCvt_FW, FCvt_FWU, FCvt_FL, FCvt_FLU,
    FEq, FLt, FLe,
    FClass, FMv_XF, FMv_FX,
    FMAdd, FMSub, FNMSub, FNMAdd
} FpuFunc deriving(Bits, Eq, FShow);

typedef enum {
    Single,
    Double
} FpuPrecision deriving(Bits, Eq, FShow);

typedef struct {
    FpuFunc         func;
    RVRoundMode     rm;
    FpuPrecision    precision;
} FpuInst deriving(Bits, Eq, FShow);

// LdStInst and AmoInst are defined in Types.bsv
typedef union tagged {
    AluFunc     Alu;
    BrFunc      Br;
    MemInst     Mem;
    MulDivInst  MulDiv;
    FpuInst     Fpu;
    void        Other;
} ExecFunc deriving(Bits, Eq, FShow);

// Rounding Modes (encoding by risc-v, not general fpu)
typedef enum {
    RNE  = 3'b000,
    RTZ  = 3'b001,
    RDN  = 3'b010,
    RUP  = 3'b011,
    RMM  = 3'b100,
    RDyn = 3'b111
} RVRoundMode deriving(Bits, Eq, FShow);

typedef enum {
    InstAddrMisaligned  = 4'd0,
    InstAccessFault     = 4'd1,
    IllegalInst         = 4'd2,
    Breakpoint          = 4'd3,
    LoadAddrMisaligned  = 4'd4,
    LoadAccessFault     = 4'd5,
    StoreAddrMisaligned = 4'd6,
    StoreAccessFault    = 4'd7,
    EnvCallU            = 4'd8,
    EnvCallS            = 4'd9,
    EnvCallM            = 4'd11,
    InstPageFault       = 4'd12,
    LoadPageFault       = 4'd13,
    StorePageFault      = 4'd15
} Exception deriving(Bits, Eq, FShow);

typedef enum {
    UserSoftware       = 4'd0,
    SupervisorSoftware = 4'd1,
    MachineSoftware    = 4'd3,
    UserTimer          = 4'd4,
    SupervisorTimer    = 4'd5,
    MachineTimer       = 4'd7,
    UserExternal       = 4'd8,
    SupervisorExternel = 4'd9,
    MachineExternal    = 4'd11

`ifdef INCLUDE_GDB_CONTROL
  , DebugHalt          = 4'd14,        // Debugger halt command (^C in GDB)
    DebugStep          = 4'd15         // dcsr.step is set and 1 instr has been processed
`endif

} Interrupt deriving(Bits, Eq, FShow);

`ifdef INCLUDE_GDB_CONTROL
typedef 16 InterruptNum;    // With debugger
`else
typedef 12 InterruptNum;    // Without debugger
`endif

// Traps are either an exception or an interrupt
typedef union tagged {
    Exception Exception;
    Interrupt Interrupt;
} Trap deriving(Bits, Eq, FShow);

// privilege modes
Bit#(2) prvU = 0;
Bit#(2) prvS = 1;
Bit#(2) prvM = 3;

// VM modes
Bit#(4) vmBare = 0;
Bit#(4) vmSv39  = 9;

typedef struct {
    // for decoding floating-point instructions
    Bit#(3) frm;
    Bool fEnabled;
    // for decoding privileged instructions
    Bit#(2) prv;
    Bool trapVM; // mstatus.tvm: trap on CSRXXX inst on satp or SFENCE.VMA
                 // executed in S mode
    Bool timeoutWait; // mstatus.tw: trap on WFI after waiting N cycles in S
                      // mode. This is currently ignore since WFI is a NOP.
    Bool trapSret; // mstatus.tsr: trap on SRET executed in S mode
    // for decoding rdcycle/time/instret
    Bool cycleReadableByS; // S mode can do rdcycle
    Bool cycleReadableByU; // U mode can do rdcycle
    Bool instretReadableByS; // S mode can do rdinstret
    Bool instretReadableByU; // U mode can do rdinstret
    Bool timeReadableByS; // S mode can do rdtime
    Bool timeReadableByU; // U mode can do rdtime
} CsrDecodeInfo deriving (Bits, Eq, FShow);

typedef struct {
    Bit#(2) prv; // has taken mstatus.mprv into account
    Asid asid; // currently always 0
    Bool sv39; // VM mode: has taken prv into account, False means Bare
    Bool exeReadable; // mstatus.mxr: can load page with X=1 and R=0
    Bool userAccessibleByS; // mstatus.sum: in S mode (after considering
                            // mstatus.mprv), accessing page with U=1 will NOT
                            // fault
    Bit#(44) basePPN; // ppn of root page table
`ifdef SECURITY
    // sanctum page walk check
    Bit#(64) sanctum_evbase;
    Bit#(64) sanctum_evmask;
    Bit#(44) sanctum_ebasePPN;
    Bit#(64) sanctum_mrbm;
    Bit#(64) sanctum_emrbm;
    Bit#(64) sanctum_parbase;
    Bit#(64) sanctum_parmask;
    Bit#(64) sanctum_eparbase;
    Bit#(64) sanctum_eparmask;
    // whether an access on shared (i.e., not in my own private protation
    // domain) memory is allowed. This should not be allowed if speculation is
    // turned on.
    Bool sanctum_authShared;
`endif
} VMInfo deriving(Bits, Eq, FShow);

instance DefaultValue#(VMInfo);
    function VMInfo defaultValue = VMInfo {
        prv:  prvM,
        asid: 0,
        sv39: False,
        exeReadable: False,
        userAccessibleByS: False,
        basePPN: 0
`ifdef SECURITY
        , sanctum_evbase:   maxBound,
        sanctum_evmask:     0,
        sanctum_ebasePPN:   0,
        sanctum_mrbm:       maxBound,
        sanctum_emrbm:      0,
        sanctum_parbase:    maxBound,
        sanctum_parmask:    0,
        sanctum_eparbase:   0,
        sanctum_eparmask:   0,
        sanctum_authShared: False
`endif
    };
endinstance

typedef struct {
    Addr  pc;
    Addr  nextPc;
    IType iType;
    Bool  taken;
    Bool  mispredict;
} Redirect deriving (Bits, Eq, FShow);

typedef struct {
    Addr pc;
    Addr nextPc;
    Bool taken;
    Bool mispredict;
} ControlFlow deriving (Bits, Eq, FShow);

typedef struct {
    DecodedInst dInst;
    ArchRegs    regs;
    Bool        illegalInst;
} DecodeResult deriving(Bits, Eq, FShow);

typedef Bit#(32) ImmData; // 32-bit decoded immediate data

typedef struct {
    IType           iType;
    ExecFunc        execFunc;
    Maybe#(CSR)     csr;
    Maybe#(ImmData) imm;
} DecodedInst deriving(Bits, Eq, FShow);

function Maybe#(Data) getDInstImm(DecodedInst dInst);
    return dInst.imm matches tagged Valid .d ? Valid (signExtend(d)) : Invalid;
endfunction

typedef struct {
    Data        data;
    Data        csrData;
    Addr        addr;
    ControlFlow controlFlow;
} ExecResult deriving(Bits, Eq, FShow);

// MMIO
typedef union tagged {
    // inst fetch: contains the maximum superscaler way to fetch
    SupWaySel Inst;
    // data access
    void Ld;
    void St;
    AmoFunc Amo;
} MMIOFunc deriving(Bits, Eq, FShow);

// req fom core to platform
typedef struct {
    Addr addr; // physical address
    MMIOFunc func; // req type
    // BE, shifted for 64-bit aligned. LOAD, STORE and AMO all need to specify
    // this. We need this for to remove redundant MMIO accesses (for MSIP), and
    // to determine AMO access range (upper 32 bits, lower 32 bits, or full 64
    // bits). INST FETCH will not specify this field.
    ByteEn byteEn;
    // For STORE: this is store data shifted to be 64-bit aligned
    // For AMO: this is UNshifted data (like normal mem req)
    Data data;
} MMIOCRq deriving(Bits, Eq, FShow);

// resp from platform to core
typedef struct {
    Bool valid; // if fase, then access fault
    // resp data only for LOAD or AMO req.
    // For LOAD: this is the aligned 64-bit result that contains the load
    // access range (similar to normal cache loads, i.e., the receiver needs to
    // shift the result before writting back to reg).
    // For AMO: this is the result that can be directly written into reg, i.e.,
    // for 32-bit access, the result has been shifted and sign-extended.
    Data data;
} MMIODataPRs deriving(Bits, Eq, FShow);

typedef union tagged {
    // Resp for INST fetch. A vector entry can be invalid for two reasons: 1)
    // that entry is not requested, 2) that entry is access fault.
    Vector#(SupSize, Maybe#(Instruction)) InstFetch;
    // Resp for DATA access, i.e. LOAD, STORE and AMO
    MMIODataPRs DataAccess;
} MMIOPRs deriving(Bits, Eq, FShow);

// req from platform to core, only access MSIP or MTIP bit
typedef enum {MSIP, MTIP} MMIOPRqTarget deriving(Bits, Eq, FShow);
typedef struct {
    MMIOPRqTarget target;
    MMIOFunc func;
    // For STORE: only data[0] matters.
    // For AMO: this is unshifted data which is truncated from the data in the
    // original cRq. AMO should only access MSIP with 32-bit access range
    // because MSIP is viewed as 32-bit wide in MMIO.
    Bit#(32) data;
} MMIOPRq deriving(Bits, Eq, FShow); // req from core to platform,

// resp from core to platform
typedef struct {
    // For LOAD or AMO: this is the original MSIP value
    // For STORE: this is garbage
    Bit#(1) data;
} MMIOCRs deriving(Bits, Eq, FShow);

// Boot rom: each block is 64-bit data
typedef `LOG_BOOT_ROM_BYTES LgBootRomBytes;
typedef TSub#(LgBootRomBytes, TLog#(NumBytes)) LgBootRomSzData;
typedef Bit#(LgBootRomSzData) BootRomIndex;

// mtime: we increment mtime by 50 every 5000 cycles, this simulates a
// 10MHz clock for a 1GHz CPU (same as spike)
typedef /*50*/ 1 TicksPerTimeInc;
typedef /*5000*/ 100 CyclesPerTimeInc;

// Op
Bit#(3) fnADD   = 3'b000;
Bit#(3) fnSLL   = 3'b001;
Bit#(3) fnSLT   = 3'b010;
Bit#(3) fnSLTU  = 3'b011;
Bit#(3) fnXOR   = 3'b100;
Bit#(3) fnSR    = 3'b101;
Bit#(3) fnOR    = 3'b110;
Bit#(3) fnAND   = 3'b111;

Bit#(7) opALU1   = 7'b0000000;
Bit#(7) opALU2   = 7'b0100000;
Bit#(7) opMULDIV = 7'b0000001;

Bit#(3) fnMUL    = 3'b000;
Bit#(3) fnMULH   = 3'b001;
Bit#(3) fnMULHSU = 3'b010;
Bit#(3) fnMULHU  = 3'b011;
Bit#(3) fnDIV    = 3'b100;
Bit#(3) fnDIVU   = 3'b101;
Bit#(3) fnREM    = 3'b110;
Bit#(3) fnREMU   = 3'b111;

// Branch
Bit#(3) fnBEQ   = 3'b000;
Bit#(3) fnBNE   = 3'b001;
Bit#(3) fnBLT   = 3'b100;
Bit#(3) fnBGE   = 3'b101;
Bit#(3) fnBLTU  = 3'b110;
Bit#(3) fnBGEU  = 3'b111;

// Load
Bit#(3) fnLB    = 3'b000;
Bit#(3) fnLH    = 3'b001;
Bit#(3) fnLW    = 3'b010;
Bit#(3) fnLD    = 3'b011;
Bit#(3) fnLBU   = 3'b100;
Bit#(3) fnLHU   = 3'b101;
Bit#(3) fnLWU   = 3'b110;

// Store
Bit#(3) fnSB    = 3'b000;
Bit#(3) fnSH    = 3'b001;
Bit#(3) fnSW    = 3'b010;
Bit#(3) fnSD    = 3'b011;

// Amo
Bit#(5) fnLR      = 5'b00010;
Bit#(5) fnSC      = 5'b00011;
Bit#(5) fnAMOSWAP = 5'b00001;
Bit#(5) fnAMOADD  = 5'b00000;
Bit#(5) fnAMOXOR  = 5'b00100;
Bit#(5) fnAMOAND  = 5'b01100;
Bit#(5) fnAMOOR   = 5'b01000;
Bit#(5) fnAMOMIN  = 5'b10000;
Bit#(5) fnAMOMAX  = 5'b10100;
Bit#(5) fnAMOMINU = 5'b11000;
Bit#(5) fnAMOMAXU = 5'b11100;

// FPU
Bit#(2) fmtS      = 2'b00;
Bit#(2) fmtD      = 2'b01;
Bit#(5) opFADD    = 5'b00000;
Bit#(5) opFSUB    = 5'b00001;
Bit#(5) opFMUL    = 5'b00010;
Bit#(5) opFDIV    = 5'b00011;
Bit#(5) opFSQRT   = 5'b01011;
Bit#(5) opFSGNJ   = 5'b00100;
Bit#(5) opFMINMAX = 5'b00101;
Bit#(5) opFCMP    = 5'b10100;
Bit#(5) opFMV_XF  = 5'b11100; // FCLASS also
Bit#(5) opFMV_FX  = 5'b11110;
Bit#(5) opFCVT_FF = 5'b01000;
Bit#(5) opFCVT_WF = 5'b11000;
Bit#(5) opFCVT_FW = 5'b11010;

//MiscMem
Bit#(3) fnFENCE  = 3'b000;
Bit#(3) fnFENCEI = 3'b001;

// System
Bit#(3) fnPRIV   = 3'b000;
Bit#(3) fnCSRRW  = 3'b001;
Bit#(3) fnCSRRS  = 3'b010;
Bit#(3) fnCSRRC  = 3'b011;
Bit#(3) fnCSRRWI = 3'b101;
Bit#(3) fnCSRRSI = 3'b110;
Bit#(3) fnCSRRCI = 3'b111;

Bit#(12) privECALL  = 12'h000;
Bit#(12) privEBREAK = 12'h001;
Bit#(12) privURET   = 12'h002;
Bit#(12) privSRET   = 12'h102;
Bit#(12) privMRET   = 12'h302;
Bit#(12) privWFI    = 12'h105;

Bit#(7) privSFENCEVMA  = 7'h9;

function Bool isSystem(IType iType) = (
    iType == Unsupported || iType == Interrupt ||
    iType == Ecall || iType == Ebreak || iType == Csr ||
    iType == SFence || iType == FenceI ||
    iType == Sret || iType == Mret
);

// instruction requires replaying (i.e. fetch next instruction after current
// instruction commits)
function Bool doReplay(IType iType) = isSystem(iType);

function Bool isFpuInst(IType iType) = (iType == Fpu);

function Bool isMemInst(IType iType) = (
    iType == Ld || iType == St || iType == Lr || iType == Sc || iType == Amo
);

function Fmt showInst(Instruction inst);
  Fmt ret = fshow("");

  Opcode opcode = unpack(inst[  6 :  0 ]);
  let rd     = inst[ 11 :  7 ];
  let funct3 = inst[ 14 : 12 ];
  let rs1    = inst[ 19 : 15 ];
  let rs2    = inst[ 24 : 20 ];
  let funct7 = inst[ 31 : 25 ];

  Bit#(32) immI   = signExtend(inst[31:20]);
  Bit#(32) immS   = signExtend({ inst[31:25], inst[11:7] });
  Bit#(32) immB   = signExtend({ inst[31], inst[7], inst[30:25], inst[11:8], 1'b0});
  Bit#(32) immU   = { inst[31:12], 12'b0 };
  Bit#(32) immJ   = signExtend({ inst[31], inst[19:12], inst[20], inst[30:25], inst[24:21], 1'b0});

  case (opcode)
    OpImm:
    begin
      ret = case (funct3)
        fnADD: fshow("addi");
        fnSLT: fshow("slti");
        fnSLTU: fshow("sltiu");
        fnAND: fshow("andi");
        fnOR: fshow("ori");
        fnXOR: fshow("xori");
        fnSLL: fshow("slli");
        fnSR: (immI[10] == 0 ? fshow("srli") : fshow("srai"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[5:0]);
        default: fshow(immI);
      endcase);
    end

    OpImm32:
    begin
      ret = case (funct3)
        fnADD: fshow("addiw");
        fnSLL: fshow("slliw");
        fnSR: (immI[10] == 0 ? fshow("srliw") : fshow("sraiw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ");
      ret = ret + (case (funct3)
        fnSLL, fnSR: fshow(immI[4:0]);
        default: fshow(immI);
      endcase);
    end

    Op:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("add") : fshow("sub"));
        fnSLT: fshow("slt");
        fnSLTU: fshow("sltu");
        fnAND: fshow("and");
        fnOR: fshow("or");
        fnXOR: fshow("xor");
        fnSLL: fshow("sll");
        fnSR: (immI[10] == 0 ? fshow("srl") : fshow("sra"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    Op32:
    begin
      ret = case (funct3)
        fnADD: (immI[10] == 0 ? fshow("addw") : fshow("subw"));
        fnSLL: fshow("sllw");
        fnSR: (immI[10] == 0 ? fshow("srlw") : fshow("sraw"));
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(rs2);
    end

    Lui:
      ret = fshow("lui ") + fshow(rd) + fshow(" ") + fshow(immU);

    Auipc:
      ret = fshow("auipc ") + fshow(rd) + fshow(" ") + fshow(immU);

    Jal:
      ret = fshow("jal ") + fshow(rd) + fshow(" ") + fshow(immJ);

    Jalr:
      ret = fshow("jalr ") + fshow(rd) + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(immI);

    Branch:
    begin
      ret = case(funct3)
        fnBEQ: fshow("beq");
        fnBNE: fshow("bne");
        fnBLT: fshow("blt");
        fnBLTU: fshow("bltu");
        fnBGE: fshow("bge");
        fnBGEU: fshow("bgeu");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immB);
    end

    Load:
    begin
      ret = case(funct3)
        fnLB: fshow("lb");
        fnLH: fshow("lh");
        fnLW: fshow("lw");
        fnLD: fshow("ld");
        fnLBU: fshow("lbu");
        fnLHU: fshow("lhu");
        fnLWU: fshow("lwu");
      endcase;
      ret = ret + fshow(" ") + fshow(rd) + fshow(" = ") + fshow(rs1) + fshow(" ") + fshow(immI);
    end

    Store:
    begin
      ret = case(funct3)
        fnSB: fshow("sb");
        fnSH: fshow("sh");
        fnSW: fshow("sw");
        fnSD: fshow("sd");
      endcase;
      ret = ret + fshow(" ") + fshow(rs1) + fshow(" ") + fshow(rs2) + fshow(" ") + fshow(immS);
    end

    MiscMem:
    begin
      ret = case (funct3)
        fnFENCE: fshow("fence");
        fnFENCEI: fshow("fence.i");
      endcase;
    end

    System:
    begin
      case (funct3)
        fnCSRRW, fnCSRRS, fnCSRRC, fnCSRRWI, fnCSRRSI, fnCSRRCI:
        begin
          ret = case(funct3)
            fnCSRRW: fshow("csrrw");
            fnCSRRC: fshow("csrrc");
            fnCSRRS: fshow("csrrs");
            fnCSRRWI: fshow("csrrwi");
            fnCSRRCI: fshow("csrrci");
            fnCSRRSI: fshow("csrrsi");
          endcase;
          ret = ret + fshow(" ") + fshow(rd) + fshow(" ") + fshow(immI) + fshow(" ") + fshow(rs1);
        end

        fnPRIV:
        begin
          ret = case (truncate(immI))
            privECALL: fshow("ecall");
            privEBREAK: fshow("ebreak");
            privURET: fshow("uret");
            privSRET: fshow("sret");
            privMRET: fshow("mret");
            privWFI: fshow("wfi");
            default: (
              funct7 == privSFENCEVMA ? 
              (fshow("sfence.vma ") + fshow(rs1) + fshow(" ") + fshow(rs2)) :
              fshow("SYSTEM not implemented")
            );
          endcase;
        end

        default:
          ret = fshow("SYSTEM not implemented");
      endcase
    end
    default:
      ret = fshow("nop");
  endcase

  return ret;
endfunction

