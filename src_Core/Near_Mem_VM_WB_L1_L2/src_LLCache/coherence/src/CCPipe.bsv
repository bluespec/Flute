
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

import Ehr::*;
import Fifos::*;
import Vector::*;
import RWBramCore::*;
import FShow::*;
import Types::*;
import CCTypes::*;

// general type param ordering: way < index < tag < msi < dir < owner < other < rep < line < pipeCmd

typedef union tagged {
    void Invalid;
    msiT DownDir; // cRs downgraded toState
    msiT UpCs; // pRs upgraded toState
} RespState#(type msiT) deriving(Bits, Eq, FShow);

typedef struct {
    pipeCmdT cmd;
    // tag match & ram output
    wayT way;
    Bool pRqMiss; // pRq miss, valid only if go through tag match
    RamData#(tagT, msiT, dirT, ownerT, otherT, lineT) ram;
    // replace info, actually not needed, just output for debug purposes
    repT repInfo;
} PipeOut#(
    type wayT,
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type otherT,
    type repT,
    type lineT,
    type pipeCmdT
) deriving(Bits, Eq, FShow);

interface CCPipe#(
    numeric type wayNum,
    type indexT,
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type otherT,
    type repT,
    type lineT,
    type pipeCmdT
);
    method Action enq(pipeCmdT cmd, Maybe#(lineT) respLine, RespState#(msiT) toState);
    method Bool notFull;
    method PipeOut#(Bit#(TLog#(wayNum)), tagT, msiT, dirT, ownerT, otherT, repT, lineT, pipeCmdT) first;
    method PipeOut#(Bit#(TLog#(wayNum)), tagT, msiT, dirT, ownerT, otherT, repT, lineT, pipeCmdT) unguard_first;
    method Bool notEmpty;
    method Action deqWrite(
        Maybe#(pipeCmdT) newCmd,
        RamData#(tagT, msiT, dirT, ownerT, otherT, lineT) wrRam,
        Bool updateRep // update replacement info
    );
    // empty signal when we need to flush self-invalidate cache
    method Bool emptyForFlush;
endinterface

// internal pipeline reg types
// three stages
// 1: enq
// 2: tag match, read data and dir
// 3: output

typedef struct {
    pipeCmdT cmd;
    // bypasses
    Vector#(wayNum, Maybe#(CacheInfo#(tagT, msiT, dirT, ownerT, otherT))) infoVec;
    Maybe#(repT) repInfo; // replacement info for the whole set
    // CRs/PRs info
    Maybe#(lineT) respLine;
    RespState#(msiT) toState;
} Enq2Match#(
    numeric type wayNum,
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type otherT,
    type repT,
    type lineT,
    type pipeCmdT
) deriving(Bits, Eq, FShow);

typedef struct {
    pipeCmdT cmd;
    // tag match results
    wayT way;
    Bool pRqMiss;
    // RAM outputs
    // cs is merged with PRs toState
    // dir is merged with CRs toState
    CacheInfo#(tagT, msiT, dirT, ownerT, otherT) info;
    repT repInfo;
    // bypassed or resp line
    Maybe#(lineT) line;
} Match2Out#(
    type wayT,
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type otherT,
    type repT,
    type lineT,
    type pipeCmdT
) deriving(Bits, Eq, FShow);

typedef struct {
    indexT index;
    wayT way;
    RamData#(tagT, msiT, dirT, ownerT, otherT, lineT) ram; // data to write into RAM
    repT repInfo; // replacement info write into RAM
} BypassInfo#(
    type wayT,
    type indexT,
    type tagT,
    type msiT,
    type dirT,
    type ownerT,
    type otherT,
    type repT,
    type lineT
) deriving(Bits, Eq, FShow);

typedef struct {
    wayT way;
    Bool pRqMiss;
} TagMatchResult#(type wayT) deriving(Bits, Eq, FShow);

typedef struct {
    msiT cs;
} UpdateByUpCs#(type msiT) deriving(Bits, Eq, FShow);

typedef struct {
    msiT cs;
    dirT dir;
} UpdateByDownDir#(type msiT, type dirT) deriving(Bits, Eq, FShow);

// index to data ram: {way, normal index}
function dataIndexT getDataRamIndex(wayT w, indexT i) provisos(
    Alias#(wayT, Bit#(_waySz)),
    Alias#(indexT, Bit#(_indexSz)),
    Alias#(dataIndexT, Bit#(TAdd#(_waySz, _indexSz)))
);
    return {w, i};
endfunction

module mkCCPipe#(
    ReadOnly#(Bool) initDone,
    function indexT getIndex(pipeCmdT cmd),
    function ActionValue#(TagMatchResult#(wayT)) tagMatch(
        // actionvalue enable us to do checking inside the function
        pipeCmdT cmd,
        // below are current RAM outputs, is merged with ram write from final stage
        // but is NOT merged with state changes carried in PRs/CRs 
        Vector#(wayNum, tagT) tagVec,
        Vector#(wayNum, msiT) csVec,
        Vector#(wayNum, ownerT) ownerVec,
        repT repInfo
    ),
    function ActionValue#(UpdateByUpCs#(msiT)) updateByUpCs(
        pipeCmdT cmd, msiT toState, Bool dataValid, msiT oldCs
    ),
    function ActionValue#(UpdateByDownDir#(msiT, dirT)) updateByDownDir(
        pipeCmdT cmd, msiT toState, Bool dataValid, msiT oldCs, dirT oldDir
    ),
    function ActionValue#(repT) updateRepInfo(repT oldRep, wayT hitWay),
    Vector#(wayNum, RWBramCore#(indexT, infoT)) infoRam,
    RWBramCore#(indexT, repT) repRam,
    RWBramCore#(dataIndexT, lineT) dataRam
)(
    CCPipe#(wayNum, indexT, tagT, msiT, dirT, ownerT, otherT, repT, lineT, pipeCmdT)
) provisos (
    Alias#(wayT, Bit#(TLog#(wayNum))),
    Alias#(indexT, Bit#(_indexSz)),
    Alias#(infoT, CacheInfo#(tagT, msiT, dirT, ownerT, otherT)),
    Alias#(ramDataT, RamData#(tagT, msiT, dirT, ownerT, otherT, lineT)),
    Alias#(respStateT, RespState#(msiT)),
    Alias#(pipeOutT, PipeOut#(wayT, tagT, msiT, dirT, ownerT, otherT, repT, lineT, pipeCmdT)),
    Alias#(enq2MatchT, Enq2Match#(wayNum, tagT, msiT, dirT, ownerT, otherT, repT, lineT, pipeCmdT)),
    Alias#(match2OutT, Match2Out#(wayT, tagT, msiT, dirT, ownerT, otherT, repT, lineT, pipeCmdT)),
    Alias#(bypassInfoT, BypassInfo#(wayT, indexT, tagT, msiT, dirT, ownerT, otherT, repT, lineT)),
    Bits#(tagT, _tagSz),
    Bits#(msiT, _msiSz),
    Bits#(dirT, _dirSz),
    Bits#(ownerT, _ownerSz),
    Bits#(otherT, _otherSz),
    Bits#(repT, _repSz),
    Bits#(lineT, _lineSz),
    Bits#(pipeCmdT, _pipeCmdSz),
    // index to data ram: {way, normal index}
    Alias#(dataIndexT, Bit#(TAdd#(TLog#(wayNum), _indexSz)))
);

    // pipeline regs

    Ehr#(3, Maybe#(enq2MatchT)) enq2Mat <- mkEhr(Invalid);
    // port 0: bypass
    Reg#(Maybe#(enq2MatchT)) enq2Mat_bypass = enq2Mat[0];
    // port 1: tag match
    Reg#(Maybe#(enq2MatchT)) enq2Mat_match = enq2Mat[1];
    // port 2: enq
    Reg#(Maybe#(enq2MatchT)) enq2Mat_enq = enq2Mat[2];

    Ehr#(2, Maybe#(match2OutT)) mat2Out <- mkEhr(Invalid);
    // port 0: out
    Reg#(Maybe#(match2OutT)) mat2Out_out = mat2Out[0];
    // port 1: tag match
    Reg#(Maybe#(match2OutT)) mat2Out_match = mat2Out[1];

    // bypass write to ram
    RWire#(bypassInfoT) bypass <- mkRWire;

    // stage 2: first get bypass
    (* fire_when_enabled, no_implicit_conditions *)
    rule doMatch_bypass(isValid(bypass.wget) && isValid(enq2Mat_bypass) && initDone);
        bypassInfoT b = fromMaybe(?, bypass.wget);
        enq2MatchT e2m = fromMaybe(?, enq2Mat_bypass);
        if(b.index == getIndex(e2m.cmd)) begin
            e2m.infoVec[b.way] = Valid (b.ram.info);
            e2m.repInfo = Valid (b.repInfo);
        end
        enq2Mat_bypass <= Valid (e2m);
    endrule

    rule doTagMatch(isValid(enq2Mat_match) && !isValid(mat2Out_match) && initDone);
        enq2MatchT e2m = fromMaybe(?, enq2Mat_match);
        // get cache output & merge with bypass
        Vector#(wayNum, infoT) infoVec;
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            infoRam[i].deqRdResp;
            infoVec[i] = fromMaybe(infoRam[i].rdResp, e2m.infoVec[i]);
        end
        repRam.deqRdResp;
        repT repInfo = fromMaybe(repRam.rdResp, e2m.repInfo);
        // do tag match to get way to occupy
        Vector#(wayNum, tagT) tagVec;
        Vector#(wayNum, msiT) csVec;
        Vector#(wayNum, ownerT) ownerVec;
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            tagVec[i] = infoVec[i].tag;
            csVec[i] = infoVec[i].cs;
            ownerVec[i] = infoVec[i].owner;
        end
        let tmRes <- tagMatch(e2m.cmd, tagVec, csVec, ownerVec, repInfo);
        wayT way = tmRes.way;
        Bool pRqMiss = tmRes.pRqMiss;
        // read data
        indexT index = getIndex(e2m.cmd);
        dataRam.rdReq(getDataRamIndex(way, index));
        // set mat2out & merge with CRs/PRs & merge with data bypass
        // resp data has higher priority than data bypass
        match2OutT m2o = Match2Out {
            cmd: e2m.cmd,
            way: way,
            pRqMiss: pRqMiss,
            info: infoVec[way],
            repInfo: repInfo,
            line: e2m.respLine
        };
        if(e2m.toState matches tagged UpCs .s) begin
            UpdateByUpCs#(msiT) upd <- updateByUpCs(
                e2m.cmd, s, isValid(e2m.respLine), m2o.info.cs
            );
            m2o.info.cs = upd.cs;
        end
        else if(e2m.toState matches tagged DownDir .s) begin
            UpdateByDownDir#(msiT, dirT) upd <- updateByDownDir(
                e2m.cmd, s, isValid(e2m.respLine), m2o.info.cs, m2o.info.dir
            );
            m2o.info.cs = upd.cs;
            m2o.info.dir = upd.dir;
        end
        if(bypass.wget matches tagged Valid .b &&& b.index == index &&& b.way == way &&& !isValid(m2o.line)) begin
            // bypass has lower priority than resp data
            m2o.line = Valid (b.ram.line);
        end
        mat2Out_match <= Valid (m2o);
        // reset enq2mat
        enq2Mat_match <= Invalid;
    endrule

    // construct output with bypass/resp data
    function pipeOutT firstOut;
        match2OutT m2o = fromMaybe(?, mat2Out_out);
        return PipeOut {
            cmd: m2o.cmd,
            way: m2o.way,
            pRqMiss: m2o.pRqMiss,
            ram: RamData {
                info: m2o.info,
                line: fromMaybe(dataRam.rdResp, m2o.line)
            },
            repInfo: m2o.repInfo
        };
    endfunction

    Bool enq_guard = !isValid(enq2Mat_enq) && initDone;

    Bool deq_guard = isValid(mat2Out_out) && initDone;

    // stage 1: enq req to pipeline: access info+rep RAM & bypass
    method Action enq(pipeCmdT cmd, Maybe#(lineT) respLine, respStateT toState) if(enq_guard);
        // read ram
        indexT index = getIndex(cmd);
        for(Integer i = 0; i < valueOf(wayNum); i = i+1) begin
            infoRam[i].rdReq(index);
        end
        repRam.rdReq(index);
        // write reg & get bypass
        enq2MatchT e2m = Enq2Match {
            cmd: cmd,
            infoVec: replicate(Invalid),
            repInfo: Invalid,
            respLine: respLine,
            toState: toState
        };
        if(bypass.wget matches tagged Valid .b &&& b.index == index) begin
            e2m.infoVec[b.way] = Valid (b.ram.info);
            e2m.repInfo = Valid (b.repInfo);
        end
        enq2Mat_enq <= Valid (e2m);
    endmethod

    method Bool notFull = enq_guard;

    method pipeOutT first if(deq_guard);
        return firstOut;
    endmethod

    method pipeOutT unguard_first;
        return firstOut;
    endmethod

    method Bool notEmpty = deq_guard;

    method Action deqWrite(Maybe#(pipeCmdT) newCmd, ramDataT wrRam, Bool updateRep) if(deq_guard);
        match2OutT m2o = fromMaybe(?, mat2Out_out);
        wayT way = m2o.way;
        indexT index = getIndex(m2o.cmd);
        // update replacement info
        repT repInfo = m2o.repInfo;
        if(updateRep) begin
            repInfo <- updateRepInfo(m2o.repInfo, way);
        end
        // write ram
        infoRam[way].wrReq(index, wrRam.info);
        repRam.wrReq(index, repInfo);
        dataRam.wrReq(getDataRamIndex(way, index), wrRam.line);
        // set bypass to Enq and Match stages
        bypass.wset(BypassInfo {
            index: index,
            way: way,
            ram: wrRam,
            repInfo: repInfo
        });
        // change pipeline reg
        if(newCmd matches tagged Valid .cmd) begin
            // update pipeline reg
            mat2Out_out <= Valid (Match2Out {
                cmd: cmd, // swapped in new cmd
                way: way, // keep way same
                pRqMiss: False, // reset (not valid for swapped in pRq)
                info: wrRam.info, // get bypass
                repInfo: repInfo, // get bypass
                line: Valid (wrRam.line) // get bypass
            });
        end
        else begin
            // XXX deq ram resp, I think this should not block
            dataRam.deqRdResp;
            // reset pipeline reg
            mat2Out_out <= Invalid;
        end
    endmethod

    method Bool emptyForFlush;
        return !isValid(mat2Out[0]) && !isValid(enq2Mat[0]);
    endmethod
endmodule
