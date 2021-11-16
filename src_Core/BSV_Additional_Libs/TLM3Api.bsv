// Copyright (c) 2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3Api;

import BUtils::*;
import CBus::*;
import ClientServer::*;
import Connectable::*;
import FIFO::*;
import FIFOF::*;
import FShow::*;
import GetPut::*;
import TLM3Defines::*;
import TLM3Limit::*;
import Vector::*;

`include "TLM.defines"

typedef Bit#(TLog#(TAdd#(m, 1)))  LBit#(numeric type m);
typedef UInt#(TLog#(TAdd#(m, 1))) LUInt#(numeric type m);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

//typedef Bit#(16)  TLMHeaderByteEn;
typedef Bit#(10)  TLMHeaderId;

typedef struct {TLMBurstLong            b_length;
		TLMRequestParams        params;
                } TLMRequestHeader deriving (Eq, Bits, Bounded);

typedef struct {TLMCommand              command;
//                TLMHeaderByteEn         byte_enable;
                TLMBurstMode            burst_mode;
                TLMBSize                b_size;
		TLMLock                 lock;
		TLMHeaderId             transaction_id;
		// protection parameters
		TLMPrivilege            privilege;
		TLMSecurity             security;
                TLMAccess               access;
		// cache parameters
		TLMCache                cache; // used for "modify" in Axi4
		TLMBuffer               buffer;
		TLMAllocate             read_allocate;
		TLMAllocate             write_allocate;
		// for Axi4
		TLMRegion               region;
		TLMQos                  qos;
		// for Flow control
		Bool                    cntrl_flow;
		//
		Bool                    spec_byte_enable;
                } TLMRequestParams deriving (Eq, Bits, Bounded);


typedef struct {TLMBurstLong            b_length;
		TLMResponseParams       params;
		} TLMResponseHeader deriving (Eq, Bits, Bounded);

typedef struct {TLMCommand           command;
                TLMStatus            status;
		TLMHeaderId          transaction_id;
		TLMErrorCode         error;
		} TLMResponseParams deriving (Eq, Bits, Bounded);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function UInt#(n) getRequestLength(TLMRequestHeader header)
   provisos (Add#(1, SizeOf#(TLMBurstLong), n));
   return extend(header.b_length) + 1;
endfunction

function TLMRequestHeader setRequestLength(TLMRequestHeader header, UInt#(n) length)
   provisos (Add#(1, SizeOf#(TLMBurstLong), n));
   let modified = header;
   modified.b_length = truncate(length - 1);
   return modified;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance FShow#(TLMRequestHeader);
   function Fmt fshow (TLMRequestHeader header);
      return ($format("<REQHDR [%0d] ", header.params.transaction_id)
              +
              fshow(header.params.command)
              +
              fshow(" ")
              +
              fshow(header.params.burst_mode)
	      +
	      fshow(" ")
	      +
	      fshow(header.params.b_size)
              +
              $format(" (%0d)>", getRequestLength(header)));
   endfunction
endinstance

instance FShow#(TLMResponseHeader);
   function Fmt fshow (TLMResponseHeader header);
      return ($format("<RESPHDR [%0d] ", header.params.transaction_id)
              +
              fshow(header.params.command)
              +
              fshow(" ")
              +
	      fshow(header.params.status)
	      +
              $format(" (%0d)>", header.b_length + 1));
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function TLMRequestHeader toTLMRequestHeader (RequestDescriptor#(`TLM_PRM) descriptor);
   TLMRequestHeader header = ?;
   TLMRequestParams params = ?;

   params.command              = descriptor.command;
   header.b_length             = unpack(extendNP(pack(descriptor.b_length)));

   params.spec_byte_enable = False;
   if (descriptor.byte_enable matches tagged Specify .b)
      params.spec_byte_enable = True;

   params.burst_mode           = descriptor.burst_mode;
   params.b_size               = descriptor.b_size;
   params.qos                  = descriptor.prty;
   params.lock                 = descriptor.lock;

   Bit#(16) max_transaction_id = extendNP(descriptor.transaction_id);
   params.transaction_id       = truncateNP(max_transaction_id);

   params.region               = descriptor.region;
   // protection parameters
   params.privilege            = descriptor.privilege;
   params.security             = descriptor.security;
   params.access               = descriptor.access;
   // cache parameters
   params.cache                = descriptor.cache;
   params.buffer               = descriptor.buffer;
   params.read_allocate        = descriptor.read_allocate;
   params.write_allocate       = descriptor.write_allocate;
   // flow control
   params.cntrl_flow           = descriptor.cntrl_flow;

   header.params = params;
   return header;
endfunction

function RequestDescriptor#(`TLM_PRM) fromTLMRequestHeader(TLMRequestHeader header);
   let params = header.params;
   RequestDescriptor#(`TLM_PRM) descriptor = ?;

   descriptor.command          = params.command;
   descriptor.mode             = REGULAR;
   descriptor.addr             = 0; // set later
   descriptor.data             = 0; // set later
   descriptor.b_length         = unpack(truncateNP(pack(header.b_length)));

   descriptor.byte_enable      = tagged Calculate;

   descriptor.burst_mode       = params.burst_mode;
   descriptor.b_size           = params.b_size;
   descriptor.prty             = params.qos;
   descriptor.lock             = params.lock;

   Bit#(16) max_transaction_id = extendNP(params.transaction_id);
   descriptor.transaction_id   = truncateNP(max_transaction_id);

   descriptor.region           = params.region;
   // protection parameters
   descriptor.privilege        = params.privilege;
   descriptor.security         = params.security;
   descriptor.access           = params.access;
   // cache parameters
   descriptor.cache            = params.cache;
   descriptor.buffer           = params.buffer;
   descriptor.read_allocate    = params.read_allocate;
   descriptor.write_allocate   = params.write_allocate;
   descriptor.mark             = OPEN;
   descriptor.cntrl_flow       = params.cntrl_flow;

   return descriptor;
endfunction


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function TLMResponseHeader toTLMResponseHeader (TLMResponse#(`TLM_PRM) response);
   TLMResponseHeader header = ?;
   TLMResponseParams params = ?;

   params.command         = response.command;
   header.b_length        = 0; // set later
   params.status          = response.status;
   params.transaction_id  = extendNP(response.transaction_id);
   params.error           = (response.status == ERROR) ? unpack(truncateNP(response.data)) : NONE;
   header.params          = params;
   return header;

endfunction

function TLMResponse#(`TLM_PRM) fromTLMResponseHeader(TLMResponseHeader header);

   let params = header.params;
   TLMResponse#(`TLM_PRM) response = ?;

   response.command        = params.command;
   response.data           = (params.status == ERROR) ? extendNP(pack(params.error)) : 0; // set later
   response.user           = 0; // set later
   response.status         = params.status;
   response.prty           = 0;
   response.transaction_id = truncateNP(params.transaction_id);
   response.is_last        = False; // set later;

   return response;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef Bit#(32) Chunk;

function Vector#(2, Chunk) hdrToChunks (a value)
   provisos(Bits#(a, sa));
   return unpack(extendNP(pack(value)));
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface ApiRecvIFC#(`TLM_RR_DCL);
   interface Server#(Chunk, Chunk) scemi;
   interface TLMSendIFC#(`TLM_RR)  out;
endinterface

interface ApiSendIFC#(`TLM_RR_DCL);
   interface TLMRecvIFC#(`TLM_RR)  in;
   interface Client#(Chunk, Chunk) scemi;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {SHeader0, SHeader1, SAddrRead, SAddrWrite, SUserRead, SUserWrite, SData} SendState deriving (Eq, Bits, Bounded);

module mkApiSender(ApiSendIFC#(`TLM_RR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(1, SizeOf#(TLMBLength#(`TLM_PRM)), n),
	    Add#(data_size, user_size, du_size),
	    // derived numeric types:
	    NumAlias#(nNA, TDiv#(addr_size, SizeOf#(Chunk))),
	    NumAlias#(nNU, TDiv#(user_size, SizeOf#(Chunk))),
	    NumAlias#(nND, TMax#(1, TDiv#(du_size, SizeOf#(Chunk)))),
	    NumAlias#(nDE_SIZE, TAdd#(du_size, TDiv#(data_size, 8))),
	    NumAlias#(nNDE, TMax#(1, TDiv#(nDE_SIZE, SizeOf#(Chunk)))),
	    NumAlias#(nZZ,  TMul#(nND, SizeOf#(Chunk))));

   let valid = checkSizes(valueOf(data_size), valueOf(addr_size), valueOf(user_size));
   let vnd   = valueOf(nND);
   let vnu   = valueOf(nNU);

   SFIFO#(TLMMark, TLMBLength#(`TLM_PRM)) mark_fifo <- mkSafeDepthParamSFIFO(5);

   FIFO#(resp_t)             tlm_tx_fifo   <- mkLFIFO;
   FIFO#(req_t)              tlm_rx_fifo   <- mkLFIFO;
   FIFO#(Chunk)              scemi_rx_fifo <- mkLFIFO;
   FIFO#(Chunk)              scemi_tx_fifo <- mkLFIFO;

   Reg#(TLMAddr#(`TLM_PRM))  addr             <- mkReg(?);
   Reg#(TLMUser#(`TLM_PRM))  user             <- mkReg(?);
   Reg#(SendState)           state            <- mkReg(SHeader0);
   Reg#(LBit#(nNA))          addr_count       <- mkReg(0);
   Reg#(LBit#(nNU))          user_count       <- mkReg(0);
   Reg#(LBit#(nNDE))         data_chunk_count <- mkReg(0);

   Reg#(UInt#(n))            data_item_count  <- mkReg(0);

   Vector#(nNA , Chunk) addr_chunks = unpack(extendNP(addr));

   FIFO#(Tuple3#(Bool, TLMByteEn#(`TLM_PRM), Bit#(du_size))) data_fifo <- mkLFIFO;
   Reg#(Bit#(nDE_SIZE))      data_buff <- mkReg(?);

   Reg#(Bool)            include_be <- mkReg(False);

   Bool small_data = valueOf(SizeOf#(Chunk)) >= valueOf(nDE_SIZE);

   function  LBit#(nNDE) getVND (Bool inc_be);
      let n = (inc_be) ? fromInteger(valueOf(nNDE))
                       : fromInteger(valueOf(nND));
      return n;
   endfunction

   function Action enq_data (req_t request);
      action
	 if (toTLMRequest(request) matches tagged Descriptor .d)
	    begin
	       let be = 0;
	       if (d.byte_enable matches tagged Specify .b)
		  be = b;
	       data_fifo.enq(tuple3(include_be, be, {d.data, d.user}));
	    end
	 if (toTLMRequest(request) matches tagged Data .d)
	    begin
	       let be = 0;
	       if (d.byte_enable matches tagged Specify .b)
		  be = b;
	       data_fifo.enq(tuple3(include_be, be, {d.data, d.user}));
	    end
      endaction
   endfunction

   rule send_header_0 (toTLMRequest(tlm_rx_fifo.first) matches tagged Descriptor .d
		       &&& state == SHeader0
		       &&& data_item_count == 0);

      let header = toTLMRequestHeader(d);
      let chunks = hdrToChunks(header);
      include_be <= header.params.spec_byte_enable;
      scemi_tx_fifo.enq(chunks[0]);
      state <= SHeader1;
      mark_fifo.enq(d.mark);
   endrule

   rule send_header_1_read (toTLMRequest(tlm_rx_fifo.first) matches tagged Descriptor .d
			    &&& d.command == READ
			    &&& state == SHeader1
			    &&& data_item_count == 0);
      let header = toTLMRequestHeader(d);
      let chunks = hdrToChunks(header);
      scemi_tx_fifo.enq(chunks[1]);
      state <= SAddrRead;
      tlm_rx_fifo.deq;
      addr <= d.addr;
      user <= d.user_addr;
      addr_count <= fromInteger(valueOf(nNA));
      user_count <= fromInteger(valueOf(nNU));
   endrule

   rule send_header_1_write (toTLMRequest(tlm_rx_fifo.first) matches tagged Descriptor .d
			     &&& d.command == WRITE
			     &&& state == SHeader1
			     &&& data_item_count == 0);
      let header = toTLMRequestHeader(d);
      let chunks = hdrToChunks(header);
      scemi_tx_fifo.enq(chunks[1]);
      state <= SAddrWrite;
      tlm_rx_fifo.deq;
      addr <= d.addr;
      user <= d.user_addr;
      addr_count <= fromInteger(valueOf(nNA));
      user_count <= fromInteger(valueOf(nNU));
      enq_data(tlm_rx_fifo.first);
      UInt#(n) cnt = truncateNP(getRequestLength(header));
      data_item_count <= cnt;
   endrule

   rule send_addr_read (state == SAddrRead && addr_count != 0);
      addr_count <= addr_count - 1;
      scemi_tx_fifo.enq(truncateNP(addr));
      addr <= addr >> valueOf(SizeOf#(Chunk));
      if (addr_count == 1) state <= ((vnu == 0) ? SHeader0 : SUserRead);
   endrule

   rule send_addr_write (state == SAddrWrite && addr_count != 0);
      addr_count <= addr_count - 1;
      scemi_tx_fifo.enq(truncateNP(addr));
      addr <= addr >> valueOf(SizeOf#(Chunk));
      if (addr_count == 1) state <= ((vnu == 0) ? SData : SUserWrite);
   endrule

   if (vnu != 0)
      begin
	 rule send_user_read (state == SUserRead && user_count != 0);
	    user_count <= user_count - 1;
	    scemi_tx_fifo.enq(truncateNP(user));
	    user <= user >> valueOf(SizeOf#(Chunk));
	    if (user_count == 1) state <= SHeader0;
	 endrule

	 rule send_user_write (state == SUserWrite && user_count != 0);
	    user_count <= user_count - 1;
	    scemi_tx_fifo.enq(truncateNP(user));
	    user <= user >> valueOf(SizeOf#(Chunk));
	    if (user_count == 1) state <= SData;
	 endrule
      end

   rule enq_write_data(toTLMRequest(tlm_rx_fifo.first) matches tagged Data .d);
      tlm_rx_fifo.deq;
      enq_data(tlm_rx_fifo.first);
   endrule

   rule send_data_chunk(state == SData && data_chunk_count != 0);
      data_chunk_count <= data_chunk_count - 1;
      if (small_data)
	 scemi_tx_fifo.enq(extendNP(data_buff));
      else
	 scemi_tx_fifo.enq(truncateNP(data_buff));
      data_buff <= data_buff >> valueOf(SizeOf#(Chunk));
      state <= (data_item_count==0 && data_chunk_count==1) ? SHeader0 : SData;
   endrule

   rule send_write_data (data_chunk_count == 0 &&
			 data_item_count  != 0 && state == SData);
      match {.inc_be, .be, .data} = data_fifo.first;
      let num = getVND(inc_be);
      let dd = {be, data};
      data_item_count <= data_item_count - 1;
      data_fifo.deq;
      if (num==1) begin
	 if (small_data)
	    scemi_tx_fifo.enq(pack(extendNP(dd)));
	 else
	    scemi_tx_fifo.enq(pack(truncateNP(dd)));
	 state <= (data_item_count == 1) ? SHeader0 : SData;
      end
      else begin
	 scemi_tx_fifo.enq(truncateNP(data));
	 data_buff <= dd >> valueOf(SizeOf#(Chunk));
	 data_chunk_count <= num-1;
      end
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   ///
   ////////////////////////////////////////////////////////////////////////////////

   Reg#(Bool)                  first            <- mkReg(True);
   Reg#(TLMResponseHeader)     header_reg       <- mkReg(?);
   Reg#(LBit#(nND))            resp_chunk_count <- mkReg(0);
   Reg#(Bit#(nZZ))             resp_buff        <- mkReg(?);

   Reg#(TLMBLength#(`TLM_PRM)) resp_count       <- mkReg(0);
   Reg#(LBit#(nNU))            write_user_count <- mkReg(0);
   Reg#(TLMUser#(`TLM_PRM))    write_user       <- mkReg(?);

   rule receive_response_header (first);
      TLMResponseHeader header = unpack(truncateNP(scemi_rx_fifo.first));
      scemi_rx_fifo.deq;
      let no_user = header.params.command == READ || vnu == 0;
      Bool is_last = header.params.command == WRITE && no_user;
      write_user_count <= (header.params.command == WRITE) ? fromInteger(vnu) : 0;
      first <= is_last;
      if (is_last) // a write response, no user data, no following data
      	 begin
	    TLMResponse#(`TLM_PRM) tlm_response = fromTLMResponseHeader(header);
	    tlm_response.is_last = is_last;
	    let mark = mark_fifo.first;
	    mark_fifo.deq;
	    if (mark == NOT_LAST)
	       tlm_response.is_last = False;
	    tlm_tx_fifo.enq(fromTLMResponse(tlm_response));
	 end
      resp_count <= truncateNP(header.b_length);
      header_reg <= header;
   endrule

   rule receive_write_response_user (!first && write_user_count != 0);
      write_user_count <= write_user_count - 1;
      write_user <= truncateNP({scemi_rx_fifo.first, write_user} >> valueOf(SizeOf#(Chunk))); // lsb first
      scemi_rx_fifo.deq;
      if (extendNP(write_user_count) == 2'b1)
	 begin // a write response, last user data, no following data
	    first <= True;
	    TLMResponse#(`TLM_PRM) tlm_response = fromTLMResponseHeader(header_reg);
	    tlm_response.is_last = True;
	    let mark = mark_fifo.first;
	    mark_fifo.deq;
	    if (mark == NOT_LAST)
	       tlm_response.is_last = False;
	    tlm_response.user = truncateNP({scemi_rx_fifo.first, write_user} >> valueOf(SizeOf#(Chunk)));
	    tlm_tx_fifo.enq(fromTLMResponse(tlm_response));
	 end
   endrule

   rule receive_response_data (!first && write_user_count == 0);
      let chunk = scemi_rx_fifo.first;
      scemi_rx_fifo.deq;
//      Bit#(du_size) raw_data;
      Bit#(nZZ) raw_data;
      if (vnd==1) begin
	 raw_data = unpack(truncateNP(chunk));
      end
      else begin
	 raw_data =
             unpack(truncateNP({chunk, resp_buff} >> valueOf(SizeOf#(Chunk))));
	 resp_buff <= extendNP(raw_data);
	 resp_chunk_count <= (resp_chunk_count==0 ? fromInteger(vnd-1) :
			      resp_chunk_count-1);
      end

      if (vnd==1 || resp_chunk_count==1) begin
	 Bool is_last = resp_count == 0;
	 first <= is_last;

	 TLMResponse#(`TLM_PRM) tlm_response  = fromTLMResponseHeader(header_reg);
	 Bit#(du_size) aa = truncateNP(raw_data);
	 match {.d,.u} = split(aa);
	 tlm_response.data = d;
	 tlm_response.user = u;
	 tlm_response.is_last = is_last;
	 if (is_last)
      	    begin
	       let mark = mark_fifo.first;
	       mark_fifo.deq;
	       if (mark == NOT_LAST)
		  tlm_response.is_last = False;
	    end
	 resp_t response = fromTLMResponse(tlm_response);
	 tlm_tx_fifo.enq(response);
	 resp_count <= resp_count - 1;
      end
   endrule

   interface TLMRecvIFC in;
      interface tx       = toGet(tlm_tx_fifo);
      interface rx       = toPut(tlm_rx_fifo);
   endinterface

   interface Client scemi;
      interface request  = toGet(scemi_tx_fifo);
      interface response = toPut(scemi_rx_fifo);
   endinterface
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {RHeader0, RHeader1, RAddr, RUser, RData} RecvState deriving (Eq, Bits, Bounded);

module mkApiReceiver#(parameter UInt#(32) max_flight) (ApiRecvIFC#(`TLM_RR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(1, SizeOf#(TLMBLength#(`TLM_PRM)), n),
	    Add#(data_size, user_size, du_size),
	    // derived numeric types:
	    NumAlias#(nNA,      TDiv#(addr_size, SizeOf#(Chunk))),
	    NumAlias#(nNU,      TDiv#(user_size, SizeOf#(Chunk))),
	    NumAlias#(nND,      TMax#(1, TDiv#(du_size, SizeOf#(Chunk)))),
	    NumAlias#(nNDE,     TMax#(1, TDiv#(TAdd#(du_size, TDiv#(data_size, 8)), SizeOf#(Chunk)))),
	    NumAlias#(nDE_SIZE, TMul#(nNDE, SizeOf#(Chunk))));

   let valid = checkSizes(valueOf(data_size), valueOf(addr_size), valueOf(user_size));
   let vnd  = valueof(nND);
   let vnu  = valueof(nNU);
   let vnde = valueof(nNDE);

   Reg#(UInt#(n))    data_item_count <- mkReg(0);
   Reg#(LBit#(nNDE)) data_chunk_count <- mkReg(0);

   FIFO#(Chunk)             scemi_rx_fifo <- mkLFIFO;
   FIFO#(Chunk)             scemi_tx_fifo <- mkLFIFO;

   FIFO#(req_t)             tlm_tx_fifo     <- mkLFIFO;
   FIFO#(resp_t)            tlm_rx_fifo     <- mkLFIFO;

   Reg#(TLMRequestHeader)   header          <- mkReg(?);
   Reg#(TLMAddr#(`TLM_PRM)) addr            <- mkReg(?);
   Reg#(TLMUser#(`TLM_PRM)) user            <- mkReg(?);

   Reg#(RecvState)           state          <- mkReg(RHeader0);
   Reg#(LBit#(nNA))          addr_count     <- mkReg(0);
   Reg#(LBit#(nNU))          user_count     <- mkReg(0);

   Reg#(Chunk)               header_chunk_0 <- mkReg(?);

   FIFO#(TLMBLength#(`TLM_PRM)) b_length_fifo <- mkSafeDepthParamFIFO(max_flight);

   Reg#(Bool)                first            <- mkReg(True);
   Reg#(Bit#(du_size))       data_buff        <- mkReg(?);
   Reg#(LBit#(nNDE))         resp_chunk_count <- mkReg(0);

   Reg#(Bit#(du_size))      req_data_buff  <- mkReg(?);
   Reg#(Bit#(nDE_SIZE))     req_de_buff    <- mkReg(?);

   Reg#(Bool)               include_be <- mkReg(False);

   function  LBit#(nNDE) getVND (Bool inc_be);
      let n = (inc_be) ? fromInteger(valueOf(nNDE))
                       : fromInteger(valueOf(nND));
      return n;
   endfunction

   LBit#(nNDE) calc_nd = getVND(include_be);

   ////////////////////////////////////////////////////////////////////////////////
   /// Send out TLM Requests
   ////////////////////////////////////////////////////////////////////////////////

   rule receive_header_0 (state == RHeader0);
      header_chunk_0 <= scemi_rx_fifo.first;
      scemi_rx_fifo.deq;
      state <= RHeader1;
   endrule

   rule receive_header_1 (state == RHeader1);
      TLMRequestHeader current = unpack(truncateNP({scemi_rx_fifo.first, header_chunk_0}));
      include_be <= current.params.spec_byte_enable;
      scemi_rx_fifo.deq;
      header <= current;
      state <= RAddr;
      addr_count <= fromInteger(valueOf(nNA));
      user_count <= fromInteger(valueOf(nNU));
      if (current.params.command == READ)
	 b_length_fifo.enq(truncateNP(current.b_length));
   endrule

   rule add_read_addr (state == RAddr && header.params.command == READ && addr_count != 0);
      addr_count <= addr_count - 1;
      addr <= truncateNP({scemi_rx_fifo.first, addr} >> valueOf(SizeOf#(Chunk))); // lsb first
      scemi_rx_fifo.deq;
      state <= (addr_count == 1) ? ((vnu == 0) ? RHeader0 : RUser) : RAddr; // start over
      if (addr_count == 1 && vnu == 0)
	 begin
	    let descriptor = fromTLMRequestHeader(header);
	    descriptor.addr = truncateNP({scemi_rx_fifo.first, addr} >> valueOf(SizeOf#(Chunk)));
	    tlm_tx_fifo.enq(fromTLMRequest(tagged Descriptor descriptor));
	 end
   endrule

   rule add_write_addr (state == RAddr && header.params.command == WRITE && addr_count != 0);
      addr_count <= addr_count - 1;
      addr <= truncateNP({scemi_rx_fifo.first, addr} >> valueOf(SizeOf#(Chunk))); // lsb first
      scemi_rx_fifo.deq;
      state <= (addr_count == 1) ? ((vnu == 0) ? RData : RUser) : RAddr;
   endrule

   if (vnu != 0)
      begin
	 rule add_read_user (state == RUser && header.params.command == READ && user_count != 0);
	    user_count <= user_count - 1;
	    user <= truncateNP({scemi_rx_fifo.first, user} >> valueOf(SizeOf#(Chunk))); // lsb first
	    scemi_rx_fifo.deq;
	    state <= (extendNP(user_count) == 2'b1) ? RHeader0 : RUser; // start over
	    if (extendNP(user_count) == 2'b1)
	       begin
		  let descriptor = fromTLMRequestHeader(header);
		  descriptor.addr =  addr;
		  descriptor.user_addr = truncateNP({scemi_rx_fifo.first, user} >> valueOf(SizeOf#(Chunk)));
		  descriptor.user = 0;
		  tlm_tx_fifo.enq(fromTLMRequest(tagged Descriptor descriptor));
	       end
	 endrule

	 rule add_write_user (state == RUser && header.params.command == WRITE && user_count != 0);
	    user_count <= user_count - 1;
	    user <= truncateNP({scemi_rx_fifo.first, user} >> valueOf(SizeOf#(Chunk))); // lsb first
	    scemi_rx_fifo.deq;
	    state <= (extendNP(user_count) == 2'b1) ? RData : RUser;
	 endrule
      end

   rule add_data (state == RData && !include_be);
      let chunk = scemi_rx_fifo.first;
      scemi_rx_fifo.deq;

      Bit#(du_size) raw_data;
      if (vnd==1) begin
	 raw_data = truncateNP(chunk);
      end
      else begin
	 raw_data = truncateNP({chunk, req_data_buff} >> valueOf(SizeOf#(Chunk)));
	 req_data_buff <= raw_data;
	 data_chunk_count <= (data_chunk_count==0 ? fromInteger(vnd-1) :
			      data_chunk_count-1);
      end
      if (vnd==1 || data_chunk_count==1) begin
	 let new_item_count = (data_item_count==0 ? truncateNP(getRequestLength(header)-1) :
			       data_item_count-1);
	 Bool is_last = new_item_count == 0;
	 if (addr_count==0) begin // first data item marker
	    let descriptor = fromTLMRequestHeader(header);
	    match {.d,.u} = split(raw_data);
	    descriptor.addr = addr;
	    descriptor.user_addr = user;
	    descriptor.data = d;
	    descriptor.user = u;
	    tlm_tx_fifo.enq(fromTLMRequest(tagged Descriptor descriptor));
	    addr_count <= 1; // indicating data items not first
	 end
	 else begin
	    let descriptor = fromTLMRequestHeader(header);
	    match {.d,.u} = split(raw_data);
	    RequestData#(`TLM_PRM) rd = RequestData {transaction_id: descriptor.transaction_id,
						     data: d,
						     user: u,
						     byte_enable: tagged Calculate,
						     is_last: is_last};
	    tlm_tx_fifo.enq(fromTLMRequest(tagged Data rd));
	 end
	 state <= (is_last) ? RHeader0 : RData;
	 data_item_count <= new_item_count;
      end
   endrule

   rule add_data_be (state == RData && include_be);
      let chunk = scemi_rx_fifo.first;
      scemi_rx_fifo.deq;


      Bit#(nDE_SIZE) new_de;
      if (vnde==1) begin
	 new_de = unpack(truncateNP(chunk));
      end
      else begin
	 new_de =
             unpack(truncateNP({chunk, req_de_buff} >> valueOf(SizeOf#(Chunk))));
	 req_de_buff <= new_de;
	 data_chunk_count <= (data_chunk_count==0 ? fromInteger(vnde-1) :
			      data_chunk_count-1);
      end
      if (vnde==1 || data_chunk_count==1) begin
	 let new_item_count = (data_item_count==0 ? truncateNP(getRequestLength(header)-1) :
			       data_item_count-1);
	 Bool is_last = new_item_count == 0;
	 if (addr_count==0) begin // first data item marker
	    let descriptor = fromTLMRequestHeader(header);
	    Bit#(du_size) raw_data = truncateNP(new_de);
	    match {.d,.u} = split(raw_data);
	    Bit#(TSub#(nDE_SIZE, du_size)) left = truncateLSBNP(new_de);
	    TLMByteEn#(`TLM_PRM) be             = truncateNP(left);

	    descriptor.addr = addr;
	    descriptor.user_addr = user;
	    descriptor.data = d;
	    descriptor.user = u;
	    descriptor.byte_enable = tagged Specify be;
	    tlm_tx_fifo.enq(fromTLMRequest(tagged Descriptor descriptor));
	    addr_count <= 1; // indicating data items not first
	 end
	 else begin
	    let descriptor = fromTLMRequestHeader(header);
	    Bit#(du_size) raw_data = truncateNP(new_de);
	    match {.d,.u} = split(raw_data);
	    Bit#(TSub#(nDE_SIZE, du_size)) left = truncateLSBNP(new_de);
	    TLMByteEn#(`TLM_PRM) be             = truncateNP(left);
	    RequestData#(`TLM_PRM) rd = RequestData {transaction_id: descriptor.transaction_id,
						     data: d,
						     user: u,
						     byte_enable: tagged Specify be,
						     is_last: is_last};
	    tlm_tx_fifo.enq(fromTLMRequest(tagged Data rd));
	 end
	 state <= (is_last) ? RHeader0 : RData;
	 data_item_count <= new_item_count;
      end
   endrule

   ////////////////////////////////////////////////////////////////////////////////
   /// Process TLM Responses;
   ////////////////////////////////////////////////////////////////////////////////

   Reg#(LBit#(nNU)) write_user_count <- mkReg(0);

   (* aggressive_implicit_conditions *)
   rule grab_response (resp_chunk_count == 0 && write_user_count == 0);
      let response = toTLMResponse(tlm_rx_fifo.first);
      let no_user = (vnu == 0 || response.command == READ);
      if (no_user) tlm_rx_fifo.deq;
      let raw = {response.data, response.user};
      Bool is_last = response.is_last && no_user;
      first <= is_last; // next one will be first
      write_user_count <= (response.command == WRITE) ? fromInteger(vnu) : 0;
      if (first) begin
	 let header = toTLMResponseHeader(response);
	 if (header.params.command == READ)
	    begin
	       header.b_length = extendNP(b_length_fifo.first);
	       b_length_fifo.deq;
	    end
	 scemi_tx_fifo.enq(extendNP(pack(header)));
	 if (response.command == READ) begin
	    data_buff <= raw;
	    resp_chunk_count <= fromInteger(vnd);
	 end
      end
      else if (response.command == READ) begin
	 if (vnd==1)
	    scemi_tx_fifo.enq(extendNP(raw));
	 else begin
	    scemi_tx_fifo.enq(truncateNP(raw));
	    data_buff <= raw >> valueof(SizeOf#(Chunk));
	    resp_chunk_count <= fromInteger(vnd-1);
	 end
      end
   endrule

   if (vnu != 0)
      rule send_scemi_user (resp_chunk_count == 0 && write_user_count != 0);
	 let response = toTLMResponse(tlm_rx_fifo.first);
	 Vector#(nNU , Chunk) user_chunks = unpack(extendNP(response.user));
	 let index = fromInteger(vnu) - write_user_count;
	 write_user_count <= write_user_count - 1;
	 scemi_tx_fifo.enq(user_chunks[index]);
	 if (extendNP(write_user_count) == 2'b1)
	    begin
	       first <= True;
	       tlm_rx_fifo.deq;
	    end
      endrule

   rule send_scemi_data (resp_chunk_count!=0);
      let new_count = resp_chunk_count-1;
      resp_chunk_count <= new_count;
      if (vnd==1)
	 scemi_tx_fifo.enq(extendNP(data_buff));
      else begin
	 scemi_tx_fifo.enq(truncateNP(data_buff));
	 data_buff <= data_buff >> valueof(SizeOf#(Chunk));
      end
   endrule



   interface Server scemi;
      interface request  = toPut(scemi_rx_fifo);
      interface response = toGet(scemi_tx_fifo);
   endinterface

   interface TLMSendIFC out;
      interface tx       = toGet(tlm_tx_fifo);
      interface rx       = toPut(tlm_rx_fifo);
   endinterface

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function Bool checkSizes (Integer data_size, Integer addr_size, Integer user_size);
   let valid_data = (data_size == 32  ||
		     data_size == 64  ||
		     data_size == 128 ||
		     data_size == 256 ||
		     data_size == 512 ||
		     data_size == 1024);
   let valid_addr = (addr_size == 32  ||
		     addr_size == 64);
   let valid_user = (user_size == 0   ||
		     user_size == 32  ||
		     user_size == 64);

      return ((!valid_data)  ? error("Unsupported data size: " + integerToString(data_size) + " .") :
	      ((!valid_addr)  ? error("Unsupported address size: " + integerToString(addr_size) + " .") :
	       ((!valid_user)  ? error("Unsupported user size: " + integerToString(user_size) + " .") : True)));

endfunction


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface CFIFO#(type a);
   method Action   enq(a count);
   method Action   deq;
   method Bool     first;
   method Action   clear;
endinterface

module mkCFIFO#(parameter UInt#(32) size) (CFIFO#(a))
   provisos(Bits#(a, sa), Arith#(a), Eq#(a), Literal#(a), Ord#(a));

   FIFO#(a)               fifo     <- mkSafeDepthParamFIFO(size);
   Reg#(a)                c        <- mkReg(0);
   PulseWire              deq_pw   <- mkPulseWire;
   PulseWire              clear_pw <- mkPulseWire;

   (* preempts = "do_clear, update" *)
   (* preempts = "do_clear, deq_1" *)
   (* preempts = "do_clear, deq_n" *)
   rule do_clear (clear_pw);
      fifo.clear;
      c <= 0;
   endrule

   rule update (c == 0);
      c  <= fifo.first;
      fifo.deq;
   endrule

   (* preempts = "deq_1, deq_n" *)
   rule deq_1 (c == 1 && deq_pw);
      c <= fifo.first;
      fifo.deq;
   endrule

   rule deq_n (deq_pw && c != 0);
      c <= c - 1;
   endrule

   method Action enq (a count);
      fifo.enq(count);
   endmethod

   method Action deq if (c!= 0);
      deq_pw.send;
   endmethod

   method Bool first if (c != 0);
      return (c == 1);
   endmethod

   method Action clear;
      fifo.clear;
   endmethod

endmodule

function a truncateLSBNP(b value)
   provisos(Bits#(a, sa), Bits#(b, sb));

   let result = truncateNP(pack(value) >> fromInteger((valueOf(sb) - valueOf(sa))));

   return unpack(result);
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkApiReceiverXL#(parameter UInt#(32) max_flight,
			Integer log_max) (ApiRecvIFC#(`TLM_RR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(req_t, s0),
	    Bits#(resp_t, s1),
	    Add#(0, length_size_ext, SizeOf#(TLMBurstLong)));

   ApiRecvIFC#(TLMRequest#(`TLM_PRM_L_EXT), TLMResponse#(`TLM_PRM_L_EXT))
   receiver <- mkApiReceiver(max_flight);

   TLMTransformIFC#(TLMRequest#(`TLM_PRM_L_EXT), TLMResponse#(`TLM_PRM_L_EXT))
   limit <- mkTLMLimitP(log_max, max_flight);

   mkConnection(receiver.out, limit.in);

   interface scemi = receiver.scemi;
   interface TLMSendIFC out;
      interface Get  tx;
	 method ActionValue#(req_t) get;
	    let value <- limit.out.tx.get;
	    req_t rtn = ?;
	    if (value matches tagged Descriptor .d)
	       begin
		  RequestDescriptor#(`TLM_PRM) r;
		  r.command        = d.command;
		  r.mode           = d.mode;
		  r.addr           = d.addr;
		  r.user_addr      = d.user_addr;
		  r.region         = d.region;
		  r.data           = d.data;
		  r.user           = d.user;
		  r.b_length       = unpack(truncateNP(pack(d.b_length)));
		  r.byte_enable    = unpack(truncateNP(pack(d.byte_enable))); // not really needed
		  r.burst_mode     = d.burst_mode;
		  r.b_size         = d.b_size;
		  r.prty           = d.prty;
		  r.lock           = d.lock;
		  r.transaction_id = d.transaction_id;
		  r.privilege      = d.privilege;
		  r.security       = d.security;
		  r.access         = d.access;
		  r.cache          = d.cache;
		  r.buffer         = d.buffer;
		  r.read_allocate  = d.read_allocate;
		  r.write_allocate = d.write_allocate;
		  r.mark           = d.mark;
		  r.cntrl_flow     = d.cntrl_flow;
		  rtn = fromTLMRequest(tagged Descriptor r);
	       end
	    if (value matches tagged Data .d)
	       begin
		  RequestData#(`TLM_PRM) r = unpack(pack(d));
		  rtn = fromTLMRequest(tagged Data r);
	       end
	    return rtn;
	 endmethod
      endinterface
      interface Put rx;
	 method Action put (value);
	    TLMResponse#(`TLM_PRM_L_EXT) r = unpack(extendNP(pack(value))); // not really needed
	    limit.out.rx.put(r);
	 endmethod
      endinterface
   endinterface
endmodule

endpackage
