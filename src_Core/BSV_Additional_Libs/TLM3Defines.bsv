// Copyright (c) 2007--2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3Defines;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import BRAM::*;
import BUtils::*;
import CBus :: *;
import Connectable::*;
import DefaultValue::*;
import FIFO::*;
import FIFOF::*;
import FShow::*;
import GetPut::*;
import Randomizable::*;
import Vector::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {DEFAULT, APB, AHB, AXI, AXI4} TLMFamily deriving(Bounded, Bits, Eq);

typedef TLMRequest#(`TLM_PRM_STD)  TLMRequestStd;
typedef TLMResponse#(`TLM_PRM_STD) TLMResponseStd;

typedef enum {READ, WRITE, UNKNOWN}          	    TLMCommand   deriving(Bounded, Bits, Eq);
typedef enum {REGULAR, DEBUG, CONTROL, UNKNOWN}     TLMMode      deriving(Bounded, Bits, Eq);
typedef enum {INCR, WRAP, CNST, UNKNOWN}     	    TLMBurstMode deriving(Bounded, Bits, Eq);


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

// typedef enum {SUCCESS, ERROR, NO_RESPONSE, UNKNOWN} TLMStatus    deriving(Bounded, Bits, Eq);
typedef enum {SUCCESS, ERROR, EXOKAY, UNKNOWN} TLMStatus deriving(Bounded, Bits, Eq);

typedef enum {NONE,              // 0
	      SPLIT_CONTINUE,    // 1 (Ahb)
	      RETRY,             // 2 (Ahb)
	      SPLIT,             // 3 (Ahb)
	      RW_ONLY,           // 4
	      UNMAPPED,          // 5
	      SLVERR,            // 6 (Axi)
	      DECERR             // 7 (Axi)
	      } TLMErrorCode deriving(Bounded, Bits, Eq, FShow);


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef enum {NORMAL, EXCLUSIVE, LOCKED, RESERVED} TLMLock    deriving(Bounded, Bits, Eq);

//typedef UInt#(8)  TLMBurstLength;
typedef UInt#(15) TLMBurstLong;
typedef Bit#(4)  TLMQos;

typedef Bit#(id_size)                    TLMId#(`TLM_PRM_DCL);
typedef Bit#(addr_size)                  TLMAddr#(`TLM_PRM_DCL);
typedef Bit#(data_size)                  TLMData#(`TLM_PRM_DCL);
typedef Bit#(TDiv#(data_size, 8))        TLMByteEn#(`TLM_PRM_DCL);
typedef UInt#(length_size)               TLMBLength#(`TLM_PRM_DCL);
typedef Bit#(user_size)                  TLMUser#(`TLM_PRM_DCL);
// TLMUserPort is obsolete, but left as an alias, for backwards compatibility
typedef TLMUser#(`TLM_PRM)               TLMUserPort#(`TLM_PRM_DCL);

typedef enum { BITS8, BITS16, BITS32, BITS64, BITS128, BITS256, BITS512, BITS1024} TLMBSize deriving(Bounded, Bits, Eq, FShow);

typedef enum {NO_CACHE, CACHE}       TLMCache  deriving(Bounded, Bits, Eq);
//typedef enum {NO_CACHE_MOD, CACHE_MOD} TLMCacheMod  deriving(Bounded, Bits, Eq);
typedef enum {NO_MODIFY, MODIFY}     TLMModify deriving(Bounded, Bits, Eq);
typedef enum {NO_BUFFER, BUFFER}     TLMBuffer deriving(Bounded, Bits, Eq);
typedef enum {NO_ALLOCATE, ALLOCATE} TLMAllocate deriving(Bounded, Bits, Eq);

typedef enum {NORMAL, PRIVILEGED} TLMPrivilege deriving(Bounded, Bits, Eq);
typedef enum {SECURE, NON_SECURE} TLMSecurity  deriving(Bounded, Bits, Eq);
typedef enum {DATA, INST}  TLMAccess    deriving(Bounded, Bits, Eq);
typedef enum {LAST, NOT_LAST, OPEN}  TLMMark deriving(Bounded, Bits, Eq);

typedef Bit#(4) TLMRegion;

typedef struct {TLMCommand              command;
                TLMMode                 mode;
		TLMAddr#(`TLM_PRM)      addr;
		TLMUser#(`TLM_PRM)      user_addr;
		TLMRegion               region;
		TLMData#(`TLM_PRM)      data;
                TLMBLength#(`TLM_PRM)   b_length;
		TLMUser#(`TLM_PRM)      user;
		TLMBEKind#(`TLM_PRM)    byte_enable;
                TLMBurstMode            burst_mode;
                TLMBSize                b_size;
                TLMQos                  prty;
		TLMLock                 lock;
		TLMId#(`TLM_PRM)        transaction_id;
		// protection parameters
		TLMPrivilege            privilege;
		TLMSecurity             security;
                TLMAccess               access;
		// cache parameters
		TLMCache                cache;
		TLMBuffer               buffer;
		TLMAllocate             read_allocate;
		TLMAllocate             write_allocate;
		TLMMark                 mark;
		Bool                    cntrl_flow;
                } RequestDescriptor#(`TLM_PRM_DCL) deriving (Eq, Bits, Bounded);

typedef struct {TLMData#(`TLM_PRM)   data;
		TLMUser#(`TLM_PRM)   user;
		TLMBEKind#(`TLM_PRM) byte_enable;
		TLMId#(`TLM_PRM)     transaction_id;
		Bool                 is_last;
                } RequestData#(`TLM_PRM_DCL) deriving (Eq, Bits, Bounded);


typedef union tagged {RequestDescriptor#(`TLM_PRM) Descriptor;
                      RequestData#(`TLM_PRM)       Data;
                      } TLMRequest#(`TLM_PRM_DCL) deriving(Eq, Bits, Bounded);

typedef struct {TLMCommand           command;
		TLMData#(`TLM_PRM)   data;
                TLMStatus            status;
		TLMUser#(`TLM_PRM)   user;
                TLMQos               prty;
                TLMId#(`TLM_PRM)     transaction_id;
		Bool                 is_last;
//		Bool                 cntrl_flow;
                } TLMResponse#(`TLM_PRM_DCL) deriving (Eq, Bits, Bounded);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance DefaultValue #(RequestDescriptor#(`TLM_PRM));
   function defaultValue ();
      RequestDescriptor#(`TLM_PRM) request;
      request.command        = READ;
      request.mode           = REGULAR;
      request.addr           = 0;
      request.user_addr      = 0;
      request.region         = 0;
      request.data           = 0;
      request.user           = 0;
      request.b_length       = 0;
      request.byte_enable    = tagged Calculate;
      request.burst_mode     = INCR;
      request.b_size         = BITS32; // assume 32 bits for now.
//    request.b_size         = getMaxBSize(valueOf(data_size));
      request.prty           = 0;
      request.lock           = NORMAL;
      request.transaction_id = 0;
      request.privilege      = NORMAL;
      request.security       = SECURE;
      request.access         = DATA;
      request.cache          = NO_CACHE;
      request.buffer         = NO_BUFFER;
      request.read_allocate  = NO_ALLOCATE;
      request.write_allocate = NO_ALLOCATE;
      request.mark           = OPEN;
      request.cntrl_flow     = False;
      return request;
   endfunction
endinstance

instance DefaultValue #(RequestData#(`TLM_PRM));
   function defaultValue ();
      RequestData#(`TLM_PRM) request;
      request.data           = 0;
      request.user           = 0;
      request.byte_enable    = tagged Calculate;
      request.transaction_id = 0;
      request.is_last        = False;
      return request;
   endfunction
endinstance

instance DefaultValue #(TLMResponse#(`TLM_PRM));
   function defaultValue ();
      TLMResponse#(`TLM_PRM) response;
      response.command        = READ;
      response.data           = 0;
      response.user           = 0;
      response.status         = SUCCESS;
      response.prty           = 0;
      response.transaction_id = 0;
//      response.cntrl_flow     = False;
      response.is_last        = False;
      return response;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass TLMRequestTC#(type a, `TLM_PRM_DCL)
   dependencies (a determines (`TLM_PRM));
   function TLMRequest#(`TLM_PRM) toTLMRequest(a value);
   function a                     fromTLMRequest(TLMRequest#(`TLM_PRM) value);
endtypeclass

typeclass TLMResponseTC#(type a, `TLM_PRM_DCL)
   dependencies (a determines (`TLM_PRM));
   function TLMResponse#(`TLM_PRM) toTLMResponse(a value);
   function a                      fromTLMResponse(TLMResponse#(`TLM_PRM) value);
endtypeclass

instance TLMRequestTC#(TLMRequest#(`TLM_PRM), `TLM_PRM);
   function toTLMRequest   = id;
   function fromTLMRequest = id;
endinstance

instance TLMResponseTC#(TLMResponse#(`TLM_PRM), `TLM_PRM);
   function toTLMResponse   = id;
   function fromTLMResponse = id;
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkTLMRandomizer#(Maybe#(TLMCommand) m_command) (Randomize#(a))
   provisos(TLMRequestTC#(a, `TLM_PRM),
            Bits#(RequestDescriptor#(`TLM_PRM), s0),
            Bounded#(RequestDescriptor#(`TLM_PRM)),
            Bits#(RequestData#(`TLM_PRM), s1),
            Bounded#(RequestData#(`TLM_PRM))
            );

   Reg#(TLMBLength#(`TLM_PRM))              count            <- mkReg(0);
   Randomize#(RequestDescriptor#(`TLM_PRM)) descriptor_gen   <- mkGenericRandomizer;
   Randomize#(TLMCommand)                   command_gen      <- mkConstrainedRandomizer(READ, WRITE);     // Avoid UNKNOWN
   Randomize#(TLMBurstMode)                 burst_mode_gen   <- mkConstrainedRandomizer(INCR, WRAP); // Avoid UNKNOWN
   Randomize#(TLMBLength#(`TLM_PRM))        burst_length_gen <- mkConstrainedRandomizer(0,15);            // legal sizes between 1 and 16
   Randomize#(Bit#(2))                      log_wrap_gen     <- mkConstrainedRandomizer(1,3);
   Randomize#(RequestData#(`TLM_PRM))       data_gen         <- mkGenericRandomizer;
   Reg#(TLMId#(`TLM_PRM))                   id               <- mkReg(0);

//   Randomize#(Bit#(TLog#(SizeOf#(TLMBurstSize#(`TLM_PRM))))) log_size_gen <- mkGenericRandomizer;
   TLMBSize max_size = unpack(fromInteger(valueOf(TLog#(TDiv#(data_size, 8)))));
   Randomize#(TLMBSize) b_size_gen <- mkConstrainedRandomizer(BITS8, max_size);

   interface Control cntrl;
      method Action init();
   //       srand(0);
         descriptor_gen.cntrl.init();
         command_gen.cntrl.init();
         burst_mode_gen.cntrl.init();
         burst_length_gen.cntrl.init();
         log_wrap_gen.cntrl.init();
         data_gen.cntrl.init();
         b_size_gen.cntrl.init();
      endmethod
   endinterface

   method ActionValue#(a) next ();

      if (count == 0)
         begin
            let descriptor <- descriptor_gen.next;
            let burst_mode <- burst_mode_gen.next;

            descriptor.command <- command_gen.next;

            let b_size <- b_size_gen.next;

            descriptor.b_size = b_size;

            // align address to burst_size
            let addr = descriptor.addr;
            addr = addr >> pack(b_size);
            addr = addr << pack(b_size);
            descriptor.addr = addr;

            if (burst_mode == WRAP)
               begin
                  let shift <- log_wrap_gen.next;
                  let burst_length = 2 << shift; // wrap legal lengths are 2, 4, 8, 16
                  descriptor.b_length = (burst_length - 1);
                  descriptor.addr = addr;
               end
            else
               begin
                  let burst_length <- burst_length_gen.next;
                  descriptor.b_length = burst_length;
               end

            descriptor.command = case (m_command) matches
                                    tagged Just .x: x;
                                    default       : descriptor.command;
                                 endcase;

            descriptor.mode = REGULAR;
//            descriptor.byte_enable = getTLMByteEnL(descriptor);
	    if (descriptor.b_length == 0 && descriptor.command == WRITE)
	       descriptor.byte_enable = tagged Specify getTLMByteEnL(descriptor);
	    else
	       descriptor.byte_enable = tagged Calculate;

            descriptor.burst_mode = burst_mode;

//            descriptor.thread_id = 0;
            descriptor.transaction_id = id;
//            descriptor.export_id = 0;

            if (descriptor.command == READ)
               begin
                  descriptor.data = 0;
//                  descriptor.byte_enable = '1;
               end
	    a r = fromTLMRequest(tagged Descriptor descriptor);
            let request = toTLMRequest(r);
            let remaining = getTLMCycleCount(descriptor);
            count <= remaining;
            id <= (remaining == 0) ? id + 1 : id;
            return fromTLMRequest(request);
         end
      else
         begin
            let data <- data_gen.next();
            data.transaction_id = unpack({0, id});
	    TLMRequest#(`TLM_PRM) request = tagged Data data;
            let remaining = count - 1;
            count <= remaining;
            id <= (remaining == 0) ? id + 1 : id;
            return fromTLMRequest(request);
         end

      endmethod

endmodule

instance Randomizable#(TLMRequest#(`TLM_PRM))
   provisos(Bits#(RequestDescriptor#(`TLM_PRM), s0),
            Bounded#(RequestDescriptor#(`TLM_PRM)),
            Bits#(RequestData#(`TLM_PRM), s1),
            Bounded#(RequestData#(`TLM_PRM))
            );

   module mkRandomizer (Randomize#(TLMRequest#(`TLM_PRM)));
      let ifc <- mkTLMRandomizer(Invalid);
      return ifc;
   endmodule

endinstance


module mkTLMSource#(Maybe#(TLMCommand) m_command, Bool verbose) (TLMSendIFC#(`TLM_RR))
   provisos(TLMRequestTC#(req_t, `TLM_PRM),
	    TLMResponseTC#(resp_t, `TLM_PRM),
	    Bits#(resp_t, s0));

   Reg#(Bool)                initialized   <- mkReg(False);

   FIFO#(resp_t)             response_fifo <- mkFIFO;
   Randomize#(req_t)         gen           <- mkTLMRandomizer(m_command);

   rule start (!initialized);
      gen.cntrl.init;
      initialized <= True;
   endrule

   rule grab_responses;
      let value = toTLMResponse(response_fifo.first);
      response_fifo.deq;
      if (verbose) $display("(%0d) Response is: ", $time, fshow(value));
   endrule

   interface Get tx;
      method ActionValue#(req_t) get;
         let value <- gen.next;
         if (toTLMRequest(value) matches tagged Descriptor .d)
            if (verbose) $display("(%0d) Request is: ", $time, fshow(d));
         return value;
      endmethod
   endinterface

   interface Put rx = toPut(response_fifo);

endmodule



module mkTLMSourceStd#(Maybe#(TLMCommand) m_command, Bool verbose) (TLMSendIFC#(TLMRequestStd, TLMResponseStd));

   Reg#(Bool)                initialized   <- mkReg(False);
   FIFO#(TLMResponseStd)     response_fifo <- mkFIFO;
   Randomize#(TLMRequestStd) gen           <- mkTLMRandomizer(m_command);

   rule start (!initialized);
      gen.cntrl.init;
      initialized <= True;
   endrule

   rule grab_responses;
      let value = response_fifo.first;
      response_fifo.deq;
      if (verbose) $display("(%0d) Response is: ", $time, fshow(value));
   endrule

   interface Get tx;
      method ActionValue#(TLMRequestStd) get;
         let value <- gen.next;
         if (value matches tagged Descriptor .d)
            if (verbose) $display("(%0d) Request is: ", $time, fshow(d));
         return value;
      endmethod
   endinterface

   interface Put rx = toPut(response_fifo);

endmodule

(* synthesize *)
module mkTLM2Source#(Maybe#(TLMCommand) m_command, Bool verbose) (TLMSendIFC#(TLMRequestStd, TLMResponseStd));

   Reg#(Bool)                initialized   <- mkReg(False);
   FIFO#(TLMResponseStd)     response_fifo <- mkFIFO;
   Randomize#(TLMRequestStd) gen           <- mkTLMRandomizer(m_command);

   rule start (!initialized);
      gen.cntrl.init;
      initialized <= True;
   endrule

   rule grab_responses;
      let value = response_fifo.first;
      response_fifo.deq;
      if (verbose) $display("(%0d) Response is: ", $time, fshow(value));
   endrule

   interface Get tx;
      method ActionValue#(TLMRequestStd) get;
         let value <- gen.next;
         if (value matches tagged Descriptor .d)
            if (verbose) $display("(%0d) Request is: ", $time, fshow(d));
         return value;
      endmethod
   endinterface

   interface Put rx = toPut(response_fifo);

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function TLMBLength#(`TLM_PRM) getTLMCycleCount (RequestDescriptor#(`TLM_PRM) desc);
   if (desc.command == READ)
      return 0;
   else
      return desc.b_length;
endfunction

function Bit#(n) getTLMIncr (RequestDescriptor#(`TLM_PRM) desc)
   provisos(Add#(TLog#(TDiv#(data_size, 8)), 1, n));
   Bit#(n) one = 1;
   if (desc.burst_mode == CNST)
      return 0;
   else
      return one << pack(desc.b_size);
endfunction

function TLMByteEn#(`TLM_PRM) getTLMByteEn (Bool big_endian, RequestDescriptor#(`TLM_PRM) tlm_descriptor);
   let be = getTLMByteEnL(tlm_descriptor);
   return ((big_endian) ? reverseBits(be) : be);
endfunction

function TLMByteEn#(`TLM_PRM) getTLMByteEnU (Bool big_endian, RequestDescriptor#(`TLM_PRM) tlm_descriptor);
   let be = getTLMByteEnUL(tlm_descriptor);
   return ((big_endian) ? reverseBits(be) : be);
endfunction

function TLMByteEn#(`TLM_PRM) getTLMByteEnL (RequestDescriptor#(`TLM_PRM) tlm_descriptor);

   if (valueOf(SizeOf#(TLMByteEn#(`TLM_PRM))) == 1)
      return 1;
   else
      begin
	 Bit#(TLog#(SizeOf#(TLMByteEn#(`TLM_PRM)))) addr = ?;
	 if (valueOf(TLog#(SizeOf#(TLMByteEn#(`TLM_PRM)))) <= valueOf(addr_size))
	    addr = truncateNP(tlm_descriptor.addr);
	 else
	    addr = extendNP(tlm_descriptor.addr);

	 TLMByteEn#(`TLM_PRM) all_ones = unpack('1);
	 let shift = 1 << pack(tlm_descriptor.b_size);
	 let mask = ~(all_ones << shift);
	 return (mask << addr);
      end
endfunction

function TLMByteEn#(`TLM_PRM) getTLMByteEnUL (RequestDescriptor#(`TLM_PRM) tlm_descriptor);

   if (valueOf(SizeOf#(TLMByteEn#(`TLM_PRM))) == 1)
      return 1;
   else
      begin
	 TLMAddr#(`TLM_PRM) ones = '1;
	 let size_shft = pack(tlm_descriptor.b_size);
	 let msk = ones << size_shft;
	 let offset = ~msk & tlm_descriptor.addr;


	 Bit#(TLog#(SizeOf#(TLMByteEn#(`TLM_PRM)))) addr = ?;
	 if (valueOf(TLog#(SizeOf#(TLMByteEn#(`TLM_PRM)))) <= valueOf(addr_size))
	    addr = truncateNP(tlm_descriptor.addr);
	 else
	    addr = extendNP(tlm_descriptor.addr);

	 // align the address
	 addr = addr >> size_shft;
	 addr = addr << size_shft;
	 TLMByteEn#(`TLM_PRM) all_ones = unpack('1);
	 let shift = 1 << size_shft;
	 let mask = ~(all_ones << shift);
	 mask = mask >> offset;
	 mask = mask << offset;
	 return (mask << addr);
      end
endfunction


function RequestDescriptor#(`TLM_PRM) incrTLMAddr(RequestDescriptor#(`TLM_PRM) desc);
   let incr = getTLMIncr(desc);
   let addr = desc.addr + cExtend(incr);
   if (desc.burst_mode == WRAP)
      begin
	 // This code assumes a valid wrap burst_length
	 // therefore b_length will be of the form: 00000111 (i.e 0,
	 // 1, 3, 7 etc)
	 Vector#(TAdd#(length_size, 1), Bit#(1)) bit_vector = unpack(extendNP(pack(desc.b_length)));
	 Bit#(4) offset = foldr1(\+ , map(extend, bit_vector));
	 Bit#(4) total = offset + extendNP(pack(desc.b_size));
	 TLMAddr#(`TLM_PRM) mask_bar = '1 << total;
	 addr = (addr & ~mask_bar) | (desc.addr & mask_bar);
      end
   desc.addr = addr;
   return desc;
endfunction

function Bit#(n) countLSBZeros (Bit#(n) value);
   Vector#(n, Bool) vector_value = unpack(value);
   let pos = findIndex(id, vector_value);
   case (pos) matches
      tagged Valid .p: return zExtend(pack(p));
      tagged  Invalid: return fromInteger(valueOf(n));
   endcase
endfunction


function TLMData#(`TLM_PRM) getTLMData(TLMRequest#(`TLM_PRM) request);
   case (request) matches
      (tagged Descriptor .d): return d.data;
      (tagged Data .d)      : return d.data;
   endcase
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function RequestDescriptor#(`TLM_PRM) incrTLMAddrN(Integer log_n, RequestDescriptor#(`TLM_PRM) desc);
   Bit#(addr_size) incr = cExtend(getTLMIncr(desc));
   let addr = desc.addr + (incr << log_n);
   desc.addr = addr;
   return desc;
   // return ((desc.burst_mode == INCR || desc.burst_mode == CNST) ? desc : error("incrTLMAddrN cannot be applied to WRAP descriptors."));
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance FShow#(TLMCommand);
   function Fmt fshow (TLMCommand label);
      case (label)
         READ:    return fshow("READ ");
         WRITE:   return fshow("WRITE");
         UNKNOWN: return fshow("UNKNOWN");
      endcase
   endfunction
endinstance

instance FShow#(TLMMode);
   function Fmt fshow (TLMMode label);
      case (label)
         REGULAR: return fshow("REG");
         DEBUG:   return fshow("DBG");
         CONTROL: return fshow("CTL");
	 UNKNOWN: return fshow("UNK");
      endcase
   endfunction
endinstance

instance FShow#(TLMBurstMode);
   function Fmt fshow (TLMBurstMode label);
      case (label)
         INCR: return fshow("INCR");
         CNST: return fshow("CNST");
         WRAP: return fshow("WRAP");
      endcase
   endfunction
endinstance

function Fmt fshowBurstMode (RequestDescriptor#(`TLM_PRM) op);
   case (op.burst_mode)
         INCR: return ($format("INCR ") + fshow(op.b_size));
         CNST: return ($format("CNST ") + fshow(op.b_size));
         WRAP: return ($format("WRAP ") + fshow(op.b_size));
      endcase
endfunction

instance FShow#(TLMStatus);
   function Fmt fshow (TLMStatus label);
      case (label)
         SUCCESS:     return fshow("SUCCESS");
         ERROR:       return fshow("ERROR  ");
 	 EXOKAY:      return fshow("EXOKAY ");
	 UNKNOWN:     return fshow("UNKNOWN");
      endcase
   endfunction
endinstance

instance FShow#(TLMLock);
   function Fmt fshow (TLMLock label);
      case (label)
         NORMAL:     return fshow("NORMAL   ");
         EXCLUSIVE:  return fshow("EXCLUSIVE");
	 LOCKED:     return fshow("LOCKED   ");
	 RESERVED:   return fshow("RESERVED ");
      endcase
   endfunction
endinstance


instance FShow#(TLMPrivilege);
   function Fmt fshow (TLMPrivilege label);
      case (label)
         NORMAL:     return fshow("NORMAL    ");
         PRIVILEGED: return fshow("PRIVILEGED");
      endcase
   endfunction
endinstance

instance FShow#(TLMSecurity);
   function Fmt fshow (TLMSecurity label);
      case (label)
         SECURE:     return fshow("SECURE    ");
         NON_SECURE: return fshow("NON_SECURE");
      endcase
   endfunction
endinstance

instance FShow#(TLMAccess);
   function Fmt fshow (TLMAccess label);
      case (label)
         DATA:        return fshow("DATA");
         INST: return fshow("INST");
      endcase
   endfunction
endinstance

instance FShow#(TLMCache);
   function Fmt fshow (TLMCache label);
      case (label)
         NO_CACHE: return fshow("NO_CACHE");
         CACHE:    return fshow("CACHE");
      endcase
   endfunction
endinstance

instance FShow#(TLMModify);
   function Fmt fshow (TLMModify label);
      case (label)
         NO_MODIFY: return fshow("NO_MODIFY");
         MODIFY:    return fshow("MODIFY");
      endcase
   endfunction
endinstance

instance FShow#(TLMBuffer);
   function Fmt fshow (TLMBuffer label);
      case (label)
         NO_BUFFER: return fshow("NO_BUFFER");
         BUFFER:    return fshow("BUFFER");
      endcase
   endfunction
endinstance

instance FShow#(TLMAllocate);
   function Fmt fshow (TLMAllocate label);
      case (label)
         NO_ALLOCATE: return fshow("NO_ALLOCATE");
         ALLOCATE:    return fshow("ALLOCATE");
      endcase
   endfunction
endinstance

instance FShow#(TLMMark);
   function Fmt fshow (TLMMark label);
      case (label)
         LAST:     return fshow("LAST");
         NOT_LAST: return fshow("NOT_LAST");
	 OPEN:     return fshow("OPEN");
      endcase
   endfunction
endinstance

instance FShow#(RequestData#(`TLM_PRM));

   function Fmt fshow (RequestData#(`TLM_PRM) data);
      return ($format("<TDATA [%0d] %h", data.transaction_id, data.data)
	      +
	      ((data.is_last) ? fshow(" (LAST)>") : fshow(">")));
   endfunction
endinstance

instance FShow#(RequestDescriptor#(`TLM_PRM));

   function Fmt fshow (RequestDescriptor#(`TLM_PRM) op);
      return ($format("<TDESC [%0d] ", op.transaction_id)
              +
              fshow(op.command)
              +
              fshow(" ")
              +
              fshow(op.lock)
              +
              fshow(" ")
              +
              fshowBurstMode(op)
              +
              $format(" (%0d)", { 1'b0, pack(op.b_length)} + 1)
              +
              $format(" A:%h", op.addr)
              +
              $format(" D:%h>", op.data));
   endfunction
endinstance

instance FShow#(TLMRequest#(`TLM_PRM))
   provisos(FShow#(RequestData#(`TLM_PRM)),
            FShow#(RequestDescriptor#(`TLM_PRM)));

   function Fmt fshow (TLMRequest#(`TLM_PRM) request);
      case (request) matches
         tagged Descriptor .a:
            return fshow(a);
         tagged Data .a:
            return fshow(a);
      endcase
   endfunction
endinstance

instance FShow#(TLMResponse#(`TLM_PRM));
   function Fmt fshow (TLMResponse#(`TLM_PRM) response);
      TLMErrorCode code = unpack(truncateNP(response.data));
      let code_or_data = (response.status == ERROR) ? fshow(code) :  $format(" %h", response.data);
      let f = $format("<TRESP [%0d] ", response.transaction_id)
              +
              fshow(response.command)
              +
              fshow(" ")
              +
              fshow(response.status)
              +
              code_or_data
              +
              ((response.is_last && response.command != WRITE) ? fshow(" (LAST)>") : fshow(">"));
      return f;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface TLMSendIFC#(type req, type resp);
   interface Get#(req)  tx;
   interface Put#(resp) rx;
endinterface

interface TLMRecvIFC#(type req, type resp);
   interface Get#(resp) tx;
   interface Put#(req)  rx;
endinterface

interface TLMReadWriteSendIFC#(type req, type resp);
   interface TLMSendIFC#(req, resp) read;
   interface TLMSendIFC#(req, resp) write;
endinterface

interface TLMReadWriteRecvIFC#(type req, type resp);
   interface TLMRecvIFC#(req, resp) read;
   interface TLMRecvIFC#(req, resp) write;
endinterface

interface TLMTransformIFC#(type req, type resp);
   interface TLMRecvIFC#(req, resp) in;
   interface TLMSendIFC#(req, resp) out;
endinterface

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance Connectable#(TLMSendIFC#(req, resp), TLMRecvIFC#(req, resp));
   module mkConnection#(TLMSendIFC#(req, resp) request, TLMRecvIFC#(req, resp) response) (Empty);
      mkConnection(request.tx, response.rx);
      mkConnection(request.rx, response.tx);
   endmodule
endinstance

instance Connectable#(TLMRecvIFC#(req, resp), TLMSendIFC#(req, resp));
   module mkConnection#(TLMRecvIFC#(req, resp) response, TLMSendIFC#(req, resp) request) (Empty);
      mkConnection(request.tx, response.rx);
      mkConnection(request.rx, response.tx);
   endmodule
endinstance

instance Connectable#(TLMReadWriteSendIFC#(req, resp), TLMReadWriteRecvIFC#(req, resp));
   module mkConnection#(TLMReadWriteSendIFC#(req, resp) request, TLMReadWriteRecvIFC#(req, resp) response) (Empty);
      let read_con  <- mkConnection(request.read,  response.read);
      let write_con <- mkConnection(request.write, response.write);
   endmodule
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass AddrMatch#(type addr_t, type ifc_t);
   function ifc_t addAddrMatch(function Bool addrMatch(addr_t value), ifc_t value);
endtypeclass

function Bool alwaysAddrMatch(a value);
   return True;
endfunction

function RequestDescriptor#(`TLM_PRM) addByteEnable (Bool big_endian, RequestDescriptor#(`TLM_PRM) request);
   let                  request_new = request;
   TLMByteEn#(`TLM_PRM) be = getTLMByteEn(big_endian, request);
   request_new.byte_enable = tagged Specify be;
   return request_new;
endfunction

function RequestDescriptor#(`TLM_PRM) addByteEnableU (Bool big_endian, RequestDescriptor#(`TLM_PRM) request);
   let                  request_new = request;
   TLMByteEn#(`TLM_PRM) be = getTLMByteEnU(big_endian, request);
   request_new.byte_enable = tagged Specify be;
   return request_new;
endfunction

function RequestDescriptor#(`TLM_PRM) alignAddress (RequestDescriptor#(`TLM_PRM) request);
   let addr = request.addr;
   addr = addr >> pack(request.b_size);
   addr = addr << pack(request.b_size);
   let d = request;
   d.addr = addr;
   return d;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance TLMRequestTC#(BRAMRequest#(Bit#(addr_size), Bit#(data_size)), `TLM_PRM)
   provisos (DefaultValue #(BRAMRequest#(Bit#(addr_size), Bit#(data_size)))
             ,DefaultValue #( RequestDescriptor#(`TLM_PRM) ));

   function TLMRequest#(`TLM_PRM) toTLMRequest(BRAMRequest#(Bit#(addr_size), Bit#(data_size)) value);
      RequestDescriptor#(`TLM_PRM) request = defaultValue;
      request.command  = value.write ? WRITE : READ;
      request.data     = value.datain;
      request.addr     = value.address;
      // responseOnWrite must be true as TLM always generates a response
      return tagged Descriptor request;
   endfunction

   function BRAMRequest#(Bit#(addr_size), Bit#(data_size)) fromTLMRequest(TLMRequest#(`TLM_PRM) value);
      BRAMRequest#(Bit#(addr_size), Bit#(data_size)) brequest = defaultValue ;
      case (value) matches
         tagged Descriptor .desc:
            begin
               brequest.write     = desc.command == WRITE ;
               brequest.datain    = desc.data;
               brequest.address   = desc.addr;
               return brequest;
            end
         tagged Data .data:
            begin
               // XXXX should never occur
               return brequest;
            end
      endcase
   endfunction
endinstance

instance TLMResponseTC#(Bit#(data_size), `TLM_PRM)
   provisos (DefaultValue# (TLMResponse#(`TLM_PRM))
             );
   function TLMResponse#(`TLM_PRM)  toTLMResponse (Bit#(data_size) value );
      TLMResponse#(`TLM_PRM) response = defaultValue ;
      response.data = value ;
      return response ;
   endfunction
   function Bit#(data_size) fromTLMResponse (TLMResponse#(`TLM_PRM) value);
      return value.data;
   endfunction
endinstance

instance TLMRequestTC#(BRAMRequestBE#(Bit#(addr_size), Bit#(data_size), n), `TLM_PRM)
   provisos (DefaultValue #(BRAMRequest#(Bit#(addr_size), Bit#(data_size)))
             ,DefaultValue #( RequestDescriptor#(`TLM_PRM) )
             ,Div#(data_size,8,n)
             ,Div#(data_size,8,TDiv#(data_size,8))
             );

   function TLMRequest#(`TLM_PRM) toTLMRequest(BRAMRequestBE#(Bit#(addr_size), Bit#(data_size), n) value);
      RequestDescriptor#(`TLM_PRM) request = defaultValue;
      request.command     = value.writeen != 0 ? WRITE : READ;
      request.data        = value.datain;
      request.addr        = value.address;
//      request.byte_enable = value.writeen != 0 ? value.writeen : '1 ;
      request.byte_enable = tagged Calculate;
      // responseOnWrite must be true as TLM always generates a response
      return tagged Descriptor request;
   endfunction

   function BRAMRequestBE#(Bit#(addr_size), Bit#(data_size), n) fromTLMRequest(TLMRequest#(`TLM_PRM) value);
      BRAMRequestBE#(Bit#(addr_size), Bit#(data_size),n) brequest = defaultValue ;
      case (value) matches
         tagged Descriptor .desc:
            begin
//               brequest.writeen   = desc.command == WRITE ?  desc.byte_enable : 0 ;
               brequest.datain    = desc.data;
               brequest.address   = desc.addr;
               return brequest;
            end
         tagged Data .data:
            begin
               // XXXX should never occur
               return brequest;
            end
      endcase
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function Bool isValidRequest(TLMRequest#(`TLM_PRM) request, TLMFamily family);
   return True;
endfunction

function Bool isValidResponse(TLMResponse#(`TLM_PRM) request, TLMFamily family);
   return True;
endfunction

module wrapFIFO#(FIFO#(a) fifo) (FIFO#(a));

   PulseWire     do_enq <- mkPulseWire;
   PulseWire     do_deq <- mkPulseWire;

   Reg#(Bit#(8)) count     <- mkReg(0);
   Reg#(Bit#(8)) max_count <- mkReg(0);

   rule incr (do_enq && !do_deq);
      count     <= count + 1;
      max_count <= max(max_count, count + 1);
      if ((count + 1) > max_count)
	 $display("(%0d) %m MAX is: %0d", $time, count + 1);
   endrule

   rule decr (!do_enq && do_deq);
      count <= count - 1;
   endrule

   method Action enq (a value);
      do_enq.send;
      fifo.enq(value);
   endmethod
   method Action deq ();
      do_deq.send;
      fifo.deq;
   endmethod
   method first = fifo.first;
   method clear = fifo.clear;

endmodule

module wrapFIFOF#(FIFOF#(a) fifo) (FIFOF#(a));

   PulseWire     do_enq <- mkPulseWire;
   PulseWire     do_deq <- mkPulseWire;

   Reg#(Bit#(8)) count     <- mkReg(0);
   Reg#(Bit#(8)) max_count <- mkReg(0);

   rule incr (do_enq && !do_deq);
      count     <= count + 1;
      max_count <= max(max_count, count + 1);
      if ((count + 1) > max_count)
	 $display("(%0d) %m MAX is: %0d", $time, count + 1);
   endrule

   rule decr (!do_enq && do_deq);
      count <= count - 1;
   endrule

   method Action enq (a value);
      do_enq.send;
      fifo.enq(value);
   endmethod
   method Action deq ();
      do_deq.send;
      fifo.deq;
   endmethod
   method first = fifo.first;
   method clear = fifo.clear;
   method notEmpty = fifo.notEmpty;
   method notFull  = fifo.notFull;

endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typedef union tagged {void                 Calculate;
                      TLMByteEn#(`TLM_PRM) Specify;
                      } TLMBEKind#(`TLM_PRM_DCL) deriving(Eq, Bits, Bounded);


////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function TLMBSize getMaxBSize (Integer data_width);
   case (data_width)
      8:    return BITS8;
      16:   return BITS16;
      32:   return BITS32;
      64:   return BITS64;
      128:  return BITS128;
      256:  return BITS256;
      512:  return BITS512;
      1024: return BITS1024;
   endcase
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

module mkSafeDepthParamFIFO#(parameter UInt#(32) depth) (FIFO#(a))
   provisos(Bits#(a, sa));
   (* hide *)
   let _ifc <- mkDepthParamFIFO(max(2,depth));
   return _ifc;
endmodule

module mkSafeDepthParamFIFOF#(parameter UInt#(32) depth) (FIFOF#(a))
   provisos(Bits#(a, sa));
   (* hide *)
   let _ifc <- mkDepthParamFIFOF(max(2,depth));
   return _ifc;
endmodule

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

interface SFIFO#(type a, type b);
   method Action  enq (a value);
   method Action  deq;
   method a       first;
   method Action  clear;
endinterface

module mkSizedSFIFO#(Integer n) (SFIFO#(a, b))
   provisos (Bits#(a,sa), Bits#(b, sb), Arith#(b), Eq#(b));

   // If the queue contains n elements, they are in q[0]..q[n-1].  The head of
   // the queue (the "first" element) is in q[0], the tail in q[n-1].

   Tuple2#(b, a) dflt = tuple2(0, ?);

   Reg#(Tuple2#(b, a)) q[n];
   for (Integer i=0; i<n; i=i+1)
      q[i] <- mkReg(dflt);
   TCounter c <- mkTCounter(n);

   PulseWire enqueueing <- mkPulseWire;
   Wire#(Tuple2#(b, a)) x_wire <- mkWire;
   PulseWire dequeueing <- mkPulseWire;

   let empty = c.isEq(0);
   let full  = c.isEq(n);

   rule incCtr (enqueueing && !dequeueing);
      c.incr;
      c.setNext(x_wire, q);
   endrule

   rule decCtr (dequeueing && !enqueueing);
      for (Integer i=0; i<n; i=i+1)
	 q[i] <= (i==(n - 1) ? dflt : q[i + 1]);
      c.decr;
   endrule

   rule both (dequeueing && enqueueing);
      for (Integer i=0; i<n; i=i+1)
	 if (!c.isEq(i + 1)) q[i] <= (i==(n - 1) ? dflt : q[i + 1]);
      c.set(x_wire, q);
   endrule

   method Action deq if (!empty);
      match {.n, .d} = q[0];
      if (n == 1)
	 dequeueing.send;
      else
	 q[0] <= tuple2(n-1, d);
   endmethod

   method first if (!empty);
      match {.n, .d} = q[0];
      return d;
   endmethod

   method Action enq(x) if (!full);
      match {.n, .d} =  c.get(q);
      if (n == 0 || (n + 1) == 0 || pack(x) != pack(d))
	 begin
	    enqueueing.send;
	    x_wire <= tuple2(1, x);
	 end
      else
	 c.set(tuple2(n+1, d), q);
   endmethod

//   method notEmpty = !empty;
//   method notFull  = !full;

   method Action clear;
      c.clear;
   endmethod
endmodule

interface TCounter;
   method Action incr;
   method Action decr;
   method Bool isEq(Integer n);
   method Action setNext (b value, Reg#(b) as[]);
   method Action set (b value, Reg#(b) as[]);
   method b getNext(Reg#(b) as[]);
   method b get(Reg#(b) as[]);
   method Action clear;
endinterface

module mkTCtr#(Reg#(UInt#(s)) c)(TCounter);
   method Action incr; c <= c+1; endmethod
   method Action decr; c <= c-1; endmethod
   method isEq(n) = (c==fromInteger(n));
   method Action setNext (b value, Reg#(b) as[]); as[c] <= value; endmethod
   method Action set (b value, Reg#(b) as[]); as[c-1] <= value; endmethod
   method b getNext(Reg#(b) as[]);
      return as[c]._read;
   endmethod
   method b get(Reg#(b) as[]);
      return as[c-1]._read;
   endmethod
   method Action clear; c <= 0; endmethod
endmodule

// A counter which can count up to m inclusive (m known at compile time):
module mkTCounter#(Integer m)(TCounter);
   let _i = ?;
   if      (m<2)      begin Reg#(UInt#(1))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<4)      begin Reg#(UInt#(2))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<8)      begin Reg#(UInt#(3))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<16)     begin Reg#(UInt#(4))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<32)     begin Reg#(UInt#(5))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<64)     begin Reg#(UInt#(6))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<128)    begin Reg#(UInt#(7))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<256)    begin Reg#(UInt#(8))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<512)    begin Reg#(UInt#(9))  r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<1024)   begin Reg#(UInt#(10)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<2048)   begin Reg#(UInt#(11)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<4096)   begin Reg#(UInt#(12)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<8192)   begin Reg#(UInt#(13)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<16384)  begin Reg#(UInt#(14)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<32768)  begin Reg#(UInt#(15)) r <- mkReg(0); _i <- mkTCtr(r); end
   else if (m<65536)  begin Reg#(UInt#(16)) r <- mkReg(0); _i <- mkTCtr(r); end
   return _i;
endmodule


module mkSafeDepthParamSFIFO#(parameter UInt#(32)  depth) (SFIFO#(a, b))
   provisos (Bits#(a,sa), Bits#(b, sb), Arith#(b), Bounded#(b), Eq#(b), Eq#(a));

   Reg#(Maybe#(Tuple2#(b, a))) head_reg <- mkReg(tagged Invalid);
   Reg#(Maybe#(Tuple2#(b, a))) tail_reg <- mkReg(tagged Invalid);
   FIFOF#(Tuple2#(b, a))       fifo     <- mkSafeDepthParamFIFOF(depth);

   PulseWire deq_pw <- mkPulseWire;
   PulseWire deq_tail_count <- mkPulseWire;

   Wire#(a)             enqValue <- mkWire;
   RWire#(a) deqValue  <- mkRWire;

   Bool can_enq = fifo.notFull;
   Bool normalOp = fifo.notEmpty || isValid(head_reg);

   // Noraml enqueue operations
   rule nEnqMove (normalOp &&& tail_reg matches tagged Valid {.nt, .dt} &&&
                    ( (nt == maxBound) || (dt != enqValue) ) );
      fifo.enq(tuple2(nt,dt));
      tail_reg <= tagged Valid tuple2(1,enqValue);
   endrule
   rule nEnqIncr (normalOp &&& tail_reg matches tagged Valid {.nt, .dt} &&&
                    ( (nt != maxBound) && (dt == enqValue) ) );
      tail_reg <= tagged Valid tuple2(nt+1,enqValue);
   endrule

   // Normal Deq operation
   rule nDeqDecr (normalOp &&& deq_pw &&& head_reg matches tagged Valid {.nh, .dh} &&&
                  nh != 1);
      head_reg <= tagged Valid tuple2 (nh-1,dh);
   endrule
   (*preempts = "nDeqMove, nDeqLast"*)
   rule nDeqMove (normalOp &&& deq_pw &&& head_reg matches tagged Valid {.nh, .dh} &&&
                  nh == 1);
      head_reg <= tagged Valid (fifo.first);
      fifo.deq;
   endrule
   rule nDeqLast (normalOp &&& deq_pw &&& head_reg matches tagged Valid {.nh, .dh} &&&
                  nh == 1 );
      head_reg <= tagged Invalid;
   endrule

   // Special case rules

   // Enq and Deq with last element in head and fifo is empty
   (* preempts = "sEnqDeqLast, (nEnqMove, nDeqLast)" *)
   rule sEnqDeqLast ((! fifo.notEmpty) &&& deq_pw &&&
                     head_reg matches tagged Valid { .nh, .dh} &&&
                     nh == 1 &&&
                     tail_reg matches tagged Valid {.nt, .dt} &&&
                    ( (nt == maxBound) || (dt != enqValue) ));
      tail_reg <= tagged Valid tuple2(1,enqValue);
      head_reg <= tail_reg;
   endrule

   rule sEnqFirst (tail_reg matches tagged Invalid);
      tail_reg <= tagged Valid tuple2(1, enqValue);
   endrule
   rule sEnqMove ( (!normalOp) && (!deq_pw) &&&
                  tail_reg matches tagged Valid {.nt, .dt} &&&
                  ( (nt == maxBound) || (dt != enqValue) ) );
      head_reg <= tail_reg;
      tail_reg <= tagged Valid tuple2(1,enqValue);
   endrule
   rule sEnqIncr ( (!normalOp) && (!deq_pw) &&&
                  tail_reg matches tagged Valid {.nt, .dt} &&&
                  ( (nt != maxBound) && (dt == enqValue) ) );
      tail_reg <= tagged Valid tuple2(nt+1,enqValue);
   endrule

   (*preempts = "sEnqDeq, (sDeqLast,sDeqDecr)"*)
   rule sEnqDeq ( (!normalOp) && deq_pw &&&
                 tail_reg matches tagged Valid {.nt, .dt});
      if (dt != enqValue) begin
         if (nt != 1) begin
            head_reg <= tagged Valid tuple2 (nt-1, dt);
         end                    // else head_reg <= tagged Invalid (same as hold)
         tail_reg <= tagged Valid tuple2 (1,enqValue);
      end
      else begin
         noAction; // +1 - 1
      end
   endrule

   rule sDeqDecr ( (!normalOp) && deq_pw &&&
                  tail_reg matches tagged Valid { .nt, .dt } &&&
                  nt != 1 );
      tail_reg <= tagged Valid tuple2(nt-1, dt);
   endrule
   rule sDeqLast ( (!normalOp) && deq_pw &&&
                  tail_reg matches tagged Valid { .nt, .dt } &&&
                  nt == 1 );
      tail_reg <= tagged Invalid;
   endrule


   // Rule to move deq value to a wire
   rule firstFromTail (head_reg matches tagged Invalid &&&
                           tail_reg matches tagged Valid {.n0, .d0});
      deqValue.wset (d0);
   endrule

   rule firstFromHead (head_reg matches tagged Valid {.n0, .d0});
      deqValue.wset (d0);
   endrule


   method Action enq(a x) if (can_enq);
      enqValue <= x;
   endmethod

   method Action deq() if (deqValue.wget matches tagged Valid .d);
      deq_pw.send;
   endmethod

   method first() if (deqValue.wget matches tagged Valid .d);
      return d;
   endmethod

   method Action clear;
      fifo.clear;
      head_reg <= tagged Invalid;
      tail_reg <= tagged Invalid;
   endmethod

   //method Bool notFull = can_enq;
   //method Bool notEmpty = (deqValue matches tagged Valid .d);

endmodule


function FIFO#(a) fromSFIFO (SFIFO#(a, b) ifc);

   return (interface FIFO;
	      method enq   = ifc.enq;
	      method deq   = ifc.deq;
	      method first = ifc.first;
	      method clear = ifc.clear;
	   endinterface);

endfunction

endpackage

