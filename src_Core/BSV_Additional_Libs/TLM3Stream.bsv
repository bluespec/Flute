package TLM3Stream;

import DefaultValue::*;
import TLM3Defines::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
/// repurpose unused length_size parameter to set keep/strobe configuration
////////////////////////////////////////////////////////////////////////////////

typedef 0 DFLT;
typedef 1 STRB;
typedef 2 KEEP;
typedef 3 KEEPSTRB;

typedef Bit#(TMul#(TMin#(1, TMax#(0, TSub#(TMax#(1, length_size), 1))), TDiv#(data_size, 8)))                                         TLMKeep#(`TLM_PRM_DCL);
typedef Bit#(TMul#(TSub#(TMax#(length_size, TAdd#(TDiv#(length_size, 2), TDiv#(length_size, 2))), length_size), TDiv#(data_size, 8))) TLMStrb#(`TLM_PRM_DCL);

typedef struct {TLMData#(`TLM_PRM)      data;
		TLMAddr#(`TLM_PRM)      dest;
		TLMUser#(`TLM_PRM)      user;
		TLMId#(`TLM_PRM)        transaction_id;
		TLMByteEn#(`TLM_PRM)    keep;
   		TLMByteEn#(`TLM_PRM)    strb;
   		Bool                    is_last;
		} TLMTransfer#(`TLM_PRM_DCL) deriving (Eq, Bits, Bounded, FShow);

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

instance DefaultValue #(TLMTransfer#(`TLM_PRM));
   function defaultValue ();
      TLMTransfer#(`TLM_PRM) transfer;
      transfer.data           = 0;
      transfer.dest           = 0;
      transfer.user           = 0;
      transfer.transaction_id = 0;
      transfer.keep           = '1;
      transfer.strb           = '1;
      transfer.is_last        = False;
      return transfer;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

typeclass TLMTransferTC#(type a, `TLM_PRM_DCL)
   dependencies (a determines (`TLM_PRM));
   function TLMTransfer#(`TLM_PRM) toTLMTransfer(a value);
   function a                      fromTLMTransfer(TLMTransfer#(`TLM_PRM) value);
endtypeclass

instance TLMTransferTC#(TLMTransfer#(`TLM_PRM), `TLM_PRM);
   function toTLMTransfer   = id;
   function fromTLMTransfer = id;
endinstance

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////


endpackage
