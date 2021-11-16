// Copyright (c) 2007--2013 Bluespec, Inc.  All rights reserved.
// $Revision$
// $Date$

package TLM3Utils;

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

import BUtils::*;
import DefaultValue::*;
import TLM3Defines::*;
import Vector::*;

`include "TLM.defines"

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////

function RequestDescriptor#(`TLM_PRM) createBasicRequestDescriptor()
   provisos(DefaultValue#(RequestDescriptor#(`TLM_PRM)));
   return defaultValue;
endfunction

/* -----\/----- EXCLUDED -----\/-----
function TLMResponse#(`TLM_PRM) createTLMResponse(TLMId#(`TLM_PRM) id, TLMStatus status)
   provisos(Bits#(TLMResponse#(`TLM_PRM), s0));
   TLMResponse#(`TLM_PRM) response = unpack(0);
   response.status = status;
   response.transaction_id = id;
   return response;
endfunction
 -----/\----- EXCLUDED -----/\----- */

function TLMResponse#(`TLM_PRM) createBasicTLMResponse ()
   provisos(DefaultValue#(TLMResponse#(`TLM_PRM)));
   return defaultValue;
endfunction

////////////////////////////////////////////////////////////////////////////////
///
////////////////////////////////////////////////////////////////////////////////


function Bit#(data_size) createTLMBitMask ( TLMBEKind#(`TLM_PRM) byte_enable)
//   provisos(Div#(data_size, 8, b))
;

   Vector#(TDiv#(data_size, 8),Bit#(8)) mask = replicate('1);
   if (byte_enable matches tagged Specify .enable_bits) begin
      Vector#(TDiv#(data_size, 8),Bit#(1)) enable = unpack(enable_bits);
      mask = map(signExtend, enable);
   end

   return cExtend(mask);

endfunction

function Bit#(data_size) maskTLMData( TLMBEKind#(`TLM_PRM) byte_enable, Bit#(data_size) data)
//   provisos(Div#(f, 8, b));
   ;

   Bit#(data_size) mask = createTLMBitMask(byte_enable);

   return mask & data;

endfunction

function Bit#(n) overwriteTLMData( TLMBEKind#(`TLM_PRM) byte_enable, Bit#(n) data_orig, Bit#(n) data)
   provisos(NumAlias#(n, data_size),
            Div#(n, 8, b));

   Bit#(n) mask = createTLMBitMask(byte_enable);

   return (~mask & data_orig) | (mask & data);

endfunction

endpackage
