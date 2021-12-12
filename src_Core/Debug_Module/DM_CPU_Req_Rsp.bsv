// Copyright (c) 2017-2021 Bluespec, Inc. All Rights Reserved.

package DM_CPU_Req_Rsp;

// ================================================================
// This package defines types for register access request and response

// ================================================================
// BSV library imports

// None

// ================================================================
// Project imports

// None

// ================================================================
// Requests and responses

typedef struct {
   Bool      write;
   Bit #(a)  address;
   Bit #(d)  data;
} DM_CPU_Req #(numeric type a, numeric type d)
deriving (Bits, Eq, FShow);

typedef struct {
   Bool     ok;
   Bit #(d) data;
} DM_CPU_Rsp #(numeric type d)
deriving (Bits, Eq, FShow);

// ================================================================

endpackage
