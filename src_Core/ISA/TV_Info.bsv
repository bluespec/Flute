// Copyright (c) 2013-2022 Bluespec, Inc. All Rights Reserved
// Author: Rishiyur S. Nikhil

package TV_Info;

// ================================================================
// Bluespec library imports

import Vector :: *;

// ================================================================
// Trace_Data is encoded by the Core into vectors of bytes, which are
// streamed out to an on-line tandem verifier/ analyzer (or to a file
// for off-line tandem-verification/analysis).

typedef 72   TV_VB_SIZE;    // max bytes needed for each transaction
typedef  Vector #(TV_VB_SIZE, Bit #(8))  TV_Vec_Bytes;

// ================================================================

typedef struct {
   Bit #(32)     num_bytes;
   TV_Vec_Bytes  vec_bytes;
} TV_Info deriving (Bits, FShow);

// ================================================================

endpackage
