// Copyright (c) 2021 Bluespec, Inc. All Rights Reserved

// ================================================================
// Definition of PC Trace Packets.
// The CPU sends out such a packet on each cycle.

// The SoC may sub-sample or compress the packet stream (to save
// bandwidth), record it in a file (in simulation), or transmit it to
// a remote recorder.

// ================================================================

package PC_Trace;

typedef struct {
   Bit #(64)  cycle;
   Bit #(64)  instret;
   Bit #(64)  pc;
   } PC_Trace
deriving (Bits, FShow);

// ================================================================

endpackage
