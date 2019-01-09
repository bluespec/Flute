// Copyright (c) 2016-2019 Bluespec, Inc. All Rights Reserved

package External_Control;

// ================================================================
// This package defines control request and response types from an
// external agent (usually GDB) to the SoC.

// ================================================================
// BSV library imports

// None

// ================================================================
// External control requests

typedef struct {
   Bit #(8)     op;
   Bit #(64)    arg1;
   Bit #(64)    arg2;
   } Control_Req
   deriving (Bits, FShow);

// ----------------
// Reads and writes to the Debug Module

Bit #(8) external_control_req_op_read_control_fabric  = 10;    // arg1: fabric_addr
Bit #(8) external_control_req_op_write_control_fabric = 11;    // arg1: fabric_addr, arg2: data

// ================================================================
// External control responses

typedef struct {
   Bit #(8)     status;
   Bit #(64)    result;
   } Control_Rsp
   deriving (Bits, FShow);

Bit #(8) external_control_rsp_status_ok  = 0;
Bit #(8) external_control_rsp_status_err = 1;

// ================================================================

endpackage
