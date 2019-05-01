// Copyright (c) 2013-2019 Bluespec, Inc.  All Rights Reserved

package C_Imports;

// ================================================================
// These are functions imported into BSV during Bluesim or Verilog simulation.
// See C_Imported_Functions.{h,c} for the corresponding C declarations
// and implementations.

// There are several independent groups of functions below; the
// groups are separated by heavy dividers ('// *******')

// Below, 'dummy' args are not used, and are present only to appease
// some Verilog simulators that are finicky about 0-arg functions.

// ================================================================
// BSV lib imports

import Vector :: *;

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions to measure simulation speed

// ================================================================
// c_start_timing()
// Start the timing interval; argument is current cycle number.

import "BDPI"
function Action c_start_timing (Bit #(64)  cycle_num);

// ================================================================
// c_end_timing()
// End the timing interval; argument is current cycle number,
// and print delta cycles, delta time and simulation speed.

import "BDPI"
function Action c_end_timing (Bit #(64)  cycle_num);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for console I/O

// ================================================================
// c_trygetchar ()
// Returns next input character (ASCII code) from the console.
// Returns 0 if no input is available.

import "BDPI"
function ActionValue #(Bit #(8)) c_trygetchar (Bit #(8) dummy);

// ================================================================
// c_putchar ()
// Writes character to stdout

import "BDPI"
function ActionValue #(Bit #(32)) c_putchar (Bit #(8) ch);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for reading in various parameters

// ================================================================
// c_get_symbol_val ()
// Returns the value of a symbol (a memory address) from a symbol-table file.
// The symbol-table file has a '<symbol> <value-in-hex>' pair on each line.
// Reads the whole symbol-table file on each call,
// which is ok if it's not called often and the file is small.

import "BDPI"
function ActionValue #(Bit #(64)) c_get_symbol_val (String symbol);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for Tandem Verification trace file output.

// ================================================================
// c_trace_file_open ()
// Open file for recording binary trace output.

import "BDPI"
function ActionValue #(Bit #(32)) c_trace_file_open (Bit #(8) dummy);

// ================================================================
// c_trace_file_load_byte_in_buffer ()
// Write 8-bit 'data' into output buffer at byte offset 'j'

import "BDPI"
function ActionValue #(Bit #(32)) c_trace_file_load_byte_in_buffer (Bit #(32) j, Bit #(8) data);

// ================================================================
// c_trace_file_load_word64_in_buffer ()
// Write 64-bit 'data' into output buffer at 'byte_offset'

import "BDPI"
function ActionValue #(Bit #(32)) c_trace_file_load_word64_in_buffer (Bit #(32) byte_offset, Bit #(64) data);

// ================================================================
// c_trace_file_write_buffer ()
// Write out 'n' bytes from the already-loaded output buffer to the trace file.

import "BDPI"
function ActionValue #(Bit #(32)) c_trace_file_write_buffer (Bit #(32)  n);

// ================================================================
// c_trace_file_close()
// Close the trace file.

import "BDPI"
function ActionValue #(Bit #(32))  c_trace_file_close (Bit #(8) dummy);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for communication with remote debug client.

// ****************************************************************
// ****************************************************************
// ****************************************************************

// ================================================================
// Commands in requests.

Bit #(16) dmi_default_tcp_port = 30000;

Bit #(8) dmi_status_err     = 0;
Bit #(8) dmi_status_ok      = 1;
Bit #(8) dmi_status_unavail = 2;

Bit #(8) dmi_op_read          = 1;
Bit #(8) dmi_op_write         = 2;
Bit #(8) dmi_op_shutdown      = 3;
Bit #(8) dmi_op_start_command = 4;

// ================================================================
// Connect to debug client as server on tcp_port.
// Return fail/ok.

import "BDPI"
function ActionValue #(Bit #(8))  c_debug_client_connect (Bit #(16)  tcp_port);

// ================================================================
// Disconnect from debug client as server.
// Return fail/ok.

import "BDPI"
function ActionValue #(Bit #(8))  c_debug_client_disconnect (Bit #(8)  dummy);

// ================================================================
// Receive 7-byte request from debug client
// Result is:    { status, data_b3, data_b2, data_b1, data_b0, addr_b1, addr_b0, op }

import "BDPI"
function ActionValue #(Bit #(64))  c_debug_client_request_recv (Bit #(8)  dummy);

// ================================================================
// Send 4-byte response 'data' to debug client.
// Returns fail/ok status

import "BDPI"
function ActionValue #(Bit #(8))  c_debug_client_response_send (Bit #(32) data);

// ****************************************************************
// ****************************************************************
// ****************************************************************

endpackage
