// Copyright (c) 2016-2019 Bluespec, Inc.  All Rights Reserved

#pragma once

// ================================================================
// These are functions imported into BSV during Bluesim or Verilog simulation.
// See C_Imports.bsv for the corresponding 'import BDPI' declarations.

// There are several independent groups of functions below; the
// groups are separated by heavy dividers ('// *******')

// Below, 'dummy' args are not used, and are present only to appease
// some Verilog simulators that are finicky about 0-arg functions.

// ================================================================

#ifdef __cplusplus
extern "C" {
#endif

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions to measure simulation speed

// ================================================================
// c_start_timing()
// Start the timing interval; argument is current cycle number.

extern
void c_start_timing (uint64_t  cycle_num);

// ================================================================
// c_end_timing()
// End the timing interval; argument is current cycle number,
// and print delta cycles, delta time and simulation speed.

extern
void c_end_timing (uint64_t  cycle_num);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for console I/O

// ================================================================
// c_trygetchar()
// Returns next input character (ASCII code) from the console.
// Returns 0 if no input is available.

extern
uint8_t c_trygetchar (uint8_t  dummy);

// ================================================================
// c_putchar()
// Writes character to stdout

extern
uint32_t c_putchar (uint8_t ch);

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

extern
uint64_t c_get_symbol_val (char * symbol);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for Tandem Verification trace file output.

// ================================================================
// c_trace_file_open()
// Open file for recording binary trace output.

extern
uint32_t c_trace_file_open (uint8_t dummy);

// ================================================================
// c_trace_file_load_byte_in_buffer ()
// Write 8-bit 'data' into output buffer at byte offset 'j'

extern
uint32_t c_trace_file_load_byte_in_buffer (uint32_t j, uint8_t data);

// ================================================================
// c_trace_file_load_word64_in_buffer ()
// Write 64-bit 'data' into output buffer at 'byte_offset'

extern
uint32_t c_trace_file_load_word64_in_buffer (uint32_t byte_offset, uint64_t data);

// ================================================================
// c_trace_file_write_buffer()
// Write out 'n' bytes from the already-loaded output buffer to the trace file.

extern
uint32_t c_trace_file_write_buffer (uint32_t n);

// ================================================================
// c_trace_file_close()
// Close the trace file.

extern
uint32_t c_trace_file_close (uint8_t dummy);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for communication with remote debug client.

// ================================================================

#define  DMI_OP_READ           1
#define  DMI_OP_WRITE          2
#define  DMI_OP_SHUTDOWN       3
#define  DMI_OP_START_COMMAND  4

#define  DMI_STATUS_ERR      0
#define  DMI_STATUS_OK       1
#define  DMI_STATUS_UNAVAIL  2

extern
uint8_t  c_debug_client_connect (const uint16_t tcp_port);

extern
uint8_t c_debug_client_disconnect (uint8_t dummy);

extern
uint64_t c_debug_client_request_recv (uint8_t dummy);

extern
uint8_t c_debug_client_response_send (const uint32_t data);

// ****************************************************************
// ****************************************************************
// ****************************************************************

#ifdef __cplusplus
}
#endif
