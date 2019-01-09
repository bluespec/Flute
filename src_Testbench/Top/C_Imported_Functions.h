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

#define  DMI_OP_READ           1
#define  DMI_OP_WRITE          2
#define  DMI_OP_SHUTDOWN       3
#define  DMI_OP_START_COMMAND  4

#define  DMI_STATUS_ERR      0
#define  DMI_STATUS_OK       1
#define  DMI_STATUS_UNAVAIL  2

// ================================================================
// Open a TCP socket as a server listening on the specified port.
// Return fail/ok.

extern
uint8_t  c_tcp_server_open (const uint16_t tcp_port);

// ================================================================
// Close the specified server socket.
// Return fail/ok.

extern
uint8_t  c_tcp_server_close (uint8_t dummy);

// ================================================================
// Send 4-byte response 'data' to remote client.
// Returns fail/ok status

extern
uint8_t c_tx_data (const uint32_t data);

// ================================================================
// Receive 7-byte request from remote client
// Result is:    { status, data_b3, data_b2, data_b1, data_b0, addr_b1, addr_b0, op }

extern
uint64_t c_rx_data (uint8_t dummy);

// ****************************************************************
// ****************************************************************
// ****************************************************************

#ifdef __cplusplus
}
#endif
