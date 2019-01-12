// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

// Hand-written System Verilog import statements for imported C functions

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for console I/O

// ================================================================

import "DPI-C"
function  byte unsigned  c_trygetchar (byte unsigned  dummy);

import "DPI-C"
function  int unsigned  c_putchar (byte unsigned  ch);

// ================================================================
// Symbol table
// Reads the whole symbol-table file on each call,
// which is ok if it's not called often and the file is small.

import "DPI-C"
function longint unsigned  c_get_symbol_val (string symbol);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// Functions for Tandem Verification trace file output.

// ================================================================

import "DPI-C"
function  int unsigned  c_trace_file_open (byte unsigned dummy);

import "DPI-C"
function  int unsigned  c_trace_file_load_byte_in_buffer (int unsigned  j, byte unsigned  data);

import "DPI-C"
function  int unsigned  c_trace_file_load_word64_in_buffer (int unsigned  byte_offset, longint unsigned  data);

import "DPI-C"
function  int unsigned  c_trace_file_write_buffer (int unsigned  n);

import "DPI-C"
function  int unsigned  c_trace_file_close (byte unsigned dummy);

// ****************************************************************
// ****************************************************************
// ****************************************************************

// ================================================================

import "DPI-C"
function  byte unsigned  c_debug_client_connect (shortint  tcp_port);

import "DPI-C"
function  byte unsigned  c_debug_client_disconnect (byte unsigned  dummy);

import "DPI-C"
function  longint unsigned  c_debug_client_request_recv (byte unsigned  dummy);

import "DPI-C"
function  byte unsigned  c_debug_client_response_send (int unsigned  data);

// ================================================================
