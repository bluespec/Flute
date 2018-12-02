// Hand-written System Verilog import statements for imported C functions

// ================================================================
// import "BDPI" function ActionValue #(Bit #(8)) c_trygetchar (Bit #(8) dummy);

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

// ================================================================
