// Copyright (c) 2016-2018 Bluespec, Inc.  All Rights Reserved

#pragma once

// ================================================================
// These are functions imported into BSV for terminal I/O during
// Bluesim or Verilog simulation.
// See ConsoleIO.bsv for the import declarations.
// ================================================================

#ifdef __cplusplus
extern "C" {
#endif

// ================================================================
// Returns next input character (ASCII code) from the console.
// Returns 0 if no input is available.

// The dummy arg is not used, and is present only to appease Verilog
// simulators which can be be finicky about zero-argument functions.

extern
uint8_t c_trygetchar (uint8_t  dummy);

// ================================================================
// Writes character to stdout

extern
void c_putchar (uint8_t ch);

// ================================================================

#ifdef __cplusplus
}
#endif
