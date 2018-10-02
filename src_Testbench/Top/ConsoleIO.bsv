// Copyright (c) 2013-2018 Bluespec, Inc.  All Rights Reserved

package ConsoleIO;

// ================================================================
// For Bluesim and Verilog simulation,
// declarations of imported C functions (hence only in simulation)
// to read and write chars to the console.
// See C_ConsoleIO_functions.{h,c} for implementation of these functions.

// ================================================================
//    extern  uint8_t c_trygetchar (void)
// If no input available on console, returns 0,
// else returns the the character.

// The dummy arg is not used, and is present only to appease Verilog
// simulators which can be be finicky about zero-argument functions.

import "BDPI" function ActionValue #(Bit #(8)) c_trygetchar (Bit #(8) dummy);

// ================================================================
//    extern  void c_write (uint32_t bytecount, void *rsp)

import "BDPI" function Action c_putchar (Bit #(8) ch);

// ================================================================

endpackage
