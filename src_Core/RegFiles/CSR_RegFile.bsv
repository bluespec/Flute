// Copyright (c) 2016-2018 Bluespec, Inc. All Rights Reserved

package CSR_RegFile;


// ================================================================
`ifdef CSR_REGFILE_MIN

// Minimal CSR RegFile: User-mode CSRs, plus just enough M-mode CSRs
// to support traps/interrupts.

import CSR_RegFile_Min :: *;
export CSR_RegFile_Min :: *;

// ================================================================
`else

// Machine-mode, Supervisor-mode and User-mode CSRs

import CSR_RegFile_MSU :: *;
export CSR_RegFile_MSU :: *;

// ================================================================
`endif

endpackage
