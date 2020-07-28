// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

package Cache_Decls;

// ================================================================
// This is just a convenience package, encapsulating the selection of
// a specific set of cache declarations corresponding to a particular
// cache config.

// ================================================================

`ifdef RV32
`ifdef SV32
import Cache_Decls_RV32_Sv32_8KB_2way :: *;
export Cache_Decls_RV32_Sv32_8KB_2way :: *;
`else
import Cache_Decls_RV32_8KB_2way :: *;
export Cache_Decls_RV32_8KB_2way :: *;
`endif
`endif

`ifdef RV64
`ifdef SV39
import Cache_Decls_RV64_Sv39_8KB_2way :: *;
export Cache_Decls_RV64_Sv39_8KB_2way :: *;
`else
import Cache_Decls_RV64_8KB_2way :: *;
export Cache_Decls_RV64_8KB_2way :: *;
`endif
`endif

endpackage
