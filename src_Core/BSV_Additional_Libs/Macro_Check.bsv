package Macro_Check;

// We implement a module for statically checking that the macros
// defined while compiling Flute satisfy their rules. Please add
// any new rules to mkMacroCheck.
//
// Usage: In a module simply instantiate mkMacroCheck
//
module mkMacroCheck (Empty);

`ifdef ISA_D
`ifndef ISA_F
   errorM ("ISA_D defined, but not ISA_F. Abort.");
`endif
`endif

`ifdef NM32
`ifdef RV64
   errorM ("NM32 does not support a RV64 system. Abort.");
`endif
`endif

`ifdef NM32
`ifdef ISA_D
   errorM ("ISA_D cannot be defined with NM32. Abort.");
`endif
`endif

`ifdef FABRIC_AXI4
`ifdef FABRIC_AHBL
`ifndef DUAL_FABRIC
   errorM ("DUAL_FABRIC must be defined if both FABRIC_AXI4 and FABRIC_AHBL are defined. Abort.");
`endif
`endif
`endif

endmodule
endpackage
