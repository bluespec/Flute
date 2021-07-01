# Piccolo-TCM-AHB Integration #


## BSCore IO pins and connections ##

```
  input         CLK                         // free running system clock
  input         RST_N                       // optional push-button reset
  input  [31:0] cpu_external_interrupt_req  // external interrupts

  // AHB master port
  output [31:0] master1_HADDR
  output  [2:0] master1_HBURST
  output        master1_HMASTLOCK           // NC
  output  [3:0] master1_HPROT               // NC
  output  [2:0] master1_HSIZE
  output  [1:0] master1_HTRANS
  output [31:0] master1_HWDATA
  output        master1_HWRITE
  input  [31:0] master1_HRDATA
  input         master1_HREADY
  input         master1_HRESP

  // JTAG connections present only when Debug Module is instantiated
  input         dmi_reset                   // Debug Module reset; connect to jtag_reset
  input         jtag_tdi                    // standard JTAG signal
  input         jtag_tms                    // standard JTAG signal
  input         jtag_tclk                   // standard JTAG signal
  output        jtag_tdo                    // standard JTAG signal
  output        CLK_jtag_tclk_out           // NC (pin present due to BSV idiosyncrasy)
  output        CLK_GATE_jtag_tclk_out      // NC (pin present due to BSV idiosyncrasy)
  output        RST_N_ndm_reset             // optional "non debug module reset" to SoC
```

## Memory Map ##
The default memory map for Piccolo only specifies the PLIC, CLINT and TCMs:

```
Device         Base address     Size (bytes)
-------        ------------     ----------------
PLIC           0C00_0000        0040_0000 (4M)
CLINT          1000_0000        0010_0000 (64K)
ITCM           C000_0000        0002_0000 (128K)
DTCM           C800_0000        0002_0000 (128K)
```

Sizes are upper limits, and the whole allocated space is not used. For example,
the PLIC supports only 32 interrupts and uses only a fraction of the allocated range.
This is similar for other peripherals, like a UART.

Addresses that don’t map to the PLIC or CLINT are routed to
the NG AHB fabric through the Piccolo AHB master.

Unless the PLIC, CLINT or TCMs are not moved, there is no need to
modify the default SoC_Map, even if devices on the AHB fabric are added or moved.

ITCM and DTCM are shown starting at 0xC0000000 and 0xC8000000 (128MB separation) respectively.

Modifications to the Piccolo memory map are made to the BSV source file SoC_Map.bsv.


## Reset for FPGA Based RISC-V Systems ##

### Reset Domains ###
The RISC-V Debug Specification discusses three reset domains:
1. The non-debug module reset, referred to as a "global reset signal",
2. The Debug Module, and
3. The Debug Transport Module.
These domains are all mutually exclusive. Domain (1) is defined as "every
component in the platform" except for those in (2) and (3).
+The Debug Specification states that the Debug Module "should only be reset at
power-up and while dmactive in dmcontrol is 0". It is implied that the Debug
Transport Module is reset by some other transport specific means. As JTAG is
the only transport that is specified by the Debug Specification, it is logical that
the JTAG Debug Transport Module would be reset by the JTAG reset signal.

### Hart Reset ###
The Debug Specification discusses a hart reset. This is what is referred to in the
Privileged Architecture Specification by the unqualified "reset". This is a higher
level reset that is communicated from the Debug Module to the hart via some
implementation defined means. It is not a reset signal in the same sense of logic
reset, and thus is not discussed further here.

### Reset Sources ###
A RISC-V system in a Xilinx FPGA board has the following sources of reset
1. Power on reset,
2. End of bitstream programming,
3. Push-button (CPU_RESET), and
4. ndmreset from the Debug Module.
Since the FPGA logic is only available after the bitstream is programmed, and
not at board power up, the first two sources are effectively the same as far as
user logic is concerned.

### Reset Methodology ###
To ensure that reset is deasserted without timing violations, the reset signal needs
to be synchronized with the variety of clock domains in the system. An effective
way to do that is with the Processor System Reset Module1
from Xilinx. While
this module is specifically designed for a different processor system architecture,
it still functions effectively for the purposes here.
It is important that clock generators have stabilized prior to releasing the system
from reset, particularly when starting from power on. The Processor System
Reset Module provides an input that, connected to an output from the clock
generator module, accomplishes this.

### Global System Reset ###
The global system reset is asserted when either of the following is asserted:
• The push-button, or
• The ndmreset signal from the Debug module.
When the global system reset is asserted, every logic block in the design is reset
with the exception of the following:
• The JTAG interface,
• The Debug Module and Debug Transport Module, and
• The system clock generator, which may be part of the memory controller.
The Processor System Reset Module combines the multiple sources of reset,
synchronizes the output reset signal to the system clock, and generates the global
system reset signal in both polarities. Note that the Processor System Reset
Module provides for delayed output signals on some outputs. This is unnecessary
for a RISC-V design, but is not harmful either.
The Processor System Reset Module asserts the global system reset immediately
after bitstream programming. An input reset assertion is not required in this
situation.

### Clock Generator Reset ###
The clock generator must be reset by an asynchronous source that does not
create a circular dependency on the generator itself. Practically, this means that
the clock generator (possibly in a memory controller) is reset by the external
push-button reset directly, and not through the Processor System Reset Module.
This also means that ndmreset does not reset the clock generator.
The clock generator module resets immediately after bitstream programming.
An input reset assertion is not required in this situation.
https://www.xilinx.com/products/intellectual-property/proc_sys_reset.html

**Note.**
It may be desirable to have the clock generator be reset by ndmreset, particularly
when the clock generator is part of a memory controller. In order to do so, the
design must ensure that the clock generator is clocking while reset is asserted,
i.e., it cannot stop clocking while reset is asserted and begin clocking only when
reset is deasserted.

### Debug Module Reset ###
The JTAG Debug Transport Module is reset by the JTAG reset signal.
The Debug Module is reset by the dmactive signal from the Debug Transport
Module.
The Debug Module and the Debug Transport Module require no other sources
of reset.

### Debugger Control ###
OpenOCD may trigger a RISC-V system reset with the reset command. The
analogous command from gdb is monitor reset. Due to what appears to be
a bug in OpenOCD, this command does not effectively toggle ndmreset as
expected.
The following gdb commands may be used as a workaround to trigger an
ndmreset:
monitor riscv dmi_write 0x10 0x80000003
monitor riscv dmi_write 0x10 0x80000001
This issue should be debugged further to determine the root cause. If it is
confirmed the issue is related to OpenOCD, it should be fixed and the fix
propagated upstream.
