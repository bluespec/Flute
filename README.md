# Open-source RISC-V CPUs from Bluespec, Inc.

This is one of a family of free, open-source RISC-V CPUs created by Bluespec, Inc.

- [Piccolo](https://github.com/bluespec/Piccolo): 3-stage, in-order pipeline

  Piccolo is intended for low-end applications (Embedded Systems, IoT, microcontrollers, etc.).

- [Flute](https://github.com/bluespec/Flute): 5-stage, in-order pipeline

  Flute is intended for low-end to medium applications that require
  64-bit operation, an MMU (Virtual Memory) and more performance than
  Piccolo-class processors.

- [Toooba](https://github.com/bluespec/Toooba): superscalar, deep, out-of-order pipeline, using MIT's RISCY-OOO core.

The three repo structures are nearly identical, and the ways to build
and run are identical.  This README is identical--please substitute
"Piccolo" or "Flute" or "Toooba" below wherever you see `<CPU>`.


### About the source codes (in BSV and Verilog)

The BSV source code in this repository, from which the synthesizable
Verilog RTL in this repository is generated, is highly parameterized
to allow generating many possible configurations, some of which are
adequate to boot a Linux kernel.

The pre-generated synthesizable Verilog RTL source files in this
repository are for a few specific configurations:

1. RV32ACIMU:    **(DARPA SSITH users: with Piccolo this is the "P1" processor)**
    - RV32I: base RV32 integer instructions
    - 'A' extension: atomic memory ops
    - 'C' extension: compressed instructions
    - 'M' extension: integer multiply/divide instructions
    - Privilege levels M (machine) and U (user)
    - Supports external, timer, software and non-maskable interrupts
    - Passes all riscv-isa tests for RV32ACIMU
    - Boots FreeRTOS

2. RV64ACDFIMSU    **(DARPA SSITH users: with Flute this is the "P2" processor)**
    - RV64I: base RV64 integer instructions
    - 'A' extension: atomic memory ops
    - 'C' extension: compressed instructions
    - 'D' extension: double-precision floating point instructions
    - 'F' extension: single-precision floating point instructions
    - 'M' extension: integer multiply/divide instructions
    - Privilege levels M (machine), S (Supervisor) and U (user)
    - Supports Sv39 virtual memory
    - Supports external, timer, software and non-maskable interrupts
    - Passes all riscv-isa tests for RV64ACDFIMSU
    - Boots the Linux kernel

If you want to generate other Verilog variants, you'll need the free
and open-source `bsc` compiler, which you can find
[here](https://github.com/B-Lang-org).

The BSV source code supports:

- RV32I or RV64I
- Optional 'A', 'C', 'D', 'F' and 'M' extensions
- Privilege level options M, MU and MSU
- For privilege S, virtual memory schemes Sv32 (RV32) and Sv39 (RV64)

- Hardware implementation option: serial shifter (smaller hardware, slower) or barrel shifter (more HW, faster) for shift instructions
- Hardware implementation option: serial integer multiplier (smaller hardware, slower) or synthesized (more HW, faster)
- AXI4 Fabric interfaces, with optional 32-bit or 64-bit datapaths (independent of RV32/RV64 choice)
- and several other localized options

### Testbench included

This repository contains a simple testbench (a small SoC) with which
one can run RISC-V binaries in simulation by loading standard mem hex
files and executing in Bluespec's Bluesim, Verilator simulation or
iVerilog simulation.  The testbench contains an AXI4 interconnect
fabric that connects the CPU to models of a boot ROM, a memory, a
timer and a UART for console I/O.

This repository contains several sample build directories, to build
RV32ACIMU or RV64ACDFIMSU simulators, using Bluespec Bluesim
simulation, Verilator Verilog simulation, or Icarus Verilog
("iverilog") simulation.

The generated Verilog is synthesizable. Bluespec tests all this code
on Xilinx FPGAs.

#### Plans

- Ongoing continuous micro-architectural improvements for performance and hardware area.

----------------------------------------------------------------
## Source codes

This repository contains two levels of source code: Verilog and BSV.

**Verilog RTL** can be found in directories with names suffixed in
'_verilator' or '_iverilog' in the 'builds' directory:

        builds/..._<verilator or iverilog>/Verilog_RTL/

[There is no difference between Verilog in a Verilator directory
vs. the corresponding iverilog directory. ]

The Verilog RTL is _synthesizable_ (and hence acceptable to
Verilator).  It can be simulated in any Verilog simulator (we provide
Makefiles to build simulation executables for Verilator and for Icarus
Verilog (iverilog)).

The RTL represents RISC-V CPU RTL, plus a rudimentary surrounding SoC
enabling immediate simulation here, and which is rich enough to enable
booting a Linux kernel.  Users are free to use the CPU RTL in their
own Verilog system designs.  The top-level module for the CPU RTL is
`Verilog_RTL/mkCore.v`.  The top-level module for the surrounding SoC
is `Verilog_RTL/mkTop_HW_Side.v`.  The SoC has an AXI4 fabric, a
timer, a software-interrupt device, and a UART.  Additional library
RTL can be found in the directory `src_bsc_lib_RTL`.  There is a
sketch of the module hierarchy in this document:

        Doc/Microarchitecture/Microarchitecture.pdf

**Bluespec BSV** source code (which was used to generate the Verilog RTL) can be found in:

- `src_Core/`, for the CPU core, with sub-directories:
   - `ISA/`:  generic types/constants/functions for the RISC-V ISA (not CPU-implementation-specific)
   - `RegFiles/`: generic register files for the GPRs (General-Purpose Registers) and CSRs (Control and Status Registers)
   - `Core/`: the CPU Core
   - `Near_Mem_VM/`: for the MMU and first-level cache.  In the CPU,
        this is instantiated twice to provide completely separate
        channels (MMU and Cache) for instructions and data.
   - `BSV_Additional_Libs/`: generic utilities (not CPU-specific)
   - `Debug_Module/`: RISC-V Debug Module to debug the CPU from GDB or other debuggers

- `src_Testbench/`, for the surrounding testbench, with sub-directories:

   - `Top/`: The system top-level (`Top_HW_Side.bsv`), a memory model
       that loads from a memory hex file, and some imported C
       functions for polled reads from the console tty (not currently
       available for Icarus Verilog).

   - `SoC/`: An interconnect, a boot ROM, a memory controller, a timer
       and software-interrupt device, and a UART for console tty I/O.

   - `Fabrics/`: Generic AXI4 code for the SoC fabric.

The BSV source code has a rich set of parameters, mentioned above. The
provided RTL source has been generated from the BSV source
automatically using Bluespec's `bsc` compiler, with certain particular
sets of choices for the various parameters.  The generated RTL is not
parameterized.

To generate Verilog variants with other parameter choices, the user
will need the free and open-source [`bsc`
compiler](https://github.com/B-Lang-org).  See the next section for
examples of how the build is configured for different ISA features.

In fact the CPU also supports a "Tandem Verifier" that produces an
instruction-by-instruction trace that can be checked for correctness
against a RISC-V Golden Reference Model.  Please contact Bluespec,
Inc. for more information.

----------------------------------------------------------------
### Building and running from the Verilog sources, out of the box

In any of the Verilog-build directories:

            builds/<ARCH>_<CPU>_verilator/
            builds/<ARCH>_<CPU>_iverilog/

  - `$ make simulator` will create a Verilog simulation executable using Verilator or iverilog, respectively

  - `$ make test` will run the executable on the standard RISC-V ISA
        test `rv32ui-p-add` or `rv64ui-p-add`, which is one of the
        tests in the `Tests/isa/` directory.  Examining the `test:`
        target in `Makefile`, we see that it first runs the program
        `Tests/elf_to_hex/elf_to_hex` on the `rv32ui-p-add` or
        `rv64ui-p-add` ELF file to create a `Mem.hex` file, and then
        runs the simulation executable which loads this `Mem.hex` file
        into its memory.

  - Following the pattern of `$ make test`, the user can run any of
    the other tests in the `Tests/isa/` directory by pointing at the
    chosen ELF file.

  - `$ make isa_tests` will run the executable on
      all the standard RISC-V ISA tests relevant for ARCH (regression testing).
      This uses the Python script `Tests/Run_regression.py`.
      Please see the documentation at the top of that program for details.

Note: an RV32ACIMU simulator will only successfully run ELF files
compiled for RV32ACIMU, privilege U and M; running it on any other ELF
file will result in illegal instruction traps.  An RV64ACDFIMSU
simulator will successfully run ELF files compiled for RV64ACDFIMSU,
privilege U, S and M.

#### Tool dependencies:

We test our builds with the following versions of iVerilog and
Verilator.  Later versions are probably ok; we have observed some
problems with earlier versions of both tools.

        $ iverilog -v
        Icarus Verilog version 10.1 (stable) ()

        $ verilator --version
        Verilator 3.922 2018-03-17 rev verilator_3_920-32-gdf3d1a4

Note: we provide a setup for iVerilog because it is well-known and
widely used. However, it is _much_ slower than Bluesim or
Verilator. For example, on a particular x86 Ubuntu platform, running
through all ISA tests takes 53 minutes with iVerilog but hardly 1
minute with Bluesim or Verilator.

----------------------------------------------------------------
### What you can build and run if you have Bluespec's `bsc` compiler

The free and open-source `bsc` compiler is available
[here](https://github.com/B-Lang-org).

Note: even without Bluespec's `bsc` compiler, you can use the Verilog
sources in any of the `builds/<ARCH>_<CPU>_verilator/Verilog_RTL`
directories-- build and run Verilog simulations, incorporate the
Verilog CPU into your own SoC, etc.  This section describes additional
things you can do with a `bsc` compiler.

#### Building a Bluesim simulator

In any of the following directories:

        builds/<ARCH>_<CPU>_bluesim

  - `$ make compile simulator`

will compile and link a Bluesim executable.  Then, you can `make test`
or `make isa_tests` as described above to run an individual ISA test
or run regressions on the full suite of relevant ISA tests.

#### Re-generating Verilog RTL

You can regenerate the Verilog RTL in any of the
`build/<ARCH>_<CPU>_verilator/` or `build/<ARCH>_<CPU>_iverilog/`
directories.  Example:

        $ cd  builds/RV32ACIMU_<CPU>_verilator
        $ make compile

#### Creating a new architecture configuration

In the `builds/` directory, you can create a new sub-directory to
build a new configuration of interest.  For example:

        $ cd  builds
	$ Resources/mkBuild_Dir.py  ..  RV32CI  bluesim

will create a new directory: `builds\RV32CIU_<CPU>_bluesim`
populated with a `Makefile` to compile and link a bluesim simulation
for an RV32 CPU with 'I' and 'C' ISA options.  You can build and run
that simulator as usual:

        $ cd  builds/RV32CIU_<CPU>_bluesim
        $ make compile simulator test isa_tests

----------------------------------------------------------------
