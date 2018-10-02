Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

>================================================================
The './isa' sub-directory contains pre-built ELF and objdump files
(.dump) for all the standard RISC-V ISA tests.  For example, the
files:

    ./isa/rv32ui-p-add
    ./isa/rv32ui-p-add.dump

are an ELF file and its objdump (disassembly) that tests the RISC-V
user-level integer ADD instruction for RV32.  The tests are built when
one clones the following GitHub repository:

    https://github.com/riscv/riscv-tools.git

and follows the build directions therein, resulting in all the ISA
tests being built, such as this:

    <riscv-tools build dir>/riscv64-unknown-elf/share/riscv-tests/isa/rv32ui-p-add

>================================================================
The Makefile has two commands to run regressions on ISA tests.

    $ make test32
    $ make test64

The former uses one of the RV32 simulators in the 'builds' directory
and runs it on all relevant RV32 ISA tests.  A per-ISA log is captured
in the 'Logs/' directory.

The latter uses one of the RV64 simulators in the 'builds' directory
and runs it on all relevant RV64 ISA tests.  A per-ISA log is captured
in the 'Logs/' directory.

Please edit the Makefile if you want to use a different simulator
build.

Each of these 'make' commands uses the Python program
'Run_regression.py', described below to orchestrate the work.

>================================================================
With the Python program './Run_regression.py' you can run a regression
on all the standard RISC-V ISA tests that are relevant to your RISC-V
simulation executable (i.e., for the RISC-V features and extensions
supported by your simulation executable).

Please do:

    $ ./Run_regression.py  --help

for usage information.

Example:

    $ ./Run_regression.py  ../sim_verilator/exe_HW_sim  RV32IMU  ./isa  ./Logs  v1

will run the verilator simulation executable on the all RISC-V ISA
tests that match the following:

    ./isa/rv32ui-p*

    ./isa/rv32mi-p*

    ./isa/rv32um-p*

and leave a transcript of each test's simulation output in files like
    ./Logs/rv32ui-p-add.log
Each log will contain an instruction trace.

If you regenerate any of the simulation executables with different
'bsc' flags, e.g., RV64 instead of RV32, or 'A' (atomics), or 'S'
(supervisor), you can provide those letters on the command line
architecture spec (RV64IMASU) to run the the relevant ISA tests for
those features.

>================================================================
