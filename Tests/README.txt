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
The Makefile has a command to run regressions on ISA tests.

    $ make test

In the Makefile, you will see definitions for ARCH (such as `RV32IMU`)
and SIM (such as `verilator`), which together specify which simulator
will be run (`builds/<ARCH>_SIM/exe_HW_sim`).  You can change the ARCH
and SIM definitions in the Makefile for a different simulator, or
redefine them on the `make` command line.

It will run the the Python program 'Run_regression.py', described
below, to run the simulator on all the ISA tests relevant to the
architecture ARCH.  A per-ISA-test log is captured in the 'Logs/'
directory.

>================================================================
With the Python program './Run_regression.py' you can run a regression
on all the standard RISC-V ISA tests that are relevant to your RISC-V
simulation executable (i.e., for the RISC-V features and extensions
supported by your simulation executable).

Please do:

    $ ./Run_regression.py  --help

for usage information.

Example:

    $ ./Run_regression.py  ../RV32IMU_verilator/exe_HW_sim  ./isa  ./Logs  v1

will run the verilator simulation executable on the all RISC-V ISA
tests that match the following:

    ./isa/rv32ui-p*
    ./isa/rv32mi-p*
    ./isa/rv32um-p*

and leave a transcript of each test's simulation output in files like
    ./Logs/rv32ui-p-add.log
Each log will contain an instruction trace.

Example:

    $ ./Run_regression.py  ../RV64AIMSU_verilator/exe_HW_sim  ./isa  ./Logs  v1

will run the verilator simulation executable on the all RISC-V ISA
tests that match the following:

    ./isa/rv64ui-p*
    ./isa/rv64um-p*
    ./isa/rv64ua-p*
    ./isa/rv64mi-p*
    ./isa/rv64si-p*

    ./isa/rv64ui-v*
    ./isa/rv64um-v*
    ./isa/rv64ua-v*
    
>================================================================
