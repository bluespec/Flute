# Flute
RISC-V CPU, simple 5-stage in-order pipeline, for low-end applications needing MMUs and some performance

Flute is one of a family of free, open-source RISC-V CPUs created by Bluespec, Inc.

- [Piccolo](https://github.com/bluespec/Piccolo): 3-stage, in-order pipeline
- [Flute](https://github.com/bluespec/Flute): 5-stage, in-order pipeline

Flute is intended for low-end to medium applications that require
64-bit operation, an MMU (Virtual Memory) and more performance than
Piccolo-class processors.

The BSV source code is parameterized for

- RV32I or RV64I
- Optional 'M' (integer multiply/divide)
- Optional 'A' (Atomic Memory Ops)
- Optional 'FD' (Single and Double Precision Floating Point (not yet available)
- Privilege U and M
- Optional Privilege S, with Sv32 Virtual Memory for RV32 and Sv39 Virtual Memory for RV64
- AXI4-Lite Fabric interfaces, with optional 32-bit or 64-bit datapaths (independent of RV32/RV64 choice)
- and several other localized options (e.g., serial shifter vs. barrel shifter for Shift instructions)

This repository contains a simple testbench with which you can run RISC-V binaries in simulation by loading standard mem hex files and executing in Bluespec's Bluesim, iVerilog simulation or Verilator simulation.  The testbench contains an AXI4-Lite interconnect fabric that connects the CPU to boot ROM model, a memory model, a timer and a UART for console I/O.

Bluespec also tests all this code regularly on Xilinx FPGAs.

----------------------------------------------------------------


__This code will be populated by June 15, 2018; we are nearing completion of some final tests__
