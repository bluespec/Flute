Copyright (c) 2019 Bluespec, Inc.  All Rights Reserved.

This directory is intended for DARPA SSITH users; others may safely ignore it.

This directory contains a wrapper and other resources that package up
Flute to fit into the "standard" core socket in the SSITH GFE
("Government Furnished Equipment").

>================================================================
Context:

The SSITH system is an SoC with a "socket" (placeholder) for a "Core"
module (a RISC-V CPU).  Various implementations are/will be plugged
into this socket:

 - "P1"
    - Baseline Piccolo (BSV) based core
    - Baseline Rocket (Chisel) based core
    - Variations/alternatives by various SSITH project teams

 - "P2"
    - Baseline Flute (BSV) based core
    - Baseline Rocket (Chisel) based core
    - Variations/alternatives by various SSITH project teams

 - "P3"
    - Baseline Bassoon (BSV) based core
    - Baseline BOOM (Chisel) based core
    - Variations/alternatives by various SSITH project teams

>================================================================
Whenever there are changes to the Piccolo core, rerun:

  $ make compile
      (which generates RTL and then $ cp Verilog_RTL/* xilinx_ip/hdl/)

>================================================================
