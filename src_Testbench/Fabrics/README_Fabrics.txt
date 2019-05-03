Copyright (c) 2017-2019 Bluespec, Inc.  All Rights Reserved

This README_Fabrics is intended for people who may be interested in
just the AXI4-Lite and/or AXI4 parts of this repo (and not the other
RISC-V stuff in which we use it).

This directory is 'src_Testbench/Fabrics' within Bluespec's
repositories for its open-source Piccolo and Flute RISC-V processors
and systems.

The source code is written in Bluespec BSV, and uses Bluespec's 'bsc'
tool to generate Verilog.  Both are available in this repository.
These are notes on both the the BSV source and the generated Verilog.

>================================================================
The source tree here is (see next section below for links to already-generated Verilog):

    .
    ├── Adapters
    │   └── AXI4_AXI4_Lite_Adapters.bsv
    ├── AXI4
    │   ├── AXI4_Deburster.bsv
    │   ├── AXI4_Fabric.bsv
    │   ├── AXI4_Types.bsv
    │   └── Unit_Test
    │       ├── Makefile
    │       └── Unit_Test_Deburster.bsv
    ├── AXI4_Lite
    │   ├── AXI4_Lite_Fabric.bsv
    │   └── AXI4_Lite_Types.bsv
    └── README_Fabrics.txt

Here is a quick tour of the BSV sources:

>----------------
AXI4_Lite/

    General notes:
    - All these have a 'user' field which is not standard in AXI4-Lite
        (same as 'user' in AXI4), which can be left unused, and/or set
        to width 0.

    AXI4_Lite_Types.bsv

        Definitions for AXI4_Lite bus types, master and slave
        interfaces, connections between masters and slaves, dummy
        master and slave tie-offs, and transactors to provide
        higher-level FIFO-like interfaces to drive masters and slaves.

        Everything is parameterizd on width of address, data and user buses.

        Note: some aspects of these definitions may seem a bit verbose
        and messy; that is not typical of BSV code, but is true here
        because it is meant to interface to hand-written Verilog, so
        we need to provide precise control on interface signal names
        and protocols that are required by the Verilog side.  Pure BSV
        code can be an order of magnitude more compact.

        Everything is parameterized on wd_addr, wd_data, wd_user.

    AXI4_Lite_Fabric.bsv

       Definition for interface and module for an MxS crossbar switch
       with AXI4-Lite interfaces (M masters, S slaves).

       This is also an example of how, within BSV code, we don't worry
       about the details of AXI4-Lite signalling. We just instantiate
       the transactors defined in AXI4_Lite_Types.bsv, and then work
       only with simple, FIFO-like interfaces.

       Everything is parameterized on num_masters, num_slaves, wd_addr, wd_data, wd_user.

>----------------
AXI4/

    General notes:
    - Burst-support is partial (this is still a work in progress).
    - FIXED and INCR only, no WRAP.  Even for INCR, not yet handling
        the general case of non-fabric-data-width-aligned addresses.


    AXI4_Types.bsv

        Definitions for AXI4 bus types, master and slave
        interfaces, connections between masters and slaves, dummy
        master and slave tie-offs, and transactors to provide
        higher-level FIFO-like interfaces to drive masters and slaves.

        Everything is parameterized on wd_id, wd_addr, wd_data, wd_user.

        Note: some aspects of these definitions may seem a bit verbose
        and messy; that is not typical of BSV code, but is true here
        because it is meant to interface to hand-written Verilog, so
        we need to provide precise control on interface signal names
        and protocols that are required by the Verilog side.  Pure BSV
        code can be an order of magnitude more compact.

    AXI4_Fabric.bsv

       Definition for interface and module for an MxS crossbar switch
       with AXI4-Lite interfaces (M masters, S slaves).

       This is also an example of how, within BSV code, we don't worry
       about the details of AXI4-Lite signalling. We just instantiate
       the transactors defined in AXI4_Lite_Types.bsv, and then work
       only with simple, FIFO-like interfaces.

       Everything is parameterized on num_masters, num_slaves, wd_id, wd_addr, wd_data, wd_user.

    AXI4_Deburster.bsv

        A module that converts a slave that does not support AXI4
        bursts into a slave that does support bursts.

    Unit_Test/Unit_Test_Deburster.bsv

        A small unit test-bench consisting of a master to drive
        hand-choreographed traffic and a slave that responds with some
        function of each request.  Can be used to drive different
        kinds of transactions through the Deburster.  $displays or
        waveforms are used to observe the traffic and check that it's
        doing the right thing.

>----------------
Adapters

    AXI4_AXI4_Lite_Adapters.bsv

        Currently has only one adapter, to convert an AXI4_Lite Master
        into an AXI4 Master.  Only handles the differences in buses.
        Does not yet handle out-of-order responses coming back to the
        master (i.e., only suitable in situations where responses are
        in order.)

>================================================================
Pre-generated Verilogs can be found here:

    builds/RV64ACDFIMSU_Flute_verilator/Verilog_RTL/

    (or in any sibling directory builds/RV*_{verilator,iverilog}/Verilog_RTL/
     but we recommend the above directory as it usually has the latest code)


The Verilog files:

    mkFabric_2x3.v
    mkFabric_AXI4.v
        Two instances of an AXI4 crossbar switch (see AXI4_Fabric.bsv above)
        instantiated at: 2 masters, 3 slaves, Wd_Id=4, Wd_Addr=64, Wd_Data=64, Wd_User=0

        (These happen to be almost identical, but there are other
         configurations of our RISC-V setup where they each have
         different instantiation parameters.)

    mkAXI4_Deburster_A.v
        An instance of the deburster (see AXI4_Deburster.bsv)
        instantiated at: Wd_Id=4, Wd_Addr=64, Wd_Data=64, Wd_User=0

>================================================================
