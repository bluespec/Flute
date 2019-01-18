'Debug_Module' implements a Debug Module for RISC-V processors in
accordance with the RISC-V standard "External Debug Support" spec:

    RISC-V External Debug Support
    Version 0.13-DRAFT
    dd8d8714184970031fa447a452068223f257b51c
    Mon Dec 18 13:54:14 2017 -0800

Note: the spec is independent of any particular RISC-V CPU
implementation.  It just specifies the standard registers in the Debug
Module that can be read and written by an external debugger (such as
GDB).  It specifies the address map of these registers, and the
semantics, i.e., what happens when one reads or writes these
registers.  The spec does not say anything about how this spec is
implemented.

Please see comments in Debug_Module.bsv for more details on our
implementation of the Debug Module spec.  This implementation is also
not specific to any particular CPU implementation.  We use it in
Bluespec RISC-V CPUs, but it could be used with other CPUs as well.
We use this Debug Module in the Bluespec RISC-V Verification Factory
(BRVF).

// ================================================================
What follows is a concise cheat-sheet on the registers in the Debug
Module based on the spec.

// ----------------
// Run Control

DM_Addr dm_addr_dmcontrol    = 'h10;
    31.30.29.28| 27.26.25.24| 23.22.21.20| 19.18.17.16| 15.14.13.12| 11.10.9.8| 7.6.5.4| 3.2.1.0|
     |  |  |         |  |------------10--------------|                                       | |dmactive
     |  |  |         |                               |hartsel                                |ndmreset
     |  |  |         |hasel
     |  |  |           0: Single hart selected (hartsel)
     |  |  |           1: Multiple harts selected (hartsel + hart array mask)
     |  |  |hartreset
     |  |resumereq
     |haltreq

DM_Addr dm_addr_dmstatus     = 'h11;
    31.30.29.28| 27.26.25.24| 23.22.21.20| 19.18.17.16| 15.14.13.12| 11.10.9.8| 7.6.5.4| 3.2.1.0|
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | | | |  |--4--|version
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | | | |  |       0: no DM present
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | | | |  |       1: DM v011
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | | | |  |       2: DM v013
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | | | |  |       15: DM vUnknown
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | | | |devtreevalid
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | | |0
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  | |authbusy
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |  |authenticated
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | | |anyhalted
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  | |allhalted
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |  |anyrunning
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |   |allrunning
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |  |anyunavail
     |            |  |     |   |  |  |  |   |  |  |  |   |  |  |allunavail
     |            |  |     |   |  |  |  |   |  |  |  |   |  |anynonexistent
     |            |  |     |   |  |  |  |   |  |  |  |   |allnonexistent
     |            |  |     |   |  |  |  |   |  |  |  |anyresumeack
     |            |  |     |   |  |  |  |   |  |  |allresumeack
     |            |  |     |   |  |  |  |   |  |anyhavereset
     |            |  |     |   |  |  |  |   |allhavereset
     |            |  |     |   |  |  |--|0
     |            |  |     |   |  |impebreak
     |            |  |     |   |    0 No implicit EBREAK at end of PB
     |            |  |     |   |    1 Implicit    EBREAK at end of PB
     |            |  |     |   |0
     |            |  |--3--|dmerr
     |            |          0 No error
     |            |          1 bad addr
     |            |          7 other error
     |----- 5-----|0

DM_Addr dm_addr_hartinfo     = 'h12;
DM_Addr dm_addr_haltsum      = 'h13;
DM_Addr dm_addr_hawindowsel  = 'h14;
DM_Addr dm_addr_hawindow     = 'h15;
DM_Addr dm_addr_devtreeaddr0 = 'h19;
DM_Addr dm_addr_authdata     = 'h30;
DM_Addr dm_addr_haltregion0  = 'h40;
DM_Addr dm_addr_haltregion31 = 'h5F;

// ----------------
// Abstract commands (read/write RISC-V registers and RISC-V CSRs)

DM_Addr dm_addr_abstractcs   = 'h16;
    31.30.29.28| 27.26.25.24| 23.22.21.20| 19.18.17.16| 15.14.13.12| 11.10.9.8| 7.6.5.4| 3.2.1.0|
              |-----5------|progsize                              |busy  |-3-|cmderr  |----5---|datacount

DM_Addr dm_addr_command      = 'h17;
    31.30.29.28| 27.26.25.24| 23.22.21.20| 19.18.17.16| 15.14.13.12| 11.10.9.8| 7.6.5.4| 3.2.1.0|
                           |            |      |  |  |   |-----------------16------------------|regno
                           |            |      |  |  |   |    0x0000-0x0FFF    CSRs (dpc => PC)
                           |            |      |  |  |   |    0x1000-0x101F    GPRs
                           |            |      |  |  |   |    0x1020-0x103F    Floating Point Regs
                           |            |      |  |  |   |    0xC000-0xFFFF    Reserved
                           |            |      |  |  |write
                           |            |      |  |    0: specified reg -> arg0 of data
                           |            |      |  |    1: specified reg <- arg0 of data
                           |            |      |  |transfer
                           |            |      |    0 Don't do the 'write' op
                           |            |      |    1 Do       the 'write' op
                           |            |      |    Allows exec of PB without valid vals in 'size' and 'regno'
                           |            |      |postexec
                           |            |        1 exec Program Buffer exactly once after the xfer
                           |      |--3--|size
                           |              2 Lowest  32b of reg
                           |              3 Lowest  64b of reg
                           |              4 Lowest 128b of reg
     |---------8-----------|cmdtype
                             0 ACCESS_REG
                             1 QUICK_ACCESS

DM_Addr dm_addr_data0        = 'h04;
DM_Addr dm_addr_data1        = 'h05;
DM_Addr dm_addr_data2        = 'h06;
DM_Addr dm_addr_data3        = 'h07;
DM_Addr dm_addr_data4        = 'h08;
DM_Addr dm_addr_data5        = 'h09;
DM_Addr dm_addr_data6        = 'h0a;
DM_Addr dm_addr_data7        = 'h0b;
DM_Addr dm_addr_data8        = 'h0c;
DM_Addr dm_addr_data9        = 'h0d;
DM_Addr dm_addr_data10       = 'h0d;
DM_Addr dm_addr_data11       = 'h0f;

DM_Addr dm_addr_abstractauto = 'h18;
DM_Addr dm_addr_progbuf0     = 'h20;

// ----------------
// System Bus access (read/write RISC-V memory/devices)

DM_Addr dm_addr_sbcs         = 'h38;
    31.30.29.28| 27.26.25.24| 23.22.21.20| 19.18.17.16| 15.14.13.12| 11.10.9.8| 7.6.5.4| 3.2.1.0|
                                        |         |  |   |  |     |                 | |  | | | |sbaccess8
                                        |         |  |   |  |     |                 | |  | | |sbaccess16
                                        |         |  |   |  |     |                 | |  | |sbaccess32
                                        |         |  |   |  |     |                 | |  |sbaccess64
                                        |         |  |   |  |     |                 | |sbaccess128
                                        |         |  |   |  |     |   |-----7-------|sbasize
                                        |         |  |   |  |--3--|sberror
                                        |         |  |   |  |       0: no bus err
                                        |         |  |   |  |       1: timeout
                                        |         |  |   |  |       2: bad addr
                                        |         |  |   |  |       3: other err (e.g., alignment)
                                        |         |  |   |  |       4: busy
                                        |         |  |   |sbautoread
                                        |         |  |sbautoincrement
                                        |   |--3--|sbaccess
                                        |   |       0:8b, 1:16b, 2:32b, 3:64b, 4:128b
                                        |singleread
                                         1 triggers single read at sbaddress of size sbaccess

DM_Addr dm_addr_sbaddress0   = 'h39;
DM_Addr dm_addr_sbaddress1   = 'h3a;
DM_Addr dm_addr_sbaddress2   = 'h3b;
DM_Addr dm_addr_sbdata0      = 'h3c;
DM_Addr dm_addr_sbdata1      = 'h3d;
DM_Addr dm_addr_sbdata2      = 'h3e;
DM_Addr dm_addr_sbdata3      = 'h3f;
