Notes:

- 'Near_Mem_IO' is for memory-mapped IO near the CPU.  It includes:
    - CLINT (Core Local Interruptor):
        - MSIP: for software interrupts
	- MTIME and MTIMECMP locations: to read TIME, and to generate timer interrupts
    - Other:
        - Example: configs for caches, alternate FPUs, ...

- Near_Mem_IO_AXI4.bsv: current version, using AXI4 interfaces, and
    sitting as a slave on the core-local AXI4 interconnect.

- Near_Mem_IO.bsv: older version that used to be hooked directly into
    MMU_Cache, using it's own Req/Rsp packet data type.
    Now obsolete; not used.
