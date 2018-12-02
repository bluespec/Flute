Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

This standalone C program takes two command-line arguments, and ELF
filename and a Mem Hex filename.  It reads the ELF file and writes out
the Mem Hex file.

It assumes a memory that is:
  - 16 MiB or 256 MiB (see C file)
  - Each word is 32 bytes (256 bits)
  - It starts at byte address 0x_8000_0000

All of these can be changed by editing the C file.
