
// Copyright (c) 2018 Massachusetts Institute of Technology
// Portions copyright (c) 2019-2020 Bluespec, Inc.
// 
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

import Types::*;
import ProcTypes::*;
import CCTypes::*;

import SoC_Map :: *;    // Bluespec setup

// data aligned addr
typedef TSub#(AddrSz, LgDataSzBytes) DataAlignedAddrSz;
typedef Bit#(DataAlignedAddrSz) DataAlignedAddr;

function DataAlignedAddr getDataAlignedAddr(Addr a) = truncateLSB(a);

// base addr for each MMIO reg/device (aligned to Data)
/* ORIGINAL MIT SETUP
DataAlignedAddr bootRomBaseAddr   = getDataAlignedAddr(64'h00001000);
DataAlignedAddr memLoaderBaseAddr = getDataAlignedAddr(64'h01000000);
DataAlignedAddr msipBaseAddr      = getDataAlignedAddr(64'h02000000);
DataAlignedAddr mtimecmpBaseAddr  = getDataAlignedAddr(64'h02004000);
DataAlignedAddr mtimeBaseAddr     = getDataAlignedAddr(64'h0200bff8);
DataAlignedAddr mainMemBaseAddr   = getDataAlignedAddr(64'h80000000);
*/

DataAlignedAddr msipBaseAddr      = getDataAlignedAddr(soc_map_struct.near_mem_io_addr_base + 64'h_0000_0000);
DataAlignedAddr mtimecmpBaseAddr  = getDataAlignedAddr(soc_map_struct.near_mem_io_addr_base + 64'h_0000_4000);
DataAlignedAddr mtimeBaseAddr     = getDataAlignedAddr(soc_map_struct.near_mem_io_addr_base + 64'h_0000_bff8);
DataAlignedAddr mainMemBaseAddr   = getDataAlignedAddr(soc_map_struct.main_mem_addr_base);

// XXX Each msip reg is 32-bit, while mtime and each mtimecmp are 64-bit. We
// assume Data is 64-bit. We hard code this relation in all MMIO logic.

// Mem loader has 2 64-bit regs
// offset 0: start addr for init mem (write to start mem initialization)
// offset 1: busy (read to determine if initialization is done)

// upper bound addr (bound itself is invalid addr) for each MMIO reg/device
// (aligned to Data)
DataAlignedAddr mainMemBoundAddr   = (mainMemBaseAddr +
				      getDataAlignedAddr(soc_map_struct.main_mem_addr_size));
DataAlignedAddr msipBoundAddr      = msipBaseAddr +
                                     fromInteger(valueof(TDiv#(CoreNum, 2)));
DataAlignedAddr mtimecmpBoundAddr  = mtimecmpBaseAddr +
                                     fromInteger(valueof(CoreNum));

// offset within each MMIO reg/device (aligned to Data)
typedef Bit#(TLog#(TDiv#(CoreNum, 2))) MSIPDataAlignedOffset;
typedef CoreId MTimCmpDataAlignedOffset;
typedef Bit#(1) MemLoaderAlignedOffset;
