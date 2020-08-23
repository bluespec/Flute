// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.

// Near_Mem_IFC encapsulates the MMU and L1 cache.
// It is 'near' the CPU (1-cycle access in common case).

// On the CPU side it directly services instruction fetches and DMem
// reads and writes.

// On the Fabric side it has two Master sub-interfaces.
// One master sub-interface is used for instruction-memory access.
// The other master sub-interface is used for data-memory and I/O access.

// It can have various implementations:
//  - As an almost empty pass-through to the fabric
//  - As a cache (unified or separate I- and D-)
//        Fabric-side Server interface is not used (no back door to caches)
//  - As a TCM (Tightly-Coupled Memory)
//        Fabric-side IMem Client is not used (all fabric traffic is data or I/O mem)

package Near_Mem_IFC;

// ================================================================
// BSV lib imports

import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import Cur_Cycle :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;

import MMU_Cache_Common :: *;
import AXI4_Types       :: *;
import Fabric_Defs      :: *;

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

// ================================================================
// Near-Mem parameters (statically defined)

// Id of requestor for 'coherent DMA' port into (optional) L2 cache

typedef 16   Wd_Id_Dma;
typedef 64   Wd_Addr_Dma;
typedef 512  Wd_Data_Dma;
typedef 0    Wd_User_Dma;

// ================================================================
// This part of the interface is lifted out to surrounding modules.

`ifdef MEM_512b

typedef 16   Wd_Id_Mem;
typedef 64   Wd_Addr_Mem;
typedef 512  Wd_Data_Mem;
typedef 0    Wd_User_Mem;

`else

typedef Wd_Id    Wd_Id_Mem;
typedef Wd_Addr  Wd_Addr_Mem;
typedef Wd_Data  Wd_Data_Mem;
typedef Wd_User  Wd_User_Mem;

`endif

typedef AXI4_Master_IFC #(Wd_Id_Mem,
			  Wd_Addr_Mem,
			  Wd_Data_Mem,
			  Wd_User_Mem)  Near_Mem_Fabric_IFC;

// ================================================================

interface Near_Mem_IFC;
   // Reset
   interface Server #(Token, Token) server_reset;

   // ----------------
   // IMem

   // CPU side
   interface IMem_IFC  imem;

   // Fabric side
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) imem_master;

   // ----------------
   // DMem

   // CPU side
   interface DMem_IFC  dmem;

   // Fabric side
   interface Near_Mem_Fabric_IFC  mem_master;

   // ----------------------------------------------------------------
   // Optional AXI4-Lite DMem slave interface

`ifdef INCLUDE_DMEM_SLAVE
   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) dmem_slave;
`endif

   // ----------------
   // Fences

   interface Server #(Token, Token) server_fence_i;

   interface Server #(Fence_Ordering, Token) server_fence;

`ifdef ISA_PRIV_S
   interface Server #(Token, Token) sfence_vma_server;
`endif

   // ----------------------------------------------------------------
   // Interface to 'coherent DMA' port of optional L2 cache

   interface AXI4_Slave_IFC #(Wd_Id_Dma, Wd_Addr_Dma, Wd_Data_Dma, Wd_User_Dma)  dma_server;

   // ----------------------------------------------------------------
   // Misc. control and status

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr

`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool watch_tohost, Bit #(64) tohost_addr);
   method Bit #(64) mv_tohost_value;
`endif

   // Inform core that DDR4 has been initialized and is ready to accept requests
   method Action ma_ddr4_ready;

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;

endinterface
   
// ================================================================
// Cache flush specs

Bit #(1) flush_to_invalid = 0;
Bit #(1) flush_to_clean   = 1;

// ================================================================
// IMem interface

interface IMem_IFC;
   // CPU side: IMem request
   (* always_ready *)
   method Action  req (Bit #(3) f3,
		       WordXL addr,
		       // The following  args for VM
		       Priv_Mode  priv,
		       Bit #(1)   sstatus_SUM,
		       Bit #(1)   mstatus_MXR,
		       WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }

   // CPU side: IMem response
   (* always_ready *)  method Bool     valid;
   (* always_ready *)  method Bool     is_i32_not_i16;
   (* always_ready *)  method WordXL   pc;
   (* always_ready *)  method Instr    instr;
   (* always_ready *)  method Bool     exc;
   (* always_ready *)  method Exc_Code exc_code;
   (* always_ready *)  method WordXL   tval;        // can be different from PC
endinterface

// ================================================================
// DMem interface

interface DMem_IFC;
   // CPU side: DMem request
   (* always_ready *)
   method Action  req (CacheOp op,
		       Bit #(3) f3,
`ifdef ISA_A
		       Bit #(7) amo_funct7,
`endif
		       WordXL addr,
		       Bit #(64) store_value,
		       // The following  args for VM
		       Priv_Mode  priv,
		       Bit #(1)   sstatus_SUM,
		       Bit #(1)   mstatus_MXR,
		       WordXL     satp);    // { VM_Mode, ASID, PPN_for_page_table }

   // CPU side: DMem response
   (* always_ready *)  method Bool       valid;
   (* always_ready *)  method Bit #(64)  word64;      // Load-value
   (* always_ready *)  method Bit #(64)  st_amo_val;  // Final store-value for ST, SC, AMO
   (* always_ready *)  method Bool       exc;
   (* always_ready *)  method Exc_Code   exc_code;
endinterface

// ================================================================
// Extract bytes from raw word read from near-mem.
// The bytes of interest are offset according to LSBs of addr.
// Arguments:
//  - a RISC-V LD/ST f3 value (encoding LB, LH, LW, LD, LBU, LHU, LWU)
//  - a byte-address
//  - a load-word (loaded from cache/mem)
// result:
//  - word with correct byte(s) shifted into LSBs and properly extended

function Bit #(64) fn_extract_and_extend_bytes (Bit #(3) f3, WordXL byte_addr, Bit #(64) word64);
   Bit #(64) result    = 0;
   Bit #(3)  addr_lsbs = byte_addr [2:0];

   case (f3)
      f3_LB: case (addr_lsbs)
		'h0: result = signExtend (word64 [ 7: 0]);
		'h1: result = signExtend (word64 [15: 8]);
		'h2: result = signExtend (word64 [23:16]);
		'h3: result = signExtend (word64 [31:24]);
		'h4: result = signExtend (word64 [39:32]);
		'h5: result = signExtend (word64 [47:40]);
		'h6: result = signExtend (word64 [55:48]);
		'h7: result = signExtend (word64 [63:56]);
	     endcase
      f3_LBU: case (addr_lsbs)
		'h0: result = zeroExtend (word64 [ 7: 0]);
		'h1: result = zeroExtend (word64 [15: 8]);
		'h2: result = zeroExtend (word64 [23:16]);
		'h3: result = zeroExtend (word64 [31:24]);
		'h4: result = zeroExtend (word64 [39:32]);
		'h5: result = zeroExtend (word64 [47:40]);
		'h6: result = zeroExtend (word64 [55:48]);
		'h7: result = zeroExtend (word64 [63:56]);
	     endcase

      f3_LH: case (addr_lsbs)
		'h0: result = signExtend (word64 [15: 0]);
		'h2: result = signExtend (word64 [31:16]);
		'h4: result = signExtend (word64 [47:32]);
		'h6: result = signExtend (word64 [63:48]);
	     endcase
      f3_LHU: case (addr_lsbs)
		'h0: result = zeroExtend (word64 [15: 0]);
		'h2: result = zeroExtend (word64 [31:16]);
		'h4: result = zeroExtend (word64 [47:32]);
		'h6: result = zeroExtend (word64 [63:48]);
	     endcase

      f3_LW: case (addr_lsbs)
		'h0: result = signExtend (word64 [31: 0]);
		'h4: result = signExtend (word64 [63:32]);
	     endcase
      f3_LWU: case (addr_lsbs)
		'h0: result = zeroExtend (word64 [31: 0]);
		'h4: result = zeroExtend (word64 [63:32]);
	     endcase

      f3_LD: case (addr_lsbs)
		'h0: result = word64;
	     endcase
   endcase
   return result;
endfunction

// ================================================================
// Extract bytes from word read from fabric.
// The bytes of interest are already in the LSBs of 'word',
// they just have to be suitably extended.
// Arguments:
//  - a RISC-V LD/ST f3 value (encoding LB, LH, LW, LD, LBU, LHU, LWU)
//  - a byte-address
//  - a load-word (loaded from fabric)
// result:
//  - word with correct byte(s), properly extended.

function Bit #(64) fn_extend_bytes (Bit #(3) f3, Bit #(64) word64);
   Bit #(64) result = 0;
   case (f3)
      f3_LB:  result = signExtend (word64 [ 7: 0]);
      f3_LBU: result = zeroExtend (word64 [ 7: 0]);

      f3_LH:  result = signExtend (word64 [15: 0]);
      f3_LHU: result = zeroExtend (word64 [15: 0]);

      f3_LW:  result = signExtend (word64 [31: 0]);
      f3_LWU: result = zeroExtend (word64 [31: 0]);

      f3_LD:  result = word64;
   endcase

   return result;
endfunction

// ================================================================

endpackage: Near_Mem_IFC
