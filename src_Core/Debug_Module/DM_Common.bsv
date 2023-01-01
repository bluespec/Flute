// Copyright (c) 2017-2023 Bluespec, Inc. All Rights Reserved.

package DM_Common;

// ================================================================
// This package has non-implementation-specific definitions, i.e.,
// just encodes things in the spec.

// ================================================================
// BSV library imports

// None

// BSV Additional Libs

import Semi_FIFOF :: *;

// ================================================================
// Project imports

// None

// ================================================================
// Debug Module Interface (DMI) addresses and data.
// Note: data is always 32b, whether the connected CPU is RV32, RV64 or RV128.

typedef Bit #(7)  DM_Addr;

DM_Addr max_DM_Addr = 'h5F;

typedef Bit #(32) DM_Word;

// ================================================================
// Debug Module address map

// ----------------
// Run Control

DM_Addr dm_addr_dmcontrol    = 'h10;
DM_Addr dm_addr_dmstatus     = 'h11;
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
DM_Addr dm_addr_command      = 'h17;

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
DM_Addr dm_addr_sbaddress0   = 'h39;
DM_Addr dm_addr_sbaddress1   = 'h3a;
DM_Addr dm_addr_sbaddress2   = 'h3b;
DM_Addr dm_addr_sbdata0      = 'h3c;
DM_Addr dm_addr_sbdata1      = 'h3d;
DM_Addr dm_addr_sbdata2      = 'h3e;
DM_Addr dm_addr_sbdata3      = 'h3f;

// ================================================================

function Fmt fshow_dm_addr (DM_Addr dm_addr);
   return case (dm_addr)
	     // Run Control
	     dm_addr_dmcontrol:    $format ("dm_addr_dmcontrol");
	     dm_addr_dmstatus:     $format ("dm_addr_dmstatus");
	     dm_addr_hartinfo:     $format ("dm_addr_hartinfo");
	     dm_addr_haltsum:      $format ("dm_addr_haltsum");
	     dm_addr_hawindowsel:  $format ("dm_addr_hawindowsel");
	     dm_addr_hawindow:     $format ("dm_addr_hawindow");
	     dm_addr_devtreeaddr0: $format ("dm_addr_devtreeaddr0");
	     dm_addr_authdata:     $format ("dm_addr_authdata");
	     dm_addr_haltregion0:  $format ("dm_addr_haltregion0");
	     dm_addr_haltregion31: $format ("dm_addr_haltregion31");

	     // Abstract Commands
	     dm_addr_abstractcs:   $format ("dm_addr_abstractcs");
	     dm_addr_command:      $format ("dm_addr_command");
	     dm_addr_data0:        $format ("dm_addr_data0");
	     dm_addr_data1:        $format ("dm_addr_data1");
	     dm_addr_data2:        $format ("dm_addr_data2");
	     dm_addr_data3:        $format ("dm_addr_data3");
	     dm_addr_data4:        $format ("dm_addr_data4");
	     dm_addr_data5:        $format ("dm_addr_data5");
	     dm_addr_data6:        $format ("dm_addr_data6");
	     dm_addr_data7:        $format ("dm_addr_data7");
	     dm_addr_data8:        $format ("dm_addr_data8");
	     dm_addr_data9:        $format ("dm_addr_data9");
	     dm_addr_data10:       $format ("dm_addr_data10");
	     dm_addr_data11:       $format ("dm_addr_data11");
	     dm_addr_abstractauto: $format ("dm_addr_abstractauto");
	     dm_addr_progbuf0:     $format ("dm_addr_progbuf0");

	     // System Bus
	     dm_addr_sbcs:         $format ("dm_addr_sbcs");
	     dm_addr_sbaddress0:   $format ("dm_addr_sbaddress0");
	     dm_addr_sbaddress1:   $format ("dm_addr_sbaddress1");
	     dm_addr_sbaddress2:   $format ("dm_addr_sbaddress2");
	     dm_addr_sbdata0:      $format ("dm_addr_sbdata0");
	     dm_addr_sbdata1:      $format ("dm_addr_sbdata1");
	     dm_addr_sbdata2:      $format ("dm_addr_sbdata2");
	     dm_addr_sbdata3:      $format ("dm_addr_sbdata3");

	     default:              $format ("<Unknown dm_abstract_command dm_addr 0x%0h>", dm_addr);
	  endcase;
endfunction

// ================================================================
// Run Control DM register fields

// ----------------------------------------------------------------
// 'dmcontrol' register

function DM_Word fn_mk_dmcontrol (Bool       haltreq,
				  Bool       resumereq,
				  Bool       hartreset,
				  Bool       hasel,
				  Bit #(10)  hartsel,
				  Bool       ndmreset,
				  Bool       dmactive);
   return {pack (haltreq),
	   pack (resumereq),
	   pack (hartreset),
	   2'b0,
	   pack (hasel),
	   hartsel,
	   14'b0,
	   pack (ndmreset),
	   pack (dmactive)};
endfunction

function Bool fn_dmcontrol_haltreq (DM_Word dm_word);
   return unpack (dm_word [31]);
endfunction

function Bool fn_dmcontrol_resumereq (DM_Word dm_word);
   return unpack (dm_word [30]);
endfunction

function Bool fn_dmcontrol_hartreset (DM_Word dm_word);
   return unpack (dm_word [29]);
endfunction

function Bool fn_dmcontrol_hasel (DM_Word dm_word);
   return unpack (dm_word [26]);
endfunction

function Bit #(10) fn_dmcontrol_hartsel (DM_Word dm_word);
   return dm_word [25:16];
endfunction

function Bool fn_dmcontrol_ndmreset (DM_Word dm_word);
   return unpack (dm_word [1]);
endfunction

function Bool fn_dmcontrol_dmactive (DM_Word dm_word);
   return unpack (dm_word [0]);
endfunction

// ----------------------------------------------------------------
// 'dmstatus' register

function Bool fn_dmstatus_allresumeack (DM_Word x);
   return unpack (x [17]);
endfunction

function Bool fn_dmstatus_anyresumeack (DM_Word x);
   return unpack (x [16]);
endfunction

function Bool fn_dmstatus_allnonexistent (DM_Word x);
   return unpack (x [15]);
endfunction

function Bool fn_dmstatus_anynonexistent (DM_Word x);
   return unpack (x [14]);
endfunction

function Bool fn_dmstatus_allunavail (DM_Word x);
   return unpack (x [13]);
endfunction

function Bool fn_dmstatus_anyunavail (DM_Word x);
   return unpack (x [12]);
endfunction

function Bool fn_dmstatus_allrunning (DM_Word x);
   return unpack (x [11]);
endfunction

function Bool fn_dmstatus_anyrunning (DM_Word x);
   return unpack (x [10]);
endfunction

function Bool fn_dmstatus_allhalted (DM_Word x);
   return unpack (x [9]);
endfunction

function Bool fn_dmstatus_anyhalted (DM_Word x);
   return unpack (x [8]);
endfunction

function Bool fn_dmstatus_authenticated (DM_Word x);
   return unpack (x [7]);
endfunction

function Bool fn_dmstatus_authbusy (DM_Word x);
   return unpack (x [6]);
endfunction

function Bool fn_dmstatus_devtreevalid (DM_Word x);
   return unpack (x [4]);
endfunction

function Bit #(4) fn_dmstatus_version (DM_Word x);
   return unpack (x [3:0]);
endfunction

function Fmt fshow_dmstatus (DM_Word x);
   Fmt fmt_version = (  (x[3:0] == 0)
		      ? $format ("v.none")
		      : (  (x[3:0] == 1)
			 ? $format ("v0.11")
			 : (  (x[3:0] == 2)
			    ? $format ("v0.13")
			    : $format ("v??"))));

   return (  $format ("(all/any) ")
	   + $format ("resumeack %0d/%0d ",   x[17], x[16])
	   + $format ("nonexistent %0d/%0d ", x[15], x[14])
	   + $format ("unavail %0d/%0d ",     x[13], x[12])
	   + $format ("running %0d/%0d ",     x[11], x[10])
	   + $format ("halted %0d/%0d ",      x[9],  x[8])
	   + $format ("authenticated %0d ",   x[7])
	   + $format ("authbusy %0d ",        x[6])
	   + $format ("devtreevalid %0d ",    x[4])
	   + fmt_version);
endfunction

// ================================================================
// Abstract Command register fields

// ----------------------------------------------------------------
// 'dm_abstractcs' register

typedef enum {DM_ABSTRACTCS_CMDERR_NONE,             // 0
	      DM_ABSTRACTCS_CMDERR_BUSY,             // 1
	      DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED,    // 2
	      DM_ABSTRACTCS_CMDERR_EXCEPTION,        // 3
	      DM_ABSTRACTCS_CMDERR_HALT_RESUME,      // 4
	      DM_ABSTRACTCS_CMDERR_UNDEF5,           // 5
	      DM_ABSTRACTCS_CMDERR_UNDEF6,           // 6
	      DM_ABSTRACTCS_CMDERR_OTHER             // 7
   } DM_abstractcs_cmderr
deriving (Bits, Eq, FShow);

// The following is used in writes, to clear cmderr
DM_abstractcs_cmderr dm_cmderr_w1c = DM_ABSTRACTCS_CMDERR_OTHER;

function DM_Word fn_mk_abstractcs (DM_abstractcs_cmderr cmderr);
   return { 0, pack (cmderr), 8'h0 };
endfunction

function Bit #(5) fn_abstractcs_progsize (DM_Word dm_word);
   return unpack (dm_word [28:24]);
endfunction

function Bool fn_abstractcs_busy (DM_Word dm_word);
   return unpack (dm_word [12]);
endfunction

function DM_abstractcs_cmderr fn_abstractcs_cmderr (DM_Word dm_word);
   return unpack (dm_word [10:8]);
endfunction

function Bit #(5) fn_abstractcs_datacount (DM_Word dm_word);
   return unpack (dm_word [4:0]);
endfunction

// ----------------------------------------------------------------
// 'command' register

typedef enum {DM_COMMAND_CMDTYPE_ACCESS_REG,
	      DM_COMMAND_CMDTYPE_QUICK_ACCESS,
	      DM_COMMAND_CMDTYPE_ACCESS_MEM,
	      DM_COMMAND_CMDTYPE_UNKNOWN
   } DM_command_cmdtype
deriving (Bits, Eq, FShow);

typedef enum {DM_COMMAND_ACCESS_SIZE_UNDEF0,     // 0
	      DM_COMMAND_ACCESS_SIZE_UNDEF1,     // 1
	      DM_COMMAND_ACCESS_SIZE_LOWER32,    // 2
	      DM_COMMAND_ACCESS_SIZE_LOWER64,    // 3
	      DM_COMMAND_ACCESS_SIZE_LOWER128,   // 4
	      DM_COMMAND_ACCESS_SIZE_UNDEF5,     // 5
	      DM_COMMAND_ACCESS_SIZE_UNDEF6,     // 6
	      DM_COMMAND_ACCESS_SIZE_UNDEF7      // 7
   } DM_command_access_size
deriving (Bits, Eq, FShow);

Integer dm_command_access_reg_regno_csr_0   = 'h0000;
Integer dm_command_access_reg_regno_csr_FFF = 'h0FFF;
Integer dm_command_access_reg_regno_gpr_0   = 'h1000;
Integer dm_command_access_reg_regno_gpr_1F  = 'h101F;
Integer dm_command_access_reg_regno_fpr_0   = 'h1020;
Integer dm_command_access_reg_regno_fpr_1F  = 'h103F;

function DM_Word fn_mk_command_access_reg (DM_command_access_size  size,
					   Bool                    postexec,
					   Bool                    transfer,
					   Bool                    write,
					   Bit #(16)               regno);
   Bit #(8)  b8_cmdtype = zeroExtend (pack (DM_COMMAND_CMDTYPE_ACCESS_REG));
   Bit #(3)  b3_size    = pack (size);
   return {b8_cmdtype,
	   1'b0,
	   b3_size,
	   1'b0,
	   pack (postexec),
	   pack (transfer),
	   pack (write),
	   regno};
endfunction

function DM_command_cmdtype fn_command_cmdtype (DM_Word dm_word);
   DM_command_cmdtype cmdtype;
   case (dm_word [31:24])
      0:       cmdtype = DM_COMMAND_CMDTYPE_ACCESS_REG;
      1:       cmdtype = DM_COMMAND_CMDTYPE_QUICK_ACCESS;
      2:       cmdtype = DM_COMMAND_CMDTYPE_ACCESS_MEM;
      default: cmdtype = DM_COMMAND_CMDTYPE_UNKNOWN;
   endcase
   return cmdtype;
endfunction

function DM_command_access_size fn_command_access_size (DM_Word dm_word);
   return unpack (dm_word [22:20]);
endfunction

function Bool fn_command_access_reg_postexec (DM_Word dm_word);
   return unpack (dm_word [18]);
endfunction

function Bool fn_command_access_reg_transfer (DM_Word dm_word);
   return unpack (dm_word [17]);
endfunction

function Bool fn_command_access_reg_write (DM_Word dm_word);
   return unpack (dm_word [16]);
endfunction

function Bit #(16) fn_command_access_reg_regno (DM_Word dm_word);
   return dm_word [15:0];
endfunction

// ================================================================
// System Bus Access DM register fields

// ----------------------------------------------------------------
// 'dm_sbcs' register

typedef enum {DM_SBACCESS_8_BIT,
	      DM_SBACCESS_16_BIT,
	      DM_SBACCESS_32_BIT,
	      DM_SBACCESS_64_BIT,
	      DM_SBACCESS_128_BIT
   } DM_sbaccess
deriving (Bits, Eq, FShow);

function Integer fn_sbaccess_to_addr_incr (DM_sbaccess sbaccess);
   case (sbaccess)
      DM_SBACCESS_8_BIT:   return 1;
      DM_SBACCESS_16_BIT:  return 2;
      DM_SBACCESS_32_BIT:  return 4;
      DM_SBACCESS_64_BIT:  return 8;
      DM_SBACCESS_128_BIT: return 16;
   endcase
endfunction

typedef enum {DM_SBERROR_NONE,                // 0
	      DM_SBERROR_TIMEOUT,             // 1
	      DM_SBERROR_BADADDR,             // 2
	      DM_SBERROR_ALIGNMENT_ERR,       // 3
	      DM_SBERROR_UNSUPPORTED_SIZE,    // 4
	      DM_SBERROR_UNDEF5,              // 5
	      DM_SBERROR_UNDEF6,              // 6
	      DM_SBERROR_OTHER                // 7
   } DM_sberror
deriving (Bits, Eq, FShow);

// Constructor

function DM_Word fn_mk_sbcs_val (Bit #(3)     sbversion,
				 Bool         sbbusyerror,
				 Bool         sbbusy,
				 Bool         sbreadonaddr,
				 DM_sbaccess  sbaccess,
				 Bool         sbautoincrement,
				 Bool         sbreadondata,
				 DM_sberror   sberror,
				 Bit #(7)     sbasize,
				 Bit #(1)     sbaccess128,
				 Bit #(1)     sbaccess64,
				 Bit #(1)     sbaccess32,
				 Bit #(1)     sbaccess16,
				 Bit #(1)     sbaccess8);
   return {sbversion,
	   6'b0,
	   pack (sbbusyerror),
	   pack (sbbusy),
	   pack (sbreadonaddr),
	   pack (sbaccess),
	   pack (sbautoincrement),
	   pack (sbreadondata),
	   pack (sberror),
	   sbasize,
	   sbaccess128,
	   sbaccess64,
	   sbaccess32,
	   sbaccess16,
	   sbaccess8};
endfunction

// Selectors

function Bit #(3)    fn_sbcs_sbversion       (DM_Word dm_word);  return unpack (dm_word [31:29]);  endfunction
function Bool        fn_sbcs_sbbusyerror     (DM_Word dm_word);  return unpack (dm_word [22]);     endfunction
function Bool        fn_sbcs_sbbusy          (DM_Word dm_word);  return unpack (dm_word [21]);     endfunction
function Bool        fn_sbcs_sbreadonaddr    (DM_Word dm_word);  return unpack (dm_word [20]);     endfunction
function DM_sbaccess fn_sbcs_sbaccess        (DM_Word dm_word);  return unpack (dm_word [19:17]);  endfunction
function Bool        fn_sbcs_sbautoincrement (DM_Word dm_word);  return unpack (dm_word [16]);     endfunction
function Bool        fn_sbcs_sbreadondata    (DM_Word dm_word);  return unpack (dm_word [15]);     endfunction
function DM_sberror  fn_sbcs_sberror         (DM_Word dm_word);  return unpack (dm_word [14:12]);  endfunction
function Bit #(7)    fn_sbcs_sbasize         (DM_Word dm_word);  return         dm_word [11:5];    endfunction
function Bool        fn_sbcs_sbaccess128     (DM_Word dm_word);  return unpack (dm_word [4]);      endfunction
function Bool        fn_sbcs_sbaccess64      (DM_Word dm_word);  return unpack (dm_word [3]);      endfunction
function Bool        fn_sbcs_sbaccess32      (DM_Word dm_word);  return unpack (dm_word [2]);      endfunction
function Bool        fn_sbcs_sbaccess16      (DM_Word dm_word);  return unpack (dm_word [1]);      endfunction
function Bool        fn_sbcs_sbaccess8       (DM_Word dm_word);  return unpack (dm_word [0]);      endfunction

// Debugging

function Fmt fshow_sbcs (DM_Word dm_word);
   return (  $format ("SBCS{")
	   + $format ("sbversion %0d",      fn_sbcs_sbversion (dm_word))
	   + $format (" sbbusyerror %0d",   fn_sbcs_sbbusyerror (dm_word))
	   + $format (" sbbusy %0d",        fn_sbcs_sbbusy (dm_word))
	   + $format (" sbreadonaddr ")     + fshow (fn_sbcs_sbreadonaddr (dm_word))
	   + $format (" sbaccess ")         + fshow (fn_sbcs_sbaccess (dm_word))
	   + $format (" sbautoincrement ")  + fshow (fn_sbcs_sbautoincrement (dm_word))
	   + $format (" sbreadondata ")     + fshow (fn_sbcs_sbreadondata (dm_word))
	   + $format (" sberror ")          + fshow (fn_sbcs_sberror (dm_word))
	   + $format (" sbasize %0d",       fn_sbcs_sbasize (dm_word))
	   + $format (" sbaccess")
	   + ((fn_sbcs_sbaccess128 (dm_word)) ? $format ("_128") : $format ("x"))
	   + ((fn_sbcs_sbaccess64  (dm_word)) ? $format ("_64")  : $format ("x"))
	   + ((fn_sbcs_sbaccess32  (dm_word)) ? $format ("_32")  : $format ("x"))
	   + ((fn_sbcs_sbaccess16  (dm_word)) ? $format ("_16")  : $format ("x"))
	   + ((fn_sbcs_sbaccess8   (dm_word)) ? $format ("_8")   : $format ("x"))
	   + $format ("}"));
endfunction

// ================================================================
// DCSR 'cause' field values

typedef enum {DCSR_CAUSE_RESERVED0,
	      DCSR_CAUSE_EBREAK,
	      DCSR_CAUSE_TRIGGER,
	      DCSR_CAUSE_HALTREQ,
	      DCSR_CAUSE_STEP,
	      DCSR_CAUSE_RESERVED5,
	      DCSR_CAUSE_RESERVED6,
	      DCSR_CAUSE_RESERVED7
   } DCSR_Cause
deriving (Bits, Eq, FShow);

// ================================================================
// Sub-interface of the Debug Module facing the remote debugger (e.g. GDB)

interface DMI;
   method Action                 read_addr  (DM_Addr dm_addr);
   method ActionValue #(DM_Word) read_data;
   method Action                 write    (DM_Addr dm_addr, DM_Word dm_word);
endinterface

// A dummy interface to tie off DMI if it is not used.

DMI dummy_DMI_ifc = interface DMI;
		       method Action                 read_addr (DM_Addr dm_addr) = noAction;
		       method ActionValue #(DM_Word) read_data = actionvalue
								    return 0;
								 endactionvalue;
		       method Action                 write    (DM_Addr dm_addr, DM_Word dm_word) = noAction;
		    endinterface;

// ----------------------------------------------------------------
// Server_DMI is a more BSV-style (FIFO-based) DMI interface

typedef struct {
   Bool      is_read;    // True => read; False => Write
   Bit #(7)  addr;       // Debug Module register address
   Bit #(32) wdata;      // Debug Module reqister write-data (relevant only for Writes)
   } DMI_Req
deriving (Bits, FShow);

typedef struct {
   Bit #(32) rdata;   // Debug Module register read-data (relevant only for Reads)
   } DMI_Rsp
deriving (Bits, FShow);

// Interface of Debug Module facing remote debugger (e.g. GDB)

typedef Server_Semi_FIFOF #(DMI_Req, DMI_Rsp) Server_DMI;

// A dummy interface to tie off DMI if it is not used.

Server_DMI dummy_DMI_Req_Rsp_ifc = interface Server_DMI;
				      interface request  = dummy_FIFOF_I;
				      interface response = dummy_FIFOF_O;
				   endinterface;

// ================================================================

endpackage
