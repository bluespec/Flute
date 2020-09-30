// Copyright (c) 2018-2020 Bluespec, Inc. All Rights Reserved

package Fabric_Defs;

// ================================================================
// Defines key parameters of the AXI4/AXI4-Lite system interconnect
// fabric to which the core connects, such as address bus width, data
// bus width, etc.

// ***** WARNING! WARNING! WARNING! *****

// During system integration, these parameters should be checked to be
// identical to the system interconnect settings.  Strong
// type-checking (EXACT match on bus widths) will do this; but some
// languages/tools may silently ignore mismatched widths.

// ================================================================
// BSV lib imports

// None

// ================================================================
// Project imports

import AXI4_Types :: *;

// ================================================================
// Fabric parameters

// ----------------
// Width of fabric 'id' buses
typedef  16            Wd_Id;
typedef  Bit #(Wd_Id)  Fabric_Id;

// ----------------
// Width of fabric 'addr' buses
`ifdef FABRIC64
typedef 64   Wd_Addr;
`endif

`ifdef FABRIC32
typedef 32   Wd_Addr;
`endif

typedef  Bit #(Wd_Addr)      Fabric_Addr;
typedef  TDiv #(Wd_Addr, 8)  Bytes_per_Fabric_Addr;

Integer  bytes_per_fabric_addr = valueOf (Bytes_per_Fabric_Addr);

// ----------------
// Width of fabric 'data' buses
`ifdef FABRIC64
typedef 64   Wd_Data;
`endif

`ifdef FABRIC32
typedef 32   Wd_Data;
`endif

typedef  Bit #(Wd_Data)             Fabric_Data;
typedef  Bit #(TDiv #(Wd_Data, 8))  Fabric_Strb;

typedef  TDiv #(Wd_Data, 8)         Bytes_per_Fabric_Data;
Integer  bytes_per_fabric_data = valueOf (Bytes_per_Fabric_Data);

// ----------------
// Width of fabric 'user' datapaths
typedef  0               Wd_User;
typedef  Bit #(Wd_User)  Fabric_User;

// ----------------
// Number of zero LSBs in a fabric address aligned to the fabric data width

typedef  TLog #(Bytes_per_Fabric_Data)  ZLSBs_Aligned_Fabric_Addr;
Integer  zlsbs_aligned_fabric_addr = valueOf (ZLSBs_Aligned_Fabric_Addr);

// ================================================================
// AXI4 defaults for this project

Fabric_Id    fabric_default_id       = 0;
AXI4_Burst   fabric_default_burst    = axburst_incr;
AXI4_Lock    fabric_default_lock     = axlock_normal;
AXI4_Cache   fabric_default_arcache  = arcache_dev_nonbuf;
AXI4_Cache   fabric_default_awcache  = awcache_dev_nonbuf;
AXI4_Prot    fabric_default_prot     = { axprot_2_data, axprot_1_secure, axprot_0_unpriv };
AXI4_QoS     fabric_default_qos      = 0;
AXI4_Region  fabric_default_region   = 0;
Fabric_User  fabric_default_user     = ?;

// ================================================================

endpackage
