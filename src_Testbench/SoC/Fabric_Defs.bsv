// Copyright (c) 2018 Bluespec, Inc. All Rights Reserved

package Fabric_Defs;

// ================================================================
// Defines key parameters of the SoC Fabric, such as address bus
// width, data bus width, etc.

// ================================================================
// Fabric parameters

// Width of fabric 'addr' and 'data' datapaths
`ifdef FABRIC64
typedef 64   Wd_Addr;
typedef 64   Wd_Data;
`else
typedef 32   Wd_Addr;
typedef 32   Wd_Data;
`endif

// Width of fabric 'user' datapaths
typedef  0   Wd_User;


typedef  Bit #(Wd_Addr)  Fabric_Addr;
typedef  Bit #(Wd_Data)  Fabric_Data;
typedef  Bit #(Wd_User)  Fabric_User;

typedef  TDiv #(Wd_Addr, 8)  Bytes_per_Fabric_Addr;
Integer  bytes_per_fabric_addr = valueOf (Bytes_per_Fabric_Addr);

typedef  TDiv #(Wd_Data, 8)  Bytes_per_Fabric_Data;
Integer  bytes_per_fabric_data = valueOf (Bytes_per_Fabric_Data);

// ================================================================

endpackage
