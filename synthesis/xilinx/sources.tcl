# Set 'sources_1' fileset object
set obj [get_filesets sources_1]
set files [list \
 [file normalize "${origin_dir}/system.v"] \
 [file normalize "${origin_dir}/Wrap_${top_level}.v"] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/ASSIGN1.v"          ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/ResetInverter.v"    ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/MakeResetA.v"       ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/SyncResetA.v"       ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/SyncWire.v"         ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/SyncHandshake.v"    ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/SyncFIFOLevel.v"    ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/BRAM1BELoad.v"      ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/BRAM1BE.v"          ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/BRAM2BELoad.v"      ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/BRAM2BE.v"          ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/BRAM2.v"            ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/FIFO10.v"           ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/FIFO1.v"            ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/FIFO20.v"           ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/FIFO2.v"            ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/FIFOL1.v"           ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/RegFileLoad.v"      ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/RegFile.v"          ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/RevertReg.v"        ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/SizedFIFO0.v"       ] \
 [file normalize "${origin_dir}/../../common/src_bsc_lib_RTL/SizedFIFO.v"        ] \
]
add_files -norecurse -fileset $obj $files


