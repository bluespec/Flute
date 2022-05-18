#!/usr/bin/env bash
# Runs synthesis on BSV-generated RTL
# This script must be invoked from MCU_INSTALL/synth/xilinx

usage()
{
    echo "usage: run_synth.sh [[-c clk_period ] [-s script] [-t top-level] [-f rtl-dir] [-d device] [-j njobs] | [-h]]"
    echo "Ensure SYN_REPO points to your synthesis tools folder" 
    echo "script, if not specified, defaults to ../synthesize.tcl"
    echo "device, if not specified, defaults to artix7 (xc7a100tcsg324-1)."
    echo "Other devices: kintex7, virtex7, virtexu, virtexuplus, kintexu, kintexuplus"
    echo "njobs, if not specified, defaults to 2"
    echo "Wraps top-level with shift-register wrapper and runs synthesis in a work folder created in PWD. All paths are relative to PWD"
    echo "NOTE: for automatic wrapper generation to work correctly, the top-level module should be defined exclusively in its own file"
}

# Defaults for CLAs
util_rpt='flat'
tclscript='../synthesize.tcl'
njobs=2

# Associative arrays relating Xilinx tradenames to devices. Add new devices here.
declare -A devices
devices['artix7']='xc7a100tcsg324-1'
devices['kintex7']='xc7k325tffg900-2'
devices['virtex7']='xc7vx485tffg1761-2'
devices['virtexu']='xcvu095-ffva2104-2-e'
devices['virtexuplus']='xcvu9p-flga2104-2L-e'
devices['kintexu']='xcku040-ffva1156-2-e'
devices['kintexuplus']='xcku5p-ffvb676-2-e'

device=${devices['artix7']}

# Command line processing
while [ "$1" != "" ]; do
    echo "\$1 = $1"
    case $1 in
        -c | --clk-period )     shift  # synthesis clock period
	                        clk_period=$1
                                ;;
        -t | --top-level )      shift  # synthesis top-level
	                        top_level=$1
                                ;;
        -f | --rtl-dir  )       shift  # location of rtl files
	                        rtl_dir=$1
                                ;;
        -d | --device)          shift
	                        device=${devices[$1]}
                                ;;
        -j | --jobs)            shift
	                        njobs=$1
                                ;;
        -s | --script)          shift
	                        tclscript=$1
                                ;;
        -h | --help )           usage
                                exit
                                ;;
        * )                     usage
                                exit 1
    esac
    shift
done

# The Vivado project
synth_dir=work-${top_level}-$(date +%Y%m%d%H%M)
echo ""
echo "--------"
echo "run_synth.sh: Creating synthesis directory: ${synth_dir}"
echo "--------"
echo ""
mkdir -p ${synth_dir}
pushd ${synth_dir}

# Prepare the synthesis directory

echo ""
echo "--------"
echo "run_synth.sh: Generating timing constraints: ${synth_dir}/${top_level}.xdc"
echo "--------"
echo ""


# Prepare the synthesis directory: create soft-links to constraints, sources
# Replace generic CLK_PERIOD in the xdc file with the specified clock period

# Constraint file. For flat synthesis it is a standard template.
echo "create_clock -period ${clk_period} -name CLK [get_ports CLK]"   > ./${top_level}.xdc
echo "set_property DONT_TOUCH true [get_cells -hierarchical ip_top]" >> ./${top_level}.xdc

if test -f "${SYN_REPO}/xilinx/exceptions.xdc"; then
   cat "${SYN_REPO}/xilinx/exceptions.xdc" >> ./${top_level}.xdc
fi

echo ""
echo "--------"
echo "run_synth.sh: Concat RTL for synthesis in: ${synth_dir}/system.v"
echo "--------"
echo ""
cat ${rtl_dir}/*.v > system.v

echo ""
echo "--------"
echo "run_synth.sh: Create synth wrapper for the top-level: ${synth_dir}/Wrap_${top_level}.v"
echo "--------"
$SYN_REPO/common/count_ports.py --module ${top_level} --top ${rtl_dir}/${top_level}.v --wrap Wrap_${top_level}.v

vivado -mode batch -source ${tclscript} -tclargs \
   --project_name ${top_level} \
   --top_level ${top_level} \
   --device ${device} \
   --jobs ${njobs}
