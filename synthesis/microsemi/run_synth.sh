#!/usr/bin/env bash
# Runs libero synthesis on BSV-generated RTL
# This script must be invoked from MCU_INSTALL/synth/microsemi

usage()
{
    echo "usage: run_synth.sh [[-c clk_period ] [-t top-level] [-f rtl-dir] | [-h]]"
    echo "Ensure SYN_REPO points to root of your synth tools repo" 
}

# Command line processing
while [ "$1" != "" ]; do
    case $1 in
        -c | --clk-period )     shift  # synthesis clock period
	                        clk_period=$1
                                ;;
        -t | --top-level )      shift  # the synthesis top-level
	                        top_level=$1
                                ;;
        -f | --rtl-dir )        shift  # the location of rtl files
	                        rtl_dir=$1
                                ;;
        -h | --help )           usage
                                exit
                                ;;
        * )                     usage
                                exit 1
    esac
    shift
done

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
echo "run_synth.sh: Generating timing constraints: ${synth_dir}/timing.sdc"
echo "--------"
echo ""

# Constraint file. For flat synthesis it is a standard template.
echo "# Constrain clock port clk with a ${clk_period} ns requirement" > timing.sdc
echo "" >> timing.sdc
echo "create_clock -name clk -period ${clk_period} [get_ports CLK]" >> timing.sdc
echo "set_input_delay -clock clk -max 1 [all_inputs]" >> timing.sdc
echo "set_output_delay -clock clk -max 1 [all_outputs]" >> timing.sdc
echo "" >> timing.sdc

# The basic constraints are used for synthesis and pnr. Add exceptions for timing
# verification.
cp timing.sdc timing-synth.sdc
cp timing.sdc timing-pnr.sdc

# Add user-specified exceptions to synth timing script
if test -f "${SYN_REPO}/microsemi/exceptions-synth.sdc"; then
   cat ${SYN_REPO}/microsemi/exceptions-synth.sdc >> timing-synth.sdc
fi

# Add user-specified exceptions to pnr timing script
if test -f "${SYN_REPO}/microsemi/exceptions-pnr.sdc"; then
   cat ${SYN_REPO}/microsemi/exceptions-pnr.sdc >> timing-pnr.sdc
fi

# Add user-specified exceptions to timing verification script
if test -f "${SYN_REPO}/microsemi/exceptions-timing.sdc"; then
   cat ${SYN_REPO}/microsemi/exceptions-timing.sdc >> timing.sdc
fi

cp ${SYN_REPO}/microsemi/report_timing.tcl ./

echo ""
echo "--------"
echo "run_synth.sh: Customizing the synthesis script: ${synth_dir}/synthesis.tcl"
echo "--------"
echo ""
# Specify the TOP-LEVEL in the synthesis script
sed "s/TOP-LEVEL/${top_level}/" ../synthesize.tcl > synthesize.tcl

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

echo ""
echo "--------"
echo "run_synth.sh: Run synthesis"
echo "--------"

libero script:synthesize.tcl

popd
