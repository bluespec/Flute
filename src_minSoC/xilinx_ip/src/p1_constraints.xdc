set_property MARK_DEBUG true [get_nets jtagtap/CLK_jtag_tclk_out]
create_clock -period 40.000 -name tck_internal -waveform {0.000 20.000} [get_nets jtagtap/CLK_jtag_tclk_out]
set_clock_uncertainty 2.00 [get_clocks *tck_internal*]
