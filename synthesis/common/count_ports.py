#!/usr/bin/python3

import sys
import argparse

outputs = []
inputs = []

def fn_count_ports (ifile, clk_name, is_chisel_rtl) :
    try : infile = open (ifile, 'r')
    except :
        sys.stderr.write ("Could not open '%s' for reading" %ifile)
        sys.exit(1)

    nout = 0
    nin = 0
    for line in infile :
        if line.lstrip().startswith('input ') :
            if not clk_name in line :
                if is_chisel_rtl :
                    # chisel-rtl declares inputs in port list with comments
                    line = line.split ('/')[0]
                width = 0
                if '[' in line :
                    width = int (line.split('[')[1].split(':')[0])
                nin = nin + width + 1

        if line.lstrip().startswith('output ') :
            if is_chisel_rtl :
                # chisel-rtl declares outputs in port list with comments
                line = line.split ('/')[0]
            width = 0
            if '[' in line :
                width = int (line.split('[')[1].split(':')[0])
            nout = nout + width + 1

    infile.close ()
    return (nout, nin)


def fn_wrap_portmap (ofile) :
    try : outfile = open (ofile, 'w')
    except :
        sys.stderr.write ("Could not open '%s' for writing" %ofile)
        sys.exit(1)

    outfile.write ("module wrap (output reg Dout    ,\n")
    outfile.write ("             input      Din     ,\n")
    outfile.write ("             input      clr     ,\n")
    outfile.write ("             input      shift   ,\n")
    outfile.write ("             input      ld      ,\n")
    outfile.write ("             input      CLK);\n")
    outfile.write ("\n")

    outfile.close ()

def fn_wrap_inputs (ifile, ofile, nin, clk_name, is_chisel_rtl) :
    global inputs
    try : infile = open (ifile, 'r')
    except :
        sys.stderr.write ("Could not open '%s' for reading" %ifile)
        sys.exit(1)

    try : outfile = open (ofile, 'a')
    except :
        sys.stderr.write ("Could not open '%s' for appending" %ofile)
        sys.exit(1)

    outfile.write ("   parameter Nin = %d;\n" %nin)
    outfile.write ("   integer i;\n")
    outfile.write ("\n")
    outfile.write ("   reg [Nin-1:0] shift_reg_i;\n")
    outfile.write ("   always @(posedge CLK) begin\n")
    outfile.write ("      if (clr) shift_reg_i <= 0;\n")
    outfile.write ("      else if (shift) begin\n")
    outfile.write ("         shift_reg_i[0] <= Din;\n")
    outfile.write ("         for (i = 1; i < Nin ; i = i + 1) begin\n")
    outfile.write ("            shift_reg_i[i] <= shift_reg_i[i-1];\n")
    outfile.write ("         end\n")
    outfile.write ("      end\n")
    outfile.write ("   end // always @ (posedge CLK)\n")
    outfile.write ("\n")
    outfile.write ("   // inputs \n")

    # In chisel-rtl the input/output is a part of the port-list and end with a ,
    # except for the last case. They all have comments
    for line in infile :
        if line.lstrip().startswith('input ') :
            if not clk_name in line :
                if is_chisel_rtl :
                    line = line.split('/')[0]    # removes comments
                    line = line.split(',')[0]    # removes , if it exists
                    wire_decl = 'wire ' + line.strip().lstrip('input') + ';'
                    outfile.write ("   %s\n" %wire_decl)
                    inputs.append (line.strip().split()[-1])
                else :
                    wire_decl = 'wire ' + line.strip().lstrip('input')
                    outfile.write ("   %s\n" %wire_decl)
                    inputs.append (line.strip().split()[-1].rstrip(';'))

    outfile.write ("\n")
    outfile.write ("   assign {\n")
    first_in = True
    for inp in inputs :
        if first_in :
            outfile.write ("       %s\n" %inp)
            first_in = False
        else :
            outfile.write ("     , %s\n" %inp)
    outfile.write ("   } = shift_reg_i;\n\n")

    outfile.close ()
    infile.close ()


def fn_wrap_outputs (ifile, ofile, nout, is_chisel_rtl) :
    global outputs
    try : infile = open (ifile, 'r')
    except :
        sys.stderr.write ("Could not open '%s' for reading" %ifile)
        sys.exit(1)

    try : outfile = open (ofile, 'a')
    except :
        sys.stderr.write ("Could not open '%s' for appending" %ofile)
        sys.exit(1)

    outfile.write ("   parameter Nout = %d;\n" %nout)
    outfile.write ("\n")
    outfile.write ("   reg [Nout-1:0] shift_reg_o;\n")
    outfile.write ("\n")
    outfile.write ("   // outputs \n")

    for line in infile :
        if line.lstrip().startswith('output ') :
            if is_chisel_rtl :
                line = line.split('/')[0]    # removes comments
                line = line.split(',')[0]    # removes , if it exists
                wire_decl = 'wire ' + line.strip().lstrip('output') + ';'
                outfile.write ("   %s\n" %wire_decl)
                outputs.append (line.strip().split()[-1])
            else :
                wire_decl = 'wire ' + line.strip().lstrip('output')
                outfile.write ("   %s\n" %wire_decl)
                outputs.append (line.strip().split()[-1].rstrip(';'))

    outfile.write ("\n")
    outfile.write ("   always @(posedge CLK) begin      \n")
    outfile.write ("      if (clr) shift_reg_o <= 0;    \n")
    outfile.write ("      else if (ld) shift_reg_o <= { \n")
    first_out = True
    for out in outputs :
        if first_out :
            outfile.write ("          %s\n" %out)
            first_out = False
        else :
            outfile.write ("        , %s\n" %out)
    outfile.write ("      };\n\n")
    outfile.write ("      else if (shift) begin\n")
    outfile.write ("         Dout <= shift_reg_o[Nout-1];\n")
    outfile.write ("         for (i = Nout-1; i > 0; i = i - 1) begin\n")
    outfile.write ("            shift_reg_o[i] <= shift_reg_o[i-1];\n")
    outfile.write ("         end\n")
    outfile.write ("      end\n")
    outfile.write ("   end // always @ (posedge CLK)\n")
    outfile.write ("\n\n")

    outfile.close ()
    infile.close ()

def fn_wrap_instantiate (ifile, ofile, top_mod, clk_name) :
    try : infile = open (ifile, 'r')
    except :
        sys.stderr.write ("Could not open '%s' for reading" %ifile)
        sys.exit(1)

    try : outfile = open (ofile, 'a')
    except :
        sys.stderr.write ("Could not open '%s' for appending" %ofile)
        sys.exit(1)

    outfile.write ("   %s ip_top ( \n" % top_mod)
    outfile.write ("        .%s (CLK) \n" % clk_name)

    for inp in inputs :
        outfile.write ("      , .%s (%s) \n" % (inp,inp))

    for out in outputs :
        outfile.write ("      , .%s (%s) \n" % (out,out))

    outfile.write ("   ); \n\n")
    outfile.write ("endmodule\n")


#------------------------
def main (argv):
    # Command-line parsing
    parser = argparse.ArgumentParser (
              formatter_class=argparse.RawDescriptionHelpFormatter
            , description='Create synthesis wrapper'
            )

    parser.add_argument (
              '--module'
            , action="store"
            , dest='module'
            , required=True
            , help='The verilog top-level module being synthesized'
            )

    parser.add_argument (
              '--top'
            , action="store"
            , dest='top'
            , required=True
            , help='The verilog top-level file being synthesized'
            )

    parser.add_argument (
              '--wrap'
            , action="store"
            , dest='ofile'
            , default='wrap.v'
            , help='The output wrapper module'
            )

    parser.add_argument (
              '--from-chisel'
            , action="store_true"
            , dest='is_chisel_rtl'
            , default=False
            , help='Is the RTL from chise?'
            )

    parser.add_argument (
              '--clock'
            , action="store"
            , dest='clock_name'
            , default='CLK'
            , help='The name of the clock pin'
            )

    args = parser.parse_args()

    fn_wrap_portmap (args.ofile)
    (nout, nin) = fn_count_ports (args.top, args.clock_name, args.is_chisel_rtl)

    fn_wrap_inputs (args.top, args.ofile, nin, args.clock_name, args.is_chisel_rtl)
    fn_wrap_outputs (args.top, args.ofile, nout, args.is_chisel_rtl)
    fn_wrap_instantiate (args.top, args.ofile, args.module, args.clock_name)

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main (sys.argv))
