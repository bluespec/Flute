#!/usr/bin/python3

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.
# ================================================================
# Reads a memhex file created by gen_bootroom.cc
# and constructs a BSV function representating that rom.

# ================================================================

import sys
import os
import datetime

# ================================================================

def main (argv = None):
    if ((len (argv) > 1) and ((argv [1] == "-h") or (argv [1] == "--help"))):
        print_usage (argv)
        return 0

    if (len (argv) != 3):
        print_usage (argv)
        return 1

    try:
        fin = open (argv [1], 'r')
    except:
        sys.stderr.write ("ERROR: unable to open or read file '{0}' for input\n".format (argv [1]))
        return 1

    try:
        fout = open (argv [2], 'w')
    except:
        sys.stderr.write ("ERROR: unable to open file '{0}' for output\n".format (argv [2]))
        return 1

    addr_lim = gen_BSV_ROM_function (fin, fout)
    sys.stderr.write ("Wrote BSV ROM function into file '{0}'; address limit is: {1}\n".format (argv [2], addr_lim))

    fin.close ()
    fout.close ()
    return 0

# ================================================================

def print_usage (argv):
    sys.stdout.write ("Usage:    {0}  <memhex input filename>  <BSV output filename>\n".format (argv [0]))
    sys.stdout.write ("    The memhex file should contain 32b words\n")

# ================================================================

def gen_BSV_ROM_function (fin, fout):

    # Read the memhex file
    lines = fin.readlines ()

    # Generate the arms of the BSV case-statement.
    case_arms_0 = []
    case_arms_4 = []
    line_num = 1
    addr = 0
    for line in lines:
        line = line.strip ()
        if line.startswith ('@'):
            sys.stdout.write ("Note: ignoring line {0}: '{1}'\n".format (line_num, line))
        else:
            if ((addr % 8) == 0):
                case_arms_0.append ("         {0}: 32'h_{1};\n".format (addr, line))
            else:
                case_arms_4.append ("         {0}: 32'h_{1};\n".format (addr, line))
            addr = addr + 4
        line_num = line_num + 1
    
    iso_utc_time = datetime.datetime.utcnow ().isoformat ()

    fout.write ("// ***** DO NOT EDIT *****\n")
    fout.write ("// ***** This file was generated from a script *****\n")
    fout.write ("// Generated at UTC {0}\n".format (iso_utc_time))
    fout.write ("\n")
    fout.write ("\n")
    fout.write ("// This file is a BSV 'include' file\n")
    fout.write ("// The function below represents a ROM of {0} bytes\n".format (addr))
    fout.write ("\n")
    fout.write ("\n")
    fout.write ("// Function for 4-bytes values at addrs aligned to 'b000\n")
    fout.write ("\n")
    fout.write ("function Bit #(32) fn_read_ROM_0 (Bit #(n) addr);\n")
    fout.write ("   return\n")
    fout.write ("      case (addr)\n")
    for case_arm in case_arms_0:
        fout.write (case_arm)
    fout.write ("         default: 32'h_AAAA_AAAA;\n")
    fout.write ("      endcase;\n")
    fout.write ("endfunction: fn_read_ROM_0\n")
    fout.write ("\n")
    fout.write ("// Function for 4-bytes values at addrs aligned to 'b100\n")
    fout.write ("\n")
    fout.write ("function Bit #(32) fn_read_ROM_4 (Bit #(n) addr);\n")
    fout.write ("   return\n")
    fout.write ("      case (addr)\n")
    for case_arm in case_arms_4:
        fout.write (case_arm)
    fout.write ("         default: 32'h_AAAA_AAAA;\n")
    fout.write ("      endcase;\n")
    fout.write ("endfunction: fn_read_ROM_4\n")

    return addr

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main (sys.argv))
