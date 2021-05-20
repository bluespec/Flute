#!/usr/bin/python3

# Copyright (c) 2018-2021 Bluespec, Inc.
# See LICENSE for license details

# ================================================================

usage_line = (
    "Usage:\n"
    "  $ CMD  <repo>  <arch>  <privs>  <cache_org>  <sim>    <debug?>  <tohost?>  <tv?>\n"
    "  where\n"
    "    (Required)\n"
    "      1. <repo>       is a path to the CPU source repo (such as Magritte/Piccolo/Flute)\n"
    "      2. <arch>       is a word like RV64GC, RV32IM, RV64IMAFDC, etc. for the unprivileged arch\n"
    "      3. <privs>      is MU or MSU for the privileged arch\n"
    "      4. <cache_org>  is WT_L1, WB_L1, WB_L1_L2, or TCM\n"
    "                         where WT = Writethrough,  WB = Writeback\n"
    "                               L1 is L1-only,  L1_L2 is L1 and L2\n"
    "                               TCM is Tightly Coupled Memory (no cache, fixed latency SRAM)\n"
    "      5. <sim>        is bluesim, verilator or iverilog\n"
    "    (Optional, in any order)\n"
    "      6. <debug?>     'debug'  (configures to include Debug Module)\n"
    "      7. <tohost?>    'tohost' (configures to watch the 'tohost' memory address\n"
    "                                 for ISA test termination)\n"
    "      8. <tv?>        'tv'     (configures to include Tandem Verification trace generation)\n"
)

# ================================================================

import sys
import os

# ================================================================

def main (argv = None):
    sys.stdout.write ("Use flag --help  or --h for a help message\n")
    if ((len (argv) < 6) or
        (len (argv) > 9) or
        (argv [1] == '-h') or (argv [1] == '--help')):

        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 0

    arg_repo      = argv [1]
    arg_arch      = argv [2]
    arg_privs     = argv [3]
    arg_cache_org = argv [4]
    arg_sim       = argv [5]
    opt_args      = argv [6:]

    # ----------------
    # Collect <cpu_repo> and check if exists

    if (not os.path.exists (arg_repo)):
        sys.stdout.write ("Error: arg <repo> (='{0}') does not exist?\n".format (arg_repo))
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1

    repo     = os.path.join ("..", arg_repo)
    repobase = os.path.basename (os.path.abspath (os.path.normpath (arg_repo)))

    # ----------------
    # Collect <arch> and check if legal

    known_arch_features = "IMAFDC"
    arch = arg_arch.upper ()

    # G is an abbreviation for IMAFD
    arch = arch.replace ("G", "IMAFD")

    # We always have "I"
    if ((not "I" in arch) and (not "G" in arch)): arch = arch + "I"

    if (not (arch.startswith ("RV32") or arch.startswith ("RV64"))):
        sys.stdout.write ("Error in command-line arg <arch> (='{0}')\n".format (arch))
        sys.stdout.write ("    Should begin with 'RV32' or 'RV64'\n")
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1
    if (not all (map ((lambda x: x in known_arch_features), arch [4:]))):
        sys.stdout.write ("Error in command-line arg <arch> (='{0}')\n".format (arch))
        sys.stdout.write ("    Should only contain alphabets from {0} after RV32/RV64\n".format (known_arch_features))
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1
    if (not ('I' in arch)):
        sys.stdout.write ("Error in command-line arg <arch> (='{0}')\n".format (arch))
        sys.stdout.write ("    Should contain 'I'\n")
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1
    if (('D' in arch) and not ('F' in arch)):
        sys.stdout.write ("Error in command-line arg 1 (<arch>='{0}')\n".format (arch))
        sys.stdout.write ("    Should contain 'F' since it contains 'D'\n")
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1

    arch = canonical_arch_string (arch)
    # sys.stdout.write ("Canonical arch string is:  '{0}'\n".format (arch))

    # ----------------
    # Collect <privs> and check if legal

    if ((arg_privs != "MU") and
        (arg_privs != "MSU")):
        sys.stdout.write ("Error in command-line arg for <privs> (='{0}')\n".format (arg_privs))
        sys.stdout.write ("    Should be MU or MSU\n")
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1

    privs = arg_privs

    # ----------------
    # Collect <cache_org> and check if legal

    if ((arg_cache_org != "WT_L1") and
        (arg_cache_org != "WB_L1") and
        (arg_cache_org != "WB_L1_L2") and
        (arg_cache_org != "TCM")):
        sys.stdout.write ("Error in command-line arg for <cache_org> (='{0}')\n".format (arg_cache_org))
        sys.stdout.write ("    Should be WT_L1, WB_L1, WB_L1_L2 or TCM\n")
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1

    cache_org = arg_cache_org

    # ----------------
    # Collect <sim> and check if legal

    sim = arg_sim.lower ()
    if ((sim != 'bluesim') and
        (sim != 'verilator') and
        (sim != 'iverilog')):
        sys.stdout.write ("Error in command-line arg for <sim> (='{0}')\n".format (sim))
        sys.stdout.write ("    Should be  'bluesim',  'verilator'  or  'iverilog'\n")
        sys.stdout.write (usage_line.replace ("CMD", argv [0]))
        sys.stdout.write ("\n")
        return 1

    # ----------------
    # Collect optional <debug>, <tohost> and <tv> args

    if "debug" in opt_args:
        debug = "_debug"
    else:
        debug = ""

    if "tohost" in opt_args:
        tohost = "_tohost"
    else:
        tohost = ""

    if "tv" in opt_args:
        tv = "_tv"
    else:
        tv = ""

    # ----------------
    # All args collected; create the build directory and its Makefile

    make_build_dir (repo, repobase, arch, privs, cache_org, sim, tohost, debug, tv)

    return 0

# ================================================================
# Create canonical architecture string (alphabetical order, no duplicates)
# Can be invoked with or without the leading "RV32" or "RV64"

def canonical_arch_string (arch):
    prefix = ""
    letters_s = arch
    if arch.startswith ("RV32"):
        prefix = "RV32"
        letters_s = arch [4:]
    elif arch.startswith ("RV64"):
        prefix = "RV64"
        letters_s = arch [4:]

    # Convert  'letters_s'  string to list of single-char strings
    letters_l  = map  ((lambda j: letters_s [j]),  (range (len (letters_s))))
    # Remove duplicates by converting into a set
    letters_fs = frozenset (letters_l)
    # Convert into a sorted list
    letters_l  = sorted (letters_fs)
    # Join them back into a string
    letters_s  = "".join (letters_l)
    # Replace IMAFD by G
    if (letters_s == "ADFIM"):
        letters_s = "G"
    elif (letters_s == "ACDFIM"):
        letters_s = "GC"

    return (prefix + letters_s)

# ================================================================
# Create the build directory and its Makefile

def make_build_dir (repo, repobase, arch, privs, cache_org, sim, tohost, debug, tv):

    # debugging only
    if True:
        sys.stdout.write ("repo      = '{0}'\n".format (repo))
        sys.stdout.write ("repobase  = '{0}'\n".format (repobase))
        sys.stdout.write ("arch      = '{0}'\n".format (arch))
        sys.stdout.write ("privs     = '{0}'\n".format (privs))
        sys.stdout.write ("cache_org = '{0}'\n".format (cache_org))
        sys.stdout.write ("sim       = '{0}'\n".format (sim))
        if (tohost != ""): sys.stdout.write ("tohost\n")
        if (debug  != ""): sys.stdout.write ("debug\n")
        if (tv     != ""): sys.stdout.write ("tv\n")

    # Create the directory
    dirname = repobase + "_" + arch + "_" + privs + "_" + cache_org + "_" + sim + tohost + debug + tv
    if (os.path.exists (dirname)):
        sys.stdout.write ("Directory  '{0}'  exists already\n".format (dirname))
    else:
        sys.stdout.write ("Creating directory    '{0}'\n".format (dirname));
        os.mkdir (dirname)

    # Create the Makefile (backing up existing copy, if any)
    Makefile_filename = os.path.join (dirname, "Makefile")
    if os.path.exists (Makefile_filename):
        # Back up exising copy by renaming it with a new numeric suffix
        j = 1
        while True:
            suffixed_name = "{0}_{1}".format (Makefile_filename, j)
            if (not os.path.exists (suffixed_name)): break
            j = j + 1
        sys.stdout.write ("    '{0}'  exists already\n".format (Makefile_filename))
        sys.stdout.write ("    Renaming it to {0}\n".format (suffixed_name))
        os.rename (Makefile_filename, suffixed_name)

    sys.stdout.write ("Creating Makefile  '{0}'\n".format (Makefile_filename))
    fo = open (Makefile_filename, "w")

    # Fill in the contents of the Makefile
    fo.write ("###  -*-Makefile-*-\n"
              "\n"
              "# *** This file is program-generated, not hand-written. ***\n"
              "# *** Edit only if you need to tweak it! ***\n")

    fo.write ("# ================================================================\n")
    fo.write ("\n")
    fo.write ("REPO   ?= {0}\n".format (repo))
    fo.write ("ARCH   ?= {0}\n".format (arch))
    fo.write ("PRIVS  ?= {0}\n".format (privs))
    fo.write ("CACHES ?= {0}\n".format (cache_org))

    if (cache_org == "WB_L1_L2"):
        fo.write ("# For WB_L1_L2, use src_Core/Core_v2 instead of default src_Core/Core\n")
        fo.write ("SRC_CORE ?= $(REPO)/src_Core/Core_v2\n")

    fo.write ("\n")

    # RISC-V config macros passed into Bluespec 'bsc' compiler
    fo.write ("# ================================================================\n")
    fo.write ("# RISC-V config macros passed into Bluespec 'bsc' compiler\n")
    fo.write ("\n")
    fo.write ("BSC_COMPILATION_FLAGS += \\\n")
    fo.write ("\t-D " + arch [0:4] + " \\\n")

    # RISC-V arch features
    arch_flags = ""
    if ("I" in arch) or ("G" in arch): arch_flags += "  -D ISA_I"
    if ("M" in arch) or ("G" in arch): arch_flags += "  -D ISA_M"
    if ("A" in arch) or ("G" in arch): arch_flags += "  -D ISA_A"
    if ("C" in arch):                  arch_flags += "  -D ISA_C"
    fo.write ("\t{0}  \\\n".format (arch_flags.lstrip()))

    if (("F" in arch) or ("D" in arch) or ("G" in arch)):
        arch_flags = ""
        if ("F" in arch) or ("G" in arch): arch_flags += "  -D ISA_F"
        if ("D" in arch) or ("G" in arch): arch_flags += "  -D ISA_D"
        arch_flags += "  -D INCLUDE_FDIV  -D INCLUDE_FSQRT"
        fo.write ("\t{0}  \\\n".format (arch_flags.lstrip()))

    # RISC-V privilege levels
    fo.write ("\t-D ISA_PRIV_M")
    if ("S" in privs): fo.write ("  -D ISA_PRIV_S")
    if ("U" in privs): fo.write ("  -D ISA_PRIV_U")
    fo.write ("  \\\n")

    # If 'S', specify Virtual Memory scheme
    if ("S" in privs):
        if (arch.startswith ("RV32")):
            fo.write ("\t-D SV32  \\\n")
        else:
            fo.write ("\t-D SV39  \\\n")

    # Bluespec HW implementation choice for shifter
    # fo.write ("\t-D SHIFT_NONE    \\\n")
    fo.write ("\t-D SHIFT_BARREL    \\\n")
    # fo.write ("\t-D SHIFT_SERIAL    \\\n")
    # fo.write ("\t-D SHIFT_MULT    \\\n")

    # Bluespec HW implementation choice for integer multiply/divide
    fo.write ("\t-D MULT_SYNTH    \\\n")
    # fo.write ("\t-D MULT_SERIAL    \\\n")

    # Bluespec HW implementation choice for "near-mem"
    fo.write ("\t-D Near_Mem_Caches    \\\n")

    # Bluespec HW implementation choice for fabric data width
    fo.write ("\t-D FABRIC64    \\\n")

    # Bluespec testing: observe writes to <tohost> and terminate when non-zero
    if (tohost != ""):
        fo.write ("\t-D WATCH_TOHOST    \\\n")

    # Support for RISC-V Debug Module
    if (debug != ""):
        fo.write ("\t-D INCLUDE_GDB_CONTROL  \\\n")

    # Support for Bluespec Tandem Verification traces
    if (tv != ""):
        fo.write ("\t-D INCLUDE_TANDEM_VERIF  \\\n")

    fo.write ("\n")

    # L2 Cache (LLC) for WB_L1_L2
    if (cache_org == "WB_L1_L2"):
        fo.write ("#================================================================\n"
                  "# For LLCache (used only for WB_L1_L2)\n"
                  "\n"
                  "# core size\n"
                  "CORE_SIZE ?= SMALL\n"
                  "# default 1 core\n"
                  "CORE_NUM ?= 1\n"
                  "# cache size\n"
                  "CACHE_SIZE ?= LARGE\n"
                  "\n"
                  "BSC_COMPILATION_FLAGS += \\\n"
                  "     -D CORE_$(CORE_SIZE) \\\n"
                  "     -D NUM_CORES=$(CORE_NUM) \\\n"
                  "     -D CACHE_$(CACHE_SIZE) \\\n"
                  "\n"
                  "LLCACHE_DIR   = $(REPO)/src_Core/Near_Mem_VM_WB_L1_L2/src_LLCache\n"
                  "PROCS_LIB_DIR = $(LLCACHE_DIR)/procs/lib\n"
                  "PROCS_OOO_DIR = $(LLCACHE_DIR)/procs/RV64G_OOO\n"
                  "COHERENCE_DIR = $(LLCACHE_DIR)/coherence/src\n"
                  "\n"
                  "CUSTOM_DIRS = $(LLCACHE_DIR):$(PROCS_LIB_DIR):$(PROCS_OOO_DIR):$(COHERENCE_DIR)\n"
                  "\n")

    # Default ISA test (RV32/RV64ui-p-add)
    fo.write ("#================================================================\n"
              + "# Default ISA test\n"
              + "\n"
              + ("TEST ?= {0}".format (arch [0:4].lower()))
              + "ui-p-add\n")
    fo.write ("\n")

    # Include common boilerplate Makefile rules
    fo.write ("#================================================================\n")
    fo.write ("# Common boilerplate rules\n")
    fo.write ("\n")
    fo.write ("include $(REPO)/builds/Resources/Include_Common.mk\n")
    fo.write ("\n")

    # Include simulator-specific Makefile rules
    fo.write ("#================================================================\n")
    fo.write ("# Makefile rules for building for specific simulator\n")
    fo.write ("\n")
    fo.write ("include $(REPO)/builds/Resources/Include_{0}.mk\n".format (sim))
    fo.write ("\n")

    fo.close ()

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main (sys.argv))
