#!/usr/bin/python3

# Copyright (c) 2017-2020 Bluespec, Inc. All Rights Reserved.
# ================================================================

# Background: CPU caches have a large number of sizing concepts such
# as total size, address size, word size, cache line size, set
# associativity, tag size, index size, address of word in cache line,
# address of cache line in set, and so on, and of course each can be
# expressed in various size units like bits, bytes, kilobytes, ...
# depending on convenience.  Further, in BSV we often want the sizes
# both as types and as values.  Since all these sizes are
# inter-related, hand-written declarations are laborious to produce;
# do not often make the inter-relationships clear; and it is easy to
# make a mistake and produce inconsistent size declarations.

# This program is given just 5 basic, independent sizes:
#    Bit per PA          (bits in a physical address)
#    KB per cache        (total cache memory size)
#    bit per cache word  (bits in a cache word)
#    cwords per cline    (cache words per cache line)
#    ways per set        (set associativity)
# and generates declarations for all the sized elements of interest in BSV
# source code as a BSV package. Each declaration shows the actual size
# and, as a comment, the formula used, if it is derived from other
# sizes.  The basic sizes are provided on the command line.
# --help will remind you of the command-line arguments.

# We assume the cache is indexed using virtual addresses
# and tagged using physical addresses.

# ================================================================

import argparse
import sys
import os
import math
import yaml

# ================================================================
# Key data structure:
#     items :: [(name::string, value::int: formula::string)]

# ================================================================

def main (argv = None):
    (bsv_package_name, items) = process_program_args ()
    print ("Generating package {0} from these basic values:".format (bsv_package_name))
    print_items (items)

    items = compute_derived_values (items)

    addr_hi_cset_in_cache = sel (items, "addr_hi_cset_in_cache")
    if (addr_hi_cset_in_cache > 11):
        print ("ERROR: addr_hi_cset_in_cache (high-bit of cache index) = {0}".format (addr_hi_cset_in_cache))
        print ("    Should be inside 4KB page (< 12) for virtual-indexed, physically-tagged cache.")
        for item in items:
            print (item)

    # ---- Generate BSV source file
    generate_BSV_file (bsv_package_name, items)

    return 0

# ================================================================
# Yaml-file and command-line argument processing

def process_program_args ():
    sys.stdout.write ("Use  --help  for help\n")

    # ---- defaults
    default_yaml_file     = 'RV32_Sv32_args.yaml'
    bsv_package_name      = 'Cache_Decls_RV32'
    item_Bits_per_PA      = 34
    item_KB_per_Cache     = 4
    item_Bits_per_CWord   = 64
    item_CWords_per_CLine = 4
    item_Ways_per_CSet    = 1

    # ---- Parse command-line arguments
    parser = argparse.ArgumentParser (description = "Generate BSV declarations for a cache module.",
                                      epilog = "Priority: command-line args > yaml file > defaults")
    parser.add_argument ('--yaml_file',
                         help = "YAML file containing the basic sizes (default {0})".format (default_yaml_file))
    parser.add_argument ('--bsv_package_name',
                         help = "Name of BSV package in generated BSV source file (default: {0})".format (bsv_package_name))
    parser.add_argument ('--Bits_per_PA',
                         help = "Bits per Physical Address (default = {0})".format (item_Bits_per_PA))
    parser.add_argument ('--KB_per_Cache' ,
                         help = "Total size of cache in kilobytes (default = {0})".format (item_KB_per_Cache))
    parser.add_argument ('--Bits_per_CWord',
                         help = "Bits per cache word (default = {0})".format (item_Bits_per_CWord))
    parser.add_argument ('--CWords_per_CLine',
                         help = "# of cache words in a cache line (default = {0})".format (item_CWords_per_CLine))
    parser.add_argument ('--Ways_per_CSet',
                         help = "Set associativity of cache (# of ways in a cache set; default {0})".format (item_Ways_per_CSet))
    args = parser.parse_args ()

    args_yaml = None
    if (args.yaml_file == None):
        try:
            sys.stdout.write ("No yaml_file specified; attempting to read default {0} file ...".format (default_yaml_file))
            sys.stdout.flush ()
            with open (default_yaml_file, 'r') as stream:
                args_yaml = yaml.load (stream)
            sys.stdout.write ("success\n")
        except:
            sys.stdout.write ("... no such file found\n")
            pass
    else:
        try:
            with open (args.yaml_file, 'r') as stream:
                sys.stdout.write ("Reading {0} file\n".format (args.yaml_file))
                args_yaml = yaml.load (stream)
        except:
            sys.stdout.write ("ERROR: unable to read {0} file\n".format (args.yaml_file))
            sys.exit (1)

    # Over-ride from yaml file
    if (args_yaml != None):
        if (args_yaml ['bsv_package_name'] != None): bsv_package_name      = args_yaml ['bsv_package_name']
        if (args_yaml ['Bits_per_PA']      != None): item_Bits_per_PA      = int (args_yaml ['Bits_per_PA'])
        if (args_yaml ['KB_per_Cache']     != None): item_KB_per_Cache     = int (args_yaml ['KB_per_Cache'])
        if (args_yaml ['Bits_per_CWord']   != None): item_Bits_per_CWord   = int (args_yaml ['Bits_per_CWord'])
        if (args_yaml ['CWords_per_CLine'] != None): item_CWords_per_CLine = int (args_yaml ['CWords_per_CLine'])
        if (args_yaml ['Ways_per_CSet']    != None): item_Ways_per_CSet    = int (args_yaml ['Ways_per_CSet'])

    # Over-ride from command-line arguments
    # ---- Create 'items' data structure with all values
    if (args.bsv_package_name  != None): bsv_package_name      = args.bsv_package_name
    if (args.Bits_per_PA       != None): item_Bits_per_PA      = int (args.Bits_per_PA)
    if (args.KB_per_Cache      != None): item_KB_per_Cache     = int (args.KB_per_Cache)
    if (args.Bits_per_CWord    != None): item_Bits_per_CWord   = int (args.Bits_per_CWord)
    if (args.CWords_per_CLine  != None): item_CWords_per_CLine = int (args.CWords_per_CLine)
    if (args.Ways_per_CSet     != None): item_Ways_per_CSet    = int (args.Ways_per_CSet)

    items = [ ("Bits_per_PA",      item_Bits_per_PA,      "(basic)"),
              ("KB_per_Cache",     item_KB_per_Cache,     "(basic)"),
              ("Bits_per_CWord",   item_Bits_per_CWord,   "(basic)"),
              ("CWords_per_CLine", item_CWords_per_CLine, "(basic)"),
              ("Ways_per_CSet",    item_Ways_per_CSet,    "(basic; associativity)") ]

    return (bsv_package_name, items)

# ================================================================
# Print 'items'

def print_items (items):
    for (attr, val, formula) in items:
        print ("    {0:20s}= {1:4}    {2}".format ((attr + ":"), val, formula))

# ================================================================
# Select value associated with name from items

def sel (items, name):
    for (attr, val, formula) in items:
        if (attr == name):
            return val
    print ("Error: no attr {0} in items".format (name))
    sys.exit (1)

def sel2 (items, name):
    for (attr, val, formula) in items:
        if (attr == name):
            return (val, formula)
    print ("Error: no attr {0} in items".format (name))
    sys.exit (1)

# ================================================================
# Compute derived values

Bits_per_Byte = 8

def compute_derived_values (items):
    items.append (("Bytes_per_CWord",
                   int (sel (items, "Bits_per_CWord") / Bits_per_Byte),
                   "Bits_per_CWord / 8"))

    items.append (("Bits_per_Byte_in_CWord",
                   int (math.log2 (sel (items, "Bytes_per_CWord"))),
                   "log2 (Bytes_per_CWord)"))

    items.append (("Bits_per_CLine",
                   sel (items, "CWords_per_CLine") * sel (items, "Bits_per_CWord"),
                   "CWords_per_CLine * Bits_per_CWord"))

    items.append (("Bytes_per_CLine",
                   int (sel (items, "Bits_per_CLine") / Bits_per_Byte),
                   "Bits_per_CLine / 8"))

    items.append (("Bytes_per_CSet",
                   sel (items, "Ways_per_CSet") * sel (items, "Bytes_per_CLine"),
                   "Ways_per_CSet * Bytes_per_CLine"))

    items.append (("Bytes_per_Cache",
                   sel (items, "KB_per_Cache") * 1024,
                   "KB_per_Cache * 1024"))

    items.append (("CWords_per_Cache",
                   int (sel (items, "Bytes_per_Cache") / sel (items, "Bytes_per_CWord")),
                   "Bytes_per_Cache / Bytes_per_CWord"))

    items.append (("CLines_per_Cache",
                   int (sel (items, "Bytes_per_Cache") / sel (items, "Bytes_per_CLine")),
                   "Bytes_per_Cache / Bytes_per_CLine"))

    items.append (("CSets_per_Cache",
                   int (sel (items, "Bytes_per_Cache") / sel (items, "Bytes_per_CSet")),
                   "Bytes_per_Cache / Bytes_per_CSet"))

    items.append (("CSet_CWords_per_Cache",
                   sel (items, "CSets_per_Cache") * sel (items, "CWords_per_CLine"),
                   "CSets_per_Cache * CWords_per_CLine"))

    items.append (("Bits_per_CWord_in_CLine",
                   int (math.log (sel (items, "CWords_per_CLine"), 2)),
                   "log2 (CWords_per_CLine)"))

    items.append (("Bits_per_Byte_in_CLine",
                   int (math.log2 (sel (items, "Bytes_per_CLine"))),
                   "log2 (Bytes_per_CLine)"))

    items.append (("Bits_per_CSet_in_Cache",
                   int (math.log2 (sel (items, "CSets_per_Cache"))),
                   "log2 (CSets_per_Cache)"))

    items.append (("Bits_per_CSet_CWord_in_Cache",
                   sel (items, "Bits_per_CSet_in_Cache") + sel (items, "Bits_per_CWord_in_CLine"),
                   "Bits_per_CSet_in_Cache + Bits_per_CWord_in_CLine"))

    items.append (("Bits_per_CTag",
                   sel (items, "Bits_per_PA") - (sel (items, "Bits_per_CSet_in_Cache")
                                                 + sel (items, "Bits_per_Byte_in_CLine")),
                   "Bits_per_PA - (Bits_per_CSet_in_Cache + Bits_per_Byte_in_CLine)"))

    items.append (("Bits_per_Way_in_CSet",
                   int (math.log2 (sel (items, "Ways_per_CSet"))),
                   "log2 (Ways_per_CSet)"))

    items.append (("addr_lo_cword_in_cline",
                   int (math.log2 (sel (items, "Bytes_per_CWord"))),
                   "log2 (Bytes_per_CWord)"))

    items.append (("addr_hi_cword_in_cline",
                   sel (items, "addr_lo_cword_in_cline") + sel (items, "Bits_per_CWord_in_CLine") - 1,
                   "addr_lo_cword_in_cline + Bits_per_CWord_in_CLine - 1"))

    items.append (("addr_lo_cset_in_cache",
                   sel (items, "addr_hi_cword_in_cline") + 1,
                   "addr_hi_cword_in_cline + 1"))

    items.append (("addr_hi_cset_in_cache",
                   sel (items, "addr_lo_cset_in_cache") + sel (items, "Bits_per_CSet_in_Cache") - 1,
                   "addr_lo_cset_in_cache + Bits_per_CSet_in_Cache - 1"))

    items.append (("addr_lo_cset_cword_in_cache",
                   int (math.log2 (sel (items, "Bytes_per_CWord"))),
                   "log2 (Bytes_per_CWord)"))

    items.append (("addr_hi_cset_cword_in_cache",
                   sel (items, "addr_lo_cset_cword_in_cache") + sel (items, "Bits_per_CSet_CWord_in_Cache") - 1,
                   "addr_lo_cword_set_in_cache + Bits_per_CWord_Set_in_Cache - 1"))

    items.append (("addr_lo_ctag",
                   sel (items, "addr_hi_cset_in_cache") + 1,
                   "addr_hi_cset_in_cache + 1"))

    return items

# ================================================================
# Generate the output (BSV source file)

def generate_BSV_file (BSV_package_name, items):
    filename = BSV_package_name + ".bsv"
    fout = open (filename, "w")

    fout.write ("// Copyright (c) 2016-2020 Bluespec, Inc. All Rights Reserved.\n" +
                "// DO NOT EDIT! This file is automatically generated from a script\n" +
                "\n" +
                "// Size, type and function declarations for cache structures\n" +
                "\n" +
                "package " + BSV_package_name + ";\n")

    fout.write ("\n" +
                "// ================================================================\n" +
                "// Basic sizes, from which everything else is derived\n" +
                "\n")

    fout.write ("// Bits_per_PA      = {0:3d}    (= 0x{1:02x})    (bits per physical addr)\n".format (
                sel (items, "Bits_per_PA"), sel (items, "Bits_per_PA")))
    fout.write ("// KB_per_Cache     = {0:3d}    (= 0x{1:02x})    (cache size)\n".format (
                sel (items, "KB_per_Cache"), sel (items, "KB_per_Cache")))
    fout.write ("// Bits_per_CWord   = {0:3d}    (= 0x{1:02x})    (bits per cache words)\n".format (
                sel (items, "Bits_per_CWord"), sel (items, "Bits_per_CWord")))
    fout.write ("// CWords_per_CLine = {0:3d}    (= 0x{1:02x})    (cache line size in cache words)\n".format (
                sel (items, "CWords_per_CLine"), sel (items, "CWords_per_CLine")))
    fout.write ("// Ways_per_CSet    = {0:3d}    (= 0x{1:02x})    (associativity)\n".format (
                sel (items, "Ways_per_CSet"), sel (items, "Ways_per_CSet")))

    fout.write ("\n" +
                "// ================================================================\n" +
                "// Type decls\n")

    gen_decls (fout, True, items)

    fout.write ("\n" +
                "// ================================================================\n" +
                "// Integer decls\n")

    gen_decls (fout, False, items)

    fout.write ("\n" +
                "// ================================================================\n" +
                "// Addresses and address-fields\n")

    fout.write ("\n" +
                "// Local synonyms (for use in this package)\n" +
                "typedef Bit #(Bits_per_CWord)               CWord;\n" +
                "typedef Bit #(Bits_per_CLine)               CLine;\n" +
                "typedef Bit #(Bits_per_CTag)                CTag;\n" +
                "typedef Bit #(Bits_per_Byte_in_CLine)       Byte_in_CLine;\n" +
                "typedef Bit #(Bits_per_CWord_in_CLine)      CWord_in_CLine;\n" +
                "typedef Bit #(Bits_per_Way_in_CSet)         Way_in_CSet;\n" +
                "typedef Bit #(Bits_per_CSet_in_Cache)       CSet_in_Cache;\n" +
                "typedef Bit #(Bits_per_CSet_CWord_in_Cache) CSet_CWord_in_Cache;\n")

    fout.write ("\n" +
                "// ================================================================\n" +
                "// Address-decode functions\n")

    fout.write ("\n" +
                "function  Bit #({:0d})  fn_Addr_to_Byte_in_CWord (Bit #(n)  addr);\n"
                .format (int (math.log2 (sel (items, "Bytes_per_CWord")))) +
                "   return  addr [{:0d}:0];\n"
                .format (int (math.log2 (sel (items, "Bytes_per_CWord"))) - 1) +
                "endfunction\n")

    fout.write ("\n" +
                "function  CWord_in_CLine  fn_Addr_to_CWord_in_CLine (Bit #(n)  addr);\n" +
                "   return  addr [addr_hi_cword_in_cline : addr_lo_cword_in_cline ];\n" +
                "endfunction\n")

    fout.write ("\n" +
                "function  CSet_in_Cache  fn_Addr_to_CSet_in_Cache (Bit #(n)  addr);\n" +
                "   return  addr [addr_hi_cset_in_cache : addr_lo_cset_in_cache ];\n" +
                "endfunction\n")

    fout.write ("\n" +
                "function  CSet_CWord_in_Cache  fn_Addr_to_CSet_CWord_in_Cache (Bit #(n)  addr);\n" +
                "   return  addr [addr_hi_cset_cword_in_cache : addr_lo_cset_cword_in_cache ];\n" +
                "endfunction\n")

    fout.write ("\n" +
                "function  CTag  fn_PA_to_CTag (Bit #(n)  pa);\n" +
                "   return  pa [(valueOf (n) - 1) : addr_lo_ctag ];\n" +
                "endfunction\n")

    fout.write ("\n" +
                "// Align to start of CLine\n" +
                "function  Bit #(n)  fn_align_Addr_to_CLine (Bit #(n)  addr);\n" +
                "   Bit #(n) mask = (1 << addr_lo_cset_in_cache) - 1;\n" +
                "   return  addr & (~ mask);\n" +
                "endfunction\n")

    fout.write ("\n" +
                "// ================================================================\n" +
                "\n" +
                "endpackage: " + BSV_package_name + "\n")

    print ("Wrote file: ", filename)

# ================================================================
# Generate declarations, either for type size or for value

def gen_decls (fout, size_not_val, items):
    fout.write ("\n" +
                "// Basic ----------------\n" +
                "\n")
    gen_decl (fout, size_not_val, "Bits_per_CWord", sel2 (items, "Bits_per_CWord"))
    gen_decl (fout, size_not_val, "Bytes_per_CWord", sel2 (items, "Bytes_per_CWord"))
    gen_decl (fout, size_not_val, "Bits_per_Byte_in_CWord", sel2 (items, "Bits_per_Byte_in_CWord"))

    fout.write ("\n" +
                "// Cache Lines ----------------\n" +
                "\n")
    gen_decl (fout, size_not_val, "CWords_per_CLine", sel2 (items, "CWords_per_CLine"))
    gen_decl (fout, size_not_val, "Bits_per_CWord_in_CLine", sel2 (items, "Bits_per_CWord_in_CLine"))
    fout.write ("\n")
    gen_decl (fout, size_not_val, "Bytes_per_CLine",   sel2 (items, "Bytes_per_CLine"))
    gen_decl (fout, size_not_val, "Bits_per_Byte_in_CLine",  sel2 (items, "Bits_per_Byte_in_CLine"))
    fout.write ("\n")
    gen_decl (fout, size_not_val, "Bits_per_CLine",    sel2 (items, "Bits_per_CLine"))

    fout.write ("\n" +
                "// Cache Sets ----------------\n" +
                "\n")
    gen_decl (fout, size_not_val, "Ways_per_CSet",  sel2 (items, "Ways_per_CSet"))
    gen_decl (fout, size_not_val, "Bits_per_Way_in_CSet", sel2 (items, "Bits_per_Way_in_CSet"))
    fout.write ("\n")
    gen_decl (fout, size_not_val, "Bytes_per_CSet", sel2 (items, "Bytes_per_CSet"))

    fout.write ("\n" +
                "// Cache ----------------\n" +
                "\n")
    gen_decl (fout, size_not_val, "KB_per_Cache",                 sel2 (items, "KB_per_Cache"))
    gen_decl (fout, size_not_val, "Bytes_per_Cache",              sel2 (items, "Bytes_per_Cache"))
    gen_decl (fout, size_not_val, "CWords_per_Cache",             sel2 (items, "CWords_per_Cache"))
    gen_decl (fout, size_not_val, "CLines_per_Cache",             sel2 (items, "CLines_per_Cache"))
    fout.write ("\n")
    gen_decl (fout, size_not_val, "CSets_per_Cache",               sel2 (items, "CSets_per_Cache"))
    gen_decl (fout, size_not_val, "Bits_per_CSet_in_Cache",        sel2 (items, "Bits_per_CSet_in_Cache"))
    fout.write ("\n")
    gen_decl (fout, size_not_val, "CSet_CWords_per_Cache",          sel2 (items, "CSet_CWords_per_Cache"))
    gen_decl (fout, size_not_val, "Bits_per_CSet_CWord_in_Cache",   sel2 (items, "Bits_per_CSet_CWord_in_Cache"))

    gen_decl (fout, size_not_val, "Bits_per_CTag",                 sel2 (items, "Bits_per_CTag"))

    if not size_not_val:
        fout.write ("\n" +
                    "// Addrs ----------------\n" +
                    "\n")
        gen_decl (fout, size_not_val, "addr_lo_cword_in_cline", sel2 (items, "addr_lo_cword_in_cline"))
        gen_decl (fout, size_not_val, "addr_hi_cword_in_cline", sel2 (items, "addr_hi_cword_in_cline"))
        fout.write ("\n")
        gen_decl (fout, size_not_val, "addr_lo_cset_in_cache", sel2 (items, "addr_lo_cset_in_cache"))
        gen_decl (fout, size_not_val, "addr_hi_cset_in_cache", sel2 (items, "addr_hi_cset_in_cache"))
        fout.write ("\n")
        gen_decl (fout, size_not_val, "addr_lo_cset_cword_in_cache", sel2 (items, "addr_lo_cset_cword_in_cache"))
        gen_decl (fout, size_not_val, "addr_hi_cset_cword_in_cache", sel2 (items, "addr_hi_cset_cword_in_cache"))
        fout.write ("\n")
        gen_decl (fout, size_not_val, "addr_lo_ctag", sel2 (items, "addr_lo_ctag"))

    return 0

# ================================================================
# Generate a single declaration

def gen_decl (fout, size_not_val, ide, val_and_formula):
    (val, formula) = val_and_formula
    if size_not_val:
        ide = str.upper (ide [0:1]) + ide [1:]
        fout.write ("typedef  {0:8d}   {1:s};    // {2}\n".format (val, ide, formula))
    else:
        ide = str.lower (ide)
        fout.write ("Integer  {0:>28s} = {1:8d};    // {2}\n".format (ide, val, formula))

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main (sys.argv))
