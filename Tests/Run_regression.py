#!/usr/bin/python3

# Copyright (c) 2018-2019 Bluespec, Inc.
# See LICENSE for license details

usage_line = (
    "  Usage:\n"
    "    $ <this_prog>    <simulation_executable>  <repo_dir>  <logs_dir>  <arch>  <opt verbosity>\n"
    "\n"
    "  Runs the RISC-V <simulation_executable>\n"
    "  on ISA tests: ELF files taken from <repo-dir>/isa and its sub-directories.\n"
    "\n"
    "  Runs it only on those ELF files that are relevant to architecture <arch>.\n"
    "\n"
    "  For each ELF file FOO, saves simulation output in <logs_dir>/FOO.log. \n"
    "\n"
    "  If <opt verbosity> is given, it must be one of the following:\n"
    "      v1:    Print instruction trace during simulation\n"
    "      v2:    Print pipeline stage state during simulation\n"
    "\n"
    "  Example:\n"
    "      $ <this_prog>  .exe_HW_sim  ~somebody/GitHub/Piccolo  ./Logs  RV32IMU  v1\n"
    "    will run the verilator simulation executable on the following RISC-V ISA tests:\n"
    "            ~somebody/GitHub/Tests/isa/rv32ui-p*\n"
    "            ~somebody/GitHub/Tests/isa/rv32mi-p*\n"
    "            ~somebody/GitHub/Tests/isa/rv32um-p*\n"
    "    which are relevant for architecture RV32IMU\n"
    "    and will leave a transcript of each test's simulation output in files like\n"
    "            ./Logs/rv32ui-p-add.log\n"
    "    Each log will contain an instruction trace (because of the 'v1' arg).\n"
)

import sys
import os
import stat
import subprocess

# ================================================================

num_executed = 0
num_passed   = 0

# ================================================================

def main (argv = None):
    print ("Use flag --help  or --h for a help message")
    if ((len (argv) <= 1) or
        (argv [1] == '-h') or (argv [1] == '--help') or
        (len (argv) < 5)):

        sys.stdout.write (usage_line)
        sys.stdout.write ("\n")
        return 0

    # Simulation executable
    if not (os.path.exists (argv [1])):
        sys.stderr.write ("ERROR: The given simulation path does not seem to exist?\n")
        sys.stderr.write ("    Simulation path: " + sim_path + "\n")
        sys.exit (1)
    args_dict = {'sim_path': os.path.abspath (os.path.normpath (argv [1]))}

    # Repo in which to find ELFs and elf_to_hex executable
    if (not os.path.exists (argv [2])):
        sys.stderr.write ("ERROR: repo directory ({0}) does not exist?\n".format (argv [2]))
        sys.stdout.write ("\n")
        sys.stdout.write (usage_line)
        sys.stdout.write ("\n")
        return 1
    repo = os.path.abspath (os.path.normpath (argv [2]))

    elfs_path = os.path.join (repo, "Tests", "isa")
    if (not os.path.exists (elfs_path)):
        sys.stderr.write ("ERROR: ELFs directory ({0}) does not exist?\n".format (elfs_path))
        sys.stdout.write ("\n")
        sys.stdout.write (usage_line)
        sys.stdout.write ("\n")
        return 1
    args_dict ['elfs_path'] = elfs_path

    # Logs directory
    logs_path = os.path.abspath (os.path.normpath (argv [3]))
    if not (os.path.exists (logs_path) and os.path.isdir (logs_path)):
        print ("Creating dir: " + logs_path)
        os.mkdir (logs_path)
    args_dict ['logs_path'] = logs_path

    # Architecture string and implied ISA test families
    arch_string = extract_arch_string (argv [4])
    if (arch_string == None):
        sys.stderr.write ("ERROR: no architecture specified?\n")
        sys.stdout.write ("\n")
        sys.stdout.write (usage_line)
        sys.stdout.write ("\n")
        return 1
    args_dict ['arch_string'] = arch_string

    test_families = select_test_families (arch_string)
    print ("Testing the following families of ISA tests")
    for tf in test_families:
        print ("    " + tf)
    args_dict ['test_families'] = test_families

    # Optional verbosity
    verbosity = 0
    for arg in argv [5:]:
         if arg == "v1":
            verbosity = 1
         elif arg == "v2":
            verbosity = 2
    args_dict ['verbosity'] = verbosity

    # elf_to_hex executable
    elf_to_hex_exe = os.path.join (repo, "Tests", "elf_to_hex", "elf_to_hex")
    if (not os.path.exists (elf_to_hex_exe)):
        sys.stderr.write ("ERROR: elf_to_hex executable does not exist?\n")
        sys.stderr.write ("    at {0}\n".format (elf_to_hex_exe))
        sys.stdout.write ("\n")
        sys.stdout.write (usage_line)
        sys.stdout.write ("\n")
        return 1
    args_dict ['elf_to_hex_exe'] = elf_to_hex_exe

    sys.stdout.write ("Parameters:\n")
    for key in iter (args_dict):
        sys.stdout.write ("    {0:<16}: {1}\n".format (key, args_dict [key]))

    # Perform the recursive traversal
    max_level = 20
    traverse (max_level, 0, args_dict)

    # Write final statistics
    sys.stdout.write ("Executed: {0} tests\n".format (num_executed))
    sys.stdout.write ("PASS:     {0} tests\n".format (num_passed))


# ================================================================
# Extract the architecture string (e.g., RV64AIMSU) from the string s

def extract_arch_string (s):
    s1     = s.upper()
    j_rv32 = s1.find ("RV32")
    j_rv64 = s1.find ("RV64")

    if (j_rv32 >= 0):
        j = j_rv32
    elif (j_rv64 >= 0):
        j = j_rv64
    else:
        sys.stderr.write ("ERROR: cannot find architecture string beginning with RV32 or RV64 in: \n")
        sys.stderr.write ("    '" + s + "'\n")
        sys.exit (1)

    k = j + 4
    rv = s1 [j:k]

    extns = ""
    while (k < len (s)):
        ch = s [k]
        if (ch < "A") or (ch > "Z"): break
        extns = extns + s [k]
        k     = k + 1

    arch = rv + extns
    return arch

# ================================================================
# Select ISA test families based on provided arch string

def select_test_families (arch):
    arch = arch.lower ()
    families = []

    if arch.find ("32") != -1:
        rv = 32
        families = ["rv32ui-p", "rv32mi-p"]
    else:
        rv = 64
        families = ["rv64ui-p", "rv64mi-p"]

    if (arch.find ("s") != -1):
        s = True
        if rv == 32:
            families.extend (["rv32ui-v", "rv32si-p"])
        else:
            families.extend (["rv64ui-v", "rv64si-p"])
    else:
        s = False

    def add_family (extension):
        if (arch.find (extension) != -1):
            if rv == 32:
                families.append ("rv32u" + extension + "-p")
                if s:
                    families.append ("rv32u" + extension + "-v")
            else:
                families.append ("rv64u" + extension + "-p")
                if s:
                    families.append ("rv64u" + extension + "-v")

    add_family ("m")
    add_family ("a")
    add_family ("f")
    add_family ("d")
    add_family ("c")

    return families

# ================================================================
# Recursively traverse the dir tree below elf_path and process each file

def traverse (max_level, level, args_dict):
    elfs_path = args_dict ['elfs_path']
    st = os.stat (elfs_path)
    is_dir = stat.S_ISDIR (st.st_mode)
    is_regular = stat.S_ISREG (st.st_mode)
    do_foreachfile_function (level, is_dir, is_regular, args_dict)
    if is_dir and level < max_level:
        for entry in os.listdir (elfs_path):
            elfs_path1 = os.path.join (elfs_path, entry)
            args_dict1 = args_dict.copy ()
            args_dict1 ['elfs_path'] = elfs_path1
            traverse (max_level, level + 1, args_dict1)
    return 0

# ================================================================
# This function is applied to every path in the
# recursive traversal

def do_foreachfile_function (level, is_dir, is_regular, args_dict):
    prefix = ""
    for j in range (level): prefix = "  " + prefix
    elfs_path = args_dict ['elfs_path']

    # directories
    if is_dir:
        print ("%s%d dir %s" % (prefix, level, elfs_path))

    # regular files
    elif is_regular:
        dirname  = os.path.dirname (elfs_path)
        basename = os.path.basename (elfs_path)
        # print ("%s%d %s" % (prefix, level, elfs_path))
        do_regular_file_function (level, dirname, basename, args_dict)

    # other files
    else:
        print ("%s%d Unknown file type: %s" % (prefix, level, os.path.basename (elfs_path)))

# ================================================================
# For each ELF file, execute it in the RISC-V simulator

def do_regular_file_function (level, dirname, basename, args_dict):
    global num_executed
    global num_passed

    full_filename = os.path.join (dirname, basename)

    # Ignore filename if has any extension (heuristic that it's not an ELF file)
    if "." in basename: return

    # Ignore filename if does not match test_families
    ignore = True
    for x in args_dict ['test_families']:
        if basename.find (x) != -1: ignore = False
    if ignore:
        # print ("Ignoring file: " + full_filename)
        return

    # TEMPORARY FILTER WHILE DEBUGGING:
    # if basename.find ("rv64ui-v-add") == -1: return
    # sys.stdout.write ("WARNING: TEMPORARY FILTER IN EFFECT; REMOVE AFTER DEBUGGING\n")

    # For debugging only
    # prefix = ""
    # for j in range (level): prefix = "  " + prefix
    # sys.stdout.write ("{0}{1} ACTION:    {2}\n".format (prefix, level, full_filename))

    # Construct the commands for sub-process execution
    command1 = [args_dict ['elf_to_hex_exe'], full_filename, "Mem.hex"]

    command2 = [args_dict ['sim_path'],  "+tohost"]
    if (args_dict ['verbosity'] == 1): command2.append ("+v1")
    elif (args_dict ['verbosity'] == 2): command2.append ("+v2")

    num_executed = num_executed + 1
    sys.stdout.write ("Test {0}: {1}\n".format (num_executed, basename))

    sys.stdout.write ("    Exec:")
    for x in command1:
        sys.stdout.write (" {0}".format (x))
    sys.stdout.write ("\n")

    sys.stdout.write ("    Exec:")
    for x in command2:
        sys.stdout.write (" {0}".format (x))
    sys.stdout.write ("\n")

    # Run command as a sub-process
    completed_process1 = run_command (command1)
    completed_process2 = run_command (command2)
    passed = completed_process2.stdout.find ("PASS") != -1
    if passed:
        sys.stdout.write ("    PASS")
        num_passed = num_passed + 1
    else:
        sys.stdout.write ("    FAIL")

    # Save stdouts in log file
    log_filename = os.path.join (args_dict ['logs_path'], basename + ".log")
    sys.stdout.write ("    Writing log: {0}\n".format (log_filename))

    fd = open (log_filename, 'w')
    fd.write (completed_process1.stdout)
    fd.write (completed_process2.stdout)
    fd.close ()

    # If Tandem Verification trace file was created, save it as well
    if os.path.exists ("./trace_out.dat"):
        trace_filename = os.path.join (args_dict ['logs_path'], basename + ".trace_data")
        os.rename ("./trace_out.dat", trace_filename)
        sys.stdout.write ("    Trace output saved in: {0}\n".format (trace_filename))

    return

# ================================================================
# This is a wrapper around 'subprocess.run' because of an annoying
# incompatible change in moving from Python 3.5 to 3.6

def run_command (command):
    python_minor_version = sys.version_info [1]
    if python_minor_version < 6:
        # Python 3.5 and earlier
        result = subprocess.run (args = command,
                                 bufsize = 0,
                                 stdout = subprocess.PIPE,
                                 stderr = subprocess.STDOUT,
                                 universal_newlines = True)
    else:
        # Python 3.6 and later
        result = subprocess.run (args = command,
                                 bufsize = 0,
                                 stdout = subprocess.PIPE,
                                 stderr = subprocess.STDOUT,
                                 encoding='utf-8')
    return result

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main (sys.argv))
