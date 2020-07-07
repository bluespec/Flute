This directory contains a Python program 'Gen_BSV_Cache_Decls.py'

Inputs:
    Top-level specs for a cache, such as size, associativity etc.
    Inputs are givein in a YAML file, and can be over-ridden on the command-line.
    Run    '$ Gen_BSV_Cache_Decls.py --help'    for all options.

Output:

    Generates a BSV package with numerous type, value and function
    declarations derived from the top-level specs, and imported into
    BSV code for the cache implementations.
