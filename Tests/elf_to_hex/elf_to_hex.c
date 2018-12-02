// Copyright (c) 2013-2018 Bluespec, Inc. All Rights Reserved

// This program reads an ELF file and outputs a Verilog hex memory
// image file (suitable for reading using $readmemh).

// ================================================================
// Standard C includes

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <fcntl.h>
#include <gelf.h>

// ================================================================
// Memory buffer into which we load the ELF file before
// writing it back out to the output file.

// 1 Gigabyte size
// #define MAX_MEM_SIZE (((uint64_t) 0x400) * ((uint64_t) 0x400) * ((uint64_t) 0x400))
#define MAX_MEM_SIZE ((uint64_t) 0x90000000)

uint8_t mem_buf [MAX_MEM_SIZE];

// Features of the ELF binary
int       bitwidth;
uint64_t  min_addr;
uint64_t  max_addr;

uint64_t  pc_start;       // Addr of label  '_start'
uint64_t  pc_exit;        // Addr of label  'exit'
uint64_t  tohost_addr;    // Addr of label  'tohost'

// ================================================================
// Load an ELF file.

void c_mem_load_elf (char *elf_filename,
		     const char *start_symbol,
		     const char *exit_symbol,
		     const char *tohost_symbol)
{
    int fd;
    // int n_initialized = 0;
    Elf *e;

    // Default start, exit and tohost symbols
    if (start_symbol == NULL)
	start_symbol = "_start";
    if (exit_symbol == NULL)
	exit_symbol = "exit";
    if (tohost_symbol == NULL)
	tohost_symbol = "tohost";
    
    // Verify the elf library version
    if (elf_version (EV_CURRENT) == EV_NONE) {
        fprintf (stderr, "ERROR: c_mem_load_elf: Failed to initialize the libelfg library!\n");
	exit (1);
    }

    // Open the file for reading
    fd = open (elf_filename, O_RDONLY, 0);
    if (fd < 0) {
        fprintf (stderr, "ERROR: c_mem_load_elf: could not open elf input file: %s\n", elf_filename);
	exit (1);
    }

    // Initialize the Elf pointer with the open file
    e = elf_begin (fd, ELF_C_READ, NULL);
    if (e == NULL) {
        fprintf (stderr, "ERROR: c_mem_load_elf: elf_begin() initialization failed!\n");
	exit (1);
    }

    // Verify that the file is an ELF file
    if (elf_kind (e) != ELF_K_ELF) {
        elf_end (e);
        fprintf (stderr, "ERROR: c_mem_load_elf: specified file '%s' is not an ELF file!\n", elf_filename);
	exit (1);
    }

    // Get the ELF header
    GElf_Ehdr ehdr;
    if (gelf_getehdr (e, & ehdr) == NULL) {
        elf_end (e);
        fprintf (stderr, "ERROR: c_mem_load_elf: get_getehdr() failed: %s\n", elf_errmsg(-1));
	exit (1);
    }

    // Is this a 32b or 64 ELF?
    if (gelf_getclass (e) == ELFCLASS32) {
	fprintf (stdout, "c_mem_load_elf: %s is a 32-bit ELF file\n", elf_filename);
	bitwidth = 32;
    }
    else if (gelf_getclass (e) == ELFCLASS64) {
	fprintf (stdout, "c_mem_load_elf: %s is a 64-bit ELF file\n", elf_filename);
	bitwidth = 64;
    }
    else {
        fprintf (stderr, "ERROR: c_mem_load_elf: ELF file '%s' is not 32b or 64b\n", elf_filename);
	elf_end (e);
	exit (1);
    }

    // Verify we are dealing with a RISC-V ELF
    if (ehdr.e_machine != 243) { // EM_RISCV is not defined, but this returns 243 when used with a valid elf file.
        elf_end (e);
        fprintf (stderr, "ERROR: c_mem_load_elf: %s is not a RISC-V ELF file\n", elf_filename);
	exit (1);
    }

    // Verify we are dealing with a little endian ELF
    if (ehdr.e_ident[EI_DATA] != ELFDATA2LSB) {
        elf_end (e);
        fprintf (stderr,
		 "ERROR: c_mem_load_elf: %s is a big-endian 64-bit RISC-V executable which is not supported\n",
		 elf_filename);
	exit (1);
    }

    // Grab the string section index
    size_t shstrndx;
    shstrndx = ehdr.e_shstrndx;

    // Iterate through each of the sections looking for code that should be loaded
    Elf_Scn  *scn   = 0;
    GElf_Shdr shdr;

    min_addr    = 0xFFFFFFFFFFFFFFFFllu;
    max_addr    = 0x0000000000000000llu;
    pc_start    = 0xFFFFFFFFFFFFFFFFllu;
    pc_exit     = 0xFFFFFFFFFFFFFFFFllu;
    tohost_addr = 0xFFFFFFFFFFFFFFFFllu;

    while ((scn = elf_nextscn (e,scn)) != NULL) {
        // get the header information for this section
        gelf_getshdr (scn, & shdr);

	char *sec_name = elf_strptr (e, shstrndx, shdr.sh_name);
	fprintf (stdout, "Section %-16s: ", sec_name);

	Elf_Data *data = 0;
	// If we find a code/data section, load it into the model
	if (   ((shdr.sh_type == SHT_PROGBITS)
		|| (shdr.sh_type == SHT_NOBITS)
		|| (shdr.sh_type == SHT_INIT_ARRAY)
		|| (shdr.sh_type == SHT_FINI_ARRAY))
	    && ((shdr.sh_flags & SHF_WRITE)
		|| (shdr.sh_flags & SHF_ALLOC)
		|| (shdr.sh_flags & SHF_EXECINSTR))) {
	    data = elf_getdata (scn, data);

	    // n_initialized += data->d_size;
	    if (shdr.sh_addr < min_addr)
		min_addr = shdr.sh_addr;
	    if (max_addr < (shdr.sh_addr + data->d_size - 1))   // shdr.sh_size + 4))
		max_addr = shdr.sh_addr + data->d_size - 1;    // shdr.sh_size + 4;

	    if (max_addr >= MAX_MEM_SIZE) {
		fprintf (stdout, "INTERNAL ERROR: max_addr (0x%0" PRIx64 ") > buffer size (0x%0" PRIx64 ")\n",
			 max_addr, MAX_MEM_SIZE);
		fprintf (stdout, "    Please increase the #define in this program, recompile, and run again\n");
		fprintf (stdout, "    Abandoning this run\n");
		exit (1);
	    }

	    if (shdr.sh_type != SHT_NOBITS) {
		memcpy (& (mem_buf [shdr.sh_addr]), data->d_buf, data->d_size);
	    }
	    fprintf (stdout, "addr %16" PRIx64 " to addr %16" PRIx64 "; size 0x%8lx (= %0ld) bytes\n",
		     shdr.sh_addr, shdr.sh_addr + data->d_size, data->d_size, data->d_size);

	}

	// If we find the symbol table, search for symbols of interest
	else if (shdr.sh_type == SHT_SYMTAB) {
	    fprintf (stdout, "Searching for addresses of '%s', '%s' and '%s' symbols\n",
		     start_symbol, exit_symbol, tohost_symbol);

 	    // Get the section data
	    data = elf_getdata (scn, data);

	    // Get the number of symbols in this section
	    int symbols = shdr.sh_size / shdr.sh_entsize;

	    // search for the uart_default symbols we need to potentially modify.
	    GElf_Sym sym;
	    int i;
	    for (i = 0; i < symbols; ++i) {
	        // get the symbol data
	        gelf_getsym (data, i, &sym);

		// get the name of the symbol
		char *name = elf_strptr (e, shdr.sh_link, sym.st_name);

		// Look for, and remember PC of the start symbol
		if (strcmp (name, start_symbol) == 0) {
		    pc_start = sym.st_value;
		}
		// Look for, and remember PC of the exit symbol
		else if (strcmp (name, exit_symbol) == 0) {
		    pc_exit = sym.st_value;
		}
		// Look for, and remember addr of 'tohost' symbol
		else if (strcmp (name, tohost_symbol) == 0) {
		    tohost_addr = sym.st_value;
		}
	    }

	    FILE *fp_symbol_table = fopen ("symbol_table.txt", "w");
	    if (fp_symbol_table != NULL) {
		fprintf (stdout, "Writing symbols to:    symbol_table.txt\n");
		if (pc_start == -1)
		    fprintf (stdout, "    No '_start' label found\n");
		else
		    fprintf (fp_symbol_table, "_start    0x%0" PRIx64 "\n", pc_start);

		if (pc_exit == -1)
		    fprintf (stdout, "    No 'exit' label found\n");
		else
		    fprintf (fp_symbol_table, "exit      0x%0" PRIx64 "\n", pc_exit);

		if (tohost_addr == -1)
		    fprintf (stdout, "    No 'tohost' symbol found\n");
		else
		    fprintf (fp_symbol_table, "tohost    0x%0" PRIx64 "\n", tohost_addr);

		fclose (fp_symbol_table);
	    }
	}
	else {
	    fprintf (stdout, "Ignored\n");
	}
    }

    elf_end (e);

    fprintf (stdout, "Min addr:            %16" PRIx64 " (hex)\n", min_addr);
    fprintf (stdout, "Max addr:            %16" PRIx64 " (hex)\n", max_addr);
}

// ================================================================
// Min and max byte addrs for various mem sizes

#define BASE_ADDR_B  0x80000000lu

// For 16 MB memory at 0x_8000_0000
#define MIN_MEM_ADDR_16MB  BASE_ADDR_B
#define MAX_MEM_ADDR_16MB  (BASE_ADDR_B + 0x1000000lu)

// For 256 MB memory at 0x_8000_0000
#define MIN_MEM_ADDR_256MB  BASE_ADDR_B
#define MAX_MEM_ADDR_256MB  (BASE_ADDR_B + 0x10000000lu)

// ================================================================

// Write out from word containing addr1 to word containing addr2
void write_mem_hex_file (FILE *fp, uint64_t addr1, uint64_t addr2)
{
    const uint64_t bits_per_raw_mem_word   = 256;
    uint64_t bytes_per_raw_mem_word  = bits_per_raw_mem_word / 8;    // 32
    uint64_t raw_mem_word_align_mask = (~ ((uint64_t) (bytes_per_raw_mem_word - 1)));

    fprintf (stdout, "Subtracting 0x%08" PRIx64 " base from addresses\n", BASE_ADDR_B);

    // Align the start and end addrs to raw mem words
    uint64_t a1 = (addr1 & raw_mem_word_align_mask);
    uint64_t a2 = ((addr2 + bytes_per_raw_mem_word - 1) & raw_mem_word_align_mask);

    fprintf (fp, "@%07" PRIx64 "    // raw_mem addr;  byte addr: %08" PRIx64 "\n",
	     ((a1 - BASE_ADDR_B) / bytes_per_raw_mem_word),
	     a1 - BASE_ADDR_B);
	     
    uint64_t addr;
    for (addr = a1; addr < a2; addr += bytes_per_raw_mem_word) {
	for (int j = (bytes_per_raw_mem_word - 1); j >= 0; j--)
	    fprintf (fp, "%02x", mem_buf [addr+j]);
	fprintf (fp, "    // raw_mem addr %08" PRIx64 ";  byte addr %08" PRIx64 "\n",
		 ((addr - BASE_ADDR_B) / bytes_per_raw_mem_word),
		 (addr  - BASE_ADDR_B));
    }

    // Write last word, if necessary, to avoid warnings about missing locations
    if (addr < (MAX_MEM_ADDR_256MB - bytes_per_raw_mem_word)) {
	addr = MAX_MEM_ADDR_256MB - bytes_per_raw_mem_word;
	fprintf (fp, "@%07" PRIx64 "    // last raw_mem addr;  byte addr: %08" PRIx64 "\n",
		 ((addr - BASE_ADDR_B) / bytes_per_raw_mem_word),
		 addr - BASE_ADDR_B);
	for (int j = (bytes_per_raw_mem_word - 1); j >= 0; j--)
	    fprintf (fp, "%02x", 0);
	fprintf (fp, "    // raw_mem addr %08" PRIx64 ";  byte addr %08" PRIx64 "\n",
		 ((addr - BASE_ADDR_B) / bytes_per_raw_mem_word),
		 (addr  - BASE_ADDR_B));
    }
}

// ================================================================

void print_usage (FILE *fp, int argc, char *argv [])
{
    fprintf (fp, "Usage:\n");
    fprintf (fp, "    %s  --help\n", argv [0]);
    fprintf (fp, "    %s  <ELF filename>  <mem hex filename>\n", argv [0]);
    fprintf (fp, "Reads ELF file and writes a Verilog Hex Memory image file\n");
    fprintf (fp, "ELF file should have addresses within this range:\n");
    fprintf (fp, "<  Max: 0x%8" PRIx64 "\n", MAX_MEM_ADDR_256MB);
    fprintf (fp, ">= Min: 0x%8" PRIx64 "\n", MIN_MEM_ADDR_256MB);
}

// ================================================================

int main (int argc, char *argv [])
{
    if ((argc == 2) && (strcmp (argv [1], "--help") == 0)) {
	print_usage (stdout, argc, argv);
	return 0;
    }
    else if (argc != 3) {
	print_usage (stderr, argc, argv);
	return 1;
    }

    // Zero out the memory buffer before loading the ELF file
    bzero (mem_buf, MAX_MEM_SIZE);
    // bzero (& (mem_buf [BASE_ADDR_B]), MAX_MEM_SIZE - BASE_ADDR_B);

    c_mem_load_elf (argv [1], "_start", "exit", "tohost");

    if ((min_addr < BASE_ADDR_B) || (MAX_MEM_ADDR_256MB <= max_addr)) {
	print_usage (stderr, argc, argv);
	exit (1);
    }

    FILE *fp_out = fopen (argv [2], "w");
    if (fp_out == NULL) {
	fprintf (stderr, "ERROR: unable to open file '%s' for output\n", argv [2]);
	return 1;
    }

    fprintf (stdout, "Writing mem hex to file '%s'\n", argv [2]);
    write_mem_hex_file (fp_out, BASE_ADDR_B, max_addr);
    // write_mem_hex_file (fp_out, BASE_ADDR_B, MAX_MEM_ADDR_256MB);

    fclose (fp_out);
}
