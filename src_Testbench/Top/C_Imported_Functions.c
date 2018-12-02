// Copyright (c) 2013-2018 Bluespec, Inc.  All Rights Reserved

// ================================================================
// These are functions imported into BSV during Bluesim or Verilog simulation.
// See C_Imports.bsv for the 'import BDPI' declarations.
// ================================================================

// ================================================================
// Includes from C library

#include <sys/types.h>        /*  socket types              */
#include <poll.h>

#include <unistd.h>           /*  misc. UNIX functions      */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>

// ================================================================
// Includes for this project

#include "C_Imported_Functions.h"

// ================================================================
// c_trygetchar()
// Returns next input character (ASCII code) from the console.
// Returns 0 if no input is available.

// The dummy arg is not used, and is present only to appease Verilog
// simulators which can be be finicky about zero-argument functions.

uint8_t c_trygetchar (uint8_t  dummy)
{
    uint8_t  ch;
    ssize_t  n;
    struct pollfd  x_pollfd;
    const int fd_stdin = 0;

    // ----------------
    // Poll for input
    x_pollfd.fd      = fd_stdin;
    x_pollfd.events  = POLLRDNORM;
    x_pollfd.revents = 0;
    poll (& x_pollfd, 1, 1);

    // printf ("INFO: c_trygetchar: Polling for input\n");
    if ((x_pollfd.revents & POLLRDNORM) == 0) {
	return 0;
    }

    // ----------------
    // Input is available

    n = read (fd_stdin, & ch, 1);
    if (n == 1) {
	return ch;
    }
    else {
	if (n == 0)
	    printf ("c_trygetchar: end of file\n");
	return 0xFF;
    }
}

// ----------------------------------------------------------------
// A small testbench for c_trygetchar

#ifdef TEST_TRYGETCHAR

char message[] = "Hello World!\n";

int main (int argc, char *argv [])
{
    uint8_t ch;
    int j;

    for (j = 0; j < strlen (message); j++)
	c_putchar (message[j]);

    printf ("Polling for input\n");

    j = 0;
    while (1) {
	ch = c_trygetchar ();
	if (ch == 0xFF) break;
	if (ch != 0)
	    printf ("Received character %0d 0x%0x '%c'\n", ch, ch, ch);
	else {
	    printf ("\r%0d ", j);
	    fflush (stdout);
	    j++;
	    sleep (1);
	}
    }
    return 0;
}

#endif

// ================================================================
// Writes character to stdout

uint32_t c_putchar (uint8_t ch)
{
    int      status;
    uint32_t success = 0;

    if ((ch == 0) || (ch > 0x7F)) {
	// Discard non-printables
	success = 1;
    }
    else {
	if ((ch == '\n') || (' ' <= ch)) {
	    status = fprintf (stdout, "%c", ch);
	    if (status > 0)
		success = 1;
	}
	else {
	    status = fprintf (stdout, "[\\%0d]", ch);
	    if (status > 0)
		success = 1;
	}

	if (success == 1) {
	    status = fflush (stdout);
	    if (status != 0)
		success = 0;
	}
    }

    return success;
}

// ================================================================
// Misc. parameters from a parameters file, if it exists.
// Each non-empty line of the file should contain
//     param-name  param-value  comment
// or  # comment

/*
#define MAX_PARAMS  128

char   sim_params_filename[] = "sim_params.txt";
static bool      params_loaded = false;
static uint64_t  param_vals [MAX_PARAMS];

#define PARAM_LINEBUF_SIZE 1024

static
void load_params (void)
{
    for (uint32_t j = 0; j < MAX_PARAMS; j++)
	param_vals [j] = 0;

    FILE *fp = fopen ("sim_params.txt", "r");
    if (fp == NULL) {
	fprintf (stdout, "INFO: no simulation parameters file: %s\n", sim_params_filename);
	return;
    }
    else {
	fprintf (stdout, "INFO: loading simulation parameters from file: %s\n", sim_params_filename);
    }

    while (true) {
	char     linebuf [PARAM_LINEBUF_SIZE], *p;

	p = fgets (linebuf, PARAM_LINEBUF_SIZE, fp);
	if (p == NULL)    // EOF
	    break;

	// Skip leading blanks and tabs
	int index = 0;
	while ((index < PARAM_LINEBUF_SIZE)
	       && ((linebuf [index] == ' ') || (linebuf [index] == '\t')))
	    index++;
	if (index == PARAM_LINEBUF_SIZE)
	    break;

	if (linebuf [index] == '#')  // comment line
	    continue;

	// Read the parameter code
	uint32_t param;
	int n = sscanf (& linebuf [index], "%" SCNd32 "%n", & param, & index);
	if ((n == EOF) || (n < 1))
	    continue;

	// Read the parameter value (decimal or hex, depending on the parameter)
	uint64_t param_val;
	if (param == sim_param_watch_tohost) {
	    int n = sscanf (& (linebuf [index]), "%" SCNd64, & param_val);
	}
	else if (param == sim_param_tohost_addr) {
	    int n = sscanf (& (linebuf [index]), "%" SCNx64, & param_val);
	}
	else {
	    int n = sscanf (& (linebuf [index]), "%" SCNd64, & param_val);
	}

	if ((n == EOF) || (n < 1))
	    continue;
	else if (param >= MAX_PARAMS) {
	    fprintf (stderr, "INFO: ignoring out-of-bounds parameter code %0" PRId32 " (should be < %0d)\n",
		     param, MAX_PARAMS);
	    fprintf (stderr, "    Value (decimal) = %0" PRId64, param_val);
	    fprintf (stderr, "    Value (hex)     = 0x%0" PRIx64, param_val);
	}
	else {
	    param_vals [param] = param_val;
	}
    }
    fclose (fp);
}

uint64_t c_get_param_val (uint32_t  param)
{
    if (! params_loaded) {
	load_params ();
	params_loaded = true;
    }

    uint64_t param_val = 0;
    if (param < MAX_PARAMS)
	param_val = param_vals [param];
    return param_val;
}

*/

// ================================================================
// Symbol table
// Reads the whole symbol-table file on each call,
// which is ok if it's not called often and the file is small.

static
char symbol_table_filename [] = "symbol_table.txt";

uint64_t c_get_symbol_val (char * symbol)
{
    bool     ok  = false;
    uint64_t val = 0;

    FILE *fp = fopen (symbol_table_filename, "r");
    if (fp == NULL) {
	fprintf (stderr, "c_get_symbol: could not open file: %s\n", symbol_table_filename);
    }
    else {
	int len     = strlen (symbol);
	int linenum = 0;

	while (true) {
	    char linebuf [1024], *p;
	    p = fgets (linebuf, 1024, fp);
	    if (p == NULL) {   // EOF
		fprintf (stderr, "c_get_symbol: could not find value for symbol %s\n", symbol);
		break;
	    }
	    else {
		linenum++;

		int cmp = strncmp (symbol, linebuf, len);
		if (cmp != 0)    // This symbol is not on this line
		    continue;
		else {
		    int n = sscanf (& (linebuf [len]), "%" SCNx64, & val);
		    if ((n == EOF) || (n < 1)) {
			fprintf (stderr, "c_get_symbol: could not find value for symbol %s\n", symbol);
			fprintf (stderr, "    on file '%s' line %d\n", symbol_table_filename, linenum);
		    }
		    else
			ok = true;
		    break;
		}
	    }
	}
    }
    return val;
}

// ================================================================
// Trace file output

static char trace_file_name[] = "trace_data.dat";

static FILE *trace_file_stream;

static uint64_t trace_file_size   = 0;
static uint64_t trace_file_writes = 0;

#define BUFSIZE 1024
static uint8_t buf [BUFSIZE];

// ----------------
// import "BDPI"
// function Action c_trace_file_open (Bit #(8) dummy);

uint32_t c_trace_file_open (uint8_t dummy)
{
    uint32_t success = 0;

    trace_file_stream = fopen ("trace_out.dat", "w");
    if (trace_file_stream == NULL) {
	fprintf (stderr, "ERROR: c_trace_file_open: unable to open file '%s'.\n", trace_file_name);
	success = 0;
    }
    else {
	fprintf (stdout, "c_trace_file_stream: opened file '%s' for trace_data.\n", trace_file_name);
	success = 1;
    }
    return success;
}

// ----------------
// import "BDPI"
// function Action c_trace_file_load_byte_in_buffer (Bit #(32) j, Bit #(8) data);

uint32_t c_trace_file_load_byte_in_buffer (uint32_t j, uint8_t data)
{
    uint32_t success = 0;

    if (j >= BUFSIZE) {
	fprintf (stderr, "ERROR: c_trace_file_load_byte_in_buffer: index (%0d) out of bounds (%0d)\n",
		 j, BUFSIZE);
	success = 0;
    }
    else {
	buf [j] = data;
	success = 1;
    }
    return success;
}

// ----------------
// import "BDPI"
// function Action c_trace_file_load_word64_in_buffer (Bit #(32) byte_offset, Bit #(64) data);

uint32_t c_trace_file_load_word64_in_buffer (uint32_t byte_offset, uint64_t data)
{
    uint32_t success = 0;

    if ((byte_offset + 7) >= BUFSIZE) {
	fprintf (stderr, "ERROR: c_trace_file_load_word64_in_buffer: index (%0d) out of bounds (%0d)\n",
		 byte_offset, BUFSIZE);
	success = 0;
    }
    else {
	uint64_t *p = (uint64_t *) & (buf [byte_offset]);
	*p = data;
	success = 1;
    }
    return success;
}

// ----------------
// import "BDPI"
// function Action c_trace_file_write_buffer (Bit #(32)  n);

uint32_t c_trace_file_write_buffer (uint32_t n)
{
    uint32_t success = 0;

    size_t n_written = fwrite (buf, 1, n, trace_file_stream);
    if (n_written != n)
	success = 0;
    else {
	trace_file_size   += n;
	trace_file_writes += 1;
	success = 1;
    }
    return success;
}

// ----------------
// import "BDPI"
// function Action c_trace_file_close (Bit #(8) dummy);

uint32_t c_trace_file_close (uint8_t dummy)
{
    uint32_t success = 0;
    int      status;

    if (trace_file_stream == NULL)
	success = 1;
    else {
	status = fclose (trace_file_stream);
	if (status != 0) {
	    fprintf (stderr, "ERROR: c_trace_file_close: error in fclose()\n");
	    success = 0;
	}
	else {
	    fprintf (stdout, "c_trace_file_stream: closed file '%s' for trace_data.\n", trace_file_name);
	    fprintf (stdout, "    Trace file writes: %0" PRId64 "\n", trace_file_writes);
	    fprintf (stdout, "    Trace file size:   %0" PRId64 " bytes\n", trace_file_size);
	    success = 1;
	}
    }
    return success;
}

// ================================================================
// DEBUGGING

/*
int main (int argc, char *argv [])
{
    uint64_t x = c_get_symbol_val ("tohost");
    fprintf (stdout, "tohost value is 0x%" PRIx64 "\n", x);
}
*/

// ================================================================
