// Copyright (c) 2013-2018 Bluespec, Inc.  All Rights Reserved

// ================================================================
// These are functions imported into BSV for terminal I/O during
// Bluesim or Verilog simulation.
// See ConsoleIO.bsv for the import declarations.
// ================================================================

#include <sys/types.h>        /*  socket types              */
#include <poll.h>

#include <unistd.h>           /*  misc. UNIX functions      */
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "C_ConsoleIO_functions.h"

// ================================================================
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

// ================================================================
// Writes character to stdout

void c_putchar (uint8_t ch)
{
    if ((ch == 0) || (ch > 0x7F)) return;

    if ((ch == '\n') || (' ' <= ch)) {
	fprintf (stdout, "%c", ch);
    }
    else {
	fprintf (stdout, "[\\%0d]", ch);
    }
    fflush (stdout);
}

// ================================================================
// A small testbench

#ifdef TESTMAIN

char message[] = "Hello World!\n";

int main (int argc, char *argv)
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
