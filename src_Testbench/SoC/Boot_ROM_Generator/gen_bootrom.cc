// Copyright (c) 2010-2019, The Regents of the University of California
// (Regents).  All Rights Reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. Neither the name of the Regents nor the
//    names of its contributors may be used to endorse or promote products
//    derived from this software without specific prior written permission.
//
// IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
// SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING
// OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF REGENTS HAS
// BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
// PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF ANY, PROVIDED
// HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION TO PROVIDE
// MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

// Modifications by Bluespec, Inc, 2018

// ================================================================

#include <iostream>
#include <sstream>
#include <cstdlib>
#include <cstring>
#include <cerrno>
#include <unistd.h>
#include <cinttypes>
//#include <signal.h>
#include <sys/wait.h>
#include <sys/types.h>


#include <vector>

// ================================================================

#define  DTC  "/usr/bin/dtc"

// ================================================================

#define  ROM_BASE  0x00001000

#define  DMEM_BASE  0x80000000
#define  DMEM_SIZE  0x10000000

#define  CLINT_BASE 0x02000000
#define  CLINT_SIZE 0x000c0000

#define  UART_BASE  0xc0000000

#define  NUM_PROCS  1

// -------------------------
// Timer configuration

// For Piccolo Sim
//
#define  CPU_HZ              50000lu

// #define  CPU_HZ              10000000lu

// ================================================================

static std::string dts;

static std::string dts_compile(const std::string& dts)
{
  // Convert the DTS to DTB
  int dts_pipe[2];
  pid_t dts_pid;

  if (pipe(dts_pipe) != 0 || (dts_pid = fork()) < 0) {
    std::cerr << "Failed to fork dts child: " << strerror(errno) << std::endl;
    exit(1);
  }

  // Child process to output dts
  if (dts_pid == 0) {
    close(dts_pipe[0]);
    int step, len = dts.length();
    const char *buf = dts.c_str();
    for (int done = 0; done < len; done += step) {
      step = write(dts_pipe[1], buf+done, len-done);
      if (step == -1) {
        std::cerr << "Failed to write dts: " << strerror(errno) << std::endl;
        exit(1);
      }
    }
    close(dts_pipe[1]);
    exit(0);
  }

  pid_t dtb_pid;
  int dtb_pipe[2];
  if (pipe(dtb_pipe) != 0 || (dtb_pid = fork()) < 0) {
    std::cerr << "Failed to fork dtb child: " << strerror(errno) << std::endl;
    exit(1);
  }

  // Child process to output dtb
  if (dtb_pid == 0) {
    dup2(dts_pipe[0], 0);
    dup2(dtb_pipe[1], 1);
    close(dts_pipe[0]);
    close(dts_pipe[1]);
    close(dtb_pipe[0]);
    close(dtb_pipe[1]);
    execl(DTC, DTC, "-O", "dtb", 0);
    std::cerr << "Failed to run " DTC ": " << strerror(errno) << std::endl;
    exit(1);
  }

  close(dts_pipe[1]);
  close(dts_pipe[0]);
  close(dtb_pipe[1]);

  // Read-out dtb
  std::stringstream dtb;

  int got;
  char buf[4096];
  while ((got = read(dtb_pipe[0], buf, sizeof(buf))) > 0) {
    dtb.write(buf, got);
  }
  if (got == -1) {
    std::cerr << "Failed to read dtb: " << strerror(errno) << std::endl;
    exit(1);
  }
  close(dtb_pipe[0]);

  // Reap children
  int status;
  waitpid(dts_pid, &status, 0);
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    std::cerr << "Child dts process failed" << std::endl;
    exit(1);
  }
  waitpid(dtb_pid, &status, 0);
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 0) {
    std::cerr << "Child dtb process failed" << std::endl;
    exit(1);
  }

  return dtb.str();
}

void make_dtb(int xlen, char *isa_string)
{
  const int reset_vec_size = 8;

  uint64_t start_pc = DMEM_BASE;

  uint32_t reset_vec[reset_vec_size] = {
    0x297,                                      // auipc  t0,0x0
    0x28593 + (reset_vec_size * 4 << 20),       // addi   a1, t0, &dtb
    0xf1402573,                                 // csrr   a0, mhartid
    xlen == 32 ?
      0x0182a283u :                             // lw     t0,24(t0)
      0x0182b283u,                              // ld     t0,24(t0)
    0x28067,                                    // jr     t0
    0,
    (uint32_t) (start_pc & 0xffffffff),
    (uint32_t) (start_pc >> 32)
  };

  std::vector<char> rom((char*)reset_vec, (char*)reset_vec + sizeof(reset_vec));

  std::stringstream s;
  s << std::dec <<
         "/dts-v1/;\n"
         "\n"
         "/ {\n"
         "  #address-cells = <2>;\n"
         "  #size-cells = <2>;\n"
         "  compatible = \"ucbbar,spike-bare-dev\";\n"
         "  model = \"ucbbar,spike-bare\";\n"
         "  cpus {\n"
         "    #address-cells = <1>;\n"
         "    #size-cells = <0>;\n"
         "    timebase-frequency = <" << CPU_HZ << ">;\n";
  // For each processor
  for (size_t i = 0; i < NUM_PROCS; i++) {
    s << "    CPU" << i << ": cpu@" << i << " {\n"
         "      device_type = \"cpu\";\n"
         "      reg = <" << i << ">;\n"
         "      status = \"okay\";\n"
         "      compatible = \"riscv\";\n"
         "      riscv,isa = \"rv" << xlen << isa_string << "\";\n"
         "      mmu-type = \"riscv," << (xlen <= 32 ? "sv32" : "sv48") << "\";\n"
         "      clock-frequency = <" << CPU_HZ << ">;\n"
         "      CPU" << i << "_intc: interrupt-controller {\n"
         "        #interrupt-cells = <1>;\n"
         "        interrupt-controller;\n"
         "        compatible = \"riscv,cpu-intc\";\n"
         "      };\n"
         "    };\n";
  }
  s <<   "  };\n";
  // For each memory
  uint64_t dmem_base = DMEM_BASE;
  uint64_t dmem_size = DMEM_SIZE;
  s << std::hex <<
         "  memory@" << dmem_base << " {\n"
         "    device_type = \"memory\";\n"
         "    reg = <0x" << (dmem_base >> 32) << " 0x" << (dmem_base & (uint32_t)-1) <<
                   " 0x" << (dmem_size >> 32) << " 0x" << (dmem_size & (uint32_t)-1) << ">;\n"
         "  };\n";
  // System
  s <<   "  soc {\n"
         "    #address-cells = <2>;\n"
         "    #size-cells = <2>;\n"
         "    compatible = \"ucbbar,spike-bare-soc\", \"simple-bus\";\n"
         "    ranges;\n"
         "    clint@" << CLINT_BASE << " {\n"
         "      compatible = \"riscv,clint0\";\n"
         "      interrupts-extended = <" << std::dec;
  for (size_t i = 0; i < NUM_PROCS; i++)
    s << "&CPU" << i << "_intc 3 &CPU" << i << "_intc 7 ";
  uint64_t clint_base = CLINT_BASE;
  uint64_t clint_size = CLINT_SIZE;
  s << std::hex << ">;\n"
         "      reg = <0x" << (clint_base >> 32) << " 0x" << (clint_base & (uint32_t)-1) <<
                     " 0x" << (clint_size >> 32) << " 0x" << (clint_size & (uint32_t)-1) << ">;\n"
         "    };\n"
         "  };\n"
      /*
         "  htif {\n"
         "    compatible = \"ucb,htif0\";\n"
         "  };\n"
      */
         "  uart@" << UART_BASE << " {\n"
         "    compatible = \"ns16550a\";\n"
         "    reg = <0x0 0x" << UART_BASE << " 0x0 0x8>;\n"
         "    reg-shift = <3>;\n"
         "    clock-frequency = <" << std::dec << CPU_HZ << std::hex << ">;\n"
         "  };\n"
         "};\n";

  dts = s.str();
  std::string dtb = dts_compile(dts);

  //printf("%s\n\n", dts.c_str());
  
  rom.insert(rom.end(), dtb.begin(), dtb.end());
  const int align = 0x1000;
  rom.resize((rom.size() + align - 1) / align * align);

  // Print the contents
  int bytes_per_word = 4;
  printf ("@ 1000\n");
  for (int i = 0; i < rom.size(); i += bytes_per_word) {
      for (int j = bytes_per_word - 1; j >= 0; j--) {
	  printf ("%02x", (unsigned char)(rom[i+j]));
      }
      printf ("\n");
  }
  if ((rom.size() % bytes_per_word) != 0)
      printf ("WARNING: last bytes of ROM omitted (incomplete word)\n");
}

void usage(char *progname) {
    printf("usage: %s <xlen> <isa>\n", progname);
    printf("example: %s 64 imafdcus\n", progname);
}

int main(int argc, char *argv[]) {
    char *progname = argv[0];

    if (argc != 3) {
	usage(progname);
	exit(1);
    }

    int xlen;
    char *rv_str = argv[1];
    if (strcmp(rv_str,"RV32") == 0) {
	xlen = 32;
    } else if (strcmp(rv_str,"RV64") == 0) {
	xlen = 64;
    } else {
	usage(progname);
	exit(1);
    }

    char isa_string[250];
    strcpy(isa_string, argv[2]);
    isa_string[249] = '\0';

    make_dtb(xlen, isa_string);
    exit(0);
}

// ================================================================
