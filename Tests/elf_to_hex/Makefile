# Makefile to create an elf_to_hex executable.
# The executable creates mem-hex files containing 32-Byte words

CC = gcc

elf_to_hex: elf_to_hex.c
	gcc -g  -o elf_to_hex  elf_to_hex.c  -lelf

# ================================================================

.PHONY: clean
clean:
	rm  -f  *~

.PHONY: full_clean
full_clean:
	rm  -f  *~  elf_to_hex
