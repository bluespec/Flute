# This Makefile is for standalone testing of various groups of
# functions in C_Imported_Functions.c

# ================================================================

C_Imported_Functions.o:  C_Imported_Functions.h  C_Imported_Functions.c
	$(CC) -c  C_Imported_Functions.c

# ================================================================
# Standalone testing

# Uncomment one of the following

TEST ?= PLEASE_CHOOSE_TEST

# TEST = TEST_TRYGETCHAR
# TEST = TEST_GET_SYMBOL_VAL
TEST = TEST_COMMS

test_exe: C_Imported_Functions.c
	$(CC) -o test_exe -D$(TEST) C_Imported_Functions.c

.PHONY: start
start:  test_exe
	./test_exe

# ================================================================

.PHONY: clean
clean:
	rm  -f  *~  *.o

.PHONY: full_clean
full_clean:
	rm  -f  *~  *.o  test_exe

# ================================================================
