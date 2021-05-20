###  -*-Makefile-*-

# *** This file is program-generated, not hand-written. ***
# *** Edit only if you need to tweak it! ***
# ================================================================

REPO   ?= ../..
ARCH   ?= RV64GC
PRIVS  ?= MSU
CACHES ?= WB_L1_L2
# For WB_L1_L2, use src_Core/Core_v2 instead of default src_Core/Core
SRC_CORE ?= $(REPO)/src_Core/Core_v2

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_C  \
	-D ISA_F  -D ISA_D  -D INCLUDE_FDIV  -D INCLUDE_FSQRT  \
	-D ISA_PRIV_M  -D ISA_PRIV_S  -D ISA_PRIV_U  \
	-D SV39  \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH    \
	-D Near_Mem_Caches    \
	-D FABRIC64    \
	-D WATCH_TOHOST    \

#================================================================
# For LLCache (used only for WB_L1_L2)

# core size
CORE_SIZE ?= SMALL
# default 1 core
CORE_NUM ?= 1
# cache size
CACHE_SIZE ?= LARGE

BSC_COMPILATION_FLAGS += \
     -D CORE_$(CORE_SIZE) \
     -D NUM_CORES=$(CORE_NUM) \
     -D CACHE_$(CACHE_SIZE) \

LLCACHE_DIR   = $(REPO)/src_Core/Near_Mem_VM_WB_L1_L2/src_LLCache
PROCS_LIB_DIR = $(LLCACHE_DIR)/procs/lib
PROCS_OOO_DIR = $(LLCACHE_DIR)/procs/RV64G_OOO
COHERENCE_DIR = $(LLCACHE_DIR)/coherence/src

CUSTOM_DIRS = $(LLCACHE_DIR):$(PROCS_LIB_DIR):$(PROCS_OOO_DIR):$(COHERENCE_DIR)

#================================================================
# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator

include $(REPO)/builds/Resources/Include_bluesim.mk

