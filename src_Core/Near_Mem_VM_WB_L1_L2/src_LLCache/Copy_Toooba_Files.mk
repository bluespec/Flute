# This Makefile copies, from Toooba, sources for the L2 cache ('LLCache').
# Those files, in turn, originate from MIT's RISCY-OOO code.

default: copy_files

# ----------------------------------------------------------------

A = ~/git_clones/Toooba/src_Core/RISCY_OOO/
B = .

PROCS_LIB_A = $(A)/procs/lib
PROCS_LIB_B = $(B)/procs/lib

PROCS_OOO_A = $(A)/procs/RV64G_OOO
PROCS_OOO_B = $(B)/procs/RV64G_OOO

COHERENCE_A = $(A)/coherence/src
COHERENCE_B = $(B)/coherence/src

.PHONY: copy_files
copy_files:
	mkdir -p  $(PROCS_OOO_B)
	cp -p  $(PROCS_OOO_A)/ProcConfig.bsv       $(PROCS_OOO_B)/ProcConfig.bsv
	mkdir -p  $(PROCS_LIB_B)
	cp -p  $(PROCS_LIB_A)/Amo.bsv              $(PROCS_LIB_B)/Amo.bsv
	cp -p  $(PROCS_LIB_A)/CacheUtils.bsv       $(PROCS_LIB_B)/CacheUtils.bsv
	cp -p  $(PROCS_LIB_A)/Ehr.bsv              $(PROCS_LIB_B)/Ehr.bsv
	cp -p  $(PROCS_LIB_A)/Fifos.bsv            $(PROCS_LIB_B)/Fifos.bsv
	cp -p  $(PROCS_LIB_A)/FullAssocTlb.bsv     $(PROCS_LIB_B)/FullAssocTlb.bsv
	cp -p  $(PROCS_LIB_A)/LatencyTimer.bsv     $(PROCS_LIB_B)/LatencyTimer.bsv
	cp -p  $(PROCS_LIB_A)/LLCRqMshrSecureModel.bsv    $(PROCS_LIB_B)/LLCRqMshrSecureModel.bsv
	cp -p  $(PROCS_LIB_A)/MemLoaderIF.bsv      $(PROCS_LIB_B)/MemLoaderIF.bsv
	cp -p  $(PROCS_LIB_A)/MemoryTypes.bsv      $(PROCS_LIB_B)/MemoryTypes.bsv
	cp -p  $(PROCS_LIB_A)/MMIOAddrs.bsv        $(PROCS_LIB_B)/MMIOAddrs.bsv
	cp -p  $(PROCS_LIB_A)/MsgFifo.bsv          $(PROCS_LIB_B)/MsgFifo.bsv
	cp -p  $(PROCS_LIB_A)/Performance.bsv      $(PROCS_LIB_B)/Performance.bsv
	cp -p  $(PROCS_LIB_A)/ProcTypes.bsv        $(PROCS_LIB_B)/ProcTypes.bsv
	cp -p  $(PROCS_LIB_A)/SafeCounter.bsv      $(PROCS_LIB_B)/SafeCounter.bsv
	cp -p  $(PROCS_LIB_A)/SetAssocTlb.bsv      $(PROCS_LIB_B)/SetAssocTlb.bsv
	cp -p  $(PROCS_LIB_A)/TranslationCache.bsv    $(PROCS_LIB_B)/TranslationCache.bsv
	cp -p  $(PROCS_LIB_A)/Types.bsv            $(PROCS_LIB_B)/Types.bsv
	mkdir -p  $(COHERENCE_B)
	cp -p  $(COHERENCE_A)/CCPipe.bsv           $(COHERENCE_B)/CCPipe.bsv
	cp -p  $(COHERENCE_A)/CCTypes.bsv          $(COHERENCE_B)/CCTypes.bsv
	cp -p  $(COHERENCE_A)/CrossBar.bsv         $(COHERENCE_B)/CrossBar.bsv
	cp -p  $(COHERENCE_A)/IBank.bsv            $(COHERENCE_B)/IBank.bsv
	cp -p  $(COHERENCE_A)/ICRqMshr.bsv         $(COHERENCE_B)/ICRqMshr.bsv
	cp -p  $(COHERENCE_A)/IPRqMshr.bsv         $(COHERENCE_B)/IPRqMshr.bsv
	cp -p  $(COHERENCE_A)/LLCRqMshr.bsv        $(COHERENCE_B)/LLCRqMshr.bsv
	cp -p  $(COHERENCE_A)/LLBank.bsv           $(COHERENCE_B)/LLBank.bsv
	cp -p  $(COHERENCE_A)/LLPipe.bsv           $(COHERENCE_B)/LLPipe.bsv
	cp -p  $(COHERENCE_A)/MshrDeadlockChecker.bsv    $(COHERENCE_B)/MshrDeadlockChecker.bsv
	cp -p  $(COHERENCE_A)/RandomReplace.bsv    $(COHERENCE_B)/RandomReplace.bsv
	cp -p  $(COHERENCE_A)/RWBramCore.bsv       $(COHERENCE_B)/RWBramCore.bsv

# ----------------------------------------------------------------

.PHONY: clean
clean:
	rm -r -f  *~

.PHONY: full_clean
full_clean: clean
	rm -r -f  procs  coherence

# ----------------------------------------------------------------
