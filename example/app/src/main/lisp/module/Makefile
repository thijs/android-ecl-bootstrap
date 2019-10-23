
BUILD_DIR		= ../fas
FAS_DIR			= fas
DIRS				= $(BUILD_DIR) $(FAS_DIR)
ECL					= $(HOST_ECL)/bin/ecl -norc

SYSTEMS			= $(wildcard *.asd)
FASS				= $(SYSTEMS:.asd=.fas)
FFAS				= $(patsubst %,$(FAS_DIR)/%,$(FASS))
TARGET_FAS	= $(patsubst %,$(BUILD_DIR)/%,$(FASS))

LISP_SRC		= $(wildcard src/*.lisp)

ECL_DIR			= $(ECL_LIBRARY)/ecl-16.1.3
ECL_FAS			= $(patsubst $(ECL_DIR)/%,%,$(wildcard $(ECL_DIR)/*.fas))
ECL_ASD			= $(patsubst $(ECL_DIR)/%,%,$(wildcard $(ECL_DIR)/*.asd))
TARGET_ECL	= $(patsubst %,$(BUILD_DIR)/%,$(ECL_FAS)) $(patsubst %,$(BUILD_DIR)/%,$(ECL_ASD))

UTILS				= ../utils/cross-compile-64.lisp ../utils/make.lisp ../utils/make-ASDF.lisp
PTHREAD_DEP = $(ANDROID_NDK_TOOLCHAIN_64)/sysroot/usr/lib/aarch64-linux-android/libpthread.a

# $(info $(SYSTEMS))
# $(info $(FFAS))
# $(info $(FASS))
# $(info $(TARGET_FAS))

# $(info $(ECL_FAS))
# $(info $(ECL_ASD))
# $(info $(TARGET_ECL))

all: $(FFAS) | $(TARGET_FAS) $(TARGET_ECL)
	$(ECL) --shell ../utils/build-load.lisp


$(BUILD_DIR)/%: $(FAS_DIR)/%
	cp -f $< $@

$(BUILD_DIR)/%: $(ECL_DIR)/%
	cp -f $< $@


fas/%.fas: %.asd $(LISP_SRC) $(UTILS) | $(PTHREAD_DEP)
	$(ECL) --shell ../utils/make.lisp --target $<


# Build a dummy libpthread.a, as all of pthread is incorporated in android libc already
$(pthread_dep):
	$(ANDROID_NDK_TOOLCHAIN_64)/bin/aarch64-linux-android-ar cr $(PTHREAD_DEP)


clean-fas:
	rm -f $(FAS_DIR)/*
	rm -f $(BUILD_DIR)/*

clean:
	rm -f *.flist
	rm -rf $(DIRS)
	rm -f src/*.o

.PHONY: $(PTHREAD_DEP) all clean clean-fas

$(info $(shell mkdir -p $(DIRS)))
