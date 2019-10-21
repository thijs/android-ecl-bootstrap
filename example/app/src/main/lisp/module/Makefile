
DIRS=fas ../fas
ECL=$(HOST_ECL)/bin/ecl -norc

systems = $(wildcard *.asd)
fass = $(systems:.asd=.fas)
ffas = $(patsubst %,fas/%,$(fass))

src=$(wildcard src/*.lisp)

utils=../utils/cross-compile-64.lisp ../utils/make.lisp ../utils/make-ASDF.lisp
pthread_dep=$(ANDROID_NDK_TOOLCHAIN_64)/sysroot/usr/lib/aarch64-linux-android/libpthread.a

# $(info $(systems))
# $(info $(ffas))
# $(info $(fass))


all: $(ffas)
	cp fas/*.fas ../fas/


fas/%.fas: %.asd $(src) $(utils) | $(pthread_dep)
	$(ECL) --shell ../utils/make.lisp --target $<


# Build a dummy libpthread.a, as all of pthread is incorporated in android libc already
$(pthread_dep):
	$(ANDROID_NDK_TOOLCHAIN_64)/bin/aarch64-linux-android-ar cr $(pthread_dep)


clean:
	rm -f *.flist
	rm -rf $(DIRS)
	rm -f src/*.o

.PHONY: $(pthread_dep) clean all

$(info $(shell mkdir -p $(DIRS)))
