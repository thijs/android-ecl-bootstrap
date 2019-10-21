# build the host ECL, which will then be used
# to build the cross-compiled Android version
# (assumes a 64bit platform)

set -x

./configure CFLAGS="-g -O2" LDFLAGS="-g -O2" \
            --prefix=${HOST_ECL} \
            --enable-manual=no \
            --disable-c99complex \
            CC=clang
make
make install
rm -r build
