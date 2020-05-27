# use the previously built host ECL to build the android version
#
# requires the ndk-18b standalone toolchain, see:
#   $ <ndk>/build/tools/make_standalone_toolchain.py -h
#
# you need to export ANDROID_NDK_TOOLCHAIN_64 pointing to it

set -x

export AR=$ANDROID_NDK_TOOLCHAIN_64/bin/aarch64-linux-android-ar
export AS=$ANDROID_NDK_TOOLCHAIN_64/bin/aarch64-linux-android-as
#export CC=$NDK_TOOLCHAIN_DIR/llvm/prebuilt/linux-x86_64/bin/aarch64-linux-android${ANDROID_PLATFORM}-clang
export CC=$NDK_TOOLCHAIN_DIR/bin/aarch64-linux-android-clang
export LD=$ANDROID_NDK_TOOLCHAIN_64/bin/aarch64-linux-android-ld
export RANLIB=$ANDROID_NDK_TOOLCHAIN_64/bin/aarch64-linux-android-ranlib
export STRIP=$ANDROID_NDK_TOOLCHAIN_64/bin/aarch64-linux-android-strip

export ECL_TO_RUN=${HOST_ECL}/bin/ecl

./configure --host=aarch64-linux-android \
            --disable-c99complex \
            --enable-manual=no \
            --prefix=${TARGET_ECL} \
            --with-cross-config=`pwd`/src/util/android-arm64.cross_config

#            --enable-debug \
#            --enable-boehm=included \

make -j4
make install
