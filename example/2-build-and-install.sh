#!/bin/bash

if [ ! -f ${HOME}/quicklisp/setup.lisp ]; then
    ./install-quicklisp.sh || exit 1
fi


VER=$(cat .version)

VERSION_CODE=$((VER + 1)) \
            VERSION_STRING="0.0.${VERSION_CODE}" \
            cat app/build.gradle.source | envsubst > app/build.gradle

echo Building version $((VER + 1))

mkdir -p app/src/main/libs/arm64-v8a
cp ${ECL_ANDROID_64}/lib/libecl.so app/src/main/libs/arm64-v8a/
cp ${ANDROID_NDK_TOOLCHAIN_64}/share/gdbserver/gdbserver app/src/main/libs/arm64-v8a/libgdbserver.so

cd app/src/main/lisp/module \
&& make \
&& cd - \
&& ./gradlew build \
&& echo $((VER + 1)) > .version \
&& echo -n "Built version " && cat .version \
&& if [ "$1" == "install" ]; then

    BASE_IP="${HOST_IP%.*}."
    if ! adb devices | grep -q ${BASE_IP}
    then
        echo Not connected yet...
        adb connect ${ANDROID_IP}:5555
        sleep 2
    fi

    ANDROID_APK_PACKAGE="org.example.testapp"

    # make sure the Activity is not already running
    adb shell am force-stop ${ANDROID_APK_PACKAGE}

    echo -n "removing old version... "

    # remove it
    adb uninstall ${ANDROID_APK_PACKAGE}

    sleep 0.25

    # install it
    adb push -p \
        app/build/outputs/apk/debug/app-debug.apk \
        /data/local/tmp

    echo -n "installing... "
    adb shell pm install /data/local/tmp/app-debug.apk
    adb shell rm /data/local/tmp/app-debug.apk

fi
