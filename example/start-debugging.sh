#!/bin/bash

#set -x

FORCE_INSTALL=0

if ! adb devices | grep -q '192.168.22.'
then
    echo Not connected yet...
    adb connect 192.168.22.81:5555
    sleep 2
fi


ANDROID_APK_PACKAGE="org.example.testapp"
#ACTIVITY_NAME="${ANDROID_APK_PACKAGE}.android.bindings.QtActivity"
ACTIVITY_NAME=".MainActivity"

# make sure the Activity is not already running
adb shell am force-stop ${ANDROID_APK_PACKAGE}

# if ! adb shell pm dump org.example.testapp | grep -q "versionCode=$(cat .version)"
# then
#     echo "Installing newer version $(cat .version)"
#     FORCE_INSTALL=1
# fi

# if [ "$1" == "FORCE" ]; then
#     echo "Forcing reinstall"
#     FORCE_INSTALL=1
# fi

# if [ "$FORCE_INSTALL" == "1" ]; then
#     # remove it
#     adb uninstall ${ANDROID_APK_PACKAGE}

#     sleep 1

#     # install it
#     adb install android-build/build/outputs/apk/debug/android-build-debug.apk

#     sleep 1
# fi


# start
echo ""
echo " ==> Starting Activity ${ACTIVITY_NAME} of APK ${ANDROID_APK_PACKAGE}"
echo "   adb shell am start '${ANDROID_APK_PACKAGE}/${ACTIVITY_NAME}'"

adb shell am start "${ANDROID_APK_PACKAGE}/${ACTIVITY_NAME}"

# Get the PID of the process created for our Activity
PID=$(adb shell "ps | grep [^/]${ANDROID_APK_PACKAGE}" | awk '{print $2}')

adb shell "ps | grep [^/]${ANDROID_APK_PACKAGE}"

# Create a unix pipe on the device to which we bind a local tcp port
echo ""
echo " ==> Forward host port : "
echo "   adb forward tcp:5039 tcp:5039"
#adb forward tcp:5039 localfilesystem:/data/data/${ANDROID_APK_PACKAGE}/debug-socket
adb forward tcp:5039 tcp:5039

APP_DIR="$(adb shell pm path $ANDROID_APK_PACKAGE)"
APP_DIR="${APP_DIR#*:}"
APP_DIR="${APP_DIR%/*}"

# Attach the gdbserver to the activity
echo ""
echo " ==> Attach gdbserver to the right process : "
echo "   adb shell run-as ${ANDROID_APK_PACKAGE} ${APP_DIR}/lib/arm64/libgdbserver.so :5039 --attach $PID"
adb shell run-as ${ANDROID_APK_PACKAGE} ${APP_DIR}/lib/arm64/libgdbserver.so :5039 --attach $PID
