#!/bin/bash

#set -x

DEVICE_NAME="${1:-device}"

LOCAL_SYSROOT="${DEVICE_NAME}"

mkdir -p "${LOCAL_SYSROOT}/system/bin"
mkdir -p "${LOCAL_SYSROOT}/system/lib"
mkdir -p "${LOCAL_SYSROOT}/vendor/lib"

adb pull /system/lib "${LOCAL_SYSROOT}/system"
adb pull /vendor/lib "${LOCAL_SYSROOT}/vendor"


adb pull /system/bin/linker "${LOCAL_SYSROOT}/system/bin/"
adb pull /system/bin/app_process "${LOCAL_SYSROOT}/system/bin/"
adb pull /system/bin/app_process_init "${LOCAL_SYSROOT}/system/bin/"
adb pull /system/bin/app_process32 "${LOCAL_SYSROOT}/system/bin/"
adb pull /system/bin/app_process64 "${LOCAL_SYSROOT}/system/bin/"
