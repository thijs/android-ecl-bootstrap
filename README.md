# android-ecl-bootstrap
Bootstrap repo for ECL on android

## Build Android apps using ECL

This repo demonstrates how to build an Android app that uses ECL
Common Lisp code.

Thanks to the following projects for inspiraition and guidance:

* [ECL](https://gitlab.com/embeddable-common-lisp/ecl) itself,
  specifically the `android` example

* [EQL](https://gitlab.com/eql/EQL5) and
  [EQL5-Android](https://gitlab.com/eql/EQL5-Android), most especially
  the examples `REPL` and `my` in EQL5-Android, as well as the code to
  cross-compile for Android in the `utils` directory

Some code has been copied/adapted from `EQL` and `EQL5-Android`.


# Getting started

The simple way is to build the docker container and use it to
cross-compile and build your Android app:

````bash
export USER=<your user nick>  # optional
./build.sh
````

Now you should have a docker container named `android-ecl-bootstrap`,
which you can start by doing:

````bash
export CODE_DIR=<your fully qualified code location>  # optional
./start.sh
````

This script assumes your code lives in `$HOME/code`. If that is not
the case, export the correct location as described above.

You should be able to build the example now, by doing:

````bash
cd code/example
./2-build-and-install.sh
````

On the first run it'll download and install a bunch of stuff.

If there were no errors, you should find an apk at
`app/build/outputs/apk/debug/app-debug.apk`

If you pass `install` as an argument to `2-build-and-install`, it will
attempt to install the app on your connected phone. For this to work,
ideally you should setup your network to provide your phone with a
fixed IP address and change `ANDROID_IP` in `start.sh` to reflect that
IP.

Alternatively, you can always manually connect your mobile test device
through `adb` before running `./2-build-and-install install`.