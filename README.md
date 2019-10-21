# android-ecl-bootstrap

Bootstrap repo for ECL on android

## Build Android apps using Embeddable Common Lisp

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

## Initial one time docker build

The simple way is to build the docker container and use it to
cross-compile and build your Android app:

````bash
export USER=<your user nick>  # optional and maybe not needed, check: env | grep USER
./build.sh
````

## Building an app

Now (after a quite lengthy build process) you should have a docker
container named `android-ecl-bootstrap`, which you can start by doing:

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

On the first run it'll download and install a bunch of stuff, which is
saved in your mounted home directory (see `start.sh`; by default this
is at `$HOME/docker.state/android-ecl-bootstrap` on your host
machine). You can remove this line, if you want, but then each time
you start the docker container it will be blank and redownload
everything.

The first run can take a couple of minutes to finish.

If there were no errors, you should find an apk at
`app/build/outputs/apk/debug/app-debug.apk`

If you pass `install` as an argument to `2-build-and-install`, it will
attempt to install the app on your connected phone. For this to work,
ideally you should setup your network to provide your phone with a
fixed IP address and change `ANDROID_IP` in `start.sh` to reflect that
IP.

Alternatively, you can always manually connect your mobile test device
through `adb` before running `./2-build-and-install install`. Do
something like this (in the running docker container, obviously):

````bash
adb connect <mobile device IP>:<port>
````

## Without docker

If you don't want to use docker (why?) you will need to setup your
build environment yourself. You're on your own there, but if you
basically perform all the actions defined in the Dockerfile (adapted
to correct paths, etc), you should be able to get things up and
running.

# Roll your own

This repo serves as a proof of concept for building Android apps with
Common Lisp using ECL. In the `example` directory you'll find
everything to build the example app.

It should be fairly straightforward to adapt it and use it as a base
to build your own app.

I suggest the following procedure:

* Make a copy of `example` to your code directory (see `CODE_DIR` env).

* Put your new copy into some source code control system.

* Replace all mentions of `org.example.testapp` with your own app
  name. If your app is `com.fancydomain.clftw`, do:
````bash
find . -path ./.git -prune -o -type f -exec sed -i 's/org_example_testapp/com_fancydomain_clftw/g' '{}' \;
find . -path ./.git -prune -o -type f -exec sed -i 's/org.example.testapp/com.fancydomain.clftw/g' '{}' \;
find . -path ./.git -prune -o -type f -exec sed -i 's/testapp/clftw/g' '{}' \;
````

* You should now be able to build (and install) your app:
````bash
cd ~/code/clftw
./2-build-and-install install
````

# Next steps

