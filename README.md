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

You might also want to grep for `TAG` and replace that with your own
tag so you can find the log messages your app sends to logcat easily.

* You should now be able to build (and install) your app:
````bash
./start.sh             # unless you're already inside the docker container
cd ~/code/clftw
./2-build-and-install install
````

# Next steps

## Figuring out how it all connects

If you look in `example/app/src/main/java/org/example/testapp/` you'll
find two `kotlin` source files: `MainActivity.kt` and
`EmbeddedCommonLisp.kt`.

`MainActivity.kt` is where your app starts. This is just a bare
template adapted from the default that Android Studio gives you if you
select a `Native C++` project to start.

`MainActivity.kt` uses the other file (`EmbeddedCommonLisp.kt`),
creating a private `ECL` instance, which it first calls `initialize`
on, then `start`.

In `initialize` we copy the dummy `.fas` file we built (`module.fas`)
to a place where we can access it later on. You should replace this
line with your own lines to copy the compiled Lisp files you've
created for your project (see below).

Next the call to `ECL.start` calls the JNI function `JNIstart` defined
in `example/app/src/main/cpp/src/cxx/main.cpp`. In that file you'll
see that the function is actually called
`Java_org_example_testapp_EmbeddedCommonLisp_JNIstart` which is JNI's
way to automatically discover native functions. You should also take
note of the matching `external fun` declaration in
`EmbeddedCommonLisp.kt`.

This function does just the bare minimum, logging some stuff and then
calling `ecl_boot`, defined in `ecl_boot.cpp` in the same directory.

The function `ecl_boot` sets up our Lisp environment, and loads the
`module.fas` we copied to the correct location earlier.

## Building your own Lisp module `.fas` files

In `example/app/src/main/lisp/module` you'll find complete examples of
how to build the Lisp code for your project. In principle the only
files you'd need to change are the `.asd`, the `.deps` and the `.lisp`
files.

The seperate package files are of course not strictly necessary.

The example as setup actually builds two seperate `.fas` files
(`module.fas` and `other.fas`), although only one is actually used in
the rest of the example project code.

### Dependency files (`.deps`)

The `.deps` files should `quickload` all the required dependencies of
your module. Basically, every `:depends-on` entry in your `.asd` file
should have a corresponding `(ql:quickload ..)` line in the `.deps`
file of your project. The `.asd` and `.deps` file should have the same
base filename (and that should be the same as your `defsystem` name
for Quicklisp/asdf to work). So the system `:module` in the example
has the files `module.asd` and `module.deps`.