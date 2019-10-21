#!/bin/bash

rm -rf app/src/main/libs

cd app/src/main/lisp/module \
    && make clean \
    && cd -

./gradlew clean
