#!/bin/bash

set -x

cd docker

docker tag android-ecl-bootstrap:latest android-ecl-bootstrap:previous

docker build --build-arg user=${USER:-user} -t android-ecl-bootstrap .

cd -
