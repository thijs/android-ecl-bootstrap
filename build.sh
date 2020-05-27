#!/bin/bash

#set -x

cd docker

docker build --build-arg user=${USER:-user} -t android-ecl-bootstrap .

if [ "$1" = "timestamp" ]; then
    docker tag android-ecl-bootstrap:latest android-ecl-bootstrap:$(date +%FT%H%M)
fi

cd -
