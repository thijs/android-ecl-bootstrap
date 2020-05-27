#!/bin/bash

if [ -z "$HOME" -o -z "$USER" ]; then
    echo "Set HOME and USER env variables before running this..."
    exit 0
fi

# set CODE_DIR env to point to your own code directory if needed
CODE_DIR="${CODE_DIR:-${HOME}/code}"

mkdir -p $HOME/docker.state/android-ecl-bootstrap

CUR_DIR=$(pwd)

if [ -z $HOST_IP ]; then
    HOST_IP=$(ifconfig | grep -A1 eth0 | awk '/inet/ {print $2}')
fi

###################################
# Change this to your own mobile device IP (see README.md)
ANDROID_IP="192.168.22.81"

docker run --rm -it \
       --net=host \
       --privileged \
       --group-add plugdev \
       -v /etc/localtime:/etc/localtime:ro \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -v /dev/dri:/dev/dri \
       -e DISPLAY=unix$DISPLAY \
       -e HOST_IP=${HOST_IP} \
       -e ANDROID_IP=${ANDROID_IP} \
       --name android-ecl-bootstrap \
       -v $HOME/docker.state/android-ecl-bootstrap:/home/$USER \
       -v ${CODE_DIR}:/home/$USER/code \
       -v ${CUR_DIR}/example:/home/$USER/code/example \
       -v /usr/share/fonts:/usr/share/fonts/host \
       android-ecl-bootstrap
