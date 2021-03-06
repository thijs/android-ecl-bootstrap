# docker for android development with ECL common lisp
#

# only used for caching (large) downloads
FROM alpine AS downloads

# Don't use ADD because that will download every time...
RUN cd / \
 && wget https://dl.google.com/android/repository/commandlinetools-linux-6609375_latest.zip

FROM ubuntu:18.04
MAINTAINER Thijs Oppermann <thijso@gmail.com>

ENV LANG=C.UTF-8

ENV SHELL=/bin/bash
ENV JAVA_HOME="/usr/lib/jvm/java-8-openjdk-amd64"

ENV ANDROID_ROOT="/opt/android"
ENV SDK_ROOT="${ANDROID_ROOT}/sdk"

RUN apt-get update && apt-get install -y \
    build-essential \
    ca-certificates \
    clang \
    curl \
    gcc-multilib \
    gettext-base \
    git \
    gpg \
    gpg-agent \
    less \
    libgcc1 \
    libncurses5 \
    libsdl1.2debian \
    libstdc++6 \
    libxcb-xkb-dev \
    libxkbcommon-dev \
    m4 \
    netbase \
    netcat \
    openjdk-8-jdk \
    python \
    screen \
    sudo \
    texinfo \
    unzip \
    wget \
    xz-utils \
    zip \
    zlib1g \
    --no-install-recommends \
 && rm -rf /var/lib/apt/lists/*


ARG user=${USER}
ENV HOME=/home/${user}

RUN echo "user: $user" \
 && echo "home: $HOME" \
 && useradd --create-home --home-dir ${HOME} ${user} \
 && chown -R ${user}:${user} ${HOME} \
 && echo "${user}   ALL = NOPASSWD: ALL" >> /etc/sudoers

RUN mkdir -p ${ANDROID_ROOT} \
 && chown -R ${user}:${user} /opt

USER $user

ENV ANDROID_HOME="${ANDROID_ROOT}"

ENV ANDROID_PLATFORM=21
ENV ANDROID_TARGET_PLATFORM=30
ENV BUILD_TOOLS_VER="30.0.2"
ENV CMAKE_VER="3.10.2.4988404"
ENV LLDB_VER="3.1"
ENV NDK_VER="21.2.6472646"

COPY --from=downloads /commandlinetools-linux-*.zip ${ANDROID_ROOT}/
RUN cd ${ANDROID_ROOT} && unzip commandlinetools-linux-*.zip \
 && sudo rm commandlinetools-linux-*.zip \
 && mkdir -p cmdline-tools/ \
 && mv tools cmdline-tools/ \
 && cd ${ANDROID_ROOT}/cmdline-tools/tools

RUN cd ${ANDROID_ROOT}/cmdline-tools/tools \
 && yes | ./bin/sdkmanager --licenses \
 && ./bin/sdkmanager --install "tools" \
 && ./bin/sdkmanager --install "platform-tools" \
 && ./bin/sdkmanager --install "cmake;${CMAKE_VER}" \
 && ./bin/sdkmanager --install "ndk-bundle" \
 && ./bin/sdkmanager --install "platforms;android-${ANDROID_PLATFORM}" \
 && ./bin/sdkmanager --install "platforms;android-${ANDROID_TARGET_PLATFORM}" \
 && ./bin/sdkmanager --install "build-tools;${BUILD_TOOLS_VER}" \
 && ./bin/sdkmanager --install "cmdline-tools;latest" \
 && ./bin/sdkmanager --list

ENV HOST_ECL="/opt/ecl-android-host"
ENV TARGET_ECL="/opt/ecl-android-target"

COPY files/1-make-ecl-host.sh /tmp/
RUN cd ${ANDROID_ROOT} \
 && git clone https://gitlab.com/embeddable-common-lisp/ecl.git \
 && cd ecl \
 && sudo mv /tmp/1-make-ecl-host.sh ./ \
 && ./1-make-ecl-host.sh \
 && sudo rm 1-make-ecl-host.sh

ENV NDK_TOOLCHAIN_DIR="${ANDROID_ROOT}/ndk_${NDK_VER}"
ENV ANDROID_NDK_TOOLCHAIN_64="${NDK_TOOLCHAIN_DIR}"

RUN cd ${ANDROID_ROOT}/ndk-bundle \
 && ./build/tools/make_standalone_toolchain.py \
    --arch arm64 --api ${ANDROID_PLATFORM} \
    --install-dir ${NDK_TOOLCHAIN_DIR}

COPY files/2-make-ecl-android.sh /tmp/
RUN cd ${ANDROID_ROOT}/ecl \
 && sudo mv /tmp/2-make-ecl-android.sh ./ \
 && ./2-make-ecl-android.sh \
 && sudo rm 2-make-ecl-android.sh

ENV ECL_LIBRARY="${TARGET_ECL}/lib"
ENV ECL_INCLUDE_DIR="${TARGET_ECL}/include"

ENV ECL_ANDROID_64="${TARGET_ECL}"
ENV ANDROID=1
ENV PATH="${PATH}:${ANDROID_ROOT}/platform-tools"

ENV ANDROID_HOME="${ANDROID_ROOT}"

WORKDIR $HOME

CMD [ "bash" ]
