#!/usr/bin/env bash

ARM_MIRROR=https://github.com/DLTcollab/toolchain-arm/raw/main
GCC_REL=11.2-2022.02

MACHINE_TYPE=`uname -m`
if [ ${MACHINE_TYPE} != 'x86_64' ]; then
    exit
fi

OS_TYPE=`uname -s`
if [ ${OS_TYPE} != 'Linux' ]; then
    exit
fi

set -x

sudo apt-get update -q -y
sudo apt-get install -q -y qemu-user

sudo apt-get install -y curl xz-utils

curl -L \
    ${ARM_MIRROR}/gcc-arm-${GCC_REL}-x86_64-arm-none-linux-gnueabihf.tar.xz \
    | tar -Jx || exit 1
