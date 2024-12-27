#!/usr/bin/env bash

. .ci/common.sh

check_platform

set -x

sudo apt-get update -q -y
sudo apt-get install -q -y qemu-user

sudo apt-get install -y curl xz-utils

curl -L \
    ${ARM_MIRROR}/arm-gnu-toolchain-${GCC_REL}-x86_64-arm-none-linux-gnueabihf.tar.xz \
    | tar -Jx || exit 1
