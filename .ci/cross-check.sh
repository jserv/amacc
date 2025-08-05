#!/usr/bin/env bash

. .ci/common.sh

check_platform

# Clang/LLVM is natively a cross-compiler.
# TODO: Do cross-compilation using Clang
# https://clang.llvm.org/docs/CrossCompilation.html
if [ $(printenv CXX | grep clang) ]; then
    exit
fi

set -x

export PATH=arm-gnu-toolchain-${GCC_REL}-x86_64-arm-none-linux-gnueabihf/bin:$PATH
make CROSS_COMPILE=arm-none-linux-gnueabihf- check || exit 1
