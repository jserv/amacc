# Amendment to AMaCC README.md for less experienced in Linux

## Introduction
Some tips for those who are less experienced in Linux, can still enjoy the beauty
of AMaCC. This contains more user friendly installation, fixes to potential
issues.


## Prerequisites
* Code generator in AMaCC relies on several GNU/Linux behaviors, and it is
  necessary to have Arm/Linux installed in your build environment. 
* Throughout this document, Debian based Linux (Ubuntu suggested) is used as an
  example. If other Linux distro is used, Linux shell command needs to be
  modified accordingly. For example, changing `apt-get` to `yum` for CentOS.
* Install [GNU Toolchain for the A-profile Architecture]
  (https://developer.arm.com/tools-and-software/open-source-software/developer-tools/gnu-toolchain/gnu-a/downloads)
  ```shell sudo apt-get install gcc-arm-linux-gnueabihf ```


## Potential Issues Running AMaCC
There might be issue related to link path setting, error message `No such file or directory` below.
```
$ make clean
rm -f amacc amacc-native elf/* elf/* out-gcc/*
$ make
  CC+LD   amacc
  CC+LD   amacc-native
$ qemu-arm ./amacc tests/hello.c
/lib/ld-linux-armhf.so.3: No such file or directory
```

It can be solved by either of the following options.
```
# option 1 - using '-L' option
$ qemu-arm -L /usr/arm-linux-gnueabihf/ ./amacc tests/hello.c
hello, world

# option 2 - set QEMU_LD_PREFIX path
$ export QEMU_LD_PREFIX=/usr/arm-linux-gnueabihf
$ echo $QEMU_LD_PREFIX
/usr/arm-linux-gnueabihf
$ qemu-arm ./amacc tests/hello.c
hello, world
```

## Related Materials
* [How to solve "error while loading shared libraries" when trying to run an arm binary with qemu-arm?](https://stackoverflow.com/questions/16158994/how-to-solve-error-while-loading-shared-libraries-when-trying-to-run-an-arm-bi)