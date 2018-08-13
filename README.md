AMaCC = Another Mini ARM C Compiler

Introduction
------------
AMaCC is built from scratch, supporting ARM architecture.
There are 3 execution modes AMaCC implements:
* Just-in-Time compiler for ARM backend
* Generate valid Executable and Linkable Format (ELF) executables
* Interpreter-only execution

Compatibility
-------------
AMaCC is capable of compiling C source files written in the following
syntax:
* data types: char, int, struct, and pointer
* condition statements: if, while, for, switch, return, and expression

Prerequisites
-------------
* Install [Linaro ARM Toolchain](http://www.linaro.org/downloads/)
    - Check "Latest Linux Targeted Binary Toolchain Releases"
    - Select `arm-linux-gnueabihf` (32-bit Armv7-a, hard-float, little-endian)

* Install QEMU for ARM user emulation
```
sudo apt-get install qemu-user
```

Running AMaCC
-------------
Run `make check` and you should see this:
```
[ C to IR translation          ] Passed
[ JIT compilation + execution  ] Passed
[ ELF generation               ] Passed
[ nested/self compilation      ] Passed
```

Acknowledgements
----------------
AMaCC is based on the infrastructure of [c4](https://github.com/rswier/c4).
[Hacker News discussions](https://news.ycombinator.com/item?id=11411124).
