AMaCC = Another Mini ARM C Compiler

Introduction
------------
AMaCC is built from scratch, supoorting ARM architecture.
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
    - linaro-toolchain-binaries (little-endian)

* Install QEMU for ARM user emulation
```
sudo apt-get install qemu-user
```

Running AMaCC
-------------
Run 'make check' and you should see this:
```
[ compiled ]
hello, world
[ nested ]
hello, world
```

Acknowledgements
----------------
AMaCC is based on the infrastructure of [c4](https://github.com/rswier/c4).
[Hacker News discussions](https://news.ycombinator.com/item?id=11411124).
