# AMaCC = Arguably Minimalist Arm C Compiler

## Introduction
AMaCC is built from scratch, targeted at 32-bit ARM architecture.
It is a considerably stripped down version of C and it is meant as
pedagogical tool for learning about compilers, linkers, and loaders.

There are 3 execution modes AMaCC implements:
* Just-in-Time compiler (JITC) for ARM backend
* Generate valid Executable and Linkable Format (ELF) executables
* Interpreter-only execution

It is worth mentioning that AMaCC is designed to compile the minimal
subset of C required to self-host with the above execution modes. For
example, global variables and, in particular, global arrays are there.

Intermediate code generation is integrated into the parsing since it
is generating code for a stack-based machine and that also follows the
sequence of actions performed when parsing.

It mixes classical recursive descent and operator precedence parser.
An operator precedence parser is actually quite a bit faster than
recursive descent parser (RDP) for expressions when operator precedence
is defined using grammar productions that would otherwise get turned
into methods.

## Compatibility
AMaCC is capable of compiling C source files written in the following
syntax:
* data types: char, int, struct, and pointer
* condition statements: if, while, for, switch, case, break, return, and
                        general expressions
* compound assignments: `+=`, `-=`, `*=`, `/=`, `%=`
* global/local variable initializations for supported data types
    - e.g. `int i = [expr]`
    - New variables are allowed to be declared within functions anywhere.

The architecture support targets armv7hf with Linux ABI, verified on
Raspberry Pi 2/3 with GNU/Linux.

## Prerequisites
* Code generator in AMaCC relies on several GNU/Linux behaviors, and it
  is necessary to have ARM/Linux installed in your build environment.
* Install [Linaro ARM Toolchain](http://www.linaro.org/downloads/)
    - Check "Latest Linux Targeted Binary Toolchain Releases"
    - Select `arm-linux-gnueabihf` (32-bit Armv7-a, hard-float, little-endian)

* Install QEMU for ARM user emulation
```shell
sudo apt-get install qemu-user
```

## Running AMaCC
Run `make check` and you should see this:
```
[ C to IR translation          ] Passed
[ JIT compilation + execution  ] Passed
[ ELF generation               ] Passed
[ nested/self compilation      ] Passed
[ Compatibility with GCC/Arm   ] ........................................
----------------------------------------------------------------------
Ran 44 tests in 4.975s

OK
```

## Internals
Check [Intermediate Representation (IR) for AMaCC Compilation](docs/IR.md).

## Acknowledgements
AMaCC is based on the infrastructure of [c4](https://github.com/rswier/c4).

## Related Materials
* [Curated list of awesome resources on Compilers, Interpreters and Runtimes](http://aalhour.com/awesome-compilers/)
* [Hacker News discussions](https://news.ycombinator.com/item?id=11411124)
