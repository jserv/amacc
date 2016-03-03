AMaCC = Another Mini ARM C Compiler

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
AMaCC is inspired by [c4](https://github.com/rswier/c4). AMaCC works
as a small Just-in-Time compiler for ARM backend.
