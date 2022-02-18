# Squint - A peephole optimizer for AMaCC Compilation

## What is AMaCC?

AMaCC is an educational compiler that produces code for a
two register stack based virtual machine (VM).  The two
registers are (1) an accumulator and (2) an Index register
that can act as a second accumulator for math operations.
A stack is used for temporary memory. Stacks are
a great tool for simplifying operations on grammars.
Because of this, stack based compilers are often the
compiler used in Compiler classes, allowing the students
to create a working compiler in a semester.  Finally,
the great thing about a stack VM is that it's simple
to transform to other VMs.


## What is peephole optimization?

Ideally, code simplifying transformations would be done at the IR level or
higher in a compiler. High level transformations typically apply across a wide
range of architectures, common subexpression elimination being an example of
such a transformation.  On the other hand, specific architectures
often have features that are hard to express at a high enough level to be
used across many architectures.  Because of this, a common compiler strategy
is to generate "general purpose code" that uses common features found in
all architectures, and then rely on a "peephole optimization" pass
for architecture specific tuning as a final step.

Peephole optimizers usually operate of a small window of assembly language
instructions, maybe one to five.  A small window can be passed
across the entire program to apply safe optimizations in a local scope
that don't require a lot of analysis.

## What is Squint?

Squint goes a step further than most peephole optimizers, and relies on
a knowledge of code generation strategies used by the AMaCC compiler.
For example, A common "high level" IR pattern in AMaCC is:
```
LEA   { Load Effective Address e.g. add R0, fp, #X }
PUSH  { push R0 }
[ do a (large) self contained calculation yielding a result in R0 ]
SI    { pop R1 ; str R0, [R1] }
```
Squint does not need to worry about intermediate register states if it
sees the "LEA, PUSH, {...}, SI" pattern.  It can safely transform the
"add R0, fp, #X" used in the LEA before the push into "str R0, [fp, #X]"
in the SI instruction. Furthermore, with just a tiny amount of analysis,
Squint can usually remove the push and pop at the assembly level, greatly
reducing data movement in the executable. Since AMaCC targets a stack
based VM, roughly half of all operations are pushes and pops, so getting
rid of push/pop pairs is a big deal!

## What are the major components of Squint?

Squint manages the following features:

* Instruction stream constants

The ARM architecture allows compilers to emebed constants
in the instruction stream. Squint has to track these for
two reasons

    - Since a window is passed over the code, Squint
      has to know if it is analyzing an instruction or
      a piece of data.

    - Assembly instructions are rewritten by the peephole
      optimizer and constants may need to be moved.

* Push/Pop analysis

Usually, in a stack machine, pushes and pops are generated as
nested contexts.  Squint does analysis to identify push/pop
pairs in the instruction stream.  The only time that push/pop
pairs do not balance in AMaCC is when function calls push
arguments onto the stack.

* Function calls

Squint must analyze pushes created by function calls.
A special IR symbol, peephole disable (PHD), is inserted by
AMaCC when the -p flag is used. A special NOP instruction
is generated that skips the instruction that follows it
so that Squint can skip/ignore code during analysis.

Another special IR symbol, Peephole R0 (PHR0), inserts
a special NOP at the end of a function call to indicate
an R0 value has been generated as a return value
by the function call.  Without PHR0, there is no simple
indication in the code that R0 has been assigned a
"live" value.

* Branch analysis

A surprisingly large amount of code generation, and also execution
time, is tied to control flow branching.  AMaCC generates very simple
branch logic that is guaranteed to be correct, but not efficient.
The branch analysis in Squint rewrites the branching logic to be efficient.
This includes rethreading branches, eliminating branches, and simplifying
comparison code sequences.

* NOP manipulation

As the peephole optimizer does its job, it leaves behind a lot of NOP
operations where instructions used to be.  All those NOPS can increase
the complexity of analysis.  Because of this, special operations
were needed to access the "previous instruction" or "next instruction".
These operations filter out all NOPS or pc-relative constants in the
instruction stream.

After peephole optimization is complete, all the NOPS have to be
compressed out of the code and the pc-relative constants and branchs/calls
have to be moved.

Finally, NOP padding is inserted to improve memory alignment wherever
it will make the hardware run more efficiently.

* Peephole optimizations

Several functions exist that operate on a window of a few instructions
in a row. These functions can be called multiple times and/or in a
specific order to get the best hardware-specific optimization with
the least amount of coding effort. Peephole optimization functions
are given a generic name since they merely provide a scope to
facilitate reordering of operations. The name is not meaningful.

All the peephole operations call the NOP manipulation functions
to get the next/last instruction, so that the algorithms for peephole
optimization can be as uncluttered as possible.

* Use/Def analysis

This is the most important analysis in Squint, even though it is only
used for a few operations.  Typically, a use-def map is created for
one function at a time.  It contains high level information about each
instruction that can be easily scanned to look for register usage or
definition within the instruction stream.  Squint is currently very small
but the use-def logic provides a foundation to add additional powerful
analysis functions with just a few lines of code.

* Frame variable register allocation

The current register allocator is very small and primitive, and applies
to function contexts.  It is currently only applied to functions that do not
call other functions. It can be extended to work at a loop level, or to apply
more agressive optimizations, especially with the help of use-def analysis.
It has been kept simple to keep the size of Squint small, in the spirit of
AMaCC itself.
