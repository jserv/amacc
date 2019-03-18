# Intermediate Representation (IR) for AMaCC Compilation

## What is an IR
An Intermediate representation (IR) is the specific data structure or code
used internally by a compiler or virtual machine to represent a "program"
between source code and target languages. Before generating binary, the
compiler front-end will generate IR to aid the compiler backend to produce
the intermediate form which is independent of the source file.


## Why is an IR used
* Because translation appears to inherently require analysis and synthesis.
* Break the difficult problem of translation into two simpler,more manageable pieces.
* To build retargetable compilers:
  - Build new back ends for an existing front end(make source language more portable and
    across machine).
  - Can build a new front-end for an existing back end.
  - We only have to write 2n half-compilers instead of n(n-1) full compilers.
  - To perform machine independent optimizations.

So how does the IR actually work? Let's have an example:
```c
int a;
a = 10 + 1 + 11;
```

Inside AMaCC, the above C source will be converted into following IR:
```
IMM  10
PSH 
IMM  1
ADD 
PSH 
IMM  11
ADD 
```

These instructions will be stored inside the stack. According to the stack
LIFO (Last in First Out) order, they will be executed sequentially from
top to bottom illustrated as following:

```
| IMM  10 | pop "IMM 10"
| PSH     |-------------> | PSH   | pop "PSH"
| IMM  1  |               | IMM 1 |----------> | IMM 1 | pop "IMM 1"
| ADD     |               | ADD   |            | ADD   |----------->
| PSH     |               | PSH   |            | PSH   |
| IMM  11 |               | IMM 11|            | IMM 11|
| ADD     |               | ADD   |            | ADD   |

* stack    *
|         |               |       |            |   10  |
* register *
|         |               |   10  |            |       |
```

```
|  ADD    |  pop "ADD"
|  PSH    |-------------> | PSH   | pop "PSH"
|  IMM 11 |               | IMM 11|----------> | IMM 11|
|  ADD    |               | ADD   |            | ADD   |

* stack    *
|   10    |               |       |            |   11  |
* register *
|   1     |               |   11  |            |       |
```

```
|  IMM 11 | pop "IMM 11"             pop "ADD"
|  ADD    | ----------->  | ADD   |  --------> |       |

* stack    *
|   11    |               |   11  |            |       |
* register *
|         |               |   11  |            |   22  | -> the result we get
```

## Instructsion sets

|   opcode  |       format      |       ARM instructions        |                       comments                                   |
|-----------|-------------------|-------------------------------|------------------------------------------------------------------|
|LEA        | LEA \<offset\>    |add r0, r11, #\<offset>        |fetch arguments inside sub function                               |
|IMM        | IMM \<num\>       |mov r0, #20                    |put immediate \<num\> into general register                       |
|JMP        | JMP \<addr\>      |b \<addr\>                     |set PC register to \<addr\>                                       |
|JSR        | JSR \<addr\>      |bl \<addr\>                    |stores current execution position and jump to \<addr\>            |
|LEV        | LEV               |add sp, r11, #0; pop {r11, pc} |fetch bookkeeping info to resume previous execution             |
|ENT        | ENT \<size\>      |push {r11, lr} ;add r11, sp, #0|called when we are about to enter the function call to "make a new calling frame".It will store the current PC value onto the stack, and save \<size\> bytes to store the local variable for function.|
|ADJ        | ADJ \<size\>      |add sp, sp, #\<size\>          |adjust the stack(to remove argument from frame)                   |
|LI         | LI                |ldr r0, [r0]                   |loads an integer into general register from a given memory address which is stored in general register before execution|
|SI         | SI                |pop {r1};str r0, [r1]          |stores the integer in general register into the memory whose  address is stored on the top of the stack|
|LC         | LC                |ldrb r0, [r0]                  |loads an character into general register from a given memory address which is stored in general register before execution|
|SC         | SC                |pop {r1}; strb r0, [r1]        |stores the character in general register into the memory whose address is stored on the top of the stack| 
|PSH        | PSH               |push {r0}                      |pushes the value in general register onto the stack               |

## Function call example

```c
int func(int a) {
    return a * 10;
}

int main() {
    func(20);
    return 0;
}
```

while compiled with AMaCC, passing argument `-s` can generate IR along with
corresponding source..
```c
1: int func(int a) {
2:     return a * 10;
3: }
    ENT  0          ; save func addres on stack
    LEA  2          ; fetch a's address on stack and save into general register
    LI              ; Load integer from memory which address is inside general register
    PSH             ; push interger to top of stack which is inside general register
    IMM  10         ; move 10 into general register
    MUL             ; pop 'a' on the top of stack,and multiply 10 which is inside general register,store result into general register
    LEV             ; return to main
4:
5: int main()
6: {
7:     func(20);
8:     return 0;
9: }
    ENT  0          ; save main address on stack
    IMM  20         ; move 20 into general register
    PSH             ; push r0 on top of stack
    JSR  -11120300  ; save sp on stack,save current execute position to lr, jump to func
    ADJ  1          ; remove 20 from stack
    IMM  0          ; move 0 into general register
    LEV             ; return to entry
```

### Arithmetic instructions

Each operator has two arguments:
* the first is stored on the top of the stack;
* the second is stored in general register;

After the calculation is done, the argument on the stack will be poped out,
and the result will be stored in general register. So, you are not able to
fetch the first argument from the stack after the calculation.

You can see the above example to know how arithmetic instructions work.

### Conditional jump instructions

The `BZ` and `BNZ` instructions must be used with arithmetic instructions,
such as `EQ`,`NE`,`LT`,`GT`,`LE` and `GE`.

Example:

```c
7:     if (n > 0) {
    LEA  2          ; fetch n's address
    LI              ; load n's value into r0 register
    PSH             ; push n on to stack
    IMM  0          ; move 0 into r0 register
    GT              ; compare r0 and r1(pop r1 first on top of stack)
    BZ   0          ; jump when r1 > r0
```
The arithmetic instructions for comparisons will be translated to ARM instructions. Example:
```
# GT
pop  {r1}
cmp  r1, r0
movgt r0, 1
movle r0, 0
```

Branch-on-zero instruction is about to be translated as following:
```
# BZ
cmp  r0, 0
beq  0xff4a31d4
```

|   opcode     | format      |      ARM instructions        | comments |
| ------------ | ----------- | ---------------------------- | -------- |
| BZ           | BZ <value>  |cmp  r0, 0;beq  \<address\>   |branch on zero
| BNZ          | BNZ <value> |cmp  r0, 0;bne  \<address\>   |branch on not zero
