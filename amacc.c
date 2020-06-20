/*
 * AMaCC is capable of compiling (subset of) C source files into GNU/Linux
 * executables or running via just-in-time compilation on 32-bit ARM
 * processor-based platforms. There is no preprocessor.
 *
 * The following options are supported:
 *   -s : Print source and generated intermediate representation (IR).
 *   -o : Create executable file and terminate normally.
 *
 * If -o and -s are omitted, the compiled code is executed immediately (if
 * there were no compile errors) with the command line arguments passed
 * after the source file parameter.
 */

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>

/* 64-bit host support */
#if defined(__x86_64__) || defined(__aarch64__)
#define int long
#endif

char *freep, *p, *lp; // current position in source code
char *freedata, *data, *_data;   // data/bss pointer
char **scnames; // system call names

int *e, *le, *text;  // current position in emitted code
int *cas;            // case statement patch-up pointer
int *brks;           // break statement patch-up pointer
int *def;            // default statement patch-up pointer
int *tsize;          // array (indexed by type) of type sizes
int tnew;            // next available type
int tk;              // current token
int ival;            // current token value
int ty;              // current expression type
int loc;             // local variable offset
int line;            // current line number
int src;             // print source and assembly flag
int signed_char;     // use `signed char` for `char`
int elf;             // print ELF format
int *n;              // current position in emitted abstract syntax tree
                     // With an AST, the compiler is not limited to generate
                     // code on the fly with parsing.
                     // This capability allows function parameter code to be
                     // emitted and pushed on the stack in the proper
                     // right-to-left order.
int ld;              // local variable depth

// identifier
struct ident_s {
    int tk;          // type-id or keyword
    int hash;
    char *name;     // name of this identifier
    /* fields starting with 'h' were designed to save and restore
     * the global class/type/val in order to handle the case if a
     * function declares a local with the same name as a global.
     */
    int class, hclass; // FUNC, GLO (global var), LOC (local var), Syscall
    int type, htype;   // data type such as char and int
    int val, hval;
    int stype;
} *id,  // currently parsed identifier
  *sym; // symbol table (simple list of identifiers)

struct member_s {
    struct ident_s *id;
    int offset;
    int type;
    struct member_s *next;
} **members; // array (indexed by type) of struct member lists

// tokens and classes (operators last and in precedence order)
// ( >= 128 so not to collide with ASCII-valued tokens)
enum {
    Num = 128, // the character set of given source is limited to 7-bit ASCII
    Func, Syscall, Glo, Par, Loc, Id, Load, Enter,
    Break, Case, Char, Default, Else, Enum, If, Int, Return, Sizeof,
    Struct, Switch, For, While,
    Assign, // operator =, keep Assign as highest priority operator
    AddAssign, SubAssign, MulAssign, DivAssign, ModAssign, // +=, -=, *=, /=, %=
    Cond, // operator: ?
    Lor, Lan, Or, Xor, And, // operator: ||, &&, |, ^, &
    Eq, Ne, Lt, Gt, Le, Ge, // operator: ==, !=, <, >, <=, >=
    Shl, Shr, Add, Sub, Mul, Div, Mod, // operator: <<, >>, +, -, *, /, %
    Inc, Dec, Dot, Arrow, Brak, // operator: ++, --, ., ->, [
};

// opcodes
/* The instruction set is designed for building intermediate representation.
 * Expression 10 + 20 will be translated into the following instructions:
 *     i = 0;
 *     text[i++] = IMM;
 *     text[i++] = 10;
 *     text[i++] = PSH;
 *     text[i++] = IMM;
 *     text[i++] = 20;
 *     text[i++] = ADD;
 *     text[i++] = PSH;
 *     text[i++] = EXIT;
 *     pc = text;
 */
enum {
    LEA , /*  0 */
    /* LEA addressed the problem how to fetch arguments inside sub-function.
     * Let's check out what a calling frame looks like before learning how
     * to fetch arguments (Note that arguments are pushed in its calling
     * order):
     *
     *     sub_function(arg1, arg2, arg3);
     *
     *     |    ....       | high address
     *     +---------------+
     *     | arg: 1        |    new_bp + 4
     *     +---------------+
     *     | arg: 2        |    new_bp + 3
     *     +---------------+
     *     | arg: 3        |    new_bp + 2
     *     +---------------+
     *     |return address |    new_bp + 1
     *     +---------------+
     *     | old BP        | <- new BP
     *     +---------------+
     *     | local var 1   |    new_bp - 1
     *     +---------------+
     *     | local var 2   |    new_bp - 2
     *     +---------------+
     *     |    ....       |  low address
     *
     * If we need to refer to arg1, we need to fetch new_bp + 4, which can not
     * be achieved by restricted ADD instruction. Thus another special
     * instrcution is introduced to do this: LEA <offset>.
     * The following pseudocode illustrates how LEA works.
     *     if (op == LEA) { ax = (int) (bp + *pc++); } // load address for arguments
     * Together with JSR, ENT, ADJ, LEV, and LEA instruction, we are able to make
     * function calls.
     */

    IMM , /*  1 */
    /* IMM <num> to put immediate <num> into general register */

    JMP , /*  2 */
    /* JMP <addr> will unconditionally set the value PC register to <addr> */
    /* The following pseudocode illustrates how JMP works:
     *     if (op == JMP) { pc = (int *) *pc; } // jump to the address
     * Note that PC points to the NEXT instruction to be executed. Thus *pc
     * stores the argument of JMP instruction, i.e. the <addr>.
     */

    JSR , /*  3 */
    /* A function is a block of code, which may be far from the instruction
     * we are currently executing. That is reason why JMP instruction exists,
     * jumping into starting point of a function. JSR is introduced to perform
     * some bookkeeping: store the current execution position so that the
     * program can resume after function call returns.
     *
     * JSR <addr> to invoke the function whose starting point is <addr> and
     * LEV to fetch the bookkeeping information to resume previous execution.
     */

    BZ  , /*  4 : conditional jump if general register is zero */
    BNZ , /*  5 : conditional jump if general register is not zero */

    ENT , /*  6 */
    /* ENT <size> is called when we are about to enter the function call to
     * "make a new calling frame". It will store the current PC value onto
     * the stack, and save some space(<size> bytes) to store the local
     * variables for function.
     */

    ADJ , /*  7 */
    /* ADJ <size> is to adjust the stack, to "remove arguments from frame"
     * The following pseudocode illustrates how ADJ works:
     *     if (op == ADJ) { sp += *pc++; } // add esp, <size>
     */

    LEV , /*  8 */
    /* LEV fetches bookkeeping info to resume previous execution.
     * There is no POP instruction in our design, and the following pseudocode
     * illustrates how LEV works:
     *     if (op == LEV) { sp = bp; bp = (int *) *sp++;
     *                      pc = (int *) *sp++; } // restore call frame and PC
     */

    LI  , /*  9 */
    /* LI loads an integer into general register from a given memory
     * address which is stored in general register before execution.
     */

    LC  , /* 10 */
    /* LC loads a character into general register from a given memory
     * address which is stored in general register before execution.
     */

    SI  , /* 11 */
    /* SI stores the integer in general register into the memory whose
     * address is stored on the top of the stack.
     */

    SC  , /* 12 */
    /* SC stores the character in general register into the memory whose
     * address is stored on the top of the stack.
     */

    PSH , /* 13 */
    /* PSH pushes the value in general register onto the stack */

    OR  , /* 14 */  XOR , /* 15 */  AND , /* 16 */
    EQ  , /* 17 */  NE  , /* 18 */
    LT  , /* 19 */  GT  , /* 20 */  LE  , /* 21 */ GE  , /* 22 */
    SHL , /* 23 */  SHR , /* 24 */
    ADD , /* 25 */  SUB , /* 26 */  MUL , /* 27 */
    /* arithmetic instructions
     * Each operator has two arguments: the first one is stored on the top
     * of the stack while the second is stored in general register.
     * After the calculation is done, the argument on the stack will be poped
     * out and the result will be stored in general register.
     * So you are not able to fetch the first argument from the stack after
     * the calculation.
     */

    /* system call shortcuts */
    OPEN,READ,WRIT,CLOS,PRTF,MALC,FREE,MSET,MCMP,MCPY,MMAP,DSYM,BSCH,STRT,DLOP,DIV,MOD,EXIT,
    CLCA, /* clear cache, used by JIT compilation */
    INVALID
};

// types
enum { CHAR, INT, PTR = 256, PTR2 = 512 };

// ELF generation
char **plt_func_addr;

char *append_strtab(char **strtab, char *str)
{
    char *s;
    for (s = str; *s && (*s != ' '); s++) ; /* ignore trailing space */
    int nbytes = s - str + 1;
    char *res = *strtab;
    memcpy(res, str, nbytes);
    res[s - str] = 0; // null terminator
    *strtab = res + nbytes;
    return res;
}

/* parse next token
 * 1. store data into id and then set the id to current lexcial form
 * 2. set tk to appropriate type
 */
void next()
{
    char *pp;

    /* using loop to ignore whitespace characters, but characters that
     * cannot be recognized by the lexical analyzer are considered blank
     * characters, such as '@' and '$'.
     */
    while ((tk = *p)) {
        ++p;
        if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') ||
            (tk == '_')) {
            pp = p - 1;
            while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
                   (*p >= '0' && *p <= '9') || (*p == '_'))
                tk = tk * 147 + *p++; // 147 is the magic number generating hash value
            tk = (tk << 6) + (p - pp);  // hash plus symbol length
            // hash value is used for fast comparison. Since it is inaccurate,
            // we have to validate the memory content as well.
            for (id = sym; id->tk; id++) { // find one free slot in table
                if (tk == id->hash && /* if token is found (hash match), overwrite */
                    !memcmp(id->name, pp, p - pp)) {
                    tk = id->tk;
                    return;
                }
            }
            /* At this point, existing symbol name is not found.
             * "id" points to the first unused symbol table entry.
             */
            id->name = pp;
            id->hash = tk;
            tk = id->tk = Id;  // token type identifier
            return;
        }
        /* Calculate the constant */
        // first byte is a number, and it is considered a numerical value
        else if (tk >= '0' && tk <= '9') {
            /* Parse with 3 conditions:
             * 1) not starting with 0 :=> decimal number;
             * 2) starting with 0x :=> hex number;
             * 3) starting with 0: octal number;
             */
            if ((ival = tk - '0')) {
                while (*p >= '0' && *p <= '9')
                    ival = ival * 10 + *p++ - '0';
            }
            // first digit is 0 and it starts with 'x', and it is considered
            // to be a hexadecimal number
            else if (*p == 'x' || *p == 'X') {
                while ((tk = *++p) &&
                       ((tk >= '0' && tk <= '9') ||
                        (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F')))
                    ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
            }
            else { // considered octal
                while (*p >= '0' && *p <= '7')
                    ival = ival * 8 + *p++ - '0';
            }
            tk = Num; // token is numeric, return
            return;
        }
        switch (tk) {
        case '\n':
            /* Take an integer (representing an operation) and printing out
             * the name of that operation. First thing to say is that "* ++le"
             * is the integer representing the operation to perform.
             * This basically walks through the array of instructions
             * returning each integer in turn.
             *
             * Starting at the beginning of the line, we have "printf" with
             * a format string of "%8.4s". This means print out the first 4
             * characters of the string that we are about to pass next (padded
             * to 8 characters). There then follows a string containing all of
             * the operation names, in numerical order, padded to 4 characters
             * and separated by commas (so the start of each is 5 apart).
             *
             * Finally, we do a lookup into this string (treating it as an
             * array) at offset "* ++le * 5", i.e. the integer representing
             * the operation multipled by "5", being the number of characters
             * between the start of each operation name). Doing this lookup
             * gives us a char, but actually we wanted the pointer to this
             * char (as we want printf to print out this char and the
             * following 3 chars), so we take the address of this char
             * (the "&" at the beginning of the whole expression).
             */
            if (src) {
                printf("%d: %.*s", line, p - lp, lp);
                lp = p;
                while (le < e) {
                    printf("%8.4s",
                           & "LEA  IMM  JMP  JSR  BZ   BNZ  ENT  ADJ  LEV  "
                             "LI   LC   SI   SC   PSH  "
                             "OR   XOR  AND  EQ   NE   LT   GT   LE   GE   "
                             "SHL  SHR  ADD  SUB  MUL  "
                             "OPEN READ WRIT CLOS PRTF MALC FREE "
                             "MSET MCMP MCPY MMAP "
                             "DSYM BSCH STRT DLOP DIV  MOD  EXIT CLCA" [*++le * 5]);
                    if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n");
                }
            }
            ++line;
        case ' ':
        case '\t':
        case '\v':
        case '\f':
        case '\r':
            break;
        case '/':
            if (*p == '/') { // comment
        case '#': // skip #include statement, preprocessor directives ignored
                while (*p != 0 && *p != '\n') ++p;
            } else if (*p == '*') { // C-style multiline comments
                int t = 0;
                for (++p; (*p != 0) && (t == 0); ++p) {
                    pp = p + 1;
                    if (*p == '\n') line++;
                    else if (*p == '*' && *pp == '/') t = 1;
                }
                ++p;
            } else {
                if (*p == '=') { ++p; tk = DivAssign; }
                else tk = Div; return;
            }
            break;
        case '\'': // quotes start with character (string)
        case '"':
            pp = data;
            while (*p != 0 && *p != tk) {
                if ((ival = *p++) == '\\') {
                    switch (ival = *p++) {
                    case 'n': ival = '\n'; break; // new line
                    case 't': ival = '\t'; break; // horizontal tab
                    case 'v': ival = '\v'; break; // vertical tab
                    case 'f': ival = '\f'; break; // form feed
                    case 'r': ival = '\r'; break; // carriage return
                    case '0': ival = '\0'; break; // an int with value 0
                    }
                }
                // if it is double quotes, it is considered as a string,
                // copying characters to data
                if (tk == '"') *data++ = ival;
            }
            ++p;
            //  if .text too big rwdata v_addr will overlap it, add that to stay away from .text
            if (tk == '"') ival = (int) pp; else tk = Num;
            return;
        case '=': if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return;
        case '+': if (*p == '+') { ++p; tk = Inc; }
                  else if (*p == '=') { ++p; tk = AddAssign; }
                  else tk = Add; return;
        case '-': if (*p == '-') { ++p; tk = Dec; }
                  else if (*p == '>') { ++p; tk = Arrow; }
                  else if (*p == '=') { ++p; tk = SubAssign; }
                  else tk = Sub; return;
        case '!': if (*p == '=') { ++p; tk = Ne; } return;
        case '<': if (*p == '=') { ++p; tk = Le; }
                  else if (*p == '<') { ++p; tk = Shl; }
                  else tk = Lt; return;
        case '>': if (*p == '=') { ++p; tk = Ge; }
                  else if (*p == '>') { ++p; tk = Shr; }
                  else tk = Gt; return;
        case '|': if (*p == '|') { ++p; tk = Lor; }
                  else tk = Or; return;
        case '&': if (*p == '&') { ++p; tk = Lan; }
                  else tk = And; return;
        case '^': tk = Xor; return;
        case '*': if (*p == '=') { ++p; tk = MulAssign; }
                  else tk = Mul; return;
        case '%': if (*p == '=') { ++p; tk = ModAssign; }
                  else tk = Mod; return;
        case '[': tk = Brak; return;
        case '?': tk = Cond; return;
        case '.': tk = Dot; return;
        default: return;
        }
    }
}

char fatal(char *msg) { printf("%d: %s\n", line, msg); exit(-1); }

/* expression parsing
 * lev represents an operator
 * because each operator `token` is arranged in order of priority, so
 * large `lev` indicates a high priority.
 */
void expr(int lev)
{
    int otk;
    int t, *b, sz, *c;
    struct ident_s *d;
    struct member_s *m;

    switch (tk) {
    case 0: fatal("unexpected EOF in expression");
    // directly take an immediate value as the expression value
    // IMM recorded in emit sequence
    case Num: *--n = ival; *--n = Num; next(); ty = INT; break;
    case '"': // string, as a literal in data segment
        *--n = ival; *--n = Num; next();
        // continuous `"` handles C-style multiline text such as `"abc" "def"`
        while (tk == '"') next();
        /* Point "data" to next integer-aligned address.
         * e.g. "-sizeof(int)" is -4, i.e. 0b11111100.
         * This guarantees to leave at least one '\0' after the string.
         *
         * append the end of string character '\0', all the data is defaulted
         * to 0, so just move data one position forward. Specify result value
         * type to char pointer. CHAR + PTR = PTR because CHAR is 0.
         */
        data = (char *) (((int) data + sizeof(int)) & (-sizeof(int)));
        ty = PTR;
        break;
    /* SIZEOF_expr -> 'sizeof' '(' 'TYPE' ')'
     * sizeof is actually an unary operator.
     * now only `sizeof(int)`, `sizeof(char)` and `sizeof(*...)` are supported.
     * FIXME: not support "sizeof (Id)".
     * In second line will not get next token, match ')' will fail.
     */
    case Sizeof:
        next();
        if (tk == '(') next();
        else fatal("open paren expected in sizeof");
        ty = INT;
        switch (tk) {
        case Int: next(); break;
        case Char: next(); ty = CHAR; break;
        case Struct:
            next();
            if (tk != Id) fatal("bad struct type");
            ty = id->stype; next(); break;
        }
        // multi-level pointers, plus `PTR` for each level
        while (tk == Mul) { next(); ty += PTR; }
        if (tk == ')') next();
        else fatal("close paren expected in sizeof");
        *--n = ty >= PTR ? sizeof(int) : tsize[ty]; *--n = Num;
        ty = INT;
        break;
    case Id:
        d = id; next();
        // function call
        if (tk == '(') {
            if (d->class != Syscall && d->class != Func)
                fatal("bad function call");
            next();
            t = 0; b = 0; // parameters count
            while (tk != ')') {
                expr(Assign); *--n = (int) b; b = n; ++t;
                if (tk == ',') {
                    next();
                    if (tk == ')') fatal("unexpected comma in function call");
		} else if (tk != ')') fatal("missing comma in function call");
            }
            next();
            // function or system call id
            *--n = t; *--n = d->val; *--n = (int) b; *--n = d->class;
            ty = d->type;
        }
        // enumeration, only enums have ->class == Num
        else if (d->class == Num) { *--n = d->val; *--n = Num; ty = INT; }
        else {
            // Variable get offset
            switch (d->class) {
            case Loc: case Par: *--n = loc - d->val; *--n = Loc; break;
            case Glo: *--n = d->val; *--n = Num; break;
            default: fatal("undefined variable");
            }
            *--n = ty = d->type; *--n = Load;
        }
        break;
    // Type cast or parenthesis
    case '(':
        next();
        if (tk == Int || tk == Char || tk == Struct) {
            switch (tk) {
            case Int: next(); t = INT; break;
            case Char: next(); t = CHAR; break;
            default:
                next();
                if (tk != Id) fatal("bad struct type");
                t = id->stype; next(); break;
            }
            // t: pointer
            while (tk == Mul) { next(); t += PTR; }
            if (tk == ')') next();
            else fatal("bad cast");
            expr(Inc); // cast has precedence as Inc(++)
            ty = t;
        }
        else {
            expr(Assign);
            if (tk == ')') next();
            else fatal("close paren expected");
        }
        break;
    case Mul: // "*", dereferencing the pointer operation
        next();
        expr(Inc); // dereference has the same precedence as Inc(++)
        if (ty >= PTR) ty -= PTR;
        else fatal("bad dereference");
        if (ty >= CHAR && ty <= PTR) {
            *--n = ty; *--n = Load;
        } else fatal("unexpected type");
        break;
    case And: // "&", take the address operation
        /* when "token" is a variable, it takes the address first and
         * then LI/LC, so `--e` becomes the address of "a".
         */
        next(); expr(Inc);
        if (*n == Load) n += 2; else fatal("bad address-of");
        ty += PTR;
        break;
    case '!': // "!x" is equivalent to "x == 0"
        next(); expr(Inc);
        if (*n == Num) n[1] = !n[1];
        else { *--n = 0; *--n = Num; --n; *n = (int) (n + 3); *--n = Eq; }
        ty = INT;
        break;
    case '~': // "~x" is equivalent to "x ^ -1"
        next(); expr(Inc);
        if (*n == Num) n[1] = ~n[1];
        else { *--n = -1; *--n = Num; --n; *n = (int) (n + 3); *--n = Xor; }
        ty = INT;
        break;
    case Add:
        next(); expr(Inc); ty = INT;
        break;
    case Sub:
        next();
        expr(Inc);
        if (*n == Num) n[1] = -n[1];
        else { *--n = -1; *--n = Num; --n; *n = (int) (n + 3); *--n = Mul; }
        ty = INT;
        break;
    case Div:
    case Mod:
        break;
    // processing ++x and --x. x-- and x++ is handled later
    case Inc:
    case Dec:
        t = tk; next(); expr(Inc);
        if (*n == Load) *n = t; else fatal("bad lvalue in pre-increment");
        break;
    default: fatal("bad expression");
    }

    // "precedence climbing" or "Top Down Operator Precedence" method
    while (tk >= lev) {
        // tk is ASCII code will not exceed `Num=128`. Its value may be changed
        // during recursion, so back up currently processed expression type
        t = ty; b = n;
        switch (tk) {
        case Assign:
            next();
            // the left part is processed by the variable part of `tk=ID`
            // and pushes the address
            if (*n != Load) { fatal("bad lvalue in assignment");}
            // get the value of the right part `expr` as the result of `a=expr`
            expr(Assign); *--n = (int) (b + 2); *--n = ty = t; *--n = Assign;
            break;
        case AddAssign: // right associated
        case SubAssign:
        case MulAssign:
        case DivAssign:
        case ModAssign:
            otk = tk;
            *--n=';'; *--n = ty = id->type; *--n = Load;
            sz = (ty = t) >= PTR2 ? sizeof(int) :
                                    ty >= PTR ? tsize[ty - PTR] : 1;
            next(); c = n; expr(otk);
            if (*n == Num) n[1] *= sz;
            *--n = (int) c; *--n = Add + (otk - AddAssign);
            *--n = (int) (b + 2); *--n = ty = t; *--n = Assign;
            ty = INT;
            break;
        case Cond: // `x?a:b` is similar to if except that it relies on else
            next(); expr(Assign);
            if (tk == ':') next();
            else fatal("conditional missing colon"); c = n;
            expr(Cond); --n;
            *n = (int) (n + 1); *--n = (int) c; *--n = (int) b; *--n = Cond;
            break;
        case Lor: // short circuit, the logical or
            next(); expr(Lan);
            if (*n == Num && *b == Num) n[1] = b[1] || n[1];
            else { *--n = (int) b; *--n = Lor; }
            ty = INT;
            break;
        case Lan: // short circuit, logic and
            next(); expr(Or);
            if (*n == Num && *b == Num) n[1] = b[1] && n[1];
            else { *--n = (int) b; *--n = Lan; }
            ty = INT;
            break;
        case Or: // push the current value, calculate the right value
            next(); expr(Xor);
            if (*n == Num && *b == Num) n[1] = b[1] | n[1];
            else { *--n = (int) b; *--n = Or; }
            ty = INT;
            break;
        case Xor:
            next(); expr(And);
            if (*n == Num && *b == Num) n[1] = b[1] ^ n[1];
            else { *--n = (int) b; *--n = Xor; }
            ty = INT;
            break;
        case And:
            next(); expr(Eq);
            if (*n == Num && *b == Num) n[1] = b[1] & n[1];
            else { *--n = (int) b; *--n = And; }
            ty = INT;
            break;
        case Eq:
            next(); expr(Lt);
            if (*n == Num && *b == Num) n[1] = b[1] == n[1];
            else { *--n = (int) b; *--n = Eq; }
            ty = INT;
            break;
        case Ne:
            next(); expr(Lt);
            if (*n == Num && *b == Num) n[1] = b[1] != n[1];
	    else { *--n = (int) b; *--n = Ne; }
            ty = INT;
            break;
        case Lt:
            next(); expr(Shl);
            if (*n == Num && *b == Num) n[1] = b[1] < n[1];
            else { *--n = (int) b; *--n = Lt; }
            ty = INT;
            break;
        case Gt:
            next(); expr(Shl);
            if (*n == Num && *b == Num) n[1] = b[1] > n[1];
            else { *--n = (int) b; *--n = Gt; }
            ty = INT;
            break;
        case Le:
            next(); expr(Shl);
            if (*n == Num && *b == Num) n[1] = b[1] <= n[1];
            else { *--n = (int) b; *--n = Le; }
            ty = INT;
            break;
        case Ge:
            next(); expr(Shl);
            if (*n == Num && *b == Num) n[1] = b[1] >= n[1];
            else { *--n = (int) b; *--n = Ge; }
            ty = INT;
            break;
        case Shl:
            next(); expr(Add);
            if (*n == Num && *b == Num) {
                if (n[1] < 0) n[1] = b[1] >> -n[1];
                else n[1] = b[1] << n[1];
            } else { *--n = (int) b; *--n = Shl; }
            ty = INT;
            break;
        case Shr:
            next(); expr(Add);
            if (*n == Num && *b == Num) {
                if (n[1] < 0) n[1] = b[1] << -n[1];
                else n[1] = b[1] >> n[1];
            } else { *--n = (int) b; *--n = Shr; }
            ty = INT;
            break;
        case Add:
            next(); expr(Mul);
            sz = (ty = t) >= PTR2 ? sizeof(int) :
                                    ty >= PTR ? tsize[ty - PTR] : 1;
            if (*n == Num) n[1] *= sz;
            if (*n == Num && *b == Num) n[1] += b[1];
            else { *--n = (int) b; *--n = Add; }
            break;
        case Sub:
            next(); expr(Mul);
            sz = t >= PTR2 ? sizeof(int) : t >= PTR ? tsize[t - PTR] : 1;
            if (sz > 1 && *n == Num) {
                *--n = sz; *--n = Num; --n; *n = (int) (n + 3); *--n = Mul;
            }
            if (*n == Num && *b == Num) n[1] -= b[1];
            else {
                *--n = (int) b; *--n = Sub;
                if (t == ty && sz > 1) {
                    switch (sz) {
                    case 4: *--n = 2; *--n = Num; --n; *n = (int) (n + 3);
                            *--n = Shr; break;
                    default: *--n = sz; *--n = Num; --n; *n = (int) (n + 3);
                             *--n = Sub;
                    }
                }
            }
            break;
        case Mul:
            next(); expr(Inc);
            if (*n == Num && *b == Num) n[1] *= b[1];
            else { *--n = (int) b; *--n = Mul; }
            ty = INT;
            break;
        case Inc:
        case Dec:
            sz = ty >= PTR2 ? sizeof(int) : ty >= PTR ? tsize[ty - PTR] : 1;
            if (*n == Load) *n = tk; else fatal("bad lvalue in post-increment");
            *--n = sz; *--n = Num;
            *--n = (int) b; *--n = (tk == Inc) ? Sub : Add;
            next();
            break;
        case Div:
            next(); expr(Inc);
            if (*n == Num && *b == Num) n[1] /= b[1];
            else { *--n = (int) b; *--n = Div; }
            ty = INT;
            break;
        case Mod:
            next(); expr(Inc);
            if (*n == Num && *b == Num) n[1] %= b[1];
            else { *--n = (int) b; *--n = Mod; }
            ty = INT;
            break;
        case Dot:
            ty += PTR;
        case Arrow:
            if (ty <= PTR+INT || ty >= PTR2) fatal("structure expected");
            next();
            if (tk != Id) fatal("structure member expected");
            m = members[ty - PTR]; while (m && m->id != id) m = m->next;
            if (!m) fatal("structure member not found");
            if (m->offset) {
                *--n = m->offset; *--n = Num; --n; *n = (int) (n + 3);
                *--n = Add;
            }
            ty = m->type;
            if (ty <= INT || ty >= PTR) *--n = (ty == CHAR) ? CHAR : INT;
            *--n = Load;
            next();
            break;
        case Brak:
            next(); expr(Assign);
            if (tk == ']') next();
            else fatal("close bracket expected");
            if (t < PTR) fatal("pointer type expected");
            sz = (t = t - PTR) >= PTR ? sizeof(int) : tsize[t];
            if (sz > 1) {
                if (*n == Num) n[1] *= sz;
                else {
                    *--n = sz; *--n = Num; --n; *n = (int) (n + 3); *--n = Mul;
                }
            }
            if (*n == Num && *b == Num) n[1] += b[1];
            else { *--n = (int) b; *--n = Add; }
            if ((ty = t) <= INT || ty >= PTR)
                *--n = (ty == CHAR) ? CHAR : INT;
            *--n = Load;
            break;
        default:
            printf("%d: compiler error tk=%d\n", line, tk); exit(-1);
        }
    }
}

// AST parsing for IR generatiion
// With a modular code generator, new targets can be easily supported such as
// native Arm machine code.
void gen(int *n)
{
    int i = *n, j, k, l;
    int *a, *b, *c, *d;

    switch (i) {
    case Num: // get the value of integer
        *++e = IMM; *++e = n[1];
        break;
    case Loc: // get the value of variable
        *++e = LEA; *++e = n[1];
        break;
    case Load:
        gen(n + 2); // load the value
        if (n[1] <= INT || n[1] >= PTR) { *++e = (n[1] == CHAR) ? LC : LI; }
        break;
    case Assign: // assign the value to variables
        gen((int *) n[2]); *++e = PSH; gen(n + 3);
        // Add SC/SI instruction to save value in register to variable address
        // held on stack.
        *++e = (n[1] == CHAR) ? SC : SI;
        break;
    // increment or decrement variables
    case Inc:
    case Dec:
        gen(n + 2);
        *++e = PSH; *++e = (n[1] == CHAR) ? LC : LI; *++e = PSH;
        *++e = IMM; *++e = (n[1] >= PTR2) ? sizeof(int) :
                                            n[1] >= PTR ? tsize[n[1] - PTR] : 1;
        *++e = (i == Inc) ? ADD : SUB;
        *++e = (n[1] == CHAR) ? SC : SI;
        break;
    case Cond: // if else condition case
        gen((int *) n[1]); // condition
        // Add jump-if-zero instruction "BZ" to jump to false branch.
        // Point "b" to the jump address field to be patched later.
        *++e = BZ; b = ++e;
        gen((int *) n[2]); // expression
	// Patch the jump address field pointed to by "b" to hold the address
        // of false branch. "+ 3" counts the "JMP" instruction added below.
        //
        // Add "JMP" instruction after true branch to jump over false branch.
        // Point "b" to the jump address field to be patched later.
        if (n[3]) {
            *b = (int) (e + 3); *++e = JMP; b = ++e; gen((int *) n[3]);
        } // else statment
        // Patch the jump address field pointed to by "d" to hold the address
        // past the false branch.
        *b = (int) (e + 1);
        break;
    // operators
    /* If current token is logical OR operator:
     * Add jump-if-nonzero instruction "BNZ" to implement short circuit.
     * Point "b" to the jump address field to be patched later.
     * Parse RHS expression.
     * Patch the jump address field pointed to by "b" to hold the address past
     * the RHS expression.
     */
    case Lor:  gen((int *) n[1]); *++e = BNZ;
               b = ++e; gen(n + 2); *b = (int) (e + 1); break;
    case Lan:  gen((int *) n[1]); *++e = BZ;
               b = ++e; gen(n + 2); *b = (int) (e + 1); break;
    /* If current token is bitwise OR operator:
     * Add "PSH" instruction to push LHS value in register to stack.
     * Parse RHS expression.
     * Add "OR" instruction to compute the result.
     */
    case Or:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = OR; break;
    case Xor:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = XOR; break;
    case And:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = AND; break;
    case Eq:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = EQ; break;
    case Ne:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = NE; break;
    case Lt:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = LT; break;
    case Gt:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = GT; break;
    case Le:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = LE; break;
    case Ge:   gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = GE; break;
    case Shl:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = SHL; break;
    case Shr:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = SHR; break;
    case Add:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = ADD; break;
    case Sub:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = SUB; break;
    case Mul:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = MUL; break;
    case Div:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = DIV; break;
    case Mod:  gen((int *) n[1]); *++e = PSH; gen(n + 2); *++e = MOD; break;
    case Syscall:
    case Func:
        c = b = (int *) n[1]; k = 0; l = 1;
        // how many parameters
        while (b && l) { ++k; if (!(int *) *b) l = 0; else b = (int *) *b; }
        j = 0; a = malloc(sizeof(int *) * k); b = c; l = 1;
        while (b && l) {
            a[j] = (int) b;
            if (!(int *) *b) l = 0; else b = (int *) *b; ++j;
        }
        if (j > 0) --j;
        // push parameters
        while (j >= 0 && k > 0) {
            gen(b + 1); *++e = PSH; --j; b = (int *) a[j];
        }
        free(a);
        if (i == Func) *++e = JSR; *++e = n[2];
        if (n[3]) { *++e = ADJ; *++e = n[3]; }
        break;
    case While:
        d = (e + 1);
        gen((int *) n[1]); // condition
        *++e = BZ; b = ++e;
        gen(n + 2); // expression
        *++e = JMP; *++e = (int) d;
        *b = (int) (e + 1);
        break;
    case For:
        gen((int *) n[4]);
        a = (e + 1);
        gen((int *) n[1]);
        *++e = BZ; d = ++e;
        *++e = JMP; c = ++e;
        b = (e + 1);
        gen((int *) n[2]); // area b
        *++e = JMP; *++e = (int) a;
        *c = (int) (e + 1);
        gen((int *) n[3]); // area c
        *++e = JMP; *++e = (int) b;
        *d = (int) (e + 1);
        break;
    case Switch:
        gen((int *) n[1]); // condition
        a = cas; *++e = JMP; cas = ++e;
        b = brks; d = def; brks = def = 0;
        gen((int *) n[2]); // case statment
        // deal with no default inside switch case
        *cas = def ? (int) def : (int) (e + 1); cas = a;
        while (brks) { a = (int *) * brks; *brks = (int) (e + 1); brks = a; }
        brks = b; def = d;
        break;
    case Case:
        *++e = JMP; ++e;
        a = 0;
        *e = (int) (e + 7); *++e = PSH; i = *cas; *cas = (int) e;
        gen((int *) n[1]); // condition
        if (e[-1] != IMM) fatal("bad case immediate");
        *++e = SUB; *++e = BNZ; cas = ++e; *e = i + e[-3];
        if (*(int *) n[2] == Switch) a = cas;
        gen((int *) n[2]); // expression
        if (a != 0) cas = a;
        break;
    case Break:
        // set jump locate
        *++e = JMP; *++e = (int) brks; brks = e;
        break;
    case Default:
        def = e + 1;
        gen((int *) n[1]); break;
    case Return:
        if (n[1]) gen((int *) n[1]); *++e = LEV; break; // parse return AST
    case '{':
        // parse expression or statment from AST
        gen((int *) n[1]); gen(n + 2); break;
    case Enter: *++e = ENT; *++e = n[1]; gen(n + 2);
                if (*e != LEV) *++e = LEV; break;
    default:
        if (i != ';') {
            printf("%d: compiler error gen=%d\n", line, i); exit(-1);
        }
    }
}

// statement parsing (syntax analysis, except for declarations)
void stmt(int ctx)
{
    int *a, *b, *c, *d, *f;
    int i, j;
    int bt, ty;
    struct member_s *m;

    switch (tk) {
    case Enum:
        next();
        // If current token is not "{", it means having enum type name.
        // Skip the enum type name.
        if (tk != '{') next();
        if (tk == '{') {
            next();
            i = 0; // Enum value starts from 0
            while (tk != '}') {
                // Current token should be enum name.
                // If current token is not identifier, stop parsing.
                if (tk != Id) fatal("bad enum identifier");
                next();
                if (tk == Assign) {
                    next();
                    expr(Cond);
                    if (*n != Num) fatal("bad enum initializer");
                    i = n[1]; // Set enum value
                }
                /* "id" is pointing to the enum name's symbol table entry.
                 * Set the symbol table entry's symbol type be "Num".
                 * Set the symbol table entry's associated value type be "INT".
                 * Set the symbol table entry's associated value be enum value.
                 */
                id->class = Num; id->type = INT; id->val = i++;
                if (tk == ',') next(); // If current token is ",", skip.
            }
            next(); // Skip "}"
        } else if (tk == Id) {
            id->type = INT; id->class = ctx; id->val = ld++;
            next();
        }
        return;
    case Int:
    case Char:
    case Struct:
        switch (tk) {
        case Struct:
            next();
            if (tk == Id) {
                if (!id->stype) id->stype = tnew++;
                bt = id->stype;
                next();
            } else {
                bt = tnew++;
            }
            if (tk == '{') {
                next();
                if (members[bt]) fatal("duplicate structure definition");
                i = 0;
                while (tk != '}') {
                    int mbt = INT;
                    switch (tk) {
                    case Int: next(); break;
                    case Char: next(); mbt = CHAR; break;
                    case Struct:
                        next();
                        if (tk != Id) fatal("bad struct declaration");
                        mbt = id->stype;
                        next(); break;
                    }
                    while (tk != ';') {
                        ty = mbt;
                        // if the beginning of * is a pointer type,
                        // then type plus `PTR` indicates what kind of pointer
                        while (tk == Mul) { next(); ty += PTR; }
                        if (tk != Id) fatal("bad struct member definition");
                        m = malloc(sizeof(struct member_s));
                        m->id = id;
                        m->offset = i;
                        m->type = ty;
                        m->next = members[bt];
                        members[bt] = m;
                        i += (ty >= PTR) ? sizeof(int) : tsize[ty];
                        i = (i + 3) & -4;
                        next();
                        if (tk == ',') next();
                    }
                    next();
                }
                next();
                tsize[bt] = i;
            }
            break;
        case Int:
        case Char:
            bt = (tk == Int) ? INT : CHAR; // basetype
            next();
            break;
        }
        /* parse statemanet such as 'int a, b, c;'
         * "enum" finishes by "tk == ';'", so the code below will be skipped.
         * While current token is not statement end or block end.
         */
        while (tk != ';' && tk != '}' && tk != ',' && tk != ')') {
            ty = bt;
            // if the beginning of * is a pointer type, then type plus `PTR`
            // indicates what kind of pointer
            while (tk == Mul) { next(); ty += PTR; }
            switch (ctx) {
            case Glo:
                if (tk != Id) fatal("bad global declaration");
                if (id->class >= ctx) fatal("duplicate global definition");
                break;
            case Loc:
                if (tk != Id) fatal("bad local declaration");
                if (id->class >= ctx) fatal("duplicate local definition");
                break;
            }
            next();
            id->type = ty;
            if (tk == '(') { // function
                if (ctx != Glo) fatal("nested function");
                id->class = Func; // type is function
                // "+ 1" is because the code to add instruction always uses "++e".
                id->val = (int) (e + 1); // function Pointer? offset/address
                id->type = ty;
                next(); ld = 0; // "ld" is parameter's index.
                while (tk != ')') { stmt(Par); if (tk == ',') next(); }
                next();
                if (tk != '{') fatal("bad function definition");
                loc = ++ld;
                next();
                // Not declare and must not be function, analyze inner block.
                // e represents the address which will store pc
                // (ld - loc) indicates memory size to allocate
                *--n = ';';
                while (tk != '}') {
                    int *t = n; stmt(Loc);
                    if (t != n) { *--n = (int) t; *--n = '{'; }
                }
                *--n = ld - loc; *--n = Enter;
                cas = 0;
                gen(n);
                id = sym; // unwind symbol table locals
                while (id->tk) {
                    if (id->class == Loc || id->class == Par) {
                        id->class = id->hclass;
                        id->type = id->htype;
                        id->val = id->hval;
                    }
                    id++;
                }
            }
            else {
                id->hclass = id->class; id->class = ctx;
                id->htype = id->type; id->type = ty;
                id->hval = id->val;
                if (ctx == Glo) { id->val = (int) data; data += sizeof(int); }
                else if (ctx == Loc) { id->val = ++ld; }
                else if (ctx == Par) { id->val = ld++; }
                if (ctx == Loc && tk == Assign) {
                    int ptk = tk;
                    *--n = loc - id->val; *--n = Loc;
                    next(); a = n; expr(ptk);
                    *--n = (int)a; *--n = ty; *--n = Assign;
                }
            }
            if (ctx != Par && tk == ',') next();
        }
        return;
    /* if (...) <statement> [else <statement>]
     *     if (...)           <cond>
     *                        JZ a
     *         <statement>    <statement>
     *     else:              JMP b
     * a:
     *     <statement>        <statement>
     * b:                     b:
     */
    case If:
        next();
        if (tk == '(') next();
        else fatal("open paren expected");
        expr(Assign); a = n;
        if (tk == ')') next();
        else fatal("close paren expected");
        stmt(ctx);
        b = n;
        if (tk == Else) { next(); stmt(ctx); d = n; } else d = 0;
        *--n = (int)d; *--n = (int) b; *--n = (int) a; *--n = Cond;
        return;
    /* while (...) <statement>
     * a:                     a:
     *     while (<cond>)         <cond>
     *                            JZ b
     *         <statement>        <statement>
     *                            JMP a
     * b:                     b:
     */
    case While:
        next();
        if (tk == '(') next();
        else fatal("open paren expected");
        expr(Assign); a = n;
        if (tk == ')') next();
        else fatal("close paren expected");
        stmt(ctx); // parse body of "while"
        *--n = (int) a; *--n = While;
        return;
    case Switch:
        i = 0; j = 0;
        if (cas) j = (int) cas;
        cas = &i;
        next();
        if (tk == '(') next();
        else fatal("open paren expected");
        expr(Assign);
        a = n;
        if (tk == ')') next();
        else fatal("close paren expected");
        stmt(ctx);
        b = n;
        *--n = (int) b; *--n = (int) a; *--n = Switch;
        if (j) cas = (int *) j;
        return;
    case Case:
        i = *cas;
        next();
        expr(Or);
        a = n;
        if (*n != Num) fatal("bad case immediate");
        j = n[1]; n[1] -= i; *cas = j;
        *--n = ';';
        if (tk == ':') next();
        else fatal("colon expected");
        stmt(ctx);
        b = n;
        *--n = (int) b;*--n = (int) a; *--n = Case;
        return;
    case Break:
        next();
        if (tk == ';') next();
        else fatal("semicolon expected");
        *--n = Break;
        return;
    case Default:
        next();
        if (tk == ':') next();
        else fatal("colon expected");
        stmt(ctx); a = n;
        *--n = (int) a; *--n = Default;
        return;
    // RETURN_stmt -> 'return' expr ';' | 'return' ';'
    case Return:
        a = 0; next();
        if (tk != ';') { expr(Assign); a = n; }
        *--n = (int) a; *--n = Return;
        if (tk == ';') next();
        else fatal("semicolon expected");
        return;
    /* For iteration is implemented as:
     * Init -> Cond -> Bz to end -> Jmp to Body
     * After -> Jmp to Cond -> Body -> Jmp to After
     */
    case For:
        next();
        if (tk == '(') next();
        else fatal("open paren expected");
        *--n = ';';
        expr(Assign);
        while (tk == ',') {
            f = n; next(); expr(Assign); *-- n = (int) f; *--n = '{';
        }
        d = n;
        if (tk == ';') next();
        else fatal("semicolon expected");
        *--n = ';';
        expr(Assign); a = n; // Point to entry of for cond
        if (tk == ';') next();
        else fatal("semicolon expected");
        *--n = ';';
        expr(Assign);
        while (tk == ',') {
            f = n; next(); expr(Assign); *-- n = (int) f; *--n = '{';
        }
        b = n;
        if (tk == ')') next(); else fatal("close paren expected");
        stmt(ctx); c = n;
        *--n = (int) d; *--n = (int) c; *--n = (int) b; *--n = (int) a;
        *--n = For;
        return;
    // stmt -> '{' stmt '}'
    case '{':
        next();
        *--n = ';';
        while (tk != '}') { a = n; stmt(ctx); *--n = (int) a; *--n = '{'; }
        next();
        return;
    // stmt -> ';'
    case ';':
        next();
        *--n = ';';
        return;
    default:
        // general statements are considered assignment statements/expressions
        expr(Assign);
        if (tk == ';') next(); else fatal("semicolon expected");
    }
}

void die(char *msg) { printf("%s\n", msg); exit(-1); }

int reloc_imm(int offset) { return ((((offset) - 8) >> 2) & 0x00ffffff); }
int reloc_bl(int offset) { return 0xeb000000 | reloc_imm(offset); }

int *codegen(int *jitmem, int *jitmap)
{
    int i, tmp;
    int *je, *tje;    // current position in emitted native code
    int *immloc, *il;

    immloc = il = malloc(1024 * 4);
    int *iv = malloc(1024 * 4);
    int *imm0 = 0;
    int genpool = 0;

    // first pass: emit native code
    int *pc = text + 1; je = jitmem; line = 0;
    while (pc <= e) {
        i = *pc;
        // Store mapping from IR index to native instruction buffer location
        // "pc - text" gets the index of IR.
        // "je" points to native instruction buffer's current location.
        jitmap[((int) pc++ - (int) text) >> 2] = (int) je;
        switch (i) {
        case LEA:
            tmp = *pc++;
            if (tmp >= 64 || tmp <= -64) {
                printf("jit: LEA %d out of bounds\n", tmp); exit(6);
            }
            if (tmp >= 0)
                *je++ = 0xe28b0000 | tmp * 4;    // add     r0, fp, #(tmp)
            else
                *je++ = 0xe24b0000 | (-tmp) * 4; // sub     r0, fp, #(tmp)
            break;
        case IMM:
            tmp = *pc++;
            if (0 <= tmp && tmp < 256)
                *je++ = 0xe3a00000 + tmp;        // mov r0, #(tmp)
            else { if (!imm0) imm0 = je; *il++ = (int) (je++); *iv++ = tmp; }
            break;
        case JSR:
        case JMP:
            pc++; je++; // postponed till second pass
            break;
        case BZ:
        case BNZ:
            *je++ = 0xe3500000; pc++; je++;      // cmp r0, #0
            break;
        case ENT:
            *je++ = 0xe92d4800; *je++ = 0xe28db000; // push {fp, lr}; add  fp, sp, #0
            tmp = *pc++; if (tmp) *je++ = 0xe24dd000 | (tmp * 4); // sub  sp, sp, #(tmp * 4)
            if (tmp >= 64 || tmp < 0) {
                printf("jit: ENT %d out of bounds\n", tmp); exit(6);
            }
            break;
        case ADJ:
            *je++ = 0xe28dd000 + *pc++ * 4;      // add sp, sp, #(tmp * 4)
            break;
        case LEV:
            *je++ = 0xe28bd000; *je++ = 0xe8bd8800; // add sp, fp, #0; pop {fp, pc}
            break;
        case LI:
            *je++ = 0xe5900000;                  // ldr r0, [r0]
            break;
        case LC:
            *je++ = 0xe5d00000; if (signed_char)  *je++ = 0xe6af0070; // ldrb r0, [r0]; (sxtb r0, r0)
            break;
        case SI:
            *je++ = 0xe49d1004; *je++ = 0xe5810000; // pop {r1}; str r0, [r1]
            break;
        case SC:
            *je++ = 0xe49d1004; *je++ = 0xe5c10000; // pop {r1}; strb r0, [r1]
            break;
        case PSH:
            *je++ = 0xe52d0004;                       // push {r0}
            break;
        case OR:
            *je++ = 0xe49d1004; *je++ = 0xe1810000; // pop {r1}; orr r0, r1, r0
            break;
        case XOR:
            *je++ = 0xe49d1004; *je++ = 0xe0210000; // pop {r1}; eor r0, r1, r0
            break;
        case AND:
            *je++ = 0xe49d1004; *je++ = 0xe0010000; // pop {r1}; and r0, r1, r0
            break;
        case SHL:
            *je++ = 0xe49d1004; *je++ = 0xe1a00011; // pop {r1}; lsl r0, r1, r0
            break;
        case SHR:
            *je++ = 0xe49d1004; *je++ = 0xe1a00051; // pop {r1}; asr r0, r1, r0
            break;
        case ADD:
            *je++ = 0xe49d1004; *je++ = 0xe0800001; // pop {r1}; add r0, r0, r1
            break;
        case SUB:
            *je++ = 0xe49d1004; *je++ = 0xe0410000; // pop {r1}; sub r0, r1, r0
            break;
        case MUL:
            *je++ = 0xe49d1004; *je++ = 0xe0000091; // pop {r1}; mul r0, r1, r0
            break;
        case DIV:
        case MOD:
            *je++ = 0xe52d0004;                     // push {r0}
            if (elf) {
                tmp = (int) plt_func_addr[i - OPEN];
            } else {
                void *handle = dlopen("libgcc_s.so.1", 1);
                if (!handle) fatal("libgcc_s.so.1 open error!");
                tmp = (int) dlsym(handle, scnames[i - OPEN]);
            }
            *je++ = 0xe49d0004 | (1 << 12); // pop r1
            *je++ = 0xe49d0004 | (0 << 12); // pop r0
            *je++ = 0xe28fe000;                          // add lr, pc, #0
            if (!imm0) imm0 = je;
            *il++ = (int) je++ + 1;
            *iv++ = tmp;
            break;
        case CLCA:
            *je++ = 0xe59d0004; *je++ = 0xe59d1000; // ldr r0, [sp, #4]
                                                    // ldr r1, [sp]
            *je++ = 0xe3a0780f; *je++ = 0xe2877002; // mov r7, #0xf0000
                                                    // add r7, r7, #2
            *je++ = 0xe3a02000; *je++ = 0xef000000; // mov r2, #0
                                                    // svc 0
            break;
        default:
            if (EQ <= i && i <= GE) {
                *je++ = 0xe49d1004; *je++ = 0xe1510000; // pop {r1}; cmp r1, r0
                if (i <= NE) { je[0] = 0x03a00000; je[1] = 0x13a00000; }   // moveq r0, #0; movne r0, #0
                else if (i == LT || i == GE) { je[0] = 0xb3a00000; je[1] = 0xa3a00000; } // movlt r0, #0; movge   r0, #0
                else { je[0] = 0xc3a00000; je[1] = 0xd3a00000; }           // movgt r0, #0; movle r0, #0
                if (i == EQ || i == LT || i == GT) je[0] = je[0] | 1;
                else je[1] = je[1] | 1;
                je += 2;
                break;
            }
            else if (i >= OPEN && i <= EXIT) {
                tmp = (int) (elf ? plt_func_addr[i - OPEN] :
                                   dlsym(0, scnames[i - OPEN]));
                if (*pc++ != ADJ) die("codegen: no ADJ after native proc");
                i = *pc;
                if (i > 10) die("codegen: no support for 10+ arguments");
                while (i > 0) *je++ = 0xe49d0004 | (--i << 12); // pop r(i-1)
                i = *pc++;
                if (i > 4) *je++ = 0xe92d03f0;               // push {r4-r9}
                *je++ = 0xe28fe000;                          // add lr, pc, #0
                if (!imm0) imm0 = je;
                *il++ = (int) je++ + 1;
                *iv++ = tmp;
                if (i > 4) *je++ = 0xe28dd018;              // add sp, sp, #24
                break;
            }
            else {
                printf("code generation failed for %d!\n", i);
                free(iv);
                return 0;
            }
        }

        if (imm0) {
            if (i == LEV) genpool = 1;
            else if ((int) je > (int) imm0 + 3000) {
                tje = je++; genpool = 2;
            }
        }
        if (genpool) {
            *iv = 0;
            while (il > immloc) {
                tmp = *--il;
                if ((int) je > tmp + 4096 + 8) die("codegen: can't reach the pool");
                iv--; if (iv[0] == iv[1]) je--;
                if (tmp & 1) {
                    // ldr pc, [pc, #..]
                    *(int *) (tmp - 1) = 0xe59ff000 | ((int) je - tmp - 7);
                } else {
                    // ldr r0, [pc, #..]
                    *(int *) tmp = 0xe59f0000 | ((int) je - tmp - 8);
                }
                *je++ = *iv;
            }
            if (genpool == 2) { // jump past the pool
                tmp = ((int) je - (int) tje - 8) >> 2;
                *tje = 0xea000000 | (tmp & 0x00ffffff); // b #(je)
            }
            imm0 = 0;
            genpool = 0;
        }
    }
    if (il > immloc) die("codegen: not terminated by a LEV");
    tje = je;

    // second pass
    pc = text + 1; // Point instruction pointer "pc" to the first instruction.
    while (pc <= e) { // While instruction end is not met.
        // Get the IR's corresponding native instruction buffer address.
        je = (int *) jitmap[((int) pc - (int) text) >> 2];
        i = *pc++; // Get current instruction
        // If the instruction is one of the jumps.
        if (i == JSR || i == JMP || i == BZ || i == BNZ) {
            switch (i) {
            case JSR:
                *je = 0xeb000000;  // bl #(tmp)
                break;
            case JMP:
                *je = 0xea000000;  // b #(tmp)
                break;
            case BZ:
                *++je = 0x0a000000; // beq #(tmp)
                break;
            case BNZ:
                *++je = 0x1a000000; // bne #(tmp)
                break;
            }
            tmp = *pc++;
            *je = (*je |
                   reloc_imm(jitmap[(tmp - (int) text) >> 2] - (int) je));
        }
        // If the instruction has operand, increment instruction pointer to
        // skip he operand.
        else if (i < LEV) { ++pc; }
    }
    free(iv);
    return tje;
}

enum {
    _PROT_EXEC = 4, _PROT_READ = 1, _PROT_WRITE = 2,
    _MAP_PRIVATE = 2, _MAP_ANON = 32
};

int jit(int poolsz, int *main, int argc, char **argv)
{
    char *jitmem;  // executable memory for JIT-compiled native code
    int retval;
    if (src) return 1; // skip for IR listing

    // setup JIT memory
    if (!(jitmem = mmap(0, poolsz, _PROT_EXEC | _PROT_READ | _PROT_WRITE,
                        _MAP_PRIVATE | _MAP_ANON, -1, 0))) {
        printf("could not mmap(%d) jit executable memory\n", poolsz);
        return -1;
    }
    int *jitmap = (int *) (jitmem + (poolsz >> 1));
    int *je = (int *) jitmem;
    *je++ = (int) &retval;
    *je++ = argc;
    *je++ = (int) argv;
    int *_start = je;
    *je++ = 0xe92d5ff0;       // push    {r4-r12, lr}
    *je++ = 0xe51f0014;       // ldr     r0, [pc, #-20] ; argc
    *je++ = 0xe51f1014;       // ldr     r1, [pc, #-20] ; argv
    *je++ = 0xe52d0004;       // push    {r0}
    *je++ = 0xe52d1004;       // push    {r1}
    int *tje = je++;          // bl      jitmain
    *je++ = 0xe51f502c;       // ldr     r5, [pc, #-44] ; retval
    *je++ = 0xe5850000;       // str     r0, [r5]
    *je++ = 0xe28dd008;       // add     sp, sp, #8
    *je++ = 0xe8bd9ff0;       // pop     {r4-r12, pc}
    if (!(je = codegen(je, jitmap))) return 1;
    if (je >= jitmap) die("jitmem too small");
    *tje = reloc_bl(jitmap[((int) main - (int) text) >> 2] - (int) tje);

    // hack to jump into specific function pointer
    __clear_cache(jitmem, je);
    int *res = bsearch(&sym, sym, 1, 1, (void *) _start);
    if (((void *) 0) != res) return 0; return -1; // make compiler happy
}

int ELF32_ST_INFO(int b, int t) { return (b << 4) + (t & 0xf); }
enum {
    EHDR_SIZE = 52, ET_EXEC = 2, EM_ARM = 40,
    PHDR_ENT_SIZE = 32, SHDR_ENT_SIZE = 40,
    SYM_ENT_SIZE = 16, REL_ENT_SIZE = 8, PLT_ENT_SIZE = 12,
    DYN_ENT_SIZE = 8
};

struct Elf32_Shdr {
    int sh_name;      // [Elf32_Word] Section name (index into string table)
    int sh_type;      // [Elf32_Word] Section type (SHT_*)
    int sh_flags;     // [Elf32_Word] Section flags (SHF_*)
    int sh_addr;      // [Elf32_Addr] Address where section is to be loaded
    int sh_offset;    // [Elf32_Off] File offset of section data, in bytes
    int sh_size;      // [Elf32_Word] Size of section, in bytes
    int sh_link;      // [Elf32_Word] Section type-specific header table
                      //              index link
    int sh_info;      // [Elf32_Word] Section type-specific extra information
    int sh_addralign; // [Elf32_Word] Section address alignment
    int sh_entsize;   // [Elf32_Word] Size of records contained within section
};

enum {
    // Special section indices
    SHN_UNDEF     = 0,      // Undefined, missing, irrelevant, or meaningless

    // Section types
    SHT_NULL          = 0,  // No associated section (inactive entry)
    SHT_PROGBITS      = 1,  // Program-defined contents
    SHT_STRTAB        = 3,  // String table
    SHT_DYNAMIC       = 6,  // Information for dynamic linking
    SHT_REL           = 9,  // Relocation entries; no explicit addends
    SHT_DYNSYM        = 11, // Symbol table

    // Section flags
    SHF_WRITE = 0x1,
    SHF_ALLOC = 0x2,
    SHF_EXECINSTR = 0x4,
};

// Symbol table entries for ELF32
struct Elf32_Sym {
    int st_name;  // [Elf32_Word] Symbol name (index into string table)
    int st_value; // [Elf32_Addr] Value or address associated with the symbol
    int st_size;  // [Elf32_Word] Size of the symbol
    char st_info; // [unsigned] Symbol's type and binding attributes
    char st_other;// [unsigned] Must be zero; reserved
    char st_shndx, st_shndx_1, st_shndx_2, st_shndx_3; // [Elf32_Half]
                  // Which section (header table index) it's defined
};

enum {
    // Symbol bindings
    STB_LOCAL = 0,   /* Local symbol, not visible outside obj file
                                      containing def */
    STB_GLOBAL = 1,  /* Global symbol, visible to all object files
                                     being combined */

    // Symbol types
    STT_NOTYPE  = 0,   // Symbol's type is not specified
    STT_FUNC    = 2,   // Symbol is executable code (function, etc.)

    // Symbol number
    STN_UNDEF = 0
};

// Program header for ELF32
struct Elf32_Phdr {
    int p_type;   // [Elf32_Word] Type of segment
    int p_offset; // [Elf32_Off] File offset where segment is located, in bytes
    int p_vaddr;  // [Elf32_Addr] Virtual address of beginning of segment
    int p_paddr;  // [Elf32_Addr] Physical address of beginning of segment
                  //              (OS-specific)
    int p_filesz; // [Elf32_Word] Number of bytes in file image of segment
                  //              (may be zero)
    int p_memsz;  // [Elf32_Word] Number of bytes in mem image of segment
                  //              (may be zero)
    int p_flags;  // [Elf32_Word] Segment flags
    int p_align;  // [Elf32_Word] Segment alignment constraint
};

// Segment types
enum {
    PT_NULL    = 0, // Unused segment
    PT_LOAD    = 1, // Loadable segment
    PT_DYNAMIC = 2, // Dynamic linking information
    PT_INTERP  = 3, // Interpreter pathname

    // Segment flag bits
    PF_X        = 1,         // Execute
    PF_W        = 2,         // Write
    PF_R        = 4,         // Read
};

int phdr_idx, shdr_idx, sym_idx;

int gen_phdr(char *ptr, int type, int offset, int addr, int size,
             int flag, int align)
{
    struct Elf32_Phdr *phdr = (struct Elf32_Phdr *) ptr;
    phdr->p_type =  type;
    phdr->p_offset = offset;
    phdr->p_vaddr = addr;
    phdr->p_paddr = addr;
    phdr->p_filesz = size;
    phdr->p_memsz = size;
    phdr->p_flags = flag;
    phdr->p_align = align;
    return phdr_idx++;
}

int gen_shdr(char *ptr, int type, int name, int offset, int addr,
             int size, int link, int info,
             int flag, int align, int entsize)
{
    struct Elf32_Shdr *shdr = (struct Elf32_Shdr *) ptr;
    shdr->sh_name = name;       shdr->sh_type = type;
    shdr->sh_addr = addr;       shdr->sh_offset = offset;
    shdr->sh_size = size;       shdr->sh_link = link;
    shdr->sh_info = info;       shdr->sh_flags = flag;
    shdr->sh_addralign = align; shdr->sh_entsize = entsize;
    return shdr_idx++;
}

int gen_sym(char *ptr, int name, char info,
            int shndx, int size, int value)
{
    struct Elf32_Sym *sym = (struct Elf32_Sym *) ptr;
    sym->st_name = name;
    sym->st_info = info;
    sym->st_other = 0;
    // sym->st_shndx = shndx;
    memcpy(&(sym->st_shndx), (char *) &shndx, 2);
    sym->st_value = value;
    sym->st_size = size;
    return sym_idx++;
}

int append_func_sym(char **data, int name)
{
    int idx = gen_sym(*data, name, ELF32_ST_INFO(STB_GLOBAL, STT_FUNC), 0, 0, 0);
    *data += SYM_ENT_SIZE;
    return idx;
}

// shdr names which start with 'S'
enum {
    SNONE = 0, SSTAB, STEXT, SDATA, SDYNS, SDYNM, SDYNC,
    SINTP, SREL, SPLT, SGOT
};

enum {
    PAGE_SIZE = 0x1000, PHDR_NUM = 4, SHDR_NUM = 11,
    DYN_NUM = 15
};

int elf32(int poolsz, int *main, int elf_fd)
{
    char *freebuf, *freecode;
    int i;

    char *code = freecode = malloc(poolsz);
    char *buf = freebuf = malloc(poolsz);
    int *jitmap = (int *) (code + (poolsz >> 1));
    memset(buf, 0, poolsz);
    char *o = buf = (char *) (((int) buf + PAGE_SIZE - 1)  & -PAGE_SIZE);
    code = (char *) (((int) code + PAGE_SIZE - 1) & -PAGE_SIZE);

    phdr_idx = 0;
    shdr_idx = 0;
    sym_idx = 0;

    /* We must assign the plt_func_addr[x] a non-zero value, and also,
     * plt_func_addr[i] and plt_func_addr[i-1] has an offset of 16
     * (4 instruction * 4 bytes), so the first codegen and second codegen
     * have consistent code_size.
     */
    int FUNC_NUM = EXIT - OPEN + 1;
    plt_func_addr = malloc(sizeof(char *) * FUNC_NUM);
    for (i = 0; i < FUNC_NUM; i++)
        plt_func_addr[i] = o + i * 16;

    /* Run __libc_start_main() and pass main trampoline.
     *
     * Note: The function prototype of __libc_start_main() is:
     *
     *     int __libc_start_main(int (*main)(int, char**, char**),
     *                           int argc, char **argv,
     *                           int (*init)(int, char**, char**),
     *                           void (*fini)(void),
     *                           void (*rtld_fini)(void),
     *                           void *stack_end);
     *
     * Usually, we should pass __libc_csu_init as init and __libc_csu_fini
     * as fini; however, we will need a interp to link the non-shared part
     * of libc.  It sounds too complex.  To keep this compiler simple,
     * let's simply pass NULL pointer.
     */
    int *stub_end = (int *) code;

    *stub_end++ = 0xe3a0b000;  // mov   fp, #0  @ initialize frame pointer
    *stub_end++ = 0xe3a0e000;  // mov   lr, #0  @ initialize link register
    *stub_end++ = 0xe49d1004;  // pop   {r1}    @ get argc
    *stub_end++ = 0xe1a0200d;  // mov   r2, sp  @ get argv
    *stub_end++ = 0xe52d2004;  // push  {r2}    @ setup stack end
    *stub_end++ = 0xe52d0004;  // push  {r0}    @ setup rtld_fini
    *stub_end++ = 0xe3a0c000;  // mov   ip, #0  @ FIXME: __libc_csu_fini()
    *stub_end++ = 0xe52dc004;  // push  {ip}    @ setup fini
    *stub_end++ = 0xe28f0010;  // add   r0, pc, #16  @ load main trampoline
    *stub_end++ = 0xe3a03000;  // mov   r3, #0  @ FIXME: __libc_csu_init()
    *stub_end++ = 0xebfffffe;  // bl    __libc_start_main  @ Need relocation

    // Return 127 if __libc_start_main() returns (which should not.)
    *stub_end++ = 0xe3a0007f;  // mov   r0, #127
    *stub_end++ = 0xe3a07001;  // mov   r7, #1
    *stub_end++ = 0xef000000;  // svc   0x00000000

    // main() trampoline: convert ARM AAPCS calling convention to ours.
    *stub_end++ = 0xe92d5ff0;  // push  {r4-r12, lr}
    *stub_end++ = 0xe52d0004;  // push  {r0}
    *stub_end++ = 0xe52d1004;  // push  {r1}
    *stub_end++ = 0xebfffffe;  // bl    0 <main>  @ Need relocation
    *stub_end++ = 0xe28dd008;  // add   sp, sp, #8
    *stub_end++ = 0xe8bd9ff0;  // pop   {r4-r12, pc}

    int start_stub_size = (char *) stub_end - code;

    // Compile and generate the code.
    char *je = (char *) codegen((int *) (code + start_stub_size), jitmap);
    if (!je) return 1;
    if ((int *) je >= jitmap) die("elf32: jitmem too small");

    // elf32_hdr
    *o++ = 0x7f; *o++ = 'E'; *o++ = 'L'; *o++ = 'F';
    *o++ = 1;    *o++ = 1;   *o++ = 1;   *o++ = 0;
    o += 8;
    *o++ = ET_EXEC; *o++ = 0; // e_type
    *o++ = EM_ARM;  *o++ = 0; // e_machine
    *(int *) o = 1;          o += 4;
    char *entry = o;               o += 4; // e_entry
    *(int *) o = EHDR_SIZE;  o += 4; // e_phoff
    char *e_shoff = o;             o += 4; // e_shoff
    *(int *) o = 0x5000400;  o += 4; // e_flags
    *o++ = EHDR_SIZE; *o++ = 0;
    *o++ = PHDR_ENT_SIZE; *o++ = 0; *o++ = PHDR_NUM; *o++ = 0; // e_phentsize & e_phnum
    *o++ = SHDR_ENT_SIZE; *o++ = 0; *o++ = SHDR_NUM; *o++ = 0; // e_shentsize & e_shnum
    *o++ =  1; *o++ = 0;

    int phdr_size = PHDR_ENT_SIZE * PHDR_NUM;
    char *phdr = o; o += phdr_size;

    // .text
    int code_off = o - buf;
    int code_size = je - code;
    char *code_addr = o;
    o += code_size;

    // .rel.plt (embedded in PT_LOAD of text)
    int rel_size = REL_ENT_SIZE * FUNC_NUM;
    int rel_off = code_off + code_size;
    char *rel_addr = code_addr + code_size;
    o += rel_size;

    // .plt (embedded in PT_LOAD of text)
    int plt_size = 20 + PLT_ENT_SIZE * FUNC_NUM; // 20 is the size of .plt entry code to .got
    int plt_off = rel_off + rel_size;
    char *plt_addr = rel_addr + rel_size;
    o += plt_size;

    memcpy(code_addr, code,  code_size);
    *(int *) entry = (int) code_addr;

    // .data
    char *_data_end = data;
    // Use load_bias to align offset and v_addr, the elf loader
    // needs PAGE_SIZE align to do mmap().
    int load_bias = PAGE_SIZE + ((int) _data & (PAGE_SIZE - 1))
                    - ((o - buf) & (PAGE_SIZE - 1));
    o += load_bias;
    char *dseg = o;

    // rwdata (embedded in PT_LOAD of data)
    // rwdata is all the data (R/O and R/W) in source code,
    // e.g, the variable with initial value and all the string.
    int rwdata_off = dseg - buf;
    int rwdata_size = _data_end - _data;
    o += rwdata_size;

    // .dynamic (embedded in PT_LOAD of data)
    char *pt_dyn = data;
    int pt_dyn_size = DYN_NUM * DYN_ENT_SIZE;
    int pt_dyn_off = rwdata_off + rwdata_size; data += pt_dyn_size;
    o += pt_dyn_size;

    // .interp (embedded in PT_LOAD of data)
    char *interp_str = "/lib/ld-linux-armhf.so.3";
    int interp_str_size = 25; // strlen(interp_str) + 1
    char *interp = data; memcpy(interp, interp_str, interp_str_size);
    int interp_off = pt_dyn_off + pt_dyn_size; data += interp_str_size;
    o += interp_str_size;

    // .shstrtab (embedded in PT_LOAD of data)
    char *shstrtab_addr = data;
    int shstrtab_off = interp_off + interp_str_size;
    int shstrtab_size = 0;

    int *shdr_names = (int *) malloc(sizeof(int) * SHDR_NUM);
    if (!shdr_names) die("elf32: could not malloc shdr_names table");

    shdr_names[SNONE] = append_strtab(&data, "") - shstrtab_addr;
    shdr_names[SSTAB] = append_strtab(&data, ".shstrtab") - shstrtab_addr;
    shdr_names[STEXT] = append_strtab(&data, ".text") - shstrtab_addr;
    shdr_names[SDATA] = append_strtab(&data, ".data") - shstrtab_addr;
    shdr_names[SDYNS] = append_strtab(&data, ".dynstr") - shstrtab_addr;
    shdr_names[SDYNM] = append_strtab(&data, ".dynsym") - shstrtab_addr;
    shdr_names[SDYNC] = append_strtab(&data, ".dynamic") - shstrtab_addr;
    shdr_names[SINTP] = append_strtab(&data, ".interp") - shstrtab_addr;
    shdr_names[SREL] = append_strtab(&data, ".rel.plt") - shstrtab_addr;
    shdr_names[SPLT] = append_strtab(&data, ".plt") - shstrtab_addr;
    shdr_names[SGOT] = append_strtab(&data, ".got") - shstrtab_addr;
    shstrtab_size = data - shstrtab_addr;
    o += shstrtab_size;

    // .dynstr (embedded in PT_LOAD of data)
    char *dynstr_addr = data;
    int dynstr_off = shstrtab_off + shstrtab_size;
    append_strtab(&data, "");
    char *libc = append_strtab(&data, "libc.so.6");
    char *ldso = append_strtab(&data, "libdl.so.2");
    char *libgcc_s = append_strtab(&data, "libgcc_s.so.1");

    int *func_entries = (int *) malloc(sizeof(int) * (EXIT + 1));
    if (!func_entries) die("elf32: could not malloc func_entries table");

    for (i = OPEN; i <= EXIT; i++)
        func_entries[i] = append_strtab(&data, scnames[i - OPEN]) - dynstr_addr;

    int dynstr_size = data - dynstr_addr;
    o += dynstr_size;

    // .dynsym (embedded in PT_LOAD of data)
    char *dynsym_addr = data;
    int dynsym_off = dynstr_off + dynstr_size;
    memset(data, 0, SYM_ENT_SIZE);
    data += SYM_ENT_SIZE;

    for (i = OPEN; i <= EXIT; i++)
        append_func_sym(&data, func_entries[i]);

    int dynsym_size = SYM_ENT_SIZE * (FUNC_NUM + 1);
    o += dynsym_size;

    // .got (embedded in PT_LOAD of data)
    char *got_addr = data;
    int got_off = dynsym_off + dynsym_size;
    *(int *) data = (int) pt_dyn; data += 4;
    data += 4;  // reserved 2 and 3 entry for interp
    char *to_got_movw = data;  // The address manipulates dynamic
    char *to_got_movt = data;  // linking, plt must jump here.
    data += 4;  // reserved 2 and 3 entry for interp
    // .got function slot
    char **got_func_slot = malloc(sizeof(char *) * FUNC_NUM);
    for (i = 0; i < FUNC_NUM; i++) {
        got_func_slot[i] = data;
        *(int *) data = (int) plt_addr; data += 4;
    }
    data += 4;  // end with 0x0
    int got_size = (int) data - (int) got_addr;
    o += got_size;

    int dseg_size = o - dseg;

    // .plt -- Now we back to handle .plt after .got was initial
    char *to = plt_addr;
    *(int *) to = 0xe52de004; to += 4; // push {lr}
    // movw r10 addr_to_got
    *(int *) to = 0xe300a000 | (0xfff & (int) (to_got_movw)) |
                  (0xf0000 & ((int) (to_got_movw) << 4));
    to += 4;
    // movt r10 addr_to_got
    *(int *) to = 0xe340a000 | (0xfff & ((int) (to_got_movt) >> 16)) |
                  (0xf0000 & ((int) (to_got_movt) >> 12));
    to += 4;
    *(int *) to = 0xe1a0e00a; to += 4;  // mov lr,r10
    *(int *) to = 0xe59ef000; to += 4;  // ldr pc, [lr]

    // We must preserve ip for code below, dyn link use this as return address
    for (i = 0; i < FUNC_NUM; i++) {
        plt_func_addr[i] = to;
        // movt ip addr_to_got
        *(int *) to = 0xe300c000 | (0xfff & (int) (got_func_slot[i])) |
                      (0xf0000 & ((int) (got_func_slot[i]) << 4));
        to += 4;
        // movw ip addr_to_got
        *(int *) to = 0xe340c000 |
                      (0xfff & ((int) (got_func_slot[i]) >> 16)) |
                      (0xf0000 & ((int) (got_func_slot[i]) >> 12));
        to += 4;
        *(int *) to = 0xe59cf000; to += 4;  // ldr pc, [ip]
    }

    // .rel.plt
    to = rel_addr;
    for (i = 0; i < FUNC_NUM; i++) {
        *(int *) to = (int) got_func_slot[i]; to += 4;
        *(int *) to = 0x16 | (i + 1) << 8 ; to += 4;
        // 0x16 R_ARM_JUMP_SLOT | .dymstr index << 8
    }

    // Generate program header after we got address, offset and size.
    to = phdr;
    // PT_LOAD for .text
    gen_phdr(to, PT_LOAD, 0, (int) buf,
            EHDR_SIZE + phdr_size + code_size + rel_size + plt_size,
            PF_X | PF_R, PAGE_SIZE);
    to += PHDR_ENT_SIZE;

    // PT_LOAD for .data
    gen_phdr(to, PT_LOAD, rwdata_off, (int) _data,
            dseg_size, PF_W | PF_R, PAGE_SIZE);
    to += PHDR_ENT_SIZE;

    // PT_INTERP for .interp
    gen_phdr(to, PT_INTERP, interp_off, (int) interp,
            interp_str_size , PF_R, 0x1);
    to += PHDR_ENT_SIZE;

    // PT_DYNAMIC for .dynamic
    gen_phdr(to, PT_DYNAMIC, pt_dyn_off, (int) pt_dyn,
            pt_dyn_size , PF_R | PF_W, 0x4);
    to += PHDR_ENT_SIZE;

    // .dynamic (embedded in PT_LOAD of data)
    to = pt_dyn;
    *(int *) to =  5; to += 4; *(int *) to = (int) dynstr_addr;  to += 4;
    *(int *) to = 10; to += 4; *(int *) to = dynstr_size;        to += 4;
    *(int *) to =  6; to += 4; *(int *) to = (int) dynsym_addr;  to += 4;
    *(int *) to = 11; to += 4; *(int *) to = 16;                 to += 4;
    *(int *) to = 17; to += 4; *(int *) to = (int) rel_addr;     to += 4;
    *(int *) to = 18; to += 4; *(int *) to = rel_size;           to += 4;
    *(int *) to = 19; to += 4; *(int *) to = 8;                  to += 4;
    *(int *) to =  3; to += 4; *(int *) to = (int) got_addr;     to += 4;
    *(int *) to =  2; to += 4; *(int *) to = rel_size;           to += 4;
    *(int *) to = 20; to += 4; *(int *) to = 17;                 to += 4;
    *(int *) to = 23; to += 4; *(int *) to = (int) rel_addr;     to += 4;
    *(int *) to =  1; to += 4; *(int *) to = libc - dynstr_addr; to += 4;
    *(int *) to =  1; to += 4; *(int *) to = ldso - dynstr_addr; to += 4;
    *(int *) to =  1; to += 4; *(int *) to = libgcc_s - dynstr_addr; to += 4;
    *(int *) to =  0; to += 8;

    /* Generate code again bacause address of .plt function slots must
     * be confirmed before codegen() to make sure the code is correct.
     */
    je = (char *) codegen((int *) (code + start_stub_size), jitmap);
    if (!je) {
        free(func_entries);
        free(shdr_names);
        return 1;
    }
    if ((int *) je >= jitmap) die("elf32: jitmem too small");

    // Relocate _start() stub.
    *((int *) (code + 0x28)) = reloc_bl(plt_func_addr[STRT - OPEN] - code_addr - 0x28);
    *((int *) (code + 0x44)) =
        reloc_bl(jitmap[((int) main - (int) text) >> 2] - (int) code - 0x44);

    // Copy the generated binary.
    memcpy(code_addr, code,  je - code);

    // Generate section header
    *(int *) e_shoff = (int) (o - buf);
    gen_shdr(o, SHT_NULL, shdr_names[SNONE], 0, 0, 0,
             0, 0, 0, 0, 0);
    o += SHDR_ENT_SIZE;

    // sh_shstrtab_idx
    gen_shdr(o, SHT_STRTAB, shdr_names[SSTAB], shstrtab_off, 0,
             shstrtab_size, 0, 0, 0, 1, 0);
    o += SHDR_ENT_SIZE;

    // sh_text_idx
    gen_shdr(o, SHT_PROGBITS, shdr_names[STEXT], code_off, (int) code_addr,
            code_size, 0, 0, SHF_ALLOC | SHF_EXECINSTR, 4, 0);
    o += SHDR_ENT_SIZE;

    // sh_data_idx
    gen_shdr(o, SHT_PROGBITS, shdr_names[SDATA], rwdata_off, (int) _data,
             dseg_size, 0, 0, SHF_ALLOC | SHF_WRITE, 4, 0);
    o += SHDR_ENT_SIZE;

    int sh_dynstr_idx =
    gen_shdr(o, SHT_STRTAB, shdr_names[SDYNS], dynstr_off, (int) dynstr_addr,
             dynstr_size, 0, 0, SHF_ALLOC, 1, 0);
    o += SHDR_ENT_SIZE;

    int sh_dynsym_idx =
    gen_shdr(o, SHT_DYNSYM, shdr_names[SDYNM], dynsym_off, (int) dynsym_addr,
             dynsym_size, sh_dynstr_idx, 1, SHF_ALLOC, 4, 0x10);
    o += SHDR_ENT_SIZE;

    // sh_dynamic_idx
    gen_shdr(o, SHT_DYNAMIC, shdr_names[SDYNC], pt_dyn_off, (int) pt_dyn,
             pt_dyn_size, sh_dynstr_idx, 0, SHF_ALLOC | SHF_WRITE, 4, 0);
    o += SHDR_ENT_SIZE;

    // sh_interp_idx
    gen_shdr(o, SHT_PROGBITS, shdr_names[SINTP], interp_off, (int) interp,
             interp_str_size, 0, 0, SHF_ALLOC, 1, 0);
    o += SHDR_ENT_SIZE;

    // sh_rel_idx
    gen_shdr(o, SHT_REL, shdr_names[SREL], rel_off, (int) rel_addr,
             rel_size, sh_dynsym_idx, 11, SHF_ALLOC | 0x40, 4, 8);
    o += SHDR_ENT_SIZE;

    // sh_plt_idx
    gen_shdr(o, SHT_PROGBITS, shdr_names[SPLT], plt_off, (int) plt_addr,
             plt_size, 0, 0, SHF_ALLOC | SHF_EXECINSTR, 4, 4);
    o += SHDR_ENT_SIZE;

    // sh_got_idx
    gen_shdr(o, SHT_PROGBITS, shdr_names[SGOT], got_off, (int) got_addr,
             got_size, 0, 0, SHF_ALLOC | SHF_WRITE, 4, 4);
    o += SHDR_ENT_SIZE;

    // Copy .data to a part of (o - buf) where _data located.
    memcpy(dseg, _data, dseg_size);
    write(elf_fd, buf, o - buf);

    free(func_entries);
    free(shdr_names);
    free(freebuf);
    free(freecode);
    free(plt_func_addr);
    free(got_func_slot);
    return 0;
}

int streq(char *p1, char *p2)
{
    while (*p1 && *p1 == *p2) { ++p1; ++p2; }
    return (*p1 > *p2) == (*p2  > *p1);
}

enum { _O_CREAT = 64, _O_WRONLY = 1 };
int main(int argc, char **argv)
{
    int *freed_ast, *ast;
    int elf_fd;
    int i;

    --argc; ++argv;
    if (argc > 0 && **argv == '-' && (*argv)[1] == 's') {
        src = 1; --argc; ++argv;
    }
    if (argc > 0 && **argv == '-' && streq(*argv, "-fsigned-char")) {
        signed_char = 1; --argc; ++argv;
    }
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'o') {
        elf = 1; --argc; ++argv;
        if (argc < 1) die("no output file argument");
        if ((elf_fd = open(*argv, _O_CREAT | _O_WRONLY, 0775)) < 0) {
            printf("could not open(%s)\n", *argv); return -1;
        }
        --argc; ++argv;
    }
    if (argc < 1) die("usage: amacc [-s] [-o object] file");

    int fd;
    if ((fd = open(*argv, 0)) < 0) {
        printf("could not open(%s)\n", *argv); return -1;
    }

    int poolsz = 256 * 1024; // arbitrary size
    if (!(text = le = e = malloc(poolsz)))
        die("could not allocate text area");
    if (!(sym = malloc(poolsz)))
        die("could not allocate symbol area");
    if (!(freedata = _data = data = malloc(poolsz)))
        printf("could not allocat data area");
    if (!(tsize = malloc(PTR * sizeof(int))))
        die("could not allocate tsize area");
    if (!(members = malloc(PTR * sizeof(struct member_s *))))
        die("could not malloc() members area");
    if (!(freed_ast = ast = malloc(poolsz)))
        die("could not allocate abstract syntax tree area");

    memset(sym, 0, poolsz);
    memset(e, 0, poolsz);
    memset(data, 0, poolsz);

    memset(tsize,   0, PTR * sizeof(int));
    memset(members, 0, PTR * sizeof(struct member_s *));
    memset(ast, 0, poolsz);
    ast = (int *) ((int) ast + poolsz); // abstract syntax tree is most efficiently built as a stack

    /* Resgister keywords and system calls to symbol stack
     * must match the sequence of enum
     */
    p = "break case char default else enum if int return "
        "sizeof struct switch for while "
        "open read write close printf malloc free "
        "memset memcmp memcpy mmap "
        "dlsym bsearch __libc_start_main "
        "dlopen __aeabi_idiv __aeabi_idivmod exit __clear_cache void main";

    // name vector to system call
    // must match the sequence of supported calls
    scnames = malloc(19 * sizeof(char *));
    scnames[ 0] = "open";    scnames[ 1] = "read";    scnames[ 2] = "write";
    scnames[ 3] = "close";   scnames[ 4] = "printf";
    scnames[ 5] = "malloc";  scnames[ 6] = "free";
    scnames[ 7] = "memset";  scnames[ 8] = "memcmp";  scnames[ 9] = "memcpy";
    scnames[10] = "mmap";    scnames[11] = "dlsym";   scnames[12] = "bsearch";
    scnames[13] = "__libc_start_main";
    scnames[14] = "dlopen";
    scnames[15] = "__aeabi_idiv";
    scnames[16] = "__aeabi_idivmod";
    scnames[17] = "exit";
    scnames[18] = "__clear_cache";

    // call "next" to create symbol table entry.
    // store the keyword's token type in the symbol table entry's "tk" field.
    for (i = Break; i <= While; i++) {
        next(); id->tk = i; // add keywords to symbol table
    }

    // add library to symbol table
    for (i = OPEN; i < INVALID; i++) {
        next(); id->class = Syscall; id->type = INT; id->val = i;
    }
    next(); id->tk = Char; // handle void type
    next();
    struct ident_s *idmain = id; // keep track of main

    if (!(freep = lp = p = malloc(poolsz)))
        die("could not allocate source area");
    if ((i = read(fd, p, poolsz - 1)) <= 0)
        die("unable to read from source file");
    p[i] = 0;
    close(fd);

    // add primitive types
    tsize[tnew++] = sizeof(char);
    tsize[tnew++] = sizeof(int);

    // real C parser begins here
    // parse the program
    line = 1;
    next();
    n = ast;
    while (tk) {
        stmt(Glo);
        next();
    }

    int ret = elf ? elf32(poolsz, (int *) idmain->val, elf_fd) :
                    jit(poolsz,   (int *) idmain->val, argc, argv);

    free(freep);
    free(freedata);
    free(text);
    free(sym);
    free(freed_ast);
    return ret;
}
