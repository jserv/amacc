// Another Mini ARM C Compiler (AMaCC)
// supported data types: char, int, and pointer
// supported statements: if, while, switch, return, and expression
//
// The features of AMaCC is just enough to allow self-compilation.

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>

char *p, *lp;         // current position in source code
char *data, *_data;   // data/bss pointer

int *e, *le, *text;  // current position in emitted code
int *id;             // currently parsed identifier
int *sym;            // symbol table (simple list of identifiers)
int *cas;            // case statement patch-up pointer
int *brks;           // break statement patch-up pointer
int *def;            // default statement patch-up pointer
int tk;              // current token
int ival;            // current token value
int ty;              // current expression type
int loc;             // local variable offset
int line;            // current line number
int src;             // print source and assembly flag
int verbose;         // print executed instructions

// tokens and classes (operators last and in precedence order)
enum {
    Num = 128, Fun, Sys, Glo, Loc, Id,
    Break, Case, Char, Default, Else, Enum, If, Int, Return, Sizeof,
    Switch, While,
    Assign, Cond,
    Lor, Lan, Or, Xor, And,
    Eq, Ne, Lt, Gt, Le, Ge,
    Shl, Shr, Add, Sub, Mul, Inc, Dec, Brak
};

// opcodes
enum {
    LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
    OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,
    OPEN,READ,WRIT,CLOS,PRTF,MALC,MSET,MCMP,MCPY,MMAP,DSYM,BSCH,CLCA,EXIT
};

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

void next()
{
    char *pp;
    while ((tk = *p)) {
        ++p;
        if ((tk >= 'a' && tk <= 'z') ||
                 (tk >= 'A' && tk <= 'Z') ||
                 (tk == '_')) {
            pp = p - 1;
            while ((*p >= 'a' && *p <= 'z') ||
                   (*p >= 'A' && *p <= 'Z') ||
                   (*p >= '0' && *p <= '9') ||
                   (*p == '_'))
                tk = tk * 147 + *p++;
            tk = (tk << 6) + (p - pp);
            id = sym;
            while (id[Tk]) {
                if (tk == id[Hash] &&
                    !memcmp((char *) id[Name], pp, p - pp)) {
                    tk = id[Tk];
                    return;
                }
                id = id + Idsz;
            }
            id[Name] = (int) pp;
            id[Hash] = tk;
            tk = id[Tk] = Id;
            return;
        }
        else if (tk >= '0' && tk <= '9') {
            if ((ival = tk - '0')) {
                while (*p >= '0' && *p <= '9')
                    ival = ival * 10 + *p++ - '0';
            }
            else if (*p == 'x' || *p == 'X') {
                while ((tk = *++p) &&
                       ((tk >= '0' && tk <= '9') ||
                        (tk >= 'a' && tk <= 'f') ||
                        (tk >= 'A' && tk <= 'F')))
                    ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0);
            }
            else {
                while (*p >= '0' && *p <= '7')
                    ival = ival * 8 + *p++ - '0';
            }
            tk = Num;
            return;
        }
        switch (tk) {
        case '\n':
            if (src) {
                printf("%d: %.*s", line, p - lp, lp);
                lp = p;
                while (le < e) {
                    printf("%8.4s",
                           &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,"
                            "LI  ,LC  ,SI  ,SC  ,PSH ,"
                            "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,"
                            "SHL ,SHR ,ADD ,SUB ,MUL ,"
                            "OPEN,READ,WRIT,CLOS,"
                            "PRTF,MALC,MSET,MCMP,MCPY,DSYM,BSCH,MMAP,"
                            "CLCA,EXIT"[*++le * 5]);
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
        case '#':
            while (*p != 0 && *p != '\n') ++p;
            break;
        case '/':
            if (*p == '/') { // comment
                ++p;
                while (*p != 0 && *p != '\n') ++p;
            } else {
                // Div is not supported
	        return;
            }
            break;
        case '\'':
        case '"':
            pp = data;
            while (*p != 0 && *p != tk) {
                if ((ival = *p++) == '\\') {
                    switch (ival = *p++) {
                    case 'n': ival = '\n'; break;
                    case 't': ival = '\t'; break;
                    case 'v': ival = '\v'; break;
                    case 'f': ival = '\f'; break;
                    case 'r': ival = '\r';
                    }
                }
                if (tk == '"') *data++ = ival;
            }
            ++p;
            if (tk == '"') ival = (int) pp; else tk = Num;
            return;
        case '=': if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return;
        case '+': if (*p == '+') { ++p; tk = Inc; } else tk = Add; return;
        case '-': if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return;
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
        case '*': tk = Mul; return;
        case '[': tk = Brak; return;
        case '?': tk = Cond; return;
        default: return;
        }
    }
}

void expr(int lev)
{
    int t, *d;

    switch (tk) {
    case 0: printf("%d: unexpected eof in expression\n", line); exit(-1);
    case Num: *++e = IMM; *++e = ival; next(); ty = INT; break;
    case '"':
        *++e = IMM; *++e = ival; next();
        while (tk == '"') next();
        data = (char *)(((int) data + sizeof(int)) & (-sizeof(int)));
        ty = PTR;
        break;
    case Sizeof:
        next();
        if (tk == '(')
            next();
        else {
            printf("%d: open paren expected in sizeof\n", line);
            exit(-1);
        }
        ty = INT;
        if (tk == Int)
    	    next();
        else if (tk == Char) { next(); ty = CHAR; }
        while (tk == Mul) { next(); ty = ty + PTR; }
        if (tk == ')')
            next();
        else {
            printf("%d: close paren expected in sizeof\n", line);
            exit(-1);
        }
        *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int);
        ty = INT;
        break;
    case Id:
        d = id; next();
        if (tk == '(') {
            next();
            t = 0;
            while (tk != ')') { expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); }
            next();
            if (d[Class] == Sys) *++e = d[Val];
            else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; }
            else { printf("%d: bad function call\n", line); exit(-1); }
            if (t) { *++e = ADJ; *++e = t; }
            ty = d[Type];
        }
        else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; }
        else {
            if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; }
            else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; }
            else { printf("%d: undefined variable\n", line); exit(-1); }
                *++e = ((ty = d[Type]) == CHAR) ? LC : LI;
        }
        break;
    case '(':
        next();
        if (tk == Int || tk == Char) {
            t = (tk == Int) ? INT : CHAR; next();
            while (tk == Mul) { next(); t = t + PTR; }
            if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
            expr(Inc);
            ty = t;
        }
        else {
            expr(Assign);
            if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        }
        break;
    case Mul:
        next(); expr(Inc);
        if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
        *++e = (ty == CHAR) ? LC : LI;
        break;
    case And:
        next(); expr(Inc);
        if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
        ty = ty + PTR;
        break;
    case '!': next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; break;
    case '~': next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; break;
    case Add: next(); expr(Inc); ty = INT; break;
    case Sub:
        next(); *++e = IMM;
        if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
        ty = INT;
        break;
    case Inc:
    case Dec:
        t = tk; next(); expr(Inc);
        if (*e == LC) { *e = PSH; *++e = LC; }
        else if (*e == LI) { *e = PSH; *++e = LI; }
        else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
        *++e = PSH;
        *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
        *++e = (t == Inc) ? ADD : SUB;
        *++e = (ty == CHAR) ? SC : SI;
        break;
    default:
        printf("%d: bad expression\n", line); exit(-1);
    }

    while (tk >= lev) { // top down operator precedence
        t = ty;
        switch (tk) {
        case Assign:
            next();
            if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
            expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
            break;
        case Cond:
            next();
            *++e = BZ; d = ++e;
            expr(Assign);
            if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
            *d = (int)(e + 3); *++e = JMP; d = ++e;
            expr(Cond);
            *d = (int)(e + 1);
            break;
        case Lor: next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; break;
        case Lan: next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; break;
        case Or:  next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; break;
        case Xor: next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; break;
        case And: next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; break;
        case Eq:  next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; break;
        case Ne:  next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; break;
        case Lt:  next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; break;
        case Gt:  next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; break;
        case Le:  next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; break;
        case Ge:  next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; break;
        case Shl: next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; break;
        case Shr: next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; break;
        case Add:
            next(); *++e = PSH; expr(Mul);
            if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
            *++e = ADD;
            break;
        case Sub:
            next(); *++e = PSH; expr(Mul);
            if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = SUB; ty = INT; }
            else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
            else *++e = SUB;
            break;
        case Mul: next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; break;
        case Inc:
        case Dec:
            if (*e == LC) { *e = PSH; *++e = LC; }
            else if (*e == LI) { *e = PSH; *++e = LI; }
            else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
            *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? ADD : SUB;
            *++e = (ty == CHAR) ? SC : SI;
            *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
            *++e = (tk == Inc) ? SUB : ADD;
            next();
            break;
        case Brak:
            next(); *++e = PSH; expr(Assign);
            if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
            if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
            else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
            *++e = ADD;
            *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
            break;
        default:
            printf("%d: compiler error tk=%d\n", line, tk); exit(-1);
        }
    }
}

void stmt()
{
    int *a, *b, *d;
    int i;

    switch (tk) {
    case If:
        next();
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e;
        stmt();
        if (tk == Else) {
            *b = (int)(e + 3); *++e = JMP; b = ++e;
            next();
            stmt();
        }
        *b = (int)(e + 1);
        return;
    case While:
        next();
        a = e + 1;
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        *++e = BZ; b = ++e;
        stmt();
        *++e = JMP; *++e = (int)a;
        *b = (int)(e + 1);
        return;
    case Switch:
        next();
        if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
        expr(Assign);
        if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
        a = cas; *++e = JMP; cas = ++e;
        b = brks; d = def; brks = def = 0;
        stmt();
        *cas = def ? (int)def : (int)(e + 1); cas = a;
        while (brks) { a = (int *)*brks; *brks = (int)(e + 1); brks = a; }
        brks = b; def = d;
        return;
    case Case:
        *++e = JMP; ++e; *e = (int)(e + 7); *++e = PSH; i = *cas; *cas = (int)e;
        next();
        expr(Or);
        if (e[-1] != IMM) { printf("%d: bad case immediate\n", line); exit(-1); }
        *e = *e - i; *++e = SUB; *++e = BNZ; cas = ++e; *e = i + e[-3];
        if (tk == ':') next(); else { printf("%d: colon expected\n", line); exit(-1); }
        stmt();
        return;
    case Break:
        next();
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
        *++e = JMP; *++e = (int)brks; brks = e;
        return;
    case Default:
        next();
        if (tk == ':') next(); else { printf("%d: colon expected\n", line); exit(-1); }
        def = e + 1;
        stmt();
        return;
    case Return:
        next();
        if (tk != ';') expr(Assign);
        *++e = LEV;
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
        return;
    case '{':
        next();
        while (tk != '}') stmt();
        next();
        return;
    case ';':
        next();
        return;
    default:
        expr(Assign);
        if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
    }
}

int *codegen(int *jitmem, int *jitmap, int reloc)
{
    int *pc;
    int i, tmp, genpool;
    int *je, *tje;    // current position in emitted native code
    int *immloc, *il, *iv, *imm0;
    char neg_char;
    int neg_int;

    immloc = il = malloc(1024 * 4);
    iv = malloc(1024 * 4);
    imm0 = 0;
    genpool = 0;
    neg_char = 255;
    neg_int = neg_char;

    // first pass: emit native code
    pc = text + 1; je = jitmem; line = 0;
    while (pc <= e) {
        i = *pc;
        if (verbose) {
            printf("%p -> %p: %8.4s", pc, je,
                   &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                    "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,"
                    "OPEN,READ,WRIT,CLOS,PRTF,MALC,MSET,MCMP,MCPY,DSYM,BSCH,MMAP,CLCA,EXIT"[i * 5]);
            if (i <= ADJ) printf(" %d\n", pc[1]); else printf("\n");
        }
        jitmap[((int)pc++ - (int)text) >> 2] = (int)je;
        if (i == LEA) {
            tmp = *pc++;
            if (tmp >= 64 || tmp <= -64) { printf("jit: LEA %d out of bounds\n", tmp); exit(6); }
            if (tmp >= 0)
                *je++ = 0xe28b0000 | tmp * 4;    // add     r0, fp, #(tmp)
            else
                *je++ = 0xe24b0000 | (-tmp) * 4; // sub     r0, fp, #(tmp)
        }
        else if (i == IMM) {
            tmp = *pc++;
            if (0 <= tmp && tmp < 256) *je++ = 0xe3a00000 + tmp; // mov r0, #(tmp)
            else { if (!imm0) imm0 = je; *il++ = (int)(je++); *iv++ = tmp;}
        }
        else if (i == JSR || i == JMP) { pc++; je++; } // postponed till second pass
        else if (i == BZ || i == BNZ) { *je++ = 0xe3500000; pc++; je++; } // cmp r0, #0
        else if (i == ENT) {
            *je++ = 0xe92d4800; *je++ = 0xe28db000; // push {fp, lr}; add  fp, sp, #0
            tmp = *pc++; if (tmp) *je++ = 0xe24dd000 | (tmp * 4); // sub  sp, sp, #(tmp * 4)
            if (tmp >= 64 || tmp < 0) { printf("jit: ENT %d out of bounds\n", tmp); exit(6); }
        }
        else if (i == ADJ)   *je++ = 0xe28dd000 + *pc++ * 4; // add sp, sp, #(tmp * 4)
        else if (i == LEV) { *je++ = 0xe28bd000; *je++ = 0xe8bd8800; } // add sp, fp, #0; pop {fp, pc}
        else if (i == LI)    *je++ = 0xe5900000;                       // ldr r0, [r0]
        else if (i == LC) {  *je++ = 0xe5d00000; if (neg_int < 0)  *je++ = 0xe6af0070; } // ldrb r0, [r0]; (sxtb r0, r0)
        else if (i == SI) {  *je++ = 0xe49d1004; *je++ = 0xe5810000; } // pop {r1}; str r0, [r1]
        else if (i == SC) {  *je++ = 0xe49d1004; *je++ = 0xe5c10000; } // pop {r1}; strb r0, [r1]
        else if (i == PSH)   *je++ = 0xe52d0004;                       // push {r0}
        else if (i == OR) {  *je++ = 0xe49d1004; *je++ = 0xe1810000; } // pop {r1}; orr r0, r1, r0
        else if (i == XOR) { *je++ = 0xe49d1004; *je++ = 0xe0210000; } // pop {r1}; eor r0, r1, r0
        else if (i == AND) { *je++ = 0xe49d1004; *je++ = 0xe0010000; } // pop {r1}; and r0, r1, r0
        else if (EQ <= i && i <= GE) {
            *je++ = 0xe49d1004; *je++ = 0xe1510000;                    // pop {r1}; cmp r1, r0
            if (i <= NE) { je[0] = 0x03a00000; je[1] = 0x13a00000; }   // moveq r0, #0; movne r0, #0
            else if (i == LT || i == GE) { je[0] = 0xb3a00000; je[1] = 0xa3a00000; } // movlt r0, #0; movge   r0, #0
            else { je[0] = 0xc3a00000; je[1] = 0xd3a00000; }           // movgt r0, #0; movle r0, #0
            if (i == EQ || i == LT || i == GT) je[0] = je[0] | 1; else je[1] = je[1] | 1;
            je = je + 2;
        }
        else if (i == SHL) { *je++ = 0xe49d1004; *je++ = 0xe1a00011; } // pop {r1}; lsl r0, r1, r0
        else if (i == SHR) { *je++ = 0xe49d1004; *je++ = 0xe1a00051; } // pop {r1}; asr r0, r1, r0
        else if (i == ADD) { *je++ = 0xe49d1004; *je++ = 0xe0800001; } // pop {r1}; add r0, r0, r1
        else if (i == SUB) { *je++ = 0xe49d1004; *je++ = 0xe0410000; } // pop {r1}; sub r0, r1, r0
        else if (i == MUL) { *je++ = 0xe49d1004; *je++ = 0xe0000091; } // pop {r1}; mul r0, r1, r0
        else if (i == CLCA) {
            *je++ = 0xe59d0004; *je++ = 0xe59d1000;                    // ldr r0, [sp, #4]; ldr r1, [sp]
            *je++ = 0xe3a0780f; *je++ = 0xe2877002;                    // mov r7, #0xf0000; add r7, r7, #2
            *je++ = 0xe3a02000; *je++ = 0xef000000;                    // mov r2, #0;       svc 0
        }
        else if (i >= OPEN) {
            if      (i == OPEN) tmp = (int)dlsym(0, "open");   else if (i == READ) tmp = (int)dlsym(0, "read");
            else if (i == WRIT) tmp = (int)dlsym(0, "write");
            else if (i == CLOS) tmp = (int)dlsym(0, "close");  else if (i == PRTF) tmp = (int)dlsym(0, "printf");
            else if (i == MALC) tmp = (int)dlsym(0, "malloc"); else if (i == MSET) tmp = (int)dlsym(0, "memset");
            else if (i == MCMP) tmp = (int)dlsym(0, "memcmp"); else if (i == MCPY) tmp = (int)dlsym(0, "memcpy");
            else if (i == MMAP) tmp = (int)dlsym(0, "mmap");   else if (i == DSYM) tmp = (int)dlsym(0, "dlsym");
            else if (i == BSCH) tmp = (int)dlsym(0, "bsearch");
            else if (i == EXIT) tmp = (int)dlsym(0, "exit");
            else { printf("unrecognized code %d\n", i); return 0; }
            if (*pc++ != ADJ) { printf("no ADJ after native proc!\n"); exit(2); }
            i = *pc;
            if (i > 10) { printf("no support for 10+ arguments!\n"); exit(3); }
            while (i > 0) *je++ = 0xe49d0004 | (--i << 12); // pop r(i-1)
            i = *pc++;
            if (i > 4) *je++ = 0xe92d03f0;                  // push {r4-r9}
            *je++ = 0xe28fe000;                             // add lr, pc, #0
            if (!imm0) imm0 = je;
            *il++ = (int)je++ + 1;
            *iv++ = tmp;
            if (i > 4) *je++ = 0xe28dd018; // add sp, sp, #24
        }
        else { printf("code generation failed for %d!\n", i); return 0; }

        if (imm0) {
            if (i == LEV) genpool = 1;
            else if ((int)je > (int)imm0 + 3000) {tje = je++; genpool = 2; }
        }
        if (genpool) {
            if (verbose) printf("POOL %d %d %d\n", genpool, il - immloc, je - imm0);
            *iv = 0;
            while (il > immloc) {
                tmp = *--il;
                if ((int)je > tmp + 4096 + 8) { printf("can't reach the pool\n"); exit(5); }
                iv--; if (iv[0] == iv[1]) je--;
                if (tmp & 1)
                    *(int*)(tmp - 1) = 0xe59ff000 | ((int)je - tmp - 7); // ldr pc, [pc, #..]
                else
                    *(int*)tmp = 0xe59f0000 | ((int)je - tmp - 8); // ldr r0, [pc, #..]
                *je++ = *iv;
            }
            if (genpool == 2) { // jump past the pool
                tmp = ((int)je - (int)tje - 8) >> 2;
                *tje = 0xea000000 | (tmp & 0x00ffffff); // b #(je)
            }
            imm0 = 0;
            genpool = 0;
        }
    }
    if (il > immloc) { printf("code is not terminated by a LEV\n"); exit(6); }
    tje = je;

    // second pass
    pc = text + 1;
    while (pc <= e) {
        je = (int*)jitmap[((int)pc - (int)text) >> 2]; i = *pc++;
        if (i == JSR || i == JMP || i == BZ || i == BNZ) {
            if      (i == JSR)   *je = 0xeb000000; // bl #(tmp)
            else if (i == JMP)   *je = 0xea000000; // bl #(tmp)
            else if (i == BZ)  *++je = 0x0a000000; // beq #(tmp)
            else if (i == BNZ) *++je = 0x1a000000; // bne #(tmp)
            tmp = *pc++;
            *je = *je | (((jitmap[(tmp - (int)text) >> 2] - (int)je - 8) >> 2) & 0x00ffffff);
        }
        else if (i < LEV) { ++pc; }
    }
    return tje;
}

int jit(int poolsz, int *start, int argc, char **argv)
{
    char *jitmem;      // executable memory for JIT-compiled native code
    int *je, *tje, *_start,  retval, *jitmap, *res;

    // setup jit memory
    // PROT_EXEC | PROT_READ | PROT_WRITE = 7
    // MAP_PRIVATE | MAP_ANON = 0x22
    jitmem = mmap(0, poolsz, 7, 0x22, -1, 0);
    if (!jitmem) { printf("could not mmap(%d) jit executable memory\n", poolsz); return -1; }
    if (src)
        return 1;
    jitmap = (int*)(jitmem + (poolsz >> 1));
    je = (int*)jitmem;
    *je++ = (int)&retval;
    *je++ = argc;
    *je++ = (int)argv;
    _start = je;
    *je++ = 0xe92d5ff0;       // push    {r4-r12, lr}
    *je++ = 0xe51f0014;       // ldr     r0, [pc, #-20] ; argc
    *je++ = 0xe51f1014;       // ldr     r1, [pc, #-20] ; argv
    *je++ = 0xe52d0004;       // push    {r0}
    *je++ = 0xe52d1004;       // push    {r1}
    tje = je++;               // bl      jitmain
    *je++ = 0xe51f502c; // ldr     r5, [pc, #-44] ; retval
    *je++ = 0xe5850000;       // str     r0, [r5]
    *je++ = 0xe28dd008;       // add     sp, sp, #8
    *je++ = 0xe8bd9ff0;       // pop     {r4-r12, pc}
    if (!(je = codegen(je, jitmap, 0))) return 1;
    if (je >= jitmap) { printf("jitmem too small\n"); exit(7); }
    *tje = 0xeb000000 | (((jitmap[((int)start - (int)text) >> 2] - (int)tje - 8) >> 2) & 0x00ffffff);
    __clear_cache(jitmem, je);
    res = bsearch(&sym, sym, 2, 1, (void*) _start); // hack to jump into a function pointer
    if (((void*) 0) != res)
        return retval;
    else {
        printf("Error: can't find the function pointer");
        exit(0);
    }
}

int main(int argc, char **argv)
{
    int fd, bt, ty, poolsz, *idmain;
    int i;

    --argc; ++argv;
    if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; }
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'v') { verbose = 1; --argc; ++argv; }
    if (argc < 1) { printf("usage: mcc [-s] [-d] file ...\n"); return -1; }

    if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; }

    poolsz = 256*1024; // arbitrary size
    if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
    if (!(text = le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
    if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }

    memset(sym, 0, poolsz);
    memset(e, 0, poolsz);
    memset(data, 0, poolsz);

    p = "break case char default else enum if int return sizeof switch while "
        "open read write close printf malloc memset memcmp memcpy mmap dlsym "
        "bsearch __clear_cache exit void main";
    i = Break; while (i <= While) { next(); id[Tk] = i++; } // add keywords to symbol table
    i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // add library to symbol table
    next(); id[Tk] = Char; // handle void type
    next(); idmain = id; // keep track of main

    if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; }
    if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; }
    p[i] = 0;
    close(fd);

    // parse declarations
    line = 1;
    next();
    while (tk) {
        bt = INT; // basetype
        if (tk == Int) next();
        else if (tk == Char) { next(); bt = CHAR; }
        else if (tk == Enum) {
            next();
            if (tk != '{') next();
            if (tk == '{') {
                next();
                i = 0;
                while (tk != '}') {
                    if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
                    next();
                    if (tk == Assign) {
                        next();
                        if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
                        i = ival;
                        next();
                    }
                    id[Class] = Num; id[Type] = INT; id[Val] = i++;
                    if (tk == ',') next();
                }
                next();
            }
        }
        while (tk != ';' && tk != '}') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
            if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
            next();
            id[Type] = ty;
            if (tk == '(') { // function
                id[Class] = Fun;
                id[Val] = (int)(e + 1);
                next(); i = 0;
                while (tk != ')') {
                    ty = INT;
                    if (tk == Int) next();
                    else if (tk == Char) { next(); ty = CHAR; }
                    while (tk == Mul) { next(); ty = ty + PTR; }
                    if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
                    if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
                    id[HClass] = id[Class]; id[Class] = Loc;
                    id[HType]  = id[Type];  id[Type] = ty;
                    id[HVal]   = id[Val];   id[Val] = i++;
                    next();
                    if (tk == ',') next();
                }
                next();
                if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
                loc = ++i;
                next();
                while (tk == Int || tk == Char) {
                    bt = (tk == Int) ? INT : CHAR;
                    next();
                    while (tk != ';') {
                        ty = bt;
                        while (tk == Mul) { next(); ty = ty + PTR; }
                        if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
                        if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
                        id[HClass] = id[Class]; id[Class] = Loc;
                        id[HType]  = id[Type];  id[Type] = ty;
                        id[HVal]   = id[Val];   id[Val] = ++i;
                        next();
                        if (tk == ',') next();
                    }
                    next();
                }
                *++e = ENT; *++e = i - loc;
                while (tk != '}') stmt();
                *++e = LEV;
                id = sym; // unwind symbol table locals
                while (id[Tk]) {
                    if (id[Class] == Loc) {
                        id[Class] = id[HClass];
                        id[Type] = id[HType];
                        id[Val] = id[HVal];
                    }
                    id = id + Idsz;
                }
            }
            else {
                id[Class] = Glo;
                id[Val] = (int)data;
                data = data + sizeof(int);
            }
            if (tk == ',') next();
        }
        next();
    }

    return jit(poolsz, (int *)idmain[Val], argc, argv);
}
