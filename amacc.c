// Another Mini ARM C Compiler (AMaCC)

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
char *ops;            // opcodes

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
int verbose;         // print executed instructions
int elf;             // print ELF format
int elf_fd;
int rwdata_align_off;

// identifier
struct ident_s {
    int tk;
    int hash;
    char *name;
    int class;
    int type;
    int val;
    int stype;
    int hclass;
    int htype;
    int hval;
} *id,  // currently parsed identifier
  *sym; // symbol table (simple list of identifiers)

struct member_s {
    struct ident_s *id;
    int offset;
    int type;
    struct member_s *next;
} **members; // array (indexed by type) of struct member lists

// tokens and classes (operators last and in precedence order)
enum {
    Num = 128, Fun, Sys, Glo, Loc, Id,
    Break, Case, Char, Default, Else, Enum, If, Int, Return, Sizeof,
    Struct, Switch, For, While,
    Assign, Cond,
    Lor, Lan, Or, Xor, And,
    Eq, Ne, Lt, Gt, Le, Ge,
    Shl, Shr, Add, Sub, Mul, Inc, Dec, Dot, Arrow, Brak
};

// opcodes
enum {
    LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
    OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,
    OPEN,READ,WRIT,CLOS,PRTF,MALC,MSET,MCMP,MCPY,MMAP,DSYM,BSCH,CLCA,EXIT
};

// types
enum { CHAR, INT, PTR = 256, PTR2 = 512 };

// ELF generation
char **plt_func_addr;

char* append_strtab(char **strtab, char *str)
{
    int nbytes;
    char *res;
    nbytes = 0;
    while (str[nbytes] != '\0') {
        ++nbytes;
    }
    ++nbytes;
    res = *strtab;
    memcpy(res, str, nbytes);
    *strtab = res + nbytes;
    return res;
}

void next()
{
    char *pp;
    while ((tk = *p)) {
        ++p;
        if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') ||
            (tk == '_')) {
            pp = p - 1;
            while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
                   (*p >= '0' && *p <= '9') || (*p == '_'))
                tk = tk * 147 + *p++;
            tk = (tk << 6) + (p - pp);
            id = sym;
            while (id->tk) {
                if (tk == id->hash &&
                    !memcmp(id->name, pp, p - pp)) {
                    tk = id->tk;
                    return;
                }
                id = id + 1;
            }
            id->name = pp;
            id->hash = tk;
            tk = id->tk = Id;
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
                    printf("%8.4s", &ops[*++le * 5]);
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
        case '#':
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
            //  if .text too big rwdata v_addr will overlap it, add that to stay away from .text
            if (tk == '"') ival = (int) pp; else tk = Num;
            return;
        case '=': if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return;
        case '+': if (*p == '+') { ++p; tk = Inc; } else tk = Add; return;
        case '-': if (*p == '-') { ++p; tk = Dec; }
                  else if (*p == '>') { ++p; tk = Arrow; }
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
        case '*': tk = Mul; return;
        case '[': tk = Brak; return;
        case '?': tk = Cond; return;
        case '.': tk = Dot; return;
        default: return;
        }
    }
}

char fatal(char *msg) { printf("%d: %s\n", line, msg); exit(-1); }

void expr(int lev)
{
    int t, *b, sz;
    struct ident_s *d;
    struct member_s *m;

    switch (tk) {
    case 0: fatal("unexpected eof in expression");
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
        while (tk == Mul) { next(); ty = ty + PTR; }
        if (tk == ')')
            next();
        else fatal("close paren expected in sizeof");
        *++e = IMM; *++e = ty >= PTR ? sizeof(int) : tsize[ty];
        ty = INT;
        break;
    case Id:
        d = id; next();
        if (tk == '(') {
            next();
            t = 0;
            while (tk != ')') {
                expr(Assign); *++e = PSH; ++t;
                if (tk == ',') next();
            }
            next();
            switch (d->class) {
            case Sys: *++e = d->val; break;
            case Fun: *++e = JSR; *++e = d->val; break;
            default: fatal("bad function call");
            }
            if (t) { *++e = ADJ; *++e = t; }
            ty = d->type;
        }
        else if (d->class == Num) { *++e = IMM; *++e = d->val; ty = INT; }
        else {
            switch (d->class) {
            case Loc: *++e = LEA; *++e = loc - d->val; break;
            case Glo: *++e = IMM; *++e = d->val; break;
            default: fatal("undefined variable");
            }
            if ((ty = d->type) <= INT || ty >= PTR)
                *++e = (ty == CHAR) ? LC : LI;
        }
        break;
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
            while (tk == Mul) { next(); t = t + PTR; }
            if (tk == ')') next();
            else fatal("bad cast");
            expr(Inc);
            ty = t;
        }
        else {
            expr(Assign);
            if (tk == ')') next();
            else fatal("close paren expected");
        }
        break;
    case Mul:
        next(); expr(Inc);
        if (ty > INT) ty = ty - PTR;
        else fatal("bad dereference");
        if (ty <= INT || ty >= PTR) *++e = (ty == CHAR) ? LC : LI;
        break;
    case And:
        next(); expr(Inc);
        if (*e == LC || *e == LI) --e;
        ty = ty + PTR;
        break;
    case '!':
        next(); expr(Inc);
        *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT;
        break;
    case '~':
        next(); expr(Inc);
        *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT;
        break;
    case Add:
        next(); expr(Inc); ty = INT;
        break;
    case Sub:
        next(); *++e = IMM;
        if (tk == Num) { *++e = -ival; next(); }
        else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
        ty = INT;
        break;
    case Inc:
    case Dec:
        t = tk; next(); expr(Inc);
        switch (*e) {
        case LC: *e = PSH; *++e = LC; break;
        case LI: *e = PSH; *++e = LI; break;
        default: fatal("bad lvalue in pre-increment");
        }
        *++e = PSH;
        *++e = IMM;
        *++e = ty >= PTR2 ? sizeof(int) :
                            (ty >= PTR) ? tsize[ty - PTR] : 1;
        *++e = (t == Inc) ? ADD : SUB;
        *++e = (ty == CHAR) ? SC : SI;
        break;
    default: fatal("bad expression");
    }

    while (tk >= lev) { // top down operator precedence
        t = ty;
        switch (tk) {
        case Assign:
            next();
            if (*e == LC || *e == LI) *e = PSH;
            else fatal("bad lvalue in assignment");
            expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
            break;
        case Cond:
            next();
            *++e = BZ; b = ++e;
            expr(Assign);
            if (tk == ':') next();
            else fatal("conditional missing colon");
            *b = (int)(e + 3); *++e = JMP; b = ++e;
            expr(Cond);
            *b = (int)(e + 1);
            break;
        case Lor:
            next(); *++e = BNZ; b = ++e;
            expr(Lan); *b = (int)(e + 1); ty = INT;
            break;
        case Lan: next(); *++e = BZ; b = ++e;
            expr(Or); *b = (int)(e + 1); ty = INT;
            break;
        case Or:  next(); *++e = PSH;
            expr(Xor); *++e = OR;  ty = INT;
            break;
        case Xor: next(); *++e = PSH;
            expr(And); *++e = XOR; ty = INT;
            break;
        case And: next(); *++e = PSH;
            expr(Eq);  *++e = AND; ty = INT;
            break;
        case Eq:
            next(); *++e = PSH;
            expr(Lt);  *++e = EQ;  ty = INT;
            break;
        case Ne:
            next(); *++e = PSH;
            expr(Lt);  *++e = NE;  ty = INT;
            break;
        case Lt:  next(); *++e = PSH;
            expr(Shl); *++e = LT;  ty = INT;
            break;
        case Gt:  next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; break;
        case Le:  next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; break;
        case Ge:  next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; break;
        case Shl: next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; break;
        case Shr: next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; break;
        case Add:
            next(); *++e = PSH; expr(Mul);
            sz = (ty = t) >= PTR2 ? sizeof(int) :
                                    ty >= PTR ? tsize[ty - PTR] : 1;
            if (sz > 1) { *++e = PSH; *++e = IMM; *++e = sz; *++e = MUL; }
            *++e = ADD;
            break;
        case Sub:
            next(); *++e = PSH; expr(Mul);
            sz = t >= PTR2 ? sizeof(int) :
                             t >= PTR ? tsize[t - PTR] : 1;
            if (t == ty && sz > 1) {
                *++e = SUB; *++e = PSH; *++e = IMM; *++e = sz; *++e = SUB;
                ty = INT;
            } else if (sz > 1) {
                *++e = PSH; *++e = IMM; *++e = sz; *++e = MUL; *++e = SUB;
            } else *++e = SUB;
            ty = t;
            break;
        case Mul:
            next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT;
            break;
        case Inc:
        case Dec:
            if (*e == LC) { *e = PSH; *++e = LC; }
            else if (*e == LI) { *e = PSH; *++e = LI; }
            else fatal("bad lvalue in post-increment");
            sz = ty >= PTR2 ? sizeof(int) :
                              ty >= PTR ? tsize[ty - PTR] : 1;
            *++e = PSH; *++e = IMM; *++e = sz;
            *++e = (tk == Inc) ? ADD : SUB;
            *++e = (ty == CHAR) ? SC : SI;
            *++e = PSH; *++e = IMM; *++e = sz;
            *++e = (tk == Inc) ? SUB : ADD;
            next();
            break;
        case Dot:
            ty = ty + PTR;
        case Arrow:
            if (ty <= PTR+INT || ty >= PTR2) fatal("structure expected");
            next();
            if (tk != Id) fatal("structure member expected");
            m = members[ty - PTR]; while (m && m->id != id) m = m->next;
            if (!m) fatal("structure member not found");
            if (m->offset) {
                *++e = PSH; *++e = IMM; *++e = m->offset; *++e = ADD;
            }
            ty = m->type;
            if (ty <= INT || ty >= PTR) *++e = (ty == CHAR) ? LC : LI;
            next();
            break;
        case Brak:
            next(); *++e = PSH; expr(Assign);
            if (tk == ']') next();
            else fatal("close bracket expected");
            if (t < PTR) fatal("pointer type expected");
            sz = (t = t - PTR) >= PTR ? sizeof(int) : tsize[t];
            if (sz > 1) { *++e = PSH; *++e = IMM; *++e = sz; *++e = MUL; }
            *++e = ADD;
            if ((ty = t) <= INT || ty >= PTR) *++e = (ty == CHAR) ? LC : LI;
            break;
        default:
            printf("%d: compiler error tk=%d\n", line, tk); exit(-1);
        }
    }
}

void stmt()
{
    int *a, *b, *d;
    int *x, *y, *z;
    int i;

    switch (tk) {
    case If:
        next();
        if (tk == '(') next();
        else fatal("open paren expected");
        expr(Assign);
        if (tk == ')') next();
        else fatal("close paren expected");
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
        if (tk == '(') next();
        else fatal("open paren expected");
        expr(Assign);
        if (tk == ')') next();
        else fatal("close paren expected");
        *++e = BZ; b = ++e;
        stmt();
        *++e = JMP; *++e = (int)a;
        *b = (int)(e + 1);
        return;
    case Switch:
        next();
        if (tk == '(') next();
        else fatal("open paren expected");
        expr(Assign);
        if (tk == ')') next();
        else fatal("close paren expected");
        a = cas; *++e = JMP; cas = ++e;
        b = brks; d = def; brks = def = 0;
        stmt();
        *cas = def ? (int)def : (int)(e + 1); cas = a;
        while (brks) { a = (int *)*brks; *brks = (int)(e + 1); brks = a; }
        brks = b; def = d;
        return;
    case Case:
        *++e = JMP; ++e;
        *e = (int)(e + 7); *++e = PSH; i = *cas; *cas = (int)e;
        next();
        expr(Or);
        if (e[-1] != IMM) fatal("bad case immediate");
        *e = *e - i; *++e = SUB; *++e = BNZ; cas = ++e; *e = i + e[-3];
        if (tk == ':') next();
        else fatal("colon expected");
        stmt();
        return;
    case Break:
        next();
        if (tk == ';') next();
        else fatal("semicolon expected");
        *++e = JMP; *++e = (int)brks; brks = e;
        return;
    case Default:
        next();
        if (tk == ':') next();
        else fatal("colon expected");
        def = e + 1;
        stmt();
        return;
    case Return:
        next();
        if (tk != ';') expr(Assign);
        *++e = LEV;
        if (tk == ';') next();
        else fatal("semicolon expected");
        return;
    case For:
        next();
        if (tk == '(') next();
        else fatal("open paren expected");
        expr(Assign);
        while (tk == ',') {
            next();
            expr(Assign);
        }
        if (tk == ';') next();
        else fatal("semicolon expected");
        a = e + 1;
        expr(Assign);
        if (tk == ';') next();
        else fatal("semicolon expected");
        *++e = BZ; b = ++e;
        x = e + 1; // Points to entry of for loop afterthought
        expr(Assign);
        while (tk == ',') {
            next();
            expr(Assign);
        }
        if (tk == ')') next();
        else fatal("close paren expected");
        y = e + 1; // Points to entry of for loop body
        stmt();
        z = e + 1; // Points to entry of jmp command
        *++e = JMP; *++e = (int) a;
        *b = (int) (e + 1);

        // Swaps body chunk and afterthought chunk
        //
        // We parse it as:
        // Init -> Cond -> Bz -> After -> Body -> Jmp
        //
        // But we want it to be:
        // Init -> Cond -> Bz -> Body -> After -> Jmp
        memcpy((void *) ((int) e + 4), x, (int) y - (int) x);
        memcpy(x, y, (int) z - (int) y);
        memcpy((void *) ((int) x + (int) z - (int) y),
               (void *) ((int) e + 4), (int)y - (int) x);
        memset((void *) ((int) e + 4), 0, (int)y - (int) x);
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
        if (tk == ';') next();
        else fatal("semicolon expected");
    }
}

void die(char *msg) { printf("codegen: %s\n", msg); exit(2); }

int *codegen(int *jitmem, int *jitmap)
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
            printf("%p -> %p: %8.4s", pc, je, &ops[i * 5]);
            if (i <= ADJ) printf(" %d\n", pc[1]); else printf("\n");
        }
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
            else { if (!imm0) imm0 = je; *il++ = (int)(je++); *iv++ = tmp;}
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
            *je++ = 0xe5d00000; if (neg_int < 0)  *je++ = 0xe6af0070; // ldrb r0, [r0]; (sxtb r0, r0)
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
                je = je + 2;
                break;
            }
            else if (i >= OPEN) {
                switch (i) {
                case OPEN:
                    tmp = (int) (elf ? plt_func_addr[0] : dlsym(0, "open"));
                    break;
                case READ:
                    tmp = (int) (elf ? plt_func_addr[1] : dlsym(0, "read"));
                    break;
                case WRIT:
                    tmp = (int) (elf ? plt_func_addr[2] : dlsym(0, "write"));
                    break;
                case CLOS:
                    tmp = (int) (elf ? plt_func_addr[3] : dlsym(0, "close"));
                    break;
                case PRTF:
                    tmp = (int) (elf ? plt_func_addr[4] : dlsym(0, "printf"));
                    break;
                case MALC:
                    tmp = (int) (elf ? plt_func_addr[5] : dlsym(0, "malloc"));
                    break;
                case MSET:
                    tmp = (int) (elf ? plt_func_addr[6] : dlsym(0, "memset"));
                    break;
                case MCMP:
                    tmp = (int) (elf ? plt_func_addr[7] : dlsym(0, "memcmp"));
                    break;
                case MCPY:
                    tmp = (int) (elf ? plt_func_addr[8] : dlsym(0, "memcpy"));
                    break;
                case MMAP:
                    tmp = (int) (elf ? plt_func_addr[9] : dlsym(0, "mmap"));
                    break;
                case DSYM:
                    tmp = (int) (elf ? plt_func_addr[10] : dlsym(0, "dlsym"));
                    break;
                case BSCH:
                    tmp = (int) (elf ? plt_func_addr[11] : dlsym(0, "bsearch"));
                    break;
                case EXIT:
                    tmp = (int) (elf ? plt_func_addr[13] : dlsym(0, "exit"));
                    break;
                default:
                    printf("unrecognized code %d\n", i);
                    return 0;
                }
                if (*pc++ != ADJ) die("no ADJ after native proc!");
                i = *pc;
                if (i > 10) die("no support for 10+ arguments!");
                while (i > 0) *je++ = 0xe49d0004 | (--i << 12); // pop r(i-1)
                i = *pc++;
                if (i > 4) *je++ = 0xe92d03f0;               // push {r4-r9}
                *je++ = 0xe28fe000;                          // add lr, pc, #0
                if (!imm0) imm0 = je;
                *il++ = (int)je++ + 1;
                *iv++ = tmp;
                if (i > 4) *je++ = 0xe28dd018;              // add sp, sp, #24
                break;
            }
            else { printf("code generation failed for %d!\n", i); return 0; }
        }

        if (imm0) {
            if (i == LEV) genpool = 1;
            else if ((int) je > (int) imm0 + 3000) {
                tje = je++; genpool = 2;
            }
        }
        if (genpool) {
            if (verbose) printf("POOL %d %d %d\n", genpool,
                                                   il - immloc, je - imm0);
            *iv = 0;
            while (il > immloc) {
                tmp = *--il;
                if ((int) je > tmp + 4096 + 8) die("can't reach the pool");
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
    if (il > immloc) die("code is not terminated by a LEV");
    tje = je;

    // second pass
    pc = text + 1;
    while (pc <= e) {
        je = (int *) jitmap[((int) pc - (int) text) >> 2]; i = *pc++;
        if (i == JSR || i == JMP || i == BZ || i == BNZ) {
            switch (i) {
            case JSR:
                *je = 0xeb000000;  // bl #(tmp)
                break;
            case JMP:
                *je = 0xea000000;  // bl #(tmp)
                break;
            case BZ:
                *++je = 0x0a000000; // beq #(tmp)
                break;
            case BNZ:
                *++je = 0x1a000000; // bne #(tmp)
                break;
            }
            tmp = *pc++;
            *je = *je |
                  (((jitmap[(tmp - (int) text) >> 2] - (int) je - 8) >> 2) &
                   0x00ffffff);
        }
        else if (i < LEV) { ++pc; }
    }
    return tje;
}

enum {
    _PROT_EXEC = 4, _PROT_READ = 1, _PROT_WRITE = 2,
    _MAP_PRIVATE = 2, _MAP_ANON = 32
};
int jit(int poolsz, int *start, int argc, char **argv)
{
    char *jitmem;  // executable memory for JIT-compiled native code
    int *je, *tje, *_start,  retval, *jitmap, *res;

    // setup JIT memory
    jitmem = mmap(0, poolsz, _PROT_EXEC | _PROT_READ | _PROT_WRITE,
                  _MAP_PRIVATE | _MAP_ANON, -1, 0);
    if (!jitmem) {
        printf("could not mmap(%d) jit executable memory\n", poolsz);
        return -1;
    }
    if (src) return 1;
    jitmap = (int *) (jitmem + (poolsz >> 1));
    je = (int *) jitmem;
    *je++ = (int) &retval;
    *je++ = argc;
    *je++ = (int) argv;
    _start = je;
    *je++ = 0xe92d5ff0;       // push    {r4-r12, lr}
    *je++ = 0xe51f0014;       // ldr     r0, [pc, #-20] ; argc
    *je++ = 0xe51f1014;       // ldr     r1, [pc, #-20] ; argv
    *je++ = 0xe52d0004;       // push    {r0}
    *je++ = 0xe52d1004;       // push    {r1}
    tje = je++;               // bl      jitmain
    *je++ = 0xe51f502c;       // ldr     r5, [pc, #-44] ; retval
    *je++ = 0xe5850000;       // str     r0, [r5]
    *je++ = 0xe28dd008;       // add     sp, sp, #8
    *je++ = 0xe8bd9ff0;       // pop     {r4-r12, pc}
    if (!(je = codegen(je, jitmap))) return 1;
    if (je >= jitmap) die("jitmem too small");
    *tje = 0xeb000000 |
           (((jitmap[((int) start - (int) text) >> 2] -
              (int) tje - 8) >> 2) &
            0x00ffffff);

    // hack to jump into specific function pointer
    __clear_cache(jitmem, je);
    res = bsearch(&sym, sym, 1, 1, (void *) _start);
    if (((void *) 0) != res) return 0; return -1; // make compiler happy
}

int ELF32_ST_INFO(int b, int t) { return (b << 4) + (t & 0xf); }
enum { PHDR_SIZE = 32, SHDR_SIZE = 40, SYM_SIZE = 16 };

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
    int sh_entsize;   // [Elf32_Word] Size of records contained within
                      //              the section
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
    STB_LOCAL = 0,   // Local symbol, not visible outside obj file
                     //               containing def
    STB_GLOBAL = 1,  // Global symbol, visible to all object files
                     //                being combined

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

    // Dynamic table entry tags
    DT_NULL         = 0,     // Marks end of dynamic array
    DT_NEEDED       = 1,     // String table offset of needed library
    DT_PLTRELSZ     = 2,     // Size of relocation entries in PLT
    DT_PLTGOT       = 3,     // Address associated with linkage table
    DT_STRTAB       = 5,     // Address of dynamic string table
    DT_SYMTAB       = 6,     // Address of dynamic symbol table
    DT_STRSZ        = 10,    // Total size of the string table
    DT_SYMENT       = 11,    // Size of a symbol table entry
    DT_REL          = 17,    // Address of relocation table (Rel entries)
    DT_RELSZ        = 18,    // Size of Rel relocation table
    DT_RELENT       = 19,    // Size of a Rel relocation entry
    DT_PLTREL       = 20,    // Type of relocation entry used for linking
    DT_JMPREL       = 23,    // Address of relocations associated with PLT
};

int PT_idx, SH_idx, sym_idx;

int gen_PT(char *ptr, int type, int offset, int addr, int size,
           int flag, int align)
{
    struct Elf32_Phdr *phdr;
    phdr = (struct Elf32_Phdr *) ptr;
    phdr->p_type =  type;
    phdr->p_offset = offset;
    phdr->p_vaddr = addr;
    phdr->p_paddr = addr;
    phdr->p_filesz = size;
    phdr->p_memsz = size;
    phdr->p_flags = flag;
    phdr->p_align = align;
    return PT_idx++;
}

int gen_SH(char *ptr, int type, int name, int offset, int addr,
           int size, int link, int info,
           int flag, int align, int entsize)
{
    struct Elf32_Shdr *shdr;
    shdr = (struct Elf32_Shdr *) ptr;
    shdr->sh_name = name;
    shdr->sh_type = type;
    shdr->sh_addr = addr;
    shdr->sh_offset = offset;
    shdr->sh_size = size;
    shdr->sh_link = link;
    shdr->sh_info = info;
    shdr->sh_flags = flag;
    shdr->sh_addralign = align;
    shdr->sh_entsize = entsize;
    return SH_idx++;
}

int gen_sym(char *ptr, int name, unsigned char info,
            int shndx, int size, int value)
{
    struct Elf32_Sym *sym;
    sym = (struct Elf32_Sym *) ptr;
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
    int idx;
    idx = gen_sym(*data, name, ELF32_ST_INFO(STB_GLOBAL, STT_FUNC), 0, 0, 0);
    *data = *data + SYM_SIZE;
    return idx;
}

enum { ALIGN = 4096 };

int elf32(int poolsz, int *start)
{
    char *o, *buf, *code, *entry, *je, *tje;
    char *to, *phdr, *dseg;
    char *pt_dyn, *libc, *ldso, *linker;
    int pt_dyn_off, linker_off, code_off, i;
    int *jitmap;
    char *tmp_code;
    int jitcode_off;
    int FUNC_NUM;
    char *e_shoff;

    int code_size, rel_size, rel_off;
    char *rel_addr, *plt_addr, *code_addr, *_data_end, *shstrtab_addr;
    int plt_size, plt_off, pt_dyn_size, rwdata_off;
    int shstrtab_off, shstrtab_size;
    char *dynstr_addr, *dynsym_addr, *_gap, *got_addr;
    int dynstr_off, dynstr_size;
    int dynsym_off, dynsym_size, gap, got_off;
    char *to_got_movw, *to_got_movt;
    char **got_func_slot;
    int *func_names;
    int got_size, rwdata_size, sh_dynstr_idx, sh_dynsym_idx;
    int code_size_align;

    code = malloc(poolsz);
    buf = malloc(poolsz);
    jitmap = (int *) (code + (poolsz >> 1));
    memset(buf, 0, poolsz);
    o = buf = (char *) (((int) buf + ALIGN - 1)  & -ALIGN);
    code =    (char *) (((int) code + ALIGN - 1) & -ALIGN);

    PT_idx = 0;
    SH_idx = 0;
    sym_idx = 0;

    // we must get the plt_func_addr[x] non-zero value, otherwise
    // code length after codegen will be wrong
    FUNC_NUM = EXIT - OPEN + 1;
    plt_func_addr = malloc(FUNC_NUM);
    for (i = 0; i < FUNC_NUM; i++)
        plt_func_addr[i] = o;

    tmp_code = code;
    jitcode_off = 7;  // 7 instruction (tmp_code)
    tmp_code = tmp_code + jitcode_off * 4;
    je = (char *) codegen((int *) tmp_code, jitmap);
    if (!je) return 1;
    if ((int*) je >= jitmap) die("jitmem too small");

    // elf32_hdr
    *o++ = 0x7f; *o++ = 'E'; *o++ = 'L'; *o++ = 'F';
    *o++ = 1;    *o++ = 1;   *o++ = 1;   *o++ = 0;
    o = o + 8;
    *o++ = 2; *o++ = 0; *o++ = 40; *o++ = 0; // e_type 2 = executable
                                             // e_machine 40 = ARM
    *(int *) o = 1; o = o + 4;
    entry = o; o = o + 4; // e_entry
    *(int *) o = 52; o = o + 4; // e_phoff
    e_shoff = o; o = o + 4; // e_shoff
    *(int *) o = 0x5000402;           o = o + 4; // e_flags
    *o++ = 52; *o++ = 0;
    *o++ = 32; *o++ = 0; *o++ = 5; *o++ = 0; // e_phentsize & e_phnum
    *o++ = 40; *o++ = 0; *o++ = 12; *o++ = 0; // e_shentsize & e_shnum
    *o++ =  1; *o++ = 0;

    phdr = o; o = o + PHDR_SIZE * 4; // e_phentsize * e_phnum for phdr size
    o = (char *) (((int) o + ALIGN - 1)  & -ALIGN); // to 0x1000
    code_off = o - buf; // 0x1000
    // must add a value >= 4. sometimes first codegen size is not equal
    // to second codegen
    code_size = je - code + 128;
    rel_size = 8 * FUNC_NUM;
    rel_off = code_off + code_size;
    rel_addr = o + code_size;

    plt_size = 20 + 12 * FUNC_NUM;
    plt_off = rel_off + rel_size;
    plt_addr = rel_addr + rel_size;

    code_addr = o;
    o++;
    code_size_align = (((int) code_size + ALIGN - 1)  & -ALIGN);
    o = (char *)(((int)(o + code_size) + ALIGN - 1)  & -ALIGN); // to 0x2000
    memcpy(code_addr, code,  0x1000);
    *(int *) entry = (int) code_addr;

    // data
    pt_dyn_size = 14 * 8 + 16;
    dseg = o; o = o + 4096; // 0x3000
    _data_end = data;
    data = (char *) (((int) data + ALIGN - 1) & -ALIGN); // force data address
                                                         // align to offset
    pt_dyn = data; pt_dyn_off = dseg - buf; data = data + pt_dyn_size;
    linker = data; memcpy(linker, "/lib/ld-linux-armhf.so.3", 25);
    linker_off = pt_dyn_off + pt_dyn_size; data = data + 25;

    // the first 4k in this address space is for elf header, especially
    // elf_phdr because ld.so must be able to see it
    // PT_LOAD for code
    to = phdr;
    // code_idx
    gen_PT(to, PT_LOAD, 0, (int) buf, code_size_align + 0x1000,
           PF_X | PF_R, 0x1000);
    to = to+ PHDR_SIZE;
    code_size_align = code_size_align - 0x1000;
    // PT_LOAD for data
    // data_idx
    gen_PT(to, PT_LOAD, pt_dyn_off, (int) pt_dyn + code_size_align, 4096,
           PF_W | PF_R, 0x1000);
    to = to + PHDR_SIZE;

    // PT_INTERP
    // interp_idx
    gen_PT(to, PT_INTERP, linker_off, (int) linker + code_size_align, 
           25 , PF_R, 0x1);
    to = to + PHDR_SIZE;

    // PT_DYNAMIC
    gen_PT(to, PT_DYNAMIC, pt_dyn_off, (int) pt_dyn + code_size_align, 
           pt_dyn_size , PF_R | PF_W, 4);
    to = to + PHDR_SIZE;

    // offset and v_addr must align for 0xFFF(or 0xFFFF?)
    rwdata_off = (pt_dyn_off + 0x1000) | ((int) _data & 0xfff);
    // PT_LOAD for others data
    // FIXME: .text and .rwdata will be at least 3 * 256 * 1024 =
    // 3 * 2^18 bytes = 0xc0000 offset
    // so now AMaCC can only generate code which code size is less than
    // about 786K
    gen_PT(to, PT_LOAD, rwdata_off, (int)_data, 
           _data_end - _data, PF_X | PF_R | PF_W, 1);
    to = to + PHDR_SIZE;

    // .shstrtab (embedded in PT_LOAD of data)
    shstrtab_addr = data;
    shstrtab_off = (int)(data - pt_dyn) + (int)(dseg - buf);
    shstrtab_size = 0;
    append_strtab(&data, "");
    append_strtab(&data, ".shstrtab");
    append_strtab(&data, ".text");
    append_strtab(&data, ".data");
    append_strtab(&data, ".dynamic");
    append_strtab(&data, ".strtab");
    append_strtab(&data, ".symtab");
    append_strtab(&data, ".dynstr");
    append_strtab(&data, ".dynsym");
    append_strtab(&data, ".interp");
    append_strtab(&data, ".rel.plt");
    append_strtab(&data, ".plt");
    append_strtab(&data, ".got");
    append_strtab(&data, ".rwdata");
    shstrtab_size = data - shstrtab_addr;

    // .dynstr (embedded in PT_LOAD of data)
    dynstr_addr = data;
    dynstr_off = shstrtab_off + shstrtab_size;
    append_strtab(&data, "");
    libc = append_strtab(&data, "libc.so.6");
    ldso = append_strtab(&data, "libdl.so.2");

    func_names = (int *)malloc(sizeof(int) * (EXIT + 1));
    if (!func_names) {
        die("could not malloc func_names table\n");
    }

    func_names[OPEN] = append_strtab(&data, "open") - dynstr_addr;
    func_names[READ] = append_strtab(&data, "read") - dynstr_addr;
    func_names[WRIT] = append_strtab(&data, "write") - dynstr_addr;
    func_names[CLOS] = append_strtab(&data, "close") - dynstr_addr;
    func_names[PRTF] = append_strtab(&data, "printf") - dynstr_addr;
    func_names[MALC] = append_strtab(&data, "malloc") - dynstr_addr;
    func_names[MSET] = append_strtab(&data, "memset") - dynstr_addr;
    func_names[MCMP] = append_strtab(&data, "memcmp") - dynstr_addr;
    func_names[MCPY] = append_strtab(&data, "memcpy") - dynstr_addr;
    func_names[MMAP] = append_strtab(&data, "mmap") - dynstr_addr;
    func_names[DSYM] = append_strtab(&data, "dlsym") - dynstr_addr;
    func_names[BSCH] = append_strtab(&data, "bsearch") - dynstr_addr;
    func_names[CLCA] = append_strtab(&data, "__clear_cache") - dynstr_addr;
    func_names[EXIT] = append_strtab(&data, "exit") - dynstr_addr;

    dynstr_size = data - dynstr_addr;

    // .dynsym (embedded in PT_LOAD of data)
    dynsym_addr = data;
    dynsym_off = dynstr_off + dynstr_size;
    memset(data, 0, SYM_SIZE);
    data = data + SYM_SIZE;

    append_func_sym(&data, func_names[OPEN]);
    append_func_sym(&data, func_names[READ]);
    append_func_sym(&data, func_names[WRIT]);
    append_func_sym(&data, func_names[CLOS]);
    append_func_sym(&data, func_names[PRTF]);
    append_func_sym(&data, func_names[MALC]);
    append_func_sym(&data, func_names[MSET]);
    append_func_sym(&data, func_names[MCMP]);
    append_func_sym(&data, func_names[MCPY]);
    append_func_sym(&data, func_names[MMAP]);
    append_func_sym(&data, func_names[DSYM]);
    append_func_sym(&data, func_names[BSCH]);
    append_func_sym(&data, func_names[CLCA]);
    append_func_sym(&data, func_names[EXIT]);

    dynsym_size = SYM_SIZE * (FUNC_NUM + 1);
    _gap = data;
    data = (char *) (((int) data + 15) & -16);
    gap = (int) data - (int) _gap;
    // .got
    got_addr = data;
    got_off = dynsym_off + dynsym_size + gap;
    *(int *) data = (int) pt_dyn; data = data + 4;
    data = data + 4;  // reserved 2 and 3 entry for linker
    to_got_movw = data + code_size_align;
    to_got_movt = data + code_size_align;  // the address manupulates dynamic
                                           // linking, plt must jump here 
    data = data + 4;  // reserved 2 and 3 entry for linker
    // .got function slot
    got_func_slot = malloc(FUNC_NUM);
    for (i = 0; i < FUNC_NUM; i++) {
        got_func_slot[i] = data + code_size_align;
        *(int *) data = (int) plt_addr; data = data + 4;
    }
    data = data + 4;  // end with 0x0
    got_size = (int) data - (int) got_addr;

    // .plt 
    to = plt_addr;
    *(int *) to = 0xe52de004; to = to + 4; // push {lr}
    // movw r10 addr_to_got
    *(int *) to = 0xe300a000 | (0xfff & (int) (to_got_movw)) |
                  (0xf0000 & ((int)(to_got_movw) << 4));
    to = to + 4;
    // movt r10 addr_to_got
    *(int *) to = 0xe340a000 | (0xfff & ((int) (to_got_movt) >> 16)) |
                  (0xf0000 & ((int) (to_got_movt) >> 12));
    to = to + 4;
    *(int *) to = 0xe1a0e00a; to = to + 4;  // mov lr,r10
    *(int *) to = 0xe59ef000; to = to + 4;  // ldr pc, [lr]

    // We must preserve ip for code below, dyn link use this as return address
    for (i = 0; i < FUNC_NUM; i++) {
        plt_func_addr[i] = to;
        // movt ip addr_to_got
        *(int *) to = 0xe300c000 | (0xfff & (int) (got_func_slot[i])) |
                      (0xf0000 & ((int) (got_func_slot[i]) << 4));
        to = to + 4;
        // movw ip addr_to_got
        *(int *) to = 0xe340c000 |
                      (0xfff & ((int) (got_func_slot[i]) >> 16)) |
                      (0xf0000 & ((int) (got_func_slot[i]) >> 12));
        to = to + 4;
        *(int *) to = 0xe59cf000; to = to + 4;  // ldr pc, [ip]
    }
    
    // .rel.plt
    to = rel_addr;
    for (i = 0; i < FUNC_NUM; i++) { 
        *(int *) to = (int) got_func_slot[i]; to = to + 4; 
        *(int *) to = 0x16 | (i + 1) << 8 ; to = to + 4;
        // 0x16 R_ARM_JUMP_SLOT | .dymstr index << 8
    }

    //  .rwdata
    rwdata_size = _data_end - _data;
    o = o + (rwdata_off - (pt_dyn_off + 0x1000)); // rwdata_off has align x bytes
    memcpy(o, _data, rwdata_size);
    o = o + rwdata_size;
    *(int *) e_shoff = (int)(o - buf);
    // .dynamic (embedded in PT_LOAD of data)
    to = pt_dyn;
    *(int *) to =  5; to = to + 4; *(int *) to = (int) dynstr_addr +
                                                 code_size_align; to = to + 4;
    *(int *) to = 10; to = to + 4; *(int *) to = dynstr_size; to = to + 4;
    *(int *) to =  6; to = to + 4; *(int *) to = (int) dynsym_addr + code_size_align; to = to + 4;
    *(int *) to = 11; to = to + 4; *(int *) to = 16; to = to + 4;
    *(int *) to = 17; to = to + 4; *(int *) to = (int) rel_addr; to = to + 4;
    *(int *) to = 18; to = to + 4; *(int *) to = rel_size; to = to + 4;
    *(int *) to = 19; to = to + 4; *(int *) to = 8; to = to + 4;
    *(int *) to =  3; to = to + 4; *(int *) to = (int) got_addr + code_size_align; to = to + 4;
    *(int *) to =  2; to = to + 4; *(int *) to = rel_size; to = to + 4;
    *(int *) to = 20; to = to + 4; *(int *) to = 17; to = to + 4;
    *(int *) to = 23; to = to + 4; *(int *) to = (int) rel_addr; to = to + 4;
    *(int *) to =  1; to = to + 4; *(int *) to = libc - dynstr_addr; to = to + 4;
    *(int *) to =  1; to = to + 4; *(int *) to = ldso - dynstr_addr; to = to + 4;
    *(int *) to =  0; to = to + 8;

    // we generate code again bacause address of .plt function slots
    // must be confirmed
    tmp_code = code;
    *(int *) tmp_code = 0xe1a00001;
             tmp_code = tmp_code + 4;  // mov     r0, r1 ; argc
    *(int *) tmp_code = 0xe1a01002;
             tmp_code = tmp_code + 4;  // mov     r1, r2 ; argv
    *(int *) tmp_code = 0xe52d0004;
             tmp_code = tmp_code + 4;  // push    {r0}
    *(int *) tmp_code = 0xe52d1004;
             tmp_code = tmp_code + 4;  // push    {r1}
             tje = tmp_code;
             tmp_code = tmp_code + 4;  // bl      jitmain
    *(int *) tmp_code = 0xe3a07001;
             tmp_code = tmp_code + 4;  // mov     r7, #1
    *(int *) tmp_code = 0xef000000;
             tmp_code = tmp_code + 4;  // svc 0
    je = (char *) codegen((int *) tmp_code, jitmap);
    if (!je)
        return 1;
    if ((int *) je >= jitmap) die("jitmem too small");
    *(int *) tje = 0xeb000000 |
          (((jitmap[((int) start - (int) text) >> 2] - (int) tje - 8) >> 2) &
           0x00ffffff);
    memcpy(code_addr, code,  je - code);

    gen_SH(o, SHT_NULL, 0, 0, 0, 0,
           0, 0, 0, 0, 0);
    o = o + SHDR_SIZE;

    // sh_shstrtab_idx
    gen_SH(o, SHT_STRTAB, 1, shstrtab_off, 0, shstrtab_size,
           0, 0, 0, 1, 0);
    o = o + SHDR_SIZE;

    // sh_text_idx
    gen_SH(o, SHT_PROGBITS, 11, code_off, (int) code_addr, code_size,
           0, 0, SHF_ALLOC | SHF_EXECINSTR, 4, 0);
    o = o + SHDR_SIZE;

    // sh_data_idx
    gen_SH(o, SHT_PROGBITS, 17, pt_dyn_off, (int) pt_dyn + code_size_align, 0x1000,
           0, 0, SHF_ALLOC | SHF_WRITE, 4, 0);
    o = o + SHDR_SIZE;

    sh_dynstr_idx =
    gen_SH(o, SHT_STRTAB, 48, dynstr_off, (int) dynstr_addr + code_size_align, dynstr_size,
           0, 0, SHF_ALLOC, 1, 0);
    o = o + SHDR_SIZE;
    
    sh_dynsym_idx =
    gen_SH(o, SHT_DYNSYM, 56, dynsym_off, (int) dynsym_addr + code_size_align, dynsym_size,
          sh_dynstr_idx, 1, SHF_ALLOC, 4, 0x10);
    o = o + SHDR_SIZE;
    
    // sh_dynamic_idx
    gen_SH(o, SHT_DYNAMIC, 23, pt_dyn_off, (int) pt_dyn + code_size_align, pt_dyn_size,
           sh_dynstr_idx, 0, SHF_ALLOC | SHF_WRITE, 4, 0);
    o = o + SHDR_SIZE;

    // sh_interp_idx
    gen_SH(o, SHT_PROGBITS, 64, linker_off, (int) linker + code_size_align, 25,
           0, 0, SHF_ALLOC, 1, 0);
    o = o + SHDR_SIZE;

    // sh_rel_idx
    gen_SH(o, SHT_REL, 72, rel_off, (int) rel_addr, rel_size,
          sh_dynsym_idx, 11, SHF_ALLOC | 0x40, 4, 8);
    o = o + SHDR_SIZE;

    // sh_plt_idx
    gen_SH(o, SHT_PROGBITS, 81, plt_off, (int) plt_addr, plt_size,
           0, 0, SHF_ALLOC | SHF_EXECINSTR, 4, 4);
    o = o + SHDR_SIZE;

    // sh_got_idx
    gen_SH(o, SHT_PROGBITS, 86, got_off, (int)got_addr + code_size_align, got_size,
           0, 0, SHF_ALLOC | SHF_WRITE, 4, 4);
    o = o + SHDR_SIZE;

    // sh_rwdata_idx
    gen_SH(o, SHT_PROGBITS, 91, rwdata_off, (int)_data, rwdata_size,
           0, 0, SHF_ALLOC, 0, 1);
    o = o + SHDR_SIZE;

    memcpy(dseg, pt_dyn, 0x1000);
    write(elf_fd, buf, o - buf);
    return 0;
}

enum { _O_CREAT = 64, _O_WRONLY = 1 };
int main(int argc, char **argv)
{
    int fd, bt, mbt, ty, poolsz;
    struct ident_s *idmain;
    struct member_s *m;
    int i;

    --argc; ++argv;
    if (argc > 0 && **argv == '-' && (*argv)[1] == 's') {
        src = 1; --argc; ++argv;
    }
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'v') {
        verbose = 1; --argc; ++argv;
    }
    if (argc > 0 && **argv == '-' && (*argv)[1] == 'o') {
        elf = 1; --argc; ++argv;
        if ((elf_fd = open(*argv, _O_CREAT | _O_WRONLY, 0775)) < 0) {
            printf("could not open(%s)\n", *argv); return -1;
        }
        ++argv;
    }
    if (argc < 1) {
        printf("usage: amacc [-s] [-v] [-o object] file ...\n"); return -1;
    }

    if ((fd = open(*argv, 0)) < 0) {
        printf("could not open(%s)\n", *argv); return -1;
    }

    poolsz = 256*1024; // arbitrary size
    if (!(text = le = e = malloc(poolsz))) {
        printf("could not malloc(%d) text area\n", poolsz); return -1;
    }
    if (!(sym = malloc(poolsz))) {
        printf("could not malloc(%d) symbol area\n", poolsz); return -1;
    }
    if (!(_data = data = malloc(poolsz))) {
        printf("could not malloc(%d) data area\n", poolsz); return -1;
    }
    if (!(tsize = malloc(PTR * sizeof(int)))) {
        printf("could not malloc() tsize area\n"); return -1;
    }
    if (!(members = malloc(PTR * sizeof(struct member_s *)))) {
        printf("could not malloc() members area\n"); return -1;
    }

    memset(sym, 0, poolsz);
    memset(e, 0, poolsz);
    memset(data, 0, poolsz);

    memset(tsize,   0, PTR * sizeof(int));
    memset(members, 0, PTR * sizeof(struct member_s *));

    ops = "LEA  IMM  JMP  JSR  BZ   BNZ  ENT  ADJ  LEV  "
          "LI   LC   SI   SC   PSH  "
          "OR   XOR  AND  EQ   NE   LT   GT   LE   GE   "
          "SHL  SHR  ADD  SUB  MUL  "
          "OPEN READ WRIT CLOS PRTF MALC MSET MCMP MCPY "
          "DSYM BSCH MMAP CLCA EXIT";

    p = "break case char default else enum if int return "
        "sizeof struct switch for while "
        "open read write close printf malloc memset memcmp memcpy mmap dlsym "
        "bsearch __clear_cache exit void main";

    i = Break;
    while (i <= While) { // add keywords to symbol table
        next(); id->tk = i++;
    }

    i = OPEN;
    while (i <= EXIT) { // add library to symbol table
        next(); id->class = Sys; id->type = INT; id->val = i++;
    }
    next(); id->tk = Char; // handle void type
    next(); idmain = id; // keep track of main

    if (!(lp = p = malloc(poolsz))) {
        printf("could not malloc(%d) source area\n", poolsz); return -1;
    }
    if ((i = read(fd, p, poolsz-1)) <= 0) {
        printf("read() returned %d\n", i); return -1;
    }
    p[i] = 0;
    close(fd);

    // add primitive types
    tsize[tnew++] = sizeof(char);
    tsize[tnew++] = sizeof(int);
    // parse declarations
    line = 1;
    next();
    while (tk) {
        bt = INT; // basetype
        switch (tk) {
        case Int:
            next();
            break;
        case Char:
            next(); bt = CHAR;
            break;
        case Enum:
            next();
            if (tk != '{') next();
            if (tk == '{') {
                next();
                i = 0;
                while (tk != '}') {
                    if (tk != Id) {
                        printf("%d: bad enum identifier %d\n", line, tk);
                        return -1;
                    }
                    next();
                    if (tk == Assign) {
                        next();
                        if (tk != Num) {
                            printf("%d: bad enum initializer\n", line);
                            return -1;
                        }
                        i = ival;
                        next();
                    }
                    id->class = Num; id->type = INT; id->val = i++;
                    if (tk == ',') next();
                }
                next();
            }
            break;
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
                if (members[bt]) {
                    printf("%d: duplicate structure definition\n", line);
                    return -1;
                }
                i = 0;
                while (tk != '}') {
                    mbt = INT;
                    switch (tk) {
                    case Int: next(); break;
                    case Char: next(); mbt = CHAR; break;
                    case Struct:
                        next(); 
                        if (tk != Id) {
                            printf("%d: bad struct declaration\n", line);
                            return -1;
                        }
                        mbt = id->stype;
                        next(); break;
                    }
                    while (tk != ';') {
                        ty = mbt;
                        while (tk == Mul) { next(); ty = ty + PTR; }
                        if (tk != Id) {
                            printf("%d: bad struct member definition\n", line);
                            return -1;
                        }
                        m = malloc(sizeof(struct member_s));
                        m->id = id;
                        m->offset = i;
                        m->type = ty;
                        m->next = members[bt];
                        members[bt] = m;
                        i = i + (ty >= PTR ? sizeof(int) : tsize[ty]);
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
        }
        while (tk != ';' && tk != '}') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) {
                printf("%d: bad global declaration\n", line); return -1;
            }
            if (id->class) {
                printf("%d: duplicate global definition\n", line); return -1;
            }
            next();
            id->type = ty;
            if (tk == '(') { // function
                id->class = Fun;
                id->val = (int)(e + 1);
                next(); i = 0;
                while (tk != ')') {
                    ty = INT;
                    switch (tk) {
                    case Int: next(); break;
                    case Char: next(); ty = CHAR; break;
                    case Struct:
                        next(); 
                        if (tk != Id) {
                            printf("%d: bad struct declaration\n", line);
                            return -1;
                        }
                        ty = id->stype;
                        next(); break;
                    }
                    while (tk == Mul) { next(); ty = ty + PTR; }
                    if (tk != Id) {
                        printf("%d: bad parameter declaration\n", line);
                        return -1;
                    }
                    if (id->class == Loc) {
                        printf("%d: duplicate parameter definition\n", line);
                        return -1;
                    }
                    id->hclass = id->class; id->class = Loc;
                    id->htype  = id->type;  id->type = ty;
                    id->hval   = id->val;   id->val = i++;
                    next();
                    if (tk == ',') next();
                }
                next();
                if (tk != '{') {
                    printf("%d: bad function definition\n", line);
                    return -1;
                }
                loc = ++i;
                next();
                while (tk == Int || tk == Char || tk == Struct) {
                    switch (tk) {
                    case Int: bt = INT; break;
                    case Char: bt = CHAR; break;
                    default:
                        next();
                        if (tk != Id) {
                            printf("%d: bad struct declaration\n", line);
                            return -1;
                        }
                        bt = id->stype; break;
                    }
                    next();
                    while (tk != ';') {
                        ty = bt;
                        while (tk == Mul) { next(); ty = ty + PTR; }
                        if (tk != Id) {
                            printf("%d: bad local declaration\n", line);
                            return -1;
                        }
                        if (id->class == Loc) {
                            printf("%d: duplicate local definition\n", line);
                            return -1;
                        }
                        id->hclass = id->class; id->class = Loc;
                        id->htype  = id->type;  id->type = ty;
                        id->hval   = id->val;   id->val = ++i;
                        next();
                        if (tk == ',') next();
                    }
                    next();
                }
                *++e = ENT; *++e = i - loc;
                while (tk != '}') stmt();
                *++e = LEV;
                id = sym; // unwind symbol table locals
                while (id->tk) {
                    if (id->class == Loc) {
                        id->class = id->hclass;
                        id->type = id->htype;
                        id->val = id->hval;
                    }
                    id = id + 1;
                }
            }
            else {
                id->class = Glo;
                id->val = (int) data;
                data = data + sizeof(int);
            }
            if (tk == ',') next();
        }
        next();
    }

    if (elf)
        return elf32(poolsz, (int *) idmain->val);
    return jit(poolsz, (int *) idmain->val, argc, argv);
}

// vim: set tabstop=4 shiftwidth=4 expandtab:
