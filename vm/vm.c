#include "vm.h"
#include "fstring.h"
#include "util.h"
#include <assert.h>
#include <dlfcn.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <unistd.h>

int amacc_vm(int argc, char* argv[])
{
    intptr_t *sp, *bp; // vm registers
    const intptr_t *stack_top;
    int64_t r0, cycle;
    intptr_t *args, nu;
    struct str_t *str;

    // find main
    struct sym_t* sym = get_sym_entry_by_str("main");
    if (!sym) {
        printf("no main function found!");
        exit(-1);
    }

    pc = get_code_entry(sym->hash);

    if (!(sp = malloc(sizeof(intptr_t) * POOLSZ))) {
        printf("could not malloc(%d) stack area\n", POOLSZ);
        exit(ERR_NOT_ENOUGH_MEMORY);
    }


    // setup stack
    sp = (intptr_t*)sp + POOLSZ;

    stack_top = sp;
    // cdecl is right to left order
    *sp = (intptr_t)argv;
    *--sp = argc;

    r0 = 0;
    bp = sp;
    cycle = 0;
    while (true) {
        ++cycle;
        switch (pc->ir) {
        case LEA:
            r0 = (int64_t)(bp + pc->arg);
            break;
        case IMM:
            r0 = pc->arg; // load global address or immediate
            break;
        case JMP:
            pc = get_code_entry(pc->arg); // jump
            continue;
        case JSR:
            *--sp = (int64_t)(pc + 1);
            pc = get_code_entry(pc->arg); // jump to subroutine
            continue;
        case BZ:
            pc = r0 ? pc + 1 : get_code_entry(pc->arg); // branch if zero
            continue;
        case BNZ:
            pc = r0 ? get_code_entry(pc->arg) : pc + 1; // branch if not zero
            continue;
        case ENT:
            *--sp = (int64_t)bp;
            bp = sp;
            sp = sp - pc->arg; // enter subroutine
            break;
        case ADJ:
            sp = sp + pc->arg; // stack adjust
            break;
        case LEV:
            sp = bp;
            bp = (intptr_t*)*sp++;
            pc = (struct code_t*)*sp++; // leave subroutine
            continue;
        case LI:
            r0 = *(int64_t*)r0; // load int
            break;
        case LC:
            // load char
            str = get_str_entry(r0);
            if(!str) r0 = *(char*)r0;
            else r0 = *(intptr_t*)str->str;
            break;
        case SI:
            *(int64_t*)*sp++ = r0; // store int
            break;
        case SC:
            r0 = *(char*)* sp++ = r0; // store char
            break;
        case PSH:
            *--sp = r0; // push
            break;
        case OR:
            r0 = *sp++ | r0;
            break;
        case XOR:
            r0 = *sp++ ^ r0;
            break;
        case AND:
            r0 = *sp++ & r0;
            break;
        case EQ:
            r0 = *sp++ == r0;
            break;
        case NE:
            r0 = *sp++ != r0;
            break;
        case LT:
            r0 = *sp++ < r0;
            break;
        case GT:
            r0 = *sp++ > r0;
            break;
        case LE:
            r0 = *sp++ <= r0;
            break;
        case GE:
            r0 = *sp++ >= r0;
            break;
        case SHL:
            r0 = *sp++ << r0;
            break;
        case SHR:
            r0 = *sp++ >> r0;
            break;
        case ADD:
            r0 = *sp++ + r0;
            break;
        case SUB:
            r0 = *sp++ - r0;
            break;
        case MUL:
            r0 = *sp++ * r0;
            break;
        case DIV:
            r0 = *sp++ / r0;
            break;
        case MOD:
            r0 = *sp++ % r0;
            break;
        case OPEN:
            r0 = open((char*)sp[1], *sp);
            break;
        case READ:
            r0 = read(sp[2], (char*)sp[1], *sp);
            break;
        case WRIT:
            r0 = write(sp[2], (char*)sp[1], *sp);
            break;
        case CLOS:
            r0 = close(*sp);
            break;
        case PRTF:
            assert((pc + 1)->ir == ADJ);
            nu = (pc + 1)->arg;
            args = malloc(sizeof(intptr_t) * nu);
            fetch_args(sp, args, nu);
            char pr_buff[MAX_BUF];
            fstring(pr_buff, MAX_BUF, (char*)args[0], args + 1 , nu - 1);
            r0 = printf("%s", pr_buff);
            free(args);
            break;
        case MALC:
            r0 = (int64_t)malloc(*sp);
            break;
        case FREE:
            break;
        case MSET:
            r0 = (int64_t)memset((char*)sp[2], sp[1], *sp);
            break;
        case MCMP:
            r0 = memcmp((char*)sp[2], (char*)sp[1], *sp);
            break;
        case MCPY:
            r0 = (int64_t)memcpy((char*)sp[2], (char*)sp[1], *sp);
            break;
        case MMAP:
            r0 = mmap((char*)sp[5], (char*)sp[4], (char*)sp[3], (char*)sp[2], (char*)sp[1], *sp);
            break;
        case DSYM:
            r0 = dlsym(sp[1], *sp);
            break;
        case DLOP:
            r0 = dlopen((char*)sp[1], *sp);
            break;
        case BSCH:
            r0 = bsearch(sp[4], sp[3], sp[2], sp[1], *sp);
            break;
        case SNPR:
            assert((pc + 1)->ir == ADJ);
            nu = (pc + 1)->arg;
            args = malloc(sizeof(intptr_t) * nu);
            fetch_args(sp, args, nu);
            assert(nu > 3);
            r0 = fstring((char*)sp[nu - 1], sp[nu - 2], (char*)sp[nu - 3], args + 3, nu - 3);
            free(args);
            break;
        case EXIT:
            return r0;
        case STRT:
        case CLCA:
            printf("IR not support!\n");
            exit(1);
        default:
            printf("unknown instruction = %d! cycle = %ld\n", pc->ir, cycle);
            exit(1);
        }
        pc++;
    }
    return 0;
}

int parse_symbol(char* line)
{
    char* t = line;
    char* tt = t;
    char* str;
    t++;
    switch (*t) {
    case '.': // string
        t += 5;
        pstr_tbl->nu = strtol(t, &t, 10);
        t += 3;
        pstr_tbl->len = strtol(t, &t, 10);
        t += 2;
        pstr_tbl->str = malloc(sizeof(char) * pstr_tbl->len + 1);
        str = pstr_tbl->str;
        // strncpy(buf, t, pstr_tbl-> len);
        for (int i = 0; i < pstr_tbl->len; ++i) {
            switch(*t){
            case '\\':
                t += 2;
                switch (*t) {
                    case 'A': *str++ = '\n'; break; // new line
                    case '9': *str++ = '\t'; break; // horizontal tab
                    case 'B': *str++ = '\v'; break; // vertical tab
                    case 'C': *str++ = '\f'; break; // form feed
                    case 'D': *str++ = '\r'; break; // carriage return
                    case '0': *str++ = '\0'; break; // an int with value 0
                }
                t++;
                i += 2;
                break;
            default: *str++ = *t++; break;
            }
        }
        *str = '\0';
        // t += pstr_tbl->len + 2;
        pstr_tbl->len = strlen(pstr_tbl->str);
        // pstr_tbl = realloc(pstr_tbl, sizeof(char) * nl);
        t += + 2;
        pstr_tbl->hash = strtol(t, NULL, 10);
        pstr_tbl++;
        break;
    default:
        // global symbol
        t = strchr(t, ' ');
        psym_tbl->len = t - tt - 1;
        psym_tbl->s = malloc(sizeof(char) * psym_tbl->len);
        tt += 1;
        strncpy(psym_tbl->s, tt, psym_tbl->len);
        t += 3;
        psym_tbl->type = strtol(t, &t, 10);
        psym_tbl->hash = strtol(t, NULL, 10);
        psym_tbl++;
        break;
    }
    return 0;
}

int free_tables(void)
{
    free(code);
    for (struct sym_t* i = sym_tbl; i < ts_sym_tbl; ++i) {
        free(i->s);
    }
    free(sym_tbl);
    for (struct str_t* i = str_tbl; i < ts_str_tbl; ++i) {
        free(i->str);
    }
    free(str_tbl);
    return 0;
}

int main(int argc, char* argv[])
{
    if (argc <= 1) {
        printf("lack of *.ll file");
        return ERR_NO_INPUT_IR_FILE;
    }

    FILE* fp = fopen(argv[1], "r");
    argc--;
    argv++;
    if (!fp) {
        perror("Open file failed");
        return ERR_FILE;
    }
    // FIXME:size may be not correct
    pc = code = malloc(sizeof(struct code_t) * POOLSZ);
    str_tbl = pstr_tbl = malloc(sizeof(struct str_t) * POOLSZ);
    sym_tbl = psym_tbl = malloc(sizeof(struct sym_t) * POOLSZ);

    char buf[MAX_BUF] = {};
    int cline = 0, arg = 0;
    char ir[MAX_IR_LEN] = {};

    while (!feof(fp) && fgets(buf, MAX_BUF, fp) != NULL) {
        switch (buf[0]) {
        case '%': // local
            sscanf(buf, "%% %d = %4s %d\n", &cline, ir, &arg);
            pc->line = cline;
            pc->ir = get_ir_to_op(ir);
            if (pc->ir <= ADJ)
                pc->arg = arg;
            pc++;
            break;
        case '@': // global
            parse_symbol(buf);
            break;
        }
    }

    pc->ir = EXIT;
    ts_code = pc;
    ts_sym_tbl = psym_tbl;
    ts_str_tbl = pstr_tbl;

    int ret = amacc_vm(argc, argv);
    free_tables();
    fclose(fp);
    return ret;
}
