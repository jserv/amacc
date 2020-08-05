#ifndef VM_H
#define VM_H

#define POOLSZ 256*1024

#define ERR_UNKONWN_INSTRUCTION 400
#define ERR_UNKONWN_SYMBOL 401
#define ERR_NOT_ENOUGH_MEMORY 402
#define ERR_NOT_SUPPORT_PRINT_FORMAT 403
#define ERR_NO_INPUT_IR_FILE 404
#define ERR_FILE 405

#define SYSCALL_NUM 48
#define MAX_IR_LEN 5
#define MAX_BUF 4096

enum {
    LEA, IMM, JMP, JSR, BZ, BNZ, ENT, ADJ, LEV,
    LI, LC, SI, SC, PSH, OR, XOR, AND, EQ, NE,
    LT, GT, LE, GE, SHL, SHR, ADD, SUB, MUL,
    /* system call shortcuts */
    OPEN,READ,WRIT,CLOS,PRTF,MALC,FREE,MSET,MCMP,MCPY,MMAP,DSYM,BSCH,STRT,DLOP,DIV,MOD,SNPR,EXIT,
    CLCA, /* clear cache, used by JIT compilation */
    INVALID
};

// types
enum { CHAR, INT, STR, PTR = 256, PTR2 = 512 };

struct str_t {
    char* str;
    int len;
    int hash;
    int nu;
} * str_tbl, *pstr_tbl, *ts_str_tbl;

struct sym_t {
    char* s;
    int type;
    int hash;
    int len;
} * sym_tbl, *psym_tbl, *ts_sym_tbl;

struct code_t {
    int line;
    int ir;
    int arg;
} * pc, *code, *ts_code; // code start, top of code stack

struct instruction_t {
    char* ir_s;
    int ir;
} op_dict[]={
    {"LEA", LEA}, {"IMM", IMM}, {"JMP", JMP}, {"JSR",JSR}, {"BZ", BZ}, {"BNZ", BNZ}, {"ENT", ENT},
    {"ADJ", ADJ}, {"LEV", LEV}, {"LI" , LI }, {"LC" ,LC }, {"SI", SI}, {"SC" , SC }, {"PSH", PSH},
    {"OR", OR}, {"XOR", XOR}, {"AND" , AND }, {"EQ" ,EQ }, {"NE", NE}, {"LT" , LT }, {"GT", GT},
    {"LE", LE}, {"GE", GE}, {"SHL" , SHL }, {"SHR" ,SHR }, {"ADD", ADD}, {"SUB" , SUB}, {"MUL", MUL},
    /* system call shortcuts */
    {"OPEN", OPEN}, {"READ", READ}, {"WRIT", WRIT}, {"READ", READ}, {"CLOS", CLOS}, {"PRTF", PRTF},
    {"MALC", MALC}, {"FREE", FREE}, {"MSET", MSET}, {"MCMP", MCMP}, {"MCPY", MCPY}, {"MMAP", MMAP},
    {"DSYM", DSYM}, {"BSCH", BSCH}, {"STRT", STRT}, {"DLOP", DLOP}, {"DIV", DIV}, {"MOD", MOD},
    {"SNPR", SNPR},
    {"EXIT", EXIT},
    {"CLCA", CLCA}, {"INVALID", INVALID}
};

#endif /* end of include guard: VM_H */
