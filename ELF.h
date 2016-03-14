#ifndef AMACC_ELF_H
#define AMACC_ELF_H

#include <stdint.h>
int gen_PT (char* ptr, int type, int offset, int addr, int size, int flag, int align);
int gen_SH (char* ptr, int type, int name, int offset, int addr, int size, int link, int info,
            int flag, int align, int entsize);
int gen_sym (char* ptr, int name, unsigned char info, int shndx, int size, int value);
#define ELF32_ST_INFO(b,t) (((b)<<4)+((t)&0xf))
#define PHDR_SIZE 32
#define SHDR_SIZE 40
#define SYM_SIZE 16

typedef uint32_t Elf32_Addr; // Program address
typedef uint32_t Elf32_Off;  // File offset
typedef uint16_t Elf32_Half;
typedef uint32_t Elf32_Word;

struct Elf32_Shdr {
    Elf32_Word sh_name;      // Section name (index into string table)
    Elf32_Word sh_type;      // Section type (SHT_*)
    Elf32_Word sh_flags;     // Section flags (SHF_*)
    Elf32_Addr sh_addr;      // Address where section is to be loaded
    Elf32_Off  sh_offset;    // File offset of section data, in bytes
    Elf32_Word sh_size;      // Size of section, in bytes
    Elf32_Word sh_link;      // Section type-specific header table index link
    Elf32_Word sh_info;      // Section type-specific extra information
    Elf32_Word sh_addralign; // Section address alignment
    Elf32_Word sh_entsize;   // Size of records contained within the section
};

// Special section indices.
enum {
    SHN_UNDEF     = 0,      // Undefined, missing, irrelevant, or meaningless
};

// Section types.
enum {
    SHT_NULL          = 0,  // No associated section (inactive entry).
    SHT_PROGBITS      = 1,  // Program-defined contents.
    SHT_STRTAB        = 3,  // String table.
    SHT_DYNAMIC       = 6,  // Information for dynamic linking.
    SHT_REL           = 9,  // Relocation entries; no explicit addends.
    SHT_DYNSYM        = 11, // Symbol table.
};

// Section flags.
enum {
    SHF_WRITE = 0x1,
    SHF_ALLOC = 0x2,
    SHF_EXECINSTR = 0x4,
};

// Symbol table entries for ELF32.
struct Elf32_Sym {
    Elf32_Word    st_name;  // Symbol name (index into string table)
    Elf32_Addr    st_value; // Value or address associated with the symbol
    Elf32_Word    st_size;  // Size of the symbol
    unsigned char st_info;  // Symbol's type and binding attributes
    unsigned char st_other; // Must be zero; reserved
    Elf32_Half    st_shndx; // Which section (header table index) it's defined in
};
// Symbol bindings.
enum {
    STB_LOCAL = 0,   // Local symbol, not visible outside obj file containing def
    STB_GLOBAL = 1,  // Global symbol, visible to all object files being combined
};

// Symbol types.
enum {
    STT_NOTYPE  = 0,   // Symbol's type is not specified
    STT_FUNC    = 2,   // Symbol is executable code (function, etc.)
};

// Symbol number.
enum {
    STN_UNDEF = 0
};

// Program header for ELF32.
struct Elf32_Phdr {
    Elf32_Word p_type;   // Type of segment
    Elf32_Off  p_offset; // File offset where segment is located, in bytes
    Elf32_Addr p_vaddr;  // Virtual address of beginning of segment
    Elf32_Addr p_paddr;  // Physical address of beginning of segment (OS-specific)
    Elf32_Word p_filesz; // Num. of bytes in file image of segment (may be zero)
    Elf32_Word p_memsz;  // Num. of bytes in mem image of segment (may be zero)
    Elf32_Word p_flags;  // Segment flags
    Elf32_Word p_align;  // Segment alignment constraint
};

// Segment types.
enum {
    PT_NULL    = 0, // Unused segment.
    PT_LOAD    = 1, // Loadable segment.
    PT_DYNAMIC = 2, // Dynamic linking information.
    PT_INTERP  = 3, // Interpreter pathname.
};
// Segment flag bits.
enum {
    PF_X        = 1,         // Execute
    PF_W        = 2,         // Write
    PF_R        = 4,         // Read
};

// Dynamic table entry tags.
enum {
    DT_NULL         = 0,        // Marks end of dynamic array.
    DT_NEEDED       = 1,        // String table offset of needed library.
    DT_PLTRELSZ     = 2,        // Size of relocation entries in PLT.
    DT_PLTGOT       = 3,        // Address associated with linkage table.
    DT_STRTAB       = 5,        // Address of dynamic string table.
    DT_SYMTAB       = 6,        // Address of dynamic symbol table.
    DT_STRSZ        = 10,       // Total size of the string table.
    DT_SYMENT       = 11,       // Size of a symbol table entry.
    DT_REL          = 17,       // Address of relocation table (Rel entries).
    DT_RELSZ        = 18,       // Size of Rel relocation table.
    DT_RELENT       = 19,       // Size of a Rel relocation entry.
    DT_PLTREL       = 20,       // Type of relocation entry used for linking.
    DT_JMPREL       = 23,       // Address of relocations associated with PLT.
};

#endif
