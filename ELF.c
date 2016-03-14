#include "ELF.h"
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dlfcn.h>

static int PT_idx = 0;
static int SH_idx = 0;
static int sym_idx = 0;

int gen_PT(char *ptr, int type, int offset, int addr, int size,
           int flag, int align)
{
    struct Elf32_Phdr *phdr = (struct Elf32_Phdr*) ptr;
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
    struct Elf32_Shdr *shdr = (struct Elf32_Shdr *) ptr;
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
    struct Elf32_Sym *sym = (struct Elf32_Sym *) ptr;
    sym->st_name = name;
    sym->st_info = info;
    sym->st_other = 0;
    sym->st_shndx = shndx;
    sym->st_value = value;
    sym->st_size = size;
    return sym_idx++;
}
