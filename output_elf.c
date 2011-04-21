/* output_elf.c ELF output driver for vasm */
/* (c) in 2002-2010 by Frank Wille */

#include "vasm.h"
#include "output_elf.h"
#if ELFCPU
static char *copyright="vasm ELF output module 1.5 (c)2002-2010 Frank Wille";
const int cpu = ELFCPU;

static struct list shdrlist,symlist,relalist;
static struct StrTabList shstrlist,strlist;
static unsigned symindex;


static unsigned addString(struct StrTabList *sl,char *s)
{
  struct StrTabNode *sn = mymalloc(sizeof(struct StrTabNode));
  unsigned idx = sl->index;

  sn->str = s;
  addtail(&(sl->l),&(sn->n));
  sl->index += (unsigned)strlen(s) + 1;
  return idx;
}


static struct Shdr32Node *addShdr32(struct list *l)
{
  struct Shdr32Node *s = mycalloc(sizeof(struct Shdr32Node));

  addtail(l,&(s->n));
  return s;
}


static struct Shdr64Node *addShdr64(struct list *l)
{
  struct Shdr64Node *s = mycalloc(sizeof(struct Shdr64Node));

  addtail(l,&(s->n));
  return s;
}


static struct Symbol32Node *addSymbol32(int be,char *name)
{
  struct Symbol32Node *sn = mycalloc(sizeof(struct Symbol32Node));

  addtail(&symlist,&(sn->n));
  if (name) {
    sn->name = name;
    setval(be,sn->s.st_name,4,addString(&strlist,name));
  }
  symindex++;
  return sn;
}


static struct Symbol64Node *addSymbol64(int be,char *name)
{
  struct Symbol64Node *sn = mycalloc(sizeof(struct Symbol64Node));

  addtail(&symlist,&(sn->n));
  if (name) {
    sn->name = name;
    setval(be,sn->s.st_name,4,addString(&strlist,name));
  }
  symindex++;
  return sn;
}


static void addRel32(int be,taddr o,taddr a,taddr i)
{
  struct RelocNode *rn = mymalloc(sizeof(struct RelocNode));

  setval(be,rn->r.r_offset,4,o);
#if RELA
  setval(be,rn->r.r_addend,4,a);
#endif
  setval(be,rn->r.r_info,4,i);
  addtail(&relalist,&(rn->n));
}


static void addRel64(int be,taddr o,taddr a,taddr i)
{
  struct RelocNode *rn = mymalloc(sizeof(struct RelocNode));

  setval(be,rn->r.r_offset,8,o);
#if RELA
  setval(be,rn->r.r_addend,8,a);
#endif
  setval(be,rn->r.r_info,8,i);
  addtail(&relalist,&(rn->n));
}


static struct Shdr32Node *makeShdr32(int be,struct list *shlst,
                                     taddr name,taddr type, taddr flags,
                                     taddr offset,taddr size,taddr info,
                                     taddr align,taddr entsize)
{
  struct Shdr32Node *shn;

  shn = addShdr32(shlst);
  setval(be,shn->s.sh_name,4,name);
  setval(be,shn->s.sh_type,4,type);
  setval(be,shn->s.sh_flags,4,flags);
  setval(be,shn->s.sh_offset,4,offset);
  setval(be,shn->s.sh_size,4,size);
  setval(be,shn->s.sh_info,4,info);
  setval(be,shn->s.sh_addralign,4,align);
  setval(be,shn->s.sh_entsize,4,entsize);
  /* @@@ set sh_addr to org? */
  return shn;
}


static struct Shdr64Node *makeShdr64(int be,struct list *shlst,
                                     taddr name,taddr type, taddr flags,
                                     taddr offset,taddr size,taddr info,
                                     taddr align,taddr entsize)
{
  struct Shdr64Node *shn;

  shn = addShdr64(shlst);
  setval(be,shn->s.sh_name,4,name);
  setval(be,shn->s.sh_type,4,type);
  setval(be,shn->s.sh_flags,8,flags);
  setval(be,shn->s.sh_offset,8,offset);
  setval(be,shn->s.sh_size,8,size);
  setval(be,shn->s.sh_info,4,info);
  setval(be,shn->s.sh_addralign,8,align);
  setval(be,shn->s.sh_entsize,8,entsize);
  /* @@@ set sh_addr to org? */
  return shn;
}


static unsigned findelfsymbol(char *name)
/* find symbol with given name in symlist, return its index */
{
  /* works for Symbol64Node too! */
  struct Symbol32Node *nextsym,*sym = (struct Symbol32Node *)symlist.first;
  unsigned sidx = 0;

  while (nextsym = (struct Symbol32Node *)sym->n.next) {
    if (sym->name)
      if (!strcmp(name,sym->name))
        break;
    ++sidx;
    sym = nextsym;
  }
  return nextsym ? sidx : 0;
}


static void init_ident(unsigned char *id,unsigned char class,
                       unsigned char endian)
{
  static char elfid[4] = { 0x7f,'E','L','F' };

  memcpy(&id[EI_MAG0],elfid,4);
  id[EI_CLASS] = class;
  id[EI_DATA] = endian;
  id[EI_VERSION] = EV_CURRENT;
  memset(&id[EI_PAD],0,EI_NIDENT-EI_PAD);
}


static unsigned long get_sec_type(section *s)
/* scan section attributes for type */
{
  char *a = s->attr;

  if (!strncmp(s->name,".note",5))
    return SHT_NOTE;

  while (*a) {
    switch (*a++) {
      case 'c':
      case 'd':
        return SHT_PROGBITS;
      case 'u':
        return SHT_NOBITS;
    }
  }
#if 0
  output_error(3,attr);  /* section attributes not suppported */
  return SHT_NULL;
#else
  return SHT_PROGBITS;
#endif
}


static taddr get_sec_flags(char *a)
/* scan section attributes for flags (read, write, alloc, execute) */
{
  taddr f = 0;

  while (*a) {
    switch (*a++) {
      case 'a':
        f |= SHF_ALLOC;
        break;
      case 'w':
        f |= SHF_WRITE;
        break;
      case 'x':
        f |= SHF_EXECINSTR;
        break;
    }
  }
  return f;
}


static unsigned char get_sym_info(symbol *s)
/* determine symbol-info: function, object, section, etc. */
{
  switch (TYPE(s)) {
    case TYPE_OBJECT:
      return STT_OBJECT;
    case TYPE_FUNCTION:
      return STT_FUNC;
    case TYPE_SECTION:
      return STT_SECTION;
    case TYPE_FILE:
      return STT_FILE;
  }
  return STT_NOTYPE;
}


static unsigned get_sym_index(symbol *s)
{
  if (s->flags & COMMON)
    return SHN_COMMON;
  if (s->type == IMPORT)
    return SHN_UNDEF;
  if (s->sec)
    return (unsigned)s->sec->idx;
  return SHN_ABS;
}


static taddr get_reloc_type(rlist **rl,
                            taddr *roffset,taddr *addend,symbol **refsym)
{
  rlist *rl2;
  taddr mask,offset;
  int size;
  taddr t = 0;

  *roffset = 0;
  *addend = 0;
  *refsym = NULL;

#ifdef VASM_CPU_M68K
#include "elf_reloc_68k.h"
#endif

#ifdef VASM_CPU_PPC
#include "elf_reloc_ppc.h"
#endif

#ifdef VASM_CPU_ARM
#include "elf_reloc_arm.h"
#endif

#ifdef VASM_CPU_X86
#include "elf_reloc_386.h"
#endif

  if (t)
    *roffset = offset>>3;
  else
    unsupp_reloc_error(*rl);

  return t;
}


static taddr make_relocs32(int be,rlist *rl,taddr pc)
/* convert all of an atom's relocs into ELF32 relocs */
{
  taddr ro = 0;

  if (rl) {
    do {
      taddr rtype,offset,addend;
      symbol *refsym;

      if (rtype = get_reloc_type(&rl,&offset,&addend,&refsym)) {
        if (refsym->type == LABSYM) {
          /* this is a local relocation */
          addRel32(be,pc+offset,addend,ELF32_R_INFO(refsym->sec->idx,rtype));
          ro += sizeof(struct ElfReloc);
        }
        else if (refsym->type == IMPORT) {
          /* this is an external symbol reference */
          unsigned idx = findelfsymbol(refsym->name);

          if (idx == 0) {
            /* create a new symbol, which can be referenced */
            struct Symbol32Node *elfsym;;

            idx = symindex;
            elfsym = addSymbol32(be,refsym->name);
            elfsym->s.st_info[0] = ELF32_ST_INFO(STB_GLOBAL,STT_NOTYPE);
          }
          addRel32(be,pc+offset,addend,ELF32_R_INFO(idx,rtype));
          ro += sizeof(struct ElfReloc);
        }
        else
          ierror(0);
      }
    }
    while (rl = rl->next);
  }
  return ro;
}


static void write_ELF64(FILE *f,section *sec,symbol *sym)
{
  int be = BIGENDIAN;   /* true for big endian */

  ierror(0);  /* FIXME */
}


static void write_ELF32(FILE *f,section *sec,symbol *sym)
{
  struct Elf32_Ehdr header;
  int be = BIGENDIAN;   /* true for big endian */
  int shdr_size = sizeof(struct Elf32_Shdr);
  unsigned symtabidx,strtabidx,shstrtabidx;
  unsigned shdrindex=0,firstglobal;
  unsigned stabidx=0,stabstridx=0;
  taddr stabsize,stabstrsize;
  taddr roffset=0,soffset=sizeof(struct Elf32_Ehdr),align1,align2;
  int i;
  section *secp;
  symbol *symp;
  struct Shdr32Node *shn;
  struct Symbol32Node *elfsym;
  struct StrTabNode *stn;
  struct RelocNode *rn;

  /* initialize ELF header */
  memset(&header,0,sizeof(struct Elf32_Ehdr));
  init_ident(header.e_ident,ELFCLASS32,be ? ELFDATA2MSB : ELFDATA2LSB);
  setval(be,header.e_type,2,ET_REL);
  setval(be,header.e_machine,2,cpu);
  setval(be,header.e_version,4,EV_CURRENT);
#ifdef VASM_CPU_ARM
  setval(be,header.e_flags,4,0x04000000);  /* EABI version 4 */
#endif
  setval(be,header.e_ehsize,2,sizeof(struct Elf32_Ehdr));
  setval(be,header.e_shentsize,2,shdr_size);

  /* init lists */
  initlist(&shdrlist);
  initlist(&symlist);
  initlist(&relalist);
  shstrlist.index = strlist.index = 0;
  initlist(&shstrlist.l);
  initlist(&strlist.l);
  addString(&shstrlist,"");  /* first string is always "" */
  symtabidx = addString(&shstrlist,".symtab");
  strtabidx = addString(&shstrlist,".strtab");
  shstrtabidx = addString(&shstrlist,".shstrtab");
  addShdr32(&shdrlist);  /* first section header is always zero */
  addString(&strlist,"");
  symindex = 0;
  addSymbol32(be,NULL);  /* first symbol is empty */

  /* generate section headers for program sections */
  for (secp=sec; secp; secp=secp->next) {
    if (get_sec_size(secp)>0 || (secp->flags & HAS_SYMBOLS)) {
      unsigned long type = get_sec_type(secp);

      shn = makeShdr32(be,&shdrlist,addString(&shstrlist,secp->name),
                       type,get_sec_flags(secp->attr),soffset,
                       get_sec_size(secp),0,secp->align,0);
      secp->idx = ++shdrindex;
      if (type != SHT_NOBITS)
        soffset += get_sec_size(secp);

      if (!strcmp(secp->name,".stabstr")) {
        stabstridx = shdrindex;
        setval(be,shn->s.sh_type,4,SHT_STRTAB);
        stabstrsize = get_sec_size(secp);
      }
      else if (!strcmp(secp->name,".stab")) {
        stabidx = shdrindex;
        stabsize = (get_sec_size(secp) / 12) - 1;  /* 12: sizeof(nlist32) */
      }

      /* add section base symbol */
      elfsym = addSymbol32(be,NULL);
      elfsym->s.st_info[0] = ELF32_ST_INFO(STB_LOCAL,STT_SECTION);
      setval(be,elfsym->s.st_shndx,2,shdrindex);
    }
    else
      secp->idx = 0;
  }

  /* file name symbol */
  if (filename) {
    elfsym = addSymbol32(be,filename);
    elfsym->s.st_info[0] = ELF32_ST_INFO(STB_LOCAL,STT_FILE);
    setval(be,elfsym->s.st_shndx,2,SHN_ABS);
  }

  /* build symbol table */
  if (!no_symbols) {
    for (symp=sym; symp; symp=symp->next) {  /* local binding first */
      /* ignore symbols preceded by '.' and ' ' (local/internal symbols) */
      if (*symp->name!='.' && *symp->name!=' ') {
        if (symp->type!=IMPORT && !(symp->flags & (EXPORT|WEAK))) {
          elfsym = addSymbol32(be,symp->name);
          setval(be,elfsym->s.st_value,4,get_sym_value(symp));
          setval(be,elfsym->s.st_size,4,get_sym_size(symp));
          elfsym->s.st_info[0] = ELF32_ST_INFO(STB_LOCAL,get_sym_info(symp));
          setval(be,elfsym->s.st_shndx,2,get_sym_index(symp));
        }
      }
    }
  }
  firstglobal = symindex;  /* now the global and weak symbols */
  for (symp=sym; symp; symp=symp->next) {
    if (*symp->name != '.') {  /* ignore symbols preceded by a '.' */
      if ((symp->type!=IMPORT && (symp->flags & (EXPORT|WEAK))) ||
          (symp->type==IMPORT && (symp->flags & (COMMON|WEAK)))) {
        elfsym = addSymbol32(be,symp->name);
        setval(be,elfsym->s.st_value,4,get_sym_value(symp));
        setval(be,elfsym->s.st_size,4,get_sym_size(symp));
        elfsym->s.st_info[0] = ELF32_ST_INFO(symp->flags & WEAK ?
                                             STB_WEAK : STB_GLOBAL,
                                             get_sym_info(symp));
        setval(be,elfsym->s.st_shndx,2,get_sym_index(symp));
      }
    }
  }

  /* ".rela.xxx" or ".rel.xxx" relocation sections */
  for (secp=sec; secp; secp=secp->next) {
    if (secp->idx) {
      atom *a;
      taddr pc=0,npc,basero=roffset;

      for (a=secp->first; a; a=a->next) {
        int align = a->align;

        npc = ((pc + align-1) / align) * align;
        if (a->type == DATA)
          roffset += make_relocs32(be,a->content.db->relocs,npc);
        if (a->type == SPACE)
          roffset += make_relocs32(be,a->content.sb->relocs,npc);
        pc = npc + atom_size(a,secp,npc);
      }
      if (basero != roffset) {  /* were there any relocations? */
        /* create .relaXX section header */
        char *sname = mymalloc(strlen(secp->name) + 6);

#if RELA
        sprintf(sname,".rela%s",secp->name);
#else
        sprintf(sname,".rel%s",secp->name);
#endif
        makeShdr32(be,&shdrlist,addString(&shstrlist,sname),SHT_RELOC,0,
                   basero, /* relative offset - will be corrected later! */
                   roffset-basero,secp->idx,4,sizeof(struct ElfReloc));
        ++shdrindex;
      }
    }
  }

  /* ".shstrtab" section header string table */
  ++shdrindex;
  makeShdr32(be,&shdrlist,shstrtabidx,SHT_STRTAB,0,
             soffset,shstrlist.index,0,1,0);
  soffset += shstrlist.index;
  align1 = ((soffset + 3) & ~3) - soffset;
  soffset += align1;

  /* set last values in ELF header */
  setval(be,header.e_shoff,4,soffset);  /* remember offset of Shdr table */
  soffset += (shdrindex+3)*sizeof(struct Elf32_Shdr);
  setval(be,header.e_shstrndx,2,shdrindex);
  setval(be,header.e_shnum,2,shdrindex+3);

  /* ".symtab" symbol table */
  ++shdrindex;
  shn = makeShdr32(be,&shdrlist,symtabidx,SHT_SYMTAB,0,soffset,
                   symindex*sizeof(struct Elf32_Sym),
                   firstglobal,4,sizeof(struct Elf32_Sym));
  setval(be,shn->s.sh_link,4,shdrindex+1);  /* associated .strtab section */
  soffset += symindex * sizeof(struct Elf32_Sym);

  /* ".strtab" string table */
  makeShdr32(be,&shdrlist,strtabidx,SHT_STRTAB,0,
             soffset,strlist.index,0,1,0);
  soffset += strlist.index;
  align2 = ((soffset + 3) & ~3) - soffset;
  soffset += align2;  /* offset for first Reloc-entry */


  /* write ELF header */
  fwdata(f,&header,sizeof(struct Elf32_Ehdr));

  /* write initialized section contents */
  for (secp=sec; secp; secp=secp->next) {
    if (secp->idx && get_sec_type(secp)!=SHT_NOBITS) {
      atom *a;
      taddr pc=0,npc;
      int align;

      if (secp->idx == stabidx) {
        /* patch compilation unit header */
        a = secp->first;
        if (a->content.db->size == 12) {
          unsigned char *p = a->content.db->data;

          setval(be,p,4,1);  /* refers to first string from .stabstr */
          setval(be,p+4,4,stabsize);
          setval(be,p+8,4,stabstrsize);
        }
      }
      for (a=secp->first; a; a=a->next) {
        align = a->align;
        npc = ((pc + align-1) / align) * align;
        for (i=pc; i<npc; i++)
          fw8(f,0);

        if (a->type == DATA) {
          fwdata(f,a->content.db->data,a->content.db->size);
        }
        else if (a->type == SPACE) {
          fwsblock(f,a->content.sb);
        }

        pc = npc + atom_size(a,secp,npc);
      }
    }
  }

  /* write .shstrtab string table */
  while (stn = (struct StrTabNode *)remhead(&shstrlist.l))
    fwdata(f,stn->str,strlen(stn->str)+1);

  /* write section headers */
  for (i=0; i<align1; i++)
    fw8(f,0);
  i = 0;
  while (shn = (struct Shdr32Node *)remhead(&shdrlist)) {
    if (i == stabidx) {
      /* set link to stabstr table for .stab section */
      setval(be,shn->s.sh_link,4,stabstridx);
    }
    if (readval(be,shn->s.sh_type,4) == SHT_RELOC) {
      /* set correct offset and link to symtab */
      setval(be,shn->s.sh_offset,4,readval(be,shn->s.sh_offset,4)+soffset);
      setval(be,shn->s.sh_link,4,shdrindex); /* index of associated symtab */
    }
    fwdata(f,&(shn->s),sizeof(struct Elf32_Shdr));
    i++;
  }

  /* write symbol table */
  while (elfsym = (struct Symbol32Node *)remhead(&symlist))
    fwdata(f,&(elfsym->s),sizeof(struct Elf32_Sym));

  /* write .strtab string table */
  while (stn = (struct StrTabNode *)remhead(&strlist.l))
    fwdata(f,stn->str,strlen(stn->str)+1);

  /* write relocations */
  for (i=0; i<align2; i++)
    fw8(f,0);
  while (rn = (struct RelocNode *)remhead(&relalist))
    fwdata(f,&(rn->r),sizeof(struct ElfReloc));
}


static void write_output(FILE *f,section *sec,symbol *sym)
{
  int bits = bytespertaddr * bitsperbyte;

  if (bits==32 && cpu!=EM_NONE)
    write_ELF32(f,sec,sym);
  else if (bits==64 && cpu!=EM_NONE)
    write_ELF64(f,sec,sym);
  else
    output_error(1,cpuname);  /* output module doesn't support cpu */
}


static int output_args(char *p)
{
  return 0;
}


int init_output_elf(char **cp,void (**wo)(FILE *,section *,symbol *),
                    int (**oa)(char *))
{
  *cp = copyright;
  *wo = write_output;
  *oa = output_args;
  return 1;
}

#else

int init_output_elf(char **cp,void (**wo)(FILE *,section *,symbol *),
                    int (**oa)(char *))
{
  return 0;
}

#endif
