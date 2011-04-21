/* reloc.c - relocation support functions */
/* (c) in 2010 by Volker Barthelmann and Frank Wille */

#include "vasm.h"


nreloc *new_nreloc(void)
{
  nreloc *new = mymalloc(sizeof(*new));
  new->mask = -1;
  new->offset = 0;
  new->size = 0;
  new->addend = 0;
  return new;
}


rlist *add_reloc(rlist **relocs,symbol *sym,taddr addend,
                 int type,int size,int offs)
{
  nreloc *r = new_nreloc();
  rlist *rl = mymalloc(sizeof(rlist));

  r->size = size;
  r->offset = offs;
  r->sym = sym;
  r->addend = addend;
  rl->type = type;
  rl->reloc = r;
  rl->next = *relocs;
  *relocs = rl;
  return rl;
}


void do_pic_check(rlist *r)
/* generate an error on a non-PC-relative relocation */
{
  int t;

  while (r) {
    t = r->type;
    if (t==REL_ABS || t==REL_UABS)
      general_error(34);  /* relocation not allowed */
    r = r->next;
  }
}

void print_reloc(FILE *f,int type,nreloc *p)
{
  if (type<=LAST_STANDARD_RELOC){
    static const char *rname[] = {
      "none","abs","pc","got","gotrel","gotoff","globdat","plt","pltrel",
      "pltoff","sd","uabs","localpc","loadrel","copy","jmpslot","secoff"
    };
    fprintf(f,"r%s(%d,%d,0x%llx,0x%llx,",rname[type],p->offset,p->size,
            ((unsigned long long)p->mask)&taddrmask,
            ((unsigned long long)p->addend)&taddrmask);
  }
#ifdef VASM_CPU_PPC
  else if (type<=LAST_PPC_RELOC){
    static const char *rname[] = {
      "sd2","sd21","sdi16","drel","brel"
    };
    fprintf(f,"r%s(%d,%d,0x%llx,0x%llx,",rname[type-(LAST_STANDARD_RELOC+1)],
            p->offset,p->size,((unsigned long long)p->mask)&taddrmask,
            ((unsigned long long)p->addend)&taddrmask);
  }
#endif
  else
    fprintf(f,"unknown reloc(");

  print_symbol(f,p->sym);
  fprintf(f,") ");
}
