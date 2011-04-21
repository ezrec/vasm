/*
** cpu.c PowerPC cpu-description file
** (c) in 2002-2008 by Frank Wille
*/

#include "vasm.h"
#include "operands.h"

mnemonic mnemonics[] = {
#include "opcodes.h"
};

int mnemonic_cnt=sizeof(mnemonics)/sizeof(mnemonics[0]);

char *cpu_copyright="vasm PowerPC cpu backend 1.4 (c) 2002-2008 Frank Wille";
char *cpuname = "PowerPC";
int bitsperbyte = 8;
int bytespertaddr = 4;
int ppc_endianess = 1;

static unsigned long cpu_type = PPC_OPCODE_ANY;
static int regnames = 1;
static taddr sdreg = 13;  /* this is default for V.4, PowerOpen = 2 */
static taddr sd2reg = 2;
static unsigned char opt_branch = 0;



int ppc_data_align(int n)
{
  if (n<=8) return 1;
  if (n<=16) return 2;
  if (n<=32) return 4;
  return 8;
}


int ppc_data_operand(int n)
{
  if (n<=8) return OP_D8;
  if (n<=16) return OP_D16;
  if (n<=32) return OP_D32;
  return OP_D64;
}


static char *parse_reloc_attr(char *p,operand *op)
{
  p = skip(p);
  while (*p == '@') {
    unsigned char chk;

    p++;
    chk = op->attr;
    if (!strncmp(p,"got",3)) {
      op->attr = REL_GOT;
      p += 3;
    }
    else if (!strncmp(p,"plt",3)) {
      op->attr = REL_PLT;
      p += 3;
    }
    else if (!strncmp(p,"sdax",4)) {
      op->attr = REL_SD;
      p += 4;
    }
    else if (!strncmp(p,"sdarx",5)) {
      op->attr = REL_SD;
      p += 5;
    }
    else if (!strncmp(p,"sdarel",6)) {
      op->attr = REL_SD;
      p += 6;
    }
    else if (!strncmp(p,"sectoff",7)) {
      op->attr = REL_SECOFF;
      p += 7;
    }
    else if (!strncmp(p,"local",5)) {
      op->attr = REL_LOCALPC;
      p += 5;
    }
    else if (!strncmp(p,"globdat",7)) {
      op->attr = REL_GLOBDAT;
      p += 7;
    }
    else if (!strncmp(p,"sda2rel",7)) {
      op->attr = REL_PPCEABI_SDA2;
      p += 7;
    }
    else if (!strncmp(p,"sda21",5)) {
      op->attr = REL_PPCEABI_SDA21;
      p += 5;
    }
    else if (!strncmp(p,"sdai16",6)) {
      op->attr = REL_PPCEABI_SDAI16;
      p += 6;
    }
    else if (!strncmp(p,"sda2i16",7)) {
      op->attr = REL_PPCEABI_SDA2I16;
      p += 7;
    }
    else if (!strncmp(p,"drel",4)) {
      op->attr = REL_MORPHOS_DREL;
      p += 4;
    }
    else if (!strncmp(p,"brel",4)) {
      op->attr = REL_AMIGAOS_BREL;
      p += 4;
    }
    if (chk!=REL_NONE && chk!=op->attr)
      cpu_error(7);  /* multiple relocation attributes */

    chk = op->mode;
    if (!strncmp(p,"ha",2)) {
      op->mode = OPM_HA;
      p += 2;
    }
    if (*p == 'h') {
      op->mode = OPM_HI;
      p++;
    }
    if (*p == 'l') {
      op->mode = OPM_LO;
      p++;
    }
    if (chk!=OPM_NONE && chk!=op->mode)
      cpu_error(8);  /* multiple hi/lo modifiers */
  }

  return p;
}


int parse_operand(char *p,int len,operand *op,int optype)
/* Parses operands, reads expressions and assigns relocation types */
{
  char *start = p;

  op->attr = REL_NONE;
  op->mode = OPM_NONE;
  p = skip(p);
  op->value = parse_expr(&p);
  if (optype < OP_D8) {
    p = parse_reloc_attr(p,op);
    p = skip(p);
  }

  if (optype < OP_D8 && p-start < len) {
    if (*p == '(') {
      /* parse d(Rn) load/store addressing mode */
      if (powerpc_operands[optype].flags & PPC_OPERAND_PARENS) {
        p++;
        op->basereg = parse_expr(&p);
        p = skip(p);
        if (*p == ')')
          p = skip(p+1);
        else
          cpu_error(5);  /* missing closing parenthesis */
      }
      else
        cpu_error(4);  /* illegal operand type */
    }
    else
      op->basereg = NULL;
  }
  else
    op->basereg = NULL;

  if (p-start < len)
    cpu_error(3);  /* trailing garbage in operand */
  op->type = optype;
  return 1;
}


static taddr read_sdreg(char **s,taddr def)
{
  expr *tree;
  taddr val = def;

  *s = skip(*s);
  tree = parse_expr(s);
  simplify_expr(tree);
  if (tree->type==NUM && tree->c.val>=0 && tree->c.val<=31)
    val = tree->c.val;
  else
    cpu_error(13);  /* not a valid register */
  free_expr(tree);
  return val;
}


char *parse_cpu_special(char *start)
/* parse cpu-specific directives; return pointer to end of
   cpu-specific text */
{
  char *name=start,*s=start;

  if (ISIDSTART(*s)) {
    s++;
    while (ISIDCHAR(*s))
      s++;
    if (s-name==6 && !strncmp(name,".sdreg",6)) {
      sdreg = read_sdreg(&s,sdreg);
      return s;
    }
    else if (s-name==7 && !strncmp(name,".sd2reg",7)) {
      sd2reg = read_sdreg(&s,sd2reg);
      return s;
    }
  }
  return start;
}


static int get_reloc_type(operand *op)
{
  int rtype = REL_NONE;

  if (op->type >= OP_D8) {  /* data relocs */
    return REL_ABS;
  }

  else {  /* handle instruction relocs */
    const struct powerpc_operand *ppcop = &powerpc_operands[op->type];

    if (ppcop->shift == 0) {
      if (ppcop->bits == 16 || ppcop->bits == 26) {

        if (ppcop->flags & PPC_OPERAND_RELATIVE) {  /* a relative branch */
          switch (op->attr) {
            case REL_NONE:
              rtype = REL_PC;
              break;
            case REL_PLT:
              rtype = REL_PLTPC;
              break;
            case REL_LOCALPC:
              rtype = REL_LOCALPC;
              break;
            default:
              cpu_error(11); /* reloc attribute not supported by operand */
              break;
          }
        }

        else if (ppcop->flags & PPC_OPERAND_ABSOLUTE) { /* absolute branch */
          switch (op->attr) {
            case REL_NONE:
              rtype = REL_ABS;
              break;
            case REL_PLT:
            case REL_GLOBDAT:
            case REL_SECOFF:
              rtype = op->attr;
              break;
            default:
              cpu_error(11); /* reloc attribute not supported by operand */
              break;
          }
        }

        else {  /* immediate 16 bit or load/store d16(Rn) instruction */
          switch (op->attr) {
            case REL_NONE:
              rtype = REL_ABS;
              break;
            case REL_GOT:
            case REL_PLT:
            case REL_SD:
            case REL_PPCEABI_SDA2:
            case REL_PPCEABI_SDA21:
            case REL_PPCEABI_SDAI16:
            case REL_PPCEABI_SDA2I16:
            case REL_MORPHOS_DREL:
            case REL_AMIGAOS_BREL:
              rtype = op->attr;
              break;
            default:
              cpu_error(11); /* reloc attribute not supported by operand */
              break;
          }
        }
      }
    }
  }

  return rtype;
}


static nreloc *get_reloc_size(nreloc *r,operand *op)
/* determine reloc size, offset and mask
   creates and returns a second nreloc structure, when required */
{
  nreloc *r2 = NULL;

  if (op->type >= OP_D8) {  /* data */
    switch (op->type) {
      case OP_D8:
        r->size = 8;
        break;
      case OP_D16:
        r->size = 16;
        break;
      case OP_D32:
        r->size = 32;
        break;
      case OP_D64:
        r->size = 64;
        break;
      default:
        ierror(0);
        break;
    }
  }
  else {  /* instructions */
    const struct powerpc_operand *ppcop = &powerpc_operands[op->type];

    if (ppcop->flags & (PPC_OPERAND_RELATIVE|PPC_OPERAND_ABSOLUTE)) {
      /* branch instruction */
      if (ppcop->bits == 26) {
        r->size = 24;
        r->offset = 6;
        r->mask = 0x3fffffc;
      }
      else {
        r->size = 14;
        r->offset = 16;
        r->mask = 0xfffc;
      }
    }
    else {
      /* load/store or immediate */
      r->size = 16;
      r->offset = 16;
      switch (op->mode) {
        case OPM_LO:
          r->mask = 0xffff;
          break;
        case OPM_HI:
          r->mask = 0xffff0000;
          break;
        case OPM_HA:
          r->mask = 0xffff0000;
          r2 = new_nreloc();
          r2->size = 16;
          r2->offset = 16;
          r2->mask = 0x8000;
          r2->addend = r->addend;
          break;
      }
    }
  }
  return r2;
}


static int valid_hiloreloc(int type)
/* checks if this relocation type allows a @l/@h/@ha modifier */
{
  switch (type) {
    case REL_ABS:
    case REL_GOT:
    case REL_PLT:
    case REL_MORPHOS_DREL:
    case REL_AMIGAOS_BREL:
      return 1;
  }
  cpu_error(6);  /* relocation does not allow hi/lo modifier */
  return 0;
}


static taddr make_reloc(int reloctype,operand *op,section *sec,
                        taddr pc,rlist **reloclist)
/* create a reloc-entry when operand contains a non-constant expression */
{
  taddr val;

  if (!eval_expr(op->value,&val,sec,pc)) {
    /* non-constant expression requires a relocation entry */
    nreloc *r = new_nreloc();
    nreloc *r2;
    rlist *rl = mymalloc(sizeof(rlist));

    rl->type = reloctype;
    if (op->mode != OPM_NONE) {
      if (!valid_hiloreloc(reloctype))  /* check if reloc allows @ha/@h/@l */
        op->mode = OPM_NONE;
    }

    if (r->sym = find_base(op->value,sec,pc)) {
      if (reloctype==REL_PC) {
        /* a relative branch - reloc is only needed for external reference */
        if (r->sym->type == LABSYM && r->sym->sec == sec) {
          myfree(rl);
          myfree(r);
          return val-pc;
        }
      }
      r->addend = val;
      if (r2 = get_reloc_size(r,op)) {
        /* a second relocation for @ha is required */
        rlist *rl2 = mymalloc(sizeof(rlist));

        r2->sym = r->sym;
        rl2->type = rl->type;
        rl2->reloc = r2;
        rl2->next = *reloclist;
        *reloclist = rl2;
      }
      rl->reloc = r;
      rl->next = *reloclist;
      *reloclist = rl;
    }
    else
      cpu_error(10);  /* illegal relocation */
  }

  return val;
}


static void fix_reloctype(dblock *db,int rtype)
{
  rlist *rl;

  for (rl=db->relocs; rl!=NULL; rl=rl->next)
    rl->type = rtype;
}


static int cnt_insn_ops(instruction *p)
{
  int cnt = 0;

  while (p->op[cnt]!=NULL && cnt<MAX_OPERANDS)
    cnt++;
  return cnt;
}


static int cnt_mnemo_ops(mnemonic *p)
{
  int cnt = 0;

  while (p->operand_type[cnt]!=UNUSED && cnt<MAX_OPERANDS)
    cnt++;
  return cnt;
}


static void range_check(taddr val,const struct powerpc_operand *o,dblock *db)
/* checks if a value fits the allowed range for this operand field */
{
  long v = (long)val;
  long minv = 0;
  long maxv = (1L << o->bits) - 1;
  int force_signopt = 0;

  if (db) {
    if (db->relocs) {
      switch (db->relocs->type) {
        case REL_SD:
        case REL_PPCEABI_SDA2:
        case REL_PPCEABI_SDA21:
          force_signopt = 1;  /* relocation allows full positive range */
          break;
      }
    }
  }

  if (o->flags & PPC_OPERAND_SIGNED) {
    minv = ~(maxv >> 1);

    /* @@@ Only recognize this flag in 32-bit mode! Don't care for now */
    if (!(o->flags & PPC_OPERAND_SIGNOPT) && !force_signopt)
      maxv >>= 1;
  }
  if (o->flags & PPC_OPERAND_NEGATIVE)
    v = -v;

  if (v<minv || v>maxv)
    cpu_error(12,v,minv,maxv);  /* operand out of range */
}


static void negate_bo_cond(unsigned long *p)
/* negates all conditions in a branch instruction's BO field */
{
  if (!(*p & 0x02000000))
    *p ^= 0x01000000;
  if (!(*p & 0x00800000))
    *p ^= 0x00400000;
}


static unsigned long insertcode(unsigned long i,taddr val,
                                const struct powerpc_operand *o)
{
  if (o->insert) {
    const char *errmsg = NULL;

    i = (o->insert)(i,(long)val,&errmsg);
    if (errmsg)
      cpu_error(0,errmsg);
  }
  else
    i |= ((long)val & ((1<<o->bits)-1)) << o->shift;
  return i;
}


taddr eval_operands(instruction *ip,section *sec,taddr pc,
                    unsigned long *insn,dblock *db)
/* evaluate expressions and try to optimize instruction,
   return size of instruction */
{
  mnemonic *mnemo = &mnemonics[ip->code];
  int i=0,j=0,omitted=0;
  taddr isize=4;
  operand op;

  if (insn != NULL) {
    if (!(mnemo->ext.available & cpu_type))
      cpu_error(1);  /* instruction not supported on selected architecture */

    *insn = mnemo->ext.opcode;
  }

  while (ip->op[i]!=NULL && i<MAX_OPERANDS) {
    op = *(ip->op[i++]);

    for (;;) {
      const struct powerpc_operand *ppcop;
      int reloctype;
      taddr val;

      op.type = mnemo->operand_type[j++];
      ppcop = &powerpc_operands[op.type];

      if (ppcop->flags & PPC_OPERAND_OPTIONAL) {
        if (cnt_insn_ops(ip)+omitted < cnt_mnemo_ops(mnemo)) {
          /* an optional operand was omitted */
          omitted++;
          if (ppcop->flags & PPC_OPERAND_NEXT) {
            ppcop++;
          }
          else {  /* use a value of 0 for omitted operand */
            if (insn != NULL)
              *insn = insertcode(*insn,0,ppcop);
            continue;
          }
        }
      }

      if ((reloctype = get_reloc_type(&op)) != REL_NONE) {
        if (db != NULL) {
          val = make_reloc(reloctype,&op,sec,pc,&db->relocs);
        }
        else {
          if (!eval_expr(op.value,&val,sec,pc)) {
            if (reloctype == REL_PC)
              val -= pc;
          }
        }
      }
      else {
        if (!eval_expr(op.value,&val,sec,pc))
          if (insn != NULL)
            cpu_error(2);  /* constant integer expression required */
      }

      /* execute modifier on val */
      if (op.mode) {
        switch (op.mode) {
          case OPM_LO:
            val &= 0xffff;
            break;
          case OPM_HI:
            val = (val>>16) & 0xffff;
            break;
          case OPM_HA:
            val = ((val>>16) + ((val & 0x8000) ? 1 : 0) & 0xffff);
            break;
        }
        if ((ppcop->flags & PPC_OPERAND_SIGNED) && (val & 0x8000))
          val -= 0x10000;
      }

      /* do optimizations here: */

      if (opt_branch) {
        if (reloctype==REL_PC &&
            (op.type==BD || op.type==BDM || op.type==BDP)) {
          if (val<-0x8000 || val>0x7fff) {
            /* "B<cc>" branch destination out of range, convert into
               a "B<!cc> ; B" combination */
            if (insn != NULL) {
              negate_bo_cond(insn);
              *insn = insertcode(*insn,8,ppcop);  /* B<!cc> $+8 */
              insn++;
              *insn = B(18,0,0);  /* set B instruction opcode */
              val -= 4;
            }
            ppcop = &powerpc_operands[LI];  /* set oper. for B instruction */
            isize = 8;
          }
        }
      }

      if (ppcop->flags & PPC_OPERAND_PARENS) {
        if (op.basereg) {
          /* a load/store instruction d(Rn) carries basereg in current op */
          taddr reg;

          if (db!=NULL && op.mode==OPM_NONE && op.attr==REL_NONE) {
            if (eval_expr(op.basereg,&reg,sec,pc)) {
              if (reg == sdreg)  /* is it a small data reference? */
                fix_reloctype(db,REL_SD);
              else if (reg == sd2reg)  /* EABI small data 2 */
                fix_reloctype(db,REL_PPCEABI_SDA2);
            }
          }
          if (insn != NULL) {
            range_check(val,ppcop,db);
            *insn = insertcode(*insn,val,ppcop);
          }
          op.attr = REL_NONE;
          op.mode = OPM_NONE;
          op.value = op.basereg;
          continue;
        }
        else
          cpu_error(14);  /* missing base register */
      }
      else {
        if (insn != NULL) {
          range_check(val,ppcop,db);
          *insn = insertcode(*insn,val,ppcop);
        }

        if (j<MAX_OPERANDS && ip->op[i]==NULL && mnemo->operand_type[j]!=UNUSED) {
          if (powerpc_operands[mnemo->operand_type[j]].flags & PPC_OPERAND_FAKE) {
            /* next operand is a fake, just reuse last operand read */
            op.attr = REL_NONE;
            op.mode = OPM_NONE;
            continue;
          }
        }
      }

      break;  /* next parsed operand */
    }
  }

  return isize;
}


taddr instruction_size(instruction *ip,section *sec,taddr pc)
/* Calculate the size of the current instruction; must be identical
   to the data created by eval_instruction. */
{
  if (opt_branch)
    return eval_operands(ip,sec,pc,NULL,NULL);

  return 4;
}


dblock *eval_instruction(instruction *ip,section *sec,taddr pc)
/* Convert an instruction into a DATA atom including relocations,
   if necessary. */
{
  dblock *db = new_dblock();
  unsigned long insn[2];

  if (db->size = eval_operands(ip,sec,pc,insn,db)) {
    unsigned char *d = db->data = mymalloc(db->size);
    int i;

    for (i=0; i<db->size/4; i++)
      d = setval(1,d,4,insn[i]);
  }

  return db;
}


dblock *eval_data(operand *op,taddr bitsize,section *sec,taddr pc)
/* Create a dblock (with relocs, if necessary) for size bits of data. */
{
  dblock *db = new_dblock();
  taddr val;

  if ((bitsize & 7) || bitsize > 64)
    cpu_error(9,bitsize);  /* data size not supported */
  if (op->type < OP_D8)
    ierror(0);

  db->size = bitsize >> 3;
  db->data = mymalloc(db->size);
  val = make_reloc(get_reloc_type(op),op,sec,pc,&db->relocs);

  switch (db->size) {
    case 1:
      db->data[0] = val & 0xff;
      break;
    case 2:
    case 4:
    case 8:
      setval(ppc_endianess,db->data,db->size,val);
      break;
    default:
      ierror(0);
      break;
  }

  return db;
}


operand *new_operand()
{
  operand *new = mymalloc(sizeof(*new));
  new->type = -1;
  new->mode = OPM_NONE;
  return new;
}


static void define_regnames(void)
{
  char r[4];
  int i;

  for (i=0; i<32; i++) {
    sprintf(r,"r%d",i);
    new_abs(r,number_expr(i));
    r[0] = 'f';
    new_abs(r,number_expr(i));
    r[0] = 'v';
    new_abs(r,number_expr(i));
  }
  for (i=0; i<8; i++) {
    sprintf(r,"cr%d",i);
    new_abs(r,number_expr(i));
  }
  new_abs("vrsave",number_expr(256));
  new_abs("lt",number_expr(0));
  new_abs("gt",number_expr(1));
  new_abs("eq",number_expr(2));
  new_abs("so",number_expr(3));
  new_abs("un",number_expr(3));
  new_abs("sp",number_expr(1));
  new_abs("rtoc",number_expr(2));
  new_abs("fp",number_expr(31));
  new_abs("fpscr",number_expr(0));
  new_abs("xer",number_expr(1));
  new_abs("lr",number_expr(8));
  new_abs("ctr",number_expr(9));
}


int init_cpu()
{
  if (regnames)
    define_regnames();
  return 1;
}


int cpu_args(char *p)
{
  int i;

  if (!strncmp(p,"-m",2)) {
    p += 2;
    if (!strcmp(p,"pwrx") || !strcmp(p,"pwr2"))
      cpu_type = PPC_OPCODE_POWER2;
    else if (!strcmp(p,"pwr"))
      cpu_type = PPC_OPCODE_POWER;
    else if (!strcmp(p,"601"))
      cpu_type = PPC_OPCODE_601;
    else if (!strcmp(p,"ppc") || !strcmp(p,"ppc32") ||
             !strcmp(p,"403") || !strcmp(p,"603") || !strcmp(p,"604"))
      cpu_type = PPC_OPCODE_PPC | PPC_OPCODE_32;
    else if (!strcmp(p,"ppc64") || !strcmp(p,"620"))
      cpu_type = PPC_OPCODE_PPC | PPC_OPCODE_64;
    else if (!strcmp(p,"com"))
      cpu_type = PPC_OPCODE_COMMON;
    else if (!strcmp(p,"any"))
      cpu_type = PPC_OPCODE_ANY;
    else if (!strcmp(p,"avec"))  /* @@@ change this to 7500? 850? G4? */
      cpu_type = PPC_OPCODE_ALTIVEC;
    else
      return 0;
  }
  else if (!strcmp(p,"-no-regnames"))
    regnames = 0;
  else if (!strcmp(p,"-little"))
    ppc_endianess = 0;
  else if (!strcmp(p,"-big"))
    ppc_endianess = 1;
  else if (!strncmp(p,"-sdreg=",7)) {
    i = atoi(p+7);
    if (i>=0 && i<=31)
      sdreg = i;
    else
      cpu_error(13);  /* not a valid register */
  }
  else if (!strncmp(p,"-sd2reg=",8)) {
    i = atoi(p+8);
    if (i>=0 && i<=31)
      sd2reg = i;
    else
      cpu_error(13);  /* not a valid register */
  }
  else if (!strcmp(p,"-opt-branch"))
    opt_branch = 1;
  else
    return 0;

  return 1;
}
