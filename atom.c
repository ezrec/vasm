/* atom.c - atomic objects from source */
/* (c) in 2010-2011 by Volker Barthelmann and Frank Wille */

#include "vasm.h"


/* test implementation, not good */
/* searches mnemonic list and tries to parse (via the cpu module)
   the operands according to the mnemonic requirements; returns an
   instruction or 0 */
instruction *new_inst(char *inst,int len,int op_cnt,char **op,int *op_len)
{
#if MAX_OPERANDS!=0
  operand ops[MAX_OPERANDS];
  int omitted[MAX_OPERANDS]={0};
#endif
  int i,j,inst_found=0,rc;
  hashdata data;
  instruction *new=mymalloc(sizeof(*new));

#ifdef HAVE_INSTRUCTION_EXTENSION
  init_instruction_ext(&new->ext);
#endif
#ifdef NEED_CLEARED_OPERANDS
  /* reset operands to allow the cpu-backend to parse them only once */
  memset(ops,0,sizeof(ops));
#endif

  if (find_namelen_nc(mnemohash,inst,len,&data)) {
    /* instruction name fits - are operands ok? */
    i = data.idx;
    inst_found = 1;

    /* search all mnemonics with the same name */
    do {
      for (j=0; j<op_cnt; j++) {
#if !ALLOW_EMPTY_OPS
        if (mnemonics[i].operand_type[j] == 0) {
#if IGNORE_FIRST_EXTRA_OP
          if (!j)
            for (; j<op_cnt; omitted[j++]=1);
#endif
          break;
        }
#endif
        rc = parse_operand(op[j],op_len[j],&ops[j],
                           mnemonics[i].operand_type[j]);
        if (rc == PO_CORRUPT) {
          myfree(new);
          return 0;
        }
        if(rc == PO_NOMATCH) {
          if (rc != PO_CORRUPT &&
              OPERAND_OPTIONAL(mnemonics[i].operand_type[j]))
            omitted[j] = 1;
          else
            break;
        }
      }
#if CPU_CHECKS_OPCNT
      if (j<op_cnt) {
#else
      if (j<op_cnt ||
          (op_cnt<MAX_OPERANDS && mnemonics[i].operand_type[op_cnt]!=0)) {
#endif
        i++;
        continue;
      }
      new->code = i;
      for (j=0; j<op_cnt; j++) {
        if (omitted[j]) {
          new->op[j] = 0;
        }
        else {
          new->op[j] = mymalloc(sizeof(operand));
          *new->op[j] = ops[j];
        }
      }
      for(; j<MAX_OPERANDS; j++)
        new->op[j] = 0;
      return new;
      i++;
    }
    while(i<mnemonic_cnt && !strnicmp(mnemonics[i].name,inst,len)
          && mnemonics[i].name[len]==0);
  }

  if (inst_found)
    general_error(0);
  else
    general_error(1,cnvstr(inst,len));
  myfree(new);

  return 0;
}


dblock *new_dblock(void)
{
  dblock *new = mymalloc(sizeof(*new));

  new->size = 0;
  new->data = 0;
  new->relocs = 0;
  return new;
}


sblock *new_sblock(expr *space,int size,expr *fill)
{
  sblock *sb = mymalloc(sizeof(sblock));

  sb->space = 0;
  sb->space_exp = space;
  sb->size = size;
  if (!(sb->fill_exp = fill))
    memset(sb->fill,0,SB_MAXSIZE);
  sb->relocs = 0;
  return sb;
}


static taddr space_size(sblock *sb,section *sec,taddr pc)
{
  taddr space=0;

  if (eval_expr(sb->space_exp,&space,sec,pc) || !final_pass)
    sb->space = space;
  else
    general_error(30);  /* expression must be constant */

  if (final_pass && sb->fill_exp) {
    if (sb->size <= sizeof(taddr)) {
      /* space is filled with an expression which may also need relocations */
      symbol *base=NULL;
      taddr fill,i;

      if (!eval_expr(sb->fill_exp,&fill,sec,pc)) {
        base = find_base(sb->fill_exp,sec,pc);
        if (!base)
          general_error(38);  /* illegal relocation */
      }
      copy_cpu_taddr(sb->fill,fill,sb->size);
      if (base && !sb->relocs) {
        /* generate relocations */
        for (i=0; i<space; i++)
          add_reloc(&sb->relocs,base,fill,REL_ABS,
                    sb->size<<3,(i*sb->size)<<3);
      }
    }
    else
      general_error(30);  /* expression must be constant */
  }

  return space * (taddr)sb->size;
}


static taddr rorg_size(expr *rorg,section *sec,taddr pc)
{
  taddr offs;

  eval_expr(rorg,&offs,sec,pc);
  offs = sec->org + offs - pc;
  return offs>0 ? offs : 0;
}


/* adds an atom to the specified section; if sec==0, the current
   section is used */
void add_atom(section *sec,atom *a)
{
  taddr size;

  if (!sec) {
    sec = default_section();
    if (!sec) {
      general_error(3);
      return;
    }
  }

  a->src = cur_src;
  a->line = cur_src->line;

  if (sec->last) {
    atom *pa = sec->last;

    pa->next = a;
    /* make sure that a label on the same line gets the same alignment */
    if (pa->type==LABEL && pa->line==a->line &&
        (a->type==INSTRUCTION || a->type==DATADEF || a->type==SPACE))
      pa->align = a->align;
  }
  else
    sec->first = a;
  a->next = 0;
  sec->last = a;

  sec->pc = (sec->pc + a->align - 1) / a->align * a->align;
  size = atom_size(a,sec,sec->pc);
#ifdef CHECK_ATOMSIZE
  a->lastsize = size;
#endif
  sec->pc += size;
  if (a->align > sec->align)
    sec->align = a->align;

  if (produce_listing) {
    a->list = last_listing;
    if (last_listing) {
      if (!last_listing->atom)
        last_listing->atom = a;
    }
  }
  else
    a->list = 0;
}


taddr atom_size(atom *p,section *sec,taddr pc)
{
  switch(p->type) {
    case LABEL:
    case LINE:
    case OPTS:
    case PRINTTEXT:
    case PRINTEXPR:
      return 0;
    case DATA:
      return p->content.db->size;
    case INSTRUCTION:
      return instruction_size(p->content.inst,sec,pc);
    case SPACE:
      return space_size(p->content.sb,sec,pc);
    case DATADEF:
      return (p->content.defb->bitsize+7)/8;
    case RORG:
      return rorg_size(p->content.roffs,sec,pc);
    default:
      ierror(0);
      break;
  }
  return 0;
}


static void print_instruction(FILE *f,instruction *p)
{
  int i;

  printf("inst %d(%s) ",p->code,mnemonics[p->code].name);
#if MAX_OPERANDS!=0
  for (i=0; i<MAX_OPERANDS; i++)
    printf("%p ",(void *)p->op[i]);
#endif
}


void print_atom(FILE *f,atom *p)
{
  size_t i;
  rlist *rl;

  switch (p->type) {
    case LABEL:
      fprintf(f,"symbol: ");
      print_symbol(f,p->content.label);
      break;
    case DATA:
      fprintf(f,"data(%lu): ",(unsigned long)p->content.db->size);
      for (i=0;i<p->content.db->size;i++)
        fprintf(f,"%02x ",(unsigned char)p->content.db->data[i]);
      for (rl=p->content.db->relocs; rl; rl=rl->next)
        print_reloc(f,rl->type,rl->reloc);
      break;
    case INSTRUCTION:
      print_instruction(f,p->content.inst);
      break;
    case SPACE:
      fprintf(f,"space(%lu,fill=",
              (unsigned long)(p->content.sb->space*p->content.sb->size));
      for (i=0; i<p->content.sb->size; i++)
        fprintf(f,"%02x%c",(unsigned char)p->content.sb->fill[i],
                (i==p->content.sb->size-1)?')':' ');
      for (rl=p->content.sb->relocs; rl; rl=rl->next)
        print_reloc(f,rl->type,rl->reloc);
      break;
    case DATADEF:
      fprintf(f,"datadef(%lu bits)",(unsigned long)p->content.defb->bitsize);
      break;
    case LINE:
      fprintf(f,"line: %d of %s",p->content.srcline,getdebugname());
      break;
#ifdef HAVE_CPU_OPTS
    case OPTS:
      print_cpu_opts(f,p->content.opts);
      break;
#endif
    case PRINTTEXT:
      fprintf(f,"text: \"%s\"",p->content.ptext);
      break;
    case PRINTEXPR:
      fprintf(f,"expr: ");
      print_expr(f,p->content.pexpr);
      break;
    case RORG:
      fprintf(f,"rorg: offset ");
      print_expr(f,p->content.roffs);
      break;
    default:
      ierror(0);
  }
}


atom *new_inst_atom(instruction *p)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = INSTRUCTION;
  new->align = INST_ALIGN;
  new->content.inst = p;
  return new;
}


atom *new_data_atom(dblock *p,int align)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = DATA;
  new->align = align;
  new->content.db=p;
  return new;
}


atom *new_label_atom(symbol *p)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = LABEL;
  new->align = 1;
  new->content.label = p;
  return new;
}


atom *new_space_atom(expr *space,int size,expr *fill)
{
  atom *new = mymalloc(sizeof(*new));
  int i;

  if (size<1)
    ierror(0);  /* usually an error in syntax-module */
  new->next = 0;
  new->type = SPACE;
  new->align = 1;
  new->content.sb = new_sblock(space,size,fill);
  return new;
}  


atom *new_datadef_atom(taddr bitsize,operand *op)
{
  atom *new = mymalloc(sizeof(*new));
  new->next = 0;
  new->type = DATADEF;
  new->align = DATA_ALIGN(bitsize);
  new->content.defb = mymalloc(sizeof(*new->content.defb));
  new->content.defb->bitsize = bitsize;
  new->content.defb->op = op;
  return new;
}


atom *new_srcline_atom(int line)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = LINE;
  new->align = 1;
  new->content.srcline = line;
  return new;
}


atom *new_opts_atom(void *o)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = OPTS;
  new->align = 1;
  new->content.opts = o;
  return new;
}


atom *new_text_atom(char *txt)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = PRINTTEXT;
  new->align = 1;
  new->content.ptext = txt ? txt : "";
  return new;
}


atom *new_expr_atom(expr *x)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = PRINTEXPR;
  new->align = 1;
  new->content.pexpr = x;
  return new;
}


atom *new_rorg_atom(expr *offs)
{
  atom *new = mymalloc(sizeof(*new));

  new->next = 0;
  new->type = RORG;
  new->align = 1;
  new->content.roffs = offs;
  return new;
}
