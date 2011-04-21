/* syntax.c  syntax module for vasm */
/* (c) in 2002-2010 by Frank Wille */

#include "vasm.h"

/* The syntax module parses the input (read_next_line), handles
   assembly-directives (section, data-storage etc.) and parses
   mnemonics. Assembly instructions are split up in mnemonic name,
   qualifiers and operands. new_inst returns a matching instruction,
   if one exists.
   Routines for creating sections and adding atoms to sections will
   be provided by the main module.
*/

char *syntax_copyright="vasm oldstyle syntax module 0.8e (c) 2002-2010 Frank Wille";

char commentchar=';';
char *defsectname = NULL;
char *defsecttype = NULL;

static int dotdirectives = 0;
static hashtable *dirhash;
static int parse_end = 0;

static char *textname=".text",*textattr="acrx";     
static char *dataname=".data",*dataattr="adrw";     
static char *rodataname=".rodata",*rodataattr="adr";
static char *bssname=".bss",*bssattr="aurw";   

#define MAXCONDLEV 63
static char cond[MAXCONDLEV+1];
static int clev,ifnesting;

#define LOCAL (RSRVD_S<<0)      /* symbol flag for local binding */


char *skip(char *s)
{
  while (isspace((unsigned char )*s))
    s++;
  return s;
}


/* check for end of line, issue error, if not */
void eol(char *s)
{
  s = skip(s);
  if (*s!='\0' && *s!=commentchar)
    syntax_error(6);
}


char *skip_operand(char *s)
{
  int par_cnt = 0;
  char c;

  for (;;) {
    c = *s;
    if (START_PARENTH(c))
      par_cnt++;
    if (END_PARENTH(c)) {
      if (par_cnt>0)
        par_cnt--;
      else
        syntax_error(3);  /* too many closing parentheses */
    }
    if(!c || c==commentchar || (c==',' && par_cnt==0))
      break;
    s++;
  }
  if(par_cnt != 0)
    syntax_error(4);  /* missing closing parentheses */
  return s;
}


static void handle_data(char *s,int size)
{
  for (;;) {
    char *opstart = s;
    operand *op;
    dblock *db = NULL;

    if (size==8 && (*s=='\"' || *s=='\'')) {
      if (db = parse_string(&opstart,*s,8)) {
        add_atom(0,new_data_atom(db,1));
        s = opstart;
      }
    }
    if (!db) {
      op = new_operand();
      s = skip_operand(s);
      if (parse_operand(opstart,s-opstart,op,DATA_OPERAND(size))) {
        atom *a;

        a = new_datadef_atom(abs(size),op);
        a->align = 1;
        add_atom(0,a);
      }
      else
        syntax_error(8);  /* invalid data operand */
    }

    s = skip(s);
    if (*s == ',') {
      s = skip(s+1);
    }
    else if (*s == commentchar) {
      break;
    }
    else if (*s) {
      syntax_error(9);  /* , expected */
      return;
    }
    else
      break;
  }

  eol(s);
}


static void handle_text(char *s)
{
  char *opstart = s;
  operand *op;
  dblock *db = NULL;

  if (db = parse_string(&opstart,*s,8)) {
    add_atom(0,new_data_atom(db,1));
    s = opstart;
  }
  if (!db) {
    op = new_operand();
    s = skip_operand(s);
    if (parse_operand(opstart,s-opstart,op,DATA_OPERAND(8))) {
      atom *a;

      a = new_datadef_atom(8,op);
      a->align = 1;
      add_atom(0,a);
    }
    else
      syntax_error(8);  /* invalid data operand */
  }
  eol(s);
}


static void handle_d8(char *s)
{
  handle_data(s,8);
}


static void handle_d16(char *s)
{
  handle_data(s,16);
}


static void handle_d24(char *s)
{
  handle_data(s,24);
}


static void handle_d32(char *s)
{
  handle_data(s,32);
}


static void do_alignment(taddr align,expr *offset)
{
  atom *a = new_space_atom(offset,1,0);

  a->align = align;
  add_atom(0,a);
}


static void handle_align(char *s)
{
  do_alignment(1<<parse_constexpr(&s),number_expr(0));
  eol(s);
}


static void handle_even(char *s)
{
  do_alignment(2,number_expr(0));
  eol(s);
}


static void do_space(int size,expr *cnt,expr *fill)
{
  add_atom(0,new_space_atom(cnt,size>>3,fill));
}


static void handle_space(char *s,int size)
{
  expr *cnt,*fill=0;

  cnt = parse_expr_tmplab(&s);
  s = skip(s);
  if (*s == ',') {
    s = skip(s+1);
    fill = parse_expr_tmplab(&s);
  }
  do_space(size,cnt,fill);
  eol(s);
}


static void handle_fixedspc1(char *s)
{
  do_space(8,number_expr(1),0);
  eol(s);
}


static void handle_fixedspc2(char *s)
{
  do_space(8,number_expr(2),0);
  eol(s);
}


static void handle_spc8(char *s)
{
  handle_space(s,8);
}


static void handle_spc16(char *s)
{
  handle_space(s,16);
}


#if 0
static void handle_spc24(char *s)
{
  handle_space(s,24);
}


static void handle_spc32(char *s)
{
  handle_space(s,32);
}
#endif


static void handle_string(char *s)
{
  handle_data(s,8);  
  add_atom(0,new_space_atom(number_expr(1),1,0));  /* terminating zero */
}


static void handle_end(char *s)
{
  parse_end = 1;
  eol(s);
}


static void handle_org(char *s)
{
  if (*s == current_pc_char) {
    char *s2 = skip(s+1);

    if (*s2++ == '+') {
      handle_space(skip(s2),8);  /*  "* = * + <expr>" to reserves bytes */
      return;
    }
  }
  new_org(parse_constexpr(&s));
  eol(s);
}


static void handle_section(char *s)
{
  char *name,*attr;

  if (!(name=parse_name(&s)))
    return;
  if (*s==',') {
    s = skip(s+1);
    attr = s;
    if (*s!= '\"')
      syntax_error(7,'\"');  /* " expected */
    else
      s++;
    attr = s;
    while (*s&&*s!='\"')
      s++;    
    attr = cnvstr(attr,s-attr);
    s = skip(s+1);
  }
  else {
    attr = "";
    if (!strcmp(name,textname)) attr = textattr;
    if (!strcmp(name,dataname)) attr = dataattr;
    if (!strcmp(name,rodataname)) attr = rodataattr;
    if (!strcmp(name,bssname)) attr = bssattr;
  }

  new_section(name,attr,1);
  switch_section(name,attr);
  eol(s);
}


static char *get_bind_name(symbol *s)
{
  if (s->flags&EXPORT)
    return "global";
  else if(s->flags&WEAK)
    return "weak";
  else if(s->flags&LOCAL)
    return "local";
  return "unknown";
}


static void do_binding(char *s,int bind)
{
  symbol *sym;
  char *name;

  while (1) {
    if (!(name=parse_identifier(&s)))
      return;
    sym = new_import(name);
    myfree(name);
    if (sym->flags&(EXPORT|WEAK|LOCAL)!=0 &&
        sym->flags&(EXPORT|WEAK|LOCAL)!=bind)
      syntax_error(20,sym->name,get_bind_name(sym));  /* binding already set */
    else
      sym->flags |= bind;
    s = skip(s);
    if (*s != ',')
      break;
    s = skip(s+1);
  }
  eol(s);
}


static void handle_global(char *s)
{
  do_binding(s,EXPORT);
}


static void handle_weak(char *s)
{
  do_binding(s,WEAK);
}


static void handle_local(char *s)
{
  do_binding(s,LOCAL);
}


static void ifdef(char *s,int b)
{
  char *name = s;
  symbol *sym;
  int result;

  name = s;
  if (!(s = skip_identifier(s))) {
    syntax_error(10);  /* identifier expected */
    return;
  }
  name = cnvstr(name,s-name);
  if (sym = find_symbol(name))
    result = sym->type != IMPORT;
  else
    result = 0;
  myfree(name);
  cond[++clev] = result == b;
  eol(s);
}


static void handle_ifd(char *s)
{
  ifdef(s,1);
}


static void handle_ifnd(char *s)
{
  ifdef(s,0);
}


static void ifexp(char *s,int c)
{
  taddr val = parse_constexpr(&s);
  int b;

  switch (c) {
    case 0: b = val == 0; break;
    case 1: b = val != 0; break;
    case 2: b = val > 0; break;
    case 3: b = val >= 0; break;
    case 4: b = val < 0; break;
    case 5: b = val <= 0; break;
    default: ierror(0); break;
  }
  cond[++clev] = b;
  eol(s);
}


static void handle_ifeq(char *s)
{
  ifexp(s,0);
}


static void handle_ifne(char *s)
{
  ifexp(s,1);
}


static void handle_ifgt(char *s)
{
  ifexp(s,2);
}


static void handle_ifge(char *s)
{
  ifexp(s,3);
}


static void handle_iflt(char *s)
{
  ifexp(s,4);
}


static void handle_ifle(char *s)
{
  ifexp(s,5);
}


static void handle_else(char *s)
{
  eol(s);
  if (clev > 0)
    cond[clev] = 0;
  else
    syntax_error(17);  /* else without if */
}


static void handle_endif(char *s)
{
  eol(s);
  if (clev > 0)
    clev--;
  else
    syntax_error(14);  /* endif without if */
}


static void handle_incdir(char *s)
{
  char *name;

  if (name = parse_name(&s))
    new_include_path(name);
  eol(s);
}


static void handle_include(char *s)
{
  char *name;

  if (name = parse_name(&s)) {
    eol(s);
    include_source(name);
  }
}


static void handle_incbin(char *s)
{
  char *name;

  if (name = parse_name(&s)) {
    eol(s);
    include_binary_file(name);
  }
}


static void handle_rept(char *s)
{
  taddr cnt = parse_constexpr(&s);

  eol(s);
  new_repeat((int)cnt,dotdirectives?".rept":"rept",dotdirectives?".endr":"endr");
}


static void handle_endr(char *s)
{
  syntax_error(19);  /* unexpected endr without rept */
}


static void handle_macro(char *s)
{
  char *name;

  if (name = parse_identifier(&s)) {
    eol(s);
    new_macro(name,dotdirectives?".endm":"endm");
    myfree(name);
  }
  else
    syntax_error(10);  /* identifier expected */
}


static void handle_endm(char *s)
{
  syntax_error(18);  /* unexpected endm without macro */
}


static void handle_defc(char *s)
{
  char *name;

  s = skip(s);
  name = parse_identifier(&s);
  if ( name != NULL ) {
    s = skip(s);
    if ( *s == '=' ) {
      s = skip(s+1);
      new_abs(name,parse_expr_tmplab(&s));
    }
    myfree(name);
  }
  else
    syntax_error(10);
}


struct {
  char *name;
  void (*func)(char *);
} directives[] = {
  "org",handle_org,
  "align",handle_align,
  "even",handle_even,
  "byte",handle_d8,
  "db",handle_d8,
  "dfb",handle_d8,
  "defb",handle_d8,
  "asc",handle_d8,
  "data",handle_d8,
  "defm",handle_text,
  "text",handle_text,
  "wor",handle_d16,
  "word",handle_d16,
  "addr",handle_d16,
  "dw",handle_d16,
  "dfw",handle_d16,
  "defw",handle_d16,
  "ds",handle_spc8,
  "dsb",handle_spc8,
  "fill",handle_spc8,
  "reserve",handle_spc8,
  "spc",handle_spc8,
  "dsw",handle_spc16,
  "blk",handle_spc8,
  "blkw",handle_spc16,
  "dc",handle_spc8,
  "byt",handle_fixedspc1,
  "wrd",handle_fixedspc2,
  "ifdef",handle_ifd,
  "ifndef",handle_ifnd,
  "if",handle_ifne,
  "ifeq",handle_ifeq,
  "ifne",handle_ifne,
  "ifgt",handle_ifgt,
  "ifge",handle_ifge,
  "iflt",handle_iflt,
  "ifle",handle_ifle,
  "else",handle_else,
  "el",handle_else,
  "endif",handle_endif,
#if !defined(VASM_CPU_Z80)
  "ei",handle_endif,  /* Clashes with z80 opcode */
#endif
  "incbin",handle_incbin,
  "mdat",handle_incbin,
  "incdir",handle_incdir,
  "include",handle_include,
  "rept",handle_rept,
  "repeat",handle_rept,
  "endr",handle_endr,
  "endrep",handle_endr,
  "endrepeat",handle_endr,
  "mac",handle_macro,
  "macro",handle_macro,
  "endm",handle_endm,
  "endmac",handle_endm,
  "endmacro",handle_endm,
  "end",handle_end,
  "section",handle_section,
  "binary",handle_incbin,
  "defs",handle_spc8,
  "defp",handle_d24,
  "defl",handle_d32,
  "defc",handle_defc,
  "xdef",handle_global,
  "xref",handle_global,
  "lib",handle_global,
  "xlib",handle_global,
  "global",handle_global,
  "extern",handle_global,
  "local",handle_local,
  "weak",handle_weak,
  "ascii",handle_string,
  "asciiz",handle_string,
  "string",handle_string,
};

int dir_cnt = sizeof(directives) / sizeof(directives[0]);


/* checks for a valid directive, and return index when found, -1 otherwise */
static int check_directive(char **line)
{
  char *s,*name;
  hashdata data;

  s = skip(*line);
  if (!ISIDSTART(*s))
    return -1;
  name = s++;
  while (ISIDCHAR(*s))
    s++;
  if (*name=='.' && dotdirectives)
    name++;
  if (!find_namelen_nc(dirhash,name,s-name,&data))
    return -1;
  *line = s;
  return data.idx;
}


/* Handles assembly directives; returns non-zero if the line
   was a directive. */
static int handle_directive(char *line)
{
  int idx = check_directive(&line);

  if (idx >= 0) {
    directives[idx].func(skip(line));
    return 1;
  }
  return 0;
}


static int oplen(char *e,char *s)
{
  while(s!=e&&isspace((unsigned char)e[-1]))
    e--;
  return e-s;
}


void parse(void)
{
  char *s,*line,*inst,*labname;
  char *ext[MAX_QUALIFIERS?MAX_QUALIFIERS:1];
  char *op[MAX_OPERANDS];
  int ext_len[MAX_QUALIFIERS?MAX_QUALIFIERS:1];
  int op_len[MAX_OPERANDS];
  int ext_cnt,op_cnt,inst_len;
  instruction *ip;

  while (line = read_next_line()) {
    if (parse_end)
      continue;
    if (clev >= MAXCONDLEV)
      syntax_error(16,clev);  /* nesting depth exceeded */

    if (!cond[clev]) {
      /* skip source until ELSE or ENDIF */
      int idx;

      s = line;
      idx = check_directive(&s);
      if (idx >= 0) {
        if (!strncmp(directives[idx].name,"if",2)) {
          ifnesting++;
        }
        else if (ifnesting==0 && directives[idx].func == handle_else) {
          cond[clev] = 1;
        }
        else if (directives[idx].func == handle_endif) {
          if (ifnesting == 0) {
            if (clev > 0)
              clev--;
            else
              syntax_error(14);  /* endif without if */
          }
          else
            ifnesting--;
        }
      }
      continue;
    }

    s = line;

    labname = get_local_label(&s); /* local label? */

    if (!labname && (ISIDSTART(*s) || *s==current_pc_char)) { /* global l.? */
      s++;
      while (ISIDCHAR(*s))
        s++;
      if (*line==current_pc_char && s-line!=1) {
        syntax_error(10);  /* identifier expected */
        continue;
      }
      labname = cnvstr(line,s-line);
      s = skip(s);
    }

    if (labname) {
      /* we have found a global or local label at first column */
      symbol *label,*labsym;

      if (*s==':' && *labname!=current_pc_char) {
        label = new_labsym(0,labname);
        add_atom(0,new_label_atom(label));
        s = skip(s+1);
      }
      else {
        if ((!strnicmp(s,"equ",3) && isspace((unsigned char)*(s+3))) ||
            (!strnicmp(s,"eq",2) && isspace((unsigned char)*(s+2)))) {
          if (*labname == current_pc_char) {
            handle_org(skip(s+3));
            continue;
          }
          else {
            if (labsym = find_symbol(labname)) {
              if (labsym->type != IMPORT)
                syntax_error(13);  /* repeatedly defined symbol */
            }
            s = skip(s+3);
            label = new_abs(labname,parse_expr_tmplab(&s));
          }
        }
        else if (*s=='=') {
          if (*labname == current_pc_char) {
            handle_org(skip(s+3));
            continue;
          }
          else {
            if (labsym = find_symbol(labname)) {
              if (labsym->type != IMPORT)
                syntax_error(13);  /* repeatedly defined symbol */
            }
            s = skip(s+1);
            label = new_abs(labname,parse_expr_tmplab(&s));
          }
        }
        else {
          if (*labname == current_pc_char) {
            syntax_error(10);  /* identifier expected */
          }
          else if (!strnicmp(s,"set",3) && isspace((unsigned char)*(s+3))) {
            /* SET allows redefinitions */
            s = skip(s+3);
            label = new_abs(labname,parse_expr_tmplab(&s));
          }
          else {
            label = new_labsym(0,labname);
            add_atom(0,new_label_atom(label));
            if (*s == current_pc_char) {
              char *s2 = skip(s+1);   /* label *= ... */

              if (*s2 == '=') {
                handle_org(skip(s2+1));
                continue;
              }
            }
          }
        }
      }
      myfree(labname);
    }

    /* check for directives first */
    s = skip(s);
    if (*s==commentchar)
      continue;

    s = parse_cpu_special(s);
    if (*s=='\0' || *s==commentchar)
      continue;

    if (*s==current_pc_char && *(s+1)=='=') {   /* "*=" org directive */ 
      handle_org(skip(s+2));
      continue;
    }
    if (handle_directive(s))
      continue;

    s = skip(s);
    if (*s=='\0' || *s==commentchar)
      continue;

    /* read mnemonic name */
    inst = s;
    ext_cnt = 0;
    if (!ISIDSTART(*s)) {
      syntax_error(10);  /* identifier expected */
      continue;
    }
#if MAX_QUALIFIERS==0
    while (*s && !isspace((unsigned char)*s))
      s++;
    inst_len = s - inst;
#else
    s = parse_instruction(s,&inst_len,ext,ext_len,&ext_cnt);
#endif
    if (!isspace((unsigned char)*s) && *s!='\0')
      syntax_error(2);  /* no space before operands */
    s = skip(s);

    if (execute_macro(inst,inst_len,ext,ext_len,ext_cnt,s,clev))
      continue;

    /* read operands, terminated by comma (unless in parentheses)  */
    op_cnt = 0;
    while (*s && *s!=commentchar && op_cnt<MAX_OPERANDS) {
      op[op_cnt] = s;
      s = skip_operand(s);
      op_len[op_cnt] = oplen(s,op[op_cnt]);
#if !ALLOW_EMPTY_OPS
      if (op_len[op_cnt] <= 0)
        syntax_error(5);  /* missing operand */
      else
#endif
        op_cnt++;
      s = skip(s);
      if (*s != ',')
        break;
      else
        s = skip(s+1);
    }      
    s = skip(s);
    if (*s!='\0' && *s!=commentchar)
      syntax_error(6);

    ip = new_inst(inst,inst_len,op_cnt,op,op_len);

#if MAX_QUALIFIERS>0
    if (ip) {
      int i;

      for (i=0; i<ext_cnt; i++)
        ip->qualifiers[i] = cnvstr(ext[i],ext_len[i]);
      for(; i<MAX_QUALIFIERS; i++)
        ip->qualifiers[i] = NULL;
    }
#endif

    if (ip)
      add_atom(0,new_inst_atom(ip));
  }

  if (clev > 0)
    syntax_error(15);  /* if without endif */
}


char *const_prefix(char *s,int *base)
{
  if (isdigit((unsigned char)*s)) {
    if (*s == '0') {
      if (s[1]=='x' || s[1]=='X'){
        *base = 16;
        return s+2;
      }
      if (s[1]=='b' || s[1]=='B'){
        *base = 2;
        return s+2;
      }    
      *base = 8;
      return s;
    } 
    else if (*(s+1)=='#' && *s>='2') {
      *base = *s & 0xf;
      return s+2;
    }
    else {
      *base = 10;
      return s;
    }
  }
  if (*s=='$' && isxdigit((unsigned char)s[1])) {
    *base = 16;
    return s+1;
  }
  if (*s=='@') {
#if defined(VASM_CPU_Z80)
    *base = 2;
#else
    *base = 8;
#endif
    return s+1;
  }
  if (*s == '%') {
    *base = 2;
    return s+1;
  }
  *base = 0;
  return s;
}


char *get_local_label(char **start)
/* Local labels start with a '.' or end with '$': "1234$", ".1" */
{
  char *s = *start;
  char *name = NULL;

  if (*s == '.') {
    s++;
    while (isalnum((unsigned char)*s) || *s=='_')  /* '_' needed for '\@' */
      s++;
    if (s > (*start+1)) {
      name = make_local_label(*start,s-*start);
      *start = skip(s);
    }
  }
  else {
    while (isalnum((unsigned char)*s) || *s=='_')  /* '_' needed for '\@' */
      s++;
    if (s!=*start && *s=='$') {
      name = make_local_label(*start,s-*start);
      *start = skip(++s);
    }
  }
  return name;
}


int init_syntax()
{
  size_t i;
  hashdata data;

  dirhash = new_hashtable(0x200); /* @@@ */
  for (i=0; i<dir_cnt; i++) {
    data.idx = i;
    add_hashentry(dirhash,directives[i].name,data);
  }
  
  current_pc_char = '*';
  cond[0] = 1;
  clev = ifnesting = 0;
  return 1;
}


int syntax_args(char *p)
{
  if (!strcmp(p,"-dotdir")) {
    dotdirectives = 1;
    return 1;
  }
  return 0;
}
