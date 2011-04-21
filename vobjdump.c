/*
 * vobjdump
 * Views the contents of a VOBJ file.
 * Written by Frank Wille <frank@phoenix.owl.de>.
 */

/*
  VOBJ Format (WILL CHANGE!):

  .byte 0x56,0x4f,0x42,0x4a
  .byte flags
    1: BIGENDIAN
    2: LITTLENDIAN
  .number bitsperbyte
  .number bytespertaddr
  .string cpu
  .number nsections [1-based]
  .number nsymbols [1-based]
  
nsymbols
  .string name
  .number type
  .number flags
  .number secindex
  .number val
  .number size

nsections
  .string name
  .string attr
  .number flags
  .number align
  .number size
  .number nrelocs
  .number databytes
  .byte[databytes]

nrelocs [standard|special]
standard
   .number type
   .number byteoffset
   .number bitoffset
   .number size
   .number mask
   .number addend
   .number symbolindex | 0 (sectionbase)

special
    .number type
    .number size
    .byte[size]

.number:[taddr]
    .byte 0--127 [0--127]
    .byte 128-255 [x-0x80 bytes little-endian]
*/

#include "vobjdump.h"

static const char *endian_name[] = { "big", "little" };
static const char emptystr[] = "";
static const char sstr[] = "s";
static const char *reloc_name[] = {
  "NONE","ABS","PC","GOT","GOTPC","GOTOFF","GLOBDAT","PLT","PLTPC","PLTOFF",
  "SD","UABS","LOCALPC","LOADREL","COPY","JMPSLOT","SECOFF",NULL
};
static const char *type_name[] = {
  "","obj","func","sect","file",NULL
};

static ubyte *vobj;   /* base address of VOBJ buffer */
static size_t vlen;   /* length of VOBJ file in buffer */
static ubyte *p;      /* current object pointer */



static void print_sep(void)
{
  printf("\n------------------------------------------------------------"
         "------------------\n");
}


static taddr read_number(void)
{
  taddr val;
  ubyte n,*q;
  int size;

  if (p >= vobj+vlen) {
    fprintf(stderr,"\nObject file is corrupt! Aborting.\n");
    exit(1);
  }

  if ((n = *p++) <= 0x7f)
    return (taddr)n;

  val = 0;

  if (n -= 0x80) {
    size = n << 3;
    p += n;
    q = p;

    while (n--)
      val = (val<<8) | *(--q);

    if (val & (1LL<<(size-1)))
      val |= ~makemask(size);
  }
  return val;
}


static void skip_string(void)
{
  while (*p)
    p++;
  p++;
}


static void read_symbol(struct vobj_symbol *vsym)
{
  vsym->offs = p - vobj;
  vsym->name = (const char *)p;
  skip_string();
  vsym->type = (int)read_number();
  vsym->flags = (int)read_number();
  vsym->sec = (int)read_number();
  vsym->val = read_number();
  vsym->size = (int)read_number();
}


static void read_section(struct vobj_section *vsect,
                         struct vobj_symbol *vsym,int nsyms)
{
  const char *attr;
  unsigned long flags;
  int align,nrelocs,i;

  vsect->offs = p - vobj;
  vsect->name = p;
  skip_string();
  attr = p;
  skip_string();
  flags = (unsigned long)read_number();
  align = (int)read_number();
  vsect->dsize = read_number();
  nrelocs = (int)read_number();
  vsect->fsize = read_number();

  print_sep();
  printf("%08lx: SECTION \"%s\" (attributes=\"%s\")\n"
         "Flags: %-8lx  Alignment: %-6d "
         "Total size: %-9lld File size: %-9lld\n",
         vsect->offs,vsect->name,attr,flags,align,vsect->dsize,vsect->fsize);
  if (nrelocs)
    printf("%d Relocation%s present.\n",nrelocs,nrelocs==1?emptystr:sstr);

  p += vsect->fsize;  /* skip section contents */

  /* read and print relocations for this section */
  for (i=0; i<nrelocs; i++) {
    ubyte type;

    if (i == 0) {
      /* print header */
      printf("\nfile offs sectoffs pos sz mask     type     symbol+addend\n");
    }
    printf("%08lx: ",(unsigned long)(p-vobj));
    type = (ubyte)read_number();

    if (type<sizeof(reloc_name)/sizeof(reloc_name[0])) {
      /* standard relocation */
      taddr offs,mask,addend;
      int bpos,bsiz,sym;

      offs = read_number();
      bpos = (int)read_number();
      bsiz = (int)read_number();
      mask = read_number();
      addend = read_number();
      sym = (int)read_number() - 1;  /* symbol index */

      if (offs<0 || offs>=vsect->dsize) {
        printf("offset 0x%lx is outside of section!\n",(unsigned long)offs);
        continue;
      }
      if (sym<0 || sym>=nsyms) {
        printf("symbol index %d is illegal!\n",sym+1);
        continue;
      }
      if (bsiz<0 || bsiz>(sizeof(taddr)<<3)) {
        printf("size of %d bits is illegal!\n",bsiz);
        continue;
      }
      if (bpos<0 || bpos>=bsiz) {
        printf("bit-position %d is outside of a %d bit word!\n",bpos,bsiz);
        continue;
      }

      printf("%08lx  %02d %02d %08lx %-8s %s%+lld\n",
             (unsigned long)offs,bpos,bsiz,(unsigned long)mask,
             reloc_name[type],vsym[sym].name,addend);
    }
    else {
      /* non-standard relocation */
      taddr rsize;

      rsize = read_number();  /* size of special relocation entry */
      p += rsize;
      printf("special relocation type %-3d with a size of %d bytes\n",
             (int)type,(int)rsize);
    }
  }
}


static const char *bind_name(int flags)
{
  if (flags & WEAK)
    return "WEAK";
  if (flags & EXPORT)
    return "GLOB";
  if (flags & COMMON)
    return "COMM";
  return "LOCL";
}


static const char *def_name(struct vobj_symbol *vs,
                            struct vobj_section *sec,int nsecs)
{
  switch (vs->type) {
    case EXPRESSION:
      return "*ABS*";
    case IMPORT:
      return "*UND*";
    case LABSYM:
      if (vs->sec>0 && vs->sec<=nsecs)
        return sec[vs->sec-1].name;
  }
  return "???";
}


static int vobjdump(void)
{
  p = vobj;

  if (vlen>4 && p[0]==0x56 && p[1]==0x4f && p[2]==0x42 && p[3]==0x4a) {
    int endian,bpb,bpt,nsecs,nsyms,i;
    const char *cpu_name;
    struct vobj_symbol *vsymbols = NULL;
    struct vobj_section *vsect = NULL;

    p += 4;	/* skip ID */
    endian = (int)*p++;  /* endianess */
    if (endian<1 || endian>2) {
      fprintf(stderr,"Wrong endianess: %d\n",endian);
      return 1;
    }

    bpb = (int)read_number();  /* bits per byte */
    if (bpb != 8) {
      fprintf(stderr,"%d bits per byte not supported!\n",bpb);
      return 1;
    }

    bpt = (int)read_number();  /* bytes per taddr */
    if (bpt > sizeof(taddr)) {
      fprintf(stderr,"%d bytes per taddr not supported!\n",bpt);
      return 1;
    }

    cpu_name = p;
    skip_string();  /* skip cpu-string */
    nsecs = (int)read_number();  /* number of sections */
    nsyms = (int)read_number();  /* number of symbols */

    /* print header */
    print_sep();
    printf("VOBJ %s (%s endian), %d bits per byte, %d bytes per word.\n"
           "%d symbol%s.\n%d section%s.\n",
           cpu_name,endian_name[endian-1],bpb,bpt,
           nsyms,nsyms==1?emptystr:sstr,nsecs,nsecs==1?emptystr:sstr);

    /* read symbols */
    if (nsyms) {
      if (vsymbols = malloc(nsyms * sizeof(struct vobj_symbol))) {
        for (i=0; i<nsyms; i++)
          read_symbol(&vsymbols[i]);
      }
      else {
        fprintf(stderr,"Cannot allocate %ld bytes for symbols!\n",
                (long)(nsyms * sizeof(struct vobj_symbol)));
        return 1;
      }
    }

    /* read and print sections */
    if (vsect = malloc(nsecs * sizeof(struct vobj_section))) {
      for (i=0; i<nsecs; i++)
        read_section(&vsect[i],vsymbols,nsyms);
    }
    else {
      fprintf(stderr,"Cannot allocate %ld bytes for sections!\n",
              (long)(nsecs * sizeof(struct vobj_section)));
      return 1;
    }

    /* print symbols */
    for (i=0; i<nsyms; i++) {
      struct vobj_symbol *vs = &vsymbols[i];

      if (i == 0) {
        printf("\n");
        print_sep();
        printf("SYMBOL TABLE\n"
               "file offs bind size     type def      value    name\n");
      }
      printf("%08lx: %-4s %08x %-4s %8.8s %08lx %s\n",
             (unsigned long)vs->offs,bind_name(vs->flags),(unsigned)vs->size,
             type_name[TYPE(vs)],def_name(vs,vsect,nsecs),
             (unsigned long)vs->val,vs->name);
    }
  }
  else {
    fprintf(stderr,"Not a VOBJ file!\n");
    return 1;
  }

  return 0;
}


static size_t filesize(FILE *fp,const char *name)
{
  long oldpos,size;

  if ((oldpos = ftell(fp)) >= 0)
    if (fseek(fp,0,SEEK_END) >= 0)
      if ((size = ftell(fp)) >= 0)
        if (fseek(fp,oldpos,SEEK_SET) >= 0)
          return (size_t)size;
  fprintf(stderr,"Cannot determine size of file \"%s\"!\n",name);
  return 0;
}


int main(int argc,char *argv[])
{
  int rc = 1;

  if (argc == 2) {
    FILE *f;

    if (f = fopen(argv[1],"rb")) {
      if (vlen = filesize(f,argv[1])) {
        if (vobj = malloc(vlen)) {
          if (fread(vobj,1,vlen,f) == vlen)
            rc = vobjdump();
          else
            fprintf(stderr,"Read error on \"%s\"!\n",argv[1]);
          free(vobj);
        }
        else
          fprintf(stderr,"Unable to allocate %lu bytes "
                  "to buffer file \"%s\"!\n",vlen,argv[1]);
      }
      fclose(f);
    }    
    else
      fprintf(stderr,"Cannot open \"%s\" for reading!\n",argv[1]);
  }
  else
    fprintf(stderr,"vobjdump V0.1\nWritten by Frank Wille\n"
            "Usage: %s <file name>\n",argv[0]);

  return rc;
}
