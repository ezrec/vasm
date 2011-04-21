/*
** cpu.h PowerPC cpu-description header-file
** (c) in 2002,2006 by Frank Wille
*/

extern int ppc_endianess;
#define BIGENDIAN (ppc_endianess)
#define LITTLEENDIAN (!ppc_endianess)
#define VASM_CPU_PPC 1

int ppc_data_align(int);
int ppc_data_operand(int);

/* maximum number of operands for one mnemonic */
#define MAX_OPERANDS 5

/* allowed to call parse_operand() with an arbitrary number of operands */
#define CPU_CHECKS_OPCNT 1

/* maximum number of mnemonic-qualifiers per mnemonic */
#define MAX_QUALIFIERS 0

/* maximum number of additional command-line-flags for this cpu */

/* data type to represent a target-address */
typedef long long taddr;

/* minimum instruction alignment */
#define INST_ALIGN 4

/* default alignment for n-bit data */
#define DATA_ALIGN(n) ppc_data_align(n)

/* operand class for n-bit data definitions */
#define DATA_OPERAND(n) ppc_data_operand(n)

/* special data operand types: */
#define OP_D8  0x1001
#define OP_D16 0x1002
#define OP_D32 0x1003
#define OP_D64 0x1004

/* PPC specific relocations */
#define REL_PPCEABI_SDA2 (LAST_STANDARD_RELOC+1)
#define REL_PPCEABI_SDA21 (LAST_STANDARD_RELOC+2)
#define REL_PPCEABI_SDAI16 (LAST_STANDARD_RELOC+3)
#define REL_PPCEABI_SDA2I16 (LAST_STANDARD_RELOC+4)
#define REL_MORPHOS_DREL (LAST_STANDARD_RELOC+5)
#define REL_AMIGAOS_BREL (LAST_STANDARD_RELOC+6)
#define LAST_PPC_RELOC (LAST_STANDARD_RELOC+6)


/* type to store each operand */
typedef struct {
  short type;
  unsigned char attr;   /* reloc attribute != REL_NONE when present */
  unsigned char mode;   /* @l/h/ha */
  expr *value;
  expr *basereg;  /* only for d(Rn) load/store addressing mode */
} operand;

/* operand modifier */
#define OPM_NONE 0
#define OPM_LO 1  /* low 16 bits */
#define OPM_HI 2  /* high 16 bits */
#define OPM_HA 3  /* high 16 bits with addi compensation */


/* additional mnemonic data */
typedef struct {
  unsigned long available;
  unsigned long opcode;
} mnemonic_extension;


/* Values defined for the 'available' field of mnemonic_extension.  */
#define PPC_OPCODE_PPC          (1)
#define PPC_OPCODE_POWER        (2)
#define PPC_OPCODE_POWER2       (4)
#define PPC_OPCODE_32           (8)
#define PPC_OPCODE_64           (0x10)
#define PPC_OPCODE_601          (0x20)
#define PPC_OPCODE_COMMON       (0x40)
#define PPC_OPCODE_ANY          (0x80)
#define PPC_OPCODE_64_BRIDGE    (0x100)
#define PPC_OPCODE_ALTIVEC      (0x200)

/* Shortcuts for known PPC models */
#undef  PPC
#define PPC     PPC_OPCODE_PPC | PPC_OPCODE_ANY
#define PPCCOM  PPC_OPCODE_PPC | PPC_OPCODE_COMMON | PPC_OPCODE_ANY
#define PPC32   PPC_OPCODE_PPC | PPC_OPCODE_32 | PPC_OPCODE_ANY
#define PPC64   PPC_OPCODE_PPC | PPC_OPCODE_64 | PPC_OPCODE_ANY
#define PPCONLY PPC_OPCODE_PPC
#define PPC403  PPC
#define PPC405  PPC403
#define PPC750  PPC
#define PPC860  PPC
#define PPCVEC  PPC_OPCODE_ALTIVEC | PPC_OPCODE_ANY
#define POWER   PPC_OPCODE_POWER | PPC_OPCODE_ANY
#define POWER2  PPC_OPCODE_POWER | PPC_OPCODE_POWER2 | PPC_OPCODE_ANY
#define PPCPWR2 PPC_OPCODE_PPC | PPC_OPCODE_POWER | PPC_OPCODE_POWER2 | PPC_OPCODE_ANY
#define POWER32 PPC_OPCODE_POWER | PPC_OPCODE_ANY | PPC_OPCODE_32
#define COM     PPC_OPCODE_POWER | PPC_OPCODE_PPC | PPC_OPCODE_COMMON | PPC_OPCODE_ANY
#define COM32   PPC_OPCODE_POWER | PPC_OPCODE_PPC | PPC_OPCODE_COMMON | PPC_OPCODE_ANY | PPC_OPCODE_32
#define M601    PPC_OPCODE_POWER | PPC_OPCODE_601 | PPC_OPCODE_ANY
#define PWRCOM  PPC_OPCODE_POWER | PPC_OPCODE_601 | PPC_OPCODE_COMMON | PPC_OPCODE_ANY
#define MFDEC1  PPC_OPCODE_POWER
#define MFDEC2  PPC_OPCODE_PPC | PPC_OPCODE_601


struct powerpc_operand
{
  int bits;
  int shift;
  unsigned long (*insert)(unsigned long instruction, long op,
                          const char **errmsg);
  unsigned long flags;
};

/* powerpc_operand flags */
#define PPC_OPERAND_SIGNED   (1)        /* signed values */
#define PPC_OPERAND_SIGNOPT  (2)        /* signed values up to 0xffff */
#define PPC_OPERAND_FAKE     (4)        /* just reuse last read operand */
#define PPC_OPERAND_PARENS   (8)        /* operand is in parentheses */
#define PPC_OPERAND_CR       (0x10)     /* CR field */
#define PPC_OPERAND_GPR      (0x20)     /* GPR field */
#define PPC_OPERAND_FPR      (0x40)     /* FPR field */
#define PPC_OPERAND_RELATIVE (0x80)     /* relative branch displacement */
#define PPC_OPERAND_ABSOLUTE (0x100)    /* absolute branch address */
#define PPC_OPERAND_OPTIONAL (0x200)    /* optional, zero if omitted */
#define PPC_OPERAND_NEXT     (0x400)    /* hack for rotate instructions */
#define PPC_OPERAND_NEGATIVE (0x800)    /* range check on negative value */
#define PPC_OPERAND_VR       (0x1000)   /* Altivec register field */


/* The BO encodings used in extended conditional branch mnemonics.  */
#define BODNZF  (0x0)
#define BODNZFP (0x1)
#define BODZF   (0x2)
#define BODZFP  (0x3)
#define BOF     (0x4)
#define BOFP    (0x5)
#define BODNZT  (0x8)
#define BODNZTP (0x9)
#define BODZT   (0xa)
#define BODZTP  (0xb)
#define BOT     (0xc)
#define BOTP    (0xd)
#define BODNZ   (0x10)
#define BODNZP  (0x11)
#define BODZ    (0x12)
#define BODZP   (0x13)
#define BOU     (0x14)

/* The BI condition bit encodings used in extended conditional branch
   mnemonics.  */
#define CBLT    (0)
#define CBGT    (1)
#define CBEQ    (2)
#define CBSO    (3)

/* The TO encodings used in extended trap mnemonics.  */
#define TOLGT   (0x1)
#define TOLLT   (0x2)
#define TOEQ    (0x4)
#define TOLGE   (0x5)
#define TOLNL   (0x5)
#define TOLLE   (0x6)
#define TOLNG   (0x6)
#define TOGT    (0x8)
#define TOGE    (0xc)
#define TONL    (0xc)
#define TOLT    (0x10)
#define TOLE    (0x14)
#define TONG    (0x14)
#define TONE    (0x18)
#define TOU     (0x1f)
