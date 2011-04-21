/* The functions used to insert complicated operands.  */

static unsigned long insert_bat(unsigned long insn,long value,
                                const char **errmsg)
{
  return insn | (((insn >> 21) & 0x1f) << 16);
}

static unsigned long insert_bba(unsigned long insn, long value,
                                const char **errmsg)
{
  return insn | (((insn >> 16) & 0x1f) << 11);
}

static unsigned long insert_bd(unsigned long insn,long value,
                                const char **errmsg)
{
  return insn | (value & 0xfffc);
}

static unsigned long insert_bdm(unsigned long insn,long value,
                                const char **errmsg)
{
  if ((value & 0x8000) != 0)
    insn |= 1 << 21;
  return insn | (value & 0xfffc);
}

static unsigned long insert_bdp(unsigned long insn,long value,
                                const char **errmsg)
{
  if ((value & 0x8000) == 0)
    insn |= 1 << 21;
  return insn | (value & 0xfffc);
}

static int valid_bo(long value)
{
  /* Certain encodings have bits that are required to be zero.  These
     are (z must be zero, y may be anything):
         001zy
         011zy
         1z00y
         1z01y
         1z1zz
     */
  switch (value & 0x14) {
    default:
    case 0:
      return 1;
    case 0x4:
      return (value & 0x2) == 0;
    case 0x10:
      return (value & 0x8) == 0;
    case 0x14:
      return value == 0x14;
  }
}

static unsigned long insert_bo(unsigned long insn,long value,
                               const char **errmsg)
{
  if (! valid_bo (value))
    *errmsg = "invalid conditional option";
  return insn | ((value & 0x1f) << 21);
}

static unsigned long insert_boe(unsigned long insn, long value,
                                const char **errmsg)
{
  if (! valid_bo (value))
    *errmsg = "invalid conditional option";
  else if ((value & 1) != 0)
    *errmsg = "attempt to set y bit when using + or - modifier";
  return insn | ((value & 0x1f) << 21);
}

static unsigned long insert_ds(unsigned long insn, long value,
                               const char **errmsg)
{
  return insn | (value & 0xfffc);
}

static unsigned long insert_li(unsigned long insn, long value,
                               const char **errmsg)
{
  if ((value & 3) != 0)
    *errmsg = "ignoring least significant bits in branch offset";
  return insn | (value & 0x3fffffc);
}

static unsigned long insert_mbe(unsigned long insn, long value,
                                const char **errmsg)
{
  unsigned long uval, mask;
  int mb, me, mx, count, last;

  uval = value;

  if (uval == 0) {
      *errmsg = "illegal bitmask";
      return insn;
  }

  mb = 0;
  me = 32;
  if ((uval & 1) != 0)
    last = 1;
  else
    last = 0;
  count = 0;

  /* mb: location of last 0->1 transition */
  /* me: location of last 1->0 transition */
  /* count: # transitions */

  for (mx = 0, mask = (long) 1 << 31; mx < 32; ++mx, mask >>= 1) {
    if ((uval & mask) && !last) {
      ++count;
      mb = mx;
      last = 1;
    }
    else if (!(uval & mask) && last) {
      ++count;
      me = mx;
      last = 0;
    }
  }
  if (me == 0)
    me = 32;

  if (count != 2 && (count != 0 || ! last)) {
    *errmsg = "illegal bitmask";
  }

  return insn | (mb << 6) | ((me - 1) << 1);
}

static unsigned long insert_mb6(unsigned long insn, long value,
                                const char **errmsg)
{
  return insn | ((value & 0x1f) << 6) | (value & 0x20);
}

static unsigned long insert_nb(unsigned long insn, long value,
                               const char **errmsg)
{
  if (value < 0 || value > 32)
    *errmsg = "value out of range";
  if (value == 32)
    value = 0;
  return insn | ((value & 0x1f) << 11);
}

static unsigned long insert_nsi(unsigned long insn, long value,
                                const char **errmsg)
{
  return insn | ((- value) & 0xffff);
}

static unsigned long insert_ral(unsigned long insn, long value,
                                const char **errmsg)
{
  if (value == 0
      || (unsigned long) value == ((insn >> 21) & 0x1f))
    *errmsg = "invalid register operand when updating";
  return insn | ((value & 0x1f) << 16);
}

static unsigned long insert_ram(unsigned long insn, long value,
                                const char **errmsg)
{
  if ((unsigned long) value >= ((insn >> 21) & 0x1f))
    *errmsg = "index register in load range";
  return insn | ((value & 0x1f) << 16);
}

static unsigned long insert_ras(unsigned long insn, long value,
                                const char **errmsg)
{
  if (value == 0)
    *errmsg = "invalid register operand when updating";
  return insn | ((value & 0x1f) << 16);
}

static unsigned long insert_rbs(unsigned long insn, long value,
                                const char **errmsg)
{
  return insn | (((insn >> 21) & 0x1f) << 11);
}

static unsigned long insert_sh6(unsigned long insn, long value,
                                const char **errmsg)
{
  return insn | ((value & 0x1f) << 11) | ((value & 0x20) >> 4);
}

static unsigned long insert_spr(unsigned long insn, long value,
                                const char **errmsg)
{
  return insn | ((value & 0x1f) << 16) | ((value & 0x3e0) << 6);
}

static unsigned long insert_tbr(unsigned long insn, long value,
                                const char **errmsg)
{
  if (value == 0)
    value = 268;
  return insn | ((value & 0x1f) << 16) | ((value & 0x3e0) << 6);
}

static unsigned long insert_slwi(unsigned long insn, long value,
                                 const char **errmsg)
{
  return insn | ((value&0x1f)<<11) | ((31-(value&0x1f))<<1);
}

static unsigned long insert_srwi(unsigned long insn, long value,
                                 const char **errmsg)
{
  return insn | (((32-value)&0x1f)<<11) | ((value&0x1f)<<6) | (31<<1);
}

static unsigned long insert_extlwi(unsigned long insn, long value,
                                   const char **errmsg)
{
  if (value<1 || value>32)
    *errmsg = "value out of range (1-32)";
  return insn | (((value-1)&0x1f)<<1);
}

static unsigned long insert_extrwi(unsigned long insn, long value,
                                   const char **errmsg)
{
  if (value<1 || value>32)
    *errmsg = "value out of range (1-32)";
  return insn | ((value&0x1f)<<11) | (((32-value)&0x1f)<<6) | (31<<1);
}

static unsigned long insert_extwib(unsigned long insn, long value,
                                   const char **errmsg)
{
  value += (insn>>11) & 0x1f;
  if (value > 32)
    *errmsg = "sum of last two operands out of range (0-32)";
  return (insn&~0xf800) | ((value&0x1f)<<11);
}

static unsigned long insert_inslwi(unsigned long insn, long value,
                                   const char **errmsg)
{
  long n = ((insn>>1) & 0x1f) + 1;
  if (value+n > 32)
    *errmsg = "sum of last two operands out of range (1-32)";
  return (insn&~0xfffe) | (((32-value)&0x1f)<<11) | ((value&0x1f)<<6)
                        | ((((value+n)-1)&0x1f)<<1);
}

static unsigned long insert_insrwi(unsigned long insn, long value,
                                   const char **errmsg)
{
  long n = ((insn>>1) & 0x1f) + 1;
  if (value+n > 32)
    *errmsg = "sum of last two operands out of range (1-32)";
  return (insn&~0xfffe) | (((32-(value+n))&0x1f)<<11) | ((value&0x1f)<<6)
                        | ((((value+n)-1)&0x1f)<<1);
}

static unsigned long insert_rotrwi(unsigned long insn, long value,
                                   const char **errmsg)
{
  return insn | (((32-value)&0x1f)<<11);
}

static unsigned long insert_clrrwi(unsigned long insn, long value,
                                   const char **errmsg)
{
  return insn | (((31-value)&0x1f)<<1);
}

static unsigned long insert_clrlslwi(unsigned long insn, long value,
                                     const char **errmsg)
{
  long b = (insn>>6) & 0x1f;
  if (value > b)
    *errmsg = "n (4th oper) must be less or equal to b (3rd oper)";
  return (insn&~0x7c0) | ((value&0x1f)<<11) | (((b-value)&0x1f)<<6)
                       | (((31-value)&0x1f)<<1);
}


/* The operands table.
   The fields are bits, shift, insert, flags. */

enum {
  UNUSED,BA,BAT,BB,BBA,BD,BDA,BDM,BDMA,BDP,BDPA,BF,OBF,BFA,BI,BO,BOE,
  BT,CR,D,DS,E,FL1,FL2,FLM,FRA,FRB,FRC,FRS,FXM,L,LEV,LI,LIA,MB,ME,
  MBE,MBE_,MB6,NB,NSI,RA,RAL,RAM,RAS,RB,RBS,RS,SH,SH6,SI,SISIGNOPT,
  SPR,SPRBAT,SPRG,SR,SV,TBR,TO,U,UI,VA,VB,VC,VD,SIMM,UIMM,SHB,
  SLWI,SRWI,EXTLWI,EXTRWI,EXTWIB,INSLWI,INSRWI,ROTRWI,CLRRWI,CLRLSL,
  STRM,AT
};

#define FRT FRS
#define ME6 MB6
#define RT RS
#define VS VD


const struct powerpc_operand powerpc_operands[] =
{
  /* UNUSED: The zero index is used to indicate the end of the list of
     operands.  */
  { 0, 0, 0, 0 },

  /* The BA field in an XL form instruction.  */
  { 5, 16, 0, PPC_OPERAND_CR },

  /* BAT: The BA field in an XL form instruction when it must be the same
     as the BT field in the same instruction.  */
  { 5, 16, insert_bat, PPC_OPERAND_FAKE },

  /* The BB field in an XL form instruction.  */
  { 5, 11, 0, PPC_OPERAND_CR },

  /* BBA: The BB field in an XL form instruction when it must be the same
     as the BA field in the same instruction.  */
  { 5, 11, insert_bba, PPC_OPERAND_FAKE },

  /* The BD field in a B form instruction.  The lower two bits are
     forced to zero.  */
  { 16, 0, insert_bd, PPC_OPERAND_RELATIVE | PPC_OPERAND_SIGNED },

  /* BDA: The BD field in a B form instruction when absolute addressing is
     used.  */
  { 16, 0, insert_bd, PPC_OPERAND_ABSOLUTE | PPC_OPERAND_SIGNED },

  /* BDM: The BD field in a B form instruction when the - modifier is used.
     This sets the y bit of the BO field appropriately.  */
  { 16, 0, insert_bdm,
      PPC_OPERAND_RELATIVE | PPC_OPERAND_SIGNED },

  /* BDMA: The BD field in a B form instruction when the - modifier is used
     and absolute address is used.  */
  { 16, 0, insert_bdm,
      PPC_OPERAND_ABSOLUTE | PPC_OPERAND_SIGNED },

  /* BDP: The BD field in a B form instruction when the + modifier is used.
     This sets the y bit of the BO field appropriately.  */
  { 16, 0, insert_bdp,
      PPC_OPERAND_RELATIVE | PPC_OPERAND_SIGNED },

  /* BDPA: The BD field in a B form instruction when the + modifier is used
     and absolute addressing is used.  */
  { 16, 0, insert_bdp,
      PPC_OPERAND_ABSOLUTE | PPC_OPERAND_SIGNED },

  /* The BF field in an X or XL form instruction.  */
  { 3, 23, 0, PPC_OPERAND_CR },

  /* OBF: An optional BF field.  This is used for comparison instructions,
     in which an omitted BF field is taken as zero.  */
  { 3, 23, 0, PPC_OPERAND_CR | PPC_OPERAND_OPTIONAL },

  /* The BFA field in an X or XL form instruction.  */
  { 3, 18, 0, PPC_OPERAND_CR },

  /* The BI field in a B form or XL form instruction.  */
  { 5, 16, 0, PPC_OPERAND_CR },

  /* The BO field in a B form instruction.  Certain values are
     illegal.  */
  { 5, 21, insert_bo, 0 },

  /* BOE: The BO field in a B form instruction when the + or - modifier is
     used.  This is like the BO field, but it must be even.  */
  { 5, 21, insert_boe, 0 },

  /* The BT field in an X or XL form instruction.  */
  { 5, 21, 0, PPC_OPERAND_CR },

  /* CR: The condition register number portion of the BI field in a B form
     or XL form instruction.  This is used for the extended
     conditional branch mnemonics, which set the lower two bits of the
     BI field.  This field is optional.  */
  { 3, 18, 0, PPC_OPERAND_CR | PPC_OPERAND_OPTIONAL },

  /* The D field in a D form instruction.  This is a displacement off
     a register, and implies that the next operand is a register in
     parentheses.  */
  { 16, 0, 0, PPC_OPERAND_PARENS | PPC_OPERAND_SIGNED },

  /* The DS field in a DS form instruction.  This is like D, but the
     lower two bits are forced to zero.  */
  { 16, 0, insert_ds, PPC_OPERAND_PARENS | PPC_OPERAND_SIGNED },

  /* The E field in a wrteei instruction.  */
  { 1, 15, 0, 0 },

  /* The FL1 field in a POWER SC form instruction.  */
  { 4, 12, 0, 0 },

  /* The FL2 field in a POWER SC form instruction.  */
  { 3, 2, 0, 0 },

  /* The FLM field in an XFL form instruction.  */
  { 8, 17, 0, 0 },

  /* The FRA field in an X or A form instruction.  */
  { 5, 16, 0, PPC_OPERAND_FPR },

  /* The FRB field in an X or A form instruction.  */
  { 5, 11, 0, PPC_OPERAND_FPR },

  /* The FRC field in an A form instruction.  */
  { 5, 6, 0, PPC_OPERAND_FPR },

  /* The FRS field in an X form instruction or the FRT field in a D, X
     or A form instruction.  */
  { 5, 21, 0, PPC_OPERAND_FPR },

  /* The FXM field in an XFX instruction.  */
  { 8, 12, 0, 0 },

  /* The L field in a D or X form instruction.  */
  { 1, 21, 0, PPC_OPERAND_OPTIONAL },

  /* The LEV field in a POWER SC form instruction.  */
  { 7, 5, 0, 0 },

  /* The LI field in an I form instruction.  The lower two bits are
     forced to zero.  */
  { 26, 0, insert_li, PPC_OPERAND_RELATIVE | PPC_OPERAND_SIGNED },

  /* The LI field in an I form instruction when used as an absolute
     address.  */
  { 26, 0, insert_li, PPC_OPERAND_ABSOLUTE | PPC_OPERAND_SIGNED },

  /* The MB field in an M form instruction.  */
  { 5, 6, 0, 0 },

  /* The ME field in an M form instruction.  */
  { 5, 1, 0, 0 },

  /* MBE: The MB and ME fields in an M form instruction expressed a single
     operand which is a bitmask indicating which bits to select.  This
     is a two operand form using PPC_OPERAND_NEXT.  See the
     description in opcode/ppc.h for what this means.  */
  { 5, 6, 0, PPC_OPERAND_OPTIONAL | PPC_OPERAND_NEXT },
  { 31, 1, insert_mbe, 0 },

  /* MB6: The MB or ME field in an MD or MDS form instruction.  The high
     bit is wrapped to the low end.  */
  { 6, 5, insert_mb6, 0 },

  /* The NB field in an X form instruction.  The value 32 is stored as
     0.  */
  { 6, 11, insert_nb, 0 },

  /* The NSI field in a D form instruction.  This is the same as the
     SI field, only negated.  */
  { 16, 0, insert_nsi,
      PPC_OPERAND_NEGATIVE | PPC_OPERAND_SIGNED },

  /* The RA field in an D, DS, X, XO, M, or MDS form instruction.  */
  { 5, 16, 0, PPC_OPERAND_GPR },

  /* RAL: The RA field in a D or X form instruction which is an updating
     load, which means that the RA field may not be zero and may not
     equal the RT field.  */
  { 5, 16, insert_ral, PPC_OPERAND_GPR },

  /* RAM: The RA field in an lmw instruction, which has special value
     restrictions.  */
  { 5, 16, insert_ram, PPC_OPERAND_GPR },

  /* RAS: The RA field in a D or X form instruction which is an updating
     store or an updating floating point load, which means that the RA
     field may not be zero.  */
  { 5, 16, insert_ras, PPC_OPERAND_GPR },

  /* The RB field in an X, XO, M, or MDS form instruction.  */
  { 5, 11, 0, PPC_OPERAND_GPR },

  /* RBS: The RB field in an X form instruction when it must be the same as
     the RS field in the instruction.  This is used for extended
     mnemonics like mr.  */
  { 5, 1, insert_rbs, PPC_OPERAND_FAKE },

  /* The RS field in a D, DS, X, XFX, XS, M, MD or MDS form
     instruction or the RT field in a D, DS, X, XFX or XO form
     instruction.  */
  { 5, 21, 0, PPC_OPERAND_GPR },

  /* The SH field in an X or M form instruction.  */
  { 5, 11, 0, 0 },

  /* SH6: The SH field in an MD form instruction.  This is split.  */
  { 6, 1, insert_sh6, 0 },

  /* The SI field in a D form instruction.  */
  { 16, 0, 0, PPC_OPERAND_SIGNED },

  /* SISIGNOPT: The SI field in a D form instruction when we accept a 
     wide range of positive values.  */
  { 16, 0, 0, PPC_OPERAND_SIGNED | PPC_OPERAND_SIGNOPT },

  /* The SPR field in an XFX form instruction.  This is flipped--the
     lower 5 bits are stored in the upper 5 and vice- versa.  */
  { 10, 11, insert_spr, 0 },

  /* SPRBAT: The BAT index number in an XFX form m[ft]ibat[lu] instruction. */
  { 2, 17, 0, 0 },

  /* The SPRG register number in an XFX form m[ft]sprg instruction.  */
  { 2, 16, 0, 0 },

  /* The SR field in an X form instruction.  */
  { 4, 16, 0, 0 },

  /* The SV field in a POWER SC form instruction.  */
  { 14, 2, 0, 0 },

  /* The TBR field in an XFX form instruction.  This is like the SPR
     field, but it is optional.  */
  { 10, 11, insert_tbr, PPC_OPERAND_OPTIONAL },

  /* The TO field in a D or X form instruction.  */
  { 5, 21, 0, 0 },

  /* The U field in an X form instruction.  */
  { 4, 12, 0, 0 },

  /* The UI field in a D form instruction.  */
  { 16, 0, 0, 0 },

  /* The VA field in a VA, VX or VXR form instruction. */
  { 5, 16, 0, PPC_OPERAND_VR },

  /* The VB field in a VA, VX or VXR form instruction. */
  { 5, 11, 0, PPC_OPERAND_VR }, 

  /* The VC field in a VA form instruction. */
  { 5, 6, 0, PPC_OPERAND_VR },

  /* The VD or VS field in a VA, VX, VXR or X form instruction. */
  { 5, 21, 0, PPC_OPERAND_VR },

  /* The SIMM field in a VX form instruction. */
  { 5, 16, 0, PPC_OPERAND_SIGNED},

  /* The UIMM field in a VX form instruction. */
  { 5, 16, 0, 0 },

  /* The SHB field in a VA form instruction. */
  { 4, 6, 0, 0 },

  /* SLWI: SH field in slwi. */
  { 5, 11, insert_slwi, 0 },

  /* SRWI: SH field in srwi. */
  { 5, 11, insert_srwi, 0 },

  /* EXTLWI: n field in extlwi. 32 bits faked to disable range checks. */
  { 31, 1, insert_extlwi, 0 },

  /* EXTRWI: n field in extrwi. 32 bits faked to disable range checks. */
  { 31, 1, insert_extrwi, 0 },

  /* EXTWIB: b field in extlwi/extrwi. Value is added to SH field. */
  { 5, 11, insert_extwib, 0 },

  /* INSLWI: b field in inslwi. Value is filled into SH, MB and ME
     using previous n-1 from ME */
  { 5, 11, insert_inslwi, 0 },

  /* INSRWI: b field in inslwi. Value is filled into SH, MB and ME
     using previous n-1 from ME */
  { 5, 11, insert_insrwi, 0 },

  /* ROTRWI: SH field in rotrwi */
  { 5, 11, insert_rotrwi, 0 },

  /* CLRRWI: last operand (n) in clrrwi */
  { 5, 1, insert_clrrwi, 0 },

  /* CLRLSL: last operand (n) in clrlslwi */
  { 5, 11, insert_clrlslwi, 0 },

  /* The STRM field in a dss/dst/dstst instruction.  */
  { 2, 21, 0, 0 },

  /* AT: The A/T field in a dss/dst/dstst instruction.  */
  { 1, 25, 0, PPC_OPERAND_OPTIONAL },
};


/* Macros used to form opcodes.  */

#define OP(x) ((((unsigned long)(x)) & 0x3f) << 26)
#define OPTO(x,to) (OP (x) | ((((unsigned long)(to)) & 0x1f) << 21))
#define OPL(x,l) (OP (x) | ((((unsigned long)(l)) & 1) << 21))
#define A(op, xop, rc) (OP (op) | ((((unsigned long)(xop)) & 0x1f) << 1) | (((unsigned long)(rc)) & 1))
#define B(op, aa, lk) (OP (op) | ((((unsigned long)(aa)) & 1) << 1) | ((lk) & 1))
#define BBO(op, bo, aa, lk) (B ((op), (aa), (lk)) | ((((unsigned long)(bo)) & 0x1f) << 21))
#define BBOCB(op, bo, cb, aa, lk) \
  (BBO ((op), (bo), (aa), (lk)) | ((((unsigned long)(cb)) & 0x3) << 16))
#define DSO(op, xop) (OP (op) | ((xop) & 0x3))
#define M(op, rc) (OP (op) | ((rc) & 1))
#define MME(op, me, rc) (M ((op), (rc)) | ((((unsigned long)(me)) & 0x1f) << 1))
#define MD(op, xop, rc) (OP (op) | ((((unsigned long)(xop)) & 0x7) << 2) | ((rc) & 1))
#define MDS(op, xop, rc) (OP (op) | ((((unsigned long)(xop)) & 0xf) << 1) | ((rc) & 1))
#define SC(op, sa, lk) (OP (op) | ((((unsigned long)(sa)) & 1) << 1) | ((lk) & 1))
#define VX(op, xop) (OP (op) | (((unsigned long)(xop)) & 0x7ff))
#define VXA(op, xop) (OP (op) | (((unsigned long)(xop)) & 0x07f))
#define VXR(op, xop, rc) (OP (op) | (((rc) & 1) << 10) | (((unsigned long)(xop)) & 0x3ff))
#define X(op, xop) (OP (op) | ((((unsigned long)(xop)) & 0x3ff) << 1))
#define XRC(op, xop, rc) (X ((op), (xop)) | ((rc) & 1))
#define XCMPL(op, xop, l) (X ((op), (xop)) | ((((unsigned long)(l)) & 1) << 21))
#define XTO(op, xop, to) (X ((op), (xop)) | ((((unsigned long)(to)) & 0x1f) << 21))
#define XTLB(op, xop, sh) (X ((op), (xop)) | ((((unsigned long)(sh)) & 0x1f) << 11))
#define XFL(op, xop, rc) (OP (op) | ((((unsigned long)(xop)) & 0x3ff) << 1) | (((unsigned long)(rc)) & 1))
#define XL(op, xop) (OP (op) | ((((unsigned long)(xop)) & 0x3ff) << 1))
#define XLLK(op, xop, lk) (XL ((op), (xop)) | ((lk) & 1))
#define XLO(op, bo, xop, lk) \
  (XLLK ((op), (xop), (lk)) | ((((unsigned long)(bo)) & 0x1f) << 21))
#define XLYLK(op, xop, y, lk) (XLLK ((op), (xop), (lk)) | ((((unsigned long)(y)) & 1) << 21))
#define XLOCB(op, bo, cb, xop, lk) \
  (XLO ((op), (bo), (xop), (lk)) | ((((unsigned long)(cb)) & 3) << 16))
#define XO(op, xop, oe, rc) \
  (OP (op) | ((((unsigned long)(xop)) & 0x1ff) << 1) | ((((unsigned long)(oe)) & 1) << 10) | (((unsigned long)(rc)) & 1))
#define XS(op, xop, rc) (OP (op) | ((((unsigned long)(xop)) & 0x1ff) << 2) | (((unsigned long)(rc)) & 1))
#define XFXM(op, xop, fxm) \
  (X ((op), (xop)) | ((((unsigned long)(fxm)) & 0xff) << 12))
#define XSPR(op, xop, spr) \
  (X ((op), (xop)) | ((((unsigned long)(spr)) & 0x1f) << 16) | ((((unsigned long)(spr)) & 0x3e0) << 6))
#define XDS(op, xop, at) \
  (X ((op), (xop)) | ((((unsigned long)(at)) & 1) << 25))
