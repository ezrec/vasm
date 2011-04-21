/* output_hunk.h header file for AmigaOS hunk format */
/* (c) in 2002-2009 by Frank Wille */

/* hunk-format relocs */
struct hunkreloc {
  struct node n;
  unsigned long hunk_id;
  int hunk_index;
  unsigned long hunk_offset;
};

/* hunk-format external reference */
struct hunkxref {
  struct node n;
  char *name;
  unsigned long type;
  unsigned long size;
  unsigned long offset;
};

/* line debug hunk */
struct hunkline {
  struct node n;
  unsigned long line;
  unsigned long offset;
};


/* additional symbol flags */
#define COMM_REFERENCED (RSRVD_O<<0)  /* common symbol was referenced */
#define SEC_DELETED     (RSRVD_O<<1)  /* this section can be deleted */
#define INTERNAL_SYM    (RSRVD_O<<2)  /* internal symbol to be ignored */


/* Amiga DOS Hunks */
#define HUNK_UNIT       999
#define HUNK_NAME       1000
#define HUNK_CODE       1001
#define HUNK_DATA       1002
#define HUNK_BSS        1003
#define HUNK_ABSRELOC32 1004
#define HUNK_RELRELOC16 1005
#define HUNK_RELRELOC8  1006
#define HUNK_EXT        1007
#define HUNK_SYMBOL     1008
#define HUNK_DEBUG      1009
#define HUNK_END        1010
#define HUNK_HEADER     1011
#define HUNK_DREL32     1015
#define HUNK_DREL16     1016
#define HUNK_DREL8      1017
#define HUNK_RELRELOC32 1021
#define HUNK_ABSRELOC16 1022

/* EHF extensions */
#define HUNK_PPC_CODE   1257
#define HUNK_RELRELOC26 1260

/* memory type */
#define HUNKB_CHIP      30
#define HUNKB_FAST	    31
#define HUNKF_CHIP	    (1L<<30)
#define HUNKF_FAST	    (1L<<31)
#define HUNKF_MEMTYPE   (HUNKF_CHIP|HUNKF_FAST)

/* hunk_ext sub-types */
#define EXT_SYMB 0
#define EXT_DEF	1
#define EXT_ABS	2
#define EXT_ABSREF32 129
#define EXT_ABSCOMMON 130
#define EXT_RELREF16 131
#define EXT_RELREF8 132
#define EXT_DEXT32 133
#define EXT_DEXT16 134
#define EXT_DEXT8 135
#define EXT_RELREF32 136
#define EXT_RELCOMMON 137
#define EXT_ABSREF16 138
#define EXT_ABSREF8 139

/* EHF extensions */
#define EXT_RELREF26 229
