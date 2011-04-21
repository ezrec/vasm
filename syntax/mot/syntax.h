/* snytax.h  syntax header file for vasm */
/* (c) in 2002,2005,2009,2010 by Volker Barthelmann and Frank Wille */

/* macros to recognize identifiers */
#define ISIDSTART(x) ((x)=='.'||(x)=='@'||(x)=='_'||isalpha((unsigned char)(x)))
#define ISIDCHAR(x) ((x)=='_'||(x)=='$'||(x)=='%'||isalnum((unsigned char)(x)))

/* symbol which contains the number of macro arguments */
#define NARGSYM "NARG"

/* symbol which contains the number of the current macro argument */
#define CARGSYM "CARG"

/* we have a special skip() function for expressions, called exp_skip() */
char *exp_skip(char *);
#define EXPSKIP() s=exp_skip(s)

/* ignore operand field, when the instruction has no operands */
#define IGNORE_FIRST_EXTRA_OP 1
