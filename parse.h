/* parse.h - global parser support functions */
/* (c) in 2009-2010 by Volker Barthelmann and Frank Wille */

#ifndef PARSE_H
#define PARSE_H 

/* defines */
#define MAXLINELENGTH 4096
#define MAXMACPARAMS 36

struct macro {
  struct macro *next;
  char *name;
  char *text;
  size_t size;
};

/* global variables */
extern int esc_sequences,nocase_macros,maxmacparams;

/* functions */
char *escape(char *,char *);
char *parse_name(char **);
char *skip_identifier(char *);
char *parse_identifier(char **);
char *skip_string(char *,char,taddr *);
dblock *parse_string(char **,char,int);
void include_binary_file(char *);
void new_repeat(int,char *,char *);
macro *new_macro(char *,char *);
int execute_macro(char *,int,char **,int *,int,char *,int);
int leave_macro(void);
char *read_next_line(void);
int init_parse(void);

#endif /* PARSE_H */
