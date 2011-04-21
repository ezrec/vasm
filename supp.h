/* supp.h miscellaneous support routines */
/* (c) in 2008-2010 by Frank Wille */

#ifndef SUPP_H
#define SUPP_H

struct node {
  struct node *next;
  struct node *pred;
};

struct list {
  struct node *first;
  struct node *dummy;
  struct node *last;
};

void initlist(struct list *);
void addtail(struct list *,struct node *);
struct node *remnode(struct node *);
struct node *remhead(struct list *);

void *mymalloc(size_t);
void *mycalloc(size_t);
void *myrealloc(void *,size_t);
void myfree(void *);

unsigned long long readval(int,void *,int);
void *setval(int,void *,int,unsigned long long);
unsigned long long readbits(int,void *,unsigned,unsigned,unsigned);
void setbits(int,void *,unsigned,unsigned,unsigned,unsigned long long);
void copy_cpu_taddr(void *,taddr,size_t);

void fw8(FILE *,unsigned char);
void fw32(FILE *,unsigned long,int);
void fwdata(FILE *,void *,size_t);
void fwalign(FILE *,taddr,taddr);
int fwsblock(FILE *,sblock *);
size_t filesize(FILE *);
char *convert_path(char *);

int stricmp(const char *,const char *);
int strnicmp(const char *,const char *,size_t);
char *mystrdup(char *);
char *cnvstr(char *,int);
char *strtolower(char *);

taddr balign(taddr,taddr);
taddr palign(taddr,taddr);

taddr get_sym_value(symbol *);
taddr get_sym_size(symbol *);
taddr get_sec_size(section *);
taddr nreloc_real_addend(nreloc *);
void unsupp_reloc_error(rlist *);

#endif
