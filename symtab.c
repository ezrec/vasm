/* symtab.c  hashtable file for vasm */
/* (c) in 2002-2004,2008 by Volker Barthelmann and Frank Wille */

#include "vasm.h"

hashtable *new_hashtable(size_t size)
{
  size_t i;
  hashtable *new=mymalloc(sizeof(*new));
  new->size=size;
  new->collisions=0;
  new->entries=mymalloc(size*sizeof(*new->entries));
  for(i=0;i<size;i++)
    new->entries[i]=0;
  return new;
}

size_t hashcode(char *name)
{
  size_t h=0,g;

  while (*name) {
    h = (h << 4) + (unsigned char)*name++;
    if (g = h & 0xf0000000)
      h ^= g >> 24;
    h &= ~g;
  }
  return (h);
}

size_t hashcodelen(char *name,int len)
{
  size_t h=0,g;
  
  while (len--) {
    h = (h << 4) + (unsigned char)*name++;
    if (g = h & 0xf0000000)
      h ^= g >> 24;
    h &= ~g;
  }
  return (h);
}

size_t hashcode_nc(char *name)
{
  size_t h=0,g;

  while (*name) {
    h = (h << 4) + tolower((unsigned char)*name++);
    if (g = h & 0xf0000000)
      h ^= g >> 24;
    h &= ~g;
  }
  return (h);
}

size_t hashcodelen_nc(char *name,int len)
{
  size_t h=0,g;
  
  while (len--) {
    h = (h << 4) + tolower((unsigned char)*name++);
    if (g = h & 0xf0000000)
      h ^= g >> 24;
    h &= ~g;
  }
  return (h);
}

/* add to hashtable; name must be unique */
void add_hashentry(hashtable *ht,char *name,hashdata data)
{
  size_t i=hashcode(name)%ht->size;
  hashentry *new=mymalloc(sizeof(*new));
  new->name=name;
  new->data=data;
  if(DEBUG){
    if(ht->entries[i])
      ht->collisions++;
  }
  new->next=ht->entries[i];
  ht->entries[i]=new;
}

/* finds unique entry in hashtable */
int find_name(hashtable *ht,char *name,hashdata *result)
{
  size_t i=hashcode(name)%ht->size;
  hashentry *p;
  for(p=ht->entries[i];p;p=p->next){
    if(!strcmp(name,p->name)){
      *result=p->data;
      return 1;
    }else
      ht->collisions++;
  }
  return 0;
}

/* same as above, but uses len instead of zero-terminated string */
int find_namelen(hashtable *ht,char *name,int len,hashdata *result)
{
  size_t i=hashcodelen(name,len)%ht->size;
  hashentry *p;
  for(p=ht->entries[i];p;p=p->next){
    if(!strncmp(name,p->name,len)&&p->name[len]==0){
      *result=p->data;
      return 1;
    }else
      ht->collisions++;
  }
  return 0;
}

/* finds unique entry in hashtable - case insensitive */
int find_name_nc(hashtable *ht,char *name,hashdata *result)
{
  size_t i=hashcode_nc(name)%ht->size;
  hashentry *p;
  for(p=ht->entries[i];p;p=p->next){
    if(!stricmp(name,p->name)){
      *result=p->data;
      return 1;
    }else
      ht->collisions++;
  }
  return 0;
}

/* same as above, but uses len instead of zero-terminated string */
int find_namelen_nc(hashtable *ht,char *name,int len,hashdata *result)
{
  size_t i=hashcodelen_nc(name,len)%ht->size;
  hashentry *p;
  for(p=ht->entries[i];p;p=p->next){
    if(!strnicmp(name,p->name,len)&&p->name[len]==0){
      *result=p->data;
      return 1;
    }else
      ht->collisions++;
  }
  return 0;
}
