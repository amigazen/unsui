#include <stdio.h>
#include <stdlib.h>

#include <dos/dos.h>
#include <exec/memory.h>
#include <exec/tasks.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <pragmas/dos_pragmas.h>
#include <string.h>

#include "amigawildcard.h"

#define ALLOC_STEP 50


void init_list(t_strlist *strl)
{
   strl->string=0;
   strl->len=0;
   strl->len_alloc=0;
}

void clear_list(t_strlist *strl)
{
   int i;

   for (i=0;i<strl->len;i++)
   {
     free (pop_elt(i,strl));
   }

   strl->len=0;
   if (strl->string!=0)  { free(strl->string); }
   strl->string=0;

}

char *pop_elt(int index,t_strlist *strl)
{
  char *ret=NULL;

  if ((index>=0)&&(index<strl->len))
  {
    ret=strl->string[index].str;
  }


  return ret;
}

static void push_elt(char *elt,t_strlist *strl)
{
   if (strl->len>=strl->len_alloc)
   {
      strl->len_alloc+=ALLOC_STEP;
      strl->string=realloc(strl->string,strl->len_alloc*sizeof(t_str));
   }

   strl->string[strl->len].str=strdup(elt);

   strl->len++;
}

void print_list(t_strlist *strl)
{
  int i;
  for (i=0;i<strl->len;i++)
  {
    puts(pop_elt(i,strl));
  }
  fflush(stdout);
}

/* fill the list with program argv */

void fill_list(char **argv,int argstart,int argcount,t_strlist *strl)
{
  int i;
  init_list(strl);

  for (i=argstart;i<argcount;i++)
  {
    scan_pattern(argv[i],strl);
  }

}

int scan_pattern(char *source,t_strlist *strl)
{
    int ret;
	struct AnchorPath *ap;
    ap=(struct AnchorPath *)AllocMem(sizeof(struct AnchorPath),MEMF_PUBLIC|MEMF_CLEAR);
    
    if (!(ret=MatchFirst(source,ap)))
    {
      do
      {
        push_elt(ap->ap_Info.fib_FileName,strl);
      }
      while (!MatchNext(ap));

      MatchEnd(ap);
    }
    else
    {
        push_elt(source,strl);
    }

    FreeMem(ap,sizeof(struct AnchorPath));

    return ret;
}
