/************************************************************************/
/*	little_globals.c (for testing purposes)				*/
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "globals.h"
#include "globals.i"


#define MAXLINE		530	/* Beware of overflows! */

static char maxline[MAXLINE];


static void DoOut(char *fmt, va_list);

void *xmalloc(unsigned size)
{
  void *poi;

  if ((poi = malloc(size)) == NULL) {
    Fatal(10,"no memory");
  }
  if (poi != NULL)
    (void)memset(poi, 0, size);		/* clear the mem */
  return poi;
}

void __stdargs Fatal(int ex, char *fmt, ...)
{
  va_list argptr;
  printf("FATAL --");
  va_start(argptr, fmt);
  DoOut(fmt,argptr);
  va_end(argptr);
  AbortRun(ex);
}

void __stdargs Warning(char *fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  DoOut(fmt,argptr);
  va_end(argptr);
}

void __stdargs Message(char *fmt, ...)
{
  va_list argptr;
  va_start(argptr, fmt);
  DoOut(fmt,argptr);
  va_end(argptr);
}

static void DoOut(char *fmt, va_list args)
{
  if (NULL == fmt) {
    puts("little_globals Test");
  }
  else {
    int length;
    if (MAXLINE <= (length = vsprintf(maxline, fmt, args))) {
      /* GURU kam bestimmt schon... (Wert ermitteln)	*/
      printf("GURU: maxline buffer overflow(%d)\n",length);
      AbortRun(20);
    }
    if (length>80) printf("Überlange Zeile folgt (%d):\n",length);
    puts(maxline);
  }
}

void AbortRun(int ex)
{
  exit(ex);
}

