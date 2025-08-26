/*
   output.c: output functions

   bf 11-22-96
*/

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <proto/dos.h>
#include <proto/exec.h>

#include "scc.h"

BPTR StdErrFH;
char *prgname;

BPTR
StdErr (void)
{
  return StdErrFH;
}

void
init_stderr (int argc, char *argv[])
{
  MyProc = (struct Process *) FindTask (0);

  prgname = argv[0];

  if (!(StdErrFH = MyProc -> pr_CES))
    StdErrFH = MyProc -> pr_COS;
}

void
fhprintf (BPTR fh, const char *fmt, ...)
{
  char l[LBUFSIZE];
  va_list ap;

  va_start (ap, fmt);
  vsprintf (l, fmt, ap);
  va_end (ap);

  Write (fh, l, strlen (l));
}

void
print_error (const char *str)
{
  char *p, l[LBUFSIZE];

  p = stpcpy (l, prgname);
  p = stpcpy (p, ": ");
  p = stpcpy (p, (char *) str);
  p = stpcpy (p, "\n");
  Write (StdErrFH, l, strlen (l));
}
