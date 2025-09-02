#include <exec/types.h>
#include <exec/execbase.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <dos/dostags.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <clib/alib_protos.h>
#include <signal.h>
#undef signal
#include <ios1.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <internal/vars.h>

#include "config.h"
#include "lisp.h"
#include "amiga.h"
#include "emacssignal.h"

extern struct ExecBase *SysBase;

int amiga_process_stack_size;

/* A few emacs support functions */
/* ----------------------------- */

wait_for_termination (pid)
     int pid;
{
  while (1)
    {
      sigsetmask (sigmask (SIGCHLD));
      if (0 > kill (pid, 0))
        {
	  sigsetmask (SIGEMPTYMASK);
	  break;
	}
      sigpause (SIGEMPTYMASK);
    }
}

char *amiga_path(void)
{
  char *path, *pp, name[128];
  int pathsize;
  struct CommandLineInterface *cli;
  BPTR lock;
  long l, *lp, nlen;

  pathsize = 128;
  path = (char *)xmalloc(pathsize);

  strcpy(path, ".");
  pp = path + 1;

  if (!(cli = (struct CommandLineInterface *)((long)_us->pr_CLI << 2)))
    return path;

  l = (long)cli->cli_CommandDir;
  while (l) {
    *pp++ = ',';
    l <<= 2;
    lp = (long *)l;
    lock = (BPTR)*(lp + 1);
    NameFromLock(lock, name, 128);
    nlen = strlen(name);
    if (pp + nlen + 5 >= path + pathsize)
      {
	char *newpath;

	pathsize = 2 * pathsize + nlen;
	newpath = (char *)xrealloc(path);
	pp = newpath + (pp - path);
	path = newpath;
      }
    memcpy(pp, name, nlen);
    pp += nlen;
    l = *lp;
  }
  /* Use of +5 above guarantees that there is enough space for c: */
  strcpy(pp, ",c:");

  return path;
}

void syms_of_amiga_processes(void)
{
  amiga_process_stack_size = 0;
  DEFVAR_INT("amiga-process-stack-size", &amiga_process_stack_size,
	 "Size of stack for called processes. 0 means same size as emacs stack.");
}
