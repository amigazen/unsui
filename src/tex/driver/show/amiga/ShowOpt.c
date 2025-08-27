/* externally define -dTEST to compile a standalone version	*/
/* otherwise, link to DVIprint					*/

#ifdef TEST
#define MAIN
#define DISPLAY
#endif

#define TEX

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include "globals.h"
#include <dos/dos.h>
#include <dos/dosextens.h>

#include <clib/exec_protos.h>
#include <pragmas/exec_pragmas.h>
extern struct ExecBase		*SysBase;
#include <clib/dos_protos.h>
#include <pragmas/dos_pragmas.h>
extern struct DosLibrary	*DOSBase;

#include "GetOpt.h"
#include "GetOpt.i"
#include "globals.h"
#include "globals.i"
#include "PrintOpt.i"	/* ShowOpt.i = PrintOpt.i */

/*
#ifndef TEST
#include "prhelp.h"
#include "prhelp.i"
#endif
*/

static void PrintHelp		(struct Options opt[], short long_hlp);

#ifdef TEST

extern char *_ProgramName;

static char ver[] = "$VER: ShowOpt 0.34 ("__DATE__")";


/* liefert den Pfad zum file, falls file=="" dann Pfad des akt. Verz. */
void getdir(char *file, char *dir)
{
  BPTR lock, lock1;
  char strh[108], *h;
  struct FileInfoBlock *fib;
  struct Process *pr;

  if (*file == '\0') {
    pr = (struct Process *)FindTask(NULL);
    if (pr->pr_CurrentDir == NULL) {
      strcpy(dir,"SYS:");
      return;
    }
    else {
      lock = DupLock(pr->pr_CurrentDir);
    }
  }
  else {
    lock = Lock(file,ACCESS_READ);
  }

  if (lock==(BPTR)NULL) {
     Fatal(5,"can't find file \"%s\" to examine!",file);
  }

  dir[0]='\0';

  fib = (struct FileInfoBlock *)xmalloc((unsigned)sizeof(struct FileInfoBlock));
  if (Examine(lock,fib) == 0) {
    Fatal(3,"can't examine file!",17);
  }

  if (file[0] == '\0' || fib->fib_DirEntryType > 0 ) {	/* aktuelles Verz. oder Dir.*/
     strcpy(dir, fib->fib_FileName);
     strcat(dir,"/");
  }
  lock1 = ParentDir(lock);
  UnLock((BPTR)lock);
  lock = lock1;
  while (lock!=(BPTR)NULL)
   {
    if (Examine(lock,fib)==0) {
       Fatal(3,"can't examine file!",17);
    }
    else {
       strcpy(strh,fib->fib_FileName);
       strcat(strh,"/");
       strcat(strh,dir);
       strcpy(dir,strh);
    }
    lock1 = ParentDir(lock);
    UnLock((BPTR)lock);
    lock = lock1;
   }
  h = strchr(dir,'/');
  if (h != NULL) {
    h[0] = ':';
  }
  xfree((char *)fib);
}

int is_dir(char *file)
{
  struct FileLock *lock;
  struct FileInfoBlock *fib;
  int is = FALSE;

  fib = (struct FileInfoBlock *)xmalloc((unsigned)sizeof(struct FileInfoBlock));
  lock = (struct FileLock *)Lock(file,ACCESS_READ);

  if (lock!=NULL) {
    if (Examine((BPTR)lock,fib)!=0) {
      if (fib->fib_DirEntryType>0) {
        is = TRUE;
      }
    }
    UnLock((BPTR)lock);
  }
  xfree((char *)fib);
  return (is);
}

#endif /* TEST */



static long help;
static long o_nlogging;
static char *filenameptr;


START_PARAM(opt)
  /* req?      key-name     abbrev  type         variable       help-txt */
  NOREQ_PARAM ("HELP",        "?",  OPT_HELP,    &help,		"print help information", NULL)
  NOREQ_PARAM ("FONTDir",     "-a", OPT_STRING,  &PXLpath,	"additional dir for fontlibs/pk-files", NULL)
  NOREQ_PARAM ("FONTMem",     "-b", OPT_LONG,    &fontmemsize,	"size of the fontmemory", NULL)
  NOREQ_PARAM ("Page",        NULL, OPT_LONG,    &FirstPage,	"start at page", NULL)
  NOREQ_PARAM ("HOFFset",     "-h", OPT_TEX_DIM, &hoffset_in_fix, "horizontal offset `num'[true]`unit'",
		"(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)")
  NOREQ_PARAM ("VOFFset",     "-v", OPT_TEX_DIM, &voffset_in_fix, "vertical offset `num'[true]`unit'",
		"(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)")
  NOREQ_PARAM ("PREload",     "-p", OPT_BOOLEAN, &PreLoad,	"preload all fonts", NULL)
  NOREQ_PARAM ("RESolution",  "-z", OPT_LONG,    &resolution,	"starting resolution in DPI", NULL)
  NOREQ_PARAM ("STATistic",   "-s", OPT_BOOLEAN, &Stats,	"more output to the logfile", NULL)
  HIDDEN_PARAM("DEBUGStat",   "-S", OPT_BOOLEAN, &DebugStats,	"close logfile after every line", NULL)
  NOREQ_PARAM ("NOLog",       "-l", OPT_BOOLEAN, &o_nlogging,	"create logfile (ShowDVI.log)", NULL)
  NOREQ_PARAM ("LOGFile",     NULL, OPT_STRING,  &g_Logname,	"logfile name", NULL)
  HIDDEN_PARAM("PRINTAUTHOR", NULL, OPT_BOOLEAN, &g_authors,	"show author name", NULL)
  NOREQ_PARAM (NULL,          NULL, OPT_OPTIONSTRING, &filenameptr,"DVI-file", NULL)
END_PARAM


void DecodeArgs(int argc, char *argv[])
{
  BOOL error;

  /*  PXLpath = FONTAREA;  */     /* default font area */

  hoffset_in_fix  = HOFFSET_IN;		/* inch */
  hoffset_is_true = TRUE;		/* true inch? */
  voffset_in_fix  = VOFFSET_IN;		/* inch */
  voffset_is_true = TRUE;		/* true inch? */

  {
    struct Option * o;
    o = GetOneOpt(&hoffset_in_fix, opt)
    if (o) o->special = hoffset_is_true;
    o = GetOneOpt(&voffset_in_fix, opt)
    if (o) o->special = voffset_is_true;
  }

  fontmemsize	  = FONTMEMSIZE;
  FirstPage	  = -1000000L;		/* first page to print (uses count0)	*/
  resolution	  =  100;		/* hes: eher in ShowDVI.config nachschauen*/


  /*----------------------------------------------------------------*/
  error = GetOpt(argc, argv, "ShowDVI", TRUE, opt);
  /*----------------------------------------------------------------*/


  if (error) {
    PrintHelp(opt, FALSE);
    AbortRun(5);	/* Programm Ende */
  }
  if (help) {
    PrintHelp(opt, TRUE);
    AbortRun(0);
  }

  /* true Flags setzen */
  hoffset_is_true = GetOneOpt(&hoffset_in_fix, opt)->special;
  voffset_is_true = GetOneOpt(&voffset_in_fix, opt)->special;

  /* Now we can check the arguments		*/
  g_logging	= o_nlogging ? -1 : 0;
  if (DebugStats)	Stats = 2;
  hconvresolution = vconvresolution = resolution;
  /* fontmemsize ? */

  /* ToDo initialisieren ? */

  if (filenameptr == NULL) {
    getdir("",filename);
  }
  else {
    strcpy(filename, filenameptr);
  }

  { char dir[250];		/* lokale Definition wg. Stack */
    char *ext, *ptr;

    if (!is_dir(filename)) {
 
      if ((ext = strrchr(filename, '.')) == NULL) {
        strcat(filename, ".dvi");
      }
      else {
        if (stricmp(ext, ".tex") == NULL) {
          strcpy(ext, ".dvi");	/* Ueberschreibe das '.tex' */
        }
        else {
          if (stricmp(ext, ".dvi") != NULL) {
            strcat(filename, ".dvi");
          }
        }
      }
      dvifp = fopen(filename, "r");
      if (dvifp != NULL) {
        /* bastle nun den vollen Pfad */
        getdir(filename, dir);
        if ((ptr = strrchr(filename, '/')) == NULL) {
          if ((ptr = strrchr(filename, ':')) == NULL) {
            strcat(dir, filename);
          }
          else {
            strcat(dir, ptr+1);
          }
        }
        else {
          strcat(dir, ptr+1);
        }
        strcpy(filename, dir);
      }
      else {
        if (strrchr(filename, ':') == NULL) {
          if (strrchr(filename, '/') == NULL) {
            getdir("", dir);		/* nicht gefunden, aber ohne Pfad */
            strcat(dir, filename);	/* haenge Pfad an. */
            strcpy(filename, dir);
          }
        }
        Message("*** Can't find file '%s'!",filename);
      }
    }
    else {		/* es ist ein Directory */
      getdir(filename, dir);
      strcpy(filename, dir);
    }
  }
}


static void PrintHelp(struct Options opt[], short long_hlp)
{
  char hlp[100];

  Message(NULL);
  Message("(c)Copyright 1990-92, (hes). All rights reserved. %s\n",__DATE__);

  sprintf(hlp, "usage: %s ", g_progname);
  GetOptShortHelp(hlp, 73, opt);

  if (long_hlp) {
    Message("");
    GetOptHelp(opt);  
  }
  
}

#ifdef TEST
void main(int argc, char *argv[])
{
  g_progname = 0 == argc ? _ProgramName : argv[0];
  g_Logname = SHOWDVI_LOGFILE;

  DecodeArgs(argc, argv);
  
  printf("help:           '%ld'\n", help);
  printf("PXLpath:        '%s'\n",  PXLpath);
  printf("fontmemsize:    '%ld'\n", fontmemsize);
  printf("FirstPage:      '%ld'\n", FirstPage);
  printf("hoffset_in_fix: '%f'\n",  hoffset_in_fix);
  printf("voffset_in_fix: '%f'\n",  voffset_in_fix);
  printf("PreLoad:        '%ld'\n", PreLoad);
  printf("resolution:     '%d'\n",  resolution);
  printf("Stats:          '%ld'\n", Stats);
  printf("DebugStats:     '%ld'\n", DebugStats);
  printf("logging:        '%ld'\n", !o_nlogging);
  printf("g_authors:      '%ld'\n", g_authors);
  printf("Logfile:        '%s'\n",  g_Logname);
  printf("Filename:       '%s'\n",  filename);

  {
  char buf[128];

  getdir("",buf);
  printf("\ngetdir(\"\",buf) = %s\n",buf);
  }

  if (argc == 0) { /* WB: Lesezeit */
    Delay(200);
  }
}
#endif /* TEST */
