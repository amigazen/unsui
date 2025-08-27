

#define TEX

#include <dos/dos.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#include <dos/dos.h>
#include <libraries/dos.h>
#include <dos/dosextens.h>

#include <clib/exec_protos.h>
#include <clib/dos_protos.h>

#include <pragmas/exec_pragmas.h>
#include <pragmas/dos_pragmas.h>

extern struct ExecBase		*SysBase;
extern struct DosLibrary	*DOSBase;


#include "GetOpt.h"
#include "GetOpt.i"

#include "little_globals.h"
#include "little_globals.i"

extern char *_ProgramName;

static void PrintHelp(struct Options opt[], short long_hlp);

static char ver[] = "$VER: ShowOpt 0.34 ("__DATE__")";


/*==================================================================*/


/* Nur zum Test... */
char  g_Logname[STRSIZE]= "ShowOpt.log";	/* name of log file, if created            */
int   g_logfile=0;			/* Are these messages going to a log file? */
FILE *g_logfp=NULL;			/* log file pointer (for errors)           */
float hoffset_in = 1.0;
float voffset_in = 1.0;
int resolution = 100;
int hconvresolution, vconvresolution;
char *PXLpath = "default";
int fontmemsize;
int FirstPage;
int PreLoad;
int Stats;
int g_logging;
int g_authors;
char filename[STRSIZE];
FILE *dvifp;
char m_string[STRSIZE];
char *g_progname;


#if 0
#define Message(str)		puts(str)
void Fatal(int ex, char *str, int bla);
void AbortRun(int ex);
void *xmalloc(unsigned size);
#endif

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
     sprintf(m_string,"can't find file \"%s\" to examine!",file);
     Fatal(3,m_string,16);
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

#if 0
void AbortRun(int ex)
{
  Delay(200);
  exit(ex);
}

void *xmalloc(unsigned size)
{
  void *poi;

  if ((poi = malloc(size)) == NULL) {
    Fatal(5,"no memory",111);
  }
  return poi;
}

void Fatal(int ex, char *str, int bla)
{
  printf("Fatal: %s\n",str);
  AbortRun(ex);
}
#endif

/*==================================================================*/


static char *filenameptr;
static long help;
static long DebugStats;
static long logging;


START_PARAM(opt)
  /* req?      key-name     abbrev  type         variable       help-txt */
  NOREQ_PARAM ("HELP",      "?",    OPT_HELP,    &help,		"print help information", NULL)
  NOREQ_PARAM ("DIRECTORY", "DIR",  OPT_STRING,  &PXLpath,	"additional dir for fontlibs/pk-files", NULL)
  NOREQ_PARAM ("FONTMEM",   NULL,   OPT_LONG,    &fontmemsize,	"size of the fontmemory", NULL)
  NOREQ_PARAM ("PAGE",      "P",    OPT_LONG,    &FirstPage,	"start at page", NULL)
  NOREQ_PARAM ("HOFFSET",   "HOFF", OPT_TEX_DIM, &hoffset_in,	"horizontal offset",
		"(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)")
  NOREQ_PARAM ("VOFFSET",   "VOFF", OPT_TEX_DIM, &voffset_in,	"vertical offset",
		"(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)")
  NOREQ_PARAM ("PRELOAD",   NULL,   OPT_BOOLEAN, &PreLoad,	"preload all fonts", NULL)
  NOREQ_PARAM ("RESOLUTION","RES",  OPT_LONG,    &resolution,	"starting resolution in DPI", NULL)
  NOREQ_PARAM ("STATISTIC", "STAT", OPT_BOOLEAN, &Stats,	"more output to the logfile", NULL)
  NOREQ_PARAM ("DEBUGSTAT", NULL,   OPT_BOOLEAN, &DebugStats,	"close logfile after every line", NULL)
  NOREQ_PARAM ("LOGFILE",   "LOG",  OPT_BOOLEAN, &logging,	"create logfile (ShowDVI.log)", NULL)
  HIDDEN_PARAM("PRINTAUTHOR", NULL, OPT_BOOLEAN, &g_authors,	"show author name", NULL)
  NOREQ_PARAM (NULL,        NULL,   OPT_OPTIONSTRING, &filenameptr,"DVI-file", NULL)
END_PARAM


void DecodeArgs(int argc, char *argv[])
{
  BOOL error;

  if (argc == 0) {
    g_progname = _ProgramName;
  }
  else {
    g_progname = argv[0];
  }

  /* Default-Werte */
  fontmemsize = FONTMEMSIZE;

  /*----------------------------------------------------------*/
  error = GetOpt(argc, argv, "ShowDVI", TRUE, opt);
  /*----------------------------------------------------------*/

  if (error) {
    PrintHelp(opt, FALSE);
    AbortRun(5);	/* Programm Ende */
  }
  if (help) {
    PrintHelp(opt, TRUE);
    AbortRun(0);	/* Programm Ende */
  }

  hconvresolution = vconvresolution = resolution;
  if (DebugStats) {
    Stats = 2;
  }
  if (!logging) {
    g_logging = -1;
  }

  if (filenameptr == NULL) {
    /* das ist eigentlich unnoetig 'filenameptr' sollte immer != NULL sein */
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
        sprintf(m_string, "*** Can't find file '%s'!",filename);
        Message(m_string);
      }
    }
    else {		/* es ist ein Directory */
      getdir(filename, dir);
      strcpy(filename, dir);
    }
  }
  
  if (!opt[12].is_given) {
    getdir("",filename);	/* kein Name angegeben => nimm das aktuelle Dir. */
  }
}


static void PrintHelp(struct Options opt[], short long_hlp)
{
  char hlp[100];

  Message(NULL);
  sprintf(m_string,"(c)Copyright 1990-91, (hes). All rights reserved. %s",__DATE__);
  Message(m_string);

  sprintf(hlp, "usage: %s ", g_progname);
  GetOptShortHelp(hlp, 73, opt);
  
  if (long_hlp) {
    Message("");
    GetOptHelp(opt);  
  }
}


void main(int argc, char *argv[])
{
  char buf[100];

  DecodeArgs(argc, argv);
  
  printf("help:         '%d'\n", help);
  printf("PXLpath:      '%s'\n", PXLpath);
  printf("fontmemsize:  '%d'\n", fontmemsize);
  printf("FirstPage:    '%d'\n", FirstPage);
  printf("hoffset_in:   '%f'\n", hoffset_in);
  printf("voffset_in:   '%f'\n", voffset_in);
  printf("PreLoad:      '%d'\n", PreLoad);
  printf("resolution:   '%d'\n", resolution);
  printf("Stats:        '%d'\n", Stats);
  printf("DebugStats:   '%d'\n", DebugStats);
  printf("logging:      '%d'\n", logging);
  printf("g_authors:    '%d'\n", g_authors);
  printf("Filename:     '%s'\n", filename);

  getdir("",buf);
  printf("\ngetdir(\"\",buf) = %s\n",buf);

  if (argc == 0) {
    Delay(200);
  }
  printf(".\n");
}
