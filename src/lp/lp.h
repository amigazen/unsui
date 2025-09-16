/*
 *  LP.H
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <fcntl.h>

#ifdef AMIGA
#include <exec/types.h>
#include <exec/memory.h>
/*#include "io.h"*/
/*#include "strfn.h"*/
#endif /* AMIGA */

#ifdef _DCC /* DICE */
extern char *getenv(const char *);
#endif /* _DCC */

#if defined(GNUDOS) || defined(__TURBOC__)
typedef int BOOL;
#define TRUE  (1==1)
#define FALSE (1!=1)
#endif /* GNUDOS || __TURBOC__ */

#ifdef GNUDOS
#define stricmp(s,t) strcmp(strupr(s),strupr(t))
#endif /* GNUDOS */

/*
 * Die JOBPARM Struktur enh"alt alle Parameter f"ur den Durckvorgang
 * einer Datei.  LP erstellt vor dem erzeugen der Ausgabe(n) eine
 * Liste von JOBPARM Strukturen und arbeitet diese dann komplett ab.
 */

typedef struct jobparm
{ int indent;       /* left margin */
  int charct;       /* characters per line = bufsize */
  int linect;       /* #of lines per page */
  int tabsize;      /* 0 = don't convert tabs -> spaces */
  int columns;      /* #of columns: width= charct/columns */
  long flags;       /* see JPFLAG_XXXX */
  char *leader;     /* output line leader */
  int firstpage;    /* pageno to start numbering with */
  int lineskip;     /* number of lines to be skiped at tof */
  char *infile;     /* input filename */
  char *outfile;    /* output filename */
  char *tocfile;    /* list file filename (table of contents) */
  char *argmem;     /* argument buffer */
  long membytes;    /* size of allocated argument buffer */
  struct jobparm *next; /* next job to submit */
} jobparm;

/*
 * jobparm.flags
 */

#define JPFLAG_APPEND       (1L)      /* append output to print destination */
#define JPFLAG_REPLACE      (1L<<1)   /* replace input file by output file */
#define JPFLAG_PLAIN        (1L<<2)   /* don't ENV:InitPrinter */
#define JPFLAG_AUTOLF       (1L<<3)   /* send CR, no LF for new lines */
#define JPFLAG_MINI         (1L<<4)   /* print superscript, subscript... */
#define JPFLAG_USEFF        (1L<<5)   /* send FF, not a sequence of LF */
#define JPFLAG_CLEANUP      (1L<<6)   /* delete SPC/TAB at the end of a line */
#define JPFLAG_HEADING      (1L<<7)   /* print a page header */
#define JPFLAG_FOOTING      (1L<<8)   /* print page footings */
#define JPFLAG_DOUBLESIDED  (1L<<9)   /* flag for type of page numbering */
#define JPFLAG_FEEDOUT      (1L<<10)  /* feed out the last sheet of a file */

#define JP_APPEND(f)        (((f)&JPFLAG_APPEND)==JPFLAG_APPEND)
#define JP_REPLACE(f)       (((f)&JPFLAG_REPLACE)==JPFLAG_REPLACE)
#define JP_PLAIN(f)         (((f)&JPFLAG_PLAIN)==JPFLAG_PLAIN)
#define JP_AUTOLF(f)        (((f)&JPFLAG_AUTOLF)==JPFLAG_AUTOLF)
#define JP_MINI(f)          (((f)&JPFLAG_MINI)==JPFLAG_MINI)
#define JP_USEFF(f)         (((f)&JPFLAG_USEFF)==JPFLAG_USEFF)
#define JP_CLEANUP(f)       (((f)&JPFLAG_CLEANUP)==JPFLAG_CLEANUP)
#define JP_HEADING(f)       (((f)&JPFLAG_HEADING)==JPFLAG_HEADING)
#define JP_FOOTING(f)       (((f)&JPFLAG_FOOTING)==JPFLAG_FOOTING)
#define JP_DOUBLESIDED(f)   (((f)&JPFLAG_DOUBLESIDED)==JPFLAG_DOUBLESIDED)
#define JP_SINGLESIDED(f)   (!(JP_DOUBLESIDED(f)))
#define JP_FEEDOUT(f)       (((f)&JPFLAG_FEEDOUT)==JPFLAG_FEEDOUT)

/* jobparm.c */
extern void free_jobparm(jobparm *);
extern jobparm *alloc_jobparm(jobparm *);
extern jobparm *jptail(jobparm *);
extern int check_jobparm(jobparm *, int, char *);
#ifdef DEBUG
extern void debug_joblist(jobparm *);
#endif /* DEBUG */


#define ENV_LPOPTS    "LPOPTS"
#define ENV_PRTINIT   "PRTINIT"
#define LOGFILENAME   "lp.log"
#define DEFOUT        "lp.out"

#if defined(AMIGA)
#define PRINTERNAME   "PRT:"
#elif defined(__MSDOS__)
#define PRINTERNAME   "PRN"
#else /* !AMIGA &! __MSDOS__ */
#define PRINTERNAME   DEFOUT
#endif /* MACHINE */

#define USAGE         "USAGE: lp [options] [infile] [@tocfile]"

#define DEFCHARCT   80L
#define DEFINDENT   0L
#define DEFTABSIZE  8L
#define DEFLINECT   66L
#define DEFJPFLAGS  JPFLAG_CLEANUP

/*
 * Error states
 */

#define STATE_OKAY    0L
#define STATE_NOMEM   1L
#define STATE_NOFILE  2L
#define STATE_ERRFILE 3L
#define STATE_ERROPT  4L
#define STATE_ERRPRT  5L
#define STATE_FATAL   6L

#define MAXARGS 50   /* maximum #of arguments per line */

/* print.h */

typedef struct jobstate
{ long inln;     /* lines of input */
  long outln;    /* lines of output */
  long wraps;    /* #of performed word wraps */
  long cracks;   /* #of broken lines */
  long cleanups; /* #of invisible chars removed */
  long fillups;  /* #of empty lines added */
} jobstate;

#ifdef AMIGA
#define SUPERSCRIPT "\15\33[2v"
#define SUBSCRIPT   "\12\33[4v"

#define INITMINI    "\33c\33#4\33[2w\33[4w\33[0z\33[2v"
#define INITNOMINI  "\33#1"   /* turn off all special modi */
#else
#define SUPERSCRIPT "\33$\0\0\33J\12\33S\0"
#define SUBSCRIPT   "\33$\0\0\33S\1"

#define INITMINI    "\33@\33\63\12\17" SUPERSCRIPT
#define INITNOMINI  "\33@"
#endif

#define HEADERLINES 3L
#define FOOTERLINES 3L

#define DEFREFRESH  DEFLINECT

#define TEMPNAME    "lp$col.tmp"

#define TAB '\t'
#define LF  '\n'
#define CR  '\r'
#define FF  '\f'

#define term_write(num)                   \
        do                                \
        { fprintf(stdout," %ld\r",num);   \
          fflush(stdout);                 \
        } while(0);

#define term_clear                        \
        do                                \
        { fprintf(stdout,"       \r");    \
          fflush(stdout);                 \
        } while(0);

/* print.c */
extern void newline(FILE *, long);
extern void skip_lines(FILE *, long);
extern char *make_heading(jobparm *);
extern char *make_footing(jobparm *, unsigned long);
extern int join_columns(jobparm *);
extern int print_file(jobparm *,jobstate *);

/* opts.c */
extern char **howtouse;

/* args.c */
extern char *alloc_argmem(jobparm *, long);
extern char *load_argfile(jobparm *, char *);
extern void arglist2jobparm(jobparm *, int, char **);
extern char *make_arglist(char *, int *, char **, long *);
extern int file2jobparm(jobparm *, char *);
