/* Main include file for this implementation.  Everybody includes this.  */

#ifndef EXTRA_H
#define EXTRA_H

#include <stdio.h>
#include "site.h"
#include "common.h"

/* Path searching.  The `...PATH' constants are used both as indices and
   just as enumeration values.  Their values must match the
   initialization of `env_var_names' in extra.c.  The `...PATHBIT'
   constants are used in the argument to `setpaths'.  */
#define BIBINPUTPATH	0
#define BIBINPUTPATHBIT (1 << BIBINPUTPATH)
#define GFFILEPATH	1
#define GFFILEPATHBIT (1 << GFFILEPATH)
#define MFBASEPATH	2
#define MFBASEPATHBIT (1 << MFBASEPATH)
#define MFINPUTPATH	3
#define MFINPUTPATHBIT (1 << MFINPUTPATH)
#define MFPOOLPATH	4
#define MFPOOLPATHBIT (1 << MFPOOLPATH)
#define PKFILEPATH	5
#define PKFILEPATHBIT (1 << PKFILEPATH)
#define TEXFORMATPATH	6
#define TEXFORMATPATHBIT (1 << TEXFORMATPATH)
#define TEXINPUTPATH	7
#define TEXINPUTPATHBIT (1 << TEXINPUTPATH)
#define TEXPOOLPATH	8
#define TEXPOOLPATHBIT (1 << TEXPOOLPATH)
#define TFMFILEPATH	9
#define TFMFILEPATHBIT (1 << TFMFILEPATH)
#define VFFILEPATH     10
#define VFFILEPATHBIT (1 << VFFILEPATH)
#ifdef TEXCONFIG
#define TEXCONFIGPATH     11
#define TEXCONFIGPATHBIT (1 << TEXCONFIGPATH)
#endif

/* Globally needed routines we can implement as macros.  */

/* Absolute value.  Without the casts to integer here, the Ultrix and
   AIX compilers (at least) produce bad code (or maybe it's that I don't
   understand all the casting rules in C) for tests on memory fields. 
   Specifically, a test in diag_round (in Metafont) on a quarterword
   comes out differently without the cast, thus causing the trap test to
   fail.  (A path at line 86 is constructed slightly differently).  */
/* For atarist: The (integer) cast is unnecessary ...*/
#if defined(atarist) || defined(AMIGA)
# ifdef __GNUC__
#  if defined AMIGA
#   ifndef abs
#    define abs(x)  ((x) >= 0 ? (x) : -(x))
#   endif
#  else
#   include <macros.h>	/* includes generic abs macro */
#  endif
# else
#  ifndef abs
#   define abs(x)  ((x) >= 0 ? (x) : -(x))
#  endif
# endif
#else
# define abs(x)  ((integer) (x) >= 0 ? (integer) (x) : (integer) -(x))
#endif

#define	chr(x)		(x)
#define	decr(x)		--(x)
#define eof(f)		test_eof (f)
#define	fabs(x)		((x) >= 0.0 ? (x) : -(x))
#define flush(f)	(void) fflush (f)
#define	Fputs(f, s)	(void) fputs (s, f)  /* fixwrites outputs this.  */
#define	incr(x)		++(x)
#define	input3ints(a, b, c)  zinput3ints (&a, &b, &c)
#ifdef __GNUC__
#define	odd(x)		((x) % 2)
#else
#define	odd(x)		((x) & 1)
#endif
#define ord(x)		(x)
#define printreal(r, n, m)  fprintreal (stdout, r, n, m)
#define	putbyte(x, f)	putc ((char) (x) & 255, f)
#ifdef atarist
   /* nur mit _binmode(1) !!! */
   /* kein undef, damit GCC einen daran erinnert */
#define getc(fp)	((--(fp)->_cnt >= 0)?((int)*(fp)->_ptr++):_filbuf(fp))
#endif
#define read(f, b)	((b) = getc (f))
#define	readln(f)	{ register c; \
                          while ((c = getc (f)) != '\n' && c != EOF); }
#if defined(atarist) || defined(AMIGA)
#define	round(x)	((integer) (((x) >= 0.0) ? ((x) + 0.5) : ((x) - 0.5)))
#else
#define	round(x)	zround ((double) (x))
#endif

/* Open files for reading and writing.  */
#define	reset(f, n) \
  ((f) ? fclose (f) : 0), (f) = checked_fopen ((char *) (n), "r")
#define rewrite(f, n) \
  (f) = checked_fopen ((char *) (n), "w")

#define	toint(x)	((integer) (x))

/* Pascal's predefined `trunc' routine.  */
#define trunc(x)	((integer) (x))

/* ``Unix exit'', since WEB already defines the symbol `exit'.  */
#define	uexit		exit

/* For throwing away input from the file F.  */
#define vgetc(f)	(void) getc (f)

/* If we don't care that strcpy(3) returns A.  */
#define vstrcpy(a, b)	(void) strcpy (a, b)

/* Write out elements START through END of BUF to the file F.  */
#define writechunk(f, buf, start, end) \
  (void) fwrite (&buf[start], sizeof (buf[start]), end - start + 1, f)

/* Like fseek(3), but cast the arguments and ignore the return value.  */
#define checkedfseek(f, n, w)  (void) fseek(f, (long) n, (int) w)


/* C doesn't distinguish between text files and other files.  */
typedef FILE *text, *file_ptr;

/* For some initializations of constant strings.  */
typedef char *ccharpointer;

#ifndef TeX
/* We need one global variable.  */
extern integer argc;
#endif

/* Routines in extra.c and main.c.  */
#ifdef ANSI
/* extern void argv (int, char[]);		?????????? */
#ifndef TeX
extern FILE *checked_fopen (char *, char *);
#endif
extern boolean eoln (FILE *);
extern void fprintreal (FILE *, double, int, int);
extern integer inputint (FILE *);
extern char *xmalloc (unsigned long), *xrealloc (char *, unsigned long);
#if !defined(atarist) && !defined(AMIGA)
extern integer zround (double);
#endif
extern FILE *testreadaccess(char *, int);

extern void end_with_null(char *);
extern void end_with_space(char *);
#ifdef atarist
extern void replace_slash(char *);
extern void replace_backslash(char *);
#endif
#if defined(atarist) || defined(AMIGA)
extern int get_env_var_index(char *);
#endif
extern void do_path(unsigned, char *);
extern void setpaths(int);

#else /* not ANSI */
/* extern void argv ();				?????????? */
#ifndef TeX
extern FILE *checked_fopen ();
#endif
extern boolean eoln ();
extern void fprintreal ();
extern integer inputint();
extern void fprintreal ();
extern char *xmalloc (), *xrealloc ();
#if !defined(atarist) && !defined(AMIGA)
extern integer zround ();
#endif
extern FILE *testreadaccess();

extern void do_path();
extern void setpaths();
#endif /* not ANSI */

#endif /* not EXTRA_H */
