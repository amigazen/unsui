/* Hand-coded routines for TeX or Metafont in C.  This code was (mostly)
   written by Tim Morgan, drawing from other Unix ports of TeX.  The
   file-opening routines are also used by BibTeX.  */

/* Either `texd.h' or `mfd.h' will include `../common/texmf.h'.  */

#ifdef AMIGA
#include <exec/types.h>
#include <workbench/startup.h>
/*#include <dos/dos.h>*/
#include <clib/dos_protos.h>
#include <pragmas/dos_pragmas.h>
extern struct DosLibrary	*DOSBase;
long __stack = 20480;			/* wenigstens 20kB stack! */
#endif


/* Instantiate data in `texd.h' or `mfd.h' here.  */
#define	EXTERN

#ifdef TeX
#include "texd.h"
#define dump_default_var TEXformatdefault
#define dump_default " plain.fmt"
#define dump_format " %s.fmt"
#define dump_ext_length 4
#define dump_default_length formatdefaultlength
#define virgin_program "virtex"
#define main_program texbody
#define edit_value tex_edit_value
#ifdef AMIGA
# define edit_var ((do_rexx) ? "TEXREXXEDIT" : "TEXEDIT")
#else
# define edit_var "TEXEDIT"
#endif
#else /* not TeX */
#include "mfd.h"
#define dump_default_var MFbasedefault
#define dump_default " plain.base"
#define dump_format " %s.base"
#define dump_ext_length 5
#define dump_default_length basedefaultlength
#define virgin_program "virmf"
#define main_program main_body
#define edit_value mf_edit_value
#define edit_var "MFEDIT"
#endif /* not TeX */

/* Catch interrupts.  */
#define	HANDLE_INTERRUPTS

#ifdef HANDLE_INTERRUPTS
# ifndef AMIGA
#  ifdef _POSIX_SOURCE
#   include <sys/types.h>
#  endif
#  include <signal.h>
# endif
#endif

#ifdef AMIGA
# include <stddef.h>
# include <stdlib.h>
extern int do_rexx;		/* work with rexx ?? */
int call_rexx(char *str);
void main(int argc, char *argv[]);
#endif

#ifdef BSD
void funny_core_dump ();
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <fcntl.h>
#else
#include <time.h>
#endif

#if defined (BSD) || defined (SYSV)
#include <sgtty.h>
#endif

#ifdef atarist
#include <limits.h>	/* for LONG_MIN */
#include <stdlib.h>
#endif

#include <ctype.h>		/* for tolower() */


#ifndef AMIGA
extern long time ();
extern struct tm *localtime ();
#endif


#if defined(atarist) && defined(__GNUC__)
	/* make _stksize explicit, thus we can change it later in
	 * the executable program.  As default, take all of the mem.
	 */
long _stksize = -1L;

	/* d:	allow pseudo devices like /dev/A for a:
	 * /:	allow '/' and '\'
	 * (b):	open files in binary (this is done with an explicit
	 *	_binmode call later).
	 */
char *_default_unixmode = "d/";
#endif


/* The main program, etc.  */

/* What we were invoked with. */
static int gargc;
static char **gargv;

#ifdef atarist
long crlfflag;		/* write \n as \r\n in textfiles ? */
#endif



/* The entry point: set up for reading the command line, which will
   happen in `topenin', then call the main body.  */
void
main (ac, av)
  int ac;
  char *av[];
{
#ifndef INI
  char custom_default[FILENAMESIZE];
#endif
#ifdef TEXCONFIG
  char *config_path;
  char debugflag;
  long tmp_user_language;
#endif
#ifdef AMIGA
  char *env;
#endif

#ifdef atarist
  /* All files should be opened in binary mode.  The quick solution,
   * using 'b' in UNIXMODE is not sufficient.
   */
  _binmode(1);			/* open all files in binary mode */
  _mallocChunkSize(1024L);
#endif

#ifdef AMIGA
  if (ac == 0) {
    /* Start from Workbench */
    struct WBStartup *WBenchMsg = (struct WBStartup *)av;
    struct WBArg *arg;

    av = (void *)xmalloc(2*sizeof(char *));	/* soviel brauch ich maximal */

    arg = WBenchMsg->sm_ArgList;
    av[0] = arg->wa_Name;		/* Name ohne Pfad */

    if (WBenchMsg->sm_NumArgs > 1) {	/* gibt es auch noch ein Argument? */
      arg++;
      av[1] = arg->wa_Name;
      (void)CurrentDir(arg->wa_Lock);	/* gehe in das Verzeichniss des Arguments */
      ac = 2;
    } else {
      ac = 1;
    }
  }
#endif

  dump_default_var = dump_default;
  dump_default_length = strlen (dump_default) - 1;	/* lc bug */


#ifndef INI
  if (readyalready != 314159) {
      char *program_name = NULL;

      program_name = rindex (av[0], '/');
#ifdef atarist
      if (program_name == NULL)
	program_name = rindex (av[0], '\\');
#endif
#ifdef AMIGA
      if (program_name == NULL)
	program_name = index (av[0], ':');
#endif
      if (program_name == NULL)
	program_name = av[0];
      else
	program_name++;
#ifdef atarist
      { char *suffix = rindex (program_name, '.');
	if( suffix != NULL )
	   *suffix = '\0';
      }
#endif
      if (strcmp (program_name, virgin_program) != 0) {
          /* TeX or Metafont adds the space at the end of the name.  */
          (void) sprintf (custom_default, dump_format, program_name);
          dump_default_var = custom_default;
          dump_default_length = strlen (program_name) + dump_ext_length;
      }
#ifdef AMIGA
      if ((env = getenv("TEXFORMAT")) != NULL) {
          (void) sprintf (custom_default, dump_format, env);
          dump_default_var = custom_default;
          dump_default_length = strlen (env) + dump_ext_length;
      }
#endif
  }
#endif /* not INI */


#ifdef TEXCONFIG
#ifdef atarist
  crlfflag = 0L;
#endif
  config_path = TEXCONFIG;
  debugflag = 0;
  tmp_user_language = LONG_MIN;

  user_language    = -1L; /* -1 means: don't overwrite language in fmt-File */
  user_interaction = -1; /* -1 means: `interaction' not set by user */

#ifdef MLTEX
  is_ML_TeX = 0;
#endif
#ifdef TEXXET
  is_TeX_XeT = 0;
#endif

  while( ac > 1  && av[1][0] == '-' ) {
    switch( tolower(av[1][1]) ) {
    case 'c':	config_path = &av[1][2];
		break;
    case 'd':	debugflag++;
		break;
#ifdef atarist
    case 't':	crlfflag=1L;		/* output \n as \r\n */
		break;
#endif
    case 'b':	user_interaction = batchmode ;
		break;
    case 'n':	user_interaction = nonstopmode ;
		break;
    case 's':	user_interaction = scrollmode ;
		break;
    case 'e':	user_interaction = errorstopmode ;
		break;
    case 'l':	tmp_user_language = strtol(&av[1][2], NULL, 0);
		/* if( tmp_user_language > 255 )
		  tmp_user_language = LONG_MIN; */
		/* \language>255 is allowed in TeX ! ;-) */
		break;
#ifdef MLTEX
    case 'm':	is_ML_TeX = 1;
		break;
#endif
#ifdef TEXXET
    case 'x':	is_TeX_XeT = 1;
		break;
#endif
    default:	printf("Unknown flag -%c ignored\n", av[1][1]);
		break;
    }
    ac--;
    av++;
  }
#endif	/* TEXCONFIG */

  gargc = ac;
  gargv = av;

#ifdef TEXCONFIG
  do_path(TEXCONFIGPATH, config_path);
  init_arrays(debugflag);

  /* overwrite user_language set in tex.cnf-file, if given in cmdline */
  if( tmp_user_language != LONG_MIN )
    user_language = tmp_user_language;
#endif

  main_program ();
  /*NOTREACHED*/
}



/* This is supposed to ``open the terminal for input'', but what we
   really do is copy command line arguments into TeX's or Metafont's
   buffer, so they can handle them.  If nothing is available, or we've
   been called already (and hence, gargc==0), we return with
   `last=first'.  */

void
topenin (void)
{
  register int i;

  buffer[first] = 0;	/* So the first `strcat' will work.  */

  if (gargc > 1) { /* We have command line arguments.  */
     for (i = 1; i < gargc; i++) {
	(void) strcat ((char *) &buffer[first], gargv[i]);
        (void) strcat ((char *) &buffer[first], " ");
     }
     gargc = 0;	/* Don't do this again.  */
  }

  /* Make `last' be one past the last non-blank character in `buffer'.  */
  for (last = first; buffer[last]; ++last)
     ;
  for (--last; last >= first && buffer[last] == ' '; --last)
     ;
  last++;
}




#ifdef HANDLE_INTERRUPTS
/* All our interrupt handler has to do is set TeX's or Metafont's global
   variable `interrupt'; then they will do everything needed.  */

#ifdef AMIGA

int _CXBRK(void)		/* overwrite standard SAS/C-function */
{
  interrupt = 1;
  return 0;
}

#else /* !AMIGA */

static void
interrupt_handler ()
{
  interrupt = 1;
  (void) signal (SIGINT, interrupt_handler);
}
#endif /* !AMIGA */

#endif /* HANDLE_INTERRUPTS */


/* Besides getting the date and time here, we also set up the interrupt
   handler, for no particularly good reason.  It's just that since the
   `fix_date_and_time' routine is called early on (section 1337 in TeX,
   ``Get the first line of input and prepare to start''), this is as
   good a place as any.  */

#ifdef atarist
#include <osbind.h>
#endif

void
get_date_and_time (integer *minutes, integer *day, integer *month, integer *year)
{
#ifndef atarist
  long clock;
  struct tm *tmptr;

  clock = time ((long *) 0);
  tmptr = localtime (&clock);

  *minutes = tmptr->tm_hour * 60 + tmptr->tm_min;
  *day = tmptr->tm_mday;
  *month = tmptr->tm_mon + 1;
  *year = tmptr->tm_year + 1900;
#else
  /* Spart gegenueber den Lib-Funktionen Zeit und viel Platz ... */
  unsigned dostime, dosdate;

  dostime = Tgettime();
  dosdate = Tgetdate();

  *minutes = ((dostime >> 5) & 63) + 60 * ((short) ((dostime >> 11) & 31));
  *day = dosdate & 31;
  *month = ((dosdate >> 5) & 15);
  *year = 1980 + ((dosdate >> 9) & 255);
#endif

#ifdef HANDLE_INTERRUPTS
# ifndef AMIGA
  {
    SIGNAL_HANDLER_RETURN_TYPE (*old_handler) ();

    if ((old_handler = signal (SIGINT, interrupt_handler)) != SIG_DFL)
      (void) signal (SIGINT, old_handler);
  }
# endif
#endif
}



/* I/O for TeX and Metafont.  */

/* Read a line of input as efficiently as possible while still looking
   like Pascal.  We set `last' to `first' and return `false' if we get
   to eof.  Otherwise, we return `true' and either `last==first' or
   `buffer[last-1]!=' ''; that is, last == first + length(line except
   trailing whitespace).  */

boolean
input_line (register FILE *f)
{
  register int i = EOF;
  register ASCIIcode *bufp;	/* bufp = &buffer[last] */
  register integer r_last;	/* register var for global `last' */
  register ASCIIcode *r_bufend = &buffer[bufsize];

  r_last = first;	/* last = first;   cf. Matthew 19:30 */
  bufp = &buffer[r_last];

#ifdef BSD
  if (f == stdin) clearerr (stdin);
#endif

  /*      last < bufsize */
  while (bufp < r_bufend && (i = getc (f)) != EOF && i != '\n') {
    *bufp++ = i;		/* buffer[last++] = i */
  }

  r_last = bufp - buffer;

  if (i == EOF && r_last == first) {
    last = r_last;
    return false;
  }

  /* We didn't get the whole line because our buffer was too small.  */
  if (i != EOF && i != '\n') {
    (void) fprintf (stderr, "\
! Unable to read an entire line---bufsize=%ld.\n\
Please set `bufsize' to a larger value.\n", bufsize);
      exit (10);
  }

  /* buffer[last] = ' '; */
  *bufp-- = ' ';	/* bufp-- because of buffer[last-1] in next loop */
	
  if (r_last > maxbufstack)
    maxbufstack = r_last;

  /* Trim trailing whitespace.  */
  while (r_last > first && (*bufp == ' ' || *bufp == '\t'
#ifdef atarist
  /* ... and trailing carriage returns.  */	|| *bufp == '\r'
#endif
								)) {
    --r_last; --bufp;
  }

  last = r_last;	/* Don't forget this ! */


  /* Now, either `last==first' or `buffer[last-1] != ' '' (or \t). */

  /* Don't bother using xord if we don't need to.  */
#ifdef NONASCII
#if 0
  for (i = first; i <= last; i++)
     buffer[i] = xord[buffer[i]];
#else
  r_bufend = &buffer[r_last];
  for( bufp = &buffer[first] ; bufp <= r_bufend ; bufp++ )
     *bufp = xord[*bufp];
#endif
#endif

    return true;
}


#ifdef BSD
/* Clear any pending terminal input.  The usual way to do this in Unix
  is to switch the terminal to get the current tty flags, set RAW mode,
  then switch back to the original setting.  If the user isn't in COOKED
  mode, though, this won't work.  At least, it leaves his terminal in
  its original mode.  */

void
bsd_clear_terminal ()
{
  int arg = FREAD;
  (void) ioctl (fileno (stdout), TIOCFLUSH, &arg);
}


/* Cancel any output cancellation (^O) by the user.  */

void
bsd_wake_up_terminal ()
{
  int i = LFLUSHO;
  (void) ioctl (fileno (stdout), TIOCLBIC, (struct sgttyb *) &i);
}

#endif /* BSD */



#ifndef atarist
/* Diese Routine braucht man eigentlich nur fuer virtex. */
/* Beim ST sollte man bei Verwendung darauf achten, dass _stksize!=-1,
   da sonst kein Platz fuer Editor da ist.
 */


/* This string specifies what the `e' option does in response to an
   error message.  */ 
static char *edit_value = EDITOR;

/* This procedure is due to sjc@s1-c.  TeX (or Metafont) calls it when
   the user types `e' in response to an error, invoking a text editor on
   the erroneous source file.  FNSTART is how far into FILENAME the
   actual filename starts; FNLENGTH is how long the filename is.
   
   See ../site.h for how to set the default, and how to override it.  */

void
calledit (ASCIIcode *filename, poolpointer fnstart,
	  integer fnlength, integer linenumber)
{
  char *temp, *command;
  char c;
  int sdone, ddone, i;

  sdone = ddone = 0;
  filename += fnstart;

#ifdef AMIGA
  if (do_rexx) {
    edit_value = REXXEDITOR;
  } else {
    edit_value = EDITOR;
  }
#endif

#ifdef TEXCONFIG
  /* Hier sollten alle Arrays (mem, eqtb, ...) wieder freigegeben werden,
     damit der Editor nicht wegen Speichermangels abbricht.
     Oder besser: Der Editor soll TeX ueberlagern, also kein system()-call.
  */
#endif

  /* Close any open input files, since we're going to kill the job.  */
  for (i = 1; i <= inopen; i++)
    (void) fclose (inputfile[i]);

  /* Replace the default with the value of the appropriate environment
     variable, if it's set.  */
  temp = getenv (edit_var);
  if (temp != NULL)
    edit_value = temp;

  /* Construct the command string.  The `11' is the maximum length an
     integer might be.  */
  command = xmalloc ((unsigned) (strlen (edit_value) + fnlength + 11));

  /* So we can construct it as we go.  */
  temp = command;

#ifdef AMIGA
  if (do_rexx) {	/* Einklammern */
    *temp++ = '\'';
  }
#endif

  while ((c = *edit_value++) != 0) {
      if (c == '%') {
          switch (c = *edit_value++) {
	    case 'd':
	      if (ddone) {
		  (void) fprintf (stderr,
			"! `%%d' cannot appear twice in editor command.\n");
	          exit (10);
	      }
              (void) sprintf (temp, "%ld", linenumber);
              while (*temp != '\0')
                temp++;
              ddone = 1;
              break;

	    case 's':
              if (sdone) {
	          (void) fprintf(stderr,
			"! `%%s' cannot appear twice in editor command.\n");
		  exit (10);
	      }
              for (i = 0 ; i < fnlength ; i++)
		*temp++ = Xchr (filename[i]);
              sdone = 1;
              break;

	    case '\0':
              *temp++ = '%';
              /* Back up to the null to force termination.  */
	      edit_value--;
	      break;

	    default:
	      *temp++ = '%';
	      *temp++ = c;
	      break;
	  }
      } else
	  *temp++ = c;
  }

#ifdef AMIGA
  if (do_rexx) {	/* Einklammern... */
    *temp++ = '\'';
  }
#endif

  *temp = 0;

  /* Execute the command.  */
#ifdef AMIGA
  if (do_rexx) {
    if (!call_rexx(command))
      (void) fprintf(stderr, "! Trouble executing ARexx command `%s'.\n", command);
  } else {
    if (system (command) != 0)
      (void) fprintf(stderr, "! Trouble executing command `%s'.\n", command);
  }
#else
  if (system (command) != 0)
    (void) fprintf(stderr, "! Trouble executing `%s'.\n", command);
#endif

  /* Quit, since we found an error.  */
  exit (5);
}
#else
void
calledit (filename, fnstart, fnlength, linenumber)
  ASCIIcode *filename;
  poolpointer fnstart;
  integer fnlength, linenumber;
{
    (void) fprintf(stderr, "! Sorry, `e' command not implemented.\n");
    exit(5);
}
#endif



#ifdef BSD
/* This procedure is due to chris@mimsy.umd.edu.  It makes a core dump
   without any sort of error status (abort(2) does give an error status,
   so we don't want to use that).  It is used only when making a preloaded
   TeX from virtex, and is triggered by a magic file name requested as
   input (see `open_input', above).

   This is what is known in computing circles as a hack.  */

void
funny_core_dump ()
{
  int pid, w;
  union wait status;

  switch (pid = vfork ()) {
    case -1:		/* failed */
      perror ("vfork");
      exit (-1);
      /*NOTREACHED*/

    case 0:             /* child */
       (void) signal (SIGQUIT, SIG_DFL);
       (void) kill (getpid (), SIGQUIT);
       (void) write (2, "how did we get here?\n", 21);
       _exit(1);
       /*NOTREACHED*/

    default:		/* parent */
      while ((w = wait (&status)) != pid && w != -1)
	;
      if (status.w_coredump)
	exit (0);
      (void) write (2, "attempt to dump core failed\n", 28);
      exit (1);
    }
}
#endif /* BSD */



#ifndef TeX
/* On-line display routines for Metafont.  Here we use a dispatch table
   indexed by the TERM environment variable to select the graphics
   routines appropriate to the user's terminal.  stdout must be
   connected to a terminal for us to do any graphics.  */

/* We don't want any other window routines screwing us up if we're
   trying to do the trap test.  We could have written another device for
   the trap test, but the terminal type conditionals in initscreen argue
   against that.  */

#ifdef TRAP
#undef SUNWIN
#undef HP2627WIN
#undef X10WIN
#undef X11WIN
#undef TEKTRONIXWIN
#endif


#ifdef HP2627WIN
extern mf_hp2627_initscreen (), mf_hp2627_updatescreen ();
extern mf_hp2627_blankrectangle (), mf_hp2627_paintrow ();
#endif

#ifdef SUNWIN
extern mf_sun_initscreen (), mf_sun_updatescreen ();
extern mf_sun_blankrectangle (), mf_sun_paintrow ();
#endif

#ifdef TEKTRONIXWIN
extern mf_tektronix_initscreen (), mf_tektronix_updatescreen ();
extern mf_tektronix_blankrectangle (), mf_tektronix_paintrow ();
#endif

#ifdef UNITERMWIN
extern mf_uniterm_initscreen (), mf_uniterm_updatescreen();
extern mf_uniterm_blankrectangle(), mf_uniterm_paintrow();
#endif

#ifdef X10WIN
extern mf_x10_initscreen (), mf_x10_updatescreen ();
extern mf_x10_blankrectangle (), mf_x10_paintrow ();
#endif

#ifdef X11WIN
extern mf_x11_initscreen (), mf_x11_updatescreen ();
extern mf_x11_blankrectangle (), mf_x11_paintrow ();
#endif


/* `mfwsw' contains the dispatch tables for each terminal.  We map the
   Pascal calls to the routines `init_screen', `update_screen',
   `blank_rectangle', and `paint_row' into the appropriate entry point
   for the specific terminal that MF is being run on.  */

struct mfwin_sw
{
  char *mfwsw_type;		/* Name of terminal a la TERMCAP.  */
  int (*mfwsw_initscreen) ();
  int (*mfwsw_updatescrn) ();
  int (*mfwsw_blankrect) ();
  int (*mfwsw_paintrow) ();
} mfwsw[] =

/* Now we have a long structure which initializes this array of
   ``Metafont window switches''.  */

{

#ifdef HP2627WIN
  { "hp2627", mf_hp2627_initscreen, mf_hp2627_updatescreen,
    mf_hp2627_blankrectangle, mf_hp2627_paintrow },
#endif

#ifdef SUNWIN
  { "sun", mf_sun_initscreen, mf_sun_updatescreen,
    mf_sun_blankrectangle, mf_sun_paintrow },
#endif

#ifdef TEKTRONIXWIN
  { "tek", mf_tektronix_initscreen, mf_tektronix_updatescreen,
    mf_tektronix_blankrectangle, mf_tektronix_paintrow },
#endif

#ifdef UNITERMWIN
   { "uniterm", mf_uniterm_initscreen, mf_uniterm_updatescreen,
     mf_uniterm_blankrectangle, mf_uniterm_paintrow },
#endif

#ifdef X10WIN
  { "xterm", mf_x10_initscreen, mf_x10_updatescreen,
    mf_x10_blankrectangle, mf_x10_paintrow },
#endif

#ifdef X11WIN
  { "xterm", mf_x11_initscreen, mf_x11_updatescreen, 
    mf_x11_blankrectangle, mf_x11_paintrow },
#endif

  { "Irrelevant", NULL, NULL, NULL, NULL },

/* Finally, we must have an entry with a terminal type of NULL.  */
  { NULL, NULL, NULL, NULL, NULL }

}; /* End of the array initialization.  */


/* This is a pointer to current mfwsw[] entry.  */
static struct mfwin_sw *mfwp;

/* The following are routines that just jump to the correct
   terminal-specific graphics code. If none of the routines in the
   dispatch table exist, or they fail, we produce trap-compatible
   output, i.e., the same words and punctuation that the unchanged
   mf.web would produce.  */


/* This returns true if window operations legal, else false.  */

boolean
initscreen ()
{
#ifndef TRAP
  register char *ttytype;

  if ((ttytype = getenv ("TERM")) == NULL || !(isatty (fileno (stdout))))
    return (0);
  for (mfwp = mfwsw; mfwp->mfwsw_type != NULL; mfwp++)
    if (!strncmp (mfwp->mfwsw_type, ttytype, strlen (mfwp->mfwsw_type))
	|| !strcmp (ttytype, "emacs"))
      if (mfwp->mfwsw_initscreen)
	return ((*mfwp->mfwsw_initscreen) ());
      else
	{			/* Test terminal type */
	  printf ("Could not init_screen for `%s'.\n", ttytype);
	  return 1;
	}
  return 0;

#else /* TRAP */
  return 1;
#endif
}


/* Make sure everything is visible.  */

void
updatescreen ()
{
#ifndef TRAP
  if (mfwp->mfwsw_updatescrn)
    ((*mfwp->mfwsw_updatescrn) ());
  else
    {
      printf ("Updatescreen called\n");
    }
#else /* TRAP */
  fprintf (logfile, "Calling UPDATESCREEN\n");
#endif
}


/* This sets the rectangle bounded by ([left,right], [top,bottom]) to
   the background color.  */

void
blankrectangle (left, right, top, bottom)
     screencol left, right;
     screenrow top, bottom;
{
#ifndef TRAP
  if (mfwp->mfwsw_blankrect)
    ((*mfwp->mfwsw_blankrect) (left, right, top, bottom));
  else
    {
      printf ("Blankrectangle l=%d  r=%d  t=%d  b=%d\n",
	      left, right, top, bottom);
    }
#else /* TRAP */
  fprintf (logfile, "\nCalling BLANKRECTANGLE(%d,%d,%d,%d)\n", left,
	   right, top, bottom);
#endif
}


/* This paints ROW, starting with the color INIT_COLOR. 
   TRANSITION_VECTOR then specifies the length of the run; then we
   switch colors.  This goes on for VECTOR_SIZE transitions.  */

void
paintrow (row, init_color, transition_vector, vector_size)
     screenrow row;
     pixelcolor init_color;
     transspec transition_vector;
     screencol vector_size;
{
#ifndef TRAP
  if (mfwp->mfwsw_paintrow)
    ((*mfwp->mfwsw_paintrow) (row, init_color,
			      transition_vector, vector_size));
  else
    {
      printf ("Paintrow r=%d  c=%d  v=");
      while (vector_size-- > 0)
	printf ("%d  ", transition_vector++);
      printf ("\n");
    }
#else /* TRAP */
  unsigned k;

  fprintf (logfile, "Calling PAINTROW(%d,%d;", row, init_color);
  for (k = 0; k <= vector_size; k++)
    {
      fprintf (logfile, "%d", transition_vector[k]);
      if (k != vector_size)
	fprintf (logfile, ",");
    }
  fprintf (logfile, ")\n");
#endif
}
#endif /* not TeX */
