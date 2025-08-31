/* file for GNU Emacs running on AmigaDOS 2.04, SAS C compiler 5.10b
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#ifndef AMIGA
#define AMIGA
#endif /* AMIGA */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "amigados"

/* Define this if you want a faster redisplay. This saves a lot of CPU
   time at the expense of more characters to be redrawn.
   On a bitmapped display you win, with a serial line you probably lose.
*/
#define FAST_DISPLAY

/* Define this to display eight bit characters. The actual characters
   that are visible can be set in init_xdisp ().
*/
#define EIGHT_BIT

/* nomultiplejobs should be defined if your system's shell
 does not have "job control" (the ability to stop a program,
 run some other program, then continue the first one).  */

#define NOMULTIPLEJOBS

/* Define this to include various patches that allow the Amiga to dump.
   This *must* be defined on the Amiga!
*/
#define AMIGA_DUMP

/* Do not use interrupt_input = 1 by default, because in 4.3
   we can make noninterrupt input work properly.  */

/* #undef INTERRUPT_INPUT */	/* This file borrowed from s-bsd4-3.h */

/* First pty name is /dev/ptyp0.  */

/* #define FIRST_PTY_LETTER 'p' */
/*
 *	Define HAVE_TIMEVAL if the system supports the BSD style clock values.
 *	Look in <sys/time.h> for a timeval structure.
 */

#define HAVE_TIMEVAL
#define USE_UTIME

/*
 *	Define HAVE_SELECT if the system supports the `select' system call.
 */

#define HAVE_SELECT

/*
 *	Define HAVE_PTYS if the system supports pty devices.
 */

/* #define HAVE_PTYS */

/* Define HAVE_SOCKETS if system supports 4.2-compatible sockets.  */

/* #define HAVE_SOCKETS */

/* But we do have socket pairs for processes ... */
#define SKTPAIR

/*
 *	Define NONSYSTEM_DIR_LIBRARY to make Emacs emulate
 *      The 4.2 opendir, etc., library functions.
 */

/* #define NONSYSTEM_DIR_LIBRARY */
#define SYSV_SYSTEM_DIR

/* Define this symbol if your system has the functions bcopy, etc. */

#define BSTRING		/* #define'ed later on */

/* subprocesses should be defined if you want to
   have code for asynchronous subprocesses
   (as used in M-x compile and M-x shell).
   This is generally OS dependent, and not supported
   under most USG systems. */

#define subprocesses

#define DID_REMOTE		/* Use 0 length write to send eof */

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* Define CLASH_DETECTION if you want lock files to be written
   so that Emacs can tell instantly when you try to modify
   a file that someone else has modified in his Emacs.  */

/* #define CLASH_DETECTION */

/* We use the Berkeley (and usg5.2.2) interface to nlist.  */

/* #define NLIST_STRUCT */

/* The file containing the kernel's symbol table is called /vmunix.  */

/* #define KERNEL_FILE "/vmunix" */

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

/* #define LDAV_SYMBOL "_avenrun" */

/* We use our own malloc for 2 reasons:
     - To check that the 6 (INTBITS - VALBITS) of allocated data are
       the same as &pure[0].
     - To release unused memory to the system (SAS's malloc keeps it
       till you quit)
*/

#define SYMS_SYSTEM syms_of_amiga()

#define SYSTEM_MALLOC		/* But I have replaced the system malloc ... */

#define DEF_PURESIZE 132000		/* Leave space for extra code for Amiga */
#ifdef emacs
extern int puresize;
#endif
#define PURESIZE puresize	/* Puresize is variable ... */

#ifdef emacs
/* Stdio must be included before redefining putchar */
#include <stdio.h>
extern char cbuffer[], *cbuffer_pos;
#define PENDING_OUTPUT_COUNT(x) (cbuffer_pos - cbuffer)
#endif
/* We divert some calls to our routines */
#define putchar(c) do { extern int noninteractive; \
		     if (noninteractive) putc (c, stdout); \
		     else emacs_putchar(c); } while(0)
#define fwrite emacs_fwrite
#define fflush emacs_fflush
#define random rand
#define srandom srand
#define main emacs_main
#define select emacs_select

#ifdef emacs
#include <string.h>
#undef index
#undef rindex
#define index strchr
#define rindex strrchr
#endif

#define fsync(x) 0		/* Emulate fsync ... */

#ifdef emacs
#include <sys/wait.h>		/* process.c doesn't have appropriate #ifdef's */
extern int amiga_process_stack_size;
#endif

/* Here are some symbols for ymakefile's benefit */

#define	LIB_STANDARD		src:unix/src/unix.lib lib:sc.lib lib:amiga.lib
#define	START_FILES		lib:c.o firstfile.o
#define	C_DEBUG_SWITCH		debug s
#define	C_OPTIMIZE_SWITCH	opt
#define	LD_SWITCH_SYSTEM
#define	C_SWITCH_SYSTEM
#define	S_SWITCH_MACHINE	/* Nothing! */
#define UNEXEC amiga_dump.o
#define OBJECTS_SYSTEM		amiga_clipboard.o amiga_tty.o amiga_serial.o \
				amiga_screen.o amiga_menu.o amiga_malloc.o \
                                amiga_rexx.o simplerexx.o amiga_term.o


/* Amiga window-specific stuff */

#define VERS "1.29DG"

#define FALSE 0
#define TRUE 1
