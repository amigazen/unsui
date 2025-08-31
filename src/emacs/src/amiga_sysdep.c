/* Interfaces to system-dependent kernel and library entries.
Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

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

#include <exec/types.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <dos/var.h>
#include <exec/execbase.h>
#include <exec/tasks.h>
#include <utility/tagitem.h>
#include <workbench/startup.h>
#include <workbench/workbench.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/icon.h>

#include <string.h>
#include <ios1.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>
#include <internal/vars.h>

#undef LONGBITS
#undef NULL
#include "config.h"
#include "lisp.h"
#include "emacssignal.h"

#define min(x,y) ((x) > (y) ? (y) : (x))

#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"
#include "dispextern.h"
#include "amiga.h"

#define EMACS_TOOL_SIZE 128	/* Room for path to emacs executable */

struct Library *IconBase;
extern struct ExecBase *SysBase;

/*long __stack = 40000;*/		/* Minimum stack size, used by c.o */
int amiga_process_stack_size;

int amiga_initialized;
int amiga_create_icons;		/* If true, we create icons when saving files */
enum exit_method amiga_fail_exit = use_xcexit;
int selecting;

/* Emacs sysdep routines */
/* --------------------- */

set_exclusive_use(int fd) {}

/* Suspend the Emacs process; give terminal to its superior.  */
sys_suspend()
{
  /* This could have been iconify, but:
     a) Not good for serial lines.
     b) emacs stays active while iconified */
}

char *get_system_name()
{
  static char sysname[32];

  gethostname(sysname, sizeof sysname);
  return sysname;
}

/*
 *	flush any pending output
 *      (may flush input as well; it does not matter the way we use it)
 */

flush_pending_output (channel)
     int channel;
{
}

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

/* A few general amiga support routines */
/* ------------------------------------ */

char *expand_path(char *path, char *buf, int len)
{
  BPTR dirlock;
  APTR window;

  window = _us->pr_WindowPtr;
  _us->pr_WindowPtr = (APTR)-1;
  dirlock = Lock(path, ACCESS_READ);
  _us->pr_WindowPtr = window;
  if (dirlock)			/* Expand lock name */
    {
      if (!NameFromLock(dirlock, buf, len)) buf = 0;
      UnLock(dirlock);
      return buf;
    }
  return 0;
}

#undef select
int emacs_select(int nfds, int *rfds, int *wfds, int *efds, struct timeval *timeout)
{
  int ret;

  selecting = TRUE;
  ret = select(nfds, rfds, wfds, efds, timeout);
  selecting = FALSE;
  return ret;
}

void no_memory(void)
{
  _fail("No memory");
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
	newpath = (char *)xrealloc(path, pathsize);
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

/* Some general amiga commands */
/* --------------------------- */

#define emacs_file_icon_width 54
#define emacs_file_icon_height 22
#define emacs_file_icon_num_planes 2

static UWORD chip emacs_file_icon_data[2][22][4] = {
    {
        0x0000,0x0000,0x0000,0x0400,0x0000,0x0000,0x0000,0x0c00,
        0x0000,0x0000,0x0000,0x0c00,0x07ff,0xffff,0xffe0,0x0c00,
        0x0400,0x0000,0x0030,0x0c00,0x0400,0x0000,0x0028,0x0c00,
        0x04ff,0xffff,0xfe24,0x0c00,0x0400,0x0000,0x0022,0x0c00,
        0x04ff,0xffff,0xfe3f,0x0c00,0x0400,0x0000,0x0000,0x8c00,
        0x04ff,0xffc0,0x0000,0x8c00,0x0400,0x0000,0x0000,0x8c00,
        0x0400,0x0000,0x0000,0x8c00,0x0400,0x0000,0x0000,0x8c00,
        0x04ff,0xffff,0xfe00,0x8c00,0x0400,0x0000,0x0000,0x8c00,
        0x04ff,0xffff,0xfe00,0x8c00,0x0400,0x0000,0x0000,0x8c00,
        0x07ff,0xffff,0xffff,0x8c00,0x0000,0x0000,0x0000,0x0c00,
        0x0000,0x0000,0x0000,0x0c00,0x7fff,0xffff,0xffff,0xfc00
    },
    {
        0xffff,0xffff,0xffff,0xf800,0x8000,0x0000,0x0000,0x0000,
        0x8000,0x0000,0x0000,0x0000,0x8000,0x0000,0x0000,0x0000,
        0x83ff,0xffff,0xffc0,0x0000,0x83ff,0xffff,0xffd0,0x0000,
        0x8300,0x0000,0x01d8,0x0000,0x83ff,0xffff,0xffdc,0x0000,
        0x8300,0x0000,0x01c0,0x0000,0x83ff,0xffff,0xffff,0x0000,
        0x8300,0x003f,0xffff,0x0000,0x83ff,0xffff,0xffff,0x0000,
        0x83ff,0xffff,0xffff,0x0000,0x83ff,0xffff,0xffff,0x0000,
        0x8300,0x0000,0x01ff,0x0000,0x83ff,0xffff,0xffff,0x0000,
        0x8300,0x0000,0x01ff,0x0000,0x83ff,0xffff,0xffff,0x0000,
        0x8000,0x0000,0x0000,0x0000,0x8000,0x0000,0x0000,0x0000,
        0x8000,0x0000,0x0000,0x0000,0x8000,0x0000,0x0000,0x0000
    },
};
struct Image far emacs_file_icon_image = {
  0, 0,
  emacs_file_icon_width, emacs_file_icon_height, emacs_file_icon_num_planes,
  (UWORD *)emacs_file_icon_data,
  3, 0,
  0
};

static char *far emacs_file_tooltypes[] = {
  "FILETYPE=TEXT",
  0
};

static char far emacs_tool[EMACS_TOOL_SIZE];

static struct DiskObject far emacs_file_icon_object = {
  WB_DISKMAGIC, WB_DISKVERSION,
  { 0, 0, 0, emacs_file_icon_width, emacs_file_icon_height,
    GFLG_GADGIMAGE | GADGBACKFILL, GACT_IMMEDIATE | GACT_RELVERIFY, GTYP_BOOLGADGET,
    (APTR)&emacs_file_icon_image },
  WBPROJECT, emacs_tool, emacs_file_tooltypes,
  NO_ICON_POSITION, NO_ICON_POSITION,
  0, 0,
  40000				/* Stack size for emacs */
};

DEFUN ("amiga-put-icon", Famiga_put_icon, Samiga_put_icon, 2, 2, 0,
       "Create an icon for FILE.\n\
If FORCE is non-nil create it unconditionally, otherwise only if one doesn't exist.\n\
Returns t if an icon was created, nil otherwise.")
     (file, force)
Lisp_Object file, force;
{
  char *fname;
  struct DiskObject *obj;

  CHECK_STRING(file, 0);
  fname = XSTRING(file)->data;

  if (NULL (force) && (obj = GetDiskObject(fname)))
    {
      /* Icon exists, don't overwrite */
      FreeDiskObject(obj);
      return Qnil;
    }
  emacs_file_icon_object.do_StackSize = _stack_size;
  if (PutDiskObject(fname, &emacs_file_icon_object)) return Qt;
  error("Icon for %s couldn't be created", fname);
}

/* Amiga initialisation routines */
/* ----------------------------- */

syms_of_amiga ()
{
  DEFVAR_BOOL("amiga-initialized", &amiga_initialized, "");
  DEFVAR_INT("amiga-malloc-bytes-used", &malloc_bytes_used,
	     "Number of malloc bytes used when emacs was dumped");
  DEFVAR_BOOL("amiga-create-icons", &amiga_create_icons,
	     "If non-nil, create icons when saving files.");
  defsubr(&Samiga_put_icon);
  amiga_process_stack_size = 0;
  DEFVAR_INT("amiga-process-stack-size", &amiga_process_stack_size,
	 "Size of stack for called processes. 0 means same size as emacs stack.");
  syms_of_amiga_tty();
  syms_of_amiga_menu();
  syms_of_amiga_clipboard();
}

static void amiga_early_init(int *_argc, char ***_argv)
{
  int argc = *_argc;
  char **argv = *_argv;

  if (argc > 2 && !strcmp(argv[1], "-pure"))
    {
      puresize = atoi(argv[2]);
      argc -= 2; argv += 2;
    }
  if (argc > 2 && !strcmp(argv[1], "-malloc"))
    {
      malloc_hunk_size = atoi(argv[2]);
      argc -= 2; argv += 2;
    }
  if (argc > 2 && !strcmp(argv[1], "-prealloc"))
    {
      pre_alloc = atoi(argv[2]);
      argc -= 2; argv += 2;
    }
  /* Handle the -dev switch, which specifies device & unit to use as terminal */
  if (argc > 3 && !strcmp (argv[1], "-dev"))
    {
      extern char *far serial_device;
      extern long far serial_unit;

      serial_device = argv[2];
      serial_unit = atoi(argv[3]);
      fprintf (stderr, "Using %s (unit %d)\n", serial_device ,serial_unit);
      argc -= 3; argv += 3;
    }
  /* Patch real argc, argv to hide arguments we used */
  argv[0] = (*_argv)[0];
  *_argv = argv;
  *_argc = argc;

  expand_path(argv[0], emacs_tool, EMACS_TOOL_SIZE);
}

void cleanup_amiga(void)
{
  cleanup_clipboard();
  cleanup_amiga_tty();
  if (IconBase) CloseLibrary(IconBase);
}

void amiga_undump_reinit(void)
/* Post-undump initialisation */
{
  extern struct WBStartup *_WBenchMsg;

  emacs_malloc_init();
  early_amiga_tty();
  early_clipboard();

  if (!onexit(cleanup_amiga)) _fail("Internal problem with onexit");

  make_environ();
  IconBase = OpenLibrary("icon.library", 0);
  if (!IconBase) _fail("Need icon.library");
  amiga_create_icons = _WBenchMsg != 0;

  init_amiga_tty(); init_clipboard();
}

#undef main
main(int argc, char **argv)
/* Effect: Call emacs_main after doing some early amiga initialisation for emacs.
*/
{
  /* This initialisation may steal some command line options */
  amiga_early_init(&argc, &argv);
  emacs_main(argc, argv);
}
