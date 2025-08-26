/*
   SAS/C frontend with
   GCC-style options

   bf 11-14-96
*/

/// Includes
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <clib/alib_protos.h>

#include "scc.h"
#include "sc_rexx.h"
#include "output.h"
///

/// Global Variables
const char *version = "\0$VER: cc 1.6 (26/08/2025)\n";
const char *stack_cookie = "$STACK: 8192";
struct Process *MyProc;
List LibList;
///

/// cleanup()
void
cleanup (void)
{
  Node *node, *next;

  for (next = node = LibList.lh_Head; next = node -> ln_Succ; node = next)
  {
    if (node -> ln_Name)
      free (node -> ln_Name);
    free (node);
  }
}
///

/// add_library_dir()
void
add_library_dir (char *path)
{
  Node *ln = NULL;

  if (!(ln = (Node *) malloc (sizeof (Node))))
    goto cleanup;

  ln -> ln_Name = NULL;
  AddHead (&LibList, ln);

  if (!(ln -> ln_Name = malloc (strlen (path) + 1)))
    goto cleanup;
  
  strcpy (ln -> ln_Name, path);
  return;  

  cleanup:

  print_error ("Out of memory");
  if (ln)
    free (ln);
  exit (20);
}
///

/// locate_library()
char *
locate_library (char *lib)
{
  static char dir[MAXPATHLEN];
  Node *node, *next;
  BPTR lock;

  for (next = node = LibList.lh_Head; next = node -> ln_Succ; node = next)
  {
    strcpy (dir, node -> ln_Name);
    AddPart (dir, lib, MAXPATHLEN);
    strcat (dir, ".lib");
    if (lock = Lock (dir, ACCESS_READ))
    {
      UnLock (lock);
      return dir;
    }
  }
  return NULL;
}
///

/// translate_path()
char *
translate_path (char *path)
{
  static char s[MAXPATHLEN];
  char *f, *t, *p, *e;
  int len, dbl_slash = 0;

  p = path-1;
  t = s;
  e = path + strlen (path);

  while (p < e)
  {
   if (p = strchr (f=p+1, '/'))
     *p = '\0';
   else
     p = e;

   if (!(len = (int) p - (int) f))
     continue;

   if (len <= 3 && *f == '.')
   {
      if (len == 1)
        continue;
      if (*(f+1) == '.')
      {
        if (t != s && *(t-1) != '/')
        {
          dbl_slash = 1;
          *t = '/';
          t++;
        }
        *t = '/';
        t++;
        continue;
      }
    }
    if (t != s && *(t-1) != '/')
    {
      *t = '/';
      t++;
    }
    t = stpcpy (t, f);
    dbl_slash = 0;
  }
  if (dbl_slash)
    *(t-1) = '\0';
  if (t == s)
    strcpy (s, "\"\"");
  return s;
}
///

/// option_argument()
char *
option_argument (int argc, char *argv[], int *i)
{
  int t = *i;
  char *arg;

  if (argv[t][2] != '\0')
    arg = &argv[t][2];
  else
  {
    if (!argv[++(*i)])
    {
      fhprintf (StdErr (), "%s: Option requires argument: '-%c'.\n", argv[0], argv[t][1]);
      exit (20);
    }
    arg = argv[*i];
  }
  return arg;
}
///

/// translate_long_option()
#define TTABSIZE 18

char *
translate_long_option (int argc, char *argv[], int *i)
{
  const struct {
    const char *from;
    const char *to;
  } TransTab[TTABSIZE] =
  {
    { "ansi",				"ansi" },
    { "fsigned-char",			"nouchar" },
    { "funsigned-char",			"uchar" },
    { "fdollars-in-identifiers",	"dolok" },
    { "fnodollars-in-identifiers",	"nodolok" },
    { "m68000",				"cpu=68000" },
    { "m68020",				"cpu=68020" },
    { "m68030",				"cpu=68030" },
    { "m68040",				"cpu=68040" },
    { "m68060",				"cpu=68060" },
    { "pedantic",			"strict" },
    { "pedantic-errors",		"strict err=all" },
    { "w",				"ign=all" },
    { "Wreturn-type",			"wvret" },
    { "Wuninitialized",			"warn=94" },
    { "Wmissing-prototypes",		"warn=100" },
    { "Werror",				"err=all" },
    { "Wall",				"warn=all" },
  };
  char *opt = &argv[*i][1];
  int t;

  for (t=0; t<TTABSIZE; t++)
  {
    if (strcmp (opt, TransTab[t].from) == 0)
      return (char *) TransTab[t].to;
  }
  return NULL;
}
///

/// is_c_source()
int
is_c_source (char *file)
{
  int len = strlen (file);

  if (len < 2)
    return 0;
  if (file[len-2] == '.' && file[len-1] == 'c')
    return 1;
  return 0;
}
///

/// is_object_module()
int
is_object_module (char *file)
{
  int len = strlen (file);

  if (len < 2)
    return 0;
  if (file[len-2] == '.' && file[len-1] == 'o')
    return 1;
  return 0;
}
///

#if !defined ( PPONLY_WITH_CPP )

/// p_file_of()
char *
p_file_of (char *file)
{
  int len = strlen (file);

  if (len < 2)
    return NULL;
  if (file[len-2] == '.' && file[len-1] == 'c')
  {
    file[len-1] = 'p';
    return file;
  }
  return NULL;
}
///

/// print_p_file()
/* SAS/C's .p-file goes to SCC's stdout */
int
print_p_file (char *c_file)
{
  char buffer[4096], *p_file;
  BPTR fh;
  int n;

  if (!(p_file = p_file_of (c_file)))
    return 0;

  if ((fh = Open (p_file, MODE_OLDFILE)) == -1)
    return 0;
  while ((n = Read (fh, buffer, 4096)) > 0)
  {
    if (SetSignal (0,0) & SIGBREAKF_CTRL_C)
    {
      errno = EINTR;
      Close (fh);
      return 0;
    }
    Write (Output (), buffer, n);
  }
  Close (fh);
  return 1;
}
///

#endif

/// process_error_messages()
int
process_error_messages (char *s, int pponly)
{
  char *p, *t;
  int e;

  /* There is no reason to print the errors to
     stdout again. SAS/C already did that.
     GNU autoconf might have redirected stderr
     to search for errors.
  */
  if (!MyProc -> pr_CES)
    return 0;

  send_rexx_msg ("top");
  for (;;)
  {
    if (!(strlen (t = send_rexx_msg ("file"))))
    {
      clear_rexx_result ();
      break;
    }
    p = stpcpy (s, FilePart (t));
    p = stpcpy (p, ":");
    clear_rexx_result ();

    if (pponly)
    {
      e = atoi (send_rexx_msg ("errnum"));
      clear_rexx_result ();
      if (e > 6)
      {
        send_rexx_msg ("delete");
        continue;
      }
    }
    t = send_rexx_msg ("line");
    p = stpcpy (p, t);
    p = stpcpy (p, ":");
    clear_rexx_result ();

    t = send_rexx_msg ("text");
    p = stpcpy (p, t);
    clear_rexx_result ();

    p = stpcpy (p, "\n");
    Write (MyProc -> pr_CES, s, strlen (s));
    send_rexx_msg ("delete");
  }
  return 1;
}
///

/// main()
int
main (int argc, char *argv[])
{
  char *p, s[LBUFSIZE], *t, *l, *c_file;
  int dont_link = 0, i, verbose = 0, pponly = 0;

  NewList (&LibList);
  init_stderr (argc, argv);
  atexit (cleanup);
  atexit (rexx_cleanup);
                     /* GCC has 18 versions of [__](amiga[os]|mc68000)[__] */
  p = stpcpy (s, "sc BATCH ERRREXX DEF=__mc68000__=1 DEF=__amigaos__=1 DEF=__AMIGA__=1 ");

  if (init_scmsg () == 0)
  {
    print_error ("Failed to start/connect to scmsg");
    exit (20);
  }

  for (i=1; i<argc; i++)
  {
    if (argv[i][0] == '-')
    {
      switch (argv[i][1])
      {
        case '?':
          fhprintf (Output (), "SAS/C frontend with gcc-style options.\n"
                    "Usage: %s [-EvgDOocILl][<long options>] <files>\n", argv[0]);
          exit (0);

        case 'E':
          p = stpcpy (p, "NOERRCON NOVERSION PPONLY");
          pponly = 1;
          break;
          
        case 'v':
          p = stpcpy (p, "VERBOSE");
          verbose = 1;        
          break;

        case 'g':
          p = stpcpy (p, "DEBUG=FF");
          break;

        case 'D':
          p = stpcpy (p, "DEF=");
          p = stpcpy (p, option_argument (argc, argv, &i));

#if defined ( SUPPORT_ADE_ENVIRONMENT )
          /* Ugly workaround for -DPATH=\"mypath\", which is
             split at the first '"' (ADE make/sh problem).
          */
          if (*(p-2) == '=' && *(p-1) == '\\' && (t = strstr (argv[i+1], "\\\"")))
          {
            *t++ = '"';
            *t = '\0';
            *(p-1) = '"';
            p = stpcpy (p, argv[++i]);
          }
#endif
          break;

        case 'O':
          p = stpcpy (p, "OPT");
          break;

        case 'o':
          if (is_object_module (t = translate_path (option_argument (argc, argv, &i))))
          {
            dont_link = 1;
            continue;
          }
          p = stpcpy (p, "TO ");
          p = stpcpy (p, t);
          break;

        case 'c':
          dont_link = 1;
          continue;

        case 'I':
          p = stpcpy (p, "IDIR=");
          p = stpcpy (p, translate_path (option_argument (argc, argv, &i)));
          break;

        case 'L':
          add_library_dir (translate_path (option_argument (argc, argv, &i)));
          continue;

        case 'l':
          l = translate_path (option_argument (argc, argv, &i));
          if (!(t = locate_library (l)))
          {
            fhprintf (StdErr (), "%s: Library not found: %s.lib.\n", argv[0], l);
            exit (20);
          }
          p = stpcpy (p, "LIB=");
          p = stpcpy (p, t);
          break;

        default:
          if (t = translate_long_option (argc, argv, &i))
          {
            p = stpcpy (p, t);
            break;
          }
          fhprintf (StdErr (), "Option ignored: %s.\n", argv[i]);
          continue;
      }
    }
    else
    {
      p = stpcpy (p, argv[i]);

      if (is_c_source (t = argv[i]))
        c_file = t;
    }
    p = stpcpy (p, " ");
  }
  if (!dont_link && !pponly)
  {
    p = stpcpy (p, "LINK ");
  }

#if defined ( PPONLY_WITH_CPP )
  if (pponly)
  {
    p = stpcpy (s, "cpp -undef -nostdinc -D_AMIGA -D_M68000 -D__SASC -D__SASC_60 -D__SASC_650 "
                    "-D__VERSION__=6 -D__REVISION__=50 -D__FUNC__=__FUNCTION__ -I/include ");
    for (i=1; i<argc; i++)
    {
      p = stpcpy (p, argv[i]);
      p = stpcpy (p, " ");
    }
  }
#endif

  if (*(--p) == ' ')
  {
    *p = '\0';
  }
  if (verbose)
    fhprintf (Output (), "%s.\n", s);

  i = system (s);

#if defined ( SUPPORT_ADE_ENVIRONMENT ) && !defined ( PPONLY_WITH_CPP )
  if (i || pponly)
  {
    if (pponly)
    {
      /* The SAS/C preprocessor doesn't print any errors so
         we have to run SAS/C twice: once to get the .p file
         and once to get the error messages (GNU autoconf
         scripts rely on this).
      */
      if (t = strstr (s, "PPONLY"))
      {
        int j;

        for (j=0; j<6; j++)
          *(t+j) = ' ';
        i = system (s);
      }
    }

    if (i)
    {
      /* GNU configure expects error messages on stderr */
      if (!process_error_messages (s, pponly))
        print_error ("Warning: pr_CES is NULL, no error output to stderr");
    }
  }
#endif

#if !defined ( PPONLY_WITH_CPP )
  if (pponly)
  {
    if (!print_p_file (c_file))
    {
      print_error ("Error reading preprocessed file");
      exit (20);
    }
  }
  else
#endif
  {
    if (i)
      fhprintf (StdErr (), "%s: SAS/C compiler returned %d.\n", argv[0], i);
  }
  exit (i);
}
///
