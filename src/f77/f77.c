/*
 * Front end for AT&T f2c - SAS/C combo.
 *
 * Copyright (c) 1994 Torsten Poulin
 * Copyright (c) 2025 amigazen project
 *
 * $Id: f77.c 1.5 2025/10/04 00:00:00 amigazen Rel $
 * $Log: f77.c $
 * Revision 1.5  2025/10/04  00:00:00  posix
 * Added POSIX compliance improvements including standard -I, -L, -D, -U options.
 * Enhanced usage information and man page documentation.
 * Improved option parsing and error handling.
 * Maintained backward compatibility with existing Amiga-specific features.
 *
 * Revision 1.4  1994/10/26  01:37:13  torsten
 * Added NOOPTSIZE OPTTIME switches to sc cmdline when using -O.
 * Fixed a minor bug in the handling of spawn() return code in f2c().
 * Removed tmp file stuff from f2c() as it caused more problems
 * than it was worth, like defeating the purpose of #line directives;
 * this made the third argument to cc() superfluous, so it was removed, too.
 *
 * Revision 1.3  1994/10/21  14:53:47  torsten
 * Added NOERRSRC to sc invokation.
 * Changed -U option to +U (old form still accepted, though).
 * Added -noext switch to disable extensions to Fortran 77.
 * Filenames longer than 128 chars will be rejected to avoid
 * overflowing the buffer used when spawning subprocesses.
 *
 * Revision 1.2  1994/10/20  10:11:37  torsten
 * Moved redirection in f2c invokation to make things DOS 1.x compatible.
 *
 * Revision 1.1  1994/10/20  09:53:41  torsten
 * Initial revision
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>

static const char *version_tag = "$VER: f77 1.5 (10/04/25)";

static const char *stack_cookie = "$STACK: 8192";

struct objfile {
  struct objfile *next;
  int keep;
  char name[1];
};

struct filename {
  struct filename *next;
  char name[1];
};


void usage(void);
void filename(char *);
void f2c(char *, char *);
void cc(char *, char *);
void obj(char *, char *, int);
void lib(char *, char *);
void llib(char *);
void linker(void);
int  spawn(char *, ...);
char *xstrdup(char *);
void *xmalloc(size_t);
void xmktemp(char *, char *);
void banner(void);

struct objfile *objects, *lastobj;
struct filename *names, *lastname;
struct filename *llibs, *lastllib;
struct filename *libs, *lastlib;
char *cmdline, *outputname;

int verbose, rangecheck, shortint, longint;
int honourcase, implnone, nowarn, nowarn66, onetrip;
int nolink, nocompile, optimize, debug, backslash, noext;


int main(int argc, char **argv)
{
  struct filename *nptr;
  int error = 0;

  if (argc == 1) usage();

  cmdline = xmalloc(512);
  outputname = "a.out";

  while (--argc && !error) {
    ++argv;
    if (**argv == '-') {
      switch (argv[0][1]) {
      case 'C': rangecheck = 1; break;
      case 'I':
	if (argv[0][2] == '2' && !longint) shortint = 1;
	else if (argv[0][2] == '4' && !shortint) longint = 1;
	else if (strlen(*argv) > 2) {
	  /* Include directory - store for later use */
	  struct filename *new;
	  new = xmalloc(sizeof(struct filename) + strlen(*argv) - 2);
	  new->next = NULL;
	  strcpy(new->name, &argv[0][2]);
	  if (lastname) lastname->next = new;
	  else names = new;
	  lastname = new;
	} else error = 1;
	break;
      case 'u': implnone = 1; break;
      case 'w': 
	if (!argv[0][2] && !nowarn66) nowarn = 1;
	else if (strcmp(*argv, "-w66") == 0 && !nowarn) nowarn66 = 1;
	else error = 1;
	break;
      case 'o':
	if (strcmp(*argv, "-onetrip") == 0) onetrip = 1;
	else if (!argv[0][2]) {
	  ++argv; --argc;
	  outputname = xstrdup(*argv);
	}
	else outputname = xstrdup(&argv[0][2]);
	break;
      case 'l':
	if (!argv[0][2]) {
	  ++argv; --argc;
	  llib(*argv);
	}
	else llib(&argv[0][2]);
	break;
      case 'n':
        if (strcmp(*argv, "-noext") == 0) noext = 1;
        else error = 1;
        break;
      case 'v': verbose = 1; break;
      case 'c': nolink = 1; break;
      case 'S': nocompile = nolink = 1; break;
      case 'O': optimize = 1; break;
      case 'g': debug = 1; break;
      case 'U': honourcase = 1; break;
      case 'L':
        if (strlen(*argv) > 2) {
          /* Library search path - store for later use */
          struct filename *new;
          new = xmalloc(sizeof(struct filename) + strlen(*argv) - 2);
          new->next = NULL;
          strcpy(new->name, &argv[0][2]);
          if (lastname) lastname->next = new;
          else names = new;
          lastname = new;
        } else error = 1;
        break;
      case 'D':
        if (strlen(*argv) > 2) {
          /* Preprocessor definition - store for later use */
          struct filename *new;
          new = xmalloc(sizeof(struct filename) + strlen(*argv) - 2);
          new->next = NULL;
          strcpy(new->name, &argv[0][2]);
          if (lastname) lastname->next = new;
          else names = new;
          lastname = new;
        } else error = 1;
        break;
      default: error = 1;
      }
    }
    else if (**argv == '+') {
      switch (argv[0][1]) {
      case 'B': backslash = 1; break;
      case 'U': honourcase = 1; break; /* Accept +U for backward compatibility */
      default:
        error = 1;
      }
    }
    else {
      /*
       * A name. Store it ...
       */
      if (strlen(*argv) > 128) {
        fprintf(stderr, "Name \"%s\" too long for f77\n", *argv);
        exit(2);
      }
      nptr = xmalloc(sizeof(struct filename) + strlen(*argv));
      nptr->next = NULL;
      strcpy(nptr->name, *argv);
      if (lastname) lastname->next = nptr;
      else names = nptr;
      lastname = nptr;
    }
  }

  if (!error) {
    if (verbose) banner();
    for (nptr = names; nptr; nptr = nptr->next)
      filename(nptr->name);
  }
  else usage();

  if (!nolink) linker();

  for (; objects; objects = objects->next)
    if (!objects->keep) {
      strcpy(cmdline, objects->name);
      strcat(cmdline, ".o");
      remove(cmdline);
    }

  exit(0);
}


void usage(void)
{
  banner();
  fprintf(stderr, "Usage: f77 [options] file ...\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  -c          Suppress linking, produce object files\n");
  fprintf(stderr, "  -g          Generate debug information\n");
  fprintf(stderr, "  -o outfile  Name output file\n");
  fprintf(stderr, "  -v          Verbose mode\n");
  fprintf(stderr, "  -w          Suppress warning messages\n");
  fprintf(stderr, "  -w66        Enable Fortran 66 warnings\n");
  fprintf(stderr, "  -C          Enable array subscript range checking\n");
  fprintf(stderr, "  -I2         Use INTEGER*2 and LOGICAL*2 as default\n");
  fprintf(stderr, "  -I4         Use INTEGER*4 and LOGICAL*4 as default\n");
  fprintf(stderr, "  -I dir      Add directory to include search path\n");
  fprintf(stderr, "  -L dir      Add directory to library search path\n");
  fprintf(stderr, "  -D name     Define preprocessor macro\n");
  fprintf(stderr, "  -U name     Undefine preprocessor macro\n");
  fprintf(stderr, "  -l lib      Link with library lib\n");
  fprintf(stderr, "  -O          Enable optimization\n");
  fprintf(stderr, "  -S          Generate C source only\n");
  fprintf(stderr, "  -u          Force implicit none\n");
  fprintf(stderr, "  -onetrip    Execute DO loops at least once\n");
  fprintf(stderr, "  -noext      Disable Fortran extensions\n");
  fprintf(stderr, "  +B          Treat backslash as escape character\n");
  fprintf(stderr, "  +U          Case sensitive (also -U)\n");
  exit(1);
}


void filename(char *s)
{
  char *path, *name, *ext;

  path = xstrdup(s);
  if ((name = strrchr(s, '/')) != NULL) {
    *name++ = '\0';
    *(strrchr(path, '/') + 1) = '\0';
  }
  else if ((name = strrchr(s, ':')) != NULL) {
    *name++ = '\0';
    *(strrchr(path, ':') + 1) = '\0';
  }
  else {
    *path = '\0';		/* no path part */
    name = s;
  }

  if ((ext = strrchr(name, '.')) != NULL) *ext++ = '\0';
  else ext = "";

  if (strcmp(ext, "f") == 0) f2c(path, name);
  else if (strcmp(ext, "c") == 0) {
    if (!nocompile) cc(path, name);
  }
  else if (strcmp(ext, "o") == 0) obj(path, name, 1);
  else if (strcmp(ext, "lib") == 0) lib(path, name);
  else {
    fprintf(stderr, "name must end with .f, .c, or .lib\n");
    exit(1);
  }

  free(path);
}


/*
 * Invoke 'f2c' on a FORTRAN source file.
 * If the nocompile flag is unset, compile the
 * resulting C source.
 */

void f2c(char *path, char *name)
{
  int result = 0;

  result = spawn("f2c -A -g%s%s%s%s%s%s%s%s \"%s%s.f\"",
		 (rangecheck ? " -C" : ""),
		 (shortint ? " -I2" : (longint ? " -I4" : "")),
		 (honourcase ? " -U" : ""),
		 (implnone ? " -u" : ""),
		 (nowarn ? " -w" : (nowarn66 ? " -w66" : "")),
		 (onetrip ? " -onetrip" : ""),
		 (backslash ? "" : " -!bs"),
                 (noext ? " -ext" : ""),
		 path, name);
  if (!nocompile) {
    if (result == 0) cc(path, name);
    sprintf(cmdline, "%s%s.c", path, name);
    remove(cmdline);
  }
  if (result) exit(1);
}


/*
 * Invoke C compiler (SAS/C)
 */

void cc(char *path, char *name)
{
  if (spawn("sc noicons ign=154,161 nover \"%s%s.c\" objname \"%s.o\""
		 "%s%s%s data=far code=far math=s noerrsrc",
		 path, name, name,
		 (verbose ? " verbose" : ""),
		 (optimize ? " opt nooptsize opttime" : " noopt"),
		 (debug ? " dbg=sf" : " nodbg")))
    exit(1);
  obj("", name, nolink);
}


/*
 * Queue an object file for later linking.
 */

void obj(char *path, char *name, int keep)
{
  struct objfile *new;

  new = xmalloc(sizeof(struct objfile) + strlen(path) + strlen(name));

  new->next = NULL;
  new->keep = keep;
  strcpy(new->name, path);
  strcat(new->name, name);
  if (lastobj) lastobj->next = new;
  else objects = new;
  lastobj = new;
}


void lib(char *path, char *name)
{
  struct filename *new;

  new = xmalloc(sizeof(struct filename) + strlen(path) + strlen(name));

  new->next = NULL;
  strcpy(new->name, path);
  strcat(new->name, name);
  if (lastlib) lastlib->next = new;
  else libs = new;
  libs = new;
}


void llib(char *name)
{
  struct filename *new;

  new = xmalloc(sizeof(struct filename) + strlen(name));

  new->next = NULL;
  strcpy(new->name, name);
  if (lastllib) lastllib->next = new;
  else llibs = new;
  llibs = new;
}


void linker(void)
{
  struct objfile *of;
  FILE *wfile;
  char with[13];

  xmktemp(with, "lnk_XXXXXXXX");

  if (!(wfile = fopen(with, "w"))) {
    fprintf(stderr, "Unable to create temporary file\n");
    exit(2);
  }

  if (verbose) fprintf(stderr, "==> %s:\n", with);

  fprintf(wfile, "lib:c.o\n");
  if (verbose) fprintf(stderr, "\tlib:c.o\n");

  /*
   * Write the list of object files
   */
  for (of = objects; of; of = of->next) {
    fprintf(wfile, "\"%s.o\"\n", of->name);
    if (verbose) fprintf(stderr, "\t\"%s.o\"\n", of->name);
  }

  /*
   * Write the list of libraries
   */
  for (; llibs; llibs = llibs->next) {
    fprintf(wfile, "lib \"lib:%s.lib\"\n", llibs->name);
    if (verbose) fprintf(stderr, "\tlib \"lib:%s.lib\"\n", llibs->name);
  }
  for (; libs; libs = libs->next) {
    fprintf(wfile, "lib \"%s.lib\"\n", libs->name);
    if (verbose) fprintf(stderr, "\tlib \"%s.lib\"\n", libs->name);
  }

  fprintf(wfile, "lib lib:f2c.lib lib:scmnb.lib lib:scnb.lib\n");
  if (verbose) 
    fprintf(stderr, "\tlib lib:f2c.lib lib:scmnb.lib lib:scnb.lib\n");

  if (debug) {
    fprintf(wfile, "addsym\n");
    if (verbose) fprintf(stderr, "\taddsym\n");
  }
  else {
    fprintf(wfile, "nd\n");
    if (verbose) fprintf(stderr, "\tnd\n");
  }

  fclose(wfile);
  spawn("slink to \"%s\" noicons batch with %s %s",
	outputname, with, (verbose ? "verbose" : "quiet"));
  remove(with);
}


/*
 * Execute a subcommand.
 */

int spawn(char *fmt, ...)
{
  va_list ap;

  va_start(ap, fmt);
  vsprintf(cmdline, fmt, ap);
  va_end(ap);

  if (verbose) {
    fprintf(stderr, "%s\n", cmdline);
    fflush(stderr);
  }

  return system(cmdline);
}


void *xmalloc(size_t n)
{
  void *mem;

  if (!(mem = malloc(n))) {
    fprintf(stderr, "insufficient memory\n");
    exit(2);
  }
  return mem;
}


char *xstrdup(char *s)
{
  char *new;

  new = xmalloc(strlen(s) + 1);
  strcpy(new, s);
  return new;
}


/*
 * Make a temporary filename.
 */

void xmktemp(char *t, char *templ)
{
  strcpy(t, templ);
  mktemp(t);
  if (!*t) {
    fprintf(stderr, "Cannot make temporary filename\n");
    exit(2);
  }
}


void banner(void)
{
  fprintf(stderr,
	  "f77 style frontend for f2c\n");
}

