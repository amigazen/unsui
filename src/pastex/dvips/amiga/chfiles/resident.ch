
@x
/*   For use with emTeX set FONTPATH to "TEXTFM"
 */
#ifndef FONTPATH
#define FONTPATH "TEXFONTS"
#endif
@y
@z

@x
#ifdef AMIGA
#include <signal.h>
#include "resident_protos.h"
@y
#include "resident_protos.h"
@z

@x
#include "flib_protos.h"
@y
#include "flib_protos.h"
#include "evpaths.h"
@z

@x
static char *getpath(char *,char *);
static char *getenvup(char *,char *);
static char *concat3(char *,char *,char *);
static void next_component (char [], char **);
#else
extern void error() ;
extern integer scalewidth() ;
extern int tfmload() ;
extern FILE *search() ;
extern shalfword pkbyte() ;
extern integer pkquad() ;
extern integer pktrio() ;
extern Boolean pkopen() ;
extern char *getenv() ;
extern char *newstring() ;
extern int add_header() ;
extern int add_name() ;
extern char *get_name() ;
extern int system() ;
extern void handlepapersize() ;
extern void checkstrings() ;
void getpsinfo() ;
extern void *revlist() ;
#endif
@y

static char *getpath(char *,char *);
static char *getenvup(char *,char *);

extern struct EnvVarPath *tfm_var,
			 *vf_var,
			 *fig_var,
			 *pict_var,
			 *header_var,
			 *config_var;
@z

@x
#ifdef SEARCH_SUBDIRECTORIES
extern char *fontsubdirpath ;
#endif
@y
@z

@x
#ifdef FONTLIB
extern char *flipath, *fliname ;
#endif
@y
#ifdef FONTLIB
extern char *flipath, *fliname ;
extern struct EnvVarPath *fli_var;
#endif
@z

@x
bad_config() {
   extern void exit() ;

   error("Error in config file:") ;
   (void)fprintf(stderr, "%s\n", was_inline) ;
   exit(1) ;
@y
bad_config() {
   extern void exit() ;
   extern void Free_EnvVars(void);

   error("Error in config file:") ;
   (void)fprintf(stderr, "%s\n", was_inline) ;
   Free_EnvVars();
   exit(1) ;
@z

@x
void
getdefaults(s)
char *s ;
{
   FILE *deffile ;
   char PSname[300] ;
   register char *p ;
   int i, j ;
   integer hsiz, vsiz ;
   char *d = configpath ;
   int canaddtopaper = 0 ;

   if (printer == NULL) {
      if (s) {
         strcpy(PSname, s) ;
      } else {
#ifndef VMCMS  /* IBM: VM/CMS - don't have home directory on VMCMS */
#ifndef MVSXA
         d = "~" ;
#endif
#endif  /* IBM: VM/CMS */
         strcpy(PSname, DVIPSRC) ;
      }
   } else {
#if defined(MSDOS) || defined(OS2)
      strcpy(PSname, printer) ;
      strcat(PSname, ".cfg") ;
#else
      strcpy(PSname, "config.") ;
      strcat(PSname, printer) ;
#endif
   }
   if ((deffile=search(d,PSname,READ))!=NULL) {
@y
void
getdefaults(s)
char *s ;
{
   FILE *deffile ;
   char PSname[300] ;
   register char *p ;
   int i, j ;
   integer hsiz, vsiz ;
   int canaddtopaper = 0 ;

   if (printer == NULL) {
      if (s)
         strcpy(PSname, s) ;
      else
         strcpy(PSname, DVIPSRC) ;
   } else {
      strcpy(PSname, "config.") ;
      strcat(PSname, printer) ;
   }

   if ((deffile=search(config_var,PSname,READ))!=NULL) {
@z

@x
#ifdef SHORTINT
         if (sscanf(was_inline+1, "%ld", &pagecopies) != 1) bad_config() ;
#else
         if (sscanf(was_inline+1, "%d", &pagecopies) != 1) bad_config() ;
#endif
@y
         if (sscanf(was_inline+1, "%d", &pagecopies) != 1) bad_config() ;
@z

@x
#ifdef SHORTINT
         if (sscanf(was_inline+1, "%ld", &swmem) != 1) bad_config() ;
#else   /* ~SHORTINT */
         if (sscanf(was_inline+1, "%d", &swmem) != 1) bad_config() ;
#endif  /* ~SHORTINT */
@y
         if (sscanf(was_inline+1, "%d", &swmem) != 1) bad_config() ;
@z

@x
      name = psmapfile ;
   if ((deffile=search(configpath, name, READ))!=NULL) {
@y
      name = psmapfile ;
   if ((deffile=search(config_var, name, READ))!=NULL) {
@z

@x
#ifdef SEARCH_SUBDIRECTORIES
#ifndef AMIGA
static char *concat3();
#endif
#endif
@y
@z

@x
      tfmpath = getenvup(FONTPATH, tfmpath) ;
      vfpath = getenvup("VFFONTS", vfpath) ;
      pictpath = getenvup("TEXPICTS", pictpath) ;
      figpath = getenvup("TEXINPUTS", figpath) ;
      headerpath = getenvup("DVIPSHEADERS", headerpath) ;
@y
      Init_EnvVarPath(tfm_var,    tfmpath,    ENVPATH_DEFSTR);
      Init_EnvVarPath(vf_var,     vfpath,     ENVPATH_DEFSTR);
      Init_EnvVarPath(pict_var,   pictpath,   ENVPATH_DEFSTR);
      Init_EnvVarPath(fig_var,    figpath,    ENVPATH_DEFSTR);
      Init_EnvVarPath(header_var, headerpath, ENVPATH_DEFSTR);
#ifdef FONTLIB
      Init_EnvVarPath(fli_var,    flipath,    ENVPATH_DEFSTR);
#endif
@z

@x
#ifdef SEARCH_SUBDIRECTORIES
#ifdef AMIGA
/* TEXFONTS and TEXFONTS_SUBDIR are usually setted for TeX tfm files;
   TEXFONTS will be used as path for PK fonts only if pkpath is NULL */

      else if (getenv(FONTPATH) && !pkpath)
         pkpath = getenvup(FONTPATH, pkpath) ;
#else
      else if (getenv(FONTPATH))
         pkpath = getenvup(FONTPATH, pkpath) ;
#endif
      if (getenv ("TEXFONTS_SUBDIR"))
         fontsubdirpath = getenvup ("TEXFONTS_SUBDIR", fontsubdirpath);
      {
         char pathsep[2] ;
         char *do_subdir_path();
         char *dirs = do_subdir_path (fontsubdirpath);
         /* If the paths were in dynamic storage before, that memory is
            wasted now.  */
         pathsep[0] = PATHSEP ;
         pathsep[1] = '\0' ;
#ifndef AMIGA
         tfmpath = concat3 (tfmpath, pathsep, dirs);
         pkpath = concat3 (pkpath, pathsep, dirs);
#else
         if (*dirs)
            tfmpath = concat3 (tfmpath, pathsep, dirs);
#endif /* AMIGA */
      }
#endif
   } else
      configpath = getenvup("TEXCONFIG", configpath) ;
}

#ifdef SEARCH_SUBDIRECTORIES

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#if defined(SYSV) || defined(AMIGA)
#include <dirent.h>
typedef struct dirent *directory_entry_type;
#else
#include <sys/dir.h>
typedef struct direct *directory_entry_type;
#endif

/* Declare the routine to get the current working directory.  */

#ifndef HAVE_GETCWD
extern char *getwd ();
#define getcwd(b, len)  ((b) ? getwd (b) : getwd (xmalloc (len)))
#else
#ifdef ANSI
extern char *getcwd (char *, int);
#else
extern char *getcwd ();
#endif /* not ANSI */
#endif /* not HAVE_GETWD */

#if defined(SYSV) || defined(VMS) || defined(MSDOS) || defined(OS2) || defined(ATARIST) || defined(AMIGA)
#define MAXPATHLEN (256)
#else   /* ~SYSV */
#include <sys/param.h>          /* for MAXPATHLEN */
#endif

extern void exit() ;
extern int chdir() ;

/* Memory operations: variants of malloc(3) and realloc(3) that just
   give up the ghost when they fail.  */

#ifndef AMIGA
extern char *realloc ();
#endif

char *
xmalloc (size)
  unsigned size;
{
  char *mem = malloc (size);
  
  if (mem == NULL)
    {
      fprintf (stderr, "! Cannot allocate %u bytes.\n", size);
      exit (10);
    }
  
  return mem;
}


char *
xrealloc (ptr, size)
  char *ptr;
  unsigned size;
{
  char *mem = realloc (ptr, size);
  
  if (mem == NULL)
    {
      fprintf (stderr, "! Cannot reallocate %u bytes at %x.\n", size, (int)ptr);
      exit (10);
    }
    
  return mem;
}


/* Return, in NAME, the next component of PATH, i.e., the characters up
   to the next PATHSEP.  */
   
static void
next_component (name, path)
  char name[];
  char **path;
{
  unsigned count = 0;
  
  while (**path != 0 && **path != PATHSEP)
    {
      name[count++] = **path;
      (*path)++; /* Move further along, even between calls.  */
    }
  
  name[count] = 0;
  if (**path == PATHSEP)
    (*path)++; /* Move past the delimiter.  */
}


#ifndef _POSIX_SOURCE
#ifndef S_ISDIR
#define S_ISDIR(m) ((m & S_IFMT) == S_IFDIR)
#endif
#endif

/* Return true if FN is a directory or a symlink to a directory,
   false if not. */

int
is_dir (fn)
  char *fn;
{
  struct stat stats;

  return stat (fn, &stats) == 0 && S_ISDIR (stats.st_mode);
}


static char *
concat3 (s1, s2, s3)
  char *s1, *s2, *s3;
{
  char *r = xmalloc (strlen (s1) + strlen (s2) + strlen (s3) + 1);
  strcpy (r, s1);
  strcat (r, s2);
  strcat (r, s3);
  return r;
}


/* DIR_LIST is the default list of directories (colon-separated) to
   search.  We want to add all the subdirectories directly below each of
   the directories in the path.
     
   We return the list of directories found.  */

char *
do_subdir_path (dir_list)
  char *dir_list;
{
  char *cwd;
  unsigned len;
  char *result = xmalloc ((unsigned)1);
  char *temp = dir_list;
  char dirsep[2] ;

  dirsep[0] = DIRSEP ;
  dirsep[1] = '\0' ;

  /* Make a copy in writable memory.  */
  dir_list = xmalloc (strlen (temp) + 1);
  strcpy (dir_list, temp);
  
  *result = 0;

  /* Unfortunately, we can't look in the environment for the current
     directory, because if we are running under a program (let's say
     Emacs), the PWD variable might have been set by Emacs' parent
     to the current directory at the time Emacs was invoked.  This
     is not necessarily the same directory the user expects to be
     in.  So, we must always call getcwd(3) or getwd(3), even though
     they are slow and prone to hang in networked installations.  */
  cwd = getcwd (NULL, MAXPATHLEN + 2);
  if (cwd == NULL)
    {
      perror ("getcwd");
      exit (errno);
    }

  do
    {
      DIR *dir;
      directory_entry_type e;
      char dirname[MAXPATHLEN];

      next_component (dirname, &dir_list);

      /* All the `::'s should be gone by now, but we may as well make
         sure `chdir' doesn't crash.  */
      if (*dirname == 0) continue;

      /* By changing directories, we save a bunch of string
         concatenations (and make the pathnames the kernel looks up
         shorter).  */
#ifdef AMIGA
      signal(SIGINT, SIG_IGN); /* ignore user break, until current
                                  is restored */
/* we must check for current dir "." in paths */
      if (chdir (dirname) != 0 && strcmp(dirname,".") != 0) continue;
#else
      if (chdir (dirname) != 0) continue;
#endif

#ifdef AMIGA
      dir = opendir ("");
#else
      dir = opendir (".");
#endif
      if (dir == NULL) continue;
#ifdef AMIGA
#ifdef DEBUG
      if (dd(D_PATHS))
        fprintf(stderr,"Searching subdir in <%s>...\n",dirname);
#endif
#endif /* AMIGA */

      while ((e = readdir (dir)) != NULL)
        {
#ifdef AMIGA
          if (is_dir (e->d_name))
            {
              char *found;
/* add dirsep only for non-logical paths or not current dir */
              if (dirname[strlen(dirname)-1] == ':' || dirname[strlen(dirname)-1] == DIRSEP)
                    found = concat3 (dirname, "", e->d_name);
              else if(!strcmp (dirname,".") )
                    found = concat3 ("","",e->d_name);
              else
                    found = concat3 (dirname, dirsep, e->d_name);
#else
          if (is_dir (e->d_name) && strcmp (e->d_name, ".") != 0
              && strcmp (e->d_name, "..") != 0)
            {
              char *found = concat3 (dirname, dirsep, e->d_name);
#endif /* AMIGA */
#ifdef AMIGA
#ifdef DEBUG
      if (dd(D_PATHS))
        fprintf(stderr,"Found subdir <%s>\n",e->d_name);
#endif
#endif /* AMIGA */

              result = xrealloc (result, strlen (result) + strlen (found) + 2);

              len = strlen (result);
              if (len > 0)
                {
                  result[len] = PATHSEP;
                  result[len + 1] = 0;
                }
              strcat (result, found);
              free (found);
            }
        }
      closedir (dir);

      /* Change back to the current directory, in case the path
         contains relative directory names.  */
      if (chdir (cwd) != 0)
        {
          perror (cwd);
          exit (errno);
        }
#ifdef AMIGA
  signal(SIGINT, SIG_DFL); /* restore user break */
#endif
    }
  while (*dir_list != 0);
  
  return result;
}
#endif /* SEARCH_SUBDIRECTORIES */
@y

   } else
      Init_EnvVarPath(config_var, configpath, ENVPATH_DEFSTR);
}
@z
