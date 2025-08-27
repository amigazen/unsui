/* fileio.c: routines used by TeX, Metafont, and BibTeX.  */

#ifndef BibTeX
#define EXTERN extern	/* Don't instantiate date here.  */
#ifdef TeX
#include "texd.h"
#else
#include "mfd.h"
#endif
#endif /* not BibTeX */

#include "fileio.h"	/* "../common/fileio.h" */

#ifdef AMIGA
# undef message
# include <exec/types.h>
# include <workbench/icon.h>
# include <workbench/workbench.h>
# include <clib/exec_protos.h>
# include <clib/icon_protos.h>
# include <pragmas/exec_pragmas.h>
# include <pragmas/icon_pragmas.h>
  extern int		create_info_file;	/* definiert in init.c */
  extern char		info_file_name[];
  extern struct Library *SysBase;
  extern struct Library *IconBase;
#endif


/* This is defined in ./texmf.c.  */
extern void funny_core_dump ();

#ifdef TeX
/* See comments in ctex.ch for why we need this.  */
#if 0	/* (br) not necessary */
extern integer tfmtemp;
#endif
#endif

extern long crlfflag;		/* write \n as \r\n in textfiles ? */

#ifdef BibTeX
/* See comments in bibtex.ch for why we need these.  */
FILE *standardinput = stdin;
FILE *standardoutput = stdout;

/* Because we don't generate a .h file with all the global definitions
   for BibTeX, as we do with TeX and Metafont, we must declare all these
   variables.  */
extern char nameoffile[];
extern integer namelength;
#endif



/* This constant is used in BibTeX, when opening the top-level .aux file.  */
#define NO_FILE_PATH -1

/* Open an input file F, using the path PATHSPEC (the values are defined
   in ./extra.h).  The filename is in `nameoffile', as a Pascal string.
   We return whether or not the open succeeded.  If it did, we also set
   `namelength' to the length of the full pathname that we opened.  */

boolean
open_input (f, path_index)
  FILE **f;
  int path_index;
{
#ifdef BSD
#ifndef BibTeX
  /* This only applies if a preloaded TeX (or Metafont) is being made;
     it allows for automatic creation of the core dump (typing ^\
     requires manual intervention).  */
  if (path_index == TEXINPUTPATH
      && strncmp (nameoffile+1, "HackyInputFileNameForCoreDump.tex", 33) == 0)
    funny_core_dump ();
#endif
#endif /* BSD */

#ifdef BibTeX
  if (path_index == NO_FILE_PATH)
    {
      unsigned temp_length;

      /* We can't use `make_c_string' or `make_pascal_string', since
         `nameoffile' is an array, not a pointer.  */
      end_with_null (nameoffile + 1);
#ifdef atarist
      replace_slash(nameoffile + 1);
#endif
      *f = fopen (nameoffile + 1, "r");
#ifdef atarist
      replace_backslash(nameoffile + 1);
#endif
      temp_length = strlen (nameoffile + 1);
      end_with_space (nameoffile + 1);
      if (*f != NULL)
        {
          namelength = temp_length;
          return true;
        }
      else
        return false;
    }
#endif

  /* end_with_null (nameoffile + 1); */ /* is done in testreadaccess */

  if ( (*f = testreadaccess (nameoffile + 1 , path_index)) != NULL )
    {
#if 0
      *f = checked_fopen (nameoffile, "r");
#else
      end_with_null (nameoffile + 1);
#endif

      
      /* If we found the file in the current directory, don't leave the
         `./' at the beginning of `nameoffile', since it looks dumb when
         TeX says `(./foo.tex ... )', and analogously for Metafont.  */

      if (nameoffile[1] == '.' && nameoffile[2] == '/') {
#if 1
	char *name = &nameoffile[1];

	while( 1 ) {
	  *name = *(name + 2);
	  if( *name == '\0' )
	    break;
	  name++;
	}
#else
        unsigned i;
        for (i = 1; nameoffile[i]; i++)
          nameoffile[i] = nameoffile[i+2];
#endif
      }

      namelength = strlen (nameoffile + 1);

#if 0
printf("DEBUG: name >%s<  len %d\n", nameoffile+1, namelength);
#endif

      /* `nameoffile' is in an anomalous state: it still begins with a
         space, but now it is terminated with a null.  */
      end_with_space (nameoffile + 1);
      
#ifdef TeX
      /* If we just opened a TFM file, we have to read the first byte,
         since TeX wants to look at it.  */
#if 0	/* (br) not necessary for new read_font_info() */
      if (path_index == TFMFILEPATH)
        tfmtemp = getc (*f);
#endif
#endif

      return true;
    }
  else {
    /* end_with_space (nameoffile + 1); */
    return false;
  }
}


/* Open an output file F either in the current directory or in
   $TEXMFOUTPUT/F, if the environment variable `TEXMFOUTPUT' exists.
   (Actually, this applies to the BibTeX output files, also, but
   `TEXMFBIBOUTPUT' was just too long.)  The filename is in the global
   `nameoffile', as a Pascal string.  We return whether or not the open
   succeeded.  If it did, the global `namelength' is set to the length
   of the actual filename.  */

boolean
open_output (f
#ifdef atarist
		, txtflag
#endif
			  )
  FILE **f;
#ifdef atarist
  int txtflag;		/* output for textfile ?? */
#endif
{
  unsigned temp_length;

  /* We can't use `checked_fopen' here, since that routine aborts if the
     file can't be opened.  We also can't use `make_c_string' or
     `make_pascal_string', since `nameoffile' is an array, not a
     pointer.  */
  end_with_null (nameoffile + 1);
#ifdef atarist
  replace_slash(nameoffile + 1);
#endif

  *f = fopen (nameoffile + 1,
#ifdef atarist
				((txtflag && crlfflag) ? "wt" : "wb")
#else
				"w"
#endif
					);
  
  if (*f == NULL)
    {
      /* Can't open in the current directory.  Try the directory
         specified by the environment variable.  */
      char *temp_dir = getenv ("TEXMFOUTPUT");

      if (temp_dir != NULL && chdir (temp_dir) == 0)
        *f = fopen (nameoffile + 1,
#ifdef atarist
				((txtflag && crlfflag) ? "wt" : "wb")
#else
				"w"
#endif
					);
    }

#ifdef AMIGA
    if (*f != NULL) {
      struct DiskObject *DiskO;
      char outname[150];
      char *ptr = strrchr(nameoffile+1, '.');
      
      if (create_info_file && ptr != NULL && !strcmp(ptr, ".dvi")) {
	/* Test, ob ein DVI File geoeffnet wird... */
        
        strcpy(outname, nameoffile + 1);
        strcat(outname, ".info");
        
        if (access(outname, 4)) {	/* not read: info-File darf nicht schon existieren */
	  if ((ptr = strrchr(&(info_file_name[0]), '.')) != NULL && stricmp(ptr, ".info") == 0) {
	    *ptr = '\0';	/* weg mit dem ".info" */
	  }

	  if ((IconBase = OpenLibrary(ICONNAME, 0)) != NULL) {
	    DiskO = GetDiskObject(info_file_name);
	    if (DiskO != NULL) {
 	      (void)PutDiskObject(nameoffile+1, DiskO);	/* entweder es geht oder nicht..egal */
 	      FreeDiskObject(DiskO);
	    }
	    CloseLibrary(IconBase);
	    IconBase = 0;
	  }
	}
      }
    }
#endif

  temp_length = strlen (nameoffile + 1);
#ifdef atarist
  replace_backslash(nameoffile + 1);
#endif
  end_with_space (nameoffile + 1);

  if (*f != NULL)
    {
      namelength = temp_length;
      return true;
    }
  
  else
    return false;
}

/* -- eof -- */
