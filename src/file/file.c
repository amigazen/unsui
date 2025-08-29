
#define VERSION "V1.2"
/*
 * file  - a Unix-like utility to determine the type of a file
 *
 * Copyright 1989 Edwin Hoogerbeets
 *
 * This code is freely redistributable as long as no charge other than
 * reasonable copying fees is levied for it.
 *
 *
 * Usage: file [-h | -v] file [file ...]
 *
 */

/*
 * Enhanced and converted to compile under SAS/C 6.0  by :-
 *
 * gduncan@philips.philips.oz.au
 *
 * Changes:-
 *
 * lharc/lz files recognised ( -lh5- )
 * GIF format recognised
 * -h option added
 * test logic improved a bit
 *
 * N.B. this source file formatted with GNU indent , option -gnu
 */


#include <stdlib.h>
#include <stdio.h>
#include <dos.h>
#include <ctype.h>
#include <exec/types.h>
#include <string.h>
#include <exec/memory.h>
#include <libraries/dosextens.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <clib/dos_protos.h>
#include <math.h>


typedef struct FileLock FILELOCK;
typedef struct FileInfoBlock FILEINFOBLOCK;

typedef struct
{
  int length;
  char *name;
  char *pattern;
} PATTERN;

typedef struct
{
  int offset;
  int length;
  char *name;
  char *pattern;
} BPATTERN;

#undef  FALSE
#undef TRUE
typedef enum
{
  FALSE = 0, TRUE
} BOOLEAN;

#include "file_protos.h"

#define FIBSIZE (long) sizeof(struct FileInfoBlock)
#define toint(a) (int)((a) - '0')
#define BLOCKSIZE 512

/***** #define BLOCKSIZE 484L              /* almost size of one disk block */


/* for binary files that start with a magic number */

BPATTERN bmagic[] =
{
  0, 4, "Amiga load file", "\x00\x00\x03\xf3",
  0, 4, "Amiga object file", "\x00\x00\x03\xe7",
  0, 2, "Amiga run-time library", "\xec\x62",

  0, 2, "Manx 3.6 run-time library", "\x61\x6a",
  0, 2, "Manx 3.6 object file", "\x41\x4a",
  0, 2, "Manx 3.4 object file", "\x6a\x67",

  0, 27, "TeX device independent output file",
  "\xf7\x02\x01\x83\x92\xc0"
  "\x1c\x3b\x00\x00\x00\x00"
  "\x03\xe8\x1b\x20\x54\x65"
  "\x58\x20\x6f\x75\x74\x70"
  "\x75\x74\x20",

  0, 2, "Amiga icon .info file", "\xe3\x10",
  0, 4, "Amiga .info file", "\xf3\x4c\x00\x12",
  0, 2, "Amiga .font file", "\x0f\x00",

  0, 2, "SEA ARC compressed archive", "\x1a\x08",

  2, 5, "lharc archive (lh0 - uncompressed)", "-lh0-",
  2, 5, "lharc archive (lh1 compression)", "-lh1-",
  2, 5, "lz  archive (lh5 compression)", "-lh5-",
  0, 3, "GIF file", "GIF",
  NULL, NULL, NULL, NULL,
};

/*
 * for ascii files that start with a magic token
 */
PATTERN amagic[] =
{
  2, "UNIX sh script file", "#!",
  4, "AmigaDos execute script file", ".key",
  4, "AmigaDos execute script file", ".bra",
  4, "AmigaDos execute script file", ".ket",
  NULL, NULL, NULL,
};

/*
 *	patterns to search for in ascii files
 */

PATTERN asearch[] =
{

  14, "LaTeX source code", "\\documentstyle",
  6, "TeX source code", "\n\\",

  2, "C++ source code", "//",
  2, "C++ source code", "::",
  8, "C++ source code", "iostream.h ",
  5, "C++ source code", "public:",

  2, "C source code", "/*",
  2, "C source code", "{\n",
  8, "C source code", "\ntypedef",
  4, "C source code", "int ",
  5, "C source code", "\n#inc",
  5, "C source code", "\n#def",

  5, "68K ASM source", "\tjsr",	/* common instrs	*/
  3, "68K ASM source", "\tlea",
  6, "68K ASM source", "move.l",

  2, "PASCAL source code", ":=",

  21, "Modula II source code", "IMPLEMENTATION MODULE",
  17, "Modula II source code", "DEFINITION MODULE",

  1, "yacc input file", "\n%TOKEN",
  1, "yacc or lex input file", "\n%%",

  1, "shell commands", "\nalias",
  1, "shell commands", "\nAlias",
  1, "shell commands", "\nset",

  1, "commands text", "\nrun",
  1, "commands text", "\nRun",
  1, "uuencoded file", "\nbegin ",
  NULL, NULL, NULL,
};

PATTERN IFFforms[] =
{
  4, "IFF interleave bit map file", "ILBM",
  4, "IFF Amiga compressed bit map files", "ACBM",
  4, "IFF anim format file", "ANIM",
  4, "IFF instrument file", "8SVX",
  4, "IFF simple music file", "SMUS",
  NULL, NULL, NULL,
};

PATTERN compress =
{
  2, "block compressed %d bit code data", "\x1f\x9d",
};

PATTERN zoo =
{
  4, "Zoo archive", "ZOO ",
};


/**************************************************************************
 *
 *	main ()
 *
 */

main (int argc, char **argv)

{
  int j;
  char *myname;
  char *q = argv[0];

  if (argc <= 1)
    {
      usage (q);
      exit (0);
    }
  myname = basename (q);

  /*
   *	loop through the file list, checking type
   */
  for (j = 1; j < argc; j++)
    {
      filetype (myname, argv[j]);
    }
}

/**************************************************************************
 *
 *	type()
 *
 */

void
type (char *ty, char *name)
{
  printf ("%-25s %s\n", name, ty);
}

/**************************************************************************
 *
 *	memncmp()
 *
 */

int
memncmp (char *a, char *b, int length)
{
  int j;

  for (j = 0; j < length; j++)
    {
      if (a[j] != b[j])
	{
	  return (a[j] > b[j] ? -1 : 1);
	}
    }

  return (0);
}

/**************************************************************************
 *
 *	strrpbrk
 *
 */

char *
strrpbrk (char *str, char *charset)
{
  char *temp;
  extern char *strchr ();

  temp = str + strlen (str) - 1;

  while (temp != (str - 1) && !strchr (charset, *temp))
    --temp;

  return ((temp != (str - 1)) ? temp : NULL);
}

/**************************************************************************
 *
 *	basename ()
 *
 */

char *
basename (char *buf)
{
  char *foo = strrpbrk (buf, ":/");

  return (foo ? (foo + 1) : buf);
}

/**************************************************************************
 *
 *	filetype()
 *
 * find whether file is directory or real file
 *
 */

void
filetype (char *myname, char *filename)
{
  BPTR lock;
  FILEINFOBLOCK *fib;

  if (lock = Lock (filename, ACCESS_READ))
    {
      if (fib = (FILEINFOBLOCK *) AllocMem (FIBSIZE, MEMF_CLEAR))
	{
	  Examine (lock, fib);

	  dofile (myname, filename, fib);

	  UnLock (lock);
	  FreeMem (fib, FIBSIZE);

	}
      else
	{
	  UnLock (lock);
	  fprintf (stderr, "%s: not enough memory!\n", myname, filename);
	  exit (-1);
	}
    }
  else
    {
      fprintf (stderr, "%s: could not access file %s\n", myname, filename);
    }
}

/**************************************************************************
 *
 *	dofile()
 *
 */

/* find what type of file filename is */

void
dofile (char *myname, char *filename, FILEINFOBLOCK * fib)
{
  char *f = &fib->fib_FileName[0];

  if (fib->fib_DirEntryType > 0)
    {
      type ("directory", f);
    }
  else if (fib->fib_Size == 0)
    {
      type ("empty", f);
    }
  else
    {
      char *buf;
      long filehandle;

      if (!(filehandle = Open (filename, MODE_OLDFILE)))
	{
	  fprintf (stderr, "%s: could not open file %s\n", myname, filename);
	  return;
	}

      if (!(buf = (char *) AllocMem (BLOCKSIZE + 1, MEMF_PUBLIC)))
	{
	  fprintf (stderr, "%s: not enough memory\n", myname);
	  Close (filehandle);
	  return;
	}

      if (!Read (filehandle, buf, BLOCKSIZE))
	{
	  fprintf (stderr, "%s: read error on file %s\n", myname, f);
	  FreeMem (buf, BLOCKSIZE + 1);
	  Close (filehandle);
	  return;
	}

      matchtype (myname, buf, f, fib);

      Close (filehandle);
      FreeMem (buf, BLOCKSIZE + 1);
    }
}

/**************************************************************************
 *
 *	istextfile()
 *
 */


BOOLEAN
istextfile (UBYTE * buf, int len)
{
  int j;
  int bad_cnt = 0;

  for (j = 0; j < (len - 1); j++, buf++)
    {
      if (*buf == 0xA9)
	continue;		/* Copyright */

      if (isascii (*buf) == FALSE)
	{
	  return (FALSE);
	}
      else
	{
	  switch (*buf)
	    {
	    case '\n':
	    case '\r':
	    case '\f':
	    case '\t':
	    case '\b':
	    case '\0':

	      break;

	    default:
	      if (isprint (*buf))
		break;
	      else
		++bad_cnt;
	      break;
	    }
	}
    }
  if (bad_cnt)
    return (FALSE);
  else
    return (TRUE);
}

/**************************************************************************
 *
 *	matchtype()
 *
 */

void
matchtype (char *myname, char *buf, char *file, FILEINFOBLOCK * fib)
{
  int j;
  int len = (fib->fib_Size < BLOCKSIZE) ? fib->fib_Size : BLOCKSIZE;

  /*
   * check magic strings (charas which start a line)
   */

  j = 0;
  while (amagic[j].length)
    {
      if (!memncmp (amagic[j].pattern, buf, amagic[j].length))
	{
	  type (amagic[j].name, file);
	  return;
	}
      ++j;
    }

  /*
   *	see if a text file, or not
   */
  if (istextfile (buf, len) == TRUE)
    {
      /*
       * check for tell-tale strings; this is rough and ready!
       */

      j = 0;

      while (asearch[j].length)
	{
	  if (search (asearch[j].pattern, buf, len) == TRUE)
	    {
	      type (asearch[j].name, file);
	      return;
	    }
	  ++j;
	}

      j = 0;

      /* check script bit */
      if (fib->fib_Protection & (1 << 6))
	{
	  type ("AmigaDos script file", file);
	  return;
	}

      type ("text", file);
    }
  else
    /* deemed not an ASCII text file */
    {
      j = 0;

      /* check magic numbers */
      while (bmagic[j].length)
	{
	  if (!memncmp (bmagic[j].pattern, buf + bmagic[j].offset, bmagic[j].length))
	    {
	      type (bmagic[j].name, file);
	      return;
	    }
	  ++j;
	}

      /* check for IFF forms -- assume FORM is first header block */
      if (!memncmp ("FORM", buf, 4))
	{
	  char buffer[40];

	  j = 0;
	  buffer[0] = '\0';

	  while (IFFforms[j].length)
	    {
	      if (!memncmp (IFFforms[j].pattern, &buf[8], IFFforms[j].length))
		{
		  type (IFFforms[j].name, file);
		  return;
		}
	      ++j;
	    }
	  sprintf (buffer, "IFF form %c%c%c%c", buf[8], buf[9], buf[10], buf[11]);
	  type (buffer, file);
	  return;
	}

      if (!memncmp (compress.pattern, buf, compress.length))
	{
	  char buffer[40];

	  sprintf (buffer, compress.name, buf[2] & 0x0f);
	  type (buffer, file);
	  return;
	}

      if (!memncmp (zoo.pattern, buf, zoo.length))
	{

	  /* make a string out of ZOO x.xx Archive\0 to print out */
	  buf[16] = '\0';

	  type (buf, file);
	  return;
	}

      type ("binary file  ?", file);
    }
}

/**************************************************************************
 *
 *	search ()
 *
 */

BOOLEAN
search (char *pat, char *text, int len)
{
  int j = 0;
  int patlen = strlen (pat);

  while (memncmp (pat, &text[j], patlen) && (j < len))
    {
      j++;
    }

  return (j < len ? TRUE : FALSE);
}

/**************************************************************************
 *
 *	usage
 *
 */

void
usage (char *name)
{
  fprintf (stderr, "%s  : Compiled under SAS/C 6.0 on %s\n",
	   VERSION, __DATE__);
  fprintf (stderr, "usage: %s  file [file...]\n", name);
}
