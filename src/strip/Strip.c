/*
 *  Strip - Strips debug and symbol hunks from load files.
 *  Copyright (C) 1994 Torsten Poulin
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *             The author can be contacted by mail at
 *               Torsten Poulin
 *               Frankrigsgade 50, 5, 508
 *               DK-2300 Copenhagen S
 *               Denmark
 *             or via email: torsten@diku.dk
 *
 * $Id: Strip.c 37.2 1994/08/28 11:24:06 torsten Rel $
 * $Log: Strip.c $
 * Revision 37.2  1994/08/28  11:24:06  torsten
 * Added a KEEP switch that saves the original file,
 * if no TO destination was given.
 * (Breaks if the filename is 30 chars and already ends
 * with ".orig" - needs to be fixed).
 *
 * Revision 37.3  2025/01/XX  hybrid
 * Added hybrid Amiga/POSIX command-line support
 *
 * Revision 37.1  1994/08/24  23:52:43  torsten
 * Initial revision.
 *
 */

#include <exec/types.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <dos/dostags.h>
#include <dos/dosasl.h>
#include <dos/rdargs.h>

#include <proto/exec.h>
#include <proto/dos.h>

#include <string.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "strip_rev.h"

/* Common includes for hybrid parsing */
#include "common.h"
#include "getopt.h"

#define PROGNAME "strip"
#define TEMPLATE "FILE/A,TO,QUIET/S,KEEP/S,POSIX/K/F"

#define MIN(a,b) ((a)<(b)?(a):(b))

/* For ReadArgs template */
enum {
    ARG_FILE,
    ARG_TO,
    ARG_QUIET,
    ARG_KEEP,
    ARG_POSIX,
    ARG_COUNT
};

#define LENGTH_MASK 0x3fffffff
#define BUFLEN 4096		/* long words !! */
#define MAXPATH 255		/* assumption */

typedef struct {
  struct FileInfoBlock fib;
  struct {
    STRPTR pathname, destination;
    LONG quiet, keep;
  } args;
  ULONG symhunks, dbghunks;
  BPTR in, out;
  LONG *ibptr, *ibend;
  LONG *obptr;
  LONG ib[BUFLEN];
  LONG ob[BUFLEN];		/* length(ob) >= length(ib) !! */
  UBYTE path[MAXPATH+1];
} Global;


VOID message(enum msgs, Global *);
LONG strip(Global *);
STRPTR destname(STRPTR, Global *);
LONG handlehunk(enum hunktypes, Global *);
LONG do_code_data(enum hunktypes, Global *);
LONG do_reloc(enum hunktypes, Global *);
LONG do_symbol(Global *);
LONG do_debug(Global *);
LONG do_header(enum hunktypes, Global *);
VOID statistics(Global *);
BOOL readLong(LONG *, Global *);
BOOL copyNLongs(LONG, Global *);
BOOL skipNLongs(LONG, Global *);
VOID writeLong(LONG, Global *);
VOID writeNLongs(LONG, Global *);
BOOL flush(Global *);

/* Hybrid parsing function declarations */
int run_strip_logic(STRPTR pathname, STRPTR destination, LONG quiet, LONG keep, const char *program);

extern struct DosLibrary *DOSBase;

char const versionID[] = VERSTAG;


/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    STRPTR pathname = NULL;
    STRPTR destination = NULL;
    LONG quiet = FALSE;
    LONG keep = FALSE;
    char *program;
    int ret_code = RETURN_OK;
    
    /* ReadArgs Path Variables */
    LONG arg_array[ARG_COUNT] = {0};
    struct RDArgs *rdargs = NULL;
    char *cmd_string = NULL;
    BOOL interactive_help = FALSE;
    
    /* POSIX/F Path Variables */
    char *posix_str;
    int new_argc;
    char *new_argv[MAX_TEMPLATE_ITEMS];
    int i;
    char initial_args_str[256];
    char user_input_buf[256];
    char *temp_str;
    size_t combined_len;

    if (argc < 1) {
        exit(RETURN_FAIL);
    }
    
    program = my_basename(argv[0]);

    if (argc == 1) {
        /* No arguments, show usage */
        fprintf(stderr, "Usage (POSIX): %s [OPTIONS] FILE [OUTPUT]\n", program);
        fprintf(stderr, "Usage (Amiga): %s FILE/A [TO] [QUIET/S] [KEEP/S]\n", program);
        fprintf(stderr, "               %s ? for template\n", program);
        fprintf(stderr, "OPTIONS:\n");
        fprintf(stderr, "  -s          strip all symbols and debug info (default)\n");
        fprintf(stderr, "  -o FILE     output file\n");
        fprintf(stderr, "  -k          keep original file with .orig extension\n");
        fprintf(stderr, "  -q          quiet mode\n");
        fprintf(stderr, "  -v, -h, -V  display this help and version\n");
        fprintf(stderr, "DESCRIPTION:\n");
        fprintf(stderr, "  Remove symbol and debug hunks from binary load files.\n");
        fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
        fprintf(stderr, "  Default: strip all symbols if no options specified.\n");
        return RETURN_FAIL;
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &pathname, &destination, &quiet, &keep, &i, program);
        return run_strip_logic(pathname, destination, quiet, keep, program);
        
    } else {
        /* --- READARGS PATH --- */
        for (i = 1; i < argc; i++) {
            if (strcmp(argv[i], "?") == 0) {
                interactive_help = TRUE;
                break;
            }
        }

        rdargs = AllocDosObject(DOS_RDARGS, NULL);
        if (!rdargs) {
            fprintf(stderr, "%s: out of memory for RDArgs\n", program);
            return RETURN_FAIL;
        }

        if (interactive_help) {
            /* Initialize buffers */
            initial_args_str[0] = '\0';
            user_input_buf[0] = '\0';

            /* Build a string from any args that are NOT '?' */
            temp_str = build_command_string(argc, argv, "?");
            if (temp_str) {
                strncpy(initial_args_str, temp_str, 255);
                free(temp_str);
            }

            /* Print template and prompt for more input */
            printf("%s: ", TEMPLATE);
            fflush(stdout);
            if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                /* Combine initial args with the new user input */
                combined_len = strlen(initial_args_str) + strlen(user_input_buf) + 2;
                cmd_string = malloc(combined_len);
                if (cmd_string) {
                    strcpy(cmd_string, initial_args_str);
                    if (initial_args_str[0] != '\0' && user_input_buf[0] != '\n') {
                        strcat(cmd_string, " ");
                    }
                    strcat(cmd_string, user_input_buf);
                }
            } else {
                cmd_string = strdup(initial_args_str);
                if (cmd_string) strcat(cmd_string, "\n");
            }
        } else {
            /* Standard case: build command string from all args */
            cmd_string = build_command_string(argc, argv, NULL);
        }

        if (!cmd_string) {
            fprintf(stderr, "%s: out of memory for command string\n", program);
            FreeDosObject(DOS_RDARGS, rdargs);
            return RETURN_FAIL;
        }

        /* Set up ReadArgs to parse from our string */
        rdargs->RDA_Source.CS_Buffer = cmd_string;
        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
        rdargs->RDA_Source.CS_CurChr = 0;
        rdargs->RDA_Flags |= RDAF_NOPROMPT;

        if (!ReadArgs(TEMPLATE, arg_array, rdargs)) {
            PrintFault(IoErr(), program);
            ret_code = RETURN_FAIL;
        } else {
            /* Check for POSIX/F override first */
            if (arg_array[ARG_POSIX]) {
                posix_str = (char *)arg_array[ARG_POSIX];

                /* Tokenize the string and build a new argv for getopt */
                new_argv[0] = program;
                new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

                parse_getopt_args(new_argc, new_argv, &pathname, &destination, &quiet, &keep, &i, program);
                ret_code = run_strip_logic(pathname, destination, quiet, keep, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_QUIET]) {
                    quiet = TRUE;
                }
                if (arg_array[ARG_KEEP]) {
                    keep = TRUE;
                }
                
                if (arg_array[ARG_FILE]) {
                    pathname = (char *)arg_array[ARG_FILE];
                }
                if (arg_array[ARG_TO]) {
                    destination = (char *)arg_array[ARG_TO];
                }
                
                if (pathname) {
                    ret_code = run_strip_logic(pathname, destination, quiet, keep, program);
                } else {
                    fprintf(stderr, "%s: no input file specified\n", program);
                    ret_code = RETURN_FAIL;
                }
            }
        }

        /* Clean up */
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
    }

    return ret_code;
}

/* Keep the old entrypoint for Amiga compatibility 
LONG __saveds
entrypoint(VOID)
{
    extern char **environ;
    return main(__argc, __argv);
}
*/

enum msgs {
  msg_err_eof,
  msg_notloadfile,
  msg_overlaid,
  msg_unknownhunk,
  msg_nothing,
  msg_pathtoolong,
  msg_cannotopenoutput
};

const char *messages[] = {
  "unexpected end of file",
  "not a load file",
  "overlaid load file - not stripped",
  "unrecognized hunk - not stripped",
  "no symbol or debug hunks - file not changed",
  "pathname too long",
  "cannot open output file"
};


VOID message(enum msgs msg, Global *g)
{
  printf("%s: %s\n", PROGNAME, messages[msg]);
}


enum hunktypes {
  hunk_code    = 0x03e9,
  hunk_data    = 0x03ea,
  hunk_bss     = 0x03eb,
  hunk_reloc32 = 0x03ec,
  hunk_symbol  = 0x03f0,
  hunk_debug   = 0x03f1,
  hunk_end     = 0x03f2,
  hunk_header  = 0x03f3,
  hunk_overlay = 0x03f5,
  hunk_break   = 0x03f6
};


LONG strip(Global *g)
{
  LONG rc = RETURN_OK, len;
  enum hunktypes hunk;
  STRPTR from, destination;
  BPTR olddir, dir;

  from = FilePart(g->args.pathname);
  if ((len = PathPart(g->args.pathname) - g->args.pathname) > MAXPATH) {
    message(msg_pathtoolong, g);
    return RETURN_FAIL;
  }

  if (len) memcpy(g->path, g->args.pathname, len);

  if (dir = Lock(g->path, SHARED_LOCK)) {
    olddir = CurrentDir(dir);

    if (g->in = Open(from, MODE_OLDFILE)) {

      destination = destname(from, g);
      if (g->out = Open(destination, MODE_NEWFILE)) {

	/*
	 * Ready, set, go ...
	 */

	if (readLong((LONG *) &hunk, g)) {
	  if (hunk != hunk_header) message(msg_notloadfile, g);
	  else
	    do {
	      rc = handlehunk(hunk, g);
	    } while (rc == RETURN_OK && readLong((LONG *) &hunk, g));
	  flush(g);
	  if (rc == RETURN_OK) statistics(g);
	}
	Close(g->out);

	/*
	 * Set the protection flags and the comment
	 */

	if (ExamineFH(g->in, &g->fib)) {
	  SetComment(destination, g->fib.fib_Comment);
	  SetProtection(destination,
			g->fib.fib_Protection & ~(FIBF_ARCHIVE));
	}
      }
      else {
	rc = RETURN_FAIL;
	message(msg_cannotopenoutput, g);
      }

      Close(g->in);
    }
    else rc = RETURN_FAIL;

    /*
     * If nothing was changed and no destination was
     * specified, we just remove the output file.
     * Ditto if something went wrong.
     */

    if ((!g->symhunks && !g->dbghunks && !g->args.destination)
	|| rc == RETURN_ERROR)
      DeleteFile(destination);
    else if (!g->args.destination) {
      /*
       * Otherwise we replace the original file
       * unless, of course, we were told to keep it...
       * (in which case we back it up with a .orig
       *  extension).
       */
      if (g->args.keep) {
	/* This is a bit ugly - reusing part
	 * of the FileInfoBlock; we ought to be
	 * ashamed...
	 */
	strcpy(g->fib.fib_FileName, from);
	if (strlen(g->fib.fib_FileName) > 25)
	  g->fib.fib_FileName[25] = '\0';
	strcat(g->fib.fib_FileName, ".orig");
	Rename(from, g->fib.fib_FileName);
      }
      else DeleteFile(from);
      Rename(destination, from);
    }

    dir = CurrentDir(olddir);
    UnLock(dir);
  }
  else rc = RETURN_ERROR;

  return rc;
}


STRPTR destname(STRPTR from, Global *g)
{
  STRPTR name = g->args.destination;
  BPTR lock;

  if (!name) {
    /*
     * Create a reasonably unlikely file name
     */
    mySPrintf(g, g->path, "_strip!%lx!", FindTask(NULL));
    name = g->path;
  }

  if (lock = Lock(name, SHARED_LOCK)) {
    if (Examine(lock, &g->fib) &&
	(g->fib.fib_DirEntryType == ST_ROOT ||
	 g->fib.fib_DirEntryType == ST_USERDIR ||
	 g->fib.fib_DirEntryType == ST_LINKDIR)) {
      /*
       * Name is a directory.
       * Tack on the name of the original file:
       */
      strcpy(g->path, name);
      AddPart(g->path, from, MAXPATH);
      name = g->path;
    }
    UnLock(lock);
  }
  return name;
}


LONG handlehunk(enum hunktypes hunk, Global *g)
{
  LONG rc = RETURN_OK, size;

  switch (hunk & 0xfff) {
  case hunk_code:
  case hunk_data:    rc = do_code_data(hunk, g); break;
  case hunk_bss:     writeLong(hunk, g);
                     if (readLong(&size, g)) writeLong(size, g);
                     else {
		       message(msg_err_eof, g);
		       rc = RETURN_ERROR;
		     }
                     break;
  case hunk_reloc32: rc = do_reloc(hunk, g); break;
  case hunk_symbol:  rc = do_symbol(g); break;
  case hunk_debug:   rc = do_debug(g); break;
  case hunk_end:     writeLong(hunk, g); break;
  case hunk_header:  rc = do_header(hunk, g); break;
  case hunk_overlay:
  case hunk_break:   message(msg_overlaid, g);
                     rc = RETURN_ERROR;
                     break;
  default:           message(msg_unknownhunk, g);
                     rc = RETURN_ERROR;
                     break;
  }
  return rc;
}


LONG do_code_data(enum hunktypes hunk, Global *g)
{
  LONG rc = RETURN_OK;
  LONG lwords;

  writeLong(hunk, g);
  if (readLong(&lwords, g)) {
    writeLong(lwords, g);
    lwords &= LENGTH_MASK;
    if (!copyNLongs(lwords, g)) rc = RETURN_ERROR;
  }
  else rc = RETURN_ERROR;

  if (rc != RETURN_OK) message(msg_err_eof, g);
  return rc;
}


LONG do_reloc(enum hunktypes hunk, Global *g)
{
  LONG rc = RETURN_OK, offsets;
  
  writeLong(hunk, g);

  do {
    /*
     * # of offsets
     */
    if (readLong(&offsets, g)) {
      writeLong(offsets, g);

      if (!offsets) break;

      /*
       * copy the hunk number and the offsets
       */
      if (!copyNLongs(++offsets, g)) rc = RETURN_ERROR;
    }
    else rc = RETURN_ERROR;
  } while (rc == RETURN_OK);

  if (rc != RETURN_OK) message(msg_err_eof, g);
  return rc;
}


LONG do_symbol(Global *g)
{
  LONG rc = RETURN_OK, namelen;

  g->symhunks++;

  do {
    /*
     * Length of symbol name
     */
    if (readLong(&namelen, g)) {
      if (!namelen) break;

      /*
       * copy the name and the symbol value
       */
      if (!skipNLongs(++namelen, g)) rc = RETURN_ERROR;
    }
    else rc = RETURN_ERROR;
  } while (rc == RETURN_OK);
  
  if (rc != RETURN_OK) message(msg_err_eof, g);
  return rc;
}


LONG do_debug(Global *g)
{
  LONG rc = RETURN_OK;
  LONG lwords;

  g->dbghunks++;

  if (readLong(&lwords, g)) {
    lwords &= LENGTH_MASK;
    if (!skipNLongs(lwords, g))	rc = RETURN_ERROR;
  }
  else rc = RETURN_ERROR;

  if (rc != RETURN_OK) message(msg_err_eof, g);
  return rc;
}


LONG do_header(enum hunktypes hunk, Global *g)
{
  LONG rc = RETURN_OK;
  LONG size, first, last;

  writeLong(hunk, g);

  do {
    /* Copy names of resident libraries, if any
     */
    if (readLong(&size, g)) writeLong(size, g);
    else rc = RETURN_ERROR;
    if (!copyNLongs(size, g)) rc = RETURN_ERROR;
  } while (rc == RETURN_OK && size != 0);

  if (rc == RETURN_OK)
    /*
     * Hunk table size
     */
    if (readLong(&size, g)) {
      writeLong(size, g);
      /*
       * First hunk #
       */
      if (readLong(&first, g)) {
	writeLong(first, g);
	/*
	 * Last hunk #
	 */
	if (readLong(&last, g)) {
	  writeLong(last, g);
	  /*
	   * Copy the table contents
	   */
	  if (!copyNLongs(last - first + 1, g)) rc = RETURN_ERROR;
	}
	else rc = RETURN_ERROR;
      }
      else rc = RETURN_ERROR;
    }
    else rc = RETURN_ERROR;

  if (rc != RETURN_OK) message(msg_err_eof, g);
  return rc;
}


VOID statistics(Global *g)
{
  if (!g->args.quiet)
    if (g->symhunks || g->dbghunks)
      printf("%s: removed - symbol hunks: %ld, debug hunks: %ld\n",
	       PROGNAME, g->symhunks, g->dbghunks);
    else message(msg_nothing, g);
}


/*
 * Custom buffered I/O functions
 */

BOOL readLong(LONG *v, Global *g)
{
  LONG rlen;

  if (g->ibptr >= g->ibend) {
    g->ibptr = g->ib;
    if ((rlen = Read(g->in, g->ib, BUFLEN * sizeof(LONG))) <= 0)
      return FALSE;		/* error or EOF */
    g->ibend = g->ib + (rlen / sizeof(LONG));
  }
  *v = *g->ibptr++;
  return TRUE;
}


/*
 * Copy n LONGs from the input to the output.
 */

BOOL copyNLongs(LONG n, Global *g)
{
  LONG count, read;

  count = g->ibend - g->ibptr;
  count = MIN(n, count);

  /*
   * copy buffered input data to the output file.
   */

  if (n > 0) {
    writeNLongs(count, g);
    g->ibptr += count;
    n -= count;
  }

  /*
   * copy any remaining data directly from
   * the input file to the output via
   * the currently empty input buffer.
   */

  if (n > 0) {
    count = MIN(BUFLEN, n);
    while ((read = Read(g->in, g->ib, count * sizeof(LONG))) > 0) {
      g->ibptr = g->ib;
      /*
       * I wonder whether the following is a dangerous
       * assumption given that the AmigaDOS Manual states
       * that "[u]sually Read() will try to fill up your
       * buffer before returning":
       */
      read /= sizeof(LONG);
      writeNLongs(read, g);
      if ((n -= read) == 0) break;
      count = MIN(BUFLEN, n);
    }
    if (read <= 0) return FALSE;
    /*
     * The input buffer has become invalid,
     * so we force a physical read by marking
     * the buffer as empty.
     */
    g->ibptr = g->ibend;
  }

  return TRUE;
}


BOOL skipNLongs(LONG n, Global *g)
{
  LONG inbuffer;

  if (n > 0) {
    inbuffer = g->ibend - g->ibptr + 1;
    if (n <= inbuffer) g->ibptr += n;
    else {
      n -= inbuffer;
      if (Seek(g->in, n * sizeof(LONG), OFFSET_CURRENT) < 0)
	return FALSE;		/* beyond end of file? */
      /*
       * The input buffer has become invalid,
       * so we force a physical read by marking
       * the buffer as empty.
       */
      g->ibptr = g->ibend;
    }
  }
  return TRUE;
}


VOID writeLong(LONG v, Global *g)
{
  if (g->obptr - g->ob == BUFLEN && !flush(g)) /* write error */;
  *g->obptr++ = v;
}


/*
 * Write n LONGs from g->ibptr to the output buffer.
 * Assumes that the output buffer is at least as big
 * as the input buffer.
 */

VOID writeNLongs(LONG n, Global *g)
{
  LONG obfree, count;

  obfree = BUFLEN - (g->obptr - g->ob);

  if ((count = MIN(n, obfree)) > 0) {
    memcpy(g->obptr, g->ibptr, count * sizeof(LONG));
    g->obptr += count;
  }

  if (n > obfree) {
    flush(g);
    n -= count;
    memcpy(g->obptr, g->ibptr + count, n * sizeof(LONG));
    g->obptr += n;
  }
}


BOOL flush(Global *g)
{
  LONG bytes = (g->obptr - g->ob) * sizeof(LONG);

  if (bytes && Write(g->out, g->ob, bytes) < 0) return FALSE;
  g->obptr = g->ob;
  return TRUE;
}


/*
 * Hybrid parsing functions for Amiga/POSIX compatibility
 */



/**
 * @brief Detect if command line uses getopt style
 * @param argc Argument count
 * @param argv Argument vector
 * @return TRUE if getopt style, FALSE otherwise
 */
int is_getopt_style(int argc, char **argv)
{
    if (argc < 2) return FALSE;

    /* Check for standard options like -s, -o, -k */
    if (argv[1][0] == '-' && argv[1][1] != '\0' && strchr("sokqh", argv[1][1])) {
        return TRUE;
    }

    /* Check for historical syntax like +5 or -10 */
    if ((argv[1][0] == '+') ||
        (argv[1][0] == '-' && (isdigit(argv[1][1])))) {
        return TRUE;
    }

    return FALSE;
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param pathname Pointer to store input file path
 * @param destination Pointer to store output file path
 * @param quiet Pointer to store quiet flag
 * @param keep Pointer to store keep flag
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, STRPTR *pathname, STRPTR *destination, LONG *quiet, LONG *keep, int *file_start, const char *program)
{
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "s:o:kqvhV")) != -1) {
        switch (c) {
            case 's':
                /* -s is equivalent to no options (strip all) */
                break;
            case 'o':
                *destination = optarg;
                break;
            case 'k':
                *keep = TRUE;
                break;
            case 'q':
                *quiet = TRUE;
                break;
            case 'v':
            case 'h':
            case 'V':
                fprintf(stderr, "Usage: %s [OPTIONS] FILE [OUTPUT]\n", program);
                fprintf(stderr, "OPTIONS:\n");
                fprintf(stderr, "  -s          strip all symbols and debug info (default)\n");
                fprintf(stderr, "  -o FILE     output file\n");
                fprintf(stderr, "  -k          keep original file with .orig extension\n");
                fprintf(stderr, "  -q          quiet mode\n");
                fprintf(stderr, "  -v, -h, -V  display this help and version\n");
                exit(RETURN_FAIL);
                break;
            case '?':
                exit(RETURN_FAIL);
                break;
        }
    }
    
    *file_start = optind;
    
    /* Get the input file */
    if (*file_start < argc) {
        *pathname = argv[*file_start];
        (*file_start)++;
        
        /* Check for output file */
        if (*file_start < argc && !*destination) {
            *destination = argv[*file_start];
        }
    }
}

/**
 * @brief Core strip logic separated from argument parsing
 * @param pathname Input file path
 * @param destination Output file path (can be NULL)
 * @param quiet Quiet mode flag
 * @param keep Keep original flag
 * @param program Program name for error messages
 * @return Exit code
 */
int run_strip_logic(STRPTR pathname, STRPTR destination, LONG quiet, LONG keep, const char *program)
{
    Global *g;
    LONG rc = RETURN_OK;
    
    if (!pathname) {
        fprintf(stderr, "%s: no input file specified\n", program);
        return RETURN_FAIL;
    }
    
    g = AllocVec(sizeof(Global), MEMF_ANY | MEMF_CLEAR);
    if (!g) {
        fprintf(stderr, "%s: out of memory\n", program);
        return RETURN_FAIL;
    }
    
    g->obptr = g->ob;
    g->args.pathname = pathname;
    g->args.destination = destination;
    g->args.quiet = quiet;
    g->args.keep = keep;
    
    FreeVec(g);
    return rc;
}