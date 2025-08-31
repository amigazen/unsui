/*
* This file is part of AUSH.
* Copyright (C) 1994 Denis Gounelle
* 
* AUSH is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AUSH is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with AUSH.  If not, see <http://www.gnu.org/licenses/>.
*
*/
/***************************************************************************
 *
 * AUSH (c)1992 par Denis GOUNELLE
 *
 ***************************************************************************/



#include "ares/ares.h"
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>

#ifdef _AMIGA
#include <dos.h>
#include <ios1.h>
#include <stat.h>
#include <dos/dostags.h>
#include <exec/execbase.h>
#include <exec/lists.h>
#include <libraries/asl.h>
#include <intuition/intuition.h>
#include <libraries/locale.h>

#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/locale.h>
#include <proto/asl.h>
#include <clib/alib_protos.h>

#define NULDEV "NIL:"
#define PIPDEV "PIPE:"
#define TMPDIR "T:"

#else /* _AMIGA */

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include "lists.h"

#define EXEC_TYPES_H		/* for aush_msg.h */
typedef long LONG ;
typedef char * STRPTR ;

#define FALSE  0
#define TRUE  -1

#define RETURN_OK	0
#define RETURN_WARN	5
#define RETURN_ERROR	10
#define RETURN_FAIL	20

#define ERROR_NO_FREE_STORE	ENOMEM
#define ERROR_OBJECT_WRONG_TYPE EACCES

#endif /* _AMIGA */

#ifdef unix
#define NULDEV "/dev/null/"
#define TMPDIR "/tmp/"
#endif

/* various constants */

#define MAXIFS	16
#define MAXNAME 32
#define MAXARG	128
#define MAXCMD	256
#define MAXBUF	488
#define MAXLINE 1024

#define ERROR( c ) pError( c , NULL )
#define IOERROR( s ) pError( IoErr() , s )

/* this is used to handle commands */

typedef struct cmd
{
  struct Node c_node ;
  char	*c_name ;		/* command name   */
  char	*c_path ;		/* name with path */
  int	 c_argc ;		/* number of args */
  int	 c_arglen ;		/* total arg. len */
  char	*c_argv[MAXARG] ;	/* arguments	  */
  char	 c_argf[MAXARG] ;	/* arg. flags	  */
  char	*c_args ;		/* final args.	  */
  char	*c_stdin ;		/* input redir.   */
  char	*c_stdout ;		/* output redir.  */
  int	 c_pri ;		/* priority	  */
  int	 c_flags ;		/* flags for cmd  */
  int	 c_procnum ;		/* process number */
  int	 c_retcode ;		/* return code	  */
  char	*c_pipe ;		/* next command   */
  struct MyFile *c_in ; 	/* opened input   */
  FILE	*c_out ;		/* opened output  */
  time_t c_time ;		/* started time   */
} CMD ;

#define CA_SQUOTED    0x01	/* 'argument' */
#define CA_DQUOTED    0x02	/* "argument" */
#define CA_FESC       0x04	/* \argument  */
#define CA_SUBST      0x08	/* `argument` */
#define CA_SPLITME    0x10	/* to split   */
#define CA_SPLITED    0x20	/* splited    */
#define CA_COMPLEX    0x40	/* ',' or '|' */
#define CA_QUOTED     (CA_SQUOTED|CA_DQUOTED)

#define CF_BACKGROUND 0x0001  /* cmd &	   */
#define CF_APPENDOUT  0x0002  /* cmd >>out */
#define CF_PIPEOUT    0x0004  /* cmd | ... */
#define CF_PIPEIN     0x0008  /* ... | cmd */
#define CF_OPENOUT    0x0010  /* c_stdout opened */
#define CF_OPENIN     0x0020  /* c_stdin opened  */
#define CF_FORDONE    0x0040  /* for...done cmd  */
#define CF_TOEXEC     0x0080  /* not started yet */
#define CF_RAMPIPE    0x0100  /* pipes in T:	 */
#define CF_TIME       0x0200  /* compute exec. time */

/* private error codes */

#define ERROR_TOO_MUCH_ARGS	500	/* more than MAXARG arguments	 */
#define ERROR_TOO_MUCH_REDIRS	501	/* stdin/stdout redirected twice */
#define ERROR_CANNOT_OPEN	502	/* cannot open pipe/redir. file  */
#define ERROR_CANNOT_EXECUTE	503	/* cannot execute command	 */
#define ERROR_CANNOT_SEEK	504	/* cannot seek to end of stdout  */
#define ERROR_BAD_ARGS		505	/* bad args. to internal command */
#define ERROR_OVERFLOW		506	/* something is too big...	 */
#define ERROR_NO_MSG_PORT	507	/* couldn't create MsgPort       */
#define ERROR_SYNTAX_ERROR	508	/* syntax error in command line  */
#define ERROR_IF_ELSE_ENDIF	509	/* error in if...else...endif	 */
#define ERROR_NO_SUCH_DIR	510	/* error in popd, dirs, etc...	 */
#define ERROR_ZERO_DIVIDE	511	/* zero divide in Eval()         */
#define ERROR_STACK_FULL	512	/* not enough stack space	 */
#define ERROR_READ_ONLY 	513	/* variable is read only	 */
#define ERROR_NO_MATCH		514	/* pattern don't match anything  */
#define ERROR_BAD_FUNCTION	515	/* var:func with bad func name	 */

/* this is used to store variables */

struct Var
{
  struct Node v_node ;
  char	v_flags ;
  char	v_name[MAXNAME+1] ;
  char	v_value[1] ;
} ;

#define VF_READONLY 0x01
#define VF_EXPORTED 0x02

/* this is used to store history */

struct Hist
{
  struct Node h_node ;
  int	h_count ;
  char	h_line[1] ;
} ;

/* this is used to store aliases */

struct Alias
{
  struct Node a_node ;
  char	a_name[MAXNAME+1] ;
  int	a_len ;
  char *a_after ;	/* after  [] */
  char	a_before[1] ;	/* before [] */
} ;

/* this is used to handle loops */

struct Loop
{
  struct Loop *l_next ; 	/* simple ptr for linking */
  int	l_flags ;
  char	l_tmpf[MAXNAME+1] ;	/* name of tmp file	  */
  FILE *l_desc ;		/* file handle on l_tmpf  */
  char *l_var ; 		/* loop variable name	  */
  int	l_nbval ;		/* number of val in val[] */
  char *l_val[MAXARG] ;
  char	l_valf[MAXARG] ;
  char	l_buf[MAXLINE+1] ;	/* buffer for reading */
} ;

#define LF_BREAKED  0x01 /* a break command */
#define LF_EXITED   0x02 /* an exit command */
#define LF_VAL2VAL  0x04 /* for var in n .. m */
#define LF_FROMFILE 0x08 /* for i from file */

/* this is used to handle files */

struct MyFile
{
  struct Node f_node ;
  FILE *f_desc ;
  int	f_flags ;
  int	f_nbif ;		/* for if..else..endif */
  char	f_if[MAXIFS] ;
  struct Loop *f_loop ; 	/* ptr on current loop */
  struct MyFile *f_parent ;	/* parent source file  */
  struct List f_private ;	/* header for local vars    */
  struct List f_public ;	/* header for exported vars */
  int	f_argc ;		/* #of args (for sourcing)  */
  char *f_argv[MAXARG] ;	/* args (for sourcing)      */
} ;

#define FF_FORDONE 0x01   /* this file is a for...done loop */
#define FF_BREAKME 0x02   /* break current loop */
#define FF_EXITME  0x04   /* return to previous level */

#define FI_TRUE 0x01 /* execute lines */
#define FI_ELSE 0x02 /* got "else"    */
#define FI_COPY 0x04 /* if in a if    */

/* this is send by a child process */

#ifdef _AMIGA

struct ChildMsg
{
  struct Message   cm_msg ;
  long		   cm_rega4 ;
  struct MsgPort  *cm_port ;
  time_t	   cm_xtime ;
  long		   cm_retcode ;
  CMD		  *cm_cmd ;
} ;

#endif

/* Function prototypes for missing functions */

#ifndef _AMIGA
/* Unix-specific function prototypes */
char *strdup(const char *str);
char *getcwd(char *buf, size_t size);
int stricmp(const char *s1, const char *s2);
#endif

