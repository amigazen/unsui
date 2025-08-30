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

#define SRC_OSDEP_C
#include "aush.h"
#include <dirent.h>

struct AnchorPath
{
  char ap_name[MAXCMD+1] ;	/* directory name */
  char ap_pat[MAXCMD+1] ;	/* pattern to find */
  DIR  *ap_dir ;		/* returned by opendir() */
  char ap_result[MAXCMD+1] ;	/* next name found */
} ;

extern int errno ;
extern char **MonEnv ;
extern struct List ChildList ;

int LastChild = -1 ;
char *__daytbl[7]  = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" } ;
char *__montbl[12] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" } ;

static int LastRet ;
static CMD *NewCmd ;

/*******************************************************************/

int IoErr( void )               { return( errno ) ; }
int IsResident( char *prg )     { return( 0 ) ; }

/*******************************************************************/

static void ChildDeath( int pid , int st ) /* called when a child process died */
{
  CMD *c ;
  time_t clk ;

  for ( c = (CMD *)ChildList.lh_Head ; c->c_node.ln_Succ ; c = (CMD *)c->c_node.ln_Succ )
    if ( c->c_procnum == pid ) break ;

  if ( ! c->c_node.ln_Succ ) return ; /* child not found */

  c->c_retcode = st ;
  time( &clk ) ;
  DoFreeChild( c , &clk ) ;
}

/*******************************************************************/

int WaitForChild( int *pid )    /* wait for a child death */
{
  int st, died ;

  for (;;)
  {
    died = wait( &st ) ;
    if ( died == -1 ) return( -1 ) ;    /* no child process */

    if ( st & 0xff00 )                  /* get return value */
    {
      st >>= 8 ;
      if ( st == 255 ) st = -1 ;
    }

    LastRet = st ;
    if ( LastChild == died ) LastChild = -1 ;

    if ( *pid == died ) return( st ) ;  /* waiting for a specific child */

    ChildDeath( died , st ) ;
    if ( *pid == -1 )                   /* waiting for any child */
    {
      *pid = died ;
      return( st ) ;
    }
  }
}

/*******************************************************************/

static void reopen( int as , FILE *fh )
{
  close( as ) ;
  dup( fh->_file ) ;
  fclose( fh ) ;
}

/*******************************************************************/

static void do_run( int sig )   /* perform redirection and start child */
{
  if ( NewCmd->c_stdin )  reopen( 0 , NewCmd->c_in->f_desc ) ;
  if ( NewCmd->c_stdout ) reopen( 1 , NewCmd->c_out ) ;
  execve( NewCmd->c_path , NewCmd->c_argv , MonEnv ) ;
  perror( NewCmd->c_path ) ;
}

/*******************************************************************/

int CheckArgs( CMD *cmd ) /* set argv[0] to path name and prepare to run */
{
  int k ;

  k = cmd->c_argc + 2 ;
  if ( k >= MAXARG ) return( 0 ) ;
  cmd->c_argv[--k] = NULL ;

  for ( k-- ; k > 0 ; k-- )
    cmd->c_argv[k] = cmd->c_argv[k-1] ;

  cmd->c_argv[0] = strdup( cmd->c_path ) ;
  cmd->c_argc++ ;

  NewCmd = cmd ;
  signal( SIGUSR1 , do_run ) ;
  return( 1 ) ;
}

/*******************************************************************/

int RunCmd( CMD *cmd )  /* run a command */
{
  int pid, fh ;

  if (! CheckArgs( cmd )) return( -1 ) ;

  LastChild = pid = fork() ;

  switch ( pid )
  {
    case -1 : return( -1 ) ;
    case  0 : pause() ;
	      exit( -1 ) ;
    default : signal( SIGUSR1 , SIG_DFL ) ;
	      kill( pid , SIGUSR1 ) ;
	      if ( LastChild == pid ) pause() ;
	      return( LastRet ) ;
  }

}

/*******************************************************************/

int RunBack( CMD *cmd ) /* start a new child process */
{
  int pid, fh ;

  if (! CheckArgs( cmd )) return( -1 ) ;

  LastChild = pid = fork() ;

  switch ( pid )
  {
    case -1 : return( -1 ) ;
    case  0 : signal( SIGINT , SIG_IGN ) ;
	      signal( SIGQUIT , SIG_IGN ) ;
	      pause() ;
	      exit( -1 ) ;
    default : signal( SIGUSR1 , SIG_DFL ) ;
	      kill( pid , SIGUSR1 ) ;
	      if ( LastChild != pid ) pid = -1 ;
	      return( pid ) ;
  }
}

/*******************************************************************/

void *AllocAnchor( char *pat , int size )      /* prepare pattern expansion */
{
  struct AnchorPath *ap ;

  ap = malloc( sizeof(struct AnchorPath) ) ;
  if ( ! ap ) return( NULL ) ;

  strcpy( ap->ap_name , pat ) ;
  pat = strrchr( ap->ap_name , '/' ) ;
  if ( pat ) pat[1] = '\0' ;
	else MyGetCwd( ap->ap_name ) ;

  ap->ap_dir = opendir( ap->ap_name ) ;
  if ( ! ap->ap_dir )
  {
    free( ap ) ;
    return( NULL ) ;
  }

  return( ap ) ;
}

/*******************************************************************/

char *FindNext( struct AnchorPath *ap )         /* returns next name matching */
{
  struct dirent *d ;

  while ( d = readdir( ap->ap_dir ) )
  {
    if (! strcmp( d->d_name , "."  )) continue ;
    if (! strcmp( d->d_name , ".." )) continue ;

    if ( re_exec( d->d_name ) == 1 )
    {
      strcpy( ap->ap_result , ap->ap_dir ) ;
      strcat( ap->ap_result , d->d_name ) ;
      return( ap->ap_result ) ;
    }
  }

  return( NULL ) ;
}

/*******************************************************************/

char *FindFirst( char *pat , struct AnchorPath *ap )    /* returns first name matching */
{
  char *p, *q ;

  p = strrchr( pat , '/' ) ;
  if ( ! p ) p = pat ;
	else p++ ;

  for ( q = ap->ap_pat , *q++ = '^' ; *p ; p++ )
  {
    if ( *p == '*' ) *q++ = '.' ;
    if ( *p == '.' ) *q++ = '\\' ;
    if ( *p == '?' ) *q++ = '.' ;
		else *q++ = *p ;
  }
  *q++ = '$' ;
  *q = '\0' ;

  if ( re_comp( ap->ap_pat ) ) return( NULL ) ;

  return( FindNext( ap ) ) ;
}

/*******************************************************************/

void FreeAnchor( struct AnchorPath *ap , int bufsize ) /* end of pattern expansion */
{
  closedir( ap->ap_dir ) ;
  free( ap ) ;
}


