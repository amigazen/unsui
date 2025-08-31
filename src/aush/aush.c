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

#define SRC_AUSH_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

#ifdef unix
#include <unistd.h>
#endif

struct internal
{
  char c_name[16+1] ;
  int (*c_func)( CMD * ) ;
} ;

/***************************************************************************/

char PourVersion2_0[] = "$VER: AUSH 3.17 " __AMIGADATE__ ,
     *AUSHVersion     = "v3.17" ,
     *ShellTitle      = "AUSH v3.17 (c)1992-1994 by Denis GOUNELLE" ,
     ChildName[]      = "AUSH Background Process" ,
#ifdef _AMIGA
     *DefaultPrompt   = "%C0;33;40m%# - %c >%C0;31;40m " ,
#else
     *DefaultPrompt   = "%# - %c > " ,
#endif
     *ShellCount      = "aush_count" ,
     *DirAlias	      = "dircmd" ,
     *OldTitle	      = NULL ,
     **MonEnv	      = NULL ,
     FromFile[MAXCMD] ;

int  ShellNumber  = 0 ,
     CmdNumber	  = 0 ,
     LoopNumber   = 0 ,
     NestedLoop   = 0 ,
     LastReturn   = 0 ,
     PipeNumber   = 0 ,
     HistNumber   = 0 ,
     FailLevel	  = 20 ,
     TaillePile   = 4096 ,
     MaPile	  = 0 ,
     ChildNumber  = 0 ;

int  Breaked	  = 0 ;

char InputPipe[MAXNAME+1], Line[MAXLINE+1], tmp[MAXLINE+1] ;

struct stat sbuf ;
struct List ChildList ;
struct MyFile *StdIn, *CurrentFile = NULL ;

#ifdef _AMIGA
struct Process *Moi = NULL ;
struct Window *ConWin = NULL ;
extern struct Library *AslBase;
struct Locale *MaLocale = NULL ;
struct MsgPort *ChildPort = NULL ;
extern struct LocaleBase *LocaleBase;
struct Catalog *MonCatalogue = NULL ;
extern struct IntuitionBase *IntuitionBase;
struct CommandLineInterface *MonCli  = NULL ;

static struct TagItem OCTags[] =
{
  OC_BuiltInCodeSet,0,
  OC_BuiltInLanguage,(ULONG)"english",
  TAG_END,NULL
} ;

#define ARRET (SIGBREAKF_CTRL_C|SIGBREAKF_CTRL_D)
#endif

/***************************************************************************/

extern char *DirStack[] ;
extern struct List VarList ;
extern int DirTop, RpnMem[] ;
extern struct ExecBase *SysBase ;

#ifdef unix
extern int LastChild ;
#endif

int ExecCmd( CMD * , FILE * ) ;

/***************************************************************************/

static void sigclr( int sig )
{
  if ( sig == SIGINT ) Breaked = 0 ;
}

#ifdef unix
static void sighand( int sig )
{
  int pid ;

  if ( sig == SIGINT )
  {
    Breaked = -1 ;
    signal( SIGINT , sighand ) ;
    return ;
  }

  if ( sig == SIGCHLD )
  {
    pid = LastChild ;
    WaitForChild( &pid ) ;
    signal( SIGCHLD , sighand ) ;
    return ;
  }
}
#endif

/***************************************************************************/

void AbortPrg( int code )
{
#ifdef _AMIGA
  FreeRequest() ;
  if ( ChildPort ) DeletePort( ChildPort ) ;
  if ( MonCatalogue ) CloseCatalog( MonCatalogue ) ;
  if ( MaLocale ) CloseLocale( MaLocale ) ;

  if ( LocaleBase ) CloseLibrary( LocaleBase ) ;
  if ( IntuitionBase ) CloseLibrary( (struct Library *)IntuitionBase ) ;
  if ( AslBase ) CloseLibrary( AslBase ) ;
#endif
  exit( code ) ;
}

/***************************************************************************/

void Termine( int code )
{
  int k ;
  int pid ;
#ifdef _AMIGA
  struct Message *Msg ;

  if ( (ConWin) && (OldTitle) ) SetWindowTitles( ConWin , OldTitle , (APTR)-1 ) ;
#endif

  if ( ChildNumber )
    printf( MyLocaleStr( MSG_WAITINGBG ) , ChildNumber ) ;

#ifdef _AMIGA
  while ( ChildNumber )
  {
    WaitPort( ChildPort ) ;
    while ( Msg = GetMsg( ChildPort ) ) FreeChild( (struct ChildMsg *)Msg ) ;
  }
#endif

#ifdef unix
  while ( ChildNumber )
  {
    pid = -1 ;
    if ( WaitForChild( &pid ) == -1 ) break ;
  }
#endif

  printf( MyLocaleStr( MSG_ENDSHELL ) , ShellNumber ) ;

  for ( k = 0 ; k < DirTop ; k++ ) free( DirStack[k] ) ;

  SaveHist() ;
  FreeHist() ;
  FreeVars( &VarList ) ;
  FreeAlias() ;
  FreeFiles() ;

  if ( StdIn ) FClose( StdIn , 0 ) ;
  AbortPrg( code ) ;
}

/***************************************************************************/

char *DoSubst( char *scmd , FILE *out ) /* perform command substitution */
{
  FILE *f ;
  CMD *cmd ;
  int lg, k ;
  char *p, *q, *cmd2 ;

  strcpy( Line , scmd ) ;

_start :

  cmd = SplitLine( Line , 0 ) ;
  while ( cmd )
  {
    if (! BuildCmd( cmd ))
    {
      FreeCmd( cmd ) ;
      break ;
    }

    if (! (cmd->c_flags & CF_PIPEOUT))
    {
      if ( cmd->c_stdout )
      {
	ERROR( ERROR_SYNTAX_ERROR ) ;
	goto _send ;
      }

      p = myalloc( MAXNAME+1 , 0 ) ;
      if ( ! p ) goto _send ;
      sprintf( p , "%ss%ld" , TMPDIR , ShellNumber ) ;
      cmd->c_stdout = p ;
    }

    if (! ExecCmd( cmd , out )) goto _send ;

    cmd2 = cmd->c_pipe ;
    if ( cmd->c_flags & CF_PIPEOUT ) strcpy( InputPipe , cmd->c_stdout ) ;
    cmd->c_pipe = NULL ;

    if ( cmd )
    {
      if ( cmd->c_stdout == p ) cmd->c_stdout = NULL ;
      FreeCmd( cmd ) ;
      cmd = NULL ;
    }

    if ( cmd2 )
    {
      strcpy( Line , cmd2 ) ;
      free( cmd2 ) ;
      if ( ! LastReturn ) goto _start ;
    }
  }

  q = NULL ;
  if ( f = fopen( p , "r" ) )
  {
    fgets( tmp , MAXLINE , f ) ;
    fclose( f ) ;

    lg = strlen( tmp ) ;
    for ( k = 0 ; tmp[k] && isspace( (unsigned char)tmp[k] ) ; k++ ) lg-- ;
    if ( (lg > 0) && (q = myalloc( lg+1 , 0 )) ) strcpy( q , &tmp[k] ) ;
  }

  remove( p ) ;
  return( q ) ;

_send:

  if ( cmd ) FreeCmd( cmd ) ;
  return( NULL ) ;
}

/***************************************************************************/

struct internal Internals[] =
{
  "setvar"   , do_setvar,
  "set"      , do_set,
  "unset"    , do_unset,
  "exit"     , do_exit,
  "stop"     , do_break,
  "history"  , do_history,
  "source"   , do_source,
  "alias"    , do_alias,
  "unalias"  , do_unalias,
  "shift"    , do_shift,
  "export"   , do_export,
  "read"     , do_read,
  "cd"       , do_cd,
  "pushd"    , do_pushd,
  "popd"     , do_popd,
  "dirs"     , do_dirs,
  "for"      , do_for,
  "done"     , do_done,
  "jobs"     , do_jobs,
  "eval"     , do_eval,
  "readonly" , do_readonly,
  "stack"    , do_stack,
  "failat"   , do_failat,
  "echo"     , do_echo,
  "window"   , do_window,
  "loadhist" , LoadHist,
  "writehist", SaveHist,
  ""        , NULL
} ;

int ExecCmd( CMD *cmd , FILE *out )
{
  int k, l ;
  char *q, *p ;

  /* cas special pour la forme "var=valeur" */

#ifdef unix

  strcpy( tmp , cmd->c_name ) ;
  if ( p = strchr( tmp , '=' ) )
  {
    *p = '\0' ;
    if (! BadVarName( tmp , NULL ))
    {
      p = strdup( cmd->c_name ) ;
      if ( p )
      {
	k = 0 ;
	putenv( p ) ;
      }
      else k = RETURN_FAIL ;
      goto _setret ;
    }
  }

#endif

  /*
   * Build argument list :
   * - first we perform all command substitutions
   * - then (when we have the real arguments) we put all args together
   *   (after redirection processing)
   *
   * IMPORTANT NOTE:
   *   at this point cmd->c_arglen is the len of all arguments *EXCEPT*
   *   those which specify command substitution
   */

  for ( k = 0 ; k < cmd->c_argc ; k++ )
    if ( cmd->c_argf[k] == CA_SUBST )
    {
      q = DoSubst( cmd->c_argv[k] , out ) ;
      free( cmd->c_argv[k] ) ;
      cmd->c_argv[k] = (q) ? q : strdup( "" ) ;
      cmd->c_argf[k] = 0 ;
      cmd->c_arglen += strlen( cmd->c_argv[k] ) + 1 ;
    }

  /* process redirection */

  if ( cmd->c_stdin )
  {
    if (! (cmd->c_in = FOpen( cmd->c_stdin , "r" )))
    {
      IOERROR( cmd->c_stdin ) ;
      return( 0 ) ;
    }
    cmd->c_flags |= CF_OPENIN ;
  }
  else cmd->c_in = StdIn ;

  if ( cmd->c_stdout )
    if ( stricmp( cmd->c_stdout , "TTY" ) )
    {
      cmd->c_out = fopen( cmd->c_stdout , (cmd->c_flags & CF_APPENDOUT) ? "a" : "w" ) ;
      if ( ! cmd->c_out )
      {
	IOERROR( cmd->c_stdout ) ;
	return( 0 ) ;
      }

      cmd->c_flags |= CF_OPENOUT ;
    }
    else cmd->c_out = stdout ;
  else cmd->c_out = out ;

  /* prepare to run command */

_restart:

  if ( cmd->c_args ) free( cmd->c_args ) ;

  /* we allocate MAXARG bytes more than needed, in prevision of splited args */
  if (! (cmd->c_args = myalloc( cmd->c_arglen+1+MAXARG , 0 ))) return( 0 ) ;

  p = cmd->c_args ;
  *p = '\0' ;
  for ( k = 0 ; k < cmd->c_argc ; k++ )
  {
    switch( cmd->c_argf[k] )
    {
      case CA_SQUOTED :
      case CA_DQUOTED : p = strxcat( p , " \"" , cmd->c_argv[k] , "\"" , NULL ) ;
			break ;
      case CA_SPLITME : DoSplit( cmd , k ) ;
			/* the first word is added in the "default" switch */
      default	      : p = strxcat( p , " " , cmd->c_argv[k] , NULL ) ;
			break ;
    }
  }

  if (! strlen( cmd->c_args )) strcpy( cmd->c_args , " " ) ;
  if ( IsSet( "debug" ) ) printf( ">>> %s%s\n" , cmd->c_name , cmd->c_args ) ;

  l = 1 ;
  cmd->c_time = time( NULL ) ;

  if (! stricmp( cmd->c_name , "if" )) { k = do_if( cmd ) ; l = -1 ; }
  else if (! stricmp( cmd->c_name , "else" )) { k = do_else() ; l = -1 ; }
  else if (! stricmp( cmd->c_name , "endif" )) { k = do_endif() ; l = -1 ; }
  else if ( (k = CurrentFile->f_nbif) && (! (CurrentFile->f_if[k-1] & FI_TRUE)) ) return( -1 ) ;

  /* internal command */

  if ( l > 0 )
    for ( l = 0 ; Internals[l].c_func ; l++ )
      if (! stricmp( cmd->c_name , Internals[l].c_name ))
      {
	k = (*Internals[l].c_func)( cmd ) ;
	l = -1 ;
	break ;
      }

  /* external */

  if ( l > 0 )
  {
    l = SearchCmdPath( cmd ) ;
    if ( ! l ) return( 0 ) ;
    if ( l > 0 ) goto _restart ;

    if ( cmd->c_flags & CF_BACKGROUND )
    {
      k = RunBack( cmd ) ;
      if ( k > 0 )
      {
	cmd->c_procnum = k ;
	cmd->c_flags &= ~CF_TOEXEC ;
	if (! (cmd->c_flags & CF_PIPEOUT))
	  printf( MyLocaleStr( MSG_STARTEDBG ) , cmd->c_procnum , cmd->c_name ) ;
	AddTail( &ChildList , (struct Node *)cmd ) ;
	LastReturn = 0 ; /* otherwise other commands won't be runned */
	ChildNumber++ ;
	return( -1 ) ;
      }
      cmd->c_flags &= ~CF_BACKGROUND ; /* otherwise not freed */
    }
    else
    {
      cmd->c_time = time( NULL ) ;
#ifdef _AMIGA
      l = SetTaskPri( &(Moi->pr_Task) , cmd->c_pri ) ;
#endif
      k = RunCmd( cmd ) ;
#ifdef _AMIGA
      SetTaskPri( &(Moi->pr_Task) , l ) ;
#endif
    }
  }

  /* delete output pipe if failed */

  if ( (k) &&
       (cmd->c_flags & CF_PIPEOUT) )
  {
    if ( (cmd->c_flags & CF_OPENOUT) && (cmd->c_out) ) fclose( cmd->c_out ) ;
    if ( cmd->c_flags & CF_RAMPIPE ) remove( cmd->c_stdout ) ;
    cmd->c_flags &= ~CF_OPENOUT ;
  }

  /* check if we could run command */

  if ( k < 0 )
  {
    pError( ERROR_CANNOT_EXECUTE , cmd->c_name ) ;
    return( 0 ) ;
  }

  /* set return code */

_setret:

  if ( cmd->c_flags & CF_TIME )
  {
    l = time( NULL ) - cmd->c_time ;
    printf( MyLocaleStr( MSG_TIMECMD ) , l ) ;
  }

  cmd->c_retcode = k ;
  cmd->c_flags &= ~CF_TOEXEC ;

  LastReturn = k ;
  sprintf( tmp , "%ld" , LastReturn ) ;
  mySetVar( "status" , tmp , TRUE ) ;
  return( -1 ) ;
}

/***************************************************************************/

int MainLoop( struct MyFile *in , FILE *out )
{
  CMD *cmd ;
  int lg, flg ;
  struct Loop *b ;
  char *p, *q, *cmd2 ;
  struct Message *Msg ;

  CurrentFile = in ;
  flg = isatty( in->f_desc->_file ) ;

  for (;;)
  {
#ifdef _AMIGA
    if ( SetSignal( 0 , 0 ) & ARRET )
    {
      Breaked = -1 ;
      Forbid() ;
      Disable() ;
      SetSignal( 0 , ARRET ) ;
      Enable() ;
      Permit() ;
    }
#endif

    if ( Breaked )
    {
      sigclr( SIGINT ) ;
      if ( ! flg )
      {
	printf( "\n***BREAK\n" ) ;
	if ( q = myGetVar( "trap" ) )
	{
	  strcpy( Line , q ) ;
	  goto _start ;
	}
	LastReturn = RETURN_FAIL ;
	sprintf( tmp , "%ld" , LastReturn ) ;
	mySetVar( "status" , tmp , TRUE ) ;
	break ;
      }
    }

    q = NULL ;
    if ( flg )
    {
      p = DisplayPrompt( in ) ;
      if ( IsSet( "lineedit" ) ) q = p ;
#ifdef _AMIGA
      if ( ! ConWin ) q = NULL ;
#endif
      LastReturn = RETURN_OK ;
    }

    *Line = '\0' ;

    if ( (flg) && (q) )
    {
      q = rawgets( in , Line , p ) ;
      if ( q == (char *)-1 ) goto _input ;
      if ( ! q ) break ;
      sigclr( SIGINT ) ;
      lg = strlen( Line ) ;
    }
    else
    {
_input:
      lg = FGetline( in , Line , MAXLINE ) ;
      if ( feof( in->f_desc ) ) break ;
    }

#ifdef _AMIGA
    if ( ChildNumber )
      while ( Msg = GetMsg( ChildPort ) ) FreeChild( (struct ChildMsg *)Msg ) ;
#endif

    if ( lg > 0 )
    {
      if ( IsSet( "debug" ) ) printf( "<<< %s\n" , Line ) ;
      if ( flg ) AddHist( Line , lg ) ;
      if (! (CurrentFile->f_flags & FF_FORDONE))
      {
	if ( *InputPipe ) remove( InputPipe ) ;
	*InputPipe = '\0' ;
      }

_start:

      cmd = SplitLine( Line , 1 ) ;

      while ( cmd )
      {
	if ( (stricmp( cmd->c_name , "done" ) || NestedLoop) &&
	     (b = CurrentFile->f_loop) &&
	     (b->l_desc) )
	{
	  q = strrchr( Line , '\n' ) ;
	  if ( q ) *q = '\0' ;
	  fprintf( b->l_desc , "%s\n" , Line ) ;
	  goto _desalloc ;
	}

	cmd2 = NULL ;
	if (! BuildCmd( cmd ))
	{
	  FreeCmd( cmd ) ;
	  break ;
	}

	/*
	 * if no args, no redirection/pipe/background, and command name
	 * is a directory name, and alias "dircmd" exists : substitute
	 * by "dircmd <dir>"
	 */

	if ( (! cmd->c_argc) &&
	     (cmd->c_flags == CF_TOEXEC) &&
	     (! stat( cmd->c_name , &sbuf )) &&
	     (sbuf.st_mode & S_IFDIR) )
	{
	  sprintf( Line , "%s %s" , DirAlias , cmd->c_name ) ;
	  FreeCmd( cmd ) ;
	  goto _start ;
	}

	if ( ExecCmd( cmd , out ) )
	{
	  if ( CurrentFile->f_flags & (FF_BREAKME|FF_EXITME) )
	  {
	    FreeCmd( cmd ) ;
	    return( LastReturn ) ;
	  }
_desalloc:
	  cmd2 = cmd->c_pipe ;
	  if ( cmd->c_flags & CF_PIPEOUT ) strcpy( InputPipe , cmd->c_stdout ) ;
	  cmd->c_pipe = NULL ;
	}

	if ( cmd )
	{
	  if (! (cmd->c_flags & CF_BACKGROUND)) FreeCmd( cmd ) ;
	  cmd = NULL ;
	}

	if ( cmd2 )
	{
	  strcpy( Line , cmd2 ) ;
	  free( cmd2 ) ;
	  if ( ! LastReturn ) goto _start ;
	}
      }
    }

    if ( (! flg) && (LastReturn >= FailLevel) ) break ;
  }

  return( LastReturn ) ;
}

/***************************************************************************/

void main( int argc , char *argv[] , char **envp )
{
  int k ;
  char *p ;
  CMD DummyCmd ;

  /* first initializations */

#ifdef unix
  signal( SIGINT  , sighand ) ;
  signal( SIGQUIT , SIG_IGN ) ;
  signal( SIGCHLD , sighand ) ;
#endif

#ifdef _AMIGA
  signal( SIGINT , SIG_IGN ) ;
  Moi = (struct Process *)FindTask( NULL ) ;
  MonCli = (struct CommandLineInterface *)( (long)Moi->pr_CLI << 2 ) ;
  if ( ! MonCli )
  {
    Delay( 60 * TICKS_PER_SECOND ) ;
    exit( RETURN_FAIL ) ;
  }

  TaillePile = MonCli->cli_DefaultStack << 2 ;
  FailLevel  = MonCli->cli_FailLevel ;
#endif

  MonEnv = envp ;
  MaPile = TaillePile ;

  strcpy( FromFile , BuildName( ".aushrc" ) ) ;
  if (! ParseArgs( argc , argv )) exit( RETURN_FAIL ) ;

  /* open libraries */

#ifdef _AMIGA
  if ( SysBase->LibNode.lib_Version > 36 )
  {
    AslBase = OpenLibrary( "asl.library" , 0L ) ;
    if ( ! AslBase ) exit( RETURN_FAIL ) ;
    LocaleBase = OpenLibrary( "locale.library" , 37L ) ;
    if ( LocaleBase )
    {
      MaLocale = OpenLocale( NULL ) ;
      MonCatalogue = OpenCatalogA( NULL , "aush.catalog" , OCTags ) ;
    }
  }
  else
  {
    printf( "You need Kickstart 2.0 !\n" ) ;
    exit( RETURN_FAIL ) ;
  }

  IntuitionBase = (struct IntuitionBase *)OpenLibrary( "intuition.library" , 0 ) ;
  if ( ! IntuitionBase ) AbortPrg( RETURN_FAIL ) ;
#endif

  /* open ressources */

#ifdef _AMIGA
  sprintf( Line , "Shell_%08lx" , Moi ) ;
  ChildPort = CreatePort( Line , 0 ) ;
  if ( ! ChildPort )
  {
    ERROR( ERROR_NO_MSG_PORT ) ;
    AbortPrg( RETURN_FAIL ) ;
  }
#endif

  printf( "\f%s\n" , ShellTitle ) ;

  /* update shell count */

#ifdef _AMIGA
  Forbid() ;
  if ( p = getenv( ShellCount ) ) ShellNumber = atol( p ) ;
  ShellNumber++ ;
  sprintf( Line , "%s=%ld" , ShellCount , ShellNumber ) ;
  putenv( Line ) ;
  Permit() ;
#endif
#ifdef unix
  ShellNumber = getpid() ;
#endif

  /* initialize vars, alias and history */

  InitRegs() ;
  InitVars() ;
  mySetVar( "prompt" , DefaultPrompt , FALSE ) ;
  mySetVar( "prompt2" , "%i> " , FALSE ) ;
  mySetVar( "prompt3" , "%l) " , FALSE ) ;
  mySetVar( "history" , "50" , FALSE ) ;
  mySetVar( "histmin" , "3"  , FALSE ) ;
#ifdef _AMIGA
  mySetVar( "filepat" , "#?" , FALSE ) ;
#endif
#ifdef unix
  mySetVar( "filepat" , "*" , FALSE ) ;
#endif
  mySetVar( "delim" , " :/" , FALSE ) ;
  mySetVar( "insert" , "1" , FALSE ) ;
  mySetVar( "keys" , "abdefklnpstvwxz" , FALSE ) ;

  InitFiles() ;
  InitAlias() ;
  InitChild() ;

  if (! (StdIn = FAssign( stdin ))) Termine( RETURN_FAIL ) ;
  CurrentFile = StdIn ;
#ifdef _AMIGA
  ConWin = GetConWin() ;
  if ( ConWin ) OldTitle = (char *)ConWin->Title ;
  mySetVar( "remote" , ( ConWin ) ? "0" : "1" , TRUE ) ;
#endif

  mySetVar( "version" , AUSHVersion , TRUE ) ;

  /* source startup file */

  if (! access( FromFile , R_OK|F_OK ))
  {
    DummyCmd.c_argc = 1 ;
    DummyCmd.c_in = StdIn ;
    DummyCmd.c_out = stdout ;
    DummyCmd.c_argv[0] = FromFile ;
    DummyCmd.c_flags = 0 ;
    do_source( &DummyCmd ) ;
  }

  /* start main loop */

  InitHist() ;
  k = MainLoop( StdIn , stdout ) ;
  Termine( k ) ;
}

