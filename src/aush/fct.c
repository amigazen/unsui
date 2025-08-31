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

#define SRC_FCT_C
#define STRINGARRAY
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

#ifdef _AMIGA
extern struct Process *Moi ;
extern struct Window *ConWin ;
extern struct Locale *MaLocale ;
extern struct LocaleBase *LocaleBase ;
extern struct Catalog *MonCatalogue ;
extern struct CommandLineInterface *MonCli ;
#endif

extern struct stat sbuf ;
extern struct List ChildList ;
extern char FromFile[], *__montbl[], *__daytbl[] ;
extern int TaillePile, ShellNumber, LastReturn, ChildNumber, HistNumber, NestedLoop ;

static char tmp[MAXLINE+1], prompt[MAXLINE+1], title[MAXLINE+1] ;
static char DaysInMonth[12] = { 31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31 } ;

/***************************************************************************/

char *MyLocaleStr( int code )
{
  char *q ;

  q = NULL ;
  if ( (code < 0) || (code > MSG_ENDSHELL) ) return( "Bad error code" ) ;

#ifdef _AMIGA
  if ( MonCatalogue ) q = GetCatalogStr( MonCatalogue , code , NULL ) ;
#endif
  if ( ! q ) q = AppStrings[code].as_Str ;
  return( q ) ;
}

/***************************************************************************/

void pError( int code , char *str )
{
  if (! str) str = "AUSH" ;

  if ( (code >= ERROR_TOO_MUCH_ARGS) && (code <= ERROR_BAD_FUNCTION) )
  {
    code -= ERROR_TOO_MUCH_ARGS ;
    printf( "%s: %s\n" , str , MyLocaleStr( code ) ) ;
    return ;
  }

#ifdef _AMIGA
  if (! Fault( code , str , title , MAXLINE ))
    sprintf( title , MyLocaleStr( MSG_UNKNOWNERR ) , str , code ) ;
  puts( title ) ;
#endif

#ifdef unix
  printf( MyLocaleStr( MSG_UNKNOWNERR ) , str , code ) ;
#endif
}

/***************************************************************************/

void *myalloc( int len, int flg )
{
  char *ptr ;

  if ( len & 1 ) len++ ;
  ptr = malloc( (size_t)len ) ;
  if (! ptr) ERROR( ERROR_NO_FREE_STORE ) ;
  if ( flg ) memset( ptr , '\0' , (size_t)len ) ;
  return( ptr ) ;
}

/***************************************************************************/

char *BuildName( char *file )

/*
 * Pr�pare un nom complet permettant d'acc�der au fichier "file" dans
 * le r�pertoire ad�quat pour les fichiers de config/pr�f�rences
 * Sur Amiga c'est "S:", sous Unix "$HOME"
 */

{
#ifdef _AMIGA
  strcpy( tmp , "S:" ) ;
#endif

#ifdef unix
  int k ;
  char *p ;

  p = getenv( "HOME" ) ;
  if ( p )
  {
    strcpy( tmp , p ) ;
    k = strlen( tmp ) - 1 ;
    if ( k >= 0 )
      if ( tmp[k] != '/' ) strcat( tmp , "/" ) ;
  }
  else *tmp = '\0' ;
#endif

  strcat( tmp , file ) ;
  return( tmp ) ;
}

/***************************************************************************/

void FreeCmd( CMD *cmd )
{
  int k ;

  if ( cmd->c_flags & CF_BACKGROUND )
    if (! (cmd->c_flags & CF_TOEXEC)) return ;

  if ( cmd->c_name ) free( cmd->c_name ) ;
  if ( cmd->c_path ) free( cmd->c_path ) ;

  for ( k = 0 ; k < cmd->c_argc ; k++ )
  {
    if (! cmd->c_argv[k]) continue ;
    if ( cmd->c_argf[k] != CA_SPLITED ) free( cmd->c_argv[k] ) ;
  }

  if ( cmd->c_flags & CF_OPENIN ) FClose( cmd->c_in , 1 ) ;
  if ( (cmd->c_flags & CF_OPENOUT) && (cmd->c_out) ) fclose( cmd->c_out ) ;

  if ( (cmd->c_flags & CF_PIPEIN) &&
       (cmd->c_flags & CF_RAMPIPE) ) remove( cmd->c_stdin ) ;

  if ( cmd->c_args ) free( cmd->c_args ) ;
  if ( cmd->c_stdout ) free( cmd->c_stdout ) ;
  if ( cmd->c_stdin ) free( cmd->c_stdin ) ;
  if ( cmd->c_pipe ) free( cmd->c_pipe ) ;

  free( cmd ) ;
}

/***************************************************************************/

void InitChild( void )
{
  NewList( &ChildList ) ;
}

/****************************************************************************/

void DoFreeChild( CMD *cmd , time_t *xtime )
{
  if (! (cmd->c_flags & CF_PIPEOUT))
  {
    printf( MyLocaleStr( MSG_FINISHBG ) , cmd->c_procnum , cmd->c_name , cmd->c_retcode ) ;
    if ( cmd->c_flags & CF_TIME ) printf( MyLocaleStr( MSG_TIMECMD ) , *xtime - cmd->c_time ) ;
  }

  cmd->c_flags &= ~CF_BACKGROUND ;
  Remove( (struct Node *)cmd ) ;
  FreeCmd( cmd ) ;
  ChildNumber-- ;
}

/****************************************************************************/

void FreeLoop( struct Loop *p )
{
  int k ;

  if ( p->l_desc ) fclose( p->l_desc ) ;
  remove( p->l_tmpf ) ;

  for ( k = 0 ; k < p->l_nbval ; k++ )
    if ( p->l_valf[k] != CA_SPLITED ) free( p->l_val[k] ) ;

  free( p->l_var ) ;
  free( p ) ;
}

/***************************************************************************/

int BuildString( char *p , char *dst , struct MyFile *in )
{
  char *q ;
  time_t clk ;
  struct tm *ladate ;
  int k, l, m, cfree, ffree ;

  time( &clk ) ;
  ladate = localtime( &clk ) ;

#ifdef _AMIGA
  Forbid() ;
  cfree = AvailMem( MEMF_CHIP ) ;
  ffree = AvailMem( MEMF_FAST ) ;
  Permit() ;
#endif

  for ( k = l = 0 ; *p ; p++ )
  {
    if ( *p == '%' )
    {
      l = 1 ;
      continue ;
    }

    if ( l )
    {
      *tmp = '\0' ;
      switch ( *p )
      {
	case 'c' : if ( q = myGetVar( "cwd" ) ) strcpy( tmp , q ) ;
		   break ;
	case 'b' : if ( q = myGetVar( "cwd" ) )
		   {
		     strcpy( tmp , q ) ;
		     q = strrchr( tmp , '/' ) ;
		     if ( q ) *q = '\0' ;
		     q = NomDeBase( tmp ) ;
		     if ( *q ) strcpy( tmp , q ) ;
		   }
		   break ;
	case 'V' : p++ ;
		   for ( m = 0 ; (*p == '_') || (isalnum( (unsigned char)*p )) ; m++ , p++ ) tmp[m] = *p ;
		   p-- ;
		   tmp[m] = '\0' ;
		   if ( m )
		     if ( q = myGetVar( tmp ) )
		       strcpy( tmp , q ) ;
		     else
		       *tmp = '\0' ;
		   break ;
	case 'T' : sprintf( tmp , "%02ld:%02ld:%02ld" , ladate->tm_hour , ladate->tm_min , ladate->tm_sec ) ;
		   break ;
	case 'd' : sprintf( tmp , "%02ld" , ladate->tm_mday ) ;
		   break ;
	case 'D' : q = NULL ;
#ifdef _AMIGA
		   if ( MaLocale ) q = GetLocaleStr( MaLocale , ABDAY_1 + ladate->tm_wday ) ;
#endif
		   if ( ! q ) q = __daytbl[ladate->tm_wday] ;
		   strcpy( tmp , q ) ;
		   break ;
	case 'm' : sprintf( tmp , "%02ld" , ladate->tm_mon ) ;
		   break ;
	case 'M' : q = NULL ;
#ifdef _AMIGA
		   if ( MaLocale ) q = GetLocaleStr( MaLocale , ABMON_1 + ladate->tm_mon ) ;
#endif
		   if ( ! q ) q = __montbl[ladate->tm_mon-1] ;
		   strcpy( tmp , q ) ;
		   break ;
	case 'y' : sprintf( tmp , "19%02ld" , ladate->tm_year ) ;
		   break ;
	case '#' : sprintf( tmp , "%ld" , ShellNumber ) ;
		   break ;
	case 's' : sprintf( tmp , "%ld" , LastReturn ) ;
		   break ;
	case 'h' : sprintf( tmp , "%ld" , HistNumber ) ;
		   break ;
	case 'i' : sprintf( tmp , "%ld" , in->f_nbif ) ;
		   break ;
	case 'l' : sprintf( tmp , "%ld" , NestedLoop ) ;
		   break ;
#ifdef _AMIGA
	case 'f' : sprintf( tmp , "%ld" , ffree ) ;
		   break ;
	case 'v' : sprintf( tmp , "%ld" , cfree ) ;
		   break ;
	case 'a' : sprintf( tmp , "%ld" , cfree+ffree ) ;
		   break ;
	case 'C' : dst[k] = '\233' ; k++ ; break ;
#endif
	case 'N' : dst[k] = '\012' ; k++ ; break ;
	default  : dst[k] = '%'    ; k++ ;
		   dst[k] = *p	   ; k++ ;
		   break ;
      }

      if ( *tmp )
      {
	strcpy( &dst[k] , tmp ) ;
	k += strlen( tmp ) ;
      }

      l = 0 ;
      continue ;
    }

    dst[k] = *p ;
    k++ ;
  }

  dst[k] = '\0' ;
  return( k ) ;
}

/***************************************************************************/

void MyGetCwd( char *tmp )
{
  char *p ;

  memset( tmp , '\0' , MAXCMD ) ;
  getcwd( tmp , MAXCMD+1 ) ;
#ifdef _AMIGA
  if ( p = strchr( tmp , ':' ) )
  {
    if ( p[1] ) strcat( tmp , "/" ) ;
  }
  else strcat( tmp , ":" ) ;
#endif
}

/***************************************************************************/

char *DisplayPrompt( struct MyFile *in )
{
  int k ;
  char *p ;

  /* refresh $cwd */

  MyGetCwd( tmp ) ;
  mySetVar( "cwd" , tmp , TRUE ) ;

  /* refresh window title */

#ifdef _AMIGA
  if ( p = myGetVar( "titlebar" ) )
  {
    k = BuildString( p , title , in ) ;
    if ( ConWin ) SetWindowTitles( ConWin , title , (APTR)-1 ) ;
  }
#endif

  /* build prompt */

  if ( in->f_nbif ) p = myGetVar( "prompt2" ) ;
  else if ( in->f_loop ) p = myGetVar( "prompt3" ) ;
  else p = myGetVar( "prompt" ) ;
  if (! p) p = "> " ;

  k = BuildString( p , prompt , in ) ;
  printf( "%s" , prompt ) ;
  fflush( stdout ) ;
  return( prompt ) ;
}

/****************************************************************************/

void DoSplit( CMD *cmd , int k )
{
  CMD oldcmd ;
  int l, j ;
  unsigned char *q ;

  /* save remaining args in oldcmd */

  l = 0 ;
  for ( j = k+1 ; j < cmd->c_argc ; j++ , l++ )
  {
    oldcmd.c_argv[l] = cmd->c_argv[j] ;
    oldcmd.c_argf[l] = cmd->c_argf[j] ;
  }
  oldcmd.c_argc = l ;

  /* split argv[k] and add pieces into cmd */

  j = 0 ;
  cmd->c_argc = k+1 ;
  cmd->c_argf[k] = 0 ;
  q = (unsigned char *)cmd->c_argv[k] ;

  for ( l = k+1 ; ; l++ )
  {
    while ( isspace( *q ) ) q++ ;
    while ( (*q) && (! isspace( *q )) ) q++ ;
    if (! *q) break ;

    *q = '\0' ;
    q++ ;
    if ( cmd->c_argc == MAXARG-1 )
    {
      ERROR( ERROR_OVERFLOW ) ;
      goto _serr ;
    }

    cmd->c_argv[l] = (char *)q ;
    cmd->c_argf[l] = CA_SPLITED ;
    cmd->c_argc++ ;
  }

  /* take back remaining args into cmd */

  while ( j < oldcmd.c_argc )
  {
    if ( cmd->c_argc == MAXARG-1 )
    {
      ERROR( ERROR_OVERFLOW ) ;
      goto _serr ;
    }
    cmd->c_argv[l] = oldcmd.c_argv[j] ;
    cmd->c_argf[l] = oldcmd.c_argf[j] ;
    cmd->c_argc++ ;
    j++ ;
    l++ ;
  }

  /* free uncopied args */

_serr:

  while ( j < oldcmd.c_argc )
  {
    free( oldcmd.c_argv[j] ) ;
    j++ ;
  }
}

/****************************************************************************/

int SearchCmdPath( CMD *cmd )

/* search path for command, and sets cmd->c_path */

{
  int k ;
  char *p, *q ;

#ifdef _AMIGA
  sbuf.st_mode &= ~S_ISCRIPT ;
#endif
  strcpy( tmp , cmd->c_name ) ;

  if ( IsResident( tmp ) ) goto _done ;
  if (! (p = myGetVar( "path" ))) goto _done ;

  /* full name : examine to test "s" bit */

  if ( strchr( tmp , ':' ) || strchr( tmp , '/' ) )
  {
    stat( tmp , &sbuf ) ;
    goto _done ;
  }

  /* loop to scan all path components */

  while ( p )
  {
    strcpy( tmp , p ) ;         /* copy current path component */

    q = strchr( p , ';' ) ;     /* set p to start of next comp. */
    if ( q ) p = &q[1] ;
	else p = NULL ;
    q = strchr( tmp , ';' ) ;   /* set '\0' mark at comp. end */
    if ( q ) *q = '\0' ;

    if (! strcmp( tmp , "." ))  /* '.' means 'Current directory' */
    {
      q = myGetVar( "cwd" ) ;
      if ( q ) strcpy( tmp , q ) ;
    }

    k = strlen( tmp ) - 1 ;     /* append '/' to comp. if needed */
    if ( (k >= 0) &&
	 (tmp[k] != ':') &&
	 (tmp[k] != '/') ) strcat( tmp , "/" ) ;

    strcat( tmp , cmd->c_name ) ; /* append command name */
    if ( (! stat( tmp , &sbuf )) && (sbuf.st_mode & S_IFREG) )
    {
      p = tmp ; 		/* found and not a directory => exit */
      break ;
    }
  }

  if ( ! p ) strcpy( tmp , cmd->c_name ) ;

  /* store full command name */

_done:

  k = strlen( tmp ) + 3 ;
  p = myalloc( k , 0 ) ;
  if ( ! p ) return( 0 ) ;
  if ( strchr( tmp , ' ' ) ) sprintf( p , "\"%s\"" , tmp ) ;
			else strcpy( p , tmp ) ;
  cmd->c_path = p ;

  /* if bit "s" is set, assume it's a script */

#ifdef _AMIGA
  if ( sbuf.st_mode & S_ISCRIPT )
  {
    if ( cmd->c_argc >= MAXARG )                        /* shift arguments */
    {
      ERROR( ERROR_OVERFLOW ) ;
      return( 0 ) ;
    }
    for ( k = cmd->c_argc ; k > 0 ; k-- )
    {
      cmd->c_argv[k] = cmd->c_argv[k-1] ;
      cmd->c_argf[k] = cmd->c_argf[k-1] ;
    }
    cmd->c_arglen += strlen( cmd->c_path ) ;
    cmd->c_argv[0] = cmd->c_path ;
    cmd->c_argf[0] = 0 ;
    cmd->c_argc++ ;
    cmd->c_path = NULL ;

    memset( tmp , '\0' , 6 ) ;                          /* read the first bytes */
    k = open( p , O_RDONLY ) ;
    if ( k != -1 )
    {
      read( k , tmp , 5 ) ;
      close( k ) ;
    }

    if (! strncmp( tmp , "/*" , 2 )) p = "rx" ;         /* select the command to run */
    else if (! stricmp( tmp , ";AUSH" )) p = "source" ;
    else p = "execute" ;

    if (! (cmd->c_name = strdup( p ))) return( 0 ) ;
    return( 1 ) ;
  }
#endif

  return( -1 ) ;
}

/***************************************************************************/

int ExpandPat( char *pat, char *tab[] )

/*
 * Expand pattern in "pat", and put result in "pat" and "tab"
 * Return the number of items in "tab".
 */

{
  int l, n ;
  char *d, *new ;
  struct AnchorPath *ap ;
  static char tmp[MAXLINE+1] ;

  /* process expansion */

  ap = (struct AnchorPath *)AllocAnchor( pat , MAXLINE ) ;
  if ( ! ap ) return( 0 ) ;

  n   = 0 ;	 /* number of items	 */
  l   = 0 ;	 /* size of expanded arg */
  d   = tmp ;	 /* expanded argument	 */
  *d  = '\0' ;
  new = FindFirst( pat , ap ) ;

  while ( new )
  {
    l += strlen( new ) + 1 ;
    if ( l >= MAXLINE )
    {
      ERROR( ERROR_OVERFLOW ) ;
      n = 0 ;
      break ;
    }

    if ( tab )
    {
      if ( n >= MAXARG )
      {
	ERROR( ERROR_OVERFLOW ) ;
	n = 0 ;
	break ;
      }
      d++ ;
      tab[n] = d ;
      n++ ;
    }

    if ( (d == tmp) || (tab) ) d = strpcpy( d , new ) ;
    else d = strxcat( d , " " , new , "" ) ;
    new = FindNext( ap ) ;
  }

  FreeAnchor( ap , MAXLINE ) ;
  strcpy( pat , tmp ) ;
  return( n ) ;
}

/***************************************************************************/

int ParseArgs( int argc , char *argv[] )
{
  int k ;

  if ( ! argc ) return( TRUE ) ;

  if (! (argc & 1)) return( FALSE ) ; /* nb arg doit �tre impair */

  for ( k = 1 ; k < argc ; k++ )
    if (! stricmp( argv[k] , "FROM" ))
    {
      k++ ;
      strcpy( FromFile , argv[k] ) ;
    }
    else return( FALSE ) ;

  return( TRUE ) ;
}

