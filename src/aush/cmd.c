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

#define SRC_INTERNAL_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

#ifdef _AMIGA
extern struct Process *Moi ;
extern struct Window *ConWin ;
extern struct CommandLineInterface *MonCli ;
#endif

extern char Breaked ;
extern struct stat sbuf ;
extern struct MyFile *CurrentFile, *StdIn ;
extern struct List VarList, HistList, AliasList, ChildList ;
extern int  ShellNumber, LoopNumber, MaPile, NestedLoop, TaillePile, FailLevel ;

#define IERROR( C ) pError( C , cmd->c_name )

int  DirTop = 0 ;
char *DirStack[MAXARG] ;
static char tmp[MAXLINE+1], aux[MAXLINE+1] ;

/***************************************************************************/

int  do_exit( CMD *cmd )
{
  int  code ;

  code = ( cmd->c_argc > 0 ) ? atoi( cmd->c_argv[0] ) : 0 ;
  CurrentFile->f_flags |= FF_EXITME ;
  cmd->c_flags &= ~CF_BACKGROUND ;
  return( code ) ;
}

/***************************************************************************/

static int  do_affvar( struct Var *p , CMD *cmd )
{
  fprintf( cmd->c_out , "%-16s (%lc%lc) %s\n" ,
			p->v_name ,
			( p->v_flags & VF_EXPORTED ) ? 'x' : ' ' ,
			( p->v_flags & VF_READONLY ) ? 'r' : ' ' ,
			p->v_value ) ;

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_setvar( CMD *cmd )
{
  struct Var *p, *q ;

  switch( cmd->c_argc )
  {
    case 0  : for ( p = (struct Var *)VarList.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
		do_affvar( p , cmd ) ;
	      return( RETURN_OK ) ;
    case 1  : if ( BadVarName( cmd->c_argv[0] , cmd->c_name ) ) return( RETURN_FAIL ) ;
	      p = GetGlobalPtr( cmd->c_argv[0] ) ;
	      if ( p ) do_affvar( p , cmd ) ;
	      return( RETURN_OK ) ;
    case 2  : if ( BadVarName( cmd->c_argv[0] , cmd->c_name ) ) return( RETURN_FAIL ) ;
	      return( mySetVar( cmd->c_argv[0] , cmd->c_argv[1] , FALSE ) ) ;
    default : IERROR( ERROR_BAD_ARGS ) ;
	      return( RETURN_FAIL ) ;
  }
}

/***************************************************************************/

int  do_set( CMD *cmd )
{
  struct Var *p ;

  switch( cmd->c_argc )
  {
    case 0  : return( ScanVar( do_affvar , cmd ) ) ;
    case 1  : if ( BadVarName( cmd->c_argv[0] , cmd->c_name ) ) return( RETURN_FAIL ) ;
	      p = GetLocalPtr( cmd->c_argv[0] ) ;
	      if ( p ) do_affvar( p , cmd ) ;
	      return( RETURN_OK ) ;
    case 2  : if ( BadVarName( cmd->c_argv[0] , cmd->c_name ) ) return( RETURN_FAIL ) ;
	      return( SetLocal( cmd->c_argv[0] , cmd->c_argv[1] , 0 ) ) ;
    default : IERROR( ERROR_BAD_ARGS ) ;
	      return( RETURN_FAIL ) ;
  }
}

/***************************************************************************/

int  do_unset( CMD *cmd )
{
  int  k ;
  struct Var *p ;

  for ( k = 0 ; k < cmd->c_argc ; k++ )
  {
    if ( BadVarName( cmd->c_argv[k] , cmd->c_name ) ) return( RETURN_FAIL ) ;
    if ( p = GetLocalPtr( cmd->c_argv[k] ) )
    {
      if ( p->v_flags & VF_READONLY )
      {
	pError( ERROR_READ_ONLY , p->v_name ) ;
	return( RETURN_FAIL ) ;
      }
      Remove( (struct Node *)p ) ;
      free( p ) ;
    }
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_export( CMD *cmd )
{
  int  k ;
  struct Var *p, *q ;

  for ( k = 0 ; k < cmd->c_argc ; k++ )
  {
    if ( BadVarName( cmd->c_argv[k] , cmd->c_name ) ) return( RETURN_FAIL ) ;
    for ( p = (struct Var *)CurrentFile->f_private.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
      if (! strcmp( cmd->c_argv[k] , p->v_name ))
      {
	Remove( (struct Node *)p ) ;
	p->v_flags |= VF_EXPORTED ;
	AddHead( &(CurrentFile->f_public) , (struct Node *)p ) ;
	break ;
      }
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_readonly( CMD *cmd )
{
  int  k ;
  struct Var *p ;

  for ( k = 0 ; k < cmd->c_argc ; k++ )
  {
    if ( BadVarName( cmd->c_argv[k] , cmd->c_name ) ) return( RETURN_FAIL ) ;
    if ( p = GetVarPtr( cmd->c_argv[k] ) )
    {
      p->v_flags |= VF_READONLY ;
      break ;
    }
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_history( CMD *cmd )
{
  int  k ;
  struct Hist *p, *q ;

  if ( cmd->c_argc > 1 )
  {
    IERROR( ERROR_BAD_ARGS ) ;
    return( RETURN_FAIL ) ;
  }

  k = ( cmd->c_argc ) ? strlen( cmd->c_argv[0] ) : 0 ;

  for ( p = (struct Hist *)HistList.lh_Head ; q = (struct Hist *)p->h_node.ln_Succ ; p = q )
  {
    if ( k && strnicmp( p->h_line , cmd->c_argv[0] , (size_t)k ) ) continue ;
    fprintf( cmd->c_out , "%3ld : %s\n" , p->h_count , p->h_line ) ;
    if ( Breaked )
    {
      printf( "***BREAK\n" ) ;
      return( RETURN_WARN ) ;
    }
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_source( CMD *cmd )
{
  int  k ;
  struct MyFile *f, *old ;

  if (! cmd->c_argc)
  {
    IERROR( ERROR_BAD_ARGS ) ;
    return( RETURN_FAIL ) ;
  }

  /* check stack free size */

#ifdef _AMIGA
  k = getreg( 15 ) - (int )Moi->pr_ReturnAddr + MaPile ;
  if ( k < 1600 )
  {
    IERROR( ERROR_STACK_FULL ) ;
    return( RETURN_FAIL ) ;
  }
#endif

  if ( f = FOpen( cmd->c_argv[0] , "r" ) )
  {
    old = CurrentFile ;
    f->f_parent = old ;
    CurrentFile = f ;	/* for SetLocal() to work ! */

    if ( cmd->c_flags & CF_FORDONE ) f->f_flags |= FF_FORDONE ;
    else
    {
      f->f_argc = cmd->c_argc ;
      for ( k = 0 ; k < cmd->c_argc ; k++ ) f->f_argv[k] = cmd->c_argv[k] ;
      sprintf( tmp , "%ld" , k - 1 ) ;
      SetLocal( "argc" , tmp , VF_READONLY ) ;
    }

    k = MainLoop( f , cmd->c_out ) ;
    CurrentFile = old ;
    if ( CurrentFile->f_loop )
    {
      if ( f->f_flags & FF_BREAKME ) CurrentFile->f_loop->l_flags |= LF_BREAKED ;
      if ( f->f_flags & FF_EXITME ) CurrentFile->f_loop->l_flags |= LF_EXITED ;
    }
    FClose( f , 1 ) ;
    return( k ) ;
  }

  pError( IoErr() , cmd->c_argv[0] ) ;
  return( RETURN_FAIL ) ;
}

/***************************************************************************/

int  do_shift( CMD *cmd )
{
  int  k ;
  struct MyFile *f ;

  f = CurrentFile ;
  for ( k = 2 ; k < f->f_argc ; k++ )
  {
    f->f_argv[k-1] = f->f_argv[k] ;
    f->f_argv[k] = NULL ;
  }

  if ( f->f_argc )
  {
    f->f_argc-- ;
    sprintf( tmp , "%ld" , f->f_argc - 1 ) ;
    SetLocal( "argc" , tmp , VF_READONLY ) ;
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_alias( CMD *cmd )
{
  struct Alias *p ;

  switch ( cmd->c_argc )
  {
    case 0  : for ( p = (struct Alias *)AliasList.lh_Head ; p->a_node.ln_Succ ; p = (struct Alias *)p->a_node.ln_Succ )
		fprintf( cmd->c_out , "%s\t%s\n" , p->a_name , p->a_before ) ;
	      break ;
    case 1  : if ( p = GetAlias( cmd->c_argv[0] ) )
		fprintf( cmd->c_out , "%s\t%s\n" , p->a_name , p->a_before ) ;
	      break ;
    case 2  : return( SetAlias( cmd->c_argv[0] , cmd->c_argv[1] ) ) ;
	      break ;
    default : IERROR( ERROR_BAD_ARGS ) ;
	      return( RETURN_FAIL ) ;
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_unalias( CMD *cmd )
{
  int  k ;
  struct Alias *p ;

  for ( k = 0 ; k < cmd->c_argc ; k++ )
    for ( p = (struct Alias *)AliasList.lh_Head ; p->a_node.ln_Succ ; p = (struct Alias *)p->a_node.ln_Succ )
      if (! stricmp( cmd->c_argv[k] , p->a_name ))
      {
	Remove( (struct Node *)p ) ;
	free( p ) ;
	break ;
      }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_if( CMD *cmd )
{
  time_t clk ;
  unsigned char c ;
  int  flg, k, l, m ;
#ifdef _AMIGA
  APTR oldwindowptr ;
#endif

  if ( cmd->c_argc < 2 ) goto _if_err ;

  k = CurrentFile->f_nbif ;
  if ( k >= MAXIFS )
  {
    ERROR( ERROR_OVERFLOW ) ;
    return( RETURN_FAIL ) ;
  }

  if ( (k) && (! (CurrentFile->f_if[k-1] & FI_TRUE )) )
  {
    CurrentFile->f_if[k] = FI_COPY ;
    CurrentFile->f_nbif++ ;
    return( RETURN_OK ) ;
  }

  k = flg = 0 ;
  if ( (! stricmp( cmd->c_argv[k] , "not" )) && (! cmd->c_argf[k]) )
  {
    flg++ ;
    k++ ;
  }

  l = k+1 ;
  if ( l >= cmd->c_argc ) goto _if_err ;

  /* if [!] {-d|-e|-f|-s} name */

  if ( (cmd->c_argv[k][0] == '-') && (! cmd->c_argf[k]) )
  {
    m = 0 ;

    c = (unsigned char)cmd->c_argv[k][1] ;
#ifdef _AMIGA
    if ( isupper( c ) )
    {
      oldwindowptr = Moi->pr_WindowPtr ;
      Moi->pr_WindowPtr = (APTR) -1L ;
    }
#endif
    k = stat( cmd->c_argv[l] , &sbuf ) ;
#ifdef _AMIGA
    if ( isupper( c ) )
    {
      Moi->pr_WindowPtr = oldwindowptr ;
      c = tolower( c ) ;
    }
#endif

    if (! strchr( "defso" , c )) goto _if_err ;

    if ( ! k )
    {
      switch ( c )
      {
	case 'd' : m = sbuf.st_mode & S_IFDIR ;
		   break ;
	case 'e' : m = 1 ;
		   break ;
	case 'f' : m = sbuf.st_mode & S_IFREG ;
		   break ;
	case 's' : m = sbuf.st_size ;
		   break ;
	case 'o' : clk = sbuf.st_mtime ;
		   k = l+1 ;
		   if ( k >= cmd->c_argc ) goto _if_err ;
		   m = 0 ;
		   if ( ! stat( cmd->c_argv[k] , &sbuf ) ) m = (sbuf.st_mtime - clk) > 0 ? 1 : 0 ;
		   break ;
      }
    }
  }
  else
  {
    m = l+1 ;
    if ( m >= cmd->c_argc ) goto _if_err ;

    if (! strcmp( cmd->c_argv[l] , "=" ))  /* if [!] str1 = str2 */
      m = strcmp( cmd->c_argv[k] , cmd->c_argv[m] ) ? 0 : 1 ;
    else if (! strcmp( cmd->c_argv[l] , "==" )) /* if [!] str1 == str2 */
      m = stricmp( cmd->c_argv[k] , cmd->c_argv[m] ) ? 0 : 1 ;
    else /* if [!] val1 {eq|gt|lt} val2 */
    {
      k = atol( cmd->c_argv[k] ) ;
      m = atol( cmd->c_argv[m] ) ;
      if (! stricmp( cmd->c_argv[l] , "eq" )) m = ( k == m ) ;
      else if (! stricmp( cmd->c_argv[l] , "gt" )) m = ( k > m ) ;
      else if (! stricmp( cmd->c_argv[l] , "lt" )) m = ( k < m ) ;
      else if (! stricmp( cmd->c_argv[l] , "ne" )) m = ( k != m ) ;
      else if (! stricmp( cmd->c_argv[l] , "ge" )) m = ( k >= m ) ;
      else if (! stricmp( cmd->c_argv[l] , "le" )) m = ( k <= m ) ;
      else goto _if_err ;
    }
  }

  if ( flg ) m = ! m ;
  CurrentFile->f_if[CurrentFile->f_nbif] = ( m ) ? FI_TRUE : 0 ;
  CurrentFile->f_nbif++ ;
  return( RETURN_OK ) ;

_if_err:

  IERROR( ERROR_BAD_ARGS ) ;
  return( RETURN_FAIL ) ;
}

/***************************************************************************/

int  do_else( void )
{
  int  k ;
  struct MyFile *f ;

  f = CurrentFile ;
  if (! f->f_nbif) goto _eerr ;

  k = f->f_nbif - 1 ;
  if ( f->f_if[k] & FI_ELSE ) goto _eerr ;

  f->f_if[k] |= FI_ELSE ;
  if ( f->f_if[k] & FI_COPY ) return( RETURN_OK ) ;

  if ( f->f_if[k] & FI_TRUE )
    f->f_if[k] &= ~FI_TRUE ;
  else
    f->f_if[k] |= FI_TRUE ;
  return( RETURN_OK ) ;

_eerr:

  pError( ERROR_IF_ELSE_ENDIF , "else" ) ;
  return( RETURN_FAIL ) ;

}

/***************************************************************************/

int  do_endif( void )
{
  if (! CurrentFile->f_nbif)
  {
    pError( ERROR_IF_ELSE_ENDIF , "endif" ) ;
    return( RETURN_FAIL ) ;
  }

  CurrentFile->f_nbif-- ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_stack( CMD *cmd )
{
  int  l ;

#ifdef _AMIGA
  switch ( cmd->c_argc )
  {
    case 1  : l = atol( cmd->c_argv[0] ) ;
	      if ( l < 128 )
	      {
		IERROR( ERROR_BAD_ARGS ) ;
		return( RETURN_FAIL ) ;
	      }
	      TaillePile = l ;
	      if ( MonCli ) MonCli->cli_DefaultStack = l >> 2 ;
    case 0  : fprintf( cmd->c_out , MyLocaleStr( MSG_STACKSIZE ) , TaillePile ) ;
	      break ;
    default : IERROR( ERROR_BAD_ARGS ) ;
	      return( RETURN_FAIL ) ;
  }
#endif
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_read( CMD *cmd )
{
  int  flg ;

  flg = 0 ;
  if ( (cmd->c_argc == 2) && (! strcmp( cmd->c_argv[0] , "-f" )) ) flg = 1 ;

  if ( (cmd->c_argc != 1) && (! flg) )
  {
    IERROR( ERROR_BAD_ARGS ) ;
    return( RETURN_FAIL ) ;
  }

  for (;;)
  {
    if (! FGetline( cmd->c_in , tmp , MAXLINE )) return( RETURN_FAIL ) ;
    if ( (! flg) || (*tmp) ) break ;
  }

  if ( BadVarName( cmd->c_argv[flg] , cmd->c_name ) ) return( RETURN_FAIL ) ;
  SetLocal( cmd->c_argv[flg] , tmp , 0 ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_failat( CMD *cmd )
{
  unsigned char c ;

  switch ( cmd->c_argc )
  {
    case 1  : c = (unsigned char)*(cmd->c_argv[0]) ;
	      if (! isdigit( c ))
	      {
		IERROR( ERROR_BAD_ARGS ) ;
		return( RETURN_FAIL ) ;
	      }
	      FailLevel = atol( cmd->c_argv[0] ) ;
#ifdef _AMIGA
	      if ( MonCli ) MonCli->cli_FailLevel = FailLevel ;
#endif
    case 0  : fprintf( cmd->c_out , MyLocaleStr( MSG_FAILLEVEL ) , FailLevel ) ;
	      break ;
    default : IERROR( ERROR_BAD_ARGS ) ;
	      return( RETURN_FAIL ) ;
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_cd( CMD *cmd )
{
/*
#ifdef _AMIGA
  struct DevProc *dp ;
#endif
*/

#ifdef unix

  if ( ! cmd->c_argc ) /* no arguments : go back to $HOME */
  {
    char *p ;

    p = getenv( "HOME" ) ;
    if ( p )
    {
      cmd->c_argc++ ;
      cmd->c_argv[0] = strdup( p ) ;
    }
  }

#else  /* unix */

  if ( ! cmd->c_argc ) /* no arguments : print current directory */
  {
    MyGetCwd( tmp ) ;
    fprintf( cmd->c_out , "%s\n" , tmp ) ;
    mySetVar( "cwd" , tmp , TRUE ) ;
    return( RETURN_OK ) ;
  }

#endif /* unix */

  if ( cmd->c_argc != 1 )
  {
    IERROR( ERROR_BAD_ARGS ) ;
    return( RETURN_FAIL ) ;
  }

  if ( stat( cmd->c_argv[0] , &sbuf ) )
  {
    IOERROR( cmd->c_argv[0] ) ;
    return( RETURN_FAIL ) ;
  }

  if (! (sbuf.st_mode & S_IFDIR))
  {
    pError( ERROR_OBJECT_WRONG_TYPE , cmd->c_argv[0] ) ;
    return( RETURN_FAIL ) ;
  }

  chdir( cmd->c_argv[0] ) ;
/*
#ifdef _AMIGA
  SetCurrentDirName( cmd->c_argv[0] ) ;
  if ( dp = GetDeviceProc( cmd->c_argv[0] , NULL ) )
  {
    SetFileSysTask( dp->dvp_Port ) ;
    FreeDeviceProc( dp ) ;
  }
#endif
*/
  getcwd( tmp , MAXLINE+1 ) ;
  mySetVar( "cwd" , tmp , TRUE ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_pushd( CMD *cmd )
{
  if ( cmd->c_argc != 1 )
  {
    IERROR( ERROR_BAD_ARGS ) ;
    return( RETURN_FAIL ) ;
  }

  MyGetCwd( aux ) ;

  if ( do_cd( cmd ) ) return( 1 ) ;

  if ( DirTop >= MAXARG )
  {
    IERROR( ERROR_OVERFLOW ) ;
    return( RETURN_FAIL ) ;
  }

  if (! (DirStack[DirTop] = strdup( aux ))) return( 1 ) ;
  DirTop++ ;

  fprintf( cmd->c_out , "%s\n" , aux ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_popd( CMD *cmd )
{
  int  lg, k ;

  if ( cmd->c_argc == 1 ) /* popd nb */
  {
    if (! isdigit( (unsigned char)*cmd->c_argv[0] )) goto _perr ;
    lg = atol( cmd->c_argv[0] ) ;
    if ( (lg < 0) || (lg >= DirTop) ) goto _perr ;
    for ( k = lg ; k < DirTop ; k++ ) free( DirStack[k] ) ;
    DirTop = lg ;
    return( RETURN_OK ) ;
  }

  if ( cmd->c_argc ) goto _perr ;

  if ( DirTop < 1 )
  {
    ERROR( ERROR_NO_SUCH_DIR ) ;
    return( RETURN_FAIL ) ;
  }

  DirTop-- ;
  cmd->c_argc = 1 ;
  cmd->c_argf[0] = 0 ;
  cmd->c_argv[0] = DirStack[DirTop] ;

  return( do_cd( cmd ) ) ;

_perr:

  IERROR( ERROR_BAD_ARGS ) ;
  return( RETURN_FAIL ) ;
}

/***************************************************************************/

int  do_dirs( CMD *cmd )
{
  int  k ;

  for ( k = 0 ; k < DirTop ; k++ )
    fprintf( cmd->c_out , "%ld\t%s\n" , k , DirStack[k] ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_for( CMD *cmd )
{
  int  k ;
  unsigned char c ;
  struct Loop *p ;

  if ( (cmd->c_argc < 3) ||
       (stricmp( cmd->c_argv[1] , "in" ) && stricmp( cmd->c_argv[1] , "from" )) )
  {
    IERROR( ERROR_BAD_ARGS ) ;
    return( RETURN_FAIL ) ;
  }

  if (! (p = (struct Loop *)myalloc( sizeof(struct Loop) , 1 )))
    return( RETURN_FAIL ) ;

  LoopNumber++ ;
  sprintf( p->l_tmpf , "%ss%ldl%ld" , TMPDIR , ShellNumber , LoopNumber ) ;
  if (! (p->l_desc = fopen( p->l_tmpf , "w" )))
  {
    pError( ERROR_CANNOT_OPEN , p->l_tmpf ) ;
    FreeLoop( p ) ;
    return( RETURN_FAIL ) ;
  }

  p->l_var = cmd->c_argv[0] ;
  cmd->c_argv[0] = NULL ;

  /* is it "for i from file" ? */

  if (! stricmp( cmd->c_argv[1] , "from" ))
  {
    p->l_flags |= LF_FROMFILE ;
    p->l_val[0] = cmd->c_argv[2] ;
    cmd->c_argv[2] = NULL ;
    goto _for_ok ;
  }

  /* normal "for i in val..." */

  p->l_nbval = 0 ;
  for ( k = 2 ; k < cmd->c_argc ; k++ )
  {
    p->l_valf[k-2] = cmd->c_argf[k] ;
    p->l_val[k-2] = cmd->c_argv[k] ;
    cmd->c_argv[k] = NULL ;
    p->l_nbval++ ;
  }

  /* is it "for var in n .. m" ? */

  if ( (p->l_nbval == 3) &&
       (! (p->l_valf[1] & CA_QUOTED)) &&
       (! strcmp( p->l_val[1] , ".." )) )
  {
    c = (unsigned char) p->l_val[0][0] ;
    if ( (! isdigit( c )) && (c != '-') ) goto _for_err ;
    c = (unsigned char) p->l_val[2][0] ;
    if ( (! isdigit( c )) && (c != '-') ) goto _for_err ;
    p->l_flags |= LF_VAL2VAL ;
  }

_for_ok:

  p->l_next = CurrentFile->f_loop ;
  CurrentFile->f_loop = p ;
  return( RETURN_OK ) ;

_for_err:

  IERROR( ERROR_SYNTAX_ERROR ) ;
  FreeLoop( p ) ;
  return( RETURN_FAIL ) ;
}

/***************************************************************************/

int  do_done( CMD *cmd )
{
  FILE *f ;
  CMD DummyCmd ;
  struct Loop *p ;
  int  k, l, m, s ;

  if (! (p = CurrentFile->f_loop))
  {
    IERROR( ERROR_SYNTAX_ERROR ) ;
    return( RETURN_FAIL ) ;
  }

  fclose( p->l_desc ) ;
  p->l_desc = NULL ;

  DummyCmd.c_argc = 1 ;
  DummyCmd.c_in = cmd->c_in ;
  DummyCmd.c_out = cmd->c_out ;
  DummyCmd.c_argv[0] = p->l_tmpf ;
  DummyCmd.c_flags = cmd->c_flags | CF_FORDONE ;

  l = 0 ;
  if ( p->l_flags & LF_VAL2VAL ) /* for var in n .. m */
  {
    k = atol( p->l_val[0] ) ;
    m = atol( p->l_val[2] ) ;
    s = ( k <= m ) ? 1 : -1 ;
    for (;;)
    {
      sprintf( tmp , "%ld" , k ) ;
      SetLocal( p->l_var , tmp , 0 ) ;
      l = do_source( &DummyCmd ) ;
      if ( p->l_flags & (LF_BREAKED|LF_EXITED) ) break ;
      if ( l >= FailLevel ) break ;
      k += s ;
      if ( (s > 0) && (k > m) ) break ;
      if ( (s < 0) && (k < m) ) break ;
    }
  }
  else if ( p->l_flags & LF_FROMFILE ) /* for var from file */
  {
    f = fopen( p->l_val[0] , "r" ) ;
    if ( f )
    {
      while ( fgets( p->l_buf , MAXLINE , f ) )
      {
	k = strlen( p->l_buf ) - 1 ;
	if ( (k >= 0) && (p->l_buf[k] == '\n') ) p->l_buf[k] = '\0' ;
	SetLocal( p->l_var , p->l_buf , 0 ) ;
	l = do_source( &DummyCmd ) ;
	if ( p->l_flags & (LF_BREAKED|LF_EXITED) ) break ;
	if ( l >= FailLevel ) break ;
      }
      fclose( f ) ;
    }
    else IOERROR( p->l_val[0] ) ;
  }
  else for ( k = 0 ; k < p->l_nbval ; k++ ) /* for var in a b c d... */
  {
    SetLocal( p->l_var , p->l_val[k] , 0 ) ;
    l = do_source( &DummyCmd ) ;
    if ( p->l_flags & (LF_BREAKED|LF_EXITED) ) break ;
    if ( l >= FailLevel ) break ;
  }

  if ( p->l_flags & LF_EXITED ) CurrentFile->f_flags |= FF_EXITME ;
  if (! (CurrentFile->f_loop = p->l_next)) NestedLoop = 0 ;
  FreeLoop( p ) ;
  return( l ) ;
}

/***************************************************************************/

int  do_break( CMD *cmd )
{
  int  code ;

  if (! (CurrentFile->f_flags & FF_FORDONE))
  {
    IERROR( ERROR_SYNTAX_ERROR ) ;
    return( RETURN_FAIL ) ;
  }

  code = ( cmd->c_argc > 0 ) ? atol( cmd->c_argv[0] ) : 0 ;
  CurrentFile->f_flags |= FF_BREAKME ;
  cmd->c_flags &= ~CF_BACKGROUND ;
  return( code ) ;
}

/***************************************************************************/

int  do_jobs( CMD *cmd )
{
  CMD *p, *q ;

  for ( p = (CMD *)ChildList.lh_Head ; q = (CMD *)p->c_node.ln_Succ ; p = q )
    fprintf( cmd->c_out , "[%ld] %s %s\n" , p->c_procnum , p->c_name , p->c_args ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_eval( CMD *cmd )
{
  char *p ;
  int  k, flg ;

  flg = 0 ;
  if ( isatty( cmd->c_in->f_desc->_file ) ) flg |= 1 ;
  if ( IsSet( "lineedit" ) ) flg |= 2 ;

  for (;;)
  {
    if ( flg == 3 )
    {
      p = rawgets( cmd->c_in , tmp , "" ) ;
      if ( (! p) || (p == (char *)-1) ) break ;
      k = strlen( tmp ) ;
    }
    else k = FGetline( cmd->c_in , tmp , MAXLINE ) ;

    if ( k < 1 ) break ;

    if ( flg & 2 ) AddHist( tmp , k ) ;

    if (! strcmp( tmp , "x" )) break ;

    if ( (k > 0) && (p = Eval( tmp )) )
    {
      fprintf( cmd->c_out , "%s\n" , p ) ;
      free( p ) ;
    }
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_echo( CMD *cmd )
{
  int  k, l ;

  k = 0 ;
  l = 1 ; /* bit 0 set if we must add '\n', bit 1 set if we must add ' ' */

  if ( (cmd->c_argc > 0) &&
       (! strcmp( cmd->c_argv[0] , "-c" )) &&
       (! (cmd->c_argf[0] & CA_QUOTED)) )
  {
    l &= ~1 ;
    k++ ;
  }

  while ( k < cmd->c_argc )
  {
    if ( l & 2 ) fputc( ' ' , cmd->c_out ) ;
    fputs( cmd->c_argv[k] , cmd->c_out ) ;
    l |= 2 ;
    k++ ;
  }

  if ( l & 1 ) fputc( '\n' , cmd->c_out ) ;
  fflush( stdout ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int  do_window( CMD *cmd )
{
#ifdef _AMIGA
  int  x, y ;

  switch ( cmd->c_argc )
  {
    case 0  : fprintf( cmd->c_out , "%d %d %d %d\n" , ConWin->LeftEdge ,
		       ConWin->TopEdge , ConWin->Width , ConWin->Height ) ;
    case 1  : if (! stricmp( cmd->c_argv[0] , "front" )) WindowToFront( ConWin ) ;
	      else if (! stricmp( cmd->c_argv[0] , "back" )) WindowToBack( ConWin ) ;
	      break ;
    case 3  : if (! stricmp( cmd->c_argv[0] , "move" ))
	      {
		x = atol( cmd->c_argv[1] ) - ConWin->LeftEdge ;
		y = atol( cmd->c_argv[2] ) - ConWin->TopEdge  ;
		if ( x || y ) MoveWindow( ConWin , x , y ) ;
		break ;
	      }
	      if (! stricmp( cmd->c_argv[0] , "size" ))
	      {
		x = atol( cmd->c_argv[1] ) - ConWin->Width  ;
		y = atol( cmd->c_argv[2] ) - ConWin->Height ;
		if ( x || y ) SizeWindow( ConWin , x , y ) ;
		break ;
	      }
	      break ;
    default : IERROR( ERROR_BAD_ARGS ) ;
	      return( RETURN_FAIL ) ;
  }
#endif

  return( RETURN_OK ) ;
}
