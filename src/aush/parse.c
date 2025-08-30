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

/* #define DEBUG_ME */

#define SRC_PARSE_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"
#include "parse.h"

#ifdef _AMIGA
extern struct Process *Moi ;
#endif

extern struct MyFile *CurrentFile ;
extern char *DirStack[], InputPipe[] ;
extern int PipeNumber, ShellNumber, CmdNumber, DirTop, NestedLoop ;

static int status ;
static CMD *cmd, *newcmd ;
static char tmp[MAXLINE+1], toparse[MAXLINE+1] ;

/***************************************************************************/

static int addarg( CMD *cmd , char *mot )

/*
 * Ajoute "mot" comme dernier argument de la commande indiquée
 * Retourne 0 en cas d'erreur
 */

{
  char *p ;

  if ( cmd->c_argc >= MAXARG )
  {
    ERROR( ERROR_TOO_MUCH_ARGS ) ;
    return( 0 ) ;
  }

  if (! (p = strdup( mot ))) return( 0 ) ;
  cmd->c_argv[cmd->c_argc] = p ;
  cmd->c_argc++ ;
  return( 1 ) ;
}

/*****************************************************************************************/

static char *do_split( CMD *cmd , char *mot , int flg )

/*
 * Découpe la chaine pointée par "mot" suivant les blancs, en tenant
 * compte des délimiteurs de chaine.
 * Les mots sont ajoutés comme argument à la commande indiquée.
 * Si "flg" est vrai, les mots sont recollés dans la chaine "toparse",
 * et séparés par un seul espace.
 */

{
  char c, *p, *q ;

  p = mot ;
  *toparse = '\0' ;

  while ( *p )
  {
    while ( isspace( (unsigned char)*p ) ) p++ ;
    c = *p ;
    if ( ! c ) break ;
    if ( flg )
      if ( (c == ',') || (c == ';') || (c == '|') ) return( (char *)p ) ;

    q = tmp ;
    if ( (c == '"') || (c== '\'') || (c == '{') )
    {
      *q++ = *p++ ;
      if ( c == '{' ) c = '}' ;
      while ( (*p) && (*p != c) )
      {
	*q = *p ;
	if ( *p == '\\' )
	{
	  if ( p[1] ) { p++ ; q++ ; }
	  *q = *p ;
	}
	p++ ;
	q++ ;
      }
      if ( *p ) *q++ = *p++ ;
    }
    else while ( (c = *p) && (! isspace( (unsigned char)c )) )
    {
      if ( flg )
	if ( (c == ',') || (c == ';') || (c == '|') ) break ;
      *q++ = c ;
      p++ ;
    }
    *q = '\0' ;

    if ( flg )
    {
      if ( *toparse ) strcat( toparse , " " ) ;
      strcat( toparse , tmp ) ;
    }
    if (! addarg( cmd , tmp )) return( NULL ) ;
  }

  return( (char *)p ) ;
}

/*****************************************************************************************/

static int GetDirName( char *str )

/*
 * Extrait le numéro de répertoire de la forme "§n" dans la chaine str, et le
 * remplace par le répertoire correspondant dans la pile.
 * On note que le répertoire se termine toujours par ":" ou "/"
 */

{
  int k ;

  k = atol( &str[1] ) ;
  if ( (k < 0) || (k >= DirTop) ) return( 0 ) ;
  strcpy( str , DirStack[k] ) ;
  return( 1 ) ;
}

/*****************************************************************************************/

static int GetHistValue( char *str )

/*
 * Extrait un rappel d'historique dans la chaine str, et le remplace par la ligne
 * correspondante, ou par une chaine vide en cas d'erreur.
 */

{
  strcpy( tmp , &str[1] ) ;
  if ( ! *tmp ) return( 0 ) ;
  if ( ExpandHist( tmp ) == -1 ) return( 0 ) ;
  strcpy( str , tmp ) ;
  printf( "%s\n" , str ) ;
  return( 1 ) ;
}

/*****************************************************************************************/

static int GetExpValue( char *str )

/* Evalue l'expression dans la chaine str, et la remplace par le résultat */

{
  char *p ;

  if ( p = Eval( &str[1] ) )
  {
    strcpy( str , p ) ;
    return( 1 ) ;
  }
  return( 0 ) ;
}

/*****************************************************************************************/

static int GetPatValue( char *str , int flg )

/* Etend le motif dans la chaine "str" */

{
   if ( (! flg) && IsSet( "noexpand" ) ) return( 1 ) ;

   ExpandPat( str , NULL ) ;
   if ( ! *str )
   {
     ERROR( ERROR_NO_MATCH ) ;
     return( 0 ) ;
   }

   return( 1 ) ;
}

/*****************************************************************************************/

static int GetVarValue( char *str )

/*
 * Extrait le nom d'une variable de la chaine str, et le remplace par la valeur
 * de cette variable, ou par une chaine vide si la variable n'existe pas.
 * Si une fonction est indiquée, applique la fonction sur le contenu
 */

{
  char *fin, *val ;

  fin = strchr( str , ':' ) ;
  if ( fin ) *fin++ = '\0' ;

  val = str ;
  if ( *val == '$' ) val++ ;
  if ( *val == '{' ) val++ ;
  val = myGetVar( val ) ;
  if ( val ) strcpy( tmp , val ) ;
	else *tmp = '\0' ;

  if ( fin )
  {
    if (! stricmp( fin , "upper" ))       strupr( tmp ) ;
    else if (! stricmp( fin , "lower"  )) strlwr( tmp ) ;
    else if (! stricmp( fin , "len"    )) sprintf( tmp , "%ld" , strlen( tmp ) ) ;
    else if (! stricmp( fin , "first"  )) tmp[1] = '\0' ;
    else if (! stricmp( fin , "split"  )) status |= ST_SPLIT ;
    else if (! stricmp( fin , "expand" )) GetPatValue( tmp , TRUE ) ;
    else if (! stricmp( fin , "slead"  ))
    {
      for ( fin = tmp ; isspace( (unsigned char)*fin ) ; fin++ ) ;
      strcpy( str , fin ) ;
      return( 1 ) ;
    }
    else if (! stricmp( fin , "base" ))
    {
      fin = NomDeBase( tmp ) ;
      strcpy( tmp , fin ) ;
    }
    else if (! stricmp( fin , "noext" ))
    {
      fin = strrchr( tmp , '.' ) ;
      if ( fin ) *fin = '\0' ;
    }
    else
    {
      pError( ERROR_BAD_FUNCTION , fin ) ;
      return( 0 ) ;
    }
  }

  strcpy( str , tmp ) ;
  status |= ST_DQUOT ;
  return( 1 ) ;
}

/*****************************************************************************************/

static char *do_subst( char *mot , char etat )
{
  char *dst ;

#ifdef DEBUG_ME
printf( "substitue <%s> en quittant l'état %ld\n" , mot , etat ) ;
#endif

  if ( (*mot) && (! (status & ST_NOEXPAND)) )
  {
    switch ( etat )
    {
      case E_REP1 : if (! GetDirName( mot ))
		    {
		      ERROR( ERROR_NO_SUCH_DIR ) ;
		      return( NULL ) ;
		    }
		    break ;
      case E_HIS1 : if (! GetHistValue( mot )) return( NULL ) ;
		    break ;
      case E_EXP1 : if (! GetExpValue( mot )) return( NULL ) ;
		    break ;
      case E_VAR2 :
      case E_VAR3 :
      case E_VAR4 :
      case E_VAR5 : if (! GetVarValue( mot )) return( NULL ) ;
		    break ;
      case E_CTR1 : *mot = toupper( mot[1] ) - '@' ;
		    mot[1] = '\0' ;
		    break ;
      default	  : break ;
    }

    if ( status & ST_MOTIF )
    {
      status &= ~ST_MOTIF ;
      if (! GetPatValue( mot , FALSE )) return( NULL ) ;
      status |= ST_SPLIT ;
    }
  }

  for ( dst = mot ; *dst ; dst++ ) ;
  return( dst ) ;
}

/*****************************************************************************************/

static void do_prep( char *mot , char *tmp , char etat )
{
  int pos ;
  char *dst ;

  pos = 0 ;
  for ( dst = mot ; *dst ; dst++ ) ;
  if ( (etat == E_STR2) && (*tmp == '"') ) *dst++ = tmp[pos++] ;
  while ( isspace( (unsigned char)tmp[pos] ) ) *dst++ = tmp[pos++] ;
  *dst = '\0' ;
  if ( pos ) strcpy( tmp , &tmp[pos] ) ;
}

/*****************************************************************************************/

static int StatusSP ;
static struct FullStatus *StatusStack[MAXSTACK] ;

static void PopStatus( char *c, char *mot , char *tmp )
{
  struct FullStatus *p ;

  if ( (StatusSP >= 0) && (p = StatusStack[StatusSP]) )
  {
    *c = p->fs_etat ;
    strcpy( tmp , p->fs_tmp ) ;
    strcat( tmp , mot ) ;
    strcpy( mot , p->fs_mot ) ;
    free( p ) ;
    StatusSP-- ;
  }
  else *c = -1 ;
}

static void PushStatus( char st , char *mot , char *tmp )
{
  struct FullStatus *p ;

  if ( (StatusSP < MAXSTACK) && (p = myalloc( sizeof(struct FullStatus) , 0 )) )
  {
    StatusSP++ ;
    StatusStack[StatusSP] = p ;
    p->fs_etat = st ;
    strcpy( p->fs_mot , mot ) ;
    strcpy( p->fs_tmp , tmp ) ;
  }
  else printf( "\nINTERNAL ERROR: parse stack is full\n" ) ;
}

static void InitStack( void )
{
  StatusSP = -1 ;
}

/*****************************************************************************************/

static char *NextWord( char *src , char *mot )

/*
 * Examine le premier mot de la chaine "src", et retourne un pointeur sur
 * le premier caractère après ce mot.
 * Le mot analysé est copié dans la chaine "mot".
 */

{
  int pos ;
  char etat, netat, *dst ;
  static char tmp[MAXLINE+1] ;

  /* initialisation résultat */

  dst = tmp ;
  *mot = '\0' ;
  *tmp = '\0' ;
  status &= (ST_NOEXPAND|ST_ALIAS) ;

  while ( isspace( (unsigned char)*src ) ) src++ ;
#ifdef DEBUG_ME
  printf( "avant: <%s>\n" , src ) ;
#endif

  if ( (! *src) || (*src == ';') )
  {
    status |= ST_END ;
    return( src ) ;
  }

  /* analyse avec l'automate */

  netat = E_DEB ;
  InitStack() ;

  for ( etat = E_DEB ; netat != -1 ; etat = netat )
  {
    if ( ! *src )
    {
      if ( status & ST_END ) break ;
      status |= ST_END ;
      netat = -1 ;
      goto _parse ;
    }

    /* cherche l'état suivant dans l'automate */

    for ( pos = 0 ; ListeChar[pos] ; pos++ ) if ( ListeChar[pos] == *src ) break ;
    if ( ! ListeChar[pos] )
    {
      if ( isalpha( (unsigned char)*src ) )      pos = CHR_ALPHA ;
      else if ( isdigit( (unsigned char)*src ) ) pos = CHR_DIGIT ;
      else if ( isspace( (unsigned char)*src ) ) pos = CHR_SPACE ;
      else			  pos = CHR_OTHER ;
    }
    netat = Automate[etat][pos] ;

    if ( (etat == E_VAR1) && (*src == '*') ) netat = E_VAR4 ;

/*
#ifdef DEBUG_ME
    printf( "chr = <%lc>, etat = %ld => netat = %ld\n" , *src , etat , netat ) ;
#endif
*/

    if ( (netat == E_CHR1) && (*mot || *tmp) ) break ;

    /* empile l'état si besoin et avance */

    if ( netat >= 0 )
    {
      if ( (netat != etat) && (PushFlg[netat]) )
      {
	*dst = '\0' ;
	PushStatus( etat , mot , tmp ) ;
	*mot = '\0' ;
	dst = tmp ;
      }

      if ( strchr( ListeMotif , *src ) && ((netat == E_NOR1) || (netat == E_STR2)) )
      {
	status |= ST_MOTIF ;
#ifdef _AMIGA
	if ( *src == '*' ) /* transforme '*' en '#?' */
	{
	  *dst++ = '#' ;
	  *dst = '?' ;
	  goto _next ;
	}
#endif
      }

      *dst = *src ;
      if ( *src == '\\' )
      {
	if ( dst == tmp ) status |= ST_FESC ;
	*dst = src[1] ;
	src++ ;
      }
_next:
      if ( (netat != E_RED2) || (etat == netat) || (! strchr( "$\"'" , *src )) )
      {
	if ( *src ) src++ ;
	dst++ ;
      }

      if ( (netat == E_CTR1) && (dst[-1] != '^') ) netat = E_DEB ;
    }

    /* si besoin, fait une substitution dans "dst" */

_parse:

    if ( (netat < 0) ||
	 ((netat > etat) && SubstFlg[etat]) ||
	 ((etat == E_STR2) && isspace( (unsigned char)*src )) )
    {
      *dst = '\0' ;
      if ( etat == E_STR2 ) do_prep( mot , tmp , etat ) ;

      if (! do_subst( tmp , etat ))
      {
	status |= ST_ERROR ;
	break ;
      }
      if ( etat == E_HIS1 )
      {
	strcpy( toparse , tmp ) ;
	strcat( toparse , src ) ;
	src = toparse ;
      }
      else
      {
	pos = strlen( mot ) + strlen( tmp ) ;
	if ( pos >= MAXLINE )
	{
	  ERROR( ERROR_OVERFLOW ) ;
	  status |= ST_ERROR ;
	  break ;
	}
	strcat( mot , tmp ) ;
      }

      dst = tmp ;
      if ( netat == E_STR2 )
	while ( isspace( (unsigned char)*src ) ) *dst++ = *src++ ;
      *dst = '\0' ;
    }

    /* dépile l'état si besoin */

    if ( netat == E_POP ) src++ ;
    if ( netat < 0 )
    {
      pos = netat ;
      PopStatus( &netat , mot , tmp ) ;
      if ( pos != netat ) status &= ~ST_END ;
#ifdef DEBUG_ME
printf( "PopStatus : %ld\n" , netat ) ;
#endif
      for ( dst = tmp ; *dst ; dst++ ) ;
    }
  }

  /* ultime substitution */

  *dst = '\0' ;
  if ( status & ST_MOTIF ) do_prep( mot , tmp , etat ) ;
  do_subst( tmp , etat ) ;
  strcat( mot , tmp ) ;

  /* transmet le résultat */

  pos = 0 ;
  switch ( *mot )
  {
    case '`'  : status |= ST_SUBST ; status &= ~ST_SPLIT ; pos++ ; break ;
    case '"'  : status |= ST_DQUOT ; status &= ~ST_SPLIT ; pos++ ; break ;
    case '\'' : status |= ST_SQUOT ; pos++ ; break ;
    case '>'  :
    case '<'  : dst = tmp ;
		*dst++ = *mot ; pos++ ;
		if ( mot[1] == '>' ) { *dst++ = '>' ; pos++ ; }
		while ( isspace( (unsigned char)mot[pos] ) ) pos++ ;
		*dst = '\0' ;
		if (! addarg( cmd , tmp )) status |= ST_ERROR ;
		break ;
  }

  if ( pos ) strcpy( mot , &mot[pos] ) ;
#ifdef DEBUG_ME
printf( "mot = <%s>, status = %lx\n" , mot , status ) ;
#endif
  return( src ) ;
}

/***************************************************************************/

static int SetRedirection( CMD *cmd , char **std , int arg )

/* Prépare une redirection vers "std" (pointe sur c_stdin ou c_stdout) */

{
  if ( *std )                           /* vérifie pas déjà redirigé */
  {
    ERROR( ERROR_TOO_MUCH_REDIRS ) ;
    return( 0 ) ;
  }

  if ( arg >= cmd->c_argc )             /* vérifie que le nom existe */
  {
    ERROR( ERROR_SYNTAX_ERROR ) ;
    return( 0 ) ;
  }

  *std = cmd->c_argv[arg] ;		/* copie le nom et détruit l'argument */
  cmd->c_argv[arg] = NULL ;
  if ( (**std == '"') || (**std == '\'') ) strcpy( *std , (*std)+1 ) ;

  free( cmd->c_argv[arg-1] ) ;          /* libère le symbole de redirection */
  cmd->c_argv[arg-1] = NULL ;
  return( 1 ) ;
}

/***************************************************************************/

int BuildCmd( CMD *cmd )
{
  int k, l ;
  char *p, c ;

#ifdef _AMIGA
  cmd->c_pri = Moi->pr_Task.tc_Node.ln_Pri ;
#endif
#ifdef unix
  cmd->c_pri = nice( 0 ) + 20 ;
#endif

  /* scan cmd->c_argv[] */

  for ( k = 0 ; k < cmd->c_argc ; k++ )
  {
    p = cmd->c_argv[k] ;
    if ( cmd->c_argf[k] ) continue ;

    switch ( *p )
    {
      case '>' : if ( p[1] == '>' ) cmd->c_flags |= CF_APPENDOUT ;
		 if (! SetRedirection( cmd , &(cmd->c_stdout) , ++k )) return( 0 ) ;
		 break ;
      case '<' : if (! SetRedirection( cmd , &(cmd->c_stdin) , ++k )) return( 0 ) ;
		 break ;
      case '|' : if ( cmd->c_stdout )
		 {
		   ERROR( ERROR_TOO_MUCH_REDIRS ) ;
		   return( 0 ) ;
		 }
		 p = myalloc( MAXNAME+1 , 0 ) ;
		 if (! p) return( 0 ) ;
#ifdef _AMIGA
		 if ( IsSet( "truepipes" ) )
		 {
		   sprintf( p , "%ss%ldp%ld" , PIPDEV , ShellNumber , PipeNumber ) ;
		   cmd->c_flags |= (CF_PIPEOUT|CF_BACKGROUND) ;
		 }
		 else
#endif
		 {
		   sprintf( p , "%ss%ldp%ld" , TMPDIR , ShellNumber , PipeNumber ) ;
		   cmd->c_flags |= (CF_PIPEOUT|CF_RAMPIPE) ;
		 }
		 cmd->c_stdout = p ;
		 PipeNumber++ ; /* pas de 'break' ! */
      case ',' : free( cmd->c_argv[k] ) ;
		 cmd->c_argv[k] = NULL ;
		 break ;
      case '&' : cmd->c_flags |= CF_BACKGROUND ;
      case '@' : if ( (*p == '@') && (p[1] == '\0') )
		 {
		   ERROR( ERROR_SYNTAX_ERROR ) ;
		   return( 0 ) ;
		 }
		 p++ ;
		 if ( *p != '\0' ) cmd->c_pri = atol( p ) ;
		 free( cmd->c_argv[k] ) ;
		 cmd->c_argv[k] = NULL ;
		 break ;
      default  : break ;
    }
  }

  /* check if we must add input pipe */

  if ( *InputPipe )
  {
    if ( cmd->c_stdin )
    {
      ERROR( ERROR_TOO_MUCH_REDIRS ) ;
      return( 0 ) ;
    }
    if (! (cmd->c_stdin = strdup( InputPipe ))) return( 0 ) ;
#ifdef _AMIGA
    if (! IsSet( "truepipes" )) cmd->c_flags |= CF_RAMPIPE ;
    cmd->c_flags |= CF_PIPEIN ;
#endif
#ifdef unix
    cmd->c_flags |= (CF_PIPEIN|CF_RAMPIPE) ;
#endif
    *InputPipe = '\0' ;
  }

  /* if piped out, clear background flag */

  if ( (cmd->c_flags & CF_PIPEOUT) && (cmd->c_flags & CF_RAMPIPE) ) cmd->c_flags &= ~CF_BACKGROUND ;

  /* redirect to NIL: if background */

  if ( cmd->c_flags & CF_BACKGROUND )
  {
    if ( (! (cmd->c_stdin )) && (! (cmd->c_stdin  = strdup( NULDEV ))) ) return( 0 ) ;
    if ( (! (cmd->c_stdout)) && (! (cmd->c_stdout = strdup( NULDEV ))) ) return( 0 ) ;
  }

  /* rebuild cmd->c_argv[] */

  cmd->c_arglen = 0 ;

  for ( k = l = 0 ; k < cmd->c_argc ; k++ )
  {
    if (! cmd->c_argv[k]) continue ;
    c = cmd->c_argf[k] ;
    cmd->c_argv[l] = cmd->c_argv[k] ;
    cmd->c_argf[l] = c ;
    if ( c != CA_SUBST )
    {
      cmd->c_arglen += strlen( cmd->c_argv[l] ) + 1 ;
      if ( c & CA_QUOTED ) cmd->c_arglen += 2 ;
    }
    l++ ;
  }

  cmd->c_argc = l ;
  return( 1 ) ;
}

/***************************************************************************/

static int get_alias( char *dst , struct Alias *a )
{
  char *p ;
  int k, lg ;

#ifdef DEBUG_ME
printf( "before <%s>, tmp <%s>, toparse <%s>\n" , a->a_before , tmp , toparse ) ;
#endif

  lg = 0 ;
  for ( p = a->a_before ; *p ; p++ )
    if ( *p == '%' )
    {
      p++ ;
      if ( (*p < '1') || (*p > '9') )
      {
	pError( ERROR_SYNTAX_ERROR , p ) ;
	return( 0 ) ;
      }
      k = *p - '1' ;
      if ( ! newcmd->c_argv[k] )
      {
	pError( ERROR_BAD_ARGS , a->a_name ) ;
	return( 0 ) ;
      }
      lg += strlen( newcmd->c_argv[k] ) ;
      if ( lg >= MAXLINE )
      {
	ERROR( ERROR_OVERFLOW ) ;
	return( 0 ) ;
      }
      strcpy( dst , newcmd->c_argv[k] ) ;
      while ( *dst ) dst++ ;
    }
    else if ( (*p == '[') && (p[1] == ']') )
    {
      lg += strlen( toparse ) ;
      if ( lg >= MAXLINE )
      {
	ERROR( ERROR_OVERFLOW ) ;
	return( 0 ) ;
      }
      strcpy( dst , toparse ) ;
      while ( *dst ) dst++ ;
      p++ ;
    }
    else
    {
      lg++ ;
      if ( lg >= MAXLINE )
      {
	ERROR( ERROR_OVERFLOW ) ;
	return( 0 ) ;
      }
      *dst++ = *p ;
      if ( (*p == '\\') && p[1] )
      {
	p++ ;
	lg++ ;
	*dst++ = *p ;
      }
    }

  lg += strlen( tmp ) ;
  if ( lg >= MAXLINE )
  {
    ERROR( ERROR_OVERFLOW ) ;
    return( 0 ) ;
  }
  strcpy( dst , tmp ) ;
  return( 1 ) ;
}

/***************************************************************************/

CMD *SplitLine( char *line , int doalias )
{
  int lg ;
  char *p ;
  struct Loop *b ;
  struct Alias *a ;
  static char mot[MAXLINE+1] ;

  cmd = (CMD *)myalloc( sizeof(CMD) , 1 ) ;
  if ( ! cmd ) return( NULL ) ;
  cmd->c_flags = CF_TOEXEC ;

  sprintf( mot , "_%ld_%ld" , ShellNumber , CmdNumber ) ;
  mySetVar( "cmdnum" , mot , TRUE ) ;
  CmdNumber++ ;

  status = 0 ;
  b = CurrentFile->f_loop ;
  if ( b && b->l_desc ) status |= ST_NOEXPAND ;

  p = line ;

_alias:

  /* get command name */

  p = NextWord( p , mot ) ;
  if ( (! *mot) || (*mot == ';') || (status & ST_ERROR) )
  {
_abort:
    FreeCmd( cmd ) ;
    return( NULL ) ;
  }

  if ( status & ST_SPLIT )
  {
    if (! do_split( cmd , mot , 0 )) goto _abort ;
    strcpy( mot , cmd->c_argv[0] ) ;
    free( cmd->c_argv[0] ) ;
    cmd->c_argv[0] = NULL ;
  }
#ifdef DEBUG_ME
  printf( "1:c_name = <%s>\n" , mot ) ;
#endif

  /* test if we have "time <cmd>" */

  if (! stricmp( mot , "time" ))
  {
    cmd->c_flags |= CF_TIME ;
    goto _alias ;
  }

  /* test if we have an alias */

  if ( status & ST_FESC ) doalias = 0 ;
  if (! stricmp( mot , "alias" )) status |= ST_ALIAS ;

  if ( (doalias) && (a = GetAlias( mot )) )
  {
    if (! (newcmd = (CMD *)myalloc( sizeof(CMD) , 1 ))) goto _abort ;
    p = do_split( newcmd , p , 1 ) ;
    strcpy( tmp , (char *)p ) ;
    lg = get_alias( (char *)line , a ) ;
    free( newcmd ) ;
    if ( ! lg ) goto _abort ;
#ifdef DEBUG_ME
printf( "2:alias = <%s>\n" , line ) ;
#endif
    p = line ;
    goto _alias ;
  }

  /* ok, store command */

  line = p ;
  if (! (cmd->c_name = strdup( mot ))) goto _abort ;

  /* loop handling */

  if (! stricmp( mot , "done" ))
    if ( NestedLoop > 0 ) NestedLoop-- ;

  if (! stricmp( mot , "for" )) NestedLoop++ ;

  /* get rest of line */

  do
  {
    line = NextWord( line , mot ) ;
#ifdef DEBUG_ME
    printf( "3:mot = <%s>, status = %lx\n" , mot , status ) ;
#endif

    if ( (status & ST_ERROR) || (! *mot) ) break ;

    if (! (status & (ST_SQUOT|ST_DQUOT)))
    {
      if ( (*mot == ',') || (*mot == '|') )
      {
	if (! (cmd->c_pipe = strdup( (char *)line ))) goto _abort ;
	if (! addarg( cmd , mot )) goto _abort ;
	*line = '\0' ;
	break ;
      }

      for ( lg = strlen( mot ) - 1 ; (lg >= 0) && isspace( (unsigned char)mot[lg] ) ; lg-- ) ;
      if ( lg < 0 ) lg = 0 ;
      mot[lg+1] = '\0' ;
    }

    if (! (status & ST_SPLIT))
    {
      p = &(cmd->c_argf[cmd->c_argc]) ;
      if (! addarg( cmd , mot )) goto _abort ;

      *p = '\0' ;
      if ( status & ST_FESC  )      *p = CA_FESC ;
      else if ( status & ST_SUBST ) *p = CA_SUBST ;
      else if ( status & ST_SQUOT ) *p = CA_SQUOTED ;
      else if ( status & ST_DQUOT ) *p = CA_DQUOTED ;
    }
    else if (! do_split( cmd , mot , 0 )) goto _abort ;

    if ( (status & ST_ALIAS) && (cmd->c_argc == 1) )
    {
      for ( p = line ; isspace( (unsigned char)*p ) ; p++ ) ;
      if ( *p )
      {
	if (! addarg( cmd , (char *)p )) goto _abort ;
	break ;
      }
    }
  }
  while (! (status & ST_END )) ;

  if ( status & ST_ERROR ) goto _abort ;

#ifdef DEBUG_ME
printf( "cmd->c_name = %s (%d/%x)\n" , cmd->c_name , cmd->c_argc , cmd->c_flags ) ;
if ( cmd->c_pipe ) printf( "cmd->c_pipe = %s\n" , cmd->c_pipe );
for ( lg = 0 ; lg < cmd->c_argc ; lg++ )
  printf( "%d : <%s>\n" , lg , cmd->c_argv[lg] ) ;
#endif

  return( cmd ) ;
}

