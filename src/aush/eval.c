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

#define SRC_EVAL_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

#define MAXREG 10

int RpnMem[MAXREG] ;

static char tmp[MAXLINE+1] ;
static int RpnStack[MAXARG] ;

/****************************************************************************/

void InitRegs( void )
{
  int k ;

  for ( k = 0 ; k < MAXREG ; k++ ) RpnMem[k] = 0. ;
}

/****************************************************************************/

static char *GetNumber( char *ptr , int *pval )
{
  char *p ;

  *pval = strtol( ptr , &p , 0 ) ;
  if ( p == ptr ) p = NULL ;
  return( p ) ;
}

/****************************************************************************/

static char *GetWord( char *ptr, char *dst )
{
  while ( (*ptr) && (! isspace( (unsigned char)*ptr )) )
  {
    *dst = *ptr ;
    dst++ ;
    ptr++ ;
  }
  *dst = '\0' ;
  return( ptr ) ;
}

/****************************************************************************/

char *Eval( char *mot )

/*
 * Evalue l'expression dans mot, et retourne l'adresse du résultat (alloué)
 */

{
  int n1, n2 ;
  char *p, *q ;
  int sp, obase, k ;

  sp = 0 ;
  p  = mot ;
  obase = 10 ;

  for (;;)
  {
    while ( isspace( (unsigned char)*p ) ) p++ ;
    if (! *p) break ;

    if ( sp > 0 ) n2 = RpnStack[sp-1] ;
    if ( sp > 1 ) n1 = RpnStack[sp-2] ;

    if ( strchr( "+*<>/%&|" , *p ) )
    {
      if ( sp < 2 ) goto _evalerr ;
      sp-- ;

      switch ( *p )
      {
	case '+' : RpnStack[sp-1] = n1 +  n2 ; break ;
	case '*' : RpnStack[sp-1] = n1 *  n2 ; break ;
	case '<' : RpnStack[sp-1] = n1 << n2 ; break ;
	case '>' : RpnStack[sp-1] = n1 >> n2 ; break ;
	case '/' : if (! n2) goto _evaldiv ;
		   RpnStack[sp-1] = n1 /  n2 ; break ;
	case '%' : if (! n2) goto _evaldiv ;
		   RpnStack[sp-1] = n1 %  n2 ; break ;
	case '&' : RpnStack[sp-1] = n1 &  n2 ; break ;
	case '|' : RpnStack[sp-1] = n1 |  n2 ; break ;
      }

      p++ ;
      continue ;
    }

    switch ( *p )
    {
      case 's' : if ( sp < 1 ) goto _evalerr ;
		 p++ ;
		 if ( (*p < '0') || (*p > '9') ) goto _evalerr ;
		 RpnMem[*p - '0'] = n2 ;
		 p++ ;
		 sp-- ;
		 break ;
      case 'l' : p++ ;
		 if ( (*p < '0') || (*p > '9') ) goto _evalerr ;
		 if ( sp < MAXARG )
		 {
		   RpnStack[sp] = RpnMem[*p - '0'] ;
		   sp++ ;
		 }
		 else ERROR( ERROR_OVERFLOW ) ;
		 p++ ;
		 break ;
      case '$' : p++ ;
		 p = GetWord( p , tmp ) ;
		 if ( BadVarName( tmp , NULL ) ) goto _evalerr ;
		 if ( q = myGetVar( tmp ) )
		   if ( GetNumber( q , &n1 ) )
		   {
		     if ( sp < MAXARG )
		     {
		       RpnStack[sp] = n1 ;
		       sp++ ;
		     }
		     else ERROR( ERROR_OVERFLOW ) ;
		     break ;
		   }
		 goto _evalerr ;
      case '-' : if (! isdigit( (unsigned char)p[1] ))
		 {
		   if ( sp < 2 ) goto _evalerr ;
		   p++ ;
		   sp-- ;
		   RpnStack[sp-1] = n1 - n2 ;
		   break ;
		 }
      default  : p = GetWord( p , tmp ) ;
		 if (! stricmp( tmp , "HEX" ))
		 {
		   obase = 16 ;
		   break ;
		 }
		 if (! GetNumber( tmp , &n1 )) goto _evalerr ;
		 if ( sp < MAXARG )
		 {
		   RpnStack[sp] = n1 ;
		   sp++ ;
		 }
		 else ERROR( ERROR_OVERFLOW ) ;
		 break ;
    }
  }

  if (! sp) return( NULL ) ;

  if ( sp > 0 )
  {
    sprintf( tmp , (obase == 10) ? "%ld" : "0x%lx" , RpnStack[sp-1] ) ;
    k = strlen( tmp ) + 1 ;
    if ( p = myalloc( k , 0 ) ) strcpy( p , tmp ) ;
    return( p ) ;
  }

_evalerr:

  pError( ERROR_SYNTAX_ERROR , mot ) ;
  return( NULL ) ;

_evaldiv:

  pError( ERROR_ZERO_DIVIDE , mot ) ;
  return( NULL ) ;
}

