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

#define SRC_HIST_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

int HistCount ;
struct List HistList ;
extern int HistNumber ;
static char HistFile[MAXCMD+1] ;

/***************************************************************************/

void FreeHist( void )
{
  struct Hist *p, *q ;

  HistCount = 0 ;
  for ( p = (struct Hist *)HistList.lh_Head ; q = (struct Hist *)p->h_node.ln_Succ ; p = q )
    free( p ) ;
}

/***************************************************************************/

void AddHist( char *line , int len )
{
  int k, l ;
  char *h ;
  struct Hist *p ;

  if ( ! len ) return ;

  /* get "history" variable, if not defined or < 1 exit */

  if (! (h = myGetVar( "history" ))) return ;
  k = atol( h ) ;
  if ( k < 1 ) return ;

  /* get "fullhist" variable, and check if 'line' is new */

  l = 1 ;
  if ( h = myGetVar( "fullhist" ) ) l = strcmp( h , "1" ) ;
  if ( l )
  {
    p = (struct Hist*)HistList.lh_TailPred ;
    if ( (p->h_node.ln_Succ) && (! strcmp( p->h_line , line )) ) return ;
  }

  /* get "histmin" variable, and check line len */

  if ( (h = myGetVar( "histmin" )) && (len < atol( h )) ) return ;

  /* remove first line if history is full */

  while ( k <= HistCount )
  {
    p = (struct Hist *)HistList.lh_Head ;
    RemHead( &HistList ) ;
    HistCount-- ;
    free( p ) ;
  }

  /* add new line at end of history */

  if (! (p = (struct Hist *)myalloc( (len + sizeof(struct Hist)) , 0 ))) return ;
  HistCount++ ;
  HistNumber++ ;
  p->h_count = HistNumber ;
  strcpy( p->h_line , line ) ;
  AddTail( &HistList , (struct Node *)p ) ;
}

/***************************************************************************/

int LoadHist( void )
{
  char *p ;
  int lg ;
  struct MyFile *f ;
  static char line[MAXLINE+1] ;

  if (! (p = myGetVar( "histfile" ))) p = HistFile ;

  if (! (f = FOpen( p , "r" ))) return( RETURN_FAIL ) ;

  while ( (lg = FGetline( f , line , MAXLINE )) > 0 ) AddHist( line , lg ) ;
  FClose( f , 1 ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

void InitHist( void )
{
  HistCount = 0 ;
  NewList( &HistList ) ;
  strcpy( HistFile , BuildName( ".history" ) ) ;
  LoadHist() ;
}

/***************************************************************************/

int ExpandHist( char *buf )
{
  int nb, event ;
  struct Hist *p ;

  if ( *buf == '!' ) event = HistNumber ;
  else if ( isdigit( (unsigned char)*buf ) ) event = atol( buf ) ;
  else /* we have "!string" */
  {
    if (! (event = strlen( buf ))) goto _nohist  ;
    for ( p = (struct Hist *)HistList.lh_TailPred ; p->h_node.ln_Pred ; p = (struct Hist *)p->h_node.ln_Pred )
      if (! strncmp( buf , p->h_line , (size_t)event ))
      {
	strcpy( buf , p->h_line ) ;
	return( p->h_count ) ;
      }
    goto _nohist ;
  }

  /* expand "!!" or "!<number>" */

  nb = event ;
  for ( p = (struct Hist *)HistList.lh_Head ; p->h_node.ln_Succ ; p = (struct Hist *)p->h_node.ln_Succ )
    if ( p->h_count == event )
    {
      strcpy( buf , p->h_line ) ;
      return( nb ) ;
    }

_nohist:

  *buf = '\0' ;
  return( -1 ) ;
}

/***************************************************************************/

int SaveHist( void )
{
  int k ;
  FILE *f ;
  char *h ;
  struct Hist *p, *q ;

  /* get "savehist" variable, if not defined or < 1 exit */

  if (! (h = myGetVar( "savehist" ))) return( RETURN_FAIL ) ;
  k = atol( h ) ;
  if ( k < 1 ) return( RETURN_FAIL ) ;

  /* go on the first command to save */

  k = HistCount - k ;
  if ( k < 0 ) k = 0 ;

  for ( p = (struct Hist *)HistList.lh_Head ; (k) && (q = (struct Hist *)p->h_node.ln_Succ) ; p = q )
    k-- ;

  /* open file and save commands */

  if (! (h = myGetVar( "histfile" ))) h = HistFile ;
  if (! (f = fopen( h , "w" ))) return( RETURN_FAIL ) ;

  for ( ; q = (struct Hist *)p->h_node.ln_Succ ; p = q ) fprintf( f , "%s\n" , p->h_line ) ;
  fclose( f ) ;
  return( RETURN_OK ) ;
}

