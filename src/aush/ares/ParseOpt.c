/*
* This file is part of ARes.
* Copyright (C) 1995 Denis Gounelle
* 
* ARes is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* ARes is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with ARes.  If not, see <http://www.gnu.org/licenses/>.
*
*/
/***************************************************************************
 *
 * ARes (C)1990-1994 par Denis GOUNELLE
 *
 ***************************************************************************/

#include "ARes.h"

static long Sargc ,	/* saved argc	*/
	    Cargc ,	/* current argc */
	    Margc ;	/* my argc	*/

static char **Sargv ,	/* saved argv	*/
	    **Margv ,	/* my argv	*/
	    tmp[64] ;	/* workspace	*/

/***************************************************************************/

long PreParseOpt( long argc , char **argv , char *specifs )

{
  long k, l ;
  char *p, *d ;

  Cargc = 0 ;
  Sargc = argc ;
  Sargv = argv ;

  /* compte le nombre de mots */

  for ( p = specifs , k = 0 ; *p != '\0' ; p++ ) if ( *p == ',' ) k++ ;
  k++ ;

  /* alloue Margv */

  Margc = k ;
  k <<= 2L ;
  Margv = calloc( k , 4 ) ;
  if (! Margv) return( 0 ) ;

  /* initialise Margv */

  l = 0 ;
  p = specifs ;
  do
  {
    for ( k = 0 , d = tmp ; (*p != ',') && (*p != '\0') ; k++ , d++ , p++ ) *d = *p ;
    if ( *p == ',' ) p++ ;
    *d = '\0' ;
    k++ ;
    Margv[l] = malloc( k ) ;
    if (! Margv[l]) goto _ppo_failed ;
    strcpy( Margv[l] , tmp ) ;
    l++ ;
  }
  while ( *p != '\0' ) ;

  return( 1 ) ;

_ppo_failed:

  for ( l-- ; l >= 0 ; l-- ) free( Margv[l] ) ;
  free( Margv ) ;
  return( 0 ) ;

}

/***************************************************************************/

char *ParseOpt( long *opt )

{
  long k ;
  char *p ;

  Cargc++ ;
  if ( Cargc >= Sargc ) return( NULL ) ;

  /* cherche l'argument courant dans Margv */

  p = Sargv[ Cargc ] ;
  for ( k = 0 ; k < Margc ; k++ )
    if (! stricmp( Margv[k] , p ))
    {
      *opt = k ;
      return( p ) ;
    }

  /* pas trouvé ! */

  *opt = -1 ;
  return( p ) ;
}

/***************************************************************************/

char *ParseNext( void )

{
  long k ;
  char *p ;

  p = ParseOpt( &k ) ;
  if (! p) return( NULL ) ;
  if ( k == -1 ) return( p ) ;
  Cargc-- ;
  return( NULL ) ;
}

/***************************************************************************/

void EndParseOpt( void )

{
  long l ;

  for ( l = 0 ; l < Margc ; l++ ) free( Margv[l] ) ;
  free( Margv ) ;
}

