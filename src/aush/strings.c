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

char *NomDeBase( char *src )    /* returns basename of name in "src" */
{
  char *p ;

  p = strrchr( src , '/' ) ;
  if ( p ) return( &p[1] ) ;
  return( src ) ;
}

/*******************************************************************/

#ifdef unix
char *strlwr( char *str )       /* change "str" to lower case */
{
  char *p ;

  for ( p = str ; *p ; p++ ) *p = tolower( *p ) ;
  return( str ) ;
}
#endif

/*******************************************************************/

#ifdef unix
char *strupr( char *str )       /* change "str" to upper case */
{
  char *p ;

  for ( p = str ; *p ; p++ ) *p = toupper( *p ) ;
  return( str ) ;
}
#endif

/*******************************************************************/

#ifdef unix
int stricmp( char *s1 , char *s2 )
{
  int d ;

  while ( *s1 )
  {
    if ( ! *s2 ) return( 1 ) ;
    d = tolower( *s1++ ) - tolower( *s2++ ) ;
    if ( d ) return( d ) ;
  }

  if ( *s2 ) return( -1 ) ;
  return( 0 ) ;
}
#endif

/*******************************************************************/

#ifdef unix
int strnicmp( char *s1 , char *s2 , int len )
{
  int d ;

  while ( (len) && (*s1) )
  {
    if ( ! *s2 ) return( 1 ) ;
    d = tolower( *s1++ ) - tolower( *s2++ ) ;
    if ( d ) return( d ) ;
    len-- ;
  }

  if ( *s2 ) return( -1 ) ;
  return( 0 ) ;
}
#endif

/*******************************************************************/

char *strpcpy( char *dst , char *src )
{
  while ( *src ) *dst++ = *src++ ;
  *dst = '\0' ;
  return( dst ) ;
}

/*******************************************************************/

char *strxcat( char *dst , char *s1 , char *s2 , char *s3 , char *s4 )
{
  while ( *dst ) dst++ ;

  if ( s1 )
  {
    while ( *s1 ) *dst++ = *s1++ ;
    if ( s2 )
    {
      while ( *s2 ) *dst++ = *s2++ ;
      if ( s3 )
      {
	while ( *s3 ) *dst++ = *s3++ ;
	if ( s4 ) while ( *s4 ) *dst++ = *s4++ ;
      }
    }
  }

  *dst = '\0' ;
  return( dst ) ;
}

