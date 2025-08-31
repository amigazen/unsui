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
/****************************************************************************
 *
 * ARes (C)1990-1994 par Denis GOUNELLE
 *
 ****************************************************************************/

#include <stdio.h>
#include <time.h>
#include <libraries/dos.h>

static struct tm _ARes_DateEtHeure ;

static char _ARes_JoursParMois[12] =
{
  31 , 28 , 31 , 30 , 31 , 30 , 31 , 31 , 30 , 31 , 30 , 31
} ;

/****************************************************************************/

long Bissextile( long an )

/*
 * Pour être bissextile, une année doit avoir son millésime divisible par
 * 4. Toutefois celles dont le millésime est divisible par 100 ne sont
 * bissextiles que si leur millésime est également divisible par 400.
 */

{
  if ( an & 3L ) return( 0L ) ;         /* pas divisible par 4   */
  if ( an % 100L ) return( 1L ) ;       /* pas divisible par 100 */
  return( (an % 400L) ? 0L : 1L ) ;
}

/****************************************************************************/

struct tm *DateEtHeure( struct DateStamp *ptr )

/*
 * Convertit la structure DateStamp indiquée en une structure "tm"
 * (note : le champ "tm_isdst" est toujours à 0).
 * Si "ptr" vaut NULL, demande la date système.
 * Retourne NULL en cas de problème.
 */

{
  struct DateStamp date ;
  struct tm *p ;
  long nj , nja , k ;

  if (! ptr)
  {
    ptr = &date ;
    DateStamp( ptr ) ;
  }
  p = &_ARes_DateEtHeure ;

  p->tm_sec  = ptr->ds_Tick / TICKS_PER_SECOND ;
  p->tm_min  = ptr->ds_Minute % 60 ;
  p->tm_hour = ptr->ds_Minute / 60 ;

  nj = ptr->ds_Days ;
  p->tm_wday = nj % 7 ; /* le 1-Jan-78 était un dimanche */
  nj++ ;

  for ( k = 1978 ; ; k++ )
  {
    nja = Bissextile( k ) ? 366 : 365 ;
    if ( nj <= nja ) break ;
    nj -= nja ;
  }

  p->tm_yday = nj ;
  p->tm_year = k % 100 ;
  _ARes_JoursParMois[1] = (char) Bissextile( k ) ? 29 : 28 ;

  for ( k = 0 ; (k < 12) && (nj > (long)_ARes_JoursParMois[k]) ; k++ )
    nj -= (long)_ARes_JoursParMois[k] ;

  if ( k >= 12 ) return( NULL ) ;

  p->tm_mon  = k + 1 ;
  p->tm_mday = nj ;
  p->tm_isdst = 0 ;
  return( p ) ;
}

