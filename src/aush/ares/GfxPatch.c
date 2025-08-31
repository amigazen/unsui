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
/*************************************************************************
 *
 * ARes (C)1990-1994 par Denis GOUNELLE
 *
 *************************************************************************/

#include "ARes.h"

static short InstrLen[] = { 1 , 1 , 2 , 2 , 4 , 4 , 4 , 4 , 2 , 2 } ;

/*************************************************************************/

static unsigned short *GfxPtr( unsigned short *code , long instr , long part )

{
  unsigned short k ;

  code++ ; /* passe le nb d'instructions */

  while ( instr ) /* se positionne sur l'instruction */
  {
    k = *code >> 12 ;	   /* k = op-code */
    code += InstrLen[k] ; /* passe l'instruction */
    instr-- ;
  }

  k = *code & 0xf000 ; /* k = op-code */
  if ( (k == GO_SETAPEN) || (k == GO_SETBPEN) ) return( code ) ;

  switch ( part )
  {
    case 0 : return( code ) ;
    case 1 : return( &code[1] ) ;
    case 2 : return( code ) ;
    case 3 : return( &code[2] ) ;
    case 4 : return( &code[3] ) ;
  }

  return( NULL ) ;
}

/*************************************************************************/

long GfxPatch( unsigned short *code , long instr, long part, long val )

{
  code = GfxPtr( code , instr , part ) ;
  if (! code) return( FALSE ) ;

  switch ( part )
  {
    case 0  : *code = (*code & 0x0fff) | (unsigned short) (val << 12) ;
	      break ;
    case 2  : *code = (*code & 0xf000) | (unsigned short) (val & 0x0fff) ;
	      break ;
    default : *code = (unsigned short) val ;
	      break ;
  }

  return( TRUE ) ;
}

/*************************************************************************/

long GfxGet( unsigned short *code , long instr, long part )

{
  code = GfxPtr( code , instr , part ) ;
  if (! code) return( -1 ) ;

  switch ( part )
  {
    case 0  : return( (long)(*code & (unsigned short)0xf000) ) ;
    case 2  : return( (long)(*code & (unsigned short)0x0fff) ) ;
    default : return( (long)*code ) ;
  }

  return ( -1 ) ;
}

