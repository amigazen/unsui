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
#include <graphics/gfxmacros.h>

static UWORD MonMotif[] = { 0x5555, 0xAAAA } ;

/**************************************************************************/

void ClearFen( struct Window *pFen , long color )

/*
 * Efface la fenêtre indiquée, en remplissant avec la couleur "color"
 * Si "color" vaut -1, remplit avec un motif en blanc
 */

{
  if ( color == -1 )
  {
    SetAPen( pFen->RPort , 2 ) ;
    SetAfPt( pFen->RPort , MonMotif , 1 ) ;
  }
  else SetAPen( pFen->RPort , color ) ;

  RectFill( pFen->RPort , pFen->BorderLeft , pFen->BorderTop ,
	    pFen->Width - pFen->BorderRight - 1 ,
	    pFen->Height - pFen->BorderBottom - 1 ) ;
  SetAfPt( pFen->RPort , NULL , 0 ) ;
}



