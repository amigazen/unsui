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

/***************************************************************************/

static void UpdateItems( struct MenuItem *pfirst , struct TextFont *pfont , long left )
{
  long y, k, l ;
  struct MenuItem *pitem ;
  struct IntuiText *ptext ;

  y = k = 0 ;

  for ( pitem = pfirst ; pitem ; pitem = pitem->NextItem )
  {
    pitem->TopEdge += y ;
    pitem->LeftEdge = left ;
    pitem->Height   = pfont->tf_YSize ;

    ptext = (struct IntuiText *) pitem->ItemFill ;
    l = 4 + pfont->tf_XSize * strlen( ptext->IText ) ;
    if ( pitem->Flags & COMMSEQ ) l += pfont->tf_XSize + COMMWIDTH ;
    if ( pitem->Flags & CHECKIT ) l += CHECKWIDTH ;

    if ( l > k ) k = l ;

    y += pfont->tf_YSize + 1 ;
  }

  for ( pitem = pfirst ; pitem ; pitem = pitem->NextItem )
  {
    ptext = (struct IntuiText *) pitem->ItemFill ;
    pitem->Width = k + ptext->LeftEdge ;
    if ( pitem->Flags & CHECKIT ) ptext->LeftEdge += CHECKWIDTH ;
    if ( pitem->SubItem ) UpdateItems( pitem->SubItem , pfont , pitem->Width ) ;
  }

}

/***************************************************************************/

void UpdateMenus( struct Menu *pmenu , struct TextFont *pfont )
{
  long x ;

  for ( x = 0 ; pmenu ; x += pmenu->Width , pmenu = pmenu->NextMenu )
  {
    pmenu->LeftEdge = x ;
    pmenu->Width = pfont->tf_XSize * ( strlen( pmenu->MenuName ) + 2 ) ;
    UpdateItems( pmenu->FirstItem , pfont , 0 ) ;
  }
}

