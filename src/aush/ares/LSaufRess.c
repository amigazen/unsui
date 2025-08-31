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
/**************************************************************************
 *
 * ARes (C)1990-1994 par Denis GOUNELLE
 *
 **************************************************************************/

#include "AResExtern.h"

/*************************************************************************/

void LibereSaufRess( long type )

/*
 * Désalloue toutes les ressources sauf celles de type indiqué
 */

{
  struct UneRess *p , *q ;

  AResErreur = AE_NOERROR ;

  for ( p = _ARes_TeteRess , q = NULL ; p ; )
  {
    if ( (p->ur_Type != type) && (p->ur_Type != AT_MARQUE) )
      p = _ARes_Libere( p , q ) ;
    else
    {
      q = p ;
      p = p->ur_Suiv ;
    }
  }

}
