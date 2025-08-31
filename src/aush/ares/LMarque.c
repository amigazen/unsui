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

long LibereMarque( long m )

/*
 * Lib�re toutes les ressources marqu�es 'm' et reprend la marque pr�c�dente
 * Si m = NULL, utilise la derni�re marque pos�e
 */

{
  long	 t ;
  struct UneRess *p , *q ;

  AResErreur = AE_NOERROR ;

  if (! m) m = ( _ARes_MarqueCour ) ? _ARes_MarqueCour : _ARes_DerniereMarque() ;
  if (! m) return( 0L ) ;

  for ( p = _ARes_TeteRess , q = NULL ; p ; )
  {
    if ( p->ur_Marq == m )
    {
      t = p->ur_Type ;
      p = _ARes_Libere( p , q ) ;
      if ( t == AT_MARQUE ) break ;
    }
    else
    {
      q = p ;
      p = p->ur_Suiv ;
    }
  }

  _ARes_MarqueCour = _ARes_DerniereMarque() ;
  return( _ARes_MarqueCour ) ;
}
