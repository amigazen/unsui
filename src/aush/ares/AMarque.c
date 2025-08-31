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

long AlloueMarque( long indic )

/*
 * indic = AM_NOUVELLE	: marque les allocations suivantes (nouvelle marque)
 * indic = AM_MARQUEPAS : stoppe le marquage
 * indic = AM_DERNIERE	: marque les allocations suivantes (reprend dernière marque)
 * Retourne la marque courante
 */

{
  long m ;

  AResErreur = AE_NOERROR ;

  switch ( indic )
  {
    case AM_DERNIERE  : _ARes_MarqueCour = _ARes_DerniereMarque() ;
			break ;
    case AM_MARQUEPAS : _ARes_MarqueCour = 0L ;
			break ;
    case AM_NOUVELLE  : m = _ARes_DerniereMarque() ;
			_ARes_MarqueCour = m + 1L ;
			Alloue( AT_MARQUE ) ;
			break ;
    default	      : AResErreur = AE_ARGUMENT ;
			break ;
  }

  return( _ARes_MarqueCour ) ;
}

