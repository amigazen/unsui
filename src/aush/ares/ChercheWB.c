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

#include <stdio.h>
#include <intuition/intuitionbase.h>
#include <intuition/screens.h>
#include <proto/intuition.h>

static struct Screen DummyScreen ;
extern struct IntuitionBase *IntuitionBase ;

/**************************************************************************/

struct Screen *ChercheWB( void )

/*
 * Retourne un pointeur sur une copie de la structure de l'écran WorkBench,
 * ou NULL en cas d'échec.
 */

{
  struct Screen *pEcran ;

  if ( IntuitionBase->LibNode.lib_Version >= 36 )
  {
    if ( pEcran = LockPubScreen( "Workbench" ) )
    {
      memcpy( &DummyScreen , pEcran , sizeof(struct Screen) ) ;
      UnlockPubScreen( NULL , pEcran ) ;
    }
    else return( NULL ) ;
  }
  else if (! GetScreenData( &DummyScreen , sizeof(struct Screen) , WBENCHSCREEN , NULL ))
    return( NULL ) ;

  return( &DummyScreen ) ;
}

