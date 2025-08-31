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
#include <libraries/diskfont.h>
#include <proto/diskfont.h>

extern struct Library *DiskfontBase ;

/*****************************************************************************/

struct TextFont *SafeOpenFont( struct TextAttr *attr )

/*
 * Ouvre la police indiquée.
 * Si OpenFont() échoue, essaye automatiquement OpenDiskFont() en ouvrant
 * "diskfont.library" si besoin.
 * Vérifie de plus que la police ouverte correspond bien à ce qui est demandé.
 */

{
  struct TextFont *p ;

  /* seule "topaz" est en ROM */

  if (! stricmp( attr->ta_Name , "topaz.font" ))
    attr->ta_Flags |=  FPF_ROMFONT ;
  else
    attr->ta_Flags &= ~FPF_ROMFONT ;

  /* Essaye par OpenFont() et vérifie la taille de la police ouverte */

  if ( p = OpenFont( attr ) )
  {
    if ( p->tf_YSize == attr->ta_YSize ) return( p ) ;
    CloseFont( p ) ;
  }

  /* Essaye par OpenDiskFont(), et vérifie la taille de la police ouverte */

  if ( ! DiskfontBase ) DiskfontBase = OpenLibrary( "diskfont.library" , 0 ) ;
  if ( ! DiskfontBase ) return( NULL ) ;

  if ( p = OpenDiskFont( attr ) )
  {
    if ( p->tf_YSize == attr->ta_YSize ) return( p ) ;
    CloseFont( p ) ;
  }

  return( NULL ) ;
}

