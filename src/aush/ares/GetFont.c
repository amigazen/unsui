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

#include <graphics/gfxbase.h>

extern struct GfxBase *GfxBase ;

/*************************************************************************/

long GetDefaultTextFont( struct TextAttr *attr , char *name )

/*
 * Récupère la police de texte par défaut, et copie ses caractéristiques
 * dans la structure TextAttr indiquée.
 * Le champ ta_Name de cette structure est initialisé à "name", qui
 * doit correspondre à un tableau d'au moins 32 caractères
 * Retourne 0 en cas d'erreur, -1 si ok
 */

{
  if ( ! GfxBase ) return( 0 ) ;

  Forbid() ;

  attr->ta_Name = (STRPTR)name ;
  strcpy( name , GfxBase->DefaultFont->tf_Message.mn_Node.ln_Name ) ;
  attr->ta_YSize = GfxBase->DefaultFont->tf_YSize ;
  attr->ta_Style = GfxBase->DefaultFont->tf_Style ;
  attr->ta_Flags = GfxBase->DefaultFont->tf_Flags ;

  Permit() ;

  return( -1 ) ;
}

