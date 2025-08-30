/*
* This file is part of AUSH.
* Copyright (C) 1994 Denis Gounelle
* 
* AUSH is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AUSH is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with AUSH.  If not, see <http://www.gnu.org/licenses/>.
*
*/
/***************************************************************************
 *
 * AUSH (c)1992 par Denis GOUNELLE
 *
 ***************************************************************************/

#define SRC_ALIAS_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

struct List AliasList ;

/***************************************************************************/

void InitAlias( void )
{
  NewList( &AliasList ) ;
}

/***************************************************************************/

struct Alias *GetAlias( char *name )
{
  struct Alias *p, *q ;

  for ( p = (struct Alias *)AliasList.lh_Head ; q = (struct Alias *)p->a_node.ln_Succ ; p = q )
    if (! stricmp( name , p->a_name )) return( p ) ;

  return( NULL ) ;
}

/***************************************************************************/

int SetAlias( char *name, char *value )
{
  int l ;
  struct Alias *p, *q ;

  if ( strlen( name ) >= MAXNAME )
  {
    ERROR( ERROR_OVERFLOW ) ;
    return( RETURN_FAIL ) ;
  }

  for ( p = (struct Alias *)AliasList.lh_Head ; q = (struct Alias *)p->a_node.ln_Succ ; p = q )
    if (! stricmp( name , p->a_name ))
    {
      Remove( (struct Node *)p ) ;
      free( p ) ;
      break ;
    }

  l = strlen( value ) ;
  p = (struct Alias *)myalloc( (l + sizeof(struct Alias)) , 0 ) ;
  if ( ! p ) return( RETURN_FAIL ) ;

  strcpy( p->a_name , name ) ;
  strcpy( p->a_before , value ) ;

  AddHead( &AliasList , (struct Node *)p ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

void FreeAlias( void )
{
  struct Alias *p, *q ;

  for ( p = (struct Alias *)AliasList.lh_Head ; q = (struct Alias *)p->a_node.ln_Succ ; p = q )
    free( p ) ;
}

