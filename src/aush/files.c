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

#define SRC_FILES_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

extern char tmp[] ;
struct List FileList ;

/****************************************************************************/

void InitFiles( void )
{
  NewList( &FileList ) ;
}

/****************************************************************************/

struct MyFile *FAssign( FILE *desc )
{
  struct MyFile *p ;

  if (! (p = (struct MyFile *)myalloc( sizeof(struct MyFile) , 1 )))
    return( NULL ) ;

  p->f_desc  = desc ;
  NewList( &(p->f_public) ) ;
  NewList( &(p->f_private) ) ;
  return( p ) ;
}

/****************************************************************************/

struct MyFile *FOpen( char *name , char *mode )
{
  FILE *desc ;
  struct MyFile *p ;

  if (! (desc = fopen( name , mode ))) return( NULL ) ;
  if (! (p = FAssign( desc )))
  {
    fclose( desc ) ;
    return( NULL ) ;
  }

  AddTail( &FileList , (struct Node *)p ) ;
  return( p ) ;
}

/****************************************************************************/

void FClose( struct MyFile *f , int flg )
{
  struct Loop *a, *b ;

  FreeVars( &(f->f_public) ) ;
  FreeVars( &(f->f_private) ) ;

  for ( a = f->f_loop ; a ; a = b )
  {
    b = a->l_next ;
    FreeLoop( a ) ;
  }

  if ( flg )
  {
    if ( f->f_desc ) fclose( f->f_desc ) ;
    Remove( (struct Node *)f ) ;
  }
  free( f ) ;
}

/****************************************************************************/

void FreeFiles( void )
{
  struct MyFile *p ;

  for (;;)
  {
    p = (struct MyFile *)FileList.lh_Head ;
    if (! p->f_node.ln_Succ) break ;
    FClose( p , 1 ) ;
  }
}

/****************************************************************************/

int FGetchar( struct MyFile *f )
{
  return( fgetc( f->f_desc ) ) ;
}

/****************************************************************************/

int FGetline( struct MyFile *file , char *buf , int maxlen )
{
  int l ;

  if (! fgets( buf , maxlen , file->f_desc )) return( 0 ) ;

  l = strlen( buf ) ;
  if ( (l > 0) && (buf[l-1] == '\n') )
  {
    l-- ;
    buf[l] = '\0' ;
  }

  return( l ) ;
}


