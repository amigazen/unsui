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

static void *Alloc_MARQUE( void )
{
  return( (void *)_ARes_MarqueCour ) ;
}

/*************************************************************************/

static void *Alloc_MEMORY( long arg1 , long arg2 )

{
  Sauveg = arg1 ;
  return( AllocMem( arg1 , arg2 ) ) ;
}

/*************************************************************************/

static void *Alloc_XLIST( long arg1 )

{
  return( (void *)arg1 ) ;
}

/*************************************************************************/

static void *Alloc_REQUEST( void )

{
  struct ReqFic *r ;

  r = (struct ReqFic *)AllocMem( sizeof( struct ReqFic ) , MEMF_PUBLIC|MEMF_CLEAR ) ;
  if (! r) AResErreur = AE_NOMEMORY ;

  return( r ) ;
}

/*************************************************************************/

static void *Free_MARQUE( void )
{
  return( NULL ) ;
}

/*************************************************************************/

static void *Free_MEMORY( long res )

{
  FreeMem( res, Sauveg ) ;
  return( NULL ) ;
}

/*************************************************************************/

static void *Free_XLIST( struct List *res )

{
  struct Node *p , *q ;

  /* empty list ? */
  if ( res->lh_TailPred == (struct Node *) res ) return( NULL ) ;

  for ( p = res->lh_Head ; q = p->ln_Succ ; p = q ) Libere( p ) ;
  return( NULL ) ;
}

/*************************************************************************/

static void *Free_REQUEST( long res )

{
  FreeMem( res , sizeof( struct ReqFic ) ) ;
  return( NULL ) ;
}

