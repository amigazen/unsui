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

#define ARES_ARES_C

#include "ARes.h"

long		 AResErreur	 = AE_NOERROR ;
		_ARes_MarqueCour = 0L ;
struct UneRess *_ARes_TeteRess	 = NULL ;

/*************************************************************************/

static long Sauveg, Identif ;
extern void *OpenLibrary( char * , long ), *AllocMem( long , long ), CloseLibrary( void * ) ;

#include "AResFonctions.h"

static void *(*TableAlloc[])(...) =
{
  Alloc_MARQUE,
  OpenLibrary,
  Alloc_MEMORY,
  Alloc_XLIST,
  Alloc_REQUEST
} ;

static void *(*TableFree[])(...) =
{
  Free_MARQUE,
  (void *(*)())CloseLibrary,
  Free_MEMORY,
  Free_XLIST,
  Free_REQUEST
} ;

/*************************************************************************/

void *Alloue( long type , long arg1 , long arg2 , long arg3 , long arg4 )

/* Alloue une ressource */

{
  void *res ;
  struct UneRess *p ;

  if (! (p = AllocMem( (long)sizeof( struct UneRess ) , MEMF_CLEAR|MEMF_PUBLIC )))
  {
    AResErreur = AE_NOMEMORY ;
    return( NULL ) ;
  }

  p->ur_Marq = _ARes_MarqueCour ;
  p->ur_Type = type ;
  AResErreur = AE_FAILED ;
  Identif    = (long)p ;

  if ((type < AT_MARQUE) || (type > AT_LASTRESS ))
  {
    res = 0L ;
    AResErreur = AE_TYPE ;
  }
  else res = (*TableAlloc[type])( arg1 , arg2 , arg3 , arg4 ) ;

  /* teste si l'allocation a �chou� */

  if (! res)
  {
    FreeMem( p , (long)sizeof( struct UneRess ) ) ;
    return( NULL ) ;
  }

  AResErreur = AE_NOERROR ;
  p->ur_Args = Sauveg ;
  p->ur_Ress = res ;

  /* ok, ins�re �l�ment en t�te de liste */

  p->ur_Suiv = _ARes_TeteRess ;
  _ARes_TeteRess = p ;

  return( (void *)p->ur_Ress ) ;
}

/*************************************************************************/

static void Desalloue( struct UneRess *p )

/*
 * Desalloue une ressource (fonction interne)
 */

{
  long type = p->ur_Type ;

  Sauveg = p->ur_Args ;

  if ( (type >= AT_MARQUE) &&
       (type <= AT_LASTRESS) ) (*TableFree[type])( p->ur_Ress ) ;

  FreeMem( p , (long)sizeof( struct UneRess ) ) ;
}

/*************************************************************************/

struct UneRess *_ARes_Libere( struct UneRess *p , struct UneRess *q )

/*
 * D�salloue une ressource et lib�re l'�l�ment correspondant
 * p pointe sur l'�l�ment, q sur l'�l�ment pr�c�dent
 * Retourne un pointeur sur l'�l�ment suivant
 */

{
  struct UneRess *a ;

  a = p->ur_Suiv ;

  if ( q )
    q->ur_Suiv = a ;
  else
    _ARes_TeteRess  = a ;

  Desalloue( p ) ;
  return( a ) ;
}

/*************************************************************************/

long Libere( void *res )

/*
 * D�salloue une ressource
 * Retourne NULL si ressource non trouv�e
 */

{
  struct UneRess *p , *q ;

  AResErreur = AE_NOERROR ;

  /* cherche l'�l�ment correspondant */

  for ( p = _ARes_TeteRess , q = NULL ; p ; q = p , p = p->ur_Suiv )
    if ( p->ur_Ress == res ) break ;

  if (! p) return( NULL ) ;

  /* d�salloue la ressource et lib�re l'�l�ment */

  _ARes_Libere( p , q ) ;
  return( (long)p ) ;
}

/*************************************************************************/

void LibereTout( void )

/* D�salloue toutes les ressources */

{
  struct UneRess *p , *q ;

  AResErreur = AE_NOERROR ;

  for ( p = _ARes_TeteRess ; p ; p = q )
  {
    q = p->ur_Suiv ;
    Desalloue( p ) ;
  }

  _ARes_MarqueCour = 0L ;
  _ARes_TeteRess = NULL ;
}

/*************************************************************************/

long _ARes_DerniereMarque( void )

/*
 * Cherche la derni�re marque pos�e
 */

{
  struct UneRess *p ;

  for ( p = _ARes_TeteRess ; p ; p = p->ur_Suiv )
    if ( p->ur_Type == AT_MARQUE ) return( p->ur_Marq ) ;

  return( 0L ) ;
}
