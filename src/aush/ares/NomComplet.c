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

static char Tmp[257] , Pere[257] ;
struct FileInfoBlock *_ARes_Fib = NULL ;

/*************************************************************************/

char *NomComplet( BPTR cle )

/*
 * Cherche le nom complet de l'objet désigné par cle.
 * Retourne un pointeur sur ce nom, ou NULL en cas de problème
 * NOTE : cle doit être une clé de lecture
 */

{
  BPTR cle1 , cle2 ;
  short k , l ; /* ATTENTION "l" = indicateur (1 si Fib alloué) */

  if (! AlloueMarque( AM_NOUVELLE )) return( NULL ) ;

  if ( l = (! _ARes_Fib) )
    if (! (_ARes_Fib = Alloue( AT_MEMORY , (long)sizeof( struct FileInfoBlock ) , MEMF_PUBLIC )))
    {
      LibereMarque( NULL ) ;
      return( NULL ) ;
    }

  if (! (cle1 = DupLock( cle )))
  {
    LibereMarque( NULL ) ;
    return( NULL ) ;
  }

  /* nom de base de l'objet dans Tmp */

  if (! Examine( cle1 , _ARes_Fib ))
  {
    LibereMarque( NULL ) ;
    UnLock( cle1 ) ;
    return( NULL ) ;
  }
  strcpy( Tmp , _ARes_Fib->fib_FileName ) ;

  /* boucle pour remonter de père en père */

  *Pere = '\0' ;
  while ( cle2 = ParentDir( cle1 ) )
  {
    UnLock( cle1 ) ;
    if ( *Pere )
    {
      strcat( Pere , "/" ) ;
      strcat( Pere , Tmp ) ;
      strcpy( Tmp , Pere ) ;
    }

    if (! Examine( cle2 , _ARes_Fib ))
    {
      LibereMarque( NULL ) ;
      UnLock( cle2 ) ;
      return( NULL ) ;
    }

    strcpy( Pere , _ARes_Fib->fib_FileName ) ;
    cle1 = cle2 ;
  }

  UnLock( cle1 ) ;

  /* on est arrivé à la racine */

  if (! *Pere)
  {
    strcpy( Pere , Tmp ) ;
    *Tmp = '\0' ;
  }

  strcat( Pere , ":" ) ;
  strcat( Pere , Tmp ) ;

  /* l'objet est un répertoire, rajoute un "/" à la fin si besoin */

  if ( Examine( cle , _ARes_Fib ))
    if ( _ARes_Fib->fib_DirEntryType > 0L )
    {
      k = strlen( Pere ) ;
      if ( Pere[k-1] != ':' ) strcat( Pere , "/" ) ;
    }

  LibereMarque( NULL ) ;
  if ( l ) _ARes_Fib = NULL ;
  return( Pere ) ;
}

