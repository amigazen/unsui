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
/***************************************************************************
 *
 * ARes (C)1990-1994 par Denis GOUNELLE
 *
 ***************************************************************************/

#include "ARes.h"

/***************************************************************************/

static long Size = 0 ;
static struct BitMapHeader MyBMHD ;

static char ReadBuf[256],
	    *IFFId[IFF_NUMID] = { "FORM", "ILBM", "BMHD", "CMAP", "BODY" } ;

/***************************************************************************/

char *IFFDecodeRLen( char *pSrc , char *pDst , long Len )

/*
 * Décodage RLEN
 * pSrc = pointeur données source
 * pDst = pointeur données destination
 * Len	= nombre d'octet à fournir
 * Retourne la nouvelle position source
 */

{
  long Cpt ;
  char Byte ;

  while ( Len > 0 )
  {
    Byte = *pSrc++ ;
    if ( Byte == -128 ) continue ;

    if ( Byte < 0 )
    {
      Cpt = -Byte + 1 ;
      Byte = *pSrc++ ;
      do
      {
	*pDst++ = Byte ;
	Len-- ;
	Cpt-- ;
      }
      while ( Cpt > 0 ) ;
    }
    else for ( Cpt = Byte + 1 ; Cpt > 0 ; Cpt-- , Len-- ) *pDst++ = *pSrc++ ;
  }

  return( pSrc ) ;
}

/***************************************************************************/

long IFFChercheChunk( BPTR inDesc , long Id )

/*
 * Cherche le début d'un chunk dans un fichier IFF
 * inDesc = descripteur de fichier
 * Id = numéro de l'identificateur du chunk
 * Retourne la taille du chunk, ou -1 en cas d'erreur
 */

{
  long Len ;
  static char *pIdStr ;

  ReadBuf[4] = '\0' ;
  pIdStr = IFFId[Id] ;

  FOREVER
  {
    if ( Read( inDesc , ReadBuf , 4 ) != 4 ) break ;
    if ( Read( inDesc , &Len , 4 ) != 4 ) break ;
    if ( Len & 1 ) Len++ ;
    if (! strcmp( ReadBuf , pIdStr )) return( Len ) ;
    if ( Seek( inDesc , Len , OFFSET_CURRENT ) == -1 ) break ;
  }

  return( -1 ) ;
}

/********************************************************************************/

BOOL IFFVerifFichier( BPTR inDesc )

/* Verifie que le fichier indiqué contient une image IFF */

{
  Seek( inDesc , 0 , OFFSET_BEGINNING ) ;
  if (! IFFChercheChunk( inDesc , IFF_FORM )) return( FALSE ) ;

  if ( Read( inDesc , ReadBuf , 4 ) != 4 ) return( FALSE ) ;
  if ( strcmp( ReadBuf , IFFId[IFF_ILBM] ) ) return( FALSE ) ;
  return( TRUE ) ;
}

/********************************************************************************/

struct BitMapHeader *IFFChargeBMHD( BPTR inDesc )

/*
 * Charge le chunck BMHD du fichier indiqué
 * Les données sont copiées dans une zone statique
 */

{
  long Len ;

  Len = IFFChercheChunk( inDesc , IFF_BMHD ) ;
  if ( Len == -1 ) return( NULL ) ;
  if ( Len > sizeof(struct BitMapHeader) ) Len = sizeof(struct BitMapHeader) ;
  if ( Read( inDesc , &MyBMHD , Len ) != Len ) return( NULL ) ;

  return( &MyBMHD ) ;
}

/********************************************************************************/

void *IFFChargeChunk( BPTR inDesc , long Id , long *pLen )

/*
 * Charge en mémoire le chunk indiqué
 * Retourne un pointeur sur la zone mémoire allouée, et copie sa taille à
 * l'endroit pointé par pLen
 */

{
  long Len ;
  void *pMem ;

  Len = IFFChercheChunk( inDesc , Id ) ;
  if ( Len == -1 ) return( NULL ) ;

  pMem = AllocMem( Len , MEMF_PUBLIC ) ;
  if ( ! pMem ) return( NULL ) ;
  Read( inDesc , pMem , Len ) ;

  *pLen = Len ;
  return( pMem ) ;
}

/********************************************************************************/

BOOL IFFChargePalette( BPTR inDesc , struct Screen *pScreen , LONG Depth )

/*
 * Charge la palette de l'image et modifie la palette de l'écran
 * Depth = nombre de plans de l'image
 */

{
  LONG Len, Plane ;
  struct CMAP *pCMAP ;

  pCMAP = IFFChargeChunk( inDesc , IFF_CMAP , &Len ) ;
  if ( ! pCMAP ) return( FALSE ) ;

  Plane = pScreen->RastPort.BitMap->Depth ;
  if ( Depth < Plane ) Plane = Depth ;
  Plane = 1 << Plane ;
  for ( Depth = 0 ; Depth < Plane ; Depth++ )
    SetRGB4( &(pScreen->ViewPort) , Depth , pCMAP[Depth].Red >> 4 , pCMAP[Depth].Green >> 4 , pCMAP[Depth].Blue >> 4 ) ;

  FreeMem( pCMAP , Len ) ;
  return( TRUE ) ;
}

/********************************************************************************/

char *IFFExtraitLigne( char *pSrc , char *pDst , long MaxLen )

/*
 * Charge la ligne suivante du BODY à l'endroit indiqué par pDst
 * En sortie, ne copie pas plus de MaxLen octets
 * Retourne la nouvelle position en entrée
 */

{
  long SrcLen ;

  SrcLen = ( MyBMHD.bm_Width + 7 ) / 8 ;

  if ( MyBMHD.bm_Compress == BMC_NONE )
  {
    memcpy( pDst , pSrc , (SrcLen < MaxLen) ? SrcLen : MaxLen ) ;
    pSrc += SrcLen ;
    if ( (long)pSrc & 1 ) pSrc++ ;
  }
  else
  {
    pSrc = IFFDecodeRLen( pSrc , ReadBuf , SrcLen ) ;
    memcpy( pDst , ReadBuf , MaxLen ) ;
  }

  return( pSrc ) ;
}

