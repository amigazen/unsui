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

#include "ARes.h"

/**************************************************************************/

static struct TextAttr Topaz8 =
{
  "topaz.font",
  (UWORD)8,
  FS_NORMAL,
  FPF_DESIGNED|FPF_ROMFONT
} ;

static struct NewScreen NEcran =
{
  0,0,0,0,
  2,0,1,
  HIRES,
  CUSTOMSCREEN,
  NULL,
  NULL,
  NULL,
  NULL
} ;

static WORD PEcran[] = { -1 } ;

static struct TagItem TEcran[] =
{
  SA_Pens,	  (ULONG)&PEcran,
  SA_Width,	  STDSCREENWIDTH,
  SA_Height,	  STDSCREENHEIGHT,
  SA_Depth,	  0,
  SA_Font,	  NULL,
  SA_Title,	  NULL,
  SA_Type,	  CUSTOMSCREEN,
  SA_DisplayID,   NULL,
  SA_PubName,	  NULL,
  SA_PubSig,	  (ULONG)-1,
  SA_AutoScroll,  TRUE,
  TAG_END,NULL
} ;

/* position des tags dans TEcran[] */

#define TE_DEPTH	3
#define TE_FONT 	4
#define TE_TITLE	5
#define TE_DISPLAYID	7
#define TE_PUBNAME	8
#define TE_PUBSIG	9

long _OE_Depth = 2 ;

/**************************************************************************/

struct Screen *OuvreEcran( char *pNomEcran , ULONG DisplayID , struct TextAttr *pAttr , char *pNomPub , long PubSig , long Flg )

/*
 * Ouvre un �cran ayant la m�me taille que l'�cran Workbench
 * pNomEcran = titre de l'�cran
 * DisplayID = mode �cran � utiliser (ignor� si vaut INVALID_ID)
 * pAttr     = police pour l'�cran (peut valoir NULL pour avoir Topaz8, ou -1
 *	       pour prendre la police texte par d�faut)
 * pNomPub   = nom de l'�cran public (peut �tre NULL, dans ce cas �cran priv�)
 * PubSig    = signal pour l'�cran public (ignor� si pNomPub vaut NULL)
 * Flg	     = indicateurs (voir #define OEF_machin)
 * Retourne NULL en cas d'�chec
 */

{
  UWORD Modes ;
  struct Screen *pEcran ;
  struct TextFont *pFonte ;

  /* positionne les flags NEWROM et PALMODE */

  if ( GfxBase->DisplayFlags & PAL ) Flg |= OEF_PALMODE ;
  if ( IntuitionBase->LibNode.lib_Version >= 36 ) Flg |= OEF_NEWROM ;

  /* cherche l'�cran WorkBench */

  pEcran = ChercheWB() ;
  if ( ! pEcran ) return( NULL ) ;

  if ( Flg & OEF_CLONEWB ) Flg |= OEF_CLONELACE ;
  Modes = pEcran->ViewPort.Modes & (HIRES|LACE) ;
  if ( (Modes == (HIRES|LACE)) && (Flg & OEF_CLONELACE) ) Flg |= OEF_FORCELACE ;

  /* fixe la police � utiliser */

  pFonte = NULL ;

  if ( pAttr == (struct TextAttr *)-1 ) pAttr = NULL ;
  else if ( pAttr )
  {
    /* ouvre la police, pour forcer un chargement depuis FONTS: si besoin */
    pFonte = SafeOpenFont( pAttr ) ;
    /* ne la prend que si n'est pas proportionnelle */
    if ( pFonte->tf_Flags & FPF_PROPORTIONAL ) pAttr = &Topaz8 ;
  }
  else pAttr = &Topaz8 ;

  NEcran.Font = pAttr ;
  TEcran[TE_FONT].ti_Data = (ULONG)pAttr ;

  /* initialise le nombre de plans */

  TEcran[TE_DEPTH].ti_Data = _OE_Depth ;
  NEcran.Depth = _OE_Depth ;

  if ( Flg & OEF_NEWROM )               /**** OUVERTURE POUR KICKSTART 2.04 ET PLUS ****/
  {
    TEcran[TE_TITLE].ti_Data = (ULONG)pNomEcran ;

    /* positionne les infos pour �cran public */
    if ( pNomPub )
    {
      TEcran[TE_PUBNAME].ti_Tag  = SA_PubName ;
      TEcran[TE_PUBNAME].ti_Data = (ULONG)pNomPub ;
      TEcran[TE_PUBSIG].ti_Data  = PubSig ;
    }
    else TEcran[TE_PUBNAME].ti_Tag = TAG_IGNORE ;

    /* positionne le mode de l'�cran */
    if ( Flg & OEF_CLONEWB )
      if ( pEcran = LockPubScreen( "Workbench" ) )
      {
	DisplayID = GetVPModeID( &(pEcran->ViewPort) ) ;
	UnlockPubScreen( NULL , pEcran ) ;
      }
      else DisplayID = INVALID_ID ;

    if ( DisplayID == INVALID_ID )
    {
      TEcran[TE_DISPLAYID].ti_Data  = ( Flg & OEF_FORCELACE ) ? HIRESLACE_KEY : HIRES_KEY ;
      TEcran[TE_DISPLAYID].ti_Data |= ( Flg & OEF_PALMODE ) ? PAL_MONITOR_ID : NTSC_MONITOR_ID ;
    }
    else TEcran[TE_DISPLAYID].ti_Data = DisplayID ;

    /* ouvre l'�cran */
    if ( pEcran = OpenScreenTagList( NULL , TEcran ) )
      if ( Flg & OEF_MAKEPUBLIC ) PubScreenStatus( pEcran , ~PSNF_PRIVATE ) ;
  }
  else					/**** OUVERTURE POUR KICKSTART 1.3 ****/
  {
    /* positionne le titre et la taille */
    NEcran.Width  = pEcran->Width ;
    NEcran.Height = pEcran->Height ;
    NEcran.DefaultTitle = pNomEcran ;
    if ( Modes & LACE ) NEcran.Height /= 2 ;

    /* positionne le mode entrelac� */
    if ( Flg & OEF_FORCELACE )
    {
      NEcran.Height *= 2 ;
      NEcran.ViewModes |= LACE ;
    }

    /* ouvre l'�cran */
    pEcran = OpenScreen( &NEcran ) ;
  }

  /* fin */

  if ( pFonte ) CloseFont( pFonte ) ;
  return( pEcran ) ;
}
