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

#define ARES_AUTOADAPT_C
#include "ARes.h"

static struct Window *pGlobFen ;
static struct TextFont *pGlobFont ;

/**********************************************************************************/

static SHORT AAGetValue( char *pVal , UWORD fsize , long flg )

/*
 * Calcule une coordonnée en fonction des champs aa_?char et aa_?val
 * "pVal"  pointe sur un champ "aa_?char"
 * "fsize" correspond à la taille en X ou en Y de la police
 * "flg"   vaut TRUE si le champ "aa_?val" est un diviseur
 */

{
  SHORT result ;

  result = *pVal++ * fsize ;			/* prend la partie aa_?char */
  if ( ! flg ) result += *pVal ;                /* prend la partie aa_?val  */
  else if ( *pVal ) result += fsize / *pVal ;   /* pas de division par zéro */
  return( result ) ;
}

/**********************************************************************************/

static void AutoGadget( struct Gadget *pGadget , struct AutoAdapt *pAuto )
{
  struct Gadget *pGRel ;

  pGRel = (struct Gadget *)pAuto->aa_ritem ;

  /* calcul du champ Width */
  pGadget->Width = AAGetValue( &(pAuto->aa_wchar), pGlobFont->tf_XSize, (pAuto->aa_flags & AAF_WDIV) ) ;

  /* calcul du champ Height */
  pGadget->Height = AAGetValue( &(pAuto->aa_hchar), pGlobFont->tf_YSize, (pAuto->aa_flags & AAF_HDIV) ) ;

  /* calcul du camp LeftEdge */
  if ( pGRel )
  {
    pGadget->LeftEdge = pGRel->LeftEdge ;
    if (! (pAuto->aa_flags & AAF_RLEFT)) pGadget->LeftEdge += pGRel->Width ;
  }
  else pGadget->LeftEdge = 0 ;
  pGadget->LeftEdge += AAGetValue( &(pAuto->aa_lchar), pGlobFont->tf_XSize, (pAuto->aa_flags & AAF_LDIV) ) ;

  if ( (pAuto->aa_flags & AAF_HCENTER) && pGlobFen )
    pGadget->LeftEdge = (pGlobFen->Width - pGadget->Width) / 2 ;

  /* calcul du camp TopEdge */
  if ( pGRel )
  {
    pGadget->TopEdge = pGRel->TopEdge ;
    if (! (pAuto->aa_flags & AAF_RTOP)) pGadget->TopEdge += pGRel->Height ;
  }
  else pGadget->TopEdge = 0 ;
  pGadget->TopEdge += AAGetValue( &(pAuto->aa_tchar), pGlobFont->tf_YSize, (pAuto->aa_flags & AAF_TDIV) ) ;

  if ( (pAuto->aa_flags & AAF_VCENTER) && pGlobFen )
    pGadget->TopEdge = (pGlobFen->Height - pGadget->Height) / 2 ;
}

/**********************************************************************************/

static void AutoText( struct IntuiText *pText , struct AutoAdapt *pAuto )
{
  long k ;
  struct Gadget *pGRel ;

  pGRel = (struct Gadget *)pAuto->aa_ritem ;

  /* calcul du camp LeftEdge */
  if ( pGRel )
  {
    pText->LeftEdge = pGRel->LeftEdge ;
    if (! (pAuto->aa_flags & AAF_RLEFT)) pText->LeftEdge += pGRel->Width ;
  }
  else pText->LeftEdge = 0 ;
  pText->LeftEdge += AAGetValue( &(pAuto->aa_lchar), pGlobFont->tf_XSize, (pAuto->aa_flags & AAF_LDIV) ) ;

  if ( (pAuto->aa_flags & AAF_HCENTER) && pGlobFen )
  {
    k = strlen( pText->IText ) * pGlobFont->tf_XSize ;
    pText->LeftEdge = ( pGlobFen->Width - k ) / 2 ;
  }

  /* calcul du camp TopEdge */
  if ( pGRel )
  {
    pText->TopEdge = pGRel->TopEdge ;
    if (! (pAuto->aa_flags & AAF_RTOP)) pText->TopEdge += pGRel->Height ;
  }
  else pText->TopEdge = 0 ;
  pText->TopEdge += AAGetValue( &(pAuto->aa_tchar), pGlobFont->tf_YSize, (pAuto->aa_flags & AAF_TDIV) ) ;

  if ( (pAuto->aa_flags & AAF_VCENTER) && pGlobFen )
    pText->TopEdge = (pGlobFen->Height - pGlobFont->tf_YSize) / 2 ;
}

/**********************************************************************************/

BOOL AutoAdapt( struct Window *pFen, struct AutoAdapt *pAuto , struct TextFont *pFont )
{
  long k ;
  struct Gadget *pGadget ;
  struct IntuiText *pText ;

  /* vérifie les paramètres */

  if ( ! pAuto ) return( FALSE ) ;
  if ( ! pFont ) return( FALSE ) ;

  pGlobFen  = pFen ;
  pGlobFont = pFont ;

  /* boucle de traitement */

  for ( ; pGadget = (struct Gadget *)pAuto->aa_item ; pAuto++ )
  {
    /* traitement d'un IntuiText */

    if ( pAuto->aa_flags & AAF_ITEXT )
    {
      pText = (struct IntuiText *)pGadget ;
      AutoText( pText , pAuto ) ;
      continue ;
    }

    /* traitement d'un gadget */

    AutoGadget( pGadget , pAuto ) ;
    pText = pGadget->GadgetText ;
    if ( ! pText ) continue ;

    /* traitement du texte du gadget */

    k = strlen( pText->IText ) * pFont->tf_XSize ;
    pText->TopEdge  = (pGadget->Height - pFont->tf_YSize) / 2 ;

    if ( pAuto->aa_flags & AAF_TCENTER ) pText->LeftEdge = (pGadget->Width - k) / 2 ;
    if ( pAuto->aa_flags & AAF_TLEFT )
    {
      if ( pGadget->GadgetType == STRGADGET ) k += 8 ;
      pText->LeftEdge = - k - 4 ;
    }
  }

  return( TRUE ) ;
}

