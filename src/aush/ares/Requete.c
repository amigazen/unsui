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
#include <graphics/gfxmacros.h>

#define GRIS  0
#define NOIR  1
#define BLANC 2
#define BLEU  3

#define LGMAXSTR	32
#define LONGINTMASK	(LONGINT|STRINGCENTER)

/**************************************************************************/

static long xsize, ysize ;
static USHORT MonMotif[] = { 0x5555, 0xAAAA } ;

static struct TextAttr Topaz8 =
{
  "topaz.font",
  (UWORD)8,
  FS_NORMAL,
  FPF_DESIGNED|FPF_ROMFONT
} ;

static struct IntuiText RSGText2 =
{
  1,0,JAM1,
  0,2,
  &Topaz8,
  NULL,
  NULL
} ;

static struct IntuiText RSGText1 =
{
  1,0,JAM1,
  0,2,
  &Topaz8,
  NULL,
  NULL
} ;

static char RSG3Undo[LGMAXSTR+1] ;
static char RSG3Tampon[LGMAXSTR+1] ;

static struct StringInfo RSG3Info =
{
  (APTR) &RSG3Tampon ,
  (APTR) &RSG3Undo ,
  0 ,
  LGMAXSTR+1 ,
  0 ,
  0 ,
  0 ,
  0 ,
  0 , 0 ,
  NULL ,
  0 ,
  NULL
} ;

static struct Gadget RSG3 =
{
  NULL,
  12,0,264,9,
  GADGHCOMP,
  RELVERIFY,
  STRGADGET,
  NULL,
  NULL,
  NULL,
  NULL,
  &RSG3Info,
  NULL,
  NULL
} ;

static struct Gadget RSG2 =
{
  NULL,
  0,-13,0,11,
  GADGHCOMP,
  RELVERIFY,
  BOOLGADGET,
  NULL,
  NULL,
  &RSGText2,
  NULL,
  NULL,
  NULL,
  NULL
} ;

static struct Gadget RSG1 =
{
  NULL,
  6,-13,0,11,
  GADGHCOMP,
  RELVERIFY,
  BOOLGADGET,
  NULL,
  NULL,
  &RSGText1,
  NULL,
  NULL,
  NULL,
  NULL
} ;

static struct NewWindow NRS =
{
  0,50,
  0,54,
  (UBYTE)-1,(UBYTE)-1,
  NULL,
  ACTIVATE|WINDOWDRAG|WINDOWDEPTH,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  0,0,
  0,0,
  NULL
} ;

static char tmp[257] ;
static struct TextFont *MaFont = NULL ;

/******************************************************************************/

void RSClose( struct Window *fen )

/* Ferme la requête indiquée */

{
  struct Message *Msg ;

  while ( Msg = GetMsg( fen->UserPort ) ) ReplyMsg( Msg ) ;
  CloseWindow( fen ) ;

  if ( MaFont )
  {
    CloseFont( MaFont ) ;
    MaFont = NULL ;
  }
}

/******************************************************************************/

long RSMsg( struct Window *fen , long flg , char *str )

/*
 * Lit les messages en attente pour la requête indiquée.
 * Retourne -1 si le gadget de fermeture a été sélectionné
 *	     0 si aucun message n'est en attente.
 *	     1 si le 1er gadget a été sélectionné.
 *	     2 si le 2nd gadget a été sélectionné.
 *	     3 si une chaine a été entrée.
 * Si "flg" vaut TRUE, mode bloquant - fait un WaitPort(), sinon
 * mode non bloquant
 */

{
  long k ;
  USHORT code ;
  ULONG classe ;
  struct Gadget *gad ;
  struct ReqSimple *r ;
  struct IntuiMessage *Msg ;

  r = (struct ReqSimple *)fen->UserData ;
  if ( r->rs_Flags & RSF_SAISIE) ActivateGadget( &RSG3 , fen , NULL ) ;

  for (;;)
  {
    if ( flg ) WaitPort( fen->UserPort ) ;
    while ( Msg = (struct IntuiMessage *) GetMsg( fen->UserPort ) )
    {
      classe = Msg->Class ;

      if ( r->rs_IDCMP & classe )
      {
	k = (*r->rs_fct)( r , Msg ) ;
	if ( k ) return( k ) ;
	continue ;
      }

      code = Msg->Code ;
      gad  = (struct Gadget *)Msg->IAddress ;
      ReplyMsg( (struct Message *)Msg ) ;

      if ( classe == CLOSEWINDOW ) return( -1 ) ;

      if ( classe == VANILLAKEY )
      {
	if ( (r->rs_Touche1) && (strchr( r->rs_Touche1 , code )) ) return( 1 ) ;
	if ( (r->rs_Touche2) && (strchr( r->rs_Touche2 , code )) ) return( 2 ) ;
      }

      if ( classe == GADGETUP )
      {
	if ( gad == &RSG1 ) return( 1 ) ;
	if ( gad == &RSG2 ) return( 2 ) ;
	if ( gad == &RSG3 )
	{
	  strcpy( str , RSG3Tampon ) ;
	  return( 3 ) ;
	}
      }
    }
    if ( ! flg ) return( 0 ) ;
  }
}

/******************************************************************************/

static void RSDisplay( struct Window *fen , char *txt , long y )

{
  long l, x, k ;

  l = strlen( txt ) ;
  x = ( fen->Width / xsize ) - 5 ;
  k = ( x - l ) >> 1 ;
  if ( k < 0 ) return ;

  memset( tmp , ' ' , x ) ;
  memcpy( &tmp[k] , txt , l ) ;

  k = fen->BorderLeft + 2 + ( xsize * 2 ) ;
  Move( fen->RPort , k , fen->BorderTop + y ) ;
  Text( fen->RPort , tmp , x ) ;
}

/******************************************************************************/

void RSText( struct Window *fen , char *txt1 , char *txt2 )

/* Affiche les textes indiqués dans une requête */

{
  long k ;
  struct ReqSimple *prs ;

  prs = (struct ReqSimple *) fen->UserData ;

  SetAPen( fen->RPort , (prs->rs_CText != -1) ? prs->rs_CText : 1 ) ;
  SetBPen( fen->RPort , GRIS ) ;
  if ( ! txt1 ) txt1 = prs->rs_Texte1 ;
  if ( txt1 )
  {
    k = ysize + 5 ;
    if ( ! txt2 ) k += ysize / 2 ;
    RSDisplay( fen , txt1 , k ) ;
  }
  if ( txt2 ) RSDisplay( fen , txt2 , (ysize << 1) + 7 ) ;
}

/******************************************************************************/

struct Window *RSOpen( struct ReqSimple *prs , struct TextAttr *attr , struct TextFont *font )

/* Ouverture d'une requête */

{
  long j, k, l, m ;
  struct RastPort *r ;
  struct Window *fen ;
  struct Gadget *pFirst ;

  /* récupère les dimensions de la police (Topaz8 par défaut) */

  xsize = ( font ) ? font->tf_XSize : 8 ;
  ysize = ( font ) ? font->tf_YSize : 8 ;

  /*
   * Récupère la taille de l'écran WB si besoin, et prépare l'ouverture de la
   * fenêtre sur l'écran demandé
   */

  if ( prs->rs_Ecran )
  {
    NRS.Type = CUSTOMSCREEN ;
    NRS.Screen = prs->rs_Ecran ;
  }
  else
  {
    if (! (prs->rs_Ecran = ChercheWB())) return( NULL ) ;
    NRS.Type = WBENCHSCREEN ;
  }

  /* Calcule la largeur de la fenêtre (nb de caractères) */

  k = LGMAXSTR ;				/* taille minimale */

  if ( prs->rs_Texte1 )                         /* ou longueur Texte1 si plus grand */
  {
    l = strlen( prs->rs_Texte1 ) ;
    if ( l > k ) k = l ;
  }

  if ( prs->rs_Texte2 )                       /* ou longueur Texte2 si plus grand */
  {
    l = strlen( prs->rs_Texte2 ) ;
    if ( l > k ) k = l ;
  }

  /* force au moins la la somme de la longueur des gadgets */

  l = m = 0 ;
  if ( prs->rs_Gadget1 ) l = strlen( prs->rs_Gadget1 ) ;
  if ( prs->rs_Gadget2 ) m = strlen( prs->rs_Gadget2 ) ;
  if ( (l + m) > k ) k = l + m ; /* NE PAS MODIFIER l OU m CAR ILS SONT UTILISES PLUS LOIN */
  if ( ! k ) return( NULL ) ;

  /* calcule la hauteur de la fenêtre (pixels) */

  NRS.Height  = 12 + ( 3 * ysize ) ;
  NRS.Height += ( prs->rs_Flags & RSF_SAISIE ) ? 4 : ysize ;
  NRS.Height += prs->rs_Ecran->WBorTop + prs->rs_Ecran->Font->ta_YSize + 1 + prs->rs_Ecran->WBorBottom ;

  /* calcule la largeur de la fenêtre (pixels) */

  j = 0 ;				/* force au moins la longueur du titre */
  k *= xsize ;
  if ( prs->rs_Titre ) j = strlen( prs->rs_Titre ) * prs->rs_Ecran->RastPort.Font->tf_XSize ;
  if ( j > k ) k = j ;
  k += xsize * 4 ;			/* deux caractères de marge de chaque côté */

  if (! (prs->rs_Flags & RSF_SAISIE))   /* vérifie le rapport hauteur/largeur */
  {
    j = ( NRS.Height * 6 ) ;
    if ( ysize >= (xsize + 2) ) j >>= 1 ;
    if ( (k < j) && (j < prs->rs_Ecran->Width) ) k = j ;
  }
  else k = RSG3.LeftEdge + RSG3.Width + 12 ;
  NRS.Width = k + prs->rs_Ecran->WBorLeft + prs->rs_Ecran->WBorRight ;

  /*
   * Place la fenêtre :
   * - horizontalement au milieu de l'écran
   * - verticalement au premier 1/4 de l'écran
   */

  k = ( prs->rs_Ecran->Width - NRS.Width ) / 2 ;
  if ( k < 0 ) return( NULL ) ;
  NRS.LeftEdge = k ;
  NRS.TopEdge  = prs->rs_Ecran->Height / 4 ;

  /* Ouvre la fenêtre */

  prs->rs_IDCMP &= ~(GADGETUP|VANILLAKEY|CLOSEWINDOW) ;
  NRS.IDCMPFlags = GADGETUP|VANILLAKEY|prs->rs_IDCMP ;

  if ( prs->rs_Flags & RSF_CLOSE )
  {
    NRS.Flags |= WINDOWCLOSE ;
    NRS.IDCMPFlags |= CLOSEWINDOW ;
  }
  else NRS.Flags &= ~WINDOWCLOSE ;

  NRS.Title = prs->rs_Titre ;
  if (! (fen = (struct Window *)OpenWindow( &NRS ))) return( NULL ) ;
  fen->UserData = (APTR) prs ;

  /* Initialise et place le(s) gadget(s) */

  k = ( l > m ) ? l : m ;       /* les deux gadgets inférieurs ont la même taille */

  l *= xsize ;			/* calcule tailles en pixels */
  m *= xsize ;
  k = ( k + 2 ) * xsize ;

  RSG1.Width   = RSG2.Width   = k ;
  RSG1.Height  = RSG2.Height  = ysize + 3 ;
  RSG1.TopEdge = RSG2.TopEdge = fen->Height - ysize - 6 ;
  RSGText1.ITextFont = RSGText2.ITextFont = attr ;

  if ( prs->rs_Gadget1 )                        /* place le gadget de droite */
  {
    RSGText1.IText = prs->rs_Gadget1 ;
    RSGText1.LeftEdge = ( k - l ) >> 1 ;
  }

  if ( prs->rs_Gadget2 )                        /* place le gadget de gauche */
  {
    RSG2.LeftEdge = fen->Width - fen->BorderRight - fen->BorderLeft - k + 2 ;
    RSGText2.IText = prs->rs_Gadget2 ;
    RSGText2.LeftEdge = ( k - m ) >> 1 ;
  }

  if ( prs->rs_Gadget1 && prs->rs_Gadget2 )     /* relie les gadgets */
  {
    RSG1.NextGadget = &RSG2 ;
    RSG1.LeftEdge = 6 ;
  }
  else						/* un seul gadget => au centre */
  {
    RSG1.NextGadget = NULL ;
    RSG1.LeftEdge = (fen->Width - RSG1.Width) >> 1 ;
  }

  /* initialise l'affichage */

  ClearFen( fen , -1 ) ;

  r = fen->RPort ;
  SetDrMd( r , JAM2 ) ;
  if ( font ) SetFont( r , font ) ;
  else if ( MaFont = (struct TextFont *)OpenFont( &Topaz8 ) ) SetFont( r , MaFont ) ;

  /* prépare le gadget de chaine */

  if ( prs->rs_Flags & RSF_SAISIE )
  {
    pFirst	    = &RSG3 ;

    RSG3.NextGadget = &RSG1 ;
    RSG3.TopEdge    = fen->BorderTop + 14 ;
    RSG3.LeftEdge   = ( fen->Width - RSG3.Width ) / 2 ;
    RSG3.Height     = ysize ;
    strncpy( RSG3Tampon , prs->rs_Texte1 , LGMAXSTR ) ;
    RSG3Tampon[LGMAXSTR] = '\0' ;
    RSG3Info.MaxChars  = LGMAXSTR+1 ;
    RSG3Info.BufferPos = 0 ;
    RSG3Info.DispPos   = 0 ;

    if ( prs->rs_Flags & RSF_LONGINT ) RSG3.Activation |=  LONGINTMASK ;
				  else RSG3.Activation &= ~LONGINTMASK ;
  }
  else pFirst = &RSG1 ;

  /* ajoute les gadgets */

  AddGList( fen , pFirst , -1 , -1 , NULL ) ;
  SetAPen( r , GRIS ) ;
  GfxFill( r , pFirst , -1 ) ;
  GfxGList( r , pFirst , -1 , BLANC , NOIR ) ;
  RefreshGList( pFirst , fen , NULL , -1 ) ;

  if (! (prs->rs_Flags & RSF_SAISIE))
  {
    /* efface la zone d'affichage */
    m = xsize + 2 ;
    k = fen->BorderLeft + m ;
    l = fen->Width - fen->BorderRight - k - m ;
    m = fen->BorderTop + 4 ;
    j = 2 * ( ysize + 4 ) ;

    SetAPen( fen->RPort , GRIS ) ;
    RectFill( fen->RPort , k , m , k+l , m+j ) ;
    GfxBox2( fen->RPort , k , m , ++l , ++j , NOIR , BLANC ) ;

    /* affiche les textes */
    RSText( fen , prs->rs_Texte1 , prs->rs_Texte2 ) ;
  }

  return( fen ) ;
}

