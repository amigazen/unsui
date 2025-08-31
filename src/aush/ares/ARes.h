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

#define ARES_ARES_H

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <dos.h>

#include <exec/types.h>
#include <exec/exec.h>
#include <exec/devices.h>
#include <exec/execbase.h>
#include <exec/memory.h>
#include <exec/ports.h>
#include <graphics/gfx.h>
#include <graphics/gfxbase.h>
#include <graphics/text.h>
#include <graphics/view.h>
#include <intuition/intuition.h>
#include <intuition/intuitionbase.h>
#include <intuition/screens.h>
#include <libraries/dos.h>
#include <libraries/dosextens.h>

#ifndef ARES_ARES_C
#include <proto/exec.h>
#endif

#ifndef ARES_IO_C
#include <proto/dos.h>
#endif

#include <proto/intuition.h>
#include <proto/graphics.h>

/**************************************************************************/

#define AT_MARQUE   0L		/* usage interne */
#define AT_LIBRARY  1L
#define AT_MEMORY   2L
#define AT_XLIST    3L
#define AT_REQUEST  4L

#define AT_LASTRESS AT_REQUEST	/* dernier code ressource */

#define AE_NOERROR  -1L 	/* et non 0, � cause de OpenDevice() */
#define AE_NOMEMORY  0L 	/* plus de m�moire libre */
#define AE_FAILED    1L 	/* l'allocation a �chou� */
#define AE_ARGUMENT  2L 	/* un des args est incorrect */
#define AE_TYPE      3L 	/* type de ressource incorrect */

#define AM_NOUVELLE   1L
#define AM_MARQUEPAS  0L
#define AM_DERNIERE  -1L

struct UneRess
{
  struct UneRess *ur_Suiv ;
  long		  ur_Marq ;
  long		  ur_Type ;
  void		 *ur_Ress ;
  long		  ur_Args ;
} ;

/**************************************************************************/

struct ReqFic
{
  char		 *rf_Titre ;
  BPTR		  rf_Cle   ;
  struct Screen  *rf_Ecran ;
  long		(*rf_Filtre)( struct FileInfoBlock * ) ;
  long		  rf_Indic ;
} ;

#define RFI_REPERT	0x01	/* gadget "Fichier" d�sactiv� */
#define RFI_COULEUR	0x02	/* fond en couleur 0 et non en couleur 3 */
#define RFI_VOLUMES	0x04	/* pas de liste des volumes */
#define RFI_ANGLAIS	0x08	/* textes de la requ�te en anglais */
#define RFI_ALLEMAND	0x10	/* textes de la requ�te en allemand */

/**************************************************************************/

struct ReqSimple
{
  long		 rs_Flags   ;
  struct Screen *rs_Ecran   ;	/* �cran o� afficher la requ�te */
  long		 rs_IDCMP   ;	/* messages � attendre		*/
  long	       (*rs_fct)( struct ReqSimple * , struct IntuiMessage * )  ;
  char		*rs_Titre   ;	/* titre de la requ�te		*/
  char		*rs_Texte1  ;	/* premi�re ligne de texte	*/
  char		*rs_Texte2  ;	/* seconde ligne de texte  (optionnel) */
  char		*rs_Gadget1 ;	/* texte du premier gadget	*/
  char		*rs_Gadget2 ;	/* texte du second gagdget (optionnel) */
  char		*rs_Touche1 ;	/* touches �quivalentes au 1er gadget  */
  char		*rs_Touche2 ;	/* touches �quivalentes au 2nd gadget  */
  char		 rs_CFond   ;	/* couleur de fond de fen�tre (ou -1)  */
  char		 rs_CText   ;	/* couleur du texte/cadre gadget (ou -1) */
  char		 rs_CGadg   ;	/* couleur du texte dans les gadgets (ou -1) */
  char		 rs_CCadr   ;	/* 2eme couleur pour le cadre (ou -1)  */
} ;

#define RSF_SAISIE  0x01
#define RSF_CLOSE   0x02
#define RSF_LONGINT 0x04

/**************************************************************************/

#define GO_SETAPEN  0x0000
#define GO_SETBPEN  0x1000
#define GO_MOVE     0x2000
#define GO_DRAW     0x3000
#define GO_RECTFILL 0x4000
#define GO_BOX	    0x5000
#define GO_TRUEBOX  0x6000
#define GO_TRUEBOX2 0x7000
#define GO_SETCOL   0x8000
#define GO_IMAGE    0x9000

#define GO_MAXCODE  GO_IMAGE

#define GM_SETAPEN( c )                    (GO_SETAPEN | c)
#define GM_SETBPEN( c )                    (GO_SETBPEN | c)
#define GM_MOVE( x , y )                   (GO_MOVE | y) , x
#define GM_DRAW( x , y )                   (GO_DRAW | y) , x
#define GM_RECTFILL( x , y , l , h )       (GO_RECTFILL | y) , x , l , h
#define GM_BOX( x , y , l , h )            (GO_BOX | y) , x , l , h
#define GM_TRUEBOX( x , y , l , h )        (GO_TRUEBOX | y) , x , l , h
#define GM_TRUEBOX2( x , y , l , h )       (GO_TRUEBOX2 | y) , x , l , h
#define GM_SETCOL( c1 , c2 )               (GO_SETCOL | c2) , c1
#define GM_IMAGE( x , y )                  (GO_IMAGE | y) , x

/**************************************************************************/

struct AutoAdapt
{
  void	*aa_item ;	/* struct Gadget pour l'instant */
  char	 aa_lchar ;	/* LeftEdge  (en caract�res)   */
  char	 aa_lval ;	/* LeftEdge  (pixels/diviseur) */
  char	 aa_tchar ;	/* TopEdge   (en caract�res)   */
  char	 aa_tval ;	/* TopEdge   (pixels/diviseur) */
  char	 aa_wchar ;	/* Width      (en caract�res)   */
  char	 aa_wval ;	/* Width      (pixels/diviseur) */
  char	 aa_hchar ;	/* Height     (en caract�res)   */
  char	 aa_hval ;	/* Height     (pixels/diviseur) */
  long	 aa_flags ;	/* voir #define ci-dessous	*/
  void	*aa_ritem ;	/* item par rapport auquel est relatif */
} ;

/* valeurs pour le champs aa_flags */

#define AAF_LDIV	0x00000001	/* aa_lval est un diviseur */
#define AAF_TDIV	0x00000002	/* aa_tval est un diviseur */
#define AAF_WDIV	0x00000004	/* aa_wval est un diviseur */
#define AAF_HDIV	0x00000008	/* aa_hval est un diviseur */
#define AAF_RLEFT	0x00000010	/* LeftEdge relatif bord gauche aa_ritem */
#define AAF_RTOP	0x00000020	/* TopEdge relatif bord sup�rieur aa_ritem */
#define AAF_HCENTER	0x00000040	/* centre le gadget horizontalement */
#define AAF_VCENTER	0x00000080	/* centre le gadget verticalement */
#define AAF_TCENTER	0x00000100	/* centre le texte du gadget */
#define AAF_TLEFT	0x00000200	/* met le texte du gadget � gauche */
#define AAF_WTXT	0x00000400	/* aa_wchar = longueur Gadget->GadgetText */
#define AAF_ITEXT	0x00000800	/* aa_item  est un (struct IntuiText *) */

/**************************************************************************/

#define OEF_NEWROM	0x01	/* Kickstart 2.04 ou sup�rieur	*/
#define OEF_PALMODE	0x02	/* moniteur PAL au lieu NTSC	*/
#define OEF_FORCELACE	0x04	/* force un �cran entrelac�	*/
#define OEF_CLONELACE	0x08	/* �cran entrelac� si WB l'est  */
#define OEF_MAKEPUBLIC	0x10	/* rend public apr�s ouverture	*/
#define OEF_CLONEWB	0x20	/* m�me mode �cran que le WB	*/

/**************************************************************************/

#define IFF_FORM	0
#define IFF_ILBM	1
#define IFF_BMHD	2
#define IFF_CMAP	3
#define IFF_BODY	4
#define IFF_NUMID	5

struct BitMapHeader
{
  UWORD bm_Width, bm_Height ;
  WORD	bm_LeftEdge, bm_TopEdge ;
  UBYTE bm_Depth ;
  UBYTE bm_Masking ;
  UBYTE bm_Compress ;
  UBYTE bm_pad1 ;
  UWORD bm_TransparentColor ;
  UBYTE bm_xAspect, bm_yAspect ;
  WORD	bm_PageWidth, bm_PageHeight ;
} ;

#define BMM_NONE	0
#define BMM_MASK	1
#define BMM_TCLR	2
#define BMM_LASS	3

#define BMC_NONE	0
#define BMC_BRUN	1

struct CMAP
{
  UBYTE Red, Green, Blue ;
} ;

/**************************************************************************/
