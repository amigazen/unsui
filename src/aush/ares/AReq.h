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

#define GID_AFFICH  0L
#define GID_VALIDER 1L
#define GID_PARENT  2L
#define GID_ANNULER 3L
#define GID_REPERT  4L
#define GID_FICHIER 5L
#define GID_ASCENS  6L
#define GID_EXAMEN  7L

#define BLEU  3
#define BLANC 2
#define NOIR  1
#define GRIS  0

#define LGMAXSTR   257
#define MAXNOM	   32L
#define NBAFFICHES 10L

#define TM_RIEN 0L
#define TM_FIN	1L
#define TM_SCAN 2L
#define TM_STOP 3L

/*************************************************************************/

struct Entree
{
  struct Node	e_Node ;
  long		e_Type ;		/* voir #define ci-dessous */
  long		e_Size ;		/* taille si ET_FILE */
  char		e_Name[MAXNOM+1] ;	/* nom de l'entrée */
  char		e_VName[MAXNOM+2] ;	/* nom du volume si ET_VOL */
} ;

#define ET_FILE     0x01
#define ET_DIR	    0x02
#define ET_VOL	    0x04
#define ET_SELECTED 0x08

#define FirstEntry( p )         ((struct Entree *)(p)->lh_Head)
#define NextEntry( p )          ((struct Entree *)(p)->e_Node.ln_Succ)

/*************************************************************************/

static struct IntuiText IText1A =
{
  NOIR , 0 ,
  JAM1	,
  53 , 98 ,
  NULL ,
  (UBYTE *)"File" ,
  NULL
} ;

static struct IntuiText IText2A =
{
  NOIR , 0 ,
  JAM1	,
  13 , 84 ,
  NULL ,
  (UBYTE *)"Directory" ,
  NULL
} ;

static struct IntuiText G1TextA =
{
  NOIR , 0 ,
  JAM1 ,
  26, 3 ,
  NULL ,
  (UBYTE *)"Ok" ,
  NULL,
} ;

static struct IntuiText G1bisTextA =
{
  NOIR , 0 ,
  JAM1 ,
  18 , 3 ,
  NULL ,
  (UBYTE *)"Scan" ,
  NULL,
} ;

static struct IntuiText G2TextA =
{
  NOIR , 0 ,
  JAM1 ,
  10 , 3 ,
  NULL ,
  (UBYTE *)"Parent" ,
  NULL,
} ;

static struct IntuiText G3TextA =
{
  NOIR , 0 ,
  JAM1 ,
  10 , 3 ,
  NULL ,
  (UBYTE *)"Cancel" ,
  NULL,
} ;

/*************************************************************************/

static struct IntuiText IText1D =
{
  NOIR , 0 ,
  JAM1	,
  53 , 98 ,
  NULL ,
  "Datei" ,
  NULL
} ;

static struct IntuiText IText2D =
{
  NOIR , 0 ,
  JAM1	,
  13 , 84 ,
  NULL ,
  "Verzeichnis" ,
  NULL
} ;

static struct IntuiText G1TextD =
{
  NOIR , 0 ,
  JAM1 ,
  26, 3 ,
  NULL ,
  "Ok" ,
  NULL,
} ;

static struct IntuiText G1bisTextD =
{
  NOIR , 0 ,
  JAM1 ,
  18 , 3 ,
  NULL ,
  "Absuchen" ,
  NULL,
} ;

static struct IntuiText G2TextD =
{
  NOIR , 0 ,
  JAM1 ,
  10 , 3 ,
  NULL ,
  "Mutterv." ,
  NULL,
} ;

static struct IntuiText G3TextD =
{
  NOIR , 0 ,
  JAM1 ,
  10 , 3 ,
  NULL ,
  "Abbruch" ,
  NULL,
} ;

/*************************************************************************/

static struct IntuiText IText1 =
{
  NOIR , 0 ,
  JAM1	,
  32 , 98 ,
  NULL ,
  (UBYTE *)"Fichier" ,
  NULL
} ;

static struct IntuiText IText2 =
{
  NOIR , 0 ,
  JAM1	,
  8 , 84 ,
  NULL ,
  (UBYTE *)"Répertoire" ,
  NULL
} ;

static struct IntuiText G1Text =
{
  NOIR , 0 ,
  JAM1 ,
  6, 3 ,
  NULL ,
  (UBYTE *)"Valider" ,
  NULL,
} ;

static struct IntuiText G1bisText =
{
  NOIR , 0 ,
  JAM1 ,
  10 , 3 ,
  NULL ,
  (UBYTE *)"Examen" ,
  NULL,
} ;

static struct IntuiText G2Text =
{
  NOIR , 0 ,
  JAM1 ,
  10 , 3 ,
  NULL ,
  (UBYTE *)"Parent" ,
  NULL,
} ;

static struct IntuiText G3Text =
{
  NOIR , 0 ,
  JAM1 ,
  6 , 3 ,
  NULL ,
  (UBYTE *)"Annuler" ,
  NULL,
} ;

/*************************************************************************/

static struct PropInfo G6Info =
{
  AUTOKNOB|FREEVERT ,
  (USHORT)-1 , 0 ,
  (USHORT)-1 , 0 ,
} ;

static struct Image G6Image =
{
  0 , 0 ,
  0 , 0 ,
  0 ,
  NULL ,
  0 , 0 ,
  NULL
} ;

static struct Gadget Gadget6 =
{
  NULL,
  290 , 12 ,
  18  , 68 ,
  GADGDISABLED ,
  GADGIMMEDIATE|RELVERIFY ,
  PROPGADGET ,
  (APTR)&G6Image ,
  NULL ,
  NULL ,
  NULL ,
  (APTR)&G6Info ,
  GID_ASCENS ,
  NULL
} ;

/*************************************************************************/

static struct Gadget Gadget3 =
{
  &Gadget6,
  241 , 110 ,
  67  , 13  ,
  GADGHCOMP,
  RELVERIFY,
  BOOLGADGET,
  NULL,
  NULL,
  &G3Text,
  NULL,
  NULL,
  GID_ANNULER,
  NULL
} ;

static struct Gadget Gadget2 =
{
  &Gadget3,
  163 , 110 ,
  67  , 13  ,
  GADGHCOMP,
  RELVERIFY,
  BOOLGADGET,
  NULL,
  NULL,
  &G2Text,
  NULL,
  NULL,
  GID_PARENT,
  NULL
} ;

static struct Gadget Gadget1bis =
{
  &Gadget2,
  85 , 110 ,
  67 , 13  ,
  GADGHCOMP,
  RELVERIFY,
  BOOLGADGET,
  NULL,
  NULL,
  &G1bisText,
  NULL,
  NULL,
  GID_EXAMEN,
  NULL
} ;

static struct Gadget Gadget1 =
{
  &Gadget1bis ,
  7  , 110 ,
  67 , 13  ,
  GADGHCOMP,
  RELVERIFY,
  BOOLGADGET,
  NULL,
  NULL,
  &G1Text,
  NULL,
  NULL,
  GID_VALIDER,
  NULL
} ;

/*************************************************************************/

static unsigned char G5Tampon[LGMAXSTR] ;

static struct StringInfo G5Info =
{
  G5Tampon ,
  NULL ,
  0 ,
  LGMAXSTR ,
  0 ,
  0 ,
  0 ,
  0 ,
  0 , 0 ,
  NULL ,
  0 ,
  NULL
} ;

static struct Gadget Gadget5 =
{
  &Gadget1,
  96  , 98 ,
  210 ,  8 ,
  GADGHCOMP,
  RELVERIFY,
  STRGADGET,
  NULL,
  NULL,
  &IText1,
  NULL,
  &G5Info,
  GID_FICHIER,
  NULL
} ;

/*************************************************************************/

static unsigned char G4Tampon[LGMAXSTR] ;

static struct StringInfo G4Info =
{
  G4Tampon ,
  NULL ,
  0 ,
  LGMAXSTR ,
  0 ,
  0 ,
  0 ,
  0 ,
  0 , 0 ,
  NULL ,
  0 ,
  NULL
} ;

static struct Gadget Gadget4 =
{
  &Gadget5 ,
  96  , 84 ,
  210 ,  8 ,
  GADGHCOMP,
  RELVERIFY,
  STRGADGET,
  NULL,
  NULL,
  &IText2,
  NULL,
  &G4Info,
  GID_REPERT,
  NULL
} ;

static struct Gadget Gadget0 =
{
  &Gadget4 ,
  9   , 14 ,
  275 , 66 ,
  GADGHNONE,
  RELVERIFY,
  BOOLGADGET,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  GID_AFFICH,
  NULL
} ;

/*************************************************************************/

static struct NewWindow NFen =
{
  166 , 25  ,
  315 , 126 ,
  0   , 1   ,
  CLOSEWINDOW|GADGETUP|GADGETDOWN|MOUSEBUTTONS|DISKINSERTED|DISKREMOVED ,
  WINDOWCLOSE|WINDOWDRAG|WINDOWDEPTH|RMBTRAP|ACTIVATE,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  0 , 0 ,
  0 , 0 ,
  WBENCHSCREEN
} ;

