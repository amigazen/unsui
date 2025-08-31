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

static struct TextAttr Topaz8 =
{
  "topaz.font",
  (UWORD)8,
  FS_NORMAL,
  FPF_DESIGNED|FPF_ROMFONT
} ;

#include "AReq.h"

#define RAW_SHIFTED	(IEQUALIFIER_LSHIFT|IEQUALIFIER_RSHIFT)

extern struct FileInfoBlock *_ARes_Fib ;

/*************************************************************************/

static short  AttendGUP ,
	      LastWasShifted ,
	      indic ;

static long   NbEntrees =  0 ,
	      NbVolumes =  0 ,
	      NomSelec	= -1 ,
	      TopOffset =  0 ,
	      OldSelec	     ,
	      PremGadget     ,
	      *NbListe	     ,
	      (*Filtre)(struct FileInfoBlock *) ;

static BPTR	 LaCle ;
struct TextFont *_ARes_Fonte ;
static char	 Tmp[LGMAXSTR] ;
static struct Window *OldWindowPtr ;

static struct Process  *Moi ;
static struct Window   *Fen ;
static struct ReqFic   *ReqCour ;
static struct InfoData *info_ptr ;
static struct List     LEntrees , LVolumes , *ListeCour ;

static struct AutoAdapt AutoReq[] =
{
  { &Gadget0   ,  0, 8, 1, 6, 32, 6, 10, 4, NULL, NULL } ,
  { &Gadget4   , 12, 3, 0, 8, 26, 0,  1, 0, AAF_RLEFT|AAF_TLEFT , &Gadget0 } ,
  { &Gadget5   ,  0, 0, 0, 8, 26, 0,  1, 0, AAF_RLEFT|AAF_TLEFT , &Gadget4 } ,
  { &Gadget1   ,  0, 8, 0, 0,  9, 4,  1, 2, AAF_WDIV|AAF_TCENTER, NULL } ,
  { &Gadget1bis,  0, 7, 0, 0,  9, 4,  1, 2, AAF_WDIV|AAF_TCENTER|AAF_RTOP, &Gadget1 } ,
  { &Gadget2   ,  0, 7, 0, 0,  9, 4,  1, 2, AAF_WDIV|AAF_TCENTER|AAF_RTOP, &Gadget1bis } ,
  { &Gadget3   ,  0, 7, 0, 0,  9, 4,  1, 2, AAF_WDIV|AAF_TCENTER|AAF_RTOP, &Gadget2 } ,
  { &Gadget6   ,  0, 8, 0, 0,  0,20, 10, 4, AAF_RTOP, &Gadget0 },
  { NULL, 0, 0, 0, 0, 0, 0, 0, 0, NULL , NULL }
} ;

#include "AReq_sub.c"

/*************************************************************************/

static void ChargeVolumes( void )

/*
 * Les volumes physiques sont ajoutés en début de liste,
 * les volumes logiques en fin de liste
 */

{
  BPTR		    cle ;
  APTR		    oldwin ;
  long		    MaVolNb   ;
  struct List	    MaVolList ;
  struct Node	   *MaVolNode ;

  struct DosLibrary *doslib   ;
  struct RootNode   *rootnode ;
  struct DosInfo    *dosinfo  ;
  struct DeviceList *devlist  ;
  struct DevInfo    *devinfo  ;

  char	*p ;
  struct Entree *e ;

  MaVolNb = 0 ;
  VideMaListe( &LVolumes ) ;
  if ( ReqCour->rf_Indic & RFI_VOLUMES ) return ;

  InitMaListe( &MaVolList , TRUE ) ;
  if (! (doslib = Alloue( AT_LIBRARY , "dos.library" , 0L ))) return ;

  oldwin = Moi->pr_WindowPtr ;
  Moi->pr_WindowPtr = (APTR) -1 ;

  Forbid() ; /* BLOCAGE DU MULTI-TACHES */

  rootnode = doslib->dl_Root ;
  dosinfo  = (struct DosInfo *)(rootnode->rn_Info << 2L) ;

  for ( devlist = (struct DeviceList *)(dosinfo->di_DevInfo << 2L) ; devlist ; devlist = (struct DeviceList *)(devlist->dl_Next << 2L) )
  {
    devinfo = (struct DevInfo *)devlist ;

    if (! (e = Alloue( AT_MEMORY , (long)sizeof( struct Entree ) , MEMF_PUBLIC )))
    {
      Permit() ; /* REPRISE DU MULTI-TACHES */
      Libere( doslib ) ;
      VideMaListe( &LVolumes ) ;
      VideMaListe( &MaVolList ) ;
      goto _end ;
    }

    if ( devlist->dl_Type == DLT_VOLUME )
    {
      if (! devlist->dl_Task)
      {
	Libere( e ) ;
	continue ;
      }
      p = (char *)((long)devlist->dl_Name << 2L ) ;
    }
    else p = (char *)((long)devinfo->dvi_Name << 2L) ;

    BtoCstr( p , e->e_Name ) ;
    strcat( e->e_Name , ":" ) ;
    e->e_VName[0] = '\0' ;

    if ( devlist->dl_Type == DLT_DEVICE )
    {
      e->e_Type = ET_VOL ;

      /* handler pas chargé : on élimine l'entrée */

      if ( (! devinfo->dvi_Task) && (! devinfo->dvi_SegList) )
      {
	Libere( e ) ;
	continue ;
      }

      /* si le nom commence par "DF" on l'accepte tout de suite   */
      /* (permet d'éviter les requêtes si pas de disque dans DFx) */

      if (! strnicmp( e->e_Name , "DF" , 2 ))
      {
	AddTail( &MaVolList , (struct Node *)e ) ;
	MaVolNb++ ;
	continue ;
      }

      /* sinon on teste s'il s'agit bien d'un disque */

      if (! (cle = Lock( e->e_Name , ACCESS_READ )))
      {
	Libere( e ) ;
	continue ;
      }

      if ( Info( cle , info_ptr ) )
      {
	if ( Examine( cle , _ARes_Fib ) )
	{
	  strcpy( e->e_VName , _ARes_Fib->fib_FileName ) ;
	  strcat( e->e_VName , ":" ) ;
	}
	AddTail( &MaVolList , (struct Node *)e ) ;
	MaVolNb++ ;
      }
      else Libere( e ) ;

      UnLock( cle ) ;
      continue ;
    }

    e->e_Type = ET_DIR ;
    AddTail( &LVolumes , (struct Node *)e ) ;
    NbVolumes++ ;
  }

  Permit() ; /* REPRISE DU MULTI-TACHES */

  Libere( doslib ) ;

  /* trie puis fusionne les deux listes */

  TrieListe( &MaVolList , MaVolNb ) ;
  TrieListe( &LVolumes , NbVolumes ) ;

  MaVolNode = MaVolList.lh_TailPred ;
  MaVolNode->ln_Succ = LVolumes.lh_Head ;
  MaVolNode->ln_Succ->ln_Pred = MaVolNode ;
  LVolumes.lh_Head = MaVolList.lh_Head ;
  LVolumes.lh_Head->ln_Pred = (struct Node *)&(LVolumes.lh_Head) ;

  NbVolumes += MaVolNb ;

  /* Libère MaVolList */

  InitMaListe( &MaVolList , FALSE ) ;
  Libere( &MaVolList ) ;

_end:
  Moi->pr_WindowPtr = oldwin ;
}

/***********************************************************************/

static long TraiteMsgSub( struct IntuiMessage *Msg )

{
  SHORT  my ;
  char	 *p ;
  ULONG  classe ;
  struct Gadget *g ;
  USHORT code, qual ;
  BPTR	 cle1, cle2 ;
  struct Entree *e ;

  if ( Msg )
  {
    code   = Msg->Code ;
    classe = Msg->Class ;
    my	   = Msg->MouseY ;
    g	   = Msg->IAddress ;
    qual   = Msg->Qualifier ;
    ReplyMsg( (struct Message *)Msg ) ;
    AttendGUP = 0 ;
  }
  else
  {
    g = &Gadget6 ;
    classe = GADGETDOWN ;
  }

  if ( classe == MOUSEBUTTONS )
  {
    if ( ReqCour->rf_Indic & RFI_VOLUMES ) return( TM_RIEN )  ;
    if ( code == MENUUP )
    {
      ChangeListe() ;
      return( TM_STOP ) ;
    }
    return( TM_RIEN ) ;
  }

  if ( (classe == DISKINSERTED) || (classe == DISKREMOVED) )
  {
    if ( ReqCour->rf_Indic & RFI_VOLUMES ) return( TM_RIEN ) ;
    ChargeVolumes() ;
    if ( ListeCour == &LVolumes ) AfficheReq() ;
    return( TM_RIEN ) ;
  }

  if ( classe == CLOSEWINDOW ) g = &Gadget3 ;

  switch ( g->GadgetID )
  {
    case GID_ANNULER : *G4Tampon = '\0' ;
		       *G5Tampon = '\0' ;
    case GID_VALIDER : return( TM_FIN ) ;
    case GID_EXAMEN  : if ( (ListeCour == &LEntrees) || (ReqCour->rf_Indic & RFI_VOLUMES) )
			 return( TM_SCAN ) ;
		       ChargeVolumes() ;
		       AfficheReq() ;
		       return( TM_RIEN ) ;
    case GID_PARENT  : if ( cle1 = Lock( G4Tampon , ACCESS_READ ) )
		       {
			 if ( cle2 = ParentDir( cle1 ) )
			 {
			   if ( p = NomComplet( cle2 ) )
			   {
			     NomSelec = -1L ;
			     strcpy( G4Tampon , p ) ;
			     UnLock( cle1 ) ;
			     UnLock( LaCle ) ;
			     LaCle = cle2 ;
			     return( TM_SCAN ) ;
			   }
			   UnLock( cle2 ) ;
			 }
			 UnLock( cle1 ) ;
		       }
		       DisplayBeep( Fen->WScreen ) ;
		       return( TM_RIEN ) ;
    case GID_FICHIER : return( TM_FIN ) ;
    case GID_AFFICH  : OldSelec = NomSelec ;
		       NomSelec = PremGadget + ((long)my - TopOffset) / _ARes_Fonte->tf_YSize ;
		       AfficheReq() ;

		       if (! (e = NomEntree( NomSelec )))
		       {
			 DisplayBeep( Fen->WScreen ) ;
			 return( TM_RIEN ) ;
		       }

		       if ( e->e_Type & ET_FILE )
		       {
			 ClearSelection() ;
			 if (! (ReqCour->rf_Indic & RFI_REPERT))
			 {
			   strcpy( G5Tampon , e->e_Name ) ;
			   Rafraichit( &Gadget4 ) ;
			   if ( OldSelec == NomSelec ) return( TM_FIN ) ;
			 }
			 return( TM_RIEN ) ;
		       }

		       if ( ListeCour == &LVolumes )
		       {
			 if (! (qual & RAW_SHIFTED))
			 {
			   ClearSelection() ;
			   *G4Tampon = '\0' ;
			   LastWasShifted = 0 ;
			 }
			 else
			 {
			   if ( ! LastWasShifted )
			   {
			     ClearSelection() ;
			     *G4Tampon = '\0' ;
			   }
			   *G5Tampon = '\0' ;
			   LastWasShifted = 1 ;
			   if ( e->e_Type & ET_SELECTED )
			   {
			     Rafraichit( &Gadget5 ) ;
			     return( TM_RIEN ) ;
			   }
			 }
			 e->e_Type |= ET_SELECTED ;
			 if ( *G4Tampon ) strcat( G4Tampon , "," ) ;
		       }

		       strcat( G4Tampon , e->e_Name ) ;
		       Rafraichit( &Gadget4 ) ;
		       if ( qual & RAW_SHIFTED )
		       {
			 AfficheReq() ;
			 return( TM_RIEN ) ;
		       }

    case GID_REPERT  : if ( cle1 = Lock( G4Tampon , ACCESS_READ ) )
		       {
			 if ( p = NomComplet( cle1 ) )
			 {
			   NomSelec = -1L ;
			   strcpy( G4Tampon , p ) ;
			   UnLock( LaCle ) ;
			   LaCle = cle1 ;
			   return( TM_SCAN ) ;
			 }
			 UnLock( cle1 ) ;
		       }
		       DisplayBeep( Fen->WScreen ) ;
		       return( TM_STOP ) ;
    case GID_ASCENS  : AttendGUP = ( classe == GADGETDOWN ) ;
		       AfficheReq() ;
		       return( TM_RIEN ) ;
  }

  return( TM_RIEN ) ;
}

/*************************************************************************/

static long ChargeRepert( char *nom )

/*
 * Lit le répertoire indiqué.
 * LaCle doit être valide et correspondre à nom.
 * Retourne TM_STOP en cas de problème,
 *	    TM_RIEN si tout va bien
 *	    TM_SCAN s'il faut rappeler ChargeRepert()
 *	    TM_FIN  s'il faut terminer
 */

{
  short  k ;
  struct Entree *p ;
  struct IntuiMessage *Msg ;

  if (! Examine( LaCle , _ARes_Fib )) return( TM_STOP ) ;
  if ( _ARes_Fib->fib_DirEntryType < 0L ) return( TM_STOP ) ;

  strcpy( G4Tampon , NomComplet( LaCle ) ) ;

  ListeCour = &LEntrees ;
  NbListe   = &NbEntrees ;
  VideMaListe( ListeCour ) ;

  G6Info.VertPot = 0 ;
  Rafraichit( &Gadget4 ) ;

  while ( ExNext( LaCle , _ARes_Fib ) )
  {
    if ( (! Filtre) || (*Filtre)( _ARes_Fib ) )
    {
      if (! (p = Alloue( AT_MEMORY , (long)sizeof( struct Entree ) , MEMF_PUBLIC )))
      {
	VideMaListe( &LEntrees ) ;
	return( TM_STOP ) ;
      }

      /* charge l'entrée suivante */

      p->e_Type = ( _ARes_Fib->fib_DirEntryType < 0 ) ? ET_FILE : ET_DIR ;

      if ( SysBase->LibNode.lib_Version >= 37 )
	if ( (_ARes_Fib->fib_DirEntryType == ST_LINKDIR ) ||
	     (_ARes_Fib->fib_DirEntryType == ST_LINKFILE) ||
	     (_ARes_Fib->fib_DirEntryType == ST_SOFTLINK) ) p->e_Type = ET_FILE ;

      p->e_Size = ( p->e_Type == ET_DIR ) ? 0 : _ARes_Fib->fib_Size ;
      strcpy( p->e_Name , _ARes_Fib->fib_FileName ) ;
      AddTail( ListeCour , (struct Node *)p ) ;
      (*NbListe)++ ;

      /* met à jour l'ascenseur */

      if ( *NbListe <= NBAFFICHES )
	G6Info.VertBody = (USHORT)0xFFFF ;
      else
	G6Info.VertBody = (USHORT)((65535L * NBAFFICHES) / (*NbListe << 1) ) ;
      Rafraichit( &Gadget6 ) ;
      AfficheReq() ;
    }

    /* réagit aux messages */

    while ( Msg = (struct IntuiMessage *)GetMsg( Fen->UserPort ) )
    {
      k = TraiteMsgSub( Msg ) ;
      if ( k == TM_RIEN ) continue ;
      if ( k == TM_STOP ) return( TM_RIEN ) ;
      VideMaListe( &LEntrees ) ;
      if ( k == TM_SCAN ) return( TM_SCAN ) ;
      else return( TM_FIN ) ;
    }
  }

  TrieListe( &LEntrees , NbEntrees ) ;
  AfficheReq() ;
  return( TM_RIEN ) ;
}

/*************************************************************************/

static long TraiteMsg( struct IntuiMessage *Msg )

{
  long k , l ;

  if ( (l = TraiteMsgSub( Msg )) == TM_SCAN )
  {
    OffGadget( &Gadget6 , Fen , NULL ) ;
    G4Info.DispPos = 0 ;
    while ( (k = ChargeRepert( G4Tampon )) == TM_SCAN ) ;
    if ( k == TM_FIN ) indic = 0 ;
    else
    {
      if (k == TM_STOP)
      {
	DisplayBeep( Fen->WScreen ) ;
	*G4Tampon = '\0' ;
      }
      OnGadget( &Gadget6 , Fen , NULL ) ;
      Rafraichit( &Gadget4 ) ;
    }
    k = l ;
  }
  else if ( l == TM_FIN ) indic = 0 ;

  return( l ) ;
}

/*************************************************************************/

char *RequeteFic( struct ReqFic *LaReq , struct TextFont *pFonte )

{
  char	 *p ;
  long	 k, l ;
  struct Screen *ecran ;
  struct IntuiMessage *Msg ;

  /* alloue les ressources */

  ReqCour = LaReq ;

  if (! AlloueMarque( AM_NOUVELLE )) return( NULL ) ;

  if (! (_ARes_Fib = Alloue( AT_MEMORY , (long)sizeof( struct FileInfoBlock ) , MEMF_PUBLIC )))
    return( NULL ) ;

  if (! (info_ptr = Alloue( AT_MEMORY , (long)sizeof( struct InfoData ) , MEMF_PUBLIC )))
    goto _end ;

  _ARes_Fonte = pFonte ;
  if ( ! _ARes_Fonte ) _ARes_Fonte = OpenFont( &Topaz8 ) ;
  if ( ! _ARes_Fonte ) goto _end ;

  /* initialisations */

  AttendGUP = 0 ;
  NomSelec  = -1L ;
  LastWasShifted = 0 ;

  Filtre = LaReq->rf_Filtre ;
  InitMaListe( &LEntrees , TRUE ) ;
  InitMaListe( &LVolumes , TRUE ) ;

  /* prépare l'interface */

  if ( LaReq->rf_Indic & RFI_REPERT )
    Gadget5.Flags |= GADGDISABLED ;
  else
    Gadget5.Flags &= ~GADGDISABLED ;

  /* sélectionne la langue */

  if ( LaReq->rf_Indic & RFI_ANGLAIS )
  {
    Gadget1.GadgetText	  = &G1TextA ;
    Gadget1bis.GadgetText = &G1bisTextA ;
    Gadget2.GadgetText	  = &G2TextA ;
    Gadget3.GadgetText	  = &G3TextA ;
    Gadget4.GadgetText	  = &IText2A ;
    Gadget5.GadgetText	  = &IText1A ;
  }

  if ( LaReq->rf_Indic & RFI_ALLEMAND )
  {
    Gadget1.GadgetText	  = &G1TextD ;
    Gadget1bis.GadgetText = &G1bisTextD ;
    Gadget2.GadgetText	  = &G2TextD ;
    Gadget3.GadgetText	  = &G3TextD ;
    Gadget4.GadgetText	  = &IText2D ;
    Gadget5.GadgetText	  = &IText1D ;
  }

  /* ouvre la fenêtre */

  if ( LaReq->rf_Ecran )
  {
    NFen.Screen = ecran = LaReq->rf_Ecran ;
    NFen.Type	= CUSTOMSCREEN ;
    k = LaReq->rf_Ecran->Width ;
    l = LaReq->rf_Ecran->Height ;
  }
  else
  {
    NFen.Type = WBENCHSCREEN ;
    ecran = ChercheWB() ;
    k = ( ecran ) ? ecran->Width  : 640 ;
    l = ( ecran ) ? ecran->Height : 200 ;
  }

  NFen.Title  = LaReq->rf_Titre ;
  NFen.Width  = 28 + ( 38 * _ARes_Fonte->tf_XSize ) ;
  NFen.Height = 40 + ( 14 * _ARes_Fonte->tf_YSize ) ;

  k = ( k - NFen.Width ) >> 1 ;
  NFen.LeftEdge = ( k > 0 ) ? k : 0 ;
  NFen.TopEdge	= ( l - NFen.Height ) / 3 ;

  if (! (Fen = OpenWindow( &NFen ))) goto _end ;
  SetFont( Fen->RPort , _ARes_Fonte ) ;

  TopOffset = Fen->BorderTop + 5 ;

  /* prépare les gadgets */

  AutoAdapt( Fen , AutoReq , _ARes_Fonte ) ;

  k = Fen->Height - Gadget1.Height - 4 ;
  Gadget1.TopEdge    = k ;
  Gadget1bis.TopEdge = k ;
  Gadget2.TopEdge    = k ;
  Gadget3.TopEdge    = k ;

  G6Info.VertPot   = 0 ;
  G6Info.VertBody  = (USHORT)0xFFFF ;
  Gadget6.Flags   |= GADGDISABLED ;
  Gadget6.LeftEdge = Gadget4.LeftEdge + Gadget4.Width + 8 - Gadget6.Width ;
  Gadget0.Width    = Gadget6.LeftEdge - Gadget0.LeftEdge - 4 ;

  /* ajoute les gadgets et fini l'affichage */

  AddGList( Fen , &Gadget0 , -1 , -1 , NULL ) ;

  ClearFen( Fen , ( LaReq->rf_Indic & RFI_COULEUR ) ? GRIS : BLEU ) ;
  SetAPen( Fen->RPort , GRIS ) ;
  GfxFill( Fen->RPort , &Gadget0 , -1 ) ;
  GfxGList( Fen->RPort , &Gadget0 , -1 , BLANC , NOIR ) ;
  Rafraichit( &Gadget0 ) ;

  Moi = (struct Process *) FindTask( NULL ) ;
  OldWindowPtr = Moi->pr_WindowPtr ;
  Moi->pr_WindowPtr = Fen ;

  /*
   * Charge les "string gadgets" à partir du nom complet de l'objet désigné
   * par LaReq->rf_Cle. En cas d'échec prend "RAM:" comme objet de départ
   */

  if (! (p = NomComplet( LaReq->rf_Cle ))) strcpy( Tmp , "RAM:" ) ;
				      else strcpy( Tmp , p ) ;
  p = NomDeBase( Tmp ) ;
  if ( *p ) /* il y a un nom de fichier */
  {
    strcpy( G5Tampon , p ) ;
    *p = '\0' ;
  }
  else *G5Tampon = '\0' ;
  strcpy( G4Tampon , Tmp ) ;

  /* essaye d'obtenir une clé sur le répertoire */

  if (! (LaCle = Lock( G4Tampon , ACCESS_READ )))
  {
    Moi->pr_WindowPtr = OldWindowPtr ;
    CloseWindow( Fen ) ;
    goto _end ;
  }

  /*** charge la liste des volumes ***/

  ChargeVolumes() ;

  /*** charge le répertoire ***/

  Rafraichit( &Gadget0 ) ;
  while ( (k = ChargeRepert( G4Tampon )) == TM_SCAN ) ;
  indic = ( k != TM_FIN ) ;
  if ( k == TM_STOP )
  {
    DisplayBeep( Fen->WScreen ) ;
    *G4Tampon = '\0' ;
  }
  OnGadget( &Gadget6 , Fen , NULL ) ;
  Rafraichit( &Gadget4 ) ;

  /*** traite les messages ***/

  while ( indic )
  {
    Wait( 1L << Fen->UserPort->mp_SigBit ) ;
    while ( (Msg = (struct IntuiMessage *)GetMsg( Fen->UserPort )) || (AttendGUP) )
    {
      TraiteMsg( Msg ) ;
      if (! indic) break ;
    }
  }

  /*** libère tout et rend la main ***/

  UnLock( LaCle ) ;
  CloseWindow( Fen ) ;
  Moi->pr_WindowPtr = OldWindowPtr ;

  if ( _ARes_Fonte == pFonte ) _ARes_Fonte = NULL ;
  AReqFree( _ARes_Fonte ) ;

  strcpy( Tmp , G4Tampon ) ;
  if (! (LaReq->rf_Indic & RFI_REPERT)) strcat( Tmp , G5Tampon ) ;
  return( Tmp ) ;

_end:

  if ( _ARes_Fonte == pFonte ) _ARes_Fonte = NULL ;
  AReqFree( _ARes_Fonte ) ;
  return( NULL ) ;
}


