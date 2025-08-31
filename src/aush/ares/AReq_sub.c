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

static void InitMaListe( struct List *p , long flg )

{
  if ( flg ) Alloue( AT_XLIST , p ) ;

  NewList( p ) ;
  if ( p == &LEntrees ) NbEntrees = 0L ;
  else if ( p == &LVolumes ) NbVolumes = 0L ;
}

/*************************************************************************/

static void VideMaListe( struct List *q )

{
  Libere( q ) ;
  InitMaListe( q , TRUE ) ;
}

/***********************************************************************/

static void Rafraichit( struct Gadget *Gadget )

{
  RefreshGadgets( Gadget , Fen , NULL ) ;
  GfxGList( Fen->RPort , &Gadget6 , 1 , BLANC , NOIR ) ;
}

/***********************************************************************/

static struct Entree *NomEntree( long no )

{
  struct Entree *p, *q ;

  for ( p = FirstEntry( ListeCour ) ; q = NextEntry( p ) ; p = q )
  {
    if ( ! no ) return( p ) ;
    no-- ;
  }

  return( NULL ) ;
}

/*************************************************************************/

void SwapNodes( struct Node *pFirst, struct Node *pSecond )

/* Swaps two nodes in an exec list */

{
  struct Node *pFPred, *pSPred ;

  /* get the preds of the two nodes */
  pFPred = pFirst->ln_Pred ;
  if ( ! pFPred->ln_Succ ) pFPred = NULL ;
  pSPred = pSecond->ln_Pred ;
  if ( ! pSPred->ln_Succ ) pSPred = NULL ;

  /* remove the second node, and insert it after the pred of the first */
  Remove( pSecond ) ;
  Insert( ListeCour , pSecond , pFPred ) ;

  /* remove the first node, and insert it after the old pred of the second */
  if ( pSPred != pFirst )
  {
    Remove( pFirst ) ;
    Insert( ListeCour , pFirst , pSPred ) ;
  }
}

/***********************************************************************/

static void TrieListe( struct List *Tete , long Taille )

/* Adaptation du tri "Shell" (cf K&R p105) */

{
  long	 g , i , j ;
  struct Entree *p , *q ;
  struct List	 *OldListeCour ;

  if ( Taille < 2L ) return ;
  OldListeCour = ListeCour ;
  ListeCour    = (struct List *)Tete ;

  for ( g = Taille >> 1L ; g > 0L ; g >>= 1L )
    for ( i = g ; i < Taille ; i++ )
      for ( j = i - g ; j >= 0L ; j -= g )
      {
	p = NomEntree( j ) ;
	q = NomEntree( j + g ) ;

	if ( (p->e_Type & ET_DIR) && (q->e_Type & ET_FILE) ) break ;
	if ( (p->e_Type & ET_FILE) && (q->e_Type & ET_DIR) ) goto _exg ;
	if ( stricmp( p->e_Name , q->e_Name ) <= 0L ) break ;

_exg:
	SwapNodes( (struct Node *)p , (struct Node *)q ) ;
      }

  ListeCour = OldListeCour ;
}

/***********************************************************************/

static void AfficheMot( long l , struct Entree *p )

/* Affiche le mot pointé par p, en complétant si besoin avec des blancs */

{
  long n ;
  char mot[MAXNOM+10L] ;

  if ( l < 0L ) return ;

  *mot = '\0' ;
  if ( p )
  {
    if ( p->e_Type & ET_FILE )
      sprintf( mot , "%-28s %6d" , p->e_Name , p->e_Size ) ;
    else if ( p->e_Type & ET_VOL )
      sprintf( mot , "%-18s %16s" , p->e_Name , p->e_VName ) ;
    else
      strcpy( mot , p->e_Name ) ;
  }
  for ( n = strlen( mot ) ; n < MAXNOM+3 ; n++ ) mot[n] = ' ' ;

  Move( Fen->RPort , 12 , (l * _ARes_Fonte->tf_YSize) + TopOffset + _ARes_Fonte->tf_Baseline ) ;
  Text( Fen->RPort , mot , MAXNOM+3 ) ;
}

/***********************************************************************/

static void AfficheReq( void )

/* Met à jour l'affichage dans la requête */

{
  long j , k , l ;
  struct RastPort *r ;
  struct Entree   *e ;

  /* calcule début affichage */

  j = *NbListe ;
  if ( j < NBAFFICHES )
    k = 0L ;
  else
    k = ((long)G6Info.VertPot * (j - NBAFFICHES)) / 65535L ;

  if ( k < 0 ) k = 0 ;
  else if ( k > j ) k = j - NBAFFICHES ;

  /* affichage */

  r = Fen->RPort ;
  PremGadget = k ;
  SetAPen( r , NOIR ) ;

  for ( l = 0L , j = k ; (l < NBAFFICHES) && (j < *NbListe) ; l++ , j++ )
  {
    if (! (e = NomEntree( j ))) continue ;
    if ( e->e_Type & ET_DIR ) SetAPen( Fen->RPort , BLANC ) ;
    if ( (j == NomSelec) || (e->e_Type & ET_SELECTED) ) SetBPen( r , BLEU ) ;
    AfficheMot( l , NomEntree( j ) ) ;
    if ( (j == NomSelec) || (e->e_Type & ET_SELECTED) ) SetBPen( r , GRIS ) ;
    SetAPen( r , NOIR ) ;
  }

  while ( l < NBAFFICHES )
  {
    AfficheMot( l , NULL ) ;
    l++ ;
  }
}

/***********************************************************************/

static void ChangeListe( void )

{
  if ( (ListeCour == &LEntrees) || (ReqCour->rf_Indic & RFI_VOLUMES) )
  {
    ListeCour = &LVolumes ;
    NbListe   = &NbVolumes ;
  }
  else
  {
    ListeCour = &LEntrees ;
    NbListe   = &NbEntrees ;
  }

  NomSelec = -1L ;
  G6Info.VertPot = 0 ;

  if ( *NbListe <= NBAFFICHES )
    G6Info.VertBody = (USHORT)0xFFFF ;
  else
    G6Info.VertBody = (USHORT)((65535L * NBAFFICHES) / (*NbListe << 1) ) ;
  Rafraichit( &Gadget6 ) ;

  AfficheReq() ;
}

/***********************************************************************/

static void AReqFree( struct TextFont *font )
{
  if ( font ) CloseFont( font ) ;

  /* On libère à part les listes, car sinon les éléments sont libérés avant */
  /* la libération de la liste elle-même, et donc le pointeur List.lh_Head  */
  /* n'est plus valide (et Enforcer n'est pas content !) */
  Libere( &LEntrees ) ;
  Libere( &LVolumes ) ;

  LibereMarque( NULL ) ;
  _ARes_Fib = NULL ;
}

/***********************************************************************/

static void ClearSelection( void )
{
  struct Entree *f, *g ;

  for ( f = FirstEntry( ListeCour ) ; g = NextEntry( f ) ; f = g )
    f->e_Type &= ~ET_SELECTED ;
}


