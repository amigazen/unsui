/*
* This file is part of AUSH.
* Copyright (C) 1994 Denis Gounelle
* 
* AUSH is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* AUSH is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with AUSH.  If not, see <http://www.gnu.org/licenses/>.
*
*/
/***************************************************************************
 *
 * AUSH (c)1992 par Denis GOUNELLE
 *
 ***************************************************************************/

/*
 * These functions emulate the following Amiga "Exec" lists functions
 *
 * NewList( struct List * )                     reset the List structure
 * AddHead( struct List * , struct Node * )     add a Node at the head of the List
 * AddTail( struct List * , struct Node * )     add a Node at the tail of the List
 * Remove( struct Node * )                      remove a Node from a List
 * RemHead( struct List * )                     remove the first Node of the List
 */

#include <stdio.h>

#ifdef DEBUG
#include <stdlib.h>
#include <string.h>
#endif

#define SRC_LISTS_C
#include "lists.h"

/********************************************************************************/

void NewList( struct List *pList )
{
  pList->lh_Head = (struct Node *)&(pList->lh_Tail) ;
  pList->lh_Tail = NULL ;
  pList->lh_TailPred = (struct Node *)&(pList->lh_Head) ;
}

/********************************************************************************/

void AddHead( struct List *pList , struct Node *pNode )
{
  pNode->ln_Succ = pList->lh_Head ;
  pList->lh_Head = pNode ;
  pNode->ln_Pred = (struct Node *)pList ;
  pNode->ln_Succ->ln_Pred = pNode ;
}

/********************************************************************************/

void AddTail( struct List *pList , struct Node *pNode )
{
  struct Node *pLast ;

  pLast = pList->lh_TailPred ;
  pNode->ln_Succ = pLast->ln_Succ ;
  pLast->ln_Succ = pNode ;
  pNode->ln_Pred = pLast ;
  pList->lh_TailPred = pNode ;
}

/********************************************************************************/

struct Node *RemHead( struct List *pList )
{
  struct Node *pFirst ;

  pFirst = pList->lh_Head ;
  if ( ! pFirst->ln_Succ ) return( NULL ) ;

  pList->lh_Head = pFirst->ln_Succ ;
  pFirst->ln_Succ->ln_Pred = (struct Node *)pList ;
  return( pFirst ) ;
}

/********************************************************************************/

void Remove( struct Node *pNode )
{
  struct Node *pNext ;

  pNext = pNode->ln_Succ ;
  pNode->ln_Pred->ln_Succ = pNext ;
  pNext->ln_Pred = pNode->ln_Pred ;
}

/********************************************************************************/

#ifdef DEBUG

struct Data
{
  struct Node d_node ;
  char	      d_data[32] ;
} ;

void main( void )
{
  int count, k ;
  struct List mylist ;
  struct Data *pdata ;
  char tmp[32], *fmt = "node %d" ;

  count = 1 ;
  NewList( &mylist ) ;

  for (;;)
  {
    printf( "\n1 - AddHead\n2 - AddTail\n3 - RemHead\n4 - Remove\n5 - display\n0 - Quit\n" ) ;
    gets( tmp ) ;
    switch ( *tmp )
    {
      case '1' : pdata = malloc( sizeof(struct Data) ) ;
		 if ( pdata )
		 {
		   sprintf( pdata->d_data , fmt , count++ ) ;
		   printf( "AddHead %s\n" , pdata->d_data ) ;
		   AddHead( &mylist , (struct Node *)pdata ) ;
		 }
		 break ;
      case '2' : pdata = malloc( sizeof(struct Data) ) ;
		 if ( pdata )
		 {
		   sprintf( pdata->d_data , fmt , count++ ) ;
		   printf( "AddTail %s\n" , pdata->d_data ) ;
		   AddTail( &mylist , (struct Node *)pdata ) ;
		 }
		 break ;
      case '3' : puts( "RemHead" ) ;
		 RemHead( &mylist ) ;
		 break ;
      case '4' : puts( "node number ?" ) ;
		 gets( tmp ) ;
		 k = atoi( tmp ) ;
		 sprintf( tmp , fmt , k ) ;
		 for ( pdata = (struct Data *)mylist.lh_Head ; pdata->d_node.ln_Succ ; pdata = (struct Data *)pdata->d_node.ln_Succ )
		   if (! strcmp( pdata->d_data , tmp )) break ;
		 if ( pdata->d_node.ln_Succ )
		 {
		   printf( "Remove %s\n" , pdata->d_data ) ;
		   Remove( (struct Node *)pdata ) ;
		 }
		 else puts( "not found" ) ;
		 break ;
      case '5' : for ( pdata = (struct Data *)mylist.lh_Head ; pdata->d_node.ln_Succ ; pdata = (struct Data *)pdata->d_node.ln_Succ )
		   puts( pdata->d_data ) ;
		 break ;
      case '0' : exit( 0 ) ;
    }
  }
}

#endif

