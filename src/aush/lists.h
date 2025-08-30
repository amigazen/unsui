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

typedef signed char   BYTE ;
typedef unsigned char UBYTE ;

struct Node
{
  struct Node *ln_Succ ;
  struct Node *ln_Pred ;
  UBYTE        ln_Type ;
  BYTE	       ln_Pri  ;
  char	      *ln_Name ;
} ;

struct List
{
  struct Node *lh_Head ;
  struct Node *lh_Tail ;
  struct Node *lh_TailPred ;
  UBYTE        lh_Type ;
  UBYTE        lh_Pad  ;
} ;

