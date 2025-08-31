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
 * ARes (c)1990-1994 par Denis GOUNELLE
 *
 *************************************************************************/

#include "ARes.h"

/****************************************************************************/

static USHORT chip MonPtr[] =
{
  0x0000, 0x0000,

  0x0600, 0x0600,
  0x0F40, 0x0F40,
  0x3FE0, 0x3FE0,
  0x7FE0, 0x7FE0,
  0x61F0, 0x7FF0,
  0x7BF8, 0x7FF8,
  0xF7F8, 0xFFF8,
  0x61FC, 0x7FFC,
  0x7F0C, 0x7FFC,
  0x3FDE, 0x3FFE,
  0x7FBC, 0x7FFC,
  0x3F0C, 0x3FFC,
  0x1FF8, 0x1FF8,
  0x07F0, 0x07F0,
  0x01C0, 0x01C0,
  0x0700, 0x0700,
  0x0FC0, 0x0FC0,
  0x0680, 0x0680,
  0x0000, 0x0000,
  0x00C0, 0x00C0,
  0x00E0, 0x00E0,
  0x0040, 0x0040,

  0x0000, 0x0000
} ;

static long OldColor = -1 ;

static void ChSpriteColor( struct Window *fen , long Entry , long Color )
{
  SetRGB4( &(fen->WScreen->ViewPort) , Entry , (Color >> 8) & 0x0F , (Color >> 4) & 0x0F , (Color & 0x0F) ) ;
}

/****************************************************************************/

void SetWaitPtr( struct Window *fen )
{
  if ( OldColor != -1 ) return ;

  MonPtr[0] = MonPtr[1] = MonPtr[46] = MonPtr[47] = 0 ;
  OldColor = GetRGB4( fen->WScreen->ViewPort.ColorMap , 19 ) ;
  ChSpriteColor( fen , 19 , 0x0B00 ) ;
  SetPointer( fen , MonPtr , 22 , 15 , -7 , -8 ) ;
}

/****************************************************************************/

void RestorePtr( struct Window *fen )
{
  ActivateWindow( fen ) ;
  ClearPointer( fen ) ;
  if ( OldColor >= 0 ) ChSpriteColor( fen , 19 , OldColor ) ;
  OldColor = -1 ;
}

