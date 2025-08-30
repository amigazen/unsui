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

#define SRC_VARS_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

struct List VarList ;
extern struct MyFile *CurrentFile ;

static char tmp[MAXLINE+1] ;
static struct List *VarHead ; /* see GetLocalPtr() and SetLocal() */

/***************************************************************************/

void InitVars( void )
{
  NewList( &VarList ) ;
}

/***************************************************************************/

struct Var *GetLocalPtr( char *name )
{
  struct MyFile *file ;
  struct Var *p, *q ;

  if ( isdigit( (unsigned char)*name ) ) return( NULL ) ;

  for ( file = CurrentFile ; (file) && (file->f_flags & FF_FORDONE) ; file = file->f_parent ) ;
  if (! file) return( NULL ) ;

  VarHead = &(file->f_private) ;
  for ( p = (struct Var *)file->f_private.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
    if (! strcmp( name , p->v_name )) return( p ) ;

  while ( file )
  {
    VarHead = &(file->f_public) ;
    for ( p = (struct Var *)file->f_public.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
      if (! strcmp( name , p->v_name )) return( p ) ;
    file = file->f_parent ;
  }

  return( NULL ) ;
}

/***************************************************************************/

struct Var *GetGlobalPtr( char *name )
{
  struct Var *p, *q ;

  VarHead = &VarList ;
  for ( p = (struct Var *)VarList.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
    if (! strcmp( name , p->v_name )) return( p ) ;

  return( NULL ) ;
}

/***************************************************************************/

struct Var *GetVarPtr( char *name )
{
  struct Var *p ;

  if ( p = GetLocalPtr( name ) ) return( p ) ;
  return( GetGlobalPtr( name ) ) ;
}

/***************************************************************************/

char *GetLocal( char *name )
{
  int k, l ;
  struct Var *p ;
  struct MyFile *file ;

  for ( file = CurrentFile ; (file) && (file->f_flags & FF_FORDONE) ; file = file->f_parent ) ;

  if ( isdigit( (unsigned char)*name ) )
  {
    if (! file) return( NULL ) ;
    k = atol( name ) ;
    if ( k < file->f_argc ) return( file->f_argv[k] ) ;
    return( NULL ) ;
  }

  if (! strcmp( name , "*" ))
  {
    *tmp = '\0' ;
    if (! file) return( NULL ) ;
    for ( k = 1 , l = 0 ; k < file->f_argc ; k++ )
    {
      l += strlen( file->f_argv[k] ) + 1 ;
      if ( k > 1 ) l++ ;
      if ( l >= MAXLINE )
      {
	ERROR( ERROR_OVERFLOW ) ;
	break ;
      }
      if ( k > 1) strcat( tmp , " " ) ;
      strcat( tmp , file->f_argv[k] ) ;
    }
    return( tmp ) ;
  }

  if ( p = GetLocalPtr( name ) ) return( p->v_value ) ;
  return( NULL ) ;
}

/***************************************************************************/

char *GetGlobal( char *name )
{
  struct Var *p ;

  if ( p = GetGlobalPtr( name ) ) return( p->v_value ) ;
  return( NULL ) ;
}

/***************************************************************************/

char *myGetVar( char *name )
{
  char *v ;
  struct Var *p ;

  if ( v = GetLocal( name )) return( v ) ;
  if ( p = GetGlobalPtr( name ) ) return( p->v_value ) ;
  if ( v = getenv( name ) ) return( v ) ;

  return( NULL ) ;
}

/***************************************************************************/

int mySetVar( char *name, char *value , int flg )
{
  int l ;
  struct Var *p, *q ;

  if ( strlen( name ) >= MAXNAME )
  {
    ERROR( ERROR_OVERFLOW ) ;
    return( RETURN_FAIL ) ;
  }

  if ( q = GetGlobalPtr( name ) )
  {
    if ( (q->v_flags & VF_READONLY) && (! flg) )
    {
      pError( ERROR_READ_ONLY , q->v_name ) ;
      return( RETURN_FAIL ) ;
    }
    Remove( (struct Node *)q ) ;
    free( q ) ;
  }

  l = strlen( value ) ;
  p = (struct Var *)myalloc( (l + sizeof(struct Var)) , 0 ) ;
  if (! p) return( RETURN_FAIL ) ;

  p->v_flags = ( flg ) ? VF_READONLY : 0 ;
  strcpy( p->v_name , name ) ;
  strcpy( p->v_value , value ) ;
  AddHead( &VarList , (struct Node *)p ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

int SetLocal( char *name, char *value , int flg )
{
  int l ;
  struct MyFile *file ;
  struct Var *p, *q ;

  if ( strlen( name ) >= MAXNAME )
  {
    ERROR( ERROR_OVERFLOW ) ;
    return( RETURN_FAIL ) ;
  }

  if ( q = GetLocalPtr( name ) )
    if ( (q->v_flags & VF_READONLY) && (! (flg & VF_READONLY)) )
    {
      pError( ERROR_READ_ONLY , q->v_name ) ;
      return( RETURN_FAIL ) ;
    }

  l = strlen( value ) ;
  p = (struct Var *)myalloc( (l + sizeof(struct Var)) , 0 ) ;
  if (! p) return( RETURN_FAIL ) ;

  p->v_flags = (char)flg ;
  strcpy( p->v_name , name ) ;
  strcpy( p->v_value , value ) ;

  if ( q )
  {
    p->v_flags |= q->v_flags ;
    Remove( (struct Node *)q ) ;
    free( q ) ;
    AddHead( VarHead , (struct Node *)p ) ;
    return( RETURN_OK ) ;
  }

  for ( file = CurrentFile ; (file) && (file->f_flags & FF_FORDONE) ; file = file->f_parent ) ;
  if (! file) return( RETURN_FAIL ) ;

  AddHead( &(file->f_private) , (struct Node *)p ) ;
  return( RETURN_OK ) ;
}

/***************************************************************************/

void FreeVars( struct List *pHead )
{
  struct Var *p, *q ;

  for ( p = (struct Var *)pHead->lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
    free( p ) ;
}

/***************************************************************************/

int ScanVar( int (*func)( struct Var * , void * ), void *param )

/*
 * Parcourt la liste des variables en appelant func() pour chaque variable,
 * avec comme arguments un pointeur sur la variable et "param".
 * Continue tant que func() retourne RETURN_OK
 */

{
  int k ;
  struct MyFile *file ;
  struct Var *p, *q ;

  for ( file = CurrentFile ; (file) && (file->f_flags & FF_FORDONE) ; file = file->f_parent ) ;
  if (! file) return( RETURN_OK ) ;

  /* VarHead = &(file->f_private) ; */
  for ( p = (struct Var *)file->f_private.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
    if ( k = (*func)( p , param ) ) return( k ) ;

  while ( file )
  {
    /* VarHead = &(file->f_public) ; */
    for ( p = (struct Var *)file->f_public.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
      if ( k = (*func)( p , param ) ) return( k ) ;
    file = file->f_parent ;
  }

  return( RETURN_OK ) ;
}

/***************************************************************************/

static int AddVar( struct Var *p , int len , char *tab[] , int *pnb )
{
  len += 2 + strlen( p->v_name ) ;
  if ( len >= MAXLINE )
  {
    ERROR( ERROR_OVERFLOW ) ;
    return( -1 ) ;
  }

  if ( tab )
  {
    if ( *pnb >= MAXARG )
    {
      ERROR( ERROR_OVERFLOW ) ;
      return( -1 ) ;
    }
    tab[*pnb] = p->v_name ;
  }

  strxcat( tmp , (*pnb) ? " $" : "$" , p->v_name , NULL ) ;
  (*pnb)++ ;
  return( len ) ;
}

/***************************************************************************/

static char *tofind ;
static int lg, nb, len ;

static int do_addvar( struct Var *p , char *tab[] )
{
  if (! strncmp( tofind , p->v_name , (size_t)lg ))
  {
    len = AddVar( p , len , tab , &nb ) ;
    if ( len < 0 ) return( RETURN_FAIL ) ;
  }
  return( RETURN_OK ) ;
}

int ExpandVar( char *name, char *tab[] )
{
  struct Var *p, *q ;

  *tmp = '\0' ;
  nb = len = 0 ;
  tofind = &name[1] ;

  lg = strlen( name ) - 1 ;
  if (! lg) return( 0 ) ;

  for ( p = (struct Var *)VarList.lh_Head ; q = (struct Var *)p->v_node.ln_Succ ; p = q )
    if (! strncmp( tofind , p->v_name , (size_t)lg ))
    {
      len = AddVar( p , len , tab , &nb ) ;
      if ( len < 0 ) break ;
    }

  if ( len >= 0 ) ScanVar( do_addvar , tab ) ;

  strcpy( name , tmp ) ;
  return( nb ) ;
}

/***************************************************************************/

int IsSet( char *name )
{
  char *p ;

  p = myGetVar( name ) ;
  if ( ! p ) return( FALSE ) ;
  return( ! strcmp( p , "1" ) ) ;
}

/****************************************************************************/

int BadVarName( char *name , char *cmd )

/* checks var name for set/setvar/eval commands */

{
  if ( ! *name ) goto _err ;
  if ( isdigit( (unsigned char)*name ) ) goto _err ;

  for ( ; *name ; name++ )
  {
    if ( isalnum( (unsigned char)*name ) ) continue ;
    if ( *name != '_' ) goto _err ;
  }

  return( 0 ) ;

_err:

  if ( cmd ) pError( ERROR_BAD_ARGS , cmd ) ;
  return( 1 ) ;
}


