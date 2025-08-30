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

#define SRC_OSDEP_C
#include "aush.h"
#include "aush_msg.h"
#include "myprotos.h"

extern char ChildName[] ;
extern int TaillePile ;
extern struct Process *Moi ;
extern struct Window *ConWin ;
extern struct Library *AslBase ;
extern struct MsgPort *ChildPort ;

static char tmp[MAXLINE+1] ;
static struct FileRequester *fr = NULL ;

/***************************************************************************/

static void ChildDeath( void )

/*
 * Function called by OS when a child terminates, whith NG_ExitData in register d1.
 * Called within child context, so we *MUST* restore a4 as soon as possible. We
 * can't use "static __saveds void ChildDeath()" because program may be resident
 * (BLink produce a "Absolute reference to _LinkerDB" warning), so we save a4 in
 * the message itself.
 */

{
  struct ChildMsg *m ;

  m = (struct ChildMsg *) getreg( REG_D1 ) ;
  putreg( REG_A4 , m->cm_rega4 ) ;

  /* set death time */
  m->cm_xtime = time( NULL ) ;

  /* send message to AUSH */
  PutMsg( m->cm_port , (struct Message *)m ) ;
}

/***************************************************************************/

struct TagItem STags[] =
{
  SYS_Input, NULL,
  SYS_Output, NULL,
  TAG_END, NULL
} ;

struct TagItem CNPTags[] =
{
  NP_Seglist,NULL,
  NP_Input,NULL,
  NP_Output,NULL,
  NP_StackSize,NULL,
  NP_Priority,NULL,
  NP_CommandName,NULL,
  NP_Arguments,NULL,
  NP_ExitData,NULL,
  NP_ExitCode,(ULONG)ChildDeath,
  NP_FreeSeglist,FALSE,
  NP_Cli,TRUE,
  NP_CloseInput,FALSE,
  NP_CloseOutput,FALSE,
  NP_ConsoleTask,NULL,
  NP_Name,(ULONG)&ChildName[0],
  TAG_END,NULL
} ;

struct TagItem ReqTags[] =
{
  ASL_Window,NULL,
  ASL_Hail,(ULONG)"AUSH",
  ASL_FuncFlags,FILF_PATGAD,
  ASL_Dir,NULL,
  ASL_File,NULL,
  TAG_END,NULL,
};

/***************************************************************************/

BPTR FILEtoBPTR( FILE *f , int flg )
{
  BPTR b ;
  struct UFB *u ;

  u = chkufb( f->_file ) ;
  if ( ! u ) return( NULL ) ;

  b = (BPTR)u->ufbfh ;
  if ( flg ) u->ufbfh = NULL ;
  return( b ) ;
}

/***************************************************************************/

int RunCmd( CMD *cmd )
{
  sprintf( tmp , "%s %s" , cmd->c_path , cmd->c_args ) ;
  STags[0].ti_Data = FILEtoBPTR( cmd->c_in->f_desc , 0 ) ;
  STags[1].ti_Data = FILEtoBPTR( cmd->c_out , 0 ) ;
  return( SystemTagList( tmp , STags ) ) ;
}

/***************************************************************************/

int IsResident( char *name )
{
  return( (int)FindSegment( name , NULL , NULL ) ) ;
}

/***************************************************************************/

int RunBack( CMD *cmd )
{
  BPTR seg, desc ;
  struct Segment *s ;
  struct Process *p ;
  struct ChildMsg *m ;
  struct FileHandle *h ;

  /* load program code */

  s = FindSegment( cmd->c_name , NULL , NULL ) ;
  if ( s ) seg = s->seg_Seg ;
      else seg = LoadSeg( cmd->c_path ) ;
  if ( ! seg )
  {
    IOERROR( cmd->c_name ) ;
    return( -1 ) ;
  }

  /* allocate and prepare child-death message */

  m = (struct ChildMsg *)AllocMem( sizeof(struct ChildMsg) , MEMF_PUBLIC|MEMF_CLEAR ) ;
  if ( ! m )
  {
    if ( ! s ) UnLoadSeg( seg ) ;
    ERROR( ERROR_NO_FREE_STORE ) ;
    return( -1 ) ;
  }
  m->cm_msg.mn_Length = sizeof(struct ChildMsg) ;
  m->cm_msg.mn_Node.ln_Type = NT_MESSAGE;
  m->cm_port = ChildPort ;
  m->cm_rega4 = getreg( REG_A4 ) ; /* see ChildDeath() function */
  m->cm_cmd = cmd ;

  /* prepare tags to create new process */

  CNPTags[0].ti_Data = seg ;
  if ( cmd->c_flags & CF_OPENIN  )
  {
    CNPTags[1].ti_Data	= FILEtoBPTR( cmd->c_in->f_desc , 1 ) ;
    CNPTags[11].ti_Data = TRUE ;
    cmd->c_in->f_desc	= NULL ;
  }
  if ( cmd->c_flags & CF_OPENOUT )
  {
    CNPTags[2].ti_Data	= FILEtoBPTR( cmd->c_out , 1 ) ;
    CNPTags[12].ti_Data = TRUE ;
    cmd->c_out = NULL ;
  }
  CNPTags[3].ti_Data = TaillePile ;
  CNPTags[4].ti_Data = cmd->c_pri ;
  CNPTags[5].ti_Data = (ULONG)cmd->c_path ;
  CNPTags[6].ti_Data = (ULONG)cmd->c_args ;
  CNPTags[7].ti_Data = (ULONG)m ;
  if ( ! s ) CNPTags[9].ti_Data = TRUE ;

  /* set a valid console task */

  desc = Open( "NIL:" , MODE_OLDFILE ) ;
  if ( desc )
  {
    h = (struct FileHandle *) ((long)desc << 2) ;
    CNPTags[13].ti_Data = (ULONG)h->fh_Type ;
    Close( desc ) ;
  }
  else CNPTags[13].ti_Data = (ULONG)Moi->pr_ConsoleTask ;

  /* create new process */

  p = CreateNewProc( CNPTags ) ;
  if ( ! p )
  {
    if ( ! s ) UnLoadSeg( seg ) ;
    return( -1 ) ;
  }

  return( p->pr_TaskNum ) ;
}

/***************************************************************************/

struct AnchorPath *AllocAnchor( char *pat , int bufsize )
{
  struct AnchorPath *ap ;

  ap = (struct AnchorPath *)AllocMem( (sizeof(struct AnchorPath) + bufsize + 1) , MEMF_CLEAR|MEMF_PUBLIC ) ;
  if ( ! ap )
  {
    ERROR( ERROR_NO_FREE_STORE ) ;
    return( NULL ) ;
  }
  ap->ap_Strlen = bufsize ;
  return( ap ) ;
}

/***************************************************************************/

char *FindFirst( char *pat , struct AnchorPath *ap )
{
  if ( MatchFirst( pat , ap ) ) return( NULL ) ;
  return( (char *)&(ap->ap_Buf) ) ;
}

/***************************************************************************/

char *FindNext( struct AnchorPath *ap )
{
  if ( MatchNext( ap ) ) return( NULL ) ;
  return( (char *)&(ap->ap_Buf) ) ;
}

/***************************************************************************/

void FreeAnchor( struct AnchorPath *ap , int bufsize )
{
  MatchEnd( ap ) ;
  FreeMem( ap , (sizeof(struct AnchorPath) + bufsize + 1) ) ;
}

/***************************************************************************/

void FreeChild( struct ChildMsg *Msg )
{
  CMD *cmd ;

  cmd = Msg->cm_cmd ;
  cmd->c_retcode = Msg->cm_retcode ;
  DoFreeChild( cmd , &(Msg->cm_xtime) ) ;
  FreeMem( Msg , sizeof(struct ChildMsg) ) ;
}

/***************************************************************************/

void FreeRequest( void )

{
  if ( fr ) FreeAslRequest( fr ) ;
  fr = NULL ;
}

/***************************************************************************/

int DoFileRequest( char *str )
{
  int k ;

  if ( ! fr ) fr = AllocFileRequest( ) ;
  if ( ! fr ) return( FALSE ) ;

  *tmp = '\0' ;
  ReqTags[0].ti_Data = (ULONG)ConWin ;
  ReqTags[3].ti_Data = (ULONG)str ;
  ReqTags[4].ti_Data = (ULONG)tmp ;

  if (! AslRequest( fr , ReqTags )) return( FALSE ) ;

  strcpy( str , fr->rf_Dir ) ;
  k = strlen( str ) - 1 ;
  if ( k >= 0 )
    if ( (str[k] != ':') && (str[k] != '/') ) strcat( str , "/" ) ;
  strcat( str , fr->rf_File ) ;
  return( TRUE ) ;
}

/****************************************************************************/

void MySendPacket( int action , int arg )
{
  DoPkt( Moi->pr_ConsoleTask , action , arg , NULL , NULL , NULL , NULL ) ;
}

/****************************************************************************/

struct Window *GetConWin( void )
{
  struct Window *win ;
  struct InfoData *infodata ;

  infodata = AllocMem( sizeof(struct InfoData) , MEMF_CLEAR|MEMF_PUBLIC ) ;
  MySendPacket( ACTION_DISK_INFO , (long)infodata >> 2 ) ;
  win = (struct Window *)infodata->id_VolumeNode ;
  FreeMem( infodata , sizeof(struct InfoData) ) ;
  return( win ) ;
}

