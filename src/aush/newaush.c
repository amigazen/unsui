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
/*
 * NewAUSH for 2.04 system release
 * 23-Aug-92 by Denis GOUNELLE
 *
 * Source code based on "starttools.c" from ToolsManager V1.5 (c) 1991 by Stefan Becker
 *
 * "The source code for ToolManager is freely distributable, but copyrighted by
 *  me. You can use it in your programs but you MUST NOT remove the copyright note
 *  from it."
 *
 */

#include <exec/types.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <exec/libraries.h>
#include <dos/dostags.h>
#include <workbench/icon.h>
#include <workbench/startup.h>
#include <workbench/workbench.h>

/* Prototypes for system functions */
#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/icon.h>

static struct TagItem SysTags[] =
{
  SYS_Output,NULL,
  SYS_Input,NULL,
  SYS_Asynch,TRUE,	/* Run tools asynchronously */
  SYS_UserShell,TRUE,	/* Use user specified shell */
  NP_ConsoleTask,NULL,
  NP_Cli,TRUE,		/* create a CLI structure */
  TAG_DONE,NULL
} ;

static char FromFile[128] = "" ;
struct Library *IconBase = NULL ;
static char OutWin[128] = "CON:0/1/640/180/AUSH" ;
static char Usage[] = "Usage: NewAUSH [FROM file] [WINDOW window]\n" ;

/**** Start AUSH as a CLI process *********************************************/

static BOOL StartAUSH( char *cmd )
{
  BPTR ofh, ifh ;		 /* AmigaDOS file handles */
  struct MsgPort *oldct ;	 /* Old ConsoleTask pointer */
  struct MsgPort *newct = NULL ; /* New ConsoleTask pointer */

  /* Open input & output file */

  ofh = Open( OutWin , MODE_NEWFILE ) ;
  if ( ! ofh ) return( FALSE ) ;

  /* Is the output file an interactive file? */
  /* Yes. We need the same file as input file for CTRL-C/D/E/F redirection */
  /* Set our ConsoleTask to the new output file, so that we can re-open it */

  newct = ((struct FileHandle *) BADDR(ofh))->fh_Type ;
  oldct = GetConsoleTask() ;
  SetConsoleTask( newct ) ;

  /* Open the new input file (Now ifh points to the same file as ofh) */
  /* and change back to old ConsoleTask */

  ifh = Open( "CONSOLE:" , MODE_OLDFILE ) ;
  SetConsoleTask( oldct ) ;
  if ( ! ifh ) goto ste2 ;

  /* Start program */

  SysTags[0].ti_Data = ofh ;
  SysTags[1].ti_Data = ifh ;
  SysTags[4].ti_Data = (ULONG)newct ;
  if ( System( cmd , SysTags ) == -1 ) goto ste3 ;
  return( TRUE ) ;

  /* error */

ste3:
  Close( ifh ) ;
ste2:
  Close( ofh ) ;
ste1:
  return( FALSE ) ;
}

/**** Parse arguments (from WB) **************************************************/

static BOOL ParseWB( struct WBStartup *msg )
{
  LONG k ;
  char *q ;
  BPTR oldrep = NULL ;
  struct DiskObject *p ;

  /* opens icon library */

  if (! (IconBase = OpenLibrary( "icon.library" , 0L ))) return( FALSE ) ;

  /* select argument slot */

  k = 1 ;
  if ( k >= msg->sm_NumArgs ) k = 0 ;

  /* changes directory */

  if ( msg->sm_ArgList[k].wa_Lock ) oldrep = CurrentDir( msg->sm_ArgList[k].wa_Lock ) ;

  /* scans "TOOL TYPES" array */

  if ( p = GetDiskObject( msg->sm_ArgList[k].wa_Name ) )
  {
    if ( q = FindToolType( p->do_ToolTypes , "WINDOW" ) ) strcpy( OutWin , q ) ;
    if ( q = FindToolType( p->do_ToolTypes , "FROM"   ) ) strcpy( FromFile , q ) ;
    FreeDiskObject( p ) ;
  }

  /* go back to previous directory */

  if ( oldrep ) CurrentDir( oldrep ) ;

  /* close library and exit */

  CloseLibrary( IconBase ) ;
  return( TRUE ) ;
}

/**** Parse arguments (from CLI) **************************************************/

static BOOL ParseCLI(int argc, char *argv[] )
{
  long k ;

  if (! (argc & 1)) return( FALSE ) ;

  for ( k = 1 ; k < argc ; k++ )
    if (! stricmp( argv[k] , "WINDOW" ))
    {
      k++ ;
      strcpy( OutWin , argv[k] ) ;
    }
    else if (! stricmp( argv[k] , "FROM" ))
    {
      k++ ;
      strcpy( FromFile , argv[k] ) ;
    }
    else return( FALSE ) ;

  return( TRUE ) ;
}

/**** main ************************************************************************/

void main( int argc , char *argv[] )
{
  long k ;
  static char cmd[256] ;

  /* parse arguments */

  if ( argc )
  {
    k = ParseCLI( argc , argv ) ;
    if ( (! k) && Output() ) Write( Output() , Usage , strlen( Usage ) ) ;
  }
  else k = ParseWB( (struct WBStartup *)argv ) ;
  if ( ! k ) exit( 0 ) ;

  /* build command to run */

  strcpy( cmd , "AUSH" ) ;
  if ( *FromFile )
  {
    strcat( cmd , " FROM \"" ) ;
    strcat( cmd , FromFile ) ;
    strcat( cmd , "\"" ) ;
  }

  /* run command */

  StartAUSH( cmd ) ;
  exit( 0 ) ;
}

