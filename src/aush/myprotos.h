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

#ifdef _AMIGA
#include "proto/ares.h"

#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/locale.h>
#include <proto/asl.h>
#endif

#ifndef SRC_OSDEP_C
extern int  RunCmd(CMD * );
extern int  IsResident(char * );
extern int  RunBack(CMD * );
extern struct AnchorPath * AllocAnchor(char *,int );
extern char * FindFirst(char * , struct AnchorPath * );
extern char * FindNext(struct AnchorPath * );
extern void FreeAnchor(struct AnchorPath * , int );
extern void FreeRequest(void);
extern int  DoFileRequest(char * );

#ifdef _AMIGA
extern BPTR FILEtoBPTR( FILE * , int );
extern void FreeChild(struct ChildMsg * );
extern struct Window * GetConWin(void);
extern void MySendPacket( int , int ) ;
#else
extern char *NomDeBase( char * ) ;
extern char *strpcpy( char * , char * );
extern char *strxcat( char * , char *, ... ) ;
#endif

#endif /* SRC_OSDEP_C */

#ifndef SRC_FILES_C
extern void InitFiles(void);
extern struct MyFile * FAssign(FILE *);
extern struct MyFile * FOpen(char * , char * );
extern void FClose(struct MyFile * , int );
extern void FreeFiles(void);
extern int FGetchar(struct MyFile * );
extern int FGetline(struct MyFile * , char * , int );
#endif

#ifndef SRC_CONSOLE_C
extern void DoRequest(char * );
extern void DoReplace(char * );
extern char * rawgets(struct MyFile * , char * , char * );
#endif

#ifndef SRC_ALIAS_C
extern void InitAlias(void);
extern struct Alias * GetAlias(char * );
extern int SetAlias(char * , char * );
extern void FreeAlias(void);
#endif

#ifndef SRC_HIST_C
extern void FreeHist(void);
extern void AddHist(char * , int );
extern int LoadHist(void);
extern void InitHist(void);
extern int ExpandHist( char * );
extern int SaveHist(void);
#endif

#ifndef SRC_VARS_C
extern void InitVars(void);
extern struct Var * GetLocalPtr(char * );
extern struct Var * GetGlobalPtr(char * );
extern struct Var * GetVarPtr(char * );
extern char * GetLocal(char * );
extern char * GetGlobal(char * );
extern char * myGetVar(char * );
extern int mySetVar(char * , char * , int );
extern int SetLocal(char * , char * , int );
extern void FreeVars(struct List * );
extern int ScanVar(int (* )(struct Var * , void * ), void * );
extern int ExpandVar(char * , char ** );
extern int IsSet(char * );
extern int BadVarName(char * , char * );
#endif

#ifndef SRC_INTERNAL_C
extern int do_exit(CMD * );
extern int do_setvar(CMD * );
extern int do_set(CMD * );
extern int do_unset(CMD * );
extern int do_export(CMD * );
extern int do_readonly(CMD * );
extern int do_history(CMD * );
extern int do_source(CMD * );
extern int do_shift(CMD * );
extern int do_alias(CMD * );
extern int do_unalias(CMD * );
extern int do_if(CMD * );
extern int do_else(void);
extern int do_endif(void);
extern int do_stack(CMD * );
extern int do_read(CMD * );
extern int do_failat(CMD * );
extern int do_cd(CMD * );
extern int do_pushd(CMD * );
extern int do_popd(CMD * );
extern int do_dirs(CMD * );
extern int do_for(CMD * );
extern int do_done(CMD * );
extern int do_break(CMD * );
extern int do_jobs(CMD * );
extern int do_eval(CMD * );
extern int do_echo(CMD * );
extern int do_window(CMD * );
#endif

#ifndef SRC_EVAL_C
extern void InitRegs(void);
extern char * Eval(char * );
#endif

#ifndef SRC_FCT_C
extern char * MyLocaleStr(int );
extern void pError(int , char * );
extern void * myalloc(int , int );
extern char *BuildName( char * ) ;
extern void FreeCmd(CMD * );
extern void InitChild(void);
extern void DoFreeChild(CMD * , time_t * );
extern void FreeLoop(struct Loop * );
extern int BuildString(char * , char * , struct MyFile * );
extern void MyGetCwd(char * );
extern char * DisplayPrompt(struct MyFile * );
extern void DoSplit(CMD * , int );
extern int SearchCmdPath(CMD * );
extern int ExpandPat(char * , char ** );
extern int ParseArgs(int , char ** );
#endif

#ifndef SRC_PARSE_C
extern int BuildCmd(CMD * );
extern CMD * SplitLine(char * , int );
#endif

#ifndef SRC_AUSH_C
extern void AbortPrg(int );
extern void Termine(int );
extern char * DoSubst(char * , FILE *);
extern int ExecCmd(CMD * , FILE *);
extern int MainLoop(struct MyFile * , FILE *);
#endif

#ifndef _AMIGA

#ifndef SRC_LISTS_C
extern void NewList( struct List * );
extern void AddHead( struct List *, struct Node *) ;
extern void AddTail( struct List *, struct Node *) ;
extern struct Node *RemHead( struct List *) ;
extern void Remove( struct Node *) ;
#endif

#endif
