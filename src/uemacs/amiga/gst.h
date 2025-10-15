/*	AMIGADOS.C:	Operating specific I/O and Spawning functions
			for MicroEMACS 3.12
			(C)Copyright 1993 by Daniel M. Lawrence
*/

#include        <stdio.h>
#ifdef __SASC
#include	<string.h>
#else
#define __aligned
#endif

#include	"estruct.h"
#if	AMIGA
#include	<exec/types.h>
#include	<exec/io.h>
#include	<exec/memory.h>
#include	<exec/libraries.h>
#include	<devices/inputevent.h>
#include	<graphics/text.h>
#include	<graphics/gfxbase.h>
#include	<graphics/view.h>
#include	<graphics/displayinfo.h>
#include	<intuition/intuition.h>
#include	<intuition/intuitionbase.h>
#include	<intuition/screens.h>
#include	<utility/tagitem.h>
#include	<libraries/asl.h>
#include	<devices/console.h>
#include	<dos/dos.h>

#ifdef __SASC
#include <proto/dos.h>
#include <clib/exec_protos.h>
#include <clib/graphics_protos.h>
#include <clib/intuition_protos.h>
#include <clib/console_protos.h>
#include <clib/asl_protos.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/graphics_pragmas.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/console_pragmas.h>
#include <pragmas/asl_pragmas.h>

void in_init(void);
int in_check(void);
void in_put(int);
int in_get(void);
int mod(int);
int sendcon(char *buf);
int doevent(void);
void dokey(struct InputEvent *);
int stuffibuf(int, int, int );
int spawncli(int , int );
int spawn(int , int );
int execprg(int , int );
int pipecmd(int , int );
void adoshello(void);
char *MakePathname(char *, char *);
int FileReq(char *, char *, unsigned);
#endif

#ifdef AZTEC_C
#define memset(a,b,c) setmem(a,c,b)
#endif

#include	"eproto.h"
#include        "edef.h"
#include	"elang.h"
