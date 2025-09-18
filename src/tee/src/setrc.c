/* setrc - set return code, similar to Un*x' /bin/true and /bin/false
 *
 * Copyright (C) 1994 by Ingo Wilken (Ingo.Wilken@informatik.uni-oldenburg.de)
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  This software is provided "as is" without express or
 * implied warranty.
 *
 * V1.0: 17/Apr/94 first version
 * V1.1: 11/Aug/94 added program name check
 * V1.2: 09/Feb/95 added result2
 */
#define THIS_PROGRAM    "setrc"
#define THIS_VERSION    "1.2"

#include <exec/types.h>
#include <exec/libraries.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <stdlib.h>
#include <string.h>

#include <proto/exec.h>
#include <proto/dos.h>

static char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

char Template[] = "OK/S,WARN/S,ERROR/S,FAIL/S,RC=RESULT/N,RC2=RESULT2/N";
__aligned struct {
    LONG    ok;
    LONG    warn;
    LONG    error;
    LONG    fail;
    LONG   *rc;
    LONG   *rc2;
} Args;

#define NAMELEN     256

const struct tbl_t {
    const char *name;
    const LONG  rc;
} table[] = {
    { "true",   1 },    /* IF `true` */
    { "false",  0 },
    { "fail",  20 },
    { "error", 10 },
    { "warn",   5 },
    { "ok",     0 },
    { NULL,     0 }
};


void
_main()
{
    struct RDArgs *rdargs;
    LONG rc = RETURN_OK;

    if( SysBase->lib_Version < 37 ) {
        #define MESSAGE "requires AmigaOS 2.04 or higher\n"
        Write(Output(), MESSAGE, sizeof(MESSAGE)-1);
        _exit(RETURN_FAIL);
        #undef MESSAGE
    }

    if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) ) {
        if( Args.rc )
            rc = *(Args.rc);
        else
        if( Args.fail )
            rc = RETURN_FAIL;
        else
        if( Args.error )
            rc = RETURN_ERROR;
        else
        if( Args.warn )
            rc = RETURN_WARN;
        else
        if( Args.ok )
            rc = RETURN_OK;
        else {
            char progname[NAMELEN];
            if( GetProgramName(progname, NAMELEN) ) {
                struct tbl_t *p;
                for( p = table; p->name; p++ ) {
                    if( stricmp(progname, p->name)==0 )
                        break;
                }
                rc = p->rc;
            }
        }
        if( Args.rc2 )
            SetIoErr(*Args.rc2);
        FreeArgs(rdargs);
    }
    else {
        PrintFault(IoErr(), THIS_PROGRAM);
        rc = RETURN_ERROR;
    }
    _exit((int)rc);
}

