/* exec
 *
 * Copyright (C) 1995 by Ingo Wilken (Ingo.Wilken@informatik.uni-oldenburg.de)
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation.  This software is provided "as is" without express or
 * implied warranty.
 *
 * V1.0: 23/Aug/95: first version
 */
#define THIS_PROGRAM    "exec"
#define THIS_VERSION    "1.0"

#include <exec/types.h>
#include <exec/libraries.h>
#include <dos/dostags.h>
#include <stdlib.h>

#define SysBase_DECLARED
#include <proto/exec.h>
#include <proto/dos.h>

extern struct Library *         SysBase;

static const char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

const char Template[] = "STACK/K/N,PRI=PRIORITY/K/N,IGNORE/K/N,SYSSHELL/S,CMD=COM=COMMAND/A/F";
__aligned struct {
    LONG *  stacksize;
    LONG *  priority;
    LONG *  ignore;
    LONG    sysshell;
    STRPTR  command;
} Args;

static struct TagItem tags[] = {
    { SYS_UserShell,    TRUE },
    { TAG_IGNORE,       TRUE },
    { TAG_IGNORE,       TRUE },
    { TAG_END,          TRUE }
};


void
_main()
{
    struct RDArgs *rdargs;
    LONG rc, err = 0;

    if( SysBase->lib_Version < 37 ) {
        #define MESSAGE "requires AmigaOS 2.04 or higher\n"
        Write(Output(), MESSAGE, sizeof(MESSAGE)-1);
        _exit(RETURN_FAIL);
        #undef MESSAGE
    }

    if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) ) {
        if( Args.sysshell )
            tags[0].ti_Data = FALSE;
        if( Args.stacksize ) {
            tags[1].ti_Tag = NP_StackSize;
            tags[1].ti_Data = *Args.stacksize;
        }
        if( Args.priority ) {
            tags[2].ti_Tag = NP_Priority;
            tags[2].ti_Data = *Args.priority;
        }

        rc = SystemTagList(Args.command, tags);
        if( rc < 0 ) {
            err = IoErr();
            rc = RETURN_FAIL;
        }
        FreeArgs(rdargs);
    }
    else {
        err = IoErr();
        rc = RETURN_FAIL;
    }

    if( err )
        PrintFault(err, THIS_PROGRAM);

    if( Args.ignore && rc <= *Args.ignore )
        rc = RETURN_OK;

    _exit(rc);
}

