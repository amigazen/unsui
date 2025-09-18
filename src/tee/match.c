/* match - check if string matches pattern
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
 * V1.0: 27/Nov/94 first version (@ Bielefeld Meeting '94)
 * V1.1: 05/Dec/94 code cleanup, bugfix
 * V1.2: 01/Jul/95 replaced a few error messages with a PrintFault() call
 */
#define THIS_PROGRAM    "match"
#define THIS_VERSION    "1.2"

#include <exec/types.h>
#include <exec/libraries.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <stdlib.h>
#include <string.h>

#define SysBase_DECLARED
#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/utility.h>
#define PatternBase_DECLARED
#include <proto/pattern.h>

extern struct Library *SysBase;
struct Library *PatternBase = NULL;

static const char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

const char Template[] = "STRING/A,PAT=PATTERN/A,NOCASE/S";
__aligned struct {
    STRPTR  string;
    STRPTR  pattern;
    LONG    nocase;
} Args;

void
_main()
{
    struct RDArgs *rdargs;
    LONG err, rc = RETURN_OK;

    if( SysBase->lib_Version < 37 ) {
        #define MESSAGE "requires AmigaOS 2.04 or higher\n"
        Write(Output(), MESSAGE, sizeof(MESSAGE)-1);
        _exit(RETURN_FAIL);
        #undef MESSAGE
    }

    if( PatternBase = OpenLibrary(PATLIB_NAME, PATLIB_MIN_VERSION) ) {
        if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) ) {
            if( Args.nocase )
                err = SimpleMatchNoCase(Args.pattern, Args.string);
            else
                err = SimpleMatch(Args.pattern, Args.string);

            switch( err ) {
                case 0:
                    rc = RETURN_WARN;
                    break;
                case 1:
                    rc = RETURN_OK;
                    err = 0;
                    break;
                default:
                    rc = RETURN_FAIL;
                    err = PatternError2DOS(err);
            }
            FreeArgs(rdargs);
        }
        else {
            err = IoErr();
            rc = RETURN_ERROR;
        }
        CloseLibrary(PatternBase);
    }
    else {
        /* no pattern library */
        PutStr(THIS_PROGRAM ": " PATLIB_NAME " not found\n");
        err = 0;
        rc = RETURN_FAIL;
    }

    if( err )
        PrintFault(err, THIS_PROGRAM);

    _exit((int)rc);
}

