/* getpubscreen - get name of frontmost screen (if public), else default pubscreen
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
 *  Basiert auf dem Hack von Henning Schmiedehausen (barnard@forge.erh.sub.org)
 *  vom Bielefelder Meeting '92, gepostet in de.alt.binaries.amigaos:
 *      Subject: Re: Bielefeld Hack #1: GetPubScreen
 *      Date: Mon, 14 Sep 1992 00:52:01 GMT
 *      Message-ID: <15XFr*1j0@forge.erh.sub.org>
 *
 * V1.0: 09/Feb/95
 * V1.1: 03/Apr/95: minor cleanup
 */
#define THIS_PROGRAM    "getpubscreen"
#define THIS_VERSION    "1.1"

#include <exec/types.h>
#include <exec/libraries.h>
#include <intuition/intuitionbase.h>
#include <intuition/screens.h>
#include <stdlib.h>
#include <string.h>

#define SysBase_DECLARED
#include <proto/exec.h>
#include <proto/dos.h>
#define IntuitionBase_DECLARED
#include <proto/intuition.h>

extern struct Library *         SysBase;
extern struct IntuitionBase *   IntuitionBase;

static const char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

const char Template[] = "DEFAULT/S";
__aligned struct {
    LONG    def;
} Args;


void
_main()
{
    struct RDArgs *rdargs;
    LONG rc = RETURN_OK;
    UBYTE name[MAXPUBSCREENNAME+1];

    if( SysBase->lib_Version < 37 ) {
        #define MESSAGE "requires AmigaOS 2.04 or higher\n"
        Write(Output(), MESSAGE, sizeof(MESSAGE)-1);
        _exit(RETURN_FAIL);
        #undef MESSAGE
    }

    if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) ) {
        LockPubScreenList();
        if( Args.def ) {
            GetDefaultPubScreen(name);
        }
        else {
            ULONG lock;
            struct Screen *firstscreen, *thisscreen;

            lock = LockIBase(0);
            firstscreen = IntuitionBase->FirstScreen;
            UnlockIBase(lock);

            if( (firstscreen->Flags & SCREENTYPE)==PUBLICSCREEN ) {
                thisscreen = NULL;
                do {
                    if( NextPubScreen(thisscreen, name)==NULL ) {
                        GetDefaultPubScreen(name);
                        rc = RETURN_WARN;
                        break;
                    }
                    thisscreen = LockPubScreen(name);
                    UnlockPubScreen(NULL, thisscreen);
                }
                while( thisscreen != firstscreen );
            }
            else {
                GetDefaultPubScreen(name);
                rc = RETURN_WARN;
            }
        }
        UnlockPubScreenList();
        FreeArgs(rdargs);

        PutStr(name); PutStr("\n");
    }
    else {
        PrintFault(IoErr(), THIS_PROGRAM);
        rc = RETURN_ERROR;
    }
    _exit(rc);
}

