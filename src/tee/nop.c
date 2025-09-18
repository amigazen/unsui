/* nop - do nothing, excepts any number of args
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
 * V1.0: 04/Apr/95  first version
 */
#define THIS_PROGRAM    "nop"
#define THIS_VERSION    "1.0"

#include <exec/types.h>
#include <exec/libraries.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <stdlib.h>

#include <proto/exec.h>
#include <proto/dos.h>

static char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

char Template[] = "DUMMY/M";
__aligned struct {
    STRPTR *dummy;
} Args;


void
_main()
{
    struct RDArgs *rdargs;

    if( SysBase->lib_Version >= 37 ) {
        if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) )
            FreeArgs(rdargs);
    }
    _exit(0);
}

