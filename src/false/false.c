/*
 * false - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <exec/types.h>
#include <dos/dos.h>

#include <proto/dos.h>
#include <proto/exec.h>

static const char *verstag = "$VER: false 1.0 (11/09/25)\n";

extern struct DosLibrary *DOSBase;

int main(int argc, char **argv)
{
    /* Initialize DOSBase */
    DOSBase = (struct DosLibrary *)OpenLibrary("dos.library", 0);
    if (!DOSBase) {
        return EXIT_FAILURE;
    }
    
    /* Clean up */
    CloseLibrary((struct Library *)DOSBase);
    
    /* Return failure status as per POSIX specification */
    return EXIT_FAILURE;
}