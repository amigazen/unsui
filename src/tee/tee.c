/* tee - T-intersection in a pipe (similar to Un*x' tee)
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
 * V1.0: 10/Feb/95: first version
 * V1.1: 12/Feb/95: added IsInteractive() check
 */
#define THIS_PROGRAM    "tee"
#define THIS_VERSION    "1.1"

#include <exec/types.h>
#include <exec/libraries.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <dos/dosasl.h>
#include <stdlib.h>
#include <string.h>

#define SysBase_DECLARED
#include <proto/exec.h>
#include <proto/dos.h>

extern struct Library *SysBase;

#define DEFAULT_BUFFERS     100     /* same as C:copy */
#define BUFFER_BLOCK_SIZE   512


static const char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

const char Template[] = "FILE,APPEND/S,BUF=BUFFER/K/N";
__aligned struct {
    STRPTR  file;
    LONG    append;
    LONG    *bufsiz;
} Args;
const char console[] =  "CONSOLE:";


void
_main()
{
    __aligned UBYTE smallbuf[BUFFER_BLOCK_SIZE];
    UBYTE *buf = NULL;
    struct RDArgs *rdargs;
    BPTR infh, outfh, teefh;
    LONG r, r2, last = 0, bufsiz = 0, err = 0;
    BOOL conmode;

    if( SysBase->lib_Version < 37 ) {
        #define MESSAGE  THIS_PROGRAM " requires AmigaOS 2.04 or higher\n"
        Write(Output(), MESSAGE, sizeof(MESSAGE)-1);
        _exit(RETURN_FAIL);
        #undef MESSAGE
    }

    if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) ) {
        infh = Input(); outfh = Output();

        /* some stream devices don't support MODE_READWRITE */
        if( teefh = Open(Args.file ? Args.file : (STRPTR)console, Args.append ? MODE_READWRITE : MODE_NEWFILE) ) {
            if( IsInteractive(teefh) )
                conmode = TRUE;
            else {
                conmode = FALSE;
                if( Args.bufsiz )
                    bufsiz = *(Args.bufsiz);
                if( bufsiz <= 0 )
                    bufsiz = DEFAULT_BUFFERS;
                bufsiz *= BUFFER_BLOCK_SIZE;

                while( bufsiz > BUFFER_BLOCK_SIZE && (buf = AllocMem(bufsiz, MEMF_ANY))==NULL )
                    bufsiz >>= 1;

                if( Args.append )
                    if( Seek(teefh, 0, OFFSET_END) < 0 )
                        err = IoErr();
            }
            if( !buf ) {
                buf = smallbuf;
                bufsiz = BUFFER_BLOCK_SIZE;
            }

            while( !err && (r = Read(infh, buf, bufsiz)) > 0 ) {
                if( CheckSignal(SIGBREAKF_CTRL_C) )
                    err = ERROR_BREAK;
                else {
                    if( Write(outfh, buf, r) != r  ||  (r2 = Write(teefh, buf, r)) != r )
                        err = IoErr();
                    if( r2 > 0 )
                        last = r2;
                }
            }
            if( conmode && last && buf[last-1] != '\n' )
                FPutC(teefh, '\n');     /* Write(teefh, "\n", 1); */

            Close(teefh);

            if( buf != smallbuf )
                FreeMem(buf, bufsiz);
        }
        else    /* could not open tee file */
            err = IoErr();

        FreeArgs(rdargs);
    }
    else    /* error in arguments */
        err = IoErr();

    if( err ) {
        /* can't use PrintFault() because it prints to stdout */
        if( Fault(err, THIS_PROGRAM, smallbuf, BUFFER_BLOCK_SIZE) ) {
            if( outfh = Open(console, MODE_READWRITE) ) {
                FPuts(outfh, smallbuf);
                FPutC(outfh, '\n');
                Close(outfh);
            }
        }
        SetIoErr(err);  /* restore for 'why' command */
    }
    _exit(err ? RETURN_ERROR : RETURN_OK);
}

