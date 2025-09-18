/* getvolume - get volume name of current dir or device
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
 * V1.0: 30/Jul/95
 */
#define THIS_PROGRAM    "getvolume"
#define THIS_VERSION    "1.0"

#include <exec/types.h>
#include <exec/libraries.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <stdlib.h>
#include <string.h>

#define SysBase_DECLARED
#include <proto/exec.h>
#include <proto/dos.h>

extern struct Library *         SysBase;

static const char amiga_version[] = "\0$VER: " THIS_PROGRAM " " THIS_VERSION " (" __COMMODORE_DATE__ ")";

const char Template[] = "DEVICE,NOREQ/S";
__aligned struct {
    STRPTR  device;
    LONG    noreq;
} Args;


void
_main()
{
    struct RDArgs *rdargs;
    __aligned struct InfoData info;
    BOOL succ = FALSE;
    UBYTE volume[258];
    APTR win;
    struct Process *thisProc = NULL;

    if( SysBase->lib_Version < 37 ) {
        #define MESSAGE "requires AmigaOS 2.04 or higher\n"
        Write(Output(), MESSAGE, sizeof(MESSAGE)-1);
        _exit(RETURN_FAIL);
        #undef MESSAGE
    }

    if( rdargs = ReadArgs(Template, (LONG *)&Args, NULL) ) {
        if( Args.noreq ) {
            thisProc = (struct Process *)FindTask(NULL);
            win = thisProc->pr_WindowPtr;
            thisProc->pr_WindowPtr = (APTR)-1;
        }
        if( Args.device ) {
            struct DevProc *devproc;

            if( devproc = GetDeviceProc(Args.device, NULL) ) {
                succ = (BOOL)DoPkt(devproc->dvp_Port, ACTION_DISK_INFO, MKBADDR(&info), 0, 0, 0, 0);
                FreeDeviceProc(devproc);
            }
        }
        else
            succ = Info(((struct Process *)FindTask(NULL))->pr_CurrentDir, &info);

        if( thisProc )
            thisProc->pr_WindowPtr = win;

        FreeArgs(rdargs);
    }

    if( succ ) {
        struct DeviceList *dlist;

        switch( info.id_DiskType ) {
            case ID_NO_DISK_PRESENT:
                SetIoErr(ERROR_NO_DISK);
                break;
            case ID_DOS_DISK:
            case ID_FFS_DISK:
            case ID_INTER_DOS_DISK:
            case ID_INTER_FFS_DISK:
            case ID_FASTDIR_DOS_DISK:
            case ID_FASTDIR_FFS_DISK:
            case ID_MSDOS_DISK:
                if( dlist = BADDR(info.id_VolumeNode) ) {
                    UBYTE *name;
                    int len;

                    /* BCPL string -> C string */
                    if( name = BADDR(dlist->dl_Name) ) {
                        len  = *name++;
                        strncpy(volume, (char *)name, len);
                        volume[len++]   = '\n';
                        volume[len] = '\0';
                        PutStr(volume);
                        _exit(RETURN_OK);
                    }
                }
            default:
                SetIoErr(ERROR_NOT_A_DOS_DISK);
        }
    }
    PrintFault(IoErr(), THIS_PROGRAM);
    _exit(RETURN_ERROR);
}

