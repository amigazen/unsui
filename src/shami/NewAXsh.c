
/*
    NewAXsh - Create a new shell window for a shell named 'AXsh'

    Michael B. Smith
    mbs%adastra.UUCP@Virginia.Edu  -or-   {uunet, mcnc}!virginia!adastra!mbs
    September, 1991

    This program uses illegal constructs (i.e., it modifies the Resident list).
    A timing hole exists, because I believe RunCommand () breaks the Forbid ().
    If someone knows a better (and/or more legal) way, please let ME know.

    Compiles with SAS/Lattice C and DICE.

    This program is freely distributable. No warranties, express or implied,
    are offered or available.
*/

#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT   "CON:0/10/640/200/AXsh/CLOSE FROM S:AXsh-Startup\n"
/* default parameter to NewShell */

long gotlock;				/* is Forbid () held? */
struct DosLibrary *dl;		/* dos.library for checking version */

void clean_exit(long result, char *msg)
{
    if(gotlock)
        Permit();

    if(msg)
	{
        PutStr(msg);
        PutStr("\n");
    }

    if(dl)
        CloseLibrary((struct Library *)dl);

    exit(result);
}

long main(long argc, char **argv)
{
    BPTR save_shell;         /* original shell_seg->seg_Seg */
    struct Segment *newshell_seg,      /* NewShell on resident list */
				   *shell_seg,         /* shell on resident list */
				   *xshell_seg;        /* xshell on resident list */
    unsigned char *p, buf[256];

    gotlock = 0;
    if(argc==0)
        /*
            We don't work from the WorkBench (PutStr() has no place to go)
        */
        clean_exit(30, NULL);

    dl = (struct DosLibrary *)OpenLibrary(DOSNAME, 37);
    if(!dl)
        clean_exit(30, "dos.library version 37 or greater is required");

    Forbid();
    gotlock=1;

    shell_seg = FindSegment("shell", NULL, CMD_SYSTEM);
    xshell_seg = FindSegment("AXsh", NULL, CMD_SYSTEM);
    newshell_seg = FindSegment("NewShell", NULL, CMD_INTERNAL);

    if(!shell_seg)
        clean_exit(30, "\"shell\" is not on System Resident list");

    if(!newshell_seg)
        clean_exit(30, "\"NewShell\" is not on Resident list");

    if(!xshell_seg)
        clean_exit(30, "\"AXsh\" is not on System Resident list");

    if(argc==1)
	{
        strcpy(buf, DEFAULT);
    }
    else
	{
        /*
            argc > 1
            For right now, don't handle more than one argument
        */
        if(argc>2)
            clean_exit(30, "Only one argument allowed");

        p = argv [1];
        if(*p == '?')
            clean_exit(10, "\nUsage: NewAXsh [<window specification>]\n");

        strcpy(buf, p);
        strcat(buf,"FROM S:AXsh-Startup\n");
    }

    save_shell = shell_seg->seg_Seg;
    shell_seg->seg_Seg = xshell_seg->seg_Seg;

    RunCommand(newshell_seg->seg_Seg, 2000, buf, strlen (buf));

    shell_seg->seg_Seg = save_shell;

    clean_exit(0, NULL);
}
