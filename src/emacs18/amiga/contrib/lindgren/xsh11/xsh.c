/*
 *  NAME
 *	xsh
 *
 *  DESCRIPTION
 *	Try to create a new wshell within an old shell window.
 *	(To be used together with emacs)
 *
 *  AUTHOR
 *	Anders Lindgren, d91ali@csd.uu.se
 *
 *  HISTORY
 *	     d91ali - Oct 28, 1992: Created.
 */

#include <exec/types.h>

#include <dos/dos.h>

#define _USEOLDEXEC_
#include <proto/exec.h>
#include <proto/dos.h>

void __saveds
not_main()
{
    register struct Library * DOSBase;
    if (DOSBase = OpenLibrary("dos.library", 0L)) {
	Execute("s:WShell-Startup", Input(), 0L);
	CloseLibrary(DOSBase);
    }
}
