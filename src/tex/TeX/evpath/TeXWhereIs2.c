/*
 * TeXWhereIs2.c - a simple file to show how EVPaths.lib routines work.
 *
 * This program do not use any startup code.
 *
 * © 1994 by Giuseppe Ghibò
 *
 * Version 1.0 - 31 Oct 1994
 *
 * Compile with:
 * 
 * 	SC OPT LINK LIB NOSTKCHK NOSTARTUP EVPaths.lib TeXWhereIs2.c
 * or
 *
 *	SC OPT NOSTKCHK TeXWhereIs2.c
 *      SLINK FROM TeXWhereIs2.o TO TeXWhereIs2 LIB EVPaths.lib LIB:sc.lib
 *
 * or
 *	SC OPT NOSTKCHK NOSTARTUP LINK TeXWhereIs2.c EVPaths.c SNPrintf.a
 *
 *
 * Usage:
 *
 *	TeXWhereIs2 VAR=MYVAR file1 file2 ... fileN [SHOW]
 *
 */

#include <exec/execbase.h>
#include <workbench/startup.h>

#include <string.h>

#include <proto/dos.h>
#include <proto/exec.h>

#include "evpaths.h"

#define TEMPLATE "VAR/K,FILE/A/M,SHOW/S"

enum {	OPT_VAR,
	OPT_FILE,
	OPT_SHOW,
	OPT_COUNT };

extern struct ExecBase *SysBase;
extern struct DosLibrary *DOSBase;

#define BUFLEN 256
#define EVPBUF 8192L

STRPTR default_path = ".,TeX:texinputs**,MF:mfinputs**";

//STRPTR default_path[] = { 	".",
//				"TeX:texinputs**",
//				"MF:mfinputs**",
//				NULL };

void __saveds Main(void) {
	struct Process *me;
	struct WBStartup *WBenchMsg;
	struct RDArgs *rdargs;
	LONG opts[OPT_COUNT];
	char buf[BUFLEN];
	STRPTR varname = "", *fname = NULL, s;
	BOOL show = 0;
	struct EnvVarPath *var;

	SysBase = *(struct ExecBase **)4;

	if ((DOSBase = (struct DosLibrary *)OpenLibrary("dos.library", 36L)) == NULL)
		return;

	me = (struct Process *)SysBase->ThisTask;

	if (me->pr_CLI)
	{
		WBenchMsg = NULL;

		memset (opts, 0, sizeof(opts));

		if (rdargs = ReadArgs(TEMPLATE, opts, NULL))
		{
			int j;

			if (opts[OPT_VAR])
				varname = (STRPTR) opts[OPT_VAR];

			if (opts[OPT_FILE])
				fname = (STRPTR *) opts[OPT_FILE];

			if (opts[OPT_SHOW])
				show = 1;

			var = Alloc_EnvVarPath(varname, EVPBUF);

//			Init_EnvVarPath(var, NULL, NULL);
//			Init_EnvVarPath(var, default_path, ENVPATH_DEFARR);
//			Init_EnvVarPath(var, default_path, ENVPATH_DEFSTR);
//			Init_EnvVarPath(var, default_path, ENVPATH_DEFSTR | ENVPATH_PREPEND_PATH);
			Init_EnvVarPath(var, default_path, ENVPATH_DEFSTR | ENVPATH_APPEND_PATH);

			if (show)
			{
				int i = 0;

				Printf("The environment variable `%s' has length %ld\n", varname, GetVarLength(varname));
				while (var->storage.strings[i])
					Printf("%ld -> `%s'\n", i, var->storage.strings[i++]);
			}

			j = 0;

			while (fname[j])
			{
				if (s = EVP_FileSearch(fname[j], var, buf, 256))
					Printf("File found: `%s'\n",s);
				else
					Printf("File `%s' not found.\n",fname[j]);

				j++;
			}

			Free_EnvVarPath(var);
			FreeArgs(rdargs);
		}
		else
			PrintFault(IoErr(), NULL);
	}
	else
	{
		BPTR StdOut;

		WaitPort(&me->pr_MsgPort);
		WBenchMsg = (struct WBStartup *)GetMsg(&me->pr_MsgPort);

		if (StdOut = Open("CON:20/20/400/80/A Window/Auto/Close/Wait", MODE_NEWFILE))
		{
			FPrintf(StdOut,"You must run me from CLI, not from WB!\n");
			Close(StdOut);
		}
	}

	if (DOSBase)
		CloseLibrary((struct Library *)DOSBase);

	if (WBenchMsg)
	{
		Forbid();
		ReplyMsg(&WBenchMsg->sm_Message);
	}
}
