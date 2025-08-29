/*
 * TeXSaveString.c - a simple file to show how EVPaths.lib routines work.
 *
 * This program do not use any startup code.
 *
 * © 1994 by Giuseppe Ghibò
 *
 * Version 1.0 - 16 Oct 1994
 *
 * Compile with:
 * 
 * 	SC OPT LINK LIB NOSTKCHK NOSTARTUP EVPaths.lib TeXSaveString.c
 * or
 *
 *	SC OPT NOSTKCHK TeXSaveString.c
 *      SLINK FROM TeXSaveString.o TO TeXSaveString LIB EVPaths.lib LIB:sc.lib
 *
 * or
 *	SC OPT NOSTKCHK NOSTARTUP LINK TeXSaveString.c EVPaths.c SNPrintf.a
 *
 *
 * Usage:
 *
 *	TeXSaveString VAR=MYVAR FILE=<filename> [SHOW] STRING=<string>
 *
 * For example, with:
 *
 *	SetEnv NAME=GOOFY STRING=ram:,T:
 *	TeXSaveString VAR=GOOFY FILE=one.txt STRING=example
 *
 * obtain:
 *
 * 	File saved: `ram:one.txt'
 *
 * Then type:
 *
 *	Protect ram:one.txt -d
 *
 * and again:
 *
 *	TeXSaveString VAR=GOOFY FILE=one.txt STRING=example SHOW
 *
 * Then obtain:
 *
 *	File saved: `ram:T.txt'
 *
 */

#include <exec/execbase.h>
#include <workbench/startup.h>

#include <string.h>

#include <proto/dos.h>
#include <proto/exec.h>

#include "evpaths.h"

#define TEMPLATE "VAR/K,FILE/A,SHOW/S,STRING/F"

enum {	OPT_VAR,
	OPT_FILE,
	OPT_SHOW,
	OPT_STRING,
	OPT_COUNT };

extern struct ExecBase *SysBase;
extern struct DosLibrary *DOSBase;

#define BUFLEN 256
#define EVPBUF 8192L

void __saveds Main(void) {
	struct Process *me;
	struct WBStartup *WBenchMsg;
	struct RDArgs *rdargs;
	LONG opts[OPT_COUNT];
	char buf[BUFLEN];
	STRPTR varname = "", fname = NULL, String;
	BOOL show = 0;
	struct EnvVarPath *var;
	BPTR fh;

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
			if (opts[OPT_VAR])
				varname = (STRPTR) opts[OPT_VAR];

			if (opts[OPT_FILE])
				fname = (STRPTR) opts[OPT_FILE];

			if (opts[OPT_STRING])
				String =  (STRPTR) opts[OPT_STRING];

			if (opts[OPT_SHOW])
				show = 1;

			var = Alloc_EnvVarPath(varname, EVPBUF);

			Init_EnvVarPath(var, NULL, NULL);
			Init_EnvVarPath(var, ".", ENVPATH_DEFSTR);

			if (show)
			{
				int i = 0;

				Printf("The environment variable `%s' has length %ld\n", varname, GetVarLength(varname));
				while (var->storage.strings[i])
					Printf("%ld -> `%s'\n", i, var->storage.strings[i++]);
			}

//			if ((fh = EVP_Open(fname, var, NULL, NULL, MODE_NEWFILE)) != DOSFALSE)
			if ((fh = EVP_Open(fname, var, buf, 256, MODE_NEWFILE)) != DOSFALSE)
			{
				Write(fh, String, strlen(String));
				Close(fh);
				Printf("File saved: `%s'\n",buf);
			}
			else
				Printf("Can't save file `%s'.\n",fname);

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
