/*
#include <exec/types.h>
#include <exec/nodes.h>
*/
#include <exec/memory.h>
#include <exec/lists.h>
#include <exec/execbase.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <dos/dostags.h>
#include <dos/var.h>
#include <proto/dos.h>
#include <proto/exec.h>
#include <stdio.h>

/*************************************************************************\
*	AXsh - Amiga external shell											  *
*																		  *
*	Pasi 'Albert' Ojala	�1991-95			albert@cs.tut.fi			  *
*																		  *
*	XShell by Michael B. Smith provided an example and a starting point   *
*   for this shell.	However, most parts are rewritten and even more parts *
*   have been added.													  *
*																		  *
*	This shell DOES require KS 2.04 (37.74 or above).					  *
*																		  *
*	To use:																  *
*		resident AXsh system											  *
*		newaxsh															  *
*																		  *
*	Btw, tab's should be set to 4 columns apart..						  *
\*************************************************************************/



#include "AXsh.h"

#include "users.h"

extern struct ExecBase *SysBase;
extern struct DosLibrary *DOSBase;
struct Library *AXshBase = NULL;

static const char version[] = "$VER: AXsh 1.7 " __DATE__ /*__AMIGADATE__*/;

struct Process *myproc;				/* The shell's process  */
struct CommandLineInterface *clip;	/* Cli() */
struct DosPacket *mypacket;			/* initialization packet from AmigaDOS */
struct Segment *seg_cmd;			/* for commands on the resident list */

long fn,			/* function result from CliInit... */
	 we_are_done,	/* final EOF has occurred */
	 cmd_state,		/* basically, where the command came from */
	 init_result;	/* calculated result from initial WaitPkt() */

BPTR new_input,		/* for execute'ing a script file */
	 seglist_cmd;	/* to be returned from NewLoadSeg */

static char Shell_Name[] = SHELL_NAME;

/*************************************************************************\
*	Definitions for the values of fn:									  *
*		Bit 31     Set to indicate flags are valid						  *
*		Bit  3     Set to indicate asynch system call					  *
*		Bit  2     Set if this is a System() call						  *
*		Bit  1     Set if user provided input stream					  *
*		Bit  0     Set if RUN provided output stream					  *
*																		  *
*	For CliInitRun():													  *
*																		  *
*	If Bit 31 is 0, then you must check IoErr() to determine if an error  *
*	occurred.  If IoErr() returns a pointer to your process, there has	  *
*	been an error, and you should clean up and exit.  The packet will	  *
*	have already been returned by CliInitRun().  If it isn't a pointer    *
*	to your process and Bit 31 is 0, you should wait before replying	  *
*	the packet until after you've loaded the first command (or when you	  *
*	exit).  This helps avoid disk "gronking" with the Run command.		  *
*	(Note: this is different from what you do for CliInitNewcli().)		  *
*																		  *
*	For CliInitNewcli():												  *
*																		  *
*	If Bit 31 is 0, then you must check IoErr() to determine if an error  *
*	occurred.  If IoErr() returns a pointer to your process, there has	  *
*	been an error, and you should clean up and exit.  The packet will	  *
*	have already been returned by CliInitNewcli().  If it isn't a pointer *
*	to your process and Bit 31 is 0, reply the packet immediately.		  *
*	(Note: this is different from what you do for CliInitRun().)		  *
*																		  *
*	If Bit 31 is 1, then if Bit 3 is one, ReplyPkt() the packet			  *
*	immediately (Asynch System()), otherwise wait until your shell exits  *
*	(Sync System(), Execute()).											  *
*	(Note: this is different from what you do for CliInitNewcli().)		  *
*																		  *
\*************************************************************************/

#define SYS_MASK	0x80000004L	/* indicates a system call */
								/* also known as -4 (RUN_SYSTEM) */

#include "internal.h"	/* Include internal commands */

extern struct InternalNode Internals[];
extern struct CmdCache CommandCache;

long _main(void);
long _main()
{
	if(shell_init() == RETURN_OK)
	{
		do
		{
			docommand(0, clip->cli_StandardInput);

			if((fn & SYS_MASK) == SYS_MASK)
			{
				break;  /* we are done */
			}
		} while(!we_are_done);

		history_free();
		shell_cleanup();

		if(is_not_system_call(clip, fn))
			return RETURN_OK;
		else
			return clip->cli_ReturnCode;
	}
	return RETURN_FAIL;
}

long is_not_system_call(struct CommandLineInterface *clip, long fn)
{
	if( !clip->cli_Background &&
		(clip->cli_CurrentInput == clip->cli_StandardInput) &&
		!((fn & SYS_MASK) == SYS_MASK))
	{
		return DOSTRUE;
	}
	return DOSFALSE;
}

void set_returncodes(struct CommandLineInterface *clip, long returncode, long result2)
{
	char temp[20];

	clip->cli_ReturnCode = returncode;
	clip->cli_Result2	 = result2;

	sprintf(temp, "%ld", returncode);
	SetVar("RC", temp, strlen(temp), GVF_LOCAL_ONLY);
	sprintf(temp, "%ld", result2);
	SetVar("Result2", temp, strlen(temp), GVF_LOCAL_ONLY);

	return;
}

void switch_input(BPTR new_input)
{
	BPTR tmp;

	tmp = SelectInput(new_input);
	if(tmp != clip->cli_StandardInput)
	{
		Close(tmp);
	}
	clip->cli_CurrentInput = new_input;

	return;
}

void clean_up_io()
{
	long ch;

	Flush(Output());

	ch = UnGetC(Input(), END_STREAM_CH) ? 0 : '\n';
	while((ch != '\n') && (ch != END_STREAM_CH))
		ch = FGetC(Input());

	return;
}

long shell_init()
{
	BPTR *seg_ptr;	   /* segment list for this process */
	char temp[ARGLEN];
	/*struct DPUser *user;*/

	SysBase = *((struct ExecBase **)4L);
	if(!DOSBase)
	{
		if(!(DOSBase = (struct DosLibrary *)OpenLibrary(DOSNAME, 36)))
		{
			return RETURN_FAIL;
		}
	}
	/*
		Absolute FIRST thing is to get the STARTUP packet.
		Otherwise, AmigaDOS will guru FAST! (#87000004)
	*/

	mypacket = WaitPkt();
	we_are_done = DOSFALSE;

	myproc = (struct Process *)FindTask(NULL);

	/*
		Magic stuff Randell says we must do...
	*/
	seg_ptr = (long *) BADDR(myproc->pr_SegList);

	if(!seg_ptr[4])
	{
		seg_ptr[4] = seg_ptr[3];
		seg_ptr[3] = NULL;
	}

	init_result = (mypacket->dp_Res1 == 0 ? 0:2) | (mypacket->dp_Res2 == 0 ? 0:1);

	switch(init_result)
	{
	case 0:
		fn = CliInitRun(mypacket);
		break;
	case 2:
		fn = CliInitNewcli(mypacket);
		break;
	default:
		/* other values are not currently defined */
		return RETURN_FAIL;
	}

	if(fn >= 0)
	{
		if((struct Process *)IoErr() == myproc)
		{
			/*
			   an error occurred in either processing of 'mypacket' or in
			   the initialization of the CLI, so just return fail status.
			*/
			return RETURN_FAIL;
		}
		/*
			For init_result == 0, this waits until after completion of
			the first command/script load (sortof).

			For init_result == 2, it means that the shell has initialized
			properly.
		*/
		if(init_result == 2)
		{
			ReplyPkt(mypacket, mypacket->dp_Res1, mypacket->dp_Res2);
			mypacket = NULL;
		}
	}

	if(myproc->pr_HomeDir)
	{
		PutStr("pr_HomeDir set!\n");
		UnLock(myproc->pr_HomeDir);
		myproc->pr_HomeDir = NULL;
	}
	/* because we are a Resident segment, we have no HomeDir */

	if(!(clip = Cli()))
	{
		/* NULL CLI()!!!! */
		return RETURN_FAIL;
	}
	clip->cli_FailLevel = 10;	/* default for Commodore Shell */

	if(IsInteractive(clip->cli_StandardInput))
	{
		/*
		myproc->pr_ConsoleTask =
			((struct FileHandle *)(clip->cli_StandardInput<<2))->fh_Type;
		*/
		SetConsoleTask(((struct FileHandle *)(clip->cli_StandardInput<<2))->fh_Type);
		DoPkt(myproc->pr_ConsoleTask,
				ACTION_CHANGE_SIGNAL,
				((struct FileHandle *)(clip->cli_StandardInput<<2))->fh_Arg1,
				(long)&myproc->pr_MsgPort,
				NULL,
				0, 0);
	}

	/*
		This is be the place to set up any local shell variables
	*/

	sprintf(temp, "%ld", ((struct Process *)SysBase->ThisTask)->pr_TaskNum);
	SetVar("process", temp, strlen(temp), GVF_LOCAL_ONLY);
	SetVar("version", (char *)version+6, strlen(version+6), GVF_LOCAL_ONLY);

	if(GetCurrentDirName( temp, ARGLEN ))
	{
		SetVar("cwd",temp,strlen(temp),GVF_LOCAL_ONLY);
	}

/*
	if(((struct Process *)(SysBase->ThisTask))->pr_Pad)
	{
		sprintf(temp, "Logged in as %d\n", ((struct Process *)(SysBase->ThisTask))->pr_Pad);
		PutStr(temp);
	}
	else
	{
		handlelogin();
		PutStr("Login: ");
	}
*/

#if 0
	sprintf(temp,"init_result: %d fn: %d\n", init_result, fn);
	PutStr(temp);
	Delay(100);

	/* Set wildstar-flag */
	Forbid();
	DOSBase->dl_Root->rn_Flags |= RNF_WILDSTAR;
	Permit();

	SelectInput(clip->cli_StandardInput);
	SelectOutput(clip->cli_StandardOutput);
	clip->cli_CurrentInput = clip->cli_StandardInput;
	clip->cli_CurrentOutput = clip->cli_StandardOutput;
#endif

	set_returncodes(clip, 0, 0);
	return RETURN_OK;
}

long shell_cleanup()
{
/*
	If you're exiting, we use fn to tell us what to close, etc.  First
	check if fn < 0.  If it isn't, Flush and Close cli_StandardOutput
	(if non-NULL), and Close cli_StandardInput (if non-NULL).

	If fn is < 0, Flush(Output()), then check fn.
	If fn&2 == 0, Close cli_StandardInput.
	If fn&1 == 1, Close cli_StandardOutput (this is opposite the previous flag!)
	If fn&8 == 0, ReplyPkt the initial packet, with cli_ReturnCode for
	result1 and cli_Result2 for result2 (i.e. return the result of the last
	program run if this was a synchronous System() or Execute() call).
*/
	if(fn >= 0)
	{
		if( mypacket )
		{
			ReplyPkt(mypacket, mypacket->dp_Res1, mypacket->dp_Res2);
			mypacket = NULL;
		}
		/*
			NOTE: This could be wrong.
			dp_Res2 should probably be 'clip->cli_Result2' and
			dp_Res1 should probably be 'clip->cli_ReturnCode'.
		*/

		if(clip->cli_StandardOutput)
		{
			Flush(clip->cli_StandardOutput);   /* why should we Flush()? */
			Close(clip->cli_StandardOutput);
		}
		if(clip->cli_StandardInput)
		{
			Close(clip->cli_StandardInput);
		}
	}
	else
	{
		if((fn & 2) == 0)
			if(!clip->cli_StandardInput)
				PutStr("Input() NULL\n");
		if((fn & 1) == 1)
			if(!clip->cli_StandardOutput)
				PutStr("Output() NULL\n");

		if(mypacket)
		{
			if((fn & 8) != 0)
				PutStr("Packet not returned!!\n");
		}

		Flush(Output());

		if((fn & 2) == 0)							/* User DID NOT provide Input() */
			Close(clip->cli_StandardInput);
		if((fn & 1) == 1)							/* RUN provided Output() */
			Close(clip->cli_StandardOutput);

/*		if((fn & 8) == 0)
		{
*/
			if(mypacket)
			{
				ReplyPkt(mypacket, clip->cli_ReturnCode, clip->cli_Result2);
				mypacket = NULL;
			}
/*		}
*/
	}

	if(myproc->pr_CurrentDir)
	{
		UnLock(myproc->pr_CurrentDir);
		myproc->pr_CurrentDir = NULL;
	}
	if(myproc->pr_HomeDir)
	{
		UnLock(myproc->pr_HomeDir);
		myproc->pr_HomeDir = NULL;
	}

	FreeCmdCache();

	if(DOSBase)
		CloseLibrary((struct Library *)DOSBase);

	return RETURN_OK;
}


long command_examine(char *fname, BPTR *homedir)
{
	/*
		Given a filename, attempt to determine if we can process it.
		Either by running it, or by 'executing' it (a script).

		Returns:
			0 = can RunCommand the file
			1 = can source/script/execute the file
		  < 0 = error

		homedir will be a lock to the parent directory or NULL
	*/

	struct FileInfoBlock *fib;
	long i;
	BPTR lock;
	UWORD owner, group;

	*homedir = NULL;
	if(!(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL)))
	{
		return -9;
	}

	if(!(lock = Lock(fname, ACCESS_READ)))
	{
		FreeDosObject(DOS_FIB, fib);
		return -1;
	}

	if(!Examine(lock, fib))
	{
		UnLock(lock);
		FreeDosObject(DOS_FIB, fib);
		return -2;
	}

	i = fib->fib_DirEntryType;

	if(i == ST_SOFTLINK)
	{
		/*
			Let our caller resolve the link, and if it resolves to a file,
			call us again.
		*/
		UnLock(lock);
		FreeDosObject(DOS_FIB, fib);
		return -10;
	}

	if((i != ST_FILE) && (i != ST_LINKFILE))
	{
		UnLock(lock);
		FreeDosObject(DOS_FIB, fib);
		return -3;
	}

	owner = fib->fib_OwnerUID;
	group = fib->fib_OwnerGID;
	i = fib->fib_Protection;

	/*
		The lower four bits (rwed) are stored complemented, whereas
		the top three (spa) are not.
	*/

	/* Done with the FIB */
	FreeDosObject(DOS_FIB, fib);

	i = i^0x0f;
	if((i & FIBF_SCRIPT))
	{
		UnLock(lock);	/* No homedir for scripts ?! */
		return 1;
	}

	/* TODO: check which one is the right bit to check.. */
	if((i & FIBF_EXECUTE) || (i & FIBF_GRP_EXECUTE) || (i & FIBF_OTR_EXECUTE))
	{
		*homedir = ParentDir(lock);
		UnLock(lock);
		return 0;
	}

	/*	Not an executable or a script file.	*/
	UnLock(lock);
	return -4;
}

long command_device(char *device, char *fname)
{
	/*
		For the Device specified by *device, search each element of the
		assign and try to find a command file. A command file can be
		either an executable file, or a script file (one with the script
		bit set).

		Returns:
			0 = can RunCommand this file (seglist_cmd set)
			1 = can source/script/execute this file (new_input set)
		  < 0 = error
		   -8 = Bad device name

		Note that this routine generates only one error of its own. All
		other results are passed straight thru from command_examine().

		Note also that fname may be a relative path !
		pr_HomeDir should be set to the actual file's parent directory,
		not to the (assigned) device.
	*/

	long result, 			/* from command_examine() */
		 done = 0;			/* found something we could use */
	struct DevProc *dp = NULL;/* for searching multi-assigned paths */
	struct MsgPort *fstask;	/* for GetFileSysTask() */
	BPTR lock, 				/* on source directory */
		 dir_lock;			/* save current directory */

	fstask = GetFileSysTask();
	do
	{
		dp = GetDeviceProc(device, dp);
		if(dp)
		{
			SetFileSysTask(dp->dvp_Port);

			lock = NULL;
			dir_lock = NULL;
			if(dp->dvp_Lock)
			{
				dir_lock = CurrentDir(dp->dvp_Lock);
			}
			else
			{
				lock = Lock(device, ACCESS_READ);
				if(lock)
				{
					dir_lock = CurrentDir(lock);
				}
			}
			if(dir_lock)
			{
				BPTR homedir;

				result = command_examine(fname, &homedir);
				/*
					NOTE:	if result <= -2, what should we do? This means
							that command_examine() actually found a file by
							the correct name, but it was unsuitable for
							some reason.

					Currently, we continue to search, hoping we can find
					another file with the same name and the right flags.
				*/
				if(result == 1)
				{
					/* It is a script! */

					new_input = Open(fname, MODE_OLDFILE);
					if(new_input == NULL)
					{
						return -5;
					}
					done = 1;	/* for scripts, do not change the homedir */

					if(homedir)
					{
						UnLock(homedir);
					}
				}
				else if(result == 0)
				{
					/* It is an executable! */

					/*	LoadSeg the sucker. At Least try.. */

					seglist_cmd = NewLoadSegTags(fname, TAG_DONE, 0);
					if(seglist_cmd)
					{
						done = 1;

						if(homedir)
						{
							myproc->pr_HomeDir = homedir;
						}
						else
						{
							/* Use the device lock if nothing else works.. */
							if(lock)
							{
								myproc->pr_HomeDir = lock;
								lock = NULL;
							}
							else
							{
								myproc->pr_HomeDir = DupLock(dp->dvp_Lock);
							}
						}
					}
					/*
						Probably a 'bad' file (i.e., not actually an executable).
						NewLoadSeg() failed
					*/
				}
				/* Else continue to search */
				if(lock)
				{
					UnLock(lock);
					/*lock = NULL;*/
				}
				CurrentDir(dir_lock);
			}
		}
	}
	while(!done && dp && (dp->dvp_Flags & DVPF_ASSIGN) && (IoErr() == ERROR_OBJECT_NOT_FOUND));

	SetFileSysTask(fstask);

	if(dp)
		FreeDeviceProc(dp);

	if(!done && result >= 0)
	{
		/*
			Can happen when GetDeviceProc returns a NULL dp on the first go.
		*/
		result = -8;
	}
	return result;
}

long command_processor(long abs_cmd, char *device, char *fname)
{
	/*	Handles finding the file and getting it set up properly.

		Results:
			Those returned by command_examine() and command_device().

			0 = can RunCommand (seglist_cmd set)
			1 = can source/script/execute (new_input set)
		  < 0 = error
	*/

	struct FileLock *fl;/* to parse the PATH */
	long result, 		/* from command_examine() or command_device() */
		 sub_cmd;		/* command is in a sub-directory */
	BPTR plock;			/* points to parent directory of fname */
	char buf[256];		/* where to put elements of the Cli Path */

	sub_cmd = (abs_cmd & 2); /* a '/' was found in the input filename */
	abs_cmd &=  1;			 /* so it only has one meaning: a ':' was found */

	if(*device == '\0')
		abs_cmd = 0;		/* handle special case of empty device */

	/*	So, we follow this order of search:

			1) CLI Path, in order
			2) C:  (if in Path, it gets searched twice!!)
			3) Current Directory (last because of security)

		If abs_cmd is set, then we only search the Device in *device.
		We must assume that it can be multi-assigned. The setting of
		sub_cmd is meaningless in this case. We DO NOT search the PATH
		for an absolute path.

		If sub_cmd is set, and abs_cmd ISN'T, then we have the case
		that *fname is relative to the current directory. We DO NOT
		search the PATH for a relative path.

		We could simply call command_examine(), but when we do it
		this way, we find the 'proper' executable file even if there
		are files with the same name before it in the multiassign.
	*/

	if(abs_cmd)
	{
		return command_device(device, fname);
	}
	if(sub_cmd)
	{
		result = command_examine(fname, &plock);
		if(result < 0)
			return result;

		if(result == 1)
		{
			/* It is a script! */

			new_input = Open(fname, MODE_OLDFILE);
			if(new_input)
			{
				/* for scripts, do not change the homedir */
				return result;
			}
			return -5;
		}
		else
		{
			/* It is an executable! */
			/*	LoadSeg the sucker. At Least try.. */

			seglist_cmd = NewLoadSegTags(fname, TAG_DONE, 0);
			if(seglist_cmd)
			{
				myproc->pr_HomeDir = plock;
				return result;
			}
			/*
				Probably a 'bad' file (i.e., not actually an executable).
				NewLoadSeg() failed
			*/
			UnLock(plock);
			return -6;
		}
	}
	/*
		Now, search the Cli Path.
	*/
	/* Forbid(); We assume that no-one else is messing with OUR path */
	fl = BADDR(clip->cli_CommandDir);

	while(fl)
	{
		if(fl->fl_Key)
		{
/* TODO: recode this to use CurrentDir() ? */
			if(NameFromLock(fl->fl_Key, buf, 256))
			{
				result = command_device(buf, fname);
				if(result >=  0)
				{
					/* Permit(); */
					return result;
				}
			}
		}
		fl = BADDR(fl->fl_Link);
	}
	/* Permit(); */

	/*
		search 'C:'
	*/
	result = command_device("C:", fname);
	if(result >= 0)
		return result;

	/*
		Lastly, current directory. (This is actually the same as the
		sub_cmd case, without any '/'s.)
	*/
	result = command_examine(fname, &plock);
	if(result >=  0)
	{
		if(result == 1)
		{
			/* It is a script! */

			new_input = Open(fname, MODE_OLDFILE);
			if(new_input)
			{
				return result;
			}

			/* for scripts, do not change the homedir */
			return -5;
		}
		else
		{
			/* It is an executable! (execute flag set) */
			/*	LoadSeg the sucker. At Least try.. */

			seglist_cmd = NewLoadSegTags(fname, TAG_DONE, 0);
			if(seglist_cmd)
			{
				myproc->pr_HomeDir = plock;
				return result;
			}
			UnLock(plock);

			/*
				Probably a 'bad' file (i.e., not actually an executable).
				NewLoadSeg() failed
			*/
			return -6;
		}
	}
	return result;
}

long docommand(int level, BPTR console)
{
	/* Stack consumption: 194 bytes */
	BPTR handle,
		 redirin, redirout,	/* handles for redirection parsing */
		 old_inp_fh,		/* old input filehandle */
		 old_out_fh;		/* old output filehandle */
	UBYTE device[32],		/* the device named specified, if any */
		  *cmd,				/* the input command line */
		  *arg_buf,			/* contains command arguments */
		  *p, *q;			/* used for parsing 'cmd' */
	long abs_cmd,			/* got a command with absolute pathname */
		 arg_len,			/* length of arguments to command */
		 rslt,				/* for munging of various results */
		 l,					/* trash variable */
		 len;				/* length of input line */

	int  buffersize = 5*ARGLEN+ARGBUF;
	char **args = (char **)AllocMem((ARGMAX+1)*sizeof(char *), MEMF_CLEAR);
	char *buffer = (char *)AllocMem(buffersize+2*CMDLINELEN, MEMF_CLEAR);
	long parsestatus = 0, oldparsestatus, argc;
	char temp[60], *inFile, *outFile;
	int  tasknum, thisinvok = -1, oldinvok;
	static int loopcount = 0, mode_if=0, level_if=0;

	/* TODO: change the IF-handling to skip?? */

	if(!args || !buffer)
	{
		PutStr(MSG_MEM_ALLOC_ERROR_STR);
		return RETURN_WARN;
	}
	cmd = buffer + buffersize;
	arg_buf = buffer + buffersize + CMDLINELEN;

	/* determine the cli number of this shell */
	if(myproc->pr_Task.tc_Node.ln_Type == NT_PROCESS)
	{
		tasknum = myproc->pr_TaskNum;
	}
	else
	{
		tasknum = 239;	/* strange, we are not a cli-process.. */
	}

	old_inp_fh = SelectInput(clip->cli_CurrentInput);
	old_out_fh = SelectOutput(clip->cli_StandardOutput);

top:
	if(clip->cli_CurrentInput !=  clip->cli_StandardInput)
	{
		/*	We are in a script file, check for errors.	*/
		if(CheckSignal(SIGBREAKF_CTRL_D))
		{
			/* We got a ctrl-D. Return from ALL script files */
			PutStr("*** BREAK - CTRL-D\n");

			switch_input(pop_stream(clip->cli_StandardInput));
			if(buffer)	FreeMem(buffer, buffersize+2*CMDLINELEN);
			if(args)	FreeMem(args, (ARGMAX+1)*sizeof(char *));
			return RETURN_FAIL;
		}
		if(clip->cli_ReturnCode >= clip->cli_FailLevel)
		{
			/* The last command failed. (IT should print an error message.) */

			switch_input(pop_stream(clip->cli_StandardInput));
			if(buffer)	FreeMem(buffer, buffersize+2*CMDLINELEN);
			if(args)	FreeMem(args, (ARGMAX+1)*sizeof(char *));
			return RETURN_FAIL;
		}
	}

	we_are_done = DOSFALSE;	/* establish the initial command state */
	seglist_cmd = NULL;
	seg_cmd = NULL;
	rslt = RETURN_OK;

	/*
		Programmer's note: Do NOT do a set_returncodes() before a command.
		You will invalidate the history mechanism (ie, 'why' will not work,
		and neither will script error-checking).
	*/

	if(getcommand(cmd, CMDLINELEN))
	{
		if(level_if || mode_if)
		{
			level_if = mode_if = 0;
		}
		/*
			EOF or Read error on Input()

			If this IS a System() call, then we are done.

			Otherwise, we may have been processing either file input
			(Execute()), or console input (shell functions), OR the input
			from a script file.

			If cli_CurrentInput ==  cli_StandardInput then it is one of the
			first two, and we are done.

			For the third case, we need to switch from the script file,
			to the console file. This happens when processing a Shell-Startup
			file, when executing a file that has the 's'cript bit sit, or
			doing an 'execute' shell command. The shell command differs
			from the 'C' Execute() in that the 'C' routine starts up a new
			shell, but the shell command does NOT. It runs on the current
			process.
		*/

		if(clip->cli_CurrentInput == clip->cli_StandardInput)
		{
			we_are_done = DOSTRUE;
		}
		switch_input(pop_stream(clip->cli_StandardInput));
		if(buffer)	FreeMem(buffer, buffersize+2*CMDLINELEN);
		if(args)	FreeMem(args, (ARGMAX+1)*sizeof(char *));
		return RETURN_OK;
	}

	l = 0;
	while(cmd[l] == ' ' || cmd[l] == '\t' || cmd[l] == '\n' || cmd[l] == '\r')
		l++;
	if(!cmd[l] || cmd[l] == '#')	/* Comment mark or empty line */
		goto top;


parseagain:
	oldparsestatus = parsestatus;
	oldinvok  = thisinvok;
	thisinvok = loopcount;
	loopcount = (loopcount+1)%50;

	l = 0;
	while(process_alias(cmd) && l++ < 10);
	if(l >= 10)
	{
		PutStr("Probable alias loop, check aliases\n");
		/* goto top; */
	}

	if((oldparsestatus & PARSE_PIPED))
	{
		sprintf(temp, "T:axsh-%ld-%ld", tasknum, oldinvok);
		parsestatus = parsecommand(console, cmd, arg_buf, args, ARGMAX, buffer, buffersize, &inFile, &outFile, temp);
	}
	else
	{
		parsestatus = parsecommand(console, cmd, arg_buf, args, ARGMAX, buffer, buffersize, &inFile, &outFile, NULL);
	}

	if((parsestatus & PARSE_ERROR))
	{
		goto top;	/* Parse error */
	}
	argc = (parsestatus & PARSE_ARGC_MASK);

	cmd_state = STATE_INIT;
	len = strlen(args[0]);

	/*
		redirin and redirout must be set to NULL
		to avoid confusion at the cleanup part.
	*/

	redirin = NULL;
	redirout = NULL;

	if(!len)
	{
		cmd_state = STATE_ERROR;

		goto cleanup;			/* empty command? how did it happen? */
	}

	if(!stricmp(args[0], "else") ||
		!stricmp(args[0], "endif") ||
		!stricmp(args[0], "if")
		)
	{
		if(!stricmp(args[0],"else"))
		{
			if(mode_if==1)
			{
				mode_if = -1;
			}
			else
			{
				mode_if = 1;
			}
			goto cleanup;
		}
		if(!stricmp(args[0],"endif"))
		{
			if(level_if)
				level_if--;
			goto cleanup;
		}
		if(!stricmp("if", args[0]))
		{
			rslt = DOif(argc, args);
			level_if++;
			if(rslt == -1)
				mode_if = -1;
			if(rslt == 0)
				mode_if = 1;

			goto cleanup;
		}
	}

	arg_len = strlen(arg_buf);  /* length of argument string (>= 1) */

	/*
		This should actually be after the if-level-check to be
		'absolutely' correct, but I like it that you can escape
		the shell anytime..
	 */
	if( !stricmp("exit", args[0]) ||
		!stricmp("endcli", args[0]) ||
		!stricmp("endshell", args[0]) ||
		!stricmp("quit", args[0]))
	{
		if(stricmp(args[0], "quit"))
			we_are_done = DOSTRUE;

		set_returncodes(clip, 0, 0);
		switch_input(pop_stream(clip->cli_StandardInput));
		if(buffer)
			FreeMem(buffer, buffersize+2*CMDLINELEN);
		if(args)
			FreeMem(args, (ARGMAX+1)*sizeof(char *));
		return RETURN_OK;
	}

	if(level_if && mode_if == 1)
	{
		/* If clause - non-executable part */

		goto cleanup;
	}
	else
	{
		if(inFile)
		{
			if(!(redirin = Open(inFile, MODE_OLDFILE)))
			{
				PutStr(MSG_CANNOT_OPEN_STR);
				PutStr(inFile);
				PutStr("\n");
				goto top;
			}
		}
		if(outFile)
		{
			if((parsestatus & PARSE_PIPED))
			{
				PutStr("Output redirection dropped because of piping\n");
				outFile = NULL;
			}
			else if((parsestatus & REDIR_INOUT))
			{
				APTR oldconsoletask = ((struct Process *)FindTask(NULL))->pr_ConsoleTask;

				((struct Process *)FindTask(NULL))->pr_ConsoleTask =
					((struct FileHandle *)(redirin<<2))->fh_Type;

				if(redirout = Open("*", MODE_OLDFILE))
				{
					((struct Process *)FindTask(NULL))->pr_ConsoleTask = oldconsoletask;
				}
				else
				{
					((struct Process *)FindTask(NULL))->pr_ConsoleTask = oldconsoletask;
					PutStr(MSG_CANNOT_CREATE_REDIRECTION_STR);
					PutStr(outFile);
					PutStr("\n");
				}
			}
			else if((parsestatus & REDIR_APPEND))
			{
				if(redirout = Open(outFile, MODE_READWRITE))
				{
					Seek(redirout, 0, OFFSET_END);	/* goto the eof */
				}
				else
				{
					if(!(redirout = Open(outFile, MODE_NEWFILE)))
					{
						PutStr(MSG_CANNOT_CREATE_REDIRECTION_STR);
						PutStr(outFile);
						PutStr("\n");
					}
				}
			}
			else
			{
				if(!(redirout = Open(outFile, MODE_NEWFILE)))
				{
					PutStr(MSG_CANNOT_OPEN_STR);
					PutStr(outFile);
					PutStr("\n");
				}
			}
		}

		if(!stricmp("run", args[0]))
		{
			if(argc == 2)
			{
				strcpy(arg_buf, args[1]);
			}
			if((parsestatus & PARSE_PIPED) ||
				(oldparsestatus & PARSE_PIPED))
			{
				PutStr("Can't RUN a piped command. Have the piping in the command.\n");
				parsestatus &= ~PARSE_MULTI;
				goto cleanup;
			}

			/*
				OK,OK, "*" was a bad idea as an input device, it messed up
				the window reports and everything..!
			*/
			if(redirin || (redirin = Open("NIL:", MODE_OLDFILE)))
			{
				if(!redirout)
				{
					/*
						If output is not redirected, direct it to the same
						shell window we are running in.
					*/
					redirout = Open("*", MODE_OLDFILE);
				}
			}

			if(redirin && redirout)
			{
				SystemTags(arg_buf,
							SYS_CustomShell, "AXsh",
							SYS_Input,      redirin,
							SYS_Output,     redirout,
							SYS_Asynch,      1,
							TAG_DONE,        0);

				/* Async System() will close the handles itself */
				redirin  = NULL;
				redirout = NULL;
				goto cleanup;
			}

			PutStr(arg_buf);
			PutStr("\n");
			goto cleanup;
		}

		if((parsestatus & PARSE_PIPED))
		{
			sprintf(temp, "T:axsh-%ld-%ld", tasknum, thisinvok);
			if(redirout)
			{
				PutStr("Piped output of command. Output redirection dropped!\n");
				Close(redirout);
			}
			redirout = Open(temp, MODE_NEWFILE);
		}

		if((oldparsestatus & PARSE_PIPED) && !(parsestatus & REDIR_PIPE))
		{
			sprintf(temp, "T:axsh-%ld-%ld", tasknum, oldinvok);
			if(redirin)
			{
				PutStr("Piped input to command. Input redirection dropped!\n");
				Close(redirin);
			}
			redirin = Open(temp, MODE_OLDFILE);
		}

		if(redirin)
		{
			old_inp_fh = SelectInput(redirin);
			clip->cli_CurrentInput = redirin;
		}
		if(redirout)
		{
			old_out_fh = SelectOutput(redirout);
			clip->cli_CurrentOutput = redirout;
		}
	}

	if(!stricmp("execute", args[0]+strlen(args[0])-7))
	{
		/* We have to execute a script file. */

		if(argc > 1)
		{
			if(redirin)
			{
				FPuts(clip->cli_StandardOutput, "Sorry, Execute input can't be redirected\n");
				SelectInput(old_inp_fh);
				clip->cli_CurrentInput = old_inp_fh;
				Close(redirin);
				redirin = NULL;
			}
			if(redirout)
			{
				FPuts(clip->cli_StandardOutput, "Sorry, Execute output can't be redirected\n");
				SelectOutput(old_out_fh);
				clip->cli_CurrentOutput = old_out_fh;
				Close(redirout);
				redirout = NULL;
			}

			if(handle = Open(args[1], MODE_OLDFILE))
			{
				clean_up_io();

				/*	Now, set up the new input file.	*/
				push_stream(clip->cli_CurrentInput);
				SelectInput(handle);
				clip->cli_CurrentInput = handle;
				handle = NULL;

				clip->cli_FailLevel = 10;   /* default for Cmdre-shell */
				clip->cli_ReturnCode = 0;	/* Clear the old return code */

				if((init_result == 0)  && /* Run()/Execute()/System() */
					(fn & 8)		 && /* set if ASynch */
					mypacket)			/* SHOULD be != 0 */
				{
					ReplyPkt(mypacket, mypacket->dp_Res1, mypacket->dp_Res2);
					mypacket = NULL;
				}

				rslt = docommand(level+1, console);

				set_returncodes(clip, rslt, IoErr());
				cmd_state = STATE_INIT;
				goto cleanup;
			}
			else
			{
				FPuts(clip->cli_StandardOutput, "Execute: Could not open ");
				FPuts(clip->cli_StandardOutput, args[1]);
				FPuts(clip->cli_StandardOutput, "\n");
				set_returncodes(clip, RETURN_FAIL, ERROR_OBJECT_NOT_FOUND);
				goto cleanup;
			}
		}
		else
		{
			/* execute without filename */
			if(redirin)
			{
				SelectOutput(old_inp_fh);
				clip->cli_CurrentInput = old_inp_fh;
				if(redirout)
				{
					FPuts(clip->cli_StandardOutput, "Sorry, Execute output can't be redirected\n");
					SelectOutput(old_out_fh);
					clip->cli_CurrentOutput = old_out_fh;
					Close(redirout);
					redirout = NULL;
				}
				clean_up_io();

				/*	Now, set up the new input file.	*/
				push_stream(clip->cli_CurrentInput);
				SelectInput(redirin);
				clip->cli_CurrentInput = redirin;
				redirin = NULL;

				clip->cli_FailLevel = 10;   /* default for Cmdre-shell */
				clip->cli_ReturnCode = 0;	/* Clear the old return code */

				rslt = docommand(level+1, console);

				set_returncodes(clip, rslt, IoErr());
				cmd_state = STATE_INIT;
				goto cleanup;
			}
			FPuts(clip->cli_StandardOutput, "Execute: Needs a filename or redirected input\n");
			set_returncodes(clip, RETURN_FAIL, ERROR_OBJECT_NOT_FOUND);
			goto cleanup;
		}
	}

	/*
		If a ':' or a '/' is encountered, the cmd_state is set to
		STATE_COMMAND, since the command cannot be either a resident
		or an internal command.

		When a ':' is found, abs_cmd |=  1, to indicate that an absolute
		filename was found, and that the path doesn't have to be searched.

		When a '/' is found, abs_cmd |=  2, to indicate that a relative
		path filename was found. So, if(abs_cmd & 1) ==  0, the filename
		is relative to the current directory, and again, the path doesn't
		have to be searched.
	*/

	abs_cmd = 0;
	l = 0;
	while(args[0][l])
	{
		if(args[0][l] == ':')
			abs_cmd |=  1;
		else if(args[0][l] == '/')
			abs_cmd |=  2;
		l++;
	}

	if(abs_cmd)
	{
		cmd_state = STATE_COMMAND;
	}

	/*	Start the actual command processing */

	if((cmd_state == STATE_INIT) && !abs_cmd)
	{
		/*	Search my internal list for the command. */

		for(l = 0; Internals[l].name; l++)
		{
			if(Internals[l].code)
			{
				if(stricmp(args[0], Internals[l].name) ==  0)
				{
					cmd_state = STATE_INTERNAL;

					if( mypacket &&
						(init_result == 0) &&	/* Run()/Execute()/System() */
						(fn & 8))				/* Async */
					{
						ReplyPkt(mypacket, mypacket->dp_Res1, mypacket->dp_Res2);
						mypacket = NULL;
					}

					if(!SetProgramName(args[0]))
					{
						SetProgramName(Shell_Name);
					}

					rslt = Internals[l].code(argc, args);

					set_returncodes(clip, rslt, IoErr());
					goto cleanup;		/*	return rslt; */
				}
			}
		}
	}

	if((cmd_state == STATE_INIT) && !abs_cmd)
	{
		/*	Search the Resident list for the command.
			Look at the regular list, and then the standard shell internal list.
		*/
		Forbid();
		if(!(seg_cmd = FindSegment(args[0], NULL, 0)))
			seg_cmd = FindSegment(args[0], NULL, CMD_INTERNAL);
		if(seg_cmd && seg_cmd->seg_UC != CMD_SYSTEM && seg_cmd->seg_UC > CMD_DISABLED)
		{
			if(seg_cmd->seg_UC >= 0)
				seg_cmd->seg_UC++;
			seglist_cmd = seg_cmd->seg_Seg; /* BPTR to actual seglist */
			cmd_state = STATE_RESIDENT;
		}
		Permit();
	}

	if((cmd_state == STATE_INIT) || (cmd_state == STATE_COMMAND))
	{
		/*	OK, search for the command on a device.	*/
		p = device;
		*p =  '\0';
		q = args[0];
		if((abs_cmd & 1))
		{
			while(*q != ':')
			{
				*p++ = *q++;
			}
			q++;
			*p++ = ':';
			*p = '\0';
		}

		rslt = command_processor(abs_cmd, device, q);

		/* If we did not find an executable program or a script */
		/* Check if there is a directory with the right name */
		if(rslt<0)
		{
			long lock;
			struct FileInfoBlock *fib;

			if(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL))
			{
				if(lock = Lock(args[0], ACCESS_READ))
				{
					if(Examine(lock, fib))
					{
						UnLock(lock);
						if(fib->fib_DirEntryType > 0)
						{
							FreeDosObject(DOS_FIB, fib);

							for(l=argc; l>=0; l--)
							{
								args[l+1] = args[l];
							}
							args[0] = "cd";
							argc++;

							cmd_state = STATE_INTERNAL;
							rslt = DOcd(argc, args);

							set_returncodes(clip, rslt, IoErr());
							goto cleanup;		/*	return rslt; */
						}
					}
					else
					{
						UnLock(lock);
					}
				}
				FreeDosObject(DOS_FIB, fib);
			}

			/*
				A lot of work was done to satisfy the user, but we couldn't
				find the program the user wanted to run.
			 */
			PutStr(MSG_COMMAND_NOT_FOUND_STR);
			PutStr(args[0]);
			PutStr("\n");

			cmd_state = STATE_ERROR;

			set_returncodes(clip, RETURN_FAIL, ERROR_OBJECT_NOT_FOUND);
			if((init_result == 0)  && /* Run()/Execute()/System() */
				(fn & 8)		 && /* set if ASynch */
				mypacket)			/* SHOULD be != 0 */
			{
				ReplyPkt(mypacket, mypacket->dp_Res1, mypacket->dp_Res2);
				mypacket = NULL;
			}
			goto cleanup;		/*	return RETURN_FAIL; */
		}
		cmd_state = STATE_COMMAND;
	}

	/*	For asynch calls, return the packet at this point (so AmigaDOS can
		return to the caller).

		We do assume to have EVERYTHING taken care of as far as running
		the program is concerned (ie, we don't need to tell the user
		"Object Not Found" or whatever).
	*/

	if((init_result == 0)  && /* Run()/Execute()/System() */
		(fn & 8)		 && /* set if ASynch */
		mypacket)			/* SHOULD be != 0 */
	{
		ReplyPkt(mypacket, mypacket->dp_Res1, mypacket->dp_Res2);
		mypacket = NULL;
	}

	/*	Clear all CTRL_x flags	*/
	CheckSignal(SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D | SIGBREAKF_CTRL_E | SIGBREAKF_CTRL_F );

	if((rslt == 1) && (cmd_state == STATE_COMMAND))
	{
		/*
			We have to execute a script file.
			Start by cleaning up the I/O status (just like command termination)
			The rest is identical to "execute file"
		*/

		clean_up_io();

		if(redirin)
		{
			FPuts(clip->cli_StandardOutput, "Sorry, Execute input can't be redirected\n");
			SelectInput(old_inp_fh);
			clip->cli_CurrentInput = old_inp_fh;
			Close(redirin);
			redirin = NULL;
		}
		if(redirout)
		{
			FPuts(clip->cli_StandardOutput, "Sorry, Execute output can't be redirected\n");
			SelectOutput(old_out_fh);
			clip->cli_CurrentOutput = old_out_fh;
			Close(redirout);
			redirout = NULL;
		}

		/*	Now, set up the new input file.	*/
		push_stream(clip->cli_CurrentInput);
		SelectInput(new_input);
		clip->cli_CurrentInput = new_input;
		new_input = NULL;

		clip->cli_FailLevel = 10;   /* default for Cmdre-shell */
		clip->cli_ReturnCode = 0;	/* Clear the old return code */

		/*
			Since cli_StandardInput !=  cli_CurrentInput, I will know to
			Close() on the input file when I get EOF, and then switch back
			to cli_StandardInput.  See the comment after the FGets() toward
			the top of this procedure. This is why I NULL new_input.
		*/

		set_returncodes(clip, docommand(level+1, console), IoErr());
		cmd_state = STATE_COMMAND;
		goto cleanup;
	}

	/*
		Clear/Set appropriate indicators
	*/
	SetIoErr(0);

	if(!SetProgramName(args[0]))
	{
		SetProgramName(Shell_Name);
	}

	/*
		Determine the required StackSize. Provide a MINIMUM of 4000 bytes.
		The parameter to RunCommand is in bytes, whereas the field in Cli()
		is in longwords (32-bit).
	*/
	if(clip->cli_DefaultStack < 1000)
	{
		clip->cli_DefaultStack = 1000;	/* in longwords */
	}

	/*	Run the command!

		Some Commodore commands REQUIRE that the arg_buf array point to
		available space, even when there are no arguments (e.g., C:List).
		Most don't. This is, however, why arg_buf exists, instead of a
		simple pointer being used instead.
	*/

	clip->cli_Module = seglist_cmd;
	rslt = RunCommand(seglist_cmd, (clip->cli_DefaultStack * 4), arg_buf, arg_len);

	if((rslt == -1) && (IoErr() == ERROR_NO_FREE_STORE))
	{
		set_returncodes(clip, RETURN_FAIL, ERROR_NO_FREE_STORE);
		goto cleanup;	/*	return RETURN_FAIL; */
	}

	set_returncodes(clip, rslt, IoErr());
	/*	return RETURN_OK; */

cleanup:
	if((init_result == 0)  && /* Run()/Execute()/System() */
		(fn & 8)		 && /* set if ASynch */
		mypacket)			/* SHOULD be != 0 */
	{
		ReplyPkt(mypacket, mypacket->dp_Res1, mypacket->dp_Res2);
		mypacket = NULL;
	}

	SetProgramName(Shell_Name);
	switch(cmd_state)
	{
	case STATE_RESIDENT:
		clip->cli_Module = NULL;

		Forbid();
		if(seg_cmd->seg_UC > 0)
			seg_cmd->seg_UC--;
		Permit();
		seglist_cmd = NULL;
		seg_cmd = NULL;

		break;
	case STATE_COMMAND:
		if(clip->cli_Module)
		{
			UnLoadSeg(clip->cli_Module);
			clip->cli_Module = NULL;
		}
		seglist_cmd = NULL;

		if(myproc->pr_HomeDir)
		{
			UnLock(myproc->pr_HomeDir);
			myproc->pr_HomeDir = NULL;
		}
		break;
	case STATE_INTERNAL:
		/*	Internal commands are responsible for their own cleanup	*/
		break;

	case STATE_ERROR:
		/*	Oh, well. Shouldn't be any cleanup to do.	*/
		break;

	default:
		/*
			It is possible (due to aliasing) for a STATE_INIT to
			arrive here.
		*/
		break;
	}	/* end of switch(cmd_state) */

	if(redirin)
	{
		SelectInput(old_inp_fh);
		clip->cli_CurrentInput = old_inp_fh;
		Close(redirin);
		redirin = NULL;
	}
	if(redirout)
	{
		SelectOutput(old_out_fh);
		clip->cli_CurrentOutput = old_out_fh;
		Close(redirout);
		redirout = NULL;
	}
	/*
		NOTE: If we opened re-direction files, close them here.
		Simulate pipes here.
	*/

	if((oldparsestatus & PARSE_PIPED))
	{
		sprintf(temp, "T:axsh-%ld-%ld", tasknum, oldinvok);
		DeleteFile(temp);
	}

	/*	Get Output() flushed, and Input() empty...	*/
	clean_up_io();

	/*
		If there were multiple commands on the line (or piped command),
		call parseline() again with the rest of the line
	*/
	if((parsestatus & PARSE_MULTI))
	{
		if(clip->cli_ReturnCode >= clip->cli_FailLevel)
			goto top;

		goto parseagain;
	}

	if(we_are_done ||
		(((fn & SYS_MASK) == SYS_MASK) && !level))
	{
	/*
		sprintf(temp,"we_are_done: %d, fn: 0x%x, level: %d\n", we_are_done, (fn & SYS_MASK), level);
		PutStr(temp);
	*/
	}
	else
	{
		goto top;
	}

	/*	-- reset the I/O pointers	WHY !?! */
	/*old_inp_fh =*/ SelectInput(clip->cli_CurrentInput);
	/*old_out_fh =*/ SelectOutput(clip->cli_StandardOutput);

	if(buffer)	FreeMem(buffer, buffersize+2*CMDLINELEN);
	if(args)	FreeMem(args, (ARGMAX+1)*sizeof(char *));

	return RETURN_OK;
}

long getcommand(char *line, long size)
{
	char temp[100], *prompt = NULL;
	int flag = IsInteractive(Input());

	Flush(Input());
	Flush(Output());

	/* if(is_not_system_call(clip, fn) && clip->cli_CurrentInput == clip->cli_StandardInput) */
	if(flag)
	{
		PutStr("\017");	/* Back to normal charset mode */
		if(GetVar("prompt", temp, sizeof(temp), GVF_LOCAL_ONLY) != -1)
		{
			prompt = process_prompt(temp);
		}
		else if(GetPrompt(temp, sizeof(temp)))
		{
			prompt = process_prompt(temp);
		}
		if(!prompt)
			prompt = temp;

		if(prompt && *prompt)
			FPuts(Output(), prompt);
		else
			FPuts(Output(), "> ");
		Flush(Output());
	}

	if(GetVar("lineedit", line, size, GVF_LOCAL_ONLY) == -1 || !flag)
	{
		if(FGets(Input(), line, size) == 0)
		{
			/*	EOF or Read error on Input() */
			return -1;
		}
	}
	else
	{
		if(getline(Input(), prompt, line, size, 1) < 0)
		{
			/*	EOF or Read error on Input() */
			return -1;
		}
	}
	return 0;
}
