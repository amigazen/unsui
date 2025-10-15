#include <dos.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <exec/execbase.h>
#include <libraries/dosextens.h>
#include <proto/exec.h>
#include <proto/dos.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "AXsh.h"
#include "internal.h"

#define HASH_BUFINITIAL	2048	/* Buffer size at first allocation */
#define HASH_NUMINITIAL	256		/* Name pointers at first allocation */
#define HASH_BUFDELTA	1024	/* Buffer growth at overflow */
#define HASH_NUMDELTA	128		/* Name pointer increase at overflow */

#define MSG_CD_USAGE_STR	"Usage: cd [<directory>]\n"
#define MSG_CHMOD_USAGE_STR "Usage: chmod [ugo][+-=][Shsparwed] <files> [ALL] [QUIET] [DIRS] [FILES]\n"
#define MSG_CHOWN_USAGE_STR "Usage: chown <uid> <files> [ALL] [QUIET] [DIRS] [FILES]\n"
#define MSG_CHGRP_USAGE_STR "Usage: chgrp <gid> <files> [ALL] [QUIET] [DIRS] [FILES]\n"
#define MSG_COPY_USAGE_STR	"Usage: copy <names> [TO] <name> [ALL] [QUIET] [CLONE] [BUF=<buffer-kB>]\n"
#define MSG_FILENOTE_USAGE_STR "Usage: filenote <files> <filenote> [ALL] [QUIET] [DIRS] [FILES]\n"
#define MSG_WHICH_USAGE_STR "Usage: which <commands>\n"
#define MSG_WHICH_ALIAS_STR "Alias\n"
#define MSG_WHICH_INTER_STR "AXsh Internal\n"
#define MSG_WHICH_SHINT_STR "AmigaShell Internal\n"
#define MSG_WHICH_RESID_STR "Resident\n"

#ifndef MSG_MEM_ALLOC_ERROR_STR
#define MSG_MEM_ALLOC_ERROR_STR "\aMemory allocation error!\n"
#define MSG_CANNOT_LOCK_OR_EXAMINE_STR "Cannot lock or examine file\n"
#endif

#ifndef FIBF_HIDDEN
#define FIBB_HIDDEN	7        /* hidden from the directory [Not by C=] */
#define FIBF_HIDDEN	(1<<FIBB_HIDDEN)
#define FIBB_SUID	31       /* SET-UID bit, compatible with MuFS */
#define FIBF_SUID	(1<<FIBB_SUID)
#endif


struct InternalNode Internals[]=
{
	{"cd",		&DOcd},
	{"chgrp",	&DOchgrp},
	{"chmod",	&DOchmod},
	{"chown",	&DOchown},
	{"echo",	&DOecho},
	{"exit",	NULL},			/* just a dummy-command for which */
	{"filenote",&DOfilenote},
	{"if",		&DOif},
	{"internal",&DOinternal},
	{"login",	NULL},			/* just a dummy-command for which */
	{"run",		NULL},			/* just a dummy-command for which */
	{"quit",	NULL},			/* just a dummy-command for which */
	{"rehash",  &DOrehash},
	{"which",   &DOwhich},
	{NULL,NULL}
};

struct CmdCache
{
	char *buffer;
	long  buffersize;
	long  bufferindex;
	long  commands;
	long  maxcommands;
	short *indices;
};

static struct CmdCache CommandCache={NULL,0,0,0,NULL};

/* Flags for ReadArgs-equivalent */
#define FLAG_ALL		(1<<0)
#define FLAG_QUIET		(1<<1)
#define FLAG_DIRS		(1<<2)
#define FLAG_FILES		(1<<3)
#define FLAG_CLONE		(1<<4)

#define MAX_FILES 256 /* Max number of files in one multiarg command */
static char *fileName[MAX_FILES];


void generateflags(char *buffer, LONG prot, LONG type, char comment);
void generateflags(char *buffer, LONG prot, LONG type, char comment)
{
	int i;

	strcpy(buffer, "dchsparwedrwedrwed");
	for(i=0; i<4; i++)
	{
		if(prot & (1<<i))
			buffer[9-i]='-';
		if(!(prot & (1L<<(i+4))))
			buffer[5-i]='-';
		if(!(prot & (1L<<(i+8))))
			buffer[13-i]='-';
		if(!(prot & (1L<<(i+12))))
			buffer[17-i]='-';
	}
	if(type==ST_FILE)
		buffer[0]='-';
	else if(type==ST_LINKFILE)
		buffer[0]='l';
	else if(type==ST_LINKDIR)
		buffer[0]='D';
	else if(type==ST_SOFTLINK)
		buffer[0]='s';

	if(!comment)
		buffer[1]='-';
}


int mygetpath(char *buffer, int bufmax, BPTR baselock)
{
    char *loc = buffer+bufmax-1; /* Last usable location */
    BPTR lock, newlock;
    struct FileInfoBlock *fib;
    int len;

	if(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL))
	{
	    *loc = '\0'; /* trailing NUL */
	    lock = baselock;
	    while(lock)
	    {
			Examine(lock, fib);
			newlock = ParentDir(lock);
			if(lock != baselock)
			    UnLock(lock);
			lock = newlock;
			len = strlen(fib->fib_FileName);

			if(bufmax < len+2) /* Take the NUL into account! */
			{
			    strcpy(buffer, loc);
				FreeDosObject(DOS_FIB, fib);
				if(lock)
					UnLock(lock);
			    return 0;
			}
			if(fib->fib_DirEntryType >= 0) /* Is it a dir ? */
			{
			    --loc;
			    --bufmax;
			    *loc = (lock)?'/':':';
			}
			loc -= len;
			strncpy(loc, fib->fib_FileName, (size_t)len);
			bufmax -= len;
	    }
	    strcpy(buffer, loc);
		FreeDosObject(DOS_FIB, fib);
	    return 1;
    }
    return 0;
}

int DOcd(int argc,char *argv[])
{
	char temp[ARGLEN], *dest;
	int i;
	long mylock, twolock;
	struct	FileInfoBlock *fib;
	struct CommandLineInterface *clip = Cli();	/* Cli() */

	if(argc > 2)
	{
		FPuts(clip->cli_StandardOutput, MSG_CD_USAGE_STR);
		return RETURN_WARN;
	}

	if(argc==2)
	{
		dest = argv[1];
/*
		strcpy(temp,argv[1]);
		i=strlen(temp)-1;
		if(i>=0 && temp[i]!='/' && temp[i]!=':')
			strcat(temp,"/");
*/
	}
	else
	{
		if(GetVar("home", temp, ARGLEN, GVF_LOCAL_ONLY) != -1)
		{
			dest = temp;
		}
		else
		{
			if(GetCurrentDirName( temp, ARGLEN ))
			{
				PutStr(temp);
				PutStr("\n");
			}
			return RETURN_OK;
		}
	}

	if(!(mylock=Lock(dest, ACCESS_READ)))	/* mylock is a local variable here */
	{
		FPuts(clip->cli_StandardOutput, "No such directory: ");
		FPuts(clip->cli_StandardOutput, dest);
		FPuts(clip->cli_StandardOutput, "\n");
		return RETURN_ERROR;
	}

	mygetpath(temp, ARGLEN, mylock);

	if(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL))
	{
		if(Examine(mylock, fib) && (fib->fib_DirEntryType >= 0))
		{
			twolock = ParentDir(mylock);
			if(	twolock /* Protections are not valid for root dirs! */ &&
				(fib->fib_Protection & FIBF_EXECUTE) /* Reversed meaning! */ &&
				!(fib->fib_Protection & FIBF_GRP_EXECUTE) &&
				!(fib->fib_Protection & FIBF_OTR_EXECUTE))
			{
				FPuts(clip->cli_StandardOutput, "No execute permission: ");
				FPuts(clip->cli_StandardOutput, dest);
				FPuts(clip->cli_StandardOutput, "\n");
				UnLock(twolock);
				UnLock(mylock);
				FreeDosObject(DOS_FIB, fib);
				return RETURN_ERROR;
			}
			if(twolock)
			{
				/*PutStr("Twolock != 0\n");*/
				UnLock(twolock);
			}
		}
		else	/* if it is not a directory... */
		{
			FPuts(clip->cli_StandardOutput, "No such directory: ");
			FPuts(clip->cli_StandardOutput, dest);
			FPuts(clip->cli_StandardOutput, "\n");
			UnLock(mylock);
			FreeDosObject(DOS_FIB, fib);
			return RETURN_ERROR;
		}
		FreeDosObject(DOS_FIB, fib);
	}
	else
	{
		UnLock(mylock);
		return RETURN_FAIL;
	}

	UnLock(CurrentDir(mylock));
	SetCurrentDirName(temp);
	SetVar("cwd", temp, strlen(temp),GVF_LOCAL_ONLY);

	return RETURN_OK;
}


#define CHMOD_BUFSIZE	1024	/* Pathsize for files */

int DOchmod(int argc,char *argv[])
{
	ULONG protection, oldProtection, bit;
	ULONG orMask=0, andMask=0xffffffff, xorMask=0;
	char *a, buffer[20];
	int mode='+', group=0, error=RETURN_OK, set=0, addMode=0, i;
	struct AnchorPath *ua;
	long temprc, flags=0, n, files=0;
	struct CommandLineInterface *clip = Cli();	/* Cli() */

	if(argc < 3)
	{
		FPuts(clip->cli_StandardOutput, MSG_CHMOD_USAGE_STR);
		return RETURN_WARN;
	}
	else
	{
		a=argv[1];
		while(*a && error==RETURN_OK)
		{
			switch(*a)
			{
			case 'u':
				group   = addMode*group | 0x0001;
				addMode = 1;
				break;
			case 'g':
				group   = addMode*group | 0x0100;
				addMode = 1;
				break;
			case 'o':
				group   = addMode*group | 0x1000;
				addMode = 1;
				break;
			case '+':
			case '-':
			case '=':
				mode = *a;
				break;
			case 'h':
				bit   = FIBF_HIDDEN;
				group = 0x0001;
				set   = -1;
				break;
			case 's':
				bit   = FIBF_SCRIPT;
				group = 0x0001;
				set   = -1;
				break;
			case 'S':
				bit   = FIBF_SUID;
				group = 0x0001;
				set   = -1;
				break;
			case 'p':
				bit   = FIBF_PURE;
				group = 0x0001;
				set   = -1;
				break;
			case 'a':
				bit   = FIBF_ARCHIVE;
				group = 0x0001;
				set   = -1;
				break;
			case 'r':
				bit   = FIBF_READ;
				set   = -1;
				break;
			case 'w':
				bit   = FIBF_WRITE;
				set   = -1;
				break;
			case 'x':
			case 'e':
				bit   = FIBF_EXECUTE;
				set   = -1;
				break;
			case 'd':
				set   = -1;
				bit   = FIBF_DELETE;
				break;
			default:
				error = RETURN_WARN;
				break;
			}
			if(*a != 'u' && *a != 'g' && *a != 'o')
				addMode = 0;

			a++;
			if(error==RETURN_OK && set)
			{
				if(!group)
					group = 0x0001;

				if(mode == '+')
				{
					orMask |= (bit*group);
				}
				else if(mode == '-')
				{
					andMask &= ~(bit*group);
				}
				else if(mode == '=')
				{
					xorMask = (bit*group);
				}
				set = 0;
			}
		}

		for(n=2; n<argc; n++)
		{
			if(!stricmp(argv[n],"ALL"))
			{
				flags |= FLAG_ALL;
			}
			else if(!stricmp(argv[n],"QUIET"))
			{
				flags |= FLAG_QUIET;
			}
			else if(!stricmp(argv[n],"DIRS"))
			{
				flags |= FLAG_DIRS;
			}
			else if(!stricmp(argv[n],"FILES"))
			{
				flags |= FLAG_FILES;
			}
			else if(!stricmp(argv[n],"FILE"))
			{
				if(n+1<argc)
				{
					n++;
					if(files<MAX_FILES)
						fileName[files++] = argv[n];
				}
			}
			else
			{
				if(files<MAX_FILES)
					fileName[files++] = argv[n];
			}
		}
		if(error!=RETURN_OK)
		{
			PutStr(MSG_CHMOD_USAGE_STR);
			return error;
		}

		/* Default is: change protections for both files and directories */
		/*             in non-recursive mode, only files in recursive mode */

		if(!(flags & (FLAG_DIRS | FLAG_FILES)))
		{
			if(flags & FLAG_ALL)
				flags |= FLAG_FILES;
			else
				flags |= FLAG_DIRS | FLAG_FILES;
		}

		if((ua = (struct AnchorPath *)AllocVec(sizeof(struct AnchorPath) + CHMOD_BUFSIZE, MEMF_PUBLIC)))
		{
			for(set = 0; set<files; set++)
			{
				memset(ua, 0, sizeof(struct AnchorPath) + CHMOD_BUFSIZE);

				ua->ap_Flags |= APF_DODOT;
				ua->ap_BreakBits = SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D;
				ua->ap_Strlen = CHMOD_BUFSIZE;

				MatchFirst(fileName[set], ua);
				temprc = IoErr();
				if( temprc == ERROR_NO_MORE_ENTRIES )
				{
					FPuts(clip->cli_StandardOutput, fileName[set]);
					FPuts(clip->cli_StandardOutput, ": no matches\n");
					/*
						return value is warn, whenever we return
					*/
					error = RETURN_WARN;
				}
				while( temprc == 0 )
				{
					if((flags & FLAG_ALL) && ua->ap_Info.fib_DirEntryType > 0)
					{
						if(!(ua->ap_Flags & APF_DIDDIR))
						{
							ua->ap_Flags |= APF_DODIR;
						}
						else
						{
							ua->ap_Flags &= ~APF_DIDDIR;
							MatchNext(ua);
							temprc = IoErr();
							continue;
						}
					}

					/* If we only want files next entry */
					if( !(flags & FLAG_DIRS) &&
						ua->ap_Info.fib_DirEntryType > 0)
					{
						MatchNext(ua);
						temprc = IoErr();
						continue;
					}

					/* If we only want directories, next entry */
					if( !(flags & FLAG_FILES) &&
						ua->ap_Info.fib_DirEntryType < 0)
					{
						MatchNext(ua);
						temprc = IoErr();
						continue;
					}

					/*
					if(ua->ap_Flags & APF_DirChanged)
					{
						PutStr(ua->ap_Buf);
						PutStr("\n");
					}
					*/

					/* invert those 4 bits so that things are not so complicated */
					oldProtection = ua->ap_Info.fib_Protection;
					protection = oldProtection ^ 0x000f;

					protection = ((protection ^ xorMask) | orMask) & andMask;

					/* invert those 4 bits back to normal */
					protection ^= 0x000f;

					if(!(flags & FLAG_QUIET))
					{
						generateflags(	buffer, protection,
										ua->ap_Info.fib_DirEntryType,
										ua->ap_Info.fib_Comment[0]);

						PutStr(buffer);
						PutStr(" ");
						PutStr(ua->ap_Buf);
						PutStr("\n");
					}

					if(protection != oldProtection)
					{
						if(SetProtection(ua->ap_Buf, protection))
						{
							temprc = IoErr();
							PrintFault(IoErr(), ua->ap_Buf);
							/*
								return value is warn, whenever we return
							*/
							error = RETURN_WARN;
						}
					}
					MatchNext(ua);
					temprc = IoErr();
				}
				if( temprc && temprc != ERROR_NO_MORE_ENTRIES )
				{
					PrintFault(temprc, fileName[set]);
					error = RETURN_ERROR;
				}
			}
			FreeVec((void *)ua);
		}
		else
		{
			FPuts(clip->cli_StandardOutput, MSG_MEM_ALLOC_ERROR_STR);
			error = RETURN_FAIL;
		}
	}
	return error;
}



int chown2(int argc,char *argv[], int mode);
/* 0 for chown, !=0 for chgrp */

int DOchown(int argc,char *argv[])
{
	return chown2(argc, argv, 0);
}

int DOchgrp(int argc,char *argv[])
{
	return chown2(argc, argv, 1);
}


#define CHOWN_BUFSIZE	1024	/* Pathsize for files */
int chown2(int argc,char *argv[], int mode)
{
	ULONG owner, oldOwner;
	struct AnchorPath *ua;
	long temprc, set, error = RETURN_OK, files=0, n, flags=0;
	struct CommandLineInterface *clip = Cli();	/* Cli() */

	if(DOSBase->dl_lib.lib_Version<37)
	{
		FPuts(clip->cli_StandardOutput, "v37 dos.library needed\n");
	}

	if(argc < 3)
	{
		if(mode)
			FPuts(clip->cli_StandardOutput, MSG_CHGRP_USAGE_STR);
		else
			FPuts(clip->cli_StandardOutput, MSG_CHOWN_USAGE_STR);
		return RETURN_WARN;
	}
	else
	{
		owner = (atol(argv[1]) & 0xffff);

		/*
			UID,FILE/A,DIRS/S,ALL/S,QUIET/S:
			chown uid file** file quiet DIRS ALL QUIET
		*/

		for(n=2; n<argc; n++)
		{
			if(!stricmp(argv[n],"ALL"))
			{
				flags |= FLAG_ALL;
			}
			else if(!stricmp(argv[n],"QUIET"))
			{
				flags |= FLAG_QUIET;
			}
			else if(!stricmp(argv[n],"DIRS"))
			{
				flags |= FLAG_DIRS;
			}
			else if(!stricmp(argv[n],"FILES"))
			{
				flags |= FLAG_FILES;
			}
			else if(!stricmp(argv[n],"FILE"))
			{
				if(n+1<argc)
				{
					n++;
					if(files < MAX_FILES)
						fileName[files++] = argv[n];
				}
			}
			else
			{
				if(files < MAX_FILES)
					fileName[files++] = argv[n];
			}
		}

		/* Default is: change owner/group for both files and directories */

		if(!(flags & (FLAG_DIRS | FLAG_FILES)))
		{
			flags |= FLAG_DIRS | FLAG_FILES;
		}

		if((ua = (struct AnchorPath *)AllocVec(sizeof(struct AnchorPath) + CHOWN_BUFSIZE, MEMF_PUBLIC)))
		{
			for(set = 0; set<files; set++)
			{
				memset(ua, 0, sizeof(struct AnchorPath) + CHOWN_BUFSIZE);

				ua->ap_Flags |= APF_DODOT;
				ua->ap_BreakBits = SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D;
				ua->ap_Strlen = CHOWN_BUFSIZE;

				MatchFirst(fileName[set], ua);
				temprc = IoErr();
				if( temprc == ERROR_NO_MORE_ENTRIES )
				{
					FPuts(clip->cli_StandardOutput, fileName[set]);
					FPuts(clip->cli_StandardOutput, ": no matches\n");
					/*
						return value is warn, whenever we return
					*/
					error = RETURN_WARN;
				}
				while( temprc == 0 )
				{
					if( (flags & FLAG_ALL) && ua->ap_Info.fib_DirEntryType > 0)
					{
						if(!(ua->ap_Flags & APF_DIDDIR))
						{
							ua->ap_Flags |= APF_DODIR;
						}
						else
						{
							ua->ap_Flags &= ~APF_DIDDIR;
							MatchNext(ua);
							temprc = IoErr();
							continue;
						}
					}

					/* If we only want files next entry */
					if( !(flags & FLAG_DIRS) &&
						ua->ap_Info.fib_DirEntryType > 0)
					{
						MatchNext(ua);
						temprc = IoErr();
						continue;
					}

					/* If we only want directories, next entry */
					if( !(flags & FLAG_FILES) &&
						ua->ap_Info.fib_DirEntryType < 0)
					{
						MatchNext(ua);
						temprc = IoErr();
						continue;
					}

					if(mode)
						oldOwner = ua->ap_Info.fib_OwnerGID;
					else
						oldOwner = ua->ap_Info.fib_OwnerUID;

					if(!(flags & FLAG_QUIET))
					{
						PutStr(ua->ap_Buf);
						if(ua->ap_Info.fib_DirEntryType > 0)
						{
							PutStr(" [DIR]\n");
						}
						else
						{
							PutStr("\n");
						}
					}

					if(owner != oldOwner)
					{
						if(mode)
							oldOwner = (ua->ap_Info.fib_OwnerUID << 16) | owner;
						else
							oldOwner = (owner << 16) | ua->ap_Info.fib_OwnerGID;

						if(SetOwner(ua->ap_Buf, oldOwner))
						{
							temprc = IoErr();
							PrintFault(IoErr(), ua->ap_Buf);
							/*
								return value is warn, whenever we return
							*/
							error = RETURN_WARN;
						}
					}
					MatchNext(ua);
					temprc = IoErr();
				}
				if( temprc && temprc != ERROR_NO_MORE_ENTRIES )
				{
					PrintFault(temprc, fileName[set]);
					error = RETURN_ERROR;
				}
			}
			FreeVec((void *)ua);
		}
		else
		{
			FPuts(clip->cli_StandardOutput, MSG_MEM_ALLOC_ERROR_STR);
			error = RETURN_FAIL;
		}
	}
	return error;
}

#if 0
/*
	TO,ALL,QUIET,BUF=BUFFER,CLONE

	1.	files+dirs	->	dir
	2.	file		->	file
	3.	dir			->	dir

	1.	copy AXsh:** temp:AXsh all clone quiet
	1.	copy ST-spelling ST-abroad Arc:texts/startrek/faq/
	2.	copy internal.c oldinternal.c
	3.	copy Temp:AXsh AXsh: all clone quiet

	copy dir1 dir2 (=> copy dir1/** dir2/dir1)
		if dir2 exists, copy contents of dir1 to dir2/dir1
		otherwise copy contents of dir1 to dir2

	copy file1 file2 dir1 file3 dir2 .. dir
		if dir exists, copy files to dir
		otherwise create dir and then copy
		Create dir1 and dir2 and copy if ALL option present

	copy file1 file2
		if file2 exists and no REPLACE, abort copy
		otherwise copy file1 to file2
*/


int DOcopy(int argc,char *argv[])
{
	struct	FileInfoBlock *fib;
	BPTR lock;
	long temprc, set, error = RETURN_OK, files=0, n, flags=0;
	char *toname = NULL;
	long buffer = 0;

	for(n=2; n<argc; n++)
	{
		if(!stricmp(argv[n],"ALL"))
		{
			flags |= FLAG_ALL;
		}
		else if(!stricmp(argv[n],"QUIET"))
		{
			flags |= FLAG_QUIET;
		}
		else if(!stricmp(argv[n],"CLONE"))
		{
			flags |= FLAG_CLONE;
		}
		else if(!stricmp(argv[n],"FILE"))
		{
			if(n+1<argc)
			{
				n++;
				if(files < MAX_FILES)
					fileName[files++] = argv[n];
			}
		}
		else if(!stricmp(argv[n],"TO"))
		{
			if(n+1<argc)
			{
				n++;
				if(!toname)
					toname = argv[n];
				else
				{
					error = RETURN_WARN;
					break;
				}
			}
		}
		else if(!strincmp(argv[n],"BUF=",4))
		{
			if(n+1<argc)
			{
				buffer = atol(argv[n]+4);
				if(buffer<0)
				{
					error = RETURN_WARN;
					break;
				}
			}
		}
		else
		{
			if(files < MAX_FILES)
				fileName[files++] = argv[n];
		}
	}
	if(error != RETURN_OK || (!toname && files<2))
	{
		/* USAGE */
		FPuts(clip->cli_StandardOutput, MSG_COPY_USAGE_STR);
		return error;
	}

	if(!toname)
	{
		toname = fileName[--files];
	}

	if(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL))
	{
		if(lock = Lock(toname, ACCESS_READ))
		{
			if(Examine(lock,fib) && (fib->fib_DirEntryType >= 0))
			{
				/* It's a directory */
			}
			UnLock(lock);
		}
		else
		{
			/* Destination dir/file does not exist */
			if(files == 1)
			{
				/* either dir->dir or file->file */
			}
			else
			{
			}
		}
		FreeDosObject(DOS_FIB, fib);
	}

	return RETURN_WARN;
}
#endif

int DOecho(int argc,char *argv[])
{
	int i, start=1, stop=argc, noline=0;

	if(argc>start && !strcmp(argv[start],"-n"))
	{
		noline=1;
		++start;
	}
	else if(argc && !stricmp(argv[argc-1],"noline"))
	{
		noline=1;
		--stop;
	}

	if(start<stop)
	{
		PutStr(argv[start]);
		for(i=start+1;i<stop;i++)
		{
			PutStr(" ");
			PutStr(argv[i]);
		}
	}
	if(!noline)
		PutStr("\n");

	return RETURN_OK;
}


int DOfilenote(int argc,char *argv[])
{
	struct AnchorPath *ua;
	long temprc, set, error = RETURN_OK, files=0, n, flags=0;
	struct CommandLineInterface *clip = Cli();	/* Cli() */

	if(argc < 3)
	{
		FPuts(clip->cli_StandardOutput, MSG_FILENOTE_USAGE_STR);
		return RETURN_WARN;
	}
	else
	{
		for(n=1; n<argc; n++)
		{
			if(!stricmp(argv[n],"ALL"))
			{
				flags |= FLAG_ALL;
			}
			else if(!stricmp(argv[n],"QUIET"))
			{
				flags |= FLAG_QUIET;
			}
			else if(!stricmp(argv[n],"DIRS"))
			{
				flags |= FLAG_DIRS;
			}
			else if(!stricmp(argv[n],"FILES"))
			{
				flags |= FLAG_FILES;
			}
			else if(!stricmp(argv[n],"FILE"))
			{
				if(n+1<argc)
				{
					n++;
					if(files < MAX_FILES)
						fileName[files++] = argv[n];
				}
			}
			else
			{
				if(files < MAX_FILES)
					fileName[files++] = argv[n];
			}
		}

		/* Default is: only files in recursive mode,
		   both files and dirs in non-recursive mode */

		if(!(flags & (FLAG_DIRS | FLAG_FILES)))
		{
			if(flags & FLAG_ALL)
				flags |= FLAG_FILES;
			else
				flags |= FLAG_FILES | FLAG_DIRS;
		}

		if((ua = (struct AnchorPath *)AllocVec(sizeof(struct AnchorPath) + CHOWN_BUFSIZE, MEMF_PUBLIC)))
		{
			for(set = 0; set<files-1; set++)
			{
				memset(ua, 0, sizeof(struct AnchorPath) + CHOWN_BUFSIZE);

				ua->ap_Flags |= APF_DODOT;
				ua->ap_BreakBits = SIGBREAKF_CTRL_C | SIGBREAKF_CTRL_D;
				ua->ap_Strlen = CHOWN_BUFSIZE;

				MatchFirst(fileName[set], ua);
				temprc = IoErr();
				if( temprc == ERROR_NO_MORE_ENTRIES )
				{
					FPuts(clip->cli_StandardOutput, fileName[set]);
					FPuts(clip->cli_StandardOutput, ": no matches\n");
					/*
						return value is warn, whenever we return
					*/
					error = RETURN_WARN;
				}
				while( temprc == 0 )
				{
					if( (flags & FLAG_ALL) && ua->ap_Info.fib_DirEntryType > 0)
					{
						if(!(ua->ap_Flags & APF_DIDDIR))
						{
							ua->ap_Flags |= APF_DODIR;
						}
						else
						{
							ua->ap_Flags &= ~APF_DIDDIR;
							MatchNext(ua);
							temprc = IoErr();
							continue;
						}
					}

					/* If we only want files next entry */
					if( !(flags & FLAG_DIRS) &&
						ua->ap_Info.fib_DirEntryType > 0)
					{
						MatchNext(ua);
						temprc = IoErr();
						continue;
					}

					/* If we only want directories, next entry */
					if( !(flags & FLAG_FILES) &&
						ua->ap_Info.fib_DirEntryType < 0)
					{
						MatchNext(ua);
						temprc = IoErr();
						continue;
					}

					if(!(flags & FLAG_QUIET))
					{
						PutStr(ua->ap_Buf);
						if(ua->ap_Info.fib_DirEntryType > 0)
						{
							PutStr(" [DIR]\n");
						}
						else
						{
							PutStr("\n");
						}
					}

					if(strcmp(ua->ap_Info.fib_Comment,fileName[files-1]))
					{
						if(SetComment(ua->ap_Buf, fileName[files-1]))
						{
							temprc = IoErr();
							PrintFault(IoErr(), ua->ap_Buf);
							/*
								return value is warn, whenever we return
							*/
							error = RETURN_WARN;
						}
					}
					MatchNext(ua);
					temprc = IoErr();
				}
				if( temprc && temprc != ERROR_NO_MORE_ENTRIES )
				{
					PrintFault(temprc, fileName[set]);
					error = RETURN_ERROR;
				}
			}
			FreeVec((void *)ua);
		}
		else
		{
			FPuts(clip->cli_StandardOutput, MSG_MEM_ALLOC_ERROR_STR);
			error = RETURN_FAIL;
		}
	}
	return error;
}


/* NOT/S,WARN/S,ERROR/S,FAIL/S,,EQ/K,GT/K,GE/K,VAL/S,EXISTS/K: */

int DOif(int argc,char *argv[])
{
	int n, inverse=0, error=0;
	char op = '\0', *op1=NULL, *op2=NULL;

	for(n=1;n<argc;n++)
	{
		if(!stricmp(argv[n],"not"))
		{
			inverse = 1-inverse;
		}
		else if(!stricmp(argv[n],"warn"))
		{
			if(Cli()->cli_ReturnCode >= 5 && !inverse)
				return -1;	/* execute then-clause */

			if(Cli()->cli_ReturnCode < 5 && inverse)
				return -1;	/* execute then-clause */

			break;
		}
		else if(!stricmp(argv[n],"error"))
		{
			if(Cli()->cli_ReturnCode >= 10 && !inverse)
				return -1;	/* execute then-clause */

			if(Cli()->cli_ReturnCode < 10 && inverse)
				return -1;	/* execute then-clause */

			break;
		}
		else if(!stricmp(argv[n],"fail"))
		{
			if(Cli()->cli_ReturnCode >= 20 && !inverse)
				return -1;	/* execute then-clause */

			if(Cli()->cli_ReturnCode < 20 && inverse)
				return -1;	/* execute then-clause */

			break;
		}
		else if(!stricmp(argv[n],"eq"))
		{
			op = '=';
		}
		else if(!stricmp(argv[n],"gt"))
		{
			op = '>';
		}
		else if(!stricmp(argv[n],"ge"))
		{
			op = 'g';
		}
		else if(!stricmp(argv[n],"=="))
		{
			op = '=';
		}
		else
		{
			if(!op1)
			{
				op1 = argv[n];
			}
			else if(!op2)
			{
				op2 = argv[n];

				if(op)
				{
					switch(op)
					{
					case '=':
						if(atol(op1) == atol(op2))
						{
							if(inverse)
								return 0;
							return -1;
						}
						if(inverse)
							return -1;
						return 0;

					case '>':
						if(atol(op1) > atol(op2))
						{
							if(inverse)
								return 0;
							return -1;
						}
						if(inverse)
							return -1;
						return 0;

						break;
					case 'g':
						if(atol(op1) >= atol(op2))
						{
							if(inverse)
								return 0;
							return -1;
						}
						if(inverse)
							return -1;
						return 0;

						break;
					default:
						error = 10;
						break;
					}
				}
				else
				{
					error = 10;
				}
			}
			else
			{
				error = 10;
				PutStr("Too many arguments\n");
				break;
			}
		}
	}
	return error;
}


int DOinternal(int argc,char *argv[])
{
	int i=0,j=0,Columns=80;
	char temp[40];

	PutStr(MSG_INTERNAL_LIST_STR);
	while(Internals[i].name)
	{
		sprintf(temp,"%-12s",Internals[i].name);
		PutStr(temp);
		if(++j>Columns/12-1)
		{
			PutStr("\n");
			j=0;
		}
		i++;
	}
	if(j)
		PutStr("\n");

	return RETURN_OK;
}

void FreeCmdCache()
{
	if(CommandCache.buffer)
	{
		FreeMem(CommandCache.buffer, CommandCache.buffersize);
		CommandCache.buffer = 0;
		CommandCache.buffersize = 0;
		CommandCache.bufferindex = 0;
	}
	if(CommandCache.indices)
	{
		FreeMem(CommandCache.indices, CommandCache.maxcommands*sizeof(short));
		CommandCache.indices = 0;
		CommandCache.maxcommands = 0;
		CommandCache.commands = 0;
	}
}

int FindCmdCache(char *cmd)
{
	int j;

	for(j=0;j<CommandCache.commands;j++)
	{
		if(!stricmp(cmd, CommandCache.buffer+CommandCache.indices[j]))
		{
			return -1;
		}
	}
	return 0;
}

static void ReAllocCmdCache(int sizedelta,int numdelta)
{
	char *temp;
	short *temp2;

	if(sizedelta)
	{
		if(temp = (char *)AllocMem(CommandCache.buffersize+sizedelta, MEMF_ANY))
		{
			if(CommandCache.buffer)
			{
				memcpy(temp, CommandCache.buffer, CommandCache.buffersize);
				FreeMem(CommandCache.buffer, CommandCache.buffersize);
			}
			CommandCache.buffer = temp;
			CommandCache.buffersize += sizedelta;
		}
	}
	if(numdelta)
	{
		if(temp2 = (short *)AllocMem((CommandCache.maxcommands+numdelta)*sizeof(short), MEMF_ANY))
		{
			if(CommandCache.indices)
			{
				memcpy(temp2, CommandCache.indices, CommandCache.maxcommands*sizeof(short));
				FreeMem(CommandCache.indices, CommandCache.maxcommands*sizeof(short));
			}
			CommandCache.indices = temp2;
			CommandCache.maxcommands += numdelta;
		}
	}
}


int DOrehash(int argc,char *argv[])
{
	struct FileLock *fl;
	struct CommandLineInterface *clip = Cli();	/* Cli() */
	struct FileInfoBlock *fib;
	struct Segment *seg_cmd;		/* for commands on the resident list */
	long i,n;
	char temp[80];

	struct DevProc *dp = NULL;/* for searching multi-assigned paths */
	struct MsgPort *fstask;	/* for GetFileSysTask() */
	BPTR lock, 				/* on source directory */
		 dir_lock;			/* save current directory */

	if(!(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL)))
	{
		return RETURN_WARN;
	}

	if(CommandCache.buffersize==0 && CommandCache.maxcommands==0)
	{
		ReAllocCmdCache(HASH_BUFINITIAL, HASH_NUMINITIAL);
		CommandCache.bufferindex = 0;
	}

	CommandCache.commands = 0;
	CommandCache.bufferindex = 0;

	for(i=0;Internals[i].name;i++)
	{

		n = strlen(Internals[i].name) + 1;
		if(CommandCache.commands == CommandCache.maxcommands)
		{
			ReAllocCmdCache(0, HASH_NUMDELTA);
		}
		if(CommandCache.bufferindex + n >= CommandCache.buffersize)
		{
			ReAllocCmdCache(HASH_BUFDELTA, 0);
		}

		if(!FindCmdCache(Internals[i].name))
		{
			strcpy(CommandCache.buffer+CommandCache.bufferindex, Internals[i].name);
			CommandCache.indices[CommandCache.commands++] = CommandCache.bufferindex;
			CommandCache.bufferindex += n;
		}
	}

	/*	Search the Resident list for the command.
		Look at the regular list, and then the standard shell internal list.
	*/

	Forbid();
	seg_cmd = (struct Segment *)(((ULONG)((struct DosInfo *)(DOSBase->dl_Root->rn_Info<<2))->di_NetHand)<<2);

	while(seg_cmd)
	{
		if(seg_cmd->seg_UC>=0 || seg_cmd->seg_UC==CMD_INTERNAL)
		{
			n = seg_cmd->seg_Name[0] + 1;

			strncpy(temp,seg_cmd->seg_Name+1,n-1);
			temp[n-1] = '\0';
			if(CommandCache.commands == CommandCache.maxcommands)
			{
				ReAllocCmdCache(0, HASH_NUMDELTA);
			}
			if(CommandCache.bufferindex + n >= CommandCache.buffersize)
			{
				ReAllocCmdCache(HASH_BUFDELTA, 0);
			}

			if(!FindCmdCache(temp))
			{
				strcpy(CommandCache.buffer+CommandCache.bufferindex, temp);
				CommandCache.indices[CommandCache.commands++]=CommandCache.bufferindex;
				CommandCache.bufferindex += n;
			}
		}
		seg_cmd = (struct Segment *)(seg_cmd->seg_Next<<2);
	}
	Permit();

	/* Forbid(); We assume that no-one else is messing with OUR path */
	fl = BADDR(clip->cli_CommandDir);

	while(fl)
	{
		if(fl->fl_Key)
		{
			if(Examine(fl->fl_Key, fib))
			{
				while(ExNext(fl->fl_Key, fib))
				{
					if(fib->fib_DirEntryType == ST_FILE || fib->fib_DirEntryType==ST_LINKFILE)
					{
						i = fib->fib_Protection^0x0f;

						n = strlen(fib->fib_FileName) + 1;
						if(CommandCache.commands == CommandCache.maxcommands)
						{
							ReAllocCmdCache(0, HASH_NUMDELTA);
						}
						if(CommandCache.bufferindex + n >= CommandCache.buffersize)
						{
							ReAllocCmdCache(HASH_BUFDELTA, 0);
						}

						if((i & FIBF_SCRIPT) ||
						   (i & FIBF_EXECUTE) ||
						   (i & FIBF_GRP_EXECUTE) ||
						   (i & FIBF_OTR_EXECUTE))
						{
							if(!FindCmdCache(fib->fib_FileName))
							{
								strcpy(CommandCache.buffer+CommandCache.bufferindex, fib->fib_FileName);
								CommandCache.indices[CommandCache.commands++]=CommandCache.bufferindex;
								CommandCache.bufferindex += n;
							}
						}
					}
				}
			}
		}
		fl = BADDR(fl->fl_Link);
	}
	/* Permit(); */

	/*
		Last, but CERTAINLY not least, search 'C:'
	*/

	fstask = GetFileSysTask();
	do
	{
		dp = GetDeviceProc("C:", dp);
		if(dp)
		{
			SetFileSysTask(dp->dvp_Port);

			dir_lock = lock = NULL;
			if(dp->dvp_Lock)
			{
				dir_lock = dp->dvp_Lock;
			}
			else
			{
				dir_lock = lock = Lock("C:", ACCESS_READ);
			}
			if(dir_lock)
			{
				if(Examine(dir_lock, fib))
				{
					while(ExNext(dir_lock, fib))
					{
						if(fib->fib_DirEntryType == ST_FILE || fib->fib_DirEntryType==ST_LINKFILE)
						{
							i = fib->fib_Protection^0x0f;

							n = strlen(fib->fib_FileName) + 1;
							if(CommandCache.commands == CommandCache.maxcommands)
							{
								ReAllocCmdCache(0, HASH_NUMDELTA);
							}
							if(CommandCache.bufferindex + n >= CommandCache.buffersize)
							{
								ReAllocCmdCache(HASH_BUFDELTA, 0);
							}

							if((i & FIBF_SCRIPT) ||
							   (i & FIBF_EXECUTE) ||
							   (i & FIBF_GRP_EXECUTE) ||
							   (i & FIBF_OTR_EXECUTE))
							{
								if(!FindCmdCache(fib->fib_FileName))
								{
									strcpy(CommandCache.buffer+CommandCache.bufferindex, fib->fib_FileName);
									CommandCache.indices[CommandCache.commands++]=CommandCache.bufferindex;
									CommandCache.bufferindex += n;
								}
							}
						}
					}
				}
				if(lock)
					UnLock(lock);
			}
		}
	}
	while(dp && (dp->dvp_Flags & DVPF_ASSIGN) && (IoErr() == ERROR_NO_MORE_ENTRIES));
	/*                                                       !!!!!!!!!!!!!!!!!!!!! */

	/* Current directory is handled separately in ccomplist() */

	SetFileSysTask(fstask);

	if(dp)
		FreeDeviceProc(dp);

	FreeDosObject(DOS_FIB, fib);

	if(argc)
	{
		sprintf(temp, "%d commands found\n", CommandCache.commands);
		PutStr(temp);
		sprintf(temp, "(buffer at 0x%x, %d/%d bytes used)\n",
					CommandCache.buffer,
					CommandCache.bufferindex,
					CommandCache.buffersize);
		PutStr(temp);
	}

	return RETURN_OK;
}

int DOwhich(int argc,char *argv[])
{
	int n, l, error=RETURN_OK;
	struct Segment *seg_cmd;		/* for commands on the resident list */
	struct FileInfoBlock *fib;
	struct FileLock *fl;
	long i;
	BPTR lock, 				/* on source directory */
		 dir_lock;			/* save current directory */
	char temp[256];
	BPTR seglist;
	struct CommandLineInterface *clip = Cli();	/* Cli() */

	if(!(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL)))
	{
		FPuts(clip->cli_StandardOutput, MSG_MEM_ALLOC_ERROR_STR);
		return RETURN_WARN;
	}

	if(argc < 2)
	{
		FPuts(clip->cli_StandardOutput, MSG_WHICH_USAGE_STR);
		return RETURN_WARN;
	}
	for(n=1; n<argc; n++)
	{
		if(FindVar(argv[n], LV_ALIAS))
		{
			PutStr(MSG_WHICH_ALIAS_STR);
		}
		/*	Search my internal list for the command. */
		for(l = 0; Internals[l].name; l++)
		{
			if(!stricmp(argv[n], Internals[l].name))
			{
				PutStr(MSG_WHICH_INTER_STR);
				break;
			}
		}
		/*	Search the Resident list for the command.
			Look at the regular list, and then the standard shell internal list.
		*/
		Forbid();
		if(seg_cmd = FindSegment(argv[n], NULL, 0))
		{
			if(seg_cmd->seg_UC != CMD_SYSTEM && seg_cmd->seg_UC > CMD_DISABLED)
			{
				PutStr(MSG_WHICH_RESID_STR);
			}
		}
		if(seg_cmd = FindSegment(argv[n], NULL, CMD_INTERNAL))
		{
			if(seg_cmd->seg_UC != CMD_SYSTEM && seg_cmd->seg_UC > CMD_DISABLED)
			{
				PutStr(MSG_WHICH_SHINT_STR);
			}
		}
		Permit();

		/* Then search the path */

		fl = BADDR(Cli()->cli_CommandDir);
		while(fl)
		{
			if(fl->fl_Key)
			{
				dir_lock = CurrentDir(fl->fl_Key);
				if(lock = Lock(argv[n], ACCESS_READ))
				{
					if(Examine(lock, fib))
					{
						if(fib->fib_DirEntryType == ST_FILE || fib->fib_DirEntryType==ST_LINKFILE)
						{
							i = fib->fib_Protection^0x0f;
							if((i & FIBF_SCRIPT))
							{
								getpath(lock, temp);
								PutStr(temp);
								PutStr(" [script]\n");
							}
							else if((i & FIBF_EXECUTE) ||
									(i & FIBF_GRP_EXECUTE) ||
									(i & FIBF_OTR_EXECUTE))
							{
								/* LoadSeg() needs the full path */
								getpath(lock, temp);
								if(seglist = LoadSeg(temp))
								{
									UnLoadSeg(seglist);
									PutStr(temp);
									PutStr("\n");
								}
							}
						}
					}
					UnLock(lock);
				}
				CurrentDir(dir_lock);
			}
			fl = BADDR(fl->fl_Link);
		}
		/* Permit(); */

		/* Then check the (possibly) multiassigned C: */

		strcpy(temp, "C:");
		strcpy(temp+2, argv[n]);
		if(lock = Lock(temp, ACCESS_READ))
		{
			if(Examine(lock, fib))
			{
				if(fib->fib_DirEntryType == ST_FILE || fib->fib_DirEntryType==ST_LINKFILE)
				{
					i = fib->fib_Protection^0x0f;
					if((i & FIBF_SCRIPT))
					{
						getpath(lock, temp);
						PutStr(temp);
						PutStr(" [script]\n");
					}
					else if((i & FIBF_EXECUTE) ||
						    (i & FIBF_GRP_EXECUTE) ||
						    (i & FIBF_OTR_EXECUTE))
					{
						/* LoadSeg() needs the full path */
						getpath(lock, temp);
						if(seglist = LoadSeg(temp))
						{
							UnLoadSeg(seglist);
							PutStr(temp);
							PutStr("\n");
						}
					}
				}
			}
			UnLock(lock);
		}

		/* then current directory */
		if(lock = Lock(argv[n], ACCESS_READ))
		{
			if(Examine(lock, fib))
			{
				if(fib->fib_DirEntryType == ST_FILE || fib->fib_DirEntryType==ST_LINKFILE)
				{
					i = fib->fib_Protection^0x0f;
					if((i & FIBF_SCRIPT))
					{
						PutStr(argv[n]);
						PutStr(" [script]\n");
					}
					else if((i & FIBF_EXECUTE) ||
						    (i & FIBF_GRP_EXECUTE) ||
						    (i & FIBF_OTR_EXECUTE))
					{
						if(seglist = LoadSeg(argv[n]))
						{
							UnLoadSeg(seglist);
							PutStr(argv[n]);
							PutStr("\n");
						}
					}
				}
			}
			UnLock(lock);
		}
	}
	FreeDosObject(DOS_FIB, fib);
	return error;
}

int tolower(int);
int ccomp(char *name)
{
	char	*found, *name1, *name2;
	int		len=strlen(name), i=0, j;
	BOOL	done=FALSE;
	const	int size=3*ARGLEN;

	if(len>=ARGLEN)	/* wont process longer commandnames than maximum.. */
	{
		PutStr("\a");
		return 0;
	}

	j=0;
	while(name[j] && name[j]!=':' && name[j]!='/')
		j++;

	if(name[j])
	{
		/* Absolute or relative path, normal filename completion */
		return fcomp(name);
	}

	if(CommandCache.buffersize==0 || CommandCache.maxcommands==0)
		DOrehash(0, NULL);

	if(found=(char *)AllocMem(size,MEMF_CLEAR))
	{
		name1=found+ARGLEN;
		name2=name1+ARGLEN;

		lowercase(name1,name);

		for(j=0;j<CommandCache.commands;j++)
		{
			lowercase(name2, CommandCache.buffer+CommandCache.indices[j]);
			if(!strncmp(name1,name2,len))
			{
				if(name2[len]=='\0')
				{		/* exact command name */
					strcpy(found,name1);
					done=TRUE;
					break;
				}
				if(found[0])
				{
					i=len;
					while(found[i]==name2[i])
						i++;
					if(found[i])	/* the commands are not identical */
					{
						found[i]='\0';
						done=FALSE;
					}
				}
				else
				{
					strcpy(found, name2);
					done=TRUE;
					if(!name1[0])
						break;
				}
			}
		}

		if(done)
		{
			strcat(found, " ");	/* we have a complete name */
		}
		else
		{
			if(fcomp(name))
				done = TRUE;

			i=0;
			while(tolower(name[i])==tolower(found[i]))
				i++;
			if(found[i])
			{
				name[i]='\0';
				done = FALSE;
			}
		}
		if(found[0])
			strcpy(name+len, found+len);

		FreeMem(found, size);
	}
	if(!done)
	{
		PutStr("\a");
		return 0;
	}
	return -1;
}


void ccomplist(char *name, int Columns)
{
	char	name1[ARGLEN],name2[ARGLEN];
	int		len=strlen(name), j, num=0;
	struct	FileInfoBlock *fib;
	BPTR 	mylock;

	if(len>=ARGLEN)	/* wont process longer commandnames than maximum.. */
	{
		PutStr("\a");
		return;
	}

	j=0;
	while(name[j] && name[j]!=':' && name[j]!='/')
		j++;

	if(name[j])
	{
		/* Absolute or relative path, normal filename completion */
		fcomplist(name, Columns);
		return;
	}

	if(CommandCache.buffersize==0 || CommandCache.maxcommands==0)
		DOrehash(0,NULL);

	lowercase(name1,name);

	for(j=0;j<CommandCache.commands;j++)
	{
		lowercase(name2, CommandCache.buffer+CommandCache.indices[j]);
		if(!strncmp(name1, name2, len))
		{
			num += strlen(name2)+2;
			if(num > Columns)
			{
				PutStr("\n");
				PutStr(CommandCache.buffer+CommandCache.indices[j]);
				PutStr("  ");
				num = strlen(name2)+2;
			}
			else
			{
				PutStr(CommandCache.buffer+CommandCache.indices[j]);
				PutStr("  ");
			}
		}
	}

	if(fib = (struct FileInfoBlock *)AllocDosObject(DOS_FIB, NULL))
	{
		if(mylock=Lock("",ACCESS_READ))
		{
			Examine(mylock, fib);
			while(ExNext(mylock, fib))
			{
				if(strnicmp(name1, fib->fib_FileName, len))
					continue;

				j = fib->fib_Protection^0x0f;
				if(fib->fib_DirEntryType > 0)
				{
					num += strlen(fib->fib_FileName)+2;
					if(num > Columns)
					{
						PutStr("\n");
						PutStr(fib->fib_FileName);
						PutStr("/ ");
						num = strlen(fib->fib_FileName)+2;
					}
					else
					{
						PutStr(fib->fib_FileName);
						PutStr("/ ");
					}
				}
				else
				{
					if((j & FIBF_SCRIPT) ||
					   (j & FIBF_EXECUTE) ||
					   (j & FIBF_GRP_EXECUTE) ||
					   (j & FIBF_OTR_EXECUTE))
					{
						num += strlen(fib->fib_FileName)+2;
						if(num > Columns)
						{
							PutStr("\n");
							PutStr(fib->fib_FileName);
							PutStr("  ");
							num = strlen(fib->fib_FileName)+2;
						}
						else
						{
							PutStr(fib->fib_FileName);
							PutStr("  ");
						}
					}
				}
			}
			UnLock(mylock);
			if(num)
				PutStr("\n");
		}
		FreeDosObject(DOS_FIB, fib);
	}
}



