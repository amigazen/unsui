/*****************************************************************************\
 * InfoQ.c                       DICE/LATTICE C/SAS C/AZTEC C + AmigaOS 2.04 *
 *                 _                                                         *
 *            _   // (c)1992 by "Quarky" Dieter Temme                        *
 *            \\ //                                                          *
 * :ts=4       \X/ --- Freeware --- ONLY AMIGA MAKES IT POSSIBLE             *
 *                                                                           *
 * replaces the Shell's standard Info command                                *
\*****************************************************************************/

#define PRGNAME "InfoQ"
#define VERSION "1.0"
#define PRGDATE "24.9.92"

#include "amigacompq.h"

#include <string.h>
#include <exec/types.h>
#include <clib/dos_protos.h>
#include <clib/exec_protos.h>
#include <clib/utility_protos.h>
#include <dos/datetime.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <exec/libraries.h>

#ifdef PRAGMAS_
 #include <pragmas/dos_lib.h>
 #include <pragmas/exec_lib.h>
 #include <pragmas/utility_lib.h>
#endif

#ifdef LATTICE
 int CXBRK(void) { return 0; }  /* Disable Lattice CTRL-C handling */
 int chkabort(void) { return 0; }
#endif

#ifdef AZTEC_C
 void _wb_parse(void) {	extern long Enable_Abort; Enable_Abort= FALSE; }
 void _cli_parse(void) { extern long _argc; _argc= 1; }
 void _abort(void) {} /* Disable Aztec CTRL-C handling */
#endif

TEXT VersionString[]= "\0$VER: " PRGNAME " " VERSION " (" PRGDATE ")";

extern struct Library *SysBase;
struct Library *UtilityBase;


/*==== print number formatted in KBytes or MBytes ====*/
void PrintNum_(ULONG num)
{	ULONG n;

	if (num>>10)
	{	n= ((num*100)>>10)%100;
		n= '0'+n/10+((n%10) >= 5);
		Printf(" %4ld.%lcM", num>>10, n);
	} else
		Printf(" %6ldK", num);
}

/*==== main program (Shell) ====*/
int main(int argc, TEXT *argv[])
{	static struct InfoData id;
	static TEXT *argray[3];
	struct DosList *ndl, *dlist;
	struct RDArgs *rdargs;
	BOOL first= TRUE;
	struct Process *proc;
	struct Window *win;

	/*- exit if called from Workbench -*/
	if (!argc) return RETURN_FAIL;

	/*- exit if version of OS is not >= 2.04 -*/
	if (SysBase->lib_Version < 37)
	{	Write(Output(), "Sorry, you'll need at least AmigaOS 2.04!\n", 42);
		return RETURN_FAIL;
	}

	/*- open utility.library -*/
	if (!(UtilityBase= OpenLibrary("utility.library", 37)))
	{	PutStr("Couldn't open 'utility.library'\n");
		return RETURN_FAIL;
	}

	/*- read arguments -*/
	rdargs= ReadArgs("DISKS/S,VOLS=VOLUMES/S,DEVICES/M", argray, NULL);

	/*- lock list of devices and volumes -*/
	ndl= dlist= LockDosList(LDF_DEVICES|LDF_VOLUMES|LDF_READ);

	/*- avoid requesters -*/
	proc= (struct Process *)FindTask(NULL);
	win= proc->pr_WindowPtr;
	proc->pr_WindowPtr= (struct Window *)~0;

	/*- show devices -*/
	if (argray[2] || argray[0] || !argray[1])
	{	static char *dstate[]=
		{	"Read Only ",
			"Validating",
			"Read/Write"
		};
		TEXT name[108];

		while (ndl= NextDosEntry(ndl, LDF_DEVICES|LDF_READ))
		{	/* continue if no file device */
			if (!ndl->dol_Task) continue;

			/* get device name */
			{	TEXT *str= ((TEXT *)BADDR(ndl->dol_Name));
				CopyMem(str+1, name, *str);
				name[*str]= ':';
				name[*str+1]= '\0';
			}

			/* if device names given, select device */
			if (argray[2])
			{	TEXT **strarray= (TEXT **)argray[2];
				while (*strarray && Stricmp(*strarray, name)) strarray++;
				if (!*strarray) continue;
			}

			/* print device information */
			{	BPTR lock;

				/* if first device to print, print title */
				if (first)
				{	if (!argray[2]) PutStr("Disks:\n");
					PutStr("Unit      Size    Used    Free "
						"Full Errs   Status   Name\n");
					first= FALSE;
				}

				/* if not lockable print status */
				lock= Lock(name, ACCESS_READ);
				if (!lock)
				{	TEXT *s;

					switch (IoErr())
					{	case ERROR_NO_DISK:
							s= "No disk present";
							goto print;
						case ERROR_NOT_A_DOS_DISK:
							s= "Unreadable disk";
print:						Printf("%-6s   %s\n", name, s);
					}
				} else if (Info(lock, &id))
				{	ULONG size, used;

					Printf("%-6s", name);
					size= (id.id_NumBlocks*id.id_BytesPerBlock)>>10;
					used= (id.id_NumBlocksUsed*id.id_BytesPerBlock)>>10;
					NameFromLock(lock, name, 108);
					name[strlen(name)-1]= '\0';
					PrintNum_(size);
					PrintNum_(used);
					PrintNum_(size-used);
					if (size&0xfe000000) /* for "bigger" disks */
					{	size>>= 7;
						used>>= 7;
					}
					Printf("%4ld%% %3ld %11s %s\n", 
						(100*used+size-1)/size, id.id_NumSoftErrors,
						((id.id_DiskState >= ID_WRITE_PROTECTED)
							&& (id.id_DiskState <= ID_VALIDATED))?
							dstate[id.id_DiskState-ID_WRITE_PROTECTED] : "",
						name);
					UnLock(lock);
				}
			}
		}
	}

	/*- show volumes -*/
	if (argray[1] || (!argray[2] && !argray[0]))
	{	static struct DateTime dt;
		TEXT ds[LEN_DATSTRING];
		dt.dat_StrDate= ds;

		if (!first) PutStr("\n");
		PutStr("Volumes:\n");
		ndl= dlist;

		while (ndl= NextDosEntry(ndl, LDF_VOLUMES|LDF_READ))
		{	Printf("%-30s", &((TEXT *)BADDR(ndl->dol_Name))[1]);
			PutStr(ndl->dol_Task? " [Mounted]" : "          ");
			CopyMem(&ndl->dol_misc.dol_volume.dol_VolumeDate, &dt,
				sizeof(struct DateStamp));
			if (DateToStr(&dt)) Printf(" created %s", ds);
			PutStr("\n");
		}
	}

	/*- reset window pointer of our process -*/
	proc->pr_WindowPtr= win;

	/*- unlock list of devices and volumes -*/
	UnLockDosList(LDF_DEVICES|LDF_VOLUMES|LDF_READ);

	/*- free arguments -*/
	FreeArgs(rdargs);

	/*- close library -*/
	CloseLibrary(UtilityBase);
}
