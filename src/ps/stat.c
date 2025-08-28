/*
 *
 * $Header$
 *
 */

/*
 *
 * Original version of this program: James M Synge, Sept. 2, 1986
 * UUCP: uw-beaver!geops!uw-atm!james
 *
 * Rewritten for AmigaOS 2.x and beyond: Henning Schmiedehausen
 * Internet: barnard@forge.franken.de
 *
 * SAS C 6.2
 *
 */




#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <exec/types.h>
#include <exec/memory.h>
#include <exec/tasks.h>
#include <exec/interrupts.h>

#include <libraries/dosextens.h>

#include <clib/exec_protos.h>


#define BUF_SIZE 256

char buf1[BUF_SIZE],
	 buf2[BUF_SIZE];

extern struct DosLibrary *DOSBase;

/* Need a macro to translate a BPTR to an APTR */

/*#ifdef BADDR
#undef BADDR
#endif
#define BADDR(bptr) (((long)bptr) << 2)
*/

void moveBSTR(BSTR bptr, char *buffer, int maxlen);

main(argc, argv)
int argc;
char *argv[];
{
	int							 tasknum,
								 pri;
	char						*msg;
	long						 process,
								 processes,
								*ta;
	struct Process				*pr;
	struct RootNode				*rn;
	struct CommandLineInterface	*cli;
	struct CliProcList			*cl;
	struct Library				*foobase;

	/* Find the RootNode */

	if(!(foobase = OpenLibrary("exec.library",37L)))
	{
		printf("You need AmigaOS V37 or beyond\n");
		exit(20);
	}
	CloseLibrary(foobase);

	rn = (struct RootNode *)(DOSBase->dl_Root);

	/* Print the title line. */
	printf("Task Pri  Address Command\t\t\t   Directory\n");

	for(cl = (struct CliProcList *)rn->rn_CliList.mlh_Head; cl->cpl_Node.mln_Succ ; cl = (struct CliProcList *)cl->cpl_Node.mln_Succ)
	{

		/* ta points to the array of tasks. The first location is the */
		/* maximum number of processes, followed by that many pointers. */
		/* Each pointer points to the MsgPort within the Process data */
		/* structure of each AmigaDos process, otherwise its value is 0L. */
		/* The AmigaDOS Technical Reference Manual calls these pointers */
		/* process ids. */


		ta = (long *) cl->cpl_Array;

		/* How many AmigaDOS processes are there? */

		processes = *ta++;

		/* Loop through each live process, printing info. */

		for (process = 0; process < processes; process++ )
		{

			/* Get the next process id (i.e. struct MsgPort *) */

			msg = (char *)(*ta++);
			if(!msg)
				continue; /* No associated Process? */

			pr = (struct Process *)(msg - sizeof(struct Task));
			tasknum = (int)(pr->pr_TaskNum);
			pri = (int)(pr->pr_Task.tc_Node.ln_Pri);

			cli = (struct CommandLineInterface *) BADDR(pr->pr_CLI);

			if(cli)
			{
				moveBSTR(cli->cli_CommandName, buf1, BUF_SIZE);
				if(!buf1[0])
					strcpy(buf1, "Waiting for a command");
				moveBSTR(cli->cli_SetName, buf2, BUF_SIZE);

			}
			else
			{
				strcpy(buf1, "Not a CLI");
				buf2[0] = 0;
			}

			printf("%3d %4d %8lx %-32s %s\n",tasknum, pri, pr, buf1, buf2);
		}
	}
	exit(0);
}

/* moveBSTR copies a BSTR to a C char string. */

void moveBSTR(BSTR bptr, char *buffer, int maxlen)
{
	register char			*ptr;
	register unsigned int	 len,
							 i;
	unsigned char			 l;

	ptr = (char *)BADDR(bptr);

	l = (unsigned int) (*ptr++);

	if (!(len = l))
	{
		*buffer = '\0';
		return;
	}
	if (len > maxlen)
		len = maxlen;
	for(i = 0; i < len; i++)
		*buffer++ = *ptr++;

	if (i < maxlen)
		*buffer = '\0'; /* If there is room, mark the end */
	return;
}

