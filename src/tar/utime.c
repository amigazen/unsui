/*
 * TITLE: touch.c This is a simple command to set the date of a file to
 * now. It was compiled using Greenhills C.  You might have to change it a
 * little for use with Lattice or Manx. The program compiles with just
 * amigalib.  (that's why those string functions are tacked on the end of
 * the file)
 */

/*
 * Changed to be utime function by Jonathan Hue
 */

/* touch.c by Phil Lindsay and Andy Finkel              */
/* (c) 1986 Commodore-Amiga, Inc.                       */
/* Permission to use in any way granted, as long as     */
/* the copyright notice stays intact                    */

#include "exec/types.h"
#include "exec/ports.h"
#include "exec/io.h"
#include "exec/memory.h"
#include "libraries/dos.h"
#include "libraries/dosextens.h"
#include <sys/types.h>
#include <errno.h>

extern LONG sendpkt();

#define ACTION_SET_DATE     34

static struct DateStamp *seconds2AmiTime();

int
utime(char *filename, time_t unixtime[2])
{
    struct DateStamp dateStamp;

    return(utime_from_stamp(filename, seconds2AmiTime(unixtime[1]),
			    &dateStamp));
}

int
utime_from_stamp(char *filename, struct DateStamp *ds)
{
    struct MsgPort *task;
    struct FileInfoBlock *fib;
    LONG arg[4];
    LONG rc;
    ULONG lock;
    ULONG plock;
    UBYTE *pointer;

    pointer = NULL;
    if (!(pointer = (UBYTE *) AllocMem(64, MEMF_PUBLIC)))
    {
	errno = ENOMEM;
	return(-1);
    }
    if (!(task = (struct MsgPort *) DeviceProc(filename)))
    {
	FreeMem(pointer, 64);
	errno = ENOENT;
	return(-1);
    }
    if (!(lock = (ULONG) Lock(filename, SHARED_LOCK)))
    {
	FreeMem(pointer, 64);
	errno = ENOENT;
	return(-1);
    }
    plock = (ULONG) ParentDir(lock);
    if (!plock)			/* filename is root dir, can't set time */
    {
	FreeMem(pointer, 64);	/* sometimes you almost want to use a goto */
	errno = EACCES;
	UnLock(lock);
	return(-1);
    }
    if (!(fib = malloc(sizeof(*fib))))
    {
	FreeMem(pointer, 64);
	errno = ENOMEM;
	UnLock(lock);
	return(-1);
    }
    Examine(lock, fib);
    UnLock(lock);
    strcpy((pointer + 1), fib->fib_FileName);
    *pointer = strlen(fib->fib_FileName);
    free(fib);
    arg[0] = NULL;
    arg[1] = plock;
    arg[2] = (ULONG) & pointer[0] >> 2;	/* BSTR of filename */
    arg[3] = (ULONG) ds;		/* DateStamp */
    rc = sendpkt(task, ACTION_SET_DATE, arg, 4);

    UnLock(plock);
    FreeMem(pointer, 64);
    return (0);
}


LONG
sendpkt(id, type, args, nargs)
struct MsgPort *id;/* process indentifier ... (handlers message port ) */
LONG type;	   /* packet type ... (what you want handler to do )   */
LONG args[];			/* a pointer to a argument list */
LONG nargs;				/* number of arguments in list  */
{

    struct MsgPort *replyport;
    struct StandardPacket *packet = NULL;
    LONG count;
    LONG *pargs;
    LONG res1 = NULL;


    if (!(replyport = (struct MsgPort *) CreatePort(NULL, NULL)))
	return (NULL);

    packet = (struct StandardPacket *)
	AllocMem((LONG) sizeof(*packet), MEMF_PUBLIC | MEMF_CLEAR);

    if (packet)
    {
	packet->sp_Msg.mn_Node.ln_Name = (char *) &(packet->sp_Pkt);	/* link packet */
	packet->sp_Pkt.dp_Link = &(packet->sp_Msg);	/* to message    */
	packet->sp_Pkt.dp_Port = replyport;	/* set-up reply port   */
	packet->sp_Pkt.dp_Type = type;	/* what to do... */

    /* move all the arguments to the packet */
	pargs = &(packet->sp_Pkt.dp_Arg1);	/* address of first
						 * argument */
	for (count = 0; (count < nargs) && (count < 7); count++)
	    pargs[count] = args[count];

	PutMsg(id, packet);	/* send packet */
	WaitPort(replyport);	/* wait for packet to come back */
	GetMsg(replyport);	/* pull message */

	res1 = packet->sp_Pkt.dp_Res1;	/* get result */
        FreeMem(packet, (LONG) sizeof(*packet));

    }
    DeletePort(replyport);
    return (res1);
}

/*
 * Convert seconds into Amiga style time (days since 1/1/78, minutes since
 * 12AM, ticks this hour)
 */
static struct DateStamp *
seconds2AmiTime(long secs, struct DateStamp *ds)
{
    extern long timezone;

    /* seconds is in GMT, so compensate */
    secs -= timezone;

    /* Subtract 8 years worth of seconds, including 2 leap years */
    secs -= ((6 * 365) + (2 * 366)) * (60 * 60 * 24);

    /* Now have seconds since 1/1/78, get days */
    ds->ds_Days = secs / (60 * 60 * 24);
    secs -= ds->ds_Days * (60 * 60 * 24);

    /* Now have seconds since midnight, get minutes */
    ds->ds_Minute = secs / 60;
    secs -= ds->ds_Minute * 60;

    /* Now have seconds this minute, convert to ticks */
    ds->ds_Tick = secs * 50;
    return(ds);
}
