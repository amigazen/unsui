/* SIMPLE TIMER EXAMPLE PROGRAM: 
 *
 * Includes dynamic allocation of data structures needed to communicate
 * with the timer device as well as the actual device IO
 *
 * Author:  Rob Peck, 12/1/85 */

/* Stripped to TimeDelay & supporting functions for Cron inclusion <CB> */

#include "exec/types.h"
#include "exec/nodes.h"
#include "exec/lists.h"
#include "exec/memory.h"
#include "exec/interrupts.h"
#include "exec/ports.h"
#include "exec/libraries.h"
#include "exec/io.h"
#include "exec/tasks.h"
#include "exec/execbase.h"
#include "exec/devices.h"
#include "devices/timer.h"

long TimerBase;		/* to get at the time comparison functions */


/* *********************************************************************** */
/*	Timer function - timedelay(seconds,microseconds)

	Your task is put to sleep for the specified time interval.

	If seconds > 0, then UNIT_VBLANK is used, delay is in multiples of
	60ths of a second.  If seconds < 0, then UNIT_MICROHZ is used for 
	more precision.    

	Returns value of 0 if no errors, nonzero (and no task sleeping)

	Notice that since this is a multi-tasking system, the delays
	shown here must be considered to be only approximate.

	Also note that this function is used primarily to show how
	a timer device is accessed, including the creation of the 
	message port and a message structure (IOStdReq).   Note that
	there is a Delay(interval) function already in the DOS.library.
	(See the DOS developer's manual for details).
	
	But this Delay() function leaves LOT to be desired, <CB>.
*/
/* *********************************************************************** */

extern struct MsgPort *CreatePort();
extern struct IORequest *CreateExtIO();

struct timerequest 
*PrepareTimer(precision)
SHORT precision;
{
	/* return a pointer to a time request.  If any problem, return NULL */

	int error;
	SHORT whichunit;

	struct MsgPort *timerport;
	struct timerequest *timermsg;
	
        timerport = CreatePort(0,0);
        if (timerport == NULL) 
		return(NULL);	/* Error during CreatePort */

	timermsg = (struct timerequest *)CreateExtIO(
					timerport,sizeof(struct timerequest));
	if (timermsg == NULL)
		{
		DeletePort(timerport);
		return(NULL);	/* Error during CreateExtIO */
		}
	
	if(precision)	/* if true, use precision timer  ( under 1 second ) */
		whichunit = UNIT_MICROHZ;
	else
		whichunit = UNIT_VBLANK;

	error = OpenDevice(TIMERNAME, whichunit, timermsg, 0);
	if (error != 0)
		{
		DeleteExtIO(timermsg,sizeof(struct timerequest));
		DeletePort(timerport);
		return(NULL);	/* Error during OpenDevice */
		}
	return(timermsg);
}

int
GetTimerBase()
{
	int tbase;
	struct timerequest *tr;
	tr = PrepareTimer();
	tbase = (int)tr->tr_node.io_Device;		
	DeleteTimer(tr);
	return(tbase);
}

int 
TimeDelay(seconds,microseconds,precision)
		/* more precise timer than AmigaDOS Delay() */
ULONG seconds,microseconds;
int precision;
{
	int precise;
	struct timerequest *tr;
	if(seconds < 0 || precision != 0)
				/* do delay in terms of microseconds */ 
		precise = TRUE; /* yes, use the precision timer.     */
	else 
		precise = FALSE; /* no, not necessary */

	tr = PrepareTimer(precise);
					/* get a pointer to an initialized
					 * timer request block */
	if(tr == NULL) return(-1);	/* any nonzero return says timedelay
					 * routine didn't work. */
	WaitForTimer(tr,seconds,microseconds);

	DeleteTimer(tr);	/* deallocate temporary structures */
	return(0);
}			/* end of timedelay */


int
WaitForTimer(tr,seconds,microseconds)
ULONG seconds,microseconds;
struct timerequest *tr;
{
	tr->tr_node.io_Command = TR_ADDREQUEST;   /* add a new timer request */
        tr->tr_time.tv_secs =  seconds;        	/* seconds */
        tr->tr_time.tv_micro = microseconds; 	/* microseconds */
        DoIO( tr );             		/* post request to the timer */
						/* goes to sleep till done */
	return(0);
}

int
DeleteTimer(tr)
struct timerequest *tr;
{
	struct MsgPort *tp;

	tp = tr->tr_node.io_Message.mn_ReplyPort;
	if(tr != 0)
	{
		CloseDevice(tr);
		DeleteExtIO(tr,sizeof(struct timerequest));
	}
	if(tp != 0)
		DeletePort(tp);
	return(0);		
}
