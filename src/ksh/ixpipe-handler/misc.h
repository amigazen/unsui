/*
 *  misc.c  - support routines - Phillip Lindsay (C) Commodore 1986
 *  You may freely distribute this source and use it for Amiga Development -
 *  as long as the Copyright notice is left intact.
 *
 * 30-SEP-86
 *
 */

/* I converted this source to ANSI-C and made it a header file, so the
   functions can be included inline into the source. Inlining will only
   work with compilers that support it, like gcc.

   1992-07-03 Markus Wild	*/


/* returnpkt() - packet support routine
 * here is the guy who sends the packet back to the sender...
 *
 * (I modeled this just like the BCPL routine [so its a little redundant] )
 */

static inline void
returnpkt (struct DosPacket *packet, struct Process *myproc, 
	   ULONG res1, ULONG res2)
{
  struct Message *mess;
  struct MsgPort *replyport;

  packet->dp_Res1          = res1;
  packet->dp_Res2          = res2;
  replyport                = packet->dp_Port;
  mess                     = packet->dp_Link;
  packet->dp_Port          = &myproc->pr_MsgPort;
  mess->mn_Node.ln_Name    = (char *) packet;
  mess->mn_Node.ln_Succ    =
    mess->mn_Node.ln_Pred  = 0;
  PutMsg (replyport, mess);
}


static inline void
returnpktplain (struct DosPacket *packet, struct Process *myproc)
{
  returnpkt (packet, myproc, packet->dp_Res1, packet->dp_Res2);
}


/*
 * taskwait() ... Waits for a message to arrive at your port and
 *   extracts the packet address which is returned to you.
 */

static inline struct DosPacket *
taskwait (struct Process *myproc)
{
  struct MsgPort *myport;
  struct Message *mymess;

  myport = &myproc->pr_MsgPort;
  WaitPort (myport);
  mymess = GetMsg (myport);
  return ((struct DosPacket *)mymess->mn_Node.ln_Name);
}

