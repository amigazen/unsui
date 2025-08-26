/*
   sc_rexx.c: SAS/C rexx interface

   bf 11-21-96
*/

#include <string.h>
#include <stdlib.h>
#include <rexx/rxslib.h>
#include <rexx/storage.h>
#include <proto/rexxsyslib.h>
#include <proto/exec.h>

struct RxsLib *RexxSysBase = NULL;
struct RexxMsg *RexxMesg = NULL;
struct MsgPort *RexxPort = NULL;

int
init_scmsg (void)
{
  system ("run sc:c/scmsg rexxonly");

  if (!(RexxSysBase = (struct RxsLib *) OpenLibrary ("rexxsyslib.library", 36)))
    return 0;
  if (!(RexxPort = CreateMsgPort ()))
    return 0;
  if (!(RexxMesg = CreateRexxMsg (RexxPort, NULL, NULL)))
    return 0;
  return 1;
}

void
rexx_cleanup (void)
{
  /* send_rexx_msg ("quit"); */

  if (RexxMesg)
    DeleteRexxMsg (RexxMesg);
  if (RexxPort)
    DeleteMsgPort (RexxPort);
  if (RexxSysBase)
    CloseLibrary ((struct Library *) RexxSysBase);
}

char *
send_rexx_msg (char *cmd)
{
  struct MsgPort *port;
  int rc;

  if (!(RexxMesg -> rm_Args[0] = CreateArgstring (cmd, strlen (cmd))))
    return NULL;
  RexxMesg -> rm_Node.mn_Node.ln_Type = NT_MESSAGE;
  RexxMesg -> rm_Node.mn_Length = sizeof (struct RexxMsg);
  RexxMesg -> rm_Action = RXFUNC | RXFF_RESULT;
  RexxMesg -> rm_Node.mn_ReplyPort = RexxPort;

  Forbid ();
  if (!(port = FindPort ("SC_SCMSG")))
  {
    Permit ();
    return NULL;
  }
  PutMsg (port, (struct Message *) RexxMesg);
  Permit ();
  do
    WaitPort (RexxPort);
  while (GetMsg (RexxPort) != (struct Message *) RexxMesg);

  rc = RexxMesg -> rm_Result1;
  ClearRexxMsg (RexxMesg, 1); /* doesn't clear rm_Result2 */
  return (char *) RexxMesg -> rm_Result2;
}

void
clear_rexx_result (void)
{
  if (RexxMesg -> rm_Result2)  /* I guess this is correct :-) */
  {
    DeleteArgstring ((UBYTE *) RexxMesg -> rm_Result2);
    RexxMesg -> rm_Result2 = 0;
  }
}
