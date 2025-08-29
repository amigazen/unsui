/*
 * This file "AMIGA/PASTEX/REXX.C" works in addition to `dvips'
 * in that it calls an ARexx script via the `call_rexx' function
 * defined below.
 *
 * Georg Heßmann   <hessmann@informatik.uni-hamburg.de>, 25th August 1991.
 * Andreas Scherer <scherer@genesis.informatik.rwth-aachen.de>, 20th January 1994.
 * Giuseppe Ghibò  <ghibo@galileo.polito.it>, 18th September 1994. Removed pragmas.
 *
 */

#include <exec/types.h>
#include <libraries/dos.h>
#include <rexx/rxslib.h>
#include <rexx/errors.h>

#include <proto/exec.h>
#include <proto/dos.h>
#include <clib/rexxsyslib_protos.h>
#include <pragmas/rexxsyslib_pragmas.h>

#include <string.h>
#include <dos.h>
#include <rexx_protos.h>

extern struct ExecBase *SysBase;
struct RxsLib *RexxSysBase = NULL;


#define PORTNAME	"Call-MF"
#define RXEXTENS	"rexx"

static int PutRexxMsg(struct MsgPort *mp, long action, STRPTR arg0,
		struct RexxMsg *arg1)
{
  struct RexxMsg *rm;
  struct MsgPort *rp;

  if ((rm = CreateRexxMsg(mp, RXEXTENS, mp->mp_Node.ln_Name)) != NULL) {
    rm->rm_Action  = action;
    rm->rm_Args[0] = arg0;
    rm->rm_Args[1] = (STRPTR)arg1;
/*    rm->rm_Stdin   = Output(); */
/*    rm->rm_Stdout  = Output(); */
    Forbid();
    if ((rp = FindPort(RXSDIR)) != NULL) {
      PutMsg(rp, (struct Message *)rm);
    }
    Permit();
    if (rp == NULL) {
      DeleteRexxMsg(rm);
    }
  }
  return rm != NULL && rp != NULL;
}

int __stdargs call_rexx(char *str, long *result)
{
  char *arg;
  struct MsgPort *mp;
  struct RexxMsg *rm, *rm2;
  int ret = FALSE;
  int pend;

  if (!(RexxSysBase = (struct RxsLib *)OpenLibrary(RXSNAME, 0 /*RXSVERS*/)))
    return(ret);

  Forbid();

  if (FindPort(PORTNAME) == NULL)
    mp = CreatePort(PORTNAME, 0);

  Permit();

  if (mp != NULL) {
    if ((arg = CreateArgstring(str, strlen(str))) != NULL) {
      if (PutRexxMsg(mp, RXCOMM | RXFF_STRING, arg, NULL)) {
        for (pend = 1; pend != 0; ) {
          if (WaitPort(mp) != NULL) {
            while ((rm = (struct RexxMsg *)GetMsg(mp)) != NULL) {
              if (rm->rm_Node.mn_Node.ln_Type == NT_REPLYMSG) {
                ret = TRUE;
                *result = rm->rm_Result1;
                if ((rm2 = (struct RexxMsg *)rm->rm_Args[1]) != NULL) {
                  rm2->rm_Result1 = rm->rm_Result1;
                  rm2->rm_Result2 = 0;
                  ReplyMsg((struct Message *)rm2);
                }
                DeleteRexxMsg(rm);
                pend--;
              }
              else {
                rm->rm_Result2 = 0;
                if (PutRexxMsg(mp, rm->rm_Action, rm->rm_Args[0], rm)) {
                  pend++;
                }
                else {
                  rm->rm_Result1 = RC_FATAL;
                  ReplyMsg((struct Message *)rm);
                }
              }
            }
          }
        } /* for */
      }
      DeleteArgstring(arg);
    }
    DeletePort(mp);
  }

  CloseLibrary((struct Library *)RexxSysBase);

  return ret;
}
