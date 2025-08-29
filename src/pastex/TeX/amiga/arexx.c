
#include <stdio.h>
#include <stdlib.h>

#include <stdio.h>
#include <exec/types.h>
#include <libraries/dos.h>
#include <string.h>
#ifdef LATTICE
#include <clib/alib_protos.h>
#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/exec_pragmas.h>
#else
#include <functions.h>
#endif

extern struct DosLibrary	*DOSBase;
extern struct ExecBase		*SysBase;

#include <rexx/rxslib.h>
#include <rexx/errors.h>


#define EXTERN extern
#include "texd.h"

int do_rexx;		/* work with rexx ?? */



static int PutRexxMsg(struct MsgPort *mp,
                      long action,
                      STRPTR arg0,
                      struct RexxMsg *arg1);


STRPTR		CreateArgstring(STRPTR, long);
void		DeleteArgstring(STRPTR);
struct RexxMsg	*CreateRexxMsg(struct MsgPort *, STRPTR, STRPTR);
void		DeleteRexxMsg(struct RexxMsg *);

#pragma libcall RexxSysBase CreateArgstring 7e 0802
#pragma libcall RexxSysBase DeleteArgstring 84 801
#pragma libcall RexxSysBase CreateRexxMsg   90 09803
#pragma libcall RexxSysBase DeleteRexxMsg   96 801

#define PORTNAME	"TeX-Rexx-Port"
#define RXEXTENS	"rexx"

struct RxsLib *RexxSysBase = NULL;


static int PutRexxMsg(struct MsgPort *mp, long action, STRPTR arg0,
		struct RexxMsg *arg1)
{
  struct RexxMsg *rm;
  struct MsgPort *rp;

  if ((rm = CreateRexxMsg(mp, RXEXTENS, mp->mp_Node.ln_Name)) != NULL) {
    rm->rm_Action  = action;
    rm->rm_Args[0] = arg0;
    rm->rm_Args[1] = (STRPTR)arg1;
    rm->rm_Stdin   = Output();
    rm->rm_Stdout  = Output();
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


int call_rexx(char *str)
{
  char *arg;
  struct MsgPort *mp;
  struct RexxMsg *rm, *rm2;
  int ret = 0;
  int pend;

  if ((RexxSysBase = (struct RxsLib *)OpenLibrary(RXSNAME, 0)) == NULL) {
    fprintf(stderr, "Can't open \"%s\"!\n", RXSNAME);
  }
  else {
    mp = NULL;
    Forbid();
    if (FindPort(PORTNAME) == NULL) {
      mp = CreatePort(PORTNAME, 0);
      Permit();
      if (mp != NULL) {
        if ((arg = CreateArgstring(str, strlen(str))) != NULL) {
          if (PutRexxMsg(mp, RXCOMM | RXFF_STRING, arg, NULL)) {

            for (pend = 1; pend != 0; ) {
              if (WaitPort(mp) != NULL) {
                while ((rm = (struct RexxMsg *)GetMsg(mp)) != NULL) {
                  if (rm->rm_Node.mn_Node.ln_Type == NT_REPLYMSG) {
                    ret = 1;	/* true == ok */
                    if (rm->rm_Result1 != 0) {
                      fprintf(stderr, "\"%s\" failed! (Res1 = %ld, Res2 = %ld)\n",
                              rm->rm_Args[0], rm->rm_Result1, rm->rm_Result2);
                      ret = 0;	/* false */
                    }
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
            }		/* for */
          }
          DeleteArgstring(arg);
        }
        DeletePort(mp);
      }
    }			/* Find port */
    else {
      Permit();
    }
    CloseLibrary((struct Library *)RexxSysBase);
  }
  return ret;
}

