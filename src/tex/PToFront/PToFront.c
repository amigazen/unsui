/* PToFront						*/
/*							*/
/* Hilfsprogramm fuer PasTeX 1.3.			*/
/*							*/
/* Unter 2.0 kann ein Argument, ein Public-Screen Name,	*/
/* uebergeben werden.					*/
/* Unter 1.3 wird immer der WB-Screen nach vorne	*/
/* geholt.						*/
/*							*/
/* Return-Codes:					*/
/*  FAIL (20) - Intuition kann nicht geoeffnet werden.	*/
/*  WARN (5)  - Screen kann nicht gefunden werden.	*/
/*							*/
/*	Georg Heﬂmann		27.07.91		*/

#include <stdlib.h>
#include <string.h>

#include <exec/types.h>
#include <exec/execbase.h>
#include <intuition/intuitionbase.h>
#include <intuition/screens.h>

#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <clib/intuition_protos.h>

#include <pragmas/exec_pragmas.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/intuition_pragmas.h>


static struct IntuitionBase	*IntuitionBase;
extern struct ExecBase		*SysBase;
extern struct Library		*DOSBase;


static const char VersionID[]="$VER: PToFront 1.0 ("__DATE__")";


void _main(char *arg)
{
  register struct Screen *scr;
  long ret = RETURN_OK;

  IntuitionBase = (struct IntuitionBase *)OpenLibrary("intuition.library",0);
  if (IntuitionBase == NULL) _exit(RETURN_FAIL);

  if (SysBase->LibNode.lib_Version >= 36L) {
    register char *ptr;
    char PubName[MAXPUBSCREENNAME+1];

    if (arg[strlen(arg)-1] == '\n') arg[strlen(arg)-1] = '\0';

    ptr = strchr(arg+1, '\"');	/* Suche Ende des Programmnamens */
    if (ptr != NULL) {
      ptr += 2;
    }
    if (ptr == NULL || *ptr == '\0') {
      /* kein Argument angegeben, suche den default PubScreen */
      GetDefaultPubScreen(PubName);
      ptr = &(PubName[0]);
    }

    scr = LockPubScreen(ptr);
    if (scr != NULL) {
      ScreenToFront(scr);
      UnlockPubScreen(ptr, NULL);
    }
    else {
      Write(Output(), "Screen not found!\n", 18);
      ret = RETURN_WARN;
    }
  }
  else {
    /* unter 1.3 wird nur der WB Screen nach vorne geholt */
    (void)WBenchToFront();
  }
  _exit(ret);
}

