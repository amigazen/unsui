
#include "defines.h"

#include <stdio.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <intuition/intuition.h>
#include <libraries/dos.h>
#include <libraries/dosextens.h>
#include <dos/dostags.h>
#include <dos/var.h>

#ifdef AZTEC_C
#  include <functions.h>
#  define __stdargs
#  define ACTION_END	1007L
#endif

#ifdef ANSI
#  include <stdlib.h>
#endif

#include <dos.h>
#include <clib/intuition_protos.h>
#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/dos_pragmas.h>

#include "globals.h"
#include "amscreen.h"
#include "amscreen.i"
#include "newcli.i"
#include "globals.i"
#include "am_requ.i"

#define ZERO	0L

#if defined(REQ_LIBRARY)
# include <libraries/reqbase.h>
# include <clib/req_protos.h>
#endif



/*
 * Fuer die locale-Library:
 *
 * Hier duerfen *nur* die MSG_#? Nummern eingebunden werden!
 * Achtung:
 * Es muss/sollte 'multiple-include' erlaubt sein!
 */
#include "local.i"

#undef  CATCOMP_ARRAY
#undef  CATCOMP_BLOCK
#undef  CATCOMP_STRINGS
#define CATCOMP_NUMBERS
#include "localstr.h"



#if 0
/* ausgeklammert, da nur noch os2 unterstuetzt wird!!! */
struct MsgPort * __stdargs NewConsole		Args((struct Window *w, struct DosLibrary *DOSBase));

/* die folgenden Funktionen werden bei is_os2 nur zu Dummy-Funktionen */
static struct StandardPacket *CreateStdPkt	Args((void));
static void DeleteStdPkt			Args((struct StandardPacket *sp));
static LONG DoStdPkt				Args((struct StandardPacket *sp,
						      struct MsgPort *mp,
						      LONG *pres2));
#endif
static struct Window *Open_cli_Win		Args((void));


extern struct DosLibrary	*DOSBase;


#ifdef AZTEC_C
#pragma regcall( DeleteStdPkt(a0))
#pragma regcall( /* d0 = */ DoStdPkt(a0,a1,a2))
#endif


static struct TagItem system_tags[] = {
			{ SYS_Input,	 NULL	},
			{ SYS_Output,	 NULL	},
			{ SYS_Asynch,	 NULL	},
			{ SYS_UserShell, TRUE 	},
			{ NP_Priority,   0    	},
			{ NP_StackSize,  4096 	},
			{ NP_WindowPtr,  NULL 	},
			{ TAG_DONE,      NULL 	},
                        { TAG_DONE,	 0L	}
  };




void execute_script(char *script, int new)
{
  char con[150], file[40], dir[60];
  int ret;

  if (!is_dvif) {
    Message(MSG_NO_DVI_FILE);
  }
  else {

    if (is_os2 && is_pubscr && script != NULL && win2 != NULL) {
	ret = GetVar("TEXFORMAT", m_string, 50, GVF_GLOBAL_ONLY);
	if (ret == -1 || new) {
	  ChangeFormatFile();
	}
	if (GetVar("ShowDVI-file", file, 39, GVF_LOCAL_ONLY) != -1 &&
	    GetVar("ShowDVI-dir", dir, 59, GVF_LOCAL_ONLY) != -1) {
		sprintf(con, "%s \"%s\" \"%s\" screenname=\"%s\"", script, file, dir, GetCurrentPubScr());
		if (start_command(GetTeXString(MSG_EXECUTE_TEX_SCRIPT), con, NULL) != 0) {
		  Warning(MSG_CANT_START, con);
		}
	}
    }
  }
}

void make_newcli(struct Screen *screen)
{
//	struct StandardPacket	*sp;
	struct Window		*w;
//	struct MsgPort		*mp;
//	struct Process		*pr;
	char 			con[90];
//	APTR			oldConsoleTask;
//	APTR			oldWindowPtr;
	BPTR			fh;
//	LONG			res2;


	if (is_os2) {
	  if (is_pubscr) {
		sprintf(con, "newshell \"CON:%d/%d/%d/%d/ShowDVI - new CLI/CLOSE/SCREEN %s\"",
		win2->LeftEdge+win2->BorderLeft+3, win2->TopEdge+2*win2->BorderTop+6,
		win2->Width-win2->BorderLeft-win2->BorderRight-42,
		(is_lace) ? 200 : 120, GetCurrentPubScr());
		system_tags[6].ti_Data = (ULONG)win2;
	  	(void)SystemTagList(con, &(system_tags[0]));
	  }
	  else {
	    if ((w = Open_cli_Win()) != NULL) {
		sprintf(con, "CON://////WINDOW 0x%0.p/CLOSE", w);
		if ((fh = Open(con, MODE_NEWFILE)) != ZERO) {
			(void)Execute("", fh, ZERO);
			Close(fh);
		        /** hier KEIN CloseWindow(w); das macht schon die Shell!! **/
		}
		else {
		  CloseWindow(w);	/* CON: ist nicht aufgegangen??? */
		}
	    }
	  }
	}
#if 0
	else {
	  if ((sp = CreateStdPkt()) != NULL) {
	    if ((w = Open_cli_Win()) != NULL) {
		if ((mp = NewConsole(w, DOSBase)) != NULL) {
		    pr = (struct Process *)FindTask(NULL);
		    oldConsoleTask	= pr->pr_ConsoleTask;
		    oldWindowPtr	= pr->pr_WindowPtr;
		    pr->pr_ConsoleTask	= (APTR)mp;
		    pr->pr_WindowPtr	= (APTR)w;

		    if ((fh = Open("*", MODE_NEWFILE)) != ZERO) {
			sp->sp_Pkt.dp_Type	= ACTION_SCREEN_MODE;
			sp->sp_Pkt.dp_Arg1	= DOSFALSE;
			(void)DoStdPkt(sp, mp, &res2);
			(void)Execute("", fh, ZERO);
			Close(fh);
		    }

		    pr->pr_ConsoleTask	= oldConsoleTask;
		    pr->pr_WindowPtr	= oldWindowPtr;
		    if (fh == ZERO) {
			sp->sp_Pkt.dp_Type	= ACTION_END;
			(void)DoStdPkt(sp, mp, &res2);
		    }
		  }
		}
		else {
		  CloseWindow(w);
		}
		DeleteStdPkt(sp);
	  }
	}
#endif
}



static struct Window *Open_cli_Win(void)
{
  struct NewWindow nw;

  nw.LeftEdge	= 5;
  nw.TopEdge	= 25;
  nw.Width	= screen->Width-30;
  nw.Height	= (is_lace) ? 280 : 160;
  nw.DetailPen	= 0;
  nw.BlockPen	= 1;
  nw.IDCMPFlags	= 0;
  nw.Flags	= WINDOWSIZING | WINDOWDRAG | SIZEBBOTTOM | WFLG_DEPTHGADGET |
		  SMART_REFRESH | NOCAREREFRESH | ACTIVATE |
		  ((is_os2) ? WINDOWCLOSE : 0);
  nw.FirstGadget= NULL;
  nw.CheckMark	= NULL;
  nw.Title	= GetTeXString(MSG_NEWCLI_TITLE);
  nw.Screen	= screen;
  nw.BitMap	= NULL;
  nw.MinWidth	= 100;
  nw.MinHeight	= 50;
  nw.MaxWidth	= screen->Width;
  nw.MaxHeight	= screen->Height;
  nw.Type	= CUSTOMSCREEN;
  
  return OpenWindow(&nw);
}

#if 0

static struct StandardPacket *CreateStdPkt(void)
{
	struct StandardPacket	*sp;

	if ((sp = (struct StandardPacket *)
		AllocMem(sizeof(struct StandardPacket), MEMF_PUBLIC)) != NULL) {
		if ((sp->sp_Msg.mn_ReplyPort = CreatePort(NULL, 0)) != NULL) {
			sp->sp_Msg.mn_Node.ln_Name	= (char *)&sp->sp_Pkt;
			sp->sp_Pkt.dp_Link		= &sp->sp_Msg;

			return sp;
		}
		FreeMem((APTR)sp, sizeof(struct StandardPacket));
	}
	return NULL;
}

static void DeleteStdPkt(struct StandardPacket *sp)
{
	DeletePort(sp->sp_Msg.mn_ReplyPort);
	FreeMem((APTR)sp, sizeof(struct StandardPacket));
}

static LONG DoStdPkt(struct StandardPacket *sp, struct MsgPort *mp, LONG *pres2)
{
	sp->sp_Pkt.dp_Port	= sp->sp_Msg.mn_ReplyPort;

	PutMsg(mp, &sp->sp_Msg);
	(void)WaitPort(sp->sp_Msg.mn_ReplyPort);
	(void)GetMsg(sp->sp_Msg.mn_ReplyPort);

	*pres2	= sp->sp_Pkt.dp_Res2;

	return sp->sp_Pkt.dp_Res1;
}

#endif
