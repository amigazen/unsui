/* app_win.c */

#include "defines.h"

#include <exec/types.h>
#include <intuition/intuitionbase.h>
#include <workbench/startup.h>
#include <workbench/workbench.h>

#include <stdio.h>
#include <string.h>

#include <clib/exec_protos.h>
#include <clib/intuition_protos.h>
#include <clib/icon_protos.h>
#include <clib/wb_protos.h>
#include <clib/dos_protos.h>

#include <pragmas/exec_pragmas.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/icon_pragmas.h>
#include <pragmas/wb_pragmas.h>
#include <pragmas/dos_pragmas.h>


#include "globals.h"
#include "amscreen.h"
#include "globals.i"
#include "amscreen.i"
#include "showdvi.i"
#include "app_win.i"


char *app_icon_name;				/* path/name of the icon */

extern struct Library		*WorkbenchBase;
extern struct Library		*IconBase;
extern struct IntuitionBase	*IntuitionBase;
extern struct GfxBase		*GfxBase;
extern struct DosLibrary	*DOSBase;

static struct MsgPort   *appMsgPort = NULL;	/* Vorbelegung ist WICHTIG! */
static struct Window    *win        = NULL;
static struct AppWindow *aw         = NULL;

static ULONG secs;				/* time of last click */
static ULONG mics;


static struct Gadget dragit =
   { 0,0, 0, 0, 0,GADGHNONE|GRELWIDTH,GADGIMMEDIATE,
   WDRAGGING, 0,0,0,0,0,0,0 };

static struct NewWindow NewWI = {
   0, 1, 0, 0, 2,0, GADGETDOWN,
   SMART_REFRESH | BORDERLESS | NOCAREREFRESH,
   NULL, NULL, NULL, NULL, NULL, 0,0,0,0, WBENCHSCREEN
};




BOOL setup_app_win(UBYTE *app_sig, UBYTE *appwin_sig)
{
  struct DiskObject *diskobj;
  ULONG id = 1, userdata = 0;
  BOOL ret = TRUE;
  char ai_name[256], *ptr;

  appMsgPort = NULL;
  win = NULL;
  aw = NULL;

  if ((appMsgPort = CreateMsgPort()) == NULL) {
    ret = FALSE;
  }
  else {
    strcpy(ai_name, app_icon_name);	/* damit ich nicht in einem constanten String aendere */
    if ((ptr = strrchr(ai_name,'.')) != NULL) {
      if (stricmp(ptr,".info") == 0) {
        *ptr = '\0';
      }
    }
    if ((diskobj = GetDiskObject(ai_name)) == NULL) {
      clear_app_win();
      ret = FALSE;
    }
    else {
      dragit.Height = NewWI.Height = diskobj->do_Gadget.Height;
      NewWI.Width  = diskobj->do_Gadget.Width;
      if (diskobj->do_CurrentX >= 0) {
	NewWI.LeftEdge = diskobj->do_CurrentX;
      }
      if (diskobj->do_CurrentY >= 0) {
	NewWI.TopEdge = diskobj->do_CurrentY;
      }

      if ((win = OpenWindow(&NewWI)) == NULL) {
        clear_app_win();
        ret = FALSE;
      }
      else {
	AddGList(win,&dragit,0,1,0);
	RefreshGadgets(&dragit,win,0);
	if ((aw = AddAppWindowA(id, userdata, win, appMsgPort, NULL)) == NULL) {
	  clear_app_win();
	  ret = FALSE;
	}
	else {
          DrawImage(win->RPort,(diskobj->do_Gadget.GadgetRender),0,0);
	  *app_sig    = appMsgPort->mp_SigBit;
	  *appwin_sig = win->UserPort->mp_SigBit;
        }
      }
      FreeDiskObject(diskobj);
    }
  }

  if (!ret) {
    Warning("Can't setup app-window!");
    unset_appwin;
  }

  secs = 0;
  mics = 0;
  
  return ret;
}


void clear_app_win(void)
{
  struct AppMessage *amsg;

  if (aw != NULL) {
    RemoveAppWindow(aw);
    aw = NULL;
  }
  if (win != NULL) {
    RemoveGadget(win,&dragit);
    CloseWindow(win);
    win = NULL;
  }
  if (appMsgPort != NULL) {
    while(amsg = (struct AppMessage *)GetMsg(appMsgPort)) {
      ReplyMsg((struct Message *)amsg);
    }
    DeleteMsgPort(appMsgPort);
    appMsgPort = NULL;
  }
  unset_appwin;
}


long work_with_app_win(void)
{
  struct IntuiMessage *imess;
  struct AppMessage *amsg;
  struct WBArg *arg;
  char file[256];
  char *ptr;
  ULONG sec2,mic2;
  BOOL double_click;
  long ret = 0L;			/* no action */
  BPTR lock;

  if (!is_appwin) {
    return 0L;
  }

  if (win == NULL || aw == NULL || appMsgPort == NULL) {
    Fatal(20,"internal error with app-win!");
  }

  /* first look for normal window messages */
  while (imess = (struct IntuiMessage *)GetMsg(win->UserPort)) {
    sec2 = imess->Seconds;
    mic2 = imess->Micros;
    ReplyMsg(&(imess->ExecMessage));
    double_click = DoubleClick(secs,mics,sec2,mic2);
    secs = sec2;
    mics = mic2;
    if (double_click) {
      ScreenToFront(screen);
      make_show_active();
    }
  }

  /* and now, look for app-messages */
  while (ret == 0L && (amsg = (struct AppMessage *)GetMsg(appMsgPort))) {
    if (amsg->am_NumArgs == 1) {
      arg = amsg->am_ArgList;               /* Argumentpointer bestimmen */
      if (*arg->wa_Name != '\0') {
        /* new file is now: 'arg->wa_Name' */
        if (NameFromLock(arg->wa_Lock, file, 256)) {
          if (AddPart(file, arg->wa_Name, 256)) {
            if (stricmp(file, filename) == 0) {	/* same file again */
              ret = KOMM + 3;
            }
            else {
              if ((ptr = strrchr(file, '.')) != NULL) {
                ptr++;
                if (stricmp(ptr, "dvi") == 0) {
                  strcpy(filename, file);
                  ret = KOMM + 4;			/* load new file */
                }
                else if (stricmp(ptr, "tex") == 0) {
                  strcpy(ptr, "dvi");			/* ueberschreibe das .tex */
		  lock = Lock(file, ACCESS_READ);
		  if (lock != NULL) {
                    strcpy(filename, file);
                    ret = KOMM + 4;			/* load new file */
                    UnLock(lock);
                  }
                }
              }
            }
          }
        }
      }
    }
    ReplyMsg((struct Message *) amsg);
  }

  if (ret != 0L) {
    OpenNewDVI(filename, FALSE);
    ScreenToFront(screen);
    make_show_active();
  }

  return ret;
}


BOOL save_app_win_pos(void)
{
  struct DiskObject *diskobj;
  char ai_name[256], *ptr;

  if (win == NULL || !is_appwin) {
    return TRUE;
  }

  strcpy(ai_name, app_icon_name);	/* damit ich nicht in einem constanten String aendere */
  if ((ptr = strrchr(ai_name,'.')) != NULL) {
    if (stricmp(ptr,".info") == 0) {
      *ptr = '\0';
    }
  }
  if ((diskobj = GetDiskObject(ai_name)) == NULL) {
    return FALSE;
  }
  else {
    /* diskobj->do_Gadget.Height = win->Height; */
    /* diskobj->do_Gadget.Width = win->Width; */
    diskobj->do_CurrentX = win->LeftEdge;
    diskobj->do_CurrentY = win->TopEdge;
    if (!PutDiskObject(ai_name, diskobj)) {
      FreeDiskObject(diskobj);
      return FALSE;
    }
    FreeDiskObject(diskobj);
  }
  return TRUE;
}
