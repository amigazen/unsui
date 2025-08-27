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
#include "app_icon.i"


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



char *app_icon_name;				/* path/name of the icon */

extern struct Library		*WorkbenchBase;
extern struct Library		*IconBase;
extern struct IntuitionBase	*IntuitionBase;
extern struct GfxBase		*GfxBase;
extern struct DosLibrary	*DOSBase;

static struct MsgPort   *appMsgPort = NULL;	/* Vorbelegung ist WICHTIG! */
static struct AppIcon   *ai         = NULL;





BOOL setup_app_icon(UBYTE *app_icon_sig)
{
  struct DiskObject *diskobj;
  ULONG id = 1, userdata = 0;
  BOOL ret = TRUE;
  char ai_name[256], *ptr;

  appMsgPort = NULL;
  ai = NULL;

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
      clear_app_icon();
      ret = FALSE;
    }
    else {
      if (show_state.AppIconX != 0) {
        diskobj->do_CurrentX = show_state.AppIconX;
      }
      else {
        diskobj->do_CurrentX = NO_ICON_POSITION;
      }
      if (show_state.AppIconY != 0) {
        diskobj->do_CurrentY = show_state.AppIconY;
      }
      else {
        diskobj->do_CurrentY = NO_ICON_POSITION;
      }
      if ((ai = AddAppIconA(id, userdata, "ShowDVI", appMsgPort, NULL, diskobj, NULL)) == NULL) {
        clear_app_icon();
        ret = FALSE;
      }
      else {
        *app_icon_sig    = appMsgPort->mp_SigBit;
      }
      FreeDiskObject(diskobj);
    }
  }

  if (!ret) {
    Warning(MSG_NO_APP_ICON);
    unset_appwin;
  }

  return ret;
}


void clear_app_icon(void)
{
  struct AppMessage *amsg;

  if (ai != NULL) {
    RemoveAppIcon(ai);
    ai = NULL;
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


long work_with_app_icon(void)
{
  struct AppMessage *amsg;
  struct WBArg *arg;
  long ret = 0L;			/* no action */

  if (!is_appwin) {
    return 0L;
  }

  if (ai == NULL || appMsgPort == NULL) {
    Fatal(20,MSG_INTERNAL_ERROR_W_ARG, "Application Icon");
  }


  /* and now, look for app-messages */
  while (ret == 0L && (amsg = (struct AppMessage *)GetMsg(appMsgPort))) {
    if (amsg->am_NumArgs == 1) {
      arg = amsg->am_ArgList;               /* Argumentpointer bestimmen */
      ret = CheckAppArgName(arg->wa_Name, arg->wa_Lock);
    }
    else {
      if (amsg->am_NumArgs == 0) { /* doppelklick */
        ScreenToFront(screen);
        make_show_active();
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


long CheckAppArgName(char * name, BPTR lock)
{
  char file[256];
  char *ptr;
  BPTR tmplock;
  long ret = 0;

  if (*name != '\0') {
    /* new file is now: 'arg->wa_Name' */
    if (NameFromLock(lock, file, 256)) {
      if (AddPart(file, name, 256)) {
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
            else {
              if (stricmp(ptr, "tex") == 0) {
                strcpy(ptr, "dvi");			/* ueberschreibe das .tex */
	 	tmplock = Lock(file, ACCESS_READ);
		if (tmplock != NULL) {
                  strcpy(filename, file);
                  ret = KOMM + 4;			/* load new file */
                  UnLock(tmplock);
                }
              }
            }
          }
        }
      }
    }
  }
  
  return ret;
}


BOOL save_app_icon_pos(void)
{
#if 0
  struct DiskObject *diskobj;
  char ai_name[256], *ptr;
#endif

  return TRUE;	/* das funktioniert mit einem app_icon nicht! */

#if 0
  if (ai == NULL || !is_appwin) {
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
#endif
}
