#include "defines.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dos.h>


#include <exec/types.h>
#include <exec/memory.h>
#include <exec/execbase.h>
#include <exec/libraries.h>
#include <intuition/screens.h>
#include <intuition/intuition.h>
#include <graphics/gfx.h>
#include <graphics/text.h>
#include <libraries/amigaguide.h>
#include <libraries/gadtools.h>
#include <utility/hooks.h>
#include <utility/tagitem.h>

#include <clib/alib_protos.h>
#include <clib/amigaguide_protos.h>
#include <clib/dos_protos.h>
#include <clib/exec_protos.h>
#include <clib/gadtools_protos.h>
#include <clib/graphics_protos.h>
#include <clib/intuition_protos.h>
#include <clib/utility_protos.h>

#include <pragmas/amigaguide_pragmas.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/gadtools_pragmas.h>
#include <pragmas/graphics_pragmas.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/utility_pragmas.h>

#include "globals.h"
#include "amscreen.h"
#include "gad_def.h"
#include "prefwin.h"

#include "globals.i"
#include "am_requ.i"
#include "help.i"


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


extern ULONG	 sig_amigaguide_long;

extern struct Window	* MessWin;
extern struct Window	* PrefWin;

#define ASM		__asm __saveds
#define REG(x)		register __ ## x

#define BASENAME	"SHOWDVI"
#define BASENAME_LENGTH	7



STRPTR AIcontext[] =
{
    "MAIN",
    "STATUS",
    NULL
};

#define WWIN_NONE	0
#define WWIN_SDVI	1
#define WWIN_PREF	2
#define WWIN_MESS	3


struct AppInfo
{
    unsigned long                ai_RegA4;		/* REGISTER A4 (erspart __saveds) */

    struct Process		*ai_Process;		/* Our process address */
    struct Screen		*ai_Screen;		/* Screen that our application will open on */
    struct Window		*ai_Window;		/* Window pointer */
    
    /* Help related information */
    LONG			 ai_NHID;		/* Unique id */
    ULONG			 ai_NHFlags;		/* Help related flags */
    UBYTE			 ai_NHName[64];		/* Unique name */
    struct Hook			 ai_NHHook;		/* Dynamic node host hook */
    struct AmigaGuideHost	*ai_NH;			/* Dynamic node host */
    AMIGAGUIDECONTEXT		 ai_AmigaGuide;		/* Pointer to the AmigaGuide context */
    struct NewAmigaGuide	 ai_NAG;		/* Used to start AmigaGuide */
    LONG			 ai_Region;		/* Region that the mouse if over */
    LONG			 ai_WhichWin;		/* über welchem Fenster... */

    /* Control information */
    BOOL			 ai_Done;		/* Done yet? */
};

struct AppInfo * AiPtr = NULL;


#define	AGHF_CONTINUOUS	(1L<<0)


extern struct Library * AmigaGuideBase;



static VOID DisplayError (LONG err)
{
    MessageStr("AmigaGuide: %s", GetAmigaGuideString (err));
}

STRPTR nodeNames[] =
{
    "WINDOW",
    "PROCESS",
    NULL,
};

STRPTR nodeTitles[] =
{
    "Project Window Information",
    "Project Process Information",
};

enum
{
    NM_WINDOW,
    NM_PROCESS,
};

static LONG Tokenize (struct AppInfo *ai, STRPTR name)
{
    UBYTE buffer[32];
    LONG i;

    /* Chop the prefix from the node name */
    strcpy (buffer, &name[strlen(BASENAME)+1]);

    /* Find the token */
    for (i = 0; nodeNames[i]; i++)
	if (Stricmp (buffer, nodeNames[i]) == 0)
	    return i;
    return -1;
}


ULONG __asm nodehost (REG (a0) struct Hook *h, REG (a2) STRPTR o, REG (a1) Msg msg)
{
    struct AppInfo *ai = (struct AppInfo *) h->h_Data;
    struct opFindHost *ofh = (struct opFindHost *)msg;
    struct opNodeIO *onm = (struct opNodeIO *)msg;
    ULONG retval = FALSE;
    LONG token;
    ULONG id;

    putreg(REG_A4, ai->ai_RegA4);
    
    D(bug("NODEHOST Node %s\n", ofh->ofh_Node));

    switch (msg->MethodID)
    {
	case HM_FINDNODE:
	    /* Get the help group */
	    id = GetTagData (AGA_HelpGroup, 0L, ofh->ofh_Attrs);

	    /* Request for the initial main node? */
	    if (id == 0)
	    {
		/* We have to have a MAIN node, even if we never use it */
		if (Stricmp (ofh->ofh_Node, "main") == 0)
		{
		    /* Show that it is ours */
		    retval = TRUE;
		}
	    }
	    /* Request for one of our true dynamic nodes? */
	    else if (id == ai->ai_NHID)
	    {
		if ((Strnicmp (ofh->ofh_Node, BASENAME, BASENAME_LENGTH) == 0) &&
		    ((token = Tokenize (ai, ofh->ofh_Node)) >= 0))
		{
		    /* Show that it is ours */
		    retval = TRUE;

		    /* Set the node title */
		    ofh->ofh_Title = nodeTitles[token];
		}
	    }
	    break;

	case HM_OPENNODE:
	    /* Allocate the document buffer */
	    if (onm->onm_DocBuffer = AllocVec (512, MEMF_CLEAR))
	    {
		/* Build the document */
		switch (token = Tokenize (ai, onm->onm_Node))
		{
		    case NM_WINDOW:
			sprintf (onm->onm_DocBuffer, "Window: %08lx\n  Size: %ld, %ld, %ld, %ld\n Group: %ld\n",
				 ai->ai_Window,
				 (LONG) ai->ai_Window->LeftEdge, (LONG) ai->ai_Window->TopEdge,
				 (LONG) ai->ai_Window->Width, (LONG) ai->ai_Window->Height,
				 ai->ai_NHID);
			break;

		    case NM_PROCESS:
			sprintf (onm->onm_DocBuffer, "Process: %08lx\n",
				 ai->ai_Process);
			break;
		}

		/* Set the buffer length */
		onm->onm_BuffLen = strlen (onm->onm_DocBuffer);

		/* Remove the node from the database when done */
		onm->onm_Flags |= HTNF_CLEAN;

		/* Show successful open */
		retval = TRUE;
	    }
	    break;

	case HM_CLOSENODE:
	    /* Free the document buffer */
	    FreeVec (onm->onm_DocBuffer);
	    retval = TRUE;
	    break;

	case HM_EXPUNGE:
	    break;
    }

    return retval;
}




void InitHelpFirst(void)
{
  struct AppInfo * ai;
 
  if (!AmigaGuideBase) return;

  AiPtr = ai = (struct AppInfo *)AllocVec (sizeof (struct AppInfo), MEMF_CLEAR);
  if (!ai) Fatal(10, MSG_NO_MEM);

  ai->ai_RegA4 = getreg(REG_A4);

  /* Our process */
  ai->ai_Process = (struct Process *) FindTask (NULL);

  /* Initialize the node host hook */
  ai->ai_NHHook.h_Entry = (ULONG (*)()) nodehost;
  ai->ai_NHHook.h_Data  = ai;

  /* Build the unique name */
  ai->ai_NHID = GetUniqueID();
  sprintf (ai->ai_NHName, "%s.%ld", BASENAME, ai->ai_NHID);

  /* Start the Dynamic Node Host */
  ai->ai_NH = AddAmigaGuideHost (&ai->ai_NHHook, ai->ai_NHName, NULL);

  /* Initialize the global data */
  ai->ai_Region = -1;
  ai->ai_AmigaGuide = NULL;
  ai->ai_WhichWin = WWIN_NONE;
}

void FreeHelpLast(void)
{
  if (AiPtr) {
    if (AiPtr->ai_NH) {
      while (RemoveAmigaGuideHost(AiPtr->ai_NH, NULL) > 0)
        Delay (10);
    }
  
    FreeVec(AiPtr);
  }
  AiPtr = NULL;
}


long GetHelpNum(void)
{
  if (AiPtr) {
    return AiPtr->ai_NHID;
  }
  else {
    return -1;
  }
}




void StartHelp(struct Screen * scr, struct Window * win)
{
  if (!AiPtr) return;

  AiPtr->ai_Screen = scr;
  AiPtr->ai_Window = win;

  /* Turn on gadget help */
  HelpControl (AiPtr->ai_Window, HC_GADGETHELP);

  /* Remember the AppInfo */
  AiPtr->ai_Window->UserData = (APTR) AiPtr;

  /* We don't want the window to automatically activate */
  AiPtr->ai_NAG.nag_Flags = HTF_NOACTIVATE;

  /* auf diesem Screen soll AmigaGuide aufgehen */
  AiPtr->ai_NAG.nag_Screen = scr;

  /* Set the application base name */
  AiPtr->ai_NAG.nag_BaseName = BASENAME;

  /* Set the document name */
  AiPtr->ai_NAG.nag_Name = "TeX:config/ShowDVI.guide";

  /* establish the base name to use for hypertext ARexx port */
  AiPtr->ai_NAG.nag_ClientPort = "SHOWDVI_HELP";

  /* Set up the context table */
  AiPtr->ai_NAG.nag_Context = AIcontext;

  /* Open the help system */
  AiPtr->ai_AmigaGuide = OpenAmigaGuideAsync (&AiPtr->ai_NAG,
					     AGA_HelpGroup, AiPtr->ai_NHID,
					     TAG_DONE);

  /* Signal-Maske holen */
  sig_amigaguide_long = AmigaGuideSignal (AiPtr->ai_AmigaGuide);

  /* Clear the AmigaGuide context */
  SetAmigaGuideContext (AiPtr->ai_AmigaGuide, 0L, NULL);

  /* alles ist bereit... */
  set_amigaguide;
}



void StopHelp(void)
{
  if (AiPtr && AiPtr->ai_AmigaGuide && AiPtr->ai_Window) {
    /* Shutdown the help system */
    CloseAmigaGuide(AiPtr->ai_AmigaGuide);
    AiPtr->ai_AmigaGuide = NULL;
    AiPtr->ai_Window = NULL;  // !!
    unset_amigaguide;
  }
}


/* ein AmigaGuide Signal ist angekommen */
long work_with_help(void)
{
  long ex = 0;
  struct AmigaGuideMsg *agm;
  
  if (!(is_amigaguide && AiPtr)) return ex;
	  
  
  /* process amigaguide messages */
  while (agm = GetAmigaGuideMsg (AiPtr->ai_AmigaGuide)) {

    /* check message types */
    switch (agm->agm_Type) {
	/* AmigaGuide is ready for us */
      case ActiveToolID:
	break;

	/* This is a reply to our cmd */
      case ToolCmdReplyID:
	if (agm->agm_Pri_Ret)
	    DisplayError (agm->agm_Sec_Ret);
	break;

	/* This is a status message */
      case ToolStatusID:
	if (agm->agm_Pri_Ret)
	    DisplayError (agm->agm_Sec_Ret);
	break;

	/* Shutdown message */
      case ShutdownMsgID:
	if (agm->agm_Pri_Ret)
	    DisplayError (agm->agm_Sec_Ret);
	break;

      default:
	break;
    }

    /* Reply to the message */
    ReplyAmigaGuideMsg (agm);
  }
  
  return ex;
}


long work_with_gadgethelp(struct IntuiMessage * imsg)
{
  long ex = 0;  
  long region, qhelp;
  long isgad = FALSE;
  
  if (!(is_amigaguide && AiPtr)) return ex;

  qhelp = region = -1;
  if (imsg->IAddress == NULL) {
    /* Not over our window */
    AiPtr->ai_WhichWin = WWIN_NONE;
    AiPtr->ai_Region = -1;
    D(bug("HELP: Irgendwo...aber nicht bei uns\n"));
  }
  else if (imsg->IAddress == (APTR)AiPtr->ai_Window) {
    /* Over our window */
    qhelp = region = 0;
    AiPtr->ai_WhichWin = WWIN_SDVI;
    AiPtr->ai_Region = 0;
    D(bug("HELP: Irgendwo über unserem Fenster\n"));
  }
  else if (MessWin && imsg->IAddress == (APTR)MessWin) {
    qhelp = region = 0;
    AiPtr->ai_WhichWin = WWIN_MESS;
    AiPtr->ai_Region = 0;
    D(bug("HELP: Irgendwo über dem MessWin Fenster\n"));
  }
  else if (PrefWin && imsg->IAddress == (APTR)PrefWin) {
    qhelp = region = 0;
    AiPtr->ai_WhichWin = WWIN_PREF;
    AiPtr->ai_Region = 0;
    D(bug("HELP: Irgendwo über dem PrefWin Fenster\n"));
  }
  else {

    /*
     * Detect system gadgets.  Figure out by looking at
     * system-gadget-type bits in GadgetType field:
     */

    LONG sysgtype = ((struct Gadget *) imsg->IAddress)->GadgetType & 0xF0;
    
    isgad = TRUE;

    /* Set the region */
    qhelp = (sysgtype >> 4) + 5;
    region = HTFC_SYSGADS + sysgtype;

    switch (sysgtype) {
      case GTYP_SIZING:
      case GTYP_WDRAGGING:
      case GTYP_WUPFRONT:
      case GTYP_WDOWNBACK:
      case GTYP_CLOSE:
	break;

      case 0:
        qhelp = region = (LONG) ((struct Gadget *)imsg->IAddress)->GadgetID;
	break;

      default:
	break;
    }

    /* Remember the region */
    AiPtr->ai_Region = region;

  }
  
  /* Send the command immediately if we are continuous */
  if ((isgad || (AiPtr->ai_Region >= 0)) && (AiPtr->ai_NHFlags & AGHF_CONTINUOUS)) {
    /* Display the node */
    HelpKeyAGuide(FALSE);
  }


  return ex;
}


void do_menu_help(long sel, int checked, int enabeled, int is_barlabel)
{
  static char mnode[64];

  if (is_amigaguide && AiPtr) {
    /* Display the node */
    long men  = MENUNUM(sel);
    long item = ITEMNUM(sel);
    long sub  = SUBNUM(sel);
    if (is_barlabel) {
      strcpy(mnode, "link menu_barlabel");
    }
    else if (sub != NOSUB) {
      sprintf(mnode, "link menu_%d_%d_%d", men, item, sub);
    }
    else {
      if (item != 63) {
        sprintf(mnode, "link menu_%d_%d", men, item);
      }
      else {
        sprintf(mnode, "link menu_%d", men);
      }
    }

    D(bug("Zeige Node %s (Menü)\n", mnode));
    SendAmigaGuideCmd (AiPtr->ai_AmigaGuide, mnode, TAG_DONE);
  }
}

static char * prefwincontext[] = {
  "xsize", "ysize", "lace", "default", "dvisze", "use", "cancel", "ownscr",
  "4col", "scrmode", "scrmname", NULL
};

void HelpKeyAGuide(int conthelp)
{
  static char hnode[64];
  
  if (conthelp) {
    /* toggle */
    if (AiPtr->ai_NHFlags & AGHF_CONTINUOUS) AiPtr->ai_NHFlags &= (~AGHF_CONTINUOUS);
    else  				     AiPtr->ai_NHFlags |= AGHF_CONTINUOUS;
  }
  
  if (is_amigaguide && AiPtr) {
    if (AiPtr->ai_Region == -1) {
      strcpy(hnode, "link Main");
    }
    else if (AiPtr->ai_Region == 0) {
      switch (AiPtr->ai_WhichWin) {
        case WWIN_SDVI:
          strcpy(hnode, "link ShowWin");
          break;
        case WWIN_PREF:
          strcpy(hnode, "link PrefWin");
          break;
        case WWIN_MESS:
          strcpy(hnode, "link MessWin");
          break;
        default:
          strcpy(hnode, "link Main");
      }
    }
    else {
      if (AiPtr->ai_Region >= HTFC_SYSGADS) {
        sprintf(hnode, "link SGAD_%d", (AiPtr->ai_Region - HTFC_SYSGADS) >> 5);
      }
      else {
        if (AiPtr->ai_Region >= PG_POT_GAD_NR && AiPtr->ai_Region <= PG_INT_GAD_NR) {
          strcpy(hnode, "link PAGEGAD");
        }
        else if (AiPtr->ai_Region >= POTX_PFEIL_LINKS_GAD_NR && AiPtr->ai_Region <= POTX_GAD_NR) {
          strcpy(hnode, "link POTXGAD");
        }
        else if (AiPtr->ai_Region >= POTY_PFEIL_OBEN_GAD_NR && AiPtr->ai_Region <= POTY_GAD_NR) {
          strcpy(hnode, "link POTYGAD");
        }
        else if (AiPtr->ai_Region >= GAD_xsize && AiPtr->ai_Region <= GAD_scrmname) {
          sprintf(hnode, "link PREF_%s", prefwincontext[AiPtr->ai_Region - GAD_xsize]);
        }
        else {
          sprintf(hnode, "link GAD%d", AiPtr->ai_Region);
        }
      }
    }
    
    D(bug("Zeige Node %s (Gadget/Window)\n", hnode));
    SendAmigaGuideCmd(AiPtr->ai_AmigaGuide, hnode, TAG_DONE);
  }
}

