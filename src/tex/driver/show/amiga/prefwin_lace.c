#include "defines.h"

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <exec/types.h>
#include <exec/lists.h>
#include <intuition/intuition.h>
#include <intuition/screens.h>
#include <intuition/gadgetclass.h>
#include <libraries/gadtools.h>

#include <clib/intuition_protos.h>
#include <clib/dos_protos.h>
#include <clib/graphics_protos.h>
#include <clib/gadtools_protos.h>

#include <pragmas/intuition_pragmas.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/graphics_pragmas.h>
#include <pragmas/gadtools_pragmas.h>


#ifdef AZTEC_C
#  include <functions.h>
#endif


#include "globals.h"
#include "amscreen.h"

#include "amscreen.i"
#include "am_requ.i"
#include "prefwin.i"
#include "globals.i"
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



/*******************************************************************************/
/*			     P R E F W I N D O W			       */
/*******************************************************************************/


#define INTERLACE_KNOB


/* externals... */
extern UBYTE		 sig_prefwin;
extern struct Screen	*screen;
extern struct Window	*win2;
extern struct TextAttr	 txtAttr;
extern short		 HeightWintitle;


/* defines... */
#define NG_xsize	(NGAry + 0)
#define NG_ysize	(NGAry + 1)
#define NG_lace		(NGAry + 2)
#define NG_default	(NGAry + 3)
#define NG_dvisize	(NGAry + 4)
#define NG_use		(NGAry + 5)
#define NG_cancel	(NGAry + 6)
#define NG_ownscr	(NGAry + 7)
#define NG_4col		(NGAry + 8)
#define NG_scrmode	(NGAry + 9)
#define NG_scrmname	(NGAry + 10)

#include "prefwin.h"	// Gadget Nummern GAD_#?

#define ACTION_NONE		((short)0)
#define ACTION_WINWIDTH		((short)1)
#define ACTION_WINHEIGHT	((short)2)
#define ACTION_WINLACE		((short)3)
#define ACTION_WINDEFAULTSIZE	((short)4)
#define ACTION_WINDVISIZE	((short)5)
#define ACTION_WINUSE		((short)6)
#define ACTION_WINCANCEL	((short)7)
#define ACTION_WINOWNSCR	((short)8)
#define ACTION_WIN4COL		((short)9)
#define ACTION_WINSCRMODE	((short)10)
#define ACTION_WINSCRMNAME	((short)11)



#define PREFWIN_WIDTH  350
#define PREFWIN_HEIGHT 207


/* globals (help.c) */
struct Window	*PrefWin = NULL;	/* = NULL wichtig, damit wird gearbeitet! */


/* locals... */
static struct Gadget	*GList = NULL;

struct MyNewGadget {
	struct NewGadget new;
	short LeftEdge, TopEdge;
	short Width, Height;
	};

static struct MyNewGadget NGAry[] = {
  {{0,0,0,0,NULL,NULL,GAD_xsize,PLACETEXT_LEFT,(void *)0L,(APTR)ACTION_WINWIDTH },       125,114, 50,20 },
  {{0,0,0,0,NULL,NULL,GAD_ysize,PLACETEXT_LEFT,(void *)0L,(APTR)ACTION_WINHEIGHT },      125,141, 50,20 },
  {{0,0,0,0,NULL,NULL,GAD_lace,PLACETEXT_LEFT,(void *)0L,(APTR)ACTION_WINLACE },         125,168, 50,20 },
  {{0,0,0,0,NULL,NULL,GAD_default,PLACETEXT_IN,(void *)0L,(APTR)ACTION_WINDEFAULTSIZE }, 190,114,135,20 },
  {{0,0,0,0,NULL,NULL,GAD_dvisize,PLACETEXT_IN,(void *)0L,(APTR)ACTION_WINDVISIZE },     190,141,135,20 },
  {{0,0,0,0,NULL,NULL,GAD_use,PLACETEXT_IN,(void *)0L,(APTR)ACTION_WINUSE },		  60,200, 80,20 },
  {{0,0,0,0,NULL,NULL,GAD_cancel,PLACETEXT_IN,(void *)0L,(APTR)ACTION_WINCANCEL },	 210,200, 80,20 },
  {{0,0,0,0,NULL,NULL,GAD_ownscr,PLACETEXT_RIGHT,(void *)0L,(APTR)ACTION_WINOWNSCR },    125, 33, 50,20 },
  {{0,0,0,0,NULL,NULL,GAD_4col,PLACETEXT_LEFT,(void *)0L,(APTR)ACTION_WIN4COL },         299,168, 50,20 },
  {{0,0,0,0,NULL,NULL,GAD_scrmode,PLACETEXT_IN,(void *)0L,(APTR)ACTION_WINSCRMODE },     125, 60,200,20 },
  {{0,0,0,0,NULL,NULL,GAD_scrmname,PLACETEXT_LEFT,(void *)0L,0 },			 125, 87,200,20 }
};

#define GADMAX	(sizeof(NGAry) / sizeof(struct MyNewGadget))


static struct Gadget *Gad_xsize;
static struct Gadget *Gad_ysize;
static struct Gadget *Gad_default;
static struct Gadget *Gad_dvisize;
static struct Gadget *Gad_use;
static struct Gadget *Gad_cancel;
static struct Gadget *Gad_lace = NULL;
static struct Gadget *Gad_ownscr;
static struct Gadget *Gad_4col;
static struct Gadget *Gad_scrmode;
static struct Gadget *Gad_scrmname;

static long new_is_lace;
static long new_is_ownscr;
static long new_is_4col;
static long new_screen_mode;

static ULONG TmpModeID;		// hier wird die DisplayID gespeichert

static long DisableLace = FALSE;



static struct TagItem	   NumberGadTags[] = {
					{GTIN_Number, 0},
					{GA_Disabled, FALSE},
					{GT_Underscore, '_'},
					{GTIN_MaxChars, 4},
					{TAG_DONE, 0L}
				};
static struct TagItem	   ButtomGadTags[] = {
					{GA_Disabled, FALSE},
					{GT_Underscore, '_'},
					{TAG_DONE, 0L}
				};
static struct TagItem	   ChangeNumberGadTags[] = {
					{GTIN_Number, 0},
					{TAG_DONE, 0L}
				};
static struct TagItem	   ChangeCheckGadTags[] = {
					{GTCB_Checked, 0},
					{TAG_DONE, 0L}
				};
static struct TagItem	   ChangeDisableGadTags[] = {
					{GA_Disabled, 0},
					{TAG_DONE, 0L}
				};
static struct TagItem	   TextGadTags[] = {
					{GTTX_Text, 0},
					{GTTX_Border, TRUE},
					{TAG_DONE, 0L}
				};
static struct TagItem	   ChangeTextGadTags[] = {
					{GTTX_Text, 0},
					{TAG_DONE, 0L}
				};


/* local functions... */
static struct Gadget *InitGads	(void);
static long ActionScreenPref	(short action, int *cont);
static void SetGads2ownscr      (void);			// verwendet new_is_onwnscr


/* functions... */

void OpenPrefWin(void)
{
  struct NewWindow new_win;
  WORD LeftEdge, TopEdge, Width, Height;

  if (is_prefwin) {
    return;
  }

  if (GadToolsBase == NULL || GadToolsBase->lib_Version < 37L) {
    MySimpleRequest(1, NULL, NULL, NULL, NULL, GetTeXString(MSG_PREFWIN_GADTOOLS));
    return;
  }
  
  GList = InitGads();

  Width  = PREFWIN_WIDTH;
  Height = PREFWIN_HEIGHT+win2->BorderTop;

  CenterWindow(&LeftEdge, &TopEdge, Width, Height);

  new_win.LeftEdge  = LeftEdge;
  new_win.TopEdge   = TopEdge;
  new_win.Width     = Width;
  new_win.Height    = Height;
  new_win.DetailPen = 0;
  new_win.BlockPen  = 1; 
  new_win.Title     = GetTeXString(MSG_PREFWIN_WIN_TITLE);
  new_win.Flags     = WFLG_DEPTHGADGET | WFLG_CLOSEGADGET | WFLG_DRAGBAR | WFLG_NOCAREREFRESH | WFLG_ACTIVATE | WFLG_RMBTRAP;
  new_win.IDCMPFlags = IDCMP_CLOSEWINDOW | IDCMP_VANILLAKEY | IDCMP_RAWKEY | BUTTONIDCMP | NUMBERIDCMP | CHECKBOXIDCMP | ((is_amigaguide) ? IDCMP_GADGETHELP : 0);
  new_win.Type      = CUSTOMSCREEN;
  new_win.FirstGadget = GList;
  new_win.CheckMark = NULL;
  new_win.Screen    = screen;
  new_win.BitMap    = NULL;
  
  if (is_amigaguide) {
    PrefWin = (struct Window *)OpenWindowTags(&new_win, WA_HelpGroupWindow, win2, TAG_DONE);
  }
  else {
    PrefWin = (struct Window *)OpenWindow(&new_win);
  }

  if (PrefWin == NULL) {
    MySimpleRequest(1, NULL, NULL, NULL, GetTeXString(MSG_SHOWDVI_MESSAGE), 
		GetTeXString(MSG_PREFWIN_CANT_OPEN));
    return;
  }


  TmpModeID = get_DisplayID();
  
  ChangeTextGadTags[0].ti_Data = (ULONG)GetModeIDName(INVALID_ID, &new_is_lace);
  GT_SetGadgetAttrsA(Gad_scrmname, PrefWin, NULL, ChangeTextGadTags);

#ifdef INTERLACE_KNOB
  ChangeCheckGadTags[0].ti_Data = (new_is_lace);	// schon fuer den Mode geaendert
  GT_SetGadgetAttrsA(Gad_lace, PrefWin, NULL, ChangeCheckGadTags);
#endif

  ChangeCheckGadTags[0].ti_Data = (is_col4);
  GT_SetGadgetAttrsA(Gad_4col, PrefWin, NULL, ChangeCheckGadTags);

  ChangeCheckGadTags[0].ti_Data = (is_ownscr);
  GT_SetGadgetAttrsA(Gad_ownscr, PrefWin, NULL, ChangeCheckGadTags);



  /* GT_RefreshWindow(PrefWin, NULL); */
  
  sig_prefwin = PrefWin->UserPort->mp_SigBit;
  new_is_lace   = is_lace;
  new_is_ownscr = is_ownscr;
  new_is_4col = is_col4;
  new_screen_mode = FALSE;
  
  set_prefwin;
}



void ClosePrefWin(void)
{
  if (!is_prefwin) {
    return;
  }

  if (PrefWin != NULL) {
    CloseWindow(PrefWin);
    PrefWin = NULL;
  }
  if (GList != NULL) {
    FreeGadgets(GList);
    GList = NULL;
  }
  
  unset_prefwin;
}



void TogglePrefWin(void)
{
  if (is_prefwin) {
    ClosePrefWin();
  }
  else {
    OpenPrefWin();
  }
}



long DoPrefWin(void)
{
  short action;
  struct IntuiMessage *msg;
  ULONG msg_class;
  UWORD	Code;
  UWORD Qualifier;
  struct Gadget *m_gad;
  int cont = TRUE;
  long ex = 0;
  
  if (PrefWin == NULL) {
    return ex;
  }
  
  while(cont && ex == 0 && PrefWin != NULL && (msg = GT_GetIMsg(PrefWin->UserPort)) != NULL) {
    msg_class	= msg->Class;
    Code  	= msg->Code;
    Qualifier	= msg->Qualifier;
    m_gad	= (struct Gadget *)(msg->IAddress);

    action = ACTION_NONE;

    switch (msg_class) {
      case IDCMP_GADGETHELP:
        work_with_gadgethelp(msg);
        break;

      case IDCMP_CLOSEWINDOW:
  	cont = FALSE;
  	break;

      case IDCMP_RAWKEY:
        if (Code == 95) HelpKeyAGuide(Qualifier & IEQUALIFIER_CONTROL);
        break;

      case IDCMP_VANILLAKEY:
        switch (toupper(Code)) {
	  case 13 :		/* RET */
	  case 'U':		/* use */
	    action = ACTION_WINUSE;
	    break;
	  case 27 :		/* ESC */
	  case 'X':		/* X (start/quit key for the pref-win) */
	  case 'C':		/* cancel */
	    action = ACTION_WINCANCEL;
	    break;
	  case 'W':		/* width */
	    if (new_is_ownscr) {
	      (void)ActivateGadget(Gad_xsize, PrefWin, NULL);
	    }
	    break;
	  case 'H':		/* height */
	    if (new_is_ownscr) {
	      (void)ActivateGadget(Gad_ysize, PrefWin, NULL);
	    }
	    break;

#ifdef INTERLACE_KNOB
	  case 'I':		/* interlace */
	    if (new_is_ownscr) {
	      new_is_lace = !new_is_lace;
	      ChangeCheckGadTags[0].ti_Data = (new_is_lace);
	      GT_SetGadgetAttrsA(Gad_lace, PrefWin, NULL, ChangeCheckGadTags);
	    }
	    break;
#endif

	  case 'O':		/* default */
	    if (new_is_ownscr) {
	      action = ACTION_WINDEFAULTSIZE;
	    }
	    break;
	  case 'D':		/* dvi page size */
	    if (new_is_ownscr && is_dvif) {
	      action = ACTION_WINDVISIZE;
	    }
	    break;
	  case 'S':		/* own screen */
	    new_is_ownscr = !new_is_ownscr;
	    ChangeCheckGadTags[0].ti_Data = (new_is_ownscr);
	    GT_SetGadgetAttrsA(Gad_ownscr, PrefWin, NULL, ChangeCheckGadTags);
	    SetGads2ownscr();		// ghoste oder nicht die anderen Gadgets
	    break;
	  case '4':		/* 4 col screen */
	    if (new_is_ownscr) {
	      new_is_4col = !new_is_4col;
	      ChangeCheckGadTags[0].ti_Data = (new_is_4col);
	      GT_SetGadgetAttrsA(Gad_4col, PrefWin, NULL, ChangeCheckGadTags);
	    }
	    break;
	  case 'M':
	    action = ACTION_WINSCRMODE;
	    break;
	  case 3  :		/* CTRL C */
	    CXBRK();
	    break;
        }
        break;
      case IDCMP_GADGETUP: 
        action = (short)m_gad->UserData;
        break;
    }

    GT_ReplyIMsg(msg);

    ex = ActionScreenPref(action, &cont);
  }
  
  return ex;
}



/*------------------------------------------------------------*/


static struct Gadget *InitGads(void)
{
    struct Gadget *gad, *glist;
    struct MyNewGadget *ng;
    struct TextAttr *txt;
    short i;
    WORD help;

    help = HeightWinTitle - 22;
    if (screen->Font->ta_YSize > 15 || TextLength(&(screen->RastPort), "m", 1) > 11) {
      txt = &txtAttr;
    }
    else {
      txt = screen->Font;
    }

    for (i = 0, ng = NGAry; i < GADMAX; ++i, ++ng) {
       ng->new.ng_GadgetText = GetTeXString(MSG_PREFWIN_GADARR_0 + i);
       ng->new.ng_VisualInfo = SDVI_VI;
       ng->new.ng_TextAttr = txt;
       ng->new.ng_TopEdge  = ng->TopEdge+help;
       ng->new.ng_LeftEdge = ng->LeftEdge;
       ng->new.ng_Width    = ng->Width;
       ng->new.ng_Height   = ng->Height;
    }

    glist = NULL;
    if ((gad = CreateContext(&glist)) == NULL) return NULL;

    NumberGadTags[0].ti_Data = screen->Width;
    NumberGadTags[1].ti_Data = !is_ownscr;	/* disable */
    Gad_xsize = gad = CreateGadgetA(INTEGER_KIND, gad, &(NG_xsize->new), NumberGadTags);
    if (gad == NULL) return(NULL);

    NumberGadTags[0].ti_Data = screen->Height;
    NumberGadTags[1].ti_Data = !is_ownscr;	/* disable */
    Gad_ysize = gad = CreateGadgetA(INTEGER_KIND, gad, &(NG_ysize->new), NumberGadTags);
    if (gad == NULL) return(NULL);

#ifdef INTERLACE_KNOB
    ButtomGadTags[0].ti_Data = DisableLace = !is_ownscr || is_numeric;	/* disable */
    Gad_lace = gad = CreateGadgetA(CHECKBOX_KIND, gad, &(NG_lace->new), ButtomGadTags);
    if (gad == NULL) return(NULL);
#endif

    ButtomGadTags[0].ti_Data = !is_ownscr;	/* disable */
    Gad_default = gad = CreateGadgetA(BUTTON_KIND, gad, &(NG_default->new), ButtomGadTags);
    if (gad == NULL) return(NULL);

    ButtomGadTags[0].ti_Data = !is_ownscr || !is_dvif;	/* disable */
    Gad_dvisize = gad = CreateGadgetA(BUTTON_KIND, gad, &(NG_dvisize->new), ButtomGadTags);
    if (gad == NULL) return(NULL);

    ButtomGadTags[0].ti_Data = FALSE;		/* disable */
    Gad_use = gad = CreateGadgetA(BUTTON_KIND, gad, &(NG_use->new), ButtomGadTags);
    if (gad == NULL) return(NULL);

    ButtomGadTags[0].ti_Data = FALSE;		/* disable */
    Gad_cancel = gad = CreateGadgetA(BUTTON_KIND, gad, &(NG_cancel->new), ButtomGadTags);
    if (gad == NULL) return(NULL);

    ButtomGadTags[0].ti_Data = FALSE;		/* disable */
    Gad_ownscr = gad = CreateGadgetA(CHECKBOX_KIND, gad, &(NG_ownscr->new), ButtomGadTags);
    if (gad == NULL) return(NULL);

    ButtomGadTags[0].ti_Data = !is_ownscr;	/* disable */
    Gad_4col = gad = CreateGadgetA(CHECKBOX_KIND, gad, &(NG_4col->new), ButtomGadTags);
    if (gad == NULL) return(NULL);

    ButtomGadTags[0].ti_Data = is_os21 && !is_ownscr;		/* disable */
    Gad_scrmode = gad = CreateGadgetA(BUTTON_KIND, gad, &(NG_scrmode->new), ButtomGadTags);
    if (gad == NULL) return(NULL);

    Gad_scrmname = gad = CreateGadgetA(TEXT_KIND, gad, &(NG_scrmname->new), TextGadTags);
    if (gad == NULL) return(NULL);

    return(glist);
}


static long ActionScreenPref(short action, int *cont)
{
  long width, height;
//  short lace;
  int use = FALSE;
  long ex = 0;

  switch (action) {
	case ACTION_NONE:
		break;
	case ACTION_WINWIDTH:
		break;
	case ACTION_WINHEIGHT:
		break;
	case ACTION_WINLACE:
	        if (new_is_ownscr) {
 		  new_is_lace = !new_is_lace;
 		}
		break;
	case ACTION_WINDEFAULTSIZE:
	        if (new_is_ownscr) {
                  struct Rectangle Rect;

                  if (QueryOverscan(TmpModeID, &Rect, OSCAN_TEXT)) {
		    ChangeNumberGadTags[0].ti_Data = width = Rect.MaxX - Rect.MinX + 1;
		    GT_SetGadgetAttrsA(Gad_xsize, PrefWin, NULL, ChangeNumberGadTags);
		    ChangeNumberGadTags[0].ti_Data = height = Rect.MaxY - Rect.MinY + 1;
		    GT_SetGadgetAttrsA(Gad_ysize, PrefWin, NULL, ChangeNumberGadTags);
                  }
                  else {
		    width = NTSC_SCR_WIDTH;	/* minimal size */
		    height = NTSC_SCR_HEIGHT;
		  }
		}
		break;
	case ACTION_WINDVISIZE:
	        if (new_is_ownscr) {
		  ChangeNumberGadTags[0].ti_Data = wx + x_win_width - x_win_i_width;
		  GT_SetGadgetAttrsA(Gad_xsize, PrefWin, NULL, ChangeNumberGadTags);
		  ChangeNumberGadTags[0].ti_Data = wy + x_win_height - x_win_i_height + HeightScreenTitle;
		  GT_SetGadgetAttrsA(Gad_ysize, PrefWin, NULL, ChangeNumberGadTags);
		}
		break;
	case ACTION_WINOWNSCR:
		new_is_ownscr = !new_is_ownscr;
		SetGads2ownscr();
		break;
	case ACTION_WIN4COL:
	        if (new_is_ownscr) {
		  new_is_4col = !new_is_4col;
		}
		break;
	case ACTION_WINUSE:
		*cont = FALSE;
		use = TRUE;
		break;
	case ACTION_WINCANCEL:
		*cont = FALSE;
		break;
	case ACTION_WINSCRMODE:
	        if (new_is_ownscr) {
		  ULONG DID = TmpModeID; //get_DisplayID();
		  long mode_lace = FALSE;
	          if (GetScreenMode(&DID, PrefWin)) {
	            struct Rectangle Rect;
	            TmpModeID = DID;
	            set_numeric;
	            new_screen_mode = TRUE;
		    ChangeTextGadTags[0].ti_Data = (ULONG)GetModeIDName(TmpModeID, &mode_lace);
		    GT_SetGadgetAttrsA(Gad_scrmname, PrefWin, NULL, ChangeTextGadTags);
		    if ((mode_lace && !new_is_lace) || (!mode_lace && new_is_lace)) {
		      new_is_lace = mode_lace;
#ifdef INTERLACE_KNOB
		      ChangeCheckGadTags[0].ti_Data = (new_is_lace);
		      GT_SetGadgetAttrsA(Gad_lace, PrefWin, NULL, ChangeCheckGadTags);
#endif
		    }

#ifdef INTERLACE_KNOB
		    ChangeDisableGadTags[0].ti_Data = DisableLace = TRUE;
		    GT_SetGadgetAttrsA(Gad_lace, PrefWin, NULL, ChangeDisableGadTags);
#endif

                    if (QueryOverscan(TmpModeID, &Rect, OSCAN_TEXT)) {
		      ChangeNumberGadTags[0].ti_Data = width = Rect.MaxX - Rect.MinX + 1;
		      GT_SetGadgetAttrsA(Gad_xsize, PrefWin, NULL, ChangeNumberGadTags);
		      ChangeNumberGadTags[0].ti_Data = height = Rect.MaxY - Rect.MinY + 1;
		      GT_SetGadgetAttrsA(Gad_ysize, PrefWin, NULL, ChangeNumberGadTags);
                    }
                    else {
		      width  = NTSC_SCR_WIDTH;	/* minimal size */
		      height = NTSC_SCR_HEIGHT;
		    }
	          }
		}
		break;
  }
  
  width  = ((struct StringInfo *)(Gad_xsize->SpecialInfo))->LongInt;
  height = ((struct StringInfo *)(Gad_ysize->SpecialInfo))->LongInt;

  if (!*cont) {
    ClosePrefWin();
  }
  
  if (use) {
    if (!new_is_ownscr) {	// die restlichen Aenderungen werden ignoriert
      unset_ownscr;
      CloseOpenScreen(FALSE, FALSE, FALSE);
    }
    else {
      const int size_change = (screen->Width != width || screen->Height != height);
    
      if (new_is_lace != is_lace || new_is_4col != is_col4 || is_ownscr != new_is_ownscr || new_screen_mode || size_change) {

        if (new_screen_mode) {
          show_state.DisplayID = TmpModeID;
        }

        if (size_change) {
          show_state.screen_size_x = width;
          show_state.screen_size_y = height;
        }

        if (new_is_ownscr) {
          set_ownscr;
        }
        else {
          // ELSE Fall wird nun schon oben abgehandelt.
          // (damit nicht doch noch andere Dinge mit geaendert werden)
        }

        CloseOpenScreen((new_is_lace != is_lace), (new_is_4col != is_col4), size_change);
      }
    }
  }
  
  return ex;
}


/*------------------------------------------------------------*/


/* setzt je nach new_is_ownscr ob Screenmode usw. eingestellt werden kann */

static void SetGads2ownscr(void)
{
  ChangeDisableGadTags[0].ti_Data = !new_is_ownscr;

  GT_SetGadgetAttrsA(Gad_xsize,    PrefWin, NULL, ChangeDisableGadTags);
  GT_SetGadgetAttrsA(Gad_ysize,    PrefWin, NULL, ChangeDisableGadTags);
  GT_SetGadgetAttrsA(Gad_default,  PrefWin, NULL, ChangeDisableGadTags);
#ifdef INTERLACE_KNOB
  DisableLace = ChangeDisableGadTags[0].ti_Data;
  GT_SetGadgetAttrsA(Gad_lace,     PrefWin, NULL, ChangeDisableGadTags);
#endif
  GT_SetGadgetAttrsA(Gad_4col,     PrefWin, NULL, ChangeDisableGadTags);
  GT_SetGadgetAttrsA(Gad_scrmode,  PrefWin, NULL, ChangeDisableGadTags);
  GT_SetGadgetAttrsA(Gad_scrmname, PrefWin, NULL, ChangeDisableGadTags);

  ChangeDisableGadTags[0].ti_Data = !new_is_ownscr || !is_dvif;
  GT_SetGadgetAttrsA(Gad_dvisize,  PrefWin, NULL, ChangeDisableGadTags);
}


