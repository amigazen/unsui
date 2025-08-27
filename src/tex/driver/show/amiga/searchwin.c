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
#include "searchwin.h"

#include "amscreen.i"
#include "am_requ.i"
#include "searchwin.i"
#include "globals.i"
#include "help.i"
#include "search.i"


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
/*			     S E A R C H W I N D O W			       */
/*******************************************************************************/

/* externals... */
extern UBYTE		 sig_searchwin;
extern struct Screen	*screen;
extern struct Window	*win2;
extern struct TextAttr	 txtAttr;
extern short		 HeightWintitle;


static long ActionScreenPref(enum SearchActions action, int * cont, char * searchbuf, int len);


#define GADID_SEARCH_STRING	1
#define GADID_SEARCH_SEARCH	2
#define GADID_SEARCH_CANCEL	3



/* globals (help.c) */
struct Window	*SearchWin = NULL;	/* = NULL wichtig, damit wird gearbeitet! */


/* locals... */
static struct Gadget	*GList = NULL;


static struct Gadget *Gad_string;
static struct Gadget *Gad_ok;
static struct Gadget *Gad_cancel;

static int IsSearchActive = FALSE;


#define SEARCHWIN_WIDTH		200
#define SEARCHWIN_HEIGHT	110


/* functions... */

void OpenSearchWin(void)
{
  struct NewWindow new_win;
  WORD LeftEdge, TopEdge, Width, Height;
  struct NewGadget ng;
  struct Gadget * gad;
  int l, h;
  int l1, l2;

  if (SearchWin) {		// kann schon mal vorkommen, mach aber nix!
    ActivateWindow(SearchWin);
    return;
  }


  if (GadToolsBase == NULL || GadToolsBase->lib_Version < 37L) {
    MySimpleRequest(1, NULL, NULL, NULL, NULL, GetTeXString(MSG_PREFWIN_GADTOOLS));
    return;
  }
  
  
  IsSearchActive = FALSE;
  

  Width  = SEARCHWIN_WIDTH;
  Height = SEARCHWIN_HEIGHT+win2->BorderTop;

  h = screen->Font->ta_YSize;
  
  Height = (h+4)*3+2*h+2 + win2->BorderTop;

  l1 = TextLength(&(screen->RastPort), GetTeXString(MSG_SEARCHWIN_SEARCH), strlen(GetTeXString(MSG_SEARCHWIN_SEARCH)));
  l2 = TextLength(&(screen->RastPort), GetTeXString(MSG_SEARCHWIN_CANCEL), strlen(GetTeXString(MSG_SEARCHWIN_CANCEL)));
  
  l = (l1>l2 ? l1 : l2) + 4;

  if (2*l+l/2+40 > Width) Width = 2*l+l/2+40;

  CenterWindow(&LeftEdge, &TopEdge, Width, Height);


  GList = NULL;
  if ((gad = CreateContext(&GList)) == NULL) return;

  ng.ng_GadgetText = GetTeXString(MSG_SEARCHWIN_STRING);  // l = TextLength(&(screen->RastPort), ng.ng_GadgetText, strlen(ng.ng_GadgetText));
  ng.ng_LeftEdge   = 20;
  ng.ng_TopEdge    = h+4+h/2 + win2->BorderTop;
  ng.ng_Width      = Width - 40;
  ng.ng_Height     = h + 4;
  ng.ng_TextAttr   = screen->Font;
  ng.ng_GadgetID   = GADID_SEARCH_STRING;
  ng.ng_Flags      = PLACETEXT_ABOVE;
  ng.ng_VisualInfo = SDVI_VI;
  ng.ng_UserData   = (void *)SEARCHWIN_ACTION_STRING;
  
  Gad_string = gad = CreateGadget(STRING_KIND, gad, &ng, GT_Underscore, '_', 
							 GTST_String, SearchPattern, 
							 GTST_MaxChars, sizeof(SearchPattern)-1, 
 							 TAG_DONE);


  ng.ng_GadgetText = GetTeXString(MSG_SEARCHWIN_SEARCH);  
  ng.ng_LeftEdge   = 20;
  ng.ng_TopEdge    = Height - (h+4+2+h/2);
  ng.ng_Width      = l;
  ng.ng_Height     = h + 4;
  ng.ng_TextAttr   = screen->Font;
  ng.ng_GadgetID   = GADID_SEARCH_SEARCH;
  ng.ng_Flags      = PLACETEXT_IN;
  ng.ng_VisualInfo = SDVI_VI;
  ng.ng_UserData   = (void *)SEARCHWIN_ACTION_SEARCH;
  
  Gad_ok = gad = CreateGadget(BUTTON_KIND, gad, &ng, GT_Underscore, '_', TAG_DONE);

  ng.ng_GadgetText = GetTeXString(MSG_SEARCHWIN_CANCEL);  
  ng.ng_LeftEdge   = Width - 20 - l;
  ng.ng_TopEdge    = Height - (h+4+2+h/2);
  ng.ng_Width      = l;
  ng.ng_Height     = h + 4;
  ng.ng_TextAttr   = screen->Font;
  ng.ng_GadgetID   = GADID_SEARCH_CANCEL;
  ng.ng_Flags      = PLACETEXT_IN;
  ng.ng_VisualInfo = SDVI_VI;
  ng.ng_UserData   = (void *)SEARCHWIN_ACTION_CANCEL;
  
  Gad_cancel = gad = CreateGadget(BUTTON_KIND, gad, &ng, GT_Underscore, '_', TAG_DONE);


  new_win.LeftEdge  = LeftEdge;
  new_win.TopEdge   = TopEdge;
  new_win.Width     = Width;
  new_win.Height    = Height;
  new_win.DetailPen = 0;
  new_win.BlockPen  = 1; 
  new_win.Title     = GetTeXString(MSG_SEARCHWIN_WIN_TITLE);
  new_win.Flags     = WFLG_DEPTHGADGET | WFLG_CLOSEGADGET | WFLG_DRAGBAR | WFLG_NOCAREREFRESH | WFLG_ACTIVATE | WFLG_RMBTRAP;
  new_win.IDCMPFlags = IDCMP_CLOSEWINDOW | IDCMP_VANILLAKEY | IDCMP_RAWKEY | BUTTONIDCMP | STRINGIDCMP | IDCMP_ACTIVEWINDOW | ((is_amigaguide) ? IDCMP_GADGETHELP : 0);
  new_win.Type      = CUSTOMSCREEN;
  new_win.FirstGadget = GList;
  new_win.CheckMark = NULL;
  new_win.Screen    = screen;
  new_win.BitMap    = NULL;
  
  if (is_amigaguide) {
    SearchWin = (struct Window *)OpenWindowTags(&new_win, WA_HelpGroupWindow, win2, TAG_DONE);
  }
  else {
    SearchWin = (struct Window *)OpenWindow(&new_win);
  }

  if (SearchWin == NULL) {
    MySimpleRequest(1, NULL, NULL, NULL, GetTeXString(MSG_SHOWDVI_MESSAGE), GetTeXString(MSG_SEARCHWIN_CANT_OPEN));
    return;
  }
  
  //DrawBevelBox(SearchWin->RPort, 20, Height - 30 - 10, Width - 40, 2, GT_VisualInfo, SDVI_VI, TAG_DONE);


  sig_searchwin = SearchWin->UserPort->mp_SigBit;
}



void CloseSearchWin(void)
{
  if (SearchWin != NULL) {
    CloseWindow(SearchWin);
    SearchWin = NULL;
  }
  if (GList != NULL) {
    FreeGadgets(GList);
    GList = NULL;
  }

  IsSearchActive = FALSE;
}




enum SearchActions DoSearchWin(char * searchbuf, int len)
{
  enum SearchActions action;
  struct IntuiMessage *msg;
  ULONG msg_class;
  UWORD	Code;
  UWORD Qualifier;
  struct Gadget *m_gad;
  int cont = TRUE;

  enum SearchActions ex = SEARCHWIN_ACTION_NONE;
  
  if (SearchWin == NULL) return ex;

  
  while(cont && ex == 0 && SearchWin != NULL && (msg = GT_GetIMsg(SearchWin->UserPort)) != NULL) {
    msg_class	= msg->Class;
    Code  	= msg->Code;
    Qualifier	= msg->Qualifier;
    m_gad	= (struct Gadget *)(msg->IAddress);

    action = SEARCHWIN_ACTION_NONE;

    switch (msg_class) {
      case IDCMP_GADGETHELP:
        work_with_gadgethelp(msg);
        break;

      case IDCMP_CLOSEWINDOW:
        action = SEARCHWIN_ACTION_CLOSE;
  	cont = FALSE;
  	break;

      case IDCMP_ACTIVEWINDOW:
        // tja, hier muesste das Gad_string Teil aktiviert werden!
        if (!IsSearchActive) (void)ActivateGadget(Gad_string, SearchWin, NULL);
	break;

      case IDCMP_RAWKEY:
        if (Code == 95) HelpKeyAGuide(Qualifier & IEQUALIFIER_CONTROL);
        break;

      case IDCMP_VANILLAKEY:
        switch (toupper(Code)) {
	  case 13 :		/* RET */
	  case 'S':		/* use */
	    action = SEARCHWIN_ACTION_SEARCH;
	    break;
	  case 27 :		/* ESC */
	    action = SEARCHWIN_ACTION_CLOSE;
	    break;
	  case 'C':		/* cancel */
	    action = SEARCHWIN_ACTION_CANCEL;
	    break;
	  case 'G':
	    action = SEARCHWIN_ACTION_SEARCH;
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

    ex = ActionScreenPref(action, &cont, searchbuf, len);
  }
  
  return ex;
}


static enum SearchActions ActionScreenPref(enum SearchActions action, int * cont, char * searchbuf, int len)
{
  enum SearchActions ret = action;

  switch (action) {

    case SEARCHWIN_ACTION_CLOSE:
      CloseSearchWin();
      *cont = FALSE;
      break;

    case SEARCHWIN_ACTION_CANCEL:
      if (IsSearchActive) {
        ReEnableSearchWin();
      }
      else {
        ret = SEARCHWIN_ACTION_CLOSE;
        CloseSearchWin();
      }
      *cont = FALSE;
      break;
    
    case SEARCHWIN_ACTION_STRING:
      if (!*((struct StringInfo *)Gad_string->SpecialInfo)->Buffer) {
        // Suche nach einem leeren String??
        (void)ActivateGadget(Gad_string, SearchWin, NULL);
      }
      ret = SEARCHWIN_ACTION_NONE;
      break;

    case SEARCHWIN_ACTION_SEARCH:
      // Wenn IsSearchActive, dann kann nicht noch eine Suche gestartet werden!
      if (IsSearchActive) {
        action = SEARCHWIN_ACTION_NONE;
      }
      else {
	if (searchbuf) {
	  if (*((struct StringInfo *)Gad_string->SpecialInfo)->Buffer) {
	    strncpy(searchbuf, ((struct StringInfo *)Gad_string->SpecialInfo)->Buffer, len);
            IsSearchActive = TRUE;
        
            // nun muessen noch ein paar Gadgets inactiviert werden
            GT_SetGadgetAttrs(Gad_string, SearchWin, NULL, GA_Disabled, TRUE, TAG_DONE);
            GT_SetGadgetAttrs(Gad_ok, SearchWin, NULL, GA_Disabled, TRUE, TAG_DONE);
            
            SetWindowTitles(SearchWin, GetTeXString(MSG_SEARCHWIN_DOSEARCH), (UBYTE *)-1);
          }
          else {
            // Suche nach einem leeren String??
            (void)ActivateGadget(Gad_string, SearchWin, NULL);
            ret = SEARCHWIN_ACTION_NONE;
          }
	}
	else {
          ret = SEARCHWIN_ACTION_NONE;
	}
      }
      break;
  }
  
  return ret;
}


// wenn eine Suche zuende war, abgebrochen wurde oder was auch immer, setze SuchWindow wieder aktiv
void ReEnableSearchWin(void)
{
  IsSearchActive = FALSE;
  if (SearchWin) {
    GT_SetGadgetAttrs(Gad_string, SearchWin, NULL, GA_Disabled, FALSE, TAG_DONE);
    GT_SetGadgetAttrs(Gad_ok, SearchWin, NULL, GA_Disabled, FALSE, TAG_DONE);
    SetWindowTitles(SearchWin, GetTeXString(MSG_SEARCHWIN_WIN_TITLE), (UBYTE *)-1);
  }
}

char * GetCurSearchString(void)
{
  return (SearchWin) ? ((struct StringInfo *)Gad_string->SpecialInfo)->Buffer : NULL;
}
