
#ifdef AMIGA

#include "defines.h"

#include <stdio.h>
#include <fcntl.h>
#include <intuition/intuition.h>
#include <libraries/dos.h>
#include <exec/memory.h>

#include <intuition/classusr.h>
#include <intuition/imageclass.h>
#include <intuition/gadgetclass.h>
#include <intuition/cghooks.h>
#include <intuition/icclass.h>
#include <intuition/classes.h>

#ifdef AZTEC_C
#  include <functions.h>
#endif

#ifdef ANSI
#  include <string.h>
#  include <stdlib.h>
#endif

#include <clib/intuition_protos.h>
#include <clib/graphics_protos.h>
#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/graphics_pragmas.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/dos_pragmas.h>

#define MEM_IN_CHIP

#ifndef MEM_IN_CHIP
#  define chip
#endif

#ifndef max
#define   max(a,b)    ((a) > (b) ? (a) : (b))
#endif


#include "globals.h"
#include "gad_def.h"
#include "amscreen.h"
#include "gadget.h"

#include "showdvi.i"
#include "gadget.i"
#include "amscreen.i"
#include "globals.i"
#include "am_requ.i"
#include "liste.i"
#include "am_menu.i"
// #include "pgscroll.i"


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



extern long		current_page_phy;
extern long		current_page;

static long		x_image_bytes;
static long		y_image_bytes;

/* Wird fuer das Gadgetumschalten benoetigt, wenn man ganz nach oben/unten gescrollt hat. */
static char		is_ganz_unten = FALSE;
static char		is_ganz_oben = FALSE;


static struct Gadget * PgGadLst = NULL;

/* lokale Funktionen */
#if !defined(REQ_LIBRARY)	/* color requester comes from the req.library */
static void set_temp_color		Args((int gadid,
              	                              int temp));
#endif

static void SetPgUpGad			Args((void));	/* Mit RemoveGad/AddGad/RefreshGad */
static void SetPgDownGad		Args((void));	/* Mit RemoveGad/AddGad/RefreshGad */



/* a macro for the "adjacent" position to the right of a gadget,
 * but safe, if the reference gadget is NULL
 */
#define RIGHTBY( g )	( ((g)==NULL)? 20: ((g)->LeftEdge + (g)->Width ) )



#define ABSOLUT(x)	(((x)>=0) ? (x) : -(x))

/* struct Remember *Rem_Key = NULL; */



#if 0
#define MARGIN_SPRITE_DATA_SIZE	11*2
#define MARGIN_SPRITE_HEIGHT	11
#define MARGIN_SPRITE_WIDTH	16
extern UWORD * margin_sprite_data;
static USHORT __chip margin_sprite_data_fast[] = {
    0x0000, 0x0000,	/* vert. and horiz. start posn. */
	/* Plane 0 */
	0xA0A0, 0x0000,
	0x60C0, 0x0000,
	0xF1E0, 0x0000,
	0x3180, 0x0000,
	0x0A00, 0x0000,
	0x0000, 0x0000,
	0x0A00, 0x0000,
	0x3180, 0x0000,
	0xF1E0, 0x0000,
	0x60C0, 0x0000,
	0xA0A0, 0x0000,
	/* Plane 1 */
	0x60C0, 0x0000,
	0xA0A0, 0x0000,
	0xD160, 0x0000,
	0x2080, 0x0000,
	0x0000, 0x0000,
	0x0000, 0x0000,
	0x0000, 0x0000,
	0x2080, 0x0000,
	0xD160, 0x0000,
	0xA0A0, 0x0000,
	0x60C0, 0x0000,
    0x0000, 0x0000,	/* reserved, must be NULL */
};
#endif






void __inline VSetScrollerValues(void)
{
  UWORD winih = win2->Height - win2->BorderTop - win2->BorderBottom;
  UWORD hidden = max(wy - winih, 0);
  if (static_y_Koo > hidden) static_y_Koo = hidden;
  poty_PropInfo.VertBody = (hidden > 0) 
			? (UWORD)(((ULONG)(winih-(winih>>3)) * MAXBODY) / (wy-(winih>>3))) 
			: MAXBODY;
  poty_PropInfo.VertPot  = (hidden > 0) 
			? (UWORD)(((ULONG)static_y_Koo * MAXPOT) / hidden) 
			: 0;
}

UWORD __inline VFindScrollerTop(void)
{
  UWORD winih = win2->Height - win2->BorderTop - win2->BorderBottom;
  UWORD top, hidden;
  hidden = max(wy - winih, 0);
  top = (((ULONG)hidden*poty_PropInfo.VertPot) + (MAXPOT/2)) >> 16;
  return top;
}

void __inline HSetScrollerValues(void)
{
  UWORD winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
  UWORD hidden = max(wx - winiw, 0);
  if (static_x_Koo > hidden) static_x_Koo = hidden;
  potx_PropInfo.HorizBody = (hidden > 0) ? (UWORD)(((ULONG)(winiw-(winiw>>3)) * MAXBODY) / (wx-(winiw>>3))) : MAXBODY;
  potx_PropInfo.HorizPot  = (hidden > 0) ? (UWORD)(((ULONG)static_x_Koo * MAXPOT) / hidden) : 0;
}

UWORD __inline HFindScrollerTop(void)
{
  UWORD winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
  UWORD top, hidden;
  hidden = max(wx - winiw, 0);
  top = (((ULONG)hidden*potx_PropInfo.HorizPot) + (MAXPOT/2)) >> 16;
  return top;
}




void ModifyXPot(void)
{
  if (is_gadg) {
      HSetScrollerValues();
      NewModifyProp(&potx_Gad, win2, NULL, potx_PropInfo.Flags, potx_PropInfo.HorizPot, 0,
				potx_PropInfo.HorizBody, 0, 1);
  }
}

void ModifyYPot(void)
{
  if (is_gadg) {
      VSetScrollerValues();
      NewModifyProp(&poty_Gad, win2, NULL, poty_PropInfo.Flags, 0, poty_PropInfo.VertPot,
				0, poty_PropInfo.VertBody, 1);
  }
}


void init_gad(void)
 {
  register struct Gadget *gad;

  potx_PropInfo.Flags = FREEHORIZ | PROPNEWLOOK | PROPBORDERLESS | AUTOKNOB;
  HSetScrollerValues();

  gad = &potx_Gad;
  gad->LeftEdge = width_left_border-1+WIDTH_PAGEX_GAD;
  gad->TopEdge  = -BObj.LArrImage->Height + 3; // -HEIGHT_SCROLL + ((is_os2) ? 3 : 1);
  gad->Width    = - BObj.UArrImage->Width - BObj.DArrImage->Width - BObj.SizeImage->Width - width_left_border + 4 - WIDTH_PAGEX_GAD;
  gad->Height   = BObj.LArrImage->Height - 4;  // (SHORT)HEIGHT_SCROLL_BAR - ((is_os2) ? 4 : 2);
  gad->UserData = 0;  // (APTR)(win2->Width-win2->BorderLeft-WIDTH_SCROLL-2 - 2*WIDTH_RL_ARROW - WIDTH_PAGEX_GAD);

  poty_PropInfo.Flags = FREEVERT | PROPNEWLOOK | PROPBORDERLESS | AUTOKNOB;
  VSetScrollerValues();

  gad = &poty_Gad;
  gad->LeftEdge = -BObj.SizeImage->Width + 5;  // -WIDTH_SCROLL_BAR + 3; /* relativ zum rechten Rand */
  gad->TopEdge = HeightWinTitle + BObj.UArrImage->Height + BObj.DArrImage->Height + 1;  // 2*HEIGHT_UD_ARROW+1;
  gad->Width = BObj.SizeImage->Width - 8;     // (SHORT)WIDTH_SCROLL_BAR - 6;
  gad->Height = -BObj.SizeImage->Height - 2*(BObj.UArrImage->Height + BObj.DArrImage->Height) - HeightWinTitle - 2;  // 4*HEIGHT_UD_ARROW - HeightWinTitle - 2;
  gad->UserData = 0;  //(APTR)(x_win_i_height-6*HEIGHT_UD_ARROW);

  gad = &potx_left_Gad;
  gad->LeftEdge = -BObj.SizeImage->Width - BObj.LArrImage->Width - BObj.RArrImage->Width + 1; // -3 * WIDTH_SCROLL - 1;
  gad->TopEdge = potx_Gad.TopEdge - ((is_os2) ? 2 : 0);
  //gad->Width = (SHORT)WIDTH_X_GADGETS;
  //gad->Height = (SHORT)HEIGHT_SCROLL_BAR;
  gad->GadgetRender = gad->SelectRender = (APTR)BObj.LArrImage;

  gad = &potx_right_Gad;
  gad->LeftEdge = -BObj.SizeImage->Width - BObj.RArrImage->Width + 1; // -2 * WIDTH_SCROLL - 1;
  gad->TopEdge = (SHORT)potx_Gad.TopEdge - ((is_os2) ? 2 : 0);
  //gad->Width = (SHORT)WIDTH_X_GADGETS;
  //gad->Height = (SHORT)HEIGHT_SCROLL_BAR;
  gad->GadgetRender = gad->SelectRender = (APTR)BObj.RArrImage;
  
  gad = &poty_up_Gad;
  gad->LeftEdge = (SHORT)poty_Gad.LeftEdge - 4;
  gad->TopEdge = -BObj.SizeImage->Height - BObj.UArrImage->Height - BObj.DArrImage->Height + 1;  // -HEIGHT_SCROLL - 2*HEIGHT_UD_ARROW + 1;
  //gad->Width = (SHORT)WIDTH_SCROLL_BAR;
  //gad->Height = (SHORT)HEIGHT_UD_ARROW;
  gad->GadgetRender = gad->SelectRender = (APTR)BObj.UArrImage;

  gad = &poty_up2_Gad;
  gad->LeftEdge = (SHORT)poty_up_Gad.LeftEdge;
  gad->TopEdge = (SHORT)HeightWinTitle;
  //gad->Width = (SHORT)WIDTH_SCROLL_BAR;
  //gad->Height = (SHORT)HEIGHT_UD_ARROW;
  gad->GadgetRender = gad->SelectRender = (APTR)BObj.UArrImage;

  gad = &poty_down_Gad;
  gad->LeftEdge = (SHORT)poty_up_Gad.LeftEdge;
  gad->TopEdge = -BObj.SizeImage->Height - BObj.DArrImage->Height + 1;  // -HEIGHT_SCROLL - HEIGHT_UD_ARROW + 1;
  //gad->Width = (SHORT)WIDTH_SCROLL_BAR;
  //gad->Height = (SHORT)HEIGHT_UD_ARROW;
  gad->GadgetRender = gad->SelectRender = (APTR)BObj.DArrImage;

  gad = &poty_down2_Gad;
  gad->LeftEdge = (SHORT)poty_up_Gad.LeftEdge;
  gad->TopEdge = (SHORT)HeightWinTitle + BObj.UArrImage->Height;  // HEIGHT_UD_ARROW;
  //gad->Width = (SHORT)WIDTH_SCROLL_BAR;
  //gad->Height = (SHORT)HEIGHT_UD_ARROW;
  gad->GadgetRender = gad->SelectRender = (APTR)BObj.DArrImage;
}


#if !defined(REQ_LIBRARY)	/* color requester comes from the req.library */
static void set_temp_color(int gadid,int temp)
{
  if (current_col.col_number_t == 0) {
    switch (gadid) {
      case COL_RED_NR	  :
	current_col.red_t0 = (short)temp;
	break;
      case COL_GREEN_NR :
	current_col.green_t0 = (short)temp;
	break;
      case COL_BLUE_NR  :
	current_col.blue_t0 = (short)temp;
	break;
      }
  }
  else {
    switch (gadid) {
      case COL_RED_NR	  :
	current_col.red_t1 = (short)temp;
	break;
      case COL_GREEN_NR :
	current_col.green_t1 = (short)temp;
	break;
      case COL_BLUE_NR  :
	current_col.blue_t1 = (short)temp;
	break;
    }
  }
}
#endif


long handle_file_gad(void)
{
#if 0
  register long ret = 0L;
  register char c;
  int i, ac;
  char *str, *tptr, string[250], fname[250];

  str = (char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->Buffer;
  strcpy(fname, str);
  if (fname[0]=='\0') {
     strcpy((char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->Buffer,
	(char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->UndoBuffer);
     return ret;
  }
  if ((tptr = strrchr(fname, '.')) == NULL || stricmp(tptr, ".dvi")) {
    strcat(fname, ".dvi");
  }
  /** zu umstaendlich ....
  if (tptr == NULL) {
    strcat(fname, ".dvi");
  }
  else {
    if (stricmp(tptr, ".dvi") != 0) {
       strcat(fname, ".dvi");
    }
  }
  **/
  str = (char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer;
  strcpy(string,str);
  /* mit ':' ist der Filename schon komplett !!! */
  if (fname[0] == ':') {	/* append device from dir-gad */
    tptr = strrchr(string,':');
    strcpy(tptr,fname);
    strcpy(fname,string);
  }

  ac = access(fname,4);	/* Test ob File allein existiert */
  if (ac != 0) {		/* File existiert so noch nicht  */
    /* Directory aus dir_Gad holen */
    str = (char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer;
    strcpy(string,str);
    i = strlen(string);
    if (i > 0)  { 	/* haenge slash an */
       c = string[i-1];
       if (c != ':' && c != '/' )  {
         string[i] = '/';
         string[i+1] = (char)0;
       }
    }
    ac = access(string,0);	/* Test ob Dir. existiert */
    if (ac != 0) {
       strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer,
	  (char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->UndoBuffer);
       RefreshGList(&dir_Gad,win2,NULL,2);
       Message("Directory \"%s\" not found!",string);
       beep();
       return ret;
    }
    /* Filename + Directory in fname */
    strcat(string, fname);
    strcpy(fname, string);
  }
  else {	/* fname allein existiert */
    if (strchr(fname,'/') == NULL && strchr(fname,':') == NULL) {
      /* kein Pfad-Anteil */
      getdir("",string);
      strcat(string,fname);
      strcpy(fname,string);
    }
  }
  ac = access(fname, 4);	/* existiert das File mit Direct. */
  if (ac != 0) {		/* Nein... */
      /* dann scheint es ueberhaupt nicht zu existieren */
      strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer,
	(char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->UndoBuffer);
      strcpy((char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->Buffer,
	(char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->UndoBuffer);
      RefreshGList(&dir_Gad,win2,NULL,2);
      Message("File \"%s\" not found!",fname);
      beep();
      return ret;
  }
  /* das File exist. mit Namen fname */

  /* das selbe wie zuvor??? */
  if (strcmp(filename, fname) == 0) {
    ret = KOMM + 3L;		/* load again */
    OpenNewDVI(filename, FALSE);
  }
  else {
    strcpy(filename, fname);	/* akt. vollst. Filename */
    /* nun Namen aufsplitten in Dir. und in Filename */
    set_Gadgets_to_fname();
    ret = KOMM +4L;		/* neues File */
    OpenNewDVI(filename, FALSE);
  }

  return ret;
#else
  return 0;
#endif
}



/* ToDo: Check auf erste, letzte Seite und geg. Gadget disabeln */ 
void CheckUpDownBorderGad(void)
{
  if (is_pscro) return;

  if (is_ganz_unten) {
    if (poty_PropInfo.VertPot <= 65400) {
      is_ganz_unten = FALSE;
      SetPgDownGad();
    }
  }
  else {
    if (poty_PropInfo.VertPot > 65400) {
      is_ganz_unten = TRUE;
      SetPgDownGad();
    }
  }
  if (is_ganz_oben) {
    if (poty_PropInfo.VertPot > 0) {
      is_ganz_oben = FALSE;
      SetPgUpGad();
    }
  }
  else {
    if (poty_PropInfo.VertPot == 0) {
      is_ganz_oben = TRUE;
      SetPgUpGad();
    }
  }
}

static void SetPgUpGad(void)
{
#if 0
  if (win2 == NULL || !is_gadg) return;

  RemoveGadget(win2, &poty_up2_Gad);
  RemoveGadget(win2, &poty_up_Gad);
  SetPgUp();
  AddGadget(win2, &poty_up_Gad,  POTY_PFEIL_OBEN_GAD_NR);
  AddGadget(win2, &poty_up2_Gad, POTY_PFEIL_OBEN2_GAD_NR);
  RefreshGList(&poty_up_Gad, win2, NULL, 2);
#endif
}

static void SetPgDownGad(void)
{
#if 0
  if (win2 == NULL || !is_gadg) return;

  RemoveGadget(win2, &poty_down2_Gad);
  RemoveGadget(win2, &poty_down_Gad);
  SetPgDown();
  AddGadget(win2, &poty_down_Gad,	POTY_PFEIL_UNTEN_GAD_NR);
  AddGadget(win2, &poty_down2_Gad,	POTY_PFEIL_UNTEN2_GAD_NR);
  RefreshGList(&poty_down_Gad, win2, NULL, 2);
#endif
}



long check_gad(struct Gadget *m_gad)
{
  register short gadid;
  /* struct PropInfo *pInf; */
  long ret = 0L;

  gadid = m_gad->GadgetID;

  if (is_print && (gadid == PG_POT_GAD_NR || gadid == PG_LEFT_GAD_NR ||
	gadid == PG_RIGHT_GAD_NR || PG_INT_GAD_NR)) {
    Message(MSG_NOT_WHILE_PRINTING);
    beep();
    Set_PgGadPageCur();	// wieder zuruecksetzen
    return 0L;
  }

  switch (gadid) {
    case PG_POT_GAD_NR:
    case PG_LEFT_GAD_NR:
    case PG_RIGHT_GAD_NR:
        {
          long phy;

          GetAttr(PGA_Top, pg_pot_Gad, (ULONG *)&phy);
          phy++;	// PGA_Top geht von 0 -- max_page_number-1, ich fang aber bei 1 an!
          
          if (gadid != PG_POT_GAD_NR) {
            if (gadid == PG_LEFT_GAD_NR) phy--;
            else                         phy++;
          
            if (phy < 1)               break; //phy = 1;
            if (phy > max_page_number) break; //phy = max_page_number;

            Set_PgGadPage(phy);          
          }
          
          set_tusephy;
          ret = phy;
        }
	break;
    
      case PG_INT_GAD_NR:
        GetAttr(STRINGA_LongVal, pg_int_Gad, (ULONG *)&ret);
        if (is_usephy) {
          if (ret < 1) ret = 1;
          if (ret > max_page_number) ret = max_page_number;
        }
        else {
          if (ret == 0) ret = -1;	// -1 entspricht Seite 0
         				// gibt allerdings Probleme mit wirklich neg. Seiten
        }
        break;
      
      case POTX_PFEIL_LINKS_GAD_NR	:    /* Hit-Gadget  */
	     window_plus_sbar_move(-STEP_ARROW_X,0);
	     break;
      case POTX_PFEIL_RECHTS_GAD_NR	:    /* Hit-Gadget  */
	     window_plus_sbar_move(STEP_ARROW_X,0);
	     break;
      case POTY_PFEIL_OBEN_GAD_NR	:    /* Hit-Gadget  */
      case POTY_PFEIL_OBEN2_GAD_NR	:    /* Hit-Gadget  */
	     window_plus_sbar_move(0,-STEP_ARROW_Y);
	     break;
      case POTY_PFEIL_UNTEN_GAD_NR	:    /* Hit-Gadget  */
      case POTY_PFEIL_UNTEN2_GAD_NR	:    /* Hit-Gadget  */
	     window_plus_sbar_move(0,STEP_ARROW_Y);
	     break;
	     
      case POTX_GAD_NR:
             window_set_x(HFindScrollerTop());
	     break;
      case POTY_GAD_NR:
             window_set_y(VFindScrollerTop());
	     break;

      default: Fatal(20,MSG_INTERNAL_ERROR_W_ARG, "unknown Gadgetnumber");
  }
  return (ret);
}


long down_gad(long gadid, struct Gadget *m_gad, ULONG time_secs, ULONG time_mics)
{
  register struct PropInfo *pInf;
  struct IntuiMessage *msg2;
  long ret = 0;
  ULONG msg_class = GADGETDOWN;	/* Initialisierung fuer die do {} while Schleife */
  long phy = 0;

  /* vv werden fuer den Doppelklick auf den Pfeilen benoetigt */
  static ULONG old_time_secs=0, old_time_mics=0;
  static long old_s_y_koo=0;

  pInf = (struct PropInfo *)m_gad->SpecialInfo;

  /* Falls eine der Pfeil Tasten gedrueckt wurde, wird auch gleich noch auf */
  /* das GADGETUP oder MOUSEBUTTON..SELECTUP!! gewartet!! */

  switch (gadid) {
    case POTX_GAD_NR:
        window_set_x(HFindScrollerTop());
	break;
    case POTY_GAD_NR:
        window_set_y(VFindScrollerTop());
	break;

    case PG_LEFT_GAD_NR:
    case PG_RIGHT_GAD_NR:
        phy = 0;
        GetAttr(PGA_Top, pg_pot_Gad, (ULONG *)&phy);
        phy++;	// PGA_Top geht von 0 -- max_page_number-1, ich fang aber bei 1 an!
        
        if (gadid == PG_LEFT_GAD_NR) phy--;
        else                         phy++;
        
        if (phy < 1)               phy = 1;
        if (phy > max_page_number) phy = max_page_number;

        Set_PgGadPage(phy);
        Delay(14);

	do {

	  msg2 = (struct IntuiMessage *)GetMsg(win2->UserPort);
	  if (msg2 == NULL) {
            phy = 0;
            GetAttr(PGA_Top, pg_pot_Gad, (ULONG *)&phy);
            phy++;	// PGA_Top geht von 0 -- max_page_number-1, ich fang aber bei 1 an!
            
            if (gadid == PG_LEFT_GAD_NR) phy--;
            else                         phy++;
            
            if (phy < 1)               phy = 1;
            if (phy > max_page_number) phy = max_page_number;

            Set_PgGadPage(phy);
            Delay(9);
          }
          else {
	    msg_class = msg2->Class;
	    ReplyMsg(&(msg2->ExecMessage));
	  }
	} while(msg_class != GADGETUP && msg_class != MOUSEBUTTONS);

	if (msg_class == GADGETUP) {
          GetAttr(PGA_Top, pg_pot_Gad, (ULONG *)&ret);
          ret++;
          set_tusephy;	// interpretiere ret als physikalische Nummer ( => ret >0)
        }
        break;
    case PG_INT_GAD_NR:
    case PG_POT_GAD_NR:
        break;

    case POTX_PFEIL_LINKS_GAD_NR:
    case POTX_PFEIL_RECHTS_GAD_NR:
	do {
	  msg2 = (struct IntuiMessage *)GetMsg(win2->UserPort);
	  if (msg2 == NULL) {
	    WaitBOVP(&(screen->ViewPort));
	    window_plus_sbar_move(((gadid == POTX_PFEIL_LINKS_GAD_NR) ? -STEP_ARROW_X : STEP_ARROW_X),0);
	  }
	  else {
	    msg_class = msg2->Class;
	    ReplyMsg(&(msg2->ExecMessage));
	  }
	} while(msg_class != GADGETUP && msg_class != MOUSEBUTTONS);
	break;
    case POTY_PFEIL_OBEN_GAD_NR:
    case POTY_PFEIL_OBEN2_GAD_NR:
    case POTY_PFEIL_UNTEN_GAD_NR:
    case POTY_PFEIL_UNTEN2_GAD_NR:
	do {
	  msg2 = (struct IntuiMessage *)GetMsg(win2->UserPort);
	  if (msg2 == NULL) {
	    //WaitBOVP(&(screen->ViewPort));
	    window_plus_sbar_move(0,((gadid == POTY_PFEIL_OBEN_GAD_NR || 
			gadid == POTY_PFEIL_OBEN2_GAD_NR) ? -STEP_ARROW_Y : STEP_ARROW_Y));
	  }
	  else {
	    msg_class = msg2->Class;
	    ReplyMsg(&(msg2->ExecMessage));
	  }
	} while(msg_class != GADGETUP && msg_class != MOUSEBUTTONS);


	if (!is_pscro && (static_y_Koo == old_s_y_koo) &&
            DoubleClick(old_time_secs, old_time_mics, time_secs, time_mics)) {
	  ret = (gadid == POTY_PFEIL_OBEN_GAD_NR || gadid == POTY_PFEIL_OBEN2_GAD_NR)
			? (KOMM - 1) : (KOMM + 1);
	}
	old_time_secs = time_secs;
	old_time_mics = time_mics;
	old_s_y_koo = static_y_Koo;
	break;
  }

  /* Test ob ich am Rand bin */
  CheckUpDownBorderGad();
  return ret;
}


void follow_pot_gad(long gadid)
{
  //register struct PropInfo *pInf;

  /* updaten des Screens nach einer Mausebewegung auf einem Pot-Gadget */ 

  switch (gadid) {
	case POTX_GAD_NR:
	  window_set_x(HFindScrollerTop());
	  break;
	case POTY_GAD_NR:
	  window_set_y(VFindScrollerTop());
	  break;

        case PG_POT_GAD_NR:
          {
            long phy = 0;

	    GetAttr(PGA_Top, pg_pot_Gad, (ULONG *)&phy);
            phy++;	// PGA_Top geht von 0 -- max_page_number-1, ich fang aber bei 1 an!
            
            if (is_usephy) {
	      Set_PgIntGad(phy);
            }
            else {
              long log = get_log_page_number(phy);
	      Set_PgIntGad(log);
            }
          }
	  break;
  }
}




void Add_system_gadgets(void)
{
  register struct Window * w = win2;
  struct Gadget * tmpgad;

  /* setze Boopsi-Gadget in den unteren Rand */
  //InitPGScrollGad(win2, SDVI_DRI);
  //ChangePGScrollGad(win2, max_page_number, current_page_phy);


  tmpgad = (struct Gadget *)&PgGadLst;
  pg_int_Gad = (struct ExtGadget *)NewObject(NULL, "strgclass", 
		GA_ID,			PG_INT_GAD_NR,
		GA_Width,		3*8,
		GA_Height,		BObj.SizeImage->Height-2,	// HEIGHT_SCROLL_BAR-2,
		GA_RelBottom,		-win2->BorderBottom+2,
		GA_Left,		win2->BorderLeft + 5,
		GA_RelVerify,		TRUE,
		GA_BottomBorder,	TRUE,
		GA_Previous,		tmpgad,
		
		STRINGA_MaxChars,	6,
		STRINGA_LongVal,	0,
		STRINGA_Justification,	STRINGRIGHT,
		TAG_END);

  if (pg_int_Gad->Flags & GFLG_EXTENDED) pg_int_Gad->MoreFlags |= GMORE_GADGETHELP;


  pg_pot_Gad = (struct ExtGadget *)NewObject(NULL, "propgclass", 
		GA_ID,			PG_POT_GAD_NR,
		GA_Height,		BObj.SizeImage->Height-4,	// HEIGHT_SCROLL_BAR - 4,
		GA_Width,		200,
		GA_RelBottom,		-win2->BorderBottom+2+1,
		GA_Left,		RIGHTBY(pg_int_Gad) + 2,
		GA_RelVerify,		TRUE,
		GA_FollowMouse,		TRUE,
		GA_Immediate,		TRUE,
		GA_BottomBorder,	TRUE,
		GA_Previous,		tmpgad,

		PGA_Freedom,		FREEHORIZ,
		PGA_NewLook,		TRUE,
		PGA_Borderless,		TRUE,
		PGA_Top,		0,
		PGA_Visible,		0,
		PGA_Total,		0,
		TAG_END);
		
  if (pg_pot_Gad->Flags & GFLG_EXTENDED) pg_pot_Gad->MoreFlags |= GMORE_GADGETHELP;

  pg_left_Gad = (struct ExtGadget *)NewObject(NULL, "buttongclass", 
		GA_ID,			PG_LEFT_GAD_NR,
		GA_RelBottom,		-win2->BorderBottom+1,
		GA_Left,		RIGHTBY(pg_pot_Gad) + 2,
		GA_BottomBorder,	TRUE,
		GA_Image,		BObj.LArrImage,
		GA_Immediate,		TRUE,
		GA_RelVerify,		TRUE,
		GA_Previous,		tmpgad,
		TAG_END);

  if (pg_left_Gad->Flags & GFLG_EXTENDED) pg_left_Gad->MoreFlags |= GMORE_GADGETHELP;


  pg_right_Gad = (struct ExtGadget *)NewObject(NULL, "buttongclass", 
		GA_ID,			PG_RIGHT_GAD_NR,
		GA_RelBottom,		-win2->BorderBottom+1,
		GA_Left,		RIGHTBY(pg_left_Gad),
		GA_BottomBorder,	TRUE,
		GA_Image,		BObj.RArrImage,
		GA_Immediate,		TRUE,
		GA_RelVerify,		TRUE,
		GA_Previous,		tmpgad,
		TAG_END);

  if (pg_right_Gad->Flags & GFLG_EXTENDED) pg_right_Gad->MoreFlags |= GMORE_GADGETHELP;


  AddGList( w, PgGadLst, -1, 4, NULL );
  RefreshGList( PgGadLst, w, NULL, 4 );

/**
  (void)AddGadget(w, &first_Page_Gad,	FIRST_GAD_NR);
  (void)AddGadget(w, &prev_Page_Gad,	PREV_GAD_NR);
  (void)AddGadget(w, &succ_Page_Gad,	SUCC_GAD_NR);
  (void)AddGadget(w, &last_Page_Gad,	LAST_GAD_NR);
  (void)AddGadget(w, &dir_Gad,		DIR_GAD_NR);
  (void)AddGadget(w, &fil_Gad,		FIL_GAD_NR);
  (void)AddGadget(w, &int_Gad,		INT_GAD_NR);
**/
  /* RefreshGadgets(&first_Page_Gad, w, NULL); wird wohl nicht unbedingt benoetigt!??*/
}

void Add_scroll_gadgets(int no_refresh)
{
  register struct Window *w = win2;

  w->BorderRight = WIDTH_SCROLL+2;
  // w->BorderBottom = HEIGHT_SCROLL;  argg...Das Sizing-Gadget kommt in den unteren Ramen und so gross wird auch der Rest

  x_win_i_width = w->Width - w->BorderLeft - w->BorderRight;

  width_left_border  = w->BorderLeft;
  width_right_border = w->BorderRight;

  {
    const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
    const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
    
    if (winiw > wx) {
      ((struct PropInfo*)potx_Gad.SpecialInfo)->HorizPot = MAXPOT;
    }
    else {
      ((struct PropInfo*)potx_Gad.SpecialInfo)->HorizPot =
 	(USHORT)(((long)(static_x_Koo * MAXPOT))/(wx-(long)x_win_i_width+1L));
    }

    if (winih > wy) {
      ((struct PropInfo*)poty_Gad.SpecialInfo)->VertPot = MAXPOT;
    }
    else {
      ((struct PropInfo*)poty_Gad.SpecialInfo)->VertPot =
	(USHORT)(((long)(static_y_Koo * MAXPOT))/(wy-(long)x_win_i_height+1L));
    }
  }


  (void)AddGadget(w, &potx_left_Gad,	POTX_PFEIL_LINKS_GAD_NR);
  (void)AddGadget(w, &potx_right_Gad,	POTX_PFEIL_RECHTS_GAD_NR);
  (void)AddGadget(w, &poty_up_Gad,	POTY_PFEIL_OBEN_GAD_NR);
  (void)AddGadget(w, &poty_up2_Gad,	POTY_PFEIL_OBEN2_GAD_NR);
  (void)AddGadget(w, &poty_down_Gad,	POTY_PFEIL_UNTEN_GAD_NR);
  (void)AddGadget(w, &poty_down2_Gad,	POTY_PFEIL_UNTEN2_GAD_NR);
  //(void)AddGadget(w, &pgscroll_Gad,	PGSCROLL_GAD_NR);
  (void)AddGadget(w, &potx_Gad,		POTX_GAD_NR);
  (void)AddGadget(w, &poty_Gad,		POTY_GAD_NR);

  if (is_pscro) {
    //AddGadget(win2, &pgscroll_no_Gad, PGSCROLL_NO_GAD_NR);
    //AddGadget(win2, &pgscroll_ok_Gad, PGSCROLL_OK_GAD_NR);
  }
  else {
    //AddGadget(win2, &pgscroll_Gad, PGSCROLL_GAD_NR);
  }

  /* Teste ob ich am Rand bin */
  CheckUpDownBorderGad();

  if (!no_refresh) {
    RefreshGadgets(&potx_left_Gad, w, NULL);
  }
}

void Remove_scroll_gadgets(int no_refresh)
{
  register struct Window *w = win2;

  if (w == NULL) {
    FatalStr(20, "No window?!");
  }

  RemoveGadget(w, &poty_Gad);
  RemoveGadget(w, &potx_Gad);
  RemoveGadget(w, &poty_down_Gad);
  RemoveGadget(w, &poty_down2_Gad);
  RemoveGadget(w, &poty_up_Gad);
  RemoveGadget(w, &poty_up2_Gad);
  RemoveGadget(w, &potx_right_Gad);
  RemoveGadget(w, &potx_left_Gad);

/*  RemoveGList(w, &potx_left_Gad, -1); */

  if (is_pscro) {
    //RemoveGadget(win2, &pgscroll_ok_Gad);
    //RemoveGadget(win2, &pgscroll_no_Gad);
  }
  else {
    //RemoveGadget(win2, &pgscroll_Gad);
  }

  w->BorderRight = w->BorderLeft;
  /* BorderBottom bleibt, da dort das SizingGadget drin ist */

  width_left_border  = w->BorderLeft;
  width_right_border = w->BorderRight;
  
  x_win_i_width = w->Width - w->BorderLeft - w->BorderRight;

  if (!no_refresh) {		// && is_col4
    /* Im Inneren des win2 die zweite Bitplane loeschen... */
    fill_block(win2->Width+poty_up2_Gad.LeftEdge-1, win2->BorderTop, 17,
		win2->Height-win2->BorderTop-win2->BorderBottom, 0);
    fill_block(win2->BorderLeft, win2->Height+potx_Gad.TopEdge, win2->Width-win2->BorderLeft-win2->BorderRight, potx_Gad.Height, 0);

#if 0
#if 0
    fill_block(poty_Gad.LeftEdge, poty_Gad.TopEdge, poty_Gad.Width, (int)poty_Gad.UserData, 0);
    fill_block(potx_Gad.LeftEdge, potx_Gad.TopEdge, (int)potx_Gad.UserData, potx_Gad.Height, 0);
#else
    /* ich loesch das ganze Window, auch wenn's kurz fakelt. :-( */
    fill_block((short)width_left_border,(short)HeightWinTitle,
			(short)x_win_i_width-1,(short)x_win_i_height-1,(short)0);
    /* fill_block(w->LeftEdge, w->TopEdge, x_win_i_width, x_win_i_height, 0); */
#endif
#endif
  }
  
}

void Remove_all_gadgets(void)
{
  register struct Window *w = win2;

  Remove_scroll_gadgets(FALSE);	// no_refresh == FALSE

  /* weg mit dem Boopsi-Gad */
  /* das muss derzeit *nach* rem-scr-gad geschehen, da Disp sonst alle Gadgets loescht :( */

  DelPgGad();

#if 0
  RemoveGadget(w, &int_Gad);
  RemoveGadget(w, &fil_Gad);
  RemoveGadget(w, &dir_Gad);
  RemoveGadget(w, &first_Page_Gad);
  RemoveGadget(w, &last_Page_Gad);
  RemoveGadget(w, &succ_Page_Gad);
  RemoveGadget(w, &prev_Page_Gad);
#endif

  width_left_border  = w->BorderLeft;
  width_right_border = w->BorderRight;
  
  x_win_i_width = w->Width - w->BorderLeft - w->BorderRight;
}



void DelPgGad(void)
{
  if (!PgGadLst || !win2) return;

  RemoveGList(win2, PgGadLst, 4);

  DisposeObject(pg_right_Gad); pg_right_Gad = NULL;
  DisposeObject(pg_left_Gad);  pg_left_Gad  = NULL;
  DisposeObject(pg_pot_Gad);   pg_pot_Gad   = NULL;
  DisposeObject(pg_int_Gad);   pg_int_Gad   = NULL;
  
  PgGadLst = NULL;
}


void Set_PgIntGad(long pg)
{
  if (!PgGadLst) return;

  SetGadgetAttrs(pg_int_Gad, win2, NULL, 
			STRINGA_LongVal,	pg,
			TAG_END);
  RefreshGadgets(pg_int_Gad, win2, NULL);
}

void Set_PgPotGadRange(void)
{
  if (!PgGadLst) return;

  SetGadgetAttrs(pg_pot_Gad, win2, NULL, 
			PGA_Visible,		1,
			PGA_Total,		max_page_number,
			TAG_END);
  RefreshGadgets(pg_pot_Gad, win2, NULL);
}

void Set_PgGadPage(long phy)
{
  if (!PgGadLst) return;

  if (is_usephy) Set_PgIntGad(phy);
  else 		 Set_PgIntGad(get_log_page_number(phy));

  RefreshGadgets(pg_int_Gad, win2, NULL);
  
  SetGadgetAttrs(pg_pot_Gad, win2, NULL,
			PGA_Top,		phy-1,
			TAG_END);
  RefreshGadgets(pg_pot_Gad, win2, NULL);
}


void Set_PgGadPageCur(void)
{
  if (!PgGadLst) return;

  if (is_usephy) Set_PgIntGad(current_page_phy);
  else 		 Set_PgIntGad(current_page);

  RefreshGadgets(pg_int_Gad, win2, NULL);
  
  SetGadgetAttrs(pg_pot_Gad, win2, NULL,
			PGA_Top,		current_page_phy-1,
			TAG_END);
  RefreshGadgets(pg_pot_Gad, win2, NULL);
}


#endif
