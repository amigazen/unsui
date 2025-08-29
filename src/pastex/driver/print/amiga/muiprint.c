#include "defines.h"

#include <sprof.h>

#include "muiprint.h"

#include "globals.h"
#include "globvars.h"
#include "version.h"

#include "globals.i"


extern const char VersionStr[];


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






static struct Library * MUIMasterBase = NULL;

// Application
static APTR MUIAppObject = NULL,
            MainWin      = NULL,
            MessWin      = NULL,
            WorkWin      = NULL,
	    FatalWin     = NULL;

// MessWin
static APTR LV_Messages  = NULL,
	    BT_CloseMess = NULL,
            TX_MessLine  = NULL;

// WorkWin
static APTR TX_PrintLine = NULL,
	    GA_Gauge     = NULL,
	    BT_WorkAbbruch = NULL;

// FatalWin
static APTR BT_FatalAbbruch = NULL;



static const char LangerString[] = "                           ";

static char DVIpattern[16];
static const char * CYA_Seiten  [] = { NULL, NULL, NULL };


static struct List MessageList;



static void MUIinit(void);
static void MUISetWorkString(void);





/* MUIfree() wird *nur* von AbortRun() [globals.c] aufgerufen! */
void MUIfree(void)
{
  if (MUIAppObject) {
    MUI_DisposeObject(MUIAppObject);
    MUIAppObject = NULL;
  }

  if (MUIMasterBase) {
    CloseLibrary(MUIMasterBase);
    MUIMasterBase = NULL;
  }
}


static void MUIinit(void)
{
  if (!(MUIMasterBase = OpenLibrary(MUIMASTER_NAME,MUIMASTER_VMIN))) {
    usegui = FALSE;
    Fatal(15, MSG_NO_LIBRARY, MUIMASTER_NAME, MUIMASTER_VMIN);
  }
  
  NewList(&MessageList);

  CYA_Seiten[0] = GetTeXString(MSG_MUI_MW_SEITEN_ALL);
  CYA_Seiten[1] = GetTeXString(MSG_MUI_MW_SEITEN_VONBIS);
}


static __inline char * GetTeXLabel(int id)
{
  return (GetTeXString(id) + 2);
}

static __inline char GetTeXKey(int id)
{
  return *GetTeXString(id);
}



/*
**
** Message Window (Message/Warning/Fatal)
**
*/

static void InitMUIMessageWindow(void)
{
  if (MUIAppObject && !MessWin) {
    MessWin = WindowObject,
	MUIA_Window_Title, GetTeXString(MSG_MUI_ME_WINTITLE),
	MUIA_Window_ID,    MAKE_ID('M','E','S','S'),
	(ArgPubname) ? MUIA_Window_PublicScreen : TAG_IGNORE, ArgPubname,
	
	WindowContents, VGroup,
		Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, MUIA_FrameTitle, GetTeXString(MSG_MUI_ME_TITEL), End,
		Child, LV_Messages = ListviewObject,
					MUIA_Listview_Input,	FALSE,
					MUIA_Listview_List,	ListObject,
					ReadListFrame,
				     End, End,
		Child, TX_MessLine = TextObject, TextFrame, MUIA_Text_Contents, LangerString, MUIA_Background, MUII_TextBack,MUIA_InputMode, MUIV_InputMode_None, End,
		Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, End,
		Child, HGroup,
			Child, HSpace(0),
			Child, HSpace(20),
			Child, BT_CloseMess = SimpleButton(GetTeXString(MSG_MUI_ME_CLOSE)),
			Child, HSpace(20),
			Child, HSpace(0),
		       End,
		End,
	End;
 
    if (!MessWin) Fatal(10, MSG_MUI_MW_APP_FAIL);

    DoMethod(MUIAppObject, OM_ADDMEMBER, MessWin);

    DoMethod(MessWin,MUIM_Notify,MUIA_Window_CloseRequest,TRUE, MessWin, 3, MUIM_Set, MUIA_Window_Open, FALSE);
    DoMethod(BT_CloseMess, MUIM_Notify, MUIA_Pressed, FALSE, MessWin, 3, MUIM_Set, MUIA_Window_Open, FALSE);
  }
}

void MUIShowMessWin(void)
{
  static int notfirst = FALSE;		// ueberspringe die Copyright-Warnung
  long open;

  if (!notfirst) {
    notfirst = TRUE;
  }
  else {
    InitMUIMessageWindow();

    get(MessWin,MUIA_Window_Open,&open);  
    if (!open) set(MessWin, MUIA_Window_Open, TRUE);
  }
}

void MUIMessage(char * str)
{
  char * ptr, * old;
  struct ListEntry * Entry;
  
  if (!MUIMasterBase || !MUIAppObject) {
    usegui = FALSE;
    return;
  }

  // alle newlines wegloeschen (fuehrendes ueberspringen, restlichen ersetzen)
  if (*str == '\n') str++;
  while (ptr = strchr(str, '\n')) *ptr = ' ';

  InitMUIMessageWindow();

    
  get(TX_MessLine, MUIA_Text_Contents, &old);

  if(Entry = (struct ListEntry *)malloc(sizeof(struct ListEntry) + strlen(old) + 1)) {
    Entry -> Title = (STRPTR)(Entry + 1);
    strcpy(Entry -> Title, old);
    AddTail(&MessageList,(struct Node *)Entry);

    DoMethod(LV_Messages, MUIM_List_Insert, &Entry -> Title, 1, MUIV_List_Insert_Bottom);
    set(LV_Messages,MUIA_List_Active,MUIV_List_Active_Bottom);
  }

  set(TX_MessLine, MUIA_Text_Contents, str);
}



/*
**
** Working Window
**
*/

void InitMUIWorkWindow(void)
{
  APTR BT_SeeMess;

  if (MUIAppObject && !WorkWin) {
   WorkWin = WindowObject,
	MUIA_Window_Title, GetTeXString(MSG_MUI_WO_WINTITLE),
	MUIA_Window_ID,    MAKE_ID('W','O','R','K'),
	(ArgPubname) ? MUIA_Window_PublicScreen : TAG_IGNORE, ArgPubname,
	
	WindowContents, VGroup,
		Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, MUIA_FrameTitle, GetTeXString(MSG_MUI_WO_TITEL), End,
		Child, HGroup,
			Child, TX_PrintLine = TextObject, TextFrame, MUIA_Text_Contents, LangerString, MUIA_Background, MUII_TextBack, MUIA_InputMode, MUIV_InputMode_None, End,
			Child, BT_SeeMess = SimpleButton(GetTeXString(MSG_MUI_WO_SEEMESS)), MUIA_Weight, 5,
			End,
		Child, GA_Gauge = GaugeObject, GaugeFrame, MUIA_Gauge_Horiz, TRUE, MUIA_Gauge_InfoText, "", End,
		Child, ScaleObject, End,
		Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, End,
		Child, HGroup,
			Child, HSpace(0),
			Child, BT_WorkAbbruch = SimpleButton(GetTeXString(MSG_MUI_MW_CANCEL)),
			Child, HSpace(0),
			End,
		End,
	End;

    if (!WorkWin) Fatal(10, MSG_MUI_MW_APP_FAIL);

    DoMethod(MUIAppObject, OM_ADDMEMBER, WorkWin);

    DoMethod(WorkWin,MUIM_Notify,MUIA_Window_CloseRequest,TRUE, MUIAppObject, 2, MUIM_Application_ReturnID, CancelPrintID);
    DoMethod(BT_WorkAbbruch, MUIM_Notify, MUIA_Pressed, FALSE, MUIAppObject, 2, MUIM_Application_ReturnID, CancelPrintID);
    
    InitMUIMessageWindow();
    DoMethod(BT_SeeMess, MUIM_Notify, MUIA_Pressed, FALSE, MessWin, 3, MUIM_Set, MUIA_Window_Open, TRUE);
    
    set(WorkWin, MUIA_Window_Open, TRUE);
  }
}

static void MUISetWorkString(void)
{
  char * ptr, * startstr, * endstr;
  char sarr[16], earr[16];
  char s[170];

  InitMUIMessageWindow();
  
  ptr = FilePart(filename);
  
  if (FirstPage == -1000000L && LastPage  == 1000000L) {
    sprintf(s, "%s, %s", ptr, GetTeXString(MSG_MUI_WORK_DOALL));
  }
  else {
    if (FirstPage == -1000000L) {
      startstr = GetTeXString(MSG_MUI_WORK_START);
    }
    else {
      sprintf(sarr, "%ld", FirstPage);
      startstr = sarr;
    }

    if (LastPage == 1000000L) {
      endstr = GetTeXString(MSG_MUI_WORK_END);
    }
    else {
      sprintf(earr, "%ld", LastPage);
      endstr = earr;
    }

    sprintf(s,"%s, %s %s %s %s", ptr, GetTeXString(MSG_MUI_WORK_FROM), startstr, GetTeXString(MSG_MUI_WORK_TO), endstr);
  }
  
  set(TX_PrintLine, MUIA_Text_Contents, s);
}


void MUISetWorkCurPage(int pagenum)
{
  if (WorkWin) {
    static char ptr[50];

    sprintf(ptr, "%s %ld", GetTeXString(MSG_MUI_WORK_PAGE), pagenum);
    set(GA_Gauge, MUIA_Gauge_InfoText, ptr);
  }
  
  MUIEvent();
}


void MUISetGauge(int lines)
{
  if (WorkWin) {
    int proz = (lines * 100) / paper_height;
    
    if (proz > 100) proz = 100;
  
    set(GA_Gauge, MUIA_Gauge_Current, proz);
  }

  MUIEvent();
}





/*
**
** Fatal-Window
**
*/

void MUIFatal(char * str)
{
  int running = TRUE;

  if (!MUIMasterBase || !MUIAppObject) {
    usegui = FALSE;
    return;
  }

  if (!FatalWin) {
    FatalWin = WindowObject,
	MUIA_Window_Title, GetTeXString(MSG_MUI_FA_WINTITLE),
	MUIA_Window_ID,    MAKE_ID('F','A','T','A'),
	(ArgPubname) ? MUIA_Window_PublicScreen : TAG_IGNORE, ArgPubname,
	
	WindowContents, VGroup,
		Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, MUIA_FrameTitle, GetTeXString(MSG_MUI_FA_TITEL), End,
		Child, TextObject, TextFrame, MUIA_Text_Contents, str, MUIA_Background, MUII_TextBack, MUIA_InputMode, MUIV_InputMode_None, End,
		Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, End,
		Child, HGroup,
			Child, HSpace(0),
			Child, BT_FatalAbbruch = SimpleButton(GetTeXString(MSG_MUI_MW_CANCEL)),
			Child, HSpace(0),
			End,
		End,
	End;

    if (!FatalWin) AbortRun(20);	// hier *kein* Fatal!

    DoMethod(MUIAppObject, OM_ADDMEMBER, FatalWin);

    DoMethod(FatalWin,MUIM_Notify,MUIA_Window_CloseRequest,TRUE, MUIAppObject, 2, MUIM_Application_ReturnID, FatalID);
    DoMethod(BT_FatalAbbruch, MUIM_Notify, MUIA_Pressed, FALSE, MUIAppObject, 2, MUIM_Application_ReturnID, FatalID);
  }

  set(MainWin, MUIA_Window_Sleep, TRUE);
  if (WorkWin) set(WorkWin, MUIA_Window_Sleep, TRUE);
  if (MessWin) set(MessWin, MUIA_Window_Sleep, TRUE);

  set(FatalWin,MUIA_Window_Open,TRUE);

  while (running) {
	ULONG retsig = 0L;

	switch (DoMethod(MUIAppObject,MUIM_Application_Input,&GUIsignals)) {

		case FatalID:
			running = FALSE;
			break;
	}

        if (running && GUIsignals) {
          PROFILE_OFF();
          retsig = Wait(GUIsignals | SIGBREAKF_CTRL_C);
          PROFILE_ON();
        }

        if (retsig & SIGBREAKF_CTRL_C) running = FALSE;
  }

  set(FatalWin,MUIA_Window_Open,FALSE);
}



/*
**
** Main-Window
**
*/

void MUImainwin(void)
{
  APTR MainWin, POP_DVIfile, CY_Seiten, ST_von, ST_bis, ST_num, ST_kopien, BT_pref, BT_print, BT_cancel;
  BOOL running = TRUE;
  BOOL startprint = FALSE;
  
  (void)ParsePatternNoCase("#?.dvi", DVIpattern, sizeof(DVIpattern));

  MUIinit();

  MUIAppObject = ApplicationObject,
		MUIA_Application_Title      , "DVIprint",
		MUIA_Application_Version    , VersionStr,
		MUIA_Application_Copyright  , "©1990-94, Georg Heﬂmann. All†Rights†Reserved.",
		MUIA_Application_Author     , "Georg Heﬂmann",
		MUIA_Application_Description, GetTeXString(MSG_MUI_MW_DESCRIPTION),
		MUIA_Application_Base       , "DVIPRINT",

		SubWindow, MainWin = WindowObject,
			MUIA_Window_Title, "DVIprint",
			MUIA_Window_ID   , MAKE_ID('M','W','I','N'),
			(ArgPubname) ? MUIA_Window_PublicScreen : TAG_IGNORE, ArgPubname,

			WindowContents, VGroup,
			
				Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, MUIA_FrameTitle, GetTeXString(MSG_MUI_MW_TITEL), End,
				
				Child, HGroup,
					Child, KeyLabel2(GetTeXLabel(MSG_MUI_MW_DVIFILE),GetTeXKey(MSG_MUI_MW_DVIFILE)),
					Child, POP_DVIfile = PopaslObject,
						MUIA_Popstring_String, KeyString(filename,256,GetTeXKey(MSG_MUI_MW_DVIFILE)),
						MUIA_Popstring_Button, PopButton(MUII_PopFile),
						ASLFR_TitleText, GetTeXString(MSG_MUI_MW_ASL_DVIFILE),
						ASLFR_RejectIcons, TRUE,
						ASLFR_AcceptPattern, DVIpattern,
						End,
				        End,
				Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, End,
				Child, ColGroup(2),
					Child, KeyLabel1(GetTeXLabel(MSG_MUI_MW_SEITEN), GetTeXKey(MSG_MUI_MW_SEITEN)), Child, CY_Seiten = KeyCycle(CYA_Seiten, GetTeXKey(MSG_MUI_MW_SEITEN)),
					Child, HSpace(0),	 Child, ColGroup(4), 
									  Child, KeyLLabel2(GetTeXLabel(MSG_MUI_MW_VON), GetTeXKey(MSG_MUI_MW_VON)), 
									  Child, ST_von = StringObject, MUIA_ControlChar, GetTeXKey(MSG_MUI_MW_VON), MUIA_String_Accept, "1234567890", MUIA_String_MaxLen, 5, StringFrame, End,
									  Child, KeyLLabel2(GetTeXLabel(MSG_MUI_MW_BIS), GetTeXKey(MSG_MUI_MW_BIS)), 
									  Child, ST_bis = StringObject, MUIA_ControlChar, GetTeXKey(MSG_MUI_MW_BIS), MUIA_String_Accept, "1234567890", MUIA_String_MaxLen, 5, StringFrame, End,
									  End,
					Child, HSpace(0),	 Child, HSpace(0),
					Child, KeyLabel2(GetTeXLabel(MSG_MUI_MW_NUM), GetTeXKey(MSG_MUI_MW_NUM)), Child, HGroup,
							Child, ST_num = StringObject, MUIA_ControlChar, GetTeXKey(MSG_MUI_MW_NUM), StringFrame, MUIA_Weight, 90, MUIA_String_Accept, "1234567890", MUIA_String_MaxLen, 5, End, 
							Child, HSpace(0),
							Child, KeyLabel2(GetTeXLabel(MSG_MUI_MW_KOPIEN), GetTeXKey(MSG_MUI_MW_KOPIEN)), Child, ST_kopien = StringObject, MUIA_ControlChar, GetTeXKey(MSG_MUI_MW_KOPIEN), StringFrame, MUIA_Weight, 50, MUIA_String_Integer, 1, MUIA_String_Accept, "1234567890", MUIA_String_MaxLen, 5, End, 
							End,
				       End,
				Child, RectangleObject, MUIA_Rectangle_HBar, TRUE, MUIA_FixHeight, 8, End,
				Child, HGroup, MUIA_Group_SameSize, TRUE,
					Child, BT_print  = SimpleButton(GetTeXString(MSG_MUI_MW_DRUCKEN)),
					Child, BT_pref = SimpleButton(GetTeXString(MSG_MUI_MW_PREF)),
					Child, BT_cancel = SimpleButton(GetTeXString(MSG_MUI_MW_CANCEL)),
					End,
				End,
			End,
		End;

  if (!MUIAppObject) {
    usegui = FALSE;
    Fatal(10, MSG_MUI_MW_APP_FAIL);
  }



/*
** Aktionen auf Button legen
*/

  DoMethod(MainWin,MUIM_Notify,MUIA_Window_CloseRequest,TRUE, MUIAppObject,2,MUIM_Application_ReturnID,MUIV_Application_ReturnID_Quit);
  DoMethod(BT_cancel,MUIM_Notify,MUIA_Pressed,FALSE, MUIAppObject,2,MUIM_Application_ReturnID,MUIV_Application_ReturnID_Quit);
  DoMethod(BT_pref,MUIM_Notify,MUIA_Pressed,FALSE, MUIAppObject,2,MUIM_Application_ReturnID, PrefsID);
  DoMethod(BT_print,MUIM_Notify,MUIA_Pressed,FALSE, MUIAppObject,2,MUIM_Application_ReturnID, PrintID);
  
  set(BT_pref, MUIA_Disabled, TRUE);	// ist noch nicht implementiert




/*
** Defaultwerte nach den Optionen setzen
*/

  /*
  ** Setzen der MainWindow Werte
  */
  if (FirstPage != -1000000L) {
    set(ST_von, MUIA_String_Integer, FirstPage);
    set(CY_Seiten, MUIA_Cycle_Active, 1);
  }
  if (LastPage  != 1000000L) {
    set(ST_bis, MUIA_String_Integer, LastPage);
    set(CY_Seiten, MUIA_Cycle_Active, 1);
  }
  if (NrOfPagesToPrint != 1000000L) {
    set(ST_num, MUIA_String_Integer, NrOfPagesToPrint);
  }
  set(ST_kopien, MUIA_String_Integer, ncopies);


/*
** CY_Seiten -- ST_von -- ST_bis handling
*/

  DoMethod(ST_von, MUIM_Notify, MUIA_String_Acknowledge, MUIV_EveryTime, CY_Seiten, 3, MUIM_Set, MUIA_Cycle_Active, 1);
  DoMethod(ST_bis, MUIM_Notify, MUIA_String_Acknowledge, MUIV_EveryTime, CY_Seiten, 3, MUIM_Set, MUIA_Cycle_Active, 1);

  DoMethod(CY_Seiten, MUIM_Notify, MUIA_Cycle_Active, 0, ST_von, 3, MUIM_Set, MUIA_Disabled, TRUE);
  DoMethod(CY_Seiten, MUIM_Notify, MUIA_Cycle_Active, 0, ST_bis, 3, MUIM_Set, MUIA_Disabled, TRUE);
  DoMethod(CY_Seiten, MUIM_Notify, MUIA_Cycle_Active, 0, MainWin, 3, MUIM_Set, MUIA_Window_ActiveObject, NULL); // diese Zeile ist notwendig, weil MUI auch disablede Gadgets aktiviert :-(
  DoMethod(CY_Seiten, MUIM_Notify, MUIA_Cycle_Active, 1, ST_von, 3, MUIM_Set, MUIA_Disabled, FALSE);
  DoMethod(CY_Seiten, MUIM_Notify, MUIA_Cycle_Active, 1, ST_bis, 3, MUIM_Set, MUIA_Disabled, FALSE);
  DoMethod(CY_Seiten, MUIM_Notify, MUIA_Cycle_Active, 1, MainWin, 3, MUIM_Set, MUIA_Window_ActiveObject, ST_von);
  
  { 
    long ac;
    get(CY_Seiten, MUIA_Cycle_Active, &ac);
    if (ac == 0) {
      nnset(ST_von, MUIA_Disabled, TRUE);
      nnset(ST_bis, MUIA_Disabled, TRUE);
    }
  }


/*
** set cycle-chain 
*/

  DoMethod(MainWin, MUIM_Window_SetCycleChain, POP_DVIfile, CY_Seiten, ST_von, ST_bis, ST_num, ST_kopien, BT_print, BT_pref, BT_cancel, NULL);

  // RETURN cycle
  DoMethod(POP_DVIfile, MUIM_Notify, MUIA_String_Acknowledge, MUIV_EveryTime, MainWin, 3, MUIM_Set, MUIA_Window_ActiveObject, CY_Seiten);
  DoMethod(ST_von,      MUIM_Notify, MUIA_String_Acknowledge, MUIV_EveryTime, MainWin, 3, MUIM_Set, MUIA_Window_ActiveObject, ST_bis);
  DoMethod(ST_bis,      MUIM_Notify, MUIA_String_Acknowledge, MUIV_EveryTime, MainWin, 3, MUIM_Set, MUIA_Window_ActiveObject, ST_num);
  DoMethod(ST_num,      MUIM_Notify, MUIA_String_Acknowledge, MUIV_EveryTime, MainWin, 3, MUIM_Set, MUIA_Window_ActiveObject, ST_kopien);
  DoMethod(ST_kopien,   MUIM_Notify, MUIA_String_Acknowledge, MUIV_EveryTime, MainWin, 3, MUIM_Set, MUIA_Window_ActiveObject, BT_print);




/*
** Input loop...
*/

  set(MainWin,MUIA_Window_Open,TRUE);


  while (running && !startprint) {
		ULONG retsig = 0L;

		switch (DoMethod(MUIAppObject,MUIM_Application_Input,&GUIsignals))
		{
			case MUIV_Application_ReturnID_Quit:
				running = FALSE;
				break;
			case PrefsID:
				break;
			case PrintID:
				set(MainWin, MUIA_Window_Sleep, TRUE);
				startprint = TRUE;
				break;
			case CancelPrintID:
				// kann eigentlich nicht vorkommen
				running = FALSE;
				break;
		}

	        if (running && !startprint && GUIsignals) {
	          PROFILE_OFF();
                  retsig = Wait(GUIsignals | SIGBREAKF_CTRL_C);
                  PROFILE_ON();
                }

	        if (retsig & SIGBREAKF_CTRL_C) running = FALSE;
  }

  set(MainWin,MUIA_Window_Open,FALSE);  // gut oder nicht??




/*
**
** Die Variablen nach den Werten in den Gadgets setzen
**
*/

  { char * ptr = NULL;

    get(ST_von, MUIA_String_Contents, &ptr);
    if (ptr && *ptr) get(ST_von, MUIA_String_Integer, &FirstPage);
    else FirstPage = -1000000L;

    get(ST_bis, MUIA_String_Contents, &ptr);
    if (ptr && *ptr) get(ST_bis, MUIA_String_Integer, &LastPage);
    else LastPage = 1000000L;

    get(ST_num, MUIA_String_Contents, &ptr);
    if (ptr && *ptr) get(ST_num, MUIA_String_Integer, &NrOfPagesToPrint);
    else NrOfPagesToPrint = 1000000L;

    get(ST_kopien, MUIA_String_Integer, &ncopies);
    if (ncopies < 1) ncopies = 1;

    get(POP_DVIfile, MUIA_String_Contents, &ptr);
    strcpy(filename, ptr);
  }
  
  if (running) {
    InitMUIWorkWindow();
    MUISetWorkString();
  }
  else {
    CXBRK();
  }

}


void MUIEvent(void)
{
  switch (DoMethod(MUIAppObject,MUIM_Application_Input,&GUIsignals)) {
	case MUIV_Application_ReturnID_Quit:
	case CancelPrintID:
		CXBRK();	/* mach Schluss Mann, ey */
		break;
	case PrefsID:
	case PrintID:
		// kann nicht sein
		break;
  }
}


