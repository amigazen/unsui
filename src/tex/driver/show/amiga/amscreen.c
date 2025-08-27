#ifdef AMIGA

/*******************************************************************/
/*                                                                 */
/*  amscreen.c    Screenroutinen fuer den Bildschirmtreiber        */
/*                showdvi                                          */
/*                                                                 */
/*                von Georg Hessmann                               */
/*                11.07.88                                         */
/*                                                                 */
/*  void Open_Showdvi()                                            */
/*  void close_all_bild()                                          */
/*  void clear_bild()                                              */
/*  int  ShowPage()                                                */
/*  long *Init_all(long,long)                                      */
/*                                                                 */
/*******************************************************************/

#include "defines.h"
#undef DEBUG
#undef BACKGROUND
#undef CATCH
#define CATCH_ZERO


/* #define DOS_DOSASL_H */

#include <sprof.h>

#include <stdio.h>
#include <signal.h>
#include <dos.h>


#include <exec/types.h>
#include <dos/dos.h>
#include <dos/dostags.h>
#include <dos/notify.h>
#include <graphics/gfxbase.h>
#include <graphics/scale.h>
#include <graphics/gfxmacros.h>
#include <graphics/layers.h>
#include <intuition/intuitionbase.h>
#include <intuition/intuition.h>
#include <intuition/imageclass.h>
#include <libraries/dos.h>
#include <libraries/asl.h>
#include <exec/execbase.h>
#include <exec/tasks.h>
#include <exec/memory.h>
#include <devices/timer.h>
#include <devices/printer.h>
#include <libraries/dosextens.h>
#include <libraries/dos.h>
#include <workbench/icon.h>
#include <workbench/workbench.h>
#include <exec/devices.h>
#include <devices/console.h>

#ifdef AZTEC_C
#  include <functions.h>
#endif

#include "globals.h"

#ifdef ANSI
#  include <string.h>
#  include <stdlib.h>
#endif

#include <clib/intuition_protos.h>
#include <clib/graphics_protos.h>
#include <clib/layers_protos.h>
#include <clib/exec_protos.h>
#include <clib/dos_protos.h>
#include <clib/timer_protos.h>
#include <clib/diskfont_protos.h>
#include <clib/alib_protos.h>
#include <clib/gadtools_protos.h>
#include <clib/wb_protos.h>

#include <pragmas/exec_pragmas.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/layers_pragmas.h>
#include <pragmas/icon_pragmas.h>
#include <pragmas/wb_pragmas.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/gadtools_pragmas.h>
#include <pragmas/graphics_pragmas.h>
#include <pragmas/diskfont_pragmas.h>
#include <pragmas/req.h>

#ifndef MTYPE_APPWINDOW
# define MTYPE_APPWINDOW		7	/* msg from an app window */
#endif

#include "version.h"
#include "gad_def.h"
#include "amscreen.h"
#include "amprint.h"
#include "minrexx.h"
#include "arexx.h"
#include "searchwin.h"

#include "bitmap.h"
extern struct bitmap	map;

#include "amscreen.i"
#include "amkey.i"
#include "showdvi.i"
#include "gadget.i"
#include "messwin.i"
#include "amprhelp.i"
#include "am_requ.i"
#include "am_menu.i"
#include "dvihand.i"
#include "globals.i"
#include "config.i"
#include "liste.i"
#include "prefwin.i"
#include "local.i"
#include "help.i"
#include "search.i"
#include "fast_cp.i"	// SetRule()
#include "searchwin.i"
#include "fullpage.i"


/* #include "pgscroll.i" */
/* #include "swin.i" */

#include "newcli.i"

#if defined(APP_WINDOW)
# include "app_win.i"
#else
# include "app_icon.i"
#endif


#if defined(ARP_LIBRARY)
# include "small_arp.h"
#endif
#if defined(REQ_LIBRARY)
# include <libraries/reqbase.h>
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




/* lokale Funktionen */
static void *OLib			Args((char *name, int vers, int required));
static void OpenLib			Args((void));
static void CloseLib 			Args((void));
static void InitBoopsi			Args((void));
static void FreeBoopsi			Args((void));
static void MyNewObject			Args((struct Image ** img,
					      ULONG tag,
					      char * str));
static void MyDisposeObject		Args((APTR * obj));
static void CPUBlit			Args((WORD SrcX, WORD SrcY, WORD DestX, WORD DestY, 
						WORD XSize, WORD YSize));
static void CloseS			Args((void));
static void CloseWin2			Args((void));
static void CloseRastPort		Args((void));
static int  test_anz_windows		Args((void));
static void OpenRastPort		Args((void));
static void close_fonts			Args((void));
static void open_fonts			Args((void));
static void OpenS			Args((void));
static void OpenWin2			Args((void));
#if 0
static void DeleteTimer			Args((struct timerequest *timermsg));
static void NewTimeRequest		Args((void));
static struct timerequest *CreateTimer	Args((void));
#endif
static void start_print_page		Args((void));
static void check_print_page		Args((void));
static void __inline win_mov		Args((const long dx,
					      const long dy,
					      const long x,
					      const long y));
static ULONG GetWBModeID		Args((void));
static int get_screen_dimension		Args((long *width,
					      long *height,
					      short *lace));
static void set_resolution		Args((void));
static void set_i_window_size		Args((void));

static int  InitDVINotify		Args((void));
static int  EndDVINotify		Args((void));
static long work_with_main_app_win	Args((void));
static long intui_message		Args((void));
static long scroll			Args((void));
static void save_active_window		Args((void));
static /*__stdargs*/ long  arexx_disp	Args((struct RexxMsg *msg,
					      struct rexxCommandList *dat,
					      char *p));
static void sighandling			Args((int i));
static void DeleteShowDVIVars		Args((void));

#ifdef CATCH
static void onGURU			Args((void));
#endif



static int is_cyber = 0;
static int WidthSysDepthGadget = 0;

/*----------------------------------------------------------------------*/
struct Screen		*screen  = NULL;
struct Window		*win2    = NULL;
struct AppWindow 	*MainAppWin = NULL;	// AppWin bzgl. win2, falls screen ein WBSCR ist
struct MsgPort		*MainAppWinMsgPort = NULL;

struct IntuitionBase	* IntuitionBase	 = NULL;
struct GfxBase		* GfxBase	 = NULL;
struct ArpBase		* ArpBase	 = NULL;
struct Library		* DiskfontBase   = NULL;
struct Library		* WorkbenchBase	 = NULL;
struct Library		* IconBase	 = NULL;
struct Library		* GadToolsBase	 = NULL;
struct Library 		* AslBase	 = NULL;
struct Library          * AmigaGuideBase = NULL;
struct Library          * UtilityBase    = NULL;
struct Library		* LayersBase     = NULL;

#if defined(REQ_LIBRARY)
 struct ReqLib		*ReqBase	= NULL;
#endif

struct Library		*ConsoleDevice  = NULL;
struct IOStdReq 	ConsoleReq;




struct BitMap		BitMapWin;
struct RastPort		myRastPort;
#if 0
struct timerequest	*TimeRequest = NULL;
#endif

extern struct MsgPort	*printerPort;	/* aus util/amprhelp.c */
extern union printerIO	*request;

extern long		current_page_phy;
extern long		current_page;

extern DVIFILE 		*dvifp;         /* DVI file pointer                    */


extern struct Window	*MessWin;
extern short 		MessWinXpos;	/* wird in OpenS() auf -1 gesetzt */
extern short 		MessWinYpos;




char task_name[]	= "ShowDVI-Task";		/* new task name */
char *old_task_name	= NULL;				/* old task name */
struct Window		*oldactiveWindow = NULL;	/* was active window */


#if 0
#define T_REPLYPORT	TimeRequest->tr_node.io_Message.mn_ReplyPort
#endif

long wx,wy;				/* Breite-Hoehe der BitMap !!	*/

long static_x_Koo,			/* akt. Anzeigekoo. der Bitmap	*/
     static_y_Koo;

/*
 * Die folgenden 4 Variablen geben an, welcher Berei des Windows
 * tatsaechlich von der Seite belegt wird.
 * Normalerweise entsprechen die 4 Punkte den Window-Borders.
 * Eben dann wenn eine Seite in keiner Richtung ganz in das Window
 * passt. Wenn dem nicht so ist, dann geben die Werte eben an, wo
 * die Seite dargestellt werden soll.
 *
 * Belegt werden die Werte in set_i_window_size();
 *
 * Benoetigt werden die Werte ueberall in den Anzeigefunktionen 
 * und im Backfill Hook.
 *
 */

extern WORD Page_LeftPoint;
extern WORD Page_RightPoint;
extern WORD Page_TopPoint;
extern WORD Page_BottomPoint;



short width_left_border;
short width_right_border;
short HeightScreenTitle;	
short HeightWinTitle;

/* wird fuer das MEss-Window benoetigt */
short FullPageLeftEdge;
short FullPageTopEdge;
short FullPageWidth;
short FullPageHeight;
short FullPageScreenXPos;	/* da war vor dem full-page der Screen */
short FullPageScreenYPos;


/* Variablen fuer die Full-Page */
static struct BitMap   BitMapFullPage;
static struct RastPort RastPortFullPage;
static short BitMapFullPageWidth, BitMapFullPageHeight;

static struct Window *old_Win_Ptr_Requester = NULL;	/* aus der Taskstruktur */

static short	win_need_refresh = FALSE;	/* gesetzt bei neuem RPort, verwendet vor scroll() */

static struct NotifyRequest notify;

static int IsScreenReOpend;


/*
 * Zum IFF-Speichern der Seite
 */
#include "iff/ilbmapp.h"
static struct ILBMInfo ilbm;
static UWORD           bw_ctable[2] = { 0x0FFF, 0x0000 };



struct state show_state;	/* Anzeigezustand			*/
	/*  is_lace  == FALSE 	   Anzeige im LACE-Modus?		*/
	/*  is_show  == FALSE 	   full-page Anzeigemodus?		*/
	/*  is_gadg  == FALSE 	   Anzeige mit Scrollbars?		*/
	/*  is_print == FALSE 	   Laeuft ein Ausdruck?			*/
	/*  is_timer == FALSE 	   Laeuft ein Timer Event?		*/
	/*  is_pscro == FALSE 	   Pagescrollingmodus?			*/
	/*  is_colre == FALSE      Color Requester am Bildschirm	*/
	/*  is_about == FALSE      About-Requester am Bildschirm	*/
	/*  is_dvif  == FALSE	   No DVI-File loadet			*/
	/*  ...			   und vieles vieles mehr		*/
	/*  set_xxx		   Aktivieren des jeweiligen Modi	*/
	/*  unset_xxx		   Loeschen des jeweiligen Modi		*/
	/*  toggle_xxx		   Umschalten des jeweiligen Modi	*/





struct BoopsiObj BObj;	/* Boopsi Object/Images */


struct current_color current_col;
struct current_color default_col = { 3, 2, 2, 6, 7, 8, 13, 13, 13, 13, 0, 0};


short	x_scr_width,		/* aktuelle Screen Breite-Hoehe		*/ 
	x_scr_height,
	x_win_width,		/* aktuelle Window Breite-Hoehe		*/
	x_win_height,
	x_win_i_width,		/* aktuelle 'innen' Window Breite-Hoehe	*/
	x_win_i_height;		/* abzueglich Scrollbar, falls vorhanden*/

#define winiw	x_win_i_width
#define winih	x_win_i_height


static short old_win_pos_x;	/* wird benoetigt um festzustellen, ob	*/
static short old_win_pos_y;	/* das Window verschoben wurde.		*/


char  PubScreenName[MAXPUBSCREENNAME+1];
char  MyPubScreenName[MAXPUBSCREENNAME+1];


struct TextFont	*font8;
struct TextFont	*font11;

struct TextAttr	txtAttr8;
struct TextAttr	txtAttr11;
struct TextAttr txtAttr;		/* TxtAttr fuer Menues oder sonstiges */
struct TextAttr GadtxtAttr;		/* TxtAttr fuer die Gadgets in der Wintow-Kopf-Zeile */

#define FONTNAME8	"topaz.font"
#define FONTYSIZE8	8
#define FONTXSIZE8	8		/* stimmt doch, oder? */
#define FONTSTYLE8	FS_NORMAL
#define FONTFLAGS8	FPF_ROMFONT

#define FONTNAME11	"topaz.font"
#define FONTYSIZE11	11
#define FONTXSIZE11	8		/* stimmt doch, oder? */
#define FONTSTYLE11	FS_NORMAL
#define FONTFLAGS11	FPF_DISKFONT


/* wird fuer auto-load-again gebraucht */
static short is_new_dvifile = TRUE;	/* das erste File ist immer ein neues File */


int	background = FALSE;		/* start mit 'run' */

#ifdef BACKGROUND
long _stack	   = MIN_STACK_SIZE;	/* Stack space for the created task		*/
char *_procname	   = "ShowDVI-Task";	/* Name of the process to be created		*/
long _priority	   = 0;			/* The priority to create the process at	*/
long _BackGroundIO = 0;			/* The flag to indicate an I/O requirement	*/
#endif

#ifdef CATCH
extern void *_ONGURU;
#endif

static int timer_wait_ticks;	/* wait intervalls fou clearing message */

UBYTE sig_print, sig_timer, sig_win, sig_app, sig_appwin, sig_aboutwin, sig_messwin, 
	sig_prefwin, sig_notify, sig_screenclose, sig_searchwin;
ULONG sig_amigaguide_long;

#define SIG_PRINT	((unsigned long)(1<<sig_print))       /* Ende der Hardcopy	*/
#define SIG_TIMER	((unsigned long)(1<<sig_timer))       /* Ablauf des Timers	*/
#define SIG_WIN		((unsigned long)(1<<sig_win))         /* Normale IntuiMess	*/
#define SIG_APP		((unsigned long)(1<<sig_app))         /* WB-Application Message	*/
#define SIG_APPWIN	((unsigned long)(1<<sig_appwin))      /* App-Win Message	*/
#define SIG_ABOUTWIN	((unsigned long)(1<<sig_aboutwin))    /* About-Win Message	*/
#define SIG_MESSWIN	((unsigned long)(1<<sig_messwin))     /* Messure-Win Message	*/
#define SIG_PREFWIN	((unsigned long)(1<<sig_prefwin))     /* Preference-Win Message	*/
#define SIG_SEARCHWIN	((unsigned long)(1<<sig_searchwin))   /* Preference-Win Message	*/
#define SIG_NOTIFY	((unsigned long)(1<<sig_notify))      /* Notify Signal 		*/
#define SIG_SCREENCLOSE	((unsigned long)(1<<sig_screenclose)) /* Screen schliesen?	*/
#define SIG_MAINAPPWIN	((unsigned long)(1<<MainAppWinMsgPort->mp_SigBit))   /* AppWin2 */
#define SIG_AMIGAGUIDE	(sig_amigaguide_long)		      /* AmigaGuide Signal      */

	/* Alle sigs mit 0 vorbelegt, nur sig_win ist immer != 0	*/

long SIG_REXX;					   /* ARexx Command arrives	*/


/*----------------------------------------------------------------------*/


static void __asm MySetABPenDrMd(register __a1 struct RastPort *, register __d0 ULONG, register __d1 ULONG, register __d2 ULONG);

static void __asm MySetABPenDrMd(register __a1 struct RastPort * rp, register __d0 ULONG a, register __d1 ULONG b, register __d2 ULONG dr)
{
  if (GfxBase->LibNode.lib_Version >= 39) SetABPenDrMd(rp, a, b, dr);
  else {
    SetAPen(rp, a); SetBPen(rp, b); SetDrMd(rp, dr);
  }
}


/*----------------------------------------------------------------------*/


static void *OLib(char *name, int vers, int required)
{
  void *ret;
  
  ret = OpenLibrary(name, vers);
  
  if (ret == NULL) {
    if (required) {
      Fatal(10, MSG_NO_LIBRARY, name, vers);
    }
    else {
      Warning(MSG_NO_LIBRARY, name, vers);
    }
  }
  
  return ret;
}



static void OpenLib(void)
{
  IntuitionBase = (struct IntuitionBase *)OLib("intuition.library", 0, TRUE);
  if (IntuitionBase->LibNode.lib_Version < 36L) {
    unset_os2;
  }
  else {
    set_os2;
    if (IntuitionBase->LibNode.lib_Version > 38L) {
      set_os3;
    }
  }
  GfxBase = (struct GfxBase *)OLib(GRAPHICSNAME, 0, TRUE);
  LayersBase = OLib("layers.library", 0, TRUE);
  DiskfontBase = OLib("diskfont.library", 0, TRUE);

  memset(&ConsoleReq,0 ,sizeof(ConsoleReq));
  if (!OpenDevice("console.device",-1L, &ConsoleReq, 0L)) {
    ConsoleDevice = &(ConsoleReq.io_Device->dd_Library);
  }
  else {
    Fatal(20, MSG_NO_CONSOLE_DEVICE);
  }

  if (is_os2) {
    WorkbenchBase = OLib(WORKBENCH_NAME, 0, TRUE);
    IconBase = OLib(ICONNAME, 0, TRUE);
    GadToolsBase = OLib("gadtools.library", 0, TRUE);
    AslBase = OLib(AslName, 0, TRUE);
    if (AslBase->lib_Version >= 38L) {
      set_os21;
    }
    AmigaGuideBase = OLib("amigaguide.library", 39, FALSE);	// ACHTUNG bei PROFILING!!
    UtilityBase    = OLib("utility.library", 0, TRUE);
  }
#if defined(ARP_LIBRARY)
  ArpBase = (struct ArpBase *)OLib(ArpName, 0, FALSE);
  if (ArpBase == NULL) unset_arp;
#else
  unset_arp;
#endif
#if defined(REQ_LIBRARY)
  ReqBase = OLib("req.library", 0, FALSE);
#endif

  // Signal fuer das Screen Schliesen allozieren
  if (is_os2) {
    sig_screenclose = AllocSignal(-1);
    if (sig_screenclose == -1) {
      Fatal(20, MSG_NO_SIGNAL);
    }
  }
}


static void CloseLib(void)
{
  if (is_os2) {
    FreeSignal(sig_screenclose);

    if (WorkbenchBase != NULL) {
      CloseLibrary(WorkbenchBase);
      WorkbenchBase = NULL;
    }
    if (IconBase != NULL) {
      CloseLibrary(IconBase);
      IconBase = NULL;
    }
    if (GadToolsBase != NULL) {
      CloseLibrary(GadToolsBase);
      GadToolsBase = NULL;
    }
    if (AslBase != NULL) {
      CloseLibrary(AslBase);
      AslBase = NULL;
    }
    if (AmigaGuideBase) {
      CloseLibrary(AmigaGuideBase);
      AmigaGuideBase = NULL;
    }
    if (UtilityBase) {
      CloseLibrary(UtilityBase);
      UtilityBase = NULL;
    }
  }

  if (ConsoleDevice != NULL) {
	CloseDevice(&ConsoleReq);
	ConsoleDevice = NULL;
  }

  if (DiskfontBase != NULL) {
	CloseLibrary(DiskfontBase);
	DiskfontBase = NULL;
  }
  if (GfxBase != NULL) {
	CloseLibrary((struct Library *)GfxBase);
	GfxBase = NULL;
  }
  if (IntuitionBase != NULL) {
	CloseLibrary((struct Library *)IntuitionBase);
	IntuitionBase = NULL;
  }
#if defined(ARP_LIBRARY)
  if (is_arp && ArpBase != NULL) {
    unset_arp;
    CloseLibrary(&(ArpBase->LibNode));
    ArpBase = NULL;
  }
#endif
#if defined(REQ_LIBRARY)
  if (ReqBase != NULL) {
    CloseLibrary((struct Library *)ReqBase);
    ReqBase = NULL;
  }
#endif
}




/* allocate all objects in 'BoopsiObj' Muss jedesmal nach OpenS() gemacht werden! */
static void InitBoopsi(void)
{
  if (is_os2 && screen != NULL) {
	MyNewObject(&BObj.LArrImage, LEFTIMAGE, "LEFTIMAGE");
	MyNewObject(&BObj.RArrImage, RIGHTIMAGE, "RIGHTIMAGE");
	MyNewObject(&BObj.UArrImage, UPIMAGE, "UPIMAGE");
	MyNewObject(&BObj.DArrImage, DOWNIMAGE, "DOWNIMAGE");
	MyNewObject(&BObj.SizeImage, SIZEIMAGE, "SIZEIMAGE");

  }
}




/* deallocate all objects in 'BoopsiObj' Muss jedesmal vor CloseS() gemacht werden! */
static void FreeBoopsi(void)
{
  if (is_os2 && screen != NULL) {
    MyDisposeObject((APTR*)&BObj.LArrImage);
    MyDisposeObject((APTR*)&BObj.RArrImage);
    MyDisposeObject((APTR*)&BObj.UArrImage);
    MyDisposeObject((APTR*)&BObj.DArrImage);
    MyDisposeObject((APTR*)&BObj.SizeImage);
  }
}



static struct TagItem NewObjTags[] =
  {
	    { SYSIA_DrawInfo,	0L	},
	    { SYSIA_Which,	0L	},
	    { TAG_DONE,		0L	}
  };


static void MyNewObject(struct Image ** img, ULONG tag, char * str)
{
  NewObjTags[0].ti_Data = (ULONG)SDVI_DRI;
  NewObjTags[1].ti_Data = tag;
  if (!( *img = NewObjectA(NULL, "sysiclass", NewObjTags))) {
    Fatal(20, MSG_NO_NEW_OBJECT, str);
  }
}


static void MyDisposeObject(APTR * obj)
{
  if (*obj) {
    DisposeObject(*obj);
    *obj = NULL;
  }
}




void FreeChipPuffRP(void)
{
  if (show_state.ChipPuffBM) {
    if (FALSE && is_os3) {
      // siehe eins tiefer
      FreeBitMap(show_state.ChipPuffBM);
    }
    else {
      FreeRaster(show_state.ChipPuffBM->Planes[0], show_state.ChipPuffBM->BytesPerRow*8, show_state.ChipPuffBM->Rows);
      xfree(show_state.ChipPuffBM);
    }
    show_state.ChipPuffBM = NULL;
  }
}


void AllocChipPuffRP(void)
{
  if (FALSE && is_os3) {
    // ich brauch eine Bitmap, die 8 identische Planes hat...also nix mit AllocBitmap!!
    show_state.ChipPuffBM = AllocBitMap(win2->Width, win2->Height, 1, BMF_CLEAR, win2->RPort->BitMap);
  }
  else {
    show_state.ChipPuffBM = xmalloc(sizeof(struct BitMap));
    InitBitMap(show_state.ChipPuffBM, 8, win2->Width, win2->Height);
    show_state.ChipPuffBM->Planes[0] = AllocRaster(show_state.ChipPuffBM->BytesPerRow*8, show_state.ChipPuffBM->Rows);
    if (!show_state.ChipPuffBM->Planes[0]) {
      xfree(show_state.ChipPuffBM);
      show_state.ChipPuffBM = NULL;
    }
  }

  if (!show_state.ChipPuffBM) Fatal(19, MSG_NO_MEM);
  
  { // mache 8 identische Planes (um eine schwarze Schrift zu erzeugen)
    int i;
    for (i=1; i<8; i++) {
      show_state.ChipPuffBM->Planes[i] = show_state.ChipPuffBM->Planes[0];
    }
  }

  InitRastPort(&show_state.ChipPuffRP);
  show_state.ChipPuffRP.BitMap = show_state.ChipPuffBM;

  D(bug("ChipPuffBM alloziert, Breite: %ld, Hoehe: %ld, Tiefe: %ld\n", 
		show_state.ChipPuffBM->BytesPerRow*8, show_state.ChipPuffBM->Rows, show_state.ChipPuffBM->Depth));
}


static void CPUBlit(WORD SrcX, WORD SrcY, WORD DestX, WORD DestY, WORD XSize, WORD YSize)
{
  int i;
  struct BitMap * src  = myRastPort.BitMap;
  struct BitMap * dest = show_state.ChipPuffBM;
  long bSrcX, bDestX, bXSize;
  long aSrcX, aDestX;
  
  if (DestY+YSize >= dest->Rows) {
    D(bug("CPUBlit: Zu hoch! DestY: %ld, YSize: %ld, Hoehe: %ld\n", DestY, YSize, dest->Rows));
    YSize = dest->Rows - DestY-1;
    if (YSize < 0) return;
  }
  if (DestX/8+(XSize+15)/8 >= dest->BytesPerRow) {
    D(bug("CPUBlit: Zu breit! DestX: %ld, XSize: %ld, Breite: %ld\n", DestX, XSize, dest->BytesPerRow*8));
    XSize = (dest->BytesPerRow-1)*8 - DestX - 1;	// BPRow-1 wg. +15
    if (XSize < 0) return;
  }
  
  bSrcX  = SrcX  / 8;
  bDestX = DestX / 8;
  bXSize = (XSize + 15) / 8;	// +15 wg. Verschnitt vorne und hinten
  aSrcX  = SrcX  & 7;
  aDestX = DestX & 7;	 
  
  
  
  /* 
  D(bug("CPUBlit SX: %ld, SY: %ld, DX: %ld, DY: %ld, XS: %ld, YS: %ld, aSX: %ld, aDX: %ld\n", 
	SrcX, SrcY, DestX, DestY, XSize, YSize, aSrcX, aDestX));
  */
  
  for (i=0; i<YSize; i++) {
    CopyMem(((char *)src->Planes[0])  + (SrcY+i)  * src->BytesPerRow  + bSrcX, 
            ((char *)dest->Planes[0]) + (DestY+i) * dest->BytesPerRow + bDestX,
            bXSize);
  }


#ifdef WEG
  MySetABPenDrMd(win2->RPort, show_state.APen, show_state.BPen, JAM2);
  { register long x = DestX+(aSrcX-aDestX);
    BltTemplate(show_state.ChipPuffRP.BitMap->Planes[0]+DestY*show_state.ChipPuffRP.BitMap->BytesPerRow+x/16, x%16, myRastPort.BitMap->BytesPerRow, win2->RPort, DestX, DestY, XSize, YSize);
  }
  return;
#endif

  { register long x = DestX+(aSrcX-aDestX);
    register long y = DestY;
    MySetABPenDrMd(win2->RPort, show_state.APen, (is_bhook) ? show_state.BPen : 0, JAM2);
    BltTemplate(show_state.ChipPuffRP.BitMap->Planes[0]+y*show_state.ChipPuffRP.BitMap->BytesPerRow+(x/16)*2, x%16,
		show_state.ChipPuffRP.BitMap->BytesPerRow,
		win2->RPort, DestX, DestY, XSize, YSize);
  }

#ifdef OLD_BLIT
  //ClipBlit(&show_state.ChipPuffRP, DestX+(aSrcX-aDestX), DestY, win2->RPort, DestX, DestY, XSize, YSize, 0xC0);

  if (is_bhook) {
    SafeSetWriteMask(win2->RPort, 0x02);
    ClipBlit(&show_state.ChipPuffRP, DestX+(aSrcX-aDestX), DestY, win2->RPort, DestX, DestY, XSize, YSize, 0x30);
  }
  SafeSetWriteMask(win2->RPort, 0x01);
  ClipBlit(&show_state.ChipPuffRP, DestX+(aSrcX-aDestX), DestY, win2->RPort, DestX, DestY, XSize, YSize, 0xC0);
  SafeSetWriteMask(win2->RPort, 0xff);
#endif
}



static void CloseS(void)
{
  FreeBoopsi();		/* deallocate all in 'BoopsiObj' */

  if (screen != NULL) {
    if (is_os2) {
      if (SDVI_VI != NULL) {
        FreeVisualInfo(SDVI_VI);
        SDVI_VI = NULL;
      }
      if (SDVI_DRI) {
        FreeScreenDrawInfo(screen, SDVI_DRI);
        SDVI_DRI = NULL;
      }
    }
    if (is_otherpubscr) {
      UnlockPubScreen(NULL, screen);
    }
    else {
      if (is_myscr) {	/* ich hab den Screen geoeffnet, also schliess ich ihn auch wieder */
        if (is_os2) {
          if (CloseScreen(screen)) {
            screen = NULL;
          }
          else {
            ULONG ret_sigs;
            Okay1(GetTeXString(MSG_CANT_CLOSE_SCREEN));
            
            printf("sig: %d, SIG: %x\n", sig_screenclose, SIG_SCREENCLOSE);
	    printf("vor wait %x\n", SIG_SCREENCLOSE | SIGBREAKF_CTRL_C);
	    PROFILE_OFF();
  	    ret_sigs = Wait(SIG_SCREENCLOSE | SIGBREAKF_CTRL_C);
  	    PROFILE_ON();
  	    printf("nach sigs: %x\n", ret_sigs);

            if (CloseScreen(screen)) {
              // wenn ich den Screen immer noch nicht schliessen kann, so geb ich es auf :-(
              // eigentlich sollte da man wohl eine Schleife machen bis CloseScreen()
              // endlich mal funktioniert... 
              screen = NULL;
              printf("ist zu\n");
            }
            // bei CTRL-C wird auch aufgegeben auf den Screen zu warten...
            // also VORSICHT
          }
        }
        else {
          CloseScreen(screen);
          screen = NULL;
        }
      }
      unset_myscr;	/* naja, jetzt ist es ganz sicher nicht mehr mein Screen */
    }
  }
}



static void CloseWin2(void)
{
  struct Process *proc;
  struct IntuiMessage *msg;

  if (win2 != NULL) {

    StopHelp();

    FreeChipPuffRP();

    /* AppWin loeschen */
    if (MainAppWinMsgPort) {
      struct Message * msg;
      if (MainAppWin) {
        RemoveAppWindow(MainAppWin);
        MainAppWin = NULL;
      }
      while(msg = GetMsg(MainAppWinMsgPort)) ReplyMsg(msg);
      DeleteMsgPort(MainAppWinMsgPort);
      MainAppWinMsgPort = NULL;
    }
    
    unblock_win2();	// falls das Window blockiert ist (siehe am_requ.c)

    win2->Flags |= WFLG_RMBTRAP;		/* Menue verbieten */
    ClearMenuStrip(win2);
    MyModifyIDCMP(0, IDCMP_MENUVERIFY);
    
    Remove_scroll_gadgets(TRUE);	// kein refresh

    proc = (struct Process *)FindTask(0);
    proc->pr_WindowPtr = (APTR)old_Win_Ptr_Requester;

    Forbid();					/* nun alle Messages wegbringen */
    do {
      msg = (struct IntuiMessage *)GetMsg(win2->UserPort);
      if (msg != NULL) {
        if (msg->Class == IDCMP_MENUVERIFY && msg->Code == MENUHOT) {
          msg->Code = MENUCANCEL;
        }
        ReplyMsg(&(msg->ExecMessage));
      }
    } while (msg != NULL);
    
    /* erst nachher alles wegnehmen... sonst gibt's keinen UserPort mehr :) */
    MyModifyIDCMP(0L, ~1L);
    Permit();

#if 0
    WakeUpWin(win2);
#endif
    CloseWindow(win2);
    win2 = NULL;
  }
}

static void CloseRastPort(void)
{
  if (BitMapWin.Planes[0] != NULL) {
    if (is_bmfast) {
      FreeMem(BitMapWin.Planes[0], (wx+7)/8*wy);
    }
    else {
      FreeRaster(BitMapWin.Planes[0],wx,wy);
    }
    BitMapWin.Planes[0] = NULL;
  }
}

/* testet, ob der Screen geschlossen werden kann, wird auch von OpenCloseScreen verwendet */
int can_i_exit(void)
{
  int anz;
  struct Process *spectask;

  if (!is_myscr || is_otherpubscr) {
    return TRUE;	/* ich muss keinen Screen schliessen */
  }

  anz = test_anz_windows();

  if (is_about) anz--;		/* ziehe meine Windows ab */
  if (MessWin != NULL) anz--;
  if (is_prefwin != NULL) anz--;
  if (SearchWin != NULL) anz--;

  if (anz > 1) {
    // SpecialHost nur vertreiben, wenn wirklich kein anderes Window mehr da ist
    if (anz == 2) {	// win2 + SpecialHost
      if ((spectask = (struct Process *)FindTask("SpecialHost")) != NULL) {
        Signal(&(spectask->pr_Task), SIGBREAKF_CTRL_E);	/* SpecialHost to WorkBench */
        Delay(2);		/* SpecialHost braucht etwas Zeit.. */
      }
    }
    anz = test_anz_windows();
    if (is_about) anz--;		/* ziehe meine Windows ab */
    if (MessWin != NULL) anz--;
    if (is_prefwin != NULL) anz--;
    if (anz > 1) {
      Message(MSG_CLOSE_WINDOWS);
      beep();
      return FALSE;		/* no exit now! */
    }
    else {
      return TRUE;
    }
  }
  else {
    return TRUE;		/* you can exit the prog */
  }
}

static int test_anz_windows(void)
{
  if (screen == NULL || !is_ownscr) {
    return 0;
  }
  else {
    int nr;
    struct Window *w;
    ULONG lck;
    long en_ab = Enable_Abort;

    Enable_Abort = 0;	/* no break */
    lck = LockIBase(0L);
    for (nr=0, w=screen->FirstWindow; w != NULL; nr++, w = w->NextWindow);
    UnlockIBase(lck);
    Enable_Abort = en_ab;
    return nr;
  }
}

static void OpenRastPort(void)
{
  D(bug("OpenRastPort wx: %ld, wy: %ld\n", wx, wy));
  
  unset_bmcpu;
  unset_scrollras;

  InitBitMap(&BitMapWin,7L,wx,wy);
  if (is_village || is_alwbmfast) {
    // wir sind auf dem VGA Screen -- es wird kein CHIP-Mem benoetigt
    // oder aber wir wollen unbedingt fast-ram
    BitMapWin.Planes[0] = AllocMem((wx+7)/8*wy, MEMF_PUBLIC|MEMF_ANY|MEMF_CLEAR);
    set_bmfast;				// use fast mem for the page bitmap
  }
  else {
    BitMapWin.Planes[0] = AllocRaster(wx,wy);
    unset_bmfast;	// use chip mem for the pae bitmap
    if (!BitMapWin.Planes[0]) {
      BitMapWin.Planes[0] = AllocMem((wx+7)/8*wy, MEMF_PUBLIC|MEMF_ANY|MEMF_CLEAR);
      set_bmfast;
    }
  }

  if (!BitMapWin.Planes[0]) Fatal(10, MSG_CANT_ALLOC_BITMAP);
  
  { // Damit ich das auch in andere Planes reinkopieren kann
    // welche Plane verwendet wird, wird ueber SetWriteMask() entschieden!
    int i;
    for (i=1; i<7; i++) {
      BitMapWin.Planes[i] = BitMapWin.Planes[0];
    }
    BitMapWin.Planes[7] = NULL;	// nur 7 Planes identisch, wg. PICASSO Treiber (chunky)
  }

  if (is_bmfast) {
    set_scrollras;
    if (!is_village) {
      set_bmcpu;
      if (win2 && !show_state.ChipPuffBM) {
        AllocChipPuffRP();
      }
    }
  }
  
  InitRastPort(&myRastPort);
  myRastPort.BitMap = &BitMapWin;

  if (!is_bmfast) SetRast(&myRastPort,0);

  D(bug("ShowDVI: Allociere Bitmap im %s-RAM.\n", is_bmfast ? "Fast" : "Chip"));
}


void init_screen_colors(void)
{
  struct ColorMap *cmap;
  short *coltab;
  struct Screen bufscr;


  if (is_clwbcol && GetScreenData(&bufscr, sizeof(bufscr), WBENCHSCREEN, NULL)) {

    cmap = bufscr.ViewPort.ColorMap;
    coltab = (short *)cmap->ColorTable;
    
    current_col.red_0   = (coltab[0] & 0x0F00) >> 8;
    current_col.green_0 = (coltab[0] & 0x00F0) >> 4;
    current_col.blue_0  =  coltab[0] & 0x000F;
    current_col.red_1   = (coltab[1] & 0x0F00) >> 8;
    current_col.green_1 = (coltab[1] & 0x00F0) >> 4;
    current_col.blue_1  =  coltab[1] & 0x000F;
    current_col.red_2   = (coltab[2] & 0x0F00) >> 8;
    current_col.green_2 = (coltab[2] & 0x00F0) >> 4;
    current_col.blue_2  =  coltab[2] & 0x000F;
    current_col.red_3   = (coltab[3] & 0x0F00) >> 8;
    current_col.green_3 = (coltab[3] & 0x00F0) >> 4;
    current_col.blue_3  =  coltab[3] & 0x000F;
  }
  else {
    /* Setzen der aktuellen (default) Farbwerte */
    current_col = default_col;
  }
}

void set_screen_colors(void)
{
  if (screen != NULL) {
    SetRGB4(&(screen->ViewPort),0,(long)current_col.red_0,(long)current_col.green_0,
					(long)current_col.blue_0);
    SetRGB4(&(screen->ViewPort),1,(long)current_col.red_1,(long)current_col.green_1,
					(long)current_col.blue_1);
    if (is_col4) {
      SetRGB4(&(screen->ViewPort),2,(long)current_col.red_2,(long)current_col.green_2,
						(long)current_col.blue_2);
      SetRGB4(&(screen->ViewPort),3,(long)current_col.red_3,(long)current_col.green_3,
						(long)current_col.blue_3);
    }
  }
}

#if 0	/* nur fuer eigenen color-requester (und der kommt nicht mehr) */
void set_temp_screen_colors(void)
{
  SetRGB4(&(screen->ViewPort),0,(long)current_col.red_t0,
			(long)current_col.green_t0, (long)current_col.blue_t0);
  SetRGB4(&(screen->ViewPort),1,(long)current_col.red_t1,
			(long)current_col.green_t1, (long)current_col.blue_t1);
}
#endif


static void close_fonts(void)
{
  if (font8 != NULL) {
    CloseFont(font8);
  }
  if (font11 != NULL) {
    CloseFont(font11);
  }
}

static void open_fonts(void)
{
  txtAttr8.ta_Name  = FONTNAME8;
  txtAttr8.ta_YSize = FONTYSIZE8;
  txtAttr8.ta_Style = FONTSTYLE8;
  txtAttr8.ta_Flags = FONTFLAGS8;
  if (DiskfontBase != NULL && (font8 = OpenFont(&txtAttr8)) == NULL) {
    font8 = OpenDiskFont(&txtAttr8);
  }

  txtAttr11.ta_Name  = FONTNAME11;
  txtAttr11.ta_YSize = FONTYSIZE11;
  txtAttr11.ta_Style = FONTSTYLE11;
  txtAttr11.ta_Flags = FONTFLAGS11;
  if (DiskfontBase != NULL) {
    font11 = OpenDiskFont(&txtAttr11);
    if (font11->tf_XSize != FONTXSIZE11 || font11->tf_YSize != FONTYSIZE11) {
      CloseFont(font11);
      font11 = NULL;
    }
  }
  else {
    font11 = NULL;
  }
  if (font11 != NULL) {		/* wird fuer das OS Menue verwendet */
    txtAttr = txtAttr11;	/* struct Assignment! */
  }
  else {
    txtAttr = txtAttr8;		/* struct Assignment! */
  }
}




static ULONG	scr_oserror = 0L;

/* I accept the default pens, but have to
 * pass something to get the new look.
 */
static UWORD	mypens[] = {
	0, 1, (UWORD)~0				/* just detail and block	*/
};

static UWORD mypens4col[] =
{
	0, /* DETAILPEN */
	1, /* BLOCKPEN	*/
	1, /* TEXTPEN	*/
	2, /* SHINEPEN	*/
	1, /* SHADOWPEN	*/
	3, /* FILLPEN	*/
	1, /* FILLTEXTPEN	*/
	0, /* BACKGROUNDPEN	*/
	2, /* HIGHLIGHTTEXTPEN	*/

	1, /* BARDETAILPEN	*/
	2, /* BARBLOCKPEN	*/
	1, /* BARTRIMPEN	*/

	(UWORD)~0,
};

static UWORD mypens2col[] =
{
	0, /* DETAILPEN */
	1, /* BLOCKPEN	*/
	1, /* TEXTPEN	*/
	0, /* SHINEPEN	*/
	1, /* SHADOWPEN	*/
	1, /* FILLPEN	*/
	0, /* FILLTEXTPEN	*/
	0, /* BACKGROUNDPEN	*/
	0, /* HIGHLIGHTTEXTPEN	*/

	1, /* BARDETAILPEN	*/
	0, /* BARBLOCKPEN	*/
	1, /* BARTRIMPEN	*/

	(UWORD)~0,
};

static struct TagItem pub_screen_tags[] = {
                          { SA_DisplayID,   DEFAULT_MONITOR_ID		},
                          { SA_PubName,	    (ULONG)MyPubScreenName	},
                          { SA_PubSig,	    -1				},
                          { SA_Type,	    PUBLICSCREEN		},
                          { SA_ErrorCode,   (ULONG)&scr_oserror		},
                          { SA_Pens,	    (ULONG)mypens		},
                          { SA_Overscan,    OSCAN_TEXT			},
                          { SA_SysFont,     1L				},
                          { SA_FullPalette, TRUE			},
//                          { SA_Interleaved, TRUE			},
                          { TAG_DONE,       0L				}
		};

static struct TagItem nor_screen_tags[] = {
                          { SA_DisplayID,   DEFAULT_MONITOR_ID	},
                          { SA_ErrorCode,   (ULONG)&scr_oserror	},
                          { SA_Pens,	    (ULONG)mypens	},
                          { SA_Overscan,    OSCAN_TEXT		},
                          { SA_SysFont,     1L			},
                          { SA_FullPalette, TRUE		},
//                          { SA_Interleaved, TRUE		},
                          { TAG_DONE,       0L			}
		};

static struct TagItem viTags[] = { {TAG_DONE, 0L} };


static void OpenS(void)
{
  static char  * DefTitle = NULL;

  struct NewScreen new_scr;
  
  if (DefTitle == NULL) DefTitle = strdup(GetCopy());

  if (is_ownscr) {
  
    if (screen = LockPubScreen(MyPubScreenName)) {
      set_otherpubscr;		// PubScr ist nicht von mir geoeffnet!
      unset_myscr;		// ist nicht mein Screen!
      MessageStr(NULL);
    }
    else {
      unset_otherpubscr;	// PubScr ist von mir! (bzw. wird von mir sein)
  

#if 0 // Wozu denn das????
      /* Bastle mir ein Pen-Array: Kopiere das Array der Workbench */
      { struct Screen * WBScreen;
        WBScreen = LockPubScreen("Workbench");
        if (WBScreen) {
          struct DrawInfo * WBDrawInfo = GetScreenDrawInfo(WBScreen);
          if (WBDrawInfo) {
            int size = WBDrawInfo->dri_NumPens * sizeof(UWORD);
            if (MyOwnScreenPens) free(MyOwnScreenPens);
            MyOwnScreenPens = xmalloc(size);
            CopyMem(WBDrawInfo->dri_Pens, MyOwnScreenPens, size);
            FreeScreenDrawInfo(WBScreen, WBDrawInfo);
          }
          UnlockPubScreen(NULL, WBScreen);
        }
      }
#endif


      new_scr.LeftEdge  = 0;
      new_scr.TopEdge   = 0;
      new_scr.Width     = x_scr_width;
      new_scr.Height    = x_scr_height;
      if (is_col4) {
        new_scr.Depth     = 2;
        new_scr.DetailPen = 0;
        new_scr.BlockPen  = 1;
      }
      else {
        new_scr.Depth     = 1;
        new_scr.DetailPen = 0;
        new_scr.BlockPen  = 1;
      }
      if (is_lace) {
        new_scr.ViewModes = HIRES | LACE;
      }
      else {
        new_scr.ViewModes = HIRES ;
      }
    
      if (is_pubscr) {
        new_scr.Type = PUBLICSCREEN;
      }
      else {
        new_scr.Type = CUSTOMSCREEN;
      }
      
      if (is_os2 && show_state.screen_size_x != 0 && show_state.screen_size_y != 0) {
        new_scr.Type |= AUTOSCROLL;		/* machen wir halt immer dann autoscroll... */
      }
      
      new_scr.Font = NULL;		/* &txtAttr8; */
      new_scr.DefaultTitle = DefTitle;  /* Copyright-String */
      new_scr.Gadgets = NULL;
      new_scr.CustomBitMap = NULL;
      
      if (is_os2) {
        struct TagItem *tags;
  
        if (is_pubscr) {
          tags = &(pub_screen_tags[0]);
          // Signal zwecks sicherem schliessen zuweisen
          tags[0].ti_Data = sig_screenclose;
          if (is_col4) tags[5].ti_Data = (ULONG)mypens4col;
          else tags[5].ti_Data = (ULONG)mypens2col;
        }
        else {
          tags = &(nor_screen_tags[0]);
          if (is_col4) tags[2].ti_Data = (ULONG)mypens4col;
          else tags[2].ti_Data = (ULONG)mypens2col;
        }
        tags[0].ti_Data = get_DisplayID();
        screen = OpenScreenTagList(&new_scr, tags);
      }
      else {
        screen = OpenScreen(&new_scr);
      }
    
      if (screen == NULL) {
        if (is_os2) {
          long strnum;
          switch (scr_oserror) {
  	        case 0:			strnum = MSG_SCRERR_NOERR;
	    				break;
  	        case OSERR_NOMONITOR:	strnum = MSG_SCRERR_NOMONITOR;
  					break;
  		case OSERR_NOCHIPS:	strnum = MSG_SCRERR_NOCHIPS;
  					break;
  		case OSERR_NOMEM:	strnum = MSG_SCRERR_NOMEM;
  					break;
  		case OSERR_NOCHIPMEM:	strnum = MSG_SCRERR_NOCHIPMEM;
  					break;
  		case OSERR_PUBNOTUNIQUE:strnum = MSG_SCRERR_PUBNOTUNIQUE;
  					break;
  		case OSERR_UNKNOWNMODE:	strnum = MSG_SCRERR_UNKNOWNMODE;
  					break;
  		default:		strnum = MSG_SCRERR_DEFAULT;
    					break;
          }
          Fatal(5,MSG_CANT_OPEN_SCR_NAME, GetTeXString(strnum));
        }
        else {
          FatalStr(5,"can't open screen!");
        }
      }
      set_myscr;			/* screen variable is *my* screen */
    }

    //printf("BPR: %d, width: %d\n", screen->RastPort.BitMap->BytesPerRow, screen->Width);

  }	/* own screen */
  else {
    unset_myscr;		/* screen is not from me opened */
    if (is_os2) {
      screen = LockPubScreen(PubScreenName);
      if (screen == NULL) {
        Warning(MSG_CANT_FIND_SCR_USE_WB, PubScreenName);
        strcpy(PubScreenName, "Workbench");
        screen = LockPubScreen(PubScreenName);
        if (screen == NULL) {
          Fatal(15, MSG_CANT_LOCK_PBSCR);
        }
      }
      /* eigentlich sollte UnlockPubScreen() erst nach OpenWin2() aufgerufen werden! */
      /* aber derzeit bin ich noch zu feige dazu (Deadlock...)			     */
      UnlockPubScreen(NULL, screen);
    }
    else {
      FatalStr(20, "Kein eigener Screen funktioniert derzeit noch nicht unter 1.3!");
    }
  }
  
  if (screen->RastPort.BitMap->Depth > 1) {
    set_col4;
  }
  else {
    unset_col4;
  }

  HeightScreenTitle = (long)screen->BarHeight+1;	/* mal sehen... */
  
  if (is_os2) {
    if (is_pubscr && is_myscr) {
      PubScreenStatus( screen, 0L );		/* mache ihn PUBLIC! */
    }
    SDVI_VI = GetVisualInfoA(screen, viTags);
    if (!SDVI_VI) Fatal(10, MSG_CANT_GET_VI);

    SDVI_DRI = GetScreenDrawInfo(screen);
    if (!SDVI_DRI) Fatal(10, MSG_CANT_GET_DI);
  }
  
#if 0
  if (HeightScreenTitle > 11 && font11 != NULL) {
    /* dann koennen wir topaz 11 nehmen */
    GadtxtAttr = txtAttr11;	/* Struktur-Assignment! */
  }
  else {
    /* bleiben wir bei topaz 8 */
    GadtxtAttr = txtAttr8;	/* Struktur-Assignment! */
  }
#else
  /* Ich nehm doch immer topaz 8 */
  GadtxtAttr = txtAttr8;	/* Struktur-Assignment! */
#endif

  /* wichtig, dies wird von set_resolution() nachgeholt */
  if (is_ownscr) {
    if (show_state.window_size_own_scr_y == -1) {
      x_win_height = x_scr_height - ((show_state.window_pos_own_scr_y != -1) ? show_state.window_pos_own_scr_y : HeightScreenTitle);
    }
  }
  else {
    if (show_state.window_size_y == -1) {
      x_win_height = x_scr_height - ((show_state.window_pos_y != -1) ? show_state.window_pos_y : HeightScreenTitle);
    }
  }

  if (is_ownscr && !is_otherpubscr) {
    SetRast(&(screen->RastPort),0);  /* screen loeschen */
    set_screen_colors();
    ShowTitle(screen, TRUE);
  }


  /*
   * Normalerweise wird immer nur mit ClipBlit() gescrollt.
   * Das ist etwas schneller und sieht auch besser aus.
   * Nur bei Village bzw. bei der Picasso wird ScrollRaster()
   * gemacht, da dies durch den Picasso Blitter viel schneller ist!
   */

  unset_scrollras;
  unset_village;

  { /* ***  VILLAGE  *** */
    long dummy;
    char * modename = GetModeIDName(INVALID_ID, &dummy);
    /* der gelieferte String ist gueltig bis zum naechsten GetModeIDName() Aufruf! */
    
    if (!strnicmp("DOMINO", modename, 6)) {
      set_village;
      //unset_scrollras;	// Domino hat keinen Blitter, also alte Methode
      D(bug("ShowDVI: DOMINO Screen! Lege die Bitmap in's Fast-RAM. (Mode: %s)\n", modename));
    }
    else if (!strnicmp("PICASSO", modename, 7)) {
      set_village;
      set_scrollras;
      D(bug("ShowDVI: PICASSO Screen! Lege die Bitmap in's Fast-RAM. (Mode: %s) (vill: %ld)\n", modename, is_village));
    }
    else if (!strnicmp("CyBER", modename, 4)) {
      set_village;
      is_cyber = 1;
      set_scrollras;
      D(bug("ShowDVI: CyBERvision Screen! Lege die Bitmap in's Fast-RAM. (Mode: %s) (vill: %ld)\n", modename, is_village));
    }
  }

  
  MessWinXpos = -1; /* Merker dafuer, dass alte MessWin-Positionen nun ungueltig sind */

  /* muss gemacht werden, nachdem dri und vi fest stehen */
  InitBoopsi();			/* allocate all in 'struct BoopsiObj'	*/
}





struct HookMsg {
  struct Layer      * layer;
  struct Rectangle    bounds;
  LONG                offsetx;
  LONG                offsety;
};

static ULONG __asm hookEntry( register __a0 struct Hook * h, 
			      register __a2 void * o, 
			      register __a1 void * msg )
{
  putreg(REG_A4, (long)h->h_Data);  
  return ((*(ULONG (* __stdargs)(struct RastPort *, struct HookMsg *))h->h_SubEntry)(o, msg));
}

static struct Hook BackHook;
static struct RastPort BackHookRP;


static void __stdargs BackHookFunc(struct RastPort * rp, struct HookMsg * msg)
{
  WORD MinX, MinY, MaxX, MaxY;

  set_i_window_size();		// ein bischen aufwenig :-((

  MinX = (msg->bounds.MinX < Page_LeftPoint)   ? Page_LeftPoint   : msg->bounds.MinX;
  MinY = (msg->bounds.MinY < Page_TopPoint)    ? Page_TopPoint    : msg->bounds.MinY;
  MaxX = (msg->bounds.MaxX > Page_RightPoint)  ? Page_RightPoint  : msg->bounds.MaxX;
  MaxY = (msg->bounds.MaxY > Page_BottomPoint) ? Page_BottomPoint : msg->bounds.MaxY;

  if (MaxX > MinX && MaxY > MinY) {
    //D(bug("BHF() MinX: %ld, MaxX: %ld, MinY: %ld, MaxY: %ld\n", MinX, MaxX, MinY, MaxY));
    RectFill(&BackHookRP, MinX, MinY, MaxX, MaxY);
  }
}




static int sleep_counter = 0;


static struct TagItem win2_tags[] = 
  {
			  { WA_MenuHelp,     TRUE	},
			  { WA_MouseQueue,   5L		},	/* 5 messages... */
			  { WA_RptQueue,     1L		},
			  { WA_AutoAdjust,   TRUE	},
			  { WA_NewLookMenus, TRUE	},
                          { TAG_DONE,        0L		}
  };


static void OpenWin2(void)
{
  struct Process *proc;
  struct NewWindow new_win; 
  int hgrp;
  

  if (is_ownscr) {  
    new_win.LeftEdge = ((show_state.window_pos_own_scr_x == -1)
				? WIN_X_KOO
				: show_state.window_pos_own_scr_x);
    new_win.TopEdge = ((show_state.window_pos_own_scr_y == -1)
				? WIN_Y_KOO-((is_os2) ? 0 : 1)
				: show_state.window_pos_own_scr_y);
  }
  else {
    new_win.LeftEdge = ((show_state.window_pos_x == -1)
				? WIN_X_KOO
				: show_state.window_pos_x);
    new_win.TopEdge = ((show_state.window_pos_y == -1)
				? WIN_Y_KOO-((is_os2) ? 0 : 1)
				: show_state.window_pos_y);
  }
  new_win.Width = x_win_width;
  new_win.Height = x_win_height;
  if (is_col4) {
    new_win.DetailPen = 0;
    new_win.BlockPen = 1; 
  }
  else {
    new_win.DetailPen = 0;
    new_win.BlockPen = 1; 
  }
  new_win.Title = "ShowDVI V" SHOWDVI_VERSION;
  new_win.Flags = WFLG_CLOSEGADGET | WFLG_DEPTHGADGET | WFLG_ACTIVATE | WFLG_DRAGBAR |
		WFLG_REPORTMOUSE | WFLG_RMBTRAP | WFLG_SIZEGADGET | WFLG_SIZEBBOTTOM;

  if (is_smartwin) new_win.Flags |= WFLG_SMART_REFRESH;
  else             new_win.Flags |= WFLG_SIMPLE_REFRESH;
  
  /**
  if (!is_osmenu) {
    new_win.Flags |= WFLG_RMBTRAP;
  }
  **/

  new_win.IDCMPFlags = IDCMP_FLAGS;
  if (IntuitionBase->LibNode.lib_Version > 36) {
    new_win.IDCMPFlags |= IDCMP_MENUHELP | IDCMP_CHANGEWINDOW;
  }

  if (is_pubscr && is_os2) {			/* naja, unter os_2 ist doch alles pubscr.. */
    new_win.Type = PUBLICSCREEN;
  }
  else {
    if (is_ownscr) {
      new_win.Type = CUSTOMSCREEN;
    }
    else {
      new_win.Type = WBENCHSCREEN;
    }
  }
  
  new_win.FirstGadget = (struct Gadget *)NULL; /* werden spaeter hinzugefuehrt */
  new_win.CheckMark = NULL;
  new_win.Screen = screen;
  new_win.BitMap = NULL;
  new_win.MinWidth = 370;
  new_win.MinHeight = 4*HEIGHT_UD_ARROW+HEIGHT_SCROLL+45;
  new_win.MaxWidth = 65535;
  new_win.MaxHeight = 65535;

  hgrp = GetHelpNum(); //(AbortRun Test)
  //hgrp = -1;
  
  if (hgrp != -1) new_win.IDCMPFlags |= IDCMP_GADGETHELP;
  show_state.aktIDCMP = new_win.IDCMPFlags;	/* !! */
  
  if (is_os2) {
    win2 = OpenWindowTags(&new_win, WA_MenuHelp,     TRUE,
				    WA_MouseQueue,   5L,
				    WA_RptQueue,     1L,	
				    WA_AutoAdjust,   TRUE,
				    WA_NewLookMenus, TRUE,
        (hgrp == -1) ? TAG_IGNORE : WA_HelpGroup,    hgrp,
				    TAG_DONE);
  }
  else {
    win2 = OpenWindow(&new_win);
  }

  if (win2 == NULL) {
     Fatal (5,MSG_CANT_OPEN_WIN);
  }
  
  sleep_counter = 0;

  MySetABPenDrMd(win2->RPort, show_state.APen, show_state.BPen, JAM2);

  if (is_bhook) {
    // ohne BHook geht es zwar auch, dann flackert das Bild aber beim Scrollen ganz häßlich!

    // erstmal alles fuellen
    fill_block(win2->BorderLeft, win2->BorderTop, 
	       win2->Width - win2->BorderRight - win2->BorderLeft + 1,  
	       win2->Height - win2->BorderBottom - win2->BorderTop + 1, 2);

    BackHookRP       = *win2->RPort;
    BackHookRP.Layer = NULL;
    SetAPen(&BackHookRP, show_state.BPen);
    SetDrMd(&BackHookRP, JAM1);

    // Installiere einen Backfill Hook
    BackHook.h_Entry    = (unsigned long (* )()) hookEntry;
    BackHook.h_SubEntry = (ULONG (* )()) BackHookFunc;
    BackHook.h_Data     = (void *)getreg(REG_A4);
    (void)InstallLayerHook(win2->WLayer, &BackHook);
  }
  
  if (hgrp != -1) StartHelp(screen, win2);

  /* So, nun nochmal screen setzen...zur Sicherheit */
  screen = win2->WScreen;		/* wg. FallBack */
  x_scr_width = screen->Width;
  x_scr_height = screen->Height;
  x_win_width = win2->Width;		/* wg. AutoAdjust */
  x_win_height = win2->Height;
  ScreenToFront(screen);		/* falls irgend ein PubScr */
  
  /* requester-window */
  proc = (struct Process *)FindTask(0);
  old_Win_Ptr_Requester = (struct Window *)proc->pr_WindowPtr;
  proc->pr_WindowPtr = (APTR)win2;

  sig_win = win2->UserPort->mp_SigBit;
#ifdef DEBUG
  if (DeBug) printf("Signal NR Window: %d\n",(int)sig_win);
#endif

  width_left_border  = win2->BorderLeft;
  width_right_border = win2->BorderRight;
  HeightWinTitle = win2->BorderTop;

  /* wird benoetigt um festzustellen, ob das Win. verschoben wurde */
  old_win_pos_x = win2->LeftEdge;
  old_win_pos_y = win2->TopEdge;
 
  // nach OpenWindow bis ier her gehen 56 Bytes verloren!
  // es geht nix verloren, wenn man hgrp auf -1 setzt! (siehe StartHelp()) (AbortRun)

  init_os_menu();
  SetDosMenu();
  
  win2->Flags &= ~WFLG_RMBTRAP;		/* jetzt Menue freigeben */
  
  
  /** nun noch ein App-Window daraus machen, falls der Screen ein WB-Screen ist */
  if (screen->Flags | WBENCHSCREEN) {
    if ((MainAppWinMsgPort = CreateMsgPort())) {
      MainAppWin = AddAppWindowA(0, 0, win2, MainAppWinMsgPort, NULL);
    }
  }
  else {
    MainAppWin        = NULL;
    MainAppWinMsgPort = NULL;
  }

  // 0 Bytes (AbortRun)

  /* falls is_bmcpu muss nun noch die Puffer-Bitmap alloziert werden */
  if (is_bmcpu) {
    AllocChipPuffRP();
  }

  { struct Gadget * g = win2->FirstGadget;
    while (g && g->GadgetType != GTYP_WDEPTH) g = g->NextGadget;
    if (g) WidthSysDepthGadget = g->Width;
    else   WidthSysDepthGadget = 20;
  }
}



static struct Requester block_req;
static int win2_blocked = FALSE;

void block_win2(void)
{
  if (!win2_blocked) {
    (void)memset(&block_req, 0, sizeof(block_req));
    win2_blocked = Request(&block_req, win2);
  }
}

void unblock_win2(void)
{
  if (win2_blocked) {
    EndRequest(&block_req, win2);
    win2_blocked = FALSE;
  }
}


/****
#define SPRITE_DATA_SIZE	24
#define SPRITE_HEIGHT		11
#define SPRITE_WIDTH		12
***/
#define SPRITE_DATA_SIZE	48
#define SPRITE_HEIGHT		16
#define SPRITE_WIDTH		15

static const USHORT __chip SleepPointerData[] = {
    0x0000, 0x0000,	/* vert. and horiz. start posn. */
	0x0400,	0x07C0,
	0x0000,	0x07C0,
	0x0100,	0x0380,
	0x0000,	0x07E0,
	0x07C0,	0x1FF8,
	0x1FF0,	0x3FEC,
	0x3FF8,	0x7FDE,
	0x3FF8,	0x7FBE,
	0x7FFC,	0xFF7F,
	0x7EFC,	0xFFFF,
	0x7FFC,	0xFFFF,
	0x3FF8,	0x7FFE,
	0x3FF8,	0x7FFE,
	0x1FF0,	0x3FFC,
	0x07C0,	0x1FF8,
	0x0000,	0x07E0,
    0x0000, 0x0000,	/* reserved, must be NULL */
};


void sleep_pointer(void)
{
  if (win2) {
    sleep_counter++;

    if (sleep_counter == 1) {
      if (is_os3) {
        SetWindowPointer(win2, WA_BusyPointer, TRUE, WA_PointerDelay, TRUE, TAG_DONE);
      }
      else {
        SetPointer(win2,(UWORD *)SleepPointerData,SPRITE_HEIGHT,SPRITE_WIDTH,0,0);
      }
      block_win2();
    }
  }
}

void clear_pointer(void)
{
  if (win2) {
    sleep_counter--;
    
    if (sleep_counter == 0) {
      unblock_win2();
      ClearPointer(win2);
    }
  }
}

void SetupMargin(void)
{
  const long widthin  = (long)(((float)pwidth_pt  / 72.27 + ((float)SAVETY_BITS_X / (float)hconvresolution) * 1000.0 / (float)mag) * 1000.0 + 0.5);
  const long heightin = (long)(((float)pheight_pt / 72.27 + ((float)SAVETY_BITS_Y / (float)vconvresolution) * 1000.0 / (float)mag) * 1000.0 + 0.5);

  show_state.margin_x = static_x_Koo * widthin / wx; 
  show_state.margin_y = static_y_Koo * heightin / wy;
  set_margin;
  
  Message(MSG_SET_MARGIN);


#if 0
  static short OffX = -6;
  static short OffY = -6;
  
  if (is_tmpmargin) {
    Message(MSG_CANCEL_MARGIN_SETTING);
    clear_pointer();
    unset_tmpmargin;
    return;
  }

  set_tmpmargin;		// erst noch auf Klick warten
  
  Message(MSG_SET_MARGIN_SETTING);

#if 0
  if (margin_sprite_data == NULL) {
    #include <libraries/iffparse.h>
    struct Library * IFFParseBase = OpenLibrary("iffparse.library", 37);

    if (IFFParseBase) {
      const char * defname = "TeX:config/MarginPointer.iff";
      struct IFFHandle * iff;
      BPTR iffstream;
      
      iff = AllocIFF();
      iffstream = Open(defname, MODE_OLDFILE);
      if (iffstream && iff) {
        iff->iff_Stream = iffstream;
        iff->iff_Flags  = IFFF_READ;
        InitIFFasDOS(iff);
        if (OpenIFF(&iff, IFFF_READ) == 0) {
          struct StoredProperty * sp;
          
          sp = FindProp(iff, MAKE_ID('I','L','B','M'));

          CloseIFF(iff);
        }
      }
      if (iff) FreeIFF(iff);
      CloseLibrary(IFFParseBase);
    }
  }
#endif

  if (margin_sprite_data != NULL) {
    SetPointer(win2, margin_sprite_data, MARGIN_SPRITE_HEIGHT, MARGIN_SPRITE_WIDTH, OffX, OffY);
  }
#endif
}


void MyModifyIDCMP(ULONG add, ULONG sub)
{
  register ULONG act = show_state.aktIDCMP;
  BOOL ret;
  
  if (win2 == NULL) return;

  act = (act | add) & (~sub);
  if (act != show_state.aktIDCMP) {	/* hat sich wirklich was geaendert? */
    show_state.aktIDCMP = act;
  
    ret = ModifyIDCMP(win2, act);
    if (IntuitionBase->LibNode.lib_Version >= 37 && ret == NULL) {
      FatalStr(20, "Can't ModifyIDCMP(%x)!", act);
    }
  }
}


/*************   T I M E R *******************************************/


/* Aufruf aus globals.c */
void AddTimeRequest(void)
{
  if (screen != NULL && win2 != NULL) {
    /* set_timer; */
    if (timer_wait_ticks == 0) {
      MyModifyIDCMP(IDCMP_INTUITICKS, 0L);
      add_inticks;
    }
    timer_wait_ticks = WAIT_TIME_TICKS;
  }
}


/***********    P R I N T   --   H A R D C O P Y    ******************/

static void start_print_page(void)
{
  unsigned short ph_inch, pw_inch;

#ifdef DEBUG
  if (DeBug) printf("Print: sizex: %d, sizey: %d, ptx: %d, pty: %d\n",
				wx,wy,pwidth_pt,pheight_pt);
#endif

  ToDo = DO_PDEVICE;

  sig_print = (UBYTE)InitPrinterDevice();

  if (sig_print != 255) {

#  ifdef DEBUG
    if (DeBug) printf("Signal NR fuer Print: %d\n",(int)sig_print);
#  endif

    set_print;
    write_status();		/* Index fuer Print */

    ph_inch = (unsigned short)((float)pheight_pt * 1000.0 / 72.27 + 0.5);
    pw_inch = (unsigned short)((float)pwidth_pt  * 1000.0 / 72.27 + 0.5);

    PrintRastPort(&myRastPort,
			(ULONG)(screen->ViewPort.Modes & 0x0000FFFF),
			(USHORT)wx, (USHORT)wy,
			pw_inch, ph_inch);

  }
}

static void check_print_page(void)
{
  EndPrintRastPort();
  ClosePrinterDevice();
  unset_print;

  write_status();
}


/* Struktur fuer den Print-Requester */
static struct EasyStruct PrintES = {
    sizeof (struct EasyStruct),
    0,
    NULL,
    NULL,
    NULL
};


void abort_print_page(void)
{
  if (!CheckIO((struct IORequest *)request)) {
    AbortIO((struct IORequest *)request);
    PROFILE_OFF();
    (void)Wait(SIG_PRINT);
    PROFILE_ON();
  }
  while (GetMsg(printerPort) != NULL) ;	/* neu 13-feb-91 */

  prnzero();				/* Drucker initialisieren */
  ClosePrinterDevice();
  unset_print;

  write_status();
}

void printing(void)
{
  if (is_print) {
    if (is_os2) {
      PrintES.es_Title        = GetTeXString(MSG_SHOWDVI_MESSAGE);
      PrintES.es_TextFormat   = GetTeXString(MSG_ABORT_PRINT_PAGE);
      PrintES.es_GadgetFormat = GetTeXString(MSG_OK_CANCEL_REQSTRING);
      if (!EasyRequestArgs(win2, &PrintES, NULL, NULL)) {
        return;
      }
    }
    Message(MSG_ABORT_PRINT);
    beep();
    abort_print_page();
    Message(MSG_PRINT_IS_ABORTED);
    set_checked_os_menu();
  }
  else {
    Message(MSG_PRINT_CUR_PAGE);
    set_checked_os_menu();
    start_print_page();
  }
  write_status();
}


/****************** Window Operationen *****************************/

void fill_block(short x,short y, short dx, short dy, short color)
					/* i.e. clear the blatt_ok_gadget */
					/* color 1: fill, color 0: clear  */
{
  struct RastPort *rp;

  if (win2 != NULL) {
    rp = win2->RPort;
    SetAPen(rp,(long)color);
    SetDrMd(rp,JAM1); 
    RectFill(rp,(long)x,(long)y,(long)(x+dx),(long)(y+dy));   /* loeschen */
    SetAPen(rp, 1);
  }
}


/* clear unused parts of the screen (if bitmap smaller than window) */
/* and refresh all gadgets in the window... */
void refresh_screen(void)
{
  RefreshWindowFrame(win2);

#if 0
  if (!is_os2) {
    /* Ziehe elfte Pixelreihe */
    struct RastPort *rp = win2->RPort;
    SetAPen(rp, 1);
    Move (rp, 0, HeightWinTitle-1);
    Draw (rp, x_win_width-1, HeightWinTitle-1);
    /* draw left border (verbreitern von 2 auf 4 Pixel) */
    fill_block((short)2,(short)HeightWinTitle-1,
    		(short)(width_left_border-2-1) /* == 2  (??) */,
    		(short)x_win_height-2-1,(short)1);
  }
#endif


#if 0 // ?????????????

  if (is_gadg) {
    if (x_win_i_width < x_win_width - WIDTH_SCROLL_BAR - width_left_border 
			- width_right_border + ((is_os2) ? 6 : 1)) {
      /* Inneres kleiner als Window, ScrollBar weiter links */
      fill_block((short)(x_win_i_width+WIDTH_SCROLL_BAR+width_left_border+width_right_border-((is_os2)?6+4:1)),
	 	 (short)HeightWinTitle,
		 (short)(x_win_width - WIDTH_SCROLL_BAR-x_win_i_width-width_left_border-width_right_border
				-((is_os2)?6-11:1)),
		 (short)(x_win_height-HeightWinTitle-2-1),(short)0);
 				   /* height_lower_border ^^ */
      /* ganz rechten Border verdicken (OS < 2.0) */
      if (!is_os2) {
        fill_block((short)(x_win_width-width_right_border), (short)HeightWinTitle,
		   (short)width_right_border-1, (short)(x_win_height-HeightWinTitle-1), (short)1);
      }
    }
    if (x_win_i_height < x_win_height - HeightWinTitle - HEIGHT_SCROLL_BAR) {
      fill_block((short)width_left_border, (short)(x_win_i_height+HeightWinTitle+HEIGHT_SCROLL_BAR),
		 (short)(x_win_width-width_left_border-width_right_border-1),
		 (short)(x_win_height-x_win_i_height-HeightWinTitle-HEIGHT_SCROLL_BAR-2-1),
		 (short)0);
	     /* height_lower_border ^^ (loescht border mit ?????? was soll das) */
	     /* Border nicht mitloeschen, aber neu zeichnen */
      /* draw bottom border (ganz unten) */
      fill_block((short)width_left_border, (short)(x_win_height-2),
		 (short)x_win_width-width_left_border-width_right_border,
		 (short)2, (short)1);
      /* zeichne Leiste unter dem vert-SBar. (nur fuer OS >= 2.0) */
      if (is_os2) {
        fill_block((short)width_left_border, (short)(x_win_i_height+HeightWinTitle+HEIGHT_SCROLL_BAR-2),
		   (short)x_win_i_width-2*(WIDTH_X_GADGETS), (short)1, (short)1);
      }
    }
    /* Border nachziehen, neben SBar */
    fill_block((short)(x_win_i_width+WIDTH_SCROLL_BAR-((is_os2)?4:0)+width_left_border),
		 (short)HeightWinTitle,
		 (short)(is_os2) ? width_right_border-1 : 2,	/* nicht so FETT unter 1.3 */
		 (short)(x_win_i_height+HEIGHT_SCROLL_BAR-1),(short)1);
  }
  else {
    if (x_win_i_width < x_win_width - width_left_border - width_right_border+((is_os2)?4:0)) {
      fill_block((short)(x_win_i_width+width_left_border-((is_os2)?2:0)),(short)HeightWinTitle,
		 (short)(x_win_width - width_left_border - width_right_border - x_win_i_width),
		 (short)(x_win_height-HeightWinTitle-2-1),(short)0);
 				   /* height_lower_border ^^ */
      /* zeichne rechten SBar neu (nur OS < 2.0) */
      if (!is_os2) {
        fill_block((short)(x_win_width-width_right_border), (short)HeightWinTitle,
		   (short)width_right_border-1, (short)(x_win_height-HeightWinTitle-1), (short)1);
      }
    }
    if (x_win_i_height < x_win_height - HeightWinTitle-1) {
      fill_block((short)width_left_border, (short)(x_win_i_height+HeightWinTitle),
		 (short)x_win_width-width_left_border-width_right_border-1,
      		 (short)(x_win_height-x_win_i_height-HeightWinTitle-2-1), (short)0);
				  /* height_lower_border ^^ */
	/* HEHH da wird ja der Border mit weggeloescht */
    }
    /* draw right border IST ZU LANG */
    fill_block((short)(x_win_i_width+width_left_border),(short)HeightWinTitle,
		(short)width_right_border,
		(short)(x_win_i_height+2),(short)1);
    /* draw bottom border IST ZU LANG */
    fill_block((short)width_left_border, (short)(x_win_i_height+HeightWinTitle),
		 (short)x_win_i_width-width_left_border+width_right_border,
		 (short)2, (short)1);
    /* draw bottom border (ganz unten) */
    fill_block((short)width_left_border, (short)(x_win_height-2),
		 (short)x_win_width-width_left_border-width_right_border,
		 (short)2, (short)1);
  }
#endif

}



#define MAXMESSAGESTRING	150
static char MessageString[MAXMESSAGESTRING];	/* Buffer for Messages! */

int write_screen(char *str)				/* KEIN static */
{
  WORD len;

  if (screen != NULL && win2 != NULL) {
    len = TextLength(&(screen->RastPort), str, strlen(str));
    if (len < screen->Width - 30) {	/* -30.. naja fuer die Gadgets halt... */
      strncpy(MessageString, str, MAXMESSAGESTRING);
      SetWindowTitles(win2, (char *)-1, MessageString);
    }
    return TRUE;
  }
  else {
    return FALSE;
  }
}

#if 0
void write_screen_counter(int nr)			/* KEIN static */
{
  char str[8];
  struct RastPort *rp;
  int len;
  WORD TopEdge;
  static long oldcounter = -1;

  if (win2 != NULL) {
    rp = win2->RPort;
    SetAPen(rp,1);
    TopEdge = (HeightWinTitle - rp->Font->tf_YSize) / 2;
    SetDrMd(rp,COMPLEMENT);
    if (oldcounter >= 0){
      Move(rp, WR_COUNTER_LEFT, TopEdge + rp->Font->tf_YSize);
      sprintf(str,"%4d",oldcounter);
      len = strlen(str);
      if (len>4) {
         str[4] = (char)0;
      }
      Text(rp,str,(long)strlen(str));
    }
    SetDrMd(rp,JAM1); 
#if 0
    RectFill(rp, WR_COUNTER_LEFT, TopEdge, WR_COUNTER_LEFT+32L, TopEdge+rp->Font->tf_YSize);	/* loeschen */
#endif
    if (nr >= 0) { 
      SetAPen(rp,0L);
      Move(rp, WR_COUNTER_LEFT, TopEdge + rp->Font->tf_YSize);
      sprintf(str,"%4d",nr);
      len = strlen(str);
      if (len>4) {
         str[4] = (char)0;
      }
      Text(rp,str,(long)strlen(str));
    }
    oldcounter = nr;
  }
}
#endif

void write_status(void)					/* KEIN static (wg. amkey) */
{
  static char TitleBuf[256];
  static char FNameBuf[200];
  int again = TRUE;
  int last  = FALSE;

  if (win2) {

    FNameBuf[sizeof(FNameBuf)-1] = '\0';  
    strncpy(FNameBuf, filename, sizeof(FNameBuf)-1);
  
    do {
      char * ptr;
      int count = take_counter(0);
      int sec = get_secundary(current_page_phy);
      char buf[24];

      if (is_dvif) {
        sprintf(TitleBuf, GetTeXString(MSG_WIN_HEADER_FILE), 
			  FNameBuf, current_page_phy, max_page_number);

        if (current_page != current_page_phy || sec != 0) {
          if (sec != 0) {
            sprintf(buf, "«%d.%d»  ", current_page, sec);
          }
          else {
            sprintf(buf, "«%d»  ", current_page);
          }
        }
        else {
          buf[0] = '\0';
        }
        strcat(TitleBuf, buf);
      }
      else {
        sprintf(TitleBuf, GetTeXString(MSG_WIN_HEADER_NO_FILE), SHOWDVI_VERSION, COMPILER);
      }

      if (count != -1) {
        sprintf(buf, "[%d]  ", count);
        strcat(TitleBuf, buf);
      }

      if (is_print || is_usephy || is_show) {
        ptr = TitleBuf + strlen(TitleBuf);
        *ptr++ = '(';
        if (is_print)  *ptr++ = 'P';
        if (is_usephy) *ptr++ = 'Y';
        if (is_show)   *ptr++ = 'F';
        *ptr++ = ')';
        *ptr   = '\0';
      }

      sec = (win2->Width - WidthSysDepthGadget*3 - 20) - TextLength(&screen->RastPort, TitleBuf, strlen(TitleBuf));	// Differenz
      
      if (is_dvif && sec < 0 && !last) {
        char * psl = strchr(FNameBuf, '/');

        if (psl) {
          char * pdd = strchr(FNameBuf, ':');
          
          if (pdd) {
            if (psl - pdd < 5) {
              psl = strchr(psl+1, '/');
            }
            if (psl) {
              int i;
              pdd[1] = '.';
              pdd[2] = '.';
              pdd[3] = '.';
              for (i=0; psl[i] && i < 50; i++) {
                pdd[i+4] = psl[i];
              }
              pdd[i+4] = '\0';
            }
            else {
              psl = strrchr(filename, '/');
              if (psl) {
                // dann nur noch der Filename alleine
                strncpy(FNameBuf, psl+1, sizeof(FNameBuf)-1);
              }
              last = TRUE;
            }
          }
        }
        else {
          again = FALSE;
        }
      }
      else {
        again = FALSE;
      }

    } while (again);

    SetWindowTitles(win2, TitleBuf, (UBYTE *)-1);	/* refresh ?? */
  }
#if 0
  struct RastPort *rp;
  static char str[4] = "   ";
  WORD TopEdge;
  LONG penno;

  if (win2 != NULL) {
      rp = win2->RPort;
      TopEdge = (HeightWinTitle - rp->Font->tf_YSize) / 2;
      penno = ReadPixel(rp, WR_STATUS_LEFT-1, TopEdge);
      if (penno == -1) penno = 1;
      SetDrMd(rp,JAM1);
      SetAPen(rp, penno);
      Move(rp, WR_STATUS_LEFT, TopEdge + rp->Font->tf_YSize);
      Text(rp,str,3);
#if 0
      RectFill(rp, WR_STATUS_LEFT, TopEdge, WR_STATUS_LEFT+24L,TopEdge+rp->Font->tf_YSize);   /* loeschen */
#endif
      SetAPen(rp,0);
      Move(rp, WR_STATUS_LEFT, TopEdge + rp->Font->tf_YSize);
      str[0] = (is_print)  ? 'P' : ' ';
      str[1] = (is_usephy) ? 'Y' : ' ';
      str[2] = (is_show)   ? 'F' : ' ';
      str[3] = '\0';
      Text(rp,str,3);
      write_screen_counter(take_counter(0));
   }
#endif
}


/* Wie ClipBlit(), nur dass etwas mit den Planes getrickst wird um ein schwarz auf weiss zu erreichen */
static __inline void ClipIt(const long destx, const long desty, const long x, const long y, const long dx, const long dy)
{
  MySetABPenDrMd(win2->RPort, show_state.APen, (is_bhook) ? show_state.BPen : 0, JAM2);
  BltTemplate(myRastPort.BitMap->Planes[0]+y*myRastPort.BitMap->BytesPerRow+(x/16)*2, x%16, myRastPort.BitMap->BytesPerRow, win2->RPort, destx, desty, dx, dy);

#ifdef OLD_BLIT
  if (is_bhook) {
    SafeSetWriteMask(win2->RPort, 0x02);
    ClipBlit(&myRastPort, x, y, win2->RPort, destx, desty, dx, dy, 0x30);
  }
  SafeSetWriteMask(win2->RPort, 0x01);
  ClipBlit(&myRastPort, x, y, win2->RPort, destx, desty, dx, dy, 0xC0);
  SafeSetWriteMask(win2->RPort, 0xff);
#endif
}


static void win_mov(const long dx, const long dy, const long x, const long y)
{
#if 0
  const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
  const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
#endif

  /*
   *   destx/desty geben den linken/oberen Punkt an.
   *   Dadurch wird, wenn noetig, das Bild zentriert.
   */
  const long destx = win2->BorderLeft + ((winiw > wx) ? ((winiw-wx) / 2) : 0);
  const long desty = win2->BorderTop  + ((winih > wy) ? ((winih-wy) / 2) : 0);
  
  //D(bug("bmfast: %ld, bmcpu: %ld, scrollras: %ld, vill: %ld\n",
  //	is_bmfast, is_bmcpu, is_scrollras, is_village));



  if (is_scrollras && (dx != 0 || dy != 0) && abs(dx) < winiw && abs(dy) < winih) {

    if (!is_bhook) SafeSetWriteMask(win2->RPort, 1);		// *nur* Plane 0 scrollen

    /*
     * Den umstaendlichen Weg ueber ScrollRaster() gehen wir nur wenn
     * is_scrollras gesetzt ist. Dies ist vor allem bei der PICASSO der
     * Fall.
     * Doch auch dann wird das nur gemacht, wenn es kein 'show' ist und
     * wenn nicht mit ALT grosse Spruenge gemacht werden. 
     */
  
    /*
     * Es wird immer nur eine Plane gescrollt ---time is money---
     */
    if (is_os3) {
      ScrollWindowRaster(win2, dx, dy,
		win2->BorderLeft, win2->BorderTop,
		win2->BorderLeft+winiw-1, win2->BorderTop+winih-1);
    }
    else {
      ScrollRaster(win2->RPort, dx, dy,
		win2->BorderLeft, win2->BorderTop,
		win2->BorderLeft+winiw-1, win2->BorderTop+winih-1);
      if (win2->RPort->Layer->Flags & LAYERREFRESH) {
        BeginRefresh(win2);
        /* kopiere *alles* nochmal! (urgs) */
        if (is_bmcpu && show_state.ChipPuffBM) {
          CPUBlit(x, y, destx, desty, (winiw > wx) ? wx : winiw, (winih > wy) ? wy : winih);
        }
        else {
          ClipIt(destx, desty, x, y, (winiw > wx) ? wx : winiw, (winih > wy) ? wy : winih);
/*
          ClipBlit(&myRastPort, x, y, win2->RPort, destx, desty,
 			(winiw > wx) ? wx : winiw, (winih > wy) ? wy : winih, 0xC0);
*/
        }
        EndRefresh(win2, TRUE);
      }
    }

    if (is_bmcpu && show_state.ChipPuffBM) {
      if (dx > 0) {
        CPUBlit(x+winiw-dx, y, destx+winiw-dx, desty, dx, (winih > wy) ? wy : winih);
      }
      if (dx < 0) {
        CPUBlit(x, y, destx, desty, -dx, (winih > wy) ? wy : winih);
      }
      if (dy > 0) {
        CPUBlit(x, y+winih-dy, destx, desty+winih-dy, (winiw > wx) ? wx : winiw, dy);
      }
      if (dy < 0) {
        CPUBlit(x, y, destx, desty, (winiw > wx) ? wx : winiw, -dy);
      }
    }
    else {
      if (dx > 0) {
        ClipIt(destx+winiw-dx, desty, x+winiw-dx, y, dx, (winih > wy) ? wy : winih);
        // ClipBlit(&myRastPort, x+winiw-dx, y, win2->RPort, destx+winiw-dx, desty, dx, (winih > wy) ? wy : winih, 0xC0);
      }
      if (dx < 0) {
        ClipIt(destx, desty, x, y, -dx, (winih > wy) ? wy : winih);
        // ClipBlit(&myRastPort, x, y, win2->RPort, destx, desty, -dx, (winih > wy) ? wy : winih, 0xC0);
      }
      if (dy > 0) {
        ClipIt(destx, desty+winih-dy, x, y+winih-dy, (winiw > wx) ? wx : winiw, dy);
        // ClipBlit(&myRastPort, x, y+winih-dy, win2->RPort, destx, desty+winih-dy, (winiw > wx) ? wx : winiw, dy, 0xC0);
      }
      if (dy < 0) {
        ClipIt(destx, desty, x, y, (winiw > wx) ? wx : winiw, -dy);
        // ClipBlit(&myRastPort, x, y, win2->RPort, destx, desty, (winiw > wx) ? wx : winiw, -dy, 0xC0);
      }
    }

    SafeSetWriteMask(win2->RPort, 0xFF);		// alle Planes wieder einschalten
  }
  else {

    if (is_bmcpu && show_state.ChipPuffBM) {
      CPUBlit(x, y, destx, desty, (winiw > wx) ? wx : winiw, (winih > wy) ? wy : winih);
    }
    else {
      ClipIt(destx, desty, x, y, (winiw > wx) ? wx : winiw, (winih > wy) ? wy : winih);
/*
      ClipBlit(&myRastPort, x, y, win2->RPort, destx, desty,
			(winiw > wx) ? wx : winiw, (winih > wy) ? wy : winih, 0xC0);
      SafeSetWriteMask(win2->RPort, 0x02);
      ClipBlit(&myRastPort, x, y, win2->RPort, destx, desty,
			(winiw > wx) ? wx : winiw, (winih > wy) ? wy : winih, 0x30);
*/
    }
  }
}

void window_show(void)
{
  set_i_window_size();

  if (x_win_i_width > wx) {
    // Window loeschen, da moeglicherweise erst neuerdings so viel sichtbar
    // spich gerade ist auf 44dpi gewechselt worden

    fill_block(win2->BorderLeft, win2->BorderTop, Page_LeftPoint-win2->BorderLeft-1, x_win_i_height-1, 0);
    fill_block(Page_RightPoint, win2->BorderTop, win2->Width-Page_RightPoint-win2->BorderRight-1, x_win_i_height-1, 0);

    static_x_Koo = 0;	// ganz links anzeigen
  }
  if (x_win_i_height > wy) {
    fill_block(win2->BorderLeft, win2->BorderTop, x_win_i_width-1, Page_TopPoint-win2->BorderTop-1, 0);
    fill_block(win2->BorderLeft, Page_BottomPoint, x_win_i_width-1, win2->Height-Page_BottomPoint-win2->BorderBottom-1, 0);

    static_y_Koo = 0;	// ganz oben anzeigen
  }

  win_mov(0, 0, static_x_Koo, static_y_Koo);
}

void window_plus_sbar_move(int dx, int dy)		/* KEIN static */
{  
#if 0
  const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
  const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
#endif

  register int xa,ya;
  register int xh, yh;

  // dx zum x-Wert addieren und Grenzen beachten
  if (wx < winiw) {
    xa = xh = 0;
  }
  else {
    xa = static_x_Koo;
    xh = xa + dx;
    if (xh > wx-winiw) xh = wx-winiw;
    else if (xh < 0)   xh = 0;
  }

  // dy zum y-Wert addieren und Grenzen beachten
  if (wy < winih) {
    ya = yh = 0;
  }
  else {
    ya = static_y_Koo;
    yh = ya + dy;
    if (yh > wy-winih) yh = wy-winih;
    else if (yh < 0)   yh = 0;
  }

  // falls sich die Koo. geaendert haben, dann neu anzeigen
  if (!(xh == xa && yh == ya)) {
      win_mov(xh-static_x_Koo, yh-static_y_Koo, xh, yh);
      static_x_Koo = xh;
      static_y_Koo = yh;

      if (xh != xa) {
	ModifyXPot();
      }
      if (yh != ya) {
	ModifyYPot();
 	CheckUpDownBorderGad();		/* Teste oben/unten */
      }
  }
}

void window_move(const int dx, const int dy)
{  
#if 0
  const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
  const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
#endif

  register int xa, ya;
  register int xh, yh;

  // dx zum x-Wert addieren und Grenzen beachten
  if (wx < winiw) {
    xa = xh = 0;
  }
  else {
    xa = static_x_Koo;
    xh = xa + dx;
    if (xh > wx-winiw) xh = wx-winiw;
    else if (xh < 0)   xh = 0;
  }

  // dy zum y-Wert addieren und Grenzen beachten
  if (wy < winih) {
    ya = yh = 0;
  }
  else {
    ya = static_y_Koo;
    yh = ya + dy;
    if (yh > wy-winih) yh = wy-winih;
    else if (yh < 0)   yh = 0;
  }

  if (!(xh == xa && yh == ya)) {
      win_mov(xh-static_x_Koo, yh-static_y_Koo, xh, yh);
      static_x_Koo = xh;
      static_y_Koo = yh;
  }
}

void window_set_x (int x)				/* KEIN static */
{
#if 0
  const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
#endif

  register int xh;

  // x als x-Wert nehmen und Grenzen beachten
  if (wx < winiw) {
    xh = 0;
  }
  else {
    xh = x;
    if (xh > wx-winiw) xh = wx-winiw;
    else if (xh < 0)   xh = 0;
  }

  win_mov(xh-static_x_Koo, 0, xh, static_y_Koo);
  static_x_Koo = xh;
}

void window_set_y (int y)				/* KEIN static */
{
#if 0
  const long winih = win2->Height - win2->BorderTop - win2->BorderBottom - 1;
#endif

  register int yh;

  // y als y-Wert nehmen und Grenzen beachten
  if (wy < winih) {
    yh = 0;
  }
  else {
    yh = y;
    if (yh > wy-winih) yh = wy-winih;
    else if (yh < 0)   yh = 0;
  }

  win_mov(0, yh-static_y_Koo, static_x_Koo, yh);
  static_y_Koo = yh;
}



/***************    R E S O L U T I O N    **************************/


static ULONG GetWBModeID(void)
{
  struct Screen *scr;
  ULONG ret = INVALID_ID;
  
  if (is_os2) {
    scr = LockPubScreen(NULL);
    if (scr != NULL) {
      ret = GetVPModeID(&(scr->ViewPort));
      UnlockPubScreen(NULL, scr);
    }
  }
  
  return ret;
}

// nicht static, da es als Default-Wert fuer GetScreenMode benoetigt wird
ULONG get_DisplayID(void)
{
  ULONG DisplayID = INVALID_ID;

  if (is_numeric) DisplayID = show_state.DisplayID;

  if (is_wbmode) {
    DisplayID = GetWBModeID();
  }
  
  if (DisplayID == INVALID_ID) {
    if (is_pal) {
      DisplayID = PAL_MONITOR_ID | ((is_lace) ? HIRESLACE_KEY : HIRES_KEY);
    }
    else if (is_ntsc) {
      DisplayID = NTSC_MONITOR_ID | ((is_lace) ? HIRESLACE_KEY : HIRES_KEY);
    }
    else if (is_prod) {
      DisplayID = VGA_MONITOR_ID | ((is_lace) ? VGAPRODUCTLACE_KEY : VGAPRODUCT_KEY);
    }
    else if (is_a2024) {
      DisplayID = A2024_MONITOR_ID | ((is_lace) ? A2024TENHERTZ_KEY : A2024FIFTEENHERTZ_KEY);
    }
    else {
      DisplayID = DEFAULT_MONITOR_ID | ((is_lace) ? HIRESLACE_KEY : HIRES_KEY);
    }
  }
  
  return DisplayID;
}


/* liefert den String zu einem DisplayID */
/* bei INVALID_ID wird der ID des aktuellen Screens zurueckgeliefert */
/* in lace wird zurueckgegeben, ob der Mode interlaced ist oder nicht */
char * GetModeIDName(ULONG id, long * lace)
{
  static char Name[DISPLAYNAMELEN+1];

  if (id == INVALID_ID) {
    id = GetVPModeID(&(screen->ViewPort));
  }
  
  if (id == INVALID_ID || ModeNotAvailable(id)) {
    return GetTeXString(MSG_UNAVAILABLE_MODE);
  }
  else {
   struct DisplayInfo DispInfo;
   struct NameInfo NameInfo;
    if (GetDisplayInfoData(NULL, (UBYTE *)&NameInfo, sizeof(struct NameInfo), DTAG_NAME, id)) {
      strcpy(Name, NameInfo.Name);
      if (GetDisplayInfoData(NULL, (UBYTE *)&DispInfo, sizeof(struct DisplayInfo), DTAG_DISP, id)) {
        *lace = DispInfo.PropertyFlags & DIPF_IS_LACE;
      }
      return Name;
    }
    else {
      return GetTeXString(MSG_UNKNOWN_MODE_NAME);
    }
  }
}


int get_default_screen_dimension(long *width, long *height, short *lace)
{
  struct Screen hscr;
  struct Rectangle Rect;
  ULONG DisplayID;
  int res;

  if (is_os2) {
    if (!is_ownscr) {
      struct Screen *scr;
      scr = LockPubScreen(PubScreenName);
      if (scr == NULL) {
        Warning(MSG_CANT_FIND_SCR_USE_WB, PubScreenName);
        strcpy(PubScreenName, "Workbench");
        scr = LockPubScreen(PubScreenName);
        if (scr == NULL) {
          Fatal(15, MSG_CANT_LOCK_PBSCR);
        }
      }
      /* DisplayID = GetVPModeID(&(scr->ViewPort)); */
      *width = scr->Width;
      *height = scr->Height;
      *lace = (short)(scr->ViewPort.Modes & LACE);
      UnlockPubScreen(NULL, scr);
      res = TRUE;
    }
    else {	/* eigener Screen */
      DisplayID = get_DisplayID();
      res = QueryOverscan(DisplayID, &Rect, OSCAN_TEXT);
      if (res) {
        *width  = Rect.MaxX - Rect.MinX + 1;
        *height = Rect.MaxY - Rect.MinY + 1;
        *lace   = is_lace;
      }
    }
  }
  else {
    res = GetScreenData((char *)&hscr, (long)sizeof(hscr), (long)WBENCHSCREEN,
				IntuitionBase->ActiveScreen);
    if (res) {
      *width  = hscr.Width;
      *height = hscr.Height;
      *lace   = (short)(hscr.ViewPort.Modes & LACE);
    }
  }
  return res;
}


static int get_screen_dimension(long *width, long *height, short *lace)
{
  int res;

  if (show_state.screen_size_x > 0 && show_state.screen_size_y > 0) {
    *width  = show_state.screen_size_x;
    *height = show_state.screen_size_y;
    *lace   = is_lace;
    res = TRUE;
  }
  else {
    res = get_default_screen_dimension(width, height, lace);
  }
  return (res);
}


static void set_resolution(void)
{
  long width, height;
  short was_lace;

  if (get_screen_dimension(&width, &height, &was_lace)) {
      if (!is_ownscr) {
        x_scr_width = width;
        x_scr_height = height;
      }
      else {
	x_scr_width = width;
	if (was_lace && is_lace) {
	  x_scr_height = height;
	}
	else {
	  if (was_lace) {
	    x_scr_height = height/2;
	  }
	  else {
	    if (is_lace) {
	      x_scr_height = height*2;
	    }
	    else {
	      x_scr_height = height;
	    }
	  }
	}
      }
  }
  else {				/* can't get screen-data ?? */
    x_scr_width  = NTSC_SCR_WIDTH;	/* minimal size */
    x_scr_height = NTSC_SCR_HEIGHT;
  }

  /* check fom minimal size */
  if (x_scr_width  < NTSC_SCR_WIDTH)  x_scr_width  = NTSC_SCR_WIDTH;
  if (x_scr_height < NTSC_SCR_HEIGHT) x_scr_height = NTSC_SCR_HEIGHT;

  if ((is_ownscr && show_state.window_size_own_scr_x == -1) || (!is_ownscr && show_state.window_size_x == -1)) {
    if (is_ownscr) {
      x_win_width  = x_scr_width  - ((show_state.window_pos_own_scr_x != -1) ? show_state.window_pos_own_scr_x : 0);
    } 
    else {
      x_win_width  = x_scr_width  - ((show_state.window_pos_x != -1) ? show_state.window_pos_x : 0);
    }
  }
  else {
    x_win_width  = (is_ownscr) ? show_state.window_size_own_scr_x : show_state.window_size_x;
  }

  if ((is_ownscr && show_state.window_size_own_scr_y == -1) || (!is_ownscr && show_state.window_size_y == -1)) {
    if (screen == NULL) {
      if (is_ownscr) {
        x_win_height = x_scr_height - ((show_state.window_pos_own_scr_y != -1) ? show_state.window_pos_own_scr_y : 0);
			/* der Rest wird spaeter in OpenS() gemacht */
      }
      else {
        x_win_height = x_scr_height - ((show_state.window_pos_y != -1) ? show_state.window_pos_y : 0);
      }
    }
    else {
      x_win_height = x_scr_height - HeightScreenTitle;
    }
  }
  else {
    x_win_height  = (is_ownscr) ? show_state.window_size_own_scr_y : show_state.window_size_y;
  }
}


static void set_i_window_size(void)
{
  const long c_winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
  const long c_winih = win2->Height - win2->BorderTop - win2->BorderBottom;

  if (winiw > wx) {
    Page_LeftPoint    = win2->BorderLeft + (c_winiw-wx) / 2;
    Page_RightPoint   = Page_LeftPoint + wx - 1;
  }
  else {
    Page_LeftPoint    = win2->BorderLeft;
    Page_RightPoint   = win2->Width - win2->BorderRight - 1;	// ??
  }

  if (winih > wy) {
    Page_TopPoint     = win2->BorderTop  + (c_winih-wy) / 2;
    Page_BottomPoint  = Page_TopPoint + wy -1;
  }
  else {
    Page_TopPoint     = win2->BorderTop;
    Page_BottomPoint  = win2->Height - win2->BorderBottom - 1;	// ??
  }
  
  x_win_i_width  = c_winiw;
  x_win_i_height = c_winih;
}


#if 0	/* wird nicht mehr gebraucht */
void ChangeScreenSize(short new_width, short new_height)
{
  show_state.screen_size_x = new_width;
  show_state.screen_size_y = new_height;
  CloseOpenScreen(FALSE, FALSE, TRUE);
}
#endif


void CloseOpenScreen(int change_lace, int change_color, int change_size)
{
  int was_about, was_messwin, was_fullpage, was_prefwin, was_pscro, was_searchwin;
  short messX, messY;

  was_about = was_messwin = was_fullpage = was_prefwin = was_pscro = was_searchwin = FALSE;

  if (is_about) {
    AboutWinDown();	/* das ist schliesslich auch ein Window */
    was_about = TRUE;
  }
  if (is_messwin) {
    /* falls nur die Farben geaendert werden, alte Pos. beibehalten */
    CloseMessWin();
    messX = MessWinXpos;	/* merke die alte Position */
    messY = MessWinYpos;
    was_messwin = TRUE;
  }
  
  if (SearchWin) {
    CloseSearchWin();
    was_searchwin = TRUE;
  }
  
  if (is_prefwin) {
    ClosePrefWin();
    was_prefwin = TRUE;
  }
  
  if (is_show) {
    was_fullpage = TRUE;
    show_full_page(TRUE);	/* aus (no_refresh == TRUE) */
  }

  if (!can_i_exit()) {		// testet die Windows, vertreibt SpecialHost und macht Msg.
    return;
  }

  if (screen!=NULL) {
#if !defined(REQ_LIBRARY)
    remove_col_request();
#endif
    AboutWinDown();
    CloseWin2();
    CloseS();
  }


  if (change_lace) {
    toggle_lace;
  }
  if (change_color) {
    toggle_col4;
  }
  if (change_size) {
    //free_images();
  }


  set_resolution();
  
  if (change_size) {
    //init_images();
  }


  IsScreenReOpend = TRUE;

  OpenS();
  

  /*
   * Nun, wenn der Screen ein Village-Screen ist und wenn die Bitmap in Chip ist,
   * dann muss nach Fast umkopiert werden.
   * Wenn der Screen kein village-Screen ist, und die Bitmap in Fast ist,
   * dann muss nach Chip kopiert werden.
   * Ausser, wenn ohnehin per CPU kopiert werden soll.
   */

  if (is_bmfast && !is_village) {
    set_bmcpu;
    set_scrollras;
  }

#if 0   
  /*
   * ACHTUNG!!
   *
   * Das funktioniert noch nicht, wenn ich von Chip nach Fast oder umgekehrt wechsle,
   * dann geht das Bild floeten. Erst nach einem zusaetzlichen Aufloesungsswitch ist
   * es wieder da!
   *
   */

  if ((is_village && !is_bmfast) || (!is_village && is_bmfast && !is_bmcpu)) {
    struct BitMap OldBitMapWin = BitMapWin;
    unsigned long BytesToCopy = BitMapWin.BytesPerRow * BitMapWin.Rows;
    
    D(bug("vor  OpenRP vill: %ld, bmfast: %ld, bmcpu: %ld\n", is_village, is_bmfast, is_bmcpu));

    /*
     * Wenn village, dann oeffnet OpenRastPort() im Fast, ansonsten in Chip.    
     */
    OpenRastPort();
    
    D(bug("nach OpenRP vill: %ld, bmfast: %ld, bmcpu: %ld\n", is_village, is_bmfast, is_bmcpu));

    if (!BitMapWin.Planes[0]) Fatal(10, MSG_CANT_ALLOC_BITMAP);

    if (BytesToCopy & 3) {
      CopyMem(OldBitMapWin.Planes[0], BitMapWin.Planes[0], BytesToCopy);
    }
    else {
      CopyMemQuick(OldBitMapWin.Planes[0], BitMapWin.Planes[0], BytesToCopy);
    }
    
    D(bug("kopiere die Plane, bmfast ist nun: %ld\n", is_bmfast));

/*
 *     Achtung: Das zurueck kopieren geht nicht...liegt moeglicherweise an DOMINO!!!!!
 */

    /*
     *  Wenn nun bmfast, dann war's vorher Chip...
     */
    if (is_bmfast) {
      FreeRaster(OldBitMapWin.Planes[0],wx,wy);
    }
    else {
      FreeMem(OldBitMapWin.Planes[0], (wx+7)/8*wy);
    }
  }
#endif



  OpenWin2();
  set_i_window_size();
  init_gad();

  Add_system_gadgets();

  Set_PgPotGadRange();
  Set_PgGadPageCur();

  if (is_gadg) {
#if 0
	/*
	 * Pot/Body werden schon in init_gad gesetzt
	 */
	if (!is_pscro) {
          const long winih = win2->Height - win2->BorderTop - win2->BorderBottom - 1;
	  //i = (int)((winih * (long)(poty_Gad.UserData)) / (long)wy);
	  //poty_Image_1.Height = (SHORT)i;
	  //poty_Image_2.Height = (SHORT)i;
	  poty_PropInfo.VertPot =
	   (USHORT)(((long)(static_y_Koo)<<16)/(wy-winih+1L));
	  poty_PropInfo.VertBody = (USHORT)((long)((long)MAXBODY * winih)/(long)wy);
	  //set_zahl((int)999);	/* setzt die Images */
	}
#endif
	Add_scroll_gadgets(FALSE);	/* no_refresh == FALSE */
  }
  
  /* setup the things */
  write_status();

#if 0
  if (is_pscro) {
    AddGadget(win2, &pgscroll_ok_Gad, PGSCROLL_OK_GAD_NR);
    AddGadget(win2, &pgscroll_no_Gad, PGSCROLL_NO_GAD_NR);
    /* i = (int)(poty_Gad.Height / max_page_number); */
    i = (int)((int)poty_Gad.UserData / max_page_number);
    if (max_page_number <100) {
      if (i<24) i=24;
    }
    else {
      if (i<35) i=35;
    }
    poty_Image_1.Height = (SHORT)i;
    poty_Image_2.Height = (SHORT)i;
    if (max_page_number == 0) Fatal(20,MSG_INTERNAL_ERROR);
    poty_PropInfo.VertPot = (USHORT)page_to_pot(current_page);
    poty_PropInfo.VertBody = (USHORT)(MAXBODY/max_page_number-1);
    set_zahl((int)current_page);
  }
#endif
  /* RefreshGadgets(&first_Page_Gad,win2,NULL); */

  refresh_screen();
  if (is_gadg) {		/* setzt die Grenzen richtig */
    window_plus_sbar_move(0,0);
  }
  else {
    window_move(0,0);
  }
  window_show();
  
  if (was_fullpage) {
    show_full_page(TRUE);
  }
  
  if (was_searchwin) {
    OpenSearchWin();
  }
  
  if (was_about) {
    AboutWinUp();
  }
  if (was_messwin) {
    if (!change_lace) {
      MessWinXpos = messX;	/* alte Position zurueck */
      MessWinYpos = messY;
      /* eigentlich muesste nur die Y-Pos restored werden */
    }
    OpenMessWin();
  }
  if (was_prefwin) {
    OpenPrefWin();
  }

  /* setze os_menu richtig */
  set_checked_os_menu();
  
  { long width, height;
    short lace;
    if (get_default_screen_dimension(&width, &height, &lace)) {
      if (width == show_state.screen_size_x && height == show_state.screen_size_y) {
        show_state.screen_size_x = 0;
        show_state.screen_size_y = 0;
      }
    }
  }
}


void ToggleColorDepth(void)
{
  CloseOpenScreen(FALSE, TRUE, FALSE);
}


void change_resolution(void)
{
  CloseOpenScreen(TRUE, FALSE, FALSE);
}


void toggle_scrollbar(int no_refresh)
{
  toggle_gadg;
  set_resolution();
  set_i_window_size();

  if (is_gadg) {
    init_gad();
    Add_scroll_gadgets(no_refresh);
  }
  else {
#if 0
    if (is_pscro) {
      long dummy;
      blatt_ok_gad(&dummy);
      unset_tusephy; /* wird leider von der blatt_ok_gad Funktion gesetzt */
    }
#endif
    Remove_scroll_gadgets(no_refresh);
#if 0
    /* und nun bei col4 muessen noch die Ueberreste geloescht werden, fuer den Fall, dass   */
    /* die Bitmap groesser wird. Fall einmal die Fenstergroesse mit geaendert wird, so wird */
    /* das nicht mehr benoetigt.								  */
    fill_block(potx_Gad.LeftEdge, potx_Gad.TopEdge,
    	2*potx_left_Gad.Width+potx_Gad.Width, potx_Gad.Height, 0);
    fill_block(poty_up2_Gad.LeftEdge, poty_up2_Gad.TopEdge, poty_up2_Gad.Width,
    	4*poty_up2_Gad.Height+poty_Gad.Height, 0);

    RefreshWindowFrame(win2);	/* flacker flacker.. naja aber sonst ist der Rahmen weg */
#endif
    
    {
      // nun Koo. neu setzen...dabei Grenzen beachten
#if 0
      const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
      const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
#endif

      if (wx < winiw) static_x_Koo = 0;
      else if (static_x_Koo + winiw > wx) static_x_Koo = wx - winiw;
      if (wy < winih) static_y_Koo = 0;
      else if (static_y_Koo + winih > wy) static_y_Koo = wy - winih;
    }
  }

  if (!no_refresh) {
    RefreshWindowFrame(win2);
    //refresh_screen();
    window_show();
  }

  /* setze os_menu richtig */
  set_checked_os_menu();
}




/*********************    F U L L   P A G E    **********************/


#define FPAGE_ABS HeightWinTitle


void display_full_page(void)
{
  /* kopiere die FullPage in den Window-Rastport */

  MySetABPenDrMd(win2->RPort, show_state.APen, (is_bhook) ? show_state.BPen : 0, JAM2);
  BltTemplate(RastPortFullPage.BitMap->Planes[0], 0,
		RastPortFullPage.BitMap->BytesPerRow, win2->RPort, FullPageLeftEdge, FullPageTopEdge, FullPageWidth, FullPageHeight);

#ifdef OLD_BLIT
  if (is_bhook) {
    SafeSetWriteMask(win2->RPort, 0x02);
    ClipBlit(&RastPortFullPage, 0, 0, win2->RPort, FullPageLeftEdge, FullPageTopEdge, FullPageWidth, FullPageHeight, 0x30);
  }
  SafeSetWriteMask(win2->RPort, 0x01);
  ClipBlit(&RastPortFullPage, 0, 0, win2->RPort, FullPageLeftEdge, FullPageTopEdge, FullPageWidth, FullPageHeight, 0xC0);
  SafeSetWriteMask(win2->RPort, 0xff);
#endif

#if 0
  // alt, ohne black-white
  ClipBlit(&RastPortFullPage,0,0,win2->RPort,FullPageLeftEdge,FullPageTopEdge,FullPageWidth,FullPageHeight,192);
#endif

  if (is_dotbord) {
    DrawDottedBorder(FALSE, TRUE);	/* nun wieder Border zeichnen */
  }
}


void show_full_page(int no_refresh)
{
  register struct RastPort *rp;
  int p_width, p_height;
  float x;
  static int war_gadg;
  long xk, yk;
  short vis_width, vis_height;			/* soviel ist sichtbar */
  short max_show_width, max_show_height;	/* soviel kann maximal verwendet werden */
  /* wenn nicht alles sichtbar, bewege ich den Screen ganz nach links oben!! */


  if (!is_show) {
	set_show;
	write_status();
	sleep_pointer();

	if (is_gadg /* && wx > x_win_i_width */) {	/* dazu muessten die SBar's disabeld werden! */
	   toggle_scrollbar(no_refresh);	/* SBar ausschalten */
	   //toggle_scrollbar(TRUE);		/* SBar ausschalten */
	   // Beim Scroll-Bar ausschalten auf keinen Fall einen Refresh machen!
	   war_gadg=TRUE;
	}
	else {
	   war_gadg=FALSE;
	}
	if (is_dotbord) {
	  unset_show;
	  DrawDottedBorder(FALSE, TRUE);	/* toggle = FALSE (ausschalten aber is_ lassen) */
	  set_show;
	}


        vis_width  = screen->Width+1;		// das ergibt 801 (bei screenw 800) (siehe unten)
        vis_height = screen->Height+1;

        if (is_os2) {
          if (is_village && !is_ownscr) {
            // Wir sind auf der Graphikkarte
            // vis_* stimmt mit dem des Screens ueberein
            //printf("beep\n");
          }
          else {
            struct ViewPort *vp;
            ULONG modeID;
            struct DimensionInfo DimInfo;
            
            vp = &(screen->ViewPort);
            if ((modeID = GetVPModeID(vp)) != INVALID_ID) {
              GetDisplayInfoData(NULL, (UBYTE *)&DimInfo, sizeof(DimInfo), DTAG_DIMS, modeID);
              vis_width  = DimInfo.TxtOScan.MaxX + 1;	// das ergibt nun nur 800!! (siehe oben)
              vis_height = DimInfo.TxtOScan.MaxY + 1;
            }
          }
        }
        
	FullPageScreenXPos = 0;
	FullPageScreenYPos = 0;
        if (is_os2 && show_state.screen_size_x != 0) {
          /* ich setze den Screen IMMER ganz nach links oben! */
	  FullPageScreenXPos = screen->LeftEdge;
	  FullPageScreenYPos = screen->TopEdge;
          MoveScreen(screen, -screen->LeftEdge, -screen->TopEdge);
        }
        

        {
#if 0
          const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
          const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
#endif

          if (winiw < vis_width-width_left_border) {
            max_show_width = winiw;
          }
          else {
            max_show_width = vis_width-width_left_border;
          }
          if (winih < vis_height-HeightWinTitle-HeightScreenTitle) {
            max_show_height = winih;
          }
          else {
            max_show_height = vis_height-HeightWinTitle-HeightScreenTitle;
          }
          
          if (max_show_width > wx)  max_show_width  = wx;
          if (max_show_height > wy) max_show_height = wy;
        }


	p_height = max_show_height - (FPAGE_ABS * 2);

	/* eine erste Vorberechnung, dient nur zur Bestimmung von p_height */
	x = (float)p_height/(float)wy;
	p_width = (int)((float)wx * x);

	D(bug("full-screen: is_lace: %ld, is_numeric: %ld\n", is_lace, is_numeric));

#ifdef UEBERKRAMPF

	// wozu bittesehr wird hier p_WIDTH verwendet??
	p_width = (is_lace) ? p_width : p_width * 2;

	if (p_width >= max_show_width) {
	  p_width = max_show_width - (FPAGE_ABS * 2);
	  x = (float)p_width/(float)wx;
	  p_height = (int)((float)wy*x);
	}
#endif
	
	/* correct aspect ratio */
	{

	/*
         *            scr_width    /  scr_height
	 *  ratio =   ---------   /   ----------
         *              mon_x    /      mon_y
         *
         */

	   const float ratio =
	   	   ((float)show_state.monitor_size_y * (float)vis_width) /
	  	   ((float)show_state.monitor_size_x * (float)vis_height);

	   p_width = ((float)wx * (float)p_height / (float)wy) * ratio;

	   /*
	   printf("wx: %d, wy: %d, scr_h: %d, scr_w: %d, mon_x: %d, mon_y: %d\n", wx, wy,
		vis_height, vis_width, show_state.monitor_size_x, show_state.monitor_size_y);
	   printf("p_h: %d, ratio: %f, p_width: %d\n", p_height, ratio, p_width);
	   printf("Aspect orig: %f, Aspect small: %f\n", (float)wx/(float)wy, (float)p_width/(float)p_height);
	   */

	   if (p_width >= max_show_width) {
	     p_width = max_show_width - (FPAGE_ABS * 2);
	   }
	}



	xk = (max_show_width - p_width) / 2;
	yk = FPAGE_ABS-1+(max_show_height - p_height) / 2;


	rp  = win2->RPort;

        InitBitMap(&BitMapFullPage,7L,p_width,p_height);
        BitMapFullPage.Planes[0] = AllocRaster(p_width, p_height);
        if (BitMapFullPage.Planes[0] == NULL) {  
	   Fatal(10,MSG_NO_CHIPMEM);
        }

        { int i;
          for (i=1; i<7; i++) {
            BitMapFullPage.Planes[i] = BitMapFullPage.Planes[0];
          }
          BitMapFullPage.Planes[7] = NULL;	// nur 7 Planes identisch, wg. PICASSO Treiber (chunky)
        }

        InitRastPort(&RastPortFullPage);
        RastPortFullPage.BitMap = &BitMapFullPage;
        SetRast(&RastPortFullPage,0);
        BitMapFullPageWidth  = p_width;
	BitMapFullPageHeight = p_height;

#if 0
	fpage_bytes = ((p_width+15)/16)*p_height*2;
	fpage = AllocMem(fpage_bytes,MEMF_CHIP|MEMF_CLEAR|MEMF_PUBLIC);
	if (fpage == NULL) {
	  Fatal(10,MSG_NO_CHIPMEM);
	}
#endif

	Message(MSG_BUILD_FULL_PAGE);
	make_full_page(rp, xk, yk, p_width, p_height, (unsigned short *)(BitMapFullPage.Planes[0]));


	/* loesche Bildschirm */
	{
#if 0
          const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
          const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
#endif
	  fill_block(width_left_border, HeightWinTitle, winiw-1, winih-1, 0);
	}
	/* SBars sind schon aus => innere Groese muesste reichen zu loeschen */


	/* wird fuer das Mess-Window benoetigt */
  	FullPageLeftEdge = xk;
  	FullPageTopEdge  = yk;
  	FullPageWidth 	 = p_width;
  	FullPageHeight	 = p_height;


	if (!no_refresh) {
	  /* und nun wird die Bitmap einkopiert... */
	  display_full_page();
  	}
	
	clear_pointer();
	MessageStr(NULL);
  }
  else
  {
        if (BitMapFullPage.Planes[0] != NULL) {
          FreeRaster(BitMapFullPage.Planes[0], BitMapFullPageWidth, BitMapFullPageHeight);
          BitMapFullPage.Planes[0] = NULL;
        }
	if (is_dotbord) {
	  set_show;
	  DrawDottedBorder(FALSE, no_refresh);	/* kein Toggle.. (loesche FullPage) */
	  unset_show;
	  DrawDottedBorder(FALSE, no_refresh);	/* kein Toggle.. (zeichne neu) */
	}
	unset_show;

        if (is_os2 && show_state.screen_size_x != 0 && screen->LeftEdge == 0 && screen->TopEdge == 0) {
          /* setzt den Screen wieder dahin zurueck, wo er war */
          MoveScreen(screen, FullPageScreenXPos, FullPageScreenYPos);
        }

	write_status();
	if (war_gadg) {
	  toggle_scrollbar(no_refresh);
	}
	if (!no_refresh) {
          if (is_gadg) {		/* setzt die Grenzen richtig */
            window_plus_sbar_move(0,0);
          }
          else {
            window_move(0,0);
          }
	  window_show();
	}
  }
}



void DrawDottedBorder(int toggle, int no_refresh)
{
  register struct RastPort *rp;
  long lx, ly, lxx, lyy;
  float widthin, heightin;	/* full width/height */

  if (toggle) {
    toggle_dotbord;
  }

  if (win2 != NULL) {
  
    widthin  = (float)pwidth_pt  / 72.27 + ((float)SAVETY_BITS_X / (float)hconvresolution) * 1000.0 / (float)mag;
    heightin = (float)pheight_pt / 72.27 + ((float)SAVETY_BITS_Y / (float)vconvresolution) * 1000.0 / (float)mag;

    if (is_show) {

      rp = win2->RPort;
      SetAPen(rp, 1);
      SetDrPt(rp, 0xAAAA);	// gfxmacros.h
      SetDrMd(rp, JAM1 | COMPLEMENT);

      lx  = (long)((float)(hoffset_in_fix * FullPageWidth) / widthin + 0.5);
      ly  = (long)((float)(voffset_in_fix * FullPageHeight) / heightin + 0.5);
      lxx = (long)((((float)pwidth_pt  / 72.27) * (float)FullPageWidth) / widthin + 0.5);
      lyy = (long)((((float)pheight_pt / 72.27) * (float)FullPageHeight) / heightin + 0.5);

      lx  = FullPageLeftEdge + lx;
      ly  = FullPageTopEdge + ly;
      lxx = FullPageLeftEdge + lxx;
      lyy = FullPageTopEdge + lyy;

      Move (rp, FullPageLeftEdge+1, ly+1);
      Draw (rp, FullPageLeftEdge+FullPageWidth-1, ly+1);
      Move (rp, FullPageLeftEdge+1, lyy-1);
      Draw (rp, FullPageLeftEdge+FullPageWidth-1, lyy-1);
      
      Move (rp, lx+1, FullPageTopEdge+1);
      Draw (rp, lx+1, FullPageTopEdge+FullPageHeight-2);
      Move (rp, lxx-1, FullPageTopEdge+1);
      Draw (rp, lxx-1, FullPageTopEdge+FullPageHeight-2);

      SetDrMd(rp, JAM1);
      SetDrPt(rp, 0xFFFF);	// gfxmacros.h
    }
    else {
      float MessWinSavetyX = (((float)SAVETY_BITS_X / (float)hconvresolution) * 1000.0 / mag) * 72.27 ;
      float MessWinSavetyY = (((float)SAVETY_BITS_Y / (float)vconvresolution) * 1000.0 / mag) * 72.27 ;

      rp = &myRastPort;

      SafeSetWriteMask(rp, 0x01);

      if (resolution < 85) {
        SetDrPt(rp, 0xAAAA);	// gfxmacros.h
      }
      else {
        SetDrPt(rp, 0x3030);	// gfxmacros.h
      }
      SetDrMd(rp, JAM1 | COMPLEMENT);

      lx = (long)((float)(hoffset_in_fix * wx) / widthin + 0.5);
      ly = (long)((float)(voffset_in_fix * wy) / heightin + 0.5);
      //lxx = (long)(((((float)pwidth_pt)  / 72.27) * (float)wx) / widthin + 0.5);
      //lyy = (long)(((((float)pheight_pt) / 72.27) * (float)wy) / heightin + 0.5);
      lxx = paper_width;
      lyy = paper_height;

      Move (rp, 1, ly+1);
      Draw (rp, wx-1, ly+1);
      Move (rp, 1, lyy-1);
      Draw (rp, wx-1, lyy-1);
      
      Move (rp, lx+1, 1);
      Draw (rp, lx+1, wy-2);
      Move (rp, lxx-1, 1);
      Draw (rp, lxx-1, wy-2);

      if (!no_refresh) {
        window_show();
      }
      SetDrMd(rp, JAM1);
      SetDrPt(rp, 0xFFFF);	// gfxmacros.h

      SafeSetWriteMask(rp, 0xFF);
    }

    if (toggle) {
      set_checked_os_menu();	/* setze Menue korrekt */
    }
  }
}


/* liefert den Pfad zum file, falls file=="" dann Pfad des akt. Verz. */
void getdir(char *file, char *dir)
{
  BPTR lck = Lock(file, ACCESS_READ);
  __aligned struct FileInfoBlock fib;

  if (lck) {
    if (Examine(lck, &fib)) {
      if (NameFromLock(lck, dir, 199)) {
        if (fib.fib_DirEntryType > 0 && fib.fib_DirEntryType != ST_SOFTLINK) {
          // es ist schon ein Directory, also unveraendert uebergeben
        }
        else {
          // weg mit dem File Part
          *FilePart(dir) = '\0';
        }
      }
      else {
        *dir = '\0';	// keine Lust fuer Mords-Fehlermeldungen
      }
    }
    else {
      *dir = '\0';	// keine Lust fuer Mords-Fehlermeldungen
    }
    UnLock(lck);
  }
  else {
    FatalStr(20, "getdir: Can't lock file \"%s\"", file);
  }


#if 0
  BPTR lock, lock1;
  char strh[250], *h;
  struct FileInfoBlock *fib;
  struct Process *pr;


  if (*file == '\0') {
    pr = (struct Process *)FindTask(NULL);
    if (pr->pr_CurrentDir == NULL) {
      strcpy(dir,"SYS:");
      return;
    }
    else {
      lock = DupLock(pr->pr_CurrentDir);
    }
  }
  else {
    lock = Lock(file,ACCESS_READ);
  }

  if (lock==(BPTR)NULL) {
     Fatal(5,"can't find file \"%s\" to examine!",file);
  }

  fib = (struct FileInfoBlock *)xmalloc((unsigned)sizeof(struct FileInfoBlock));

  dir[0]='\0';
  if (Examine(lock,fib) == 0) {
    Fatal(5,"can't examine file!");
  }

  if (file[0] == '\0' || fib->fib_DirEntryType > 0 ) {	/* aktuelles Verz. oder Dir.*/
     strcpy(dir, fib->fib_FileName);
     strcat(dir,"/");
  }
  lock1 = ParentDir(lock);
  UnLock((BPTR)lock);
  lock = lock1;
  while (lock!=(BPTR)NULL)
   {
    if (Examine(lock,fib)==0) {
       Fatal(5,"can't examine file!");
    }
    else {
       strcpy(strh,fib->fib_FileName);
       strcat(strh,"/");
       strcat(strh,dir);
       strcpy(dir,strh);
    }
    lock1 = ParentDir(lock);
    UnLock((BPTR)lock);
    lock = lock1;
   }
  h = strchr(dir,'/');
  if (h != NULL) {
    h[0] = ':';
  }
  xfree((char *)fib);
#endif
}

int is_dir(char *file)
{
  BPTR lck = Lock(file, ACCESS_READ);
  __aligned struct FileInfoBlock fib;
  int ret = FALSE;

  if (lck) {
    if (Examine(lck, &fib)) {
      ret = fib.fib_DirEntryType > 0 && fib.fib_DirEntryType != ST_SOFTLINK;
    }
    UnLock(lck);
  }
  return ret;

#if 0
  struct FileLock *lock;
  struct FileInfoBlock *fib;
  int is = FALSE;

  fib = (struct FileInfoBlock *)xmalloc((unsigned)sizeof(struct FileInfoBlock));
  lock = (struct FileLock *)Lock(file,ACCESS_READ);

  if (lock!=NULL) {
    if (Examine((BPTR)lock,fib)!=0) {
      if (fib->fib_DirEntryType>0) {
        is = TRUE;
      }
    }
    UnLock((BPTR)lock);
  }
  xfree((char *)fib);
  return (is);
#endif
}



void SavePageIFF(char * name)
{
  int clip;
  int mode;

  clip = *name == 'C' && name[1] == 'L' && name[2] == 'I' && name[3] == 'P';

  memset(&ilbm, 0, sizeof(ilbm));
  ilbm.ParseInfo.iff = AllocIFF();
  if (!ilbm.ParseInfo.iff) {
    Fatal(10, MSG_NO_MEM);
  }
  
  mode = GetVPModeID(&(screen->ViewPort));
  if (saveilbm(&ilbm, myRastPort.BitMap, mode,
		wx, wy, wx, wy, bw_ctable, 2, 4, 0, 0, NULL, NULL, name)) {
    if (clip) Warning( MSG_CANT_SAVE_TO_CLIP);
    else      Warning( MSG_CANT_SAVE_TO_IFF, name);
  }
  FreeIFF(ilbm.ParseInfo.iff);
}



int is_newer_dvi_file(void)		/* kein static! (arexx.c) */
{
  static struct DateStamp date_cur_file;
  struct FileLock *lock;
  __aligned struct FileInfoBlock fib;
  int ret = FALSE;

  if (is_new_dvifile) {
    is_new_dvifile = FALSE;
    date_cur_file.ds_Days   = MAXINT;
    date_cur_file.ds_Minute = MAXINT;
    date_cur_file.ds_Tick   = MAXINT;
  }

  lock =  (struct FileLock *)Lock(filename,ACCESS_READ);

  if (lock != NULL) {
    if (Examine((BPTR)lock, &fib)!=0) {
          ret = (CompareDates(&date_cur_file, &(fib.fib_Date)) > 0);
    }
    date_cur_file = fib.fib_Date;	/* zyklische Zuweisung. Passt nach dem ersten Mal */
    UnLock((BPTR)lock);
  }
  return ret;
}


/* Initialisiere Notify auf das aktuelle DVI File */
static int InitDVINotify(void)
{
  int suc;
  char fehler[62];

  if (DOSBase->dl_lib.lib_Version < 37L) {	/* notify mach ich erst ab Version 37.x */
    return FALSE;
  }
  if (is_notify) {	/* falls schon ein Notify laeuft, wird dies erst beendet */
    EndDVINotify();
  }
  if (!is_autoag) {	/* wenn kein autoag -> dann auch kein notify */
    return FALSE;
  }

  sig_notify = AllocSignal(-1);
  if (sig_notify == -1) {
    Warning(MSG_NO_SIGNAL);
    return FALSE;
  }

  notify.nr_Name = filename;
  notify.nr_FullName = NULL;
  notify.nr_UserData = NULL;
  notify.nr_Flags = NRF_SEND_SIGNAL; 	/* | NRF_NOTIFY_INITIAL; */
  notify.nr_stuff.nr_Signal.nr_Task = FindTask(NULL);
  notify.nr_stuff.nr_Signal.nr_SignalNum = sig_notify;

  suc = StartNotify(&notify);
  if (suc != DOSTRUE) {
    if (Fault(IoErr(), "Error", fehler, 61)) {
      WarningStr(fehler);
      //MySimpleRequest(1, NULL, NULL, NULL, NULL, GetTeXString(MSG_CANT_NOTIFY), filename, fehler);
    }
    return FALSE;
  }
  set_notify;
  return TRUE;
}

/* Beende Notify */
static int EndDVINotify(void)
{
  if (is_os2) {
    if (is_notify) {
      EndNotify(&notify);
    }
    if (sig_notify > 0) {
      FreeSignal(sig_notify);
    }
    unset_notify;
    return TRUE;
  }
  return FALSE;
}

void ToggleAutoAgain(void)
{
  toggle_autoag;
  if (!is_autoag) EndDVINotify();
  else InitDVINotify();
}




void set_Gadgets_to_fname(void)			/* KEIN static / kein refresh */
{
#if 0
  char fname[250], currentdir[250], *tptr;

  if (filename[0]=='\0') {  /* kein filename => kein Aendern der Gadgets noetig */
     return;
  }
  if (is_dir(filename)) {  /* nur ein Directory */
     strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer, filename);
     strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->UndoBuffer, filename);
     *((struct StringInfo *)(fil_Gad.SpecialInfo))->Buffer = (char)0;
     *((struct StringInfo *)(fil_Gad.SpecialInfo))->UndoBuffer = (char)0;
  }     
  else
  {
    strcpy(fname, filename);
    /* zuerst Extension abspalten */
    tptr = strrchr(fname, '.');
    if (tptr != NULL) {
       *tptr = (char)0;
    }
    /* nun noch Direct. abspalten */
    tptr = strrchr(fname, '/');
    if (tptr == NULL) {
       tptr = strrchr(fname, ':');
       if (tptr == NULL) {
	     /* es exist. gar kein Direct. Teil */
	     getdir("",currentdir);	/* path of the current dir */
	     strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer, currentdir);
	     strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->UndoBuffer, currentdir);
	     strcpy((char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->Buffer, fname);
	     strcpy((char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->UndoBuffer, fname);
       }
    }
    if (tptr != NULL) {
       strcpy((char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->Buffer, tptr+1);
       strcpy((char *)((struct StringInfo *)(fil_Gad.SpecialInfo))->UndoBuffer, tptr+1);
       if (*tptr == '/') {
	  *tptr = (char)0;
       }
       else {	/* Dir. Teil endet mit ':', soll erhalten bleiben */
	  tptr++;
	  *tptr = (char)0;
       }
       strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer, fname);
       strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->UndoBuffer, fname);
    }
  }
  /* NO refresh */
#endif
}


void set_int_gad()			/* KEIN static / KEIN refresh */
{
#if 0
  struct StringInfo *si;
  char *iBuf, *iuBuf;
  struct RastPort *rp;
  char str[8];
  long nr, len;
  long TopEdge;
  static long secundary = 0;

  si = (struct StringInfo *)int_Gad.SpecialInfo;	/* Seitennummer eintragen */
  iBuf = (char *)si->Buffer;
  iuBuf = (char *)si->UndoBuffer;

  rp = win2->RPort;
  TopEdge = (HeightWinTitle - rp->Font->tf_YSize) / 2;

  /* alte Zahl löschen */
  SetAPen(rp,1);
  if (secundary > 0) {
    SetDrMd(rp,COMPLEMENT);
    Move(rp, WR_SECUNDARY_LEFT,  TopEdge + rp->Font->tf_YSize);
    sprintf(str,"%d",secundary);
    len = strlen(str);
    if (len>4) {
       str[4] = (char)0;
    }
    Text(rp,str,(long)strlen(str));
    SetDrMd(rp, JAM1);
  }
  
  SetAPen(rp,0L);

  if (is_usephy) {
    sprintf(iBuf,"%d",current_page_phy);
    sprintf(iuBuf,"%d",current_page_phy);
    si->LongInt = (long)current_page_phy;
  }
  else {
    sprintf(iBuf,"%d",current_page);
    sprintf(iuBuf,"%d",current_page);
    si->LongInt = (long)current_page;

    if (win2 != NULL) {
      if ((nr = get_secundary(current_page_phy)) > 0) {
        Move(rp, WR_SECUNDARY_LEFT, TopEdge + rp->Font->tf_YSize);
        sprintf(str,"%d",nr);
        len = strlen(str);
        if (len>4) {
           str[4] = (char)0;
        }
        Text(rp,str,(long)strlen(str));
      }
      secundary = nr;
    }
  }
#endif
}

#if 0
static void page_scroll(int dx, int dy)
{
  long page;

  if (is_gadg) {
      if (dy != 0) {
      page = pot_to_page((long)poty_PropInfo.VertPot);
      switch (dy) {
        case -SCROLL_Y_NORM:
        case -SCROLL_Y_NORM_R:
        case -SCROLL_Y_NORM_R-1:		// im col4 Fall
        case -SCROLL_Y_SHIFT:
        case -SCROLL_Y_SHIFT_R:
		   page--;
		   if (page < 1) page = 1;
		   break;
        case SCROLL_Y_NORM:
        case SCROLL_Y_NORM_R:
        case SCROLL_Y_NORM_R+1:			// im col4 Fall
        case SCROLL_Y_SHIFT:
        case SCROLL_Y_SHIFT_R:
		   page++;
		   if (page > max_page_number) page = max_page_number;
		   break;
        default	 :
		   if (dy == -wy) {
		     page = 1;
		   }
		   else if (dy == wy) {
		          page = max_page_number;
		        }
		   break;
      }
      poty_PropInfo.VertPot = (USHORT)page_to_pot(page);
      set_zahl((int)page);
      ModifyYPot();
      /* RefreshGadgets(&poty_Gad,win2,NULL); */
      Delay(2);
    }
  }

  if (is_gadg) {
     window_plus_sbar_move(dx,0);
  }
  else {
     window_move(dx,0);
  }
}
#endif



static long work_with_main_app_win(void)
{
  struct AppMessage * msg;
  long ex = 0;

  while (ex == 0L && (msg = (struct AppMessage *)GetMsg(MainAppWinMsgPort)) != NULL) {
    if (msg->am_Type == MTYPE_APPWINDOW && msg->am_ID == 0) {
      int i;
      struct WBArg * arg = msg->am_ArgList;

      // bei mehreren Args such ich s lange, bis einmal ex != 0
      for (i=0; ex == 0 && i<msg->am_NumArgs; i++) {
        ex = CheckAppArgName(arg[i].wa_Name, arg[i].wa_Lock);
      }
    }
    ReplyMsg((struct Message *)msg);
  }
  
  if (ex != 0L) {
    OpenNewDVI(filename, FALSE);
    ScreenToFront(screen);
    make_show_active();
  }

  return ex;
}


static long intui_message(void)
{
  static long		current_gadget = -1;

  struct IntuiMessage 	*msg;
  ULONG 		msg_Class;
  UWORD			Code,Qualifier;
  long			ex = 0L, ret;
  UWORD			sel;
  struct MenuItem 	*ItemAdr;
  WORD			MouseX, MouseY;
  WORD			IMouseX, IMouseY;	// aus IBase...ist aktueller
  ULONG			secs, mics;
  struct Gadget		*m_gad;
  int			is_mouse_moved = FALSE;



  while (ex == 0L && (msg = (struct IntuiMessage *)GetMsg(win2->UserPort)) != NULL) {
  
    struct IntuiMessage copy_msg = *msg;	// komplett kopieren!

    msg_Class  = msg->Class;
    Code    = msg->Code;
    MouseX  = msg->MouseX;
    MouseY  = msg->MouseY;
    IMouseX = IntuitionBase->MouseX-win2->LeftEdge;
    IMouseY = IntuitionBase->MouseY-win2->TopEdge;
    //D(bug("msgM X: %3ld, Y: %3ld, IntM: X: %3ld, Y: %3ld, DX: %3ld, DY: %3ld\n", 
    //	MouseX, MouseY, IMouseX, IMouseY, MouseX-IMouseX, MouseY-IMouseY));
    secs   = msg->Seconds;
    mics   = msg->Micros;
    m_gad  = (struct Gadget *)msg->IAddress;
    Qualifier = msg->Qualifier;
    
    if (msg_Class == IDCMP_MENUVERIFY) {
      if (Code == MENUHOT) {
        if (!is_osmenu && MouseY >= HeightWinTitle) {
          msg->Code = Code = MENUCANCEL;
        }
      }
    }
    
    ReplyMsg(&(msg->ExecMessage));
    msg = NULL;


    switch (msg_Class) {
	  case IDCMP_INTUITICKS:
		   if (timer_wait_ticks > 0) {
		     timer_wait_ticks--;
		     if (timer_wait_ticks == 0) {
		       ULONG lck;
		       sub_inticks;
		       if (!is_inticks) {
		         MyModifyIDCMP(0L, IDCMP_INTUITICKS);
		       }
		       lck = LockIBase(0L);
		       if (IntuitionBase->ActiveWindow == win2) {
		         UnlockIBase(lck);
		         MessageStr(NULL);		/* refresh title */
		         unset_titref;
		       }
		       else {
		         UnlockIBase(lck);
		         set_titref;		/* merken, dass zu refreshen ist */
		       }
		     }
		   }
		   if (current_gadget != -1) {	/* Scrollbar Bewegungen */
		     follow_pot_gad(current_gadget);
		   }
		   if (MessWin != NULL) {
		     WorkMessWin(MouseX, MouseY, FALSE, FALSE);
		   }
		   is_mouse_moved = FALSE;	/* update ist gerade erfolgt */
		   break;
    	  case IDCMP_MENUVERIFY:
	  case IDCMP_MOUSEBUTTONS: 
		   if ((Code == MENUCANCEL) ||
		       (Code == MENUDOWN && !is_osmenu) || 
		       (Code == MIDDLEDOWN && is_midmenu)) {
		     ret = show_menu(Code, MouseX, MouseY);
					/* ret ==  0 normal        */
					/* ret ==  5 no action     */
					/* ret == -1 Seite zurueck */
					/* ret ==  1 Seite vor     */
					/* ret == -2 Textanfang    */
					/* ret ==  2 Textende      */
					/* ret == 10 Programmende  */
					/* ret ==  3 neues DVI-F.  */
                     if ((ret != 0)&&(ret != 5)) {
                       ex = KOMM+(long)ret;
                       /** warum beim Menu immer nach oben springen ??? 
                       if (ret == -1 || ret == 1 || ret == -2 || ret == 2) {
                         static_y_Koo = 0;
                       }
                       **/
                     }    
                     else {
                       if (ret == 0) {
			 if (is_pscro) {
			   //page_scroll(0,0);
			 }
			 else {
			   if (is_gadg) {	/* setzt die Grenzen richtig */
			     window_plus_sbar_move(0,0);
			   }
			   else {
			     window_move(0,0);
			   }
			 }
                       } /* setzt auch die Scrollbars richtig */
                     }
		   }
		   else {
		     /* linke Mausetaste wird bisher nicht benoetigt */
		     /* (oder rechte Mausetaste, falls etwas schiefgegangen ist) */

#if 0		     
		     if (is_tmpmargin) {
		       // wir warten auf den Klick um den Rand zu setzen
		       // widthin : inch * 1000 (Groesse inklusive des Offsets)
		       long xpos, ypos;
		       long widthin  = (long)(((float)pwidth_pt  / 72.27 + ((float)SAVETY_BITS_X / (float)hconvresolution) * 1000.0 / (float)mag) * 1000.0 + 0.5);
		       long heightin = (long)(((float)pheight_pt / 72.27 + ((float)SAVETY_BITS_Y / (float)vconvresolution) * 1000.0 / (float)mag) * 1000.0 + 0.5);

		       if (MouseX < win2->BorderLeft) MouseX = win2->BorderLeft;
		       if (MouseY < win2->BorderTop)  MouseY = win2->BorderTop;
		       xpos = MouseX - win2->BorderLeft + static_x_Koo;
		       ypos = MouseY - win2->BorderTop + static_y_Koo;

		       show_state.margin_x = xpos * widthin / wx; 
		       show_state.margin_y = ypos * heightin / wy;

		       clear_pointer();
		       unset_tmpmargin;
		       set_margin;
		       
		       Message(MSG_MARGIN_SET);
		     }
		     else {
		       if (is_messwin) {
		         WorkMessWin(MouseX, MouseY, Code == SELECTDOWN, Code == SELECTUP);
		       }
		     }
#endif
		     if (is_messwin) {
		       WorkMessWin(MouseX, MouseY, Code == SELECTDOWN, Code == SELECTUP);
		     }
		   }
                   break;

          case IDCMP_GADGETDOWN: 
		   current_gadget = m_gad->GadgetID;
		   ex = down_gad(current_gadget, m_gad, secs, mics);
		   if (current_gadget == POTX_GAD_NR || current_gadget == POTY_GAD_NR
				|| current_gadget == PG_POT_GAD_NR) {
		     /* das haette eigentlich mit FOLLOWMOUSE funktionieren muessen!! 	*/
		     /* Es geht inzwischen auch...ich bekomme MOUSEMOVE Events... 	*/
		     /* wohl aber irgendwie zu wenige...also mach ich trotzdem noch 	*/
		     /* ein paar Intui-Ticks dazu :) 					*/
		     MyModifyIDCMP(IDCMP_MOUSEMOVE | IDCMP_INTUITICKS, 0L);
		     add_mmove;
		     add_inticks;	/* zur besseren Ansprechung auf Mausbewegungen */
		   }
		   else {
		     current_gadget = -1;	/* alle Gadgets die kein FOLLOWMOUSE brauchen */
		   }

		   /* Falls Doppelklick auf einen der Pfeiltasten */
        	   if (ex == KOMM-1) {
        	     set_jmpdown;
        	   }
        	   else if (ex == KOMM+1) {
        	   	  set_jmpup;
        	   }
	 	   break;

          case IDCMP_GADGETUP: 
		   if (current_gadget == POTX_GAD_NR || current_gadget == POTY_GAD_NR ||
			current_gadget == PG_POT_GAD_NR) {
		     /* das haette eigentlich mit FOLLOWMOUSE funktionieren muessen!! */
		     /* tut's ja im Prinzip auch */
		     sub_mmove;
		     if (!is_mmove) {
		       MyModifyIDCMP(0L, IDCMP_MOUSEMOVE);
		     }
		     sub_inticks;
		     if (!is_inticks) {
		       MyModifyIDCMP(0L, IDCMP_INTUITICKS);
		     }
		   }
                   current_gadget = -1;
		   ex = check_gad(m_gad);
                   break;

	  case IDCMP_MOUSEMOVE: 
		   is_mouse_moved = TRUE;
		   if (current_gadget != -1) {
		     follow_pot_gad(current_gadget);
		   }
		   else {
		     if (MessWin != NULL) {
		       WorkMessWin(MouseX, MouseY, FALSE, FALSE);
		     }
		   }
  		   break;

          case IDCMP_CLOSEWINDOW:
        	   if (real_prog_end()) {
        	     Enable_Abort = 0;
                     ex = KOMM+10L; 
        	   }
                   break;

	  case IDCMP_CHANGEWINDOW:
	  	   if (win2->LeftEdge != old_win_pos_x) {
	  	     if (is_ownscr) {
	  	       show_state.window_pos_own_scr_x = win2->LeftEdge;
	  	     }
	  	     else {
	  	       show_state.window_pos_x = win2->LeftEdge;
	  	     }
	  	   }
	  	   if (win2->TopEdge != old_win_pos_y) {
	  	     if (is_ownscr) {
	  	       show_state.window_pos_own_scr_y = win2->TopEdge;
	  	     }
	  	     else {
	  	       show_state.window_pos_y = win2->TopEdge;
	  	     }
	  	   }
		   break;

          case IDCMP_NEWSIZE:
          	   set_i_window_size();
          	   init_gad();
          	   if (is_show){
          	     show_full_page(TRUE); 	/* ausschalten */
          	     show_full_page(TRUE); 	/* einschalten ohne Refresh (RefWin kommt noch) */
          	     //show_full_page(FALSE); 	/* einschalten mit Refresh */
          	     
          	     display_full_page();      	     // Seite einkopieren...
          	   }
          	   else {
#if 0
		     const long winiw = win2->Width - win2->BorderLeft - win2->BorderRight;
		     const long winih = win2->Height - win2->BorderTop - win2->BorderBottom;
#endif

		     if (is_gadg) {		/* setzt die Grenzen richtig */
		       window_plus_sbar_move(0,0);
          	       RefreshGadgets(&potx_left_Gad, win2, NULL); /* Refresh bis zum Ende!! */
		     }
		     else {
		      window_move(0,0);
		     }

		     // Loesche Window-Inneres, falls neu zentriert werden muss
		     if (winiw > wx || winih > wy) {
		       fill_block(win2->BorderLeft, win2->BorderTop, winiw-1, winih-1, 0);
		     }

		     window_show();
          	   }
          	   /* speichere neue Groesse ab */
          	   if (is_ownscr) {
          	     show_state.window_size_own_scr_x = win2->Width;
          	     show_state.window_size_own_scr_y = win2->Height;
          	   }
          	   else {
          	     show_state.window_size_x = win2->Width;
          	     show_state.window_size_y = win2->Height;
          	   }
          	   
          	   /* Puffer-RastPort wieder auf den neuesten Stand bringen */
          	   if (is_bmcpu) {
          	     FreeChipPuffRP();
          	     AllocChipPuffRP();
          	   }
          	   
          	   write_status();
		   break;

	  case IDCMP_REFRESHWINDOW:
          	   BeginRefresh(win2);
          	   if (is_show) {
		     /* und nun wird die Bitmap einkopiert... */
          	     display_full_page();      	     // Seite einkopieren...
          	   }
          	   else {
          	     window_show();			/* nur einfach neu anzigen */
          	   }
          	   EndRefresh(win2, TRUE);
		   break;

          case IDCMP_INACTIVEWINDOW:
          	   if (is_messwin) {
		     WorkMessWin(MouseX, MouseY, FALSE, TRUE);	// Maus hoch
		   }
		   break;

          case IDCMP_ACTIVEWINDOW:
		   write_status();
		   if (is_autoag && !is_notify && is_dvif && is_newer_dvi_file()) {
		     /* than: load file again */
		     OpenNewDVI(filename, FALSE);
		     ex = KOMM + 3;
		   }
		   if (is_titref) {
		     ULONG lck;		/* zur Sicherheit nochmal ein test, ob das Window (immer noch) active ist */
		     lck = LockIBase(0L);
		     if (IntuitionBase->ActiveWindow == win2) {
		       UnlockIBase(lck);
		       MessageStr(NULL);		/* refresh title */
		       unset_titref;
		     }
		     else {
		       UnlockIBase(lck);
		     }
		   }
		   break;

	  case IDCMP_MENUPICK:
	  	   if (!is_osmenu) {
	  	     /* Um AMIGA-A zu erlauben, wurde VERIFY weggemacht. Nun muss es wieder hin. */
	  	     MyModifyIDCMP(IDCMP_MENUVERIFY, 0);	/* MENUVERIFY wieder hin */
	  	   }

		   if (is_messwin) {
		     WorkMessWin(MouseX, MouseY, FALSE, TRUE);	// Maus hoch
		   }

		   ex = KOMM + 5;
		   sel = Code;
		   
		   IsScreenReOpend = FALSE;

                   /* Reply muss vor work_with.. wg. toggle-lace */
		   while (sel != MENUNULL && !IsScreenReOpend) {
		     // sel != 0 ... es gibt kein Menu 0,0,0  !!
		     // aber wird schon mit IsScreenReOpend abgefangen...
		     ex = work_with_os_menu(MENUNUM(sel), ITEMNUM(sel), SUBNUM(sel));
		     if (!IsScreenReOpend) {
		       ItemAdr = ItemAddress(win2->MenuStrip, sel);
		       sel = ItemAdr->NextSelect;
		     }
		   }

		   if (ex == KOMM + 5) {
		     ex = 0;
		   }
		   break;

	  case IDCMP_MENUHELP:
	  	   sel = Code;
	  	   if (is_amigaguide) {
	  	     /* AmigaGuide Hilfe (os3) */
	  	     ItemAdr = ItemAddress (win2->MenuStrip, Code);
	  	     if (ItemAdr) {
                       do_menu_help(sel, ItemAdr->Flags & CHECKED, ItemAdr->Flags & ITEMENABLED, !(ItemAdr->Flags & ITEMTEXT));
	  	     }
	  	     else {
                       do_menu_help(sel, 0, 0, 0);
	  	     }
#if 0
		     if (ItemAdr) {
		       /* Get the context ID */
		       long id = (LONG) MENU_USERDATA (ItemAdr);

		       /* Adjust for being selected */
		       if (ItemAdr->Flags & CHECKED) id++;

                       do_menu_help(sel, ItemAdr->Flags & CHECKED);
  		     }
#endif
	  	   }
	  	   else {
	  	     /* normale Requester-Hilfe */
  	  	     work_with_os_help_menu(MENUNUM(sel), ITEMNUM(sel), SUBNUM(sel));
  	  	   }
		   break;

  	  case IDCMP_GADGETHELP:
		   work_with_gadgethelp(&copy_msg);
		   break;

          case IDCMP_RAWKEY:
		   ex = work_with_raw_key(Code, Qualifier, MouseX, MouseY);
		   if (MessWin != NULL) WorkMessWin(MouseX, MouseY, FALSE, FALSE);
                   break;
	  case IDCMP_IDCMPUPDATE:
	     	   // Bisher hibt es nur das Boopsi Gad links unten das diese Message erzeugt
	           if (Qualifier == 0) {
	             // Test--quasi auf Gadgetup..will hier nicht forlaufend neue Seiten aufbauen
	             if (Code != current_page_phy) {
	               // dann springe auf diese (phy) Seite
	               set_tusephy;	/* temp use of physical number */
		       ex = Code;	/* kein 0 Hack...da immer phy Nummer */
	             }
	           }
		   break;
          default:
		   FatalStr(20, "Unknown Intui-Message %d/%d\n", msg_Class, Code);
		   break;
     } 	/* switch */
     

     // ACHTUNG: Darf *NICHT* nach hinten verlegt werden, da ja moeglicherweise das Window geschlossen wird.
     // ReplyMsg(&(msg->ExecMessage));	// ACHTUNG: ist nach hinten verlagert


     if (is_print && ex != 0) {
       Message(MSG_NOT_WHILE_PRINTING);
       beep();
       ex = 0;
     }
  }

  if (is_mouse_moved) {
    if (current_gadget != -1) {
      follow_pot_gad(current_gadget);
    }
    if (MessWin != NULL) {
      WorkMessWin(MouseX, MouseY, FALSE, FALSE);
    }
  }

  
  return ex;		/* ex == -1 => go to page 0 */
}

/*
 ***********************************************************************
 *	M A I N  -  F u n k t i o n	--->	 W A I T     <----
 ***********************************************************************
 */
static long scroll(void)
{
  register long ex;
  unsigned long signals;
  unsigned long ret_sigs;
  /* struct MsgPort	*mp; fuer das alte Timer Zeuks... */

  clear_pointer();
  
  /* nun muessten alle is_* richtig gesetzt sein, also update des Menues */
  set_checked_os_menu();

  /* und nun endlich: refresh des ganzen */
  if (is_gadg) RefreshGadgets(&potx_Gad,win2,NULL);  /* erstes Gadget in der Liste (von potx und poty :) */


  /* Setze IDCMP_MENUVERIFY falls pop_up gesetzt ist */
  if (!is_osmenu) {
    MyModifyIDCMP(IDCMP_MENUVERIFY, 0);
  }


  /* zuerst ueberall Mal schaun, ob was noch aussteht */
#if defined(APP_WINDOW)
  ex = work_with_app_win();
#else
  ex = work_with_app_icon();
#endif

  /* intui_message muss recht frue abgefragt werden (wg. MENUVERIFY) !! */
  if (ex == 0L) {
    ex = intui_message();		/* schau schon mal nach Messages */
  }
  if (ex == 0L && MainAppWin) {		/* ist win2 ein AppWin */
    ex = work_with_main_app_win();
  }
  if (ex == 0L) {
      /* ok("Rexx Message..."); */
      ex = dispRexxPort();				/* REXX */
  }

  /* Schleife ueber Messages innerhalb einer Seite */
  while (ex == 0L) {
  
    signals = ((is_print) ? SIG_PRINT : 0L)			|
    	      ((is_appwin) ? SIG_APP | SIG_APPWIN : 0L)		|
    	      ((is_about) ? SIG_ABOUTWIN : 0L)			|
    	      ((MessWin != NULL) ? SIG_MESSWIN : 0L)		|
    	      ((SearchWin) ? SIG_SEARCHWIN : 0L)		|
    	      ((is_prefwin) ? SIG_PREFWIN : 0L)			|
    	      ((is_notify) ? SIG_NOTIFY : 0L)			|
    	      ((MainAppWin) ? SIG_MAINAPPWIN : 0L)		|
    	      ((is_amigaguide) ? SIG_AMIGAGUIDE : 0L)		|
              SIGBREAKF_CTRL_C					|
              SIGBREAKF_CTRL_E					|
              SIGBREAKF_CTRL_F					|
              SIG_TIMER						|
              SIG_REXX						|
              SIG_WIN;

    //((is_os2 && is_myscr) ? SIG_SCREENCLOSE : 0L)		|

    PROFILE_OFF();
    ret_sigs = Wait(signals);		/*------- W A I T -------*/
    PROFILE_ON();

    if (Enable_Abort && (ret_sigs & SIGBREAKF_CTRL_C)) {	/* CTR-C */
      if (can_i_exit()) {
        if (!is_osmenu) MyModifyIDCMP(0, IDCMP_MENUVERIFY);
	if (is_print) { 
	  abort_print_page();
	}
	CXBRK();		/* lattice ^C funktion */
      }
    }

    if (ret_sigs & SIG_REXX) {
      /* ok("Rexx Message..."); */
      ex = dispRexxPort();				/* REXX */
    }

    if (ret_sigs & SIGBREAKF_CTRL_E) {			/* CTR-E */
      WindowToFront(win2);
      ScreenToFront(screen);
      make_show_active();
    }

    if (ret_sigs & SIGBREAKF_CTRL_F) {			/* CTR-F */
      WindowToFront(win2);
      ScreenToFront(screen);
      make_show_active();
      if (access(filename,4) == 0) {
        char *p = strrchr(filename,'.');
        if (p != NULL && stricmp(p,".dvi") == 0 && is_dvif) {	/* nur is_dvif ?? */
          OpenNewDVI(filename, FALSE);
          ex = KOMM + 3;		/* neues File, Name nicht geaendert. */
        }
      }
    }

    if (is_print && (ret_sigs & SIG_PRINT)) {			/* PRINT */
      check_print_page();
      MessageStr(NULL);
    }

    if (ex == 0 && (is_notify &&(ret_sigs & SIG_NOTIFY))) {	/* Notify Signal */
      if (is_newer_dvi_file()) {		/* das File muss sich auch wirklich geaendert haben */
        if (access(filename,4) == 0) {		/* es koennte ja sein, dass es bereits neu geladen wurde */
          char *p = strrchr(filename,'.');
          if (p != NULL && stricmp(p,".dvi") == 0 && is_dvif) {	/* nur is_dvif ?? */
            BPTR lock;
            lock = Lock(filename, ACCESS_READ);	/* Hoffnung, damit zu verhindern, dass mir jemand das File wieder wegschnappt... */
            if (lock != NULL) {
              OpenNewDVI(filename, FALSE);
              UnLock(lock);
              ex = KOMM + 3;		/* neues File, Name nicht geaendert. */
            }
          }
        }
      }
    }
    

    if (ret_sigs && SIG_SCREENCLOSE) {
      // wuesste nicht, was hier nun zu tun waere
      // ich will den Screen nun mal niht schliessen!! :-)
      //printf("Debug: SIG_SCREENCLOSE arrived!\nsig: %ld\nSIG: %lx\nret_sigs: %lx", sig_screenclose, SIG_SCREENCLOSE, ret_sigs);
      // iwrd staendig aufgerufen...????
    }
    
    if (ex == 0 && (ret_sigs & SIG_AMIGAGUIDE)) {
      ex = work_with_help();
    }


    if (ex == 0 && (MainAppWin && (ret_sigs & SIG_MAINAPPWIN))) {
      ex = work_with_main_app_win();
    }


    if (ex == 0 && (is_about && (ret_sigs & SIG_ABOUTWIN))) {	/* About-Window */
      ex = AboutWinMsg();
    }
    
    if (ex == 0 && (ret_sigs & (SIG_APP | SIG_APPWIN))) {	/* WB-App-Win */
#if defined(APP_WINDOW)
      ex = work_with_app_win();
#else
      ex = work_with_app_icon();
#endif
    }

    if (ex == 0 && is_messwin && (ret_sigs & SIG_MESSWIN)) {	/* Messuring-Window */
      /* die is_messwin Abfrage *muss* sein, da */
      /* sonst das SIG_ nicht gueltig ist! */
      ex = MessWinMsg();
    }
    
    if (ex == 0 && is_prefwin && (ret_sigs & SIG_PREFWIN)) {	/* Preference-Window */
      ex = DoPrefWin();
    }
    
    if (ex == 0 && SearchWin && (ret_sigs & SIG_SEARCHWIN)) {	/* Preference-Window */
      char SBuf[100] = "";
      enum SearchActions sr;
      if (InSearchMode) {
        sr = DoSearchWin(NULL, 0);
      }
      else {
        sr = DoSearchWin(SBuf, sizeof(SBuf)-1);
      }
      if (sr == SEARCHWIN_ACTION_SEARCH) {
        ex = KOMM + StartSearch(SO_StartSearchAgain);
      }
    }
    
    if (ex == 0 && (ret_sigs & SIG_WIN)) {			/* INTUI - IDCMP */
      ex = intui_message();
    }

  }	/* end while (ex == 0 ) */


	/* ret == -1        go to page 0 !!	*/
	/* ret <  KOMM -  2 ret = Seite		*/
	/* ret == KOMM +  5 no action		*/
	/* ret == KOMM -  1 Seite zurueck	*/
	/* ret == KOMM +  1 Seite vor		*/
	/* ret == KOMM -  2 Textanfang		*/
	/* ret == KOMM +  2 Textende		*/
	/* ret == KOMM + 10 Programmende	*/
	/* ret == KOMM +  3 selbes File nochm.	*/
	/* ret == KOMM +  4 neues DVI-F.	*/


  /* Loesche IDCMP_MENUVERIFY (darf nicht so lange gesetzt sein) */
  if (!is_osmenu) {
    MyModifyIDCMP(0, IDCMP_MENUVERIFY);
  }


  if (ex == KOMM+10L) { 	/* Programmende ? */
    Enable_Abort = 0;		/* nur zur Sicherheit! Nun kein ^C mehr */
    if (is_print) { 
      abort_print_page();
    }
  }
  
  /* Wenn neues File geladen wird, dann werden die derzeitigen Raender ungueltig */
  if (ex == KOMM+4) {	// neues DVI-File
    unset_margin;
    show_state.margin_x = 0;
    show_state.margin_y = 0;
  }

  sleep_pointer();
  return ex;
}
/*******************************************************************/


void clear_bild(void)			/* KEIN static */
{
  // Im Such-Modus wird nicht wirklich ein neues Bild aufgebaut
  if (InSearchMode) return;

  if (is_bmfast) {
    long len = (wx+7)/8*wy;
    long pad = len & 3;
    long i;
    long * ptr = (long *)myRastPort.BitMap->Planes[0];
 
    D(bug("ClearBild: Länge: %ld, pad: %ld\n", len, pad));

    for (i=0; i<len/4; i++) *(ptr + i) = 0L;
    if (pad > 0) memset(ptr+i, 0, pad);
  }
  else {
    SetRast(&myRastPort,0L);
  }

  if ((!is_bmfast || is_village) && !is_cyber) {
    /* und nun einen Rand zeichnen */
    MySetABPenDrMd(&myRastPort, show_state.APen, show_state.APen, JAM1);
    SetDrPt(&myRastPort, 0xFFFF);	// gfxmacros.h

    Move(&myRastPort, 0,    0);
    Draw(&myRastPort, wx-1, 0);
    Draw(&myRastPort, wx-1, wy-1);
    Draw(&myRastPort, 0,    wy-1);
    Draw(&myRastPort, 0,    0);
    
    D(bug("ClearBild: Move/Draw\n"));
  }
  else {
    register int i;
    char * ptr = (char *)(myRastPort.BitMap->Planes[0]);
    long rlen  = myRastPort.BitMap->BytesPerRow;

    for (i=0; i<wy; i++) {
      *(ptr + i     * myRastPort.BitMap->BytesPerRow)    = 0x80;
      *(ptr + (i+1) * myRastPort.BitMap->BytesPerRow -1) = 0x01;
    }
    for (i=0; i<rlen; i++) {
      *(ptr             + i) = 0xFF;
      *(ptr+(wy-1)*rlen + i) = 0xFF;
    }

    D(bug("ClearBild: handmade\n"));
  }
}


/* saves pointer to current active window */
static void save_active_window(void)
{
  oldactiveWindow = IntuitionBase->ActiveWindow;
}

/* make old window active again */
void make_old_active(void)
{
  /* das ist etwas buggy...vielleicht sollte ich diese Funktion ganz herausnehmen?? */
  /* Ich fummle bei einem nicht mir gehoerenden Window rum :-(( */
  struct Window * fwin;
  struct Screen * fscr;
  int found = FALSE;

  if (oldactiveWindow != NULL) {
    ULONG lck;
    lck = LockIBase(0L);
    fscr = IntuitionBase->FirstScreen;
    while (fscr && !found) {
      fwin = fscr->FirstWindow;
      while (fwin && !found) {
        if (fwin == oldactiveWindow) {
          found = TRUE;
        }
        else {
          fwin = fwin->NextWindow;
        }
      }
      if (!found) fscr = fscr->NextScreen;
    }
    Forbid();
    UnlockIBase(lck);
    if (found) ActivateWindow(oldactiveWindow);
    Permit();
  }
}

/* make showdvi active and saves old active window */
void make_show_active(void)
{
  save_active_window();
  if (win2 != NULL) ActivateWindow(win2);
}


/* ARexx  Dispatcher */
static /*__stdargs*/ long arexx_disp(struct RexxMsg *msg, struct rexxCommandList *dat, char *p)
{
  long (*userfunc)(struct RexxMsg *, char *);
  long ret = 0;

  if (dat->userdata != NULL) {
    userfunc = (long (*)(struct RexxMsg *, char *))dat->userdata;
    ret = userfunc(msg, p);
    return ret;
  }

  replyRexxCmd(msg, 20L, 10L, NULL);

  return ret;
}


/* exit-proc ***********
int exit_procedure(int ret)
{
  close_all_bild();
  return ret;
}
***********************/


/*********************************************************/
/* SCHNITTSTELLE zu den anderen Modulen :                */


void beep(void)
{
  if (screen != NULL && is_beep) {
    DisplayBeep(screen);
  }
}


/* return: print help? */
int init_task_name(int argc, char **argv)
{
  struct Process *my_proc;
  int rexx_result;
  char rexx_command[150];

#ifndef BACKGROUND
  struct CommandLineInterface *cli;
  /* long stack_size; */
#endif

  /*====*/
  ArpBase = NULL;		
  unset_arp;			/* default: no ARP-library found		*/
  OpenLib(); 			/* open Intuition,Gfx... and Arp libraries	*/

  font8 = font11 = NULL;	/* init Pointers 			*/

  BitMapFullPage.Planes[0]=NULL;/* init Full-Page bitmap!!!		*/

  timer_wait_ticks = 0;		/* init INTUI TICK counter		*/
  /*====*/


#ifndef BACKGROUND
  // feststellen, ob mit run gestartet
  background = FALSE;  
  my_proc =(struct Process *)FindTask(NULL);
  if (my_proc != NULL /*&& my_proc->pr_TaskNum != 0*/) {	/* cli-process */
    cli = (struct CommandLineInterface *)(my_proc->pr_CLI << 2);
    if (cli != NULL) {
      /* stack_size = cli->cli_DefaultStack << 2; */		/* in bytes */
      background = (cli->cli_Background != DOSFALSE);
    }
  }
#endif


  if ((my_proc = (struct Process *)FindTask(task_name)) != NULL) {
    // es laeuft schon ein ShowDVI :: nun estmal die Optionen parsen
    char * fptr;

    fptr = DecodeArgs( argc, argv );

    Signal(&(my_proc->pr_Task),SIGBREAKF_CTRL_E);	/* screen to front */
   
    if (fptr) {
      // Filename wurde uebergeben 
      BPTR lck;
      char fpuf[200], * ptr;
      
      strncpy(fpuf, fptr, sizeof(fpuf)-6);
      ptr = strrchr(fpuf, '.');

      // in fpuf den Namen etwas verschoenern:
      // - ein .tex durch ein .dvi ersetzen
      // - falls keine Extension ein .dvi anhaengen (falls das File dann existiert)
      // Danach dann noch den kompletten Pfad daraus machen, da das erste ShowDVI 
      // vermutlich aus einem anderen Dir. aufgerufen wurde.

      if (!ptr) {      
        // gar keine Extension
        ptr = fpuf + strlen(fpuf);	// zeigt auf das Null-Byte
        strcpy(ptr, ".dvi");
        lck = Lock(fpuf, ACCESS_READ);
        if (!lck) {
          *ptr = '\0';	// mit .dvi gibt es das File nicht, also wieder weg damit
        }
        else {
          UnLock(lck);
          lck = NULL;
        }
      }
      else {
        if (!stricmp(ptr, ".tex")) {
          // .tex --> ersetzen
          strcpy(ptr, ".dvi");
        }
      }
      
      lck = Lock(fpuf, ACCESS_READ);
      if (lck) {
        if (!NameFromLock(lck, fpuf, sizeof(fpuf)-1)) {
          // wuaehh...nun mag ich nicht mehr
          strncpy(fpuf, fptr, sizeof(fpuf)-6);
        }
        UnLock(lck);
        lck = NULL;
      }
      
      sprintf(rexx_command, "address \"showdvi\" \"loadnew %s\"", fpuf);

      if (!call_rexx(rexx_command, &rexx_result)) {
        Warning(MSG_AREXX_COMM_START_FAILED);
        AbortRun(5);					/* end of programm */
      }
      else {
        if (rexx_result != 0) {
          Warning(MSG_AREXX_COMM_FAILED, rexx_command, rexx_result);
          AbortRun(5);					/* end of programm */
        }
      }
    }

    AbortRun(0);					/* end of programm */
  }


  my_proc =(struct Process *)FindTask(NULL);
  if (my_proc != NULL) {
    old_task_name = my_proc->pr_Task.tc_Node.ln_Name;	/* save task name */
    my_proc->pr_Task.tc_Node.ln_Name = task_name;	/* change task name */
  }

#ifdef CATCH
  _ONGURU = (void *)onGURU;
#endif


#ifdef CATCH_ZERO
  (void)signal( SIGFPE ,(void (*)(int))sighandling);
#endif

  return 0;	/* keine Hilfe...alles geht normal weiter */
}


static void sighandling(int i)
{
  Fatal(20,MSG_DIV_ZERO);
}

#ifdef CATCH
static void onGURU(void)
{
  close_all_bild();
  if (g_logfp != NULL) {
    fclose(g_logfp);
  }
}
#endif



void SetShowDVIVars(char *name)
{
  char *f, *d;
  int len;

  if (is_os2) {
    if (!is_dir(name)) {
      if ((f = strrchr(name, '/')) == NULL) {
        f = strchr(name, ':');
        if (f == NULL) f = name;
      }

      /* *ohne* .dvi Endung!! */
      if (!SetVar("ShowDVI-file", f+1, strlen(f+1)-4, GVF_LOCAL_ONLY)) {
        Warning(MSG_CANT_SET_VARIABLE, "ShowDVI-file");
      }

      if (f == NULL) {
        d = "";
        len = 0;
      }
      else {
        d = name;
        len = f - d + 1;
      }
      if (!SetVar("ShowDVI-dir", d, len, GVF_LOCAL_ONLY)) {
        Warning(MSG_CANT_SET_VARIABLE, "ShowDVI-dir");
      }
    }
  }
}

static void DeleteShowDVIVars(void)
{
  if (is_os2) {
    DeleteVar("ShowDVI-file", GVF_LOCAL_ONLY);
    DeleteVar("ShowDVI-dir", GVF_LOCAL_ONLY);
  }
}



char *GetCurrentPubScr(void)
{
  if (is_ownscr) {
    return &(MyPubScreenName[0]);
  }
  else {
    return &(PubScreenName[0]);
  }
}



void Init_ShowDVI(void)		/* VERY first procedure!! */
{
#if 0
  /* das muss schon in init_task_name gemacht werden, sonst Aerger bei doppel-Start!! */
  ArpBase = NULL;		
  unset_arp;			/* default: no ARP-library found		*/
  OpenLib(); 			/* open Intuition,Gfx... and Arp libraries	*/

  font8 = font11 = NULL;	/* init Pointers 			*/

  BitMapFullPage.Planes[0]=NULL;/* init Full-Page bitmap!!!		*/

  timer_wait_ticks = 0;		/* init INTUI TICK counter		*/
#endif

  if (SysBase->LibNode.lib_Version < 37) exit(20);

  set_lace;			/* default: interlace			*/
  set_gadg;			/* default: scrollbars			*/
  unset_show;			/* default: not in full-screen modus	*/
  unset_timer;			/* default: no timer request is running	*/
  unset_pscro;			/* default: not in page-scroll modus	*/
  unset_colre;			/* default: no color-req. on the screen	*/
  unset_about;			/* default: no about-req. on the screen */
  unset_dvif;			/* default: no DVI-File loadet jet	*/
  unset_ndvif;			/* default: no new DVI-File loadet jet	*/
  set_beep;			/* default: beep on warnings		*/
  unset_escex;			/* default: no exit on ESC		*/
  set_imenu;			/* default: intelligent menu! :-)	*/
  set_bmenu;			/* default: big (topaz 11) menu		*/
  set_unit_in;			/* default: used unit = inch		*/
  unset_mesu;			/* default: no mesuring			*/
  unset_jmpup;			/* default: jump not to top of page	*/
  unset_jmpdown;		/* default: jump not to bottom of page	*/
  undef_scrmode;		/* default: no 2.0 screen mode		*/
  set_osmenu;			/* default: use pull-down menu		*/
  unset_autoag;			/* default: no auto load again		*/
  unset_notify;			/* default: no notify mechanism		*/
  unset_messwin;		/* default: no mess-window open		*/
  clear_mmove;			/* default: no MOUSEMOVE IDCMP flag	*/
  set_col4;			/* default: use 4 color screen		*/
  set_clwbcol;			/* default: clone the wb colors		*/
  unset_midmenu;		/* default: don't use middle pop-up menu*/
  unset_titref;			/* default: no title to refresh		*/
  unset_quickex;		/* default: no immediately exit		*/
  unset_chres;			/* default: resolution is not changed	*/
  unset_dotbord;		/* default: no border line		*/
  unset_prefwin;		/* default: no preference window up	*/
  set_ownscr;			/* default: use own screen		*/
  unset_myscr;			/* default: screen is not (jet) my scr  */
  unset_village;		/* default: we are not on a vga screen	*/
  unset_scrollras;		/* default: use ClipBlit()		*/
  unset_bmfast;			/* default: bitmap in chip ram		*/
  unset_alwbmfast;		/* default: use Chip-RAM		*/
  unset_bmcpu;			/* default: use blitter for bm copy	*/
  set_smartwin;			/* default: use smart-refresh window	*/
  unset_margin;			/* default: no margins set		*/
  unset_amigaguide;		/* default: AmigaGuide ist nicht bereit */
  unset_bhook;			/* default: no Backfill Hook            */


  set_wbmode;			// DisplayID == Workbench
  
  show_state.APen = 1;		/* black on white */
  show_state.BPen = 2;
  
  strcpy(PubScreenName, "Workbench");
  strcpy(MyPubScreenName, "ShowDVI-PubScr");
  
  show_state.ChipPuffBM = NULL; /* ungleich NULL wenn is_bmcpu */
  InitRastPort(&show_state.ChipPuffRP);


  if (is_os2) {
    set_appwin;			/* default: under >= 2.0: app-window!	*/
    set_pubscr;			/* default: under >= 2.0: public-screen	*/
  }
  else {
    unset_appwin;		/* default: under < 2.0: no app-window	*/
    unset_pubscr;
  }

  app_icon_name = DEFAULT_APP_ICON_NAME;
  show_state.AppIconX = 0;	/* default: no specific AppIcon position*/
  show_state.AppIconY = 0;

  (void)clear_counter(0);	/* set the page-jump-counter to '-1'	*/

  /* set default screen size. 0 0 => clone workbench screen */
  show_state.screen_size_x = 0;
  show_state.screen_size_y = 0;

  /* set default window pos/size, -1 -1 is the default */
  show_state.window_size_x = 600;
  show_state.window_size_y = 180;
  show_state.window_pos_x = 0;
  show_state.window_pos_y = 0;

  show_state.window_size_own_scr_x = -1;
  show_state.window_size_own_scr_y = -1;
  show_state.window_pos_own_scr_x = -1;
  show_state.window_pos_own_scr_y = -1;

  show_state.menu_res_lines = 5;/* default: nr. of new res-menu lines	*/
  show_state.mres[0] = 120;
  show_state.mres[1] = 100;
  show_state.mres[2] =  91;
  show_state.mres[3] =  83;
  show_state.mres[4] =  44;

  /* default aspect-ratio (640x400) */
  show_state.monitor_size_x = 4;
  show_state.monitor_size_y = 3;
  
  /* default margins: no margins */
  show_state.margin_x = 0;
  show_state.margin_y = 0;
  

  /* Setzen der aktuellen (default) Farbwerte */
  init_screen_colors();

  /* Wie werden die Seitennummern interpretiert */
  unset_usephy;			/* use logical numbers by default	*/
  unset_tusephy;		/* no tmp physical numbers (!pscro)	*/
  
  /* default command for the 'r' command */
  show_state.command = "TeX:s/ShowDVI.sh";
  
  /* default script for the start of the TeX-server */
  show_state.arexxstart = "rx TeX-server.rexx";


  /* ###############  read configuration-file  ################# */
  if (!read_config_file()) {
    Warning(MSG_ERROR_IN_CONFIG_FILE);
  }

  /* Ueberschreibe lace bei wbmode || bei is_numeric */
  if (is_wbmode || is_numeric) {
    long mode_lace = FALSE;
    ULONG id;
    
    if (is_wbmode) id = GetWBModeID();
    else id = show_state.DisplayID;		// is_numeric

    (void)GetModeIDName(id, &mode_lace);
    
    if (mode_lace) set_lace;
    else unset_lace;
  }
  
  if (is_clwbcol) {
    /* nun die Farben nochmal setzen */
    init_screen_colors();
  }

  if (!is_os2) {
    unset_appwin;		/* < 2.0 => no app-win available	*/
  }
  if (!is_autoag) {
    unset_notify;		/* notify only if auto-again is active */
  }
  
  if (!is_ownscr) {
    show_state.screen_size_x = show_state.screen_size_y = 0;
  }
  
  InitHelpFirst();
}



void Open_ShowDVI(int isDVIfile)	/* !!!first procedure!!! */
  /* isDVIfile: BOOL gibt an, ob gleich am Anfang ein DVI-File geladen wird */
{
  long i;
  
  // wenn ein PubScreen Name als Arg uebergeben wurde, dann nimm diesen
  if (ArgPubname) {  
    struct Screen * pscr = LockPubScreen(ArgPubname);
    if (pscr) {
      unset_ownscr;
      strncpy(PubScreenName, ArgPubname, sizeof(PubScreenName)-1);
      UnlockPubScreen(NULL, pscr);
    }
    else {
      set_ownscr;
      strncpy(MyPubScreenName, ArgPubname, sizeof(MyPubScreenName)-1);
    }
  }

  if (isDVIfile) {
    set_dvif;		/* das wird zwar auch noch mal in showdvi.c/ReadFile() gemacht... */
  }			/* aber hier ist's irgendwie sinnvoller */
  else {
    unset_dvif;
  }

  for (i=0; i < show_state.menu_res_lines && show_state.mres[i] != resolution; i++);
  if (i == show_state.menu_res_lines) {
    show_state.mres[i] = resolution;
    show_state.menu_res_lines++;
  }

  wx = wy = 0;
  set_resolution();		/* initialize the size of screen/wind.	*/
  
  save_active_window();		/* saves current active window		*/

  open_fonts();			/* open the fonts topaz 8 and topaz 11	*/
  init_menu();			/* alloc memory for the menu		*/
  InitFileReq();		/* initialize filerequest structure	*/
  OpenS();			/* open screen				*/

  OpenWin2();			/* open display window			*/

  // 0 Bytes, wenn hgrp auf -1 ist (AbortRun)

  set_i_window_size();
  wx = x_win_i_width;		/* Vorabdefinition fuer init_gad	*/
  wy = x_win_i_height;

  sleep_pointer();		/* set mouse to zz			*/

  init_gad();			/* set sizes and locations of gadgets	*/

  Add_system_gadgets();		/* add +,++,-,... gadgets 		*/
  if (is_gadg) {
    Add_scroll_gadgets(FALSE);	/* add scrollbars with refresh		*/
  }
  
  set_i_window_size();
  wx = x_win_i_width;		/* nun zuruecksetzen			*/
  wy = x_win_i_height;

  // 264 Bytes, wenn hgrp=-1 (AbortRun)
  //AbortRun(0);
  
  // ########### refresh_screen();		/* refrsh screen (window)		*/
  
  if (is_appwin) {
#if defined(APP_WINDOW)
    if (!setup_app_win(&sig_app, &sig_appwin)) {
      unset_appwin;
    }
#else
    if (!setup_app_icon(&sig_app)) {
      sig_appwin = 0;	/* koennte man auch loeschen */
      unset_appwin;
    }
#endif
  }

#if 0
  TimeRequest = CreateTimer();	/* open timerdevice			*/
  if (TimeRequest == NULL) {
    Warning("Can't open Timer-Device!");
  }
  /*** wenn TimeRequest==NULL => keine Panik, geht auch so!! ***/
#endif

  SIG_REXX = upRexxPort("showdvi", rcl, "sd", &arexx_disp);

#if 0
  getdir("",direct);		/* Pfadname des aktuellen Verzeichnisses */
  strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->Buffer, direct);
  strcpy((char *)((struct StringInfo *)(dir_Gad.SpecialInfo))->UndoBuffer, direct);
  //RefreshGadgets(&dir_Gad,win2,NULL);		/* refresh all gadgets	*/
  RefreshGList(&dir_Gad,win2,NULL,2);		/* Test: Nur 2 Gadgets refreshen  */
#endif

#if 0
  NewTimeRequest();		/* start timer				*/
#endif

  if (is_autoag && is_dvif) {
    (void)InitDVINotify();		/* ob's geht oder nicht ist egal */
					/* autoag geht auf jeden Fall */
    (void)is_newer_dvi_file();		/* initialisiere das Datum...*/
  }
  
#ifdef BETACOPYRIGHT
  Warning(MSG_BETA_VERSION);
#endif
}


void close_all_bild(void)
{
  struct Task *my_task;

  /* Speicher der Full-Page-Bitmap freigeben */
  if (BitMapFullPage.Planes[0] != NULL) {
#if 0
    if (is_bmfast) {
      FreeMem(BitMapFullPage.Planes[0], (wx+7)/8*wy);	// <-- das kann doch nie gestimmt haben??
    }
    else {
      FreeRaster(BitMapFullPage.Planes[0], BitMapFullPageWidth, BitMapFullPageHeight);
    }
#endif
    // FullPage ist immer mit AllocRaster() alloziert!
    FreeRaster(BitMapFullPage.Planes[0], BitMapFullPageWidth, BitMapFullPageHeight);
    BitMapFullPage.Planes[0] = NULL;
  }

  if (is_print) { 		/* */
    abort_print_page();
  }

  // Notfiy beenden/freigeben (Signal)
  EndDVINotify();


#if defined(APP_WINDOW)
  clear_app_win();
#else
  clear_app_icon();
#endif
  dnRexxPort();					/* close REXX port */
#if 0
  DeleteTimer(TimeRequest);
#endif
  free_menu();
  FreeFileReq();
  CloseRastPort();
  clear_pointer();
#if !defined(REQ_LIBRARY)
  remove_col_request();
#endif
  CloseSearchWin();
  CloseMessWin();
  ClosePrefWin();
  AboutWinDown();
  CloseWin2();
  FreeHelpLast();	// schliesst etwaiges Amiga-Guide Fenster
  
  DeleteShowDVIVars();

  if (is_myscr && test_anz_windows() > 0 && !is_os2) {
    MessageStr("FATAL!!!  Close screen with window!?!?!?\n");	/*###########*/
    LoggingStr("FATAL!!!  Close screen with window!?!?!?\n");	/*###########*/
    beep();
  }
  else {
    CloseS();
  }
  
  close_fonts();
  my_task = FindTask(NULL);
  if (my_task != NULL && old_task_name != NULL) {	/* restore task name */
    my_task->tc_Node.ln_Name = old_task_name;
  }
  CloseLib();
}


long *Init_all(int pix_length, int map_width)
{
  CloseRastPort();
  wx = map_width * 8; /* in pixel */
  wy = pix_length;    /* in pixel */


/** 
  static_x_Koo = 0;  that's not good!
  static_y_Koo = 0;  bleibt sonst nicht an der selben Stelle bei 'load file again' usw.
**/

  if (wx & 0x0000000F != 0) {		// % 16
    Fatal(20, MSG_INTERNAL_ERROR_W_ARG, GetTeXString(MSG_WRONG_MAP_WIDTH));
  }
#ifdef DEBUG
  if (DeBug) {
     Warning("MAP Breite/Hoehe = %d/%d",wx,wy);
  }
#endif




  set_resolution();		/* set to map-size (if map smaller than screen) */
  set_i_window_size();
  init_gad();			/* geaenderte Groesse beruecksichtigen */

#if 0
  /* und nun bei col4 muessen noch die Ueberreste geloescht werden, fuer den Fall, dass   */
  /* die Bitmap groesser wird. Fall einmal die Fenstergroesse mit geaendert wird, so wird */
  /* das nicht mehr benoetigt.								  */
  /* loesche Bildschirm IGITTTTT */
  fill_block((short)width_left_border,(short)HeightWinTitle,
			(short)x_win_i_width+poty_up2_Gad.Width-1,
			(short)x_win_i_height+potx_Gad.Height-1,(short)0);
  refresh_screen();
#else
  win_need_refresh = TRUE;	/* wird erst direkt vor dem erneutem scroll() gemacht */
#endif

  OpenRastPort();

#ifdef FONTS_IN_CHIPMEM
  /** initialisieren der Bitmap fuer Char-Kopieren */
  InitBitMap(&copy_char_bm,1L,1,1);
#endif

  return((long *)BitMapWin.Planes[0]);
}


long ShowPage(int same_page)
{
  long ret;
  short error;
  static int static_was_show = 0;
  static int file_again = FALSE;
  /* same_page: 0 normal, 1: bin auf selber Seite geblieben */


  if (InSearchMode) {
    if (SearchFoundRect) {
      // es wurde auf dieser Seite etwas gefunden
      InSearchMode = FALSE;
      // SearchFoundRect = NULL; (1)
      SearchFoundPage = current_page_phy;
      if (SearchWin) SetWindowTitles(SearchWin, GetTeXString(MSG_SEARCHWIN_FOUND), (UBYTE *)-1);
      return KOMM + 7;		// end search (Springt auf SearchFoundPage)
    }
    else {
      // es wurde nix gefunden, also weitersuchen
      enum SearchActions sr = DoSearchWin(NULL, 0);
      
      if (current_page_phy == SearchStartPage || sr == SEARCHWIN_ACTION_CLOSE || sr == SEARCHWIN_ACTION_CANCEL) {
        // bin einmal ganz rum und hab nix gefunden
        ReEnableSearchWin();
        InSearchMode = FALSE;
        SearchFoundPage = (current_page_phy == SearchStartPage) ? SearchStartPage : current_page_phy;
        if (current_page_phy == SearchStartPage) {
          Message(MSG_SEARCH_STRING_NOT_FOUND, SearchPattern);
        }
        else {
          Message(MSG_SEARCH_STRING_CANCELED);
        }
        return KOMM + 7;	// end search (Springt auf SearchFoundPage)
      }
      return KOMM + 6;
    }
  }
  else {
    if (SearchFoundRect) {
      // es wurde vorhin etwas gefunden, der SuchMode wurde in (1) abgeschaltet, das Rechteck gibt es aber noch.
      // die Seite wo was gefunden wurde ist nun aufgebaut. Jetzt wird in das Rechteck was gemalt und dann das Rechteck geloescht.
      long row      = SearchFoundRect->MinY - 1;
      long num_rows = SearchFoundRect->MaxY - SearchFoundRect->MinY + 2;
      long col      = (SearchFoundRect->MinX + 7) / 8;
      long num_cols = (SearchFoundRect->MaxX - col*8) / 8;
      
      long map_width = map.width / 8;
      
      char * bmp_ptr = ((char *)map.pixptr) + row * map_width + col;

      int i, j;
      
      long l_bits = SearchFoundRect->MinX & 7;
      long r_bits = SearchFoundRect->MaxX & 7;
      
      for (i=0; i<num_rows; i++) {
        if (l_bits > 0) bmp_ptr[-1] ^= (1<<(8-l_bits)) - 1;
        for (j=0; j<num_cols; j++) bmp_ptr[j] ^= 0xFF;
        if (r_bits > 0) bmp_ptr[num_cols] ^= ~((1<<(8-r_bits)) - 1);
        bmp_ptr += map_width;
      }
      
      if (static_y_Koo > SearchFoundRect->MinY) {
        int dy = SearchFoundRect->MinY - static_y_Koo - 10;
        if (is_gadg) window_plus_sbar_move(0, dy);
        else         window_move(0, dy);
      }
      if (static_y_Koo + x_win_i_height < SearchFoundRect->MaxY) {
        int dy = SearchFoundRect->MaxY - x_win_i_height - static_y_Koo + 10;
        if (is_gadg) window_plus_sbar_move(0, dy);
        else         window_move(0, dy);
      }

      ReEnableSearchWin();
      SearchFoundRect = NULL;	// nun ist's endlich vorbei mit der Sucherei
    }
  }



  use_phy_number = is_usephy;
  unset_tusephy;		/* set if page number via pscro */

  if (is_messwin) {  
    SetUpMessWin(FALSE);	/* no delta */
  }
  if (!same_page && is_dotbord) {		/* das sollte nur einmal pro Seite gemacht werden! */
    DrawDottedBorder(FALSE, TRUE);		/* no_refresh .. TRUE */
  }

  if (win_need_refresh) {
    /* und nun bei col4 muessen noch die Ueberreste geloescht werden, fuer den Fall, dass   */
    /* die Bitmap groesser wird. Fall einmal die Fenstergroesse mit geaendert wird, so wird */
    /* das nicht mehr benoetigt.								  */
    /* loesche Bildschirm IGITTTTT */

    /* zum Test wird mal so geloescht... */
    //fill_block((short)width_left_border,(short)HeightWinTitle,
    //			(short)x_win_i_width-1,(short)x_win_i_height-1,(short)0);
#if 0
    fill_block((short)width_left_border,(short)HeightWinTitle,
				(short)x_win_i_width+poty_up2_Gad.Width-1,
				(short)x_win_i_height+potx_Gad.Height-1,(short)0);
#endif
    // ich moechte den Bildschirm nicht loeschen muessen...
    // es muss reichen, dass ich ein win_show einfach darueber setze.
    // Moeglicherweise gibt es Probleme bei kleiner-groesser werdenden RPorts
    // doch das muss ohnehin ueberarbeitet werden...
    // Die Scrollbars muessen immer an den Fensterraendern bleiben!
    refresh_screen();
    win_need_refresh = FALSE;
  }


  do {		/* while (error != 0) */

    error = 0;
    
    if (is_dvif) {
      //pgscroll_Gad.Flags &= ~GFLG_DISABLED;
    }

    Set_PgGadPageCur();

#if 0
    set_int_gad();			/* no refresh */

    set_Gadgets_to_fname();		/* no refresh */
#endif

    if (is_dvif && dvifp != NULL) {  
      TempCloseDVI(dvifp);
    }
    unset_ndvif;


    if (is_gadg) {
      static_x_Koo = HFindScrollerTop();
      static_y_Koo = VFindScrollerTop();
    }


    // Nun gilt es die Seite neu zu positionieren:
    //
    // Auch im Full-Page Modus setze ich die Seitenposition neu.
    //
    // Ansonsten wird gechecked, ob ein Margin gesetzt ist.
    // Wenn nicht, dann wird ganz nach links oben gescrolled.
    // Wenn doch, dann wird an diesen Punkt gescrolled.
    // Wobei margin_(x|y) in 1000'tel Inch definiert ist.
    //
    // An den Margin-Punkt wird nur bei jmpup gesprungen.

    if (!same_page) {
      if (is_jmpup) {
        if (is_margin) {
          static_x_Koo = show_state.margin_x * hconvresolution / 1000;
          static_y_Koo = show_state.margin_y * vconvresolution / 1000;
        }
        else {
          static_y_Koo = 0;
        }
        unset_jmpup;
      }
      if (is_jmpdown) {
        if (is_margin) {
          static_x_Koo = show_state.margin_x * hconvresolution / 1000;
        }
        static_y_Koo = wy;
        unset_jmpdown;
      }
    }
    else {
      unset_jmpup;
      unset_jmpdown;
    }
    
    if (static_x_Koo < 0) static_x_Koo = 0;
    if (static_y_Koo < 0) static_y_Koo = 0;


    /* set scrollbars */
    VSetScrollerValues();
    HSetScrollerValues();


    if (static_was_show == 1) {
      show_full_page(TRUE);  		/* show-Modus einschalten */
      // Seite einkopieren...
      display_full_page();      	     // Seite einkopieren...
    }
    else {
      if (is_gadg) {
        window_plus_sbar_move(0, 0);	/* setze den Scrollbar richtig */
      }
      else {
        window_move(0, 0);		/* setzt static_{x,y}_Koo richtig */
      }
      window_show();			/* loescht noch zusaetzlich was...zeigt aber zum 2.ten Mal an */
    }

#if 0
    else {
    
      long lx = static_x_Koo, lxx;
      long ly = 0;	/* soll ich hoch springen?? */

      if (is_dvif && (is_new_dvifile || is_chres)) {
        /* Zentriere den Text, bzw. gehe an den linken Rand */
        const float widthin  = (float)pwidth_pt  / 72.27 + ((float)SAVETY_BITS_X / (float)hconvresolution) * 1000.0 / (float)mag;
        
        ly = -wy;	/* wenn obige Bedingung gegeben ist -> hoch springen */
        lx = (long)((float)(hoffset_in_fix * wx) / widthin + 0.5);
        lxx = (long)((((float)pwidth_pt / 72.27) * (float)wx) / widthin + 0.5);

		/* printf("widthin: %f, lx: %ld, lxx: %ld, x_win_i_width: %ld", widthin, lx, lxx, x_win_i_width); */

        if (lxx - lx < x_win_i_width) {
          /* Wenn das innere ganz auf den Screen passt, dann zentrieren! */
          lx -= (x_win_i_width - (lxx - lx)) / 2;
        }
        else {
          lx -= 5;
        }

      }

      if (is_gadg) {		/* setzt die Grenzen richtig */
        /* springe auf (lx,0) */
        if (same_page) {
          window_plus_sbar_move(0, 0);			/* sind delta Werte! */
        }
        else {
          window_plus_sbar_move(lx-static_x_Koo, ly);	/* sind delta Werte! */
        }
        
      }
      else {
        if (same_page) {
          window_move(0, 0);
        }
        else {
          window_move(lx-static_x_Koo, ly);
        }
      }
      // wird dieser Refresh wirklich benoetigt? Wird doch auch in scroll() gemacht...
      // window_show();		/* anzeigen <-> refresh */
    }
#endif

    unset_chres;	/* nun ist's aus mit der neuen resolution   */
  			/* bzw. die Info. wird nicht mehr benoetigt */


    write_status();	/* neue Seitennummer anzeigen */

    /* ######################################################################### */
    ret = scroll();					/* ### main function ### */
    /* ######################################################################### */



    if (is_show) {
      static_was_show = 1;
      show_full_page(TRUE);  /* show-Modus ausschalten */
      set_show;
      write_status();		/* NAJA */
      unset_show;
    }
    else {
      static_was_show = 0;
    }


    /* hab ich ein load-file-again gemacht, wird fuer das zenter in scroll gebraucht */
    file_again = (ret == KOMM + 3);
    
    if (ret == KOMM + 4 && is_autoag && is_notify) {
      InitDVINotify();	/* notify auf das neue File ausrichten */
    }

    if (ret == KOMM + 4 || !is_autoag) {  /* wird fuer das auto-load-again gebraucht! */
      is_new_dvifile = TRUE;		  /* KOMM + 4 == neues DVI-File laden */
    }					  /* !is_autoag damit das Datum immer richtig gesetzt wird */

    if (!is_dvif && ret != KOMM+10 && ret != KOMM+4) {
      error = 1;
      Message(MSG_NO_DVI_FILE);
    }
    else {
      if (is_dvif && !is_ndvif) {		/* No new DVI-File */
        TempOpenDVI(&dvifp);
        if (dvifp == NULL) {
          unset_dvif;			/* no DVI loadet */
          Message(MSG_DVI_FILE_LOST);
          beep();
          error = 1;
        }
      }
    }

    if (dvifp == NULL) {		/* savety */
      unset_dvif;
    }

  } while (error != 0);
  
  if (ret == -1) {
    ret = 0;			/* go to page 0 !! */
  }

  use_phy_number = (is_usephy || is_tusephy);
  
  return ret;
}

#endif
