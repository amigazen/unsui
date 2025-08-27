
#define REQ_LIBRARY		/* use the req.library for the color-requester */
#undef  ARP_LIBRARY
#undef  ASL_LIBRARY

#undef  REQ_FILEREQ
#undef  ARP_FILEREQ
#define ASL_FILEREQ


/* Startnummern der einzelnen GadID's */
#define WIN2_GADS	1
#define PREFWIN_GADS	(WIN2_GADS + 20)


#define MIN_STACK_SIZE	10000		/* minimal stack size */


extern struct ExecBase		* SysBase;
extern struct IntuitionBase	* IntuitionBase;
extern struct GfxBase		* GfxBase;
extern struct ArpBase		* ArpBase;
extern struct Library		* DiskfontBase;
extern struct Library		* WorkbenchBase;
extern struct Library		* IconBase;
extern struct DosLibrary	* DOSBase;
extern struct Library		* GadToolsBase;
extern struct Library 		* AslBase;
extern struct Library           * AmigaGuideBase;
extern struct Library           * UtilityBase;


#if defined(REQ_LIBRARY)
 extern struct ReqLib		* ReqBase;
#endif



#define PAL_SCR_WIDTH	640
#define PAL_SCR_HEIGHT	256
#define NTSC_SCR_WIDTH	640
#define NTSC_SCR_HEIGHT	200


/* Position der Ausgabefelder in der Kopfzeile */
#define WR_SECUNDARY_LEFT	(INT_LEFT_EDGE+32+2)
#define WR_COUNTER_LEFT		(WR_SECUNDARY_LEFT+16+8)
#define WR_STATUS_LEFT		(WR_COUNTER_LEFT+32+8)


/* Hoehe der beiden Kopfzeilen */
/* #define HEIGHT_SCREEN_TITLE_BAR	10	Diese zwei Werte sind nun in Variablen*/
/* #define HEIGHT_WIN_TITLE_BAR		11	abgelegt und werden immer neu berechnet!*/
/* #define HEIGHT_TITLE_BARS	(HeightScreenTitle+HeightWinTitle)	*/

/* Koo. des Anzeigefensters */
#define WIN_X_KOO		0
#define WIN_Y_KOO		HeightScreenTitle


/* Schrittweiten fuer das Scrollen (wird nur in gadget.c benötigt) */
/* Schrittweiten fuer Tastatur in amkey.c */
#define STEP_ARROW_X		6
#define STEP_ARROW_Y		18


/* Wartezeit fuer den Refresh der Kopfzeile */
#define WAIT_TIME_TICKS		35	/* Anzahl Intervalle (INTUITICKS) */



/************************************************************************/
/* *** Abstrakter Datentyp fuer Showdvi				    *** */

#undef USE_BITFIELD

#ifdef USE_BITFIELD
  /* spart 'etwas' Speicher, ist aber laenger im Zugriff */
# define BITS(name,nr)	unsigned name : nr
#else
  /* Jedes Feld ein char gross. nr darf halt nie groesser als 8 werden! */
  /* braucht etwas mehr Speicher, ist dafuer aber schneller im Zugriff und auch kuerzer im Code */
# define BITS(name,nr)	unsigned char name
#endif


struct state {	BITS(lace  	, 1);		/* Interlace			*/
		BITS(show  	, 1);		/* Full Page Modus		*/
		BITS(gadg  	, 1);		/* Scrollbars			*/
		BITS(print 	, 1);		/* laeuft Ausdruck		*/
		BITS(timer 	, 1);		/* laeuft dein Timer-Request	*/
		BITS(pscro 	, 1);		/* Seitenscrollen		*/
		BITS(colre 	, 1);		/* Color-Requester angezeigt	*/
		BITS(about 	, 1);		/* About angezeigt		*/
		BITS(help  	, 1);		/* Help angezeigt		*/
		BITS(arp   	, 1);		/* ARP-Lib. im System		*/
		BITS(dvif  	, 1);		/* DVI-File specifiziert	*/
		BITS(ndvif 	, 1);		/* neues DVI-File geladen?	*/
		BITS(escex	, 1);		/* Ende bei ESC?		*/
		BITS(beep  	, 1);		/* soll gebeept werden?		*/
		BITS(imenu 	, 1);		/* intelligentes menu?		*/
		BITS(wbench	, 1);		/* von WBench gestartet		*/
		BITS(bmenu 	, 1);		/* Menu in 11 Punkt Schrift?	*/
		BITS(mesu  	, 1);		/* mesuring an?			*/
		BITS(jmpup	, 1);		/* jump to top of page		*/
		BITS(jmpdown	, 1);		/* jump to bottom of page	*/
		BITS(sysmenu	, 1);		/* use system menu (not popup)	*/
		BITS(os2	, 1);		/* do we running under KS 2.0?	*/
		BITS(os21	, 1);		/* do we running under KS 2.1?	*/
		BITS(os3	, 1);		/* do we running under KS 3.0?	*/
		BITS(usephy	, 1);		/* use physical page numbers	*/
		BITS(tusephy	, 1);		/* tmp use physical page numbers*/
		BITS(appwin	, 1);		/* create app-window?		*/
		BITS(osmenu	, 1);		/* use normal pull down menu?	*/
		BITS(autoag	, 1);		/* auto load again		*/
		BITS(pubscr	, 1);		/* open a Public-Screen?	*/
		BITS(messwin	, 1);		/* open a messure window	*/
		BITS(dotbord	, 1);		/* dotted border		*/
		BITS(col4	, 1);		/* use a 4 color screen?	*/
		BITS(midmenu	, 1);		/* use the middle pop-up menu	*/
		BITS(chres  	, 1);		/* is the resolution changed	*/
		BITS(mmove	, 3);		/* MOUSEMOVE Counter		*/
		BITS(inticks	, 3);		/* INTUITICKS Counter		*/
		BITS(scrmode	, 3);		/* screenm (pal,ntsc,prod,a2024)*/
		BITS(whunit	, 2);		/* in: 0, cm: 1, pt: 2		*/
		BITS(titref	, 1);		/* Screen-Title to refresh?	*/
		BITS(clwbcol	, 1);		/* clone the wb colors?		*/
		BITS(notify	, 1);		/* use notify mechanism		*/
		BITS(quickex	, 1);		/* quit immediately		*/
		BITS(prefwin	, 1);		/* is Pref-Win up?		*/
		BITS(ownscr	, 1);		/* use own screen?		*/
		BITS(myscr	, 1);		/* is screen my own screen?	*/
		BITS(otherpubscr, 1);		/* is the pubscr opend from me?	*/
		BITS(village	, 1);		/* window on the village screen?*/
		BITS(bmfast	, 1);		/* page bitmap in fast ram?	*/
		BITS(alwbmfast	, 1);		/* bm alway in fast ram?	*/
		BITS(scrollras	, 1);		/* use ScrollRaster()		*/
		BITS(bmcpu	, 1);		/* copy (fast) bm with CPU	*/
		BITS(smartwin	, 1);		/* use smart-refresh window	*/
		BITS(margin	, 1);		/* are margins set?		*/
		BITS(amigaguide	, 1);		/* AmigaGuide ready?		*/
		BITS(bhook      , 1);		/* Backfill Hook?		*/

		short menu_res_lines;		/* nr of lines in the res-menu	*/
		unsigned short mres[11];	/* dpi-resolutions		*/

		short screen_size_x;		/* screen_size, 0 clone wb	*/
		short screen_size_y;		/* screen_size, 0 clone wb	*/
		short window_pos_x;		/* window_pos, -1 def pos	*/
		short window_pos_y;		/* window_pos, -1 def pos	*/
		short window_size_x;		/* window_size, -1 def size	*/
		short window_size_y;		/* window_size, -1 def size	*/
		short window_pos_own_scr_x;	/* window_pos, -1 def pos	*/
		short window_pos_own_scr_y;	/* window_pos, -1 def pos	*/
		short window_size_own_scr_x;	/* window_size, -1 def size	*/
		short window_size_own_scr_y;	/* window_size, -1 def size	*/
		long monitor_size_x;		/* aspect-ratio x		*/
		long monitor_size_y;		/* aspect-ratio y		*/
		long margin_x;			/* x margin to show		*/
		long margin_y;			/* y margin to show		*/
		ULONG aktIDCMP;			/* aktual IDCMP of win2		*/
		LONG  AppIconX;			/* AppIcon X Position		*/
		LONG  AppIconY;			/* AppIcon Y Position		*/
		ULONG DisplayID;		/* numeric DisplayID		*/
	        char *ftast[20];		/* F-Tasten Belegung AREXX	*/
	        char *command;			/* command of the 'r' key 	*/
	        char *arexxstart;		/* script, starts TeX-server	*/
	        
	        struct BitMap   * ChipPuffBM;	/* Puffer Bitmap in Chip-Mem	*/
	        struct RastPort   ChipPuffRP;	/* RastPort for ChipPuffBM	*/

		struct DrawInfo	* dri;
		void		* vi;
		
		UBYTE  APen;			/* foreground pen		*/
		UBYTE  BPen;			/* background pen		*/
 };


extern struct state show_state;	/* Anzeigezustand		 */
				/* interner Zustand: Abfrage plus setzen */

#define SDVI_DRI	show_state.dri
#define SDVI_VI		show_state.vi




/* in folgendem struct werden alle Images/Boopsi Objekte gespeichert */

struct BoopsiObj {
	struct Image *	LArrImage;	/* left arrow			*/
	struct Image *	RArrImage;	/* right arrow			*/
	struct Image *	UArrImage;	/* up arrow			*/
	struct Image *	DArrImage;	/* down arrow			*/
	struct Image *  SizeImage;	/* sizing button 		*/
 };

extern struct BoopsiObj BObj;




#define is_lace		(show_state.lace)
#define set_lace	show_state.lace = 1
#define unset_lace	show_state.lace = 0
#define toggle_lace	show_state.lace = !show_state.lace

#define is_show		(show_state.show)
#define set_show	show_state.show = 1
#define unset_show	show_state.show = 0
#define toggle_show	show_state.show = !show_state.show

#define is_gadg		(show_state.gadg)
#define set_gadg	show_state.gadg = 1
#define unset_gadg	show_state.gadg = 0
#define toggle_gadg	show_state.gadg = !show_state.gadg

#define is_print	(show_state.print)
#define set_print	show_state.print = 1
#define unset_print	show_state.print = 0
#define toggle_print	show_state.print = !show_state.print

#define is_timer	(show_state.timer)
#define set_timer	show_state.timer = 1
#define unset_timer	show_state.timer = 0
#define toggle_timer	show_state.timer = !show_state.timer

#define is_pscro	(show_state.pscro)
#define set_pscro	show_state.pscro = 1
#define unset_pscro	show_state.pscro = 0
#define toggle_pscro	show_state.pscro = !show_state.pscro

#define is_colre	(show_state.colre)
#define set_colre	show_state.colre = 1
#define unset_colre	show_state.colre = 0
#define toggle_colre	show_state.colre = !show_state.colre

#define is_about	(show_state.about)
#define set_about	show_state.about = 1
#define unset_about	show_state.about = 0
#define toggle_about	show_state.about = !show_state.about

#define is_arp		(show_state.arp)
#define set_arp		show_state.arp = 1
#define unset_arp	show_state.arp = 0
#define toggle_arp	show_state.arp = !show_state.arp

#define is_dvif		(show_state.dvif)
#define set_dvif	show_state.dvif = 1
#define unset_dvif	show_state.dvif = 0
#define toggle_dvif	show_state.dvif = !show_state.dvif

#define is_ndvif	(show_state.ndvif)
#define set_ndvif	show_state.ndvif = 1
#define unset_ndvif	show_state.ndvif = 0
#define toggle_ndvif	show_state.ndvif = !show_state.ndvif

#define is_beep		(show_state.beep)
#define set_beep	show_state.beep = 1
#define unset_beep	show_state.beep = 0
#define toggle_beep	show_state.beep = !show_state.beep

#define is_escex	(show_state.escex)
#define set_escex	show_state.escex = 1
#define unset_escex	show_state.escex = 0
#define toggle_escex	show_state.escex = !show_state.escex

#define is_imenu	(show_state.imenu)
#define set_imenu	show_state.imenu = 1
#define unset_imenu	show_state.imenu = 0
#define toggle_imenu	show_state.imenu = !show_state.imenu

#define is_wbench	(show_state.wbench)
#define set_wbench	show_state.wbench = 1
#define unset_wbench	show_state.wbench = 0
#define toggle_wbench	show_state.wbench = !show_state.wbench

#define is_bmenu	(show_state.bmenu)
#define set_bmenu	show_state.bmenu = 1
#define unset_bmenu	show_state.bmenu = 0
#define toggle_bmenu	show_state.bmenu = !show_state.bmenu

#define is_mesu		(show_state.mesu)
#define set_mesu	show_state.mesu = 1
#define unset_mesu	show_state.mesu = 0
#define toggle_mesu	show_state.mesu = !show_state.mesu

#define is_jmpup	(show_state.jmpup)
#define set_jmpup	show_state.jmpup = 1
#define unset_jmpup	show_state.jmpup = 0
#define toggle_jmpup	show_state.jmpup = !show_state.jmpup

#define is_jmpdown	(show_state.jmpdown)
#define set_jmpdown	show_state.jmpdown = 1
#define unset_jmpdown	show_state.jmpdown = 0
#define toggle_jmpdown	show_state.jmpdown = !show_state.jmpdown

#define is_sysmenu	(show_state.sysmenu)
#define set_sysmenu	show_state.sysmenu = 1
#define unset_sysmenu	show_state.sysmenu = 0
#define toggle_sysmenu	show_state.sysmenu = !show_state.sysmenu

#define is_os2		(show_state.os2)
#define set_os2		show_state.os2 = 1
#define unset_os2	show_state.os2 = 0
#define toggle_os2	show_state.os2 = !show_state.os2

#define is_os21		(show_state.os21)
#define set_os21	show_state.os21 = 1
#define unset_os21	show_state.os21 = 0
#define toggle_os21	show_state.os21 = !show_state.os21

#define is_os3		(show_state.os3)
#define set_os3		show_state.os3 = 1
#define unset_os3	show_state.os3 = 0
#define toggle_os3	show_state.os3 = !show_state.os3

#define is_usephy	(show_state.usephy)
#define set_usephy	show_state.usephy = 1
#define unset_usephy	show_state.usephy = 0
#define toggle_usephy	show_state.usephy = !show_state.usephy

#define is_tusephy	(show_state.tusephy)
#define set_tusephy	show_state.tusephy = 1
#define unset_tusephy	show_state.tusephy = 0
#define toggle_tusephy	show_state.tusephy = !show_state.tusephy

#define is_appwin	(show_state.appwin)
#define set_appwin	show_state.appwin = 1
#define unset_appwin	show_state.appwin = 0
#define toggle_appwin	show_state.appwin = !show_state.appwin

#define is_osmenu	(show_state.osmenu)
#define set_osmenu	show_state.osmenu = 1
#define unset_osmenu	show_state.osmenu = 0
#define toggle_osmenu	show_state.osmenu = !show_state.osmenu

#define is_autoag	(show_state.autoag)
#define set_autoag	show_state.autoag = 1
#define unset_autoag	show_state.autoag = 0
#define toggle_autoag	show_state.autoag = !show_state.autoag

#define is_pubscr	(show_state.pubscr)
#define set_pubscr	show_state.pubscr = 1
#define unset_pubscr	show_state.pubscr = 0
#define toggle_pubscr	show_state.pubscr = !show_state.pubscr

#define is_messwin	(show_state.messwin)
#define set_messwin	show_state.messwin = 1
#define unset_messwin	show_state.messwin = 0
#define toggle_messwin	show_state.messwin = !show_state.messwin

#define is_dotbord	(show_state.dotbord)
#define set_dotbord	show_state.dotbord = 1
#define unset_dotbord	show_state.dotbord = 0
#define toggle_dotbord	show_state.dotbord = !show_state.dotbord

#define is_col4		(show_state.col4)
#define set_col4	show_state.col4 = 1
#define unset_col4	show_state.col4 = 0
#define toggle_col4	show_state.col4 = !show_state.col4

#define is_midmenu	(show_state.midmenu)
#define set_midmenu	show_state.midmenu = 1
#define unset_midmenu	show_state.midmenu = 0
#define toggle_midmenu	show_state.midmenu = !show_state.midmenu

#define is_titref	(show_state.titref)
#define set_titref	show_state.titref = 1
#define unset_titref	show_state.titref = 0
#define toggle_titref	show_state.titref = !show_state.titref

#define is_clwbcol	(show_state.clwbcol)
#define set_clwbcol	show_state.clwbcol = 1
#define unset_clwbcol	show_state.clwbcol = 0
#define toggle_clwbcol	show_state.clwbcol = !show_state.clwbcol

#define is_notify	(show_state.notify)
#define set_notify	show_state.notify = 1
#define unset_notify	show_state.notify = 0
#define toggle_notify	show_state.notify = !show_state.notify

#define is_quickex	(show_state.quickex)
#define set_quickex	show_state.quickex = 1
#define unset_quickex	show_state.quickex = 0
#define toggle_quickex	show_state.quickex = !show_state.quickex

#define is_chres	(show_state.chres)
#define set_chres	show_state.chres = 1
#define unset_chres	show_state.chres = 0
#define toggle_chres	show_state.chres = !show_state.chres

#define is_prefwin	(show_state.prefwin)
#define set_prefwin	show_state.prefwin = 1
#define unset_prefwin	show_state.prefwin = 0
#define toggle_prefwin	show_state.prefwin = !show_state.prefwin

#define is_ownscr	(show_state.ownscr)
#define set_ownscr	show_state.ownscr = 1
#define unset_ownscr	show_state.ownscr = 0
#define toggle_ownscr	show_state.ownscr = !show_state.ownscr

#define is_myscr	(show_state.myscr)
#define set_myscr	show_state.myscr = 1
#define unset_myscr	show_state.myscr = 0
#define toggle_myscr	show_state.myscr = !show_state.myscr

#define is_otherpubscr		(show_state.otherpubscr)
#define set_otherpubscr		show_state.otherpubscr = 1
#define unset_otherpubscr	show_state.otherpubscr = 0
#define toggle_otherpubscr	show_state.otherpubscr = !show_state.otherpubscr

#define is_village	(show_state.village)
#define set_village	show_state.village = 1
#define unset_village	show_state.village = 0
#define toggle_village	show_state.village = !show_state.village

#define is_bmfast	(show_state.bmfast)
#define set_bmfast	show_state.bmfast = 1
#define unset_bmfast	show_state.bmfast = 0
#define toggle_bmfast	show_state.bmfast = !show_state.bmfast

#define is_alwbmfast		(show_state.alwbmfast)
#define set_alwbmfast		show_state.alwbmfast = 1
#define unset_alwbmfast		show_state.alwbmfast = 0
#define toggle_alwbmfast	show_state.alwbmfast = !show_state.alwbmfast

#define is_scrollras		(show_state.scrollras)
#define set_scrollras		show_state.scrollras = 1
#define unset_scrollras		show_state.scrollras = 0
#define toggle_scrollras	show_state.scrollras = !show_state.scrollras

#define is_bmcpu	(show_state.bmcpu)
#define set_bmcpu	show_state.bmcpu = 1
#define unset_bmcpu	show_state.bmcpu = 0
#define toggle_bmcpu	show_state.bmcpu = !show_state.bmcpu

#define is_smartwin	(show_state.smartwin)
#define set_smartwin	show_state.smartwin = 1
#define unset_smartwin	show_state.smartwin = 0
#define toggle_smartwin	show_state.smartwin = !show_state.smartwin

#define is_margin	(show_state.margin)
#define set_margin	show_state.margin = 1
#define unset_margin	show_state.margin = 0
#define toggle_margin	show_state.margin = !show_state.margin

#define is_amigaguide		(show_state.amigaguide)
#define set_amigaguide		show_state.amigaguide = 1
#define unset_amigaguide	show_state.amigaguide = 0
#define toggle_amigaguide	show_state.amigaguide = !show_state.amigaguide

#define is_bhook	(show_state.bhook)
#define set_bhook	show_state.bhook = 1
#define unset_bhook	show_state.bhook = 0
#define toggle_bhook	show_state.bhook = !show_state.bhook



#define undef_scrmode	show_state.scrmode = 0
#define set_pal		show_state.scrmode = 1
#define set_ntsc	show_state.scrmode = 2
#define set_prod	show_state.scrmode = 3
#define set_a2024	show_state.scrmode = 4
#define set_wbmode	show_state.scrmode = 5
#define set_numeric	show_state.scrmode = 6		// numeric ID (DisplayID)
#define is_pal		(show_state.scrmode == 1)
#define is_ntsc		(show_state.scrmode == 2)
#define is_prod		(show_state.scrmode == 3)
#define is_a2024	(show_state.scrmode == 4)
#define is_wbmode	(show_state.scrmode == 5)
#define is_numeric	(show_state.scrmode == 6)
#define is_scr_mode	(show_state.scrmode != 0)

#define set_unit_in	show_state.whunit = 0
#define set_unit_cm	show_state.whunit = 1
#define set_unit_pt	show_state.whunit = 2
#define is_unit_in	(show_state.whunit == 0)
#define is_unit_cm	(show_state.whunit == 1)
#define is_unit_pt	(show_state.whunit == 2)

#define add_mmove	show_state.mmove++
#define sub_mmove	show_state.mmove--
#define is_mmove	(show_state.mmove)
#define clear_mmove	show_state.mmove=0

#define add_inticks	show_state.inticks++
#define sub_inticks	show_state.inticks--
#define is_inticks	(show_state.inticks)
#define clear_inticks	show_state.inticks=0





/*------------------------------------------------------------------------*/

/* These definitions will appear in V39 include files.  Until then,
 * they're available locally
 */

#ifndef	WA_NewLookMenus
#define WA_NewLookMenus	(WA_Dummy + 0x30)
#endif

#ifndef	WA_AmigaKey
#define WA_AmigaKey	(WA_Dummy + 0x31)
#endif

#ifndef SYSIA_ReferenceFont
#define SYSIA_ReferenceFont	(IA_Dummy + 0x19)
#endif

#ifndef MENUCHECK
#define	MENUCHECK	(0x10L)
#endif

#ifndef AMIGAKEY
#define AMIGAKEY	(0x11L)
#endif

#ifndef GTMN_Checkmark
#define GTMN_Checkmark	     GT_TagBase+65
#endif

#ifndef GTMN_AmigaKey
#define GTMN_AmigaKey	     GT_TagBase+66
#endif

#ifndef GTMN_NewLookMenus
#define GTMN_NewLookMenus	    GT_TagBase+67
#endif

/*------------------------------------------------------------------------*/



/****************************************************************************/
/* Merker fuer die Farben */
struct current_color {
  short red_0,  green_0,  blue_0, 
	red_1,  green_1,  blue_1,
	red_2,  green_2,  blue_2,
	red_3,  green_3,  blue_3;
#if 0	/* nur fuer eigenen color-requester */
	red_t0, green_t0, blue_t0,		/* temporaer */
	red_t1, green_t1, blue_t1,		/* temporaer */
	col_number_t;
#endif
  };

extern struct current_color current_col;

extern int background;		/* TRUE - FALSE je nach Programmstart */


/****************************************************************************/
/* Flags fuer das Show-Fenster win2 */
#define IDCMP_FLAGS	IDCMP_CLOSEWINDOW | IDCMP_RAWKEY | IDCMP_GADGETUP | IDCMP_GADGETDOWN |\
		IDCMP_MOUSEBUTTONS | IDCMP_ACTIVEWINDOW | IDCMP_INACTIVEWINDOW | IDCMP_MENUPICK |\
		IDCMP_NEWSIZE | IDCMP_REFRESHWINDOW | IDCMP_IDCMPUPDATE


#define ABS(x)		(((x)>0) ? (x) : -(x))
#define ABSOLUT(x)	(((x)>=0) ? (x) : -(x))


extern int Enable_Abort;
extern struct RastPort		myRastPort;  /* BitMap */
extern long wx, wy;			/* gebr. von gadget.c */
extern long static_x_Koo,		/* gebr. von gadget.c */
	    static_y_Koo;

extern short use_phy_number;	/* logical or physikal page numbers */

extern short width_left_border;
extern short width_right_border;

extern short HeightScreenTitle;
extern short HeightWinTitle;


extern struct TextAttr	txtAttr8;
extern struct TextAttr	txtAttr11;
extern struct TextAttr	txtAttr;
extern struct TextAttr	GadtxtAttr;




extern short	x_win_i_height, /* needet for the scrollbars in gadget.c */
		x_win_i_width,
		x_win_width,	/* needet for the menus			 */
		x_win_height,
		x_scr_width,	/* needet for the filerequester		 */
		x_scr_height;

extern struct Window *win2;
extern struct Screen *screen;
extern APTR	      visual;		/* visual Info des jeweiligen Screens */
extern char 	      PubScreenName[];
extern char	      MyPubScreenName[];

extern char	     *directory;





extern char *app_icon_name;	/* defined in app_win.c */
#define DEFAULT_APP_ICON_NAME	"TeX:config/AppIcon.info"

#ifdef FONTS_IN_CHIPMEM
/***  Bitmap zum Char-Kopieren !! ***/
extern struct BitMap copy_char_bm;
#endif





#undef is_os2
#define is_os2	TRUE
#undef is_pscro
#define is_pscro FALSE


enum SearchOpt { SO_OpenSearchWin, SO_StartSearchAgain };

// SO_OpenSearchWin    : 's' oder Menue
// SO_StartSearchAgain : SearchWin ist offen, Im SearchWin wurde 'Search' gedrueckt

