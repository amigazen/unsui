#ifdef AMIGA


#define SYS_GAD_Y	1


struct ExtGadget	* pg_pot_Gad,		// BOOPSI
			* pg_left_Gad,
			* pg_right_Gad,
			* pg_int_Gad;




#if 0 // -----------------------------------------------

static struct IntuiText firstPage_Title =
 {
	1, 0, JAM2, 0, 0, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"--",
	(struct IntuiText*)NULL
 };
static struct IntuiText prevPage_Title =
 {
	1, 0, JAM2, 0, 0, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"-",
	(struct IntuiText*)NULL
 };
static struct IntuiText lastPage_Title =
 {
	1, 0, JAM2, 0, 0, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"++",
	(struct IntuiText*)NULL
 };
static struct IntuiText succPage_Title =
 {
	1, 0, JAM2, 0, 0, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"+",
	(struct IntuiText*)NULL
 };
static struct IntuiText dir_gad_Title =
 {
	0, 1, JAM1, -9, 1, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"D",
	(struct IntuiText*)NULL
 };
static struct IntuiText fil_gad_Title =
 {
	0, 1, JAM1, -9, 1, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"F",
	(struct IntuiText*)NULL
 };
static struct IntuiText int_gad_Title =
 {
	0, 1, JAM1, -9, 1, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"#",
	(struct IntuiText*)NULL
 };

static UBYTE	dir_buffer[200],
		dir_buffer_undo[200],
		fil_buffer[200],
		fil_buffer_undo[200],
		int_buffer[5],
		int_buffer_undo[5];

static struct StringInfo dir_strInfo =
 {
	dir_buffer, dir_buffer_undo, 0, 199, 0, 0, 0, 0,
	0, 0, NULL, 0L,
	(struct KeyMap*)NULL
 },
		  fil_strInfo =
 {
	fil_buffer, fil_buffer_undo, 0, 199, 0, 0, 0, 0,
	0, 0, NULL, 0L,
	(struct KeyMap*)NULL
 },
		  int_strInfo =
 {
	int_buffer, int_buffer_undo, 0, 4, 0, 0, 0, 0,
	0, 0, NULL, 0L,
	(struct KeyMap*)NULL
 };


struct Gadget first_Page_Gad =
 {
	(struct Gadget*)NULL, PA1_LEFT_EDGE, SYS_GAD_Y,
	16, 10, GADGHCOMP, RELVERIFY | TOPBORDER ,
	BOOLGADGET, (APTR)NULL, (APTR)NULL,
	&firstPage_Title, 0L, (APTR)NULL,
	FIRST_GAD_NR, (APTR)NULL
 };
struct Gadget prev_Page_Gad =
 {
	(struct Gadget*)NULL, PA2_LEFT_EDGE, SYS_GAD_Y,
	8, 10, GADGHCOMP, RELVERIFY | TOPBORDER,
	BOOLGADGET, (APTR)NULL, (APTR)NULL,
	&prevPage_Title, 0L, (APTR)NULL,
	PREV_GAD_NR, (APTR)NULL
 };
struct Gadget succ_Page_Gad =
 {
	(struct Gadget*)NULL, PA3_LEFT_EDGE, SYS_GAD_Y,
	8, 10, GADGHCOMP, RELVERIFY | TOPBORDER,
	BOOLGADGET, (APTR)NULL, (APTR)NULL,
	&succPage_Title, 0L, (APTR)NULL,
	SUCC_GAD_NR, (APTR)NULL
 };
struct Gadget last_Page_Gad =
 {
	(struct Gadget*)NULL, PA4_LEFT_EDGE, SYS_GAD_Y,
	16, 10, GADGHCOMP, RELVERIFY | TOPBORDER,
	BOOLGADGET, (APTR)NULL, (APTR)NULL,
	&lastPage_Title, 0L, (APTR)NULL,
	LAST_GAD_NR, (APTR)NULL
 };

struct Gadget dir_Gad = 
 {
	(struct Gadget*)NULL, DIR_LEFT_EDGE, SYS_GAD_Y,
	160, 10, GADGHCOMP, RELVERIFY | TOPBORDER,
	STRGADGET, (APTR)NULL, (APTR)NULL,
	(struct IntuiText*)&dir_gad_Title, 0L,
	(APTR)&dir_strInfo,
	DIR_GAD_NR, (APTR)NULL
 };
struct Gadget fil_Gad = 
 {
	(struct Gadget*)NULL, STR_LEFT_EDGE, SYS_GAD_Y,
	104, 10, GADGHCOMP, RELVERIFY | STRINGCENTER | TOPBORDER,
	STRGADGET, (APTR)NULL, (APTR)NULL,
	(struct IntuiText*)&fil_gad_Title, 0L,
	(APTR)&fil_strInfo,
	FIL_GAD_NR, (APTR)NULL
 };
struct Gadget int_Gad = 
 {
	(struct Gadget*)NULL, INT_LEFT_EDGE, SYS_GAD_Y,
	30, 10, GADGHCOMP, RELVERIFY | LONGINT | TOPBORDER,
	STRGADGET, (APTR)NULL, (APTR)NULL,
	(struct IntuiText*)&int_gad_Title, 0L,
	(APTR)&int_strInfo,
	INT_GAD_NR, (APTR)NULL
 };

#endif // -----------------------------------------





struct PropInfo potx_PropInfo,
		poty_PropInfo;
struct Image    potx_Image,
		poty_Image;

struct ExtGadget potx_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	0, 0,  /* wird spaeter besetzt */
	GFLG_GADGHCOMP | GFLG_RELBOTTOM | GFLG_RELWIDTH | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_BOTTOMBORDER | GACT_FOLLOWMOUSE,
	GTYP_PROPGADGET,
	&potx_Image,
	NULL,
	NULL, 0L,
	(APTR)&potx_PropInfo,
	POTX_GAD_NR, NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };
struct ExtGadget poty_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	0, 0,  /* wird spaeter besetzt */
	GFLG_GADGHCOMP | GFLG_RELRIGHT | GFLG_RELHEIGHT | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER | GACT_FOLLOWMOUSE,
	GTYP_PROPGADGET,
	&poty_Image,
	NULL,
	NULL, 0L,
	(APTR)&poty_PropInfo,
	POTY_GAD_NR, NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };


struct ExtGadget potx_left_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	(SHORT)WIDTH_X_GADGETS, (SHORT)HEIGHT_SCROLL_BAR,
	GFLG_GADGIMAGE | GFLG_RELBOTTOM | GFLG_RELRIGHT | GFLG_GADGHIMAGE | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_BOTTOMBORDER,
	BOOLGADGET /* | GZZGADGET */, 
	NULL, NULL,
	NULL, 0L, (APTR)NULL,
	POTX_PFEIL_LINKS_GAD_NR, (APTR)NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };


struct ExtGadget potx_right_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	(SHORT)WIDTH_X_GADGETS, (SHORT)HEIGHT_SCROLL_BAR,
	GFLG_GADGIMAGE | GFLG_GADGHIMAGE | GFLG_RELBOTTOM | GFLG_RELRIGHT | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_BOTTOMBORDER,
	BOOLGADGET /* | GZZGADGET */, 
	NULL, NULL,
	NULL, 0L, (APTR)NULL,
	POTX_PFEIL_RECHTS_GAD_NR, (APTR)NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };


struct ExtGadget poty_up_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	WIDTH_SCROLL_BAR, HEIGHT_UD_ARROW,
	GFLG_GADGHIMAGE | GFLG_GADGIMAGE | GFLG_RELRIGHT | GFLG_RELBOTTOM | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER,
	BOOLGADGET /* | GZZGADGET */, 
	NULL, NULL,
	NULL, 0L, (APTR)NULL,
	POTY_PFEIL_OBEN_GAD_NR, (APTR)NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };

struct ExtGadget poty_up2_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	WIDTH_SCROLL_BAR, HEIGHT_UD_ARROW,
	GFLG_GADGHIMAGE | GFLG_GADGIMAGE | GFLG_RELRIGHT | GFLG_RELWIDTH | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER,
	BOOLGADGET /* | GZZGADGET */, 
	NULL, NULL,
	NULL, 0L, (APTR)NULL,
	POTY_PFEIL_OBEN2_GAD_NR, (APTR)NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };


struct ExtGadget poty_down_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	WIDTH_SCROLL_BAR, HEIGHT_UD_ARROW,
	GFLG_GADGHIMAGE | GFLG_GADGIMAGE | GFLG_RELRIGHT | GFLG_RELBOTTOM | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER,
	BOOLGADGET /* | GZZGADGET */,
	NULL, NULL,
	NULL, 0L, (APTR)NULL,
	POTY_PFEIL_UNTEN_GAD_NR, (APTR)NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };

struct ExtGadget poty_down2_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	WIDTH_SCROLL_BAR, HEIGHT_UD_ARROW,
	GFLG_GADGHIMAGE | GFLG_GADGIMAGE | GFLG_RELRIGHT | GFLG_RELWIDTH | GFLG_EXTENDED,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER,
	BOOLGADGET /* | GZZGADGET */,
	NULL, NULL,
	NULL, 0L, (APTR)NULL,
	POTY_PFEIL_UNTEN2_GAD_NR, (APTR)NULL,
	
	GMORE_GADGETHELP,
	0, 0, 0, 0
 };

#if 0
struct Gadget blatt_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	0, 0,
	GADGHCOMP | GADGIMAGE | GFLG_RELRIGHT | GFLG_RELBOTTOM,
	GADGIMMEDIATE | RELVERIFY | BOTTOMBORDER,
	BOOLGADGET /* | GZZGADGET */, 
	(APTR)&blatt, (APTR)NULL,
	NULL, 0L, (APTR)NULL,
	BLATT_GAD_NR, (APTR)NULL
 };


static struct IntuiText blatt_ok_txt =
 {
	1, 0, JAM2, 0, 0, (struct TextAttr*)&GadtxtAttr, (UBYTE*)"OK",
	(struct IntuiText*)NULL
 };

struct Gadget blatt_ok_Gad =
 {
	(struct Gadget*)NULL,
	BOK_LEFT_EDGE, SYS_GAD_Y,	/* left edge depends on resolution */
	WIDTH_OK_BLATT_GADGET, HEIGHT_BLATT_GADGET,
	GADGHCOMP /*| GADGIMAGE*/, GADGIMMEDIATE | RELVERIFY | TOPBORDER,
	BOOLGADGET /* | GZZGADGET */,
#if 0
	(APTR)&blatt_ok, (APTR)NULL,
#endif
	(APTR)NULL, (APTR)NULL,
	&blatt_ok_txt, 0L, (APTR)NULL,
	BLATT_OK_GAD_NR, (APTR)NULL
 };

static const USHORT chip zahlen[70] = 
 {	 0x0006, 0x0009, 0x0009, 0x0009, 0x0009, 0x0009, 0x0006,
	 0x0001, 0x0003, 0x0001, 0x0001, 0x0001, 0x0001, 0x0001,
	 0x000E, 0x0001, 0x0001, 0x0006, 0x0008, 0x0008, 0x000F,
	 0x000E, 0x0001, 0x0001, 0x0006, 0x0001, 0x0001, 0x000E,
	 0x000A, 0x000A, 0x000A, 0x000F, 0x0002, 0x0002, 0x0002,
	 0x000F, 0x0008, 0x0008, 0x000E, 0x0001, 0x0001, 0x000E,
	 0x0007, 0x0008, 0x0008, 0x000E, 0x0009, 0x0009, 0x0006,
	 0x000F, 0x0001, 0x0002, 0x0004, 0x0004, 0x0004, 0x0004,
	 0x0006, 0x0009, 0x0009, 0x0006, 0x0009, 0x0009, 0x0006,
	 0x0006, 0x0009, 0x0009, 0x0007, 0x0001, 0x0001, 0x000E
 };

static struct Image zahl_Image =
 {
	0, 0, 4, 7, 1, NULL, 1, 0, NULL
 };
#endif


#if 0
/* kein CHIP, da es ohnehin kopiert werden muss */
static const USHORT blatt_fast[20] =
 {	0xFFFF, 0xF800,
 	0xF272, 0x7800,
 	0xE471, 0x3800,
 	0xC800, 0x9800,
 	0x9000, 0x4800,
 	0x9000, 0x4800,
 	0xC800, 0x9800,
 	0xE471, 0x3800,
 	0xF272, 0x7800,
	0xFFFF, 0xF800,
 };


/* kein CHIP, da es ohnehin kopiert werden muss */
static const USHORT blatt_no_data[20] = 
{	0x0000, 0x0000,
	0x1047, 0x8000,
	0x1848, 0x4000,
	0x1C48, 0x4000,
	0x1648, 0x4000,
	0x1348, 0x4000,
	0x11C8, 0x4000,
	0x10C8, 0x4000,
	0x1047, 0x8000,
	0x0000, 0x0000
};

static const USHORT chip blatt_ok_data[20] = 
{	0x0000, 0x0800,
	0x0788, 0x8800,
	0x0849, 0x0800,
	0x084A, 0x0800,
	0x084C, 0x0800,
	0x084A, 0x0800,
	0x0849, 0x0800,
	0x0788, 0x8800,
	0x0000, 0x0800,
	0x0000, 0x0800
};
#endif

#if 0
static struct Image img_pgscroll =
 {
   (SHORT)0,(SHORT)0,
   (SHORT)WIDTH_SCROLL+2, (SHORT)2*HEIGHT_UD_ARROW, (SHORT)1,
   (USHORT *)pgscroll, (SHORT)3,(SHORT)0, (struct Image *)NULL
 };

static struct Image img_pgscroll_no =
 {
   (SHORT)0,(SHORT)0,
   (SHORT)WIDTH_SCROLL+2, (SHORT)HEIGHT_UD_ARROW, (SHORT)1,
   (USHORT *)pgscroll_no, (SHORT)3,(SHORT)0, (struct Image *)NULL
 };

static struct Image img_pgscroll_ok =
 {
   (SHORT)0,(SHORT)0,
   (SHORT)WIDTH_SCROLL+2, (SHORT)HEIGHT_UD_ARROW, (SHORT)1,
   (USHORT *)pgscroll_ok, (SHORT)3,(SHORT)0, (struct Image *)NULL
 };

static struct Image img_pgscroll_sel =
 {
   (SHORT)0,(SHORT)0,
   (SHORT)WIDTH_SCROLL+2, (SHORT)2*HEIGHT_UD_ARROW, (SHORT)1,
   (USHORT *)pgscroll_sel, (SHORT)3,(SHORT)0, (struct Image *)NULL
 };

static struct Image img_pgscroll_no_sel =
 {
   (SHORT)0,(SHORT)0,
   (SHORT)WIDTH_SCROLL+2, (SHORT)HEIGHT_UD_ARROW, (SHORT)1,
   (USHORT *)pgscroll_no_sel, (SHORT)3,(SHORT)0, (struct Image *)NULL
 };

static struct Image img_pgscroll_ok_sel =
 {
   (SHORT)0,(SHORT)0,
   (SHORT)WIDTH_SCROLL+2, (SHORT)HEIGHT_UD_ARROW, (SHORT)1,
   (USHORT *)pgscroll_ok_sel, (SHORT)3,(SHORT)0, (struct Image *)NULL
 };


struct Gadget pgscroll_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	0, 0,
	GFLG_GADGHIMAGE| GFLG_GADGIMAGE | GFLG_RELRIGHT,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER,
	BOOLGADGET, 
	(APTR)&img_pgscroll, (APTR)&img_pgscroll_sel,
	NULL, 0L, (APTR)NULL,
	PGSCROLL_GAD_NR, (APTR)NULL
 };

struct Gadget pgscroll_no_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	0, 0,
	GFLG_GADGHIMAGE| GFLG_GADGIMAGE | GFLG_RELRIGHT,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER,
	BOOLGADGET, 
	(APTR)&img_pgscroll_no, (APTR)&img_pgscroll_no_sel,
	NULL, 0L, (APTR)NULL,
	PGSCROLL_NO_GAD_NR, (APTR)NULL
 };

struct Gadget pgscroll_ok_Gad =
 {
	(struct Gadget*)NULL,
	0, 0,
	0, 0,
	GFLG_GADGHIMAGE| GFLG_GADGIMAGE | GFLG_RELRIGHT,
	GACT_IMMEDIATE | GACT_RELVERIFY | GACT_RIGHTBORDER,
	BOOLGADGET, 
	(APTR)&img_pgscroll_ok, (APTR)&img_pgscroll_ok_sel,
	NULL, 0L, (APTR)NULL,
	PGSCROLL_OK_GAD_NR, (APTR)NULL
 };
#endif

#endif
