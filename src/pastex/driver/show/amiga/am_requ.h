/* *** am_requ.h *** */



#define CREQ_LOOK_X		13
#define CREQ_LOOK_Y		10

#define CREQ_HEADER_Y		13

#define CREQ_X_KOO		170
#define CREQ_Y_KOO		40
#define CREQ_WIDTH		300
#define CREQ_HEIGHT		115
#define CREQ_DISTANCE_Y		30	/* Distance left-right-border */

#define CREQ_OC_WIDTH		60
#define CREQ_OC_HEIGHT		20
#define CREQ_OC_HEIGHT_NL	20	/* Hoehe im Non-Interlace */
#define CREQ_OK_X		(CREQ_DISTANCE_Y)
#define CREQ_OK_Y		(CREQ_HEIGHT - (3*CREQ_OC_HEIGHT)/2)
#define CREQ_CANCEL_X		(CREQ_WIDTH - CREQ_DISTANCE_Y - CREQ_OC_WIDTH)
#define CREQ_CANCEL_Y		(CREQ_OK_Y)

#define CREQ_RGB_LENGTH		(CREQ_WIDTH-2*CREQ_DISTANCE_Y-10)
#define CREQ_RGB_HEIGHT		10
#define CREQ_RGB_DIST		7
#define CREQ_R_Y		30
#define CREQ_G_Y		(CREQ_R_Y+CREQ_RGB_HEIGHT+CREQ_RGB_DIST)
#define CREQ_B_Y		(CREQ_G_Y+CREQ_RGB_HEIGHT+CREQ_RGB_DIST)

#define CREQ_COL_WIDTH		40
#define CREQ_COL_HEIGHT		12
#define CREQ_COL_DIS_01		15
#define CREQ_DIST_FR_TOP	11
#define CREQ_COL_1_X		(CREQ_WIDTH-CREQ_DISTANCE_Y-CREQ_COL_WIDTH)
#define CREQ_COL_0_X		(CREQ_COL_1_X-CREQ_COL_DIS_01-CREQ_COL_WIDTH)
#define CREQ_COL_1_Y		(CREQ_DIST_FR_TOP)
#define CREQ_COL_0_Y		(CREQ_DIST_FR_TOP)


#if !defined(REQ_LIBRARY)


static struct IntuiText col_cancel_text =
 {
	1, 0, JAM2, 6, 6,
	(struct TextAttr*)&txtAttr, (UBYTE*)"cancel",
	(struct IntuiText*)NULL
 };

static struct IntuiText col_ok_text =
 {
	1, 0, JAM2, 23, 6,
	(struct TextAttr*)&txtAttr, (UBYTE*)"ok",
	(struct IntuiText*)NULL
 };


static struct IntuiText color_text =
 {
	1, 0, JAM2, CREQ_DISTANCE_Y, CREQ_HEADER_Y, (struct TextAttr*)&txtAttr, 
	(UBYTE*)"Color-Requester:", (struct IntuiText*)NULL
 };

static SHORT cborder_oc_pairs[10] = 
  {  0,0, CREQ_OC_WIDTH-1,0, CREQ_OC_WIDTH-1,
     CREQ_OC_HEIGHT-1, 0,CREQ_OC_HEIGHT-1, 0,0 };

static struct Border cborder_oc =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    cborder_oc_pairs, NULL
 };

static struct IntuiText col_rgb_text_r =
 {
	1, 0, JAM2, CREQ_RGB_LENGTH+2, CREQ_RGB_HEIGHT-8,
	(struct TextAttr*)&txtAttr, (UBYTE*)"R", (struct IntuiText*)NULL
 };

static struct IntuiText col_rgb_text_g =
 {
	1, 0, JAM2, CREQ_RGB_LENGTH+2, CREQ_RGB_HEIGHT-8,
	(struct TextAttr*)&txtAttr, (UBYTE*)"G", (struct IntuiText*)NULL
 };

static struct IntuiText col_rgb_text_b =
 {
	1, 0, JAM2, CREQ_RGB_LENGTH+2, CREQ_RGB_HEIGHT-8,
	(struct TextAttr*)&txtAttr, (UBYTE*)"B", (struct IntuiText*)NULL
 };

static SHORT cborder_rgb_pairs[10] = 
  {  0,0, CREQ_RGB_LENGTH-1,0, CREQ_RGB_LENGTH-1,
     CREQ_RGB_HEIGHT-1, 0,CREQ_RGB_HEIGHT-1, 0,0 };

static struct Border cborder_rgb_r =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    cborder_rgb_pairs, NULL
 };

static struct Border cborder_rgb_g =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    cborder_rgb_pairs, NULL
 };

static struct Border cborder_rgb_b =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    cborder_rgb_pairs, NULL
 };

/* die naechsten 3 PropInfo's *nicht* static wg. gatget.c/follow_pot_gad */
/* werden in am_requ.i extern definiert */
struct PropInfo cprop_r_info =
 {
    AUTOKNOB | FREEHORIZ,
    0,0,
    4095,0,
    0,0,0,0,0,0
 };

struct PropInfo cprop_g_info =
 {
    AUTOKNOB | FREEHORIZ,
    1000,0,
    4095,0,
    0,0,0,0,0,0
 };

struct PropInfo cprop_b_info =
 {
    AUTOKNOB | FREEHORIZ,
    10000,0,
    4095,0,
    0,0,0,0,0,0
 };


static struct Gadget color_red_gadg =
  {
    NULL,
    CREQ_OK_X, CREQ_R_Y, CREQ_RGB_LENGTH, CREQ_RGB_HEIGHT,
    GADGHNONE,
    GADGIMMEDIATE | RELVERIFY,
    PROPGADGET | REQGADGET,
    (APTR)&cborder_rgb_r,
    NULL,
    &col_rgb_text_r,
    0L,
    (APTR)&cprop_r_info,
    COL_RED_NR,
    NULL
  };

static struct Gadget color_green_gadg =
  {
    &color_red_gadg,
    CREQ_OK_X, CREQ_G_Y, CREQ_RGB_LENGTH, CREQ_RGB_HEIGHT,
    GADGHCOMP,
    GADGIMMEDIATE | RELVERIFY,
    PROPGADGET | REQGADGET,
    (APTR)&cborder_rgb_g,
    NULL,
    &col_rgb_text_g,
    0L,
    (APTR)&cprop_g_info,
    COL_GREEN_NR,
    NULL
  };

static struct Gadget color_blue_gadg =
  {
    &color_green_gadg,
    CREQ_OK_X, CREQ_B_Y, CREQ_RGB_LENGTH, CREQ_RGB_HEIGHT,
    GADGHBOX,
    GADGIMMEDIATE | RELVERIFY,
    PROPGADGET | REQGADGET,
    (APTR)&cborder_rgb_b,
    NULL,
    &col_rgb_text_b,
    0L,
    (APTR)&cprop_b_info,
    COL_BLUE_NR,
    NULL
  };


static struct Gadget color_cancel_gadg =
  {
    &color_blue_gadg,
    CREQ_CANCEL_X, CREQ_CANCEL_Y, CREQ_OC_WIDTH, CREQ_OC_HEIGHT,
    GADGHCOMP,
    RELVERIFY | ENDGADGET,
    BOOLGADGET | REQGADGET,
    (APTR)&cborder_oc,
    NULL,
    &col_cancel_text,
    0L,
    NULL,
    COL_CANCEL_GAD_NR,
    NULL
  };


static struct Gadget color_ok_gadg =
  {
    &color_cancel_gadg,
    CREQ_OK_X, CREQ_OK_Y, CREQ_OC_WIDTH, CREQ_OC_HEIGHT,
    GADGHCOMP,
    RELVERIFY | ENDGADGET,
    BOOLGADGET | REQGADGET,
    (APTR)&cborder_oc,
    NULL,
    &col_ok_text,
    0L,
    NULL,
    COL_OK_GAD_NR,
    NULL
  };


static SHORT cborder_01_pairs[10] = 
  {  0,0, CREQ_COL_WIDTH-1,0, CREQ_COL_WIDTH-1,
     CREQ_COL_HEIGHT-1, 0,CREQ_COL_HEIGHT-1, 0,1 };

static struct Border cborder_01 =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    cborder_01_pairs, NULL
 };

static struct Gadget color_0_gadg =
  {
    &color_ok_gadg,
    CREQ_COL_0_X, CREQ_COL_0_Y, CREQ_COL_WIDTH, CREQ_COL_HEIGHT,
    GADGHBOX,
    RELVERIFY,
    BOOLGADGET | REQGADGET,
    (APTR)&cborder_01,
    NULL,
    NULL,
    0L,
    NULL,
    COL_0_GAD_NR,
    NULL
  };


static struct Gadget color_1_gadg =
  {
    &color_0_gadg,
    CREQ_COL_1_X, CREQ_COL_1_Y, CREQ_COL_WIDTH, CREQ_COL_HEIGHT,
    GADGHBOX,
    RELVERIFY,
    BOOLGADGET | REQGADGET,
    (APTR)&cborder_01,
    NULL,
    NULL,
    0L,
    NULL,
    COL_1_GAD_NR,
    NULL
  };



static SHORT color_border_pairs[18] = 
  {  CREQ_LOOK_X, 0,
     CREQ_WIDTH-CREQ_LOOK_X-1, 0, 
     CREQ_WIDTH-1, CREQ_LOOK_Y,
     CREQ_WIDTH-1, CREQ_HEIGHT-CREQ_LOOK_Y-1,
     CREQ_WIDTH-CREQ_LOOK_X-1, CREQ_HEIGHT-1,
     CREQ_LOOK_X,CREQ_HEIGHT-1, 
     0, CREQ_HEIGHT-CREQ_LOOK_Y-1, 
     0, CREQ_LOOK_Y,
     CREQ_LOOK_X,0
  };

static struct Border color_border =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)9,
    color_border_pairs, NULL
 };


static struct Requester color_request = 
  { 
    NULL,
    0, 0,	/* wird beim Aufruf belegt */
    CREQ_WIDTH, CREQ_HEIGHT,
    0, 0,
    &color_1_gadg,
    &color_border,
    &color_text,
    0,
    0,
    NULL,
    {0},
    NULL,
    NULL,
    {0}
  };

#endif /* REQ_LIBRARY */



/****************************************************************************/


struct MyTextStruct {
  short xKoo, yKoo;	/* 10, 10 ist Zeile 1, Spalte 1 */
  struct IntuiText txt;
};


#define ABOUTREQNUM 14

static struct MyTextStruct AboutReqText[] = {
   180,   6, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "Pas",								NULL },
   208,   6, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "T",								NULL },
   215,   9, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "E",								NULL },
   224,   6, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "X  ---  S h o w D V I",					NULL },
   298,  20, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "by",								NULL },
   250,  30, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "Georg Heßmann",						NULL },
   100,  44, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "Copyright © 1990-1995, All Rights Reserved.",			NULL },
    30,  59, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "The programs ShowDVI, DVIprint and Flib are freely",		NULL },
    30,  69, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "distributable copyrighted software (see the README file).",	NULL },
    30,  82, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "See also the GNU general public license (COPYING file).",	NULL },
    30,  95, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "If you have any hints, bugs, or questions about",		NULL },
    30, 105, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "the programs, ask in comp.sys.amiga.applications,",		NULL },
    30, 115, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "de.comp.sys.amiga.misc or contact me.",			NULL },
   190, 130, { 1, 0, JAM1, 0, 0, &AbWintxtAttr, "email: hessmann@informatik.uni-hamburg.de",			NULL },
};




/********************************************************************/




#define YOFFLACE		16
#define YOFFNOLA		10

#define NORM_HLACE		130
#define NORM_HUNLA		160

#define ABOUT_WIDTH		500
#define ABOUT_HEIGHT		((is_lace) ? (2 * NORM_HLACE) : (NORM_HUNLA))

#if 0	/* wird nicht mehr gebraucht */
#define ABOUT_X_KOO		((x_win_width-ABOUT_WIDTH)/2)
#define ABOUT_Y_KOO		((x_win_height-ABOUT_HEIGHT)/2)
#endif

#define ABOUT_OK_WIDTH		(CREQ_OC_WIDTH)
#define ABOUT_OK_HEIGHT		((is_lace) ? (CREQ_OC_HEIGHT) : (CREQ_OC_HEIGHT_NL))
#define ABOUT_OK_X		((ABOUT_WIDTH-ABOUT_OK_WIDTH)/3)
#define ABOUT_HELP_X		(2*((ABOUT_WIDTH-ABOUT_OK_WIDTH)/3))
#define ABOUT_OK_Y		(ABOUT_HEIGHT - (3*ABOUT_OK_HEIGHT)/2 \
					+ ((is_lace) ? 0 : 3))


#define ABOUT_HELP_STRING	" help "
#define ABOUT_ABOUT_STRING	" about"
#define ABOUT_MORE_STRING	" more "


static struct IntuiText about_ok_text = 
  {  1, 0, JAM2, 23, 6,
     (struct TextAttr*)&txtAttr, (UBYTE*)"ok",
     (struct IntuiText*)NULL
  };

static struct IntuiText about_help_text = 
  {  1, 0, JAM2, 7, 6,
     (struct TextAttr*)&txtAttr, (UBYTE*)NULL,
     (struct IntuiText*)NULL
  };

static SHORT about_border_no_pairs[10] = 
  {  0,0, ABOUT_OK_WIDTH-1,0, ABOUT_OK_WIDTH-1,
     3, 0,3, 0,0 };

static SHORT about_border_ok_pairs[10] = 	/* 3-> zu Laufzeit eingesetzt */
 { -2,-2, ABOUT_OK_WIDTH+1,-2, ABOUT_OK_WIDTH+1,3,
   -2,3, -2,-2
 };


static struct Border about_border_no =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    about_border_no_pairs, NULL
 };

static struct Border about_border_ok =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    about_border_ok_pairs, &about_border_no
 };

static struct Gadget about_help_gadg =
  {
    NULL,
    ABOUT_HELP_X, 0, ABOUT_OK_WIDTH, 0,
    GADGHCOMP,
    RELVERIFY,
    BOOLGADGET | REQGADGET,
    (APTR)&about_border_no,
    NULL,
    &about_help_text,
    0L,
    NULL,
    ABOUT_HELP_GAD_NR,
    NULL
  };


static struct Gadget about_ok_gadg =
  {
    &about_help_gadg,
    ABOUT_OK_X, 0, ABOUT_OK_WIDTH, 0,
    GADGHCOMP,
    RELVERIFY | ENDGADGET,
    BOOLGADGET | REQGADGET,
    (APTR)&about_border_ok,
    NULL,
    &about_ok_text,
    0L,
    NULL,
    ABOUT_OK_GAD_NR,
    &about_help_gadg /*NULL*/
  };

static SHORT about_border_pairs[10] = 
 { 0,0, ABOUT_WIDTH-1,0, ABOUT_WIDTH-1,3,	/* 3-> zu Laufzeit eingesetzt */
   0,3, 0,0
 };

static struct Border about_border =
 {
    (SHORT)0, (SHORT)0,
    (SHORT)1, (SHORT)0, JAM1, (SHORT)5,
    about_border_pairs, NULL
 };


#if 0	/* dies wird nicht mehr gebraucht! */
static struct IntuiText about_text =
 {
	1, 0, JAM2, ABOUT_WIDTH/2, 200, (struct TextAttr*)&txtAttr, 
	(UBYTE*)"About-Requester:", (struct IntuiText*)NULL
 };

static struct Requester about_request = 
  { 
    NULL,
    0, 0,	/* wird beim Aufruf belegt */
    ABOUT_WIDTH, 0,
    0, 0,
    &about_ok_gadg,
    &about_border,
    NULL /*&about_text*/,
    0,
    0,
    NULL,
    {0},
    NULL,
    NULL,
    {0}
  };
#endif

static struct IntuiText end_body_text =
 {
	0, 1, JAM2, 14, 15,
	(struct TextAttr*)&txtAttr, (UBYTE*)"... really exit the program?",
	(struct IntuiText*)NULL
 };


static struct IntuiText end_positive_text =
 {
	0, 1, JAM2, 7, 4,
	(struct TextAttr*)&txtAttr, (UBYTE*)"Yes",
	(struct IntuiText*)NULL
 };


static struct IntuiText end_negative_text =
 {
	0, 1, JAM2, 7, 4,
	(struct TextAttr*)&txtAttr, (UBYTE*)"Oh, No!",
	(struct IntuiText*)NULL
 };


