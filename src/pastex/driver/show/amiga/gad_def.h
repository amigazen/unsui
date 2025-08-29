#ifdef AMIGA


extern struct Remember *Rem_Key;	/* Memory-Management */


extern struct Gadget	//first_Page_Gad,
			//prev_Page_Gad,
			//succ_Page_Gad,
			//last_Page_Gad,
			//dir_Gad,
			//fil_Gad,
			//int_Gad,
			* pg_pot_Gad,		// BOOPSI
			* pg_left_Gad,
			* pg_right_Gad,
			* pg_int_Gad,
			potx_Gad,
			poty_Gad,
			potx_left_Gad,
			potx_right_Gad,
			poty_up_Gad,
			poty_up2_Gad,
			poty_down_Gad,
			poty_down2_Gad;
			//pgscroll_Gad,
			//pgscroll_no_Gad,
			//pgscroll_ok_Gad;

extern struct PropInfo  potx_PropInfo,
			poty_PropInfo;

extern struct Image	potx_Image,
			poty_Image;


/* Gadget-ID Nummern:					*/
#define PG_POT_GAD_NR			(WIN2_GADS+1)
#define PG_LEFT_GAD_NR			(WIN2_GADS+2)
#define PG_RIGHT_GAD_NR			(WIN2_GADS+3)
#define PG_INT_GAD_NR			(WIN2_GADS+4)

#define POTX_PFEIL_LINKS_GAD_NR		(WIN2_GADS+5)
#define POTX_PFEIL_RECHTS_GAD_NR	(WIN2_GADS+6)
#define POTX_GAD_NR			(WIN2_GADS+7)
#define POTY_PFEIL_OBEN_GAD_NR		(WIN2_GADS+8)
#define POTY_PFEIL_OBEN2_GAD_NR		(WIN2_GADS+9)
#define POTY_PFEIL_UNTEN_GAD_NR		(WIN2_GADS+10)
#define POTY_PFEIL_UNTEN2_GAD_NR	(WIN2_GADS+11)
#define POTY_GAD_NR			(WIN2_GADS+12)

#if 0
/* Gadgets im Color-Requester */
#define COL_CANCEL_GAD_NR		20
#define COL_OK_GAD_NR			21
#define COL_RED_NR			22
#define COL_GREEN_NR			23
#define COL_BLUE_NR			24
#define COL_0_GAD_NR			25
#define COL_1_GAD_NR			26
#endif

/* Gadgets im About-Requester */
#define ABOUT_OK_GAD_NR			(WIN2_GADS+13)	/* sind nicht im WIN2 !! */
#define ABOUT_HELP_GAD_NR		(WIN2_GADS+14)



/* Gadget x-Koo.		*/
#define PA1_LEFT_EDGE 			40			/*  40 */
#define PA2_LEFT_EDGE 			(PA1_LEFT_EDGE+16+6)	/*  62 */
#define PA3_LEFT_EDGE 			(PA2_LEFT_EDGE+8+8)	/*  78 */
#define PA4_LEFT_EDGE 			(PA3_LEFT_EDGE+8+6)	/*  92 */
#define DIR_LEFT_EDGE 			(PA4_LEFT_EDGE+16+24+4)	/*  */
#define STR_LEFT_EDGE 			(DIR_LEFT_EDGE+160+16)	/*  */
#define INT_LEFT_EDGE 			(STR_LEFT_EDGE+104+16)	/*  */
#define BOK_LEFT_EDGE 			620	/* schmarrn.. */

/* Dicke der beiden Scrollbars	*/
#define WIDTH_SCROLL			16
#define HEIGHT_SCROLL			10
#define HEIGHT_UD_ARROW			11
#define WIDTH_RL_ARROW			16
#define POTX_IM_HEIGHT			4
#define POTY_IM_WIDTH			12

#define WIDTH_PAGEX_GAD			280	/* Seitenscroll-Gad unten links */

#define WIDTH_SCROLL_BAR		 WIDTH_SCROLL
#define HEIGHT_SCROLL_BAR		 HEIGHT_SCROLL
#define WIDTH_X_GADGETS			 WIDTH_SCROLL
#define HEIGHT_Y_GADGETS		 HEIGHT_SCROLL
#define WIDTH_BLATT_GADGET		 WIDTH_SCROLL_BAR
#define WIDTH_OK_BLATT_GADGET		 WIDTH_SCROLL
#define HEIGHT_BLATT_GADGET		 HEIGHT_SCROLL_BAR
#define HEIGHT_OK_BLATT_GADGET		 HEIGHT_SCROLL



/* shortcuts */
#define MAXPOT_H	((MAXPOT)>>1)


#define TIME_DOUBLE	1000000

/* Funktionen die aus gadget.c exportiert werden: */
/***
void init_images();
void init_gad();
long check_gad();
long follow_gad();
void set_zahl();
void Add_all_gadgets();
void Add_system_gadgets();
void Add_scroll_gadgets();
void Remove_scroll_gadgets();
***/

#endif
