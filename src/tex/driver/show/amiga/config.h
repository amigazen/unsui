/** config.h **/


/* path of the config-file */

/* name of the config-file */
#define CONFIG_NAME	"ShowDVI.config"


/* numbers of all possible values in the config-file */
/* ungerade bedeutet: on/off value	*/
#define NR_NO_KEYWORD	0
#define NR_COLOR_0	2
#define NR_COLOR_1	4
#define NR_SCR_BAR	(6+1)
#define NR_LACE		8+1
#define NR_BASE_DPI	10
#define NR_DPI_MENU	12
#define NR_BEEP		(14+1)
#define NR_ESCEX	(16+1)
#define NR_IMENU	(18+1)
#define NR_BIG_MENU	(20+1)
#define NR_UNIT		22
#define NR_SCR_MODE	24
#define NR_USE_PHY	(26+1)
#define NR_APP_WIN	(28+1)
#define NR_APP_NAME	30
#define NR_OS_MENU	(32+1)
#define NR_AUTO_AGAIN	(34+1)
#define NR_SCREEN_SIZE	36
#define NR_USECOL4	(38+1)
#define NR_COLOR_2	40
#define NR_COLOR_3	42
#define NR_COMMAND	44
#define NR_MIDMENU	(46+1)
#define NR_AREXXSTART	48
#define NR_APPICONPOS	50
#define NR_APPICON	(52+1)
#define NR_CLWBCOL	(54+1)
#define NR_QUICKQUIT	(56+1)
#define NR_MONITOR_SIZE	58
#define NR_BLINE	(60+1)
#define NR_OWN_SCREEN	(62+1)
#define NR_PUBSCR_NAME	64
#define NR_WINDOW_POS	66
#define NR_WINDOW_SIZE	68
#define NR_MY_PUBSCR_NAME 70
#define NR_WINDOW_POS_OWN_SCR	72
#define NR_WINDOW_SIZE_OWN_SCR	74
#define NR_MAXDVIBUF_SIZE	76
#define NR_ALWBMFAST		(78+1)
#define NR_SMARTWIN		(80+1)
#define NR_BACKHOOK		(82+1)
#define NR_APEN		84
#define NR_BPEN		86

/* strings for the config-file */
#define STR_COLOR_0	"color_0"		/* R,G,B */
#define STR_COLOR_1	"color_1"		/* R,G,B */
#define STR_COLOR_2	"color_2"		/* R,G,B */
#define STR_COLOR_3	"color_3"		/* R,G,B */
#define STR_SCR_BAR	"scrollbar"		/* on/off */
#define STR_LACE	"interlace"		/* on/off */
#define STR_BASE_DPI	"default_resolution"	/* dpi */
#define STR_DPI_MENU	"resolution_menu"	/* max 10 short numbers */
#define STR_BEEP	"beep"			/* on/off */
#define STR_ESCEX	"esc-exit"		/* on/off */
#define STR_IMENU	"int-menu"		/* on/off */
#define STR_BIG_MENU	"big-menu"		/* on/off */
#define STR_UNIT	"unit"			/* 'in' 'cm' or 'pt' */
#define STR_SCR_MODE	"screen-mode"		/* 'pal' 'ntsc' or 'productivity' */
#define STR_USE_PHY	"physical-numbers"	/* on/off */
#define STR_APP_WIN	"application-window"	/* on/off */
#define STR_APP_NAME	"app-icon-name"		/* string */
#define STR_OS_MENU	"popup-menu"		/* on/off */
#define STR_AUTO_AGAIN	"auto-load-again"	/* on/off */
#define STR_SCREEN_SIZE "screen-size"		/* x y (0 => clone wb-screen) */
#define STR_USECOL4	"use-four-colors"	/* on/off */
#define STR_COMMAND	"shell-script"		/* string */
#define STR_MIDMENU	"middle-menu"		/* on/off */
#define STR_AREXXSTART	"arexx-start-script"	/* string */
#define STR_APPICONPOS	"app-icon-pos"		/* nr, nr */
#define STR_APPICON	"application-icon"	/* on/off */
#define STR_CLWBCOL	"clone-wb-colors"	/* on/off */
#define STR_QUICKQUIT	"quick-exit"		/* on/off */
#define STR_MONITOR_SIZE "monitor-size"		/* x y */
#define STR_BLINE	"border-line"		/* on/off */
#define STR_OWN_SCREEN	"use-own-screen"	/* on/off */
#define STR_PUBSCR_NAME "public-screen-name"	/* name */
#define STR_WINDOW_POS	"window-pos"		/* x y */
#define STR_WINDOW_SIZE	"window-size"		/* x y */
#define STR_MY_PUBSCR_NAME	"my-public-screen-name"		/* name */
#define STR_WINDOW_POS_OWN_SCR	"window-pos-own-screen"		/* x y */
#define STR_WINDOW_SIZE_OWN_SCR	"window-size-own-screen"	/* x y */
#define STR_MAXDVIBUF_SIZE	"maximal-dvibuff-size"		/* size */
#define STR_ALWBMFAST		"page-always-in-fast-ram"	/* on/off */
#define STR_SMARTWIN		"use-smart-refresh-win"		/* on/off */
#define STR_BACKHOOK		"write-black-on-white"		/* on/off */
#define STR_APEN	"a-pen"			/* foreground pen */
#define STR_BPEN	"b-pen"			/* background pen */


typedef struct {	char *keyword;
			short keynumber;
		} keys;

keys KeyTab[] = {
	{ STR_COLOR_0,	NR_COLOR_0 },
	{ STR_COLOR_1,	NR_COLOR_1 },
	{ STR_COLOR_2,	NR_COLOR_2 },
	{ STR_COLOR_3,	NR_COLOR_3 },
	{ STR_SCR_BAR,	NR_SCR_BAR },
	{ STR_LACE,	NR_LACE },
	{ STR_BASE_DPI,	NR_BASE_DPI },
	{ STR_DPI_MENU,	NR_DPI_MENU },
	{ STR_BEEP,	NR_BEEP },
	{ STR_ESCEX,	NR_ESCEX },
	{ STR_IMENU,	NR_IMENU },
	{ STR_BIG_MENU,	NR_BIG_MENU },
	{ STR_UNIT,	NR_UNIT },
	{ STR_SCR_MODE,	NR_SCR_MODE },
	{ STR_USE_PHY,	NR_USE_PHY },
	{ STR_APP_WIN,	NR_APP_WIN },
	{ STR_APP_NAME,	NR_APP_NAME },
	{ STR_OS_MENU,	NR_OS_MENU },
	{ STR_AUTO_AGAIN,  NR_AUTO_AGAIN },
	{ STR_SCREEN_SIZE, NR_SCREEN_SIZE },
	{ STR_USECOL4,	NR_USECOL4 },
	{ STR_COMMAND,	NR_COMMAND },
	{ STR_MIDMENU,	NR_MIDMENU },
	{ STR_AREXXSTART,  NR_AREXXSTART },
	{ STR_APPICONPOS,  NR_APPICONPOS },
	{ STR_APPICON,	   NR_APPICON },
	{ STR_CLWBCOL,	   NR_CLWBCOL },
	{ STR_QUICKQUIT,   NR_QUICKQUIT },
	{ STR_MONITOR_SIZE,  NR_MONITOR_SIZE },
	{ STR_BLINE,	   NR_BLINE },
	{ STR_OWN_SCREEN,  NR_OWN_SCREEN },
	{ STR_PUBSCR_NAME,  NR_PUBSCR_NAME },
	{ STR_WINDOW_POS,  NR_WINDOW_POS },
	{ STR_WINDOW_SIZE,  NR_WINDOW_SIZE },
	{ STR_MY_PUBSCR_NAME,  NR_MY_PUBSCR_NAME },
	{ STR_WINDOW_POS_OWN_SCR,  NR_WINDOW_POS_OWN_SCR },
	{ STR_WINDOW_SIZE_OWN_SCR,  NR_WINDOW_SIZE_OWN_SCR },
	{ STR_MAXDVIBUF_SIZE, NR_MAXDVIBUF_SIZE },
	{ STR_ALWBMFAST, NR_ALWBMFAST },
	{ STR_SMARTWIN,  NR_SMARTWIN },
	{ STR_BACKHOOK,  NR_BACKHOOK },
	{ STR_APEN,	NR_APEN },
	{ STR_BPEN,	NR_BPEN }
 };

#define NKEYS (sizeof(KeyTab)/sizeof(KeyTab[0]))


/* strings for on/off */
#define STR_ON		"on"
#define STR_OFF		"off"


