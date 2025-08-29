#ifndef DISPLAY

#ifdef ATARI
#define PRINTER_CONFIG	"DVIprint.prt"
#else
#define PRINTER_CONFIG	"DVIprint.printers"
#endif

#define FS			((char)28)
#define ESC			((char)27)
#define LF			((char)10)
#define CR			((char)13)
#define FF			((char)12)

/* Spaces replace a blank graphic when there are more than : */
#define SPACES_GUT		6

/* ESC $ is only used when there are more points free than : */
#define POINT_GUT		10





/****** P R I N T E R -- T Y P E S ** pp_printer ************************/

#define SLM_804			0	/* Atari Laser Printer SLM 804	*/
#define GENERIC_PRINTER		0	/* AMIGA printer.device driver  */
#define DESKJET			1	/* HP-DeskJet			*/
#define CHEAPDJ			2	/* HP-DeskJet saves ink		*/
#define LASERJET		3	/* HP-LaserJet			*/
#define LASERJET4		4	/* HP-LaserJet -- 4 (600dpi)	*/
#define CANON			5	/* Canon Laserwriter		*/
#define STANDARD_PRINTER	6	/* Hardcopy_standard() printer	*/

#ifdef AMIGA
# define DEFAULT_PRINTER	GENERIC_PRINTER
#else
# define DEFAULT_PRINTER	SLM804
#endif




/****** P R I N T E R -- R E S O L U T I O N ** pp_draft ****************/

#define HIGH_Q			0
#define DRAFT_Q			1
#define NO_MATTER_DRAFT		2

#define DEFAULT_QUALITY		HIGH_Q





/****** P R I N T E R -- D I R E C T I O N ** pp_unidir *************************/

#define UNIDIR			0			/* default		*/
#define BIDIR			1			/* bidirectional	*/

#define DEFAULT_DIRECTION	UNIDIR





/****** O P T I M I Z A T I O N	** pp_secure ***************/

#define NO_MATTER_OPT		0
#define SECURE			1
#define OPTIMIZE		2




/****** S P E C I A L ** pp_special ***************/

#define SPECIAL_NO_REDIR	(1<<1)	/* no output redirection	*/
					/* allowed (prefs).		*/
#define SPECIAL_PREFS		(1<<2)	/* generic printer treatment	*/
#define SPECIAL_STD		(1<<3)	/* Standard Hardcopy printer	*/



/****** M E T H O D ** pp_method ***************/

#define METHOD_MOVE_POINT	1	/* moves to n/60 inch		*/
#define METHOD_GFX_ONLY		2	/* whole line as graphics	*/
#define METHOD_USE_SPACES	3	/* use spaces in big blanks	*/



/******	P R I N T E R -- P A R A M E T E R S ********************/

/* If you want to use the standard Hardcopy driver, you must set pp_grouping != 0 */

struct printer_para {
 char	*pp_name;
 char	*pp_ID_string;		/* describes this printer	*/
 short   pp_method;		/* blanking method		*/
 short   pp_printer;		/* printer			*/
 short	 pp_draft;		/* draft ?			*/
 short   pp_secure;		/* optimize or not		*/
 short   pp_unidir;		/* unidirectional ? 		*/
 short   pp_special;		/* special (??)			*/
 short	 pp_passes_height;	/* Total of skips: Bitmap must be multiple of height */
 short	 pp_xdpi;		/* X resolution			*/
 short	 pp_ydpi;		/* Y resolution			*/
 short	 pp_init_len;		/* string length		*/
 short	 pp_exit_len;		/* string length		*/
 short	 pp_grouping;		/* 1 or 3, for 8 or 24 pins	*/
 short	 pp_passes;		/* [1..]			*/
 short	 pp_skip_cmd_len;	/* length of command		*/
 short	 pp_gfx_cmd_len;	/* length of command		*/
 short	 pp_pica_width;		/* Printer width in pica chars	*/
 char	*pp_skip;		/* Array of skips for each pass	*/
 char	*pp_init_string;	/* Initialize printer on startup */
 char	*pp_exit_string;	/* Initialize printer on exit	*/
 char	*pp_skip_cmd;		/* ESC J or ESC 3		*/
 char	*pp_gfx_cmd;		/* Esc * ...			*/
 long	 pp_buffer_size;	/* -e command buffer size	*/
};


/******	B I T M A P -- C O N F I G U R A T I O N ****************/

/* The bitmap is read by accesses of type : */
typedef unsigned short  WP_TYP;			/* Die Bitmap wird Wortweise gelesen */
#define WP_GROESSE	(sizeof(WP_TYP)*8)	/* Bits in einem Wort */

#define MAX_GROUPING	12			/* Bytes pro Zeile */


/* default size of the graphics line buffer */
#ifdef AMIGA
# define BUFFLEN 	10240L
#endif



#define PP_INS_DEC		((char)0x82)
#define PP_INS_LOW		((char)0x80)
#define PP_INS_HIGH		((char)0x81)
#define PP_INS_NR100		((char)0x83)
#define PP_INS_NR10		((char)0x84)
#define PP_INS_NR1		((char)0x85)
#define PP_NORMAL_STRING	(PP_INS_HIGH<<8 | PP_INS_LOW)

/*
**	The command strings above accept two special characters:
**
**		0xff		low order byte of numeric argument
**
**		0xfe		high order byte
**
**	Examples:	{ESC,'*','\0',0xff,0xfe}
**			{0x0d,ESC,'J',0xff,0x0a}
*/

#endif	/* !DISPLAY*/
