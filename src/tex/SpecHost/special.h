/*
**	SpecialHost for PasTeX
**
**	Copyright © by Olaf Barthel & Georg Heﬂmann
*/

/* special.h */

#include <dos/dos.h>

#define MAGIC_WORD		(('S'<<24) | ('P'<<16) | ('E'<<8) | 'C')

#define SPECIAL_PORT		"special_dvi"
#define SPECIAL_REPLY		"special_reply"

/* 4 commands for \special-strings */
#define AC_SEND_SPECIAL		1
#define AC_REPLY_SPECIAL	2
#define AC_OK_BITMAP		3
#define AC_REPLY_BITMAP		4

/* 2 commands for tpic commands */
#define AC_SEND_TPIC		5
#define AC_REPLY_TPIC		6

/* SpecialHost don't know the current action */
#define AC_REPLY_UNKNOWN	7


#define LOC_NONE		0	/* no picture available		     */
#define LOC_BITMAP		1	/* bimap in RAM			     */
#define LOC_FILE		2	/* bitmap in a file		     */
#define LOC_BORDER		3	/* no picture, draw only a border    */
#define LOC_RECTANGLE		4	/* no picture, draw a full rectangle */
					/* LOC_BORDER || LOC_RECTANGLE 	     */
					/*		=> loc.map == NULL   */
#define LOC_BITMAP_BORDER	5	/* bitmap in RAM + draw border	     */
#define LOC_FILE_BORDER		6	/* bitmap in file + draw border	     */



/* TPIC commands */
/* only the '!' commands call SpecialHost */

#define TPIC_NO_TPIC	0
#define TPIC_PN		1 
#define TPIC_PA 	2
#define TPIC_FP 	3		/* ! */
#define TPIC_IP 	4		/* ! */
#define TPIC_DA 	5		/* ! */
#define TPIC_DT		6		/* ! */
#define TPIC_SP 	7		/* ! */
#define TPIC_SPB	8		/* ! (same as _SP) */
#define TPIC_AR 	9		/* ! */
#define TPIC_IA 	10		/* ! */
#define TPIC_SH		11
#define TPIC_SHB	12
#define TPIC_WH		13
#define TPIC_BK		14
#define TPIC_TX		15


struct tpic_msg {
		char	tpic_com;
		char	whiten;
		char	blacken;
		char	pad;
		long    shade;				/* mult 10000 */
		long	opt_long[4];			/* options to the tpic command */
		char	opt_float[2][12];		/* float werden als String uebergeben! */
		long	pen_size;
		long	path_len;
		long   *xx;
		long   *yy;
	};


struct driver_map {
		/* cursor */
		long	x;			/* actual cursor position   */
		long	y;			/* in the bitmap.	    */
		/* bitmap */
		long	width;			/* width of bitmap in bits  */
		long	height;			/* height of bitmap in bits */
		long	lower_limit;
		long	upper_limit;
		long   *pixptr;			/* pointer to bitmap	    */
		long    null_x;			/* (0,0) of the TeX page    */
		long    null_y;
		long	page_width;		/* width of the full page   */
		long	page_height;		/* height of the full page  */
		long	pass;			/* number of current pass   */
		long    passes;			/* number of passes         */
	};

/**  Bitmap:

	     width
        +---------------+
	|		|
 	|		|
	|		|
	|		|
	|		|
     ----------------------	upper_limit      \
	|		|                         |
	|		|                          - height
	|		|                         |
     ----------------------	lower_limit      /
	|		|
	|		|
	|		|
	|		|
        +---------------+

  Bitmap is only between upper_limit and lower_limit in memory!

*****/





union  location {
		unsigned long	*map;		/* picture as bitmap, lines with word (2Byte) alignment */
		char 		*filename;	/* picture as file */
	};


struct special_map {
		short		 where_is;	/* LOC_#? */
		long		 hoffset;	/* all in pixel */
		long		 voffset;
		long		 width;		/* in pixel (bitmap word-alignment!) */
		long		 height;	/* in pixel (bitmap word-alignment!) */
	union	location	 loc;
		long		 reserved1;
		long		 reserved2;
		long		 reserved3;
		long		 reserved4;
	};

struct special_msg {
	struct	Message		 msg;			/* 20 Bytes */
		short		 action;		/* 22 */
		short		 ret;			/* 24 */
		char		*special_string;	/* 28 */
		long		 hresolution;		/* 32 */
		long		 vresolution;		/* 36 */
	struct	tpic_msg	*tpic;			/* 40, if != NULL => tpic message */
	struct	driver_map	*dmap;			/* 44 */
	struct	special_map	*bmap;			/* 48 bytes */
		BPTR		 DVIdirLock;		/* 52, new since V1.26 */
							/* Lock to the directory, where the DVI-File resists */
							/* watch out for msg->msg.mn_Length to check, if */
							/* this field exists! (NULL != SYS:) */

		long		 DVImagnification;	/* 56 bytes */
							/* new since SpecialHost V1.21, ShowDVI V1.39ﬂ */
							/* (global DVI magnification, in thousandth of magnification */
		char		*DVIfilename;		/* 60 bytes */
							/* new since SpecialHost V1.23, ShowDVI V1.39ﬂ */
							/* (DVI file name) */
	struct	DateStamp	*DVIfiledate;		/* 64 bytes */
		long		 DVIcurphypage;		/* 68 bytes physical page number */
};


/* functions */

struct special_map *send_special(char *sp_string);
void 		    send_tpic(struct tpic_msg *tp);
void		    special_ok(void);

