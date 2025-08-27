/* This file defines all useful external variables. It's automatically */
/* included by globals.h, thus every module includes it too. */

#ifndef MAIN
#define EX extern
#else
#define EX
#endif

#include <dos/dos.h>

/**********************************************************************/
/*************************  Global Variables  *************************/
/**********************************************************************/

#ifndef DISPLAY
			/*   -------	Globals only in DVIprint  ---------	*/

EX long	 bufflen;		/* printergraphics line buffer		*/
EX char	*o_printer_name;	/* which printer?			*/
EX long	 last_form_feed;	/* a FormFeed at the last page? 	*/
EX long	 o_optimize;		/* Optimize printer ouput option	*/
EX long	 Reverse;		/* process DVI pages in reverse order ? */
EX long  unidirect;		/* print direction for hardcopy 	*/
EX long  draft;			/* quality for hardcopy 		*/
EX char *output_file_name;	/* name of the output file		*/
EX short full_page_in_ram;	/* must the whole page-bitmap into ram? */
EX short print_page_numbers;	/* print: 0=all, 1=odd, 2=even page nrs */
EX short print_width;		/* nr of lines printed per pass 	*/
EX short nr_printed_pages;	/* how much pages are printed		*/
EX long  o_showprinter;		/* show printer definition file		*/
EX long	 do_accounting;		/* should I do accounting		*/

EX FILE	*output_file_ptr;	/* output file pointer			*/
EX long	 ncopies;		/* number of copies to print		*/
EX long  NrOfPagesToPrint;	/* number of pages to print		*/
EX long  PhyPageNumbers;	/* from/to are physical numbers		*/

EX long  bookmode;		/* bookmode (not implemented)           */

EX long  usegui;		/* open a GUI 				*/
EX ULONG GUIsignals;		/* signal mask for the GUI		*/

/*	Schau, was Du hier loeschen kannst.	*/
#if 0
 #if 0
 EX int	 g_logfile;		/* Are these messages going to a log file? */
 #endif
EX int	 printer_type;		/* printer-type for hardcopy		*/
EX int	 output_to_file;	/* should output redirected to a file	*/
EX int	 bitsperlong;
EX int	 g_errenc;		/* has an error been encountered?	*/
#endif

#ifdef AMIGA
EX long	 o_density;		/* preferences printer density		*/
EX long  iffprint;		/* print to iff-file			*/
EX long  turbo_mode;		/* print in TURBO mode? 		*/
EX short os_2;			/* do we work with AMIGA OS 2.0 or greater */
#endif /* AMIGA */



#else /* DISPLAY */
			/* -----------	Globals only in ShowDVI	  ------------	*/

EX FILE *pkfile;
EX long	 bytestoread;
EX long  max_page_number;	/* max. number of pages in the actual file */
EX long  current_page;        	/* number of the actual page *//*hes:CurrentPage gibt es auch noch !*/
EX short corrupt;		/* DVI file corrupted		      */
EX short use_phy_number;	/* use phy numbers insteat of logical */

EX WORD Page_LeftPoint;		/* Eck-Koordinaten der dargestellten Seite */
EX WORD Page_RightPoint;
EX WORD Page_TopPoint;
EX WORD Page_BottomPoint;

EX short AddXpixel;		/* wieviele Pixel kommen auf Alignment-Gruende zur Bitmap hinzu... */

EX short InSearchMode;		/* wird gerade nur gesucht? Wichtig fuer SetChar() */
EX long  SearchStartPage;
EX long  SearchFoundPage;

#include <graphics/gfx.h>
EX struct Rectangle * SearchRect;	/* wenn etwas gefunden wurde, dann steht dort wo */
EX struct Rectangle * SearchFoundRect;	/* da wird es bis zum Ende der Seite gespeichert */

#endif /* DISPLAY */



		/*   ------------	Globals to both programs  -----------	*/

EX long 	current_page_phy;	/* number of the actual physical page */
EX char       * ArgPubname;		/* PubScreen Name als Argument 		*/

EX char	 	filename[STRSIZE];	/* DVI file name (with path)		*/
EX char	 	dirname[STRSIZE];	/* current path of DVI file		*/
EX char       * g_progname;		/* program name	(*not* an array!)	*/
EX int          MaxDviBufSize;		/* should the DVI-File copied int RAM	*/
EX DVIFILE    * dvifp; 			/* DVI file pointer			*/
EX struct DateStamp dvidatestamp;	/* Zeit des DVIfiles			*/
EX FILE	      * g_logfp;		/* log file pointer (for errors)        */
EX long	 	g_authors;		/* print author names			*/
EX long	 	FirstPage;		/* first page to print (uses count0)    */
EX long	 	LastPage;		/* last page to print			*/
EX long	 	CurrentPage;		/* the current page number		*/
EX long         NumOfPages;		/* number of pages in DVI-file		*/
EX long	 	PreLoad;		/* preload the font descriptions?	*/
EX long	 	resolution;		/* resolution				*/
EX int	 	hconvresolution;
EX int	 	vconvresolution;
EX int	 	hmaxdrift;		/* max pixels away from true rounded position */
EX int	 	vmaxdrift;		/* max pixels away from true rounded position */
EX int	 	thinspace;		/* needet in dvihand.c for rounding	*/
EX int	 	backspace;
EX int	 	vertsmallspace;	/* also ^^				*/
EX int	 	bytesperlong;
EX double 	alpha;		/* conversion ratio, DVI unit per TFM unit */
EX long	 	den;			/* denominator specified in preamble	*/
EX long  	h;			/* current horizontal position		*/
EX long  	hh;			/* current h on device			*/
EX long  	v;			/* current vertical position		*/
EX long  	vv;			/* current v on device			*/
EX long  	mag;			/* magnification specified in preamble	*/
EX long  	num;			/* numerator specified in preamble	*/
EX int   	pheight_pt;	        /* real height in pt			*/
EX int   	pwidth_pt;	        /* real width in pt			*/
EX int	 	hoffset, voffset;	/* h. and vert. page offset in pixel	*/
EX long	 	paper_height, paper_width;
EX float	user_paper_height_in, user_paper_width_in;
EX long	 	hconv, vconv;		/* converts DVI units to pixels 	*/
EX long  	ppagep;		/* previous page pointer		*/
EX long  	postambleptr;		/* Pointer to the postamble		*/
EX char	      * PXLpath;		/* new pxl-area ('FONTDIR')		*/
EX long	 	fontmemsize;
EX long	 	maxbitmapsize;		/* maximal size of bitmap		*/
EX long	 	g_quiet;		/* for quiet operation			*/
EX long		mark_fonts;		/* markiere alle benoetigten Fonts      */
EX long  	Stats;			/* more output to log file		*/
EX long  	DebugStats;		/* flush stats file			*/
EX char	      * g_Logname;		/* name of log file, if created 	*/
EX int		g_logging;		/* Are we logging ?			*/
EX long	 	ToDo;			/* libs/devices/files to process,	*/
				/* see flags in globals.h		*/

EX long         landscape;		/* print in landscape modus?		*/
EX long		twopage;		/* two pages at once			*/
EX long		leftpage;		/* current page leftpage of twopage?	*/

EX long		start_specialhost;	/* start specialhost automatically	*/

EX float	moffset_in_fix;		/* twopage mid-offset			*/
EX int		moffset_is_true;
EX float	moffset_in;		/* mid-offset in inch			*/
EX int		moffset;		/* moffset in pixel			*/


/* Die zwei folgenden Variablen halten den h/v-offset, so wie er eingegeben wurde. */
EX float hoffset_in_fix;	/* hoffset in inch			*/
EX float voffset_in_fix;	/* voffset in inch			*/
EX int   hoffset_is_true;	/* hoffset in true inch			*/
EX int   voffset_is_true;	/* hoffset in true inch			*/

/* Das ist der jeweils geltende h/v-offset, angepasst an die Magnification.	*/
EX float hoffset_in;		/* hoffset in inch			*/
EX float voffset_in;		/* voffset in inch			*/


/* Folgende zwei Variablen legen den (0,0) Punkt innerhalb der Bitmap fest		 */
/* Die Werte werden beim Zeichnen der Buchstaben/Linien, sowie bei den Bildern benoetigt */
/* Gesetzt werden sie laut den hoff/voff. Besonderer Fall ist bei twoup. Dort wird	 */
/* der Wert je nach linker/rechter Seite geaendert.					 */
EX long  OffsetBitmap_X;
EX long  OffsetBitmap_Y;

#ifdef DEBUG
EX   int DeBug;
#endif

#ifdef AMIGA
EX int Enable_Abort;		/* Ctrl-C --> _abort			*/
EX long task_priority;
EX long old_task_priority;
#endif

/*	Schau, was Du hier loeschen kannst.	*/
EX char m_string[STRSIZE];	/* messagestring for warnings and fatal errors */
