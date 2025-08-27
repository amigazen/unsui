

#define TEX

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

extern struct DosLibrary	*DOSBase;

#include <clib/dos_protos.h>
#include <pragmas/dos_pragmas.h>

#include "GetOpt.h"
#include "GetOpt.i"


#include "little_globals.h"
#include "little_globals.i"



extern char *_ProgramName;

static void open_logfile	(void);
static void PrintHelp		(struct Options opt[], short long_hlp);
static void check_printer_type	(char *str);


/*==================================================================*/

/* Nur zum Test... */
FILE *pkfile;
long bytestoread;
int   g_authors;		/* print author names                      */
int   g_errenc;                 /* has an error been encountered?          */
char  g_Logname[STRSIZE];       /* name of log file, if created            */
int   g_logging;                /* Are we logging warning messages?        */
int   g_logfile;                /* Are these messages going to a log file? */
FILE *g_logfp;                  /* log file pointer (for errors)           */
char *g_progname;/*!*/      /* program name                            */
int   g_quiet;                  /* for quiet operation                     */
int hoffset, voffset;           /* horizontal and vertical page offset     */
int draft;                      /* quality for hardcopy                    */
int unidirect;                  /* print direction for hardcopy            */
int landscape;			/* print in landscape modus?		   */
int iffprint;			/* print to iff-file			   */
int full_page_in_ram;		/* must the whole page-bitmap into ram?	   */
int printer_type;		/* printer-type for hardcopy		   */
int print_width;		/* nr of lines printed per pass		   */
int turbo_mode;			/* print in TURBO mode?			   */
int os_2;			/* do we work with AMIGA OS 2.0 or greater */
int output_to_file;		/* should output redirected to a file	   */
char *output_file_name;		/* name of the output file		   */
FILE *output_file_ptr;		/* output file pointer			   */
static int print_page_numbers;	/* print: 0=all, 1=odd, 2=even page nrs	   */
int Stats;
int bitsperlong, bytesperlong;
char m_string[STRSIZE];         /* messagestring for warnings and fatal errors */
char  *PXLpath = "default";	/* new pxl-area			      */


int resolution;                 /* resolution */
int hconvresolution;
int vconvresolution;

int   FirstPage = -1000000L;    /* first page to print (uses count0)        */
int   LastPage = 1000000L;      /* last page to print                       */
int   CurrentPage = 0;		/* the current page number		    */

char filename[STRSIZE];                  /* DVI file name                            */

long  hconv, vconv;             /* converts DVI units to pixels             */
long  den;                      /* denominator specified in preamble        */
FILE *dvifp  = NULL;            /* DVI file pointer                         */
int   PreLoad = FALSE;          /* preload the font descriptions?           */
long  h;                        /* current horizontal position              */
long  hh = 0L;                  /* current h on device */
long  v;                        /* current vertical position                */
long  vv = 0L;                  /* current v on device */
long  mag;                      /* magnification specified in preamble      */
int   ncopies = 1;              /* number of copies to print                */
long  num;                      /* numerator specified in preamble          */
long  postambleptr;             /* Pointer to the postamble                 */
long  ppagep;                   /* previous page pointer                    */
int   Reverse = FALSE;          /* process DVI pages in reverse order ?     */
char  rootname[STRSIZE];        /* DVI filename without extension           */
int   hmaxdrift;                /* max pixels away from true rounded position */
int   vmaxdrift;                /* max pixels away from true rounded position */
int   thinspace;		/* needet in dvihand.c for rounding   */
int   vertsmallspace;		/* also ^^			      */
double alpha;                   /* conversion ratio, DVI unit per TFM unit */

int last_form_feed = TRUE;	/* a FormFeed at the last page?		    */

int paper_height, paper_width;
int pheight_pt;		        /* real height in pt		      */
int pwidth_pt;		        /* real width in pt		      */


long fontmemsize = FONTMEMSIZE;
long maxbitmapsize = MAXBITMAPSIZE;	/* maximal size of bitmap     */

int bufflen;

static double hoffset_in;	/* hoffset in inch */
static double voffset_in;	/* voffset in inch */

static int		density = 7;



/*==================================================================*/




static long help;
static long DebugStats;
static long logging;
static long onlydraft, onlycheap, optimize;
static long odd, even;
static char *printer_str = NULL;
static char *resolution_str = NULL;
static char *filenameptr = NULL;



START_PARAM(opt)
  /* req?      key-name     abbrev  type         variable       help-txt */
  NOREQ_PARAM ("HELP",      "?",    OPT_HELP,    &help,		"print help information", NULL)
  NOREQ_PARAM ("DIRECTORY", "DIR",  OPT_STRING,  &PXLpath,	"dir for fontlibs/pk-files", NULL)
  NOREQ_PARAM ("FONTMEM",   NULL,   OPT_LONG,    &fontmemsize,	"size of the fontmemory", NULL)
  NOREQ_PARAM ("MAXBITMEM", NULL,   OPT_LONG,	 &maxbitmapsize,"max size of the bitmap",
		"(0 == unlimited size)")
  NOREQ_PARAM ("PRTBUFFER", NULL,   OPT_LONG,	 &bufflen,	"size of the printer buffer", NULL)
  NOREQ_PARAM ("FROM",      "F",    OPT_LONG,    &FirstPage,	"start at page", NULL)
  NOREQ_PARAM ("TO",        "T",    OPT_LONG,    &LastPage,	"stop at page", NULL)
  NOREQ_PARAM ("ODD",       NULL,   OPT_BOOLEAN, &odd,		"print only all 'odd' pages", NULL)
  NOREQ_PARAM ("EVEN",      NULL,   OPT_BOOLEAN, &even,		"print only all 'even' pages", NULL)
  NOREQ_PARAM ("HOFFSET",   "HOFF", OPT_TEX_DIM, &hoffset_in,	"horizontal offset",
		"(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)")
  NOREQ_PARAM ("VOFFSET",   "VOFF", OPT_TEX_DIM, &hoffset_in,	"vertical offset",
		"(unit out of: pt|cm|in|pc|dd|cc|bp|mm|mi|cp)")
  NOREQ_PARAM ("PRINTER",   "PRT",  OPT_STRING,  &printer_str,	"printer type",
		"out of Generic, NECP6, EpsFX, EpsLQ, HPDesk")
  NOREQ_PARAM ("OPTIMIZE",  "OPT",  OPT_BOOLEAN, &optimize,	"optimize printer output?",
		"(sometimes produce 'no' otimze the better result)")
  NOREQ_PARAM ("DRAFT",     NULL,   OPT_BOOLEAN, &onlydraft,	"print in draft modus", NULL)
  NOREQ_PARAM ("HQCHEAP",   NULL,   OPT_BOOLEAN, &onlycheap,	"HP DeskJet cheap modus", NULL)
  NOREQ_PARAM ("DENSITY",   "DEN",  OPT_LONG,    &density,	"printer density",
		"(only for the 'generic' printer)")
  NOREQ_PARAM ("BIDIRECT",  NULL,   OPT_BOOLEAN, &unidirect,	"print bidirectional", NULL)
  NOREQ_PARAM ("LANDSCAPE", "LSC",  OPT_BOOLEAN, &landscape,	"print in landscape modus", NULL)
  NOREQ_PARAM ("IFF",       NULL,   OPT_BOOLEAN, &iffprint,	"print pages to IFF-files", NULL)
  NOREQ_PARAM ("LASTFORMFEED",NULL, OPT_BOOLEAN, &last_form_feed, "no formfeed at the end of last page", NULL)
  NOREQ_PARAM ("REVERSE",   "REV",  OPT_BOOLEAN, &Reverse,	"print in reverse order", NULL)
  NOREQ_PARAM ("RESOLUTION","RES",  OPT_STRING,  &resolution_str, "bitmap resolution",
		"(e.g. RES 100 or RES 180/360)")
  NOREQ_PARAM ("PRELOAD",   NULL,   OPT_BOOLEAN, &PreLoad,	"preload all fonts", NULL)
  NOREQ_PARAM ("FAST",      NULL,   OPT_BOOLEAN, &turbo_mode,	"fast-mode",
		"(needs under 1.3 the 'puffer.device')")
  NOREQ_PARAM ("STATISTIC", "STAT", OPT_BOOLEAN, &Stats,	"more output to the logfile", NULL)
  NOREQ_PARAM ("DEBUGSTAT", NULL,   OPT_BOOLEAN, &DebugStats,	"close logfile after every line", NULL)
  NOREQ_PARAM ("LOGFILE",   "LOG",  OPT_BOOLEAN, &logging,	"create logfile (DVIprint.log)", NULL)
  NOREQ_PARAM ("OUTPUT",    "OUT",  OPT_STRING,  &output_file_name, "output to file, not to printer", NULL)
  HIDDEN_PARAM("PRINTAUTHOR", NULL, OPT_BOOLEAN, &g_authors,	"show author name", NULL)
  REQ_PARAM   (NULL,        NULL,   OPT_OPTIONSTRING, &filenameptr,"DVI-file", NULL)
END_PARAM


void DecodeArgs(int argc, char *argv[])
{
  BOOL error;
  char *ext;	/* extension */
  char prt[20], res[10];

  if (argc == 0) {
    g_progname = _ProgramName;
  }
  else {
    g_progname = argv[0];
  }

  
  switch (draft) {
    case HIGH_Q:
      onlydraft = FALSE;
      onlycheap = FALSE;
      break;
    case DRAFT:
      onlydraft = TRUE;
      onlycheap = FALSE;
      break;
    case HIGH_Q_CHEAP:
      onlydraft = FALSE;
      onlycheap = TRUE;
      break;
    case DRAFT_CHEAP:
      onlydraft = TRUE;
      onlycheap = TRUE;
      break;
  }
  optimize = TRUE;
  sprintf(res, "%3d", resolution);
  resolution_str = res;
  strcpy(prt,"generic");
  printer_str = prt;

  /*----------------------------------------------------------------*/
  error = GetOpt(argc, argv, "DVIprint", TRUE, opt);
  /*----------------------------------------------------------------*/

  if (error) {
    PrintHelp(opt, FALSE);
    AbortRun(5);	/* Programm Ende */
  }

  if (help) {
    PrintHelp(opt, TRUE);
    AbortRun(0);
  }

  if (maxbitmapsize != 0 && maxbitmapsize < MINBITMAPSIZE) {
    printf("*** not a valid value, minimum is %ld or 0 for unlimited size.\n",MINBITMAPSIZE);
  }

  if (odd && even) {
    printf("*** decide you! only 'odd' or 'even'!\n");
  }
  if (odd) {
    print_page_numbers = 1;
  }
  if (even) {
    print_page_numbers = 2;
  }

  check_printer_type(printer_str);

  if (density < 1 || density > 7) {
    Message("*** Density must be out of the range [1..7]!");
    AbortRun(5);
  }

  if (landscape || iffprint) {
    full_page_in_ram = TRUE;
  }

  if (Reverse) {
    PreLoad = TRUE;
  }

  if (strchr(resolution_str,'/') != NULL) {
    if (sscanf(resolution_str, "%d/%d", &hconvresolution, &vconvresolution) != 2 ) {
      sprintf(m_string, "*** wrong resolution format '%s'! Use 'RES nr' or 'RES hnr/vnr'.", resolution_str);
      Message(m_string);
      AbortRun(5);
    }
    /* resolution = bleibt 0 um xx/yy von dd unterscheiden zu koennen! */
  }
  else {
    if (sscanf(resolution_str, "%d", &resolution) != 1 ) {
      sprintf(m_string, "*** wrong resolution format '%s'! Use 'RES nr' or 'RES hnr/vnr'.", resolution_str);
      Message(m_string);
      AbortRun(5);
    }
    hconvresolution = vconvresolution = resolution;
  }

  if (DebugStats) {
    Stats = 2;
  }
  if (!logging) {
    g_logging = -1;
  }
  if (output_file_name != NULL) {
    output_to_file = TRUE;
  }


  if (filenameptr != NULL) {
    strcpy(filename, filenameptr);
  }
  else {
    Fatal(5,"filenameptr == NULL ???",22);
  }

  if ((ext = strrchr(filename, '.')) == NULL) {
    strcat(filename, ".dvi");
  }
  else {
    ext++;
    if (stricmp(ext, "tex") == NULL) {
      strcpy(ext, "dvi");	/* Ueberschreibe das '.tex' */
    }
    else {
      if (stricmp(ext, "dvi") != NULL) {
        strcat(filename, ".dvi");
      }
    }
  }
  dvifp = fopen(filename, "r");
  if (dvifp == NULL) {
    sprintf(m_string, "*** don't find DVI-file '%s'!", filename);
    Message(m_string);
    AbortRun(5);
  }

  
  /* Test ob ein paar unzulaessigen Sachen dabei sind */
  
  if (onlydraft && onlycheap) {
    draft = DRAFT_CHEAP;
  }
  else if (!onlydraft && onlycheap) {
    draft = HIGH_Q_CHEAP;
  }
  else if (onlydraft && !onlycheap) {
    draft = DRAFT;
  }
  else {
    draft = HIGH_Q;
  }

  open_logfile();
}



static void open_logfile(void)
{
  strcpy(g_Logname, DVIPRINT_LOGFILE);

  if (g_logging==1) {
    if ((g_logfp = fopen(g_Logname,"w")) == NULL) {
      fprintf(stderr,"\n");
      fprintf(stderr,"%s: can't open LOG-file \"%s\"\n\n", g_progname, g_Logname);
      AbortRun(5);
    }
  }
}


static void check_printer_type(char *str)
{
  printf("Printer Name: \"%s\"\n", str);
}


static void PrintHelp(struct Options opt[], short long_hlp)
{
  char hlp[100];

  /* Message(NULL); */
  sprintf(m_string,"(c)Copyright 1990-91, (hes). All rights reserved. %s",__DATE__);
  Message(m_string);

  sprintf(hlp, "usage: %s ", g_progname);
  GetOptShortHelp(hlp, 73, opt);
  
  if (long_hlp) {
    Message("");
    GetOptHelp(opt);  
  }
  
}



void main(int argc, char *argv[])
{
  DecodeArgs(argc, argv);
  
  printf("help:         '%d'\n", help);
  printf("PXLpath:      '%s'\n", PXLpath);
  printf("fontmemsize:  '%d'\n", fontmemsize);
  printf("FirstPage:    '%d'\n", FirstPage);
  printf("hoffset_in:   '%f'\n", hoffset_in);
  printf("voffset_in:   '%f'\n", voffset_in);
  printf("PreLoad:      '%d'\n", PreLoad);
  printf("resolution:   '%d'\n", resolution);
  printf("Stats:        '%d'\n", Stats);
  printf("DebugStats:   '%d'\n", DebugStats);
  printf("logging:      '%d'\n", logging);
  printf("g_authors:    '%d'\n", g_authors);
  printf("Filename:     '%s'\n", filename);

  if (argc == 0) {
    Delay(200);
  }
  printf(".\n");
}
