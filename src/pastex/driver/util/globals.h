/**********************************************************************/
/************************  Global Definitions  ************************/
/**********************************************************************/


typedef int BOOLEAN;

#ifdef AMIGA
#include <stdio.h>
#include <stdlib.h>
#include <exec/types.h>
#endif

#ifdef ATARI
# define BINARYOPEN(name) fopen(name,"rb")  /* byte-oriented host version */
#else
# ifdef AMIGA
#  define BINARYOPEN(name) fopen(name,"r")
# else
#  define BINARYOPEN(name) fopen(name,"r")
# endif
#endif

#define  DVIFORMAT        2

#ifndef TRUE
# define  TRUE             1
#endif
#ifndef FALSE
# define  FALSE            0
#endif


/** ** directory of the config files ** **/

#ifdef AMIGA
#  define	DEFAULT_PATH		"TeX:config/"
#else
#  define	DEFAULT_PATH		"c:\\config\\"
#endif

/** ** environment-var name of the config-directory ** **/

#define		ENV_DEFAULT_PATH	"TEXCONFIG"



    /* changes for output medium */

#ifdef DISPLAY						/* SHOWDVI */
# define  RESOLUTION		100			/* default resolution          */
# define  hconvRESOLUTION	100
# define  vconvRESOLUTION	100
# define  FONTMEMSIZE	     50000L			/* bytes fontarea needed for   */
# define  SHOWDVI_LOGFILE	"ShowDVI.log"		/* only one logfile per run    */
#else
#  ifdef ATARI
#   define  FONTMEMSIZE	     200000L			/* bytes fontarea needed for   */
#   define  MAXBITMAPSIZE     500000L			/* maximal bitmapsize */
#  else
#   define  FONTMEMSIZE	     100000L			/* bytes fontarea needed for   */
#   define  MAXBITMAPSIZE     500000L			/* maximal bitmapsize */
#  endif
#  define  MINBITMAPSIZE  50000L			/* minimal maximal bitmapsize */
#  define  DVIPRINT_LOGFILE	"DVIprint.log"
#endif /* DISPLAY */


#define  HOFFSET_IN		1.0			/* HOffset = 1 inch          */
#define  VOFFSET_IN		1.0			/* VOffset = 1 inch          */
#define  MOFFSET_IN		0.0			/* MID-Offset = 0 inch       */

/* SAVETY_BITS: Sicherheitsrand gegen "Overfull hbox/vbox" */
#ifdef DISPLAY
#define SAVETY_BITS_X	(hconvresolution+AddXpixel)	/* rows on the right border  */
#else
#define SAVETY_BITS_X	(hconvresolution)		/* rows on the right border  */
#endif
#define SAVETY_BITS_Y	(vconvresolution)		/* lines on the lower border */
			     /* entspricht 1in horiz. und 1in vert. */

/* These flags are used in the ToDo var. and describe the libraries,	*/
/* devices, files, buffers and actions to open, allocate or execute. A	*/
/* separate identifier is used to see if the devices and such *have*	*/
/* actually been opened. 						*/

#define DO_PDEVICE	(1<<0)		/* request		*/
#define DO_OUTFILE	(1<<1)		/* output_file_name/ptr	*/
#define DO_IFFLIB	(1<<2)		/* IFFBase		*/
#define DO_PBUFFERS	(1<<3)		/* bufferA/B		*/
#define DO_GFXLINEBUF	(1<<4)		/* PrnBuffer		*/
#define DO_PPREFS	(1<<5)		/* old_density		*/
#define DO_NOZERO	(1<<6)		/* (send 0s if aborted)	*/
#define DO_REALPRINT	(1<<7)		/* to printer, not file	*/


#ifdef AMIGA
# ifdef __SASC_60
#    define  MAXOPEN        (FOPEN_MAX-9)
# else
#   ifdef LATTICE
#     define  MAXOPEN       (_NFILE-9)		/* limit on number of open files */
#   else
#     ifdef AZTEC_C
#       define  MAXOPEN     (FOPEN_MAX-9)	/* limit on number of open files */
#     else
#       define  MAXOPEN     10			/* limit on number of open files */
#     endif
#   endif
# endif
#else
#  define  MAXOPEN         5			/* limit on number of open files */
#endif


#define  KOMM           1000000L		/* from ShowPage / mark a command */


/* important constants for flib-handling */
#define OLD_LIBMAGIC	0xA797F033L		/* old magic number */
#define LIBMAGIC	(((long)'F'<<24) | ((long)'L'<<16) | ((long)'I'<<8) | (long)'B')

#define FILENAMELEN	14		/* length for old flib version */
#define NEWFILENAMELEN	22		/* length for new flib version */

#define	OLD_VERSION	0		/* defines old flib version */
#define NEW_VERSION	1		/* defines new flib version */

#define  PATHLEN         128			/* max length of a file-path   */
#define  STACKSIZE       100
#define  STRSIZE         257
#define  HSIZE             8			/* paper-width in inch         */
#define  VSIZE            11.5			/* paper-height in inch        */



/*
 * DVI Handle: DVI-File ist entweder ein normales Level-2 File
 *             oder aber in's RAM gemappt.
 * Dazu gibt es folgende Funktionen:
 *  DVIFILE * OpenDVI(char * name, int toRam)
 *  void CloseDVI(DVIFILE * dvifp)
 *  void TempCloseDVI(DVIFILE * dvifp)
 *  void TempOpenDVI(DVIFILE * dvifp)
 */

enum DviFileFlags { InRam = 1<<1, Eof = 1<<2, TmpClosed = 1<<3 };

typedef struct {
  FILE 		    * fp;
  char 		    * rambuf;
  long		      pointer;
  long		      size;
  long		      tmpftell;
  enum DviFileFlags   flags;
  char		      name[256];
} DVIFILE;


/**********************************************************************/
/*************************  System Procedures  ************************/
/**********************************************************************/

/*  int	page_counter();  Hilfsfunktionen aus showdvi.c */
#define clear_counter(i)	page_counter(i,0)
#define set_counter(i)		page_counter(i,1)
#define take_counter(i)		page_counter(i,2)

#ifdef AMIGA
# ifdef LATTICE
#  define Chk_Abort	chkabort
# endif
#endif


/*
 * Debugging:
 *
 * MYDEBUG wird in SCOPTIONS definiert (oder nicht).
 * Achtung: Moeglicherweise gibt's Probleme mit GST Files.
 * Debug-Ausgaben werden mittels D(bug("bla %ld blub\n", i)) plaziert.
 * Achtung: Um int auszugeben wird %ld benoetigt!
 */

extern void __stdargs kprintf(UBYTE *fmt,...);
extern void __stdargs dprintf(UBYTE *fmt,...);
#define DEBTIME 0
#define bug kprintf
#if MYDEBUG
# include <clib/dos_protos.h>
# include <pragmas/dos_pragmas.h>
# define D(x) (x); if(DEBTIME>0) Delay(DEBTIME);
#else
# define D(x) ;
#endif /* MYDEBUG */


/*
 * MemWatch von SAS
 * Das Define MWDEBUG wird mittels SCOPTIONS ein/aus-geschaltet
 */

#include <clib/exec_protos.h>
#include <pragmas/exec_pragmas.h>
#if defined(MWDEBUG)
# include "sc:extras/memlib/memwatch.h"
#endif


/*
 * eigenes Malloc-Debugging ausschalten; 
 * Dadurch bekommt die memlib die korrekte Position des free's.
 */
#if !defined(MALLOC_DEBUG)
# define xfree(x)	free(x)
#endif

/**********************************************************************/
/*************************  Global Variables   ************************/
/**********************************************************************/

#include "globvars.h"


