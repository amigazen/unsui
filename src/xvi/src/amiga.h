/* Copyright (c) 1990,1991,1992 Chris and John Downey; */
/* Copyright (c) 1994 Dan Schmelzer                    */

/* Filename amiga.h       */
/* Initial Date:  9/24/94 */
/* Revised Date: 10/18/94 */
/* Author: Dan Schmelzer  */

/*
 * @(#)amiga.h
 * Amiga specific type definitions are here.
 */

#include <dos.h>
#include <workbench/startup.h>
#include <workbench/workbench.h>
#include <graphics/text.h>
#include <intuition/intuition.h>
#include <intuition/intuitionbase.h>
#include <pragmas/icon_pragmas.h>
#include <pragmas/graphics_pragmas.h>
#include <pragmas/intuition_pragmas.h>
#include <pragmas/exec_pragmas.h>
#include <pragmas/dos_pragmas.h>
#include <pragmas/diskfont_pragmas.h>

#ifdef OLD_AMIGA_OS /*________________________________________________*/
#define LIB_REV  34  /* This is 1.3 libs definition. */
#else /*______________________________________________________________*/
#define LIB_REV  36  /* This is 2.0 libs definition (37 for 2.04). */
#endif /*_____________________________________________________________*/

#define MIN_ROWS 2   /* Minimum allowed rows for an xvi Amiga window. */
#define MIN_COLS 8   /* Min allowed columns for an xvi Amiga window.  */
#define MAX_ROWS 400 /* Maximum allowed rows for an xvi Amiga window. */
#define MAX_COLS 800 /* Max allowed columns for an xvi Amiga window.  */
#define OUT_BUFFER_SIZE  5000  /* Size of output display buffer. */
#define HELPFILE  "help.xvi"   /* Default help file. */
#define MAX_NAME_LENGTH 40  /* Max number of chars for name in title. */
#define MAX_LEFT_EDGE 1000  /* Max pixels screen-left to window-left. */
#define MAX_TOP_EDGE 800    /* Max pixels screen-top to window-top. */

/* If started from workbench these are some of the initial window   */
/* parameters.  They must be within the expected min and max window */
/* dimensions that are chosen when the actual window size is        */
/* determined.  This will depend on MIN_ROWS, MIN_COLS, MAX_ROWS,   */
/* MAX_COL, and on the font size used.  A bit tricky...             */

#ifdef OLD_AMIGA_OS /*________________________________________________*/
/* Pre-2.0 Amiga OS.  We have to be careful not to make the initial */
/* window too large or the old OS will have problems.  We know the  */
/* font size is more limited and worst case is probably smaller.    */
#define DFLT_LEFT_EDGE 0
#define DFLT_TOP_EDGE 0
#define INIT_WIDTH 160
#define INIT_HEIGHT 100
/* For OS ver 1.3 worst case is if running lo-res, "60" text width. */
#define DEFAULT_ROWS 20  /* Number of rows if started from Workbench. */
#define DEFAULT_COLS 60  /* Number of cols if started from Workbench. */

#else /*______________________________________________________________*/
/* 2.0 or later Amiga OS.  This should work for all */
/* but the very largest or very smallest fonts.     */
#define DFLT_LEFT_EDGE 0
#define DFLT_TOP_EDGE 8	 /* Just a little down from top. */
#define INIT_WIDTH 320
#define INIT_HEIGHT 200
#define DEFAULT_ROWS 25  /* Number of rows if started from Workbench. */
#define DEFAULT_COLS 80  /* Number of cols if started from Workbench. */
#endif /*_____________________________________________________________*/

/* Default shell command when shell started from xvi without */
/*  SHELL defined.                                           */
#define DFLT_SHELL "newshell WINDOW=CON:18/68/640/390/xvi_Shell/ALT5/250/145/27/CLOSE";

extern int Rows;         /* Size of screen (Amiga text window). */
extern int Columns;

/*
 * System-dependent constants.
 */
#define MAXPATHLEN 160  /* Maximum length of full path name. */
#define MAXNAMLEN   30  /* Maximum length of filename. */
#define DIRSEPS    "/"  /* Pathname separator for system calls. */

/*
 * Default file format characters for \r, \n, and EOF.
 * Amiga uses UNIX.
 */
#define DEF_TFF fmt_UNIX

/*
 * Need to pass pointers to argc and argv to Amiga init code.
 */
#define sys_init() amiga_sys_init(&argc,&argv)

/*
 * Terminal driving functions.
 */
#define erase_line()   outstr("\233K")     /* Erase to end of line */
#define insert_line()  outstr("\233L")     /* Insert one line */
#define delete_line()  outstr("\233M")     /* Delete one line */
#define invis_cursor() outstr("\2330 p")   /* Invisible cursor */
#define vis_cursor()   outstr("\233 p")    /* Visible cursor */

/*
 * Scrolling not currently used.
 */
#define can_scroll_area FALSE
#define scroll_up(x,y,z)
#define scroll_down(x,y,z)

/* Cost of using a tty_goto() call. Doesn't seem to be used for much.*/
#define cost_goto   10

/*
 * Inserting and deleting lines are also used for scrolling.
 * For Amiga we restrict these to just one line because of performance.
 */
#define can_ins_line TRUE
#define can_del_line TRUE
/* tty_linefeed() isn't needed if can_del_line is TRUE. */
#define tty_linefeed()

/*
 * Insert character not used.
 */
#define can_inschar  FALSE
#define inschar(c)

/*
 * Color handling.
 * Note that you MUST have leading zero for octal to work!!!
 *
 * Octal number format: 0<sytle><background><foreground>
 *   <sytle> = 0  Plain Text
 *           = 1  Bold-face
 *           = 2  TURN OFF COLOR HANDLING    
 *           = 3  Italic
 *           = 4  Underscore
 *           = 5    (Turn off color handling)
 *           = 6    (Turn off color handling)
 *           = 7  Inverse-video
 *
 *   <background> | <foreground>
 *           = 0  Grey      (OS 2.0 default)
 *           = 1  Black     (OS 2.0 default)
 *           = 2  White     (OS 2.0 default)
 *           = 3  Dark Blue (OS 2.0 default)
 *           = 4  User definable from Workbench
 *           = 5  User definable from Workbench
 *           = 6  User definable from Workbench
 *           = 7  User definable from Workbench
 */
#define DEF_COLOUR     0001 /* Color for "colour" / "co" parameter.   */
#define DEF_ROSCOLOUR  0012 /* Color for "roscolour" / "rst" param.   */
#define DEF_STCOLOUR   0032 /* Color for "statuscolour" / "st" param. */
#define DEF_SYSCOLOUR  0001 /* Color for "systemcolour" / "sy" param. */

/*
 * Macros to open files in binary mode,
 * and to expand filenames.
 */
#define fopenrb(f)  fopen((f), "rb")
#define fopenwb(f)  fopen((f), "wb")
#define fexpand(f)  (f)

/*
 * Declarations for system interface routines in amiga.c.
 */
extern int    inchar(long);
extern void   outchar(char);
extern void   outstr(char *);
extern void   flush_output();
extern void   erase_display();
extern void   alert(void);
extern void   sleep(unsigned);
extern void   delay(void);
extern void   amiga_sys_init(int, char **);
extern void   sys_exit(int);
extern void   tty_goto(int, int);
extern int    call_system(char *);
extern int    call_shell(char *);
extern void   sys_startv(void);
extern void   sys_endv(void);
extern bool_t can_write(char *);
extern bool_t sys_pipe(char *, int (*)(), long (*)());
extern void   set_colour(int);
extern char   *tempfname(char *);

/* End of file________________________________________________________*/
