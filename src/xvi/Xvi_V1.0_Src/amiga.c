/* Copyright (c) 1990,1991,1992 Chris and John Downey; */
/* Copyright (c) 1994 Dan Schmelzer                    */

/* Filename amiga.c       */
/* Initial Date:  9/24/94 */
/* Revised Date: 10/27/94 */
/* Author: Dan Schmelzer  */

#ifndef lint
static char *sccsid_amiga="@(#)amiga.c   2.1 (Dan Schmelzer) 10/27/94";
#endif

/*______________________________________________________________________
* program name:
    xvi
* function:
    Portable version of UNIX "vi" editor, with extensions.
* module name:
    amiga.c
* module function:
    System interface module for the Amiga
* history:
    STEVIE - ST Editor for VI Enthusiasts, Version 3.10
    Originally by Tim Thompson (twitch!tjt)
    Extensive modifications by Tony Andrews (onecom!wldrdg!tony)
    Heavily modified by Chris & John Downey
    Amiga porting started by Dougie for Lattice 6.0
    Amiga modifications/porting by Dan Schmelzer for SAS 6.5
    This file created by Dan Schmelzer

* Static functions defined in this file:
      OpenConsole()
      Init_IO()
      Cleanup_IO()
      WriteWindowTitle()
      ResizeWindow()
      QueueRead()
      ConGetChar()
      ConPutChar()
      ConPutStr()
      XviFindToolType()

* Global functions defined in this file:
      inchar()
      outchar()
      outstr()
      flush_output()
      erase_display()
      alert()
      sleep()
      delay()
      amiga_sys_init()
      sys_exit()
      tty_goto()
      sys_pipe()
      call_system()
      call_shell()
      sys_startv()
      sys_endv()
      can_write()
      set_colour()
      tempfname()
      exists()
      amiga_version()

* DEBUG: undef DEBUG    No debug
           def DEBUG=1  Minimal debug
           def DEBUG=2  Medium amount of debug, no Output functions
           def DEBUG=3  Large amount of debug, Output functions included
           def DEBUG=4  Maximum debug information (lots)

         undef NO_OUT_BUFFERING  Normal output buffering.
           def NO_OUT_BUFFERING  No output buffering for easier debug.
______________________________________________________________________*/

#include "xvi.h"

#if DEBUG > 1
int ttygoto_count = 0;
#endif

/*____________________________________________________________________*/
/* AMIGA Version Strings.                                             */

#ifdef OLD_AMIGA_OS /*________________________________________________*/
/* For pre-2.0 Amiga OS.  Must be short and sweet (about 58 cols). */
char amiga_ver_str[] =
 "Generic Xvi V2.15, Amiga Xvi V1.0  (for Amiga OS V1.3)";

#else /*______________________________________________________________*/
/* For 2.0 and later Amiga OS. */
char amiga_ver_str[] =
 "Xvi Editor Copyright \251 1990,\n"
 "1991, 1992 Chris & John Downey\n"
 "\n"
 "Amiga Conversion for Xvi Editor\n"
 "Copyright \251 1994 Dan Schmelzer\n"
 "\n"
 "Generic Xvi Version: 2.15\n"
 "  Amiga Xvi Version: 1.0\n"
 "Amiga Version Built: " __DATE__;
#endif /*_____________________________________________________________*/


/*____________________________________________________________________*/
/* These are globals which are set by the OS-specific module,         */
/* and used for various purposes throughout the rest of xvi.          */

int Rows;		/* Number of Rows and Columns */
int Columns;		/* in the current window. */


/*____________________________________________________________________*/
/* Library bases.  Variable names must not be altered.                */

struct DiskfontBase   *DiskfontBase = NULL;
struct IntuitionBase  *IntuitionBase = NULL;
struct GfxBase        *GfxBase = NULL;

/* DOSBase already defined in compiler startup code */
extern struct DosLibrary *DOSBase;

/*____________________________________________________________________*/
/* External system function declarations.                             */

extern APTR OpenLibrary();
extern struct Screen    *OpenScreen();
extern struct Window    *OpenWindow();
extern struct MsgPort   *CreatePort();
extern struct IOStdReq  *CreateStdIO();
extern struct TextFont  *OpenDiskFont();

extern void  CloseLibrary();
extern void  CloseScreen();
extern void  CloseWindow();
extern void  DeletePort();
extern void  DeleteStdIO();
extern void  CloseDevice();
extern void  CloseFont();
extern void  ActivateWindow();

extern void  LoadRGB4();
extern long  SetFont();
extern void  SendIO();
extern long  DoIO();
extern struct Message  *GetMsg();
extern VirtScr defscr;

/*____________________________________________________________________*/
/* Forward referencing for static functions defined in this file.     */

static int  OpenConsole();
static int  Init_IO();
static void Cleanup_IO();
static void WriteWindowTitle();
static void ResizeWindow();
static void QueueRead();
static int  ConGetChar();
static void ConPutChar();
static void ConPutStr();
static char *XviFindToolType();


/*____________________________________________________________________*/
/* Global variables                                                   */

bool_t workbench = FALSE;  /* Was xvi started up via workbench? */
bool_t map_ret_flag = FALSE;	/* Do we map "\r" to "\r0" for cmds? */
bool_t repaint = FALSE;	/* Do we need to repaint display? */
UBYTE *saved_title;	/* Saved window title if started up via CLI. */
char readstring[4];	/* Use only first character. */

struct WBStartup *wb_startup;	/* Workbench startup stuff. */
int *saved_argc_ptr;		/* Saved pointer to argc. */
char ***saved_argv_ptr;		/* Saved pointer to argv. */

struct Window *xvi_window;

/* We start out with initial values that we think are safe. */
struct NewWindow xvi_newwind =
{
  0,			/* Left Edge (filled in later) */
  0,			/* Top Edge (filled in later)  */
  INIT_WIDTH,		/* Width        */
  INIT_HEIGHT,		/* Height       */
  -1,			/* Detail Pen   */
  -1,			/* Block Pen    */
  0,			/* IDCMP Flags  */
  WINDOWCLOSE | SMART_REFRESH | ACTIVATE | WINDOWDRAG |
   WINDOWDEPTH | WINDOWSIZING | NOCAREREFRESH, /* Flags  */
  NULL,			/* Gadgets      */
  NULL,			/* Menu Check Mark */
  "xvi window",		/* Title        */
  NULL,			/* Screen       */
  NULL,			/* SuperBitMap  */
  1,			/* Min Width    */
  1,			/* Min Height   */
  10000,		/* Max Width    */
  10000,		/* Max Height   */
  WBENCHSCREEN		/* Screen Type  */
};

struct IOStdReq *consoleWriteMsg;
struct IOStdReq *consoleReadMsg;

struct MsgPort *consoleWritePort;
struct MsgPort *consoleReadPort;

char out_buffer[OUT_BUFFER_SIZE+2];	/* Output display buffer. */
int out_buf_index = 0;			/* Index into out_buffer. */

char *name_str = NULL;		/* For Workbench NAME Tool Type. */
char *parms_str = NULL;		/* For Workbench PARMS Tool Type. */

char *font_str = NULL;			/* For custom font. */
struct TextFont *tf = NULL;
struct TextAttr ta;

int left_edge = 0; /* Pixels from left screen edge to left window edge*/
int top_edge = 0;  /* Pixels from top screen edge to top window edge. */
  

/* The follwing structures are for the "exit" requestor. */

#ifdef OLD_AMIGA_OS /*________________________________________________*/

/* For pre-2.0 Amiga OS. */
/* (If this is used for 2.0 OS or later the only  */
/*  part of the structure used is the text part.) */

struct IntuiText xvi_exit_bodytext =
{
  1,		/* Foreground color */
  0,		/* Background color */
  JAM2,		/* Draw mode */
  6,		/* Left Edge */
  3,		/* Top Edge */
  NULL,		/* Text Attribute (use default) */
  "Some Buffers Not Written!",
  NULL		/* Next IntuiText */
};
  
struct IntuiText xvi_exit_postext =
{
  1,0,JAM2,6,3,NULL,"EXIT Anyway",NULL
};

struct IntuiText xvi_exit_negtext =
{
  1,0,JAM2,6,3,NULL,"RESUME Xvi",NULL
};

struct IntuiText xvi_ver_bodytext =
{
  1,		/* Foreground color */
  0,		/* Background color */
  JAM2,		/* Draw mode */
  6,		/* Left Edge */
  3,		/* Top Edge */
  NULL,		/* Text Attribute (use default) */
  amiga_ver_str,
  NULL		/* Next IntuiText */
};

struct IntuiText xvi_ver_negtext =
{
  1,0,JAM2,6,3,NULL,"Continue",NULL
};

#else /*______________________________________________________________*/

/* For 2.0 Amiga OS or later. */
struct EasyStruct xvi_exit_requester =
{
  sizeof(struct EasyStruct),
  0,
  "Xvi Exit Request",
  "Some Buffers Not Written!",
  "EXIT Anyway|RESUME Xvi"
};

/* For 2.0 Amiga OS or later. */
struct EasyStruct xvi_ver_requester =
{
  sizeof(struct EasyStruct),
  0,
  "Xvi Version",
  amiga_ver_str,
  "Continue"
};
#endif /*_____________________________________________________________*/


/*____________________________________________________________________*/
/* OpenConsole()                                                      */
/*   Routine to open a console device.  Zero is returned if ok.       */
/*                                                                    */
/* PARMS: Pointers to I/O request structures and Window structure.    */
/* RETURNS: Error value from OpenDevice() call.                       */
/*____________________________________________________________________*/
/* FUNCTION */

static int OpenConsole(writerequest, readrequest, window)
  struct IOStdReq  *writerequest;
  struct IOStdReq  *readrequest;
  struct Window    *window;
{
  int error;

#if DEBUG > 2
  fprintf(stderr, "> OpenConsole()\n");
#endif

  writerequest->io_Data = (APTR) window;
  writerequest->io_Length = sizeof(struct Window);

  error = (int)OpenDevice("console.device", 0L, writerequest, 0L);
  readrequest->io_Device = writerequest->io_Device;
  readrequest->io_Unit = writerequest->io_Unit;

#ifdef DEBUG
  fprintf(stderr, "< OpenConsole(): error=%d\n", error);
#endif
  return (error);
}


/*____________________________________________________________________*/
/* Init_IO()                                                          */
/*   Routine to open a screen, then open a window in it, and attach   */
/*   the console device to it.                                        */
/*                                                                    */
/* RETURNS:  0  Operation successful                                  */
/*           1  Failed to open graphics library                       */
/*           2  Failed to open intuition library                      */
/*           3  Failed to open diskfont library                       */
/*           4  Failed to open DOS library                            */
/*           5  Failed to open custom window                          */
/*           6  Failed to load custom disk font                       */
/*           7  Unable to change Min and Max size for custom window   */
/*           8  Failed to create write port                           */
/*           9  Failed to create standard out                         */
/*          10  Failed to create read port                            */
/*          11  Failed to create standard in                          */
/*          12  Failed to open console device                         */
/*____________________________________________________________________*/
/* FUNCTION */

static int Init_IO()
{
  ULONG lockcall;
  ULONG lock_number = 0;
  int min_rows;
  int min_columns;
  int max_rows;
  int max_columns;
  int min_wind_width;
  int min_wind_height;
  int max_wind_width;
  int max_wind_height;
  int wind_width;
  int wind_height;
  int text_width;
  int text_height;
  int lr_border;
  int tb_border;
  int error = 0;
  char *tmp_str;

#if DEBUG > 1
  fprintf(stderr, "> Init_IO()\n");
#endif

  out_buf_index = 0;  /* Let's just make sure buffer is set to empty. */

  if ((GfxBase = (struct GfxBase *)
   OpenLibrary("graphics.library", LIB_REV)) == NULL)
  {
    error = 1;		/* Failed to open graphics library. */
#ifndef OLD_AMIGA_OS /*_______________________________________________*/
    printf("Unable to open graphics.library.\n"
           "This software requires Amiga OS version 2.0 or later.\n");
#endif /*_____________________________________________________________*/
    goto exit_pt;
  }

  if ((IntuitionBase = (struct IntuitionBase *)
   OpenLibrary("intuition.library", LIB_REV)) == NULL)
  {
    error = 2;		/* Failed to open intuition library. */
    goto exit_pt;
  }

  if ((DiskfontBase = (struct DiskfontBase *)
   OpenLibrary("diskfont.library", LIB_REV)) == NULL)
  {
    error = 3;		/* Failed to open diskfont library. */
    goto exit_pt;
  }

  if ((DOSBase = (struct DOSBase *)
   OpenLibrary("dos.library", LIB_REV)) == NULL)
  {
    error = 4;		/* Failed to open DOS library. */
    goto exit_pt;
  }

  /* Get the Amiga window we'll be using. */
  if (workbench)
  {
    /* Xvi was started from Workbench so create a new window. */
    xvi_newwind.LeftEdge = left_edge;
    xvi_newwind.TopEdge = top_edge;
    if ((xvi_window = OpenWindow(&xvi_newwind)) == NULL)
    {
      error = 5;	/* Failed to open custom window. */
      goto exit_pt;
    }
  }
  else
  {
    /* Xvi was started from CLI so connect to existing window. */

    lockcall = LockIBase(lock_number);	/* Don't try to debug here... */
    xvi_window = IntuitionBase->ActiveWindow;
    UnlockIBase(lockcall);		/* ...to here.                */
  }

  /* Only if the font has NOT been overriden by the workbench     */
  /* tool type do we use the XVIFONT environment variable.        */
  /*                                                              */
  /* The XVIFONT environment variable or FONT tool type string    */
  /* must be of the following formats:                            */
  /*   [<font_path>]<font_name>.font,<size>                       */
  /*   [<font_path>]<font_name>.font <size>                       */
  /*                                                              */
  /* For example:                                                 */
  /*   Fonts:topaz.font,8                                         */
  /*   topaz.font 16                                              */
  /*   FONT=topaz.font,16     (workbench tool type for xvi)       */

  /* Get the XVIFONT environment variable, if it exists */
  if (!font_str)
    font_str = getenv("XVIFONT");

  /* If specified, load a custom font. */
  if (font_str)
  {
    char *str_ptr = font_str;

    /* Find ',' */
    while (*str_ptr && (*str_ptr != ',') && (*str_ptr != ' '))
      str_ptr++;

    if (*str_ptr)
    {
      *str_ptr = '\000';	/* Terminate Name part of font string.*/

      /* Attributes of font to load. */
      ta.ta_Name = font_str;
      ta.ta_YSize = atoi(str_ptr + 1);	/* Get font size. */
      ta.ta_Style = 0;
      ta.ta_Flags = FPF_DISKFONT;

      /* Load the special font. */
      if ((tf = (struct TextFont *)OpenDiskFont(&ta)) == NULL)
      {
        printf("Unable to load font \"%s\" size %d\n",
               ta.ta_Name, ta.ta_YSize);
        error = 6;	/* Failed to open disk font. */
        goto exit_pt;
      }
      SetFont(xvi_window->RPort, tf);	/* Attach font to window. */
    }
    else
    {
      printf("Bad font definition string in XVIFONT: \"%s\"\n",
              font_str);
      error = 6;	/* Failed to open disk font. */
      goto exit_pt;
    }
  }

  /* Now resize the Amiga window as needed to get everything all ok. */
  if (workbench)
  {
    /* Note that the Rows and Columns variables have already been set */
    /* by the amiga_sys_init() routine before this.                   */

    /* Start off with default min and max sizes. */
    min_rows = MIN_ROWS;
    min_columns = MIN_COLS; /* May be overriden if too small. */
    max_rows = MAX_ROWS;
    max_columns = MAX_COLS;

    /* Get Text size. */
    text_width = xvi_window->RPort->TxWidth;
    text_height = xvi_window->RPort->TxHeight;

    /* Make sure text width is no less than 4 times right border to   */
    /* guarantee that window system gadets are visible when minimized.*/
    if ((text_width * min_columns) < (4 * xvi_window->BorderRight))
    {
      /* Recalculate minimum number of columns. */
      min_columns = ((4 * xvi_window->BorderRight) / text_width) + 1;
    }

    /* Get borders. */
    lr_border = xvi_window->BorderLeft + xvi_window->BorderRight;
    tb_border = xvi_window->BorderTop + xvi_window->BorderBottom;

    /* Set up actual window dimensions based on rows and columns. */
    min_wind_width = (text_width * min_columns) + lr_border;
    min_wind_height = (text_height * min_rows) + tb_border;
    max_wind_width = (text_width * max_columns) + lr_border;
    max_wind_height = (text_height * max_rows) + tb_border;
    wind_width = (text_width * Columns) + lr_border;
    wind_height = (text_height * Rows) + tb_border;

    /* First resize window based on Row and Column. */
    SizeWindow(xvi_window, (wind_width - xvi_window->Width),
               (wind_height - xvi_window->Height));

    /* Now adjust window size limits to nominal values. */
    if (!WindowLimits(xvi_window, min_wind_width, min_wind_height,
                     max_wind_width, max_wind_height))
    {
      printf("Unable to change Min and Max size of window.\n"
             "Font used may be too large or too small.\n");
      error = 7;	/* Call to WindowLimits() Failed. */
      goto exit_pt;
    }
  }
  else
  {
    /* Xvi was started from CLI so calculate rows and columns from */
    /* existing CLI window.                                        */

    /* Get window border size. */
    lr_border = xvi_window->BorderLeft + xvi_window->BorderRight;
    tb_border = xvi_window->BorderTop + xvi_window->BorderBottom;

    /* Calculate Columns and Rows based on size of existing window. */
    Columns = (xvi_window->Width - lr_border) /
               xvi_window->RPort->TxWidth;
    Rows = (xvi_window->Height - tb_border) /
            xvi_window->RPort->TxHeight;

    saved_title = xvi_window->Title;    /* Save original window title.*/
  }

  /* Change the window title to show how many rows and columns. */
  WriteWindowTitle();

  if ((consoleWritePort = CreatePort("xvi.con.write",0L)) == NULL)
  {
    error = 8;		/* Failed to create console write port. */
    goto exit_pt;
  }

  if ((consoleWriteMsg = CreateStdIO(consoleWritePort)) == NULL)
  {
    error = 9;		/* Failed to create standard out. */
    goto exit_pt;
  }

  if ((consoleReadPort = CreatePort("xvi.con.read",0L)) == NULL)
  {
    error = 10;		/* Failed to create console read port. */
    goto exit_pt;
  }

  if ((consoleReadMsg = CreateStdIO(consoleReadPort)) == NULL)
  {
    error = 11;		/* Failed to create standard in. */
    goto exit_pt;
  }

  if (OpenConsole(consoleWriteMsg,consoleReadMsg,xvi_window) != 0)
  {
    error = 12;		/* Failed to open console device. */
    goto exit_pt;
  }

  QueueRead();		/* Queue up first read. */

  outstr("\2332{");	/* Ask for RAW event: Mouse Info. */
  outstr("\23312{");	/* Ask for RAW event: Window Resize. */
  outstr("\23311{");	/* Ask for RAW event: Close Gadget. */
  if(!workbench)
    outstr("\23313{");	/* Ask for RAW event: Window Refreshed. */

  flush_output();	/* Flush the above output. */

  /* Check for existance of XVIMAP_RET environment variable.    */
  /* If it exists we want to map "\r" to "\r0" while in NORMAL  */
  /* mode.  This is not possible thru the normal "map" command. */
  if (tmp_str = getenv("XVIMAP_RET"))
  {
    map_ret_flag = TRUE;
    free(tmp_str);	/* Clean up from getenv(). */
  }
  else
    map_ret_flag = FALSE;

#if DEBUG > 1
  fprintf(stderr, "  Init_IO(): Window Information:\n");
  fprintf(stderr, "    Left Edge=%d, Top Edge=%d\n",
          xvi_window->LeftEdge, xvi_window->TopEdge);
  fprintf(stderr, "    Width=%d, Height=%d\n",
          xvi_window->Width, xvi_window->Height);
  fprintf(stderr, "    Min Width=%d, Min Height=%d\n",
          xvi_window->MinWidth, xvi_window->MinHeight);
  fprintf(stderr, "    Max Width=%d, Max Height=%d\n",
          xvi_window->MaxWidth, xvi_window->MaxHeight);
  fprintf(stderr, "    Border Left=%d, Border Right=%d\n",
          xvi_window->BorderLeft, xvi_window->BorderRight);
  fprintf(stderr, "    Border Top=%d, Border Bottom=%d\n",
          xvi_window->BorderTop, xvi_window->BorderBottom);
  fprintf(stderr, "    Text Width=%d, Text Height=%d\n",
          xvi_window->RPort->TxWidth, xvi_window->RPort->TxHeight);
#endif

exit_pt:

#if DEBUG > 1
  fprintf(stderr, "< Init_IO(): error=%d\n", error);
#endif
  return (error);
}


/*____________________________________________________________________*/
/* Cleanup_IO()                                                       */
/*   Routine to cleanup by closing the console device, ports,         */
/*   I/O streams, the custom window, the custom screen, and libraries.*/
/*                                                                    */
/* PARMS: err - This is the result of Init_IO() call and is used to   */
/*              determine how much stuff needs to be unraveled.       */
/*____________________________________________________________________*/
/* FUNCTION */

static void Cleanup_IO(err)
  int err;
{
#ifdef DEBUG
  fprintf(stderr, "> Cleanup_IO(): err=%d\n", err);
#endif

  switch(err)
  {
    case 0:
      CloseDevice(consoleWriteMsg);	/* Close the console device. */
    case 12:
      DeleteStdIO(consoleReadMsg);	/* Delete standard in. */
    case 11:
      DeletePort(consoleReadPort);	/* Delete console read port. */
    case 10:
      DeleteStdIO(consoleWriteMsg);	/* Delete standard out. */
    case 9:
      DeletePort(consoleWritePort);	/* Delete console write port. */
    case 8:
    case 7:
      if (font_str)
        CloseFont(tf);			/* Close any custom disk font.*/
    case 6:
      /* Free any memory gotten via getenv() or getmem(). */
      if (font_str)
        free(font_str);

      if (workbench)
      {
        /* Xvi was started from Workbench so close the xvi window. */
        CloseWindow(xvi_window);	/* Close custom window. */
      }
      else
      {
        /* Restore original CLI window title.*/
        xvi_window->Title = saved_title;

        /* Reactivate window for use by CLI. */
        /* First have to unselect window. Do this by selecting it's */
        /* parent.  Then select the window.                         */
        if (xvi_window->Parent)
          ActivateWindow(xvi_window->Parent);
        ActivateWindow(xvi_window);
        printf("\014");			/* Erase the CLI screen. */
      }
    case 5:
      CloseLibrary(DOSBase); 		/* Close DOS library. */
    case 4:
      CloseLibrary(DiskfontBase);	/* Close diskfont library. */
    case 3:
      CloseLibrary(IntuitionBase);	/* Close intuition library. */
    case 2:
      CloseLibrary(GfxBase);		/* Close graphics library. */
    case 1:
  }

#if DEBUG > 2
  fprintf(stderr, "< Cleanup_IO()\n");
#endif
}


/*____________________________________________________________________*/
/* WriteWindowTitle()                                                 */
/*   Routine to write the current window's title.                     */
/*____________________________________________________________________*/
/* FUNCTION */

static void WriteWindowTitle()
{
  static char title_str[MAX_NAME_LENGTH + 24]; 
  char *name;

  if (name_str)
  {
    name = name_str;

    /* Truncate name if necessary. */
    if (strlen(name) > MAX_NAME_LENGTH)
      *(name + MAX_NAME_LENGTH) = '\000';
  }
  else
    name = "xvi";

  /* Change the window title to show how many rows and columns. */
  (void)sprintf(title_str, "%s %dx%d window", name, Rows, Columns);
  SetWindowTitles(xvi_window, title_str, -1);
}


/*____________________________________________________________________*/
/* ResizeWindow()                                                     */
/*   Do all the goo to resize not only the display but also to        */
/*   get the generic part of xvi to work with new Row and Column      */
/*   values.                                                          */
/*____________________________________________________________________*/
/* FUNCTION */

static void ResizeWindow()
{
  int min_rows;
  int new_columns;
  int new_rows;
  int delta_columns;
  int delta_rows;
  int wind_width;
  int wind_height;
  int text_width;
  int text_height;
  int lr_border;
  int tb_border;
  Xviwin  *temp_win;

#if DEBUG > 2
  fprintf(stderr, "> ResizeWindow()\n");
#endif

  /* Get text size and border size. */
  text_width = xvi_window->RPort->TxWidth;
  text_height = xvi_window->RPort->TxHeight;
  lr_border = xvi_window->BorderLeft + xvi_window->BorderRight;
  tb_border = xvi_window->BorderTop + xvi_window->BorderBottom;

  /* Temporarily turning off RAW event "Window Resize" around   */
  /* 2nd resize does not seem to keep from getting us into an   */
  /* infinite loop, so we break the cycle by checking if resize */
  /* will change row or column.  If not we don't do the resize. */

  /* Recalculate Columns and Rows based on new size of window. */
  new_columns = (xvi_window->Width - lr_border) / text_width;
  new_rows = (xvi_window->Height - tb_border) / text_height;

  if (new_columns != Columns || new_rows != Rows)
  {
    /* Calculate the minimum number of rows we can have.        */
    /* This will be all other virtual window row counts plus 2. */
    min_rows = (Rows - curwin->w_nrows) + 2;

    /* Make sure we don't try to resize too small vertically. */
    if (new_rows < min_rows)
      new_rows = min_rows;

    /* Calculate how row and column counts have changed. */
    delta_columns = new_columns - Columns;
    delta_rows = new_rows - Rows;

    /* Update the globals. */
    Columns = new_columns;
    Rows = new_rows;

    /* Now resize window again based on Row and Column to correct */
    /* for any inacuracies when resizing window with gadget.      */
    wind_width = (text_width * Columns) + lr_border;
    wind_height = (text_height * Rows) + tb_border;

    SizeWindow(xvi_window, (wind_width - xvi_window->Width),
               (wind_height - xvi_window->Height));

    WriteWindowTitle();		/* Update the window's title. */

    /* Now tell the generic part of xvi about what we've done. */

    /* Update the screen structure directly. */
    defscr.pv_rows = Rows;
    defscr.pv_cols = Columns;

    /* Update the virtual window(s) structures. */
    /* Note that virtual window may not fill full virtual screen so   */
    /* we must change curwin's w_nrows and w_ncols by the difference. */
    curwin->w_nrows += delta_rows;
    curwin->w_ncols += delta_columns;
    curwin->w_cmdline += delta_rows;

    /* Adjust the width of ALL other virtual windows. */
    temp_win = curwin;
    while(temp_win = temp_win->w_next)
    {
      temp_win->w_ncols += delta_columns;
    }
    temp_win = curwin;
    while(temp_win = temp_win->w_last)
    {
      temp_win->w_ncols += delta_columns;
    }

    /* Update the real screen data structures. */
    init_screen(curwin);

    /* Tell any file buffers to refresh their window/screen. */
    update_buffer(curbuf);

    redraw_screen();		/* Redraw the screen. */

    move_window_to_cursor(curwin);
    cursupdate(curwin);		/* Update the cursor. */
    wind_goto(curwin);

#ifdef DEBUG
    fprintf(stderr, "  ResizeWindow(): Window has been resized.\n");
#endif
  }
#if DEBUG > 3
  fprintf(stderr, "< ResizeWindow()\n");
#endif
}


/*____________________________________________________________________*/
/* QueueRead()                                                        */
/*   Routine to queue up a read request to the console, show where to */
/*   put the character when ready to be returned. Should be first     */
/*   called just after opening the console.                           */
/*                                                                    */
/* SIDEEFFECTS:  consoleReadMsg                                       */
/*____________________________________________________________________*/
/* FUNCTION */

static void QueueRead()
{
#if DEBUG > 3
  fprintf(stderr, "> QueueRead()\n");
#endif
  consoleReadMsg->io_Command = CMD_READ;
  consoleReadMsg->io_Data = (APTR)&readstring[0];
  consoleReadMsg->io_Length = 1;
  SendIO(consoleReadMsg);
#if DEBUG > 3
  fprintf(stderr, "< QueueRead()\n");
#endif
}


/*____________________________________________________________________*/
/* ConGetChar()                                                       */
/*   Routine to get a character from standard in.  It will wait if    */
/*   necessary (task is put to sleep).                                */
/*                                                                    */
/* PARMS: cursor_on   Make cursor visible during keyboard input flag. */
/* RETURNS: Integer holding character.                                */
/*____________________________________________________________________*/
/* FUNCTION */

static int ConGetChar(cursor_on)
  bool_t cursor_on;
{
  int inputchar;

#if DEBUG > 3
  fprintf(stderr, "> ConGetChar(): cursor_on=%d\n", cursor_on);
#endif

  if (cursor_on)
  {
    vis_cursor();	/* Turn on the cursor while waiting for input.*/
    flush_output();
  }

  while (GetMsg(consoleReadPort) == NULL)
    WaitPort(consoleReadPort);
  inputchar = (UBYTE)readstring[0];	/* Get waiting character. */
  QueueRead();			/* Queue up another read request. */

  if (cursor_on)
    invis_cursor();	/* Since we turned cursor on, now turn it off.*/

#if DEBUG > 2
  fprintf(stderr, "< ConGetChar(): inputchar=0x%02x='%c'\n",
   inputchar, inputchar);
#endif
  return (inputchar);
}


/*____________________________________________________________________*/
/* ConPutChar()                                                       */
/*   Routine to send 1 character thru standard out to the custom      */
/*   window.                                                          */
/*                                                                    */
/* PARMS: character                                                   */
/*____________________________________________________________________*/
/* FUNCTION */

static void ConPutChar(outputchar)
  char outputchar;
{
#if DEBUG > 2
  fprintf(stderr, "> ConPutChar(): outputchar=0x%02x='%c'\n",
   outputchar, outputchar);
#endif
  consoleWriteMsg->io_Command = CMD_WRITE;
  consoleWriteMsg->io_Data = (APTR)&outputchar;
  consoleWriteMsg->io_Length = 1;
  DoIO(consoleWriteMsg);	/* DoIO blocks until command is done. */
#if DEBUG > 3
  fprintf(stderr, "< ConPutChar()\n");
#endif
}


/*____________________________________________________________________*/
/* ConPutStr()                                                        */
/*   Routine to send a null terminated string thru standard out to    */
/*   the custom window.                                               */
/*                                                                    */
/* PARMS: string                                                      */
/*____________________________________________________________________*/
/* FUNCTION */

static void ConPutStr(string)
  char *string;
{
#if DEBUG > 2
  /* Print leading <CSI>.  Skip any other leading non-printable. */
  fprintf(stderr, "> ConPutStr(): string=\"%s%s\"\n",
   (*string == '\233') ? "<CSI>" : "",
   (*string < ' ' || *string > '~') ? string+1 : string);
#endif
  consoleWriteMsg->io_Command = CMD_WRITE;
  consoleWriteMsg->io_Data = (APTR)string;
  consoleWriteMsg->io_Length = -1; /* End when find terminating zero. */
  DoIO(consoleWriteMsg);	/* DoIO blocks until command is done. */
#if DEBUG > 3
  fprintf(stderr, "< ConPutStr()\n");
#endif
}


/*____________________________________________________________________*/
/* XviFindToolType()                                                  */
/*   Routine similar to the Amiga OS routine FindToolType() except    */
/*   that _WBArgc and _WBArgv are used from the SAS "C" startup       */
/*   environment.  This makes life a lot easier.                      */
/*                                                                    */
/*   WARNING: This routine may be called only if we started via       */
/*            workbench!                                              */
/*                                                                    */
/* PARMS: tool_name  Name of tool to search for (should end with '=').*/
/* RETURNS: Pointer to string value associated with "tool_name" or    */
/*           NULL if "tool_name" is not found.                        */
/*____________________________________________________________________*/
/* FUNCTION */

static char *XviFindToolType(tool_name)
  char *tool_name;
{
  int i;
  int size = strlen(tool_name);

  /* Search Workbench arguments. Skip first argument since we know */
  /* it is not a Tool Type.                                        */
  for(i = 1; i < _WBArgc; i++)
  {
    if (strncmp(tool_name, _WBArgv[i], size) == 0)
    {
      return(_WBArgv[i] + size);  /* Found tool_name, return string. */
    }
  }

  return(NULL);		/* Did not find tool_name. */
}


/*____________________________________________________________________*/
/* inchar()                                                           */
/*   Get a character from the keyboard (console device).              */
/*   Do any remapping of characters and trapping of special           */
/*   characters in this routine.                                      */
/*                                                                    */
/* PARMS: timeout  (DAS - not handled at this time)                   */
/* RETURNS: Integer holding character.                                */
/*____________________________________________________________________*/
/* FUNCTION */

int inchar(timeout)
  long  timeout;
{
  static char *special_buffer = NULL;
  int input_char;
  bool_t special_key = FALSE;

#if DEBUG > 2
  fprintf(stderr, "> inchar(): timeout=%d\n", timeout);
#endif

  /* Check if an Amiga requestor has possibly messed up the display. */
  if (repaint)
  {
    /* We need to repaint window (with CNTL-L). */
    special_buffer = "\033\014";
    repaint = FALSE;
  }

  /* Get a character and check for <CSI>. Turn cursor on during input.*/
  while (!special_buffer &&
         (input_char = ConGetChar(TRUE)) == (UBYTE)'\233')
  {
    int input_char1;
    int input_char2 = 0;
    int input_char3 = 0;

    /* Get next 1, 2, or 3 characters as required. */
    input_char1 = ConGetChar(FALSE);
    switch ((char)input_char1)
    {
      case '1':
        input_char2 = ConGetChar(FALSE);
        switch ((char)input_char2)
        {
          case '0':	/* <CSI>10 */
          case '1':	/* <CSI>11 */
          case '2':	/* <CSI>12 */
          case '3':	/* <CSI>13 */
          case '4':	/* <CSI>14 */
          case '5':	/* <CSI>15 */
          case '6':	/* <CSI>16 */
          case '7':	/* <CSI>17 */
          case '8':	/* <CSI>18 */
          case '9':	/* <CSI>19 */
            input_char3 = ConGetChar(FALSE);
            if ((char)input_char3 == '~')
              special_key = TRUE;
            break;

          case '~':	/* <CSI>1~ */
            special_key = TRUE;
        }
        break;

      case '0':		/* <CSI>0 */
      case '2':		/* <CSI>2 */
      case '3':		/* <CSI>3 */
      case '4':		/* <CSI>4 */
      case '5':		/* <CSI>5 */
      case '6':		/* <CSI>6 */
      case '7':		/* <CSI>7 */
      case '8':		/* <CSI>8 */
      case '9':		/* <CSI>9 */
      case '?':		/* <CSI>? */
        input_char2 = ConGetChar(FALSE);
        if ((char)input_char2 == '~')
          special_key = TRUE;
        break;

      case ' ':
        input_char2 = ConGetChar(FALSE);
        if (((char)input_char2 == 'A') ||	/* <CSI> A */
            ((char)input_char2 == '@'))		/* <CSI> @ */
          special_key = TRUE;
        break;

      case 'A':		/* <CSI>A */
      case 'B':		/* <CSI>B */
      case 'C':		/* <CSI>C */
      case 'D':		/* <CSI>D */
      case 'S':		/* <CSI>S */
      case 'T':		/* <CSI>T */
        special_key = TRUE;
    }

    /* First check for Function, Help, or Arrow keys and map them. */

    /* Look for a special key. */
    if (special_key)
    {
      switch ((char)input_char1)
      {
        case '1':
          switch ((char)input_char2)
          {
            case '0':
              special_buffer = "\001p"; break;	/* Shifted F1 */
            case '1': 
              special_buffer = "\001q"; break;	/* Shifted F2 */
            case '2':
              special_buffer = "\001r"; break;	/* Shifted F3 */
            case '3':
              special_buffer = "\001s"; break;	/* Shifted F4 */
            case '4':
              special_buffer = "\001t"; break;	/* Shifted F5 */
            case '5':
              special_buffer = "\001u"; break;	/* Shifted F6 */
            case '6':
              special_buffer = "\001v"; break;	/* Shifted F7 */
            case '7':
              special_buffer = "\001w"; break;	/* Shifted F8 */
            case '8':
              special_buffer = "\001x"; break;	/* Shifted F9 */
            case '9':
              special_buffer = "\001y"; break;	/* Shifted F10 */
            case '~':
              special_buffer = "\001Q"; break;	/* F2 */
          }
          break;

        case '0':
          special_buffer = "\001P"; break;	/* F1 */
        case '2':
          special_buffer = "\001R"; break;	/* F3 */
        case '3':
          special_buffer = "\001S"; break;	/* F4 */
        case '4':
          special_buffer = "\001T"; break;	/* F5 */
        case '5':
          special_buffer = "\001U"; break;	/* F6 */
        case '6':
          special_buffer = "\001V"; break;	/* F7 */
        case '7':
          special_buffer = "\001W"; break;	/* F8 */
        case '8':
          special_buffer = "\001X"; break;	/* F9 */
        case '9':
          special_buffer = "\001Y"; break;	/* F10 */
        case '?':
          special_buffer = "\001?"; break;	/* Help */
        case 'A':
          special_buffer = "\001A"; break;	/* Up Arrow */
        case 'B':
          special_buffer = "\001B"; break;	/* Down Arrow */
        case 'C':
          special_buffer = "\001C"; break;	/* Right Arrow */
        case 'D':
          special_buffer = "\001D"; break;	/* Left Arrow */
        case 'T':
          special_buffer = "\001a"; break;	/* Shift Up Arrow */
        case 'S':
          special_buffer = "\001b"; break;	/* Shift Down Arrow */

        case ' ':
          switch ((char)input_char2)
          {
            case '@':
              special_buffer = "\001c"; break;	/* Shift Right Arrow */
            case 'A':
              special_buffer = "\001d";		/* Shift Left Arrow */
          }
      }
    } /* ...Look for a special key. */
    else 
    {
      /* Look for Resize Window event. */
      if (input_char1 == (UBYTE)'1' &&
          input_char2 == (UBYTE)'2' &&
          input_char3 == (UBYTE)';')
      {
        /* Wait for end of event sequence.*/
        while (ConGetChar(FALSE) != (UBYTE)'|');
  
        ResizeWindow();		/* Resize the window. */
      }
      else
      {
        /* Look for Close Window event. */
        if (input_char1 == (UBYTE)'1' &&
            input_char2 == (UBYTE)'1' &&
            input_char3 == (UBYTE)';')
        {
          Xviwin *wp;
          bool_t changed;

          /* Wait for end of event sequence.*/
          while (ConGetChar(FALSE) != (UBYTE)'|');

  #ifdef DEBUG
          fprintf(stderr, "  inchar(): Attempting to close window.\n");
  #endif

          /* First check to see if any xvi window contains */
          /* changes that have not been saved.             */
          changed = FALSE;
	  wp = curwin;
	  do
          {
            if (is_modified(wp->w_buffer))
            {
              changed = TRUE;
              break;
            }
          } while ((wp = next_window(wp)) != curwin);

	  if (changed)
          {
            /* Display "exit" requestor if there were changes. */
#ifdef OLD_AMIGA_OS /*________________________________________________*/
            /* For pre-2.0 Amiga OS.  Last two numbers       */
            /* are for pre-2.0 OS requestor size parameters. */
            if (AutoRequest(xvi_window, xvi_exit_bodytext,
                            xvi_exit_postext, xvi_exit_negtext,
                            0, 0, 320, 50))
#else /*______________________________________________________________*/
            /* For 2.0 Amiga OS or later. */
            if (EasyRequestArgs(xvi_window, xvi_exit_requester,
                                NULL, NULL) == 1)
#endif /*_____________________________________________________________*/
            {
              special_buffer = "\033:q!\015";	/* EXIT Anyway. */
            }
            else	/* Just repaint window with CNTL-L. */
              special_buffer = "\033\014";
          }
          else		/* Exit, buffers do not need saving. */
            special_buffer = "\033:q\015";
        }
        else
        {
          /* Check for mouse event. */
          if (input_char1 == (UBYTE)'2' &&
              input_char2 == (UBYTE)';')
          {
            int mouse_x;
            int mouse_y;
            int border_left;
            int border_top;
            int text_width;
            int text_height;
            int new_row;	/* New row for cursor. */
            int new_column;	/* New column for cursor. */
  
            /* Skip subclass field. */
            while (ConGetChar(FALSE) != (UBYTE)';');
  
            /* Look for keycode "104" which is left mouse button down.*/
            if ((ConGetChar(FALSE) == (UBYTE)'1') &&
                (ConGetChar(FALSE) == (UBYTE)'0') &&
                (ConGetChar(FALSE) == (UBYTE)'4') &&
                (ConGetChar(FALSE) == (UBYTE)';'))
            {
              mouse_x = xvi_window->MouseX;
              mouse_y = xvi_window->MouseY;
              border_left = xvi_window->BorderLeft;
              border_top = xvi_window->BorderTop;
  
  #if DEBUG > 1
              fprintf(stderr, "  inchar(): Mouse X=%d, Y=%d\n",
                      xvi_window->MouseX, xvi_window->MouseY);
  #endif
              if ((mouse_x > border_left) &&
                  (mouse_x < 
                   xvi_window->Width - xvi_window->BorderRight) &&
                  (mouse_y > border_top) &&
                  (mouse_y < 
                   xvi_window->Height - xvi_window->BorderBottom))
              {
                text_width = xvi_window->RPort->TxWidth;
                text_height = xvi_window->RPort->TxHeight;
  
                /* Calculate new row and column for cursor. */
                new_column = ((mouse_x - border_left) / text_width) + 1;
                new_row = ((mouse_y - border_top) / text_height) + 1;
  
                /* If window is slightly larger than an exact modulo  */
                /* of row height or column width we need to make sure */
                /* the mouse is not in this "extra" area.             */
                if (new_column > Columns)
                  new_column = Columns;
                if (new_row > Rows)
                  new_row = Rows;
  #ifdef DEBUG
                fprintf(stderr,
                      "  inchar(): Cursor: new_column=%d, new_row=%d\n",
                        new_column, new_row);
  #endif
                /*####################################################*/
                /* THIS FIXES A BUG in mouseclick() where the cursor  */
                /* is placed 8 positions to the right of where it     */
                /* should be if line numbers are on.                  */
                /*####################################################*/
                if (Pb(P_number))
                {
                  if (new_column < 9)
                    new_column = 1;
                  else
                    new_column -= 8;
                }
  
                /* Do actual cursor movement only if in normal mode. */
                if (State == NORMAL)
                {
                  /* Move cursor.  Note that mouseclick() counts  */
                  /* rows and columns starting at 0 instead of 1. */
                  mouseclick(new_row - 1, new_column - 1);
  
                  /* Update status line for current virtual window. */
                  show_file_info(curwin);
                  cursupdate(curwin);
                  wind_goto(curwin);
                }
              }
            }
  
            /* Wait for end of event sequence.*/
            while (ConGetChar(FALSE) != (UBYTE)'|');
  
          }  /* if...Check for mouse event. */
          else
          {
            /* Look for Window refresh event (if started via CLI). */
            if (!workbench &&
                input_char1 == (UBYTE)'1' &&
                input_char2 == (UBYTE)'3' &&
                input_char3 == (UBYTE)';')
            {
              /* Wait for end of event sequence.*/
              while (ConGetChar(FALSE) != (UBYTE)'|');

              /* Just repaint window with CNTL-L. */
              special_buffer = "\033\014";
            }

          }  /* else...Check for mouse event. */
  
        }  /* else...Look for Close Window event. */
  
      }  /* else...Look for Resize Window event. */
  
    }  /* else...Look for Special key. */

  }  /* while(...) */

  /* Translate RETURN key so we go to START of next line (normal mode)*/
  if (input_char == '\r' && State == NORMAL && map_ret_flag)
  {
    special_buffer = "\r0";
  }

  /* Return any special characters buffered up in this routine first. */
  if (special_buffer)
  {
    input_char = (UBYTE)(*special_buffer++);
    if (!(*special_buffer))
      special_buffer = NULL;	/* At end of special buffer. */

#if DEBUG > 2
    fprintf(stderr, "  inchar(): Character from special buffer\n");
#endif
  }

#if DEBUG > 1
  fprintf(stderr, "  inchar(): input_char=0x%02x='%c'\n",
          input_char, input_char);
#endif
  return (input_char);
}


/*____________________________________________________________________*/
/* outchar()                                                          */
/*   Send a character to display (console device).                    */
/*                                                                    */
/* PARMS: c - character to send out.                                  */
/*____________________________________________________________________*/
/* FUNCTION */

void outchar(c)
  char c;
{
#ifdef NO_OUT_BUFFERING
  ConPutChar(c);
#else
  out_buffer[out_buf_index++] = c;	/* Buffer the character. */

  if (out_buf_index >= OUT_BUFFER_SIZE)
    flush_output();		/* Flush the buffer if it is full. */
#endif
}

/*____________________________________________________________________*/
/* outstr()                                                           */
/*   Send a string to display (console device).                       */
/*                                                                    */
/* PARMS: s - character to send out.                                  */
/*____________________________________________________________________*/
/* FUNCTION */

void outstr(s)
  char *s;
{
#ifdef NO_OUT_BUFFERING
  ConPutStr(s);
#else
  int length;

  /* First make sure there is room in the output buffer. */
  length = strlen(s);
  if ((out_buf_index + length) >= OUT_BUFFER_SIZE)
    flush_output();		/* Flush the buffer. */

  if (length < OUT_BUFFER_SIZE)
  {
    /* Send string to buffer. */
    (void)strcpy(&out_buffer[out_buf_index], s);
    out_buf_index += length;
  }
  else
    ConPutStr(s);		/* Buffer not large enough. */
#endif
}


/*____________________________________________________________________*/
/* flush_output()                                                     */
/*   Flush any output held in the output buffer to the Amiga screen.  */
/*____________________________________________________________________*/
/* FUNCTION */

void flush_output()
{

#if DEBUG > 1
  fprintf(stderr, "> flush_output(): Prior calls to ttygoto() = %d\n",
          ttygoto_count);
  ttygoto_count=0;
  fprintf(stderr, "  flush_output(): Character count to flush = %d\n",
          out_buf_index);
#endif

#ifndef NO_OUT_BUFFERING
  /* Skip if out buffer is empty. */
  if (out_buf_index)
  {
    /* Terminate buffer string with NULL. */
    out_buffer[out_buf_index] = '\000';

    ConPutStr(out_buffer);	/* Send the buffer out to the display.*/
    out_buf_index = 0;		/* Set index back to start of buffer. */
  }
#endif

#if DEBUG > 3
  fprintf(stderr, "< flush_output()\n");
#endif

}


/*____________________________________________________________________*/
/* erase_display()                                                    */
/*   Erase the Amiga screen.                                          */
/*____________________________________________________________________*/
/* FUNCTION */

void erase_display()
{
  outstr("\014");
  flush_output();
}


/*____________________________________________________________________*/
/* alert()                                                            */
/*   Do a visual bell.                                                */
/*____________________________________________________________________*/
/* FUNCTION */

void alert()
{
  if (Pb(P_vbell))
  {
    outchar('\007');
    flush_output();	/* Flush the above output. */
  }
}

/*____________________________________________________________________*/
/* sleep()                                                            */
/*   Wait a period of time (give up this task).                       */
/*                                                                    */
/* PARMS: n - delay in seconds                                        */
/*____________________________________________________________________*/
/* FUNCTION */

void sleep(n)
  int n;
{
  Delay(n * 60);	/* Delay n X 60 ticks (1/60 of second). */
}

/*____________________________________________________________________*/
/* delay()                                                            */
/*   Wait a short period of time.                                     */
/*____________________________________________________________________*/
/* FUNCTION */

void delay()
{
  Delay(12);		/* Delay 1/5 of a second. */
}

/*____________________________________________________________________*/
/* amiga_sys_init()                                                   */
/*   Initialize the system stuff, like libraries, screen, windows,    */
/*   font, etc.  This routine takes the place of sys_init().          */
/*____________________________________________________________________*/
/* FUNCTION */

void amiga_sys_init(argc_ptr, argv_ptr)
  int *argc_ptr;
  char ***argv_ptr;
{
  int index;
  int error;


#if DEBUG > 1
  fprintf(stderr, "> amiga_sys_init(): argc=%d\n", *argc_ptr);
  for (index = 0; index < *argc_ptr; index++)
  {
    fprintf(stderr, "    argv[%d]=\"%s\"\n",
            index, (*argv_ptr)[index]);
  }
#endif

  /* This determines the default row and column count if starting  */
  /* up via workbench without any tool type override.  Note that   */
  /* if starting up via CLI the Rows and Columns variables will be */
  /* recalculated and changed later.                               */
  Rows = DEFAULT_ROWS;
  Columns = DEFAULT_COLS;

  /* This is the default number of pixels between the left/top edge */
  /* Amiga Workbench screen and the left/top edge of the new Xvi    */
  /* window if running Xvi from the Workbench.  These values may be */
  /* overridden by Xvi tool types "LEFT_EDGE" and "TOP_EDGE".       */
  left_edge = DFLT_LEFT_EDGE;
  top_edge = DFLT_TOP_EDGE;

  name_str = NULL;
  font_str = NULL;
  parms_str = NULL;

  if (*argc_ptr == 0)
  {
    /* We started from workbench. */

    char *temp_str;
    int parms_argc;
    int wb_argc;
    int index_wb;

    workbench = TRUE;
    parms_argc = 0;	/* Assume no Workbench PARMS Tool Type. */

    /* Save the workbench startup stuff. */
    wb_startup = (struct WBStartup *)(*argv_ptr);

    wb_argc = wb_startup->sm_NumArgs;	/* Number of workbench args. */

#if DEBUG > 1
    /* Workbench only arguments. */
    fprintf(stderr, "  amiga_sys_init(): Workbench argc=%d\n", wb_argc);
    for (index = 0; index < wb_argc; index++)
    {
      fprintf(stderr, "    Workbench argv[%d]=\"%s\"\n",
              index, (wb_startup->sm_ArgList + index)->wa_Name);
    }

    /* Workbench and Tool Type (from .info file) arguments merged. */
    fprintf(stderr, "  amiga_sys_init(): _WBArgc=%d\n", _WBArgc);
    for (index = 0; index < _WBArgc; index++)
    {
      fprintf(stderr, "    _WBArgv[%d]=\"%s\"\n",
              index, _WBArgv[index]);
    }
#endif

    /* Check for any "Tool Types" that we want to apply to xvi. */

    name_str = XviFindToolType("NAME=");

    if (temp_str = XviFindToolType("ROWS="))
    {
      Rows = atoi(temp_str);	/* Get Rows value. */

      /* Make sure value is legal. */
      if ((Rows < MIN_ROWS) || (Rows > MAX_ROWS))
        Rows = DEFAULT_ROWS;
    }

    if (temp_str = XviFindToolType("COLUMNS="))
    {
      Columns = atoi(temp_str);	/* Get Columns value. */

      /* Make sure value is legal. */
      if ((Columns < MIN_COLS) || (Columns > MAX_COLS))
        Columns = DEFAULT_COLS;
    }

    if (temp_str = XviFindToolType("LEFT_EDGE="))
    {
      left_edge = atoi(temp_str);

      /* Make sure value is reasonable. */
      if ((left_edge < 0) || (left_edge > MAX_LEFT_EDGE))
        left_edge = 0;
    }

    if (temp_str = XviFindToolType("TOP_EDGE="))
    {
      top_edge = atoi(temp_str);

      /* Make sure value is reasonable. */
      if ((top_edge < 0) || (top_edge > MAX_TOP_EDGE))
        top_edge = 0;
    }

    if (temp_str = XviFindToolType("FONT="))
    {
      /* Need to make a copy of the string because we expect */
      /* that font_str will be freed by Cleanup_IO().        */
      font_str = getmem(strlen(temp_str) + 1);
      (void)strcpy(font_str, temp_str);
    }

    if (temp_str = XviFindToolType("PARMS="))
    {
      /* Make sure we do not have an empty string. */
      if (*temp_str)
      {
        /* Need to make a copy of the string because we will be */
        /* modifying it.  Note that parms_str will be freed by  */
        /* sys_exit().                                          */
        parms_str = getmem(strlen(temp_str) + 1);
        (void)strcpy(parms_str, temp_str);

        /* Count the number of arguments specified by "PARMS". */
        /* Expect that arguments are separated by one or more  */
        /* spaces (tabs will not work).                        */
        for (temp_str = parms_str + 1, parms_argc = 1; *temp_str;
             temp_str++)
        {
          /* Look for a non-space after a space to indicate */
          /* that we have another argument.                 */
          if ((*(temp_str - 1) == ' ') && (*temp_str != ' '))
            parms_argc++;
        }
#if DEBUG > 2
        fprintf(stderr, "  amiga_sys_init(): parms_str=\"%s\"\n",
                parms_str);
#endif

      }
    }

    /* Now fix up real argc and argv. */

    /* Calculate total number of arguments (will be at least 1). */
    *argc_ptr = wb_argc + parms_argc;

    /* Note that this memory will be freed by sys_exit().  Also,     */
    /* we ask for an extra pointer location to store an ending NULL. */
    *argv_ptr = getmem(sizeof(char *) * ((*argc_ptr) + 1));

    /* Always have first workbench argument, which is program name. */
    /* Get it from SAS "C" workbench environment so we get full     */
    /* path name.                                                   */
    (*argv_ptr)[0] = _WBArgv[0];

    index = 1;

    /* Add any PARMS Tool Type arguments to the argv list here. */
    if (parms_str)
      (*argv_ptr)[index++] = parms_str;	/* First PARMS argument. */

    for (temp_str = parms_str + 1, parms_argc = 1; *temp_str;
         temp_str++)
    {
      /* Again look for a non-space after a space */
      /* to find each argument.                   */
      if ((*(temp_str - 1) == ' ') && (*temp_str != ' '))
      {
        *(temp_str - 1) = '\000';	/* Terminate prior argument. */
        if (index < *argc_ptr)	    /* Check argc just to make sure. */
          (*argv_ptr)[index++] = temp_str;	/* Get next argument. */
      }
    }

    /* Add any "files to edit" workbench arguments to argv list.  */
    /* Get them from SAS "C" workbench environment so we get full */
    /* path name for any file(s) to edit.                         */

    /* Get index into _WBArgv where first "file to edit" argument is. */
    index_wb = (_WBArgc - wb_argc) + 1;

    while(index_wb < _WBArgc)
    {
      if (index < *argc_ptr)	    /* Check argc just to make sure. */
        (*argv_ptr)[index++] = _WBArgv[index_wb++];
    }

    /* Put a terminating NULL at end of argv list.*/
    (*argv_ptr)[index] = NULL;

    /* Save the argc and argv POINTERS passed in */
    /* so we can use them at exit time.          */
    saved_argc_ptr = argc_ptr;
    saved_argv_ptr = argv_ptr;
  }
  else
  {
    /* We started from CLI. */
    workbench = FALSE;
  }

  /* Now do the I/O and Amiga windowing initialization. */
  if (error = Init_IO())
  {
    Cleanup_IO(error);	/* Something is wrong, so bail out. */
    sleep(15);		/* Wait 15 seconds for any output to be seen. */
    exit(error);
  }

#ifdef DEBUG
  fprintf(stderr, "< amiga_sys_init(): argc=%d\n", *argc_ptr);
  for (index = 0; index < *argc_ptr; index++)
  {
    fprintf(stderr, "    argv[%d]=\"%s\"\n",
            index, (*argv_ptr)[index]);
  }
  /* Wait 4 seconds so output can be seen if running from CLI. */
  if (!workbench)
    sleep(4);
#endif
}


/*____________________________________________________________________*/
/* sys_exit()                                                         */
/*   Exit the program to the system, cleaning up any system stuff.    */
/*                                                                    */
/* PARMS: r - Return code to return to the system.                    */
/*____________________________________________________________________*/
/* FUNCTION */

void sys_exit(r)
  int r;
{
  tty_goto(Rows-1, 0);
  outchar('\n');
  flush_output();	/* Flush the above remaining output. */
  if (r && !workbench)
    sleep(4);		/* Wait for any displayed errors to be seen. */

  Cleanup_IO(0);	/* Do normal cleanup. */


  if (workbench)
  {
    /* Free any memory from getmem() calls in amiga_sys_init(). */
    if(parms_str)
      free(parms_str);
    free(*saved_argv_ptr);

    /* Now put back the original values into argv and argc. */
    *saved_argv_ptr = (char *)wb_startup;
    *saved_argc_ptr = 0;
  }

  exit(r);
}


/*____________________________________________________________________*/
/* tty_goto()                                                         */
/*   Send the cursor to "row_num" and "col_num" position on the       */
/*   screen.                                                          */
/*                                                                    */
/*   A string containing the appropiate control sequence is created   */
/*   and then sent to outstr().  This 9 character string contains:    */
/*   "\233###;###H" where \233 is the <CSI>, the first ### is the row */
/*   number as decimal digits and the second ### is the column number.*/
/*                                                                    */
/* PARMS: row_num - row number                                        */
/*        col_num - column number                                     */
/*____________________________________________________________________*/
/* FUNCTION */

void tty_goto(row_num, col_num)
  int row_num;
  int col_num;
{
  int remainder;

  /* 11-character array of characters to hold 10-character string */
  /* plus terminating NULL.                                       */
  char str[10] = {'\233',0,0,0,';',0,0,0,'H','\000'};

#if DEBUG > 2
  fprintf(stderr, "> tty_goto(): row_num=%d  col_num=%d\n",
   row_num, col_num);
#endif

#if DEBUG > 1
  ttygoto_count++;
#endif

  /* Correct for row/column numbering starting at zero. */
  row_num++;
  col_num++;

  /* Make sure we don't go past 999 limit. */
  if (row_num > 999)
    row_num = 999;
  if (col_num > 999)
    col_num = 999;

  /* Handle the row digits. */
  str[1] = '0' + (row_num / 100);
  str[2] = '0' + ((remainder = row_num % 100) / 10);
  str[3] = '0' + (remainder % 10);

  /* Handle the column digits. */
  str[5] = '0' + (col_num / 100);
  str[6] = '0' + ((remainder = col_num % 100) / 10);
  str[7] = '0' + (remainder % 10);

  outstr(str);		/* Send string to screen. */

#if DEBUG > 3
 		 	/* Print string but skip leading ESC. */
  fprintf(stderr, "< tty_goto(): str=\"%s\"\n", &str[1]);
#endif
}


/*____________________________________________________________________*/
/* sys_pipe()                                                         */
/*   Used for the ! command.  The first parameter is the command to   */
/*   invoke, while the second and third are functions which should    */
/*   be called with an open file pointer in order to write out old,   */
/*   or read in new lines, respectively.                              */
/*                                                                    */
/*   On the Amiga this function might have been implemented using the */
/*   PIPE: handler but it would take almost as much work and there    */
/*   might be collisions with other applications using the same named */
/*   pipes.  Also, some commands might need the whole file present as */
/*   input wheras PIPE: has a 4K buffer that is a stream.             */
/*                                                                    */
/* PARMS: command           Command to invoke.                        */
/*        file_write_funct(FILE *)  Function to write old lines to    */
/*                                   temp file.                       */
/*        file_read_funct(FILE *)   Function to read new lines from   */
/*                                   temp file.                       */
/* RETURNS: TRUE if all succeeds.                                     */
/*____________________________________________________________________*/
/* FUNCTION */

bool_t sys_pipe(command, file_write_funct, file_read_funct)
  char *command;
  int (*file_write_funct)();
  long (*file_read_funct)();
{
  char buffer[120];
  char tmp_infname[L_tmpnam];
  char tmp_outfname[L_tmpnam];
  FILE *tmp_infile;
  FILE *tmp_outfile;
  int sys_error;
  int error = 0;          /* Assume success. */
  bool_t ret_val = TRUE;  /* Assume success. */


#if DEBUG > 1
  fprintf(stderr, "> sys_pipe(): command=%s\n", command);
#endif

  /* Skip any leading blanks in command. */
  command = stpblk(command);

  /* Check for null command. */
  if (!(*command))
    goto exit_pt;

  /* Get a unique, temporary filenames for input and output. */
  (void)strcpy(tmp_infname, tmpnam(NULL));
  (void)strcpy(tmp_outfname, tmpnam(NULL));

#if DEBUG > 1
  fprintf(stderr, "  sys_pipe(): tmp_infname=\"%s\"\n", tmp_infname);
  fprintf(stderr, "  sys_pipe(): tmp_outfname=\"%s\"\n",
          tmp_outfname);
#endif

  /* Open input temp file for creating, writing, then reading. */
  if (tmp_infile = fopen(tmp_infname, "w+"))
  {
    /* Write "old" lines into input temp file. */
    (void)((file_write_funct)(tmp_infile));

    /* Close input temp file. */ 
    if (!fclose(tmp_infile))
    {
      /* Send the command getting/sending I/O from/to temporary files.*/
      sprintf(buffer, "%s %s %s", command, tmp_infname, tmp_outfname);
      if (!(sys_error = system(buffer)))
      {
        /* Open output temp file for reading. */
        if (tmp_outfile = fopen(tmp_outfname, "r"))
        {
          /* Read "new" lines from output temp file. */
          if (((file_read_funct)(tmp_outfile)) == -1)
            error = 1;

          /* Close output temp file. */ 
          if (fclose(tmp_outfile))
            error = 2;	/* Can't close output file. */
        }
        else		/* Can't open output file. */
          error = 3;

        /* Remove the output temp file. */
        if (remove(tmp_outfname))
          error = 4;	/* Can't remove output file. */
      }
      else		/* Command failed. */
        error = 5;
    }
    else		/* Can't close input file. */
      error = 6;

    /* Remove the input temp file. */
    if (remove(tmp_infname))
      error = 7;	/* Can't remove input file. */
  }
  else  /* if...fopen(tmp_infname...) */
    error = 8;		/* Can't open input file. */


  /* Check for any errors. */
  if (error)
  {
#ifdef DEBUG
    char *error_str;
    switch (error)
    {
      case 1:
        error_str = "(file_read_funct)() failed";
        break;
      case 2:
        error_str = "Can't close output temp file";
        break;
      case 3:
        error_str = "Can't open output temp file";
        break;
      case 4:
        error_str = "Can't remove output temp file";
        break;
      case 5:
        error_str = "Command failed";
        break;
      case 6:
        error_str = "Can't close input temp file";
        break;
      case 7:
        error_str = "Can't remove input temp file";
        break;
      case 8:
        error_str = "Can't open input temp file";
        break;
      default:
        error_str = "???";
    }
    fprintf(stderr, "  sys_pipe(): buffer=\"%s\"\n", buffer);
    fprintf(stderr, "  sys_pipe(): %s\n", error_str);
#endif
    ret_val = FALSE;
  }

exit_pt:

#if DEBUG > 1
  fprintf(stderr, "< sys_pipe(): sys_error=%d, ret_val=%d\n",
          sys_error, ret_val);
#endif
  return(ret_val);
}


/*____________________________________________________________________*/
/* call_system()                                                      */
/*   Invoke the given command sending output to the xvi window.       */
/*                                                                    */
/* PARMS: command   Command to invoke.                                */
/* RETURNS: 0 if all succeeds.                                        */
/*____________________________________________________________________*/
/* FUNCTION */

int call_system(command)
  char *command;
{
  char buffer[120];
  char *tmp_fname;
  FILE *tmp_file;
  int ret_val = 0;  /* Assume success. */

#if DEBUG > 1
  fprintf(stderr, "> call_system(): command=%s\n", command);
#endif

  /* Skip any leading blanks in command. */
  command = stpblk(command);

  /* Check for null command. */
  if (!(*command))
    goto exit_pt;

  /* Get a unique, temporary filename. */
  tmp_fname = tmpnam(NULL);

#ifdef DEBUG
  fprintf(stderr, "  call_system(): tmp_fname=\"%s\"\n", tmp_fname);
#endif

  /* Send the command redirecting output to temporary file. */
  sprintf(buffer, "%s >%s", command, tmp_fname);
  if (ret_val = system(buffer))
  {
    sprintf(buffer, "\nERROR: %s command failed\n", command);
    outstr(buffer);
    goto exit_pt;
  }

  /* Copy temporary file to display. */
  if (!(tmp_file = fopen(tmp_fname, "r")))
  {
    sprintf(buffer, "\nERROR: Cannot open temp file: %s\n", tmp_fname);
    outstr(buffer);
    ret_val = -1;
    goto exit_pt;
  }
  while (fgets(buffer, 120, tmp_file))
    outstr(buffer);
  if (fclose(tmp_file))
  {
    sprintf(buffer, "\nERROR: Cannot close temp file: %s\n", tmp_fname);
    outstr(buffer);
    ret_val = -2;
    goto exit_pt;
  }

  /* Remove the temporary file. */
  if (remove(tmp_fname))
  {
    sprintf(buffer,"\nERROR: Cannot remove temp file: %s\n", tmp_fname);
    outstr(buffer);
    ret_val = -3;
    goto exit_pt;
  }

exit_pt:
  flush_output();	/* Flush any output from above. */

#if DEBUG > 1
  fprintf(stderr, "< call_system(): ret_val=%d\n", ret_val);
#endif
  return(ret_val);
}


/*____________________________________________________________________*/
/* call_shell()                                                       */
/*   Invoke a shell.  On the Amiga this creates a new shell attached  */
/*   to a new window, which is independent of the existing xvi        */
/*   session.  The SHELL variable is checked in this routine even     */
/*   though the same stuff is in "command" if SHELL is defined,       */
/*   becuase we want to use a default shell command if SHELL is not   */
/*   defined.                                                         */
/*                                                                    */
/* PARMS: command   Shell to invoke. (unused)                         */
/* RETURNS: 0 if all succeeds.                                        */
/*____________________________________________________________________*/
/* FUNCTION */

int call_shell(command)
  char *command;
{
  char *shell_str;
  char *shell_start_str;
  char *tmp_str = NULL;	/* No temp string to start. */
  int ret_val = 0;	/* Assume success. */
  bool_t default_shell = FALSE;

#if DEBUG > 1
  fprintf(stderr, "> call_shell(): command=%s\n", command);
#endif

  /* Use the SHELL environment variable if it exists. */
  if (!(shell_str = getenv("SHELL")))
  {
    default_shell = TRUE;  /* Can't free the default shell string.*/
    shell_str = DFLT_SHELL;
  }

  /* Use the XVISHSTART environment variable, if it exists, */
  /* to get an xvi specific shell startup file.             */
  if (shell_start_str = getenv("XVISHSTART"))
  {
    tmp_str = getmem(strlen(shell_str) + strlen(shell_start_str) + 10);
    sprintf(tmp_str, "%s FROM %s", shell_str, shell_start_str);

    /* Free any memory from getenv(). */
    if (!default_shell)
      free(shell_str);
    free(shell_start_str);

    default_shell = FALSE;
    shell_str = tmp_str;
  }

#if DEBUG > 1
  fprintf(stderr, "  call_shell(): shell_str=\"%s\"\n", shell_str);
#endif

  ret_val = system(shell_str);	/* Make a new shell. */

  /* Free any remaining memory gotten from getmem() or getenv(). */
  if (!default_shell)
    free(shell_str);

#if DEBUG > 1
  fprintf(stderr, "< call_shell(): ret_val=%d\n", ret_val);
#endif
  return(ret_val);
}

/*____________________________________________________________________*/
/* sys_startv()                                                       */
/*   Switch to "visual" mode.  (not used)                             */
/*____________________________________________________________________*/
/* FUNCTION */

void sys_startv()
{
}

/*____________________________________________________________________*/
/* sys_endv()                                                         */
/*   Switch out of "visual" mode.  (not used)                         */
/*____________________________________________________________________*/
/* FUNCTION */

void sys_endv()
{
}


/*____________________________________________________________________*/
/* can_write()                                                        */
/*   Determines if file "filename" can be written, i.e. if a call to  */
/*   fopenwb(filename) will succeed.                                  */
/*                                                                    */
/* PARMS: filename  File to check.                                    */
/* RETURNS: TRUE if the file can be written to.                       */
/*____________________________________________________________________*/

/* FUNCTION */
bool_t can_write(filename)
  char  *filename;
{
  bool_t ret_value;

#if DEBUG > 1
  fprintf(stderr, "> can_write(): filename=\"%s\"\n", filename);
#endif

  /* If file does not exist or if file */
  /* exists but is writable we're OK.  */
  if (access(filename, 0) == -1 || access(filename, 2) == 0)
    ret_value = TRUE;
  else
    ret_value = FALSE;

#if DEBUG > 1
  fprintf(stderr, "< can_write(): ret_value=%d\n", ret_value);
#endif
  return (ret_value);
}


/*____________________________________________________________________*/
/* set_colour()                                                       */
/*   Set text, commands, system, etc colors.                          */
/*   Note: Do not try to blank cursor in this routine.                */
/*                                                                    */
/*   This routine is currently unused.                                */
/*                                                                    */
/* PARMS: color  Color to set.                                        */
/*____________________________________________________________________*/

/* FUNCTION */
void set_colour(color)
  int color;
{
  int style;
  int bg;
  int fg;

  /* Initialize to "Plain text", BG=grey, FG=black. */
  char color_str[10] =
    {'\233','0',';','4','0',';','3','1','m','\000'};

#if DEBUG > 1
  fprintf(stderr, "> set_colour(): color=%04o (Octal)\n", color);
#endif

  switch (style = color >> 6)
  {
    case 0:
    case 1:
    case 3:
    case 4:
    case 7:
      bg = (color >> 3) & 0x0007;
      fg = color & 0x0007;
      color_str[1] = '0' + style;	/* Set the style. */
      color_str[4] = '0' + bg;		/* Set the background color. */
      color_str[7] = '0' + fg;		/* Set the foreground color. */
      outstr(color_str);		/* Send it to the console. */
      break;

    default:
      color_str[0] = '\000';
  }

#if DEBUG > 1
  /* Skip <CSI> when printing. */
  if (color_str[0])
    fprintf(stderr, "< set_colour(): color_str=\"%s\"\n", color_str+1);
  else
    fprintf(stderr, "< set_colour(): Color handling disabled\n");
#endif
}


/*____________________________________________________________________*/
/* tempfname()                                                        */
/*   Create a unique filename, possibly using "srcname" as a base,    */
/*   for use by do_preserve() to create a backup file for the file    */
/*   named "srcname".  On the Amiga this filename should NOT have a   */
/*   path of "T:" (directory for temporary files) since this is in    */
/*   RAM: and will be lost with a system crash defeating the purpose  */
/*   of do_preserve().                                                */
/*                                                                    */
/*   WARNING: The string returned must  have been allocated using     */
/*   malloc() or a derivative; NULL may be returned if there is no    */
/*   more memory available.                                           */
/*                                                                    */
/* PARMS: srcname  Basename for unique filename.                      */
/* RETURNS: TRUE if the file can be written to.                       */
/*____________________________________________________________________*/

/* FUNCTION */
char *tempfname(srcname)
  char *srcname;
{
  char *fname_str = NULL;
  char *tmp_str;

#ifdef DEBUG
  fprintf(stderr, "> tempfname(): srcname=\"%s\"\n", srcname);
#endif

  /* Allocate memory for the unique filename string. */
  if (fname_str = getmem(L_tmpnam + strlen(srcname)))
  {
    tmp_str = tmpnam(NULL);  /* Get a unique filename. */

    /* Replace "T:" at start of string from tmpnam() */
    /* with "__" and append it to basename.          */
    sprintf(fname_str, "%s__%s", srcname, tmp_str + 2);
  }

#ifdef DEBUG
  fprintf(stderr, "< tempfname(): fname_str=\"%s\"\n", fname_str);
#endif
  return(fname_str);
}


/*____________________________________________________________________*/
/* exists()                                                           */
/*   Check for existance of a file.                                   */
/*                                                                    */
/* PARMS: filename  File to check for existance.                      */
/* RETURNS: TRUE if the file exists.                                  */
/*____________________________________________________________________*/

/* FUNCTION */
bool_t exists(char *filename)
{
  BPTR lock;
  bool_t retval;

#if DEBUG > 1
  fprintf(stderr, "> exists(): filename=\"%s\"\n", filename);
#endif

  if ((lock = findpath(filename)) == ((BPTR)-1))
    retval = FALSE;
  else
  {
    UnLock(lock);
    retval = TRUE;
  }

#if DEBUG > 1
  fprintf(stderr, "< exists(): retval=%d\n", retval);
#endif
  return (retval);
}


/*____________________________________________________________________*/
/* amiga_version()                                                    */
/*   Put up a requester with copyright and version information.       */
/*____________________________________________________________________*/

/* FUNCTION */
void amiga_version()
{
#ifdef OLD_AMIGA_OS /*________________________________________________*/
  /* For pre-2.0 Amiga OS.  Last two numbers are for pre-2.0 OS       */
  /* requestor size parameters.  Only need negative-text for 1 button.*/
  if (AutoRequest(xvi_window, xvi_ver_bodytext,
                  NULL, xvi_ver_negtext, 0, 0, 630, 50))
#else /*______________________________________________________________*/
  /* For 2.0 Amiga OS or later. */
  (void) EasyRequestArgs(xvi_window, xvi_ver_requester, NULL, NULL);
#endif /*_____________________________________________________________*/

  repaint = TRUE;
}

/* End of file________________________________________________________*/
