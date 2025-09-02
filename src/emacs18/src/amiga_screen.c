#include "config.h"
#undef NULL
#include "lisp.h"
#include "termchar.h"
#include "dispextern.h"

#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <internal/devices.h>
#include <internal/vars.h>

#define min(x,y) ((x) > (y) ? (y) : (x))
#define max(x,y) ((x) < (y) ? (y) : (x))

#undef LONGBITS

#include <exec/types.h>
#include <exec/interrupts.h>
#include <devices/input.h>
#include <devices/inputevent.h>
#include <intuition/intuitionbase.h>
#include <intuition/intuition.h>
#include <devices/conunit.h>
#include <devices/inputevent.h>
#include <graphics/gfxbase.h>
#include <graphics/gfxmacros.h>
#include <utility/hooks.h>
#include <workbench/startup.h>
#include <workbench/workbench.h>

#include <proto/exec.h>
#include <proto/dos.h>
#include <proto/intuition.h>
#include <proto/graphics.h>
#include <proto/console.h>
#include <proto/diskfont.h>
#include <proto/wb.h>

/* this is defined for those unlucky enough
 * not to have the 3.0 headers  -ch3/16/93. */
#ifndef WA_NewLookMenus
#define WA_NewLookMenus (WA_Dummy + 0x30)
#endif

#include "amiga.h"

#define SHIFT_MASK (IEQUALIFIER_LSHIFT | IEQUALIFIER_RSHIFT)
#define CONTROL_MASK IEQUALIFIER_CONTROL
#define META_MASK IEQUALIFIER_LALT

struct GfxBase *GfxBase;
struct IntuitionBase *IntuitionBase;
struct Library *DiskfontBase, *KeymapBase, *WorkbenchBase;

static char intkey_code, intkey_qualifier;
static struct IOStdReq *input_req;
static struct Interrupt int_handler_hook;
static int hooked;

static struct MsgPort *wbport;
static struct AppWindow *emacs_app_win;
static struct AppIcon *emacs_icon;

struct Library *ConsoleDevice;
static struct TextFont *font;
static int font_opened;
/* The reset string resets the console, turns off scrolling and sets up
   the foreground & background colors. */
#define CONSOLE_RESET "\x1b""c\x9b>1l\x9b""3%d;4%d;>%dm"
static char reset_string[20]; /* Must be big enough for
			  printf(CONSOLE_RESET, foreground, background, background);
			  (0 <= foreground, background <= 7) */

/* These are the pen numbers for emacs window's base colors */
int foreground = 1, background = 0;

/* Current window, and its main characteristics */
struct Window *emacs_win;
WORD emacs_x = 0, emacs_y = 0, emacs_w = 640, emacs_h = 200;
char *emacs_screen_name;
char emacs_screen_name_storage[MAXPUBSCREENNAME+1];
int emacs_backdrop = 0;		/* Use backdrop window ? */

/* Current window size: */
#define EMACS_X() (emacs_win ? emacs_win->LeftEdge : emacs_x)
#define EMACS_Y() (emacs_win ? emacs_win->TopEdge : emacs_y)
#define EMACS_W() (emacs_win ? emacs_win->Width : emacs_w)
#define EMACS_H() (emacs_win ? emacs_win->Height : emacs_h)

/* used for setting the color of standout text  -ch3/16/93. */
int inverse_fill_pen = 8, inverse_text_pen = 8;

/* IO request for all console io. */
static struct IOStdReq *emacs_console;

/* a storage area for the name of the screen last opened on */

#define emacs_icon_width 57
#define emacs_icon_height 55
#define emacs_icon_num_planes 1
#define emacs_icon_words_per_plane 220

UWORD chip emacs_icon_data[1][55][4] = {
  {
    0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
    0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
    0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0fe0,0x6000,
    0x0000,0x0000,0x0060,0x6000,0x0000,0x0000,0x0fff,0xe000,
    0x0000,0x0000,0x1800,0x2000,0x0000,0x0000,0x13ff,0xa000,
    0x0000,0x0000,0x1400,0xa000,0x0000,0x0000,0x3600,0xa000,
    0x0000,0x0000,0x0000,0xa000,0x0000,0x0000,0x0c00,0xa000,
    0x0000,0x0000,0x1e00,0xa000,0x0000,0x0000,0x0c00,0xa000,
    0x0000,0x0000,0x0000,0xa000,0x0000,0x0000,0x2100,0xa000,
    0x0000,0x0000,0x3300,0xa000,0x0000,0x0000,0x0c00,0xa000,
    0x003f,0xffff,0xffff,0xb000,0x001f,0xffff,0xffff,0x8000,
    0x004e,0x0000,0x0001,0xf000,0x00c6,0x00f0,0x0001,0x8000,
    0x00c6,0x0100,0x0001,0x8000,0x0006,0x0103,0x9201,0x8000,
    0x0006,0x013a,0x5201,0x8000,0x00c6,0x010a,0x5201,0x8000,
    0x00c6,0x010a,0x5601,0x8000,0x0086,0x00f2,0x4a01,0x8000,
    0x0006,0x0000,0x0001,0x8000,0x0046,0x0000,0x0001,0x8000,
    0x00c6,0x7c00,0x0001,0x8000,0x00c6,0x4000,0x0001,0x8000,
    0x0006,0x41d8,0xc319,0x8000,0x0006,0x7925,0x24a1,0x8000,
    0x00c6,0x4125,0x2419,0x8000,0x01c6,0x4125,0x2485,0x8000,
    0x0086,0x7d24,0xd319,0x8000,0x0007,0x0000,0x0003,0x8000,
    0x0003,0xffe3,0xffff,0x0000,0x0081,0xfff7,0xfffe,0x0000,
    0x01c0,0x0036,0x0000,0x0000,0x0180,0x0014,0x0f80,0x0000,
    0x0000,0x0014,0x1040,0x0000,0x0000,0x0014,0x2720,0x0000,
    0x0000,0x0012,0x28a0,0x0000,0x0080,0x000a,0x48a0,0x0000,
    0x01c0,0x0009,0x90a0,0x0000,0x0180,0x0004,0x20a0,0x0000,
    0x0000,0x0003,0xc0a0,0x0000,0x0000,0x0000,0x00a0,0x0000,
    0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,
    0x0000,0x0000,0x0000,0x0000
  },
};

struct Image far emacs_icon_image = {
  0, 0,
  emacs_icon_width, emacs_icon_height, emacs_icon_num_planes,
  (UWORD *)emacs_icon_data,
  3, 0,
  0
};

static struct DiskObject far emacs_icon_object = {
  0, 0,
  { 0, 0, 0, emacs_icon_width, emacs_icon_height, 0, 0, 0, (APTR)&emacs_icon_image },
  0, 0, 0,
  NO_ICON_POSITION, NO_ICON_POSITION
};

static struct Hook background_hook;

#define EVENTSIZE 32

static struct event {
  ULONG class;
  UWORD code, qual;
  WORD x, y;
} events[EVENTSIZE];
static int event_num, event_in, event_out;

static struct wbevent {
  struct wbevent *next;
  char file[1];
} *wbevents;

Lisp_Object Vamiga_mouse_pos;
Lisp_Object Vamiga_mouse_item;
extern Lisp_Object MouseMap;
int amiga_remap_bsdel;
int amiga_remap_numeric_keypad;
int amiga_mouse_initialized;
int amiga_wb_initialized;
int emacs_iconified;

static int amiga_pos_x(int x)
{
  return (x - emacs_win->BorderLeft) / emacs_win->RPort->Font->tf_XSize;
}

static int amiga_pos_y(int y)
{
  return (y - emacs_win->BorderTop) / emacs_win->RPort->Font->tf_YSize;
}

static void amiga_change_size(void)
{
  int new_height = amiga_pos_y(emacs_win->Height - emacs_win->BorderBottom);
  int new_width = amiga_pos_x(emacs_win->Width - emacs_win->BorderRight);

  /* Hack to force redisplay */
  if (screen_height == new_height) screen_height--;
  /* I consider that refreshes are possible during a select, which is
     true for the current state of emacs */
  change_screen_size(new_height, new_width, 0, !selecting && !waiting_for_input, 1);
}

/* Get terminal size from system.
   Store number of lines into *heightp and width into *widthp.
   If zero or a negative number is stored, the value is not valid.  */

void get_window_size (widthp, heightp)
     int *widthp, *heightp;
{
  if (emacs_win)
    {
      *heightp = amiga_pos_y(emacs_win->Height - emacs_win->BorderBottom);
      *widthp = amiga_pos_x(emacs_win->Width - emacs_win->BorderRight);
    }
  else
    {
      *heightp = 0;
      *widthp = 0;
    }
}

static int set_min_size(struct Window *win, struct TextFont *font,
			WORD *minw, WORD *minh)
{
  *minw = 11 * font->tf_XSize + win->BorderLeft + win->BorderRight;
  *minh = 4 * font->tf_YSize + win->BorderTop + win->BorderBottom;

  return (int)WindowLimits(win, *minw, *minh, 0, 0);
}

struct fill
{
  struct Layer *layer;
  struct Rectangle bounds;
  WORD offsetx, offsety;
};

/* __interrupt disables stack checking.   -ch3/19/93. */
static ULONG __asm __saveds __interrupt
fill_background(register __a2 struct RastPort *obj,
		register __a1 struct fill *msg)
{
  struct Layer *l;

  SetAPen(obj, background);
  SetDrMd(obj, JAM1);
  SetAfPt(obj, 0, 0);
  SetWrMsk(obj, 0xff);
  /* Gross hack starts here */
  l = obj->Layer;
  obj->Layer = 0;
  /* Stops */
  RectFill(obj, msg->bounds.MinX, msg->bounds.MinY,
	   msg->bounds.MaxX, msg->bounds.MaxY);
  /* Starts again */
  obj->Layer = l;
  /* And finally dies */

  return 0;
}

static void clear_window(void)
{
  SetAPen(emacs_win->RPort, background);
  RectFill(emacs_win->RPort, emacs_win->BorderLeft, emacs_win->BorderTop,
	   emacs_win->Width - emacs_win->BorderRight - 1,
	   emacs_win->Height - emacs_win->BorderBottom - 1);
}

static int make_reset_string(void)
{
  sprintf(reset_string, CONSOLE_RESET, foreground, background, background);
}

void reset_window(void)
{
  make_reset_string();
  if (emacs_win)
    {
      screen_puts (reset_string, strlen(reset_string));
      clear_window();
      amiga_change_size ();
    }
}

static void close_app_win(void)
{
  if (emacs_app_win)
    {
      struct AppMessage *msg;

      RemoveAppWindow(emacs_app_win); /* What can I do if it fails ?! */
      while (msg = (struct AppMessage *)GetMsg(wbport)) ReplyMsg(msg);
    }
}

static int close_emacs_window(void)
{
  close_app_win();
  inputsig &= ~(1L << emacs_win->UserPort->mp_SigBit);
  _device_close(emacs_console);
  if(emacs_win)
    {
      /* put title back the way it should be   -ch3/19/93. */
      ShowTitle(emacs_win->WScreen, !emacs_backdrop);
    }
  CloseWindow(emacs_win);
  emacs_console = 0;
  emacs_win = 0;
  ConsoleDevice = 0;
}

/* We need this function becuase we do not always have the string
 * for the screen we opened on. for example LockPubScreen(NULL);
 * This function will get the name by looping through all public
 * screens looking for the one that matches ours. -ch3/20/93 */

char *get_screen_name(struct Screen *this, char *namebuf)
{
  struct PubScreenNode *pubscreens =
    (struct PubScreenNode *)LockPubScreenList()->lh_Head;

  while (pubscreens->psn_Node.ln_Succ)
    {
      if (pubscreens->psn_Screen == this)
	{
	  strcpy(namebuf, pubscreens->psn_Node.ln_Name);
	  UnlockPubScreenList();
	  return namebuf;
	}
      pubscreens = (struct PubScreenNode *)pubscreens->psn_Node.ln_Succ;
    }
  /* Failed to find screen */
  namebuf[0] = '\0';
  UnlockPubScreenList();

  return 0;
}

/* added two parameters to eliminate the need for the global
 * which was causing some unwanted effect (bugs). -ch3/19/93 */

static enum { ok, no_screen, no_window }
open_emacs_window(UWORD x, UWORD y, UWORD w, UWORD h, int backdrop,
		  char *pubscreen_name)
     /* Open or reopen emacs window */
{
  WORD minw, minh;
  struct Screen *new_screen;
  struct Window *new_win;
  struct IOStdReq *new_console;
  int no_backdrop = !backdrop;

  new_screen = LockPubScreen(pubscreen_name);

  if (!new_screen)
    return no_screen;

  /* removed newwindow structure, and added as tag
   * items so that we can change them easier. -ch3/16/93. */

  new_win = OpenWindowTags(0, WA_Left, x, WA_Top, y,
			      WA_Width, w, WA_Height, h,	/* Static items */
			      WA_AutoAdjust, 1, WA_NewLookMenus, 1,
			      WA_IDCMP, IDCMP_CLOSEWINDOW | IDCMP_RAWKEY |
			                IDCMP_MOUSEBUTTONS| IDCMP_NEWSIZE |
			                IDCMP_MENUPICK | IDCMP_MENUHELP,
			      WA_PubScreen, new_screen,
			      WA_BackFill, &background_hook,
			      WA_MenuHelp, 1, WA_Activate, 1,
			      WA_SimpleRefresh, 1,
			      WA_MaxWidth, -1, WA_MaxHeight, -1,
			      WA_Backdrop, backdrop,	/* changing items */
			      WA_Borderless, backdrop,
			      WA_CloseGadget, no_backdrop,
			      WA_SizeGadget, no_backdrop,
			      WA_DragBar, no_backdrop,
			      WA_DepthGadget, no_backdrop,
			      WA_Title, no_backdrop ?
			       "GNU Emacs 18.59, Amiga port "VERS : 0,
                              TAG_END);

  UnlockPubScreen(0L, new_screen);

  if (new_win)
    {
      /* if emacs_backdrop then the screen title will show BEHIND the window
	 -ch3/16/93. */
      ShowTitle(new_screen, !emacs_backdrop);
      SetFont(new_win->RPort, font);

      if (set_min_size(new_win, font, &minw, &minh) &&
	  (new_console = (struct IOStdReq *)
	   _device_open("console.device", CONU_CHARMAP, CONFLAG_NODRAW_ON_NEWSIZE,
			(APTR)new_win, sizeof(*new_win),
			sizeof(struct IOStdReq))))
	{
	  inputsig |= 1L << new_win->UserPort->mp_SigBit;
	  ConsoleDevice = (struct Library *)new_console->io_Device;
	  emacs_app_win = AddAppWindowA(0, 0, new_win, wbport, 0);

	  /* Copy the info into permanent storage */
	  emacs_win = new_win;
	  emacs_console = new_console;

	  /* fetch the name of the current screen -ch3/19/93 */
	  emacs_screen_name = get_screen_name(emacs_win->WScreen,
					      emacs_screen_name_storage);

	  emacs_backdrop = backdrop;

	  reset_window();

	  return ok;
	}
      CloseWindow(new_win);
    }
  return no_window;
}

void force_window(void)
{
  if (!emacs_win && !emacs_iconified)
    {
      if (open_emacs_window(emacs_x, emacs_y, emacs_w, emacs_h, emacs_backdrop,
			    emacs_screen_name) != ok)
	{
	  /* Try to return to defaults (Workbench, etc) */
	  if (open_emacs_window(0, 0, 640, 200, 0, 0) != ok)
	      _fail("I've lost my window ! Exiting.");
	}
      resume_menus();
    }
}

/* returns:
 *	-2 if msg is not class RAWKEY
 *	same as RawKeyConvert otherwise:
 *	buffer length if <= kbsize
 *	-1 else
 */
static DeadKeyConvert(struct IntuiMessage *msg, UBYTE *kbuffer, int kbsize,
		      struct KeyMap *kmap)
{
  static struct InputEvent ievent = {0, IECLASS_RAWKEY, 0, 0, 0};
  int extra = 0, res;

  if (msg->Class != RAWKEY) return (-2);

  /* Do some keymapping ourselves to make emacs users happy */

  /* Ctrl-space becomes Ctrl-@ */
  if (msg->Code == 0x40 && msg->Qualifier & CONTROL_MASK)
    {
      *kbuffer = 0;
      return 1;
    }
  /* Backspace becomes DEL */
  if (msg->Code == 0x41 && amiga_remap_bsdel)
    {
      *kbuffer = 0177;
      return 1;
    }
  /* And DEL becomes CTRL-D */
  if (msg->Code == 0x46 && amiga_remap_bsdel)
    {
      *kbuffer = 04;
      return 1;
    }
  /* Stick numeric pad prefix in front of numeric keypad chars */
  if (msg->Qualifier & IEQUALIFIER_NUMERICPAD && amiga_remap_numeric_keypad)
    {
      *kbuffer++ = 'x' & 037;
      *kbuffer++ = '^' & 037;
      *kbuffer++ = 'K';
      kbsize -= 3;
      extra = 3;
    }

  /* pack input event */
  ievent.ie_Code = msg->Code;

  /* Ignore meta in decoding keys */
  ievent.ie_Qualifier = msg->Qualifier & ~META_MASK;

  /* get previous codes from location pointed to by IAddress
   *  this pointer is valid until IntuiMessage is replied.
   */
  ievent.ie_position.ie_addr = *((APTR *)msg->IAddress);
  ievent.ie_position.ie_dead.ie_prev1DownQual &= ~META_MASK;
  ievent.ie_position.ie_dead.ie_prev2DownQual &= ~META_MASK;

  res = RawKeyConvert(&ievent, kbuffer, kbsize, kmap);
  return res ? res + extra : 0;
}

void add_wbevent(struct WBArg *wbarg)
{
  char filename[256];

  if (wbarg->wa_Lock && NameFromLock(wbarg->wa_Lock, filename, 256))
    {
      struct wbevent *event;

      if (wbarg->wa_Name) AddPart(filename, wbarg->wa_Name, 256);
      if (event = (struct wbevent *)malloc(offsetof(struct wbevent, file) +
					   strlen(filename) + 1))
	{
	  event->next = wbevents;
	  strcpy(event->file, filename);
	  wbevents = event;
	}
    }
}

void check_window(int force)
{
  ULONG class;
  USHORT code, qualifier;
  UWORD mx, my;
  unsigned char buf[32];
  int buflen, deiconify, i;
  struct IntuiMessage *msg;
  int mouse_event = FALSE, wb_event = FALSE;
  struct AppMessage *amsg;

  force_window();

  if (emacs_win)
    while (msg = (struct IntuiMessage *)GetMsg(emacs_win->UserPort))
      {
	class = msg->Class;
	code = msg->Code;
	qualifier = msg->Qualifier;
	mx = msg->MouseX; my = msg->MouseY;
	buflen = DeadKeyConvert(msg, buf, 32, 0);
	ReplyMsg(msg);

	switch (class)
	  {
	  case IDCMP_CLOSEWINDOW: {
	    enque(030, FALSE); enque(03, FALSE); /* ^X^C */
	    break;
	  }
	  case IDCMP_RAWKEY: {
	    if (buflen > 0)
	      {
		unsigned char *sbuf = buf;
		int meta = qualifier & META_MASK;

		/* Don't set META on CSI */
		do enque(*sbuf++, meta); while (--buflen);
	      }
	    break;
	  }
	  case IDCMP_NEWSIZE: amiga_change_size(); break;
	  case IDCMP_MENUPICK: case IDCMP_MENUHELP:
	    if (code == MENUNULL) break; /* else fall through */
	  case IDCMP_MOUSEBUTTONS: {
	    mouse_event = TRUE;
	    if (event_num == EVENTSIZE) break;

	    events[event_in].class = class;
	    events[event_in].code = code;
	    events[event_in].qual = qualifier;
	    events[event_in].x = mx;
	    events[event_in].y = my;
	    event_num++;
	    event_in = (event_in + 1) % EVENTSIZE;

	    break;
	  }
	  }
      }
  /* Handle App requests */
  while (amsg = (struct AppMessage *)GetMsg(wbport))
    {
      /* Add an event for all these files */
      for (i = 0; i < amsg->am_NumArgs; i++) add_wbevent(amsg->am_ArgList + i);
      wb_event = TRUE;
      /* Reply to the message, and deiconify if was icon */
      deiconify = amsg->am_NumArgs == 0;
      ReplyMsg(amsg);
      if (deiconify && emacs_icon)
	/* Reopen window */
	if (open_emacs_window(emacs_x, emacs_y, emacs_w, emacs_h, emacs_backdrop,
			      emacs_screen_name) == ok)
	  {
	    resume_menus();
	    RemoveAppIcon(emacs_icon);
	    emacs_icon = 0;
	    emacs_iconified = 0;
	  }
    }

  if (amiga_mouse_initialized && (force && event_num > 0 || mouse_event))
    {
      enque(AMIGASEQ, FALSE); enque('M', FALSE);
    }
  if (amiga_wb_initialized && (force && wbevents || wb_event))
    {
      enque(AMIGASEQ, FALSE); enque('W', FALSE);
    }
}

void setup_intchar(char intchar)
{
  char cqbuf[2];

  if (MapANSI(&intchar, 1, cqbuf, 1, 0) == 1)
    {
      intkey_code = cqbuf[0];
      intkey_qualifier = cqbuf[1];
    }
  else
    {
      /* Default is CTRL-G in usa0 keymap */
      intkey_code = 0x24;
      intkey_qualifier = IEQUALIFIER_CONTROL;
    }
}

/* Hack to detect interrupt char as soon as it is pressed */
/* __interrupt disables stack checking.  -ch3/19/93.*/
static long __saveds __interrupt __asm
int_handler(register __a0 struct InputEvent *ev)
{
  struct InputEvent *ep, *laste;
  static struct InputEvent retkey;
  ULONG lock = LockIBase(0);

  if (emacs_win && IntuitionBase->ActiveWindow == emacs_win)
    {
      laste = 0;

      /* run down the list of events to see if they pressed the magic key */
      for (ep = ev; ep; laste = ep, ep = ep->ie_NextEvent)
	if (ep->ie_Class == IECLASS_RAWKEY &&
	    (ep->ie_Qualifier & 0xff) == intkey_qualifier &&
	    ep->ie_Code == intkey_code)
	  {
	    /* Remove this key from input sequence */
	    if (laste) laste->ie_NextEvent = ep->ie_NextEvent;
	    else ev = ep->ie_NextEvent;

	    Vquit_flag = Qt;
	    Signal(_us, SIGBREAKF_CTRL_C);
	  }
    }
  UnlockIBase(lock);

  /* pass on the pointer to the event */
  return (long)ev;
}

DEFUN ("amiga-mouse-events", Famiga_mouse_events, Samiga_mouse_events, 0, 0, 0,
       "Return number of pending mouse events from Intuition.")
     ()
{
  register Lisp_Object tem;

  check_intuition ();

  XSET (tem, Lisp_Int, event_num);

  return tem;
}

DEFUN ("amiga-proc-mouse-event", Famiga_proc_mouse_event, Samiga_proc_mouse_event,
       0, 0, 0,
       "Pulls a mouse event out of the mouse event buffer and dispatches\n\
the appropriate function to act upon this event.")
()
{
  register Lisp_Object mouse_cmd;
  register char com_letter;
  register char key_mask;
  register Lisp_Object tempx;
  register Lisp_Object tempy;
  extern Lisp_Object get_keyelt ();
  extern int meta_prefix_char;
  struct event *ev;
  int posx, posy;

  check_intuition ();

  if (event_num) {
    ev = &events[event_out];
    event_out = (event_out + 1) % EVENTSIZE;
    event_num--;
    if (ev->class == MOUSEBUTTONS)
      {
	switch (ev->code)
	  {
	  case SELECTDOWN: com_letter = 2; break;
	  case SELECTUP: com_letter = 6; break;
	  case MIDDLEDOWN: com_letter = 1; break;
	  case MIDDLEUP: com_letter = 5; break;
	  case MENUDOWN: com_letter = 0; break;
	  case MENUUP: com_letter = 4; break;
	  default: com_letter = 3; break;
	  }
	posx = amiga_pos_x(ev->x);
	posy = amiga_pos_y(ev->y);
	XSET (tempx, Lisp_Int, min (screen_width-1, max (0, posx)));
	XSET (tempy, Lisp_Int, min (screen_height-1, max (0, posy)));
      }
    else
      {
	/* Must be Menu Pick or Help */
	com_letter = ev->class == IDCMP_MENUPICK ? 3 : 7;

	/* The parameters passed describe the selected item */
	XSET (tempx, Lisp_Int, MENUNUM(ev->code));
	XSET (tempy, Lisp_Int, ITEMNUM(ev->code));
      }
    if (ev->qual & META_MASK) com_letter |= 0x20;
    if (ev->qual & SHIFT_MASK) com_letter |= 0x10;
    if (ev->qual & CONTROL_MASK) com_letter |= 0x40;

    Vamiga_mouse_pos = Fcons (tempx, Fcons (tempy, Qnil));
    Vamiga_mouse_item = make_number (com_letter);
    mouse_cmd = get_keyelt (access_keymap (MouseMap, com_letter));
    if (NULL (mouse_cmd)) {
      bell ();
      Vamiga_mouse_pos = Qnil;
    }
    else return call1 (mouse_cmd, Vamiga_mouse_pos);
  }
  return Qnil;
}

DEFUN ("amiga-get-mouse-event", Famiga_get_mouse_event, Samiga_get_mouse_event,
       1, 1, 0,
       "Get next mouse event out of mouse event buffer (com-letter (x y)).\n\
ARG non-nil means return nil immediately if no pending event;\n\
otherwise, wait for an event.")
(arg)
Lisp_Object arg;
{
  register char com_letter;
  register char key_mask;

  register Lisp_Object tempx;
  register Lisp_Object tempy;
  struct event *ev;
  int posx, posy;

  check_intuition ();

  if (NULL (arg))
    {
      amiga_consume_input();
      while (!event_num)
	{
	  int rfds = 1;

	  select(1, &rfds, 0, 0, 0);
	  amiga_consume_input();
	}
    }
  /*** ??? Surely you don't mean to busy wait??? */

  if (event_num) {
    ev = &events[event_out];
    event_out = (event_out + 1) % EVENTSIZE;
    event_num--;
    switch (ev->code)
      {
      case SELECTDOWN: com_letter = 2; break;
      case SELECTUP: com_letter = 6; break;
      case MIDDLEDOWN: com_letter = 1; break;
      case MIDDLEUP: com_letter = 5; break;
      case MENUDOWN: com_letter = 0; break;
      case MENUUP: com_letter = 4; break;
      default: com_letter = 3; break;
      }
    if (ev->qual & META_MASK) com_letter |= 0x20;
    if (ev->qual & SHIFT_MASK) com_letter |= 0x10;
    if (ev->qual & CONTROL_MASK) com_letter |= 0x40;

    posx = amiga_pos_x(ev->x);
    posy = amiga_pos_y(ev->y);
    XSET (tempx, Lisp_Int, min (screen_width-1, max (0, posx)));
    XSET (tempy, Lisp_Int, min (screen_height-1, max (0, posy)));

    Vamiga_mouse_pos = Fcons (tempx, Fcons (tempy, Qnil));
    Vamiga_mouse_item = make_number (com_letter);
    return Fcons (com_letter, Fcons (Vamiga_mouse_pos, Qnil));
  }
  return Qnil;
}

DEFUN ("amiga-get-wb-event", Famiga_get_wb_event, Samiga_get_wb_event,
       1, 1, 0,
       "Get next Workbench event out of workbench event buffer (a file name).\n\
ARG non-nil means return nil immediately if no pending event;\n\
otherwise, wait for an event.")
(arg)
Lisp_Object arg;
{
  Lisp_Object file;
  struct wbevent *ev;

  check_intuition ();

  if (NULL (arg))
    {
      amiga_consume_input();
      while (!wbevents)
	{
	  int rfds = 1;

	  select(1, &rfds, 0, 0, 0);
	  amiga_consume_input();
	}
    }
  /*** ??? Surely you don't mean to busy wait??? */

  if (wbevents) {
    file = build_string(wbevents->file);
    ev = wbevents;
    wbevents = wbevents->next;
    free(ev);
    return file;
  }
  return Qnil;
}

DEFUN("amiga-set-foreground-color", Famiga_set_foreground_color,
      Samiga_set_foreground_color, 1, 1, "nPen number: ",
      "Use PEN as foreground color")
     (pen)
{
  int fg;

  check_intuition();
  CHECK_NUMBER(pen, 0);

  fg = XUINT (pen);
  if (pen > 7) error("Pen colors must be between 0 & 7");
  foreground = fg;
  reset_window();
  return Qnil;
}

DEFUN("amiga-set-background-color", Famiga_set_background_color,
      Samiga_set_background_color, 1, 1, "nPen number: ",
      "Use PEN as background color")
     (pen)
{
  int bg;

  check_intuition();
  CHECK_NUMBER(pen, 0);

  bg = XUINT (pen);
  if (pen > 7) error("Pen colors must be between 0 & 7");
  background = bg;
  reset_window();
  return Qnil;
}

DEFUN("amiga-set-inverse-fill-pen", Famiga_set_inverse_fill_pen,
      Samiga_set_inverse_fill_pen, 1, 1, "nPen number: ",
      "Use PEN's color for inverse fills (0-7 or 8 for reverse)")
     (pen)
{
  int ifp = 8;

  check_intuition();
  CHECK_NUMBER(pen, 0);

  ifp = XUINT (pen);
  if (pen > 8)
    error("choices are from 0 to 8");
  inverse_fill_pen = ifp;
  reset_window();
  return Qnil;
}

DEFUN("amiga-set-inverse-text-pen", Famiga_set_inverse_text_pen,
      Samiga_set_inverse_text_pen, 1, 1, "nPen number: ",
      "Use PEN's color for inverse fills (0-7 or 8 for reverse)")
     (pen)
{
  int itp = 8;

  check_intuition();
  CHECK_NUMBER(pen, 0);

  itp = XUINT (pen);
  if (pen > 8)
    error("choices are from 0 to 8");
  inverse_text_pen = itp;
  reset_window();
  return Qnil;
}

DEFUN("amiga-set-font", Famiga_set_font, Samiga_set_font, 2, 2,
      "sFont: \n\
nSize: ",
      "Set font used for window to FONT with given HEIGHT.\n\
The font used must be non-proportional.")
(wfont, height)
{
  struct TextAttr attr;
  struct TextFont *newfont;
  char *fname;
  struct Lisp_String *fstr;
  WORD minw, minh, oldmw, oldmh;

  CHECK_STRING (wfont, 0);
  CHECK_NUMBER (height, 0);

  check_intuition();

  fstr = XSTRING (wfont);
  fname = (char *)alloca (fstr->size + 6);
  strcpy (fname, fstr->data);
  strcat (fname, ".font");
  attr.ta_Name = fname;
  attr.ta_YSize = XFASTINT (height);
  attr.ta_Style = 0;
  attr.ta_Flags = 0;
  newfont = OpenDiskFont (&attr);

  if (!newfont)
    error ("Font %s %d not found", fstr->data, XFASTINT (height));
  if (newfont->tf_Flags & FPF_PROPORTIONAL)
    {
      CloseFont(newfont);
      error ("Font %s %d is proportional", fstr->data, XFASTINT (height));
    }

  if (emacs_win)
    {
      if (!set_min_size(emacs_win, newfont, &minw, &minh))
	{
	  CloseFont(newfont);
	  if (!set_min_size(emacs_win, font, &oldmw, &oldmh))
	    _fail("Failed to restore old font, exiting.");
	  error("Window is too small for this font, need at least %d(w) by %d(h)",
		minw, minh);
	}
      SetFont(emacs_win->RPort, newfont);
    }
  if (font_opened) CloseFont(font);
  font_opened = TRUE;
  font = newfont;
  reset_window();
  return Qnil;
}

DEFUN("amiga-set-geometry", Famiga_set_geometry, Samiga_set_geometry, 4, MANY, 0,
      "Set Emacs window geometry and screen.\n\
First 4 parameters are the (X,Y) position of the top-left corner of the window\n\
and its WIDTH and HEIGHT. These must be big enough for an 11x4 characters window.\n\
If nil is given for any of these, that means to keep the same value as before.\n\
The optional argument SCREEN specifies which screen to use, nil stands for the\n\
same screen as the window is on, t stands for the default public screen (normally\n\
the Workbench), a string specifies a given public screen.\n\
If optional argument BACKDROP is t, a backdrop window is used.")
  (nargs, args)
    int nargs;
    Lisp_Object *args;
{
  Lisp_Object x, y, w, h, scr = Qnil, backdrop = Qnil;
  int opened;
  WORD tempx, tempy, tempw, temph;
  char *screen_name;
  int use_backdrop;

  if (nargs > 6) error("Too many arguments to amiga-set-geometry");
  x = args[0]; y = args[1]; w = args[2]; h = args[3];
  if (nargs > 4)
    {
      scr = args[4];
      if (nargs > 5) backdrop = args[5];
    }

  check_intuition();

  if (!NULL (x))
    {
      CHECK_NUMBER(x, 0);
      tempx = XUINT(x);
    }
  else tempx = EMACS_X();
  if (!NULL (y))
    {
      CHECK_NUMBER(y, 0);
      tempy = XUINT(y);
    }
  else tempy = EMACS_Y();
  if (!NULL (w))
    {
      CHECK_NUMBER(w, 0);
      tempw = XUINT(w);
    }
  else tempw = EMACS_W();
  if (!NULL (h))
    {
      CHECK_NUMBER(h, 0);
      temph = XUINT(h);
    }
  else temph = EMACS_H();

  use_backdrop = !NULL(backdrop);

  if (scr == Qt) screen_name = 0; /* set to zero for def. */
  else if (!NULL (scr))
    {
      CHECK_STRING (scr, 0);
      screen_name = XSTRING (scr)->data;
    }
  else screen_name = emacs_screen_name;

  if (emacs_win)
    {
      struct Window *old_win = emacs_win;
      struct IOStdReq *old_console = emacs_console;

      suspend_menus();
      opened = open_emacs_window(tempx, tempy, tempw, temph, use_backdrop,
				 screen_name);
      if (opened != ok)
	{
	  resume_menus();

	  if (opened == no_window) error("Failed to open desired window");
	  else if (screen_name)
	    error("Unknown public screen %s", screen_name);
	  else error("The default screen wasn't found !?");
	}

      _device_close(old_console);
      CloseWindow(old_win);
      if (!resume_menus()) error("Failed to recover menus (No memory?)");
    }
  else /* No window, set defaults */
    {
      emacs_screen_name = screen_name;
      if (screen_name)
	{
	  emacs_screen_name_storage[MAXPUBSCREENNAME] = '\0';
	  strncpy(emacs_screen_name_storage, screen_name, MAXPUBSCREENNAME);
	}
      emacs_x = tempx;
      emacs_y = tempy;
      emacs_w = tempw;
      emacs_h = temph;
      emacs_backdrop = use_backdrop;
    }
  return Qnil;
}


/* The next 2 functions are very usefull for writing
 * arexx/lisp functions that interact with other programs
 * that will be sharing the same screen.  -ch3/19/93. */

DEFUN("amiga-get-window-geometry",
      Famiga_get_window_geometry, Samiga_get_window_geometry, 0, 0, 0,
      "Get Emacs window geometry.\n\
a list returned is of the form:  (iconified x y width height backdrop)\n\
where x, y, width, height are integers, backdrop is t or nil and iconified\n\
is t if the window is iconified and nil otherwise")
()
{
  Lisp_Object x, y, w, h, b, i;

  XSET(x, Lisp_Int, EMACS_X());
  XSET(y, Lisp_Int, EMACS_Y());
  XSET(w, Lisp_Int, EMACS_W());
  XSET(h, Lisp_Int, EMACS_H());
  b = emacs_backdrop ? Qt : Qnil;
  i = emacs_iconified ? Qt : Qnil;

  return Fcons(i, Fcons(x, Fcons(y, Fcons(w, Fcons(h, Fcons(b, Qnil))))));
}

DEFUN("amiga-get-screen-geometry",
      Famiga_get_screen_geometry, Samiga_get_screen_geometry, 0, 0, 0,
      "Get geometry of the screen emacs window resides on.\n\
a list returned is of the form:  (name x y width height)\n\
where name is a string, x, y, width, height are integers.\n\
Only the public screen name is returned if the window is not currently open.\n\
In this last case, the name may be nil if the window will be opened on the\n\
default public screen.")
()
{
  Lisp_Object name;

  if (emacs_screen_name) name = Qnil;
  else name = build_string(emacs_screen_name);

  if(emacs_win)
    {
      struct Screen *s = emacs_win->WScreen;
      Lisp_Object x, y, w, h;

      XSET(x, Lisp_Int, s->LeftEdge);
      XSET(y, Lisp_Int, s->TopEdge);
      XSET(w, Lisp_Int, s->Width);
      XSET(h, Lisp_Int, s->Height);

      return Fcons(name, Fcons(x, Fcons(y, Fcons(w, Fcons(h, Qnil)))));
    }
  return Fcons(name, Qnil);
}

DEFUN("amiga-iconify", Famiga_iconify, Samiga_iconify, 0, 0, "",
      "Toggle the emacs iconification state.")
()
{
  check_intuition();

  if (emacs_iconified)
    {
      /* Deiconify */

      /* Reopen window */
      if (open_emacs_window(emacs_x, emacs_y, emacs_w, emacs_h, emacs_backdrop,
			    emacs_screen_name) != ok)
	error("Failed to deiconify (No memory?)");
      resume_menus();

      RemoveAppIcon(emacs_icon);
      emacs_icon = 0;
      emacs_iconified = 0;
    }
  else
    if (emacs_icon = AddAppIconA(0, 0, "Emacs", wbport, 0, &emacs_icon_object, 0))
      {
	if (emacs_win)
	  {
	    /* Close window */
	    emacs_x = EMACS_X(); emacs_y = EMACS_Y();
	    emacs_w = EMACS_W(); emacs_h = EMACS_H();
	    suspend_menus();
	    close_emacs_window();
	  }
	emacs_iconified = 1;
      }
    else error("Iconify attempt failed\n");

  return Qnil;
}

DEFUN("amiga-set-icon-pos", Famiga_set_icon_pos, Samiga_set_icon_pos, 2, 2,
"nX position: \n\
nY position: ",
      "Set the X Y position of the icon for emacs when iconified.")
  (Lisp_Object x, Lisp_Object y)
{
  long xpos, ypos;

  if (NULL (x)) emacs_icon_object.do_CurrentX = NO_ICON_POSITION;
  else
    {
      CHECK_NUMBER (x, 0);
      emacs_icon_object.do_CurrentX = XINT(x);
    }
  if (NULL (y)) emacs_icon_object.do_CurrentY = NO_ICON_POSITION;
  else
    {
      CHECK_NUMBER (y, 0);
      emacs_icon_object.do_CurrentY = XINT(y);
    }

  return Qnil;
}

struct EClockVal scount[16], ecount[16];
long total[16], counting[16], nb[16], susp[16];

void start_count(int n)
{
  nb[n]++;
  if (counting[n]) printf("Restarted %d\n", n);
  counting[n] = 1;
  /*ReadEClock(&scount[n]);*/
}

void stop_count(int n)
{
  if (counting[n])
    {
      /*ReadEClock(&ecount[n]);*/
      counting[n] = 0;

      total[n] += ecount[n].ev_lo - scount[n].ev_lo;
    }
}

void suspend_count(int n)
{
  if (counting[n] && susp[n]++ == 0)
    {
      /*ReadEClock(&ecount[n]);*/
      total[n] += ecount[n].ev_lo - scount[n].ev_lo;
    }
}

void resume_count(int n)
{
  if (counting[n] && --susp[n] == 0) /*ReadEClock(&scount[n])*/;
}

disp_counts(void)
{
  int i;

  for (i = 0; i < 16; i++)
    {
      printf("%d(%d) ", total[i], nb[i]);
      total[i] = nb[i] = 0;
    }
  printf("\n");
}

void screen_puts(char *str, unsigned int len)
{
  if (emacs_win)
    {
      int i;

      emacs_console->io_Command = CMD_WRITE;
      emacs_console->io_Data    = (APTR)str;
      emacs_console->io_Length  = len;

      /*    start_count(0);
	    for (i = 1; i <= 6; i++) suspend_count(i);*/
      DoIO(emacs_console);
      /*    for (i = 1; i <= 6; i++) resume_count(i);
	    stop_count(0);*/
    }
}

DEFUN ("amiga-activate-window", Famiga_activate_window, Samiga_activate_window, 0, 0, 0,
       "Makes emacs window the currently active one.")
     ()
{
  if(emacs_win) {
    ActivateWindow(emacs_win);
    return Qnil;
  }
  error("No window to make active.");
  return Qnil;
}

DEFUN ("amiga-window-to-front", Famiga_window_to_front, Samiga_window_to_front, 0, 0, 0,
       "Pulls the emacs window to the front (including screen)")
     ()
{
  if(emacs_win) {
    WindowToFront(emacs_win);
    ScreenToFront(emacs_win->WScreen);
    return Qnil;
  }
  error("No window to pull to the front.");
  return Qnil;
}

DEFUN ("amiga-window-to-back", Famiga_window_to_back, Samiga_window_to_back, 0, 0, 0,
       "Pushes the emacs window to the back (including screen)")
     ()
{
  if(emacs_win) {
    WindowToBack(emacs_win);
    ScreenToBack(emacs_win->WScreen);
    return Qnil;
  }
  error("No window to push back.");
  return Qnil;
}


void syms_of_amiga_screen(void)
{
  DEFVAR_LISP ("amiga-mouse-item", &Vamiga_mouse_item,
	       "Encoded representation of last mouse click, corresponding to\n\
numerical entries in amiga-mouse-map.");
  Vamiga_mouse_item = Qnil;
  DEFVAR_LISP ("amiga-mouse-pos", &Vamiga_mouse_pos,
	       "Current x-y position of mouse by row, column as specified by font.");
  Vamiga_mouse_pos = Qnil;

  DEFVAR_BOOL ("amiga-remap-bsdel", &amiga_remap_bsdel,
	       "*If true, map DEL to Ctrl-D and Backspace to DEL. \n\
This is the most convenient (and default) setting. If nil, don't remap.");
  amiga_remap_bsdel = 1;

  DEFVAR_BOOL ("amiga-remap-numeric-keypad", &amiga_remap_numeric_keypad,
	       "*If true, numeric keypad keys are prefixed with C-x C-^ K.\n\
This enables you to remap them, but causes problems with functions like\n\
isearch-forward-regexp on some keyboards. Default to true.");
  amiga_remap_numeric_keypad = 1;

  DEFVAR_BOOL ("amiga-mouse-initialized", &amiga_mouse_initialized,
	       "Set to true once lisp has been setup to process mouse commands.\n\
No mouse processing request (C-X C-^ M) will be queued while this is nil.");
  amiga_mouse_initialized = 0;

  DEFVAR_BOOL ("amiga-wb-initialized", &amiga_wb_initialized,
	       "Set to true once lisp has been setup to process workbench commands.\n\
No workbench processing request (C-X C-^ W) will be queued while this is nil.");
  amiga_mouse_initialized = 0;

  defsubr (&Samiga_mouse_events);
  defsubr (&Samiga_proc_mouse_event);
  defsubr (&Samiga_get_mouse_event);
  defsubr (&Samiga_get_wb_event);
  defsubr (&Samiga_set_font);
  defsubr (&Samiga_set_geometry);
  defsubr (&Samiga_set_background_color);
  defsubr (&Samiga_set_foreground_color);
  defsubr (&Samiga_iconify);
  defsubr (&Samiga_set_icon_pos);

  /* New functions  -ch3/19/93. */
  defsubr (&Samiga_set_inverse_text_pen);
  defsubr (&Samiga_set_inverse_fill_pen);
  defsubr (&Samiga_window_to_front);
  defsubr (&Samiga_window_to_back);
  defsubr (&Samiga_activate_window);
  defsubr (&Samiga_get_window_geometry);
  defsubr (&Samiga_get_screen_geometry);

}

void init_amiga_screen(void)
{
  event_num = event_in = event_out = 0;

  if (!((IntuitionBase = (struct IntuitionBase *)
	 OpenLibrary("intuition.library", 37L)) &&
	(GfxBase = (struct GfxBase *)OpenLibrary("graphics.library", 0L)) &&
	(DiskfontBase = OpenLibrary("diskfont.library", 0L)) &&
	(WorkbenchBase = OpenLibrary("workbench.library", 37)) &&
	(KeymapBase = OpenLibrary("keymap.library", 36)) &&
	(input_req = (struct IOStdReq *)_device_open("input.device", 0, 0, 0, 0,
						     sizeof(struct IOStdReq)))))
    _fail("Need version 2.04 and diskfont.library!");

  if (!(wbport = CreateMsgPort())) no_memory();

  /* Add Ctrl-G detector */
  int_handler_hook.is_Data = 0;
  int_handler_hook.is_Code = (void *)int_handler;
  int_handler_hook.is_Node.ln_Pri = 100; /* 100 not 127 is the standard value
					  * for input stream handlers.  -ch3/19/93. */
  /* it is standard for interrupts to have names  -ch3/19/93.*/
  int_handler_hook.is_Node.ln_Name = "GNU Emacs CTRL-G handler";
  input_req->io_Command = IND_ADDHANDLER;
  input_req->io_Data = (APTR)&int_handler_hook;

  /* wasn't checking for error. -ch3/19/93. */
  if(0 == DoIO(input_req))
    hooked = TRUE;
  else
    {
      hooked = FALSE;
      _fail("couldn't get input handler hook for CTRL-G");
    }

  inputsig |= 1L << wbport->mp_SigBit;

  background_hook.h_Entry = (ULONG (*)()) fill_background; /* added cast. */
  font = GfxBase->DefaultFont;

  init_amiga_menu();
}

void cleanup_amiga_screen(void)
{
  if (hooked)
    {
      input_req->io_Command = IND_REMHANDLER;
      input_req->io_Data = (APTR)&int_handler_hook;
      DoIO(input_req);
    }
  close_app_win();
  if (wbport) DeleteMsgPort(wbport);
  cleanup_amiga_menu();
  _device_close(emacs_console);
  if (emacs_win) CloseWindow(emacs_win);
  if (font_opened) CloseFont(font);
  if (IntuitionBase) CloseLibrary(IntuitionBase);
  if (GfxBase) CloseLibrary(GfxBase);
  if (DiskfontBase) CloseLibrary(DiskfontBase);
  if (WorkbenchBase) CloseLibrary(WorkbenchBase);
  if (KeymapBase) CloseLibrary(KeymapBase);
  _device_close(input_req);
}
