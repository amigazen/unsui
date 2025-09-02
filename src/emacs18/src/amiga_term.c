/* Amiga terminal control routines.
   Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <stdio.h>
#include <ctype.h>
#include "config.h"
#include "termhooks.h"
#include "termchar.h"
#include "termopts.h"

/* internal state */

/* nonzero means supposed to write text in standout mode.  */
static int standout_requested;

static int standout_mode;	/* Nonzero when in standout mode.  */

static char tens[100] = {
    '0', '0', '0', '0', '0', '0', '0', '0', '0', '0',
    '1', '1', '1', '1', '1', '1', '1', '1', '1', '1',
    '2', '2', '2', '2', '2', '2', '2', '2', '2', '2',
    '3', '3', '3', '3', '3', '3', '3', '3', '3', '3',
    '4', '4', '4', '4', '4', '4', '4', '4', '4', '4',
    '5', '5', '5', '5', '5', '5', '5', '5', '5', '5',
    '6', '6', '6', '6', '6', '6', '6', '6', '6', '6',
    '7', '7', '7', '7', '7', '7', '7', '7', '7', '7',
    '8', '8', '8', '8', '8', '8', '8', '8', '8', '8',
    '9', '9', '9', '9', '9', '9', '9', '9', '9', '9',
};

static char ones[100] = {
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
};

#define addnum(str, num) if (num < 100) \
                             { *--str = ones[num]; *--str = tens[num]; } \
                         else do { *--str = '0' + num % 10; num /= 10; } while (num != 0)

static background_highlight ();
static turn_off_highlight ();


/* Cursor motion stuff (from cm.c) */
static int curX, curY;

/* Move to absolute position, specified origin 0 */

Amove_cursor (row, col)
{
  char buf[32], *pos = buf + 32;

  if (curY == row && curX == col)
    return;

  curX = col; curY = row;
  *--pos = 'H';
  col = col + 1; row = row + 1;
  addnum(pos, col);
  *--pos = ';';
  addnum(pos, row);
  *--pos = 0x9b;
  emacs_output(pos, buf + 32 - pos);
  if (pos < buf) abort();
}


Aring_bell ()
{
  emacs_output("\07", 1);
}

Aset_terminal_modes ()
{
}

Areset_terminal_modes ()
{
  turn_off_highlight ();
}

Aupdate_begin ()
{
  /* Hide cursor */
  emacs_output("\x9b\x30\x20\x70", 4);
}

Aupdate_end ()
{
  background_highlight ();
  standout_requested = 0;
  emacs_output("\x9b\x20\x70", 3); /* Show cursor */
}


/* Handle highlighting when TN_standout_width (termcap sg) is not specified.
   In these terminals, output is affected by the value of standout
   mode when the output is written.

   These functions are called on all terminals, but do nothing
   on terminals whose standout mode does not work that way.  */

static turn_off_highlight ()
{
  if (standout_mode)
    {
      extern int background, foreground;      
      extern int inverse_fill_pen, inverse_text_pen;
      int b = background + 40, f = foreground + 30;
      if(inverse_fill_pen < 8 && inverse_text_pen < 8)
	{
	  char buf[32], *pos = buf + 32;
	  /* UnDo inverse fill */
	  *--pos = '\0';
	  *--pos = 'm';
	  addnum(pos, b);
	  *--pos = ';';
	  /* UnDo inverse text */
	  addnum(pos, f);
	  *--pos = 0x9b;
	  emacs_output(pos, buf + 32 - pos);
	}
      else
	{
	  emacs_output("\x9b""27m", 4);
	}
    }
  standout_mode = 0;
}

static turn_on_highlight ()
{
  if (!standout_mode)
    {
      extern int inverse_fill_pen, inverse_text_pen;
      int b = inverse_fill_pen + 40, f = inverse_text_pen + 30;
      if(inverse_fill_pen < 8 && inverse_text_pen < 8)
	{
	  char buf[32], *pos = buf + 32;
	  /* Do inverse fill */
	  *--pos = '\0';
	  *--pos = 'm';
	  addnum(pos, b);
	  *--pos = ';';

	  /* Do inverse text */
	  addnum(pos, f);
	  *--pos = 0x9b;
	  emacs_output(pos, buf + 32 - pos);
	}
      else
	{
	  emacs_output("\x9b\x37m", 3);
	}
    }
  standout_mode = 1;
}

/* Set standout mode to the state it should be in for
   empty space inside windows.  What this is,
   depends on the user option inverse-video.  */

static background_highlight ()
{
  if (inverse_video)
    turn_on_highlight ();
  else
    turn_off_highlight ();
}

/* Set standout mode to the mode specified for the text to be output.  */

static
highlight_if_desired ()
{
  if (!inverse_video == !standout_requested)
    turn_off_highlight ();
  else
    turn_on_highlight ();
}

/* External interface to control of standout mode.
   Call this when about to modify line at position VPOS
   and not change whether it is highlighted.  */

Areassert_line_highlight (highlight, vpos)
     int highlight;
     int vpos;
{
  standout_requested = highlight;
}

/* Call this when about to modify line at position VPOS
   and change whether it is highlighted.  */

Achange_line_highlight (new_highlight, vpos, first_unused_hpos)
     int new_highlight, vpos, first_unused_hpos;
{
  standout_requested = new_highlight;

  move_cursor (vpos, 0);

  background_highlight ();
  clear_end_of_line (first_unused_hpos);
  reassert_line_highlight (new_highlight, curY);
}

/* Erase operations */

/* clear from cursor to end of screen */
Aclear_to_end ()
{
  background_highlight ();
  emacs_output("\x9bJ", 2);
}

/* Clear entire screen */

Aclear_screen ()
{
  background_highlight ();
  emacs_output("\f", 1);
  curX = curY = 0;
}

/* Clear to end of line, but do not clear any standout marker.
   Assumes that the cursor is positioned at a character of real text,
   which implies it cannot be before a standout marker
   unless the marker has zero width.

   Note that the cursor may be moved.  */

Aclear_end_of_line (first_unused_hpos)
     int first_unused_hpos;
{
  if (curX >= first_unused_hpos)
    return;

  background_highlight ();
  emacs_output("\x9bK", 2);
}

Aoutput_chars (string, len)
     register char *string;
     int len;
{
  highlight_if_desired ();

  curX += len;
  emacs_output(string, len);
}

/* If start is zero, insert blanks instead of a string at start */

Ainsert_chars (start, len)
     register char *start;
     int len;
{
  char buf[32], *pos = buf + 32;

  highlight_if_desired ();

  *--pos = '@';
  addnum(pos, len);
  *--pos = 0x9b;
  emacs_output(pos, buf + 32 - pos);
  if (pos < buf) abort();
  if (start) emacs_output(start, len);
}

Adelete_chars (n)
     register int n;
{
  char buf[32], *pos = buf + 32;

  *--pos = 'P';
  addnum(pos, n);
  *--pos = 0x9b;
  emacs_output(pos, buf + 32 - pos);
  if (pos < buf) abort();
}

/* Insert N lines at vpos VPOS.  If N is negative, delete -N lines.  */

Ains_del_lines (vpos, n)
     int vpos, n;
{
  register int i = n > 0 ? n : -n;
  char buf[32], *pos = buf + 32;

  if (n > 0)
    {
      i = n;
      *--pos = 'L';
    }
  else
    {
      i = -n;
      *--pos = 'M';
    }
  if (vpos + i >= screen_height) return;

  move_cursor (vpos, 0);
  background_highlight ();
  addnum(pos, i);
  *--pos = 0x9b;
  emacs_output(pos, buf + 32 - pos);
  if (pos < buf) abort();
}

Acalculate_costs (extra, costvec, ncostvec)
     int extra;
     int *costvec, *ncostvec;
{
  CalcLID(2, 40, extra, 0, costvec, ncostvec);
}

Aset_terminal_window (size)
int size;
{
}


amiga_term_init ()
{
  must_write_spaces = FALSE;
  min_padding_speed = 0;
  memory_below_screen = FALSE;
  meta_key = TRUE;
  scroll_region_ok = FALSE;
  line_ins_del_ok = FALSE;	/* much cleaner display when FALSE  -ch3/19/93. */
  char_ins_del_ok = FALSE;
  fast_clear_end_of_line = TRUE;
  no_redraw_on_reenter = FALSE;

  clear_screen_hook = Aclear_screen;
  clear_end_of_line_hook = Aclear_end_of_line;
  clear_to_end_hook = Aclear_to_end;
  ins_del_lines_hook = Ains_del_lines;
  change_line_highlight_hook = Achange_line_highlight;
  insert_chars_hook = Ainsert_chars;
  output_chars_hook = Aoutput_chars;
  delete_chars_hook = Adelete_chars;
  ring_bell_hook = Aring_bell;
  reset_terminal_modes_hook = Areset_terminal_modes;
  set_terminal_modes_hook = Aset_terminal_modes;
  update_begin_hook = Aupdate_begin;
  update_end_hook = Aupdate_end;
  set_terminal_window_hook = Aset_terminal_window;
  move_cursor_hook = Amove_cursor;
  reassert_line_highlight_hook = Areassert_line_highlight;

  dont_calculate_costs = 1;
  calculate_costs_hook = Acalculate_costs;

  /* Get screen size from system, or else from somewhere ...  */
  get_screen_size (&screen_width, &screen_height);
  /* Random defaults to avoid any problems */
  if (screen_width <= 0) screen_width = 80;
  if (screen_height <= 0) screen_height = 23;

  init_baud_rate ();
}
