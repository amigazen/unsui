#include "config.h"
#undef NULL
#include "lisp.h"
#include "termchar.h"

#include <stdio.h>
#include <errno.h>
#include <internal/files.h>
#include <internal/vars.h>

#undef LONGBITS

#include <exec/types.h>
#include <dos/dos.h>
#include <proto/exec.h>

#include "amiga.h"
#include "termhooks.h"

static int term_initialised;
ULONG inputsig;

/* A few tty system dependent routines unused on the Amiga */

setpgrp_of_tty(int pid) {}
init_sigio() {}
reset_sigio() {}
request_sigio() {}
unrequest_sigio() {}

/* Return nonzero if safe to use tabs in output.
   At the time this is called, init_sys_modes has not been done yet.  */

tabs_safe_p()
{
  if (noninteractive)
    return 1;

  return 0;			/* Not safe on Amiga !? */
}

/* Get terminal size from system.
   Store number of lines into *heightp and width into *widthp.
   If zero or a negative number is stored, the value is not valid.  */

get_screen_size (widthp, heightp)
     int *widthp, *heightp;
{
    if (term_initialised && !inhibit_window_system)
	get_window_size(widthp, heightp);
    else /* We don't known what size the terminal is */
    {
	*widthp = 0;
	*heightp = 0;
    }
}

init_baud_rate ()
{
  if (noninteractive || !term_initialised) baud_rate = 1200;
  else if (!inhibit_window_system) baud_rate = 38400;
  else baud_rate = serial_baud_rate();
}

void check_intuition ()
{
    if (noninteractive || inhibit_window_system)
        error ("You aren't using a window.");
}

#define TTYBUFSIZE 256		/* Same size as kbd_buffer */
static char ttybuf[TTYBUFSIZE];
static int tty_count;
#define TTYPUT(c) { if (tty_count < TTYBUFSIZE) ttybuf[tty_count++] = c; }

static int interrupt_char;

void enque(unsigned int c, int meta)
/* place input keys in keyboard buffer
   If high bit is set, precede character with ^Q (hack).
   If meta is true, set high bit.
   If both the high bit & meta are true, we have a problem. Ignore it.
   If c == AMIGASEQ (256) enqueue the amiga sequence introducer (C-x C-^)
*/
{
  /* Hack CSI to be AMIGASEQ (to allow defining function keys, etc) */
  if (c == 0233 || c == AMIGASEQ)
    {
      TTYPUT('x' & 037);
      TTYPUT('^' & 037);
    }
  else if (c >= 0200)	/* Special character, precede with ^Q */
    {
      TTYPUT('q' & 037);
      TTYPUT(c);
    }
  else
    {
      if (meta) c |= 0200;
      if (c == interrupt_char) Signal(_us, SIGBREAKF_CTRL_C);
      else TTYPUT(c);
    }
}

int get_ttycount(void)
{
  return tty_count;
}

init_sys_modes ()
{
  extern int quit_char;

  if (noninteractive)
    return;

  if (inhibit_window_system) clear_screen();

  interrupt_char = quit_char;
  if (!inhibit_window_system) setup_intchar(interrupt_char);
}

reset_sys_modes ()
{
  if (noninteractive)
    {
      fflush (stdout);
      return;
    }
  move_cursor (screen_height - 1, 0);
  clear_end_of_line (screen_width);
  /* clear_end_of_line may move the cursor */
  move_cursor (screen_height - 1, 0);
}

void amiga_consume_input(void)
{
  extern int this_command_key_count;
  int force = this_command_key_count == 0;
  /* If force is TRUE & some non-keyboard (eg mouse events) input is pending,
     insert the appropriate magic sequence in the input stream */

  if (term_initialised)
    {
      if (!inhibit_window_system) check_window(force);
      else check_serial(force);
      check_arexx(force, TRUE);
    }
}

discard_tty_input ()
{
  if (noninteractive)
    return;

  amiga_consume_input();
  tty_count = 0;
  chkabort();
}

/* Code for the fd describing the emacs input (terminal or window) */

static ULONG __regargs ttyin_select_start(void *userinfo, int rd, int wr)
{
  if (!inhibit_window_system) force_window();

  return tty_count ? -1 : inputsig;
}

static void __regargs ttyin_select_poll(void *userinfo, int *rd, int *wr)
{
  amiga_consume_input();
  if (!tty_count) *rd = 0;
}

static int __regargs ttyin_read(void *userinfo, void *buffer, unsigned int length)
{
  amiga_consume_input();
  if (length > tty_count) length = tty_count;
  memcpy(buffer, ttybuf, length);
  tty_count -= length;
  if (tty_count) memmove(ttybuf, ttybuf + length, tty_count - length);

  return (int)length;
}

static int __regargs ttyin_write(void *userinfo, void *buffer, unsigned int length)
{
  errno = EACCES;
  return -1;
}

static int __regargs ttyin_lseek(void *userinfo, long rpos, int mode)
{
  errno = ESPIPE;
  return -1;
}

static int __regargs ttyin_close(void *userinfo, int internal)
{
  return 0;
}

static int __regargs ttyin_ioctl(void *userinfo, int request, void *data)
{
  errno = EINVAL;
  return -1;
}

#define CBUFSIZE 1024
#undef fwrite
#undef fflush

char cbuffer[CBUFSIZE + 16], *cbuffer_pos;

int emacs_fflush(FILE *f)
{
    if (noninteractive || f != stdout) return fflush(f);
    else
    {
	int len;

	len = cbuffer_pos - cbuffer;
	if (term_initialised)
	    if (!inhibit_window_system) screen_puts(cbuffer, len);
	    else serial_puts(cbuffer, len);
	if (termscript) fwrite (cbuffer, 1, len, termscript);
	cbuffer_pos = cbuffer;

	return 0;
    }
}

void emacs_putchar(int c)
{
    if (cbuffer_pos >= cbuffer + CBUFSIZE) emacs_fflush(stdout);
    *cbuffer_pos++ = c;
}

void emacs_output(char *str, int size)
{
    if (cbuffer_pos + size > cbuffer + CBUFSIZE) emacs_fflush(stdout);
    if (size > CBUFSIZE)
    {
	if (term_initialised)
	    if (!inhibit_window_system) screen_puts(str, size);
	    else serial_puts(str, size);
    }
    else
    {
	memcpy(cbuffer_pos, str, size);
	cbuffer_pos += size;
    }
}

void emacs_fwrite(char *str, unsigned int nblocks, unsigned int len, FILE *f)
{
    if (noninteractive || f != stdout) fwrite (str, nblocks, len, f);
    else
    {
	unsigned int size;

	if (nblocks == 1) size = len; /* Emacs always uses 1 "block" */
	else size = nblocks * len;

	emacs_output(str, size);
    }
}

void syms_of_amiga_tty(void)
{
  syms_of_amiga_screen();
  syms_of_amiga_rexx();
}

void init_amiga_tty()
{
  inputsig = 0;
  term_initialised = FALSE;
  init_amiga_rexx();
}

void cleanup_amiga_tty()
{
  cleanup_amiga_rexx();
  cleanup_amiga_serial();
  cleanup_amiga_screen();
}

void early_amiga_tty()
{
  cbuffer_pos = cbuffer;
  tty_count = 0;
}

void amiga_term_open(void)
{
  inhibit_window_system ? init_amiga_serial() : init_amiga_screen();
  close(0);
  if (_alloc_fd((void *)1, FI_READ, ttyin_select_start, ttyin_select_poll, ttyin_read,
		ttyin_write, ttyin_lseek, ttyin_close, ttyin_ioctl) == 0)
    term_initialised = TRUE;
  else _fail("Failed to initialise I/O, no memory ?");
}

