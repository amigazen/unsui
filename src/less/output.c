/*
 * High level routines dealing with the output to the screen.
 */

#include "less.h"


public int errmsgs;     /* Count of messages displayed by error() */

extern int sigs;
extern int sc_width, sc_height;
extern int ul_width, ue_width;
extern int so_width, se_width;
extern int bo_width, be_width;
#ifdef AMIGA
extern int it_width, ie_width;
extern int nv_width, ne_width;
extern int nrow;  /* vertical screen size */
extern int ShowCR;	/* suppress ^M characters? */
#endif
#ifdef EIGHTBIT
extern int bs_mode;
#endif
extern int tabstop;
extern int twiddle;
extern int any_display;
extern char *line;
extern char *first_cmd;

/*
 * Display the line which is in the line buffer.
 */
#ifdef __STDC__
void put_line (void)
#else
        public void
put_line()
#endif
{
#ifdef AMIGA
        register unsigned char *p;
#else
        register char *p;
#endif
        register int c;
        register int column;
        extern int auto_wrap, ignaw;

        if (sigs)
                /*
                 * Don't output if a signal is pending.
                 */
                return;

        if (line == NULL)
                line = (twiddle) ? "~" : "";

        column = 0;
#ifdef AMIGA
        for (p = (unsigned char *)line;  *p != '\0';  p++)
#else
        for (p = line;  *p != '\0';  p++)
#endif
        {
                switch (c = *p)
                {
                case UL_CHAR:
                        ul_enter();
                        column += ul_width;
                        break;
                case UE_CHAR:
                        ul_exit();
                        column += ue_width;
                        break;
                case BO_CHAR:
                        bo_enter();
                        column += bo_width;
                        break;
                case BE_CHAR:
                        bo_exit();
                        column += be_width;
                        break;
#ifdef AMIGA
                case IT_CHAR:
                        it_enter();
                        column += it_width;
                        break;
                case IE_CHAR:
                        it_exit();
                        column += ie_width;
                        break;
                case NV_CHAR:
                        nv_enter();
                        column += nv_width;
                        break;
                case NE_CHAR:
                        nv_exit();
                        column += ne_width;
                        break;
#endif
                case '\t':
                        do
                        {
                                putchr(' ');
                                column++;
                        } while ((column % tabstop) != 0);
                        break;
                case '\b':
#ifdef EIGHTBIT
                        if (bs_mode == BS_CONTROL)
                        {
                                putchr('^');
                                putchr(carat_char(c));
                                column += 2;
                                break;
                        }
#endif
                        putbs();
                        column--;
                        break;
                default:
#ifdef EIGHTBIT
                                /* Control characters are still control
                                 * characters.  Replace them with some-
                                 * thing printable.
                                 */
#ifdef AMIGA
						if ( ShowCR || c != '\r' )
#endif
                        if (control_char(c))
                        {
                                putchr('^');
                                putchr(carat_char(c));
                                column += 2;
                        }
#else
                        if (c & 0200)
                        {
                                /*
                                 * Control characters arrive here as the
                                 * normal character [carat_char(c)] with
                                 * the 0200 bit set.  See pappend().
                                 */
                                putchr('^');
                                putchr(c & 0177);
                                column += 2;
                        }
#endif
                        else
                        {
                                putchr(c);
                                column++;
                        }
                }
        }
        if (column < sc_width || !auto_wrap || ignaw)
                putchr('\n');
}

/*
 * Is a given character a "control" character?
 * {{ ASCII DEPENDENT }}
 */
#ifdef __STDC__
int control_char (int c)
#else
        public int
control_char(c)
        int c;
#endif
{
#ifdef EIGHTBIT
        /* SAS says 0xff is a control character, for some reason */
        return iscntrl(c) && c != 0xff;
#else
        return ( c < ' ' || c == '\177' );
#endif
}

/*
 * Return the printable character used to identify a control character
 * (printed after a carat; e.g. '\3' => "^C").
 * {{ ASCII DEPENDENT }}
 */
#ifdef __STDC__
int carat_char (int c)
#else
        public int
carat_char(c)
        int c;
#endif
{
        return ((c == '\177') ? '?' : (c | 0100));
}


static char obuf[1024];
static char *ob = obuf;

/*
 * Flush buffered output.
 */
#ifdef __STDC__
void flush (void)
#else
        public void
flush()
#endif
{
#ifdef AMIGA
        ttwrite ( obuf, (long) (ob-obuf));
#else
        write(1, obuf, ob-obuf);
#endif
        ob = obuf;
}

/*
 * Discard buffered output.
 */
#ifdef __STDC__
void dropout (void)
#else
        public void
dropout()
#endif
{
        ob = obuf;
}

/*
 * Output a character.
 */
#ifdef __STDC__
void putchr (int c)
#else
        public void
putchr(c)
        int c;
#endif
{
        if (ob >= &obuf[sizeof(obuf)])
                flush();
        *ob++ = c;
}

/*
 * Output a string.
 */
#ifdef __STDC__
void putstr (register char *s)
#else
        public void
putstr(s)
        register char *s;
#endif
{
        while (*s != '\0')
                putchr(*s++);
}

/*
 * Output a message in the lower left corner of the screen
 * and wait for carriage return.
 */

static char return_to_continue[] = "  (press RETURN)";

#ifdef __STDC__
void error (char *s)
#else
        public void
error(s)
        char *s;
#endif
{
        register int c;
        static char buf[2];
#ifdef AMIGA
		struct Screen *wbs;
#endif

        errmsgs++;
#ifdef AMIGA
        /* nrow tells us if the window has been opened yet.
           any_display tells us if we have initialized reading a file yet.
           We display error in the window if there is one, else to Output(),
           which does not, however, exist in a WB environment.  Thus
           if there is no Less window, also Beep the display.
        */
        if ( nrow < 2 )
#else
        if (!any_display)
#endif
        {
                /*
                 * Nothing has been displayed yet.
                 * Output this message on error output (file
                 * descriptor 2) and don't wait for a keystroke
                 * to continue.
                 *
                 * This has the desirable effect of producing all
                 * error messages on error output if standard output
                 * is directed to a file.  It also does the same if
                 * we never produce any real output; for example, if
                 * the input file(s) cannot be opened.  If we do
                 * eventually produce output, code in edit() makes
                 * sure these messages can be seen before they are
                 * overwritten or scrolled away.
                 */
#ifdef AMIGA
                /* If the window is too small, s may be NULL on Amiga */
                if ( s )
                    Write ( Output(), s, strlen(s) );
                else
                    Write ( Output(), "Error", 5 );
                Write ( Output(), "\n", 1 );
                wbs = MyFindWB ();
                DisplayBeep ( wbs );
                MyFreeWB (wbs);
#else
                write(2, s, strlen(s));
                write(2, "\n", 1);
#endif
                return;
        }

        lower_left();
        clear_eol();
        so_enter();
#ifdef AMIGA
        if ( s )
#endif
        putstr(s);
        putstr(return_to_continue);
        so_exit();

#if ONLY_RETURN
        while ((c = getchr()) != '\n' && c != '\r')
                bell();
#else
        c = getchr();
        if (c != '\n' && c != '\r' && c != ' ')
        {
                buf[0] = c;
                first_cmd = buf;
        }
#endif
        lower_left();

#ifdef AMIGA
        if ( s &&
                strlen(s) + sizeof(return_to_continue) +
                so_width + se_width + 1 > sc_width
                )
#else
        if (strlen(s) + sizeof(return_to_continue) +
                so_width + se_width + 1 > sc_width)
#endif
                /*
                 * Printing the message has probably scrolled the screen.
                 * {{ Unless the terminal doesn't have auto margins,
                 *    in which case we just hammered on the right margin. }}
                 */
                repaint();

        flush();
}
