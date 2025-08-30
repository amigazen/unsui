/*
 * Routines dealing with getting input from the keyboard (i.e. from the user).
 */

#include "less.h"

/*
 * The boolean "reading" is set true or false according to whether
 * we are currently reading from the keyboard.
 * This information is used by the signal handling stuff in signal.c.
 * {{ There are probably some race conditions here
 *    involving the variable "reading". }}
 */
public int reading;

#ifndef AMIGA
static int tty;
#endif


/*
 * Open keyboard for input.
 * (Just use file descriptor 2.)
 */
#ifdef __STDC__
void open_getchr (void)
#else
        public void
open_getchr()
#endif
{
#ifdef AMIGA
        ttopen();
#else
        tty = 2;
#endif
}

/*
 * Get a character from the keyboard.
 */
#ifdef __STDC__
int getchr (void)
#else
        public int
getchr()
#endif
{
        char c;
        int result;

        reading = 1;
        do
        {
                flush();
#ifdef AMIGA
                c = ttgetc();
                result = 1;
#else
                result = read(tty, &c, 1);
#endif
        } while (result != 1);
        reading = 0;
#ifdef EIGHTBIT
        return (int) c;
#else
        return (c & 0177);
#endif
}
