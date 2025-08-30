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

#ifdef amiga
extern struct FileHandle *tty;
#else
static int tty;
#endif

/*
 * Open keyboard for input.
 * (Just use file descriptor 2.)
 */
	public void
open_getc()
{
#ifdef amiga
	ttopen();
#else
	tty = 2;
#endif
}

/*
 * Get a character from the keyboard.
 */
	public int
getc()
{
	char c;
	int result;

	reading = 1;
	do
	{
		flush();
#ifdef amiga
		result = Read(tty, &c, 1L);
#else
		result = read(tty, &c, 1);
#endif
	} while (result != 1);
	reading = 0;
	return (c & 0177);
}
