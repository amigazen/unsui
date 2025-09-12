#include <stdio.h>
#include "tools.h"

/*
 * Compares line and pattern.  Line is a character string while pat
 * is a pattern template made by getpat().
 * Returns:
 *	1. A zero if no match was found.
 *
 *	2. A pointer to the last character satisfing the match
 *	   if ret_endp is non-zero.
 *
 *	3. A pointer to the beginning of the matched string if
 *	   ret_endp is zero.
 *
 * e.g.:
 *
 *	matchs ("1234567890", getpat("4[0-9]*7), 0);
 * will return a pointer to the '4', while:
 *
 *	matchs ("1234567890", getpat("4[0-9]*7), 1);
 * will return a pointer to the '7'.
 */
char	*
matchs(line, pat, ret_endp)
char	*line;
TOKEN	*pat;
int	ret_endp;
{

	char	*rval, *bptr;

	bptr = line;

	while(*line)
	{
		if ((rval = amatch(line, pat, bptr)) == 0)
		{
			line++;
		} else {
			if(rval > bptr && rval > line)
				rval--;	/* point to last char matched */
			rval = ret_endp ? rval : line;
			break;
		}
	}
	return (rval);
}
