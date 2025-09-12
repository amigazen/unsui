#include <stdio.h>
#include "tools.h"

/*     Scans throught the pattern template looking for a match
 * with lin.  Each element of lin is compared with the template
 * until either a mis-match is found or the end of the template
 * is reached.  In the former case a 0 is returned; in the latter,
 * a pointer into lin (pointing to the character following the
 * matched pattern) is returned.
 *
 *	"lin"	is a pointer to the line being searched.
 *	"pat"	is a pointer to a template made by makepat().
 *	"boln"	is a pointer into "lin" which points at the
 *			character at the beginning of the line.
 */
char	*
amatch(lin, pat, boln)
char	*lin;
TOKEN	*pat;
char	*boln;
{
	register char	*bocl, *rval, *strstart;

	if(pat == 0)
		return 0;

	strstart = lin;

	while(pat)
	{
		if(pat->tok == CLOSURE && pat->next)
		{
				/* Process a closure:
				 * first skip over the closure token to the
				 * object to be repeated.  This object can be
				 * a character class.
				 */

			pat = pat->next;

				/* Now match as many occurrences of the
				 * closure pattern as possible.
				 */
			bocl = lin;

			while( *lin && omatch(&lin, pat))
				;

				/* 'Lin' now points to the character that made
				 * made us fail.  Now go on to process the
				 * rest of the string.  A problem here is
				 * a character following the closure which
				 * could have been in the closure.
				 * For example, in the pattern "[a-z]*t" (which
				 * matches any lower-case word ending in a t),
				 * the final 't' will be sucked up in the while
				 * loop.  So, if the match fails, we back up a
				 * notch and try to match the rest of the
				 * string again, repeating this process
				 * recursively until we get back to the
				 * beginning of the closure.  The recursion
				 * goes, at most two levels deep.
				 */

			if(pat = pat->next)
			{
				while(bocl <= lin)
				{
					if(rval = amatch(lin, pat, boln))
					{
							/* success */
						return(rval);
					} else
						--lin;
				}
				return (0);	/* match failed */
			}
		} else if (omatch(&lin, pat, boln))
		{
			pat = pat->next;
		} else {
			return (0);
		}
	}
		/* Note that omatch() advances lin to point at the next
		 * character to be matched.  Consequently, when we reach
		 * the end of the template, lin will be pointing at the
		 * character following the last character matched.  The
		 * exceptions are templates containing only a BOLN or EOLN
		 * token.  In these cases omatch doesn't advance.
		 *
		 * A philosophical point should be mentioned here.  Is $
		 * a position or a character? (i.e. does $ mean the EOL
		 * character itself or does it mean the character at the end
		 * of the line.)  I decided here to make it mean the former,
		 * in order to make the behavior of amatch() consistent.  If
		 * you give amatch the pattern ^$ (match all lines consisting
		 * only of an end of line) then, since something has to be
		 * returned, a pointer to the end of line character itself is
		 * returned.
		 */

	return ((char *)max(strstart , lin));
}
