/*
 * Copyright 1987 Brian Beattie Rights Reserved.
 *
 * Permission to copy and/or distribute granted under the
 * following conditions:
 *
 * 1). No charge may be made other than resonable charges
 *	for reproduction.
 *
 * 2). This notice must remain intact.
 *
 * 3). No further restrictions may be added.
 *
 */
#include <stdio.h>
#include "tools.h"
#include "ed.h"

char	*
catsub(from, to, sub, new, newend)
char	*from, *to, *sub, *new, *newend;
{
	char	*cp, *cp2;

	for(cp = new; *sub != EOS && cp < newend;)
	{
		if(*sub == DITTO)
			for(cp2 = from; cp2 < to;)
			{
				*cp++ = *cp2++;
				if(cp >= newend)
					break;
			}
		else
			*cp++ = *sub;
		sub++;
	}

	return(cp);
}
