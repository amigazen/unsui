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

TOKEN	*
optpat(oldpat)
TOKEN	*oldpat;
{
	char	delim, str[MAXPAT], *cp;

	delim = *inptr++;
	cp = str;
	while(*inptr != delim && *inptr != NL)
	{
		if(*inptr == ESCAPE)
			if(inptr[1] == '/' || inptr[1] == '?')
				inptr++;
		*cp++ = *inptr++;
	}

	*cp = EOS;
	if(*str == EOS)
		return(oldpat);
	if(oldpat)
		unmakepat(oldpat);
	return(getpat(str));
}
