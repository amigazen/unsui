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

ckglob()
{
	static TOKEN	*glbpat;
	char	c, delim, *lin;
	int	num, nmatch;
	LINE	*ptr;

	c = toupper(*inptr);
	nmatch = 0;

	if(c == 'G' || c == 'V')
	{
		delim = *++inptr;
		if(delim <= ' ')
			return(0);
		if(inptr[0] != inptr[1])	/* if null string use last */
		{
			glbpat = optpat(glbpat);
		}
		if(*inptr == delim)
			inptr++;
		num = curln;
		while(1)
		{
			if(num)		/* do not do zero */
			{
				ptr = getptr(num);
				lin = gettxt(num);
				if(matchs(lin, glbpat, 0))
				{
					ptr->l_stat = (c == 'G' ? LGLOB:LEXCL);
					nmatch++;
				}
			}
			if((num = nextln(num)) == curln)
				break;
		}
	}
	return(nmatch);
}
