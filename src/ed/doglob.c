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

doglob()
{
	int	cursav, lin, stat, count;
	char	*cmd;
	LINE	*ptr;

	lin = 1;
	count = 0;

	cmd = inptr;
	while(1)
	{
		ptr = getptr(lin);
		if(ptr->l_stat & (LGLOB|LEXCL))
		{
			ptr->l_stat &= ~(LGLOB|LEXCL);
			cursav = curln = lin;
			inptr = cmd;
			if((stat = getlst()) < 0)
				return(stat);
			if((stat = docmd(1)) < 0)
				return(stat);
			count = 0;
		} else {
			count++;
			lin = nextln(lin);
		}
		if(count > lastln)
			return(0);
	}
}
