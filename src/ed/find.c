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

find(pat, dir)
TOKEN	*pat;
int	dir;
{
	int	num;
	char	*lin;

	for(num = curln;(num = (dir ? nextln(num) : prevln(num))) != curln;)
	{
		lin = gettxt(num);
		if(matchs(lin, pat, 0))
		{
			return(num);
		}
	}
	return ( ERR );
}
