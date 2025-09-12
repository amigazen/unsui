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

del(from, to)
int	from, to;
{
	LINE	*first, *last, *next, *tmp;

	if(from < 1)
		from = 1;
	first = getptr(prevln(from));
	last = getptr(nextln(to));
	next = first->l_next;
	while(next != last && next != &line0)
	{
		tmp = next->l_next;
		free(next);
		next = tmp;
	}
	relink(first, last, first, last);
	lastln -= (to - from)+1;
	curln = prevln(from);
}
