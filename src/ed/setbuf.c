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

relink(a, x, y, b)
LINE	*a, *x, *y, *b;
{
	x->l_prev = a;
	y->l_next = b;
}

clrbuf()
{
	del(1, lastln);
}

setbuf()
{
	relink(&line0, &line0, &line0, &line0);
	curln = lastln = 0;
}
