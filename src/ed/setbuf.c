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

void relink(LINE *a, LINE *x, LINE *y, LINE *b)
{
	x->l_prev = a;
	y->l_next = b;
}

void clrbuf(void)
{
	del(1, lastln);
}

int ed_setbuf(void)
{
	relink(&line0, &line0, &line0, &line0);
	curln = lastln = 0;
	return 0;
}
