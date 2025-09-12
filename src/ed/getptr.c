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

LINE	*
getptr(num)
int	num;
{
	LINE	*ptr;
	int	j;

	ptr = &line0;
	j = 0;
	for(j = 0; j < num; j++)
		ptr = ptr->l_next;
	return(ptr);
}
