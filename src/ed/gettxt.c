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
gettxt(num)
int	num;
{
	LINE	*lin;
	static char	txtbuf[MAXLINE];

	lin = getptr(num);
	strcpy(txtbuf,lin->l_buff);
	strcat(txtbuf,"\n");
	return(txtbuf);
}
