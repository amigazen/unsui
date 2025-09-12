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

move(num)
int	num;
{
	LINE	*k0, *k1, *k2, *k3;

	if(line1 <= 0 || line1 <= num && num <= line2)
		return( ERR );
	k0 = getptr(prevln(line1));
	k1 = getptr(line1);
	k2 = getptr(line2);
	k3 = getptr(nextln(line2));

	relink(k0, k3, k0, k3);

	if(num > line1)
	{
		curln = num;
		num += line2-line1+1;
	} else
		curln = num + (line2 - line1 + 1);

	k0 = getptr(num);
	k3 = getptr(nextln(num));

	relink(k0, k1, k2, k3);
	relink(k2, k3, k0, k1);

	return( 1 );
}
