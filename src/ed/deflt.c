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

deflt(def1, def2)
int	def1, def2;
{
	if(nlines == 0)
	{
		line1 = def1;
		line2 = def2;
	}
	if(line1 > line2 || line1 <= 0)
		return (ERR);
}
