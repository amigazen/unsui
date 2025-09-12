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

getone()
{
	int	c, i, num;

	if((num = getnum()) >= 0)
	{
		while(1)
		{
			while(*inptr == SP || *inptr == HT)
				inptr++;

			if(*inptr != '+'&& *inptr != '-')
				break;
                        c = *inptr++;

			if((i = getnum()) < 0)
				return ( i );
			if(c == '+')
			{
				num += i;
			} else {
				num -= i;
			}
		}
	}
	return ( num );
}
