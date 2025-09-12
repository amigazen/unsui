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

TOKEN	*srchpat;
char	lstsrch;
getnum()
{
	int	num;
	char	c;

	while(*inptr == SP || *inptr == HT)
		inptr++;

	if(*inptr >= '0' && *inptr <= '9')	/* line number */
	{
		for(num = 0; *inptr >= '0' && *inptr <= '9';)
		{
			num = (num * 10) + *inptr - '0';
			inptr++;
		}
		return ( num > lastln ? ERR : num );
	}

	switch(c = *inptr)
	{
	case '.':
		inptr++;
		return (curln);

	case '$':
		inptr++;
		return (lastln);

	case '/':
	case '?':
		lstsrch = c;
		srchpat = optpat(srchpat);
		if(*inptr == c)
			*inptr++;
		return(find(srchpat,c == '/'?1:0));

	case '-':
	case '+':
		return(curln);

	default:
		return ( EOF );		/* unknown address */
	}
}
