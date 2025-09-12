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

getrhs(sub)
char	*sub;
{
	if(inptr[0] == NL || inptr[1] == NL)	/* check for eol */
		return( ERR );

	if(maksub(sub, MAXPAT) == NULL)
		return( ERR );
	
	inptr++;		/* skip over delimter */
	while(*inptr == SP || *inptr == HT)
			inptr++;
	if(toupper(*inptr) == 'G')
	{
		*inptr++;
		return( 1 );
	}
	return( 0 );
}
