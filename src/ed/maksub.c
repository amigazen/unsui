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
maksub(sub, subsz)
char	*sub;
int	subsz;
{
	int	size;
	char	delim, *cp;

	size = 0;
	cp = sub;

	delim = *inptr++;
	for(size = 0; *inptr != delim && *inptr != NL && size < subsz; size++)
	{
		if(*inptr == '&')
		{
			*cp++ = DITTO;
			inptr++;
		} else {
			if(*inptr == ESCAPE)
			{
				inptr++;
				if(*inptr < '0' || *inptr > '7')
					switch(toupper(*++inptr))
					{
					case NL:
						*cp++ == ESCAPE;
						break;
					case 'S':
						*cp++ = SP;
						inptr++;
						break;
					case 'N':
						*cp++ = NL;
						inptr++;
						break;
					case 'T':
						*cp++ = HT;
						inptr++;
						break;
					case 'B':
						*cp++ = BS;
						inptr++;
						break;
					case 'R':
						*cp++ = CR;
						inptr++;
						break;
					default:
						*cp++ = *inptr;
						inptr++;
						break;
					}
				else {
					*cp = 0;
					while(*inptr >= '0' && *inptr <= '7')
					{
						*cp = (*cp << 3)+(*inptr-'0');
						inptr++;
					}
					cp++;
				}
			} else
				*cp++ = *inptr++;
		}
	}
	if(size >= subsz)
		return( NULL );

	*cp = EOS;
	return( sub );
}
