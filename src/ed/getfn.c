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

extern char	fname[MAXFNAME];
char	*
getfn()
{
	static char	file[256];
	char	*cp;

	if(*inptr == NL)
	{
		strcpy(file, fname);
	} else {
		while(*inptr == SP || *inptr == HT)
			inptr++;

		cp = file;
		while(*inptr && *inptr != NL && *inptr != SP && *inptr != HT)
		{
			*cp++ = *inptr++;
		}
		*cp = '\0';

		if(strlen(file) == 0)
		{
			printf("bad file name\n");
			return( NULL );
		}
	}

	if(strlen(file) == 0)
	{
		printf("no file name\n");
		return(NULL);
	}
	return( file );
}
