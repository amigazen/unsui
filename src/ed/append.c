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

append(line, glob)
int	line, glob;
{
	int	stat;
	char	lin[MAXLINE];

	if(glob)
		return(ERR);
	curln = line;
	while(1)
	{
		if(nflg)
			fprintf(stdout,"%6d. ",curln+1);

		if(fgets(lin, MAXLINE, stdin) == NULL)
			return( EOF );
		if(lin[0] == '.' && lin[1] == '\n')
			return(0);
		stat = ins(lin);
		if(stat < 0)
			return( ERR );
		
	}
}
