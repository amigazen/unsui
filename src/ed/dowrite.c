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

dowrite(from, to, fname, apflg)
int	from, to;
char	*fname;
int	apflg;
{
	extern FILE	*fopen();
	FILE	*fp;
	int	lin, err;
	int	lines, bytes;
	char	*str;

	err = 0;

	lines = bytes = 0;
	printf("\"%s\" ",fname);
	if((fp = fopen(fname,(apflg?"a":"w"))) == NULL)
	{
		printf("file open error\n");
		return( ERR );
	}
	for(lin = from; lin <= to; lin++)
	{
		str = gettxt(lin);
		lines++;
		bytes += strlen(str);
		if(fputs(str, fp) == EOF)
		{
			printf("file write error\n");
			err++;
			break;
		}
	}
	printf("%d lines %d bytes\n",lines,bytes);
	fclose(fp);
	return( err );
}
