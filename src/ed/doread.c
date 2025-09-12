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

doread(lin, fname)
int	lin;
char	*fname;
{
	extern FILE	*fopen();
	FILE	*fp;
	int	err;
	long	bytes;
	int	lines;
	static char	str[MAXLINE];

	err = 0;
	nonascii = nullchar = truncated = 0;

	printf("\"%s\" ",fname);
	if((fp = fopen(fname, "r")) == NULL)
	{
		printf("file open err\n");
		return( ERR );
	}
	curln = lin;
	for(lines = 0, bytes = 0;(err = egets(str,MAXLINE,fp)) > 0;)
	{
		bytes += strlen(str);
		if(ins(str) < 0)
		{
			printf("file insert error\n");
			err++;
			break;
		}
		lines++;
	}
	fclose(fp);
	if(err < 0)
		return(err);
	printf("%d lines %d bytes",lines,bytes);
	if(nonascii)
		printf(" [%d non-ascii]",nonascii);
	if(nullchar)
		printf(" [%d nul]",nullchar);
	if(truncated)
		printf(" [%d lines truncated]",truncated);
	printf("\n");
	return( err );
}
