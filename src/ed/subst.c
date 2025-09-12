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
subst(pat, sub, gflg, pflag)
TOKEN	*pat;
char	*sub;
int	gflg, pflag;
{
	int	lin, chngd, nchngd;
	char	*txtptr, *txt;
	char	*lastm, *m, *new, buf[MAXLINE];

	if(line1 <= 0)
		return( ERR );
	nchngd = 0;		/* reset count of lines changed */
	for(lin = line1; lin <= line2; lin++)
	{
		txt = txtptr = gettxt(lin);
		new = buf;
		chngd = 0;
		lastm = NULL;
		while(*txtptr)
		{
			if(gflg || !chngd)
				m = amatch(txtptr, pat, txt);
			else
				m = NULL;
			if(m != NULL && lastm != m)
			{
				chngd++;
				new = catsub(txtptr, m, sub, new,
						buf+MAXLINE);
				lastm = m;
			}
			if(m == NULL || m == txtptr)
			{
				*new++ = *txtptr++;
			} else {
				txtptr = m;
			}
		}
		if(chngd)
		{
			if(new >= buf+MAXLINE)
				return( ERR );
			*new++ = EOS;
			del(lin,lin);
			ins(buf);
			nchngd++;
			if(pflag)
				doprnt(curln, curln);
		}
	}
	if(nchngd == 0 && !gflg)
	{
		printf("string not found\n");
		return(ERR);
	}
	return( nchngd );
}
