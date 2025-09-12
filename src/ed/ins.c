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
 
ins(str)
char	*str;
{
	char	buf[MAXLINE], *cp;
	LINE	*new, *cur, *nxt;

	cp = buf;
	while(1)
	{
		if((*cp = *str++) == NL)
			*cp = EOS;
		if(*cp)
		{
			cp++;
			continue;
		}
		if((new = (LINE *)malloc(sizeof(LINE)+strlen(buf))) == NULL)
			return( ERR ); 	/* no memory */

		strcpy(new->l_buff,buf);	/* build new line */
		cur = getptr(curln);		/* get current line */
		nxt = getptr(nextln(curln));	/* get next line */
		relink(cur, new, new, nxt);	/* add to linked list */
		relink(new, nxt, cur, new);
		lastln++;
		curln++;

		if(*str == EOS)		/* end of line ? */
			return( 1 );

		cp = buf;
	}
}
