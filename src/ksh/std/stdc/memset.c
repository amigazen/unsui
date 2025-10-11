/* $Id: memset.c,v 1.3 93/05/05 21:18:16 sjg Exp $ */

#include <string.h>

void *
memset(ap, c, n)
	void *ap;
	register int c;
	register size_t n;
{
	register char *p = ap;

	if (n++ > 0)
		while (--n > 0)
			*p++ = c;
	return ap;
}

