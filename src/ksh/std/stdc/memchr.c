/* $Id: memchr.c,v 1.3 93/05/05 21:18:09 sjg Exp $ */

#include <string.h>

void *
memchr(ap, c, n)
	const void *ap;
	register int c;
	register size_t n;
{
	register char *p = ap;

	if (n++ > 0)
		while (--n > 0)
			if (*p++ == c)
				return --p;
	return NULL;
}

