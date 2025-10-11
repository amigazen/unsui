#include <string.h>
/* $Id: strcpy.c,v 1.3 93/05/05 21:18:36 sjg Exp $ */

/*
 * strcpy - copy string src to dst
 */
char *				/* dst */
strcpy(dst, src)
char *dst;
const char *src;
{
	register char *dscan;
	register const char *sscan;

	dscan = dst;
	sscan = src;
	while ((*dscan++ = *sscan++) != '\0')
		continue;
	return(dst);
}
