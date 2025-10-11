#include <string.h>
/* $Id: strcat.c,v 1.3 93/05/05 21:18:31 sjg Exp $ */

/*
 * strcat - append string src to dst
 */
char *				/* dst */
strcat(dst, src)
char *dst;
const char *src;
{
	register char *dscan;
	register const char *sscan;

	for (dscan = dst; *dscan != '\0'; dscan++)
		continue;
	sscan = src;
	while ((*dscan++ = *sscan++) != '\0')
		continue;
	return(dst);
}
