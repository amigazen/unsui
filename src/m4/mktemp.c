#include <stdio.h>

/* mktemp(0 shamelessly swiped off our vax, didn't find */
/* a (c) anywhere on this piece of code..               */


char *
mktemp(as)
char *as;
{
	register char *s;
	register int tmp;
	register i;
	extern int mytmpnum;

loop:	tmp = mytmpnum%100000;
	s = as;
	while (*s++)
		;
	s--;
	while (*--s == 'X') {
		*s = (tmp%10) + '0';
		tmp /= 10;
	}
	s++;
	i = 'a';
	while (access(as, 0) != -1) {
		if (i=='z')
			{
			mytmpnum+=1;
			goto loop;
			}
		*s = i++;
	}
	mytmpnum+=1;
	return(as);
}
