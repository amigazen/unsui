#include <stdio.h>

#define SIZE 2048

main(int argc, char **argv)
{
    char buf[SIZE];

    buf[SIZE - 1] = 0;
    while (fgets(buf, SIZE - 1, stdin))
    {
	if (buf[0] != '#')
	{
	    char *p = buf, *e;

	    while (*p && *p == ' ') p++;
	    e = p + strlen(p);
	    while (e > p && (e[-1] == ' ' || e[-1] == '\t' || e[-1] == '\n')) e--;
	    *e = '\0';
	    if (*p) puts(p);
	}
    }
}
