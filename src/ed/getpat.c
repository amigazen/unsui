#include <stdio.h>
#include "tools.h"

/* Translate arg into a TOKEN string */
TOKEN *getpat(char *arg)
{
	
	return (makepat(arg, '\000'));
}
