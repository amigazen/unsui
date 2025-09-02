/*
 * MOVEMAIL.C
 *
 * compile: cc
 *
 * Written by Tapio Heiskanen
 *
 * Copyright (c) 1992 Ferry Island Technologies
 * All rights reserved
 *
 * Created: 22-Jul-92
 */

#include "version.h"

#include <proto/dos.h>
#include <stdio.h>
#include <stdlib.h>

char *version="$VER: movemail V"VERSION;

void main(int ac, char **arg)
	{
	char buf[256];
	FILE *tofile, *spoolfile;

	if(!(spoolfile=fopen(arg[1], "r")))
		{
		printf("movemail: Can't open spool file %s\n", arg[1]);
		exit(10);
		}

	if(!(tofile=fopen(arg[2], "a")))
		{
		printf("movemail: Can't open to file %s\n", arg[2]);
		exit(10);
		}

	while(fgets(buf, 256, spoolfile))
		fputs(buf, tofile);

	fcloseall();
	remove(arg[1]);
	}
