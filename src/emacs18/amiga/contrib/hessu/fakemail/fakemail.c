/*
 * FAKEMAIL.C
 *
 * compile: cc
 *
 * Written by Tapio Heiskanen
 *
 * Copyright (c) 1992 Ferry Island Technologies
 * All rights reserved
 *
 * Created: 20-Jul-92
 */

#include "version.h"

#include <proto/dos.h>
#include <stdio.h>
#include <stdlib.h>

char *version="$VER: fakemail V"VERSION;

void main(int ac, char **arg)
	{
	char buf[256], hdr[256], name[256];
	FILE *fifo, *sign;
	int header=1;

	sprintf(name, "T:fakemail%d", (int)name);

	if(!(fifo=fopen(name, "w")))
		{
		printf("FAKEMAIL: Can't open temporary file %s\n", name);
		exit(10);
		}

/* pipe stdin to t:<name> */

	while(fgets(buf, 256, stdin))
		{
		if(*buf=='\n' && header)
			if(sign=fopen("uulib:.fakemailhdrs", "r"))
				{		
				while(fgets(hdr, 256, sign))
					fputs(hdr, fifo);
				fclose(sign);
				header=0;
				}
		fputs(buf, fifo);
		}


/* cat uulib:.signature to t:<name> */

	if(sign=fopen("uulib:.signature", "r"))
		{		
		while(fgets(buf, 256, sign))
			fputs(buf, fifo);
		fclose(sign);
		}
	fclose(fifo);

/* give the file for sendmail */

	sprintf(name, "sendmail -f $user <T:fakemail%d", (int)name);
	Execute(name, 0, 0);
	sprintf(name, "T:fakemail%d", (int)name);
	remove(name);
	}
