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
#include <setjmp.h>
jmp_buf	env;

LINE	line0;
int	curln = 0;
int	lastln = 0;
char	*inptr;
static char	inlin[MAXLINE];
int	nflg, lflg, pflg, pflag;
int	line1, line2, nlines;
extern char	fname[];
int	version = 1;

intr()
{
	printf("Interrupt:\n");
	longjmp(env, 1);
}

main(argc,argv)
int	argc;
char	**argv;
{
	int	stat, i, j, prmpt;

	setbuf();
	prmpt = isatty(0);		/* if interactive */
	if(argc > 1)
	{
		for(i = 1; i < argc; i++)
		{
			if(doread(0,argv[i]))
				curln = 1;
			strcpy(fname, argv[i]);
			break;
		}
	}
	while(1)
	{
		setjmp(env);
		signal(2, intr);

		if(nflg)
			fprintf(stdout,"%d: ",curln);
		else if(prmpt)
			fprintf(stdout,": ");

		if (fgets(inlin, sizeof(inlin),stdin) == NULL)
		{
			break;
		}
		if(*inlin == '!')
		{
			for(inptr = inlin; *inptr != NL; inptr++)
				;
			*inptr = EOS;
			system(inlin+1);
			continue;
		}
		inptr = inlin;
		if(getlst() >= 0)
			if(ckglob())
			{
				if((stat = doglob()) >= 0)
				{
					curln = stat;
					continue;
				}
			} else {
				if((stat = docmd(0)) >= 0)
				{
					if(stat == 1)
						doprnt(curln, curln);
					continue;
				}
			}
		if(stat == EOF)
		{
			exit(0);
		}
		if(stat == FATAL)
		{
			fputs("FATAL ERROR\n",stderr);
			exit(1);
		}
		fputs("\007",stderr);
	}
}
