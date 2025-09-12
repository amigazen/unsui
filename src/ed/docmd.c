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

char	fname[MAXFNAME];
int	fchanged;

docmd(glob)
int	glob;
{
	static char	rhs[MAXPAT];
	static TOKEN	*subpat;
	int	c, err, line3;
	int	i, apflg, pflag, gflag;
	int	nchng;
	char	*fptr;

	pflag = FALSE;
	while(*inptr == SP && *inptr == HT)
		inptr++;

	c = *inptr++;

	switch(c)
	{
	case NL:
		if(nlines == 0)
		{
			line2 = nextln(curln);
		}
		curln = line2;
		return (1);
		break;

	case '=':
		printf("%6d=\n",line2);
		break;

	case 'A':	
	case 'a':
		if(*inptr != NL || nlines > 1)
			return(ERR);

		if(append(line1, glob) < 0)
			return(ERR);;
		fchanged = TRUE;
		break;

	case 'C':
	case 'c':
		if(*inptr != NL)
			return(ERR);

		if(deflt(curln, curln) < 0)
			return(ERR);

		if(del(line1, line2) < 0)
			return(ERR);
		if(append(curln, glob) < 0)
			return(ERR);
		fchanged = TRUE;
		break;

	case 'D':
	case 'd':
		if(*inptr != NL)
			return(ERR);

		if(deflt(curln, curln) < 0)
			return(ERR);

		if(del(line1, line2) < 0)
			return(ERR);
		if(nextln(curln) != 0)
			curln = nextln(curln);
		fchanged = TRUE;
		break;

	case 'E':
	case 'e':
		if(nlines > 0)
			return(ERR);
		if(fchanged && *inptr != '!')
		{
			printf("File not saved\n");
			return(ERR);
		}
		if(*inptr == '!')
			inptr++;

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		if((fptr = getfn()) == NULL)
			return(ERR);

		clrbuf();
		if((err = doread(0, fptr)) < 0)
			return(err);

		strcpy(fname, fptr);
		fchanged = FALSE;
		break;

	case 'I':
	case 'i':
		if(*inptr != NL || nlines > 1)
			return(ERR);

		if(append(prevln(line1), glob) < 0)
			return(ERR);
		fchanged = TRUE;
		break;

	case 'M':
	case 'm':
		if((line3 = getone()) < 0)
			return(ERR);
		if(deflt(curln,curln) < 0)
			return(ERR);
		if(move(line3) < 0)
			return(ERR);
		fchanged = TRUE;
		break;

	case 'P':
	case 'p':
		if(*inptr != NL)
			return(ERR);
		if(deflt(curln,curln) < 0)
			return(ERR);
		if(doprnt(line1,line2) < 0)
			return(ERR);
		break;

	case 'Q':
	case 'q':
		if(fchanged && *inptr != '!')
		{
			printf("File not saved\n");
			return(ERR);
		}
		if(*inptr == '!')
			inptr++;
		if(*inptr == NL && nlines == 0 && !glob)
			return(EOF);
		else
			return(ERR);

	case 'R':
	case 'r':
		if(nlines > 1)
			return(ERR);

		if(nlines = 0)
			line2 = lastln;

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		if((fptr = getfn()) == NULL)
			return(ERR);

		if((err = doread(line2, fptr)) < 0)
			return(err);
		fchanged = TRUE;
		break;

	case 'S':
	case 's':
		if(toupper(*inptr) == 'E')
			return(set());
		while(*inptr == SP || *inptr == HT)
			inptr++;
		if((subpat = optpat(subpat)) == NULL)
			return(ERR);
		if((gflag = getrhs(rhs)) < 0)
			return(ERR);
		if(toupper(*inptr) == 'P')
			pflag++;
		if(deflt(curln, curln) < 0)
			return(ERR);
		if((nchng = subst(subpat, rhs, gflag, pflag)) < 0)
			return(ERR);
		if(nchng)
			fchanged = TRUE;
		return (1);

	case 'W':
	case 'w':
		apflg = 0;
		if(inptr[0] == '>' && inptr[1] == '>')
		{
			apflg++;
			inptr++;
			inptr++;
		}

		if(*inptr != ' ' && *inptr != HT && *inptr != NL)
			return(ERR);

		if((fptr = getfn()) == NULL)
			return(ERR);

		if(deflt(1, lastln) < 0)
			return(ERR);
		if(dowrite(line1, line2, fptr, apflg) < 0)
			return(ERR);
		fchanged = FALSE;
		break;

	case 'X':
	case 'x':
		if(*inptr == NL && nlines == 0 && !glob)
		{
			if((fptr = getfn()) == NULL)
				return(ERR);
			if(dowrite(1, lastln, fptr, 0) >= 0)
				return(EOF);
		}
		return(ERR);

	case 'Z':
	case 'z':
		if(deflt(curln,curln) < 0)
			return(ERR);

		switch(*inptr)
		{
		case '-':
			if(doprnt(line1-21,line1) < 0)
				return(ERR);
			break;

		case '.':
			if(doprnt(line1-11,line1+10) < 0)
				return(ERR);
			break;

		case '+':
		case '\n':
			if(doprnt(line1,line1+21) < 0)
				return(ERR);
			break;
		}
		break;

	default:
		return(ERR);
	}
	return (0);
}
