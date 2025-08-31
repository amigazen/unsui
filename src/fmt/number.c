/* Copyright (c) 1982, 1985 Gary Perlman */
/* Copies can be made if not for material gain */

/*
	number: report if a string is a UNIX formatted number

	notes:
		a number in UNIX is one that can be converted from
		a string to an integer or real with no loss of information
			due to bad format
		all numbers can be surrounded by whitespace
		an integer has an optional minus sign, followed by digits
		a real number has an optional minus sign followed by digits
		if a string has a decimal point, followed by zeros, it is real, not int

	value:
		1 is string is an integer [-] 0-9+
		2 is string is a real number (as seen by atof)
		0 for non-numbers

	compilation flags:
		-DSTANDALONE	includes test main program
		$Compile: cc -DSTANDALONE -O -o %F %f
	
	deficiencies:
		does not check to see if significant digits will be ignored
	
	author:
		Gary Perlman

	date:
		Wed May 22 13:30:40 EDT 1985
		Sun Sep  1 14:53:51 EDT 1985 (modified test module)
		
*/
#include <ctype.h>

#ifndef lint
static char sccsfid[] = "@(#) number.c 5.2 (unix|stat) 9/1/85";
#endif

#define	IS_NOT      0            /* not a number */
#define	IS_INT      1            /* an integer */
#define	IS_REAL     2            /* a real number */

#define	EOS         '\0'

/*LINTLIBRARY*/

number (string)
char	*string;                 /* the string to be tested */
	{
	int 	answer = IS_INT;     /* start by assuming it is an integer */
	int 	before = 0;          /* anything before the decimal? */
	int 	after = 0;           /* anything after the decimal? */
	while (isspace (*string))    /* skip over blank space */
		string++;
	if (*string == EOS)          /* empty string not allowed */
		return (IS_NOT);
	if (*string == '+' || *string == '-') /* old atoi didn't allow '+' */
		{
		string++;
		if (!isdigit (*string) && *string != '.')
			return (IS_NOT);
		}
	if (isdigit (*string))       /* note that there was a digit before . */
		{
		before = 1;
		while (isdigit (*string))
			string++;
		}
	if (*string == '.')          /* found a decimal point, parse for real */
		{
		answer = IS_REAL;
		string++;
		if (isdigit (*string))   /* note that there was a digit after . */
			{
			after = 1;
			while (isdigit (*string))
				string++;
			}
		}
	if (!before && !after)       /* must be digit somewhere */
		return (IS_NOT);
	if (*string == 'E' || *string == 'e') /* exponent */
		{
		answer = IS_REAL;
		string++;
		if (*string == '+' || *string == '-') /* optional sign */
			string++;
		if (!isdigit (*string))  /* missing exponent */
			return (IS_NOT);
		while (isdigit (*string))
			string++;
		}
	while (isspace (*string))    /* skip optional spaces */
		string++;
	/* should now have exhausted the input string */
	return (*string == EOS ? answer : IS_NOT);
	}

#ifdef STANDALONE

#include <stdio.h>
/*
	exits with status = the number of args not numerical
	Shell Example:
		if number -i $*
		then
			echo processing $*
		else
			echo $0: arguments must be integers 
		fi
	Options:
		-i  arguments must be integer
		-n  arguments must be non-negative
*/
int 	NoNegative;   /* do the values have to be non-negative? */
int 	Integer;      /* do the values have to be integers? */

static
int
initial (argc, argv) char **argv;
	{
	extern	char	*optarg;
	extern	int 	optind;
	int 	errflg = 0;
	int 	C;
	char	*optstring = "in";
	char	*usage = "[-in] string ...";
	while ((C = getopt (argc, argv, optstring)) != EOF)
		switch (C)
			{
			case 'i': Integer = 1; break;
			case 'n': NoNegative = 1; break;
			default: errflg++; break;
			}
	if (errflg)
		{
		fprintf (stderr, "Usage: %s %s\n", argv[0], usage);
		exit (1);
		}
	return (optind);
	}

main (argc, argv) char **argv;
	{
	int 	status = 0;
	int 	arg = initial (argc, argv);
	char	*string;
	while (arg < argc)
		{
		string = argv[arg++];
		if (NoNegative && *string == '-') status++;
		else switch (number (string))
			{
			case IS_NOT:  status++; break;
			case IS_REAL: if (Integer) status++; break;
			case IS_INT:  break;
			default: /* CAN'T HAPPEN */ break;
			}
		}
	exit (status);
	}

#endif
