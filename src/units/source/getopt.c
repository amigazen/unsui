/* optarg - parse command-line arguments */
/* Author: AT&T */

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#define ERR(s, c)				if(opterr){\
	char errbuf[2];\
	errbuf[0] = c; errbuf[1] = '\n';\
	fwrite(argv[0], 1, (unsigned)strlen(argv[0]),stderr);\
	fwrite(s, 1, (unsigned)strlen(s),stderr);\
	fwrite(errbuf, 1, 2, stderr);}

extern int opterr;

/* prototypes as desired by SAS/C 6.51 for Amiga (Ron Charlton) */
#include "getopt_protos.h"
#include "units_protos.h"

int	opterr = 1;		/* getopt prints errors if this is on */
int	optind = 1;		/* token pointer */
int	optopt;			/* option character passed back to user */
char	*optarg;		/* flag argument (or value) */

int	/* return option character, EOF if no more or ? if problem */
getopt(argc, argv, opts)
int	argc;
char	**argv;
char	*opts;				/* option string */
{
	static int sp = 1;		/* character index in current token */
	register char *cp;		/* pointer into current token */

	if(sp == 1)
		/* check for more flag-like tokens */
		if(optind >= argc ||
		   argv[optind][0] != '-' || argv[optind][1] == '\0')
			return(EOF);
		else if(strcmp(argv[optind], "--") == 0) {
			optind++;
			return(EOF);
		}
	optopt = argv[optind][sp];
	if(optopt == ':' || (cp=strchr(opts, optopt)) == 0) {
		ERR(": illegal option -- ", optopt);
		/* if no chars left in this token, move to next token */
		if(argv[optind][++sp] == '\0') {
			optind++;
			sp = 1;
		}
		return('?');
	}

	if(*++cp == ':') {	/* if a value is expected, get it */
		if(argv[optind][sp+1] != '\0')
			/* flag value is rest of current token */
			optarg = &argv[optind++][sp+1];
		else if(++optind >= argc) {
			ERR(": option requires an argument -- ", optopt);
			sp = 1;
			return('?');
		} else
			/* flag value is next token */
			optarg = argv[optind++];
		sp = 1;
	} else {
		/* set up to look at next char in token, next time */
		if(argv[optind][++sp] == '\0') {
			/* no more in current token, so setup next token */
			sp = 1;
			optind++;
		}
		optarg = 0;
	}
	return(optopt);/* return the current flag character found */
}
