/*  getopt.h - Get next option letter from argument vector.
               v1.1  12-Dec-1987  aklevin
*/

#define GETOPT_H

#ifndef _STDIO_H
#include <stdio.h>
#endif

/*  optarg points to an option's argument (if any).
    optind holds the index of the next argument vector element to parse.
     Once all options have been parsed, points to the first non-option argument.
	 [If (optind > argc) then there are no more arguments].
    opterr, if set to 0 will suppress getopt's error messages (default is 1).
    optopt, while not usually documented, is used here to return the actual
     option character found, even when getopt itself returns '?'.
*/
char *optarg;
int optind=1, opterr=1, optopt;

int
getopt(argc, argv, optstring)
int argc;
char *argv[], *optstring;
{

int any_more, i, result;
static int opthold, optsub=1;

/*  Reset optarg upon entry  */
*optarg = '\0';

/*  Reset optsub if caller has changed optind.  */
if (optind != opthold) optsub = 1;

/*  Look at each element of the argument vector still unparsed.  */
for ( ; optind < argc; optind++) {
	/*  Done if a non-option argument or single dash is reached.
		However, don't skip over said argument.  */
	if (argv[optind][0] != '-' || argv[optind][1] == '\0') break;

	/*  Got an option.  */

	/*  Done if "--" is reached.  Skip over it, too.  */
	if (argv[optind][1] == '-') {
		optind++;
		break;
	}

	/*  Look at each character in optstring.  */
	for (i=0; i < strlen(optstring); i++) {
		if ( (optopt = argv[optind][optsub]) != optstring[i]) continue;

		/*  Got a match.  */

		/*  Are there any more chars in this option?  e.g. `-abc'  */
		any_more = strlen(argv[optind])-optsub-1;

		/*  Does this option require an argument?  */
		if (optstring[i+1] == ':') {

			/*  Yes.  If this is the last argument, complain.  */
			if (optind == argc-1 && !any_more) {
				if (opterr) fprintf(stderr, "%s: `-%c' option requires an argument.\n", argv[0], optopt);
				optind++;
				result='?';
				goto leave;
			} /* end if (opt */

			/*  Qualifier is either rest of this argument (if any)
			    or next argument.  */
			else {
				if (!any_more) optarg = argv[++optind];
				else optarg = &argv[optind][optsub+1];
				optind++;
				optsub=1;
			} /* end else */
		} /* end if (opt */
		else {
			/*  No argument; just adjust indices.  */
			/*  Advance to next argument.  */
			if (!any_more) {
				optind++;
				optsub=1;
			} /* end if (! */
			/*  Advance to next character.  */
			else optsub++;
		} /* end else */
		result=optopt;
		goto leave;
	} /* end for (i=0 */
if (opterr) fprintf(stderr, "%s: Unrecognized option `-%c'.\n", argv[0], optopt);
if (strlen(argv[optind])-optsub-1) optsub++;
else {
	optind++;
	optsub=1;
}
result='?';
goto leave;
} /* end for ( ; */
result=EOF;
leave:
	opthold = optind;
	return(result);
} /* end getopt() */

