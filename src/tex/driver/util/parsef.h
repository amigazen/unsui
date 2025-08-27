#define ARG_MAXLINE	256	/* configuration file max. linelength	*/
#define ARG_STRING_SIZE	50	/* one single argument length		*/
#define MAX_KEY_LEN	14	/* max. keyword length			*/
#define MAX_FORMAT_LEN	50	/* max. format string length		*/

typedef void __stdargs (* string_parse_function)(struct string_parse *, char *line, ...) ;

struct string_parse {
	char	*paf_keyword;	/*	keyword to match	*/
	char	*paf_format;	/*	format string		*/
#if 0
	void (*paf_function)(char *keyword, char *line, long arg1, long arg2, long arg3);
#else
	string_parse_function paf_function;
#endif
	long	 paf_flags;	/*	flags see below		*/
	long	 paf_userdata;	/*	?			*/
};

#define PAF_NO_ARG	1
#define PAF_1_ARG	2
#define PAF_2_ARGS	3
#define PAF_3_ARGS	4
#define PAF_ARGS	7	/* function deals with arg(s)		*/
#define PAF_BOOL_ARG	8	/* You have to set PAF_1_ARG too	*/
#define PAF_NOKEY	16

extern int Parse_Linenr;	/* current line number in file		*/
extern int Parse_Control;

#define PAC_END		1	/* force an exit			*/
#define PAC_END_UNKNOWN 2	/* stop if unknown keyword		*/
#define PAC_WARNING	4	/* use Warning() instead of Message()	*/
#define PAC_END_SEARCH	8	/* only PAF_NOKEY: stop parsing this line */

/* Functions are called with :

   PAF_BOOL_ARG :
	(struct string_parse *actual,
	 char *line_of_file,
	 int 1 if on, 0 otherwise);

   PAF_NO_1_2_3_ARG(S) :
	(struct string_parse *actual,
	 char *line_of_file,
	 arg1, arg2, arg3);

   PAF_ARGS :
	#ifdef USE_PAFARGS
	(struct string_parse *actual,
	 char *line_of_file,
	 arg1, arg2, arg3);
	#else		keyword is still in line_of_file
	(struct string_parse *actual,
	 char *line_of_file);
	#endif

   PAF_NO_KEY :		kind of demon, I can imagine some stuff
	(struct string_parse *actual,
	 char *line_of_file);
*/

/*#define USE_PAFARGS*/

/* #Define USE_PAFARGS if in the PAF_ARGS case you *want* your
functions to have the format string scanned and be called with
_no_more_ than 3 (three) parameters. Otherwise, the paf_format format
string is free to be used by your functions, and your functions are
called with no other parameter. (see below) */

#if 0
#define PARSE_INTS	1	/*	returns array of ints	*/
#define PARSE_PSTRING	2	/*	returns printer string	*/
#endif

/* normally, parse_line() returns after a matching keyword has been found,
this can be disabled in a PAF_NO_KEY function case if the called function
sets the var below. This can be used to accelerate reading or to disable
options. */

extern int keep_parse_line;
