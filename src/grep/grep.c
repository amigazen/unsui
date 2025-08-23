/*
 * grep - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on bgrep by Arnold Robbins and Roy Mongiovi, and Henry Spencer's regex.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <exec/types.h>
#include <dos/dos.h>
#include <exec/memory.h>
#include <clib/dos_protos.h>
#include <clib/exec_protos.h>
#include "regex.h"

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/rdargs.h>

/* Common utilities and getopt */
#include "/common/common.h"
#include "/common/getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: grep 2.1 (22/08/25)\n";

#define TRUE	1
#define FALSE	0

#define MAXPATS	60		/* Maximum number of patterns (reduced for NEAR) */
#define MAXLINE	(128 + 1)       /* Reduced for NEAR memory constraints */

/* For ReadArgs template */
enum {
    ARG_PATTERN,
    ARG_FILE,
    ARG_COUNT_LINES,
    ARG_LIST_NAMES,
    ARG_NUMBER_LINES,
    ARG_IGNORE_CASE,
    ARG_INVERT_MATCH,
    ARG_EXACT_MATCH,
    ARG_SILENT,
    ARG_EXTENDED_REGEX,
    ARG_QUIET,
    ARG_ALWAYS_FILENAME,
    ARG_NEVER_FILENAME,
    ARG_LIST_NO_MATCH,
    ARG_MAX_MATCHES,
    ARG_POSIX,
    ARG_COUNT
};

/* Command line options */
int Allbut = FALSE;		/* Print lines that don't match pattern */
int Exact = FALSE;		/* Only print lines that match exactly */
int Countlines = FALSE;		/* Only print a count of matching lines */
int Listnames = FALSE;		/* Only list file names that match */
int Numberlines = FALSE;	/* Print relative line number */
int Silent = FALSE;		/* -s: suppress error messages about files */
int Monocase = FALSE;		/* -i: ignore case distinctions */
int Extended = FALSE;		/* -E: use extended regex patterns */
int Quiet = FALSE;		/* -q: suppress all normal output, exit on first match */
int Always_filename = FALSE;	/* -H: always show filenames */
int Never_filename = FALSE;	/* -h: never show filenames */
int List_no_match = FALSE;	/* -L: list files that don't match */
int Max_matches = 0;		/* -m: maximum number of matches */
int Match_count = 0;		/* Current match count */

/* Variables */
long Curline = 0;		/* Current file input line */
long Lines_matched = 0;		/* How many lines matched the pattern */
int Lotsafiles = FALSE;		/* Are there more than one file? */
int Pat_length[MAXPATS];	/* Length of pattern */
int Line_length = 0;		/* Length of line */
int Couldnt_open_files = FALSE;	/* One or more files could not be opened */
int Exit_val = 0;		/* Return code status */
int Curpat = 0;			/* Current pattern comparing against */
int Numpats = 0;		/* Total number of patterns */
int Pattern_type[MAXPATS];	/* 0=simple, 1=basic_regex, 2=complex_regex */

char Inbuf[MAXLINE];		/* Input buffer */
char Pattern[MAXPATS][MAXLINE];	/* Pattern to be matched */
char *Program = NULL;		/* Program name */

int Argc;			/* Make argc and argv global */
char **Argv;

/* Boyer-Moore tables for simple patterns */
int D1[MAXPATS][64];
int D2[MAXPATS][64];  
int F[MAXPATS][64];  

/* POSIX regex for complex patterns */
regex_t regex_expressions[MAXPATS];

/* Function declarations for forward references */
int is_simple_pattern();
int is_basic_regex();
int enhanced_match();
int boyer_moore_match();
int basic_regex_match();
int wildcard_match();
int complex_regex_match();
void initialize_boyer_moore();
void setpats();
void patfromfile();
void process();
void parse_getopt_args();
void parse_readargs();
void check_argument_conflicts();
void mapdown();
void usage();
char *my_index();
int run_grep_logic(int file_count, char **files);

/* External function declarations for system functions */
extern int regcomp(regex_t *preg, const char *pattern, int cflags);
extern int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t *pmatch, int eflags);
extern void regfree(regex_t *preg);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
	int i;
	char **files = NULL;
	int file_count = 0;
	
	/* ReadArgs Path Variables */
	const char *template = "PATTERN/K,FILE/M,COUNTLINES/S,LISTNAMES/S,NUMBERLINES/S,IGNORECASE/S,INVERTMATCH/S,EXACTMATCH/S,SILENT/S,REGEX/S,QUIET/S,ALWAYSFILENAME/S,NEVERFILENAME/S,LISTNOMATCH/S,MAXMATCHES/K/N,POSIX/K/F";
	LONG arg_array[ARG_COUNT] = {0};
	struct RDArgs *rdargs = NULL;
	char *cmd_string = NULL;
	int ret_code = 0;
	BOOL interactive_help = FALSE;
	
	/* POSIX/F Path Variables */
	char *posix_str;
	int new_argc;
	char *new_argv[MAX_TEMPLATE_ITEMS];
	char initial_args_str[256];
	char user_input_buf[256];
	char *temp_str;
	size_t combined_len;

	if (argc < 1) {
		exit(1);
	}
	
	Program = my_basename(argv[0]);
	Argc = argc;
	Argv = argv;

	if (argc == 1) {
		/* No arguments, show usage */
		usage();
	}

	/* --- Logic to decide which parser to use --- */
	if (is_getopt_style(argc, argv)) {
		/* --- GETOPTS PATH --- */
		parse_getopt_args(argc, argv);
		
		/* Handle pattern from remaining arguments */
		if (Pattern[0][0] == '\0')	/* Not from -f or -e */
		{
			if (Argv[0] == NULL)	/* No string given */
				usage();
			setpats(Argv[0]);
			Argc--;
			Argv++;
		}
		
		/* Get files from remaining arguments */
		if (Argc > 0) {
			files = Argv;
			file_count = Argc;
		}
		ret_code = run_grep_logic(file_count, files);
		
	} else {
		/* --- READARGS PATH --- */
		for (i = 1; i < argc; i++) {
			if (strcmp(argv[i], "?") == 0) {
				interactive_help = TRUE;
				break;
			}
		}

		rdargs = AllocDosObject(DOS_RDARGS, NULL);
		if (!rdargs) {
			fprintf(stderr, "%s: out of memory for RDArgs\n", Program);
			return 1;
		}

		if (interactive_help) {
			/* Initialize buffers */
			initial_args_str[0] = '\0';
			user_input_buf[0] = '\0';

			/* Build a string from any args that are NOT '?' */
			temp_str = build_command_string(argc, argv, "?");
			if (temp_str) {
				strncpy(initial_args_str, temp_str, 255);
				free(temp_str);
			}

			/* Print template and prompt for more input */
			printf("%s: ", template);
			fflush(stdout);
			if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
				/* Combine initial args with the new user input */
				combined_len = strlen(initial_args_str) + strlen(user_input_buf) + 2;
				cmd_string = malloc(combined_len);
				if (cmd_string) {
					strcpy(cmd_string, initial_args_str);
					if (initial_args_str[0] != '\0' && user_input_buf[0] != '\n') {
						strcat(cmd_string, " ");
					}
					strcat(cmd_string, user_input_buf);
				}
			} else {
				cmd_string = strdup(initial_args_str);
				if (cmd_string) strcat(cmd_string, "\n");
			}
		} else {
			/* Standard case: build command string from all args */
			cmd_string = build_command_string(argc, argv, NULL);
		}

		if (!cmd_string) {
			fprintf(stderr, "%s: out of memory for command string\n", Program);
			FreeDosObject(DOS_RDARGS, rdargs);
			return 1;
		}

		/* Set up ReadArgs to parse from our string */
		rdargs->RDA_Source.CS_Buffer = cmd_string;
		rdargs->RDA_Source.CS_Length = strlen(cmd_string);
		rdargs->RDA_Source.CS_CurChr = 0;
		rdargs->RDA_Flags |= RDAF_NOPROMPT;

		if (!ReadArgs(template, arg_array, rdargs)) {
			PrintFault(IoErr(), Program);
			ret_code = 1;
		} else {
			/* Check for POSIX/F override first */
			if (arg_array[ARG_POSIX]) {
				posix_str = (char *)arg_array[ARG_POSIX];

				/* Tokenize the string and build a new argv for getopt */
				new_argv[0] = Program;
				new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

				parse_getopt_args(new_argc, new_argv);
				/* Get files from remaining arguments */
				if (Argc > 0) {
					files = Argv;
					file_count = Argc;
				}
				ret_code = run_grep_logic(file_count, files);

			} else {
				/* Standard ReadArgs processing */
				parse_readargs(arg_array);
				
				/* Get pattern from ReadArgs */
				if (arg_array[ARG_PATTERN]) {
					setpats((char *)arg_array[ARG_PATTERN]);
				} else {
					/* No pattern specified */
					usage();
				}
				
				/* Get files from ReadArgs */
				if (arg_array[ARG_FILE]) {
					/* Count files and allocate array */
					file_count = 0;
					while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
						file_count++;
					}
					
					files = (char **)arg_array[ARG_FILE];
					ret_code = run_grep_logic(file_count, files);
				} else {
					/* No files specified, use stdin */
					files = NULL;
					file_count = 0;
					ret_code = run_grep_logic(file_count, files);
				}
			}
		}

		/* Clean up */
		FreeDosObject(DOS_RDARGS, rdargs);
		free(cmd_string);
	}

	return ret_code;
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 */
void parse_getopt_args(argc, argv)
int argc;
char **argv;
{
	int c;
	
	reset_getopt();
	
	while ((c = getopt(argc, argv, "cEef:hHiIlLm:nqsvxyV")) != -1) {
		switch (c) {
		case 'c':
			Countlines = TRUE;
			break;

		case 'E':
			Extended = TRUE;
			break;

		case 'e':
			strcpy(Pattern[0], optarg);
			Pattern[0][sizeof Pattern[0] - 1] = '\0';
			break;

		case 'f':
			patfromfile(optarg);
			break;

		case 'h':
			Never_filename = TRUE;
			break;

		case 'H':
			Always_filename = TRUE;
			break;

		case 'i':
		case 'y':
			Monocase = TRUE;
			break;

		case 'l':
			Listnames = TRUE;
			break;

		case 'L':
			List_no_match = TRUE;
			break;

		case 'm':
			Max_matches = atoi(optarg);
			if (Max_matches <= 0) {
				fprintf(stderr, "%s: invalid max count: %s\n", Program, optarg);
				usage();
			}
			break;

		case 'n':
			Numberlines = TRUE;
			break;

		case 'q':
			Quiet = TRUE;
			break;

		case 's':
			Silent = TRUE;
			break;

		case 'v':
			Allbut = TRUE;
			break;

		case 'x':
			Exact = TRUE;
			break;

		case 'V':
			printf("%s", verstag);
			exit(0);
			break;

		case '?':
			usage();
			break;
		}
	}
	
	/* Update Argc and Argv to point to remaining arguments */
	Argc = argc - optind;
	Argv = &argv[optind];
	
	/* Check for argument conflicts */
	check_argument_conflicts();
}

/**
 * @brief Parse arguments using ReadArgs (Amiga style)
 * @param arg_array Array of parsed ReadArgs values
 */
void parse_readargs(arg_array)
LONG *arg_array;
{
	/* Set flags based on ReadArgs results */
	if (arg_array[ARG_COUNT_LINES]) {
		Countlines = TRUE;
	}
	if (arg_array[ARG_LIST_NAMES]) {
		Listnames = TRUE;
	}
	if (arg_array[ARG_NUMBER_LINES]) {
		Numberlines = TRUE;
	}
	if (arg_array[ARG_IGNORE_CASE]) {
		Monocase = TRUE;
	}
	if (arg_array[ARG_INVERT_MATCH]) {
		Allbut = TRUE;
	}
	if (arg_array[ARG_EXACT_MATCH]) {
		Exact = TRUE;
	}
	if (arg_array[ARG_SILENT]) {
		Silent = TRUE;
	}
	if (arg_array[ARG_EXTENDED_REGEX]) {
		Extended = TRUE;
	}
	if (arg_array[ARG_QUIET]) {
		Quiet = TRUE;
	}
	if (arg_array[ARG_ALWAYS_FILENAME]) {
		Always_filename = TRUE;
	}
	if (arg_array[ARG_NEVER_FILENAME]) {
		Never_filename = TRUE;
	}
	if (arg_array[ARG_LIST_NO_MATCH]) {
		List_no_match = TRUE;
	}
	if (arg_array[ARG_MAX_MATCHES]) {
		Max_matches = *(LONG *)arg_array[ARG_MAX_MATCHES];
		if (Max_matches <= 0) {
			fprintf(stderr, "%s: invalid max count: %ld\n", Program, Max_matches);
			usage();
		}
	}
	
	/* Check for argument conflicts */
	check_argument_conflicts();
}

/**
 * @brief Check for argument conflicts
 */
void check_argument_conflicts()
{
	if (
		(Silent &&
			(Allbut || Exact || Countlines || Listnames ||
				Numberlines))
		||
		(Allbut && Exact)
		||
		(Countlines && Listnames)
		||
		(Listnames && List_no_match)
		||
		(Always_filename && Never_filename)
	)
	{
		fprintf(stderr, "%s: argument conflict -- see the man page\n",
			Program);
		usage();	/* Will exit */
	}
}

/**
 * @brief Core grep logic separated from argument parsing
 * @param file_count Number of files to process
 * @param files Array of file names
 * @return Exit code
 */
int run_grep_logic(file_count, files)
int file_count;
char **files;
{
	int i;
	int flags;
	
	/* Process each pattern */
	for (Curpat = 0; Curpat <= Numpats; Curpat++)
	{
		if (Monocase)
			mapdown(Pattern[Curpat]);

		Pat_length[Curpat] = strlen(Pattern[Curpat]);
		
		/* Detect pattern type for smart matching */
		if (is_simple_pattern(Pattern[Curpat])) {
			Pattern_type[Curpat] = 0; /* Simple - use Boyer-Moore */
			initialize_boyer_moore();	/* Set up necessary tables */
		} else if (is_basic_regex(Pattern[Curpat])) {
			Pattern_type[Curpat] = 1; /* Basic regex - use enhanced matching */
		} else {
			Pattern_type[Curpat] = 2; /* Complex regex - use POSIX regex */
			/* Compile POSIX regex */
			flags = REG_EXTENDED | (Monocase ? REG_ICASE : 0);
			if (regcomp(&regex_expressions[Curpat], Pattern[Curpat], flags) != 0) {
				fprintf(stderr, "%s: bad regular expression: %s\n", Program, Pattern[Curpat]);
				exit(2);
			}
		}
	}

	Lotsafiles = (file_count > 1);	/* More than one file left */

	if (file_count == 0)		/* Search stdin */
		process("-");
	else
		for (i = 0; i < file_count; i++)
			process(files[i]);
	
	if (!Silent && Countlines)
		printf("%ld\n", Lines_matched);
	
	/* Exit with appropriate status for quiet mode */
	if (Quiet) {
		Exit_val = (Lines_matched > 0) ? 0 : 1;
	}

	/* Clean up regex expressions */
	for (Curpat = 0; Curpat <= Numpats; Curpat++) {
		if (Pattern_type[Curpat] == 2) {
			regfree(&regex_expressions[Curpat]);
		}
	}

	if (Lines_matched == 0)
		return 1;
	else if (Couldnt_open_files)
		return 2;
	else
		return 0;
}

/* Process files */
void process(infile)
char *infile;
{
	FILE *fp;
	int c;
	int match_success;
	long prev_lines_matched;
	
	prev_lines_matched = Lines_matched;	/* Save count */

	Curline = 0;	/* Reset for each file */

	if (infile[0] == '-' && infile[1] == '\0')
		fp = stdin;
	else if ((fp = fopen(infile, "r")) == NULL)
	{
		Couldnt_open_files = TRUE;
		if (!Silent) {
			perror(infile);
		}
		return;
	}

	while (fgets(Inbuf, sizeof Inbuf, fp) != NULL)
	{
		Curline++;
		if (Monocase)
			mapdown(Inbuf);
		Line_length = strlen(Inbuf);

		/* First, throw away rest of a truncated input line */
		if (Inbuf[Line_length - 1] != '\n')
			while ((c = getc(fp)) != '\n' && c != EOF)
				;
		else
			Inbuf[--Line_length] = '\0';
			/* Newline is there, nuke it */

		/* Check each pattern */
		for (Curpat = 0; Curpat <= Numpats; Curpat++)
		{
			if (match_success = enhanced_match(Inbuf, Pattern[Curpat], Pattern_type[Curpat]))
			{
				Lines_matched++;
				Match_count++;
				
				/* Check if we've reached max matches */
				if (Max_matches > 0 && Match_count >= Max_matches) {
					if (Quiet) {
						/* Exit immediately on first match in quiet mode */
						fclose(fp);
						return;
					}
				}
			}
			
			/* Do any necessary output */
			if (!Silent && !Countlines && !Listnames &&
				((match_success != FALSE) ^ (Allbut != FALSE)))
				/* Either match_success, or Allbut, but not both,
				   and not neither */
			{
				/* Handle filename display logic */
				int show_filename = FALSE;
				if (Always_filename || (Lotsafiles && !Never_filename)) {
					show_filename = TRUE;
				}
				
				if (show_filename)
					printf("%s:", infile);
				if (Numberlines)
					printf("%ld:", Curline);
				printf("%s\n", Inbuf);
			}
		}
	}

	fclose(fp);
	
	/* Handle file listing based on options */
	if (!Silent) {
		if (Listnames && prev_lines_matched < Lines_matched) {
			/* -l: List files with matches */
			printf("%s\n", infile);
		} else if (List_no_match && prev_lines_matched == Lines_matched) {
			/* -L: List files without matches */
			printf("%s\n", infile);
		}
	}
}



/* Map string to lowercase */
void mapdown(str)
char *str;
{
	int i;

	for (i = 0; str[i] != '\0'; i++)
		if (isupper(str[i]))
			str[i] = tolower(str[i]);
}



/* Print usage message and exit */
void usage()
{
	fprintf(stderr, "Version: %s", &verstag[6]);
	fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [PATTERN] [FILE...]\n", Program);
	fprintf(stderr, "Usage (Amiga): %s PATTERN/K [FILE/M] [COUNTLINES/S] [LISTNAMES/S] [NUMBERLINES/S] [IGNORECASE/S] [INVERTMATCH/S] [EXACTMATCH/S] [SILENT/S] [REGEX/S] [QUIET/S] [ALWAYSFILENAME/S] [NEVERFILENAME/S] [LISTNOMATCH/S] [MAXMATCHES/K/N]\n", Program);
	fprintf(stderr, "               %s ? for template\n", Program);
	fprintf(stderr, "OPTIONS:\n");
	fprintf(stderr, "  -c          print only a count of matching lines\n");
	fprintf(stderr, "  -E          PATTERN is an extended regular expression\n");
	fprintf(stderr, "  -e PATTERN  use PATTERN for matching\n");
	fprintf(stderr, "  -f FILE     obtain PATTERN from FILE\n");
	fprintf(stderr, "  -h          never print filenames with output lines\n");
	fprintf(stderr, "  -H          always print filenames with output lines\n");
	fprintf(stderr, "  -i          ignore case distinctions\n");
	fprintf(stderr, "  -l          print only names of files with matches\n");
	fprintf(stderr, "  -L          print only names of files without matches\n");
	fprintf(stderr, "  -m NUM      stop after NUM matches\n");
	fprintf(stderr, "  -n          print line number with output lines\n");
	fprintf(stderr, "  -q          suppress all normal output\n");
	fprintf(stderr, "  -s          suppress error messages\n");
	fprintf(stderr, "  -v          select non-matching lines\n");
	fprintf(stderr, "  -x          match whole lines only\n");
	fprintf(stderr, "  -y          ignore case (obsolete, use -i)\n");
	fprintf(stderr, "  -V          display version information\n");
	fprintf(stderr, "DESCRIPTION:\n");
	fprintf(stderr, "  Search for PATTERN in each FILE or standard input.\n");
	fprintf(stderr, "  PATTERN is, by default, a basic regular expression (BRE).\n");
	fprintf(stderr, "  Example: %s -i 'hello world' menu.h main.c\n", Program);
	exit(2);
}

/* Index function for compatibility */
char *my_index(str, c)
char *str, c;
{
	for (; *str; str++)
		if (*str == c)
			return (char *)str;
	
	return NULL;
}

/* Retrieve patterns from file */
void patfromfile(infile)
char *infile;
{
	int i, j;
	FILE *fp;
	int c;

	if ((fp = fopen(infile, "r")) == NULL ||
			(c = getc(fp)) == EOF)
	{
		if (!Silent) {
			perror(infile);	/* Be like standard grep */
		}
		exit(2);
	}
	else
		ungetc(c, fp);

	for (i = 0; fgets(Pattern[i], MAXLINE, fp) != NULL; i++)
	{
		if (i >= 120)
		{
			fprintf(stderr, "%s: Only %d strings allowed\n",
				Program, MAXPATS);
			exit(2);
		}
		j = strlen(Pattern[i]);
		if (Pattern[i][j - 1] == '\n')
			Pattern[i][--j] = '\0';
	}
	Numpats = i - 1;

	fclose(fp);
}

/* Set up patterns from a string */
void setpats(str)
char *str;
{
	int i, j;

	while (*str == '\n' || *str == '\r')
		str++;

	for (i = j = 0; *str; str++)
	{
		if (*str == '\n')
		{
			Pattern[i][j] = '\0';
			j = 0;
			i++;
		}
		else
			Pattern[i][j++] = *str;
	}
	Numpats = i;
}

/* Initialize Boyer-Moore tables */
void initialize_boyer_moore()
{
	int i, t;
	unsigned char c;

	for (i = 0; i < 64; i++)
		D1[Curpat][i] = Pat_length[Curpat];
	
	for (i = 0; i < Pat_length[Curpat]; i++) {
		c = (unsigned char)Pattern[Curpat][i];
		if (c < 64)  /* Only handle ASCII 0-63 for NEAR memory */
			D1[Curpat][c] = Pat_length[Curpat] - i - 1;
	}
	
	for (i = 0; i < Pat_length[Curpat]; i++)
		D2[Curpat][i] = (Pat_length[Curpat] << 1) - i - 1;
	
	for (i = (t = Pat_length[Curpat]) - 1; i >= 0; i--, t--)
		for (F[Curpat][i] = t; t < Pat_length[Curpat]
			&& Pattern[Curpat][i] != Pattern[Curpat][t];
							t = F[Curpat][t])
			if (Pat_length[Curpat] - i - 1 < D2[Curpat][t])
				D2[Curpat][t] = Pat_length[Curpat] - i - 1;

	for (i = 0; i <= t; i++)
		if (Pat_length[Curpat] + t - i < D2[Curpat][i])
			D2[Curpat][i] = Pat_length[Curpat] + t - i;
}

/* Boyer-Moore pattern search */
int boyer_moore_match(line, pattern)
char *line, *pattern;
{
	int i, j;

	if (Exact && Pat_length[Curpat] != Line_length)
		return FALSE;

	i = Pat_length[Curpat] - 1;

	while (i < Line_length)
	{
		j = Pat_length[Curpat] - 1;
		while (j >= 0)
		{
			if (line[i] == Pattern[Curpat][j])
				i--, j--;
			else
				break;
		}

		if (j < 0)
		{
			/* Found a match */
			return TRUE;
		}
		else
		{
			unsigned char c = (unsigned char)line[i];
			if (c < 64) {
				j = (D1[Curpat][c] >= D2[Curpat][j]) ?
					D1[Curpat][c]
				:
					D2[Curpat][j];
        } else {
				j = D2[Curpat][j];
			}
			i += j;
			/* Shift right by j places */
		}
	}

	return FALSE;
}

/* Pattern type detection functions */

/* Check if pattern is simple text (no regex) */
int is_simple_pattern(pattern)
char *pattern;
{
	/* Check for regex metacharacters */
	return !strpbrk(pattern, ".*+?|()[]{}^$\\");
}

/* Check if pattern uses basic regex features */
int is_basic_regex(pattern)
char *pattern;
{
	char *p = pattern;
	
	/* Skip if it's a simple pattern */
	if (is_simple_pattern(pattern))
		return FALSE;
	
	/* Check for basic regex features only */
	while (*p) {
		switch (*p) {
		case '.':
		case '*':
		case '?':
		case '[':
		case ']':
		case '^':
		case '$':
			return TRUE;
		case '\\':
			p++; /* Skip escaped character */
			if (*p) p++;
			break;
		default:
			p++;
			break;
		}
	}
	return FALSE;
}

/* Lightweight regex for common patterns */
int basic_regex_match(line, pattern)
char *line, *pattern;
{
	/* Simple regex implementation for common patterns */
	/* This is much faster than full regex but handles most use cases */
	
	char *p = pattern;
	char *l = line;
	int line_len = strlen(line);
	int pat_len = strlen(pattern);
	
	/* Handle start anchor */
	if (*p == '^') {
		p++;
		pat_len--;
		if (pat_len > line_len) return FALSE;
		return basic_regex_match(line, p);
	}
	
	/* Handle end anchor */
	if (pattern[pat_len-1] == '$') {
		pattern[pat_len-1] = '\0';
		pat_len--;
		if (pat_len > line_len) return FALSE;
		l = line + line_len - pat_len;
		return basic_regex_match(l, pattern);
	}
	
	/* Simple pattern matching with wildcards */
	return wildcard_match(line, pattern);
}

/* Handle * and ? wildcards */
int wildcard_match(line, pattern)
char *line, *pattern;
{
	char *p = pattern;
	char *l = line;
	int matched;
	
	while (*p && *l) {
		if (*p == '*') {
			/* Skip multiple characters */
			p++;
			if (!*p) return TRUE; /* * at end matches anything */
			
			/* Find next occurrence of pattern after * */
			while (*l) {
				if (wildcard_match(l, p)) return TRUE;
				l++;
			}
			return FALSE;
		} else if (*p == '?') {
			/* Skip one character */
			p++;
			l++;
		} else if (*p == '[') {
			/* Character class */
			p++;
			if (!*l) return FALSE;
			
			matched = FALSE;
			while (*p && *p != ']') {
				if (*p == *l) {
					matched = TRUE;
					break;
				}
				p++;
			}
			if (!matched) return FALSE;
			
			/* Skip to end of character class */
			while (*p && *p != ']') p++;
			if (*p == ']') p++;
			l++;
		} else if (*p == '\\') {
			/* Escaped character */
			p++;
			if (*p != *l) return FALSE;
			p++;
			l++;
		} else {
			/* Literal character */
			if (*p != *l) return FALSE;
			p++;
			l++;
		}
	}
	
	/* Both pattern and line must be exhausted */
	return (*p == '\0' && *l == '\0');
}

/* Smart pattern matching with fallback */
int enhanced_match(line, pattern, pattern_type)
char *line, *pattern;
int pattern_type;
{
	switch (pattern_type) {
	case 0: /* Simple pattern */
		return boyer_moore_match(line, pattern);
	case 1: /* Basic regex */
		return basic_regex_match(line, pattern);
	case 2: /* Complex regex */
		return complex_regex_match(line, pattern, Curpat);
	default:
		return FALSE;
	}
}

/* POSIX regex for complex patterns */
int complex_regex_match(line, pattern, pattern_index)
char *line, *pattern;
int pattern_index;
{
	/* Use compiled POSIX regex */
	return (regexec(&regex_expressions[pattern_index], line, 0, NULL, 0) == 0);
}
