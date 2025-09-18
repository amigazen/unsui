/*
 * SPDX-License-Identifier: BSD-4-Clause-UC
 * See LICENSE.md for full license text.
 */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

#include "extern.h"
#include "getopt.h"
#include "common.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: tr 2.0 (18/09/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_STRING1,
    ARG_STRING2,
    ARG_COMPLEMENT,
    ARG_DELETE,
    ARG_SQUEEZE,
    ARG_UNBUFFERED,
    ARG_POSIX,
    ARG_COUNT
};

static int string1[NCHARS] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,		/* ASCII */
	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
	0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
	0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
	0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
	0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
	0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
	0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
	0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
	0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
	0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
	0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
	0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
	0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
	0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
	0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
	0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
	0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
	0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
	0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
	0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
	0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
	0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
	0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
	0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
}, string2[NCHARS];

STR s1 = { STRING1, NORMAL, 0, OOBCH, { 0, OOBCH }, NULL, NULL };
STR s2 = { STRING2, NORMAL, 0, OOBCH, { 0, OOBCH }, NULL, NULL };

static void setup(int *, char *, STR *, int);
static void usage(char *program);
static int run_tr_logic(int cflag, int dflag, int sflag, int uflag, char *string1_arg, char *string2_arg, const char *program);
static void parse_getopt_args(int argc, char **argv, int *cflag, int *dflag, int *sflag, int *uflag, char **string1_arg, char **string2_arg, const char *program);

int
main(int argc, char **argv)
{
    int cflag = FALSE;
    int dflag = FALSE;
    int sflag = FALSE;
    int uflag = FALSE;
    char *string1_arg = NULL;
    char *string2_arg = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "STRING1/M,STRING2/K,COMPLEMENT/S,DELETE/S,SQUEEZE/S,UNBUFFERED/S,POSIX/K/F";
    LONG arg_array[ARG_COUNT] = {0};
    struct RDArgs *rdargs = NULL;
    char *cmd_string = NULL;
    int ret_code = SUCCESS;
    BOOL interactive_help = FALSE;
    
    /* POSIX/F Path Variables */
    char *posix_str;
    int new_argc;
    char *new_argv[MAX_TEMPLATE_ITEMS];
    int i;
    char initial_args_str[256];
    char user_input_buf[256];
    char *temp_str;
    size_t combined_len;

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    if (argc == 1) {
        /* No arguments, show usage */
        usage(program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &cflag, &dflag, &sflag, &uflag, &string1_arg, &string2_arg, program);
        return run_tr_logic(cflag, dflag, sflag, uflag, string1_arg, string2_arg, program);
        
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
            fprintf(stderr, "%s: out of memory for RDArgs\n", program);
            return FAILURE;
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
            fprintf(stderr, "%s: out of memory for command string\n", program);
            FreeDosObject(DOS_RDARGS, rdargs);
            return FAILURE;
        }

        /* Set up ReadArgs to parse from our string */
        rdargs->RDA_Source.CS_Buffer = cmd_string;
        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
        rdargs->RDA_Source.CS_CurChr = 0;
        rdargs->RDA_Flags |= RDAF_NOPROMPT;

        if (!ReadArgs(template, arg_array, rdargs)) {
            PrintFault(IoErr(), program);
            ret_code = FAILURE;
        } else {
            /* Check for POSIX/F override first */
            if (arg_array[ARG_POSIX]) {
                posix_str = (char *)arg_array[ARG_POSIX];

                /* Tokenize the string and build a new argv for getopt */
                new_argv[0] = program;
                new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

                parse_getopt_args(new_argc, new_argv, &cflag, &dflag, &sflag, &uflag, &string1_arg, &string2_arg, program);
                ret_code = run_tr_logic(cflag, dflag, sflag, uflag, string1_arg, string2_arg, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_COMPLEMENT]) {
                    cflag = TRUE;
                }
                if (arg_array[ARG_DELETE]) {
                    dflag = TRUE;
                }
                if (arg_array[ARG_SQUEEZE]) {
                    sflag = TRUE;
                }
                if (arg_array[ARG_UNBUFFERED]) {
                    uflag = TRUE;
                }
                
                if (arg_array[ARG_STRING1]) {
                    string1_arg = (char *)arg_array[ARG_STRING1];
                }
                if (arg_array[ARG_STRING2]) {
                    string2_arg = (char *)arg_array[ARG_STRING2];
                }
                
                ret_code = run_tr_logic(cflag, dflag, sflag, uflag, string1_arg, string2_arg, program);
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
 * @param cflag Flag to complement character sets
 * @param dflag Flag to delete characters
 * @param sflag Flag to squeeze repeated characters
 * @param uflag Flag for unbuffered output
 * @param string1_arg Pointer to string1 argument
 * @param string2_arg Pointer to string2 argument
 * @param program Program name for error messages
 */
static void
parse_getopt_args(int argc, char **argv, int *cflag, int *dflag, int *sflag, int *uflag, char **string1_arg, char **string2_arg, const char *program) {
    int c;
    int string_count = 0;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "Ccdsu")) != -1) {
        switch (c) {
            case 'C':
            case 'c':
                *cflag = TRUE;
                break;
            case 'd':
                *dflag = TRUE;
                break;
            case 's':
                *sflag = TRUE;
                break;
            case 'u':
                *uflag = TRUE;
                break;
            case '?':
                usage(program);
                break;
        }
    }
    
    /* Collect string arguments */
    while (optind < argc) {
        if (string_count == 0) {
            *string1_arg = argv[optind];
        } else if (string_count == 1) {
            *string2_arg = argv[optind];
        } else {
            err("too many arguments");
        }
        string_count++;
        optind++;
    }
    
    /* Validate argument combinations */
    if (*dflag && *sflag) {
        if (!*string1_arg || !*string2_arg) {
            err("tr -ds requires both string1 and string2");
        }
    } else if (*dflag) {
        if (!*string1_arg) {
            err("tr -d requires string1");
        }
        if (*string2_arg) {
            err("tr -d does not accept string2");
        }
    } else if (*sflag && !*string2_arg) {
        if (!*string1_arg) {
            err("tr -s requires string1");
        }
    } else {
        if (!*string1_arg || !*string2_arg) {
            err("tr requires both string1 and string2");
        }
    }
}

/**
 * @brief Core tr logic separated from argument parsing
 * @param cflag Flag to complement character sets
 * @param dflag Flag to delete characters
 * @param sflag Flag to squeeze repeated characters
 * @param uflag Flag for unbuffered output
 * @param string1_arg String1 argument
 * @param string2_arg String2 argument
 * @param program Program name for error messages
 * @return Exit code
 */
static int
run_tr_logic(int cflag, int dflag, int sflag, int uflag, char *string1_arg, char *string2_arg, const char *program) {
    register int ch, cnt, lastch, *p;
    int isstring2 = (string2_arg != NULL);
    
    /* Set unbuffered output if -u option specified */
    if (uflag) {
        setvbuf(stdout, NULL, _IONBF, 0);
    }

    /*
     * tr -ds [-c] string1 string2
     * Delete all characters (or complemented characters) in string1.
     * Squeeze all characters in string2.
     */
    if (dflag && sflag) {
        if (!isstring2)
            err("tr -ds requires both string1 and string2");

        setup(string1, string1_arg, &s1, cflag);
        setup(string2, string2_arg, &s2, 0);
        
        for (lastch = OOBCH; (ch = getchar()) != EOF;)
            if (!string1[ch] && (!string2[ch] || lastch != ch)) {
                lastch = ch;
                (void)putchar(ch);
            }
        return SUCCESS;
    }

    /*
     * tr -d [-c] string1
     * Delete all characters (or complemented characters) in string1.
     */
    if (dflag) {
        if (isstring2)
            err("tr -d does not accept string2");

        setup(string1, string1_arg, &s1, cflag);

        while ((ch = getchar()) != EOF)
            if (!string1[ch])
                (void)putchar(ch);
        return SUCCESS;
    }

    /*
     * tr -s [-c] string1
     * Squeeze all characters (or complemented characters) in string1.
     */
    if (sflag && !isstring2) {
        setup(string1, string1_arg, &s1, cflag);

        for (lastch = OOBCH; (ch = getchar()) != EOF;)
            if (!string1[ch] || lastch != ch) {
                lastch = ch;
                (void)putchar(ch);
            }
        return SUCCESS;
    }

    /*
     * tr [-cs] string1 string2
     * Replace all characters (or complemented characters) in string1 with
     * the character in the same position in string2.  If the -s option is
     * specified, squeeze all the characters in string2.
     */
    if (!isstring2)
        err("tr requires both string1 and string2");

    s1.str = string1_arg;
    s2.str = string2_arg;

    if (cflag)
        for (cnt = NCHARS, p = string1; cnt--;)
            *p++ = OOBCH;

    if (!next(&s2))
        err("empty string2");

    /* If string2 runs out of characters, use the last one specified. */
    if (sflag)
        while (next(&s1)) {
            string1[s1.lastch] = ch = s2.lastch;
            string2[ch] = 1;
            (void)next(&s2);
        }
    else
        while (next(&s1)) {
            string1[s1.lastch] = ch = s2.lastch;
            (void)next(&s2);
        }

    if (cflag)
        for (cnt = 0, p = string1; cnt < NCHARS; ++p, ++cnt)
            *p = *p == OOBCH ? ch : cnt;

    if (sflag)
        for (lastch = OOBCH; (ch = getchar()) != EOF;) {
            ch = string1[ch];
            if (!string2[ch] || lastch != ch) {
                lastch = ch;
                (void)putchar(ch);
            }
        }
    else
        while ((ch = getchar()) != EOF)
            (void)putchar(string1[ch]);
    
    return SUCCESS;
}

static void
setup(int *string, char *arg, STR *str, int cflag)
{
	register int cnt, *p;

	str->str = arg;
	memset(string, 0, NCHARS * sizeof(int));
	while (next(str))
		string[str->lastch] = 1;
	if (cflag)
		for (p = string, cnt = NCHARS; cnt--; ++p)
			*p = !*p;
}

static void
usage(char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] string1 [string2]\n", program);
    fprintf(stderr, "Usage (Amiga): %s STRING1/M [STRING2/K] [COMPLEMENT/S] [DELETE/S] [SQUEEZE/S] [UNBUFFERED/S]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -c, -C      complement the set of characters in string1\n");
    fprintf(stderr, "  -d          delete characters in string1 from input\n");
    fprintf(stderr, "  -s          squeeze repeated characters\n");
    fprintf(stderr, "  -u          unbuffered output\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Translate, squeeze, and/or delete characters from standard input.\n");
    fprintf(stderr, "  Read from standard input, write to standard output.\n");
    fprintf(stderr, "  Supports character classes, ranges, and escape sequences.\n");
    exit(FAILURE);
}

#ifdef  __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#ifdef __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#ifdef  __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "tr: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
