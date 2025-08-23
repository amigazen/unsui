/*
 * wc - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on wc by David Messer and Andy Tanenbaum.
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

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

#include "wc.h"
#include "/common/common.h"
#include "/common/getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: wc 2.0 (23/08/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_FILE,
    ARG_LINES,
    ARG_WORDS,
    ARG_CHARS,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void count(FILE *f);
void usage(char *program);
void reset_counters(void);
int run_wc_logic(int lflag, int wflag, int cflag, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, int *lflag, int *wflag, int *cflag, int *file_start, const char *program);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    int lflag = FALSE;
    int wflag = FALSE;
    int cflag = FALSE;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILE/M,LINES/S,WORDS/S,CHARS/S,POSIX/K/F";
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
        /* No arguments, read from stdin with default settings */
        lflag = TRUE;
        wflag = TRUE;
        cflag = TRUE;
        return run_wc_logic(lflag, wflag, cflag, 0, NULL, program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &lflag, &wflag, &cflag, &file_start, program);
        files = &argv[file_start];
        return run_wc_logic(lflag, wflag, cflag, argc - file_start, files, program);
        
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

                parse_getopt_args(new_argc, new_argv, &lflag, &wflag, &cflag, &file_start, program);
                files = &new_argv[file_start];
                ret_code = run_wc_logic(lflag, wflag, cflag, new_argc - file_start, files, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_LINES]) {
                    lflag = TRUE;
                }
                if (arg_array[ARG_WORDS]) {
                    wflag = TRUE;
                }
                if (arg_array[ARG_CHARS]) {
                    cflag = TRUE;
                }
                
                /* If no flags are set, treat as wc -lwc */
                if (!lflag && !wflag && !cflag) {
                    lflag = TRUE;
                    wflag = TRUE;
                    cflag = TRUE;
                }
                
                if (arg_array[ARG_FILE]) {
                    /* Count files and allocate array */
                    int file_count = 0;
                    while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
                        file_count++;
                    }
                    
                    files = (char **)arg_array[ARG_FILE];
                    ret_code = run_wc_logic(lflag, wflag, cflag, file_count, files, program);
                } else {
                    /* No files specified, read from stdin */
                    ret_code = run_wc_logic(lflag, wflag, cflag, 0, NULL, program);
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
 * @param lflag Flag to count lines
 * @param wflag Flag to count words
 * @param cflag Flag to count characters
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, int *lflag, int *wflag, int *cflag, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "lwchV")) != -1) {
        switch (c) {
            case 'l':
                *lflag = TRUE;
                break;
            case 'w':
                *wflag = TRUE;
                break;
            case 'c':
                *cflag = TRUE;
                break;
            case 'h':
            case 'V':
                usage(program);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    *file_start = optind;
    
    /* If no flags are set, treat as wc -lwc */
    if (!*lflag && !*wflag && !*cflag) {
        *lflag = TRUE;
        *wflag = TRUE;
        *cflag = TRUE;
    }
}

/**
 * @brief Core wc logic separated from argument parsing
 * @param lflag Flag to count lines
 * @param wflag Flag to count words
 * @param cflag Flag to count characters
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_wc_logic(int lflag, int wflag, int cflag, int file_count, char **files, const char *program) {
    int i;
    int tflag;
    long ltotal = 0;
    long wtotal = 0;
    long ctotal = 0;
    FILE *f;
    
    /* Check to see if input comes from std input */
    if (file_count == 0) {
        count(stdin);
        if (lflag) printf(" %6ld", lcount);
        if (wflag) printf(" %6ld", wcount);
        if (cflag) printf(" %6ld", ccount);
        printf("\n");
        fflush(stdout);
        return SUCCESS;
    }

    /* There is an explicit list of files. Loop on files */
    tflag = file_count >= 2; /* Set if # files > 1 */
    
    for (i = 0; i < file_count; i++) {
        if ((f = fopen(files[i], "r")) == NULL) {
            fprintf(stderr, "%s: cannot open %s\n", program, files[i]);
        } else {
            reset_counters();
            count(f);
            if (lflag) printf(" %6ld", lcount);
            if (wflag) printf(" %6ld", wcount);
            if (cflag) printf(" %6ld", ccount);
            printf(" %s\n", files[i]);
            fclose(f);
            
            /* Add to totals */
            ltotal += lcount;
            wtotal += wcount;
            ctotal += ccount;
        }
    }

    /* Print totals if multiple files */
    if (tflag) {
        if (lflag) printf(" %6ld", ltotal);
        if (wflag) printf(" %6ld", wtotal);
        if (cflag) printf(" %6ld", ctotal);
        printf(" total\n");
    }
    
    fflush(stdout);
    return SUCCESS;
}

/* Global counters for current file */
long lcount = 0;        /* Count of lines */
long wcount = 0;        /* Count of words */
long ccount = 0;        /* Count of characters */

/*
 * Count lines, words, and characters in a file
 * Updates global counters
 */
void count(FILE *f)
{
	int c;
	int word = FALSE;

	reset_counters();

	while ((c = getc(f)) != EOF) {
		ccount++;

		if (isspace(c)) {
			if (word) {
				wcount++;
			}
			word = FALSE;
		} else {
			word = TRUE;
		}

		if (c == '\n' || c == '\f') {
			lcount++;
		}
	}
}

/*
 * Reset per-file counters
 */
void reset_counters(void)
{
	lcount = 0;
	wcount = 0;
	ccount = 0;
}

/*
 * Display usage information
 */
void usage(char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [FILE...]\n", program);
    fprintf(stderr, "Usage (Amiga): %s FILE/M [LINES/S] [WORDS/S] [CHARS/S]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -c          print the character counts\n");
    fprintf(stderr, "  -l          print the line counts\n");
    fprintf(stderr, "  -w          print the word counts\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Print newline, word, and byte counts for each FILE.\n");
    fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
    fprintf(stderr, "  Default: all three counts if no options specified.\n");
	exit(FAILURE);
}
