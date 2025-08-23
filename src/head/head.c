/*
 * head - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on head by Andy Tanenbaum.
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

#include "head.h"
#include "/common/common.h"
#include "/common/getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: head 1.0 (23/08/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */
#define DEFAULT_LINES 10        /* default number of lines */

/* For ReadArgs template */
enum {
    ARG_FILE,
    ARG_LINES,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void do_file(int n, FILE *f);
void usage(char *program);
int run_head_logic(int lines, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, int *lines, int *file_start, const char *program);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    int lines = DEFAULT_LINES;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILE/M,LINES/K/N,POSIX/K/F";
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
    int file_count;
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
        return run_head_logic(lines, 0, NULL, program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &lines, &file_start, program);
        files = &argv[file_start];
        return run_head_logic(lines, argc - file_start, files, program);
        
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

                parse_getopt_args(new_argc, new_argv, &lines, &file_start, program);
                files = &new_argv[file_start];
                ret_code = run_head_logic(lines, new_argc - file_start, files, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_LINES]) {
                    lines = (int)*(LONG *)arg_array[ARG_LINES];
                    if (lines <= 0) {
                        fprintf(stderr, "%s: invalid number of lines: %d\n", program, lines);
                        ret_code = FAILURE;
                    }
                }
                
                if (arg_array[ARG_FILE]) {
                    /* Count files and allocate array */
                    file_count = 0;
                    while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
                        file_count++;
                    }
                    
                    files = (char **)arg_array[ARG_FILE];
                    ret_code = run_head_logic(lines, file_count, files, program);
                } else {
                    /* No files specified, read from stdin */
                    ret_code = run_head_logic(lines, 0, NULL, program);
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
 * @param lines Number of lines to output
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, int *lines, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "n:hV")) != -1) {
        switch (c) {
            case 'n':
                *lines = atoi(optarg);
                if (*lines <= 0) {
                    fprintf(stderr, "%s: invalid number of lines: %s\n", program, optarg);
                    exit(FAILURE);
                }
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
}

/**
 * @brief Core head logic separated from argument parsing
 * @param lines Number of lines to output
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_head_logic(int lines, int file_count, char **files, const char *program) {
    FILE *f;
    int i;
    
    if (file_count == 0) {
        /* Print standard input only */
        do_file(lines, stdin);
        return SUCCESS;
    }

    /* One or more files have been listed explicitly */
    for (i = 0; i < file_count; i++) {
        if (file_count > 1) {
            printf("==> %s <==\n", files[i]);
        }
        
        if ((f = fopen(files[i], "r")) == NULL) {
            fprintf(stderr, "%s: cannot open %s\n", program, files[i]);
        } else {
            do_file(lines, f);
            fclose(f);
        }
        
        if (i < file_count - 1) {
            printf("\n");
        }
    }
    
    return SUCCESS;
}

/*
 * Print the first 'n' lines of a file
 * Uses character-by-character processing for robustness
 */
void do_file(int n, FILE *f)
{
	int c;

	/* Print the first 'n' lines of a file */
	while (n > 0) {
		c = getc(f);
		switch (c) {
			case EOF:
				return;
			case '\n':
				--n;
				/* Fall through to print the newline */
			default:
				putc((char)c, stdout);
		}
	}
}

/*
 * Display usage information
 */
void usage(char *program)
{
	fprintf(stderr, "Version: %s\n", &verstag[6]);
	fprintf(stderr, "Usage (POSIX): %s [-n number] [file...]\n", program);
	fprintf(stderr, "Usage (Amiga): %s FILE/M [LINES/K/N]\n", program);
	fprintf(stderr, "               %s ? for template\n", program);
	fprintf(stderr, "OPTIONS:\n");
	fprintf(stderr, "  -n NUM      print the first NUM lines (default: %d)\n", DEFAULT_LINES);
	fprintf(stderr, "  -h, -V      display this help and version\n");
	fprintf(stderr, "DESCRIPTION:\n");
	fprintf(stderr, "  Print the first few lines of each FILE to standard output.\n");
	fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
	exit(FAILURE);
}
