/*
 * basename - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on basename by B. Garfolo & P. Nelson.
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

#include "basename.h"
#include "/common/common.h"
#include "/common/getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: basename 2.0 (23/08/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_NAME,
    ARG_SUFFIX,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void usage(char *program);
int run_basename_logic(char *name, char *suffix, const char *program);
void parse_getopt_args(int argc, char **argv, char **name, char **suffix, int *file_start, const char *program);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    char *name = NULL;
    char *suffix = NULL;
    int file_start = 1;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "NAME/M,SUFFIX/S,POSIX/K/F";
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
        parse_getopt_args(argc, argv, &name, &suffix, &file_start, program);
        return run_basename_logic(name, suffix, program);
        
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

                parse_getopt_args(new_argc, new_argv, &name, &suffix, &file_start, program);
                ret_code = run_basename_logic(name, suffix, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_NAME]) {
                    name = (char *)arg_array[ARG_NAME];
                    if (arg_array[ARG_SUFFIX]) {
                        suffix = (char *)arg_array[ARG_SUFFIX];
                    }
                    ret_code = run_basename_logic(name, suffix, program);
                } else {
                    fprintf(stderr, "%s: NAME argument required\n", program);
                    ret_code = FAILURE;
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
 * @param name Name to process
 * @param suffix Suffix to remove
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, char **name, char **suffix, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "hV")) != -1) {
        switch (c) {
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
    
    /* Check for required arguments */
    if (optind >= argc) {
        fprintf(stderr, "%s: NAME argument required\n", program);
        exit(FAILURE);
    }
    
    *name = argv[optind++];
    
    if (optind < argc) {
        *suffix = argv[optind++];
    }
    
    if (optind < argc) {
        fprintf(stderr, "%s: too many arguments\n", program);
        exit(FAILURE);
    }
}

/**
 * @brief Core basename logic separated from argument parsing
 * @param name The name to process
 * @param suffix The suffix to remove (can be NULL)
 * @param program Program name for error messages
 * @return Exit code
 */
int run_basename_logic(char *name, char *suffix, const char *program) {
    char *result_string;    /* The pointer into name */
    char *temp;             /* Used to move around in name */
    int suffix_len;         /* Length of the suffix */
    int suffix_start;       /* Where the suffix should start */

    if (!name) {
        fprintf(stderr, "%s: NAME argument required\n", program);
        return FAILURE;
    }

    /* Check for all /'s or :'s (Amiga path separator) */
    for (temp = name; (*temp == '/') || (*temp == ':'); temp++) {
        /* Move to next char */
    }
    if (*temp == '\0') {
        printf("%c\n", (name[0] == '/') ? '/' : ':');
        return SUCCESS;
    }

    /* Build the basename */
    result_string = name;

    /* Find the last / or : */
    temp = strrchr(result_string, '/');
    if (temp == NULL) {
        temp = strrchr(result_string, ':');
    }

    if (temp != NULL) {
        /* Remove trailing /'s or :'s */
        while ((*(temp + 1) == '\0') && ((*temp == '/') || (*temp == ':'))) {
            *temp-- = '\0';
        }

        /* Set result_string to last part of path */
        if (*temp != '/' && *temp != ':') {
            temp = strrchr(result_string, '/');
            if (temp == NULL) {
                temp = strrchr(result_string, ':');
            }
        }
        if (temp != NULL && (*temp == '/' || *temp == ':')) {
            result_string = temp + 1;
        }
    }

    /* Remove the suffix, if any */
    if (suffix) {
        suffix_len = strlen(suffix);
        suffix_start = strlen(result_string) - suffix_len;
        if (suffix_start > 0) {
            if (strcmp(result_string + suffix_start, suffix) == 0) {
                *(result_string + suffix_start) = '\0';
            }
        }
    }

    /* Print the resultant string */
    printf("%s\n", result_string);
    return SUCCESS;
}

/*
 * Display usage information
 */
void usage(char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] NAME [SUFFIX]\n", program);
    fprintf(stderr, "Usage (Amiga): %s NAME/M [SUFFIX/s]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Print NAME with any leading directory components removed.\n");
    fprintf(stderr, "  If SUFFIX is specified, also remove a trailing SUFFIX.\n");
    exit(FAILURE);
}
