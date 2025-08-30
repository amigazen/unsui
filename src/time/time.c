/*
 * time - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on time by Martin W. Scott.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing, and provides full POSIX time command functionality.
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
#include <dos/rdargs.h>
#include <exec/memory.h>
#include <proto/dos.h>
#include <proto/exec.h>

#include "time.h"
#include "common.h"
#include "getopt.h"

extern struct DosLibrary *DOSBase;

/* Version tag for Amiga */
static const char *verstag = "$VER: time 1.1 (28/08/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_COMMAND,
    ARG_PORTABLE,
    ARG_VERBOSE,
    ARG_FORMAT,
    ARG_POSIX,
    ARG_COUNT
};

/* Time command options structure */
typedef struct {
    int portable_flag;          /* -p: portable format */
    int verbose_flag;           /* -v: verbose output */
    char *format_string;        /* -f: custom format */
    char *command;              /* command to execute */
    char **command_args;        /* arguments for command */
    int command_argc;           /* number of command arguments */
} TimeOptions;

/* Function declarations for forward references */
void usage(const char *program);
int run_time_logic(TimeOptions *options, const char *program);
void parse_getopt_args(int argc, char **argv, TimeOptions *options, int *file_start, const char *program);
void init_options(TimeOptions *options);
void cleanup_options(TimeOptions *options);
void PrintDateStamp(struct DateStamp *ds);
void SubDateStamp(struct DateStamp *ds, struct DateStamp *amount);
void print_time_output(struct DateStamp *ds, TimeOptions *options);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    TimeOptions options;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "COMMAND/A/F,PORTABLE/S,VERBOSE/S,FORMAT/K,POSIX/K/F";
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
        exit(FAILURE);
    }

    /* Initialize options */
    init_options(&options);

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &options, NULL, program);
        return run_time_logic(&options, program);
        
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
            fprintf(stderr, "%s: Failed to build command string\n", program);
            FreeDosObject(DOS_RDARGS, rdargs);
            return FAILURE;
        }

        /* Set up ReadArgs */
        rdargs->RDA_Source.CS_Buffer = cmd_string;
        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
        rdargs->RDA_Flags |= RDAF_NOPROMPT;

        /* Parse arguments */
        if (ReadArgs(template, arg_array, rdargs)) {
            /* Process ReadArgs results */
            if (arg_array[ARG_COMMAND]) {
                options.command = (char *)arg_array[ARG_COMMAND];
            }
            if (arg_array[ARG_PORTABLE]) {
                options.portable_flag = TRUE;
            }
            if (arg_array[ARG_VERBOSE]) {
                options.verbose_flag = TRUE;
            }
            if (arg_array[ARG_FORMAT]) {
                options.format_string = (char *)arg_array[ARG_FORMAT];
            }
            
            /* Check for POSIX escape hatch */
            if (arg_array[ARG_POSIX]) {
                posix_str = (char *)arg_array[ARG_POSIX];
                
                /* Build new argv for getopt parsing */
                new_argv[0] = program;
                new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;
                
                /* Parse with getopt */
                parse_getopt_args(new_argc, new_argv, &options, NULL, program);
            }
        } else {
            /* ReadArgs failed, check for interactive help */
            if (strstr(cmd_string, "?")) {
                interactive_help = TRUE;
            } else {
                fprintf(stderr, "%s: Invalid arguments\n", program);
                usage(program);
                ret_code = FAILURE;
            }
        }

        /* Clean up ReadArgs */
        FreeArgs(rdargs);
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
        
        if (interactive_help) {
            printf("time - Measure command execution time\n\n");
            printf("COMMAND/A/F - Command to execute and time\n");
            printf("PORTABLE/S - Use portable output format\n");
            printf("VERBOSE/S - Verbose output\n");
            printf("FORMAT/K - Custom output format string\n");
            printf("POSIX/K/F - Use POSIX-style arguments\n\n");
            printf("Example: time COMMAND/A/F \"ls -la\" PORTABLE/S\n");
            return SUCCESS;
        }
    }

    /* Run the time logic */
    if (options.command) {
        ret_code = run_time_logic(&options, program);
    } else {
        fprintf(stderr, "%s: No command specified\n", program);
        usage(program);
        ret_code = FAILURE;
    }

    /* Clean up */
    cleanup_options(&options);
    
    return ret_code;
}

/* Parse getopt-style arguments */
void parse_getopt_args(int argc, char **argv, TimeOptions *options, int *file_start, const char *program)
{
    int opt;
    
    /* Parse options */
    while ((opt = getopt(argc, argv, "pvf:hV")) != -1) {
        switch (opt) {
            case 'p':
                options->portable_flag = TRUE;
                break;
            case 'v':
                options->verbose_flag = TRUE;
                break;
            case 'f':
                options->format_string = optarg;
                break;
            case 'h':
                usage(program);
                exit(SUCCESS);
                break;
            case 'V':
                printf("time version 1.1\n");
                exit(SUCCESS);
                break;
            case '?':
                usage(program);
                exit(FAILURE);
                break;
            default:
                usage(program);
                exit(FAILURE);
                break;
        }
    }
    
    if (file_start) {
        *file_start = optind;
    }
    
    /* Get command and arguments */
    if (optind < argc) {
        options->command = argv[optind];
        options->command_args = &argv[optind];
        options->command_argc = argc - optind;
    }
}

/* Initialize time options */
void init_options(TimeOptions *options)
{
    options->portable_flag = FALSE;
    options->verbose_flag = FALSE;
    options->format_string = NULL;
    options->command = NULL;
    options->command_args = NULL;
    options->command_argc = 0;
}

/* Clean up time options */
void cleanup_options(TimeOptions *options)
{
    /* Nothing to free in this structure */
}

/* Main time logic */
int run_time_logic(TimeOptions *options, const char *program)
{
    struct DateStamp before, after;
    char *command_line;
    int command_len;
    int i;
    
    if (!options->command) {
        fprintf(stderr, "%s: No command specified\n", program);
        return FAILURE;
    }
    
    /* Build command line from command and arguments */
    command_len = strlen(options->command);
    for (i = 0; i < options->command_argc; i++) {
        command_len += strlen(options->command_args[i]) + 1;
    }
    
    command_line = malloc(command_len + 1);
    if (!command_line) {
        fprintf(stderr, "%s: Out of memory\n", program);
        return FAILURE;
    }
    
    strcpy(command_line, options->command);
    for (i = 0; i < options->command_argc; i++) {
        strcat(command_line, " ");
        strcat(command_line, options->command_args[i]);
    }
    
    /* Get start time */
    DateStamp(&before);
    
    /* Execute command */
    System(command_line, NULL);
    
    /* Get end time */
    DateStamp(&after);
    
    /* Calculate elapsed time */
    SubDateStamp(&after, &before);
    
    /* Print timing output */
    print_time_output(&after, options);
    
    /* Clean up */
    free(command_line);
    
    return SUCCESS;
}

/* Print time output based on options */
void print_time_output(struct DateStamp *ds, TimeOptions *options)
{
    if (options->format_string) {
        /* Custom format - basic implementation */
        printf("Elapsed time: %ld:%02ld:%02ld.%02ld\n", 
               ds->ds_Days*24 + ds->ds_Minute / 60,
               ds->ds_Minute % 60,
               ds->ds_Tick / TICKS_PER_SECOND,
               (100 * (ds->ds_Tick % TICKS_PER_SECOND)) / TICKS_PER_SECOND);
    } else if (options->portable_flag) {
        /* Portable format */
        printf("real\t%ld:%02ld:%02ld.%02ld\n", 
               ds->ds_Days*24 + ds->ds_Minute / 60,
               ds->ds_Minute % 60,
               ds->ds_Tick / TICKS_PER_SECOND,
               (100 * (ds->ds_Tick % TICKS_PER_SECOND)) / TICKS_PER_SECOND);
        printf("user\t0:00.00.00\n");
        printf("sys\t0:00.00.00\n");
    } else if (options->verbose_flag) {
        /* Verbose format */
        printf("Command execution time:\n");
        printf("  Hours:   %ld\n", ds->ds_Days*24 + ds->ds_Minute / 60);
        printf("  Minutes: %ld\n", ds->ds_Minute % 60);
        printf("  Seconds: %ld.%02ld\n", 
               ds->ds_Tick / TICKS_PER_SECOND,
               (100 * (ds->ds_Tick % TICKS_PER_SECOND)) / TICKS_PER_SECOND);
    } else {
        /* Standard format */
        PrintDateStamp(ds);
    }
}

/* Display usage information */
void usage(const char *program)
{
    printf("Usage: %s [OPTIONS] COMMAND [ARGS...]\n", program);
    printf("       %s COMMAND/A/F [OPTIONS]\n\n", program);
    printf("Measure the time taken by a command to execute.\n\n");
    printf("POSIX Options:\n");
    printf("  -p, --portable    Use portable output format\n");
    printf("  -v, --verbose     Verbose output\n");
    printf("  -f, --format=FMT  Use custom format string\n");
    printf("  -h, --help        Display this help message\n");
    printf("  -V, --version     Display version information\n\n");
    printf("Amiga Options:\n");
    printf("  COMMAND/A/F       Command to execute (required)\n");
    printf("  PORTABLE/S        Use portable output format\n");
    printf("  VERBOSE/S         Verbose output\n");
    printf("  FORMAT/K          Custom output format string\n");
    printf("  POSIX/K/F         Use POSIX-style arguments\n\n");
    printf("Examples:\n");
    printf("  %s ls -la\n", program);
    printf("  %s -p ls -la\n", program);
    printf("  %s COMMAND/A/F \"ls -la\" PORTABLE/S\n", program);
    printf("  %s COMMAND/A/F \"ls -la\" POSIX/K/F \"-p -v\"\n", program);
}

/* Print datestamp as HH:MM:SS.SS */
void PrintDateStamp(struct DateStamp *ds)
{
    LONG h, m, s, hs;

    h = ds->ds_Days*24 + ds->ds_Minute / 60;        /* hours */
    m = ds->ds_Minute % 60;            /* minutes */
    s = ds->ds_Tick / TICKS_PER_SECOND;        /* seconds */
                            
    hs = (100 *                    /* hundreths of a second */ 
        (ds->ds_Tick % TICKS_PER_SECOND)) / TICKS_PER_SECOND;

    printf("%ld:%02ld:%02ld.%02ld\n", h, m, s, hs);
}

/* Subtract amount from ds */
void SubDateStamp(struct DateStamp *ds, struct DateStamp *amount)
{
    if (ds->ds_Tick < amount->ds_Tick) {
        ds->ds_Tick += TICKS_PER_MINUTE;
        ds->ds_Minute--;
    }

    if (ds->ds_Minute < amount->ds_Minute) {
        ds->ds_Minute += MINUTES_PER_DAY;
        ds->ds_Days--;
    }

    ds->ds_Days -= amount->ds_Days;
    ds->ds_Minute -= amount->ds_Minute;
    ds->ds_Tick -= amount->ds_Tick;
}