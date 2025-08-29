/*
 * nice - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on Nice by Tak Tang.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <proto/dos.h>
#include <proto/exec.h>
#include <dos/dos.h>
#include <dos/dostags.h>
#include <dos/rdargs.h>
#include <dos/dosextens.h>
#include <exec/types.h>
#include <exec/execbase.h>
#include <exec/memory.h>

#include "nice.h"
#include "common.h"
#include "getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: nice 2.0 (29/08/25)\n";

/* For ReadArgs template */
enum {
    ARG_PRIORITY,
    ARG_ABSOLUTE,
    ARG_STICKY,
    ARG_COMMAND,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void usage(const char *program);
int run_nice_logic(int priority, int absolute, int sticky, char **command, const char *program);
void parse_getopt_args(int argc, char **argv, int *priority, int *absolute, int *sticky, int *file_start, const char *program);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    int priority = DEFAULT_PRIORITY_CHANGE;
    int absolute = FALSE;
    int sticky = FALSE;
    char **command = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "PRIORITY/N/K,ABSOLUTE/S,STICKY/S,COMMAND/F,POSIX/K/F";
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
        return FAILURE;
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &priority, &absolute, &sticky, &i, program);
        command = &argv[i];
        return run_nice_logic(priority, absolute, sticky, command, program);
        
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

                parse_getopt_args(new_argc, new_argv, &priority, &absolute, &sticky, &i, program);
                command = &new_argv[i];
                ret_code = run_nice_logic(priority, absolute, sticky, command, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_PRIORITY]) {
                    priority = *(LONG *)arg_array[ARG_PRIORITY];
                }
                if (arg_array[ARG_ABSOLUTE]) {
                    absolute = TRUE;
                }
                if (arg_array[ARG_STICKY]) {
                    sticky = TRUE;
                }
                
                if (arg_array[ARG_COMMAND]) {
                    command = (char **)arg_array[ARG_COMMAND];
                    ret_code = run_nice_logic(priority, absolute, sticky, command, program);
                } else {
                    /* No command specified, show usage */
                    usage(program);
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
 * @param priority Priority value (output)
 * @param absolute Absolute priority flag (output)
 * @param sticky Sticky priority flag (output)
 * @param file_start Index where command starts in argv (output)
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, int *priority, int *absolute, int *sticky, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "n:Vh")) != -1) {
        switch (c) {
            case 'n':
                /* Invert priority for POSIX compatibility: lower POSIX values = higher Amiga priority */
                *priority = -atoi(optarg);
                break;
            case 'V':
            case 'h':
                usage(program);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    *file_start = optind;
    
    /* Check if there's a command to run */
    if (*file_start >= argc) {
        fprintf(stderr, "%s: missing command\n", program);
        usage(program);
        exit(FAILURE);
    }
}

/**
 * @brief Core nice logic separated from argument parsing
 * @param priority Priority value
 * @param absolute Absolute priority flag
 * @param sticky Sticky priority flag
 * @param command Array of command and arguments
 * @param program Program name for error messages
 * @return Exit code
 */
int run_nice_logic(int priority, int absolute, int sticky, char **command, const char *program) {
    struct Task *task;
    int newpri, oldpri;
    int ret_code = SUCCESS;
    
    /* Get current task */
    task = FindTask(NULL);
    if (!task) {
        fprintf(stderr, "%s: cannot find current task\n", program);
        return FAILURE;
    }
    
    /* Calculate new priority */
    if (absolute) {
        newpri = priority;
    } else {
        newpri = task->tc_Node.ln_Pri + priority;
    }
    
    /* Clamp priority change to Amiga system guidelines */
    if (newpri < MIN_PRIORITY_CHANGE) newpri = MIN_PRIORITY_CHANGE;
    if (newpri > MAX_PRIORITY_CHANGE) newpri = MAX_PRIORITY_CHANGE;
    
    /* Set new priority */
    oldpri = SetTaskPri(task, newpri);
    
    /* Execute command if provided */
    if (command && command[0]) {
        /* Build command string */
        char *cmd_str = NULL;
        int i = 0;
        size_t total_len = 0;
        
        /* Calculate total length needed */
        while (command[i]) {
            total_len += strlen(command[i]) + 1; /* +1 for space */
            i++;
        }
        
        if (total_len > 0) {
            cmd_str = malloc(total_len);
            if (cmd_str) {
                cmd_str[0] = '\0';
                for (i = 0; command[i]; i++) {
                    if (i > 0) strcat(cmd_str, " ");
                    strcat(cmd_str, command[i]);
                }
                
                /* Execute command */
                ret_code = SystemTags(cmd_str, SYS_Input, Input(), SYS_Output, Output(), TAG_DONE);
                
                free(cmd_str);
            }
        }
    }
    
    /* Restore old priority unless sticky */
    if (!sticky) {
        SetTaskPri(task, oldpri);
    }
    
    return ret_code;
}

/*
 * Display usage information
 */
void usage(const char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] COMMAND [ARGS...]\n", program);
    fprintf(stderr, "Usage (Amiga): %s [PRIORITY/N] [ABSOLUTE/S] [STICKY/S] COMMAND/F\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -n N         add N to priority (default: -3)\n");
    fprintf(stderr, "  -h, -V       display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Run COMMAND with modified priority. Priority change range: -5 to +5.\n");
    fprintf(stderr, "  Default: reduce priority by 3. Use ABSOLUTE for absolute priority.\n");
    fprintf(stderr, "  Use STICKY to keep the new priority after command completes.\n");
    fprintf(stderr, "  Note: POSIX -n values are inverted in Amiga mode.\n");
    exit(FAILURE);
}

