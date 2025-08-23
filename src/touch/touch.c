/*
 * touch - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on touch by Dave Schreiber.
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
#include <proto/dos.h>
#include <proto/exec.h>
#include <dos/rdargs.h>

#include "touch.h"
#include "/common/common.h"
#include "/common/getopt.h"

extern struct DosLibrary *DOSBase;

/* Version tag for Amiga */
static const char *verstag = "$VER: touch 1.0 (23/08/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_FILE,
    ARG_ACCESS,
    ARG_MODIFY,
    ARG_CREATE,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void usage(const char *program);
int run_touch_logic(int access_flag, int modify_flag, int create_flag, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, int *access_flag, int *modify_flag, int *create_flag, int *file_start, const char *program);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    int access_flag = FALSE;
    int modify_flag = FALSE;
    int create_flag = TRUE;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILE/M,ACCESS/S,MODIFY/S,CREATE/S,POSIX/K/F";
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
        /* No arguments, show usage */
        usage(program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &access_flag, &modify_flag, &create_flag, &file_start, program);
        files = &argv[file_start];
        return run_touch_logic(access_flag, modify_flag, create_flag, argc - file_start, files, program);
        
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
            FreeDosObject(DOS_RDARGS, rdargs);
            free(cmd_string);
            return FAILURE;
        }

        /* Check for POSIX/F override first */
        if (arg_array[ARG_POSIX]) {
            posix_str = (char *)arg_array[ARG_POSIX];

            /* Tokenize the string and build a new argv for getopt */
            new_argv[0] = program;
            new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

            parse_getopt_args(new_argc, new_argv, &access_flag, &modify_flag, &create_flag, &file_start, program);
            files = &new_argv[file_start];
            ret_code = run_touch_logic(access_flag, modify_flag, create_flag, new_argc - file_start, files, program);

        } else {
            /* Standard ReadArgs processing */
            if (arg_array[ARG_ACCESS]) {
                access_flag = TRUE;
            }
            if (arg_array[ARG_MODIFY]) {
                modify_flag = TRUE;
            }
            if (arg_array[ARG_CREATE]) {
                create_flag = TRUE;
            }
            
            /* If no specific time flags, update both access and modification */
            if (!access_flag && !modify_flag) {
                access_flag = TRUE;
                modify_flag = TRUE;
            }
            
            if (arg_array[ARG_FILE]) {
                /* Count files and allocate array */
                file_count = 0;
                while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
                    file_count++;
                }
                
                files = (char **)arg_array[ARG_FILE];
                ret_code = run_touch_logic(access_flag, modify_flag, create_flag, file_count, files, program);
            } else {
                fprintf(stderr, "%s: FILE argument required\n", program);
                ret_code = FAILURE;
            }
        }

        /* Clean up */
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
        
        return ret_code;
    }
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param access_flag Flag to update access time
 * @param modify_flag Flag to update modification time
 * @param create_flag Flag to create files
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, int *access_flag, int *modify_flag, int *create_flag, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "acmhV")) != -1) {
        switch (c) {
            case 'a':
                *access_flag = TRUE;
                break;
            case 'c':
                *create_flag = FALSE;
                break;
            case 'm':
                *modify_flag = TRUE;
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
    
    /* If no specific time flags, update both access and modification */
    if (!*access_flag && !*modify_flag) {
        *access_flag = TRUE;
        *modify_flag = TRUE;
    }
}

/**
 * @brief Core touch logic separated from argument parsing
 * @param access_flag Flag to update access time
 * @param modify_flag Flag to update modification time
 * @param create_flag Flag to create files
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_touch_logic(int access_flag, int modify_flag, int create_flag, int file_count, char **files, const char *program) {
    int i;
    
    if (file_count == 0) {
        fprintf(stderr, "%s: FILE argument required\n", program);
        return FAILURE;
    }

    /* Process files */
    for (i = 0; i < file_count; i++) {
        touch_file(files[i], access_flag, modify_flag, create_flag);
    }
    
    return SUCCESS;
}

/*
 * Touch a single file
 * Updates timestamps and creates file if needed
 */
void touch_file(char *filename, int access_flag, int modify_flag, int create_flag)
{
	BPTR file;
	struct DateStamp currentDate;
	BPTR origDir;
	char temp[514];

	/* Try to open existing file for timestamp update */
	file = Open(filename, MODE_READWRITE);
	if (file != NULL) {
		/* File exists - update timestamps */
		origDir = CurrentDir(file);
		DateStamp(&currentDate);
		
		if (access_flag) {
			/* Update access time */
			SetFileDate(filename, &currentDate);
		}
		
		if (modify_flag) {
			/* Update modification time */
			SetFileDate(filename, &currentDate);
		}
		
		CurrentDir(origDir);
		Close(file);
	} else {
		/* File doesn't exist */
		if (create_flag) {
			/* Check if it's a valid filename (not a pattern) */
			if (ParsePatternNoCase(filename, temp, sizeof(temp)) == 0) {
				/* Create empty file */
				file = Open(filename, MODE_NEWFILE);
				if (file != NULL) {
					Close(file);
				} else {
					fprintf(stderr, "touch: cannot create %s\n", filename);
				}
			} else {
				fprintf(stderr, "touch: cannot create %s (pattern)\n", filename);
			}
		} else {
			fprintf(stderr, "touch: cannot touch %s (no such file)\n", filename);
		}
	}
}

/*
 * Display usage information
 */
void usage(const char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] FILE...\n", program);
    fprintf(stderr, "Usage (Amiga): %s FILE/M [ACCESS/S] [MODIFY/S] [CREATE/S]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -a          change only the access time\n");
    fprintf(stderr, "  -c          do not create any files\n");
    fprintf(stderr, "  -m          change only the modification time\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Update the access and modification times of each FILE to the current time.\n");
    fprintf(stderr, "  A FILE argument that does not exist is created empty.\n");
	exit(FAILURE);
}
