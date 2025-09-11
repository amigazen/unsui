/*
 * pwd - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Inspired by Oberon pwd by Roland Jesse.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>

/* Amiga-specific includes */
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <libraries/dosextens.h>
#include <exec/memory.h>

#include <proto/dos.h>
#include <proto/utility.h>

#include "pwd.h"
#include "common.h"
#include "getopt.h"

/* Define snprintf if not available */
#ifndef snprintf
int snprintf(char *str, size_t size, const char *format, ...) {
    va_list args;
    int result;
    va_start(args, format);
    result = VSNPrintf(str, size, format, args);
    va_end(args);
    return result;
}
#endif

/* Version tag for Amiga */
static const char *verstag = "$VER: pwd 3.0 (11/09/25)\n";

extern struct DosLibrary *DOSBase;


/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_LOGICAL,
    ARG_PHYSICAL,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void usage(const char *program);
int run_pwd_logic(const char *program, int logical);
int parse_getopt_args(int argc, char **argv, int *logical, const char *program);
char *amiga_path_of_lock(struct FileLock *origl);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    char *program;
    char *template;
    LONG arg_array[ARG_COUNT];
    struct RDArgs *rdargs;
    char *cmd_string;
    int ret_code;
    BOOL interactive_help;
    char *posix_str;
    int new_argc;
    char *new_argv[MAX_TEMPLATE_ITEMS];
    int i;
    char initial_args_str[256];
    char user_input_buf[256];
    char *temp_str;
    size_t combined_len;
    int logical;
    
    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    if (argc == 1) {
        /* No arguments, just print current directory */
        return run_pwd_logic(program, 0);  /* Default to physical path */
    }

    /* Initialize ReadArgs Path Variables */
    template = "LOGICAL/S,PHYSICAL/S,POSIX/K/F";
    arg_array[ARG_LOGICAL] = 0;
    arg_array[ARG_PHYSICAL] = 0;
    arg_array[ARG_POSIX] = 0;
    rdargs = NULL;
    cmd_string = NULL;
    ret_code = SUCCESS;
    interactive_help = FALSE;
    posix_str = NULL;
    new_argc = 0;
    i = 0;
    initial_args_str[0] = '\0';
    user_input_buf[0] = '\0';
    temp_str = NULL;
    combined_len = 0;

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        logical = 0;
        if (parse_getopt_args(argc, argv, &logical, program) == SUCCESS) {
            return run_pwd_logic(program, logical);
        }
        return FAILURE;
        
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
            /* Interactive help mode - output ReadArgs template */
            printf("%s: ", template);
            fflush(stdout);
            
            if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                /* Remove newline */
                user_input_buf[strcspn(user_input_buf, "\n")] = '\0';
                
                if (strlen(user_input_buf) > 0) {
                    /* Build command string with user input */
                    cmd_string = malloc(strlen(program) + strlen(user_input_buf) + 4);
                    if (cmd_string) {
                        snprintf(cmd_string, strlen(program) + strlen(user_input_buf) + 4, "%s %s\n", program, user_input_buf);
                    }
                } else {
                    /* Empty input, use default */
                    cmd_string = strdup("pwd\n");
                }
            } else {
                cmd_string = strdup("pwd\n");
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
            /* Check for POSIX override first */
            if (arg_array[ARG_POSIX]) {
                posix_str = (char *)arg_array[ARG_POSIX];

                /* Tokenize the string and build a new argv for getopt */
                new_argv[0] = program;
                new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

                logical = 0;
                if (parse_getopt_args(new_argc, new_argv, &logical, program) == SUCCESS) {
                    ret_code = run_pwd_logic(program, logical);
                } else {
                    ret_code = FAILURE;
                }

            } else {
                /* Standard ReadArgs processing - check for LOGICAL/PHYSICAL switches */
                logical = 0;  /* Default to physical path */
                
                if (arg_array[ARG_LOGICAL]) {
                    logical = 1;  /* LOGICAL switch specified */
                } else if (arg_array[ARG_PHYSICAL]) {
                    logical = 0;  /* PHYSICAL switch specified */
                }
                /* If neither specified, default to physical (logical = 0) */
                
                ret_code = run_pwd_logic(program, logical);
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
 * @param program Program name for error messages
 */
int parse_getopt_args(int argc, char **argv, int *logical, const char *program) {
    int c;
    int help_requested = 0;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "hVLP")) != -1) {
        switch (c) {
            case 'h':
            case 'V':
                help_requested = 1;
                break;
            case 'L':
                *logical = 1;  /* Use logical path (PWD environment variable) */
                break;
            case 'P':
                *logical = 0;  /* Use physical path (resolve symlinks) */
                break;
            case '?':
                return FAILURE;
        }
    }
    
    /* If help was requested, show it and exit */
    if (help_requested) {
        usage(program);
    }
    
    return SUCCESS;
}

/**
 * @brief Core pwd logic separated from argument parsing
 * Uses native Amiga path resolution for better compatibility
 * @param program Program name for error messages
 * @param logical If 1, use PWD environment variable (logical path)
 *                If 0, resolve symlinks (physical path)
 * @return Exit code
 */
int run_pwd_logic(const char *program, int logical) {
    struct FileLock *lock;
    char *path;
    char *env_pwd;
    
    /* Check for logical path (-L option) */
    if (logical) {
        env_pwd = getenv("PWD");
        if (env_pwd && env_pwd[0] != '\0') {
            printf("%s\n", env_pwd);
            fflush(stdout);
            return SUCCESS;
        }
        /* Fall through to physical path if PWD not set */
    }
    
    /* Use native Amiga path resolution for physical path (-P option) */
    lock = (struct FileLock *)Lock("", ACCESS_READ);
    if (!lock) {
        /* Fallback to POSIX getcwd if Amiga method fails */
        char cwd[PATH_MAX];
        if (getcwd(cwd, sizeof(cwd)) == NULL) {
            fprintf(stderr, "%s: cannot get current directory\n", program);
            return FAILURE;
        }
        printf("%s\n", cwd);
    } else {
        path = amiga_path_of_lock(lock);
        printf("%s\n", path);
        UnLock((BPTR)lock);
    }
    
    fflush(stdout);
    return SUCCESS;
}

/**
 * @brief Walk up the ParentDir tree, building the full Amiga path
 * Based on AmigaPerl pwd.c implementation with enhancements
 * @param origl Original FileLock to start from
 * @return Static string containing the full path
 */
char *amiga_path_of_lock(struct FileLock *origl) {
    static char tmp[256], path[256];
    struct FileInfoBlock *fib;
    struct FileLock *l, *p = origl;
    
    path[0] = '\0';
    fib = (struct FileInfoBlock *)malloc(sizeof(*fib));
    if (!fib) {
        strcpy(path, "ERROR: Out of memory");
        return path;
    }
    
    while (l = p) {
        Examine((BPTR)l, fib);
        if (!*(fib->fib_FileName)) {
            strcpy(tmp, "RAM");        /* If null name, assume RAM disk */
        } else {
            strcpy(tmp, (char *)(fib->fib_FileName));
        }
        
        if (!(p = (struct FileLock *)ParentDir((BPTR)l))) {
            strcat(tmp, ":");          /* Root directory gets colon */
        } else if (path[0]) {
            strcat(tmp, "/");          /* Subdirectories get slash */
        }
        
        if (l != origl) {
            UnLock((BPTR)l);           /* Clean up intermediate locks */
        }
        
        strcat(tmp, path);
        strcpy(path, tmp);
    }
    
    free(fib);
    return path;
}

/*
 * Display usage information
 */
void usage(const char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS]\n", program);
    fprintf(stderr, "Usage (Amiga): %s [LOGICAL/S] [PHYSICAL/S] [POSIX/K/F]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -L          use PWD from environment, even if it contains symlinks\n");
    fprintf(stderr, "  -P          avoid all symlinks (default)\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "AMIGA SWITCHES:\n");
    fprintf(stderr, "  LOGICAL     use PWD from environment (same as -L)\n");
    fprintf(stderr, "  PHYSICAL    avoid all symlinks (same as -P, default)\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Print the name of the current working directory.\n");
    fprintf(stderr, "  If no option is specified, -P is assumed.\n");
	exit(FAILURE);
}
