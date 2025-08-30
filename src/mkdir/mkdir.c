/*
 * mkdir - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on mkdir by Georg Hessmann (hessmann@fmi.uni-passau.de)
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing, providing both POSIX compatibility and
 * Amiga native functionality.
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
#include <dos/dosasl.h>
#include <dos/rdargs.h>
#include <workbench/workbench.h>
#include <exec/memory.h>
#include <dos/dosextens.h>

#include <proto/dos.h>
#include <proto/exec.h>
#include <proto/icon.h>

#include "common.h"
#include "getopt.h"

extern struct DosLibrary *DOSBase;

/* Version tag for Amiga */
static const char *verstag = "$VER: mkdir 1.1 (23/08/25)\n";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_DIRECTORY,
    ARG_PARENTS,
    ARG_MODE,
    ARG_VERBOSE,
    ARG_ICON,
    ARG_NOICON,
    ARG_POSIX,
    ARG_COUNT
};

/* Global options structure */
typedef struct {
    BOOL parents_flag;          /* -p: create parent directories */
    BOOL verbose_flag;          /* -v: verbose output */
    BOOL icon_flag;             /* Create .info files */
    char *mode_string;          /* -m: mode string */
    int exit_code;              /* Exit code */
} MkdirOptions;

/* Function declarations for forward references */
void usage(const char *program);
void print_version(const char *program);
int run_mkdir_logic(MkdirOptions *options, int dir_count, char **directories, const char *program);
void parse_getopt_args(int argc, char **argv, MkdirOptions *options, int *dir_start, const char *program);
void init_options(MkdirOptions *options);
void cleanup_options(MkdirOptions *options);

/* Static function declarations */
static int create_directory(const char *dirname, MkdirOptions *options, const char *program);
static int create_parent_directories(const char *full_path, MkdirOptions *options, const char *program);
static BOOL create_directory_icon(const char *dirname, const char *program);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    MkdirOptions options;
    int dir_start = 1;
    char **directories = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "DIRECTORY/M,PARENTS/S,MODE/K,VERBOSE/S,ICON/S,NOICON/S,POSIX/K/F";
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
    int dir_count;
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

    /* Initialize options */
    init_options(&options);

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &options, &dir_start, program);
        directories = &argv[dir_start];
        dir_count = argc - dir_start;
        
        if (dir_count == 0) {
            fprintf(stderr, "%s: missing operand\n", program);
            usage(program);
            cleanup_options(&options);
            return FAILURE;
        }
        
        return run_mkdir_logic(&options, dir_count, directories, program);
        
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
            cleanup_options(&options);
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
            cleanup_options(&options);
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
            cleanup_options(&options);
            return FAILURE;
        }

        /* Check for POSIX/F override first */
        if (arg_array[ARG_POSIX]) {
            posix_str = (char *)arg_array[ARG_POSIX];

            /* Tokenize the string and build a new argv for getopt */
            new_argv[0] = program;
            new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

            parse_getopt_args(new_argc, new_argv, &options, &dir_start, program);
            directories = &new_argv[dir_start];
            dir_count = new_argc - dir_start;
            
            if (dir_count == 0) {
                fprintf(stderr, "%s: missing operand\n", program);
                usage(program);
                cleanup_readargs(rdargs, cmd_string);
                cleanup_options(&options);
                return FAILURE;
            }
            
            ret_code = run_mkdir_logic(&options, dir_count, directories, program);

        } else {
            /* Standard ReadArgs processing */
            if (arg_array[ARG_PARENTS]) {
                options.parents_flag = TRUE;
            }
            if (arg_array[ARG_VERBOSE]) {
                options.verbose_flag = TRUE;
            }
            if (arg_array[ARG_MODE]) {
                options.mode_string = strdup((char *)arg_array[ARG_MODE]);
            }
            if (arg_array[ARG_ICON]) {
                options.icon_flag = TRUE;
            }
            if (arg_array[ARG_NOICON]) {
                options.icon_flag = FALSE;
            }
            
            /* Get directories from ReadArgs */
            if (arg_array[ARG_DIRECTORY]) {
                /* Count directories and allocate array */
                dir_count = 0;
                while (((char **)arg_array[ARG_DIRECTORY])[dir_count] != NULL) {
                    dir_count++;
                }
                
                directories = (char **)arg_array[ARG_DIRECTORY];
                ret_code = run_mkdir_logic(&options, dir_count, directories, program);
            } else {
                /* No directories specified */
                fprintf(stderr, "%s: missing operand\n", program);
                usage(program);
                ret_code = FAILURE;
            }
        }

        /* Clean up ReadArgs */
        cleanup_readargs(rdargs, cmd_string);
    }
    
    cleanup_options(&options);
    return ret_code;
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param options Options structure to populate
 * @param dir_start Index where directories start
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, MkdirOptions *options, int *dir_start, const char *program)
{
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "pm:vVh")) != -1) {
        switch (c) {
            case 'p':
                options->parents_flag = TRUE;
                break;
            case 'm':
                if (optarg) {
                    options->mode_string = strdup(optarg);
                }
                break;
            case 'v':
                options->verbose_flag = TRUE;
                break;
            case 'V':
                print_version(program);
                exit(SUCCESS);
                break;
            case 'h':
                usage(program);
                exit(SUCCESS);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    /* Update dir_start to point to remaining arguments */
    *dir_start = optind;
}

/**
 * @brief Initialize options structure
 * @param options Options structure to initialize
 */
void init_options(MkdirOptions *options)
{
    options->parents_flag = FALSE;
    options->verbose_flag = FALSE;
    options->icon_flag = TRUE;  /* Default to creating icons on Amiga */
    options->mode_string = NULL;
    options->exit_code = SUCCESS;
}

/**
 * @brief Clean up options structure
 * @param options Options structure to clean up
 */
void cleanup_options(MkdirOptions *options)
{
    if (options->mode_string) {
        free(options->mode_string);
        options->mode_string = NULL;
    }
}

/**
 * @brief Print usage information
 * @param program Program name
 */
void usage(const char *program)
{
    fprintf(stderr, 
        "Usage: %s [OPTION]... DIRECTORY...\n"
        "Create the DIRECTORY(ies), if they do not already exist.\n\n"
        "Mandatory arguments to long options are mandatory for short options too.\n"
        "  -m, --mode=MODE   set file permissions (as in chmod), not a=rwx - umask\n"
        "  -p, --parents     no error if existing, make parent directories as needed\n"
        "  -v, --verbose     print a message for each created directory\n"
        "  -V, --version     output version information and exit\n"
        "  -h, --help        display this help and exit\n\n"
        "Amiga-specific options:\n"
        "  ICON              create .info files for directories\n"
        "  NOICON            do not create .info files\n\n"
        "Examples:\n"
        "  %s -p /tmp/a/b/c    Create directory /tmp/a/b/c with parents\n"
        "  %s -m 755 dir1      Create dir1 with permissions 755\n",
        program, program, program);
}

/**
 * @brief Print version information
 * @param program Program name
 */
void print_version(const char *program)
{
    printf("%s version 1.1\n", program);
}

/**
 * @brief Main mkdir logic - creates directories based on options
 * @param options Options structure
 * @param dir_count Number of directories to create
 * @param directories Array of directory names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_mkdir_logic(MkdirOptions *options, int dir_count, char **directories, const char *program)
{
    int i;
    int ret_code = SUCCESS;
    
    for (i = 0; i < dir_count; i++) {
        if (create_directory(directories[i], options, program) != SUCCESS) {
            ret_code = FAILURE;
        }
    }
    
    return ret_code;
}

/**
 * @brief Create a single directory
 * @param dirname Directory name to create
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int create_directory(const char *dirname, MkdirOptions *options, const char *program)
{
    BPTR lock;
    int rc = SUCCESS;
    
    /* Check if directory already exists */
    lock = Lock(dirname, ACCESS_READ);
    if (lock) {
        /* Directory exists, verify it's actually a directory */
        struct FileInfoBlock fib;
        if (Examine(lock, &fib)) {
            if (fib.fib_DirEntryType > 0) {
                /* It's a directory */
                if (options->verbose_flag) {
                    printf("%s: directory '%s' already exists\n", program, dirname);
                }
                UnLock(lock);
                return SUCCESS;
            } else {
                /* Path exists but isn't a directory */
                fprintf(stderr, "%s: '%s' exists but is not a directory\n", program, dirname);
                SetIoErr(ERROR_OBJECT_WRONG_TYPE);
                PrintFault(IoErr(), dirname);
                UnLock(lock);
                return FAILURE;
            }
        } else {
            /* Examine failed */
            PrintFault(IoErr(), dirname);
            UnLock(lock);
            return FAILURE;
        }
    }
    
    /* If parents flag is set, create parent directories */
    if (options->parents_flag) {
        rc = create_parent_directories(dirname, options, program);
        if (rc != SUCCESS) {
            return rc;
        }
    }
    
    /* Create the directory */
    lock = CreateDir(dirname);
    if (lock) {
        if (options->verbose_flag) {
            printf("%s: created directory '%s'\n", program, dirname);
        }
        
        /* Create .info file if requested */
        if (options->icon_flag) {
            if (!create_directory_icon(dirname, program)) {
                /* Icon creation failed, but directory was created */
                fprintf(stderr, "%s: warning: failed to create icon for '%s'\n", program, dirname);
            }
        }
        
        UnLock(lock);
    } else {
        /* CreateDir failed, check the specific error */
        if (IoErr() == ERROR_OBJECT_NOT_FOUND) {
            fprintf(stderr, "%s: cannot create directory '%s': parent directory does not exist\n", program, dirname);
        } else if (IoErr() == ERROR_DISK_WRITE_PROTECTED) {
            fprintf(stderr, "%s: cannot create directory '%s': disk is write protected\n", program, dirname);
        } else if (IoErr() == ERROR_DISK_FULL) {
            fprintf(stderr, "%s: cannot create directory '%s': disk is full\n", program, dirname);
        } else {
            fprintf(stderr, "%s: cannot create directory '%s'\n", program, dirname);
        }
        PrintFault(IoErr(), dirname);
        rc = FAILURE;
    }
    
    return rc;
}

/**
 * @brief Create parent directories for a path
 * @param full_path Full path to create
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int create_parent_directories(const char *full_path, MkdirOptions *options, const char *program)
{
    char *buffer;
    BPTR lock;
    int rc = SUCCESS;
    int p;
    
    /* Allocate a temporary buffer to hold the path */
    buffer = AllocVec(strlen(full_path) + 1, MEMF_ANY);
    if (!buffer) {
        fprintf(stderr, "%s: out of memory\n", program);
        return FAILURE;
    }
    
    /* Iterate through the buffer and stop between path components */
    for (p = 0; ; p++) {
        if (full_path[p] == '/' || full_path[p] == '\0') {
            /* Temporary NULL termination */
            buffer[p] = '\0';
            
            /* Skip empty path components (like // or device: only) */
            if (strlen(buffer) == 0 || 
                (strchr(buffer, ':') && strlen(buffer) == 1)) {
                if (full_path[p] == '\0') break;
                continue;
            }
            
            /* Check if this directory exists */
            lock = Lock(buffer, ACCESS_READ);
            if (lock) {
                /* Directory exists, verify it's actually a directory */
                struct FileInfoBlock fib;
                if (Examine(lock, &fib)) {
                    if (fib.fib_DirEntryType > 0) {
                        /* It's a directory, continue to next component */
                        UnLock(lock);
                    } else {
                        /* Path exists but isn't a directory */
                        fprintf(stderr, "%s: '%s' exists but is not a directory\n", program, buffer);
                        SetIoErr(ERROR_OBJECT_WRONG_TYPE);
                        PrintFault(IoErr(), buffer);
                        UnLock(lock);
                        rc = FAILURE;
                        break;
                    }
                } else {
                    /* Examine failed */
                    PrintFault(IoErr(), buffer);
                    UnLock(lock);
                    rc = FAILURE;
                    break;
                }
            } else {
                /* Directory doesn't exist, check why */
                if (IoErr() == ERROR_OBJECT_NOT_FOUND) {
                    /* Create the directory */
                    lock = CreateDir(buffer);
                    if (lock) {
                        if (options->verbose_flag) {
                            printf("%s: created directory '%s'\n", program, buffer);
                        }
                        
                        /* Create .info file if requested */
                        if (options->icon_flag) {
                            if (!create_directory_icon(buffer, program)) {
                                fprintf(stderr, "%s: warning: failed to create icon for '%s'\n", program, buffer);
                            }
                        }
                        
                        UnLock(lock);
                    } else {
                        /* CreateDir failed */
                        fprintf(stderr, "%s: cannot create directory '%s'\n", program, buffer);
                        PrintFault(IoErr(), buffer);
                        rc = FAILURE;
                        break;
                    }
                } else {
                    /* Other error (permission denied, etc.) */
                    fprintf(stderr, "%s: cannot access '%s'\n", program, buffer);
                    PrintFault(IoErr(), buffer);
                    rc = FAILURE;
                    break;
                }
            }
            
            if (full_path[p] == '\0') {
                /* Reached the end of the string */
                break;
            }
        }
        
        /* Copy character to work buffer */
        buffer[p] = full_path[p];
    }
    
    FreeVec(buffer);
    return rc;
}

/**
 * @brief Create a .info file for a directory
 * @param dirname Directory name
 * @param program Program name for error messages
 * @return TRUE if successful, FALSE otherwise
 */
static BOOL create_directory_icon(const char *dirname, const char *program)
{
    struct DiskObject *disk_obj;
    BOOL success = FALSE;
    
    disk_obj = GetDefDiskObject(WBDRAWER);
    if (disk_obj) {
        if (PutDiskObject(dirname, disk_obj)) {
            success = TRUE;
        } else {
            /* Icon creation failed, check specific error */
            if (IoErr() == ERROR_DISK_WRITE_PROTECTED) {
                fprintf(stderr, "%s: warning: cannot create icon for '%s': disk is write protected\n", program, dirname);
            } else if (IoErr() == ERROR_DISK_FULL) {
                fprintf(stderr, "%s: warning: cannot create icon for '%s': disk is full\n", program, dirname);
            } else {
                fprintf(stderr, "%s: warning: cannot create icon for '%s'\n", program, dirname);
                PrintFault(IoErr(), dirname);
            }
        }
        FreeDiskObject(disk_obj);
    } else {
        fprintf(stderr, "%s: warning: cannot get default drawer icon\n", program);
    }
    
    return success;
}
