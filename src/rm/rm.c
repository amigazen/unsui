/*
 * rm - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing, providing both POSIX compatibility and
 * Amiga native functionality using dos.library DeleteFile API.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>
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
#include <proto/utility.h>

#include "common.h"
#include "getopt.h"

extern struct DosLibrary *DOSBase;

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

/* External function declarations from common library */
extern char *my_basename(char *path);
extern int is_getopt_style(int argc, char **argv);
extern char *build_command_string(int argc, char **argv, const char *exclude);
extern int tokenize_string(char *str, char **argv, int max_args);
extern void reset_getopt(void);
extern int getopt(int argc, char * const argv[], const char *optstring);
extern char *optarg;
extern int optind;
extern char **wildexpand(char *w);
extern void wildfree(char **freelist);
extern int amigaizepath(char *to);

/* Version tag for Amiga */
static const char *verstag = "rm 1.1 (10/09/25)\n";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_FILES,
    ARG_FORCE,
    ARG_INTERACTIVE,
    ARG_RECURSIVE,
    ARG_VERBOSE,
    ARG_POSIX,
    ARG_COUNT
};

/* Global options structure */
typedef struct {
    BOOL force_flag;            /* -f: force deletion without prompting */
    BOOL interactive_flag;      /* -i: prompt before each deletion */
    BOOL recursive_flag;        /* -r: remove directories recursively */
    BOOL verbose_flag;          /* -v: verbose output */
    int exit_code;              /* Exit code */
} RmOptions;

/* Function declarations for forward references */
void usage(const char *program);
void print_version(const char *program);
int run_rm_logic(RmOptions *options, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, RmOptions *options, int *file_start, const char *program);
void init_options(RmOptions *options);
void cleanup_options(RmOptions *options);

/* Static function declarations */
static int delete_file(const char *filename, RmOptions *options, const char *program);
static int delete_directory_recursive(const char *dirname, RmOptions *options, const char *program);
static BOOL confirm_deletion(const char *filename, const char *program);
static void print_error(const char *program, const char *filename, LONG error_code);
static BOOL check_filename_safety(const char *filename, const char *program);
static int process_wildcard_pattern(const char *pattern, RmOptions *options, const char *program);
static int sanitize_path(char *path, const char *program);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    RmOptions options;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILES/M,FORCE/S,INTERACTIVE/S,RECURSIVE/S,VERBOSE/S,POSIX/K/F";
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
        parse_getopt_args(argc, argv, &options, &file_start, program);
        files = &argv[file_start];
        file_count = argc - file_start;
        
        if (file_count == 0) {
            fprintf(stderr, "%s: missing operand\n", program);
            usage(program);
            cleanup_options(&options);
            return FAILURE;
        }
        
        return run_rm_logic(&options, file_count, files, program);
        
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
            /* Interactive help mode - output ReadArgs template */
            printf("FORCE/S,INTERACTIVE/S,RECURSIVE/S,VERBOSE/S,POSIX/K/F");
            
            if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                /* Remove newline */
                user_input_buf[strcspn(user_input_buf, "\n")] = '\0';
                
                if (strlen(user_input_buf) > 0) {
                    /* Process POSIX arguments */
                    posix_str = user_input_buf;
                    
                    /* Build initial arguments string */
                    snprintf(initial_args_str, sizeof(initial_args_str), "%s %s", program, posix_str);
                    
                    /* Tokenize the string */
                    new_argc = tokenize_string(initial_args_str, new_argv, MAX_TEMPLATE_ITEMS);
                    
                    if (new_argc > 1) {
                        /* Parse as POSIX arguments */
                        parse_getopt_args(new_argc, new_argv, &options, &file_start, program);
                        files = &new_argv[file_start];
                        file_count = new_argc - file_start;
                        
                        if (file_count > 0) {
                            ret_code = run_rm_logic(&options, file_count, files, program);
                        } else {
                            fprintf(stderr, "%s: missing operand\n", program);
                            usage(program);
                            ret_code = FAILURE;
                        }
                    } else {
                        usage(program);
                        ret_code = FAILURE;
                    }
                } else {
                    /* Use Amiga ReadArgs style */
                    cmd_string = build_command_string(argc, argv, "?");
                    if (!cmd_string) {
                        fprintf(stderr, "%s: out of memory\n", program);
                        cleanup_options(&options);
                        FreeDosObject(DOS_RDARGS, rdargs);
                        return FAILURE;
                    }
                    
                    rdargs->RDA_Source.CS_Buffer = cmd_string;
                    rdargs->RDA_Source.CS_Length = strlen(cmd_string);
                    
                    if (ReadArgs(template, arg_array, rdargs)) {
                        /* Process ReadArgs results */
                        if (arg_array[ARG_FORCE]) options.force_flag = TRUE;
                        if (arg_array[ARG_INTERACTIVE]) options.interactive_flag = TRUE;
                        if (arg_array[ARG_RECURSIVE]) options.recursive_flag = TRUE;
                        if (arg_array[ARG_VERBOSE]) options.verbose_flag = TRUE;
                        
                        if (arg_array[ARG_FILES]) {
                            files = (char **)arg_array[ARG_FILES];
                            file_count = 1; /* ReadArgs gives us one file at a time */
                            
                            ret_code = run_rm_logic(&options, file_count, files, program);
                        } else {
                            fprintf(stderr, "%s: missing operand\n", program);
                            usage(program);
                            ret_code = FAILURE;
                        }
                    } else {
                        fprintf(stderr, "%s: invalid arguments\n", program);
                        usage(program);
                        ret_code = FAILURE;
                    }
                }
            } else {
                usage(program);
                ret_code = FAILURE;
            }
        } else {
            /* Standard ReadArgs processing */
            cmd_string = build_command_string(argc, argv, NULL);
            if (!cmd_string) {
                fprintf(stderr, "%s: out of memory\n", program);
                cleanup_options(&options);
                FreeDosObject(DOS_RDARGS, rdargs);
                return FAILURE;
            }
            
            rdargs->RDA_Source.CS_Buffer = cmd_string;
            rdargs->RDA_Source.CS_Length = strlen(cmd_string);
            
            if (ReadArgs(template, arg_array, rdargs)) {
                /* Process ReadArgs results */
                if (arg_array[ARG_FORCE]) options.force_flag = TRUE;
                if (arg_array[ARG_INTERACTIVE]) options.interactive_flag = TRUE;
                if (arg_array[ARG_RECURSIVE]) options.recursive_flag = TRUE;
                if (arg_array[ARG_VERBOSE]) options.verbose_flag = TRUE;
                
                if (arg_array[ARG_FILES]) {
                    files = (char **)arg_array[ARG_FILES];
                    file_count = 1; /* ReadArgs gives us one file at a time */
                    
                    ret_code = run_rm_logic(&options, file_count, files, program);
                } else {
                    fprintf(stderr, "%s: missing operand\n", program);
                    usage(program);
                    ret_code = FAILURE;
                }
            } else {
                fprintf(stderr, "%s: invalid arguments\n", program);
                usage(program);
                ret_code = FAILURE;
            }
        }

        /* Clean up */
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
    }

    cleanup_options(&options);
    return ret_code;
}

/**
 * @brief Parse arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param options Options structure to populate
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, RmOptions *options, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "firvVh")) != -1) {
        switch (c) {
            case 'f':
                options->force_flag = TRUE;
                break;
            case 'i':
                options->interactive_flag = TRUE;
                break;
            case 'r':
                options->recursive_flag = TRUE;
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
    
    *file_start = optind;
}

/**
 * @brief Core rm logic separated from argument parsing
 * @param options Options structure
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_rm_logic(RmOptions *options, int file_count, char **files, const char *program) {
	int i;
    int result = SUCCESS;
    char *pattern;
    
    for (i = 0; i < file_count; i++) {
        if (files[i] && strlen(files[i]) > 0) {
            /* Make a copy for path sanitization */
            pattern = strdup(files[i]);
            if (!pattern) {
                fprintf(stderr, "%s: out of memory\n", program);
                result = FAILURE;
                continue;
            }
            
            /* Sanitize the path */
            if (sanitize_path(pattern, program) != SUCCESS) {
                free(pattern);
                result = FAILURE;
                continue;
            }
            
            /* Convert Unix-style paths to Amiga format */
            amigaizepath(pattern);
            
            /* Process the pattern (handles wildcards) */
            if (process_wildcard_pattern(pattern, options, program) != SUCCESS) {
                result = FAILURE;
            }
            
            free(pattern);
        }
    }
    
    return result;
}

/**
 * @brief Initialize options structure
 * @param options Options structure to initialize
 */
void init_options(RmOptions *options) {
    options->force_flag = FALSE;
    options->interactive_flag = FALSE;
    options->recursive_flag = FALSE;
    options->verbose_flag = FALSE;
    options->exit_code = SUCCESS;
}

/**
 * @brief Clean up options structure
 * @param options Options structure to clean up
 */
void cleanup_options(RmOptions *options) {
    /* Nothing to clean up for this structure */
    (void)options;
}

/**
 * @brief Delete a file or directory using Amiga native DeleteFile API
 * @param filename Name of file/directory to delete
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int delete_file(const char *filename, RmOptions *options, const char *program) {
    BPTR lock;
    struct FileInfoBlock fib;
    LONG result;
    BOOL is_directory = FALSE;
    
    /* Check if file exists and get information */
    lock = Lock((STRPTR)filename, ACCESS_READ);
    if (lock) {
        if (Examine(lock, &fib)) {
            is_directory = (fib.fib_DirEntryType > 0);
        }
        UnLock(lock);
    } else {
        /* File doesn't exist */
        if (!options->force_flag) {
            fprintf(stderr, "%s: cannot remove '%s': No such file or directory\n", program, filename);
            return FAILURE;
        }
        return SUCCESS; /* -f flag: ignore missing files */
    }
    
    /* Handle directories */
    if (is_directory) {
        if (!options->recursive_flag) {
            fprintf(stderr, "%s: cannot remove '%s': Is a directory\n", program, filename);
            return FAILURE;
        }
        return delete_directory_recursive(filename, options, program);
    }
    
    /* Check filename safety for long names (Amiga filesystem truncation protection) */
    if (!check_filename_safety(filename, program)) {
        if (!options->force_flag) {
            fprintf(stderr, "%s: refusing to delete potentially unsafe filename\n", program);
            return FAILURE;
        } else {
            fprintf(stderr, "%s: proceeding with potentially unsafe deletion due to -f flag\n", program);
        }
    }
    
    /* Interactive confirmation */
    if (options->interactive_flag && !options->force_flag) {
        if (!confirm_deletion(filename, program)) {
            return SUCCESS; /* User chose not to delete */
        }
    }
    
    /* Delete the file using Amiga native DeleteFile API */
    result = DeleteFile((STRPTR)filename);
    if (result) {
        if (options->verbose_flag) {
            printf("removed '%s'\n", filename);
        }
        return SUCCESS;
    } else {
        print_error(program, filename, IoErr());
        return FAILURE;
    }
}

/**
 * @brief Delete a directory recursively using Amiga native functions
 * @param dirname Name of directory to delete
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int delete_directory_recursive(const char *dirname, RmOptions *options, const char *program) {
    BPTR lock;
    struct FileInfoBlock fib;
    char full_path[512];
    int result = SUCCESS;
    
    /* Open directory for reading */
    lock = Lock((STRPTR)dirname, ACCESS_READ);
    if (!lock) {
        fprintf(stderr, "%s: cannot access '%s': No such file or directory\n", program, dirname);
        return FAILURE;
    }
    
    /* Examine directory */
    if (!Examine(lock, &fib)) {
        fprintf(stderr, "%s: cannot examine '%s'\n", program, dirname);
        UnLock(lock);
        return FAILURE;
    }
    
    /* Check if it's actually a directory */
    if (fib.fib_DirEntryType <= 0) {
        fprintf(stderr, "%s: cannot remove '%s': Not a directory\n", program, dirname);
        UnLock(lock);
        return FAILURE;
    }
    
    /* Process directory contents */
    while (ExNext(lock, &fib)) {
        /* Skip . and .. entries */
        if (strcmp(fib.fib_FileName, ".") == 0 || strcmp(fib.fib_FileName, "..") == 0) {
            continue;
        }
        
        /* Build full path */
        snprintf(full_path, sizeof(full_path), "%s/%s", dirname, fib.fib_FileName);
        
        /* Recursively delete subdirectories */
        if (fib.fib_DirEntryType > 0) {
            if (delete_directory_recursive(full_path, options, program) != SUCCESS) {
                result = FAILURE;
            }
        } else {
            /* Delete file */
            if (delete_file(full_path, options, program) != SUCCESS) {
                result = FAILURE;
            }
        }
    }
    
    UnLock(lock);
    
    /* If all contents were deleted successfully, delete the directory itself */
    if (result == SUCCESS) {
        /* Check filename safety for long directory names */
        if (!check_filename_safety(dirname, program)) {
            if (!options->force_flag) {
                fprintf(stderr, "%s: refusing to delete potentially unsafe directory name\n", program);
                return FAILURE;
            } else {
                fprintf(stderr, "%s: proceeding with potentially unsafe directory deletion due to -f flag\n", program);
            }
        }
        
        /* Interactive confirmation for directory */
        if (options->interactive_flag && !options->force_flag) {
            if (!confirm_deletion(dirname, program)) {
                return SUCCESS; /* User chose not to delete */
            }
        }
        
        /* Delete the directory */
        if (DeleteFile((STRPTR)dirname)) {
            if (options->verbose_flag) {
                printf("removed directory '%s'\n", dirname);
            }
        } else {
            print_error(program, dirname, IoErr());
            result = FAILURE;
        }
    }
    
    return result;
}

/**
 * @brief Ask user for confirmation before deletion
 * @param filename Name of file to delete
 * @param program Program name for prompts
 * @return TRUE if user confirms, FALSE otherwise
 */
static BOOL confirm_deletion(const char *filename, const char *program) {
    char response[10];
    
    printf("%s: remove '%s'? ", program, filename);
    fflush(stdout);
    
    if (fgets(response, sizeof(response), stdin)) {
        /* Remove newline */
        response[strcspn(response, "\n")] = '\0';
        
        /* Check for 'y' or 'yes' (case insensitive) */
        if (strlen(response) > 0 && 
            (response[0] == 'y' || response[0] == 'Y')) {
            return TRUE;
        }
    }
    
    return FALSE;
}

/**
 * @brief Check filename safety to prevent wrong file deletion due to truncation
 * @param filename Name of file to check
 * @param program Program name for error messages
 * @return TRUE if safe to delete, FALSE if potentially dangerous
 */
static BOOL check_filename_safety(const char *filename, const char *program) {
    BPTR lock1, lock2;
    BOOL is_safe = TRUE;
    char truncated[31];
    
    /* Check if filename is longer than Amiga's typical 30-character limit */
    if (strlen(filename) > 30) {
        /* Get lock on the full filename */
        lock1 = Lock((STRPTR)filename, ACCESS_READ);
        if (lock1) {
            /* Create truncated version (30 characters) */
            strncpy(truncated, filename, 30);
            truncated[30] = '\0';
            
            /* Get lock on the truncated filename */
            lock2 = Lock((STRPTR)truncated, ACCESS_READ);
            if (lock2) {
                /* Check if the locks point to the same file */
                if (SameLock(lock1, lock2) == LOCK_SAME) {
                    /* Potential risk - files might be the same due to truncation */
                    fprintf(stderr, "%s: WARNING: Filename '%s' may be truncated by filesystem\n", program, filename);
                    fprintf(stderr, "%s: This could result in deleting the wrong file\n", program);
                    fprintf(stderr, "%s: Truncated name would be: '%s'\n", program, truncated);
                    is_safe = FALSE;
                }
                UnLock(lock2);
            }
            UnLock(lock1);
        }
    }
    
    return is_safe;
}

/**
 * @brief Print error message based on Amiga error code
 * @param program Program name
 * @param filename Name of file that caused error
 * @param error_code Amiga error code from IoErr()
 */
static void print_error(const char *program, const char *filename, LONG error_code) {
    switch (error_code) {
        case ERROR_DELETE_PROTECTED:
            fprintf(stderr, "%s: cannot remove '%s': Permission denied\n", program, filename);
            break;
        case ERROR_OBJECT_IN_USE:
            fprintf(stderr, "%s: cannot remove '%s': Object in use\n", program, filename);
            break;
        case ERROR_DISK_NOT_VALIDATED:
            fprintf(stderr, "%s: cannot remove '%s': Disk not validated\n", program, filename);
            break;
        case ERROR_WRITE_PROTECTED:
            fprintf(stderr, "%s: cannot remove '%s': Write protected\n", program, filename);
            break;
        case ERROR_OBJECT_EXISTS:
            fprintf(stderr, "%s: cannot remove '%s': Object exists\n", program, filename);
            break;
        case ERROR_DIRECTORY_NOT_EMPTY:
            fprintf(stderr, "%s: cannot remove '%s': Directory not empty\n", program, filename);
            break;
        default:
            fprintf(stderr, "%s: cannot remove '%s': Error %ld\n", program, filename, error_code);
            break;
    }
}

/**
 * @brief Print usage information
 * @param program Program name
 */
void usage(const char *program) {
    printf("Usage: %s [OPTION]... FILE...\n", program);
    printf("Remove (unlink) the FILE(s).\n\n");
    printf("  -f, --force     ignore nonexistent files and arguments, never prompt\n");
    printf("  -i              prompt before every removal\n");
    printf("  -r, -R, --recursive  remove directories and their contents recursively\n");
    printf("  -v, --verbose   explain what is being done\n");
    printf("  -h, --help      display this help and exit\n");
    printf("  -V, --version   output version information and exit\n\n");
    printf("FILE may contain wildcards: * matches any characters, ? matches any single\n");
    printf("character, [abc] matches any character in the set, and ~ expands to home directory.\n\n");
    printf("By default, rm does not remove directories.  Use the --recursive (-r or -R)\n");
    printf("option to remove each listed directory, too, along with all of its contents.\n\n");
    printf("To remove a file whose name starts with a '-', for example '-foo',\n");
    printf("use one of these commands:\n");
    printf("  %s -- -foo\n", program);
    printf("  %s ./-foo\n", program);
}

/**
 * @brief Print version information
 * @param program Program name
 */
void print_version(const char *program) {
    printf("%s", verstag);
}

/**
 * @brief Process wildcard pattern and expand it to actual files
 * @param pattern Pattern to expand (may contain wildcards)
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int process_wildcard_pattern(const char *pattern, RmOptions *options, const char *program) {
    char **expanded_files;
    char **file_ptr;
    int result = SUCCESS;
    
    /* Check if pattern contains wildcards */
    if (strpbrk(pattern, "*?[\\~")) {
        /* Expand wildcards */
        expanded_files = wildexpand((char *)pattern);
        if (expanded_files) {
            /* Process each expanded file */
            for (file_ptr = expanded_files; *file_ptr; file_ptr++) {
                if (delete_file(*file_ptr, options, program) != SUCCESS) {
                    result = FAILURE;
                }
            }
            wildfree(expanded_files);
        } else {
            /* No matches found for wildcard pattern */
            if (!options->force_flag) {
                fprintf(stderr, "%s: no matches found: %s\n", program, pattern);
                result = FAILURE;
            }
        }
    } else {
        /* No wildcards, process as regular file */
        if (delete_file(pattern, options, program) != SUCCESS) {
            result = FAILURE;
        }
    }
    
    return result;
}

/**
 * @brief Sanitize and validate a file path
 * @param path Path to sanitize (will be modified)
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int sanitize_path(char *path, const char *program) {
    char *last;
    int i, slash_count = 0;
    
    if (!path || strlen(path) == 0) {
        return FAILURE;
    }
    
    /* Check for device protection (paths ending with ':') */
    last = &path[strlen(path) - 1];
    if (*last == ':') {
        fprintf(stderr, "%s: cannot remove '%s': Cannot delete device\n", program, path);
        return FAILURE;
    }
    
    /* Remove trailing slashes and count them */
    for (i = 0; *last == '/'; i++, last--) {
        *last = '\0';
        slash_count++;
    }
    
    /* Validate path after slash removal */
    if (!*path || slash_count > 1) {
        fprintf(stderr, "%s: invalid path '%s': Too many trailing slashes\n", program, path);
        return FAILURE;
    }
    
    return SUCCESS;
}
