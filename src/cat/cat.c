/*
 * cat - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * This version integrates AmigaDOS ReadArgs and standard POSIX getopt
 * for command-line parsing, providing both POSIX compatibility and
 * Amiga native functionality with wildcard expansion support.
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

/* Wildcard expansion functions */
extern char **wildexpand(char *w);
extern void wildfree(char **freelist);
extern int amigaizepath(char *to);

/* Version tag for Amiga */
static const char *verstag = "$VER: cat 1.0 (10/09/25)";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_FILES,
    ARG_NUMBER,
    ARG_NONBLANK,
    ARG_SHOW_ALL,
    ARG_SHOW_ENDS,
    ARG_SHOW_TABS,
    ARG_SHOW_NONPRINTING,
    ARG_SQUEEZE_BLANK,
    ARG_POSIX,
    ARG_COUNT
};

/* Global options structure */
typedef struct {
    int number_flag;           /* -n: number all output lines */
    int number_nonblank_flag;  /* -b: number nonempty output lines */
    int show_all_flag;         /* -A: equivalent to -vET */
    int show_ends_flag;        /* -E: display $ at end of each line */
    int show_tabs_flag;        /* -T: display TAB characters as ^I */
    int show_nonprinting_flag; /* -v: use ^ and M- notation, except for LFD and TAB */
    int squeeze_blank_flag;    /* -s: suppress repeated empty output lines */
    int exit_code;             /* Exit code */
} CatOptions;

/* Function declarations for forward references */
void usage(const char *program);
void print_version(const char *program);
int run_cat_logic(CatOptions *options, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, CatOptions *options, int *file_start, const char *program);
void init_options(CatOptions *options);
void cleanup_options(CatOptions *options);

/* Static function declarations */
static int cat_file(const char *filename, CatOptions *options, const char *program);
static void print_error(const char *program, const char *filename, LONG error_code);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    CatOptions options;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILES/M,NUMBER/S,NONBLANK/S,SHOWALL/S,SHOWENDS/S,SHOWTABS/S,SHOWNONPRINTING/S,SQUEEZEBLANK/S,POSIX/K/F";
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
    unsigned int combined_len;

    if (argc < 1) {
        exit(FAILURE);
    }
    
    program = my_basename(argv[0]);

    /* Initialize options */
    init_options(&options);

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &options, &file_start, program);
        files = &argv[file_start];
        file_count = argc - file_start;
        
        if (file_count == 0) {
            /* No files specified, read from stdin */
            file_count = 1;
            files = (char **)malloc(sizeof(char *));
            files[0] = "-";
        }
        
        return run_cat_logic(&options, file_count, files, program);
        
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
                if (initial_args_str[0] != '\0' && strlen(initial_args_str) > 0) {
                    /* Remove trailing whitespace from initial_args_str */
                    char *p = initial_args_str + strlen(initial_args_str) - 1;
                    while (p >= initial_args_str && isspace((unsigned char)*p)) {
                        *p = '\0';
                        p--;
                    }
                    
                    if (strlen(initial_args_str) > 0) {
                        combined_len = strlen(initial_args_str) + strlen(user_input_buf) + 2;
                        cmd_string = malloc(combined_len);
                        if (cmd_string) {
                            strcpy(cmd_string, initial_args_str);
                            strcat(cmd_string, " ");
                            strcat(cmd_string, user_input_buf);
                        }
                    } else {
                        cmd_string = strdup(user_input_buf);
                    }
                } else {
                    cmd_string = strdup(user_input_buf);
                }
            } else {
                cmd_string = strdup(initial_args_str);
            }
        } else {
            /* Standard case: build command string from all args */
            cmd_string = build_command_string(argc, argv, NULL);
        }

        if (!cmd_string) {
            fprintf(stderr, "%s: out of memory for command string\n", program);
            cleanup_options(&options);
            FreeDosObject(DOS_RDARGS, rdargs);
            return FAILURE;
        }

        /* Set up ReadArgs to parse from our string */
        fprintf(stderr, "DEBUG: Command string: '%s'\n", cmd_string);
        rdargs->RDA_Source.CS_Buffer = cmd_string;
        rdargs->RDA_Source.CS_Length = strlen(cmd_string);
        rdargs->RDA_Source.CS_CurChr = 0;
        rdargs->RDA_Flags |= RDAF_NOPROMPT;

        if (!ReadArgs(template, arg_array, rdargs)) {
            LONG error = IoErr();
            fprintf(stderr, "DEBUG: ReadArgs failed with error %ld\n", error);
            PrintFault(error, program);
            ret_code = FAILURE;
        } else {
            fprintf(stderr, "DEBUG: ReadArgs succeeded\n");
            /* Check for POSIX/F override first */
            if (arg_array[ARG_POSIX]) {
                posix_str = (char *)arg_array[ARG_POSIX];

                /* Tokenize the string and build a new argv for getopt */
                new_argv[0] = program;
                new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;

                parse_getopt_args(new_argc, new_argv, &options, &file_start, program);
                files = &new_argv[file_start];
                ret_code = run_cat_logic(&options, new_argc - file_start, files, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_NUMBER]) options.number_flag = TRUE;
                if (arg_array[ARG_NONBLANK]) options.number_nonblank_flag = TRUE;
                if (arg_array[ARG_SHOW_ALL]) options.show_all_flag = TRUE;
                if (arg_array[ARG_SHOW_ENDS]) options.show_ends_flag = TRUE;
                if (arg_array[ARG_SHOW_TABS]) options.show_tabs_flag = TRUE;
                if (arg_array[ARG_SHOW_NONPRINTING]) options.show_nonprinting_flag = TRUE;
                if (arg_array[ARG_SQUEEZE_BLANK]) options.squeeze_blank_flag = TRUE;
                
                if (arg_array[ARG_FILES]) {
                    files = (char **)arg_array[ARG_FILES];
                    file_count = 1; /* ReadArgs gives us one file at a time */
                    
                    ret_code = run_cat_logic(&options, file_count, files, program);
                } else {
                    /* No files specified, read from stdin */
                    file_count = 1;
                    files = (char **)malloc(sizeof(char *));
                    files[0] = "-";
                    ret_code = run_cat_logic(&options, file_count, files, program);
                }
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
void parse_getopt_args(int argc, char **argv, CatOptions *options, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "AbEnTuvVh")) != -1) {
        switch (c) {
            case 'A':
                options->show_all_flag = TRUE;
                break;
            case 'b':
                options->number_nonblank_flag = TRUE;
                break;
            case 'E':
                options->show_ends_flag = TRUE;
                break;
            case 'n':
                options->number_flag = TRUE;
                break;
            case 'T':
                options->show_tabs_flag = TRUE;
                break;
            case 'u':
                /* -u is ignored in POSIX cat */
                break;
            case 'v':
                options->show_nonprinting_flag = TRUE;
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
 * @brief Core cat logic separated from argument parsing
 * @param options Options structure
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_cat_logic(CatOptions *options, int file_count, char **files, const char *program) {
    int i;
    int result = SUCCESS;
    char **cpp, **cppo;
    
    for (i = 0; i < file_count; i++) {
        if (files[i] && strlen(files[i]) > 0) {
            /* Apply Amiga path conversion */
            amigaizepath(files[i]);
            
            /* Handle wildcard expansion */
            if (!(cppo = wildexpand(files[i])) || !strcmp(files[i], *cppo)) {
                /* No wildcards or no expansion */
                if (cat_file(files[i], options, program) != SUCCESS) {
                    result = FAILURE;
                }
	} else {
                /* Wildcard expansion found matches */
                for (cpp = cppo; *cpp; cpp++) {
                    if (cat_file(*cpp, options, program) != SUCCESS) {
                        result = FAILURE;
                    }
                }
	    wildfree(cppo);
            }
        }
    }
    
    return result;
}

/**
 * @brief Initialize options structure
 * @param options Options structure to initialize
 */
void init_options(CatOptions *options) {
    options->number_flag = FALSE;
    options->number_nonblank_flag = FALSE;
    options->show_all_flag = FALSE;
    options->show_ends_flag = FALSE;
    options->show_tabs_flag = FALSE;
    options->show_nonprinting_flag = FALSE;
    options->squeeze_blank_flag = FALSE;
    options->exit_code = SUCCESS;
}

/**
 * @brief Clean up options structure
 * @param options Options structure to clean up
 */
void cleanup_options(CatOptions *options) {
    /* Nothing to clean up for this structure */
    (void)options;
}

/**
 * @brief Cat a file or stdin using Amiga native functions
 * @param filename Name of file to cat, or "-" for stdin
 * @param options Options structure
 * @param program Program name for error messages
 * @return SUCCESS or FAILURE
 */
static int cat_file(const char *filename, CatOptions *options, const char *program) {
    FILE *fp;
    char buf[1024];
    int line_number = 1;
    int last_was_blank = 0;
    
    if (strcmp(filename, "-") == 0) {
        fp = stdin;
    } else {
        fp = fopen(filename, "r");
        if (!fp) {
            print_error(program, filename, IoErr());
            return FAILURE;
        }
    }
    
    while (fgets(buf, sizeof(buf), fp)) {
        int is_blank = (strlen(buf) == 1 && buf[0] == '\n');
        
        /* Handle squeeze blank lines */
        if (options->squeeze_blank_flag && is_blank && last_was_blank) {
            continue;
        }
        last_was_blank = is_blank;
        
        /* Handle line numbering */
        if (options->number_flag || (options->number_nonblank_flag && !is_blank)) {
            printf("%6d\t", line_number++);
        }
        
        /* Handle show options */
        if (options->show_all_flag || options->show_ends_flag) {
            /* Show $ at end of line */
            int len = strlen(buf);
            if (len > 0 && buf[len-1] == '\n') {
                buf[len-1] = '\0';
                printf("%s$\n", buf);
            } else {
                printf("%s", buf);
            }
        } else if (options->show_tabs_flag) {
            /* Show tabs as ^I */
            char *p = buf;
            while (*p) {
                if (*p == '\t') {
                    printf("^I");
                } else {
                    putchar(*p);
                }
                p++;
            }
        } else if (options->show_nonprinting_flag) {
            /* Show non-printing characters */
            char *p = buf;
            while (*p) {
                if (*p >= 32 && *p <= 126) {
                    putchar(*p);
                } else if (*p == '\n') {
                    putchar('\n');
                } else if (*p == '\t') {
                    putchar('\t');
                } else {
                    printf("^%c", *p + 64);
                }
                p++;
            }
        } else {
            /* Normal output */
	    fputs(buf, stdout);
        }
    }
    
    if (fp != stdin) {
        fclose(fp);
    }
    
    return SUCCESS;
}

/**
 * @brief Print error message based on Amiga error code
 * @param program Program name
 * @param filename Name of file that caused error
 * @param error_code Amiga error code from IoErr()
 */
static void print_error(const char *program, const char *filename, LONG error_code) {
    switch (error_code) {
        case ERROR_OBJECT_NOT_FOUND:
            fprintf(stderr, "%s: %s: No such file or directory\n", program, filename);
            break;
        case ERROR_OBJECT_WRONG_TYPE:
            fprintf(stderr, "%s: %s: Is a directory\n", program, filename);
            break;
        case ERROR_READ_PROTECTED:
            fprintf(stderr, "%s: %s: Permission denied\n", program, filename);
            break;
        case ERROR_DISK_NOT_VALIDATED:
            fprintf(stderr, "%s: %s: Disk not validated\n", program, filename);
            break;
        default:
            fprintf(stderr, "%s: %s: Error %ld\n", program, filename, error_code);
            break;
    }
}

/**
 * @brief Print usage information
 * @param program Program name
 */
void usage(const char *program) {
    fprintf(stderr, "Version: %s", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [FILE...]\n", program);
    fprintf(stderr, "Usage (Amiga): %s FILE/M [NUMBER/S] [NONBLANK/S] [SHOWALL/S] [SHOWENDS/S] [SHOWTABS/S] [SHOWNONPRINTING/S] [SQUEEZEBLANK/S]\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -A, --show-all           equivalent to -vET\n");
    fprintf(stderr, "  -b, --number-nonblank    number nonempty output lines\n");
    fprintf(stderr, "  -E, --show-ends          display $ at end of each line\n");
    fprintf(stderr, "  -n, --number             number all output lines\n");
    fprintf(stderr, "  -s, --squeeze-blank      suppress repeated empty output lines\n");
    fprintf(stderr, "  -T, --show-tabs          display TAB characters as ^I\n");
    fprintf(stderr, "  -v, --show-nonprinting   use ^ and M- notation, except for LFD and TAB\n");
    fprintf(stderr, "  -h, -V                   display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Concatenate FILE(s) to standard output.\n");
    fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
    fprintf(stderr, "  Default: display file contents if no options specified.\n");
    exit(FAILURE);
}

/**
 * @brief Print version information
 * @param program Program name
 */
void print_version(const char *program) {
    printf("%s\n", verstag);
}