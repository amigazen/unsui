/*
 * uniq.c - Uniq command for Unsui POSIX runtime
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on uniq by John Woods (public domain).
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
#include "uniq.h"
#include "/common/common.h"
#include "/common/getopt.h"

extern struct DosLibrary *DOSBase;

/* Version tag for Amiga */
static const char *verstag = "$VER: uniq 1.0 (24.08.25)\n";
static const char *stack_cookie = "$STACK: 4096";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* exit code in case of failure */

/* For ReadArgs template */
enum {
    ARG_UNIQUE,
    ARG_DUPLICATE,
    ARG_COUNT_FLAG,
    ARG_FIELDS,
    ARG_CHARS,
    ARG_CHARS_PLUS,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void uniq_usage(const char *program);
int uniq_parse_getopt_args(int argc, char **argv, struct uniq_options *opts, int *file_start, const char *program);
int run_uniq_logic(struct uniq_options *opts, const char *program);
int uniq_process_file(const char *filename, struct uniq_options *opts);
int uniq_process_stdin(struct uniq_options *opts);
char *skip_fields_and_chars(char *line, struct uniq_options *opts);
int lines_equal(char *line1, char *line2, struct uniq_options *opts);
void show_line(char *line, int count, struct uniq_options *opts);
int uniq_process_input(FILE *input, struct uniq_options *opts);
int read_line(char **lineptr, size_t *n, FILE *stream);

/**
 * @brief Display usage information
 */
void uniq_usage(const char *program) {
    fprintf(stderr, "Version: %s", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [INPUT [OUTPUT]]\n", program);
    fprintf(stderr, "Usage (Amiga): %s [UNIQUE/S] [DUPLICATE/S] [COUNT/S] [FIELDS/N] [CHARS/N] [CHARSPLUS/N]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -u           output only unique lines (lines that appear only once)\n");
    fprintf(stderr, "  -d           output only duplicate lines (lines that appear more than once)\n");
    fprintf(stderr, "  -c           prefix each line with count of occurrences\n");
    fprintf(stderr, "  -f n         skip n fields before comparing\n");
    fprintf(stderr, "  -s n         skip n characters after skipping fields\n");
    fprintf(stderr, "  +n           skip n characters after skipping fields (alternative syntax)\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Remove duplicate consecutive lines from sorted text files.\n");
    fprintf(stderr, "  By default, outputs lines that are not repeated in the input.\n");
    exit(FAILURE);
}

/**
 * @brief Parse command line arguments using getopt (POSIX style)
 * @param argc Argument count
 * @param argv Argument vector
 * @param opts Options structure to fill
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 * @return SUCCESS on success, FAILURE on error
 */
int uniq_parse_getopt_args(int argc, char **argv, struct uniq_options *opts, int *file_start, const char *program) {
    int c;
    int i;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "udcf:s:hV")) != -1) {
        switch (c) {
            case 'u':
                opts->unique_flag = TRUE;
                opts->duplicate_flag = FALSE;
                break;
            case 'd':
                opts->duplicate_flag = TRUE;
                opts->unique_flag = FALSE;
                break;
            case 'c':
                opts->count_flag = TRUE;
                break;
            case 'f':
                opts->fields_skip = atoi(optarg);
                if (opts->fields_skip < 0) {
                    fprintf(stderr, "Error: fields to skip must be >= 0\n");
                    return FAILURE;
                }
                break;
            case 's':
                opts->chars_skip = atoi(optarg);
                if (opts->chars_skip < 0) {
                    fprintf(stderr, "Error: characters to skip must be >= 0\n");
                    return FAILURE;
                }
                break;
            case 'h':
            case 'V':
                uniq_usage(program);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    /* Handle +n syntax for character skipping */
    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '+') {
            opts->chars_skip_plus = atoi(argv[i] + 1);
            if (opts->chars_skip_plus < 0) {
                fprintf(stderr, "Error: characters to skip must be >= 0\n");
                return FAILURE;
            }
            break;
        }
    }
    
    *file_start = optind;
    
    return SUCCESS;
}

/**
 * @brief Core uniq logic separated from argument parsing
 * @param opts Options structure
 * @param program Program name for error messages
 * @return Exit code
 */
int run_uniq_logic(struct uniq_options *opts, const char *program) {
    return uniq_process_stdin(opts);
}

/**
 * @brief Process input from stdin
 * @param opts Options structure
 * @return SUCCESS on success, FAILURE on error
 */
int uniq_process_stdin(struct uniq_options *opts) {
    return uniq_process_input(stdin, opts);
}

/**
 * @brief Process input from file
 * @param filename Input filename
 * @param opts Options structure
 * @return SUCCESS on success, FAILURE on error
 */
int uniq_process_file(const char *filename, struct uniq_options *opts) {
    FILE *file;
    int result;
    
    if (strcmp(filename, "-") == 0) {
        return uniq_process_stdin(opts);
    }
    
    file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Error: cannot open file '%s'\n", filename);
        return FAILURE;
    }
    
    result = uniq_process_input(file, opts);
    fclose(file);
    return result;
}

/**
 * @brief Skip fields and characters according to options
 * @param line Input line
 * @param opts Options structure
 * @return Pointer to position after skipping
 */
char *skip_fields_and_chars(char *line, struct uniq_options *opts) {
    char *pos = line;
    int i;
    int chars_to_skip;
    
    /* Skip fields (separated by whitespace) */
    for (i = 0; i < opts->fields_skip; i++) {
        /* Skip blanks */
        while (*pos && (*pos == ' ' || *pos == '\t')) pos++;
        if (!*pos) return pos;
        while (*pos && (*pos != ' ' && *pos != '\t')) pos++;
        if (!*pos) return pos;
    }
    
    /* Skip characters after fields */
    chars_to_skip = opts->chars_skip + opts->chars_skip_plus;
    for (i = 0; i < chars_to_skip && *pos; i++) {
        pos++;
    }
    
    return pos;
}

/**
 * @brief Compare two lines for equality, considering skip options
 * @param line1 First line
 * @param line2 Second line
 * @param opts Options structure
 * @return TRUE if lines are equal, FALSE otherwise
 */
int lines_equal(char *line1, char *line2, struct uniq_options *opts) {
    char *pos1 = skip_fields_and_chars(line1, opts);
    char *pos2 = skip_fields_and_chars(line2, opts);
    
    return strcmp(pos1, pos2) == 0;
}

/**
 * @brief Display a line according to options
 * @param line Line to display
 * @param count Count of consecutive occurrences
 * @param opts Options structure
 */
void show_line(char *line, int count, struct uniq_options *opts) {
    if (opts->count_flag) {
        printf("%4d %s", count, line);
    } else {
        if ((opts->unique_flag && count == 1) || (opts->duplicate_flag && count != 1)) {
            printf("%s", line);
        }
    }
}

/**
 * @brief Read a line from input using getchar (portable)
 * @param lineptr Pointer to line buffer
 * @param n Size of buffer
 * @param input Input stream
 * @return Number of characters read, or -1 on EOF/error
 */
int read_line(char **lineptr, size_t *n, FILE *input) {
    int c;
    size_t pos = 0;
    
    if (!*lineptr || *n == 0) {
        *n = BUFLEN;
        *lineptr = malloc(*n);
        if (!*lineptr) return -1;
    }
    
    while ((c = getc(input)) != EOF) {
        if (pos >= *n - 1) {
            size_t new_size = *n * 2;
            char *new_ptr = realloc(*lineptr, new_size);
            if (!new_ptr) return -1;
            *lineptr = new_ptr;
            *n = new_size;
        }
        
        (*lineptr)[pos++] = c;
        
        if (c == '\n') {
            break;
        }
    }
    
    if (pos == 0 && c == EOF) {
        return -1; /* EOF */
    }
    
    (*lineptr)[pos] = '\0';
    return (int)pos;
}

/**
 * @brief Process input and remove duplicate consecutive lines
 * @param input Input file stream
 * @param opts Options structure
 * @return SUCCESS on success, FAILURE on error
 */
int uniq_process_input(FILE *input, struct uniq_options *opts) {
    char *prevline = NULL;
    char *nowline = NULL;
    char *temp;
    int seen = 0;
    size_t len = 0;
    int read;
    
    /* Read first line */
    prevline = malloc(BUFLEN);
    if (!prevline) {
        fprintf(stderr, "Error: no memory for line buffer\n");
        return FAILURE;
    }
    
    if ((read = read_line(&prevline, &len, input)) == -1) {
        free(prevline);
        return SUCCESS; /* Empty input */
    }
    
    seen = 1;
    
    /* Allocate second buffer */
    nowline = malloc(BUFLEN);
    if (!nowline) {
        free(prevline);
        fprintf(stderr, "Error: no memory for line buffer\n");
        return FAILURE;
    }
    
    /* Process remaining lines */
    while ((read = read_line(&nowline, &len, input)) != -1) {
        if (!lines_equal(prevline, nowline, opts)) {
            show_line(prevline, seen, opts);
            seen = 1;
            
            /* Swap buffers */
            temp = prevline;
            prevline = nowline;
            nowline = temp;
        } else {
            seen += 1;
        }
    }
    
    /* Show last line */
    show_line(prevline, seen, opts);
    
    /* Cleanup */
    free(prevline);
    free(nowline);
    
    return SUCCESS;
}

/**
 * @brief Main function: dispatcher for parsing style
 * @param argc Argument count
 * @param argv Argument vector
 * @return Exit status
 */
int main(int argc, char **argv) {
    struct uniq_options opts;
    int file_start = 1;
    char *program;
    char *input_file = NULL;
    char *output_file = NULL;
    
    /* ReadArgs Path Variables */
    const char *template = "UNIQUE/S,DUPLICATE/S,COUNT/S,FIELDS/N,CHARS/N,CHARSPLUS/N,POSIX/K/F";
    LONG arg_array[ARG_COUNT] = {0};
    struct RDArgs *rdargs = NULL;
    char *cmd_string = NULL;
    int ret_code = SUCCESS;
    BOOL interactive_help = FALSE;
    
    /* POSIX/F Path Variables */
    char *posix_str;
    int new_argc;
    char *new_argv[64]; /* MAX_TEMPLATE_ITEMS equivalent */
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
        uniq_usage(program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        /* Initialize defaults */
        opts.unique_flag = TRUE;
        opts.duplicate_flag = TRUE;
        opts.count_flag = FALSE;
        opts.fields_skip = 0;
        opts.chars_skip = 0;
        opts.chars_skip_plus = 0;
        
        uniq_parse_getopt_args(argc, argv, &opts, &file_start, program);
        
        /* Handle input/output files */
        if (file_start < argc) {
            input_file = argv[file_start];
            if (file_start + 1 < argc) {
                output_file = argv[file_start + 1];
            }
        }
        
        /* Redirect output if specified */
        if (output_file && strcmp(output_file, "-") != 0) {
            if (freopen(output_file, "w", stdout) == NULL) {
                fprintf(stderr, "Error: cannot open output file '%s'\n", output_file);
                return FAILURE;
            }
        }
        
        /* Process input */
        if (input_file) {
            ret_code = uniq_process_file(input_file, &opts);
        } else {
            ret_code = uniq_process_stdin(&opts);
        }
        
        return ret_code;
        
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
            new_argc = tokenize_string(posix_str, &new_argv[1], 63) + 1;

            /* Initialize defaults */
            opts.unique_flag = TRUE;
            opts.duplicate_flag = TRUE;
            opts.count_flag = FALSE;
            opts.fields_skip = 0;
            opts.chars_skip = 0;
            opts.chars_skip_plus = 0;

            uniq_parse_getopt_args(new_argc, new_argv, &opts, &file_start, program);
            ret_code = run_uniq_logic(&opts, program);

        } else {
            /* Standard ReadArgs processing */
            /* Initialize defaults */
            opts.unique_flag = TRUE;
            opts.duplicate_flag = TRUE;
            opts.count_flag = FALSE;
            opts.fields_skip = 0;
            opts.chars_skip = 0;
            opts.chars_skip_plus = 0;
            
            if (arg_array[ARG_UNIQUE]) {
                opts.unique_flag = TRUE;
                opts.duplicate_flag = FALSE;
            }
            if (arg_array[ARG_DUPLICATE]) {
                opts.duplicate_flag = TRUE;
                opts.unique_flag = FALSE;
            }
            if (arg_array[ARG_COUNT_FLAG]) {
                opts.count_flag = TRUE;
            }
            if (arg_array[ARG_FIELDS]) {
                opts.fields_skip = *(LONG *)arg_array[ARG_FIELDS];
                if (opts.fields_skip < 0) {
                    fprintf(stderr, "Error: fields to skip must be >= 0\n");
                    ret_code = FAILURE;
                }
            }
            if (arg_array[ARG_CHARS]) {
                opts.chars_skip = *(LONG *)arg_array[ARG_CHARS];
                if (opts.chars_skip < 0) {
                    fprintf(stderr, "Error: characters to skip must be >= 0\n");
                    ret_code = FAILURE;
                }
            }
            if (arg_array[ARG_CHARS_PLUS]) {
                opts.chars_skip_plus = *(LONG *)arg_array[ARG_CHARS_PLUS];
                if (opts.chars_skip_plus < 0) {
                    fprintf(stderr, "Error: characters to skip must be >= 0\n");
                    ret_code = FAILURE;
                }
            }
            
            if (ret_code == SUCCESS) {
                ret_code = run_uniq_logic(&opts, program);
            }
        }

        /* Clean up */
        FreeDosObject(DOS_RDARGS, rdargs);
        free(cmd_string);
        
        return ret_code;
    }
}
