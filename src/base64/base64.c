/*
 * base64 - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on base64 by John Walker (fourmilab.ch).
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

#include "base64.h"
#include "/common/common.h"
#include "/common/getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: base64 2.0 (09/09/25)\n";
static const chat *stack_cookie = "$STACK: 8192";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_FILE,
    ARG_DECODE,
    ARG_ENCODE,
    ARG_IGNORE_GARBAGE,
    ARG_WRAP,
    ARG_POSIX,
    ARG_COUNT
};

/* Base64 alphabet */
static const char base64_alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
static const char base64_pad = '=';

/* Function declarations for forward references */
void usage(const char *program);
int run_base64_logic(int decode, int encode, int ignore_garbage, int wrap_col, 
                    char *input_file, char *output_file, const char *program);
void parse_getopt_args(int argc, char **argv, int *decode, int *encode, 
                      int *ignore_garbage, int *wrap_col, char **input_file, 
                      char **output_file, const char *program);
int base64_encode(FILE *input, FILE *output, int wrap_col);
int base64_decode(FILE *input, FILE *output, int ignore_garbage);

/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    int decode = FALSE;
    int encode = FALSE;
    int ignore_garbage = FALSE;
    int wrap_col = 76;
    char *input_file = NULL;
    char *output_file = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILE/K,DECODE/S,ENCODE/S,IGNOREGARBAGE/S,WRAP/K/N,POSIX/K/F";
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
        /* No arguments, encode from stdin to stdout */
        encode = TRUE;
        return run_base64_logic(decode, encode, ignore_garbage, wrap_col, 
                               input_file, output_file, program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &decode, &encode, &ignore_garbage, 
                         &wrap_col, &input_file, &output_file, program);
        return run_base64_logic(decode, encode, ignore_garbage, wrap_col, 
                               input_file, output_file, program);
        
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

                parse_getopt_args(new_argc, new_argv, &decode, &encode, &ignore_garbage, 
                                 &wrap_col, &input_file, &output_file, program);
                ret_code = run_base64_logic(decode, encode, ignore_garbage, wrap_col, 
                                           input_file, output_file, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_DECODE]) {
                    decode = TRUE;
                }
                if (arg_array[ARG_ENCODE]) {
                    encode = TRUE;
                }
                if (arg_array[ARG_IGNORE_GARBAGE]) {
                    ignore_garbage = TRUE;
                }
                if (arg_array[ARG_WRAP]) {
                    wrap_col = (int)(LONG)arg_array[ARG_WRAP];
                }
                
                /* If no flags are set, default to encode */
                if (!decode && !encode) {
                    encode = TRUE;
                }
                
                if (arg_array[ARG_FILE]) {
                    input_file = (char *)arg_array[ARG_FILE];
                }
                
                ret_code = run_base64_logic(decode, encode, ignore_garbage, wrap_col, 
                                           input_file, output_file, program);
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
 * @param decode Flag to decode base64
 * @param encode Flag to encode base64
 * @param ignore_garbage Flag to ignore non-base64 characters
 * @param wrap_col Column to wrap output at
 * @param input_file Input file name
 * @param output_file Output file name
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, int *decode, int *encode, 
                      int *ignore_garbage, int *wrap_col, char **input_file, 
                      char **output_file, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "deiw:hV")) != -1) {
        switch (c) {
            case 'd':
                *decode = TRUE;
                break;
            case 'e':
                *encode = TRUE;
                break;
            case 'i':
                *ignore_garbage = TRUE;
                break;
            case 'w':
                *wrap_col = atoi(optarg);
                if (*wrap_col < 4) {
                    fprintf(stderr, "%s: wrap column must be at least 4\n", program);
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
    
    /* If no flags are set, default to encode */
    if (!*decode && !*encode) {
        *encode = TRUE;
    }
    
    /* Check for conflicting flags */
    if (*decode && *encode) {
        fprintf(stderr, "%s: cannot specify both -d and -e\n", program);
        exit(FAILURE);
    }
    
    /* Get input and output files */
    if (optind < argc) {
        *input_file = argv[optind];
        if (optind + 1 < argc) {
            *output_file = argv[optind + 1];
        }
    }
}

/**
 * @brief Core base64 logic separated from argument parsing
 * @param decode Flag to decode base64
 * @param encode Flag to encode base64
 * @param ignore_garbage Flag to ignore non-base64 characters
 * @param wrap_col Column to wrap output at
 * @param input_file Input file name
 * @param output_file Output file name
 * @param program Program name for error messages
 * @return Exit code
 */
int run_base64_logic(int decode, int encode, int ignore_garbage, int wrap_col, 
                    char *input_file, char *output_file, const char *program) {
    FILE *input = stdin;
    FILE *output = stdout;
    int ret_code = SUCCESS;
    
    /* Open input file if specified */
    if (input_file) {
        input = fopen(input_file, "rb");
        if (!input) {
            fprintf(stderr, "%s: cannot open %s\n", program, input_file);
            return FAILURE;
        }
    }
    
    /* Open output file if specified */
    if (output_file) {
        output = fopen(output_file, "wb");
        if (!output) {
            fprintf(stderr, "%s: cannot open %s\n", program, output_file);
            if (input != stdin) fclose(input);
            return FAILURE;
        }
    }
    
    /* Perform the encoding or decoding */
    if (decode) {
        ret_code = base64_decode(input, output, ignore_garbage);
    } else {
        ret_code = base64_encode(input, output, wrap_col);
    }
    
    /* Clean up */
    if (input != stdin) fclose(input);
    if (output != stdout) fclose(output);
    
    return ret_code;
}

/**
 * @brief Encode input to base64
 * @param input Input file stream
 * @param output Output file stream
 * @param wrap_col Column to wrap output at
 * @return Exit code
 */
int base64_encode(FILE *input, FILE *output, int wrap_col) {
    int c;
    int in_count = 0;
    int out_count = 0;
    unsigned char in_buf[3];
    unsigned char out_buf[4];
    int i;
    
    while ((c = getc(input)) != EOF) {
        in_buf[in_count++] = (unsigned char)c;
        
        if (in_count == 3) {
            /* Convert 3 bytes to 4 base64 characters */
            out_buf[0] = base64_alphabet[in_buf[0] >> 2];
            out_buf[1] = base64_alphabet[((in_buf[0] & 0x03) << 4) | (in_buf[1] >> 4)];
            out_buf[2] = base64_alphabet[((in_buf[1] & 0x0F) << 2) | (in_buf[2] >> 6)];
            out_buf[3] = base64_alphabet[in_buf[2] & 0x3F];
            
            /* Write output */
            for (i = 0; i < 4; i++) {
                putc(out_buf[i], output);
                out_count++;
                
                /* Wrap line if needed */
                if (wrap_col > 0 && out_count % wrap_col == 0) {
                    putc('\n', output);
                }
            }
            
            in_count = 0;
        }
    }
    
    /* Handle remaining bytes */
    if (in_count > 0) {
        /* Pad input to 3 bytes */
        for (i = in_count; i < 3; i++) {
            in_buf[i] = 0;
        }
        
        /* Convert remaining bytes */
        out_buf[0] = base64_alphabet[in_buf[0] >> 2];
        out_buf[1] = base64_alphabet[((in_buf[0] & 0x03) << 4) | (in_buf[1] >> 4)];
        out_buf[2] = base64_alphabet[((in_buf[1] & 0x0F) << 2) | (in_buf[2] >> 6)];
        out_buf[3] = base64_alphabet[in_buf[2] & 0x3F];
        
        /* Add padding */
        for (i = in_count; i < 3; i++) {
            out_buf[i + 1] = base64_pad;
        }
        
        /* Write output */
        for (i = 0; i < 4; i++) {
            putc(out_buf[i], output);
            out_count++;
            
            /* Wrap line if needed */
            if (wrap_col > 0 && out_count % wrap_col == 0) {
                putc('\n', output);
            }
        }
    }
    
    /* Add final newline if we wrapped */
    if (wrap_col > 0 && out_count % wrap_col != 0) {
        putc('\n', output);
    }
    
    return SUCCESS;
}

/**
 * @brief Decode base64 input
 * @param input Input file stream
 * @param output Output file stream
 * @param ignore_garbage Flag to ignore non-base64 characters
 * @return Exit code
 */
int base64_decode(FILE *input, FILE *output, int ignore_garbage) {
    int c;
    int in_count = 0;
    unsigned char in_buf[4];
    unsigned char out_buf[3];
    int pad_count = 0;
    int val;
    int i;
    int bytes_to_write;
    
    while ((c = getc(input)) != EOF) {
        /* Skip whitespace and newlines */
        if (isspace(c)) {
            continue;
        }
        
        /* Handle padding */
        if (c == base64_pad) {
            if (in_count < 2) {
                fprintf(stderr, "base64: invalid padding\n");
                return FAILURE;
            }
            pad_count++;
            in_buf[in_count++] = 0;
            continue;
        }
        
        /* Convert character to 6-bit value */
        val = -1;
        for (i = 0; i < 64; i++) {
            if (base64_alphabet[i] == c) {
                val = i;
                break;
            }
        }
        
        if (val == -1) {
            if (ignore_garbage) {
                continue;
            } else {
                fprintf(stderr, "base64: invalid character '%c'\n", c);
                return FAILURE;
            }
        }
        
        in_buf[in_count++] = (unsigned char)val;
        
        if (in_count == 4) {
            /* Convert 4 base64 characters to 3 bytes */
            out_buf[0] = (in_buf[0] << 2) | (in_buf[1] >> 4);
            out_buf[1] = (in_buf[1] << 4) | (in_buf[2] >> 2);
            out_buf[2] = (in_buf[2] << 6) | in_buf[3];
            
            /* Write output, accounting for padding */
            bytes_to_write = 3 - pad_count;
            for (i = 0; i < bytes_to_write; i++) {
                putc(out_buf[i], output);
            }
            
            in_count = 0;
            pad_count = 0;
        }
    }
    
    /* Handle remaining characters */
    if (in_count > 0) {
        if (in_count < 4) {
            fprintf(stderr, "base64: incomplete base64 input\n");
            return FAILURE;
        }
        
        /* Convert remaining characters */
        out_buf[0] = (in_buf[0] << 2) | (in_buf[1] >> 4);
        out_buf[1] = (in_buf[1] << 4) | (in_buf[2] >> 2);
        out_buf[2] = (in_buf[2] << 6) | in_buf[3];
        
        /* Write output, accounting for padding */
        bytes_to_write = 3 - pad_count;
        for (i = 0; i < bytes_to_write; i++) {
            putc(out_buf[i], output);
        }
    }
    
    return SUCCESS;
}

/**
 * @brief Display usage information
 */
void usage(const char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [FILE] [OUTFILE]\n", program);
    fprintf(stderr, "Usage (Amiga): %s [FILE] [DECODE/S] [ENCODE/S] [IGNORE_GARBAGE/S] [WRAP/K/N]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -d          decode input\n");
    fprintf(stderr, "  -e          encode input (default)\n");
    fprintf(stderr, "  -i          ignore non-alphabet characters\n");
    fprintf(stderr, "  -w COLS     wrap encoded lines at COLS (default 76)\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Encode or decode base64 data.\n");
    fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
    fprintf(stderr, "  Default: encode if no options specified.\n");
    exit(FAILURE);
}
