/*
 * tee - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 1995 Ingo Wilken
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Amiga-specific includes */
#include <proto/dos.h>
#include <proto/exec.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

#include "tee.h"
#include "common.h"
#include "getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: tee 2.0 (18/09/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* Buffer configuration */
#define DEFAULT_BUFFERS     100     /* same as C:copy */
#define BUFFER_BLOCK_SIZE   512

/* For ReadArgs template */
enum {
    ARG_FILE,
    ARG_APPEND,
    ARG_IGNORE,
    ARG_BUFFER,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
void usage(char *program);
int run_tee_logic(int append_flag, int ignore_flag, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, int *append_flag, int *ignore_flag, int *file_start, const char *program);


/* Main function: dispatcher for parsing style */
int main(int argc, char **argv)
{
    int append_flag = FALSE;
    int ignore_flag = FALSE;
    int file_start = 1;
    char **files = NULL;
    char *program;
    
    /* ReadArgs Path Variables */
    const char *template = "FILE/M,APPEND/S,IGNORE/S,BUF=BUFFER/K/N,POSIX/K/F";
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
        /* No arguments, read from stdin with default settings */
        return run_tee_logic(append_flag, ignore_flag, 0, NULL, program);
    }

    /* --- Logic to decide which parser to use --- */
    if (is_getopt_style(argc, argv)) {
        /* --- GETOPTS PATH --- */
        parse_getopt_args(argc, argv, &append_flag, &ignore_flag, &file_start, program);
        files = &argv[file_start];
        return run_tee_logic(append_flag, ignore_flag, argc - file_start, files, program);
        
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

                parse_getopt_args(new_argc, new_argv, &append_flag, &ignore_flag, &file_start, program);
                files = &new_argv[file_start];
                ret_code = run_tee_logic(append_flag, ignore_flag, new_argc - file_start, files, program);

            } else {
                /* Standard ReadArgs processing */
                if (arg_array[ARG_APPEND]) {
                    append_flag = TRUE;
                }
                if (arg_array[ARG_IGNORE]) {
                    ignore_flag = TRUE;
                }
                
                if (arg_array[ARG_FILE]) {
                    /* Count files and allocate array */
                    int file_count = 0;
                    while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
                        file_count++;
                    }
                    
                    files = (char **)arg_array[ARG_FILE];
                    ret_code = run_tee_logic(append_flag, ignore_flag, file_count, files, program);
                } else {
                    /* No files specified, read from stdin */
                    ret_code = run_tee_logic(append_flag, ignore_flag, 0, NULL, program);
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
 * @param append_flag Flag to append to files
 * @param ignore_flag Flag to ignore interrupts
 * @param file_start Index where files start in argv
 * @param program Program name for error messages
 */
void parse_getopt_args(int argc, char **argv, int *append_flag, int *ignore_flag, int *file_start, const char *program) {
    int c;
    
    reset_getopt();
    
    while ((c = getopt(argc, argv, "aihV")) != -1) {
        switch (c) {
            case 'a':
                *append_flag = TRUE;
                break;
            case 'i':
                /* Ignore interrupts - no-op on Amiga as CheckSignal() is always used */
                *ignore_flag = TRUE;
                break;
            case 'h':
            case 'V':
                usage((char *)program);
                break;
            case '?':
                exit(FAILURE);
                break;
        }
    }
    
    *file_start = optind;
}

/**
 * @brief Core tee logic separated from argument parsing
 * @param append_flag Flag to append to files
 * @param ignore_flag Flag to ignore interrupts
 * @param file_count Number of files to process
 * @param files Array of file names
 * @param program Program name for error messages
 * @return Exit code
 */
int run_tee_logic(int append_flag, int ignore_flag, int file_count, char **files, const char *program) {
    UBYTE smallbuf[BUFFER_BLOCK_SIZE];
    UBYTE *buf = NULL;
    BPTR infh, outfh, teefh;
    LONG r, r2, last = 0, bufsiz = 0, err = 0;
    BOOL conmode;
    int i;
    LONG mode;
    const char console[] = "CONSOLE:";
    
    /* Check AmigaOS version requirement */
    if( SysBase->lib_Version < 37 ) {
        fprintf(stderr, "%s requires AmigaOS 2.04 or higher\n", program);
        return FAILURE;
    }

    infh = Input(); 
    outfh = Output();

    /* If no files specified, just copy stdin to stdout */
    if (file_count == 0) {
        while ((r = Read(infh, smallbuf, BUFFER_BLOCK_SIZE)) > 0) {
            if (!ignore_flag && CheckSignal(SIGBREAKF_CTRL_C)) {
                err = ERROR_BREAK;
                break;
            }
            if (Write(outfh, smallbuf, r) != r) {
                err = IoErr();
                break;
            }
        }
        if (r < 0) {
            err = IoErr();
        }
        return err ? FAILURE : SUCCESS;
    }

    /* Process each file */
    for (i = 0; i < file_count; i++) {
        /* some stream devices don't support MODE_READWRITE */
        mode = append_flag ? MODE_READWRITE : MODE_NEWFILE;
        teefh = Open(files[i] ? (STRPTR)files[i] : (STRPTR)console, mode);
        
        if (teefh) {
            if (IsInteractive(teefh)) {
                conmode = TRUE;
            } else {
                conmode = FALSE;
                bufsiz = DEFAULT_BUFFERS * BUFFER_BLOCK_SIZE;

                while (bufsiz > BUFFER_BLOCK_SIZE) {
                    buf = (UBYTE *)AllocMem(bufsiz, MEMF_ANY);
                    if (buf == NULL) {
                        bufsiz >>= 1;
                    } else {
                        break;
                    }
                }

                if (append_flag) {
                    if (Seek(teefh, 0, OFFSET_END) < 0) {
                        err = IoErr();
                    }
                }
            }
            
            if (!buf) {
                buf = smallbuf;
                bufsiz = BUFFER_BLOCK_SIZE;
            }

            /* Reset input to beginning for each file */
            Seek(infh, 0, OFFSET_BEGINNING);
            
            while (!err && (r = Read(infh, buf, bufsiz)) > 0) {
                if (!ignore_flag && CheckSignal(SIGBREAKF_CTRL_C)) {
                    err = ERROR_BREAK;
                    break;
                } else {
                    if (Write(outfh, buf, r) != r) {
                        err = IoErr();
                    } else {
                        r2 = Write(teefh, buf, r);
                        if (r2 != r) {
                            err = IoErr();
                        }
                    }
                    if (r2 > 0) {
                        last = r2;
                    }
                }
            }
            
            if (conmode && last && buf[last-1] != '\n') {
                FPutC(teefh, '\n');
            }

            Close(teefh);

            if (buf != smallbuf) {
                FreeMem(buf, bufsiz);
            }
        } else {
            /* could not open tee file */
            err = IoErr();
            fprintf(stderr, "%s: cannot open %s\n", program, files[i]);
        }
    }

    if (err) {
        /* can't use PrintFault() because it prints to stdout */
        if (Fault(err, program, smallbuf, BUFFER_BLOCK_SIZE)) {
            outfh = Open(console, MODE_READWRITE);
            if (outfh) {
                FPuts(outfh, smallbuf);
                FPutC(outfh, '\n');
                Close(outfh);
            }
        }
        SetIoErr(err);  /* restore for 'why' command */
    }

    return err ? FAILURE : SUCCESS;
}

/*
 * Display usage information
 */
void usage(char *program)
{
    fprintf(stderr, "Version: %s\n", &verstag[6]);
    fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [FILE...]\n", program);
    fprintf(stderr, "Usage (Amiga): %s FILE/M [APPEND/S] [IGNORE/S] [BUF=BUFFER/K/N]\n", program);
    fprintf(stderr, "               %s ? for template\n", program);
    fprintf(stderr, "OPTIONS:\n");
    fprintf(stderr, "  -a          append to the given files, do not overwrite\n");
    fprintf(stderr, "  -i          ignore interrupts\n");
    fprintf(stderr, "  -h, -V      display this help and version\n");
    fprintf(stderr, "DESCRIPTION:\n");
    fprintf(stderr, "  Copy standard input to each FILE, and also to standard output.\n");
    fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
    exit(FAILURE);
}

