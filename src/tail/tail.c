/*
 * tail - Unsui POSIX runtime for Amiga
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on tail by Norbert Schlenker.
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

 /* Amiga-specific includes */
 #include <proto/dos.h>
 #include <dos/dos.h>
 #include <dos/rdargs.h>

 #include "tail.h"
#include "/common/common.h"
#include "/common/getopt.h"

 /* Version tag for Amiga */
 static const char *verstag = "$VER: tail 1.6 (23/08/25)\n";

 /* Magic numbers suggested or required by Posix specification */
 #define SUCCESS 0               /* exit code in case of success */
 #define FAILURE 1               /* or failure */
 #define DEFAULT_COUNT 10        /* default number of lines or bytes */
 #define LINE_MAX 2048           /* minimum acceptable lower bound */
 #define MIN_BUFSIZE (LINE_MAX * DEFAULT_COUNT)
 #define SLEEP_INTERVAL 1        /* sleep for one second intervals with -f */
 #define INPUT_BUFFER_SIZE 256   /* For interactive '?' mode */

 #define FALSE 0
 #define TRUE 1

 /* For ReadArgs template */
 enum {
     ARG_FILE,
     ARG_LINES,
     ARG_BYTES,
     ARG_FOLLOW,
     ARG_POSIX,
     ARG_COUNT
 };

 /* C89 FIX: Declare getopt() externs for older compilers */
 extern char *optarg;
 extern int optind;

 /* Function declarations for forward references */
 char *my_basename(char *path);
 int tail(int count, int bytes, int read_until_killed);
 int keep_reading(void);
 void usage(char *program);
 void parse_obsolescent_args(int argc, char **argv, int *cflag, int *nflag,
                            int *fflag, int *number, int *file_start);
 int run_tail_logic(int number, int cflag, int fflag, int file_count, char **files, const char *program);
 int is_getopt_style(int argc, char **argv);
 char* build_command_string(int argc, char **argv, const char* exclude);
 void parse_getopt_args(int argc, char **argv, int *cflag, int *nflag,
                        int *fflag, int *number, int *file_start, const char *program);
 int tokenize_string(char *str, char **argv, int max_args);


 /* Main function: dispatcher for parsing style */
 int main(int argc, char **argv) {
     int cflag = FALSE;
     int nflag = FALSE;
     int fflag = FALSE;
     int number = -DEFAULT_COUNT;
     int file_start = 1;
     char **files = NULL;
     char *program;

     /* ReadArgs Path Variables */
     const char *template = "FILE/M,LINES/K/N,BYTES/K/N,FOLLOW/S,POSIX/K/F";
     LONG arg_array[ARG_COUNT] = {0};
     struct RDArgs *rdargs = NULL;
     char *cmd_string = NULL;
     int ret_code = SUCCESS;
     BOOL interactive_help = FALSE;

     /* POSIX/F Path Variables */
     char *posix_str;
     int new_argc;
     char *new_argv[MAX_TEMPLATE_ITEMS]; /* Use a fixed-size array for simplicity */
     int i;
     int file_count;
     char initial_args_str[INPUT_BUFFER_SIZE];
     char user_input_buf[INPUT_BUFFER_SIZE];
     char *temp_str;
     size_t combined_len;

     if (argc < 1) {
         exit(FAILURE);
     }
     program = my_basename(argv[0]);

     if (argc == 1) {
         /* No arguments, read from stdin with default settings */
         return run_tail_logic(number, cflag, fflag, 0, NULL, program);
     }

     /* --- Logic to decide which parser to use --- */
     if (is_getopt_style(argc, argv)) {
         /* --- GETOPTS PATH --- */
         parse_getopt_args(argc, argv, &cflag, &nflag, &fflag, &number, &file_start, program);
         files = &argv[file_start];
         return run_tail_logic(number, cflag, fflag, argc - file_start, files, program);

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
                 strncpy(initial_args_str, temp_str, INPUT_BUFFER_SIZE - 1);
                 free(temp_str);
             }

             /* Print template and prompt for more input */
             printf("%s: ", template);
             fflush(stdout); /* Ensure prompt is visible before fgets */
             if (fgets(user_input_buf, sizeof(user_input_buf), stdin)) {
                 /* Combine initial args with the new user input */
                 combined_len = strlen(initial_args_str) + strlen(user_input_buf) + 2; /* space + null */
                 cmd_string = malloc(combined_len);
                 if (cmd_string) {
                     strcpy(cmd_string, initial_args_str);
                     /* Add a space if there were initial args and user typed something */
                     if (initial_args_str[0] != '\0' && user_input_buf[0] != '\n') {
                        strcat(cmd_string, " ");
                     }
                     strcat(cmd_string, user_input_buf);
                 }
             } else {
                 /* If fgets fails (e.g., EOF), just use the initial args */
                 cmd_string = strdup(initial_args_str);
                 if (cmd_string) strcat(cmd_string, "\n"); /* Ensure newline */
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

         /* This is where the concatenated string is passed to ReadArgs */
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

                 parse_getopt_args(new_argc, new_argv, &cflag, &nflag, &fflag, &number, &file_start, program);
                 files = &new_argv[file_start];
                 ret_code = run_tail_logic(number, cflag, fflag, new_argc - file_start, files, program);

             } else {
                 /* Standard ReadArgs processing */
                 if (arg_array[ARG_LINES]) {
                     number = (int)*(LONG *)arg_array[ARG_LINES];
                     if (number == 0) {
                         number = -DEFAULT_COUNT;
                     }
                 }
                 
                 if (arg_array[ARG_BYTES]) {
                     cflag = TRUE;
                     nflag = FALSE;
                 }
                 
                 if (arg_array[ARG_FOLLOW]) {
                     fflag = TRUE;
                 }
                 
                 if (arg_array[ARG_FILE]) {
                     /* Count files and allocate array */
                     file_count = 0;
                     while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
                         file_count++;
                     }
                     
                     files = (char **)arg_array[ARG_FILE];
                     ret_code = run_tail_logic(number, cflag, fflag, file_count, files, program);
                 } else {
                     /* No files specified, read from stdin */
                     ret_code = run_tail_logic(number, cflag, fflag, 0, NULL, program);
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
  * @brief The core logic for tailing files after arguments have been parsed.
  */
 int run_tail_logic(int number, int cflag, int fflag, int file_count, char **files, const char *program) {
     /* C89 FIX: All variables must be declared at the top of the block */
     int i;

     if (file_count > 0) {
         for (i = 0; i < file_count; i++) {
             if (file_count > 1) {
                 printf("==> %s <==\n", files[i]);
             }

             if (strcmp(files[i], "-") == 0) {
                  /* Reading from stdin explicitly */
                  tail(number, cflag, FALSE); /* Cannot follow stdin */
             } else {
                 if (freopen(files[i], "r", stdin) != stdin) {
                     fprintf(stderr, "%s: could not open %s\n", program, files[i]);
                     continue;
                 }
                 tail(number, cflag, fflag);
             }
             if (i < file_count - 1) {
                 printf("\n");
             }
         }
     } else {
         /* No files specified - read from stdin */
         tail(number, cflag, FALSE); /* Force -f off when reading a pipe */
     }
     return SUCCESS;
 }




 /**
  * @brief Parses arguments using the getopt and obsolescent methods.
  */
 void parse_getopt_args(int argc, char **argv, int *cflag, int *nflag,
                        int *fflag, int *number, int *file_start, const char *program)
 {
     /* C89 FIX: All variables must be declared at the top of the block */
     int opt;

     /* Check for obsolescent syntax first */
     if (argc > 1 && ((argv[1][0] == '+') ||
         (argv[1][0] == '-' && ((isdigit(argv[1][1])) ||
                                (argv[1][1] == 'l') ||
                                (argv[1][1] == 'c' && argv[1][2] == 'f'))))) {
         parse_obsolescent_args(argc, argv, cflag, nflag, fflag, number, file_start);
     } else {
         /* Standard syntax */
         *file_start = 1;
         /* Reset optind for potential re-parsing (e.g., from POSIX/F) */
         optind = 1;

         while ((opt = getopt(argc, argv, "c:fn:hV")) != -1) {
             switch (opt) {
                 case 'c':
                     *cflag = TRUE;
                     if (*optarg == '+' || *optarg == '-') {
                         *number = atoi(optarg);
                     } else if (isdigit(*optarg)) {
                         *number = -atoi(optarg);
                     } else {
                         usage((char*)program);
                     }
                     if (*number == 0) {
                         if (*optarg == '+') *number = 1; else exit(SUCCESS);
                     }
                     break;
                 case 'f':
                     *fflag = TRUE;
                     break;
                 case 'n':
                     *nflag = TRUE;
                     if (*optarg == '+' || *optarg == '-') {
                         *number = atoi(optarg);
                     } else if (isdigit(*optarg)) {
                         *number = -atoi(optarg);
                     } else {
                         usage((char*)program);
                     }
                      if (*number == 0) {
                         if (*optarg == '+') *number = 1; else exit(SUCCESS);
                     }
                     break;
                 case 'h':
                 case 'V':
                     usage((char*)program);
                     break;
                 default:
                     usage((char*)program);
             }
         }
         *file_start = optind;
     }

     /* Validate arguments */
     if (*cflag && *nflag) {
         usage((char*)program);
     }
 }


 /* --- Original functions from the provided code, with one key fix --- */

 void parse_obsolescent_args(int argc, char **argv, int *cflag, int *nflag,
                            int *fflag, int *number, int *file_start)
 {
     char *suffix;

     --argc; ++argv;
     *file_start = 2;

     if (isdigit(argv[0][1])) {
         *number = (int)strtol(argv[0], &suffix, 10);
         if (*number == 0) {
             if (argv[0][0] == '+') *number = 1; else exit(SUCCESS);
         }
     } else {
         *number = (argv[0][0] == '+') ? DEFAULT_COUNT : -DEFAULT_COUNT;
         suffix = &(argv[0][1]);
     }

     if (*suffix != '\0') {
         if (*suffix == 'c') { *cflag = TRUE; ++suffix; }
         else if (*suffix == 'l') { *nflag = TRUE; ++suffix; }
     }
     if (*suffix != '\0') {
         if (*suffix == 'f') { *fflag = TRUE; ++suffix; }
     }
     if (*suffix != '\0') {
         *number = -DEFAULT_COUNT;
         *cflag = *nflag = *fflag = FALSE;
         *file_start = 1;
     }
 }

 int tail(int count, int bytes, int read_until_killed)
 {
     int c;
     char *buf;
     char *buf_end;
     char *start;
     char *finish;
     int wrapped_once = FALSE;

     /*
      * LOGIC FIX: For a positive count (+N), we skip N-1 lines.
      */
     if (count > 0) {
         count--;
     }

     if (count >= 0) {
         /* This branch handles positive counts (from start of file) */
         while (count > 0 && (c = getchar()) != EOF) {
             if (bytes || c == '\n') {
                 --count;
             }
         }
         while ((c = getchar()) != EOF) {
             if (putchar(c) == EOF) return FAILURE;
         }
         if (read_until_killed) return keep_reading();
         return ferror(stdin) ? FAILURE : SUCCESS;
     }

     /* This branch handles negative counts (from end of file) */
     if ((buf = (char *)malloc(MIN_BUFSIZE + 1)) == (char *)NULL) {
         fprintf(stderr, "tail: out of memory\n");
         return FAILURE;
     }
     buf_end = buf + (MIN_BUFSIZE + 1);

     finish = buf;
     while ((c = getchar()) != EOF) {
         *finish++ = c;
         if (finish == buf_end) {
             finish = buf;
             wrapped_once = TRUE;
         }
     }
     if (ferror(stdin)) {
         free(buf);
         return FAILURE;
     }

     if (finish != buf || wrapped_once) {
        start = (finish == buf) ? buf_end - 1 : finish - 1;

        /*
         * LOGIC FIX: If file ends with a newline, start searching from the
         * character before it, so the final newline doesn't consume one of
         * our counts, which would result in one too few lines being shown.
         */
        if (!bytes && (finish != buf || wrapped_once) && *start == '\n') {
            if (start == buf) {
                if (wrapped_once) start = buf_end - 2;
            } else {
                start--;
            }
        }

         while (start != finish) {
             if ((bytes || *start == '\n') && ++count == 0) {
                 break;
             }
             if (start == buf) {
                 if (!wrapped_once) break;
                 start = buf_end - 1;
             } else {
                 --start;
             }
         }
         if (++start == buf_end) {
             start = buf;
         }

         if (finish > start) {
             fwrite(start, 1, finish - start, stdout);
         } else {
             fwrite(start, 1, buf_end - start, stdout);
             fwrite(buf, 1, finish - buf, stdout);
         }
     }

     free(buf);
     if (read_until_killed) return keep_reading();
     return ferror(stdout) ? FAILURE : SUCCESS;
 }


 int keep_reading(void)
 {
     int c;
     for (;;) {
         /* Amiga-specific delay (50 ticks = 1 second) */
         Delay(50 * SLEEP_INTERVAL);
         clearerr(stdin);
         while ((c = getchar()) != EOF) {
             if (putchar(c) == EOF) return FAILURE;
         }
         if (ferror(stdin)) return FAILURE;
     }
 }



 void usage(char *program)
 {
     fprintf(stderr, "Version: %s\n", &verstag[6]);
     fprintf(stderr, "Usage (POSIX): %s [-f] [-c | -n number] [file...]\n", program);
     fprintf(stderr, "Usage (Amiga): %s FILE/M [LINES/K/N] [BYTES/K/N] [FOLLOW/S]\n", program);
     fprintf(stderr, "               %s ? for template\n", program);
     fprintf(stderr, "OPTIONS:\n");
     fprintf(stderr, "  -c NUM      Output the last NUM bytes.\n");
     fprintf(stderr, "  -n NUM      Output the last NUM lines (default: %d).\n", DEFAULT_COUNT);
     fprintf(stderr, "  -f          Follow (keep reading after file grows).\n");
     fprintf(stderr, "  -h, -V      Display this help and version.\n");
     fprintf(stderr, "  NUM may have a leading '+' to count from the beginning.\n");
     exit(FAILURE);
 }


