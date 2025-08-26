/*   Shar.c

     Copyright (c) 1987 by Fabbian G. Dufoe, III
     All rights reserved.

     Permission is granted to redistribute this program provided the source
     code is included in the distribution and this copyright notice is
     unchanged.

     This program creates a Unix-compatible shell archive in the first file
     named on the command line.  It packs all the command-line files after
     the first into that archive.

     For each file to be included in the archive the program writes
          echo "Creating filename"
          cat > filename <<"***EOF filename***"
     Then it writes a copy of the file and terminates it with
          ***EOF filename***

     Enhanced for Unsui POSIX runtime with getopt and ReadArgs support.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef AMIGA
#include <error.h>
#include <exec/types.h>
#else
#include <errno.h>
#include <ctype.h>
extern int sys_nerr;
extern char *sys_errlist[];
#endif

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

#include "/common/common.h"
#include "/common/getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: shar 2.0 (23/08/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_OUTPUT,
    ARG_FILES,
    ARG_VERBOSE,
    ARG_CHECK,
    ARG_OVERWRITE,
    ARG_PREFIX,
    ARG_DELIMITER,
    ARG_POSIX,
    ARG_COUNT
};

/* Global options */
static int verbose_flag = FALSE;
static int check_flag = FALSE;
static int overwrite_flag = FALSE;
static char *prefix_string = NULL;
static char *delimiter_string = NULL;

/* Function declarations for forward references */
void usage(char *program);
void version(void);
int create_archive(char *output_file, int file_count, char **files);
int run_shar_logic(char *output_file, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, char **output_file, int *file_start, const char *program);

int main(argc, argv)
int argc;
char **argv;
{
     char *output_file = NULL;
     int file_count = 0;
     char **files = NULL;
     char *program;
     
     /* ReadArgs Path Variables */
     const char *template = "OUTPUT/M,FILES/M,VERBOSE/S,CHECK/S,OVERWRITE/S,PREFIX/K,DELIMITER/K,POSIX/K/F";
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
         exit(FAILURE);
     }

     /* --- Logic to decide which parser to use --- */
     if (is_getopt_style(argc, argv)) {
         /* --- GETOPTS PATH --- */
         parse_getopt_args(argc, argv, &output_file, &file_count, program);
         files = &argv[argc - file_count];
         return run_shar_logic(output_file, file_count, files, program);
     } else {
         /* --- READARGS PATH --- */
         if (argc == 2 && (strcmp(argv[1], "?") == 0 || strcmp(argv[1], "-?") == 0)) {
             interactive_help = TRUE;
         }
         
         if (interactive_help) {
             printf("shar - Create shell archive\n\n");
             printf("OUTPUT/M - Output archive file\n");
             printf("FILES/M - Files to archive (separate with spaces)\n");
             printf("VERBOSE/S - Verbose output\n");
             printf("CHECK/S - Add size checking\n");
             printf("OVERWRITE/S - Overwrite existing files\n");
             printf("PREFIX/K - Prefix for extracted files\n");
             printf("DELIMITER/K - Custom delimiter string\n");
             printf("POSIX/K/F - Use POSIX-style arguments\n\n");
             printf("Example: shar OUTPUT/M archive.sh FILES/M file1.txt file2.txt VERBOSE/S\n");
             return SUCCESS;
         }

         /* Build command string for ReadArgs */
         cmd_string = build_command_string(argc, argv, NULL);
         if (!cmd_string) {
             fprintf(stderr, "%s: Failed to build command string\n", program);
             exit(FAILURE);
         }

         /* Set up ReadArgs */
         rdargs = setup_readargs(template, cmd_string);
         if (!rdargs) {
             fprintf(stderr, "%s: Failed to set up ReadArgs\n", program);
             free(cmd_string);
             exit(FAILURE);
         }

         /* Parse arguments */
         if (!ReadArgs(template, arg_array, rdargs)) {
             if (IoErr() == ERROR_REQUIRED_ARG_MISSING) {
                 fprintf(stderr, "%s: Missing required arguments\n", program);
                 usage(program);
             } else {
                 fprintf(stderr, "%s: Argument parsing failed\n", program);
             }
             cleanup_readargs(rdargs, cmd_string);
             exit(FAILURE);
         }

         /* Extract arguments */
         if (arg_array[ARG_OUTPUT]) {
             output_file = (char *)arg_array[ARG_OUTPUT];
         }
         
         if (arg_array[ARG_FILES]) {
             /* Parse files string into array */
             char *files_str = (char *)arg_array[ARG_FILES];
             file_count = tokenize_string(files_str, new_argv, MAX_TEMPLATE_ITEMS);
             files = (char **)malloc(file_count * sizeof(char *));
             if (!files) {
                 fprintf(stderr, "%s: Memory allocation failed\n", program);
                 cleanup_readargs(rdargs, cmd_string);
                 exit(FAILURE);
             }
             for (i = 0; i < file_count; i++) {
                 files[i] = strdup(new_argv[i]);
             }
         }

         /* Extract flags */
         verbose_flag = arg_array[ARG_VERBOSE] ? TRUE : FALSE;
         check_flag = arg_array[ARG_CHECK] ? TRUE : FALSE;
         overwrite_flag = arg_array[ARG_OVERWRITE] ? TRUE : FALSE;
         
         if (arg_array[ARG_PREFIX]) {
             prefix_string = strdup((char *)arg_array[ARG_PREFIX]);
         }
         
         if (arg_array[ARG_DELIMITER]) {
             delimiter_string = strdup((char *)arg_array[ARG_DELIMITER]);
         }

         /* Check for POSIX escape hatch */
         if (arg_array[ARG_POSIX]) {
             posix_str = (char *)arg_array[ARG_POSIX];
             
             /* Build new argv for getopt parsing */
             new_argv[0] = program;
             new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;
             
             /* Parse with getopt */
             parse_getopt_args(new_argc, new_argv, &output_file, &file_count, program);
             files = &new_argv[optind];
             
             ret_code = run_shar_logic(output_file, file_count, files, program);
             
             /* Clean up */
             cleanup_readargs(rdargs, cmd_string);
             return ret_code;
         }

         /* Validate required arguments */
         if (!output_file || file_count == 0) {
             fprintf(stderr, "%s: Missing required arguments\n", program);
             usage(program);
             cleanup_readargs(rdargs, cmd_string);
             exit(FAILURE);
         }

         ret_code = run_shar_logic(output_file, file_count, files, program);

         /* Clean up */
         if (files) {
             for (i = 0; i < file_count; i++) {
                 free(files[i]);
             }
             free(files);
         }
         cleanup_readargs(rdargs, cmd_string);
         return ret_code;
     }
}

/* Parse getopt-style arguments */
void parse_getopt_args(int argc, char **argv, char **output_file, int *file_count, const char *program)
{
     int c;
     int file_start;
     
     reset_getopt();
     
     while ((c = getopt(argc, argv, "vcohp:d:")) != -1) {
         switch (c) {
             case 'v':
                 verbose_flag = TRUE;
                 break;
             case 'c':
                 check_flag = TRUE;
                 break;
             case 'o':
                 overwrite_flag = TRUE;
                 break;
             case 'h':
                 usage(program);
                 exit(SUCCESS);
                 break;
             case 'p':
                 if (prefix_string) free(prefix_string);
                 prefix_string = strdup(optarg);
                 break;
             case 'd':
                 if (delimiter_string) free(delimiter_string);
                 delimiter_string = strdup(optarg);
                 break;
             case '?':
                 usage(program);
                 exit(FAILURE);
                 break;
             default:
                 usage(program);
                 exit(FAILURE);
         }
     }
     
     file_start = optind;
     *file_count = argc - file_start;
     
     if (*file_count < 2) {
         fprintf(stderr, "%s: Missing output file and input files\n", program);
         usage(program);
         exit(FAILURE);
     }
     
     *output_file = argv[file_start];
     *file_count = *file_count - 1; /* Exclude output file from count */
}

/* Main shar logic */
int run_shar_logic(char *output_file, int file_count, char **files, const char *program)
{
     int i;

     if (verbose_flag) {
         printf("Creating shell archive: %s\n", output_file);
         printf("Files to archive: %d\n", file_count);
         for (i = 0; i < file_count; i++) {
             printf("  %s\n", files[i]);
         }
     }
     
     return create_archive(output_file, file_count, files);
}

/* Create the shell archive */
int create_archive(char *output_file, int file_count, char **files)
{
     int i;
     FILE *in;
     FILE *out;
     long t;
     char *delimiter = delimiter_string ? delimiter_string : "***EOF";
     
     /* Check if output file exists and handle overwrite flag */
     if ((out = fopen(output_file, "r")) != NULL) {
         if (!overwrite_flag) {
             fprintf(stderr, "shar: %s already exists. Use -o to overwrite.\n", output_file);
             fclose(out);
             return FAILURE;
         }
         fclose(out);
     }
     
     /* Open output file for writing */
     if ((out = fopen(output_file, "w")) == NULL) {
         fprintf(stderr, "shar: Couldn't open %s for output.\n", output_file);
         if (errno > 0 && errno < sys_nerr) {
             fprintf(stderr, "\t%d: %s\n", errno, sys_errlist[errno]);
         }
         return FAILURE;
     }

     /* Write header comments */
     time(&t);
     fprintf(out, "# This is a shell archive. Remove anything before this line,\n");
     fprintf(out, "# then unpack it by saving it in a file and typing \"unshar file\"\n");
     fprintf(out, "# Created %s", ctime(&t));
     fprintf(out, "# This archive contains:\n");
     
     for (i = 0; i < file_count; i++) {
         fprintf(out, "#\t\t%s\n", files[i]);
     }
     
     if (check_flag) {
         fprintf(out, "# Size verification enabled\n");
     }
     if (prefix_string) {
         fprintf(out, "# Files will be extracted with prefix: %s\n", prefix_string);
     }
     fprintf(out, "\n");

     /* Process each input file */
     for (i = 0; i < file_count; i++) {
          int c;

         if (verbose_flag) {
             printf("Processing: %s\n", files[i]);
         }
         
         if ((in = fopen(files[i], "r")) == NULL) {
             fprintf(stderr, "shar: couldn't open %s for input.\n", files[i]);
             if (errno > 0 && errno < sys_nerr) {
                 fprintf(stderr, "\t%d: %s\n", errno, sys_errlist[errno]);
             }
             continue;
         }
         
         /* Write file header */
         fprintf(out, "echo \"Creating %s\"\n", files[i]);
         
         if (prefix_string) {
             fprintf(out, "cat > %s%s <<\"%s %s\"\n", prefix_string, files[i], delimiter, files[i]);
         } else {
             fprintf(out, "cat > %s <<\"%s %s\"\n", files[i], delimiter, files[i]);
         }
         
         /* Copy file contents */
         
         while ((c = fgetc(in)) != EOF) {
             fputc(c, out);
         }
         
         /* Write file footer */
         fprintf(out, "%s %s\n\n", delimiter, files[i]);
         
         fclose(in);
         
         if (verbose_flag) {
             printf("Added: %s\n", files[i]);
         }
     }
     
     fclose(out);
     
     if (verbose_flag) {
         printf("Archive created successfully: %s\n", output_file);
     }
     
     return SUCCESS;
}

/* Display usage information */
void usage(char *program)
{
     fprintf(stderr, "Version: %s\n", &verstag[6]);
     fprintf(stderr, "Usage (POSIX): %s [OPTIONS] output_file file...\n", program);
     fprintf(stderr, "Usage (Amiga): %s OUTPUT/M FILES/M [VERBOSE/S] [CHECK/S] [OVERWRITE/S] [PREFIX/K] [DELIMITER/K]\n", program);
     fprintf(stderr, "               %s ? for template\n", program);
     fprintf(stderr, "OPTIONS:\n");
     fprintf(stderr, "  -v          verbose output\n");
     fprintf(stderr, "  -c          add size checking to archive\n");
     fprintf(stderr, "  -o          overwrite existing output file\n");
     fprintf(stderr, "  -p STR      prefix for extracted files\n");
     fprintf(stderr, "  -d STR      custom delimiter string\n");
     fprintf(stderr, "  -h          display this help\n");
     fprintf(stderr, "DESCRIPTION:\n");
     fprintf(stderr, "  Create a shell archive containing the specified files.\n");
     fprintf(stderr, "  The archive is a self-extracting shell script that can\n");
     fprintf(stderr, "  be run on any POSIX-compliant system.\n");
     exit(FAILURE);
}

/* Display version information */
void version(void)
{
     printf("%s", verstag);
}
