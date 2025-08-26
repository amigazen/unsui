/* Sh.c

   Copyright (c) 1987 by F. G. Dufoe, III
   All rights reserved.

   Permission is granted to redistribute this program provided the source
   code is included in the distribution and this copyright notice is
   unchanged.

   Enhanced for Unsui POSIX runtime with getopt and ReadArgs support.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Token.h"

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

#include "/common/common.h"
#include "/common/getopt.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: unshar 2.0 (23/08/25)\n";

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* For ReadArgs template */
enum {
    ARG_ARCHIVE,
    ARG_VERBOSE,
    ARG_OVERWRITE,
    ARG_PREFIX,
    ARG_POSIX,
    ARG_COUNT
};

/* Global options */
static int verbose_flag = FALSE;
static int overwrite_flag = FALSE;
static char *prefix_string = NULL;

/* Function declarations for forward references */
void usage(char *program);
void version(void);
int extract_archive(char *archive_file);
int run_unshar_logic(char *archive_file, const char *program);
void parse_getopt_args(int argc, char **argv, char **archive_file, const char *program);

int main(argc, argv)
   /* This program unpacks a shell archive created by Shar. If the user
      typed more than one file name or typed a question mark on the command
      line the program prints an error message and terminates. Otherwise it
      tries to open the specified file for input. If it cannot open the
      file it prints an error message and terminates. */
int argc;
   /* This is the number of arguments on the command line. */
char **argv;
   /* This points to a character array containing the argument values. */
{
   char *archive_file = NULL;
   char *program;
   
   /* ReadArgs Path Variables */
   const char *template = "ARCHIVE/M,VERBOSE/S,OVERWRITE/S,PREFIX/K,POSIX/K/F";
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
       parse_getopt_args(argc, argv, &archive_file, program);
       return run_unshar_logic(archive_file, program);
   } else {
       /* --- READARGS PATH --- */
       if (argc == 2 && (strcmp(argv[1], "?") == 0 || strcmp(argv[1], "-?") == 0)) {
           interactive_help = TRUE;
       }
       
       if (interactive_help) {
           printf("unshar - Extract shell archive\n\n");
           printf("ARCHIVE/M - Archive file to extract\n");
           printf("VERBOSE/S - Verbose output\n");
           printf("OVERWRITE/S - Overwrite existing files\n");
           printf("PREFIX/K - Prefix for extracted files\n");
           printf("POSIX/K/F - Use POSIX-style arguments\n\n");
           printf("Example: unshar ARCHIVE/M archive.sh VERBOSE/S\n");
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
       if (arg_array[ARG_ARCHIVE]) {
           archive_file = (char *)arg_array[ARG_ARCHIVE];
       }

       /* Extract flags */
       verbose_flag = arg_array[ARG_VERBOSE] ? TRUE : FALSE;
       overwrite_flag = arg_array[ARG_OVERWRITE] ? TRUE : FALSE;
       
       if (arg_array[ARG_PREFIX]) {
           prefix_string = strdup((char *)arg_array[ARG_PREFIX]);
       }

       /* Check for POSIX escape hatch */
       if (arg_array[ARG_POSIX]) {
           posix_str = (char *)arg_array[ARG_POSIX];
           
           /* Build new argv for getopt parsing */
           new_argv[0] = program;
           new_argc = tokenize_string(posix_str, &new_argv[1], MAX_TEMPLATE_ITEMS - 1) + 1;
           
           /* Parse with getopt */
           parse_getopt_args(new_argc, new_argv, &archive_file, program);
           
           ret_code = run_unshar_logic(archive_file, program);
           
           /* Clean up */
           cleanup_readargs(rdargs, cmd_string);
           return ret_code;
       }

       /* Validate required arguments */
       if (!archive_file) {
           fprintf(stderr, "%s: Missing required arguments\n", program);
           usage(program);
           cleanup_readargs(rdargs, cmd_string);
           exit(FAILURE);
       }

       ret_code = run_unshar_logic(archive_file, program);

       /* Clean up */
       cleanup_readargs(rdargs, cmd_string);
       return ret_code;
   }
}

/* Parse getopt-style arguments */
void parse_getopt_args(int argc, char **argv, char **archive_file, const char *program)
{
   int c;
   
   reset_getopt();
   
   while ((c = getopt(argc, argv, "vohp:")) != -1) {
       switch (c) {
           case 'v':
               verbose_flag = TRUE;
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
           case '?':
               usage(program);
               exit(FAILURE);
               break;
           default:
               usage(program);
               exit(FAILURE);
       }
   }
   
   if (optind >= argc) {
       fprintf(stderr, "%s: Missing archive file\n", program);
       usage(program);
       exit(FAILURE);
   }
   
   *archive_file = argv[optind];
}

/* Main unshar logic */
int run_unshar_logic(char *archive_file, const char *program)
{
   if (verbose_flag) {
       printf("Extracting shell archive: %s\n", archive_file);
       if (prefix_string) {
           printf("Files will be extracted with prefix: %s\n", prefix_string);
       }
   }
   
   return extract_archive(archive_file);
}

/* Extract the shell archive */
int extract_archive(char *archive_file)
{
   char ehd[512];
      /* When we find a token which identifies the end of the "here
         document" we'll store it here. */
   int ehdl;
      /* This is the length of the "end here document" string. */
   char errmsg[256];
      /* We'll use this character array to put together error messages which
         contain variables. */
   char *fgetline(FILE *);
      /* This function gets the next line of text from a file. */
   FILE *ifile;
      /* This FILE pointer identifies the input file we want to read with
         fgetline(). */
   char *line;
      /* This is the pointer to the line of text returned by fgetline(). */
   int linelen;
      /* This is the length of the line returned by fgetline(). */
   enum
   {
      NEUTRAL,
      COPY
   } mode = NEUTRAL;
      /* In NEUTRAL mode the program reads through the file looking for
         "echo" and "cat" commands and their arguments.  In COPY mode it
         writes each line to an output file until it encounters the text
         string marking the end of the "here document". */
   FILE *ofile;
      /* This file pointer identifies the current output file. */
   struct Token *parse(char *, char *);
      /* This function parses a line of text. */
   TOKEN state;
      /* State is the token type of the previous token. */
   char *text = NULL;
      /* This points to the text buffer where the token text will be
         stored. */
   int textlen = 0;
      /* This is the length of the buffer allocated for token text. */
   struct Token *token;
      /* This points to the list of Token structures returned by parse(). */

   if ((ifile = fopen(archive_file, "r")) == NULL)
   {
      sprintf(errmsg, "unshar: Can't open %s for input.", archive_file);
      fatal(errmsg);
   }
      /* If we can't open the input file print an error message and
         terminate. */

   while ((line = fgetline(ifile)) != NULL)
      /* Read the file, line by line, until we get to the end. */
   {
      switch (mode)
      {
      case NEUTRAL:
         if (textlen < (linelen = strlen(line)))
            /* If the line is longer than the text buffer we already have
               let's get a new one. */
         {
            if (text != NULL)
               free(text);
                  /* If we had a text buffer allocated we must free it
                     before we allocate another one. */
            if ((text = malloc(linelen)) == NULL)
               /* If we couldn't allocate memory for the token text print an
                  error message and terminate. */
               fatal("unshar: Couldn't allocate buffer for token text.");
            textlen = linelen;
               /* Remember the new text buffer length. */
         }
         token = parse(line, text);
            /* Break the line down into tokens. */
         switch (token->type)
         {
         case T_CAT:
            while (token->next != NULL)
            {
               token = token->next;
               switch (token->type)
               {
               case T_GT:
                  state = T_GT;
                  continue;
               case T_LTLT:
                  state = T_LTLT;
                  continue;
               case T_WORD:
                  switch (state)
                  {
                  case T_GT:
                     {
                        char *output_filename = token->text;
                        if (prefix_string) {
                           char *prefixed_name = malloc(strlen(prefix_string) + strlen(output_filename) + 1);
                           if (prefixed_name) {
                              strcpy(prefixed_name, prefix_string);
                              strcat(prefixed_name, output_filename);
                              output_filename = prefixed_name;
                           }
                        }
                        
                        /* Check if file exists and handle overwrite flag */
                        if ((ofile = fopen(output_filename, "r")) != NULL) {
                           if (!overwrite_flag) {
                              if (verbose_flag) {
                                 printf("Skipping existing file: %s (use -o to overwrite)\n", output_filename);
                              }
                              fclose(ofile);
                              ofile = NULL;
                              state = T_WORD;
                              continue;
                           }
                           fclose(ofile);
                        }
                        
                        if ((ofile = fopen(output_filename, "w")) == NULL)
                        {
                           sprintf(errmsg, "unshar: Can't open %s for output.", output_filename);
                           fatal(errmsg);
                        }
                        
                        if (verbose_flag) {
                           printf("Extracting: %s\n", output_filename);
                        }
                        
                        if (prefix_string && output_filename != token->text) {
                           free(output_filename);
                        }
                     }
                     state = T_WORD;
                     continue;
                  case T_LTLT:
                     strcpy(ehd, token->text);
                     ehdl = strlen(ehd);
                     mode = COPY;
                     state = T_WORD;
                     continue;
                  }
               }
            }
            continue;
         case T_ECHO:
            while (token->next != NULL)
            {
               token = token->next;
               if (verbose_flag) {
                  printf("%s ", token->text);
               }
            }
            if (verbose_flag) {
               printf("\n");
            }
            continue;
         }
         continue;
      case COPY:
         if (strncmp(line, ehd, ehdl) == 0)
            /* This line marks the "here document" end. */
         {
            mode = NEUTRAL;
            if (ofile) {
               fclose(ofile);
               ofile = NULL;
            }
            continue;
         }
         if (ofile) {
            fprintf(ofile, "%s", line);
         }
         continue;
      }
   }
   parse(NULL, NULL);
   fclose(ifile);
   
   if (verbose_flag) {
      printf("Archive extraction completed successfully\n");
   }
   
   return SUCCESS;
}

/* Display usage information */
void usage(char *program)
{
   fprintf(stderr, "Version: %s\n", &verstag[6]);
   fprintf(stderr, "Usage (POSIX): %s [OPTIONS] archive_file\n", program);
   fprintf(stderr, "Usage (Amiga): %s ARCHIVE/M [VERBOSE/S] [OVERWRITE/S] [PREFIX/K]\n", program);
   fprintf(stderr, "               %s ? for template\n", program);
   fprintf(stderr, "OPTIONS:\n");
   fprintf(stderr, "  -v          verbose output\n");
   fprintf(stderr, "  -o          overwrite existing files\n");
   fprintf(stderr, "  -p STR      prefix for extracted files\n");
   fprintf(stderr, "  -h          display this help\n");
   fprintf(stderr, "DESCRIPTION:\n");
   fprintf(stderr, "  Extract files from a shell archive created by shar.\n");
   fprintf(stderr, "  The archive is parsed and files are extracted to the\n");
   fprintf(stderr, "  current directory or with the specified prefix.\n");
   exit(FAILURE);
}

/* Display version information */
void version(void)
{
   printf("%s", verstag);
}
