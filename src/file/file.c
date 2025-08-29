/*
 * file - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on file by Edwin Hoogerbeets and Gary Duncan.
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
 
 #include "file.h"
 #include "/common/common.h"
 #include "/common/getopt.h"
 
 /* Version tag for Amiga */
 static const char *verstag = "$VER: file 2.0 (23/08/25)\n";
 
 /* Magic numbers for binary files */
 BPATTERN bmagic[] = {
     0, 4, "Amiga load file", "\x00\x00\x03\xf3",
     0, 4, "Amiga object file", "\x00\x00\x03\xe7",
     0, 2, "Amiga run-time library", "\xec\x62",
     0, 2, "Manx 3.6 run-time library", "\x61\x6a",
     0, 2, "Manx 3.6 object file", "\x41\x4a",
     0, 2, "Manx 3.4 object file", "\x6a\x67",
     0, 27, "TeX device independent output file",
     "\xf7\x02\x01\x83\x92\xc0"
     "\x1c\x3b\x00\x00\x00\x00"
     "\x03\xe8\x1b\x20\x54\x65"
     "\x78\x20\x6f\x75\x74\x70"
     "\x75\x74\x20",
     0, 2, "Amiga icon .info file", "\xe3\x10",
     0, 4, "Amiga .info file", "\xf3\x4c\x00\x12",
     0, 2, "Amiga .font file", "\x0f\x00",
     0, 2, "SEA ARC compressed archive", "\x1a\x08",
     2, 5, "lharc archive (lh0 - uncompressed)", "-lh0-",
     2, 5, "lharc archive (lh1 compression)", "-lh1-",
     2, 5, "lz archive (lh5 compression)", "-lh5-",
     0, 3, "GIF file", "GIF",
     NULL, NULL, NULL, NULL,
 };
 
 /* Magic tokens for ASCII files */
 PATTERN amagic[] = {
     2, "UNIX sh script file", "#!",
     4, "AmigaDos execute script file", ".key",
     4, "AmigaDos execute script file", ".bra",
     4, "AmigaDos execute script file", ".ket",
     NULL, NULL, NULL,
 };
 
 /* Patterns to search for in ASCII files */
 PATTERN asearch[] = {
     14, "LaTeX source code", "\\documentstyle",
     6, "TeX source code", "\n\\",
     2, "C++ source code", "//",
     2, "C++ source code", "::",
     8, "C++ source code", "iostream.h ",
     5, "C++ source code", "public:",
     2, "C source code", "/*",
     2, "C source code", "{\n",
     8, "C source code", "\ntypedef",
     4, "C source code", "int ",
     5, "C source code", "\n#inc",
     5, "C source code", "\n#def",
     5, "68K ASM source", "\tjsr",
     3, "68K ASM source", "\tlea",
     6, "68K ASM source", "move.l",
     2, "PASCAL source code", ":=",
     21, "Modula II source code", "IMPLEMENTATION MODULE",
     17, "Modula II source code", "DEFINITION MODULE",
     1, "yacc input file", "\n%TOKEN",
     1, "yacc or lex input file", "\n%%",
     1, "shell commands", "\nalias",
     1, "shell commands", "\nAlias",
     1, "shell commands", "\nset",
     1, "commands text", "\nrun",
     1, "commands text", "\nRun",
     1, "uuencoded file", "\nbegin ",
     NULL, NULL, NULL,
 };
 
 /* IFF form patterns */
 PATTERN IFFforms[] = {
     4, "IFF interleave bit map file", "ILBM",
     4, "IFF Amiga compressed bit map files", "ACBM",
     4, "IFF anim format file", "ANIM",
     4, "IFF instrument file", "8SVX",
     4, "IFF simple music file", "SMUS",
     NULL, NULL, NULL,
 };
 
 /* Compression patterns */
 PATTERN compress = {
     2, "block compressed %d bit code data", "\x1f\x9d",
 };
 
 /* Archive patterns */
 PATTERN zoo = {
     4, "Zoo archive", "ZOO ",
 };
 
 /* Function declarations for forward references */
 void usage(char *program);
 int run_file_logic(int mime_flag, int ignore_case_flag, int preserve_date_flag, 
                    int file_count, char **files, const char *program);
 void parse_getopt_args(int argc, char **argv, int *mime_flag, int *ignore_case_flag, 
                        int *preserve_date_flag, int *file_start, const char *program);
 
 /* Main function: dispatcher for parsing style */
 int main(int argc, char **argv)
 {
     int mime_flag = FALSE;
     int ignore_case_flag = FALSE;
     int preserve_date_flag = FALSE;
     int file_start = 1;
     char **files = NULL;
     char *program;
     
     /* ReadArgs Path Variables */
     const char *template = "FILE/M,MIME/S,IGNORE_CASE/S,PRESERVE_DATE/S,POSIX/K/F";
     LONG arg_array[ARG_COUNT] = {0};
     struct RDArgs *rdargs = NULL;
     char *cmd_string = NULL;
     int ret_code = SUCCESS;
     BOOL interactive_help = FALSE;
     
     /* POSIX/F Path Variables */
     char *posix_str;
     int new_argc;
     char *new_argv[64];  /* Use fixed size array */
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
         return FAILURE;
     }
 
     /* --- Logic to decide which parser to use --- */
     if (is_getopt_style(argc, argv)) {
         /* --- GETOPTS PATH --- */
         parse_getopt_args(argc, argv, &mime_flag, &ignore_case_flag, &preserve_date_flag, &file_start, program);
         files = &argv[file_start];
         return run_file_logic(mime_flag, ignore_case_flag, preserve_date_flag, argc - file_start, files, program);
         
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
                 new_argc = tokenize_string(posix_str, &new_argv[1], 63) + 1;
 
                 parse_getopt_args(new_argc, new_argv, &mime_flag, &ignore_case_flag, &preserve_date_flag, &file_start, program);
                 files = &new_argv[file_start];
                 ret_code = run_file_logic(mime_flag, ignore_case_flag, preserve_date_flag, new_argc - file_start, files, program);
 
             } else {
                 /* Standard ReadArgs processing */
                 if (arg_array[ARG_MIME_TYPE]) {
                     mime_flag = TRUE;
                 }
                 if (arg_array[ARG_IGNORE_CASE]) {
                     ignore_case_flag = TRUE;
                 }
                 if (arg_array[ARG_PRESERVE_DATE]) {
                     preserve_date_flag = TRUE;
                 }
                 
                 if (arg_array[ARG_FILE]) {
                     /* Count files and allocate array */
                     int file_count = 0;
                     while (((char **)arg_array[ARG_FILE])[file_count] != NULL) {
                         file_count++;
                     }
                     
                     files = (char **)arg_array[ARG_FILE];
                     ret_code = run_file_logic(mime_flag, ignore_case_flag, preserve_date_flag, file_count, files, program);
                 } else {
                     /* No files specified, show usage */
                     usage(program);
                     ret_code = FAILURE;
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
  * @param mime_flag Flag for MIME type output
  * @param ignore_case_flag Flag to ignore case
  * @param preserve_date_flag Flag to preserve dates
  * @param file_start Index where files start in argv
  * @param program Program name for error messages
  */
 void parse_getopt_args(int argc, char **argv, int *mime_flag, int *ignore_case_flag, 
                        int *preserve_date_flag, int *file_start, const char *program) {
     int c;
     
     reset_getopt();
     
     while ((c = getopt(argc, argv, "mihvV")) != -1) {
         switch (c) {
             case 'm':
                 *mime_flag = TRUE;
                 break;
             case 'i':
                 *ignore_case_flag = TRUE;
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
  * @brief Core file logic separated from argument parsing
  * @param mime_flag Flag for MIME type output
  * @param ignore_case_flag Flag to ignore case
  * @param preserve_date_flag Flag to preserve dates
  * @param file_count Number of files to process
  * @param files Array of file names
  * @param program Program name for error messages
  * @return Exit code
  */
 int run_file_logic(int mime_flag, int ignore_case_flag, int preserve_date_flag, 
                    int file_count, char **files, const char *program) {
     int i;
     
     if (file_count == 0) {
         fprintf(stderr, "%s: no files specified\n", program);
         return FAILURE;
     }
 
     /* Process each file */
     for (i = 0; i < file_count; i++) {
         filetype((char *)program, files[i]);
     }
     
     return SUCCESS;
 }
 
 /**
  * @brief Determine file type and display result
  * @param myname Program name
  * @param filename File to examine
  */
 void filetype(char *myname, char *filename)
 {
     BPTR lock;
     struct FileInfoBlock *fib;
 
     if (lock = Lock(filename, ACCESS_READ)) {
         if (fib = (struct FileInfoBlock *)AllocMem(sizeof(struct FileInfoBlock), MEMF_CLEAR)) {
             Examine(lock, fib);
             dofile(myname, filename, fib);
             UnLock(lock);
             FreeMem(fib, sizeof(struct FileInfoBlock));
         } else {
             UnLock(lock);
             fprintf(stderr, "%s: not enough memory!\n", myname);
         }
     } else {
         fprintf(stderr, "%s: could not access file %s\n", myname, filename);
     }
 }
 
 /**
  * @brief Process a single file
  * @param myname Program name
  * @param filename File to examine
  * @param fib File info block
  */
 void dofile(char *myname, char *filename, struct FileInfoBlock *fib)
 {
     char *f = &fib->fib_FileName[0];
 
     if (fib->fib_DirEntryType > 0) {
         type("directory", f);
     } else if (fib->fib_Size == 0) {
         type("empty", f);
     } else {
         char *buf;
         BPTR filehandle;
 
         if (!(filehandle = Open(filename, MODE_OLDFILE))) {
             fprintf(stderr, "%s: could not open file %s\n", myname, filename);
             return;
         }
 
         if (!(buf = (char *)AllocMem(BLOCKSIZE + 1, MEMF_PUBLIC))) {
             fprintf(stderr, "%s: not enough memory\n", myname);
             Close(filehandle);
             return;
         }
 
         if (!Read(filehandle, buf, BLOCKSIZE)) {
             fprintf(stderr, "%s: read error on file %s\n", myname, f);
             FreeMem(buf, BLOCKSIZE + 1);
             Close(filehandle);
             return;
         }
 
         matchtype(myname, buf, f, fib);
         Close(filehandle);
         FreeMem(buf, BLOCKSIZE + 1);
     }
 }
 
 /**
  * @brief Display file type information
  * @param ty Type string
  * @param name File name
  */
 void type(char *ty, char *name)
 {
     printf("%s: %s\n", name, ty);
 }
 
 /**
  * @brief Memory comparison function
  * @param a First buffer
  * @param b Second buffer
  * @param length Length to compare
  * @return Comparison result
  */
 int memncmp(char *a, char *b, int length)
 {
     int j;
 
     for (j = 0; j < length; j++) {
         if (a[j] != b[j]) {
             return (a[j] > b[j] ? -1 : 1);
         }
     }
     return 0;
 }
 
 /**
  * @brief Reverse strpbrk function
  * @param str String to search
  * @param charset Character set
  * @return Pointer to last occurrence or NULL
  */
 char *strrpbrk(char *str, char *charset)
 {
     char *temp;
     temp = str + strlen(str) - 1;
 
     while (temp != (str - 1) && !strchr(charset, *temp)) {
         --temp;
     }
 
     return ((temp != (str - 1)) ? temp : NULL);
 }
 
 /**
  * @brief Extract basename from path
  * @param buf Path string
  * @return Basename
  */
 char *basename(char *buf)
 {
     char *foo = strrpbrk(buf, ":/");
     return (foo ? (foo + 1) : buf);
 }
 
 /**
  * @brief Check if buffer contains text
  * @param buf Buffer to check
  * @param len Length of buffer
  * @return TRUE if text, FALSE otherwise
  */
 int istextfile(unsigned char *buf, int len)
 {
     int j;
     int bad_cnt = 0;
 
     for (j = 0; j < (len - 1); j++, buf++) {
         if (*buf == 0xA9) continue; /* Copyright */
 
         if (!isascii(*buf)) {
             return FALSE;
         } else {
             switch (*buf) {
                 case '\n':
                 case '\r':
                 case '\f':
                 case '\t':
                 case '\b':
                 case '\0':
                     break;
                 default:
                     if (isprint(*buf)) break;
                     else ++bad_cnt;
                     break;
             }
         }
     }
     return (bad_cnt ? FALSE : TRUE);
 }
 
 /**
  * @brief Match file type based on content
  * @param myname Program name
  * @param buf File buffer
  * @param file File name
  * @param fib File info block
  */
 void matchtype(char *myname, char *buf, char *file, struct FileInfoBlock *fib)
 {
     int j;
     int len = (fib->fib_Size < BLOCKSIZE) ? fib->fib_Size : BLOCKSIZE;
 
     /* Check magic strings (characters which start a line) */
     j = 0;
     while (amagic[j].length) {
         if (!memncmp(amagic[j].pattern, buf, amagic[j].length)) {
             type(amagic[j].name, file);
             return;
         }
         ++j;
     }
 
     /* See if a text file, or not */
     if (istextfile(buf, len) == TRUE) {
         /* Check for tell-tale strings */
         j = 0;
         while (asearch[j].length) {
             if (search(asearch[j].pattern, buf, len) == TRUE) {
                 type(asearch[j].name, file);
                 return;
             }
             ++j;
         }
 
         /* Check script bit */
         if (fib->fib_Protection & (1 << 6)) {
             type("AmigaDos script file", file);
             return;
         }
 
         type("text", file);
     } else {
         /* Deemed not an ASCII text file */
         j = 0;
 
         /* Check magic numbers */
         while (bmagic[j].length) {
             if (!memncmp(bmagic[j].pattern, buf + bmagic[j].offset, bmagic[j].length)) {
                 type(bmagic[j].name, file);
                 return;
             }
             ++j;
         }
 
         /* Check for IFF forms -- assume FORM is first header block */
         if (!memncmp("FORM", buf, 4)) {
             char buffer[40];
             j = 0;
             buffer[0] = '\0';
 
             while (IFFforms[j].length) {
                 if (!memncmp(IFFforms[j].pattern, &buf[8], IFFforms[j].length)) {
                     type(IFFforms[j].name, file);
                     return;
                 }
                 ++j;
             }
             sprintf(buffer, "IFF form %c%c%c%c", buf[8], buf[9], buf[10], buf[11]);
             type(buffer, file);
             return;
         }
 
         if (!memncmp(compress.pattern, buf, compress.length)) {
             char buffer[40];
             sprintf(buffer, compress.name, buf[2] & 0x0f);
             type(buffer, file);
             return;
         }
 
         if (!memncmp(zoo.pattern, buf, zoo.length)) {
             /* Make a string out of ZOO x.xx Archive\0 to print out */
             buf[16] = '\0';
             type(buf, file);
             return;
         }
 
         type("binary file", file);
     }
 }
 
 /**
  * @brief Search for pattern in text
  * @param pat Pattern to search for
  * @param text Text to search in
  * @param len Length of text
  * @return TRUE if found, FALSE otherwise
  */
 int search(char *pat, char *text, int len)
 {
     int j = 0;
     int patlen = strlen(pat);
 
     while (memncmp(pat, &text[j], patlen) && (j < len)) {
         j++;
     }
 
     return (j < len ? TRUE : FALSE);
 }
 
 /**
  * @brief Display usage information
  * @param program Program name
  */
 void usage(char *program)
 {
     fprintf(stderr, "Version: %s\n", &verstag[6]);
     fprintf(stderr, "Usage (POSIX): %s [OPTIONS] [FILE...]\n", program);
     fprintf(stderr, "Usage (Amiga): %s FILE/M [MIME/S] [IGNORE_CASE/S] [PRESERVE_DATE/S]\n", program);
     fprintf(stderr, "               %s ? for template\n", program);
     fprintf(stderr, "OPTIONS:\n");
     fprintf(stderr, "  -m          output MIME type (e.g., text/plain)\n");
     fprintf(stderr, "  -i          ignore case in pattern matching\n");
     fprintf(stderr, "  -h, -V      display this help and version\n");
     fprintf(stderr, "DESCRIPTION:\n");
     fprintf(stderr, "  Determine file type by examining file contents.\n");
     fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
     exit(FAILURE);
 }
 