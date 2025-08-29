/*
 * file.h - Header for file command with hybrid getopt/ReadArgs support
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

#ifndef FILE_H
#define FILE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <libraries/dosextens.h>
#include <proto/dos.h>
#include <proto/exec.h>

/* Common constants */
#define FALSE 0
#define TRUE 1
#define SUCCESS 0
#define FAILURE 1

/* Magic numbers suggested or required by Posix specification */
#define BLOCKSIZE 512

/* For ReadArgs template */
enum {
    ARG_FILE,
    ARG_MIME_TYPE,
    ARG_IGNORE_CASE,
    ARG_PRESERVE_DATE,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations */
int main(int argc, char **argv);
void parse_getopt_args(int argc, char **argv, int *mime_flag, int *ignore_case_flag, 
                       int *preserve_date_flag, int *file_start, const char *program);
int run_file_logic(int mime_flag, int ignore_case_flag, int preserve_date_flag, 
                   int file_count, char **files, const char *program);
void filetype(char *myname, char *filename);
void dofile(char *myname, char *filename, struct FileInfoBlock *fib);
void matchtype(char *myname, char *buf, char *file, struct FileInfoBlock *fib);
void type(char *ty, char *name);
int memncmp(char *a, char *b, int length);
char *strrpbrk(char *str, char *charset);
char *basename(char *buf);
int istextfile(unsigned char *buf, int len);
int search(char *pat, char *text, int len);
void usage(char *program);

/* Type definitions */
typedef struct FileLock FILELOCK;
typedef struct FileInfoBlock FILEINFOBLOCK;

typedef struct {
    int length;
    char *name;
    char *pattern;
} PATTERN;

typedef struct {
    int offset;
    int length;
    char *name;
    char *pattern;
} BPATTERN;

/* External pattern arrays */
extern BPATTERN bmagic[];
extern PATTERN amagic[];
extern PATTERN asearch[];
extern PATTERN IFFforms[];
extern PATTERN compress;
extern PATTERN zoo;

#endif /* FILE_H */
