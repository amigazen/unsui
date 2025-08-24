/*
 * uniq.h - Uniq command for Unsui POSIX runtime
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#ifndef UNIQ_H
#define UNIQ_H

#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <exec/types.h>
#include "common.h"

/* C89 compatibility definitions - none needed for current implementation */

/* Constants */
#define BUFLEN 1024
#define MAX_LINES 10000

/* Data structures - not used in current implementation but kept for future extensibility */

struct uniq_options {
    int unique_flag;      /* -u: output only unique lines */
    int duplicate_flag;   /* -d: output only duplicate lines */
    int count_flag;       /* -c: prefix with count */
    int fields_skip;      /* -f: skip n fields before comparing */
    int chars_skip;       /* -s: skip n chars after fields */
    int chars_skip_plus;  /* +n: skip n chars after fields (alternative syntax) */
};



/* Function prototypes */
void uniq_usage(const char *program);
int uniq_parse_getopt_args(int argc, char **argv, struct uniq_options *opts, int *file_start, const char *program);
int run_uniq_logic(struct uniq_options *opts, const char *program);
int uniq_process_file(const char *filename, struct uniq_options *opts);
int uniq_process_stdin(struct uniq_options *opts);
char *skip_fields_and_chars(char *line, struct uniq_options *opts);
int lines_equal(char *line1, char *line2, struct uniq_options *opts);
void show_line(char *line, int count, struct uniq_options *opts);
int uniq_process_input(FILE *input, struct uniq_options *opts);

/* Portable line reading function using getchar */
int read_line(char **lineptr, size_t *n, FILE *stream);

#endif /* UNIQ_H */
