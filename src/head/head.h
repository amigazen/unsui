/*
 * head.h - Header file for head utility
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef HEAD_H
#define HEAD_H

/* Function prototypes */
void do_file(int n, FILE *f);
void usage(char *program);
int run_head_logic(int lines, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, int *lines, int *file_start, const char *program);

/* Constants */
#define DEFAULT_LINES	10

#endif /* HEAD_H */
