/*
 * dirname.h - Header file for dirname utility
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef DIRNAME_H
#define DIRNAME_H

/* Function prototypes */
int test_part(char p);
int test_absolute(char *p);
void usage(char *program);
int run_dirname_logic(char *path, const char *program);
void parse_getopt_args(int argc, char **argv, char **path, int *file_start, const char *program);

/* Constants */
#define TRUE	1
#define FALSE	0

#endif /* DIRNAME_H */
