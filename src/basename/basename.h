/*
 * basename.h - Header file for basename utility
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef BASENAME_H
#define BASENAME_H

/* Function prototypes */
void usage(char *program);
int run_basename_logic(char *name, char *suffix, const char *program);
void parse_getopt_args(int argc, char **argv, char **name, char **suffix, int *file_start, const char *program);

/* Constants */
#define TRUE	1
#define FALSE	0

#endif /* BASENAME_H */
