/*
 * common.h - Common utilities for Unsui POSIX runtime commands
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#ifndef COMMON_H
#define COMMON_H

#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

/* Common constants */
#define FALSE 0
#define TRUE 1
#define SUCCESS 0
#define FAILURE 1

extern struct DosLibrary *DOSBase;

/* Function declarations for hybrid argument parsing */
int is_getopt_style(int argc, char **argv);
char* build_command_string(int argc, char **argv, const char* exclude);
int tokenize_string(char *str, char **argv, int max_args);
char *my_basename(char *path);

/* Common ReadArgs setup and cleanup */
struct RDArgs* setup_readargs(const char *template, char *cmd_string);
void cleanup_readargs(struct RDArgs *rdargs, char *cmd_string);

#endif /* COMMON_H */
