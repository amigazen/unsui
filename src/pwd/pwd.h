/*
 * pwd.h - Header file for pwd command
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef PWD_H
#define PWD_H

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <libraries/dosextens.h>
#include <exec/memory.h>

/* Constants */
#define PATH_MAX 1024

/* Function declarations */
void usage(const char *program);
int run_pwd_logic(const char *program, int logical);
int parse_getopt_args(int argc, char **argv, int *logical, const char *program);
char *amiga_path_of_lock(struct FileLock *origl);

#endif /* PWD_H */
