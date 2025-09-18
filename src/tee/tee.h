/*
 * tee.h - Header file for tee command
 * 
 * Copyright (c) 1995 Ingo Wilken
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef TEE_H
#define TEE_H

/* Amiga-specific includes */
#include <exec/types.h>
#include <exec/libraries.h>
#include <exec/memory.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <dos/dosasl.h>

/* Standard C includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* External SysBase declaration */
extern struct ExecBase *SysBase;



/* Function declarations - only declare if not already declared */
#ifndef COMMON_H_INCLUDED
int is_getopt_style(int argc, char **argv);
char *build_command_string(int argc, char **argv, const char *exclude);
int tokenize_string(const char *str, char **argv, int max_args);
char *my_basename(const char *path);
#endif

#endif /* TEE_H */
