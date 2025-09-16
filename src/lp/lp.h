/*
 * lp.h - unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <fcntl.h>

#ifdef AMIGA
#include <exec/types.h>
#include <exec/memory.h>
#include <exec/ports.h>
#include <dos/dos.h>
#include <dos/dosextens.h>
#include <dos/rdargs.h>
#include <proto/exec.h>
#include <proto/dos.h>
#endif /* AMIGA */

/* POSIX compliance includes */
#include "/common/common.h"
#include "/common/getopt.h"

/* Spool system integration */
#include "spool.h"

/* Spool system constants */
#define SPOOL_ME "SPOOL system"
#define LOG_IN '+'
#define LOG_OUT '-'

/* Function declarations */
extern int is_getopt_style(int argc, char **argv);
extern void reset_getopt(void);
extern char *my_basename(char *path);
extern char *build_command_string(int argc, char **argv, const char *user_input);