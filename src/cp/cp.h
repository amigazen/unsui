/*
 * cp.h - POSIX cp command for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on ACP by Fred Cassirer, placed in Public Domain.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef CP_H
#define CP_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <proto/dos.h>
#include <proto/exec.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <dos/dosasl.h>
#include <exec/memory.h>
#include <exec/types.h>
#include <exec/exec.h>
#include <libraries/dosextens.h>
#include <clib/alib_protos.h>
#include <proto/dos.h>
#include <proto/utility.h>

/* Magic numbers suggested or required by Posix specification */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* or failure */

/* Default buffer size */
#define DEFAULT_BUFFER_SIZE (8 * 1024)  /* 8KB default buffer */

/* For ReadArgs template */
enum {
    ARG_SOURCE,
    ARG_DESTINATION,
    ARG_INTERACTIVE,
    ARG_FORCE,
    ARG_RECURSIVE,
    ARG_VERBOSE,
    ARG_PRESERVE,
    ARG_POSIX,
    ARG_COUNT
};

/* Function declarations for forward references */
int copy_file(const char *source, const char *dest, int buffer_size, int *force, int verbose);
int copy_directory(const char *source, const char *dest, int recursive, int *force, int verbose);
int is_directory(const char *path);
int is_regular_file(const char *path);
void usage(const char *program);
int run_cp_logic(int argc, char **argv, int interactive, int force, int recursive, int verbose, int preserve, const char *program);
void parse_getopt_args(int argc, char **argv, int *interactive, int *force, int *recursive, int *verbose, int *preserve, int *file_start, const char *program);

/* Amiga-specific directory handling structures */
struct DPTR {
    BPTR lock;                    /* lock on directory */
    struct FileInfoBlock *fib;    /* mod'd fib for entry */
};

/* Amiga console communication structures - use system definition */

/* Amiga-specific function declarations */
struct DPTR *dopen(const char *name, int *stat);
int dclose(struct DPTR *dp);
int do_copy(const char *s, const char *destination, int xtramem);

#endif /* CP_H */
