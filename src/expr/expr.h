/*
 * expr.h - Header file for expr command
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on expr by Peter S. Housel.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef EXPR_H
#define EXPR_H

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>

/* Amiga-specific includes */
#include <proto/exec.h>
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <libraries/dosextens.h>
#include <exec/memory.h>

/* Define intmax_t for compilers that don't have it */
#ifndef INTMAX_MAX
typedef long intmax_t;
typedef unsigned long uintmax_t;
#define INTMAX_MAX LONG_MAX
#define INTMAX_MIN LONG_MIN
#define UINTMAX_MAX ULONG_MAX
#endif

/* Constants */
#define SUCCESS 0               /* exit code in case of success */
#define FAILURE 1               /* exit code in case of failure */
#define SYNTAX_ERROR 2          /* exit code in case of syntax error */

/* Version tag for Amiga */
#define VERSION_TAG "$VER: expr 2.0 (17/09/25)\n"

/* Structure for expression values */
struct value {
  intmax_t numval;              /* numeric value */
  int nf_valid;                 /* "numeric value field valid" flag */
  char *strval;                 /* string value */
};

/* Global variables */
extern char *progname;
extern char **argp;
extern char NUMARG[];

/* Function declarations */
int main(int argc, char **argv);
void expr1(struct value *valp);
void expr2(struct value *valp);
void expr3(struct value *valp);
void expr4(struct value *valp);
void expr5(struct value *valp);
void expr6(struct value *valp);
void expr7(struct value *valp);
int nullz(struct value *valp);
int numvalue(struct value *valp);
char *strvalue(struct value *valp);
char *strsave(char *string);
void invalid(char *err);
void docolon(struct value *match, struct value *pattern);
void rcomp(char *regexp);
void rmatch(char *str);
char *rtry(char *str, unsigned char **pcp);
char *tryone(char *str, unsigned char **pcp);
void usage(const char *program);
void show_version(void);
int run_expr_logic(int argc, char **argv);

/* Macros */
#define numresult(valp,number)    (((valp)->nf_valid = 1),((valp)->strval = NULL),((valp)->numval = (number)))

#endif /* EXPR_H */
