/*
 * Bison 1.25 - GNU Compiler Compiler
 * Main header file consolidating all includes and function prototypes
 * 
 * This is a port of GNU Bison 1.25 for Amiga OS using SAS/C compiler
 * Original author: Free Software Foundation
 * Amiga port: Giacomo Magnini <prometeo@flashnet.it>
 */

#ifndef BISON_MAIN_H
#define BISON_MAIN_H

/* System includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Amiga includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <utility/utility.h>

/* Local includes */
#include "system.h"
#include "machine.h"
#include "files.h"
#include "gram.h"
#include "lex.h"
#include "state.h"
#include "types.h"
#include "getopt.h"

/* Global variables */
extern int lineno;
extern int verboseflag;
extern int failure;
extern char *program_name;

/* Function prototypes from main.c */
int main(int argc, char *argv[]);
char *printable_version(char c);
void done(int status);

/* Function prototypes from getargs.c */
void getargs(int argc, char *argv[]);

/* Function prototypes from files.c */
void openfiles(void);

/* Function prototypes from reader.c */
void reader(void);

/* Function prototypes from reduce.c */
void reduce_grammar(void);

/* Function prototypes from derives.c */
void set_derives(void);

/* Function prototypes from nullable.c */
void set_nullable(void);

/* Function prototypes from LR0.c */
void generate_states(void);

/* Function prototypes from lalr.c */
void lalr(void);

/* Function prototypes from conflicts.c */
void initialize_conflicts(void);

/* Function prototypes from print.c */
void verbose(void);
void terse(void);

/* Function prototypes from output.c */
void output(void);

/* Function prototypes from getopt.c */
int getopt(int argc, char *const argv[], const char *optstring);
extern char *optarg;
extern int optind, opterr, optopt;

/* Function prototypes from getopt1.c */
int getopt_long(int argc, char *const argv[], const char *options,
                const struct option *long_options, int *opt_index);

/* Function prototypes from allocate.c */
void *allocate(size_t size);

/* Function prototypes from closure.c */
void closure(void);

/* Function prototypes from symtab.c */
void symtab_init(void);

/* Function prototypes from warshall.c */
void warshall(void);

/* Function prototypes from version.c */
extern const char *version_string;

#endif /* BISON_MAIN_H */
