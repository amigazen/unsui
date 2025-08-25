/*
 * Flex 2.5.4 - Fast Lexical Analyzer Generator
 * Main header file consolidating all includes and function prototypes
 * 
 * This is a port of GNU Flex 2.5.4 for Amiga OS using SAS/C compiler
 * Original author: Vern Paxson, University of California, Berkeley
 * Amiga port: Flex254 distribution
 */

#ifndef FLEX_MAIN_H
#define FLEX_MAIN_H

/* System includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Amiga includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <exec/types.h>
#include <exec/memory.h>
#include <utility/utility.h>

/* Local includes */
#include "Src/flexdef.h"
#include "Src/version.h"
#include "Src/config.h"

/* Version information */
extern char flex_version[];

/* Global variables from flexdef.h */
extern int printstats, syntaxerror, eofseen, ddebug, trace, nowarn, spprdflt;
extern int interactive, caseins, lex_compat, do_yylineno, useecs, fulltbl, usemecs;
extern int fullspd, gen_line_dirs, performance_report, backing_up_report;
extern int C_plus_plus, long_align, use_read, yytext_is_array, do_yywrap, csize;
extern int yymore_used, reject, real_reject, continued_action, in_rule;
extern int yymore_really_used, reject_really_used;
extern int datapos, dataline, linenum, out_linenum;
extern FILE *skelfile;
extern int skel_ind;
extern char *action_array;
extern int action_size, defs1_offset, prolog_offset, action_offset, action_index;
extern char *infilename, *outfilename;
extern int did_outfilename;
extern char *prefix, *yyclass;
extern int do_stdinit, use_stdout;

/* Function prototypes from main.c */
int main(int argc, char **argv);
void flexinit(int argc, char **argv);
void readin(void);
void set_up_initial_allocations(void);

/* Function prototypes from parse.y */
int yyparse(void);
void yyerror(const char *s);

/* Function prototypes from scan.l */
int flexscan(void);

/* Function prototypes from gen.c */
void gen(void);
void write_tables(void);

/* Function prototypes from dfa.c */
void add_accept(int dfa_num, int accept_num);
void dump_tables(void);

/* Function prototypes from nfa.c */
void add_accept(int dfa_num, int accept_num);
void dump_tables(void);

/* Function prototypes from misc.c */
void *allocate_array(int size, int element_size);
void *allocate_integer_array(int size);
void *allocate_character_array(int size);
void *reallocate_array(void *array, int size, int element_size);

/* Function prototypes from sym.c */
void add_symbol(char *name, int value);
int find_symbol(char *name);

/* Function prototypes from tblcmp.c */
void mkentry(int *array, int offset, int lbl, int defnext, int defback);
void mk1tbl(int s, int *tbl, int *acclist, int *dtrans);

/* Function prototypes from ecs.c */
void ccl2ecl(void);
void sympartition(int *symlist, int num, int *duplist);

/* Function prototypes from ccl.c */
void cclinit(void);
void ccladd(int cclp, int ch);

/* Function prototypes from yylex.c */
int yylex(void);

/* Function prototypes from alloca.c */
void *alloca(unsigned int size);

/* Library function prototypes */
int libmain(int argc, char **argv);
int libyywrap(void);

#endif /* FLEX_MAIN_H */
