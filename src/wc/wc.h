/*
 * wc.h - Header file for wc utility
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef WC_H
#define WC_H

/* Function prototypes */
void count(FILE *f);
void usage(char *program);
void reset_counters(void);
int run_wc_logic(int lflag, int wflag, int cflag, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, int *lflag, int *wflag, int *cflag, int *file_start, const char *program);

/* Constants */
#define TRUE	1
#define FALSE	0

/* Global variables (extern declarations) */
extern long lcount, wcount, ccount;

#endif /* WC_H */
