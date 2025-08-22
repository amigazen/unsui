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
char *my_basename(char *path);
void count(FILE *f);
void usage(char *program);
void reset_counters(void);

/* Constants */
#define TRUE	1
#define FALSE	0

/* Global variables (extern declarations) */
extern int lflag, wflag, cflag;
extern long lcount, wcount, ccount;
extern long ltotal, wtotal, ctotal;

#endif /* WC_H */
