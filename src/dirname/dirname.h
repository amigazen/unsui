/*
 * dirname.h - Header file for dirname utility
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef DIRNAME_H
#define DIRNAME_H

/* Function prototypes */
char *my_basename(char *path);
int test_part(char p);
int test_absolute(char *p);
void usage(char *program);

/* Constants */
#define TRUE	1
#define FALSE	0

#endif /* DIRNAME_H */
