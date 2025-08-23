/*
 * getopt.h - POSIX getopt implementation for Amiga
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#ifndef GETOPT_H
#define GETOPT_H

/* External variables for getopt */
extern char *optarg;
extern int optind;
extern int opterr;
extern int optopt;

/* Function declarations */
int getopt(int argc, char * const argv[], const char *optstring);

/* Reset getopt for new parsing */
void reset_getopt(void);

#endif /* GETOPT_H */
