/*
 * nice.h - Header for nice command (UNSUI hybrid POSIX/Amiga)
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on Nice by Tak Tang.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#ifndef NICE_H
#define NICE_H

#include <exec/types.h>
#include <dos/dos.h>

/* Constants */
#define SUCCESS 0
#define FAILURE 1
#define FALSE 0
#define TRUE 1

/* Priority limits - Amiga system guidelines */
#define MIN_PRIORITY_CHANGE -5
#define MAX_PRIORITY_CHANGE +5
#define DEFAULT_PRIORITY_CHANGE -3

/* Template items limit */
#define MAX_TEMPLATE_ITEMS 32

/* Function declarations */
int run_nice_logic(int priority, int absolute, int sticky, char **command, const char *program);
void parse_getopt_args(int argc, char **argv, int *priority, int *absolute, int *sticky, int *file_start, const char *program);
void usage(const char *program);

#endif /* NICE_H */
