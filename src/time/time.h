/*
 * time.h - Header file for Unsui POSIX time command
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef TIME_H
#define TIME_H

#include <exec/types.h>
#include <dos/dos.h>

/* Constants for time calculations */
#define TICKS_PER_SECOND    50
#define TICKS_PER_MINUTE    (TICKS_PER_SECOND*60)
#define MINUTES_PER_DAY      (60*24)

/* Function declarations */
void usage(const char *program);
int run_time_logic(void *options, const char *program);
void parse_getopt_args(int argc, char **argv, void *options, int *file_start, const char *program);
void init_options(void *options);
void cleanup_options(void *options);
void PrintDateStamp(struct DateStamp *ds);
void SubDateStamp(struct DateStamp *ds, struct DateStamp *amount);
void print_time_output(struct DateStamp *ds, void *options);

#endif /* TIME_H */
