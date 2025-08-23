/*
 * tail.h - Header file for tail utility
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 */

 #ifndef TAIL_H
 #define TAIL_H
 
 /* Function prototypes */
 char *my_basename(char *path);
 int tail(int count, int bytes, int read_until_killed);
 int keep_reading(void);
 void usage(char *program);
 void parse_obsolescent_args(int argc, char **argv, int *cflag, int *nflag, 
                            int *fflag, int *number, int *file_start);
 int run_tail_logic(int number, int cflag, int fflag, int file_count, char **files, const char *program);
 int is_getopt_style(int argc, char **argv);
 char* build_command_string(int argc, char **argv, const char* exclude);
 void parse_getopt_args(int argc, char **argv, int *cflag, int *nflag,
                        int *fflag, int *number, int *file_start, const char *program);
 
 #endif /* TAIL_H */