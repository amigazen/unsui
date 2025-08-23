/*
 * touch.h - Header file for touch utility
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef TOUCH_H
#define TOUCH_H

/* Function prototypes */
void touch_file(char *filename, int access_flag, int modify_flag, int create_flag);
void usage(const char *program);
int run_touch_logic(int access_flag, int modify_flag, int create_flag, int file_count, char **files, const char *program);
void parse_getopt_args(int argc, char **argv, int *access_flag, int *modify_flag, int *create_flag, int *file_start, const char *program);

/* Constants */
#define TRUE	1
#define FALSE	0



#endif /* TOUCH_H */
