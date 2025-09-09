/*
 * base64.h - Header file for base64 command
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef BASE64_H
#define BASE64_H

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Amiga-specific includes */
#include <proto/dos.h>
#include <dos/dos.h>
#include <dos/rdargs.h>

/* Common project includes */
#include "/common/common.h"
#include "/common/getopt.h"

/* Constants */
#define SUCCESS 0
#define FAILURE 1

/* Function declarations */
void usage(const char *program);
int run_base64_logic(int decode, int encode, int ignore_garbage, int wrap_col, 
                    char *input_file, char *output_file, const char *program);
void parse_getopt_args(int argc, char **argv, int *decode, int *encode, 
                      int *ignore_garbage, int *wrap_col, char **input_file, 
                      char **output_file, const char *program);
int base64_encode(FILE *input, FILE *output, int wrap_col);
int base64_decode(FILE *input, FILE *output, int ignore_garbage);
void reset_state(void);

#endif /* BASE64_H */
