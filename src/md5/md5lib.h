/*
 * md5lib.h - Header file for common MD5 library functions
 * 
 * This file contains declarations for shared functionality used by both
 * the md5 and md5sum commands.
 */

#ifndef MD5LIB_H
#define MD5LIB_H

#include "md5.h"

/* Global variables for command behavior */
extern int md5_quiet;
extern int md5_reverse;
extern int md5_binary;
extern int md5_verbose;

/* Function prototypes */
void md5_print_digest(unsigned char *digest);
int md5_compute_file(FILE *fp, unsigned char *digest);
int md5_compute_string(const char *str, unsigned char *digest);
int md5_hex_digit(int c);
int md5_parse_line(FILE *fp, unsigned char *digest, char *filename, int *binary_flag);
int md5_verify_file(const char *filename, const unsigned char *expected_digest, int binary_mode);
void md5_print_bsd_format(const char *filename, unsigned char *digest, int is_string);
void md5_print_gnu_format(const char *filename, unsigned char *digest, int binary_mode);

#endif /* MD5LIB_H */
