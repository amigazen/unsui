/*
 * md5.c - BSD-style MD5 command
 * derived from the RSA Data Security, Inc. MD5 Message-
 * Digest Algorithm
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 * This implements the BSD-style md5 command with options:
 * -c string    Compare digest against string
 * -p           Pass-through mode (echo stdin to stdout, append checksum)
 * -q           Quiet mode (only print checksum)
 * -r           Reverse format (filename hash)
 * -s string    Print checksum of string
 * -t           Time trial (no-op for compatibility)
 * -x           Self-test
 */


/***********************************************************************
 ** Copyright (C) 1990, RSA Data Security, Inc. All rights reserved.  **
 **                                                                   **
 ** License to copy and use this software is granted provided that    **
 ** it is identified as the "RSA Data Security, Inc. MD5 Message-     **
 ** Digest Algorithm" in all material mentioning or referencing this  **
 ** software or this function.                                        **
 **                                                                   **
 ** License is also granted to make and use derivative works          **
 ** provided that such works are identified as "derived from the RSA  **
 ** Data Security, Inc. MD5 Message-Digest Algorithm" in all          **
 ** material mentioning or referencing the derived work.              **
 **                                                                   **
 ** RSA Data Security, Inc. makes no representations concerning       **
 ** either the merchantability of this software or the suitability    **
 ** of this software for any particular purpose.  It is provided "as  **
 ** is" without express or implied warranty of any kind.              **
 **                                                                   **
 ** These notices must be retained in any copies of any part of this  **
 ** documentation and/or software.                                    **
 ***********************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include "md5.h"
#include "md5lib.h"
#include "getopt.h"

/* External variables from getopt */
extern char *optarg;
extern int optind;

/* External variables from md5lib */
extern int md5_quiet;
extern int md5_reverse;
extern int md5_binary;
extern int md5_verbose;

/* External functions from md5lib */
extern void md5_print_digest(unsigned char *digest);
extern int md5_compute_file(FILE *fp, unsigned char *digest);
extern int md5_compute_string(const char *str, unsigned char *digest);
extern int md5_hex_digit(int c);
extern int md5_verify_file(const char *filename, const unsigned char *expected_digest, int binary_mode);
extern void md5_print_bsd_format(const char *filename, unsigned char *digest, int is_string);

/* External functions from getopt */
extern int getopt(int argc, char **argv, char *opts);

/* Global variables */
char *progname;
int check_mode = 0;
int passthrough_mode = 0;
int string_mode = 0;
int time_trial = 0;
int self_test = 0;
char *check_string = NULL;
char *string_arg = NULL;

/* Function prototypes */
void usage(void);
void bsd_usage(void);
int do_self_test(void);
int do_time_trial(void);
int do_string_check(const char *str);
int do_passthrough(void);
int do_file_processing(int argc, char **argv);

/*
 * Print usage information for BSD-style md5 command
 */
void bsd_usage(void)
{
    fprintf(stderr, "usage: md5 [-pqr] [-c string] [-s string] [file ...]\n");
    fprintf(stderr, "       md5 -t\n");
    fprintf(stderr, "       md5 -x\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "BSD options:\n");
    fprintf(stderr, "  -c string    Compare the digest of the file against this string\n");
    fprintf(stderr, "  -p           Echo stdin to stdout and append the checksum\n");
    fprintf(stderr, "  -q           Quiet mode - only the checksum is printed\n");
    fprintf(stderr, "  -r           Reverse the format of the output\n");
    fprintf(stderr, "  -s string    Print a checksum of the given string\n");
    fprintf(stderr, "  -t           Run a built-in time trial\n");
    fprintf(stderr, "  -x           Run a built-in test script\n");
}

/*
 * Print usage and exit
 */
void usage(void)
{
    bsd_usage();
    exit(2);
}

/*
 * Perform self-test
 */
int do_self_test(void)
{
    unsigned char digest[16];
    const char *test_strings[] = {
        "",
        "a",
        "abc",
        "message digest",
        "abcdefghijklmnopqrstuvwxyz",
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
        "12345678901234567890123456789012345678901234567890123456789012345678901234567890",
        NULL
    };
    const char *expected[] = {
        "d41d8cd98f00b204e9800998ecf8427e",
        "0cc175b9c0f1b6a831c399e269772661",
        "900150983cd24fb0d6963f7d28e17f72",
        "f96b697d7cb7938d525a2f31aaf161d0",
        "c3fcd3d76192e4007dfb496cca67e13b",
        "d174ab98d277d9f5a5611c2c9f419d9f",
        "57edf4a22be3c955ac49da2e2107b67a",
        NULL
    };
    int i, passed = 0, total = 0;
    char result[33];
    int j;
    
    printf("MD5 self-test:\n");
    
    for (i = 0; test_strings[i] != NULL; i++) {
        md5_compute_string(test_strings[i], digest);
        
        printf("Test %d: ", i + 1);
        md5_print_digest(digest);
        printf(" \"%s\"", test_strings[i]);
        
        /* Compare with expected result */
        for (j = 0; j < 16; j++) {
            sprintf(result + j * 2, "%02x", (unsigned int)digest[j]);
        }
        
        if (strcmp(result, expected[i]) == 0) {
            printf(" - PASSED\n");
            passed++;
        } else {
            printf(" - FAILED (expected %s)\n", expected[i]);
        }
        total++;
    }
    
    printf("Self-test: %d/%d tests passed\n", passed, total);
    return (passed == total) ? 0 : 1;
}

/*
 * Perform time trial (no-op for compatibility)
 */
int do_time_trial(void)
{
    printf("Time trial not implemented (compatibility mode)\n");
    return 0;
}

/*
 * Check string against expected digest
 */
int do_string_check(const char *str)
{
    unsigned char digest[16];
    char result[33];
    int i;
    
    md5_compute_string(str, digest);
    
    for (i = 0; i < 16; i++) {
        sprintf(result + i * 2, "%02x", (unsigned int)digest[i]);
    }
    
    if (md5_quiet) {
        printf("%s\n", result);
    } else {
        printf("MD5 (\"%s\") = %s", str, result);
        if (strcmp(result, check_string) == 0) {
            printf(" [OK]\n");
            return 0;
        } else {
            printf(" [Failed]\n");
            return 2;
        }
    }
    
    return 0;
}

/*
 * Pass-through mode: echo stdin to stdout and append checksum
 */
int do_passthrough(void)
{
    unsigned char digest[16];
    char buf[1024];
    size_t n;
    FILE *temp_fp;
    
    /* Create temporary file to store input */
    temp_fp = tmpfile();
    if (temp_fp == NULL) {
        fprintf(stderr, "%s: cannot create temporary file\n", progname);
        return 1;
    }
    
    /* Copy stdin to temporary file and stdout */
    while ((n = fread(buf, 1, sizeof(buf), stdin)) > 0) {
        fwrite(buf, 1, n, stdout);
        fwrite(buf, 1, n, temp_fp);
    }
    
    /* Compute digest of the data */
    rewind(temp_fp);
    md5_compute_file(temp_fp, digest);
    fclose(temp_fp);
    
    /* Print digest */
    printf(" ");
    md5_print_digest(digest);
    printf("\n");
    
    return 0;
}

/*
 * Process files in BSD mode
 */
int do_file_processing(int argc, char **argv)
{
    unsigned char digest[16];
    FILE *fp;
    int i, rc = 0;
    const char *mode = md5_binary ? "rb" : "r";
    
    if (argc == 0) {
        /* Read from stdin */
        if (md5_compute_file(stdin, digest) != 0) {
            fprintf(stderr, "%s: read error on stdin\n", progname);
            return 1;
        }
        md5_print_bsd_format("-", digest, 0);
        return 0;
    }
    
    for (i = 0; i < argc; i++) {
        fp = fopen(argv[i], mode);
        if (fp == NULL) {
            fprintf(stderr, "%s: %s: %s\n", progname, argv[i], strerror(errno));
            rc = 1;
            continue;
        }
        
        if (md5_compute_file(fp, digest) != 0) {
            fprintf(stderr, "%s: error reading %s\n", progname, argv[i]);
            rc = 1;
        } else {
            md5_print_bsd_format(argv[i], digest, 0);
        }
        
        fclose(fp);
    }
    
    return rc;
}

/*
 * Main function for BSD-style md5 command
 */
int main(int argc, char **argv)
{
    int opt;
    
    progname = argv[0];
    
    /* Parse command line options */
    while ((opt = getopt(argc, argv, "c:pqrs:tx")) != EOF) {
        switch (opt) {
        case 'c':
            check_mode = 1;
            check_string = optarg;
            break;
        case 'p':
            passthrough_mode = 1;
            break;
        case 'q':
            md5_quiet = 1;
            break;
        case 'r':
            md5_reverse = 1;
            break;
        case 's':
            string_mode = 1;
            string_arg = optarg;
            break;
        case 't':
            time_trial = 1;
            break;
        case 'x':
            self_test = 1;
            break;
        default:
            usage();
        }
    }
    
    argc -= optind;
    argv += optind;
    
    /* Handle special modes */
    if (self_test) {
        return do_self_test();
    }
    
    if (time_trial) {
        return do_time_trial();
    }
    
    if (passthrough_mode) {
        return do_passthrough();
    }
    
    if (string_mode) {
        return do_string_check(string_arg);
    }
    
    if (check_mode) {
        unsigned char expected[16];
        int i;
        int result;
        
        if (argc != 1) {
            fprintf(stderr, "%s: -c option requires exactly one file\n", progname);
            return 2;
        }
        /* Parse expected digest from check_string */
        if (strlen(check_string) != 32) {
            fprintf(stderr, "%s: invalid digest format\n", progname);
            return 2;
        }
        for (i = 0; i < 16; i++) {
            int d1 = md5_hex_digit(check_string[i * 2]);
            int d2 = md5_hex_digit(check_string[i * 2 + 1]);
            if (d1 == -1 || d2 == -1) {
                fprintf(stderr, "%s: invalid digest format\n", progname);
                return 2;
            }
            expected[i] = (unsigned char)(d1 * 16 + d2);
        }
        
        /* Verify file */
        result = md5_verify_file(argv[0], expected, md5_binary);
        if (result == 1) {
            if (!md5_quiet) {
                printf("MD5 (%s) = %s [OK]\n", argv[0], check_string);
            }
            return 0;
        } else if (result == 0) {
            if (!md5_quiet) {
                printf("MD5 (%s) = %s [Failed]\n", argv[0], check_string);
            }
            return 2;
        } else {
            return 1;
        }
    }
    
    /* Process files normally */
    return do_file_processing(argc, argv);
}