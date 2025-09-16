/*
 * md5lib.c - Common MD5 library functions for md5 and md5sum commands
 * 
 * This file contains shared functionality used by both the md5 and md5sum
 * commands, ensuring consistency and reducing code duplication.
 *
 * Copyright (c) 1990, RSA Data Security, Inc. All rights reserved.
 * This code is derived from the RSA Data Security, Inc. MD5 Message-Digest Algorithm.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "md5.h"
#include "md5lib.h"

/* Global variables for command behavior */
int md5_quiet = 0;
int md5_reverse = 0;
int md5_binary = 0;
int md5_verbose = 0;

/* Function prototypes */
void md5_print_digest(unsigned char *digest);
int md5_compute_file(FILE *fp, unsigned char *digest);
int md5_compute_string(const char *str, unsigned char *digest);
int md5_hex_digit(int c);
int md5_parse_line(FILE *fp, unsigned char *digest, char *filename, int *binary_flag);
int md5_verify_file(const char *filename, const unsigned char *expected_digest, int binary_mode);
void md5_print_bsd_format(const char *filename, unsigned char *digest, int is_string);
void md5_print_gnu_format(const char *filename, unsigned char *digest, int binary_mode);

/*
 * Print MD5 digest in hexadecimal format
 */
void md5_print_digest(unsigned char *digest)
{
    int i;
    
    for (i = 0; i < 16; i++) {
        printf("%02x", digest[i]);
    }
}

/*
 * Compute MD5 digest of a file
 */
int md5_compute_file(FILE *fp, unsigned char *digest)
{
    unsigned char buf[1024];
    MD5_CTX ctx;
    size_t n;
    
    MD5Init(&ctx);
    while ((n = fread(buf, 1, sizeof(buf), fp)) > 0) {
        MD5Update(&ctx, buf, (unsigned int)n);
    }
    MD5Final(digest, &ctx);
    
    if (ferror(fp)) {
        return -1;
    }
    return 0;
}

/*
 * Compute MD5 digest of a string
 */
int md5_compute_string(const char *str, unsigned char *digest)
{
    MD5_CTX ctx;
    
    MD5Init(&ctx);
    MD5Update(&ctx, (unsigned char *)str, (unsigned int)strlen(str));
    MD5Final(digest, &ctx);
    
    return 0;
}

/*
 * Convert hex character to digit
 */
int md5_hex_digit(int c)
{
    if (c >= '0' && c <= '9') {
        return c - '0';
    }
    if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    }
    if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    }
    return -1;
}

/*
 * Parse a line from a checksum file
 * Returns: 1 for text mode, 2 for binary mode, 0 for invalid line, -1 for EOF
 */
int md5_parse_line(FILE *fp, unsigned char *digest, char *filename, int *binary_flag)
{
    char buf[1024];
    int i, d1, d2, rc;
    char *p = buf;
    
    if (fgets(buf, sizeof(buf), fp) == NULL) {
        return -1;
    }
    
    /* Parse 32 hex digits */
    for (i = 0; i < 16; i++) {
        if ((d1 = md5_hex_digit(*p++)) == -1) {
            return 0;
        }
        if ((d2 = md5_hex_digit(*p++)) == -1) {
            return 0;
        }
        digest[i] = (unsigned char)(d1 * 16 + d2);
    }
    
    /* Skip whitespace */
    while (*p == ' ' || *p == '\t') {
        p++;
    }
    
    /* Check for binary mode indicator */
    if (*p == '*') {
        *binary_flag = 1;
        p++;
    } else {
        *binary_flag = 0;
    }
    
    /* Skip whitespace after mode indicator */
    while (*p == ' ' || *p == '\t') {
        p++;
    }
    
    /* Extract filename */
    i = 0;
    while (*p != '\0' && *p != '\n' && *p != '\r' && i < 255) {
        filename[i++] = *p++;
    }
    filename[i] = '\0';
    
    if (i == 0) {
        return 0; /* No filename */
    }
    
    return 1;
}

/*
 * Verify a file against an expected digest
 */
int md5_verify_file(const char *filename, const unsigned char *expected_digest, int binary_mode)
{
    FILE *fp;
    unsigned char actual_digest[16];
    const char *mode = binary_mode ? "rb" : "r";
    
    fp = fopen(filename, mode);
    if (fp == NULL) {
        if (!md5_quiet) {
            fprintf(stderr, "md5: can't open %s: %s\n", filename, strerror(errno));
        }
        return -1;
    }
    
    if (md5_compute_file(fp, actual_digest) != 0) {
        if (!md5_quiet) {
            fprintf(stderr, "md5: error reading %s\n", filename);
        }
        fclose(fp);
        return -1;
    }
    
    fclose(fp);
    
    if (memcmp(expected_digest, actual_digest, 16) != 0) {
        return 0; /* Mismatch */
    }
    
    return 1; /* Match */
}

/*
 * Print digest in BSD format (hash filename or hash (filename))
 */
void md5_print_bsd_format(const char *filename, unsigned char *digest, int is_string)
{
    if (md5_quiet) {
        md5_print_digest(digest);
        printf("\n");
        return;
    }
    
    if (is_string) {
        printf("MD5 (\"%s\") = ", filename);
    } else {
        if (md5_reverse) {
            printf("%s ", filename);
            md5_print_digest(digest);
        } else {
            md5_print_digest(digest);
            printf(" %s", filename);
        }
    }
    printf("\n");
}

/*
 * Print digest in GNU format (hash  filename or hash *filename)
 */
void md5_print_gnu_format(const char *filename, unsigned char *digest, int binary_mode)
{
    md5_print_digest(digest);
    printf(" %c%s\n", binary_mode ? '*' : ' ', filename);
}
