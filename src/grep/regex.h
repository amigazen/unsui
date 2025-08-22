/*
 * regex.h - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on POSIX regex standard and Henry Spencer's design principles.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#ifndef _REGEX_H_
#define _REGEX_H_

#include <stddef.h>

/* POSIX regex types */
typedef long regoff_t;

typedef struct {
    int re_magic;
    size_t re_nsub;
    const char *re_endp;
    void *re_g;  /* internal guts */
} regex_t;

typedef struct {
    regoff_t rm_so;
    regoff_t rm_eo;
} regmatch_t;

/* POSIX regex flags for regcomp */
#define REG_BASIC      0000
#define REG_EXTENDED   0001
#define REG_ICASE      0002
#define REG_NOSUB      0004
#define REG_NEWLINE    0010
#define REG_NOSPEC     0020
#define REG_PEND       0040
#define REG_DUMP       0200

/* POSIX regex flags for regexec */
#define REG_NOTBOL     00001
#define REG_NOTEOL     00002
#define REG_STARTEND   00004
#define REG_TRACE      00400
#define REG_LARGE      01000
#define REG_BACKR      02000

/* POSIX regex error codes */
#define REG_OKAY       0
#define REG_NOMATCH    1
#define REG_BADPAT     2
#define REG_ECOLLATE   3
#define REG_ECTYPE     4
#define REG_EESCAPE    5
#define REG_ESUBREG    6
#define REG_EBRACK     7
#define REG_EPAREN     8
#define REG_EBRACE     9
#define REG_BADBR      10
#define REG_ERANGE     11
#define REG_ESPACE     12
#define REG_BADRPT     13
#define REG_EMPTY      14
#define REG_ASSERT     15
#define REG_INVARG     16
#define REG_ATOI       255
#define REG_ITOA       0400

/* Function prototypes */
int regcomp(regex_t *preg, const char *pattern, int cflags);
int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
size_t regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
void regfree(regex_t *preg);

#endif /* _REGEX_H_ */
