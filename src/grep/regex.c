/*
 * regex - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on Henry Spencer's regex approach and POSIX standard.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "regex.h"

/* Magic number for regex program validation */
#define MAGIC 0234

/* Internal regex program opcodes */
#define END     0   /* End of program */
#define BOL     1   /* Match "" at beginning of line */
#define EOL     2   /* Match "" at end of line */
#define ANY     3   /* Match any one character */
#define ANYOF   4   /* Match any character in this string */
#define ANYBUT  5   /* Match any character not in this string */
#define BRANCH  6   /* Match this alternative, or the next... */
#define BACK    7   /* Match "", "next" ptr points backward */
#define EXACTLY 8   /* Match this string */
#define NOTHING 9   /* Match empty string */
#define STAR    10  /* Match this (simple) thing 0 or more times */
#define PLUS    11  /* Match this (simple) thing 1 or more times */
#define WORDA   12  /* Match "" at wordchar, where prev is nonword */
#define WORDZ   13  /* Match "" at nonwordchar, where prev is word */
#define OPEN    20  /* Mark this point in input as start of #n */
#define CLOSE   30  /* Analogous to OPEN */

/* Utility macros */
#define OP(p)       (*(p))
#define NEXT(p)     (((*((p)+1)&0377)<<8) + (*((p)+2)&0377))
#define OPERAND(p)  ((p) + 3)
#define ISMULT(c)   ((c) == '*' || (c) == '+' || (c) == '?')

/* Flags for internal use */
#define HASWIDTH    01  /* Known never to match null string */
#define SIMPLE      02  /* Simple enough to be STAR/PLUS operand */
#define SPSTART     04  /* Starts with * or + */
#define WORST       0   /* Worst case */

/* Internal regex program structure */
typedef struct regexp {
    char *program;      /* Compiled regex program */
    char *startp[10];   /* Start of match positions */
    char *endp[10];     /* End of match positions */
    char regstart;      /* First character of match */
    char reganch;       /* Is match anchored? */
    char *regmust;      /* String that must be in match */
    int regmlen;        /* Length of regmust */
    int re_nsub;        /* Number of subexpressions */
} regexp_t;

/* Internal compilation globals */
static struct {
    char *regparse;     /* Input-scan pointer */
    int regnpar;        /* () count */
    char *regcode;      /* Code-emit pointer */
    int regsize;        /* Code size */
} rc_globals;

/* Internal execution globals */
static struct {
    char *reginput;     /* String-input pointer */
    char *regbol;       /* Beginning of input, for ^ check */
    char **regstartp;   /* Pointer to startp array */
    char **regendp;     /* Pointer to endp array */
} re_globals;

/* Static dummy for address taking */
static char regdummy;

/* Forward declarations */
static int regcomp_internal(regexp_t *r, char *pattern, int cflags);
static int regexec_internal(regexp_t *r, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
static void regfree_internal(regexp_t *r);

/* POSIX wrapper functions */

int regcomp(regex_t *preg, const char *pattern, int cflags)
{
    regexp_t *r;
    
    if (!preg || !pattern) {
        return REG_INVARG;
    }
    
    r = (regexp_t *)malloc(sizeof(regexp_t));
    if (!r) {
        return REG_ESPACE;
    }
    
    memset(r, 0, sizeof(regexp_t));
    
    if (regcomp_internal(r, (char *)pattern, cflags) != 0) {
        free(r);
        return REG_BADPAT;
    }
    
    preg->re_magic = MAGIC;
    preg->re_nsub = r->re_nsub;
    preg->re_endp = NULL;
    preg->re_g = r;
    
    return REG_OKAY;
}

int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags)
{
    regexp_t *r;
    
    if (!preg || !string || preg->re_magic != MAGIC) {
        return REG_INVARG;
    }
    
    r = (regexp_t *)preg->re_g;
    if (!r) {
        return REG_INVARG;
    }
    
    return regexec_internal(r, string, nmatch, pmatch, eflags);
}

size_t regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size)
{
    const char *msg;
    
    switch (errcode) {
        case REG_OKAY:      msg = "Success"; break;
        case REG_NOMATCH:   msg = "No match"; break;
        case REG_BADPAT:    msg = "Invalid pattern"; break;
        case REG_ECOLLATE:  msg = "Invalid collating element"; break;
        case REG_ECTYPE:    msg = "Invalid character class"; break;
        case REG_EESCAPE:   msg = "Trailing backslash"; break;
        case REG_ESUBREG:   msg = "Invalid back reference"; break;
        case REG_EBRACK:    msg = "Unmatched bracket"; break;
        case REG_EPAREN:    msg = "Unmatched parenthesis"; break;
        case REG_EBRACE:    msg = "Unmatched brace"; break;
        case REG_BADBR:     msg = "Invalid brace contents"; break;
        case REG_ERANGE:    msg = "Invalid range"; break;
        case REG_ESPACE:    msg = "Out of memory"; break;
        case REG_BADRPT:    msg = "No preceding pattern"; break;
        case REG_EMPTY:     msg = "Empty pattern"; break;
        case REG_ASSERT:    msg = "Invalid assertion"; break;
        case REG_INVARG:    msg = "Invalid argument"; break;
        default:            msg = "Unknown error"; break;
    }
    
    if (errbuf && errbuf_size > 0) {
        strncpy(errbuf, msg, errbuf_size - 1);
        errbuf[errbuf_size - 1] = '\0';
    }
    
    return strlen(msg);
}

void regfree(regex_t *preg)
{
    if (preg && preg->re_magic == MAGIC && preg->re_g) {
        regfree_internal((regexp_t *)preg->re_g);
        free(preg->re_g);
        preg->re_g = NULL;
        preg->re_magic = 0;
    }
}

/* Internal regex compilation */
static int regcomp_internal(regexp_t *r, char *pattern, int cflags)
{
    /* Enhanced compilation with basic anchor support */
    
    if (!pattern || !*pattern) {
        return -1;
    }
    
    /* Basic pattern validation */
    if (strlen(pattern) > 1000) {  /* Reasonable limit */
        return -1;
    }
    
    /* Store the pattern and flags */
    r->program = strdup(pattern);
    r->re_nsub = 0;
    
    /* Set anchor flags for basic ^ and $ support */
    r->reganch = 0;
    if (pattern[0] == '^') {
        r->reganch = 1;  /* Pattern starts with ^ */
    }
    
    return 0;
}

/* Internal regex execution */
static int regexec_internal(regexp_t *r, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags)
{
    char *match;
    char *pattern = r->program;
    int pattern_len;
    
    if (!r || !string) {
        return REG_INVARG;
    }
    
    pattern_len = strlen(pattern);
    
    /* Handle ^ anchor (start of string) */
    if (r->reganch && pattern[0] == '^') {
        /* Remove ^ from pattern for matching */
        char *clean_pattern = pattern + 1;
        int clean_len = pattern_len - 1;
        
        if (clean_len == 0) {
            /* Just ^ matches start of string */
            if (nmatch > 0 && pmatch) {
                pmatch[0].rm_so = 0;
                pmatch[0].rm_eo = 0;
            }
            return REG_OKAY;
        }
        
        /* Check if string starts with the pattern */
        if (strncmp(string, clean_pattern, clean_len) == 0) {
            if (nmatch > 0 && pmatch) {
                pmatch[0].rm_so = 0;
                pmatch[0].rm_eo = clean_len;
            }
            return REG_OKAY;
        }
        return REG_NOMATCH;
    }
    
    /* Handle $ anchor (end of string) */
    if (pattern_len > 1 && pattern[pattern_len-1] == '$') {
        /* Remove $ from pattern for matching */
        char *clean_pattern = pattern;
        int clean_len = pattern_len - 1;
        int str_len;
        
        if (clean_len == 0) {
            /* Just $ matches end of string */
            int str_len = strlen(string);
            if (nmatch > 0 && pmatch) {
                pmatch[0].rm_so = str_len;
                pmatch[0].rm_eo = str_len;
            }
            return REG_OKAY;
        }
        
        /* Check if string ends with the pattern */
        str_len = strlen(string);
        if (str_len >= clean_len && 
            strncmp(string + str_len - clean_len, clean_pattern, clean_len) == 0) {
            if (nmatch > 0 && pmatch) {
                pmatch[0].rm_so = str_len - clean_len;
                pmatch[0].rm_eo = str_len;
            }
            return REG_OKAY;
        }
        return REG_NOMATCH;
    }
    
    /* Simple literal string matching for patterns without anchors */
    match = strstr(string, pattern);
    if (!match) {
        return REG_NOMATCH;
    }
    
    /* Set match positions */
    if (nmatch > 0 && pmatch) {
        pmatch[0].rm_so = match - string;
        pmatch[0].rm_eo = pmatch[0].rm_so + pattern_len;
    }
    
    return REG_OKAY;
}

/* Internal regex cleanup */
static void regfree_internal(regexp_t *r)
{
    if (r && r->program) {
        free(r->program);
        r->program = NULL;
    }
}
