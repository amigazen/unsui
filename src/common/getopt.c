/*
 * getopt.c - POSIX getopt implementation for Amiga
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#include <stdio.h>
#include <string.h>
#include "getopt.h"

/* Global variables for getopt */
char *optarg = NULL;
int optind = 1;
int opterr = 1;
int optopt = 0;

/* Internal state */
static char *nextchar = NULL;

/**
 * @brief Reset getopt state for new parsing
 */
void reset_getopt(void) {
    optind = 1;
    optarg = NULL;
    optopt = 0;
    nextchar = NULL;
}

/**
 * @brief POSIX getopt implementation
 * @param argc Argument count
 * @param argv Argument vector
 * @param optstring Option string
 * @return Option character or -1 when done
 */
int getopt(int argc, char * const argv[], const char *optstring) {
    char c;
    char *cp;
    
    /* Reset if we're starting fresh */
    if (optind == 1) {
        nextchar = NULL;
    }
    
    /* Check if we need to move to next argument */
    if (nextchar == NULL || *nextchar == '\0') {
        /* Skip non-option arguments */
        while (optind < argc && argv[optind][0] != '-') {
            optind++;
        }
        
        if (optind >= argc) {
            return -1; /* No more options */
        }
        
        /* Check for -- (end of options) */
        if (strcmp(argv[optind], "--") == 0) {
            optind++;
            return -1;
        }
        
        /* Check for single - (not an option) */
        if (strcmp(argv[optind], "-") == 0) {
            optind++;
            return -1;
        }
        
        /* Check if this looks like an option */
        if (argv[optind][0] != '-') {
            return -1;
        }
        
        nextchar = argv[optind] + 1;
    }
    
    /* Get the next option character */
    c = *nextchar++;
    optopt = c;
    
    /* Find the option in optstring */
    cp = strchr(optstring, c);
    if (cp == NULL) {
        if (opterr) {
            fprintf(stderr, "Unknown option: -%c\n", c);
        }
        return '?';
    }
    
    /* Check if option requires an argument */
    if (cp[1] == ':') {
        if (*nextchar != '\0') {
            /* Argument is in same argv element */
            optarg = nextchar;
            optind++;
            nextchar = NULL;
        } else {
            /* Argument is in next argv element */
            optind++;
            if (optind >= argc) {
                if (opterr) {
                    fprintf(stderr, "Option -%c requires an argument\n", c);
                }
                return '?';
            }
            optarg = argv[optind];
            optind++;
            nextchar = NULL;
        }
    } else {
        /* No argument required */
        if (*nextchar == '\0') {
            optind++;
            nextchar = NULL;
        }
        optarg = NULL;
    }
    
    return c;
}
