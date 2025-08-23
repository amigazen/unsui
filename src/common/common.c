/*
 * common.c - Common utilities for Unsui POSIX runtime commands
 *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "common.h"

/**
 * @brief Detect if command line arguments follow POSIX style
 * @param argc Argument count
 * @param argv Argument vector
 * @return TRUE if POSIX style, FALSE if Amiga style
 */
int is_getopt_style(int argc, char **argv) {
    if (argc < 2) return FALSE;

    /* Check for standard options like -n, -c, -f, -v, -h */
    if (argv[1][0] == '-' && argv[1][1] != '\0' && 
        strchr("cfnvhV", argv[1][1])) {
        return TRUE;
    }

    /* Check for historical syntax like +5 or -10 */
    if ((argv[1][0] == '+') ||
        (argv[1][0] == '-' && (isdigit((unsigned char)argv[1][1])))) {
        return TRUE;
    }

    return FALSE;
}

/**
 * @brief Build a command string from argv for ReadArgs
 * @param argc Argument count
 * @param argv Argument vector
 * @param exclude String to exclude from concatenation (or NULL)
 * @return Allocated string containing concatenated arguments
 */
char* build_command_string(int argc, char **argv, const char* exclude) {
    size_t total_len = 0;
    char *result;
    int i;
    
    /* Calculate total length needed */
    for (i = 1; i < argc; i++) {
        if (!exclude || strcmp(argv[i], exclude) != 0) {
            total_len += strlen(argv[i]) + 1; /* +1 for space */
        }
    }
    
    if (total_len == 0) {
        return strdup("\n");
    }
    
    /* Allocate space for string plus newline and null terminator */
    result = malloc(total_len + 2);
    if (!result) return NULL;
    
    result[0] = '\0';
    
    /* Concatenate arguments */
    for (i = 1; i < argc; i++) {
        if (!exclude || strcmp(argv[i], exclude) != 0) {
            strcat(result, argv[i]);
            strcat(result, " ");
        }
    }
    
    /* Replace final space with newline for ReadArgs */
    if (result[0] != '\0') {
        result[strlen(result) - 1] = '\n';
    } else {
        strcpy(result, "\n");
    }
    
    return result;
}

/**
 * @brief A robust, non-destructive string tokenizer
 * @param str The string to tokenize (will be modified)
 * @param argv The array to fill with pointers to tokens
 * @param max_args The maximum number of arguments to parse
 * @return The number of tokens found
 */
int tokenize_string(char *str, char **argv, int max_args) {
    int count = 0;
    char *p = str;

    while (*p && count < max_args) {
        /* Skip leading whitespace */
        while (*p && isspace((unsigned char)*p)) {
            p++;
        }

        if (*p) {
            /* This is the start of a token */
            argv[count++] = p;

            /* Find the end of the token */
            while (*p && !isspace((unsigned char)*p)) {
                p++;
            }

            /* If we are not at the end of the string, null-terminate the token */
            if (*p) {
                *p = '\0';
                p++;
            }
        }
    }
    return count;
}

/**
 * @brief Extract the basename from a path
 * @param path The path to extract basename from
 * @return Pointer to the filename part
 */
char *my_basename(char *path) {
    char *temp;
    
    if (path == NULL) {
        return "unknown";
    }
    
    temp = strrchr(path, '/');
    if (temp == NULL) {
        temp = strrchr(path, ':');
    }
    
    return (temp != NULL) ? temp + 1 : path;
}

/**
 * @brief Set up ReadArgs structure for parsing
 * @param template The ReadArgs template string
 * @param cmd_string The command string to parse
 * @return Allocated RDArgs structure or NULL on failure
 */
struct RDArgs* setup_readargs(const char *template, char *cmd_string) {
    struct RDArgs *rdargs = AllocDosObject(DOS_RDARGS, NULL);
    if (!rdargs) return NULL;
    
    /* Point ReadArgs to the command string buffer */
    rdargs->RDA_Source.CS_Buffer = cmd_string;
    rdargs->RDA_Source.CS_Length = strlen(cmd_string);
    rdargs->RDA_Source.CS_CurChr = 0;
    rdargs->RDA_Flags |= RDAF_NOPROMPT;
    
    return rdargs;
}

/**
 * @brief Clean up ReadArgs structure and command string
 * @param rdargs The RDArgs structure to free
 * @param cmd_string The command string to free
 */
void cleanup_readargs(struct RDArgs *rdargs, char *cmd_string) {
    if (rdargs) {
        FreeDosObject(DOS_RDARGS, rdargs);
    }
    if (cmd_string) {
        free(cmd_string);
    }
}
