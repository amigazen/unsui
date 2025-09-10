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
#include "stat.h"
#include "dir.h"
#include "common.h"

/* Forward declaration for stat function */
extern int stat(char *path, struct stat *buf);

/* Forward declarations for wildcard expansion */
static int indircmp(char **l, char **r);
static void AddDirFile(char *dir, char *file);
static void recursive_expand(char *w);
static int wildmatch(char *str, char *pat);

/**
 * @brief Detect if command line arguments follow POSIX style
 * @param argc Argument count
 * @param argv Argument vector
 * @return TRUE if POSIX style, FALSE if Amiga style
 */
int is_getopt_style(int argc, char **argv) {
    if (argc < 2) return FALSE;

    /* Check for standard options like -n, -c, -f, -v, -h, -w */
    if (argv[1][0] == '-' && argv[1][1] != '\0' && 
        strchr("cfnvhVw", argv[1][1])) {
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
    int count = 0, i;
    char *p = str;
    

    while (*p && count < max_args) {
        /* Skip leading whitespace */
        while (*p && isspace((unsigned char)*p)) {
            p++;
        }

        if (*p) {
            /* This is the start of a token */
            char *token_start = p;

            /* Find the end of the token */
            while (*p && !isspace((unsigned char)*p)) {
                p++;
            }

            /* If we are not at the end of the string, null-terminate the token */
            if (*p) {
                *p = '\0';
                p++;
            }
            
            /* Skip any additional whitespace after the token */
            while (*p && isspace((unsigned char)*p)) {
                p++;
            }
            
            /* Set the token pointer to the start of the token */
            argv[count] = token_start;
            count++;
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

/**
 * @brief Free wildcard expansion result
 * @param freelist Array of strings to free
 */
void wildfree(char **freelist) {
    char **cpp;

    if (!freelist) return;
    for (cpp = freelist; *cpp; cpp++)
        free(*cpp);

    free(freelist);
}

/**
 * @brief Expand wildcards in filename pattern
 * @param w Wildcard pattern to expand
 * @return Array of matching filenames, NULL-terminated
 */
char **wildexpand(char *w) {
    static char **listp = NULL;
    static int curcount = 0, maxcount = 0;
    static char *workpath, *curp;
    char *cp, *getenv();
    int i;

    curcount = maxcount = 0;
    listp = NULL;
    curp = workpath = (char *)malloc(1024); /* Bigger than AmigaDOS path */

    if (*w == '~' && *(w+1) == '/') {
        if (cp = getenv("HOME")) {
            strcpy(workpath, cp);
            i = strlen(cp);
            if (workpath[i-1] != '/' && workpath[i-1] != ':')
                workpath[i++] = '/';
            curp += i;
            w += 2;
        }
    }
    recursive_expand(w);

    free(workpath);

    if (listp)
        qsort(listp, curcount, sizeof(char *), indircmp);
    else
        AddDirFile(w, "");

    listp[curcount] = NULL;
    return(listp);
}

/**
 * @brief Amigaize path by converting Unix-style paths to Amiga format
 * @param to Path string to modify
 * @return Number of characters removed
 */
int amigaizepath(char *to) {
    char *from = to;
    int last = 0, removed = 0;

    while (*from == ' ') {
        from++;
        removed++;
    }

    if (*from == '/') {
        if (strncmp(from, "/dev/", 5)) return(removed);

        from += 5;
        if (!strcmp(from, "null")) {
            strcpy(to, "NULL:");
            removed += 4;
        } else if (!strcmp(from, "tty")) {
            strcpy(to, "CON:");
            removed += 4;
        } else if (!strcmp(from, "console")) {
            strcpy(to, "CON:");
            removed += 8;
        }
        return (removed);
    }

    while (*from) {
        if (*from == '.') {			/* Found ".", need hacking? */
            /* If beginning of line or last char was not alphanumeric */
            if (!last || !isalnum(last)) {
                if (!last) {			/* Look for plain "." & ".." */
                    if (!*(from + 1)) {
                        *from = '\0';		/* Change it to "" */
                        return(0);
                    } else if (*(from + 1) == '.' && !*(from + 2)) {
                        *from = '/';		/* Change it to "/" */
                        *(from+1) = '\0';
                        return(0);
                    }
                }

                if (*(from + 1) == '/') {
                    from += 2;			/* Skip "./" */
                    removed += 2;
                    last = '/';
                    continue;			/* And go on to next char */
                } else if (*(from + 1) == '.') {
                    if (*(from + 2) == '/') {	/* Found "../"; skip ".." and */
                        from += 2;		/*  fall through allowing '/' */
                        removed += 2;
                    }
                }				/*  to be assigned below      */
            }
        }
        last = *from;
        *to++ = *from++;
    }
    *to = '\0';
    return (removed);
}

/* Helper functions for wildcard expansion */
static char wildlist[] = "\\~*?[";	/* List of chars we want to look at */

static int indircmp(char **l, char **r) {
    return (strcmp(*l, *r));
}

static void AddDirFile(char *dir, char *file) {
    static char **listp = NULL;
    static int curcount = 0, maxcount = 0;
    char *cp;

    if (curcount + 1 >= maxcount) {	/* Need room for final NULL */
        maxcount += 32;
        listp = (char **)realloc(listp, maxcount * sizeof (char *));
        /* listp is null at start, which realloc takes to mean "malloc" */
    }
    cp = listp[curcount++] = (char *)malloc(strlen(dir) + strlen(file) + 1);
    strcpy(cp, dir);
    if (*file) strcat(cp, file);
}

static void recursive_expand(char *w) {
    static char **listp = NULL;
    static int curcount = 0, maxcount = 0;
    static char *workpath, *curp;
    char *cp, *hold_curp = curp;
    struct direct *d;
    DIR *dp;

    if (!(cp = strpbrk(w, wildlist))) {
        *curp = '\0';
        return;
    }
    for (; cp > w && *cp != '/' && *cp != ':'; cp--) ;
    if (*cp == '/' || *cp == ':') cp++;

    if (cp > w) {
        strncpy(curp, w, cp - w);
        curp += cp - w;
    }
    *curp = '\0';

    if (!(dp = opendir(workpath))) return;
    while (d = readdir(dp)) {
        if (!(d->d_ino)) continue;
        if (*(d->d_name) == '.' && *cp != '.') continue;
        if (wildmatch(d->d_name, cp))
            AddDirFile(workpath, d->d_name);
    }
    closedir(dp);
    curp = hold_curp;
    *curp = '\0';
}

static int wildmatch(char *str, char *pat) {
    char *cp, *sp = str;
    struct stat st;
    int c, c2, patc, not;

    while (patc = *pat++) {
        switch (patc) {
        case '?':
            if (!*str) return (0);
            str++;
            break;

        case '\\':
            if (*pat++ != *str++) return(0);
            break;

        case '[':
            /* All the tests for '\0' mean: "if bad pattern, no match." */
            if (!(c = *str++)) return(0);
            if (not = (*pat == '^')) pat++;

            if (!(patc = *pat++)) return (0);
            if (patc == ']') return(0);
            for (;;) {
                if (patc == ']') break;
                if (patc == '\\') if (!(patc = *pat++)) return (0);

                if (c != patc) {
                    if (!(c2 = *pat)) return (0);
                    if (c2 == '\\') { patc = c2; continue; }
                    pat++;

                    if (c2 == '-') {
                        if (!(c2 = *pat++)) return (0);
                        if (c2 == '\\') if (!(c2 = *pat++)) return (0);

                    } else {
                        patc = c2;
                        continue;
                    }
                    if (c < patc || c > c2) {
                        if (!(patc = *pat++)) return (0);
                        continue;
                    }
                }
                /* We wind up here on a match */
                if (not) return(0);
                while (patc && patc != ']') {
                    patc = *pat++;
                    if (patc == '\\') pat++;
                }
                if (!patc) return (0);
                not = 1;  /* "*hack*" to get a "break" outside loop */
                break;
            }
            if (not) break;	/* no match, but didn't want to (or "*hack*")*/
            return (0);	/* no match  and wanted to */

        case '*':
            if (!*pat) return (1);
            if (*pat != '/') {
                for (cp = str; *cp; cp++)
                    if (wildmatch(cp, pat))
                        return (1);
                return (0);
            }
            /* fall through */
        case '/':
            if (*str) return (0);	/* If str(ing) continues, no match */
            return (0);

        default:
            if (patc != *str++) return (0);
            break;
        }
    }
    return (!*str);
}
