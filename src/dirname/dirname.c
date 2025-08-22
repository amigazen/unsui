/*
 * dirname - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on dirname by Peter Holzer.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dirname.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: dirname 2.0 (22/08/25)\n";

/* Function declarations for forward references */
char *my_basename(char *path);
int test_part(char p);
int test_absolute(char *p);
void usage(char *program);

/* Main function */
int main(int argc, char **argv)
{
	char *p;
	char *path;
	char *program;

	program = my_basename(argv[0]);

	if (argc != 2) {
		fprintf(stderr, "Usage: %s path\n", program);
		usage(program);
		return 1;
	}

	path = argv[1];
	p = path + strlen(path);

	/* Remove trailing separators */
	while (p > path && test_part(p[-1])) {
		p--;
	}

	/* Remove last component */
	while (p > path && (!test_part(p[-1]))) {
		p--;
	}

	/* Remove trailing separators again */
	while (p > path && test_part(p[-1])) {
		p--;
	}

	if (p == path) {
		/* Root path or current directory */
		if (test_absolute(path)) {
			printf("%c\n", (path[0] == '/') ? '/' : ':');
		} else {
			printf(".\n");
		}
	} else {
		/* Print directory portion */
		printf("%.*s\n", (int)(p - path), path);
	}

	return 0;
}

/*
 * Test if character is a path separator
 * Returns TRUE if character is / or :
 */
int test_part(char p)
{
	return ((p == '/') || (p == ':'));
}

/*
 * Test if path is absolute (starts with / or :)
 * Returns TRUE if path is absolute
 */
int test_absolute(char *p)
{
	int tp;

	if (p == NULL || *p == '\0') {
		return FALSE;
	}

	tp = test_part(p[0]);

	if (!tp) {
		/* Check for Amiga device:path format */
		tp = (strstr(p, ":") != NULL);
	}

	return tp;
}

/*
 * Extract the basename from a path
 * Returns pointer to the filename part
 */
char *my_basename(char *path)
{
	char *temp;
	
	if (path == NULL) {
		return "dirname";
	}
	
	/* Find the last / or : */
	temp = strrchr(path, '/');
	if (temp == NULL) {
		temp = strrchr(path, ':');
	}
	
	if (temp != NULL) {
		return temp + 1;
	}
	
	return path;
}

/*
 * Display usage information
 */
void usage(char *program)
{
	fprintf(stderr, "usage: %s [OPTIONS] PATH\n", program);
	fprintf(stderr, "OPTIONS:\n");
	fprintf(stderr, "  -V          display version information\n");
	fprintf(stderr, "  -h          display this help message\n");
	fprintf(stderr, "DESCRIPTION:\n");
	fprintf(stderr, "  Print the directory portion of a pathname.\n");
	fprintf(stderr, "  Handles both Unix (/) and Amiga (:) path separators.\n");
	fprintf(stderr, "EXAMPLES:\n");
	fprintf(stderr, "  %s /usr/bin/ls          -> /usr/bin\n", program);
	fprintf(stderr, "  %s /usr/bin/            -> /usr/bin\n", program);
	fprintf(stderr, "  %s Work:Documents:file  -> Work:Documents\n", program);
	exit(1);
}
