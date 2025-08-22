/*
 * basename - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on basename by B. Garfolo & P. Nelson.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "basename.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: basename 2.0 (22/08/25)\n";

/* Main function */
int main(int argc, char **argv)
{
	char *result_string;	/* The pointer into argv[1] */
	char *temp;		/* Used to move around in argv[1] */
	int suffix_len;		/* Length of the suffix */
	int suffix_start;	/* Where the suffix should start */
	char *program;

	program = my_basename(argv[0]);

	/* Check for the correct number of arguments */
	if ((argc < 2) || (argc > 3)) {
		fprintf(stderr, "Usage: %s string [suffix]\n", program);
		exit(1);
	}

	/* Check for all /'s or :'s (Amiga path separator) */
	for (temp = argv[1]; (*temp == '/') || (*temp == ':'); temp++) {
		/* Move to next char */
	}
	if (*temp == '\0') {
		printf("%c\n", (argv[1][0] == '/') ? '/' : ':');
		exit(0);
	}

	/* Build the basename */
	result_string = argv[1];

	/* Find the last / or : */
	temp = strrchr(result_string, '/');
	if (temp == NULL) {
		temp = strrchr(result_string, ':');
	}

	if (temp != NULL) {
		/* Remove trailing /'s or :'s */
		while ((*(temp + 1) == '\0') && ((*temp == '/') || (*temp == ':'))) {
			*temp-- = '\0';
		}

		/* Set result_string to last part of path */
		if (*temp != '/' && *temp != ':') {
			temp = strrchr(result_string, '/');
			if (temp == NULL) {
				temp = strrchr(result_string, ':');
			}
		}
		if (temp != NULL && (*temp == '/' || *temp == ':')) {
			result_string = temp + 1;
		}
	}

	/* Remove the suffix, if any */
	if (argc > 2) {
		suffix_len = strlen(argv[2]);
		suffix_start = strlen(result_string) - suffix_len;
		if (suffix_start > 0) {
			if (strcmp(result_string + suffix_start, argv[2]) == 0) {
				*(result_string + suffix_start) = '\0';
			}
		}
	}

	/* Print the resultant string */
	printf("%s\n", result_string);
	return 0;
}

/*
 * Extract the basename from a path
 * Returns pointer to the filename part
 */
char *my_basename(char *path)
{
	char *temp;
	
	if (path == NULL) {
		return "basename";
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
	fprintf(stderr, "usage: %s [OPTIONS] NAME [SUFFIX]\n", program);
	fprintf(stderr, "OPTIONS:\n");
	fprintf(stderr, "  -V          display version information\n");
	fprintf(stderr, "  -h          display this help message\n");
	fprintf(stderr, "DESCRIPTION:\n");
	fprintf(stderr, "  Print NAME with any leading directory components removed.\n");
	fprintf(stderr, "  If SUFFIX is specified, also remove a trailing SUFFIX.\n");
	fprintf(stderr, "EXAMPLES:\n");
	fprintf(stderr, "  %s /usr/bin/ls          -> ls\n", program);
	fprintf(stderr, "  %s /usr/bin/ls .exe     -> ls\n", program);
	fprintf(stderr, "  %s Work:Documents:file  -> file\n", program);
	exit(1);
}
