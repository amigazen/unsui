/*
 * wc - Unsui POSIX runtime for Amiga
 * 
 * Copyright (c) 2025 amigazen project. All rights reserved.
 * Based on wc by David Messer and Andy Tanenbaum.
 * 
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 * 
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "wc.h"

/* Version tag for Amiga */
static const char *verstag = "$VER: wc 2.0 (22/08/25)\n";

/* Global flags and counters */
int lflag = FALSE;		/* Count lines */
int wflag = FALSE;		/* Count words */
int cflag = FALSE;		/* Count characters */

long lcount = 0;		/* Count of lines */
long wcount = 0;		/* Count of words */
long ccount = 0;		/* Count of characters */

long ltotal = 0;		/* Total count of lines */
long wtotal = 0;		/* Total count of words */
long ctotal = 0;		/* Total count of characters */

/* Function declarations for forward references */
char *my_basename(char *path);
void count(FILE *f);
void usage(char *program);
void reset_counters(void);

/* Main function */
int main(int argc, char **argv)
{
	int k;
	char *cp;
	int tflag, files;
	char *program;

	program = my_basename(argv[0]);

	/* Get flags */
	files = argc - 1;
	k = 1;
	
	if (argc > 1 && argv[1][0] == '-') {
		cp = argv[1] + 1;	/* Skip the '-' */
		files--;
		k++;			/* Points to first file */
		
		while (*cp != '\0') {
			switch (*cp) {
				case 'l':	lflag = TRUE;	break;
				case 'w':	wflag = TRUE;	break;
				case 'c':	cflag = TRUE;	break;
				default:	usage(program);
			}
			cp++;
		}
	}

	/* If no flags are set, treat as wc -lwc */
	if (!lflag && !wflag && !cflag) {
		lflag = TRUE;
		wflag = TRUE;
		cflag = TRUE;
	}

	/* Process files */
	tflag = files >= 2;	/* Set if # files > 1 */

	/* Check to see if input comes from std input */
	if (k >= argc) {
		count(stdin);
		if (lflag) printf(" %6ld", lcount);
		if (wflag) printf(" %6ld", wcount);
		if (cflag) printf(" %6ld", ccount);
		printf("\n");
		fflush(stdout);
		return 0;
	}

	/* There is an explicit list of files. Loop on files */
	while (k < argc) {
		FILE *f;

		if ((f = fopen(argv[k], "r")) == NULL) {
			fprintf(stderr, "%s: cannot open %s\n", program, argv[k]);
		} else {
			reset_counters();
			count(f);
			if (lflag) printf(" %6ld", lcount);
			if (wflag) printf(" %6ld", wcount);
			if (cflag) printf(" %6ld", ccount);
			printf(" %s\n", argv[k]);
			fclose(f);
		}
		k++;
	}

	/* Print totals if multiple files */
	if (tflag) {
		if (lflag) printf(" %6ld", ltotal);
		if (wflag) printf(" %6ld", wtotal);
		if (cflag) printf(" %6ld", ctotal);
		printf(" total\n");
	}
	
	fflush(stdout);
	return 0;
}

/*
 * Count lines, words, and characters in a file
 * Updates global counters and totals
 */
void count(FILE *f)
{
	int c;
	int word = FALSE;

	reset_counters();

	while ((c = getc(f)) != EOF) {
		ccount++;

		if (isspace(c)) {
			if (word) {
				wcount++;
			}
			word = FALSE;
		} else {
			word = TRUE;
		}

		if (c == '\n' || c == '\f') {
			lcount++;
		}
	}

	/* Add to totals */
	ltotal += lcount;
	wtotal += wcount;
	ctotal += ccount;
}

/*
 * Reset per-file counters
 */
void reset_counters(void)
{
	lcount = 0;
	wcount = 0;
	ccount = 0;
}

/*
 * Extract the basename from a path
 * Returns pointer to the filename part
 */
char *my_basename(char *path)
{
	char *temp;
	
	if (path == NULL) {
		return "wc";
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
	fprintf(stderr, "usage: %s [OPTIONS] [FILE...]\n", program);
	fprintf(stderr, "OPTIONS:\n");
	fprintf(stderr, "  -c          print the character counts\n");
	fprintf(stderr, "  -l          print the line counts\n");
	fprintf(stderr, "  -w          print the word counts\n");
	fprintf(stderr, "  -V          display version information\n");
	fprintf(stderr, "  -h          display this help message\n");
	fprintf(stderr, "DESCRIPTION:\n");
	fprintf(stderr, "  Print newline, word, and byte counts for each FILE.\n");
	fprintf(stderr, "  With no FILE, or when FILE is -, read standard input.\n");
	fprintf(stderr, "  Default: all three counts if no options specified.\n");
	fprintf(stderr, "EXAMPLES:\n");
	fprintf(stderr, "  %s file.txt              ->      15     120     850 file.txt\n", program);
	fprintf(stderr, "  %s -l file.txt           ->      15 file.txt\n", program);
	fprintf(stderr, "  %s -w -c file.txt        ->    120     850 file.txt\n", program);
	exit(1);
}
