/*
 * cron.c - POSIX-compliant cron daemon for Amiga
 *
 * This program operates as a daemon, waking up every minute
 * to execute scheduled commands from a crontab file.
 * *
 * Copyright (c) 2025 amigazen project. All rights reserved.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 * See LICENSE.md for full license text.
 */

#include "cron.h"

/* POSIX cron version information */
#define VERSION "3.0.0"
#define PROGRAM_NAME "cron"
 
int	eof;
short	sleepy;
char	min[SIZE], hour[SIZE], day[SIZE],
	month[SIZE], wday[SIZE], command[SIZE];
char	*tokv[] = { min, hour, day, month, wday };
char	crontabs[200];  /* Path to crontab file */
FILE	*fd;
 
 
/*
 * Main daemon loop and command execution
 */


void showhelp(void);
void wakeup(void);
char *scanner(register char *token, register char *offset);
int match(register char *left, register int right);
int getline(void);

int main(int argc, char *argv[])
{

	/* Get current time for timing calculations */

	register struct tm *tm;
	struct tm *localtime();
	long cur_time;
 	
	/* Display POSIX-compliant usage information */
	printf("%s version %s\n", PROGRAM_NAME, VERSION);
	printf("Usage: %s [crontab_file]\n", argv[0]);
	printf("       %s -h for help\n", argv[0]);

	/* Parse command line arguments for crontab file path or help options */
  
	if (argc == 2) {
		if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0) { 
			showhelp();
			exit(SUCCESS);
		}
		else if (strcmp(argv[1], "-v") == 0 || strcmp(argv[1], "--version") == 0) {
			printf("%s version %s\n", PROGRAM_NAME, VERSION);
			exit(SUCCESS);
		}
		else {
			(void)strcpy(crontabs, argv[1]);
		}
	}
	else {
		(void)strcpy(crontabs, CRONTAB);
	}	
	/* Use default crontab path if none specified */

	/* Display crontab file path */
	printf("Using crontab file: %s\n", crontabs);
	printf("%s daemon started, monitoring for scheduled tasks...\n", PROGRAM_NAME);

	for (;;)  {

		wakeup();

		time(&cur_time);		/* get the current time */
		tm = localtime(&cur_time);	/* break it down */

		/* Calculate seconds until next minute */

		sleepy = (60-tm->tm_sec);	

		/* Use Amiga.lib TimeDelay API for precise timing */
		TimeDelay(UNIT_VBLANK, sleepy, 0);	
		/* sleep 'till next minute using Amiga.lib TimeDelay */
	}
}

void wakeup(void)
{
	register struct tm *tm;
	long cur_time;
	
	char doit[80];
 	
	time(&cur_time);		/* Get current time */
	tm = localtime(&cur_time);	/* Convert to local time structure */
 
	/* Check if crontab file exists and is readable */ 	

	if ((fd = fopen(crontabs, "r")) == NULL) {
		fprintf(stderr, "%s: cannot open crontab file '%s': No such file or directory\n", PROGRAM_NAME, crontabs);
		exit(FAILURE);
	}


	eof = FALSE;
 
	while (!eof)  {
		if (getline() && match(min,tm->tm_min) &&
		   match(hour,tm->tm_hour) && match(day,tm->tm_mday) &&
		   match(month,tm->tm_mon+1) && match(wday,tm->tm_wday))  {
		/* Adjust for localtime months (0-11 range) */
			printf("%s: executing scheduled command: %s\n", PROGRAM_NAME, command);
			(void)strcpy(doit,"RUN ");
			(void)strcat(doit,command);
			(void)Execute(doit,NULL,NULL);
			}
		}
	fclose(fd);
}
 
 
/*
 * Crontab line parsing and field extraction
 *
 * Each line consists of six fields: minute, hour, day, month, weekday, command
 * Fields are separated by spaces or tabs, with the first field left-justified
 * (no leading spaces or tabs).
 */
 
int getline(void)
{
	register char *p;
	register int   i;
	char    buffer[MAXLINE];
 
	if (fgets(buffer, sizeof buffer, fd) == NULL)  {
		eof = TRUE;
		return(FALSE);
		}
 
	for (p = buffer, i = 0; i < 5; i++)  {
		if ((p = scanner(tokv[i], p)) == (char *)NULL)
			return(FALSE);
		}
 
	(void)strcpy(command, p);     /* Extract command field */
	return(TRUE);
}
 
 
char *scanner(register char *token, register char *offset)
{
	while ((*offset != ' ') && (*offset != '\t') && *offset)
		*token++ = *offset++;
 
	/*
	 * Check for end of string
	 */
         
	if (!*offset)
		return ((char *)NULL);
 
	*token = '\0';
        
	while ((*offset == ' ') || (*offset == '\t'))
		offset++;
 
	return (offset);
}
 
 
/*
 * Pattern matching for crontab time fields
 *
 * Supports the following syntax:
 * *       - Any value (matches all possible values)
 * x,y,z   - List of specific values
 * x-y     - Range of values from x through y
 */
 
int match(register char *left, register int right)
{
	register int	n;
	register char	c;
 
	n = 0;

	if (!strcmp(left, "*"))
		return(TRUE);
 
	while ((c = *left++) && (c >= '0') && (c <= '9'))
		n  =  (n * 10) + c - '0';
 
	switch (c)  {
		case '\0':
			return (right == n);
 
		case ',':
			if (right == n)
				return(TRUE);
			do {
				n = 0;
				while ((c = *left++) && (c >= '0') && (c <= '9'))
					n = (n * 10) + c - '0';
 				if (right == n)
					return(TRUE);
				} while (c == ',');
			return(FALSE);
 
		case '-':
			if (right < n)
				return(FALSE);
 
			n = 0;
			while ((c = *left++) && (c >= '0') && (c <= '9'))
				n = (n * 10) + c - '0';
 
			return(right <= n);
		}
}

void showhelp(void)
{ 
	printf("Usage: %s [OPTION] [crontab_file]\n", PROGRAM_NAME);
	printf("       %s -h, --help     display this help and exit\n", PROGRAM_NAME);
	printf("       %s -v, --version  display version information and exit\n", PROGRAM_NAME);
	printf("\n");
	printf("DESCRIPTION\n");
	printf("  %s is a daemon that executes scheduled commands at specified times.\n", PROGRAM_NAME);
	printf("  It reads commands from a crontab file and executes them at the\n");
	printf("  appropriate intervals.\n");
	printf("\n");
	printf("  crontab_file  Path to the crontab file (default: %s)\n", CRONTAB);
	printf("\n");
	printf("EXAMPLES\n");
	printf("  %s                    # Use default crontab file\n", PROGRAM_NAME);
	printf("  %s /tmp/my.crontab   # Use custom crontab file\n", PROGRAM_NAME);
	printf("  %s -v                # Show version information\n", PROGRAM_NAME);
	printf("\n");
	printf("CRONTAB FORMAT\n");
	printf("  Each line contains: minute hour day month weekday command\n");
	printf("  Fields: 0-59 0-23 1-31 1-12 0-6 (Sunday=0)\n");
	printf("  Special: * (any), , (list), - (range), / (step)\n");
	printf("\n");
	printf("  Example: 0 2 * * 1 backup  # Every Monday at 2:00 AM\n");
}
