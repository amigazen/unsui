/*
 *	amicron.c	(Version 2.5(a&L) by <CB>)
 *
 *	Public Domain (p) No Rights Reserved
 *
 *	This program operates as a daemon, waking up every minute
 *	to execute the CRONTAB table.  
 *
 *	Put in startup-sequence like:
 *		run newcli con:0/140/160/50/CronTask s:startcron
 *	The startcron file needs to contains just one line:
 *		amicron
 *
 *	Some notes are included below concerning the cron table
 *	file format.  In this version the cron table file is left
 *	on disk and not moved to core, and  the command character
 *	to signify a newline '%' is not used.
 *
 *	Some cron table entry examples:
 *
 *	Print the date in the crontask window every minute:
 *		* * * * * date
 *
 *	Print the date in the crontask window on the hour, every hour:
 *		0 * * * * date
 *
 *	Run uupc at 4:30 am every day except Sat and Sun:
 *		30 4 * * 1-5 uupc -siscuva
 *
 *	Backup the files every other day at 7:30 pm:
 *		30 19 * * 1,3,5 sdbackup -s LAST dh0: incbkup_1:
 *
 */

/* 
 * Public Domain (p) by S. R. Sampson
 * Version 2.3, October 1987
 * Amiga port by Rick Schaeffer October 1987
 *
 * Rick Schaeffer          UUCP:  seismo!uunet!iscuva!ricks!ricks
 * E. 13611 26th Ave.      Phone: (509)928-3533
 * Spokane, WA  99216
 *
 * Modified path for CronTab & CronErr to suit Amiga enviroment better
 * Version 2.31  <CB> 15.12.87 
 *
 * Fixed bug with CronTab entries specifying an event by date (month)
 * i.e. "* * 24 12 * say Merry christmas", now works!
 * Version 2.32 <CB> 25.12.87
 *
 * Removed "CronErr", an obvious Unix "feature"
 * Version 2.33 <CB> 31.12.87 
 *
 * Additional support for Lattice 4.0, changed to complete sleep (no more  
 * 5 I/O ints. per second), added command line parameter for CronTab path.
 * Added feature to align Cron to start on 01 seconds, including "loss"
 * due to lengthy command calls or extensive I/O.
 * Version 2.4 <CB> 10.01.88 
 *
 * GRRRRRRR. Just when I thought it was safe to release that thing, C=A brings
 * out AmyDos 1.3. So to have the output of programs called by Cron visible, I
 * changed the "Run >nil:" parameter for execute() back to "Run ". This means,                         
 * that you will have to look at the annoying CLI[#] messages, each time some 
 * program is called. I was pleased with the way the old RUN >nil: worked, and
 * will try to convince Andy Finkel to bring it back.
 * In the meantime, this is:
 * Version 2.5 <CB> 27.03.88

                         
______  /          
______\O                    - The Software Brewery - 
      \\              		    
       o            Sparkling, fresh software from W.-Germany
                 
     @@@@@	      Straight from the bar to your Amiga
     |~~~|\        
     | | |/        
     |___|        With our regards to the Software Distillery

Members are (listed alphabetically):
Christian Balzer alias <CB>, Lattice C, user interfaces, beer addict. 
Christof Bonnkirch, Aztec C, telecommunications, money adict.
Heiko Rath alias <HR>, Assembler, ROM-Kernal stuff, Marabou addict. 
Peter Stark alias PS, Lattice C, IO & utilities, WordStar addict.
Ralf Woitinas alias RAF, Assembler, anything, Ray-Tracing addict.
Torsten Wronski alias MM, Assembler, anything, girls addict.

Beverages: Altenmuenster Brauer Bier, Urfraenkisches Landbier, Grohe Bock.

Send exotic drinks, comments, critizism, flames to:

The Software Brewery
Christian Balzer
Im Wingertsberg 45
D-6108 Weiterstadt
West-Germany

Our BBS "AmigaNode" isn't online yet. As soon as it becomes available, 
you'll be the first to know :-).

 *
 * Compilation notes:
 * Just use the supplied "makefile.manx" for Aztec C 3.4(b) or later. 
 * Lattice C 4.0 users just execute "lmake" or use "makefile.lattice".
 * The whole compilation and link procedure (including timer.c) from RAM
 * took me 50 seconds with Lattice 4.0 and 67 seconds with Aztec 3.4b.
 * Please note that you need to link timer.o for the TimeDelay() function
 * if you don't use make or execute lmake.
 *
 * Well, the Lattice folks claim that LATTICE get's defined by their 
 * compiler, but it didn't work for me. So, if you don't use make or lmake, 
 * or define LATTICE, you'll get blasted by (my) Lattice if ya don't remove 
 * all the lines marked with "LK" below...
 *						<CB>
 */

#include "cron.h"

/* POSIX cron version information */
#define VERSION "3.0"
#define PROGRAM_NAME "cron"
 
int	eof;
short	sleepy;
char	min[SIZE], hour[SIZE], day[SIZE],
	month[SIZE], wday[SIZE], command[SIZE];
char	*tokv[] = { min, hour, day, month, wday };
char	crontabs[200];  /* You might define quite a path! <CB> */
FILE	*fd;
 
 
/*
 *	This is the basics, ready for bells and whistles
 *
 */


void showhelp(void);
void wakeup(void);
char *scanner(register char *token, register char *offset);
int match(register char *left, register int right);
int getline(void);

int main(int argc, char *argv[])
{

	/* Below we use the same functions & variables as in wakeup()
	   just to get the current second  <CB>*/

	register struct tm *tm;
	struct tm *localtime();
	long cur_time;
 	
	/* Display POSIX-compliant usage information */
	printf("%s version %s\n", PROGRAM_NAME, VERSION);
	printf("Usage: %s [crontab_file]\n", argv[0]);
	printf("       %s -h for help\n", argv[0]);

	/* Now let's see if the user told us to look someplace else
	 * for our input file or if he want's some more usage hints <CB> */
  
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
	/* If there isn't a user path, we'll use the default <CB>*/

	/* Display crontab file path */
	printf("Using crontab file: %s\n", crontabs);
	printf("%s daemon started, monitoring for scheduled tasks...\n", PROGRAM_NAME);

	for (;;)  {

		wakeup();

		time(&cur_time);		/* get the current time */
		tm = localtime(&cur_time);	/* break it down */

		/* Now we calculate the amount of seconds
		 * 'till the next minute <CB>*/

		sleepy = (60-tm->tm_sec);	
		/* PLEASE don't decrease the 60 above, believe me,
		 * I know what I'm doing there! <CB> */ 

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
 	
	time(&cur_time);		/* get the current time */
	tm = localtime(&cur_time);	/* break it down */
 
	/* Now let's see if there is a CronTab file out there <CB> */ 	

	if ((fd = fopen(crontabs, "r")) == NULL) {
		fprintf(stderr, "%s: cannot open crontab file '%s': No such file or directory\n", PROGRAM_NAME, crontabs);
		exit(FAILURE);
	}


	eof = FALSE;
 
	while (!eof)  {
		if (getline() && match(min,tm->tm_min) &&
		   match(hour,tm->tm_hour) && match(day,tm->tm_mday) &&
		   match(month,tm->tm_mon+1) && match(wday,tm->tm_wday))  {
		/* Weird localtime months ^ range from 0-11 !!! <CB>*/
			printf("%s: executing scheduled command: %s\n", PROGRAM_NAME, command);
			(void)strcpy(doit,"RUN ");
			(void)strcat(doit,command);
			(void)Execute(doit,NULL,NULL);
			}
		}
	fclose(fd);
}
 
 
/*
 *	A line consists of six fields.  The first five are:
 *
 *		minute:         0-59
 *		hour:           0-23
 *		day:            1-31
 *		month:          1-12
 *		weekday:        0-6 (Sunday = 0)
 *
 *	The fields are seperated by spaces or tabs, with the
 *	first field left justified (no leading spaces or tabs).
 *	See below for optional field syntax.
 *
 *	The last field is the command field.  This command will
 *	be executed by the CLI just as if typed from a console.
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
 
	(void)strcpy(command, p);     /* scoop the command */
	return(TRUE);
}
 
 
char *scanner(register char *token, register char *offset)
{
	while ((*offset != ' ') && (*offset != '\t') && *offset)
		*token++ = *offset++;
 
	/*
	 *      Check for possible error condition
	 */
         
	if (!*offset)
		return ((char *)NULL);
 
	*token = '\0';
        
	while ((*offset == ' ') || (*offset == '\t'))
		offset++;
 
	return (offset);
}
 
 
/*
 *	This routine will match the left string with the right number.
 *
 *	The string can contain the following syntax:
 *
 *	*		This will return TRUE for any number
 *	x,y [,z, ...]	This will return TRUE for any number given.
 *	x-y		This will return TRUE for any number within
 *			the range of x thru y.
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
