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

#include <stdio.h>
#include <time.h>

#ifndef LATTICE			/*LK*/
#include <functions.h>		/*LK*/
#endif				/*LK*/

#ifdef LATTICE			/*LK*/
#include <string.h>
#include <proto/dos.h>
#endif				/*LK -the last one*/

#define TRUE 1
#define FALSE 0

#define CRONTAB "S:CronTab"	/* Our default path */

#define MAXLINE	132
#define SIZE	64

#define TITLE "\x9B1;33mAmiCron v2.5\x9B0m by \x9B1m<CB>\x9B0m & - \x9B3mThe Software Brewery\x9B0m -\n"
#define WHERE "Im Wingertsberg 45, D-6108 Weiterstadt, \x9B1mW-Germany\x9B0m\n"
 
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


void main(argc, argv)
int argc;
char *argv[];
{
	void showhelp();
	void wakeup();

	/* Below we use the same functions & variables as in wakeup()
	   just to get the current second  <CB>*/

	register struct tm *tm;
	struct tm *localtime();
	long cur_time;
 	
	/* Tell the guy out there what he might do with this deamon
	 * and who made it. <CB>*/

	printf(TITLE);
	printf(WHERE);
	printf("Usage: %s [Path & Filename]\n",argv[0]);
	printf("       or '%s ?' for help\n",argv[0]);

	/* Now let's see if the user told us to look someplace else
	 * for our input file or if he want's some more usage hints <CB> */
  
	if (argc == 2) {
		if (argv[1][0] == '?') { 
			showhelp();
			exit(10);
		}
		else (void)strcpy(crontabs, argv[1]);
		
	}
	else (void)strcpy(crontabs, CRONTAB);	
	/* If there isn't a user path, we'll use the default <CB>*/

	/*Now tell the user with what path & filename we wound up*/
	printf("CronTab path = %s \n",crontabs);

	for (;;)  {

		wakeup();

		time(&cur_time);		/* get the current time */
		tm = localtime(&cur_time);	/* break it down */

		/* Now we calculate the amount of seconds
		 * 'till the next minute <CB>*/

		sleepy = (60-tm->tm_sec);	
		/* PLEASE don't decrease the 60 above, believe me,
		 * I know what I'm doing there! <CB> */ 

		TimeDelay(sleepy,0,0);	
		/* sleep 'till next minute */
	}
}

void wakeup()
{
	register struct tm *tm;
	struct tm *localtime();
	long cur_time;
	
	char doit[80];
 	
	time(&cur_time);		/* get the current time */
	tm = localtime(&cur_time);	/* break it down */
 
	/* Now let's see if there is a CronTab file out there <CB> */ 	

	if ((fd = fopen(crontabs, "r")) == NULL) {
	fprintf(stderr, "Can't open %s\nTry again\n", crontabs);
	exit(1);
	}


	eof = FALSE;
 
	while (!eof)  {
		if (getline() && match(min,tm->tm_min) &&
		   match(hour,tm->tm_hour) && match(day,tm->tm_mday) &&
		   match(month,tm->tm_mon+1) && match(wday,tm->tm_wday))  {
		/* Weird localtime months ^ range from 0-11 !!! <CB>*/
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
 
int getline()
{
	register char *p;
	register int   i;
	char    buffer[MAXLINE], *scanner(), *fgets();
 
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
 
 
char *scanner(token, offset)
register char   *token;		/* target buffer to receive scanned token */
register char   *offset;	/* place holder into source buffer */
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
 
int match(left, right)
register char   *left;
register int    right;
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

void showhelp()		/* by <CB> */

{ 
	printf("\nWell, you really should read the .doc file,\n");
	printf("but here are some more hints:\n");
	printf("You might specify only ONE command line parameter,\n");
	printf("the path were AmiCron should look for it's command table,\n");
	printf("including the filename of the command table.\n");
	printf("If you don't supply a path, AmiCron will use it's default\n");
	printf("path (S:CronTab).\n");
}
