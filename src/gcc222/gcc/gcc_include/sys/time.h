/*	system time for ansic.library		*/
/*	(c)Copyright 1992 Davide Pasetto 	*/

#ifndef	_SYS_TIME_H
#define _SYS_TIME_H

/* Structure returned by gettimeofday(2) system call, and used in other calls. */


struct timeval {
	long	tv_sec; 	/* seconds */
	long	tv_usec;	/* and microseconds */
};


struct timezone {
	int	tz_minuteswest;	/* minutes west of Greenwich */
	int	tz_dsttime;		/* type of dst correction */
};

/* types recognized for dst correction */

#define DST_NONE	0	/* not on dst */
#define DST_USA 	1	/* USA style dst */
#define DST_AUST	2	/* Australian style dst */
#define DST_WET 	3	/* Western European dst */
#define DST_MET 	4	/* Middle European dst */
#define DST_EET 	5	/* Eastern European dst */

/* timer macro (not implemented)

#define timerclear(tvp) (tvp)->tv_sec = (tvp)->tv_usec = 0L
#define timerisset(tvp) ((tvp)->tv_sec || (tvp)->tv_usec)

*/

/* Names of the interval timers, and structure defining a timer setting (not implemented). */

#define ITIMER_REAL     0
#define ITIMER_VIRTUAL  1
#define ITIMER_PROF     2

struct	itimerval {
	struct timeval	it_interval;	/* timer interval */
	struct timeval	it_value;		/* current value */
};


/* On the Amiga system date starts from 1st Jan 1978	*/
/* On every Unix box system date starts from 1st Jan 1970				*/

#define	DATE_AMIGA_TO_UNIX_SEC	252460800 	/* seconds */

#endif	/* _SYS_TIME_H */
