/*
 * sys/times.h: POSIX times()
 */
/* $Id: times.h,v 1.3 93/05/05 21:17:53 sjg Exp $ */

#if ! _TIMES_H
#define	_TIMES_H 1

#include <time.h>		/* defines CLK_TCK */

#if __STDC__
#define	ARGS(args)	args
#else
#define	ARGS(args)	()
#endif

struct tms {
	clock_t	tms_utime, tms_stime;
	clock_t	tms_cutime, tms_cstime;
};

#if _V7
#define times times_
#endif

clock_t	times ARGS((struct tms *tmsp));

#endif

